# ==========================================================
# BUILD SUBNATIONAL MALARIA RESPONSE GAP DATASET (admin-1)
# Source: Malaria Atlas Project (rasters + admin boundaries)
# Output: data/malaria_response_gap_admin1.csv
#         data/admin1_boundaries.gpkg
#
# Metric definition (per revised scope):
#   Burden_Score   = mean( minmax(Incidence), minmax(Prevalence) )    # 0-1
#   Response_Score = mean( ITN_Use, Effective_Treatment )             # 0-1
#   Gap_Index      = max( Burden_Score - Response_Score, 0 )
#
# Run in YOUR OWN R session (needs internet to the MAP geoserver).
# Manual step: confirm the SURFACES dataset_ids at STEP 3 if the guard
# reports any as invalid.
# ==========================================================

# install.packages(c("malariaAtlas","sf","terra","exactextractr",
#                     "dplyr","tidyr","stringr","purrr","readr"))
library(malariaAtlas)   # CRAN >= 1.7.0
library(sf)
library(terra)
library(exactextractr)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(readr)

# ==========================================================
# CONFIG
# ==========================================================

ISO_CODES <- c(
  Tanzania      = "TZA",
  Kenya         = "KEN",
  Uganda        = "UGA",
  Ethiopia      = "ETH",
  Eritrea       = "ERI",
  Rwanda        = "RWA",
  `South Sudan` = "SSD",
  Madagascar    = "MDG"
)

YEARS       <- 2015:2024
ADMIN_LEVEL <- "admin1"      # default first level for every country
# Per-country override. Uganda's admin-1 is only 4 statistical regions, so use
# admin-2 (districts) there to make "regions" meaningful everywhere.
ADMIN_LEVEL_BY_COUNTRY <- c(Uganda = "admin2")

# Burden scaling:
#  "absolute"     - prevalence used directly (PfPR is already 0-1); incidence
#                   scaled by a FIXED cap. Panel-independent. Recommended for a
#                   mixed-burden set like this (avoids the gap collapsing to 0).
#  "panel_minmax" - min-max each indicator across the panel (original method;
#                   makes burden RELATIVE to the worst region in the set).
BURDEN_SCALING   <- "absolute"
INC_CAP_PER_1000 <- 500       # incidence (cases per 1,000/yr) mapped to burden=1

# Gap definition:
#  "difference" - max(Burden - Response, 0). Matches your abstract.
#  "unmet_need" - Burden * (1 - Response): naturally graded 0-1, no flooring.
#                 A DEFINITIONAL change from the abstract - clear with supervisor.
GAP_METHOD <- "difference"

# Tiering:
#  "fixed"    - absolute cutoffs (0.15 / 0.35 / 0.55).
#  "quantile" - rank positive gaps into Moderate/High/Critical (top 10% = Critical).
#               Best for a prioritisation MAP when most absolute gaps are small.
TIER_METHOD <- "quantile"

# IRS dropped from scope. Response = mean(ITN use, effective treatment).
# Population raster (optional) for population-weighted aggregation.
POP_DATASET_ID <- NA   # a MAP population dataset_id, OR
POP_TIF_PATH   <- NA   # path to your own WorldPop GeoTIFF; both NA => area-weighted

OUT_CSV  <- "data/malaria_response_gap_admin1.csv"
OUT_GPKG <- "data/admin1_boundaries.gpkg"

lvl_num <- as.integer(gsub("\\D", "", ADMIN_LEVEL))

# ==========================================================
# STEP 1 — Admin boundaries (per-country level)
# ==========================================================
pick_col <- function(df, patterns, what) {
  nm <- names(df)
  for (p in patterns) {
    hit <- nm[grepl(p, nm, ignore.case = TRUE)]
    if (length(hit)) return(hit[[1]])
  }
  stop("Could not find the ", what, " column.\nAvailable: ",
       paste(nm, collapse = ", "), call. = FALSE)
}

get_units <- function(iso, cname) {
  lvl  <- if (cname %in% names(ADMIN_LEVEL_BY_COUNTRY)) ADMIN_LEVEL_BY_COUNTRY[[cname]] else ADMIN_LEVEL
  lnum <- as.integer(gsub("\\D", "", lvl))
  message("  ", cname, " -> ", lvl)
  shp  <- sf::st_as_sf(getShp(ISO = iso, admin_level = lvl))
  ncol <- pick_col(shp, c(paste0("^name_?", lnum, "$"), paste0("name.*", lnum, "$"), "shapename"),
                   paste(cname, "name"))
  icol <- pick_col(shp, c(paste0("^id_?", lnum, "$"), paste0("^gid_?", lnum, "$"),
                          paste0("id.*", lnum, "$"), "shapeid"), paste(cname, "id"))
  shp$Admin1      <- as.character(shp[[ncol]])
  shp$Admin1_ID   <- paste(cname, as.character(shp[[icol]]), sep = "_")  # unique across mixed levels
  shp$Country     <- cname
  shp$Admin_Level <- lvl
  shp[, c("Admin1", "Admin1_ID", "Country", "Admin_Level")]
}

message("Downloading boundaries (per-country level) ...")
admin1 <- imap(ISO_CODES, get_units) %>% bind_rows()
if (any(duplicated(admin1$Admin1_ID)))
  admin1$Admin1_ID <- make.unique(admin1$Admin1_ID, sep = "_")
stopifnot(!all(is.na(admin1$Admin1)))
message("Units per country:")
print(sf::st_drop_geometry(admin1) %>% count(Country, Admin_Level, name = "n_units"))

# dTolerance in degrees (~1 km); lon/lat warning is expected & harmless.
admin1_simplified <- suppressWarnings(sf::st_simplify(admin1, dTolerance = 0.01))

ext_vec <- sf::st_bbox(admin1)
extent_arg <- c(xmin = ext_vec[["xmin"]], xmax = ext_vec[["xmax"]],
                ymin = ext_vec[["ymin"]], ymax = ext_vec[["ymax"]])

# ==========================================================
# STEP 2 — Population raster (optional weighting)
# ==========================================================
rast_catalogue <- as_tibble(listRaster(printed = FALSE))

# Coerce whatever getRaster returns (SpatRaster / RasterLayer / list) to SpatRaster.
as_spatraster <- function(x) {
  if (inherits(x, "SpatRaster")) return(x)
  if (is.list(x) && !inherits(x, "Raster")) {
    return(do.call(c, lapply(x, function(e)
      if (inherits(e, "SpatRaster")) e else terra::rast(e))))
  }
  terra::rast(x)
}

# Fetch + coerce with retry/backoff (MAP server resets connections under load).
fetch_raster <- function(dataset_id, year, tries = 4) {
  for (i in seq_len(tries)) {
    r <- tryCatch(getRaster(dataset_id = dataset_id, shp = admin1, year = year),
                  error = function(e) e)
    if (!inherits(r, "error")) return(as_spatraster(r))
    wait <- 3 * i
    message("  fetch failed (", i, "/", tries, "): ", conditionMessage(r),
            " - retrying in ", wait, "s")
    Sys.sleep(wait)
  }
  stop("getRaster failed after ", tries, " tries: ", dataset_id, " ", year, call. = FALSE)
}

get_pop_raster <- function(year) {
  if (!is.na(POP_DATASET_ID)) {
    fetch_raster(POP_DATASET_ID, year)
  } else if (!is.na(POP_TIF_PATH)) {
    terra::rast(POP_TIF_PATH)
  } else NULL
}

# ==========================================================
# STEP 3 — Surfaces (incidence, prevalence, ITN use, treatment)
# ==========================================================
# NOTE: Pf-only. In Ethiopia/Eritrea/Madagascar P. vivax is co-endemic,
# so Pf surfaces may understate true burden — flag in your methods.
SURFACES <- list(
  Incidence_Rate       = "Malaria__202508_Global_Pf_Incidence_Rate",
  Infection_Prevalence = "Malaria__202508_Global_Pf_Parasite_Rate",   # PfPR 2-10, same release
  ITN_Use              = "Interventions__202508_Africa_Insecticide_Treated_Net_Use",
  Effective_Treatment  = "Interventions__202508_Global_Antimalarial_Effective_Treatment"
)

# --- validate dataset_ids before downloading ------------------------------
ids     <- unlist(SURFACES)
invalid <- ids[grepl("^REPLACE_WITH", ids) | !(ids %in% rast_catalogue$dataset_id)]
if (length(invalid)) {
  message("\n!! Invalid dataset_id(s):"); print(invalid)
  message("\nFull (untruncated) options — copy EXACTLY:")
  for (kw in c("incidence", "parasite rate|prevalence|pfpr|pf_pr|_pr$",
               "itn.*use|net use|insecticide-treated.*use", "effective treatment")) {
    cat("\n## matches for '", kw, "':\n", sep = "")
    print(rast_catalogue$dataset_id[
      grepl(kw, tolower(paste(rast_catalogue$dataset_id, rast_catalogue$title,
                              rast_catalogue$title_extended, rast_catalogue$abstract)))])
  }
  stop("Set SURFACES to full dataset_id strings, then re-run.", call. = FALSE)
}

# --- trim YEARS to the range ALL chosen surfaces cover --------------------
cov <- rast_catalogue %>%
  filter(dataset_id %in% ids, !is.na(min_raster_year), !is.na(max_raster_year))
if (nrow(cov) > 0) {
  yr_lo <- max(as.integer(cov$min_raster_year))
  yr_hi <- min(as.integer(cov$max_raster_year))
  avail <- YEARS[YEARS >= yr_lo & YEARS <= yr_hi]
  if (length(avail) < length(YEARS))
    message(sprintf("NOTE: surfaces jointly cover %d-%d; trimming YEARS.", yr_lo, yr_hi))
  YEARS <- avail
  stopifnot(length(YEARS) > 0)
  message("Building for years: ", paste(range(YEARS), collapse = "-"))
}

# ==========================================================
# STEP 4 — Zonal aggregation: raster -> admin unit, per year
# ==========================================================
extract_one <- function(dataset_id, year) {
  r <- fetch_raster(dataset_id, year)
  # Some releases return a multi-layer stack (whole time series or bounds).
  # Pick the layer matching this year; fall back to the first layer.
  if (terra::nlyr(r) > 1) {
    idx <- which(grepl(as.character(year), names(r)))
    r <- if (length(idx) >= 1) r[[idx[1]]] else {
      message("  ", dataset_id, ": ", terra::nlyr(r),
              " layers, none named for ", year, " - using layer 1")
      r[[1]]
    }
  }
  pop <- get_pop_raster(year)
  if (!is.null(pop)) {
    pop  <- terra::resample(pop, r)
    vals <- exact_extract(r, admin1, fun = "weighted_mean", weights = pop, progress = FALSE)
  } else {
    vals <- exact_extract(r, admin1, fun = "mean", progress = FALSE)
  }
  if (is.data.frame(vals)) vals <- vals[[1]]      # guard against residual multi-column
  tibble(Admin1_ID = admin1$Admin1_ID, value = as.numeric(vals))
}

build_indicator <- function(indicator, dataset_id) {
  message("Aggregating ", indicator, " ...")
  map_dfr(YEARS, function(y)
    extract_one(dataset_id, y) %>% mutate(Year = y, indicator = indicator))
}

long <- imap_dfr(SURFACES, ~ build_indicator(.y, .x))

wide <- long %>%
  pivot_wider(names_from = indicator, values_from = value) %>%
  left_join(sf::st_drop_geometry(admin1)[, c("Admin1_ID", "Admin1", "Country", "Admin_Level")] %>% distinct(),
            by = "Admin1_ID")

# ==========================================================
# STEP 5 — Recompute the MRGI (revised metric)
# ==========================================================
# Put proportion indicators on 0-1 (MAP surfaces are usually 0-1;
# values > 1 are treated as percentages).
to01 <- function(x) ifelse(x > 1.0001, x / 100, x)
prop_cols <- c("Infection_Prevalence", "ITN_Use", "Effective_Treatment")
wide <- wide %>% mutate(across(all_of(prop_cols), to01))

# Put incidence on a per-1,000 scale whatever the surface used (Explorer is
# per-person ~0-0.5; the Malaria__ release rate may be per-1,000).
if (median(wide$Incidence_Rate, na.rm = TRUE) < 5) {
  message("Incidence looks per-person; scaling x1000 to per-1,000.")
  wide$Incidence_Rate <- wide$Incidence_Rate * 1000
}

# Panel min-max helper (used only when BURDEN_SCALING == "panel_minmax").
minmax <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (!is.finite(diff(rng)) || diff(rng) == 0) return(rep(0, length(x)))
  pmin(pmax((x - rng[1]) / diff(rng), 0), 1)
}

if (BURDEN_SCALING == "absolute") {
  wide <- wide %>%
    mutate(inc_n  = pmin(pmax(Incidence_Rate / INC_CAP_PER_1000, 0), 1),
           prev_n = pmin(pmax(Infection_Prevalence, 0), 1))
} else {
  wide <- wide %>%
    mutate(inc_n = minmax(Incidence_Rate), prev_n = minmax(Infection_Prevalence))
}

mrgi <- wide %>%
  mutate(Burden_Score = rowMeans(cbind(inc_n, prev_n), na.rm = TRUE)) %>%
  rowwise() %>%
  mutate(Response_Score = mean(c_across(all_of(c("ITN_Use", "Effective_Treatment"))),
                               na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Gap_Index = if (GAP_METHOD == "unmet_need")
                       Burden_Score * (1 - Response_Score)
                     else pmax(Burden_Score - Response_Score, 0))

assign_tiers <- function(g) {
  if (TIER_METHOD == "quantile") {
    pos <- g[g > 0 & is.finite(g)]
    if (length(pos) >= 4) {
      cM <- quantile(pos, 0.40); cH <- quantile(pos, 0.70); cC <- quantile(pos, 0.90)
      return(case_when(
        g <= 0  ~ "Low",
        g >= cC ~ "Critical",
        g >= cH ~ "High",
        g >= cM ~ "Moderate",
        TRUE    ~ "Low"
      ))
    }
  }
  case_when(                                   # "fixed"
    g >= 0.55 ~ "Critical",
    g >= 0.35 ~ "High",
    g >= 0.15 ~ "Moderate",
    TRUE      ~ "Low"
  )
}
mrgi$Priority_Tier <- assign_tiers(mrgi$Gap_Index)

# ==========================================================
# STEP 6 — Sanity summary (catch unit/scale problems)
# ==========================================================
message("\nAdmin-1 units per country:")
print(mrgi %>% distinct(Country, Admin1) %>% count(Country, name = "n_units"))

message("\nIndicator ranges (should be plausible: prevalence/ITN/treatment 0-1):")
print(mrgi %>% summarise(across(c(Incidence_Rate, Infection_Prevalence,
                                  ITN_Use, Effective_Treatment, Gap_Index),
                                list(min = ~min(.x, na.rm = TRUE),
                                     med = ~median(.x, na.rm = TRUE),
                                     max = ~max(.x, na.rm = TRUE)))) %>%
        pivot_longer(everything()))

message("\nTier distribution & NA counts:")
print(count(mrgi, Priority_Tier))
print(mrgi %>% summarise(across(everything(), ~sum(is.na(.x)))) %>%
        pivot_longer(everything()) %>% filter(value > 0))

message("\nWhere the gap concentrates (top 15 admin-1 unit-years):")
print(mrgi %>% arrange(desc(Gap_Index)) %>%
        transmute(Country, Admin1, Year,
                  Burden   = round(Burden_Score, 3),
                  Response = round(Response_Score, 3),
                  Gap      = round(Gap_Index, 3),
                  Priority_Tier) %>%
        head(15))

# ==========================================================
# STEP 7 — Write outputs
# ==========================================================
out <- mrgi %>%
  mutate(across(all_of(prop_cols), ~ round(.x * 100, 2)),           # proportions -> percent
         Incidence_Rate = round(Incidence_Rate, 2)) %>%             # already per 1,000
  select(Country, Admin1, Admin1_ID, Admin_Level, Year,
         Incidence_Rate, Infection_Prevalence, ITN_Use, Effective_Treatment,
         Burden_Score, Response_Score, Gap_Index, Priority_Tier) %>%
  arrange(Country, Admin1, Year)

dir.create(dirname(OUT_CSV), showWarnings = FALSE, recursive = TRUE)
write_csv(out, OUT_CSV)

sf::st_write(admin1_simplified[, c("Admin1", "Admin1_ID", "Country")],
             OUT_GPKG, delete_dsn = TRUE)

message("Done. Wrote ", OUT_CSV, " and ", OUT_GPKG)


install.packages("rsconnect")
library(rsconnect)

rsconnect::setAccountInfo(name='ebansaria', token='571ACEB01F2812AA44BEA25A6C375A3E', secret='HCTgoUTBneSb0gGtF4YGGX6SmB9w5bWp6cdMu1Gr')
rsconnect::deployApp("C:/Users/eban/Documents/ROI/MRGS")
