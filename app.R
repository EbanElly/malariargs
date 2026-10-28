# ==========================================================
# MALARIA RESPONSE GAP — subnational dashboard (plain-language edition)
# About · Overview · Explore · Trends · Rankings
# Reads:  data/malaria_response_gap_admin1.csv  +  data/admin1_boundaries.gpkg
# Needs:  shiny shinydashboard shinyWidgets shinycssloaders sparkline plotly leaflet sf
#         dplyr tidyr DT scales ggplot2 gridExtra ggiraph
# ==========================================================

library(shiny); library(shinydashboard); library(shinyWidgets); library(shinycssloaders)
library(sparkline); library(plotly); library(leaflet); library(sf)
library(dplyr); library(tidyr); library(DT); library(scales)
library(ggplot2); library(gridExtra); library(grid); library(ggiraph)

# ---------------- DATA ----------------
DATA_CSV <- "data/malaria_response_gap_admin1.csv"; DATA_GPKG <- "data/admin1_boundaries.gpkg"
stopifnot(file.exists(DATA_CSV)); if (!file.exists(DATA_GPKG)) stop("Missing ", DATA_GPKG, call. = FALSE)

df <- read.csv(DATA_CSV, stringsAsFactors = FALSE) %>% filter(!grepl("^Lake ", Admin1))
df$Admin1_ID <- as.character(df$Admin1_ID)
all_years <- sort(unique(df$Year))
admin_all <- st_read(DATA_GPKG, quiet = TRUE) %>% st_transform(4326)
admin_sf <- admin_all %>% filter(!grepl("^Lake ", Admin1))
admin_sf$Admin1_ID <- as.character(admin_sf$Admin1_ID)
admin_sf <- tryCatch(st_make_valid(st_simplify(admin_sf, dTolerance = 0.008, preserveTopology = TRUE)),
                     error = function(e) admin_sf)
countries <- sort(unique(df$Country))

# Masks that blank out the basemap outside the study area ("All") or outside one country,
# so maps only show the places in this work. Built from the unfiltered layer so lakes stay
# visible; interior rings are dropped to close slivers between neighbouring regions.
make_mask <- function(g) {
  old <- sf_use_s2(FALSE); on.exit(sf_use_s2(old))
  u <- st_union(st_make_valid(g))
  u <- st_union(st_sfc(lapply(st_cast(u, "POLYGON"), function(p) st_polygon(p[1])), crs = st_crs(u)))
  world <- st_as_sfc(st_bbox(c(xmin = -180, ymin = -85, xmax = 180, ymax = 85), crs = st_crs(4326)))
  suppressWarnings(st_simplify(st_difference(world, u), dTolerance = 0.008))
}
MASKS <- c(list(All = make_mask(admin_all)), lapply(split(admin_all, admin_all$Country), make_mask))

# Layers for the ggplot maps: lighter admin-1 shapes (keeps the SVG small enough to redraw
# each year), lakes drawn as water, and country outlines with slivers between areas closed.
ADMIN_GG <- local({ old <- sf_use_s2(FALSE); on.exit(sf_use_s2(old))
  suppressWarnings(st_make_valid(st_simplify(admin_sf, dTolerance = 0.025, preserveTopology = TRUE))) })
LAKES <- admin_all %>% filter(grepl("^Lake ", Admin1)) %>% st_geometry()
COUNTRY_LINES <- local({ old <- sf_use_s2(FALSE); on.exit(sf_use_s2(old))
  outline <- function(g) { u <- st_cast(st_union(st_make_valid(g)), "POLYGON")
    u <- st_sfc(lapply(u, function(p) st_polygon(p[1])), crs = st_crs(g)); u <- u[as.numeric(st_area(st_transform(u, 3857))) > 1e7]  # drop slivers under ~10 km²
    suppressWarnings(st_simplify(st_union(u), dTolerance = 0.025)) }
  parts <- split(admin_all, admin_all$Country)
  st_sf(Country = names(parts), geometry = do.call(c, lapply(parts, outline))) })
LAKES_BY <- c(list(All = LAKES), local({ old <- sf_use_s2(FALSE); on.exit(sf_use_s2(old))
  setNames(lapply(COUNTRY_LINES$Country, function(k) suppressWarnings(st_intersection(LAKES, st_geometry(COUNTRY_LINES)[COUNTRY_LINES$Country == k]))), COUNTRY_LINES$Country) }))

GAP_NAME <- "Response gap"

# The gap = how much of the malaria burden the response leaves unaddressed.
compute_gap <- function(b, r) b * (1 - r)
make_tiers <- function(g) {
  pos <- g[g > 0 & is.finite(g)]; out <- rep("Low", length(g))
  if (length(pos) >= 4) { q <- quantile(pos, c(.40, .70, .90))
    out[g >= q[1]] <- "Moderate"; out[g >= q[2]] <- "High"; out[g >= q[3]] <- "Critical"
  } else out[g >= .15] <- "Moderate"; out
}
df$Gap_Index <- compute_gap(df$Burden_Score, df$Response_Score)
df$Priority_Tier <- make_tiers(df$Gap_Index)

HAS_POP <- "Population" %in% names(df)
.lg <- df$Gap_Index[df$Year == max(all_years)]; .pos <- .lg[.lg > 0 & is.finite(.lg)]
TIER_CUTS <- if (length(.pos) >= 4) as.numeric(quantile(.pos, c(.40, .70, .90))) else c(.15, .35, .55)
apply_tiers <- function(g, q = TIER_CUTS) { out <- rep("Low", length(g)); out[g >= q[1]] <- "Moderate"; out[g >= q[2]] <- "High"; out[g >= q[3]] <- "Critical"; out }
AREA_LOOKUP <- df %>% distinct(Admin1_ID, Admin1, Country) %>% arrange(Country, Admin1)
AREA_CHOICES <- setNames(AREA_LOOKUP$Admin1_ID, paste0(AREA_LOOKUP$Admin1, " \u2014 ", AREA_LOOKUP$Country))
SIM_MAX <- nrow(df %>% filter(Year == max(all_years)))

TIER_COLORS <- c(Critical = "#B10026", High = "#E3611C", Moderate = "#D98A00", Low = "#3E8E9C")
COUNTRY_COLORS <- c(Tanzania = "#E3611C", Kenya = "#31688E", Uganda = "#35B779", Ethiopia = "#B63679",
  Eritrea = "#21918C", Rwanda = "#8C2D8C", `South Sudan` = "#B10026", Madagascar = "#443983")
METRICS <- list(
  Gap_Index = list(label = "Gap", worse = "high"), Burden_Score = list(label = "Burden", worse = "high"),
  Response_Score = list(label = "Response", worse = "low"))

# Map colours. The gap map is coloured by priority level (same colours as the tier badges),
# so map, legend and rankings tell one story. Burden and response use the ColorBrewer YlOrRd and
# YlGnBu scales common in disease-mapping work (dark = more). Both are fixed to the full data
# range, so a colour means the same value in every year and every country.
METRIC_RAMPS <- list(
  Burden_Score   = c("#FFFFCC", "#FFEDA0", "#FED976", "#FEB24C", "#FD8D3C", "#FC4E2A", "#E31A1C"),  # YlOrRd
  Response_Score = c("#FFFFD9", "#EDF8B1", "#C7E9B4", "#7FCDBB", "#41B6C4", "#1D91C0", "#225EA8"))  # YlGnBu
METRIC_DOMAINS <- lapply(names(METRIC_RAMPS), function(m) range(df[[m]], na.rm = TRUE)) %>% setNames(names(METRIC_RAMPS))
TIER_LEVELS <- c("Critical", "High", "Moderate", "Low")

pal_for <- function(metric) {
  if (metric == "Gap_Index") return(colorFactor(unname(TIER_COLORS[TIER_LEVELS]), levels = TIER_LEVELS, ordered = TRUE, na.color = "#E4E9F2"))
  colorNumeric(METRIC_RAMPS[[metric]], METRIC_DOMAINS[[metric]], na.color = "#E4E9F2")
}
map_value <- function(sfd, metric) if (metric == "Gap_Index") factor(sfd$Priority_Tier, levels = TIER_LEVELS) else sfd[[metric]]

FAVICON <- "data:image/svg+xml,<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 32 32'><circle cx='16' cy='16' r='15' fill='%23000000'/><circle cx='16' cy='16' r='7' fill='%23EBBC40'/></svg>"

empty_map <- function(msg = "Nothing to show for this selection") leaflet() %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>% setView(35, 2, 4) %>%
  addControl(html = paste0("<div style='font-family:Lato;background:#fff;border:1px solid #E5E5E5;border-radius:10px;padding:10px 14px;color:#555555;font-size:13px;box-shadow:0 2px 10px rgba(0,0,0,.06)'>", msg, "</div>"), position = "topright")

# ---------------- CSS ----------------
custom_css <- "
@import url('https://fonts.googleapis.com/css2?family=Lato:wght@300;400;700;900&display=swap');
*{box-sizing:border-box}body,.content-wrapper,.wrapper{font-family:Lato,sans-serif !important;background:#F3F3F3 !important}
.main-sidebar{background:#000000 !important;width:230px !important}
.sidebar-menu>li>a{color:#BDBDBD !important;font-size:13px !important;padding:11px 16px !important;border-radius:9px !important;margin:3px 10px !important}
.sidebar-menu>li>a:hover{background:#212121 !important;color:#fff !important}
.sidebar-menu>li.active>a{background:#EBBC40 !important;color:#000 !important;font-weight:700 !important;border-left-color:#EBBC40 !important}
.main-header .navbar,.main-header .logo{background:#000000 !important;border:none !important}
.main-header .logo{width:230px !important}.main-header .navbar{margin-left:230px !important}
.navbar-nav>li>a,.navbar .sidebar-toggle{color:#BDBDBD !important}
.content-wrapper{margin-left:230px !important}.content{padding:20px !important}
@keyframes fadeUp{from{opacity:0;transform:translateY(14px)}to{opacity:1;transform:none}}
.anim{animation:fadeUp .55s cubic-bezier(.2,.7,.2,1) both}
.hero{position:relative;overflow:hidden;background:linear-gradient(120deg,#000000 0%,#212121 55%,#2E2E2E 100%);border-radius:18px;padding:28px 32px;margin-bottom:16px;color:#fff;display:flex;justify-content:space-between;align-items:center;gap:24px;flex-wrap:wrap}
.hero::after{content:'';position:absolute;right:-60px;top:-70px;width:260px;height:260px;border-radius:50%;background:radial-gradient(circle at 30% 30%,rgba(235,188,64,.35),rgba(235,188,64,0) 70%)}
.hero .kick{color:#EBBC40;font-size:11px;font-weight:700;letter-spacing:.16em;text-transform:uppercase;margin-bottom:9px}
.hero h1{font-family:Lato,sans-serif;font-size:30px;margin:0 0 8px;line-height:1.05}
.hero .sub{color:#C7C7C7;font-size:14px;max-width:600px;line-height:1.45}
.hero-stat{position:relative;z-index:1;text-align:right;flex:none;min-width:150px}
.hero-stat .n{font-size:54px;font-weight:800;line-height:1;color:#fff}
.hero-stat .l{font-size:11px;color:#BDBDBD;text-transform:uppercase;letter-spacing:.06em;margin-top:2px}
.hero-stat .sn{font-size:14px;color:#DDDDDD;margin-top:10px}.hero-stat .sn b{color:#fff}
.kpi-row{display:flex;gap:14px;margin-bottom:16px;flex-wrap:wrap}
.kpi-card{flex:1;min-width:170px;background:#fff;border:1px solid #E5E5E5;border-radius:16px;padding:16px 18px;position:relative;overflow:hidden;box-shadow:0 2px 10px rgba(0,0,0,.04);transition:.2s}
.kpi-card:hover{transform:translateY(-2px);box-shadow:0 8px 22px rgba(0,0,0,.09)}
.kpi-card::before{content:'';position:absolute;top:0;left:0;right:0;height:3px}
.kpi-card.red::before{background:linear-gradient(90deg,#B10026,#F1605D)}.kpi-card.amber::before{background:linear-gradient(90deg,#E3611C,#D98A00)}
.kpi-card.green::before{background:linear-gradient(90deg,#B69337,#EBBC40)}.kpi-card.blue::before{background:linear-gradient(90deg,#000000,#555555)}
.kpi-icon{font-size:16px;margin-bottom:8px}.kpi-card.red .kpi-icon{color:#F1605D}.kpi-card.amber .kpi-icon{color:#E3611C}.kpi-card.green .kpi-icon{color:#B69337}.kpi-card.blue .kpi-icon{color:#000}
.kpi-value{font-size:24px;font-weight:800;color:#212121;line-height:1.05;margin-bottom:4px}
.kpi-label{font-size:11px;font-weight:600;color:#8A8A8A;text-transform:uppercase;letter-spacing:.05em}
.kpi-delta{font-size:11.5px;font-weight:700;margin-top:6px}
.dash-panel{background:#fff;border:1px solid #E5E5E5;border-radius:16px;padding:18px 20px;margin-bottom:16px;box-shadow:0 2px 10px rgba(0,0,0,.04)}
.panel-title{font-size:12px;font-weight:600;color:#8A8A8A;text-transform:uppercase;letter-spacing:.07em;margin-bottom:14px;display:flex;align-items:center;gap:8px}
.panel-title::before{content:'';width:3px;height:13px;background:#EBBC40;border-radius:2px}
.panel-title .pt-help{margin-left:auto;color:#C7C7C7;font-size:12px;cursor:help;border:1px solid #DDDDDD;border-radius:50%;width:16px;height:16px;display:inline-flex;align-items:center;justify-content:center}
.section-header h2{font-family:Lato,sans-serif;font-size:23px;color:#212121;margin:0 0 4px;font-weight:700}.section-header p{font-size:13px;color:#8A8A8A;margin:0 0 16px}
.newsflash{display:flex;align-items:center;gap:14px;background:#000000;border-radius:14px;padding:11px 18px;margin-bottom:16px;overflow:hidden}
.nf-tag{display:flex;align-items:center;gap:7px;flex:none;color:#fff;font-size:11px;font-weight:700;letter-spacing:.08em;text-transform:uppercase;background:#B10026;padding:5px 11px;border-radius:20px}
.nf-dot{width:8px;height:8px;border-radius:50%;background:#fff;animation:pulse 1.1s infinite}@keyframes pulse{0%{opacity:.3}50%{opacity:1}100%{opacity:.3}}
.ticker-wrap{flex:1;overflow:hidden;white-space:nowrap}.ticker{display:inline-block;padding-left:100%;animation:scroll 34s linear infinite;color:#E7ECF7;font-size:13.5px}
.ticker b{color:#FFD166}.ticker .sep{color:#EBBC40;margin:0 18px}@keyframes scroll{0%{transform:translateX(0)}100%{transform:translateX(-100%)}}
.spotlight{background:linear-gradient(135deg,#000000,#212121);border-radius:16px;padding:20px;color:#fff;margin-bottom:16px;box-shadow:0 2px 10px rgba(0,0,0,.06)}
.spot-kick{font-size:10px;font-weight:700;letter-spacing:.12em;text-transform:uppercase;color:#EBBC40;margin-bottom:10px}
.rd-name{font-family:Lato,sans-serif;font-size:22px;margin:0}.rd-country{font-size:12px;color:#BDBDBD;text-transform:uppercase;letter-spacing:.06em;margin-bottom:14px}
.rd-grid{display:flex;gap:10px;flex-wrap:wrap}.rd-stat{flex:1;min-width:70px;background:rgba(255,255,255,.06);border-radius:10px;padding:10px 12px}
.rd-stat .v{font-size:19px;font-weight:700}.rd-stat .l{font-size:10px;color:#BDBDBD;text-transform:uppercase}
.region-detail{background:linear-gradient(135deg,#000000,#212121);border-radius:16px;padding:20px;color:#fff;margin-bottom:16px}
.tier-badge{display:inline-block;font-size:10px;font-weight:700;padding:3px 9px;border-radius:20px;letter-spacing:.04em;text-transform:uppercase}
.tier-Critical{background:#FBE0E3;color:#B10026}.tier-High{background:#FBEADD;color:#C4501A}.tier-Moderate{background:#FAF0D6;color:#B57400}.tier-Low{background:#E1EFF1;color:#2F7C88}
.metric-toggle{display:flex;gap:7px;flex-wrap:wrap}
.metric-btn{padding:6px 13px;border-radius:20px;font-size:12px;font-weight:500;border:1.5px solid #E5E5E5;background:#fff;color:#555555;cursor:pointer}
.metric-btn:hover{border-color:#EBBC40;color:#000}.metric-btn.active{background:#EBBC40;border-color:#EBBC40;color:#000;font-weight:700}
.year-caption{font-size:13px;color:#333333;background:#FBF3DC;border-radius:10px;padding:9px 14px;margin-bottom:10px}.year-caption b{color:#212121}
.app-footer{margin-top:8px;padding:16px 4px 6px;color:#8A8A8A;font-size:11.5px;line-height:1.5;border-top:1px solid #E5E5E5}.app-footer b{color:#555555}
.tierbar{display:flex;height:36px;border-radius:10px;overflow:hidden;box-shadow:inset 0 0 0 1px #E5E5E5}
.tierseg{display:flex;align-items:center;justify-content:center;color:#fff;font-size:12.5px;font-weight:700;white-space:nowrap;overflow:hidden;animation:growseg .9s cubic-bezier(.2,.7,.2,1) both}
@keyframes growseg{from{flex-basis:0 !important}}
.tierleg{display:flex;gap:18px;flex-wrap:wrap;margin-top:13px}
.tierleg .li{display:flex;align-items:center;gap:7px;font-size:12px;color:#555555;font-weight:600}
.tierleg .dot{width:11px;height:11px;border-radius:3px}
table.dataTable{font-size:13px !important}
table.dataTable thead th{font-size:11px !important;font-weight:600 !important;text-transform:uppercase !important;color:#8A8A8A !important;background:#F7F7F7 !important}
::-webkit-scrollbar{width:5px;height:5px}::-webkit-scrollbar-thumb{background:#C7C7C7;border-radius:4px}
.irs-bar,.irs-bar-edge,.irs-single,.irs-handle>i:first-child{background:#EBBC40 !important;border-color:#EBBC40 !important;color:#000 !important}
.btn-export{background:#fff;border:1.5px solid #E5E5E5;color:#000;border-radius:20px;font-size:12px;font-weight:600;padding:6px 14px;margin-right:6px}
.btn-export:hover{background:#EBBC40;border-color:#EBBC40;color:#000}
.about-hero{position:relative;overflow:hidden;background:linear-gradient(120deg,#000000,#212121 60%,#2E2E2E);border-radius:18px;padding:28px 32px;color:#fff;margin-bottom:16px}
.about-hero::after{content:'';position:absolute;right:-50px;bottom:-80px;width:230px;height:230px;border-radius:50%;background:radial-gradient(circle at 30% 30%,rgba(235,188,64,.32),rgba(235,188,64,0) 70%)}
.about-hero .kick{color:#EBBC40;font-size:11px;font-weight:700;letter-spacing:.16em;text-transform:uppercase;margin-bottom:9px}
.about-hero h2{font-family:Lato,sans-serif;font-size:28px;margin:0 0 8px;color:#fff;position:relative;z-index:1}
.about-hero p{color:#DDDDDD;font-size:15px;max-width:720px;line-height:1.5;margin:0;position:relative;z-index:1}
.concept{display:flex;gap:14px;flex-wrap:wrap}
.concept .c{flex:1;min-width:210px;background:#fff;border:1px solid #E5E5E5;border-radius:16px;padding:20px;box-shadow:0 2px 10px rgba(0,0,0,.04);transition:.2s}
.concept .c:hover{transform:translateY(-3px);box-shadow:0 12px 26px rgba(0,0,0,.10)}
.icirc{width:46px;height:46px;border-radius:14px;display:flex;align-items:center;justify-content:center;color:#fff;font-size:19px;margin-bottom:13px}
.concept .c h4{font-size:16px;color:#212121;margin:0 0 6px;font-weight:700}
.concept .c p{font-size:13px;color:#555555;line-height:1.5;margin:0}
.flow{display:flex;align-items:center;gap:12px;flex-wrap:wrap;justify-content:center;padding:4px 0}
.flow .node{background:#F3F3F3;border:1px solid #E5E5E5;border-radius:12px;padding:12px 18px;font-weight:700;color:#212121;font-size:13.5px;text-align:center}
.flow .op{font-size:20px;color:#8A8A8A;font-weight:700}.flow .arrow{color:#EBBC40;font-size:20px;font-weight:700}
.levels{display:flex;gap:12px;flex-wrap:wrap}
.levels .lv{flex:1;min-width:160px;border-radius:14px;padding:16px 18px;color:#fff;box-shadow:0 2px 10px rgba(0,0,0,.06)}
.levels .lv h5{margin:0 0 4px;font-size:15px;font-weight:800}.levels .lv p{margin:0;font-size:12px;opacity:.93;line-height:1.4}
.glossary{display:grid;grid-template-columns:1fr 1fr;gap:2px 26px}
.glossary .g{display:flex;gap:11px;align-items:baseline;padding:10px 0;border-bottom:1px solid #F0F0F0}
.glossary .g .k{font-weight:700;color:#212121;font-size:13px;min-width:118px;flex:none}
.glossary .g .d{color:#555555;font-size:12.5px;line-height:1.4}
@media(max-width:820px){.glossary{grid-template-columns:1fr}}
.levels .lv-range{font-size:12px;font-weight:700;opacity:.92;margin:3px 0 5px;letter-spacing:.02em}
.callout{background:linear-gradient(135deg,#000000,#212121);color:#fff;border-radius:16px;padding:18px 22px;margin-bottom:16px;font-size:16px;line-height:1.5;box-shadow:0 2px 10px rgba(0,0,0,.06)}
.callout b{color:#EBBC40}
.pt-help{position:relative}
.pt-help::after{content:attr(data-tip);position:absolute;bottom:135%;right:0;transform:translateY(4px);background:#000000;color:#fff;font-size:11.5px;font-weight:500;line-height:1.45;padding:9px 12px;border-radius:9px;width:230px;text-align:left;box-shadow:0 8px 22px rgba(20,20,40,.22);opacity:0;pointer-events:none;transition:opacity .15s ease,transform .15s ease;z-index:30}
.pt-help::before{content:'';position:absolute;bottom:100%;right:4px;border:5px solid transparent;border-top-color:#000000;opacity:0;transition:opacity .15s ease;z-index:30}
.pt-help:hover::after,.pt-help:focus::after{opacity:1;transform:translateY(0)}
.pt-help:hover::before,.pt-help:focus::before{opacity:1}
.bootstrap-select>.dropdown-toggle{border:1.5px solid #E5E5E5 !important;border-radius:10px !important;background:#fff !important;color:#212121 !important;font-size:13px !important;font-weight:500 !important;box-shadow:none !important;height:38px !important}
.bootstrap-select>.dropdown-toggle:hover,.bootstrap-select>.dropdown-toggle:focus{border-color:#EBBC40 !important;outline:none !important;box-shadow:0 0 0 3px rgba(235,188,64,.12) !important}
.bootstrap-select .dropdown-toggle .filter-option{color:#212121 !important}
.bootstrap-select .dropdown-menu{border:1px solid #E5E5E5 !important;border-radius:12px !important;box-shadow:0 10px 30px rgba(20,20,40,.12) !important;padding:6px !important;margin-top:6px !important}
.bootstrap-select .dropdown-menu li a{border-radius:8px !important;font-size:13px !important;color:#333333 !important;padding:8px 12px !important}
.bootstrap-select .dropdown-menu li a:hover,.bootstrap-select .dropdown-menu>li:hover a{background:#FBF3DC !important;color:#000 !important}
.bootstrap-select .dropdown-menu li.selected a,.bootstrap-select .dropdown-menu>.active>a{background:#EBBC40 !important;color:#000 !important}
.bootstrap-select .bs-searchbox input{border:1px solid #E5E5E5 !important;border-radius:8px !important;font-size:12.5px !important;box-shadow:none !important}
.bootstrap-select .bs-searchbox input:focus{border-color:#EBBC40 !important;outline:none !important}
.dataTables_wrapper .dataTables_filter input{border:1.5px solid #E5E5E5 !important;border-radius:10px !important;padding:6px 12px !important;font-size:13px !important;margin-left:8px !important;box-shadow:none !important}
.dataTables_wrapper .dataTables_filter input:focus{border-color:#EBBC40 !important;outline:none !important;box-shadow:0 0 0 3px rgba(235,188,64,.12) !important}
.dataTables_wrapper .dataTables_paginate{padding-top:14px !important}
.dataTables_wrapper .dataTables_paginate .paginate_button{border-radius:8px !important;margin:0 2px !important;padding:5px 11px !important;color:#555555 !important;border:1px solid transparent !important}
.dataTables_wrapper .dataTables_paginate .paginate_button:hover{background:#FBF3DC !important;color:#000 !important;border-color:#FBF3DC !important}
.dataTables_wrapper .dataTables_paginate .paginate_button.current,.dataTables_wrapper .dataTables_paginate .paginate_button.current:hover{background:#EBBC40 !important;color:#000 !important;border-color:#EBBC40 !important}
.dataTables_wrapper .dataTables_paginate .paginate_button.disabled,.dataTables_wrapper .dataTables_paginate .paginate_button.disabled:hover{color:#CCCCCC !important;background:transparent !important;border-color:transparent !important;cursor:default !important}
"

spin <- function(x) withSpinner(x, type = 4, color = "#EBBC40", size = 0.7, proxy.height = "220px")

light_plotly <- function(p) p %>% layout(font = list(family = "Lato", color = "#555555", size = 12),
  paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
  xaxis = list(gridcolor = "#F0F0F0", tickfont = list(size = 11, color = "#8A8A8A"), title = ""),
  yaxis = list(gridcolor = "#F0F0F0", tickfont = list(size = 11, color = "#8A8A8A")),
  legend = list(font = list(size = 11, color = "#555555")), margin = list(t = 10, b = 40, l = 48, r = 18)) %>% config(displayModeBar = FALSE)

scope_data <- function(d, year, scope) { yr <- d %>% filter(Year == year); if (scope != "All") yr <- yr %>% filter(Country == scope); yr }

map_tips <- function(sfd, gap_label) {
  tc <- TIER_COLORS[sfd$Priority_Tier]
  paste0("<div style='font-family:Lato;min-width:180px'><div style='font-size:15px;font-weight:700;color:#212121'>", sfd$Admin1, "</div><div style='font-size:11px;color:#8A8A8A;margin-bottom:8px'>", sfd$Country, "</div><table style='font-size:12px;width:100%'>",
    "<tr><td style='color:#8A8A8A'>", gap_label, "</td><td style='text-align:right;font-weight:600'>", sprintf("%.3f", sfd$Gap_Index), "</td></tr>",
    "<tr><td style='color:#8A8A8A'>Malaria burden</td><td style='text-align:right;font-weight:600'>", sprintf("%.2f", sfd$Burden_Score), "</td></tr>",
    "<tr><td style='color:#8A8A8A'>Response</td><td style='text-align:right;font-weight:600'>", sprintf("%.2f", sfd$Response_Score), "</td></tr></table>",
    "<div style='margin-top:8px'><span style='font-size:10px;font-weight:700;padding:3px 9px;border-radius:20px;background:", tc, "22;color:", tc, "'>", sfd$Priority_Tier, "</span></div></div>")
}

# ggplot map (no web tiles): admin-1 areas with white borders, country outlines and lakes.
# Hover/click come from ggiraph; the same plot is reused for the PNG/PDF downloads.
gg_map <- function(data, year_val, metric, scope_country, gap_label = GAP_NAME) {
  yr <- scope_data(data, year_val, scope_country) %>% select(-Admin1, -Country)
  sfd <- ADMIN_GG %>% inner_join(yr, by = "Admin1_ID")
  sfd$map_value <- map_value(sfd, metric); sfd$tip <- map_tips(sfd, gap_label)
  outl <- if (scope_country == "All") COUNTRY_LINES else COUNTRY_LINES %>% filter(Country == scope_country)
  bb <- st_bbox(sfd)
  g <- ggplot() +
    geom_sf_interactive(data = sfd, aes(fill = map_value, tooltip = tip, data_id = Admin1_ID), colour = "#FFFFFF", linewidth = .2, alpha = .85, show.legend = TRUE) +
    geom_sf(data = LAKES_BY[[scope_country]], fill = "#D6E6F2", colour = NA) +
    geom_sf(data = outl, fill = NA, colour = "#4A4A4A", linewidth = .45) +
    coord_sf(xlim = bb[c(1, 3)], ylim = bb[c(2, 4)], expand = TRUE, datum = NA) +
    theme_void(base_family = "sans") +
    theme(legend.position = "inside", legend.position.inside = c(.02, .04), legend.justification = c(0, 0),
          legend.title = element_text(size = 9, face = "bold", colour = "#333333"), legend.text = element_text(size = 8, colour = "#555555"),
          plot.margin = margin(4, 4, 4, 4))
  if (metric == "Gap_Index")
    g + scale_fill_manual(values = TIER_COLORS, limits = TIER_LEVELS, drop = FALSE, na.value = "#E4E9F2", name = paste(gap_label, "(priority)"))
  else g + scale_fill_gradientn(colours = METRIC_RAMPS[[metric]], limits = METRIC_DOMAINS[[metric]], na.value = "#E4E9F2", name = METRICS[[metric]]$label,
                                guide = guide_colourbar(barheight = unit(7, "lines"), barwidth = unit(.6, "lines")))
}

girafe_map <- function(g, selected_id = NULL) {
  girafe(ggobj = g, width_svg = 7.5, height_svg = 6.6, options = list(
    opts_sizing(rescale = TRUE, width = 1),
    opts_hover(css = "stroke:#222222;stroke-width:1.4px;cursor:pointer;"),
    opts_hover_inv(css = "opacity:.55;"),
    opts_selection(type = "single", only_shiny = TRUE, selected = selected_id, css = "stroke:#000000;stroke-width:2px;"),
    opts_tooltip(css = "background:#FFFFFF;padding:10px 12px;border-radius:8px;box-shadow:0 4px 16px rgba(0,0,0,.15);", use_fill = FALSE),
    opts_zoom(min = 1, max = 4),
    opts_toolbar(saveaspng = FALSE, position = "topright")))
}

build_map <- function(data, year_val, metric, scope_country, gap_label = GAP_NAME) {
  yr <- scope_data(data, year_val, scope_country) %>% select(-Admin1, -Country)
  sfd <- admin_sf %>% inner_join(yr, by = "Admin1_ID")
  if (nrow(sfd) == 0) return(empty_map())
  sfd$map_value <- map_value(sfd, metric); pal <- pal_for(metric); bb <- as.numeric(st_bbox(sfd))
  leg_vals <- if (metric == "Gap_Index") factor(TIER_LEVELS, levels = TIER_LEVELS) else METRIC_DOMAINS[[metric]]
  leg_title <- if (metric == "Gap_Index") paste(gap_label, "(priority)") else METRICS[[metric]]$label
  labels <- lapply(map_tips(sfd, gap_label), HTML)
  # Fractional zoom lets fitBounds fill the panel instead of snapping out to the next whole zoom level.
  leaflet(sfd, options = leafletOptions(attributionControl = FALSE, minZoom = 3, maxZoom = 9, zoomSnap = 0.1, zoomDelta = 0.5)) %>% addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
    addPolygons(data = MASKS[[scope_country]], fillColor = "#FFFFFF", fillOpacity = 1, stroke = FALSE, options = pathOptions(interactive = FALSE)) %>%
    setMaxBounds(bb[1] - 4, bb[2] - 4, bb[3] + 4, bb[4] + 4) %>%
    addPolygons(layerId = ~Admin1_ID, fillColor = ~pal(map_value), fillOpacity = .7, weight = .7, color = "#FFFFFF", opacity = 1, smoothFactor = .8,
      highlightOptions = highlightOptions(weight = 2, color = "#333333", fillOpacity = .85, bringToFront = TRUE),
      label = labels, labelOptions = labelOptions(style = list(padding = "0", background = "transparent", border = "none", "box-shadow" = "none"))) %>%
    addLegend(position = "bottomright", pal = pal, values = leg_vals, title = leg_title, opacity = .7) %>%
    fitBounds(bb[1], bb[2], bb[3], bb[4], options = list(padding = c(8, 8)))
}

static_map <- function(data, year_val, metric, scope_country, gap_label) {
  ttl <- paste0(if (metric == "Gap_Index") gap_label else METRICS[[metric]]$label, " \u2014 ", if (scope_country == "All") "all areas" else scope_country, ", ", year_val)
  gg_map(data, year_val, metric, scope_country, gap_label) + labs(title = ttl) + theme(plot.title = element_text(face = "bold", size = 13))
}
top_table <- function(data, year_val, scope_country, n = 12) scope_data(data, year_val, scope_country) %>% arrange(desc(Gap_Index)) %>% head(n) %>% transmute(Area = Admin1, Country, Gap = round(Gap_Index, 3), Priority = Priority_Tier)

tier_segments <- function(tiers) { tot <- length(tiers); if (!tot) return(NULL); ord <- c("Critical", "High", "Moderate", "Low")
  cnt <- sapply(ord, function(t) sum(tiers == t))
  segs <- lapply(ord, function(t) { n <- cnt[[t]]; if (n == 0) return(NULL); pct <- 100 * n / tot
    div(class = "tierseg", style = sprintf("flex:0 0 %.3f%%;background:%s", pct, TIER_COLORS[[t]]), if (pct > 6) as.character(n) else "") })
  leg <- lapply(ord, function(t) div(class = "li", div(class = "dot", style = sprintf("background:%s", TIER_COLORS[[t]])), sprintf("%s \u00b7 %d", t, cnt[[t]])))
  tagList(div(class = "tierbar", segs), div(class = "tierleg", leg)) }


# ---- MOBILE / RESPONSIVE (Option B) ----
mobile_css <- "
@media (max-width: 820px){
  .main-header .logo{width:100% !important; float:none !important; text-align:left !important; padding-left:15px !important;}
  .main-header .navbar{margin-left:0 !important;}
  .content-wrapper{margin-left:0 !important;}
  .content{padding:12px !important;}
  .hero{padding:20px 18px !important; flex-direction:column; align-items:flex-start;}
  .hero h1{font-size:22px !important;}
  .hero .sub{font-size:12.5px !important;}
  .hero-stat{text-align:left !important; min-width:0 !important; margin-top:10px;}
  .hero-stat .n{font-size:38px !important;}
  .kpi-card{min-width:calc(50% - 8px) !important;}
  .kpi-value{font-size:20px !important;}
  .section-header h2{font-size:19px !important;}
  .metric-toggle{gap:8px;}
  .metric-btn{padding:9px 14px !important; font-size:13px !important;}
  .desktop-only{display:none !important;}
  .mobile-note{display:block !important;}
  .glossary{grid-template-columns:1fr !important;}
  .dataTables_wrapper{overflow-x:auto !important; -webkit-overflow-scrolling:touch;}
  table.dataTable{min-width:680px;}
  .concept .c{min-width:100% !important;}
  .levels .lv{min-width:calc(50% - 6px) !important;}
  .btn-export{margin-bottom:6px;}
}
.mobile-note{display:none; background:#FBF3DC; border:1px solid #EBBC40; color:#000; border-radius:10px; padding:10px 13px; font-size:13px; margin-bottom:14px; font-weight:500;}
"

# ==========================================================
# UI
# ==========================================================
ui <- dashboardPage(
  dashboardHeader(title = tags$span(style = "font-family:Lato,sans-serif;font-size:15px;color:#fff;", "Malaria Response Gap")),
  dashboardSidebar(tags$head(
      tags$title("Malaria Response Gap \u2014 Dashboard"),
      tags$link(rel = "icon", href = FAVICON),
      tags$meta(name = "viewport", content = "width=device-width, initial-scale=1, maximum-scale=1"),
      tags$style(HTML(custom_css)),
      tags$style(HTML(mobile_css))),
    sidebarMenu(id = "tab",
      menuItem("About",        tabName = "methods",  icon = icon("circle-info")),
      menuItem("Overview",     tabName = "overview", icon = icon("globe-africa")),
      menuItem("Explore",      tabName = "explorer", icon = icon("map-marked-alt")),
      menuItem("Trends",       tabName = "trends",   icon = icon("chart-line")),
      menuItem("Rankings",     tabName = "rankings", icon = icon("list-ol")))),
  dashboardBody(
   tabItems(
    tabItem("overview",
      div(class = "dash-panel", style = "display:flex;gap:20px;align-items:center;flex-wrap:wrap;padding:12px 20px",
        div(style = "min-width:240px", pickerInput("ov_country", "Show", choices = c("All countries" = "All", setNames(countries, countries)), selected = "All", width = "100%"))),
      uiOutput("hero"),
      uiOutput("kpi_strip"),
      div(class = "dash-panel anim", div(class = "panel-title", "Distribution by priority level"), uiOutput("tier_bar")),
      fluidRow(
        column(8, div(class = "dash-panel anim", uiOutput("ov_map_title"), spin(leafletOutput("overview_map", height = 620)))),
        column(4,
          uiOutput("spotlight"),
          div(class = "dash-panel anim desktop-only", uiOutput("ov_hotspot_title"), spin(plotlyOutput("hotspot_bar", height = 300)))))),
    tabItem("explorer",
      div(class = "section-header", tags$h2("Explore"), tags$p("Select a country and year, choose an indicator, and select an area to view its detail.")),
      div(class = "mobile-note", "On a phone: tap ☰ (top-left) to switch views. Pinch to zoom the map; map/data downloads are available on a computer."),
      div(class = "dash-panel", style = "display:flex;gap:20px;align-items:center;flex-wrap:wrap",
        div(style = "min-width:200px", pickerInput("exp_country", "Country", choices = c("All countries" = "All", setNames(countries, countries)), selected = "All", width = "100%")),
        div(style = "min-width:230px", pickerInput("find_area", "Find an area", choices = c("Type to search\u2026" = "", AREA_CHOICES), options = pickerOptions(liveSearch = TRUE, size = 10), width = "100%")),
        div(style = "flex:1;min-width:240px", sliderInput("exp_year", "Year", min = min(all_years), max = max(all_years), value = max(all_years), step = 1, sep = "", width = "100%", animate = animationOptions(interval = 2500))),
        div(div(class = "panel-title", style = "margin-bottom:7px", "Indicator"),
          div(class = "metric-toggle",
            tags$button(class = "metric-btn active", onclick = "setMetric('Gap_Index')", "Gap"),
            tags$button(class = "metric-btn", onclick = "setMetric('Burden_Score')", "Burden"),
            tags$button(class = "metric-btn", onclick = "setMetric('Response_Score')", "Response")))),
      tags$input(type = "hidden", id = "exp_metric", value = "Gap_Index"),
      tags$script(HTML("function setMetric(v){Shiny.setInputValue('exp_metric',v,{priority:'event'});document.querySelectorAll('.metric-btn').forEach(function(b){b.classList.remove('active')});event.target.classList.add('active');}")),
      fluidRow(
        column(8, uiOutput("year_caption"), div(class = "dash-panel", spin(girafeOutput("exp_map", height = 620)),
          div(class = "desktop-only", style = "margin-top:12px", downloadButton("dl_png", "Download map (PNG)", class = "btn-export"), downloadButton("dl_csv", "Download data (CSV)", class = "btn-export"), downloadButton("dl_pdf", "Download summary (PDF)", class = "btn-export"), bookmarkButton(label = "Copy link", icon = icon("link"), class = "btn-export")))),
        column(4, uiOutput("region_detail"),
          div(class = "dash-panel", div(class = "panel-title", "Area trend",
              tags$span(class = "pt-help", `data-tip` = "The selected area over time. The shaded red area is the gap: the part of the burden the response is not reaching. It shrinks when burden falls or response rises.", "?")),
            spin(plotlyOutput("region_trend", height = 260)),
            tags$p(style = "font-size:11.5px;color:#8A8A8A;margin:8px 0 0", "Orange: how much malaria. Blue: how much protection. Red shading: the gap between them — smaller is better."))))),
    tabItem("trends",
      div(class = "section-header", tags$h2("Trends"), tags$p("The relationship between burden and response, and how the gap has changed over time.")),
      div(class = "mobile-note", "Best viewed on a larger screen — the ‘largest changes’ chart is hidden on phones to keep this page fast."),
      fluidRow(
        column(7, div(class = "dash-panel",
          div(class = "panel-title", "Burden vs response, by area",
            tags$span(class = "pt-help", `data-tip` = "Each point is an area. Points toward the lower-right have high burden with low response.", "?")),
          sliderInput("tr_year", NULL, min = min(all_years), max = max(all_years), value = max(all_years), step = 1, sep = "", width = "60%", animate = animationOptions(interval = 2500)),
          spin(plotlyOutput("scatter", height = 420)),
          tags$p(style = "font-size:11.5px;color:#8A8A8A;margin-top:8px", "Right: higher burden. Up: stronger response. The shaded area (high burden, low response) marks the highest-priority combination; colour indicates priority level."))),
        column(5,
          div(class = "dash-panel", div(class = "panel-title", "Mean gap by country"), spin(plotlyOutput("country_traj", height = 250))),
          div(class = "dash-panel desktop-only", div(class = "panel-title", paste0("Largest changes since ", min(all_years)),
            tags$span(class = "pt-help", `data-tip` = "Areas with the largest change in gap since the first year. Red: increased. Teal: decreased.", "?")), spin(plotlyOutput("movers", height = 300)))))),
    tabItem("rankings",
      div(class = "section-header", tags$h2("Rankings"), tags$p("Areas ranked by gap, with a trend line for each since the first year.")),
      div(class = "mobile-note", "Swipe the table sideways to see all columns."),
      div(class = "dash-panel", style = "max-width:320px", sliderInput("rank_year", "Year", min = min(all_years), max = max(all_years), value = max(all_years), step = 1, sep = "", width = "100%")),
      div(class = "dash-panel", spin(DTOutput("rank_table")))),
    tabItem("methods",
      div(class = "about-hero anim",
        div(class = "kick", "About"),
        tags$h2("Malaria burden and the response gap"),
        tags$p("For administrative areas across the region, this dashboard compares the malaria burden with the strength of the response, and highlights where the gap between them is largest.")),
      div(class = "dash-panel anim",
        div(class = "panel-title", "The idea in three parts"),
        div(class = "concept",
          div(class = "c", div(class = "icirc", style = "background:#B10026", icon("fire")),
            tags$h4("Malaria burden"), tags$p("How much malaria an area has, from Malaria Atlas Project estimates.")),
          div(class = "c", div(class = "icirc", style = "background:#21918C", icon("shield-alt")),
            tags$h4("Response"), tags$p("The strength of the response, based on bed-net use and effective treatment coverage.")),
          div(class = "c", div(class = "icirc", style = "background:#E3611C", icon("exclamation-triangle")),
            tags$h4("The gap"), tags$p("The share of the burden not matched by the response \u2014 largest where burden is high and coverage is low.")))),
      div(class = "dash-panel anim",
        div(class = "panel-title", "How the gap is defined"),
        div(class = "flow",
          div(class = "node", "High burden"), div(class = "op", "+"),
          div(class = "node", "low coverage"), div(class = "arrow", "\u2192"),
          div(class = "node", style = "background:#000000;color:#fff;border:none", "large gap"),
          div(class = "arrow", "\u2192"), div(class = "node", "priority level")),
        tags$p(style = "text-align:center;color:#8A8A8A;font-size:12.5px;margin-top:14px;margin-bottom:0",
          "Where burden is low or coverage is already strong, the gap is small.")),
      div(class = "dash-panel anim",
        div(class = "panel-title", "Priority levels and gap ranges"),
        div(class = "levels",
          div(class = "lv", style = "background:#B10026", tags$h5("Critical"), tags$div(class = "lv-range", sprintf("gap \u2265 %.3f", TIER_CUTS[3])), tags$p("Greatest unmet need \u2014 act here first.")),
          div(class = "lv", style = "background:#E3611C", tags$h5("High"), tags$div(class = "lv-range", sprintf("%.3f \u2013 %.3f", TIER_CUTS[2], TIER_CUTS[3])), tags$p("Substantial shortfall in coverage.")),
          div(class = "lv", style = "background:#D98A00", tags$h5("Moderate"), tags$div(class = "lv-range", sprintf("%.3f \u2013 %.3f", TIER_CUTS[1], TIER_CUTS[2])), tags$p("A noticeable gap.")),
          div(class = "lv", style = "background:#3E8E9C", tags$h5("Low"), tags$div(class = "lv-range", sprintf("0 \u2013 %.3f", TIER_CUTS[1])), tags$p("Response broadly matches burden."))),
        tags$p(style = "color:#8A8A8A;font-size:12px;margin:14px 0 0", "Ranges are set from the spread of gap values across all areas (the highest 10% fall in Critical), so they reflect this dataset rather than fixed thresholds.")),
      div(class = "dash-panel anim",
        div(class = "panel-title", "Definitions"),
        div(class = "glossary",
          div(class = "g", div(class = "k", "Burden"), div(class = "d", "How much malaria an area has, scored 0 (lowest in the region) to 1 (highest)")),
          div(class = "g", div(class = "k", "Response"), div(class = "d", "Average of net use and treatment coverage, from 0 to 1")),
          div(class = "g", div(class = "k", "Net use"), div(class = "d", "Share of people sleeping under a treated bed net")),
          div(class = "g", div(class = "k", "Treatment"), div(class = "d", "Share of people getting effective malaria treatment")),
          div(class = "g", div(class = "k", "Gap"), div(class = "d", "How much of the burden the response isn't reaching")),
          div(class = "g", div(class = "k", "Priority"), div(class = "d", "Critical, High, Moderate or Low")))),
      div(class = "dash-panel anim", style = "display:flex;gap:13px;align-items:center",
        div(class = "icirc", style = "background:#000000;flex:none", icon("database")),
        div(tags$div(style = "font-weight:700;color:#212121;font-size:14px", "Data source"),
          tags$div(style = "color:#555555;font-size:12.5px;margin-top:2px",
            HTML(paste0("Malaria Atlas Project estimates, ", min(all_years), "\u2013", max(all_years), ", for ", length(countries), " countries and ", dplyr::n_distinct(df$Admin1_ID), " areas. Best estimates for setting priorities.")))))
    )
   ),
   div(class = "app-footer", HTML(paste0(
     "Malaria estimates from the <b>Malaria Atlas Project</b>, ", min(all_years), "\u2013", max(all_years), ", covering ", length(countries), " countries and ", dplyr::n_distinct(df$Admin1_ID), " areas. ",
     "A guide for setting priorities.")))
  )
)

# ==========================================================
# SERVER
# ==========================================================
server <- function(input, output, session) {
  gdf <- reactive(df)
  latest_r <- reactive(df %>% filter(Year == max(all_years)))
  metric_r <- reactiveVal("Gap_Index"); observeEvent(input$exp_metric, metric_r(input$exp_metric))
  scoped <- reactive(scope_data(df, input$exp_year, input$exp_country))

  delta_html <- function(d, digits = 3) {
    if (is.na(d) || d == 0) return("<span class='kpi-delta' style='color:#8A8A8A'>same as last year</span>")
    worse <- d > 0; col <- if (worse) "#B10026" else "#2A9D8F"; arr <- if (worse) "\u25B2" else "\u25BC"
    sprintf("<span class='kpi-delta' style='color:%s'>%s %s vs last year</span>", col, arr, formatC(abs(d), format = "f", digits = digits))
  }

  # ---- OVERVIEW (scoped to the "Show" country picker) ----
  ov_scope <- reactive(if (is.null(input$ov_country)) "All" else input$ov_country)
  ov_latest <- reactive(scope_data(df, max(all_years), ov_scope()))

  output$hero <- renderUI({ lat <- ov_latest(); ncr <- sum(lat$Priority_Tier %in% c("High", "Critical"))
    top <- lat %>% arrange(desc(Gap_Index)) %>% slice(1); n_areas <- dplyr::n_distinct(lat$Admin1_ID)
    div(class = "hero anim",
      div(div(class = "kick", "Malaria Atlas Project \u00b7 Dar Node"),
        tags$h1(if (ov_scope() == "All") "Subnational malaria response gap" else paste0(ov_scope(), ": malaria response gap")),
        div(class = "sub", if (ov_scope() == "All")
          paste0("Comparing malaria burden with intervention coverage across ", length(countries), " countries and ", n_areas, " administrative areas, ", min(all_years), "\u2013", max(all_years), ".")
          else paste0("Comparing malaria burden with intervention coverage across ", n_areas, " administrative areas of ", ov_scope(), ", ", min(all_years), "\u2013", max(all_years), "."))),
      div(class = "hero-stat", div(class = "n", ncr), div(class = "l", "high- and critical-priority areas"),
        div(class = "sn", HTML(paste0("Highest gap: <b>", top$Admin1, "</b>", if (ov_scope() == "All") paste0(", ", top$Country) else ""))))) })

  output$ov_map_title <- renderUI(div(class = "panel-title", paste0("Response gap by area, ", if (ov_scope() == "All") "" else paste0(ov_scope(), ", "), max(all_years))))
  output$ov_hotspot_title <- renderUI(div(class = "panel-title", paste0("Highest-gap areas, ", if (ov_scope() == "All") "" else paste0(ov_scope(), ", "), max(all_years))))

  output$kpi_strip <- renderUI({ lat <- ov_latest(); prev <- scope_data(df, max(all_years) - 1, ov_scope())
    meanNow <- mean(lat$Gap_Index, na.rm = TRUE); meanPrev <- if (nrow(prev)) mean(prev$Gap_Index, na.rm = TRUE) else NA
    critNow <- sum(lat$Priority_Tier %in% c("High", "Critical")); critPrev <- if (nrow(prev)) sum(prev$Priority_Tier %in% c("High", "Critical")) else NA
    top <- lat %>% arrange(desc(Gap_Index)) %>% slice(1)
    div(class = "kpi-row anim",
      div(class = "kpi-card red", div(class = "kpi-icon", icon("fire")), div(class = "kpi-value", top$Admin1), div(class = "kpi-label", "Highest-gap area"),
        HTML(sprintf("<span class='kpi-delta' style='color:#8A8A8A'>%s \u00b7 gap %.3f</span>", top$Country, top$Gap_Index))),
      div(class = "kpi-card amber", div(class = "kpi-icon", icon("exclamation-triangle")), div(class = "kpi-value", critNow), div(class = "kpi-label", "High & critical areas"),
        HTML(delta_html(critNow - critPrev, 0))),
      div(class = "kpi-card blue", div(class = "kpi-icon", icon("layer-group")), div(class = "kpi-value", dplyr::n_distinct(lat$Admin1_ID)), div(class = "kpi-label", "Administrative areas"),
        HTML(sprintf("<span class='kpi-delta' style='color:#8A8A8A'>%s</span>", if (ov_scope() == "All") sprintf("across %d countries", length(countries)) else paste("in", ov_scope())))),
      div(class = "kpi-card green", div(class = "kpi-icon", icon("chart-line")), div(class = "kpi-value", sprintf("%.3f", meanNow)), div(class = "kpi-label", "Mean gap"),
        HTML(delta_html(meanNow - meanPrev, 3)))) })

  output$tier_bar <- renderUI(tier_segments(ov_latest()$Priority_Tier))

  output$year_caption <- renderUI({ dy <- scoped(); if (!nrow(dy)) return(div(class = "year-caption", "No areas match the current selection."))
    top <- dy %>% arrange(desc(Gap_Index)) %>% slice(1); ncr <- sum(dy$Priority_Tier %in% c("High", "Critical"))
    div(class = "year-caption", HTML(sprintf("<b>%s.</b> %d area(s) high or critical priority. Highest gap: <b>%s</b> (%s).", input$exp_year, ncr, top$Admin1, top$Country))) })

  output$spotlight <- renderUI({ r <- ov_latest() %>% arrange(desc(Gap_Index)) %>% slice(1); if (!nrow(r)) return(NULL)
    div(class = "spotlight anim",
      div(class = "spot-kick", "Highest-gap area"),
      tags$p(class = "rd-name", r$Admin1), div(class = "rd-country", r$Country),
      div(class = "rd-grid",
        div(class = "rd-stat", div(class = "v", sprintf("%.3f", r$Gap_Index)), div(class = "l", "Gap")),
        div(class = "rd-stat", div(class = "v", sprintf("%.2f", r$Burden_Score)), div(class = "l", "Burden")),
        div(class = "rd-stat", div(class = "v", sprintf("%.2f", r$Response_Score)), div(class = "l", "Response"))),
      div(style = "margin-top:12px", tags$span(class = paste0("tier-badge tier-", r$Priority_Tier), r$Priority_Tier))) })

  output$hotspot_bar <- renderPlotly({ d <- ov_latest() %>% arrange(desc(Gap_Index)) %>% slice(1:8) %>% arrange(Gap_Index) %>%
      mutate(lab = if (ov_scope() == "All") paste0(Admin1, " \u00b7 ", Country) else Admin1, col = unname(TIER_COLORS[Priority_Tier])); d$lab <- factor(d$lab, levels = d$lab)
    plot_ly(d, x = ~Gap_Index, y = ~lab, type = "bar", orientation = "h", marker = list(color = ~col),
            text = ~sprintf("%.3f", Gap_Index), textposition = "outside", hoverinfo = "text") %>%
      light_plotly() %>% layout(xaxis = list(title = "Gap"), yaxis = list(title = "", tickfont = list(size = 10)), margin = list(l = 150)) })

  output$overview_map <- renderLeaflet(build_map(df, max(all_years), "Gap_Index", ov_scope()))
  selected <- reactiveVal(NULL)
  # On load or a new country: keep the selected area if it is in that country, otherwise jump to
  # its highest gap. Defined before the map so the first render already outlines the selection.
  observeEvent(input$exp_country, { sel <- selected(); scoped <- scope_data(df, input$exp_year, input$exp_country)
    if (is.null(sel) || !sel %in% scoped$Admin1_ID) { top <- scoped %>% arrange(desc(Gap_Index)) %>% slice(1) %>% pull(Admin1_ID)
      selected(top); session$sendCustomMessage("exp_map_set", top) } })
  # isolate(): clicking an area outlines it client-side, so the map is not redrawn on selection.
  output$exp_map <- renderGirafe(girafe_map(gg_map(df, input$exp_year, metric_r(), input$exp_country), isolate(selected())))
  observeEvent(input$exp_map_selected, { req(length(input$exp_map_selected) > 0); selected(input$exp_map_selected) })
  observeEvent(input$find_area, { req(input$find_area != "")
    row <- AREA_LOOKUP[AREA_LOOKUP$Admin1_ID == input$find_area, ]
    updatePickerInput(session, "exp_country", selected = row$Country[1]); selected(input$find_area)
    session$sendCustomMessage("exp_map_set", input$find_area) })
  output$region_detail <- renderUI({ rid <- selected(); if (is.null(rid)) return(NULL)
    r <- df %>% filter(Admin1_ID == rid, Year == input$exp_year); if (!nrow(r)) r <- df %>% filter(Admin1_ID == rid) %>% arrange(desc(Year)) %>% slice(1); if (!nrow(r)) return(NULL); r <- r[1, ]
    div(class = "region-detail", tags$p(class = "rd-name", r$Admin1), div(class = "rd-country", r$Country),
      div(class = "rd-grid",
        div(class = "rd-stat", div(class = "v", sprintf("%.3f", r$Gap_Index)), div(class = "l", "Gap")),
        div(class = "rd-stat", div(class = "v", sprintf("%.2f", r$Burden_Score)), div(class = "l", "Burden")),
        div(class = "rd-stat", div(class = "v", sprintf("%.2f", r$Response_Score)), div(class = "l", "Response"))),
      div(style = "margin-top:12px", tags$span(class = paste0("tier-badge tier-", r$Priority_Tier), r$Priority_Tier))) })
  output$region_trend <- renderPlotly({ rid <- selected(); if (is.null(rid)) return(NULL)
    d <- df %>% filter(Admin1_ID == rid) %>% arrange(Year)
    # Gap is drawn as a shaded area (the part of the burden not reached); burden and response
    # use the same hues as their maps.
    plot_ly(d, x = ~Year) %>%
      add_trace(y = ~Gap_Index, name = "Gap", type = "scatter", mode = "lines", fill = "tozeroy",
                fillcolor = "rgba(177,0,38,0.18)", line = list(color = "#B10026", width = 2), hovertemplate = "Gap %{y:.2f}<extra></extra>") %>%
      add_trace(y = ~Burden_Score, name = "Burden", type = "scatter", mode = "lines+markers", line = list(color = "#FC4E2A", width = 2.5),
                marker = list(color = "#FC4E2A", size = 6), hovertemplate = "Burden %{y:.2f}<extra></extra>") %>%
      add_trace(y = ~Response_Score, name = "Response", type = "scatter", mode = "lines+markers", line = list(color = "#1D91C0", width = 2.5),
                marker = list(color = "#1D91C0", size = 6), hovertemplate = "Response %{y:.2f}<extra></extra>") %>%
      light_plotly() %>% layout(hovermode = "x unified", yaxis = list(range = c(0, 1), title = "Score (0–1)"),
                                xaxis = list(dtick = 1), legend = list(orientation = "h", y = -.2)) })

  # ---- TRENDS ----
  output$scatter <- renderPlotly({ d <- df %>% filter(Year == input$tr_year)
    plot_ly(d, x = ~Burden_Score, y = ~Response_Score, color = ~Priority_Tier, colors = TIER_COLORS,
            type = "scatter", mode = "markers", marker = list(size = 9, opacity = .82, line = list(width = .5, color = "#fff")),
            text = ~paste0("<b>", Admin1, "</b><br>", Country, "<br>Malaria burden ", sprintf("%.2f", Burden_Score), "<br>Response ", sprintf("%.2f", Response_Score), "<br>Gap ", sprintf("%.3f", Gap_Index)), hoverinfo = "text") %>%
      light_plotly() %>%
      layout(xaxis = list(title = "More malaria \u2192", range = c(0, 1)), yaxis = list(title = "Stronger response \u2191", range = c(0, 1)),
        legend = list(orientation = "h", y = -.18),
        shapes = list(list(type = "rect", x0 = .5, x1 = 1, y0 = 0, y1 = .5, fillcolor = "rgba(177,0,38,0.06)", line = list(width = 0), layer = "below")),
        annotations = list(list(x = .78, y = .07, text = "Greatest need", showarrow = FALSE, font = list(size = 10, color = "#B10026")))) })

  output$country_traj <- renderPlotly({ d <- df %>% group_by(Country, Year) %>% summarise(g = mean(Gap_Index, na.rm = TRUE), .groups = "drop")
    plot_ly(d, x = ~Year, y = ~g, color = ~Country, colors = COUNTRY_COLORS, type = "scatter", mode = "lines+markers", line = list(width = 2.5), marker = list(size = 4),
            hovertemplate = paste0("<b>%{fullData.name}</b><br>%{x}: %{y:.3f}<extra></extra>")) %>%
      light_plotly() %>% layout(yaxis = list(title = "Mean gap"), xaxis = list(dtick = 1), legend = list(font = list(size = 9))) })

  output$movers <- renderPlotly({
    base <- df %>% filter(Year == min(all_years)) %>% select(Admin1_ID, Admin1, Country, g0 = Gap_Index)
    last <- df %>% filter(Year == max(all_years)) %>% select(Admin1_ID, gL = Gap_Index)
    m <- base %>% inner_join(last, by = "Admin1_ID") %>% mutate(chg = gL - g0) %>% arrange(desc(abs(chg))) %>% slice(1:15) %>% arrange(chg) %>%
      mutate(lab = paste0(Admin1, " (", Country, ")"), col = ifelse(chg > 0, "#B10026", "#2A9D8F"))
    m$lab <- factor(m$lab, levels = m$lab)
    plot_ly(m, x = ~chg, y = ~lab, type = "bar", orientation = "h", marker = list(color = ~col),
            hovertemplate = ~paste0(lab, ": %{x:+.3f}<extra></extra>")) %>%
      light_plotly() %>% layout(xaxis = list(title = "Change in gap"), yaxis = list(title = "", tickfont = list(size = 9)), margin = list(l = 150)) })

  # ---- RANKINGS with sparklines ----
  output$rank_table <- renderDT({
    ser <- df %>% arrange(Year) %>% group_by(Admin1_ID) %>% summarise(v = list(round(Gap_Index, 3)), .groups = "drop")
    dyr <- df %>% filter(Year == input$rank_year) %>% arrange(desc(Gap_Index)) %>% left_join(ser, by = "Admin1_ID")
    dyr$Trend <- vapply(dyr$v, function(v) spk_chr(v, type = "line", width = 74, height = 22,
      lineColor = "#EBBC40", fillColor = "#FBF3DC", lineWidth = 1.6, spotRadius = 2,
      spotColor = "#B10026", minSpotColor = "", maxSpotColor = "#B10026"), character(1))
    tbl <- dyr %>% mutate(Priority_Tier = paste0('<span class="tier-badge tier-', Priority_Tier, '">', Priority_Tier, '</span>')) %>%
      select(Country, Admin1, Gap_Index, Trend, Priority_Tier, Burden_Score, Response_Score) %>%
      rename(Area = Admin1, Gap = Gap_Index, "Trend since '15" = Trend, Priority = Priority_Tier, Burden = Burden_Score, Response = Response_Score)
    dt <- datatable(tbl, escape = FALSE, rownames = FALSE,
      options = list(pageLength = 15, dom = "ftp", order = list(list(2, "desc")),
        columnDefs = list(list(orderable = FALSE, targets = 3)),
        drawCallback = DT::JS("function(){ if (window.HTMLWidgets) HTMLWidgets.staticRender(); }"))) %>%
      formatRound(c("Gap", "Burden", "Response"), 3)
    spk_add_deps(dt)
  })

 
  # ---- SAVE / EXPORT ----
  output$dl_csv <- downloadHandler(filename = function() sprintf("malaria_gap_%s_%s.csv", input$exp_country, input$exp_year),
    content = function(file) write.csv(scoped() %>% select(-any_of(c("Incidence_Rate", "Infection_Prevalence"))) %>% arrange(desc(Gap_Index)), file, row.names = FALSE))
  output$dl_png <- downloadHandler(filename = function() sprintf("malaria_gap_map_%s_%s.png", input$exp_country, input$exp_year),
    content = function(file) ggsave(file, static_map(df, input$exp_year, metric_r(), input$exp_country, GAP_NAME), width = 9, height = 7, dpi = 150, bg = "white"))
  output$dl_pdf <- downloadHandler(filename = function() sprintf("malaria_gap_summary_%s_%s.pdf", input$exp_country, input$exp_year),
    content = function(file) { pdf(file, width = 8.5, height = 11)
      grid.arrange(static_map(df, input$exp_year, metric_r(), input$exp_country, GAP_NAME),
        tableGrob(top_table(df, input$exp_year, input$exp_country), rows = NULL), ncol = 1, heights = c(2.2, 1.4),
        top = textGrob(paste0("Malaria response gap \u2014 ", if (input$exp_country == "All") "all areas" else input$exp_country, ", ", input$exp_year), gp = gpar(fontsize = 14, fontface = "bold"))); dev.off() })
}

shinyApp(ui, server, enableBookmarking = "url")