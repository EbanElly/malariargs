# ==========================================================
# MALARIA RESPONSE GAP — subnational dashboard (plain-language edition)
# About · Overview · Explore · Trends · Rankings
# Reads:  data/malaria_response_gap_admin1.csv  +  data/admin1_boundaries.gpkg
# Needs:  shiny shinydashboard shinyWidgets shinycssloaders sparkline plotly leaflet sf
#         dplyr tidyr DT scales ggplot2 gridExtra
# ==========================================================

library(shiny); library(shinydashboard); library(shinyWidgets); library(shinycssloaders)
library(sparkline); library(plotly); library(leaflet); library(sf)
library(dplyr); library(tidyr); library(DT); library(scales)
library(ggplot2); library(gridExtra); library(grid)

# ---------------- DATA ----------------
DATA_CSV <- "data/malaria_response_gap_admin1.csv"; DATA_GPKG <- "data/admin1_boundaries.gpkg"
stopifnot(file.exists(DATA_CSV)); if (!file.exists(DATA_GPKG)) stop("Missing ", DATA_GPKG, call. = FALSE)

df <- read.csv(DATA_CSV, stringsAsFactors = FALSE) %>% filter(!grepl("^Lake ", Admin1))
df$Admin1_ID <- as.character(df$Admin1_ID)
all_years <- sort(unique(df$Year))
admin_sf <- st_read(DATA_GPKG, quiet = TRUE) %>% st_transform(4326) %>% filter(!grepl("^Lake ", Admin1))
admin_sf$Admin1_ID <- as.character(admin_sf$Admin1_ID)
admin_sf <- tryCatch(st_make_valid(st_simplify(admin_sf, dTolerance = 0.008, preserveTopology = TRUE)),
                     error = function(e) admin_sf)
countries <- sort(unique(df$Country))

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
  Incidence_Rate = list(label = "Cases /1,000", worse = "high"), Infection_Prevalence = list(label = "Infection %", worse = "high"),
  Response_Score = list(label = "Response", worse = "low"), ITN_Use = list(label = "Net use %", worse = "low"),
  Effective_Treatment = list(label = "Treatment %", worse = "low"))

# Per-indicator single-hue ramps (light = low, dark = high) so each indicator
# reads distinctly on the map while staying intuitive to its meaning.
METRIC_RAMPS <- list(
  Gap_Index            = c("#FFF7BC", "#FEC44F", "#FC4E2A", "#B10026"),  # yellow -> deep red (alarm)
  Burden_Score         = c("#FEE5D9", "#FC9272", "#DE2D26", "#A50F15"),  # reds (disease)
  Incidence_Rate       = c("#FEEDDE", "#FDBE85", "#FD8D3C", "#D94701"),  # oranges (cases)
  Infection_Prevalence = c("#F2F0F7", "#9E9AC8", "#6A51A3", "#3F007D"),  # purples (prevalence)
  Response_Score       = c("#EDF8E9", "#74C476", "#238B45", "#00441B"),  # greens (protection)
  ITN_Use              = c("#EFF3FF", "#6BAED6", "#2171B5", "#08306B"),  # blues (nets)
  Effective_Treatment  = c("#E0F3F1", "#5FBFB4", "#1F8A7E", "#0B5F57"))  # teal (treatment)

pal_for <- function(metric, values) {
  rng <- range(values, na.rm = TRUE)
  if (metric == "Gap_Index") rng <- c(0, max(.001, rng[2]))
  ramp <- METRIC_RAMPS[[metric]]
  if (is.null(ramp)) ramp <- if (METRICS[[metric]]$worse == "high") "magma" else "viridis"
  colorNumeric(ramp, rng, na.color = "#E4E9F2")
}

FAVICON <- "data:image/svg+xml,<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 32 32'><circle cx='16' cy='16' r='15' fill='%231F1A38'/><circle cx='16' cy='16' r='7' fill='%2321918C'/></svg>"

empty_map <- function(msg = "Nothing to show for this selection") leaflet() %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>% setView(35, 2, 4) %>%
  addControl(html = paste0("<div style='font-family:DM Sans;background:#fff;border:1px solid #E8ECF2;border-radius:10px;padding:10px 14px;color:#6B7A99;font-size:13px;box-shadow:0 2px 10px rgba(0,0,0,.06)'>", msg, "</div>"), position = "topright")

# ---------------- CSS ----------------
custom_css <- "
@import url('https://fonts.googleapis.com/css2?family=DM+Sans:wght@400;500;600;700;800&family=DM+Serif+Display&display=swap');
*{box-sizing:border-box}body,.content-wrapper,.wrapper{font-family:'DM Sans',sans-serif !important;background:#F4F3F8 !important}
.main-sidebar{background:#1F1A38 !important;width:230px !important}
.sidebar-menu>li>a{color:#9BA8C7 !important;font-size:13px !important;padding:11px 16px !important;border-radius:9px !important;margin:3px 10px !important}
.sidebar-menu>li>a:hover{background:#2C2552 !important;color:#fff !important}
.sidebar-menu>li.active>a{background:linear-gradient(90deg,#21918C,#2FB8B0) !important;color:#fff !important;font-weight:600 !important}
.main-header .navbar,.main-header .logo{background:#1F1A38 !important;border:none !important}
.main-header .logo{width:230px !important}.main-header .navbar{margin-left:230px !important}
.navbar-nav>li>a,.navbar .sidebar-toggle{color:#9BA8C7 !important}
.content-wrapper{margin-left:230px !important}.content{padding:20px !important}
@keyframes fadeUp{from{opacity:0;transform:translateY(14px)}to{opacity:1;transform:none}}
.anim{animation:fadeUp .55s cubic-bezier(.2,.7,.2,1) both}
.hero{position:relative;overflow:hidden;background:linear-gradient(120deg,#171331 0%,#2C2552 55%,#3A2F63 100%);border-radius:18px;padding:28px 32px;margin-bottom:16px;color:#fff;display:flex;justify-content:space-between;align-items:center;gap:24px;flex-wrap:wrap}
.hero::after{content:'';position:absolute;right:-60px;top:-70px;width:260px;height:260px;border-radius:50%;background:radial-gradient(circle at 30% 30%,rgba(47,184,176,.35),rgba(47,184,176,0) 70%)}
.hero .kick{color:#57C9BE;font-size:11px;font-weight:700;letter-spacing:.16em;text-transform:uppercase;margin-bottom:9px}
.hero h1{font-family:'DM Serif Display',serif;font-size:30px;margin:0 0 8px;line-height:1.05}
.hero .sub{color:#AEB9D6;font-size:14px;max-width:600px;line-height:1.45}
.hero-stat{position:relative;z-index:1;text-align:right;flex:none;min-width:150px}
.hero-stat .n{font-size:54px;font-weight:800;line-height:1;color:#fff}
.hero-stat .l{font-size:11px;color:#9BA8C7;text-transform:uppercase;letter-spacing:.06em;margin-top:2px}
.hero-stat .sn{font-size:14px;color:#CBD5F0;margin-top:10px}.hero-stat .sn b{color:#fff}
.kpi-row{display:flex;gap:14px;margin-bottom:16px;flex-wrap:wrap}
.kpi-card{flex:1;min-width:170px;background:#fff;border:1px solid #E8ECF2;border-radius:16px;padding:16px 18px;position:relative;overflow:hidden;box-shadow:0 2px 10px rgba(0,0,0,.04);transition:.2s}
.kpi-card:hover{transform:translateY(-2px);box-shadow:0 8px 22px rgba(0,0,0,.09)}
.kpi-card::before{content:'';position:absolute;top:0;left:0;right:0;height:3px}
.kpi-card.red::before{background:linear-gradient(90deg,#B10026,#F1605D)}.kpi-card.amber::before{background:linear-gradient(90deg,#E3611C,#D98A00)}
.kpi-card.green::before{background:linear-gradient(90deg,#1F8A7E,#2A9D8F)}.kpi-card.blue::before{background:linear-gradient(90deg,#3B3585,#2FB8B0)}
.kpi-icon{font-size:16px;margin-bottom:8px}.kpi-card.red .kpi-icon{color:#F1605D}.kpi-card.amber .kpi-icon{color:#E3611C}.kpi-card.green .kpi-icon{color:#2A9D8F}.kpi-card.blue .kpi-icon{color:#2FB8B0}
.kpi-value{font-size:24px;font-weight:800;color:#1A1F36;line-height:1.05;margin-bottom:4px}
.kpi-label{font-size:11px;font-weight:600;color:#9BA3B8;text-transform:uppercase;letter-spacing:.05em}
.kpi-delta{font-size:11.5px;font-weight:700;margin-top:6px}
.dash-panel{background:#fff;border:1px solid #E8ECF2;border-radius:16px;padding:18px 20px;margin-bottom:16px;box-shadow:0 2px 10px rgba(0,0,0,.04)}
.panel-title{font-size:12px;font-weight:600;color:#9BA3B8;text-transform:uppercase;letter-spacing:.07em;margin-bottom:14px;display:flex;align-items:center;gap:8px}
.panel-title::before{content:'';width:3px;height:13px;background:#21918C;border-radius:2px}
.panel-title .pt-help{margin-left:auto;color:#C7D0E4;font-size:12px;cursor:help;border:1px solid #E1E6F0;border-radius:50%;width:16px;height:16px;display:inline-flex;align-items:center;justify-content:center}
.section-header h2{font-family:'DM Serif Display',serif;font-size:23px;color:#1A1F36;margin:0 0 4px;font-weight:400}.section-header p{font-size:13px;color:#9BA3B8;margin:0 0 16px}
.newsflash{display:flex;align-items:center;gap:14px;background:#1F1A38;border-radius:14px;padding:11px 18px;margin-bottom:16px;overflow:hidden}
.nf-tag{display:flex;align-items:center;gap:7px;flex:none;color:#fff;font-size:11px;font-weight:700;letter-spacing:.08em;text-transform:uppercase;background:#B10026;padding:5px 11px;border-radius:20px}
.nf-dot{width:8px;height:8px;border-radius:50%;background:#fff;animation:pulse 1.1s infinite}@keyframes pulse{0%{opacity:.3}50%{opacity:1}100%{opacity:.3}}
.ticker-wrap{flex:1;overflow:hidden;white-space:nowrap}.ticker{display:inline-block;padding-left:100%;animation:scroll 34s linear infinite;color:#E7ECF7;font-size:13.5px}
.ticker b{color:#FFD166}.ticker .sep{color:#21918C;margin:0 18px}@keyframes scroll{0%{transform:translateX(0)}100%{transform:translateX(-100%)}}
.spotlight{background:linear-gradient(135deg,#1F1A38,#33285C);border-radius:16px;padding:20px;color:#fff;margin-bottom:16px;box-shadow:0 2px 10px rgba(0,0,0,.06)}
.spot-kick{font-size:10px;font-weight:700;letter-spacing:.12em;text-transform:uppercase;color:#57C9BE;margin-bottom:10px}
.rd-name{font-family:'DM Serif Display',serif;font-size:22px;margin:0}.rd-country{font-size:12px;color:#9BA8C7;text-transform:uppercase;letter-spacing:.06em;margin-bottom:14px}
.rd-grid{display:flex;gap:10px;flex-wrap:wrap}.rd-stat{flex:1;min-width:70px;background:rgba(255,255,255,.06);border-radius:10px;padding:10px 12px}
.rd-stat .v{font-size:19px;font-weight:700}.rd-stat .l{font-size:10px;color:#9BA8C7;text-transform:uppercase}
.region-detail{background:linear-gradient(135deg,#1F1A38,#2C2552);border-radius:16px;padding:20px;color:#fff;margin-bottom:16px}
.tier-badge{display:inline-block;font-size:10px;font-weight:700;padding:3px 9px;border-radius:20px;letter-spacing:.04em;text-transform:uppercase}
.tier-Critical{background:#FBE0E3;color:#B10026}.tier-High{background:#FBEADD;color:#C4501A}.tier-Moderate{background:#FAF0D6;color:#B57400}.tier-Low{background:#E1EFF1;color:#2F7C88}
.metric-toggle{display:flex;gap:7px;flex-wrap:wrap}
.metric-btn{padding:6px 13px;border-radius:20px;font-size:12px;font-weight:500;border:1.5px solid #E8ECF2;background:#fff;color:#6B7A99;cursor:pointer}
.metric-btn:hover{border-color:#21918C;color:#21918C}.metric-btn.active{background:#21918C;border-color:#21918C;color:#fff}
.year-caption{font-size:13px;color:#374151;background:#ECF5F4;border-radius:10px;padding:9px 14px;margin-bottom:10px}.year-caption b{color:#1A1F36}
.app-footer{margin-top:8px;padding:16px 4px 6px;color:#9BA3B8;font-size:11.5px;line-height:1.5;border-top:1px solid #E8ECF2}.app-footer b{color:#6B7A99}
.tierbar{display:flex;height:36px;border-radius:10px;overflow:hidden;box-shadow:inset 0 0 0 1px #E8ECF2}
.tierseg{display:flex;align-items:center;justify-content:center;color:#fff;font-size:12.5px;font-weight:700;white-space:nowrap;overflow:hidden;animation:growseg .9s cubic-bezier(.2,.7,.2,1) both}
@keyframes growseg{from{flex-basis:0 !important}}
.tierleg{display:flex;gap:18px;flex-wrap:wrap;margin-top:13px}
.tierleg .li{display:flex;align-items:center;gap:7px;font-size:12px;color:#6B7A99;font-weight:600}
.tierleg .dot{width:11px;height:11px;border-radius:3px}
table.dataTable{font-size:13px !important}
table.dataTable thead th{font-size:11px !important;font-weight:600 !important;text-transform:uppercase !important;color:#9BA3B8 !important;background:#F8FAFD !important}
::-webkit-scrollbar{width:5px;height:5px}::-webkit-scrollbar-thumb{background:#C7D0E4;border-radius:4px}
.irs-bar,.irs-bar-edge,.irs-single,.irs-handle>i:first-child{background:#21918C !important;border-color:#21918C !important}
.btn-export{background:#fff;border:1.5px solid #E8ECF2;color:#21918C;border-radius:20px;font-size:12px;font-weight:600;padding:6px 14px;margin-right:6px}
.about-hero{position:relative;overflow:hidden;background:linear-gradient(120deg,#171331,#2C2552 60%,#3A2F63);border-radius:18px;padding:28px 32px;color:#fff;margin-bottom:16px}
.about-hero::after{content:'';position:absolute;right:-50px;bottom:-80px;width:230px;height:230px;border-radius:50%;background:radial-gradient(circle at 30% 30%,rgba(47,184,176,.32),rgba(47,184,176,0) 70%)}
.about-hero .kick{color:#57C9BE;font-size:11px;font-weight:700;letter-spacing:.16em;text-transform:uppercase;margin-bottom:9px}
.about-hero h2{font-family:'DM Serif Display',serif;font-size:28px;margin:0 0 8px;color:#fff;position:relative;z-index:1}
.about-hero p{color:#CBD5F0;font-size:15px;max-width:720px;line-height:1.5;margin:0;position:relative;z-index:1}
.concept{display:flex;gap:14px;flex-wrap:wrap}
.concept .c{flex:1;min-width:210px;background:#fff;border:1px solid #E8ECF2;border-radius:16px;padding:20px;box-shadow:0 2px 10px rgba(0,0,0,.04);transition:.2s}
.concept .c:hover{transform:translateY(-3px);box-shadow:0 12px 26px rgba(0,0,0,.10)}
.icirc{width:46px;height:46px;border-radius:14px;display:flex;align-items:center;justify-content:center;color:#fff;font-size:19px;margin-bottom:13px}
.concept .c h4{font-size:16px;color:#1A1F36;margin:0 0 6px;font-weight:700}
.concept .c p{font-size:13px;color:#6B7A99;line-height:1.5;margin:0}
.flow{display:flex;align-items:center;gap:12px;flex-wrap:wrap;justify-content:center;padding:4px 0}
.flow .node{background:#F4F3F8;border:1px solid #E8ECF2;border-radius:12px;padding:12px 18px;font-weight:700;color:#1A1F36;font-size:13.5px;text-align:center}
.flow .op{font-size:20px;color:#9BA3B8;font-weight:700}.flow .arrow{color:#21918C;font-size:20px;font-weight:700}
.levels{display:flex;gap:12px;flex-wrap:wrap}
.levels .lv{flex:1;min-width:160px;border-radius:14px;padding:16px 18px;color:#fff;box-shadow:0 2px 10px rgba(0,0,0,.06)}
.levels .lv h5{margin:0 0 4px;font-size:15px;font-weight:800}.levels .lv p{margin:0;font-size:12px;opacity:.93;line-height:1.4}
.glossary{display:grid;grid-template-columns:1fr 1fr;gap:2px 26px}
.glossary .g{display:flex;gap:11px;align-items:baseline;padding:10px 0;border-bottom:1px solid #F1F4FA}
.glossary .g .k{font-weight:700;color:#1A1F36;font-size:13px;min-width:118px;flex:none}
.glossary .g .d{color:#6B7A99;font-size:12.5px;line-height:1.4}
@media(max-width:820px){.glossary{grid-template-columns:1fr}}
.levels .lv-range{font-size:12px;font-weight:700;opacity:.92;margin:3px 0 5px;letter-spacing:.02em}
.callout{background:linear-gradient(135deg,#1F1A38,#2C2552);color:#fff;border-radius:16px;padding:18px 22px;margin-bottom:16px;font-size:16px;line-height:1.5;box-shadow:0 2px 10px rgba(0,0,0,.06)}
.callout b{color:#57C9BE}
.pt-help{position:relative}
.pt-help::after{content:attr(data-tip);position:absolute;bottom:135%;right:0;transform:translateY(4px);background:#1F1A38;color:#fff;font-size:11.5px;font-weight:500;line-height:1.45;padding:9px 12px;border-radius:9px;width:230px;text-align:left;box-shadow:0 8px 22px rgba(20,20,40,.22);opacity:0;pointer-events:none;transition:opacity .15s ease,transform .15s ease;z-index:30}
.pt-help::before{content:'';position:absolute;bottom:100%;right:4px;border:5px solid transparent;border-top-color:#1F1A38;opacity:0;transition:opacity .15s ease;z-index:30}
.pt-help:hover::after,.pt-help:focus::after{opacity:1;transform:translateY(0)}
.pt-help:hover::before,.pt-help:focus::before{opacity:1}
.bootstrap-select>.dropdown-toggle{border:1.5px solid #E8ECF2 !important;border-radius:10px !important;background:#fff !important;color:#1A1F36 !important;font-size:13px !important;font-weight:500 !important;box-shadow:none !important;height:38px !important}
.bootstrap-select>.dropdown-toggle:hover,.bootstrap-select>.dropdown-toggle:focus{border-color:#21918C !important;outline:none !important;box-shadow:0 0 0 3px rgba(33,145,140,.12) !important}
.bootstrap-select .dropdown-toggle .filter-option{color:#1A1F36 !important}
.bootstrap-select .dropdown-menu{border:1px solid #E8ECF2 !important;border-radius:12px !important;box-shadow:0 10px 30px rgba(20,20,40,.12) !important;padding:6px !important;margin-top:6px !important}
.bootstrap-select .dropdown-menu li a{border-radius:8px !important;font-size:13px !important;color:#374151 !important;padding:8px 12px !important}
.bootstrap-select .dropdown-menu li a:hover,.bootstrap-select .dropdown-menu>li:hover a{background:#EDF7F6 !important;color:#21918C !important}
.bootstrap-select .dropdown-menu li.selected a,.bootstrap-select .dropdown-menu>.active>a{background:#21918C !important;color:#fff !important}
.bootstrap-select .bs-searchbox input{border:1px solid #E8ECF2 !important;border-radius:8px !important;font-size:12.5px !important;box-shadow:none !important}
.bootstrap-select .bs-searchbox input:focus{border-color:#21918C !important;outline:none !important}
.dataTables_wrapper .dataTables_filter input{border:1.5px solid #E8ECF2 !important;border-radius:10px !important;padding:6px 12px !important;font-size:13px !important;margin-left:8px !important;box-shadow:none !important}
.dataTables_wrapper .dataTables_filter input:focus{border-color:#21918C !important;outline:none !important;box-shadow:0 0 0 3px rgba(33,145,140,.12) !important}
.dataTables_wrapper .dataTables_paginate{padding-top:14px !important}
.dataTables_wrapper .dataTables_paginate .paginate_button{border-radius:8px !important;margin:0 2px !important;padding:5px 11px !important;color:#6B7A99 !important;border:1px solid transparent !important}
.dataTables_wrapper .dataTables_paginate .paginate_button:hover{background:#EDF7F6 !important;color:#21918C !important;border-color:#EDF7F6 !important}
.dataTables_wrapper .dataTables_paginate .paginate_button.current,.dataTables_wrapper .dataTables_paginate .paginate_button.current:hover{background:#21918C !important;color:#fff !important;border-color:#21918C !important}
.dataTables_wrapper .dataTables_paginate .paginate_button.disabled,.dataTables_wrapper .dataTables_paginate .paginate_button.disabled:hover{color:#D5DAE6 !important;background:transparent !important;border-color:transparent !important;cursor:default !important}
"

spin <- function(x) withSpinner(x, type = 4, color = "#21918C", size = 0.7, proxy.height = "220px")

light_plotly <- function(p) p %>% layout(font = list(family = "DM Sans", color = "#6B7A99", size = 12),
  paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
  xaxis = list(gridcolor = "#F1F4FA", tickfont = list(size = 11, color = "#9BA3B8"), title = ""),
  yaxis = list(gridcolor = "#F1F4FA", tickfont = list(size = 11, color = "#9BA3B8")),
  legend = list(font = list(size = 11, color = "#6B7A99")), margin = list(t = 10, b = 40, l = 48, r = 18)) %>% config(displayModeBar = FALSE)

scope_data <- function(d, year, scope) { yr <- d %>% filter(Year == year); if (scope != "All") yr <- yr %>% filter(Country == scope); yr }

build_map <- function(data, year_val, metric, scope_country, gap_label = GAP_NAME) {
  yr <- scope_data(data, year_val, scope_country) %>% select(-Admin1, -Country)
  sfd <- admin_sf %>% inner_join(yr, by = "Admin1_ID")
  if (nrow(sfd) == 0) return(empty_map())
  vals <- sfd[[metric]]; pal <- pal_for(metric, vals); bb <- as.numeric(st_bbox(sfd))
  leg_title <- if (metric == "Gap_Index") gap_label else METRICS[[metric]]$label
  labels <- lapply(seq_len(nrow(sfd)), function(i) { r <- sfd[i, ]; tc <- TIER_COLORS[r$Priority_Tier]
    HTML(paste0("<div style='font-family:DM Sans;min-width:180px'><div style='font-size:15px;font-weight:700;color:#1A1F36'>", r$Admin1, "</div><div style='font-size:11px;color:#9BA3B8;margin-bottom:8px'>", r$Country, "</div><table style='font-size:12px;width:100%'>",
      "<tr><td style='color:#9BA3B8'>", gap_label, "</td><td style='text-align:right;font-weight:600'>", sprintf("%.3f", r$Gap_Index), "</td></tr>",
      "<tr><td style='color:#9BA3B8'>Malaria burden</td><td style='text-align:right;font-weight:600'>", sprintf("%.2f", r$Burden_Score), "</td></tr>",
      "<tr><td style='color:#9BA3B8'>Response</td><td style='text-align:right;font-weight:600'>", sprintf("%.2f", r$Response_Score), "</td></tr>",
      "<tr><td style='color:#9BA3B8'>Cases /1,000</td><td style='text-align:right;font-weight:600'>", sprintf("%.1f", r$Incidence_Rate), "</td></tr>",
      "<tr><td style='color:#9BA3B8'>Infection rate</td><td style='text-align:right;font-weight:600'>", sprintf("%.1f%%", r$Infection_Prevalence), "</td></tr></table>",
      "<div style='margin-top:8px'><span style='font-size:10px;font-weight:700;padding:3px 9px;border-radius:20px;background:", tc, "22;color:", tc, "'>", r$Priority_Tier, "</span></div></div>")) })
  leaflet(sfd, options = leafletOptions(attributionControl = FALSE, minZoom = 3, maxZoom = 9)) %>% addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
    addPolygons(layerId = ~Admin1_ID, fillColor = ~pal(get(metric)), fillOpacity = .80, weight = 1, color = "#3A4A63", opacity = .9, smoothFactor = .8,
      highlightOptions = highlightOptions(weight = 2.5, color = "#1F1A38", fillOpacity = .95, bringToFront = TRUE),
      label = labels, labelOptions = labelOptions(style = list(padding = "0", background = "transparent", border = "none", "box-shadow" = "none"))) %>%
    addLegend(position = "bottomright", pal = pal, values = vals, title = leg_title, opacity = .9) %>% fitBounds(bb[1], bb[2], bb[3], bb[4])
}

static_map <- function(data, year_val, metric, scope_country, gap_label) {
  yr <- scope_data(data, year_val, scope_country) %>% select(-Admin1, -Country)
  sfd <- admin_sf %>% inner_join(yr, by = "Admin1_ID")
  ramp <- METRIC_RAMPS[[metric]]
  ttl <- paste0(if (metric == "Gap_Index") gap_label else METRICS[[metric]]$label, " \u2014 ", if (scope_country == "All") "all areas" else scope_country, ", ", year_val)
  g <- ggplot(sfd) + geom_sf(aes(fill = .data[[metric]]), color = "#3A4A63", linewidth = .1) +
    labs(title = ttl, fill = NULL) + theme_void(base_family = "sans") +
    theme(plot.title = element_text(face = "bold", size = 13), legend.position = "right")
  if (is.null(ramp)) g + scale_fill_viridis_c(option = if (METRICS[[metric]]$worse == "high") "magma" else "viridis", na.value = "#E4E9F2")
  else g + scale_fill_gradientn(colours = ramp, na.value = "#E4E9F2")
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
.mobile-note{display:none; background:#EEF2FF; border:1px solid #D6E0FF; color:#3B5BDB; border-radius:10px; padding:10px 13px; font-size:13px; margin-bottom:14px; font-weight:500;}
"

# ==========================================================
# UI
# ==========================================================
ui <- dashboardPage(
  dashboardHeader(title = tags$span(style = "font-family:'DM Serif Display',serif;font-size:15px;color:#fff;", "Malaria Response Gap")),
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
      uiOutput("hero"),
      uiOutput("kpi_strip"),
      div(class = "dash-panel anim", div(class = "panel-title", "Distribution by priority level"), uiOutput("tier_bar")),
      fluidRow(
        column(8, div(class = "dash-panel anim", uiOutput("ov_map_title"), spin(leafletOutput("overview_map", height = 486)))),
        column(4,
          uiOutput("spotlight"),
          div(class = "dash-panel anim desktop-only", div(class = "panel-title", paste0("Highest-gap areas, ", max(all_years))), spin(plotlyOutput("hotspot_bar", height = 300)))))),
    tabItem("explorer",
      div(class = "section-header", tags$h2("Explore"), tags$p("Select a country and year, choose an indicator, and select an area to view its detail.")),
      div(class = "mobile-note", "On a phone: tap ☰ (top-left) to switch views. Pinch to zoom the map; map/data downloads are available on a computer."),
      div(class = "dash-panel", style = "display:flex;gap:20px;align-items:center;flex-wrap:wrap",
        div(style = "min-width:200px", pickerInput("exp_country", "Country", choices = c("All countries" = "All", setNames(countries, countries)), selected = "All", width = "100%")),
        div(style = "min-width:230px", pickerInput("find_area", "Find an area", choices = c("Type to search\u2026" = "", AREA_CHOICES), options = pickerOptions(liveSearch = TRUE, size = 10), width = "100%")),
        div(style = "flex:1;min-width:240px", sliderInput("exp_year", "Year", min = min(all_years), max = max(all_years), value = max(all_years), step = 1, sep = "", width = "100%", animate = animationOptions(interval = 1300))),
        div(div(class = "panel-title", style = "margin-bottom:7px", "Indicator"),
          div(class = "metric-toggle",
            tags$button(class = "metric-btn active", onclick = "setMetric('Gap_Index')", "Gap"),
            tags$button(class = "metric-btn", onclick = "setMetric('Burden_Score')", "Burden"),
            tags$button(class = "metric-btn", onclick = "setMetric('Response_Score')", "Response"),
            tags$button(class = "metric-btn", onclick = "setMetric('Incidence_Rate')", "Case rate"),
            tags$button(class = "metric-btn", onclick = "setMetric('Infection_Prevalence')", "Infection rate")))),
      tags$input(type = "hidden", id = "exp_metric", value = "Gap_Index"),
      tags$script(HTML("function setMetric(v){Shiny.setInputValue('exp_metric',v,{priority:'event'});document.querySelectorAll('.metric-btn').forEach(function(b){b.classList.remove('active')});event.target.classList.add('active');}")),
      fluidRow(
        column(8, uiOutput("year_caption"), div(class = "dash-panel", spin(leafletOutput("exp_map", height = 500)),
          div(class = "desktop-only", style = "margin-top:12px", downloadButton("dl_png", "Download map (PNG)", class = "btn-export"), downloadButton("dl_csv", "Download data (CSV)", class = "btn-export"), downloadButton("dl_pdf", "Download summary (PDF)", class = "btn-export"), bookmarkButton(label = "Copy link", icon = icon("link"), class = "btn-export")))),
        column(4, uiOutput("region_detail"),
          div(class = "dash-panel", div(class = "panel-title", "Area trend"), spin(plotlyOutput("region_trend", height = 230)))))),
    tabItem("trends",
      div(class = "section-header", tags$h2("Trends"), tags$p("The relationship between burden and response, and how the gap has changed over time.")),
      div(class = "mobile-note", "Best viewed on a larger screen — the ‘largest changes’ chart is hidden on phones to keep this page fast."),
      fluidRow(
        column(7, div(class = "dash-panel",
          div(class = "panel-title", "Burden vs response, by area",
            tags$span(class = "pt-help", `data-tip` = "Each point is an area. Points toward the lower-right have high burden with low response.", "?")),
          sliderInput("tr_year", NULL, min = min(all_years), max = max(all_years), value = max(all_years), step = 1, sep = "", width = "60%", animate = animationOptions(interval = 1300)),
          spin(plotlyOutput("scatter", height = 420)),
          tags$p(style = "font-size:11.5px;color:#9BA3B8;margin-top:8px", "Right: higher burden. Up: stronger response. The shaded area (high burden, low response) marks the highest-priority combination; colour indicates priority level."))),
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
            tags$h4("Malaria burden"), tags$p("The amount of malaria in an area, based on case rates and infection prevalence.")),
          div(class = "c", div(class = "icirc", style = "background:#21918C", icon("shield-alt")),
            tags$h4("Response"), tags$p("The strength of the response, based on bed-net use and effective treatment coverage.")),
          div(class = "c", div(class = "icirc", style = "background:#E3611C", icon("exclamation-triangle")),
            tags$h4("The gap"), tags$p("The share of the burden not matched by the response \u2014 largest where burden is high and coverage is low.")))),
      div(class = "dash-panel anim",
        div(class = "panel-title", "How the gap is defined"),
        div(class = "flow",
          div(class = "node", "High burden"), div(class = "op", "+"),
          div(class = "node", "low coverage"), div(class = "arrow", "\u2192"),
          div(class = "node", style = "background:#1F1A38;color:#fff;border:none", "large gap"),
          div(class = "arrow", "\u2192"), div(class = "node", "priority level")),
        tags$p(style = "text-align:center;color:#9BA3B8;font-size:12.5px;margin-top:14px;margin-bottom:0",
          "Where burden is low or coverage is already strong, the gap is small.")),
      div(class = "dash-panel anim",
        div(class = "panel-title", "Priority levels and gap ranges"),
        div(class = "levels",
          div(class = "lv", style = "background:#B10026", tags$h5("Critical"), tags$div(class = "lv-range", sprintf("gap \u2265 %.3f", TIER_CUTS[3])), tags$p("Greatest unmet need \u2014 act here first.")),
          div(class = "lv", style = "background:#E3611C", tags$h5("High"), tags$div(class = "lv-range", sprintf("%.3f \u2013 %.3f", TIER_CUTS[2], TIER_CUTS[3])), tags$p("Substantial shortfall in coverage.")),
          div(class = "lv", style = "background:#D98A00", tags$h5("Moderate"), tags$div(class = "lv-range", sprintf("%.3f \u2013 %.3f", TIER_CUTS[1], TIER_CUTS[2])), tags$p("A noticeable gap.")),
          div(class = "lv", style = "background:#3E8E9C", tags$h5("Low"), tags$div(class = "lv-range", sprintf("0 \u2013 %.3f", TIER_CUTS[1])), tags$p("Response broadly matches burden."))),
        tags$p(style = "color:#9BA3B8;font-size:12px;margin:14px 0 0", "Ranges are set from the spread of gap values across all areas (the highest 10% fall in Critical), so they reflect this dataset rather than fixed thresholds.")),
      div(class = "dash-panel anim",
        div(class = "panel-title", "Definitions"),
        div(class = "glossary",
          div(class = "g", div(class = "k", "Cases /1,000"), div(class = "d", "Malaria cases per 1,000 people in a year")),
          div(class = "g", div(class = "k", "Infection rate"), div(class = "d", "Share of people carrying the malaria parasite")),
          div(class = "g", div(class = "k", "Net use"), div(class = "d", "Share of people sleeping under a treated bed net")),
          div(class = "g", div(class = "k", "Treatment"), div(class = "d", "Share of people getting effective malaria treatment")),
          div(class = "g", div(class = "k", "Gap"), div(class = "d", "How much of the burden the response isn't reaching")),
          div(class = "g", div(class = "k", "Priority"), div(class = "d", "Critical, High, Moderate or Low")))),
      div(class = "dash-panel anim", style = "display:flex;gap:13px;align-items:center",
        div(class = "icirc", style = "background:#443983;flex:none", icon("database")),
        div(tags$div(style = "font-weight:700;color:#1A1F36;font-size:14px", "Data source"),
          tags$div(style = "color:#6B7A99;font-size:12.5px;margin-top:2px",
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
    if (is.na(d) || d == 0) return("<span class='kpi-delta' style='color:#9BA3B8'>same as last year</span>")
    worse <- d > 0; col <- if (worse) "#B10026" else "#2A9D8F"; arr <- if (worse) "\u25B2" else "\u25BC"
    sprintf("<span class='kpi-delta' style='color:%s'>%s %s vs last year</span>", col, arr, formatC(abs(d), format = "f", digits = digits))
  }

  output$hero <- renderUI({ lat <- latest_r(); ncr <- sum(lat$Priority_Tier %in% c("High", "Critical"))
    top <- lat %>% arrange(desc(Gap_Index)) %>% slice(1)
    div(class = "hero anim",
      div(div(class = "kick", "Malaria Atlas Project \u00b7 Dar Node"),
        tags$h1("Subnational malaria response gap"),
        div(class = "sub", paste0("Comparing malaria burden with intervention coverage across ", length(countries), " countries and ", dplyr::n_distinct(df$Admin1_ID), " administrative areas, ", min(all_years), "\u2013", max(all_years), "."))),
      div(class = "hero-stat", div(class = "n", ncr), div(class = "l", "high- and critical-priority areas"),
        div(class = "sn", HTML(paste0("Highest gap: <b>", top$Admin1, "</b>, ", top$Country))))) })

  output$ov_map_title <- renderUI(div(class = "panel-title", paste0("Response gap by area, ", max(all_years))))

  output$kpi_strip <- renderUI({ lat <- latest_r(); prev <- df %>% filter(Year == max(all_years) - 1)
    meanNow <- mean(lat$Gap_Index, na.rm = TRUE); meanPrev <- if (nrow(prev)) mean(prev$Gap_Index, na.rm = TRUE) else NA
    critNow <- sum(lat$Priority_Tier %in% c("High", "Critical")); critPrev <- if (nrow(prev)) sum(prev$Priority_Tier %in% c("High", "Critical")) else NA
    top <- lat %>% arrange(desc(Gap_Index)) %>% slice(1)
    div(class = "kpi-row anim",
      div(class = "kpi-card red", div(class = "kpi-icon", icon("fire")), div(class = "kpi-value", top$Admin1), div(class = "kpi-label", "Highest-gap area"),
        HTML(sprintf("<span class='kpi-delta' style='color:#9BA3B8'>%s \u00b7 gap %.3f</span>", top$Country, top$Gap_Index))),
      div(class = "kpi-card amber", div(class = "kpi-icon", icon("exclamation-triangle")), div(class = "kpi-value", critNow), div(class = "kpi-label", "High & critical areas"),
        HTML(delta_html(critNow - critPrev, 0))),
      div(class = "kpi-card blue", div(class = "kpi-icon", icon("layer-group")), div(class = "kpi-value", dplyr::n_distinct(df$Admin1_ID)), div(class = "kpi-label", "Administrative areas"),
        HTML(sprintf("<span class='kpi-delta' style='color:#9BA3B8'>across %d countries</span>", length(countries)))),
      div(class = "kpi-card green", div(class = "kpi-icon", icon("chart-line")), div(class = "kpi-value", sprintf("%.3f", meanNow)), div(class = "kpi-label", "Mean gap"),
        HTML(delta_html(meanNow - meanPrev, 3)))) })

  output$tier_bar <- renderUI(tier_segments(latest_r()$Priority_Tier))

  output$year_caption <- renderUI({ dy <- scoped(); if (!nrow(dy)) return(div(class = "year-caption", "No areas match the current selection."))
    top <- dy %>% arrange(desc(Gap_Index)) %>% slice(1); ncr <- sum(dy$Priority_Tier %in% c("High", "Critical"))
    div(class = "year-caption", HTML(sprintf("<b>%s.</b> %d area(s) high or critical priority. Highest gap: <b>%s</b> (%s).", input$exp_year, ncr, top$Admin1, top$Country))) })

  output$spotlight <- renderUI({ r <- latest_r() %>% arrange(desc(Gap_Index)) %>% slice(1); if (!nrow(r)) return(NULL)
    div(class = "spotlight anim",
      div(class = "spot-kick", "Highest-gap area"),
      tags$p(class = "rd-name", r$Admin1), div(class = "rd-country", r$Country),
      div(class = "rd-grid",
        div(class = "rd-stat", div(class = "v", sprintf("%.3f", r$Gap_Index)), div(class = "l", "Gap")),
        div(class = "rd-stat", div(class = "v", sprintf("%.0f", r$Incidence_Rate)), div(class = "l", "Case rate")),
        div(class = "rd-stat", div(class = "v", sprintf("%.0f%%", r$Infection_Prevalence)), div(class = "l", "Infection"))),
      div(style = "margin-top:12px", tags$span(class = paste0("tier-badge tier-", r$Priority_Tier), r$Priority_Tier))) })

  output$hotspot_bar <- renderPlotly({ d <- latest_r() %>% arrange(desc(Gap_Index)) %>% slice(1:8) %>% arrange(Gap_Index) %>%
      mutate(lab = paste0(Admin1, " \u00b7 ", Country), col = unname(TIER_COLORS[Priority_Tier])); d$lab <- factor(d$lab, levels = d$lab)
    plot_ly(d, x = ~Gap_Index, y = ~lab, type = "bar", orientation = "h", marker = list(color = ~col),
            text = ~sprintf("%.3f", Gap_Index), textposition = "outside", hoverinfo = "text") %>%
      light_plotly() %>% layout(xaxis = list(title = "Gap"), yaxis = list(title = "", tickfont = list(size = 10)), margin = list(l = 150)) })

  output$overview_map <- renderLeaflet(build_map(df, max(all_years), "Gap_Index", "All"))
  output$exp_map <- renderLeaflet(build_map(df, input$exp_year, metric_r(), input$exp_country))

  selected <- reactiveVal(NULL)
  observe(if (is.null(selected())) selected(latest_r() %>% arrange(desc(Gap_Index)) %>% slice(1) %>% pull(Admin1_ID)))
  observeEvent(input$exp_map_shape_click, selected(input$exp_map_shape_click$id))
  observeEvent(input$find_area, { req(input$find_area != "")
    row <- AREA_LOOKUP[AREA_LOOKUP$Admin1_ID == input$find_area, ]
    updatePickerInput(session, "exp_country", selected = row$Country[1]); selected(input$find_area) })
  output$region_detail <- renderUI({ rid <- selected(); if (is.null(rid)) return(NULL)
    r <- df %>% filter(Admin1_ID == rid, Year == input$exp_year); if (!nrow(r)) r <- df %>% filter(Admin1_ID == rid) %>% arrange(desc(Year)) %>% slice(1); if (!nrow(r)) return(NULL); r <- r[1, ]
    div(class = "region-detail", tags$p(class = "rd-name", r$Admin1), div(class = "rd-country", r$Country),
      div(class = "rd-grid",
        div(class = "rd-stat", div(class = "v", sprintf("%.3f", r$Gap_Index)), div(class = "l", "Gap")),
        div(class = "rd-stat", div(class = "v", sprintf("%.2f", r$Burden_Score)), div(class = "l", "Burden")),
        div(class = "rd-stat", div(class = "v", sprintf("%.2f", r$Response_Score)), div(class = "l", "Response")),
        div(class = "rd-stat", div(class = "v", sprintf("%.0f", r$Incidence_Rate)), div(class = "l", "Cases/1k"))),
      div(style = "margin-top:12px", tags$span(class = paste0("tier-badge tier-", r$Priority_Tier), r$Priority_Tier))) })
  output$region_trend <- renderPlotly({ rid <- selected(); if (is.null(rid)) return(NULL)
    d <- df %>% filter(Admin1_ID == rid) %>% arrange(Year)
    plot_ly(d, x = ~Year) %>%
      add_trace(y = ~Gap_Index, name = "Gap", type = "scatter", mode = "lines+markers", line = list(color = "#B10026", width = 2.5)) %>%
      add_trace(y = ~Burden_Score, name = "Burden", type = "scatter", mode = "lines+markers", line = list(color = "#E3611C", width = 2)) %>%
      add_trace(y = ~Response_Score, name = "Response", type = "scatter", mode = "lines+markers", line = list(color = "#21918C", width = 2)) %>%
      light_plotly() %>% layout(yaxis = list(range = c(0, 1)), xaxis = list(dtick = 1), legend = list(orientation = "h", y = -.2)) })

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
      lineColor = "#21918C", fillColor = "#E4F3F1", lineWidth = 1.6, spotRadius = 2,
      spotColor = "#B10026", minSpotColor = "", maxSpotColor = "#B10026"), character(1))
    tbl <- dyr %>% mutate(Priority_Tier = paste0('<span class="tier-badge tier-', Priority_Tier, '">', Priority_Tier, '</span>')) %>%
      select(Country, Admin1, Gap_Index, Trend, Priority_Tier, Burden_Score, Response_Score, Incidence_Rate, Infection_Prevalence) %>%
      rename(Area = Admin1, Gap = Gap_Index, "Trend since '15" = Trend, Priority = Priority_Tier, Burden = Burden_Score, Response = Response_Score, "Cases /1,000" = Incidence_Rate, "Infection %" = Infection_Prevalence)
    dt <- datatable(tbl, escape = FALSE, rownames = FALSE,
      options = list(pageLength = 15, dom = "ftp", order = list(list(2, "desc")),
        columnDefs = list(list(orderable = FALSE, targets = 3)),
        drawCallback = DT::JS("function(){ if (window.HTMLWidgets) HTMLWidgets.staticRender(); }"))) %>%
      formatRound(c("Gap", "Burden", "Response"), 3) %>% formatRound(c("Cases /1,000", "Infection %"), 1)
    spk_add_deps(dt)
  })

 
  # ---- SAVE / EXPORT ----
  output$dl_csv <- downloadHandler(filename = function() sprintf("malaria_gap_%s_%s.csv", input$exp_country, input$exp_year),
    content = function(file) write.csv(scoped() %>% arrange(desc(Gap_Index)), file, row.names = FALSE))
  output$dl_png <- downloadHandler(filename = function() sprintf("malaria_gap_map_%s_%s.png", input$exp_country, input$exp_year),
    content = function(file) ggsave(file, static_map(df, input$exp_year, metric_r(), input$exp_country, GAP_NAME), width = 9, height = 7, dpi = 150, bg = "white"))
  output$dl_pdf <- downloadHandler(filename = function() sprintf("malaria_gap_summary_%s_%s.pdf", input$exp_country, input$exp_year),
    content = function(file) { pdf(file, width = 8.5, height = 11)
      grid.arrange(static_map(df, input$exp_year, metric_r(), input$exp_country, GAP_NAME),
        tableGrob(top_table(df, input$exp_year, input$exp_country), rows = NULL), ncol = 1, heights = c(2.2, 1.4),
        top = textGrob(paste0("Malaria response gap \u2014 ", if (input$exp_country == "All") "all areas" else input$exp_country, ", ", input$exp_year), gp = gpar(fontsize = 14, fontface = "bold"))); dev.off() })
}

shinyApp(ui, server, enableBookmarking = "url")