# ==========================================================
# SUBNATIONAL MALARIA RESPONSE GAP — admin-1 dashboard
# Overview · Explorer (drill-down, bivariate, year play, exports) · Trends · Rankings · Methods
# Reads:  data/malaria_response_gap_admin1.csv  +  data/admin1_boundaries.gpkg
# Needs:  shiny shinydashboard shinyWidgets plotly leaflet sf dplyr tidyr DT scales ggplot2 gridExtra
# ==========================================================

library(shiny); library(shinydashboard); library(shinyWidgets)
library(plotly); library(leaflet); library(sf)
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
countries <- sort(unique(df$Country))

compute_gap <- function(b, r, m) if (m == "unmet") b * (1 - r) else pmax(b - r, 0)
GAP_LABEL <- c(unmet = "Unmet Need", difference = "Response Gap")
make_tiers <- function(g) {
  pos <- g[g > 0 & is.finite(g)]; out <- rep("Low", length(g))
  if (length(pos) >= 4) { q <- quantile(pos, c(.40, .70, .90))
    out[g >= q[1]] <- "Moderate"; out[g >= q[2]] <- "High"; out[g >= q[3]] <- "Critical"
  } else out[g >= .15] <- "Moderate"; out
}
recompute <- function(d, m) { d$Gap_Index <- compute_gap(d$Burden_Score, d$Response_Score, m)
  d$Priority_Tier <- make_tiers(d$Gap_Index); d }

TIER_COLORS <- c(Critical = "#C0392B", High = "#E67E22", Moderate = "#F39C12", Low = "#27AE60")
COUNTRY_COLORS <- c(Tanzania = "#E67E22", Kenya = "#2980B9", Uganda = "#27AE60", Ethiopia = "#8E44AD",
  Eritrea = "#16A085", Rwanda = "#D4A017", `South Sudan` = "#C0392B", Madagascar = "#5C7CFA")
METRICS <- list(
  Gap_Index = list(label = "Gap", worse = "high"), Burden_Score = list(label = "Burden", worse = "high"),
  Incidence_Rate = list(label = "Incidence /1k", worse = "high"), Infection_Prevalence = list(label = "Prevalence %", worse = "high"),
  Response_Score = list(label = "Response", worse = "low"), ITN_Use = list(label = "ITN Use %", worse = "low"),
  Effective_Treatment = list(label = "Treatment %", worse = "low"))
pal_for <- function(metric, values) { rng <- range(values, na.rm = TRUE)
  if (metric == "Gap_Index") rng <- c(0, max(.001, rng[2]))
  if (METRICS[[metric]]$worse == "high") colorNumeric("YlOrRd", rng, na.color = "#E4E9F2")
  else colorNumeric("YlGnBu", rng, na.color = "#E4E9F2") }

BIV <- c("11"="#cfe8df","12"="#9ad4c4","13"="#54b39a","21"="#f4d06f","22"="#dbb06b","23"="#a98f5d","31"="#d7301f","32"="#b1564b","33"="#7c5d59")
biv_bins <- function(x) cut(x, quantile(x, c(0,.34,.67,1), na.rm=TRUE), labels=1:3, include.lowest=TRUE)

# ---------------- CSS ----------------
custom_css <- "
@import url('https://fonts.googleapis.com/css2?family=DM+Sans:wght@400;500;600;700&family=DM+Serif+Display&display=swap');
*{box-sizing:border-box}body,.content-wrapper,.wrapper{font-family:'DM Sans',sans-serif !important;background:#F4F6FA !important}
.main-sidebar{background:#10162B !important;width:230px !important}
.sidebar-menu>li>a{color:#9BA8C7 !important;font-size:13px !important;padding:11px 16px !important;border-radius:9px !important;margin:3px 10px !important}
.sidebar-menu>li>a:hover{background:#1B2444 !important;color:#fff !important}
.sidebar-menu>li.active>a{background:linear-gradient(90deg,#3B5BDB,#5C7CFA) !important;color:#fff !important;font-weight:600 !important}
.main-header .navbar,.main-header .logo{background:#10162B !important;border:none !important}
.main-header .logo{width:230px !important}.main-header .navbar{margin-left:230px !important}
.navbar-nav>li>a,.navbar .sidebar-toggle{color:#9BA8C7 !important}
.content-wrapper{margin-left:230px !important}.content{padding:20px !important}
.gapdef{padding:14px 18px;margin:6px 12px 0;border-top:1px solid #243056}
.gapdef-lab{color:#6E7DA6;font-size:10px;font-weight:700;letter-spacing:.09em;text-transform:uppercase;margin-bottom:9px}
.newsflash{display:flex;align-items:center;gap:14px;background:#10162B;border-radius:14px;padding:11px 18px;margin-bottom:18px;overflow:hidden}
.nf-tag{display:flex;align-items:center;gap:7px;flex:none;color:#fff;font-size:11px;font-weight:700;letter-spacing:.08em;text-transform:uppercase;background:#C0392B;padding:5px 11px;border-radius:20px}
.nf-dot{width:8px;height:8px;border-radius:50%;background:#fff;animation:pulse 1.1s infinite}@keyframes pulse{0%{opacity:.3}50%{opacity:1}100%{opacity:.3}}
.ticker-wrap{flex:1;overflow:hidden;white-space:nowrap}.ticker{display:inline-block;padding-left:100%;animation:scroll 34s linear infinite;color:#E7ECF7;font-size:13.5px}
.ticker b{color:#FFD166}.ticker .sep{color:#3B5BDB;margin:0 18px}@keyframes scroll{0%{transform:translateX(0)}100%{transform:translateX(-100%)}}
.kpi-row{display:flex;gap:14px;margin-bottom:18px;flex-wrap:wrap}
.kpi-card{flex:1;min-width:160px;background:#fff;border:1px solid #E8ECF2;border-radius:16px;padding:18px 20px;position:relative;overflow:hidden;box-shadow:0 2px 10px rgba(0,0,0,.04);transition:.2s}
.kpi-card:hover{transform:translateY(-2px);box-shadow:0 8px 22px rgba(0,0,0,.09)}
.kpi-card::before{content:'';position:absolute;top:0;left:0;right:0;height:3px}
.kpi-card.red::before{background:linear-gradient(90deg,#C0392B,#E74C3C)}.kpi-card.amber::before{background:linear-gradient(90deg,#E67E22,#F39C12)}
.kpi-card.green::before{background:linear-gradient(90deg,#16A085,#27AE60)}.kpi-card.blue::before{background:linear-gradient(90deg,#2980B9,#5C7CFA)}
.kpi-icon{font-size:17px;margin-bottom:9px}.kpi-card.red .kpi-icon{color:#E74C3C}.kpi-card.amber .kpi-icon{color:#E67E22}.kpi-card.green .kpi-icon{color:#27AE60}.kpi-card.blue .kpi-icon{color:#5C7CFA}
.kpi-value{font-size:27px;font-weight:700;color:#1A1F36;line-height:1;margin-bottom:4px}.kpi-label{font-size:11px;font-weight:600;color:#9BA3B8;text-transform:uppercase;letter-spacing:.06em}
.insight-card{display:flex;gap:13px;align-items:flex-start;background:#fff;border:1px solid #E8ECF2;border-left:4px solid #3B5BDB;border-radius:12px;padding:14px 16px;margin-bottom:11px}
.insight-card.red{border-left-color:#C0392B}.insight-card.amber{border-left-color:#E67E22}.insight-card.green{border-left-color:#27AE60}.insight-card.blue{border-left-color:#3B5BDB}
.ic-icon{font-size:18px;margin-top:2px}.insight-card.red .ic-icon{color:#C0392B}.insight-card.amber .ic-icon{color:#E67E22}.insight-card.green .ic-icon{color:#27AE60}.insight-card.blue .ic-icon{color:#3B5BDB}
.ic-text{font-size:13.5px;color:#374151;line-height:1.45}.ic-text b{color:#1A1F36}
.dash-panel{background:#fff;border:1px solid #E8ECF2;border-radius:16px;padding:18px 20px;margin-bottom:16px;box-shadow:0 2px 10px rgba(0,0,0,.04)}
.panel-title{font-size:12px;font-weight:600;color:#9BA3B8;text-transform:uppercase;letter-spacing:.07em;margin-bottom:14px;display:flex;align-items:center;gap:8px}
.panel-title::before{content:'';width:3px;height:13px;background:#3B5BDB;border-radius:2px}
.section-header h2{font-family:'DM Serif Display',serif;font-size:23px;color:#1A1F36;margin:0 0 4px;font-weight:400}.section-header p{font-size:13px;color:#9BA3B8;margin:0 0 16px}
.tier-badge{display:inline-block;font-size:10px;font-weight:700;padding:3px 9px;border-radius:20px;letter-spacing:.04em;text-transform:uppercase}
.tier-Critical{background:#FDE8E8;color:#C0392B}.tier-High{background:#FEF0E6;color:#E67E22}.tier-Moderate{background:#FEF9E5;color:#D4A017}.tier-Low{background:#E8F8F0;color:#27AE60}
.metric-toggle{display:flex;gap:7px;flex-wrap:wrap}
.metric-btn{padding:6px 13px;border-radius:20px;font-size:12px;font-weight:500;border:1.5px solid #E8ECF2;background:#fff;color:#6B7A99;cursor:pointer}
.metric-btn:hover{border-color:#3B5BDB;color:#3B5BDB}.metric-btn.active{background:#3B5BDB;border-color:#3B5BDB;color:#fff}
.region-detail{background:linear-gradient(135deg,#10162B,#1B2444);border-radius:16px;padding:20px;color:#fff;margin-bottom:16px}
.rd-name{font-family:'DM Serif Display',serif;font-size:22px;margin:0}.rd-country{font-size:12px;color:#9BA8C7;text-transform:uppercase;letter-spacing:.06em;margin-bottom:14px}
.rd-grid{display:flex;gap:10px;flex-wrap:wrap}.rd-stat{flex:1;min-width:80px;background:rgba(255,255,255,.06);border-radius:10px;padding:10px 12px}
.rd-stat .v{font-size:19px;font-weight:700}.rd-stat .l{font-size:10px;color:#9BA8C7;text-transform:uppercase}
.year-caption{font-size:13px;color:#374151;background:#EEF2FF;border-radius:10px;padding:9px 14px;margin-bottom:10px}.year-caption b{color:#1A1F36}
table.dataTable{font-size:13px !important}
table.dataTable thead th{font-size:11px !important;font-weight:600 !important;text-transform:uppercase !important;color:#9BA3B8 !important;background:#F8FAFD !important}
::-webkit-scrollbar{width:5px;height:5px}::-webkit-scrollbar-thumb{background:#C7D0E4;border-radius:4px}
.irs-bar,.irs-bar-edge,.irs-single,.irs-handle>i:first-child{background:#3B5BDB !important;border-color:#3B5BDB !important}
.btn-export{background:#fff;border:1.5px solid #E8ECF2;color:#3B5BDB;border-radius:20px;font-size:12px;font-weight:600;padding:6px 14px;margin-right:6px}
"

light_plotly <- function(p) p %>% layout(font = list(family = "DM Sans", color = "#6B7A99", size = 12),
  paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
  xaxis = list(gridcolor = "#F1F4FA", tickfont = list(size = 11, color = "#9BA3B8"), title = ""),
  yaxis = list(gridcolor = "#F1F4FA", tickfont = list(size = 11, color = "#9BA3B8")),
  legend = list(font = list(size = 11, color = "#6B7A99")), margin = list(t = 10, b = 40, l = 48, r = 18)) %>% config(displayModeBar = FALSE)

scope_data <- function(d, year, scope) { yr <- d %>% filter(Year == year); if (scope != "All") yr <- yr %>% filter(Country == scope); yr }

build_map <- function(data, year_val, metric, scope_country, gap_label = "Gap") {
  yr <- scope_data(data, year_val, scope_country) %>% select(-Admin1, -Country)
  sfd <- admin_sf %>% inner_join(yr, by = "Admin1_ID")
  if (nrow(sfd) == 0) return(leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>% setView(35, 2, 4))
  if (metric == "Bivariate") {
    bb <- biv_bins(sfd$Burden_Score); rb <- biv_bins(sfd$Response_Score)
    cols <- unname(BIV[paste0(as.integer(bb), as.integer(rb))]); cols[is.na(cols)] <- "#E4E9F2"
    leg <- paste0("<div style='background:#fff;padding:8px;border-radius:8px;box-shadow:0 1px 6px rgba(0,0,0,.15);font-family:DM Sans'><div style='font-size:10px;color:#6B7A99;margin-bottom:4px'>Burden \u2192 / Response \u2191</div><table style='border-collapse:collapse'>",
      paste(sapply(3:1, function(r) paste0("<tr>", paste(sapply(1:3, function(b) paste0("<td style='width:16px;height:16px;background:", BIV[paste0(b,r)], "'></td>")), collapse=""), "</tr>")), collapse=""), "</table></div>")
    bbx <- as.numeric(st_bbox(sfd))
    return(leaflet(sfd, options = leafletOptions(attributionControl = FALSE)) %>% addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(layerId = ~Admin1_ID, fillColor = cols, fillOpacity = .82, weight = 1, color = "#3A4A63", opacity = .9,
        highlightOptions = highlightOptions(weight = 2.5, color = "#10162B", bringToFront = TRUE),
        label = lapply(seq_len(nrow(sfd)), function(i) HTML(paste0("<b>", sfd$Admin1[i], "</b><br>", sfd$Country[i], "<br>Burden ", sprintf("%.2f", sfd$Burden_Score[i]), " | Response ", sprintf("%.2f", sfd$Response_Score[i]))))) %>%
      addControl(html = leg, position = "bottomright") %>% fitBounds(bbx[1], bbx[2], bbx[3], bbx[4]))
  }
  vals <- sfd[[metric]]; pal <- pal_for(metric, vals); bb <- as.numeric(st_bbox(sfd))
  leg_title <- if (metric == "Gap_Index") gap_label else METRICS[[metric]]$label
  labels <- lapply(seq_len(nrow(sfd)), function(i) { r <- sfd[i, ]; tc <- TIER_COLORS[r$Priority_Tier]
    HTML(paste0("<div style='font-family:DM Sans;min-width:180px'><div style='font-size:15px;font-weight:700;color:#1A1F36'>", r$Admin1, "</div><div style='font-size:11px;color:#9BA3B8;margin-bottom:8px'>", r$Country, "</div><table style='font-size:12px;width:100%'>",
      "<tr><td style='color:#9BA3B8'>", gap_label, "</td><td style='text-align:right;font-weight:600'>", sprintf("%.3f", r$Gap_Index), "</td></tr>",
      "<tr><td style='color:#9BA3B8'>Burden</td><td style='text-align:right;font-weight:600'>", sprintf("%.3f", r$Burden_Score), "</td></tr>",
      "<tr><td style='color:#9BA3B8'>Response</td><td style='text-align:right;font-weight:600'>", sprintf("%.3f", r$Response_Score), "</td></tr>",
      "<tr><td style='color:#9BA3B8'>Incidence/1k</td><td style='text-align:right;font-weight:600'>", sprintf("%.1f", r$Incidence_Rate), "</td></tr>",
      "<tr><td style='color:#9BA3B8'>Prevalence</td><td style='text-align:right;font-weight:600'>", sprintf("%.1f%%", r$Infection_Prevalence), "</td></tr></table>",
      "<div style='margin-top:8px'><span style='font-size:10px;font-weight:700;padding:3px 9px;border-radius:20px;background:", tc, "22;color:", tc, "'>", r$Priority_Tier, "</span></div></div>")) })
  leaflet(sfd, options = leafletOptions(attributionControl = FALSE, minZoom = 3, maxZoom = 9)) %>% addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(layerId = ~Admin1_ID, fillColor = ~pal(get(metric)), fillOpacity = .80, weight = 1, color = "#3A4A63", opacity = .9, smoothFactor = .8,
      highlightOptions = highlightOptions(weight = 2.5, color = "#10162B", fillOpacity = .95, bringToFront = TRUE),
      label = labels, labelOptions = labelOptions(style = list(padding = "0", background = "transparent", border = "none", "box-shadow" = "none"))) %>%
    addLegend(position = "bottomright", pal = pal, values = vals, title = leg_title, opacity = .9) %>% fitBounds(bb[1], bb[2], bb[3], bb[4])
}

static_map <- function(data, year_val, metric, scope_country, gap_label) {
  if (metric == "Bivariate") metric <- "Gap_Index"
  yr <- scope_data(data, year_val, scope_country) %>% select(-Admin1, -Country)
  sfd <- admin_sf %>% inner_join(yr, by = "Admin1_ID")
  pal <- if (METRICS[[metric]]$worse == "high") "YlOrRd" else "YlGnBu"
  ttl <- paste0(if (metric == "Gap_Index") gap_label else METRICS[[metric]]$label, " \u2014 ", if (scope_country == "All") "all regions" else scope_country, ", ", year_val)
  ggplot(sfd) + geom_sf(aes(fill = .data[[metric]]), color = "#3A4A63", linewidth = .1) +
    scale_fill_distiller(palette = pal, direction = 1, na.value = "#E4E9F2") +
    labs(title = ttl, fill = NULL) + theme_void(base_family = "sans") +
    theme(plot.title = element_text(face = "bold", size = 13), legend.position = "right")
}
top_table <- function(data, year_val, scope_country, n = 12) scope_data(data, year_val, scope_country) %>% arrange(desc(Gap_Index)) %>% head(n) %>% transmute(Region = Admin1, Country, Gap = round(Gap_Index, 3), Tier = Priority_Tier)

# ==========================================================
# UI
# ==========================================================
ui <- dashboardPage(
  dashboardHeader(title = tags$span(style = "font-family:'DM Serif Display',serif;font-size:15px;color:#fff;", "Malaria Response Gap")),
  dashboardSidebar(tags$head(tags$style(HTML(custom_css))),
    sidebarMenu(id = "tab",
      menuItem("Overview", tabName = "overview", icon = icon("globe-africa")),
      menuItem("Explorer", tabName = "explorer", icon = icon("map-marked-alt")),
      menuItem("Trends",   tabName = "trends",   icon = icon("chart-line")),
      menuItem("Rankings", tabName = "rankings", icon = icon("list-ol")),
      menuItem("Methods",  tabName = "methods",  icon = icon("circle-info"))),
    div(class = "gapdef", div(class = "gapdef-lab", "Gap definition"),
      radioGroupButtons("gap_method", choices = c("Unmet need" = "unmet", "Difference" = "difference"),
        selected = "unmet", direction = "vertical", size = "sm", status = "primary", width = "100%"))),
  dashboardBody(tabItems(
    tabItem("overview",
      uiOutput("newsflash"),
      div(class = "section-header", tags$h2("Subnational Malaria Response Gap"), uiOutput("overview_sub")),
      div(class = "kpi-row",
        div(class = "kpi-card red", div(class = "kpi-icon", icon("fire")), div(class = "kpi-value", textOutput("kpi_top", inline = TRUE)), div(class = "kpi-label", "Highest-Gap Region")),
        div(class = "kpi-card amber", div(class = "kpi-icon", icon("exclamation-triangle")), div(class = "kpi-value", textOutput("kpi_crit", inline = TRUE)), div(class = "kpi-label", "High/Critical Regions")),
        div(class = "kpi-card blue", div(class = "kpi-icon", icon("layer-group")), div(class = "kpi-value", textOutput("kpi_units", inline = TRUE)), div(class = "kpi-label", "Admin Units")),
        div(class = "kpi-card green", div(class = "kpi-icon", icon("chart-line")), div(class = "kpi-value", textOutput("kpi_mean", inline = TRUE)), div(class = "kpi-label", "Mean Gap"))),
      fluidRow(
        column(8, div(class = "dash-panel", uiOutput("ov_map_title"), leafletOutput("overview_map", height = 460))),
        column(4, div(class = "dash-panel", div(class = "panel-title", "Live Insights"), uiOutput("insight_cards"))))),
    tabItem("explorer",
      div(class = "section-header", tags$h2("Interactive Explorer"), tags$p("Drill into a country, scrub or play the years, switch metric, click a region")),
      div(class = "dash-panel", style = "display:flex;gap:20px;align-items:center;flex-wrap:wrap",
        div(style = "min-width:200px", pickerInput("exp_country", "Country", choices = c("All countries" = "All", setNames(countries, countries)), selected = "All", width = "100%")),
        div(style = "flex:1;min-width:240px", sliderInput("exp_year", "Year", min = min(all_years), max = max(all_years), value = max(all_years), step = 1, sep = "", width = "100%", animate = animationOptions(interval = 1300))),
        div(div(class = "panel-title", style = "margin-bottom:7px", "Metric"),
          div(class = "metric-toggle",
            tags$button(class = "metric-btn active", onclick = "setMetric('Gap_Index')", "Gap"),
            tags$button(class = "metric-btn", onclick = "setMetric('Burden_Score')", "Burden"),
            tags$button(class = "metric-btn", onclick = "setMetric('Response_Score')", "Response"),
            tags$button(class = "metric-btn", onclick = "setMetric('Incidence_Rate')", "Incidence"),
            tags$button(class = "metric-btn", onclick = "setMetric('Infection_Prevalence')", "Prevalence"),
            tags$button(class = "metric-btn", onclick = "setMetric('Bivariate')", "Bivariate")))),
      tags$input(type = "hidden", id = "exp_metric", value = "Gap_Index"),
      tags$script(HTML("function setMetric(v){Shiny.setInputValue('exp_metric',v,{priority:'event'});document.querySelectorAll('.metric-btn').forEach(function(b){b.classList.remove('active')});event.target.classList.add('active');}")),
      fluidRow(
        column(8, uiOutput("year_caption"), div(class = "dash-panel", leafletOutput("exp_map", height = 500),
          div(style = "margin-top:12px", downloadButton("dl_png", "Map PNG", class = "btn-export"), downloadButton("dl_csv", "Data CSV", class = "btn-export"), downloadButton("dl_pdf", "PDF brief", class = "btn-export")))),
        column(4, uiOutput("region_detail"),
          div(class = "dash-panel", div(class = "panel-title", "Trajectory"), plotlyOutput("region_trend", height = 230))))),
    tabItem("trends",
      div(class = "section-header", tags$h2("Trends & Patterns"), tags$p("How burden and response relate, and how the gap has shifted since 2015")),
      fluidRow(
        column(7, div(class = "dash-panel",
          div(class = "panel-title", "Burden vs Response \u2014 every region"),
          sliderInput("tr_year", NULL, min = min(all_years), max = max(all_years), value = max(all_years), step = 1, sep = "", width = "60%", animate = animationOptions(interval = 1300)),
          plotlyOutput("scatter", height = 420),
          tags$p(style = "font-size:11.5px;color:#9BA3B8;margin-top:8px", "Dotted line: response equals burden. Shaded corner: high burden with low response \u2014 the priority quadrant. Points are coloured by priority tier."))),
        column(5,
          div(class = "dash-panel", div(class = "panel-title", "Mean gap by country"), plotlyOutput("country_traj", height = 250)),
          div(class = "dash-panel", div(class = "panel-title", paste0("Biggest movers ", min(all_years), "\u2013", max(all_years))), plotlyOutput("movers", height = 300))))),
    tabItem("rankings",
      div(class = "section-header", tags$h2("Regional Rankings"), tags$p("Every unit ranked by the current gap definition")),
      div(class = "dash-panel", style = "max-width:320px", sliderInput("rank_year", "Year", min = min(all_years), max = max(all_years), value = max(all_years), step = 1, sep = "", width = "100%")),
      div(class = "dash-panel", DTOutput("rank_table"))),
    tabItem("methods",
      div(class = "section-header", tags$h2("How the index is built")),
      div(class = "dash-panel", style = "font-size:14px;color:#374151;line-height:1.7",
        tags$p(HTML("<b>Burden</b> = mean of incidence (capped at 500/1,000) and prevalence (PfPR<sub>2-10</sub>), 0-1.")),
        tags$p(HTML("<b>Response</b> = mean of ITN use and effective treatment coverage.")),
        tags$p(HTML("<b>Unmet need</b> (default) = Burden &times; (1 &minus; Response). <b>Difference</b> = max(Burden &minus; Response, 0).")),
        tags$p(HTML("<b>Tiers</b> rank-based on the chosen gap (top 10% = Critical).")),
        tags$p(style = "color:#9BA3B8;font-size:12.5px", "MAP modelled surfaces aggregated to admin-1 (Uganda admin-2), P. falciparum. A relative prioritisation index, not a count of avertable cases.")))
  )))

# ==========================================================
# SERVER
# ==========================================================
server <- function(input, output, session) {
  gdf <- reactive(recompute(df, input$gap_method))
  latest_r <- reactive(gdf() %>% filter(Year == max(all_years)))
  gaplab <- reactive(GAP_LABEL[[input$gap_method]])
  metric_r <- reactiveVal("Gap_Index"); observeEvent(input$exp_metric, metric_r(input$exp_metric))
  scoped <- reactive(scope_data(gdf(), input$exp_year, input$exp_country))

  output$overview_sub <- renderUI(tags$p(paste0("Admin-1 burden vs coverage \u00b7 ", min(all_years), "\u2013", max(all_years), " \u00b7 ", length(countries), " countries \u00b7 definition: ", gaplab())))
  output$ov_map_title <- renderUI(div(class = "panel-title", paste0(gaplab(), " by Region \u2014 ", max(all_years))))
  output$kpi_top <- renderText(latest_r() %>% arrange(desc(Gap_Index)) %>% slice(1) %>% pull(Admin1))
  output$kpi_crit <- renderText(sum(latest_r()$Priority_Tier %in% c("High", "Critical")))
  output$kpi_units <- renderText(dplyr::n_distinct(df$Admin1_ID))
  output$kpi_mean <- renderText(sprintf("%.3f", mean(latest_r()$Gap_Index, na.rm = TRUE)))

  insights <- reactive({ y <- input$exp_year; lab <- gaplab(); dy <- scoped(); if (!nrow(dy)) return(list())
    top <- dy %>% arrange(desc(Gap_Index)) %>% slice(1); ncr <- sum(dy$Priority_Tier %in% c("High", "Critical")); mg <- mean(dy$Gap_Index, na.rm = TRUE)
    base <- gdf() %>% filter(Year == min(all_years)) %>% select(Admin1_ID, g0 = Gap_Index)
    mov <- dy %>% select(Admin1_ID, Admin1, Gap_Index) %>% left_join(base, by = "Admin1_ID") %>% mutate(chg = Gap_Index - g0) %>% arrange(desc(chg)) %>% slice(1)
    sl <- if (input$exp_country == "All") "the region" else input$exp_country
    list(list(cls="red",icon="fire",text=sprintf("Highest %s in %s: <b>%s</b> (%s) at <b>%.3f</b>.", tolower(lab), y, top$Admin1, top$Country, top$Gap_Index)),
      list(cls="amber",icon="exclamation-triangle",text=sprintf("<b>%d</b> region(s) High/Critical in %s.", ncr, y)),
      list(cls="blue",icon="arrow-up",text=if (!is.na(mov$chg)) sprintf("Sharpest rise since %s: <b>%s</b> (%+.3f).", min(all_years), mov$Admin1, mov$chg) else "Trend unavailable."),
      list(cls="green",icon="chart-line",text=sprintf("Mean %s across %s in %s: <b>%.3f</b>.", tolower(lab), sl, y, mg))) })
  output$newsflash <- renderUI({ items <- insights(); if (!length(items)) return(NULL)
    div(class="newsflash", div(class="nf-tag", div(class="nf-dot"), "Live"), div(class="ticker-wrap", div(class="ticker", HTML(paste(sapply(items, function(i) i$text), collapse="<span class='sep'>\u25cf</span>"))))) })
  output$insight_cards <- renderUI({ items <- insights(); if (!length(items)) return(NULL)
    tagList(lapply(items, function(i) div(class=paste("insight-card", i$cls), div(class="ic-icon", icon(i$icon)), div(class="ic-text", HTML(i$text))))) })

  output$year_caption <- renderUI({ dy <- scoped(); if (!nrow(dy)) return(NULL)
    top <- dy %>% arrange(desc(Gap_Index)) %>% slice(1); ncr <- sum(dy$Priority_Tier %in% c("High", "Critical"))
    div(class="year-caption", HTML(sprintf("<b>%s.</b> %d region(s) High/Critical; highest %s in <b>%s</b> (%s), %.3f.", input$exp_year, ncr, tolower(gaplab()), top$Admin1, top$Country, top$Gap_Index))) })

  output$overview_map <- renderLeaflet(build_map(gdf(), max(all_years), "Gap_Index", "All", gaplab()))
  output$exp_map <- renderLeaflet(build_map(gdf(), input$exp_year, metric_r(), input$exp_country, gaplab()))

  selected <- reactiveVal(NULL)
  observe(if (is.null(selected())) selected(latest_r() %>% arrange(desc(Gap_Index)) %>% slice(1) %>% pull(Admin1_ID)))
  observeEvent(input$exp_map_shape_click, selected(input$exp_map_shape_click$id))
  output$region_detail <- renderUI({ rid <- selected(); if (is.null(rid)) return(NULL)
    r <- gdf() %>% filter(Admin1_ID == rid, Year == input$exp_year); if (!nrow(r)) r <- gdf() %>% filter(Admin1_ID == rid) %>% arrange(desc(Year)) %>% slice(1); r <- r[1, ]
    div(class="region-detail", tags$p(class="rd-name", r$Admin1), div(class="rd-country", r$Country),
      div(class="rd-grid",
        div(class="rd-stat", div(class="v", sprintf("%.3f", r$Gap_Index)), div(class="l", gaplab())),
        div(class="rd-stat", div(class="v", sprintf("%.3f", r$Burden_Score)), div(class="l","Burden")),
        div(class="rd-stat", div(class="v", sprintf("%.3f", r$Response_Score)), div(class="l","Response")),
        div(class="rd-stat", div(class="v", sprintf("%.0f", r$Incidence_Rate)), div(class="l","Inc/1k"))),
      div(style="margin-top:12px", tags$span(class=paste0("tier-badge tier-", r$Priority_Tier), r$Priority_Tier))) })
  output$region_trend <- renderPlotly({ rid <- selected(); if (is.null(rid)) return(NULL)
    d <- gdf() %>% filter(Admin1_ID == rid) %>% arrange(Year)
    plot_ly(d, x=~Year) %>%
      add_trace(y=~Gap_Index, name=gaplab(), type="scatter", mode="lines+markers", line=list(color="#C0392B", width=2.5)) %>%
      add_trace(y=~Burden_Score, name="Burden", type="scatter", mode="lines+markers", line=list(color="#E67E22", width=2)) %>%
      add_trace(y=~Response_Score, name="Response", type="scatter", mode="lines+markers", line=list(color="#27AE60", width=2)) %>%
      light_plotly() %>% layout(yaxis=list(range=c(0,1)), xaxis=list(dtick=1), legend=list(orientation="h", y=-.2)) })

  # ---- TRENDS ----
  output$scatter <- renderPlotly({ d <- gdf() %>% filter(Year == input$tr_year)
    plot_ly(d, x=~Burden_Score, y=~Response_Score, color=~Priority_Tier, colors=TIER_COLORS,
            type="scatter", mode="markers", marker=list(size=9, opacity=.82, line=list(width=.5, color="#fff")),
            text=~paste0("<b>", Admin1, "</b><br>", Country, "<br>Burden ", sprintf("%.2f", Burden_Score), "<br>Response ", sprintf("%.2f", Response_Score), "<br>", gaplab(), " ", sprintf("%.3f", Gap_Index)), hoverinfo="text") %>%
      light_plotly() %>%
      layout(xaxis=list(title="Burden \u2192", range=c(0,1)), yaxis=list(title="Response \u2191", range=c(0,1)),
        legend=list(orientation="h", y=-.18),
        shapes=list(
          list(type="rect", x0=.5, x1=1, y0=0, y1=.5, fillcolor="rgba(192,57,43,0.06)", line=list(width=0), layer="below"),
          list(type="line", x0=0, x1=1, y0=0, y1=1, line=list(dash="dot", color="#9BA3B8", width=1.5))),
        annotations=list(list(x=.78, y=.07, text="Priority quadrant", showarrow=FALSE, font=list(size=10, color="#C0392B")))) })

  output$country_traj <- renderPlotly({ d <- gdf() %>% group_by(Country, Year) %>% summarise(g = mean(Gap_Index, na.rm=TRUE), .groups="drop")
    plot_ly(d, x=~Year, y=~g, color=~Country, colors=COUNTRY_COLORS, type="scatter", mode="lines+markers", line=list(width=2.5), marker=list(size=4),
            hovertemplate=paste0("<b>%{fullData.name}</b><br>%{x}: %{y:.3f}<extra></extra>")) %>%
      light_plotly() %>% layout(yaxis=list(title=paste0("Mean ", gaplab())), xaxis=list(dtick=1), legend=list(font=list(size=9))) })

  output$movers <- renderPlotly({
    base <- gdf() %>% filter(Year == min(all_years)) %>% select(Admin1_ID, Admin1, Country, g0 = Gap_Index)
    last <- gdf() %>% filter(Year == max(all_years)) %>% select(Admin1_ID, gL = Gap_Index)
    m <- base %>% inner_join(last, by = "Admin1_ID") %>% mutate(chg = gL - g0) %>% arrange(desc(abs(chg))) %>% slice(1:15) %>% arrange(chg) %>%
      mutate(lab = paste0(Admin1, " (", Country, ")"), col = ifelse(chg > 0, "#C0392B", "#27AE60"))
    m$lab <- factor(m$lab, levels = m$lab)
    plot_ly(m, x=~chg, y=~lab, type="bar", orientation="h", marker=list(color=~col),
            hovertemplate=~paste0(lab, ": %{x:+.3f}<extra></extra>")) %>%
      light_plotly() %>% layout(xaxis=list(title=paste0("Change in ", gaplab())), yaxis=list(title="", tickfont=list(size=9)), margin=list(l=150)) })

  output$rank_table <- renderDT({ tbl <- gdf() %>% filter(Year == input$rank_year) %>% arrange(desc(Gap_Index)) %>%
      mutate(Priority_Tier = paste0('<span class="tier-badge tier-', Priority_Tier, '">', Priority_Tier, '</span>')) %>%
      select(Country, Admin1, Gap_Index, Priority_Tier, Burden_Score, Response_Score, Incidence_Rate, Infection_Prevalence) %>%
      rename(Region = Admin1, Gap = Gap_Index, Priority = Priority_Tier, Burden = Burden_Score, Response = Response_Score, "Incidence/1k" = Incidence_Rate, "Prevalence %" = Infection_Prevalence)
    datatable(tbl, escape = FALSE, rownames = FALSE, options = list(pageLength = 15, dom = "ftp", order = list(list(2, "desc")))) %>%
      formatRound(c("Gap", "Burden", "Response"), 3) %>% formatRound(c("Incidence/1k", "Prevalence %"), 1) })

  # ---- EXPORTS ----
  output$dl_csv <- downloadHandler(filename = function() sprintf("mrgi_%s_%s.csv", input$exp_country, input$exp_year),
    content = function(file) write.csv(scoped() %>% arrange(desc(Gap_Index)), file, row.names = FALSE))
  output$dl_png <- downloadHandler(filename = function() sprintf("mrgi_map_%s_%s.png", input$exp_country, input$exp_year),
    content = function(file) ggsave(file, static_map(gdf(), input$exp_year, metric_r(), input$exp_country, gaplab()), width = 9, height = 7, dpi = 150, bg = "white"))
  output$dl_pdf <- downloadHandler(filename = function() sprintf("mrgi_brief_%s_%s.pdf", input$exp_country, input$exp_year),
    content = function(file) { pdf(file, width = 8.5, height = 11)
      grid.arrange(static_map(gdf(), input$exp_year, metric_r(), input$exp_country, gaplab()),
        tableGrob(top_table(gdf(), input$exp_year, input$exp_country), rows = NULL), ncol = 1, heights = c(2.2, 1.4),
        top = textGrob(paste0("Malaria Response Gap brief \u2014 ", gaplab(), ", ", if (input$exp_country == "All") "all regions" else input$exp_country, ", ", input$exp_year), gp = gpar(fontsize = 14, fontface = "bold"))); dev.off() })
}

shinyApp(ui, server)