# app.R
# Hubbard Brook Experimental Forest: Watersheds 3 & 9

library(shiny)
library(tidyverse)
library(ggplot2)
library(readxl)
library(readr)

# ----------------------------
# Hourly file names
# ----------------------------
FILE_SNOW_W3 <- "snowdepthw3_hourly.csv"
FILE_SNOW_W9 <- "snowdepthw9_hourly.csv"

FILE_W3_TDR_EPOD    <- "epodzol_w3_soi_mois_5_min_tdr_hourly.csv"
FILE_W3_TDR_BHS     <- "Bhs_w3_soi_mois_5_min_tdr_hourly.csv"
FILE_W3_TDR_TYP     <- "typ_w3_soi_mois_5_min_tdr_hourly.csv"
FILE_W3_TERROS_EPOD <- "epodzol_w3_soi_mois_5_min_terros_hourly.csv"
FILE_W3_TERROS_BHS  <- "Bhs_w3_soi_mois_5_min_terros_hourly.csv"
FILE_W3_TERROS_TYP  <- "typ_w3_soi_mois_5_min_terros_hourly.csv"

FILE_W9_TDR_EPOD    <- "epodzol_w9_soi_mois_5_min_tdr_hourly.csv"
FILE_W9_TDR_BHS     <- "Bhs_w9_soi_mois_5_min_tdr_hourly.csv"
FILE_W9_TDR_TYP     <- "typ_w9_soi_mois_5_min_tdr_hourly.csv"
FILE_W9_TERROS_EPOD <- "epodzol_w9_soi_mois_5_min_terros_hourly.csv"
FILE_W9_TERROS_BHS  <- "Bhs_w9_soi_mois_5_min_terros_hourly.csv"
FILE_W9_TERROS_TYP  <- "typ_w9_soi_mois_5_min_terros_hourly.csv"

FILE_WELL_159 <- "well_159_record_hourly.csv"
FILE_WELL_176 <- "well_176_record_hourly.csv"
FILE_WELL_179 <- "well_179_record_hourly.csv"
FILE_WELL_42  <- "well_42_4_d1_hourly.csv"
FILE_WELL_N1  <- "well_N1_record_hourly.csv"
FILE_WELL_N5  <- "well_N5_record_hourly.csv"

FILE_W3_PRECIP <- "HBEF_W3precipitation_15min_hourly.csv"
FILE_W9_PRECIP <- "HBEF_W9precipitation_15min_hourly.csv"

FILE_W3_STREAM <- "w3_stmflow_1957_2012_hourly.csv"
FILE_W9_STREAM <- "w9_stmflow_1995_2012_hourly.csv"

# ----------------------------
# Soil file mapping
# ----------------------------
soil_file_map <- list(
  `3` = list(
    tdr = c(
      epod = FILE_W3_TDR_EPOD,
      bhs  = FILE_W3_TDR_BHS,
      typ  = FILE_W3_TDR_TYP
    ),
    terros = c(
      epod = FILE_W3_TERROS_EPOD,
      bhs  = FILE_W3_TERROS_BHS,
      typ  = FILE_W3_TERROS_TYP
    )
  ),
  `9` = list(
    tdr = c(
      epod = FILE_W9_TDR_EPOD,
      bhs  = FILE_W9_TDR_BHS,
      typ  = FILE_W9_TDR_TYP
    ),
    terros = c(
      epod = FILE_W9_TERROS_EPOD,
      bhs  = FILE_W9_TERROS_BHS,
      typ  = FILE_W9_TERROS_TYP
    )
  )
)

# ----------------------------
# Well mapping
# Watershed 3: 42_4_d1, N1, N5
# Watershed 9: 159, 176, 179
# ----------------------------
wells_by_ws <- list(
  `3` = tibble(
    well_id = c("42_4_d1", "N1", "N5"),
    file    = c(FILE_WELL_42, FILE_WELL_N1, FILE_WELL_N5)
  ),
  `9` = tibble(
    well_id = c("159", "176", "179"),
    file    = c(FILE_WELL_159, FILE_WELL_176, FILE_WELL_179)
  )
)

# ----------------------------
# Plot styling
# ----------------------------
plot_theme_blue <- function() {
  theme_classic() +
    theme(
      plot.title = element_text(face = "bold", size = 14, color = "#163A5F"),
      axis.title = element_text(size = 11, color = "#163A5F"),
      axis.text = element_text(size = 10, color = "#24476B"),
      axis.line = element_line(color = "#4F6D8A", linewidth = 0.5),
      axis.ticks = element_line(color = "#4F6D8A", linewidth = 0.4),
      panel.background = element_rect(fill = "#F4F8FC", color = NA),
      plot.background = element_rect(fill = "#F4F8FC", color = NA),
      legend.background = element_rect(fill = "#F4F8FC", color = NA),
      legend.key = element_rect(fill = "#F4F8FC", color = NA),
      legend.text = element_text(color = "#24476B"),
      legend.title = element_text(color = "#163A5F", face = "bold"),
      strip.background = element_rect(fill = "#DCEAF7", color = "#A9C4DD"),
      strip.text = element_text(color = "#163A5F", face = "bold")
    )
}

depth_colors <- c(
  "10" = "#1D4E89",
  "30" = "#2C7FB8",
  "50" = "#41B6C4"
)

well_colors_w3 <- c(
  "42_4_d1" = "#1D4E89",
  "N1"      = "#2C7FB8",
  "N5"      = "#41B6C4"
)

well_colors_w9 <- c(
  "159" = "#1D4E89",
  "176" = "#2C7FB8",
  "179" = "#41B6C4"
)

# ----------------------------
# Helpers
# ----------------------------
as_posix <- function(x) {
  if (inherits(x, "POSIXct")) return(x)
  suppressWarnings(as.POSIXct(x, tz = "UTC"))
}

read_soil_for_selection <- function(watershed, soil_key) {
  ws <- as.character(watershed)
  
  tdr <- read_csv(soil_file_map[[ws]]$tdr[[soil_key]], show_col_types = FALSE) %>%
    mutate(
      TIMESTAMP = as_posix(hour),
      sensor = "TDR"
    ) %>%
    select(-hour)
  
  ter <- read_csv(soil_file_map[[ws]]$terros[[soil_key]], show_col_types = FALSE) %>%
    mutate(
      TIMESTAMP = as_posix(hour),
      sensor = "Terros"
    ) %>%
    select(-hour) %>%
    mutate(across(where(is.numeric), ~ ifelse(. < 0, NA, .)))
  
  bind_rows(tdr, ter)
}

soil_long <- function(df) {
  df %>%
    select(TIMESTAMP, sensor, where(is.numeric)) %>%
    pivot_longer(
      cols = -c(TIMESTAMP, sensor),
      names_to = "series",
      values_to = "value"
    ) %>%
    filter(!is.na(TIMESTAMP), !is.na(value)) %>%
    mutate(
      depth_cm = case_when(
        str_detect(series, "10") ~ 10,
        str_detect(series, "30") ~ 30,
        str_detect(series, "50") ~ 50,
        TRUE ~ NA_real_
      )
    ) %>%
    filter(!is.na(depth_cm))
}

read_wells_for_ws <- function(watershed) {
  ws_tbl <- wells_by_ws[[as.character(watershed)]]
  
  purrr::map2_dfr(ws_tbl$well_id, ws_tbl$file, \(id, fp) {
    read_csv(fp, show_col_types = FALSE) %>%
      transmute(
        datetime = as_posix(hour),
        wt_cm = `water table depth (cm)`,
        well_id = id
      ) %>%
      filter(!is.na(datetime), !is.na(wt_cm))
  })
}

# ----------------------------
# Load hourly datasets once
# ----------------------------
snow_all <- bind_rows(
  read_csv(FILE_SNOW_W3, show_col_types = FALSE) %>%
    transmute(
      watershed = 3L,
      date = as_posix(hour),
      snow_cm = hourly_snow_depth
    ),
  read_csv(FILE_SNOW_W9, show_col_types = FALSE) %>%
    transmute(
      watershed = 9L,
      date = as_posix(hour),
      snow_cm = hourly_snow_depth
    )
)

precip_all <- bind_rows(
  read_csv(FILE_W3_PRECIP, show_col_types = FALSE) %>%
    transmute(
      watershed = 3L,
      DateTime = as_posix(hour),
      precip = hourly_precip
    ),
  read_csv(FILE_W9_PRECIP, show_col_types = FALSE) %>%
    transmute(
      watershed = 9L,
      DateTime = as_posix(hour),
      precip = hourly_precip
    )
)

stream_all <- bind_rows(
  read_csv(FILE_W3_STREAM, show_col_types = FALSE) %>%
    transmute(
      watershed = 3L,
      DateTime = as_posix(hour),
      Gage_ft,
      Discharge_cfs,
      Discharge_ls
    ),
  read_csv(FILE_W9_STREAM, show_col_types = FALSE) %>%
    transmute(
      watershed = 9L,
      DateTime = as_posix(hour),
      Gage_ft,
      Discharge_cfs,
      Discharge_ls
    )
)

all_wells_for_range <- bind_rows(
  read_wells_for_ws(3),
  read_wells_for_ws(9)
)

date_min <- min(
  c(
    snow_all$date,
    precip_all$DateTime,
    stream_all$DateTime,
    all_wells_for_range$datetime
  ),
  na.rm = TRUE
)

date_max <- max(
  c(
    snow_all$date,
    precip_all$DateTime,
    stream_all$DateTime,
    all_wells_for_range$datetime
  ),
  na.rm = TRUE
)

# ----------------------------
# UI
# ----------------------------
ui <- fluidPage(
  titlePanel("Hubbard Brook Experimental Forest: Watersheds 3 & 9"),
  
  fluidRow(
    column(
      width = 4,
      wellPanel(
        selectInput(
          "watershed", "Watershed",
          choices = c("Watershed 3" = 3, "Watershed 9" = 9),
          selected = 3
        ),
        selectInput(
          "soil_type", "Soil type",
          choices = c("E Podzol" = "epod", "Bhs" = "bhs", "Typ" = "typ"),
          selected = "epod"
        ),
        selectInput(
          "stream_metric", "Streamflow metric",
          choices = c(
            "Gage height" = "gage",
            "Discharge (cfs)" = "cfs",
            "Discharge (L/s)" = "ls"
          ),
          selected = "cfs"
        ),
        dateRangeInput(
          "date_range",
          "Date range",
          start = as.Date(date_min),
          end = as.Date(date_max),
          min = as.Date(date_min),
          max = as.Date(date_max)
        ),
        actionButton("apply_filters", "Filter", class = "btn-primary"),
        br(), br(),
        helpText("Change selectors, then click Filter to update plots.")
      )
    ),
    
    column(
      width = 8,
      plotOutput("soil_plot", height = 260),
      plotOutput("wt_plot", height = 260),
      plotOutput("snow_plot", height = 260),
      plotOutput("precip_plot", height = 260),
      plotOutput("stream_plot", height = 260)
    )
  )
)

# ----------------------------
# Server
# ----------------------------
server <- function(input, output, session) {
  
  filters <- eventReactive(input$apply_filters, {
    list(
      watershed = as.integer(input$watershed),
      soil_type = input$soil_type,
      stream_metric = input$stream_metric,
      date_range = input$date_range
    )
  }, ignoreInit = FALSE)
  
  soil_df_long <- reactive({
    f <- filters()
    
    start_dt <- as.POSIXct(f$date_range[1], tz = "UTC")
    end_dt   <- as.POSIXct(f$date_range[2], tz = "UTC") + 24 * 60 * 60 - 1
    
    df <- read_soil_for_selection(f$watershed, f$soil_type) %>%
      filter(TIMESTAMP >= start_dt, TIMESTAMP <= end_dt)
    
    validate(need(nrow(df) > 0, "No soil moisture rows found for this selection and date range."))
    soil_long(df)
  })
  
  output$soil_plot <- renderPlot({
    f <- filters()
    df <- soil_df_long()
    soil_label <- c(epod = "E Podzol", bhs = "Bhs", typ = "Typ")[[f$soil_type]]
    
    ggplot(df, aes(TIMESTAMP, value, color = factor(depth_cm), group = interaction(sensor, depth_cm))) +
      geom_line(linewidth = 0.9, alpha = 0.98) +
      facet_wrap(~ sensor, scales = "free_y", ncol = 1) +
      scale_color_manual(values = depth_colors) +
      plot_theme_blue() +
      theme(
        legend.position = "top",
        legend.direction = "horizontal"
      ) +
      labs(
        title = paste0("Soil Moisture in Watershed ", f$watershed, " (", soil_label, ")"),
        x = "",
        y = "Soil moisture",
        color = "Depth (cm)"
      )
  })
  
  wt_df <- reactive({
    f <- filters()
    
    start_dt <- as.POSIXct(f$date_range[1], tz = "UTC")
    end_dt   <- as.POSIXct(f$date_range[2], tz = "UTC") + 24 * 60 * 60 - 1
    
    df <- read_wells_for_ws(f$watershed) %>%
      filter(wt_cm >= 0, wt_cm <= 100) %>%
      filter(datetime >= start_dt, datetime <= end_dt)
    
    validate(need(nrow(df) > 0, "No water table data found for this watershed and date range."))
    df
  })
  
  output$wt_plot <- renderPlot({
    f <- filters()
    df <- wt_df()
    
    well_palette <- if (f$watershed == 3) well_colors_w3 else well_colors_w9
    
    ggplot(df, aes(datetime, wt_cm, color = well_id, group = well_id)) +
      geom_line(linewidth = 1.0, alpha = 0.98) +
      scale_color_manual(values = well_palette) +
      scale_y_reverse() +
      plot_theme_blue() +
      theme(
        legend.position = "top",
        legend.direction = "horizontal",
        legend.box.margin = margin(0, 0, -10, 0)
      ) +
      labs(
        title = paste("Water Table Depth (Wells) - Watershed", f$watershed),
        x = "",
        y = "Water Table Depth (cm)",
        color = NULL
      )
  })
  
  output$snow_plot <- renderPlot({
    f <- filters()
    
    start_dt <- as.POSIXct(f$date_range[1], tz = "UTC")
    end_dt   <- as.POSIXct(f$date_range[2], tz = "UTC") + 24 * 60 * 60 - 1
    
    df <- snow_all %>%
      filter(watershed == f$watershed) %>%
      filter(date >= start_dt, date <= end_dt) %>%
      filter(snow_cm > 0)
    
    validate(need(nrow(df) > 0, "No snow depth data found for this watershed and date range."))
    
    ggplot(df, aes(date, snow_cm)) +
      geom_line(linewidth = 1.0, color = "#1D4E89", alpha = 0.98) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +  #fix
      plot_theme_blue() +
      labs(
        title = paste("Snow Depth in Watershed", f$watershed),
        x = "",
        y = "Snow Depth (cm)"
      )
  })
  
  output$precip_plot <- renderPlot({
    f <- filters()
    
    start_dt <- as.POSIXct(f$date_range[1], tz = "UTC")
    end_dt   <- as.POSIXct(f$date_range[2], tz = "UTC") + 24 * 60 * 60 - 1
    
    df <- precip_all %>%
      filter(watershed == f$watershed) %>%
      filter(DateTime >= start_dt, DateTime <= end_dt)
    
    validate(need(nrow(df) > 0, "No precipitation data found for this watershed and date range."))
    
    ggplot(df, aes(DateTime, precip)) +
      geom_line(linewidth = 1.0, color = "#1D4E89", alpha = 0.98) +
      scale_y_reverse() +
      plot_theme_blue() +
      labs(
        title = paste("Precipitation in Watershed", f$watershed),
        x = "",
        y = "Hourly Precipitation"
      )
  })
  
  stream_series <- reactive({
    f <- filters()
    
    start_dt <- as.POSIXct(f$date_range[1], tz = "UTC")
    end_dt   <- as.POSIXct(f$date_range[2], tz = "UTC") + 24 * 60 * 60 - 1
    
    df <- stream_all %>%
      filter(watershed == f$watershed) %>%
      filter(DateTime >= start_dt, DateTime <= end_dt)
    
    validate(need(nrow(df) > 0, "No streamflow data found for this watershed and date range."))
    
    if (f$stream_metric == "gage") {
      df %>% transmute(DateTime, value = Gage_ft, ylab = "Gage Height (ft)")
    } else if (f$stream_metric == "cfs") {
      df %>% transmute(DateTime, value = Discharge_cfs, ylab = "Discharge (cfs)")
    } else {
      df %>% transmute(DateTime, value = Discharge_ls, ylab = "Discharge (L/s)")
    }
  })
  
  output$stream_plot <- renderPlot({
    f <- filters()
    s <- stream_series()
    
    ggplot(s, aes(DateTime, value)) +
      geom_line(linewidth = 1.0, color = "#1D4E89", alpha = 0.98) +
      plot_theme_blue() +
      labs(
        title = paste("Streamflow in Watershed", f$watershed),
        x = "",
        y = unique(s$ylab)
      )
  })
}

shinyApp(ui, server)