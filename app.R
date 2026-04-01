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

soil_type_labels <- c(
  epod = "E Podzol",
  bhs  = "Bhs",
  typ  = "Typ"
)

# ----------------------------
# Plot styling
# ----------------------------
plot_theme_blue <- function() {
  theme_classic(base_size = 15) +
    theme(
      plot.title = element_text(face = "bold", size = 18, color = "#163A5F"),
      axis.title = element_text(size = 14, color = "#163A5F"),
      axis.text = element_text(size = 12, color = "#24476B"),
      axis.line = element_line(color = "#4F6D8A", linewidth = 0.6),
      axis.ticks = element_line(color = "#4F6D8A", linewidth = 0.5),
      panel.background = element_rect(fill = "#F4F8FC", color = NA),
      plot.background = element_rect(fill = "#F4F8FC", color = NA),
      legend.background = element_rect(fill = "#F4F8FC", color = NA),
      legend.key = element_rect(fill = "#F4F8FC", color = NA),
      legend.text = element_text(color = "#24476B", size = 12),
      legend.title = element_text(color = "#163A5F", face = "bold", size = 13),
      strip.background = element_rect(fill = "#DCEAF7", color = "#A9C4DD"),
      strip.text = element_text(color = "#163A5F", face = "bold", size = 13),
      legend.position = "top",
      legend.direction = "horizontal",
      legend.box.margin = margin(0, 0, -6, 0)
    )
}

depth_colors <- c(
  "10" = "#1D4E89",
  "30" = "#2C7FB8",
  "50" = "#41B6C4"
)

watershed_colors <- c(
  "3" = "#1D4E89",
  "9" = "#41B6C4"
)

well_colors <- c(
  "42_4_d1" = "#1D4E89",
  "N1"      = "#2C7FB8",
  "N5"      = "#41B6C4",
  "159"     = "#0B3C5D",
  "176"     = "#328CC1",
  "179"     = "#7DB9DE"
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
  
  bind_rows(tdr, ter) %>%
    mutate(
      watershed = as.integer(watershed),
      soil_type = soil_key
    )
}

soil_long <- function(df) {
  df %>%
    select(TIMESTAMP, sensor, watershed, soil_type, where(is.numeric)) %>%
    pivot_longer(
      cols = -c(TIMESTAMP, sensor, watershed, soil_type),
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
      ),
      soil_label = recode(soil_type, !!!soil_type_labels)
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
        well_id = id,
        watershed = as.integer(watershed)
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
  tags$head(
    tags$style(HTML("
      body {
        background-color: #EEF5FB;
        font-size: 16px;
      }
      .main-title {
        color: #163A5F;
        font-weight: 700;
        margin-bottom: 18px;
      }
      .control-card, .plot-card {
        background: #F4F8FC;
        border: 1px solid #D6E4F0;
        border-radius: 14px;
        padding: 18px;
        margin-bottom: 18px;
        box-shadow: 0 2px 6px rgba(0,0,0,0.05);
      }
      .card-title {
        font-weight: 700;
        color: #163A5F;
        margin-bottom: 12px;
        font-size: 20px;
      }
      .btn-primary {
        background-color: #1D4E89;
        border-color: #1D4E89;
        font-size: 16px;
        padding: 8px 16px;
      }
      .collapse-btn {
        background-color: #DCEAF7;
        border: 1px solid #A9C4DD;
        color: #163A5F;
        font-weight: 600;
        font-size: 16px;
        border-radius: 10px;
        padding: 8px 14px;
        width: 100%;
        text-align: left;
        margin-bottom: 12px;
      }
      .form-group label, .checkbox label, .help-block {
        font-size: 16px;
      }
      .shiny-input-container {
        width: 100%;
      }
    "))
  ),
  
  div(class = "main-title", h1("Hubbard Brook Experimental Forest: Watersheds 3 & 9")),
  
  div("The data for these visualizations are sourced from the Hubbard Brook Experimental Forest, focusing on Watersheds 3 and 9. Select the parameters you want to explore and then click “Filter” to update the visualizations."),
  
  fluidRow(
    column(
      width = 12,
      
      actionButton("toggle_controls", "Hide Filters", class = "collapse-btn"),
      
      conditionalPanel(
        condition = "output.show_controls",
        div(
          class = "control-card",
          div(class = "card-title", "Filters"),
          
          fluidRow(
            column(
              width = 4,
              checkboxGroupInput(
                "watersheds",
                "Watersheds",
                choices = c("Watershed 3" = 3, "Watershed 9" = 9),
                selected = c(3, 9)
              )
            ),
            column(
              width = 4,
              checkboxGroupInput(
                "soil_types",
                "Soil types",
                choices = c("E Podzol" = "epod", "Bhs" = "bhs", "Typ" = "typ"),
                selected = c("epod")
              )
            ),
            column(
              width = 4,
              checkboxGroupInput(
                "stream_metrics",
                "Streamflow metrics",
                choices = c(
                  "Gage height" = "gage",
                  "Discharge (cfs)" = "cfs",
                  "Discharge (L/s)" = "ls"
                ),
                selected = c("cfs")
              )
            )
          ),
          
          dateRangeInput(
            "date_range",
            "Date range",
            start = as.Date(date_min),
            end = as.Date(date_max),
            min = as.Date(date_min),
            max = as.Date(date_max)
          ),
          
          actionButton("apply_filters", "Apply Filters", class = "btn-primary"),
          br(), br(),
          helpText("Select one or more watersheds and soil types, then click Apply Filters.")
        )
      ),
      
      div(
        class = "plot-card",
        div(class = "card-title", "Soil Moisture"),
        plotOutput("soil_plot", height = 360)
      ),
      
      div(
        class = "plot-card",
        div(class = "card-title", "Water Table Depth"),
        plotOutput("wt_plot", height = 360)
      ),
      
      div(
        class = "plot-card",
        div(class = "card-title", "Snow Depth"),
        plotOutput("snow_plot", height = 320)
      ),
      
      div(
        class = "plot-card",
        div(class = "card-title", "Precipitation"),
        plotOutput("precip_plot", height = 320)
      ),
      
      div(
        class = "plot-card",
        div(class = "card-title", "Streamflow"),
        plotOutput("stream_plot", height = 360)
      )
    )
  )
)

# ----------------------------
# Server
# ----------------------------
server <- function(input, output, session) {
  
  controls_open <- reactiveVal(TRUE)
  
  observeEvent(input$toggle_controls, {
    controls_open(!controls_open())
    updateActionButton(
      session,
      "toggle_controls",
      label = if (controls_open()) "Hide Filters" else "Show Filters"
    )
  })
  
  output$show_controls <- reactive({
    controls_open()
  })
  outputOptions(output, "show_controls", suspendWhenHidden = FALSE)
  
  filters <- eventReactive(input$apply_filters, {
    list(
      watersheds = as.integer(input$watersheds),
      soil_types = input$soil_types,
      stream_metrics = input$stream_metrics,
      date_range = input$date_range
    )
  }, ignoreInit = FALSE)
  
  soil_df_long <- reactive({
    f <- filters()
    
    validate(
      need(length(f$watersheds) > 0, "Select at least one watershed."),
      need(length(f$soil_types) > 0, "Select at least one soil type.")
    )
    
    start_dt <- as.POSIXct(f$date_range[1], tz = "UTC")
    end_dt   <- as.POSIXct(f$date_range[2], tz = "UTC") + 24 * 60 * 60 - 1
    
    soil_list <- purrr::map_dfr(f$watersheds, function(ws) {
      purrr::map_dfr(f$soil_types, function(st) {
        read_soil_for_selection(ws, st)
      })
    })
    
    df <- soil_list %>%
      filter(TIMESTAMP >= start_dt, TIMESTAMP <= end_dt)
    
    validate(need(nrow(df) > 0, "No soil moisture rows found for this selection and date range."))
    soil_long(df)
  })
  
  output$soil_plot <- renderPlot({
    df <- soil_df_long()
    
    ggplot(
      df,
      aes(
        TIMESTAMP,
        value,
        color = factor(depth_cm),
        group = interaction(sensor, watershed, soil_type, depth_cm)
      )
    ) +
      geom_line(linewidth = 1.0, alpha = 0.98) +
      facet_grid(sensor ~ paste("WS", watershed, "-", soil_label), scales = "free_y") +
      scale_color_manual(values = depth_colors) +
      plot_theme_blue() +
      labs(
        title = "Soil Moisture Comparison",
        x = "",
        y = "Soil moisture",
        color = "Depth (cm)"
      )
  })
  
  wt_df <- reactive({
    f <- filters()
    
    validate(need(length(f$watersheds) > 0, "Select at least one watershed."))
    
    start_dt <- as.POSIXct(f$date_range[1], tz = "UTC")
    end_dt   <- as.POSIXct(f$date_range[2], tz = "UTC") + 24 * 60 * 60 - 1
    
    df <- purrr::map_dfr(f$watersheds, read_wells_for_ws) %>%
      filter(wt_cm >= 0, wt_cm <= 100) %>%
      filter(datetime >= start_dt, datetime <= end_dt)
    
    validate(need(nrow(df) > 0, "No water table data found for this selection and date range."))
    df
  })
  
  output$wt_plot <- renderPlot({
    df <- wt_df()
    
    ggplot(df, aes(datetime, wt_cm, color = well_id, group = well_id)) +
      geom_line(linewidth = 1.05, alpha = 0.98) +
      scale_color_manual(values = well_colors) +
      scale_y_reverse() +
      facet_wrap(
        ~ watershed,
        ncol = 1,
        scales = "free_x",
        labeller = labeller(watershed = \(x) paste("Watershed", x))
      ) +
      plot_theme_blue() +
      labs(
        title = "Water Table Depth Comparison",
        x = "",
        y = "Water Table Depth (cm)",
        color = "Well"
      )
  })
  
  output$snow_plot <- renderPlot({
    f <- filters()
    
    validate(need(length(f$watersheds) > 0, "Select at least one watershed."))
    
    start_dt <- as.POSIXct(f$date_range[1], tz = "UTC")
    end_dt   <- as.POSIXct(f$date_range[2], tz = "UTC") + 24 * 60 * 60 - 1
    
    df <- snow_all %>%
      filter(watershed %in% f$watersheds) %>%
      filter(date >= start_dt, date <= end_dt) %>%
      filter(snow_cm > 0)
    
    validate(need(nrow(df) > 0, "No snow depth data found for this selection and date range."))
    
    ggplot(df, aes(date, snow_cm, color = factor(watershed), group = watershed)) +
      geom_line(linewidth = 1.05, alpha = 0.98) +
      scale_color_manual(values = watershed_colors) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      plot_theme_blue() +
      labs(
        title = "Snow Depth Comparison",
        x = "",
        y = "Snow Depth (cm)",
        color = "Watershed"
      )
  })
  
  output$precip_plot <- renderPlot({
    f <- filters()
    
    validate(need(length(f$watersheds) > 0, "Select at least one watershed."))
    
    start_dt <- as.POSIXct(f$date_range[1], tz = "UTC")
    end_dt   <- as.POSIXct(f$date_range[2], tz = "UTC") + 24 * 60 * 60 - 1
    
    df <- precip_all %>%
      filter(watershed %in% f$watersheds) %>%
      filter(DateTime >= start_dt, DateTime <= end_dt)
    
    validate(need(nrow(df) > 0, "No precipitation data found for this selection and date range."))
    
    ggplot(df, aes(DateTime, precip, color = factor(watershed), group = watershed)) +
      geom_line(linewidth = 1.05, alpha = 0.98) +
      scale_color_manual(values = watershed_colors) +
      scale_y_reverse() +
      plot_theme_blue() +
      labs(
        title = "Precipitation Comparison",
        x = "",
        y = "Hourly Precipitation",
        color = "Watershed"
      )
  })
  
  stream_long <- reactive({
    f <- filters()
    
    validate(
      need(length(f$watersheds) > 0, "Select at least one watershed."),
      need(length(f$stream_metrics) > 0, "Select at least one streamflow metric.")
    )
    
    start_dt <- as.POSIXct(f$date_range[1], tz = "UTC")
    end_dt   <- as.POSIXct(f$date_range[2], tz = "UTC") + 24 * 60 * 60 - 1
    
    df <- stream_all %>%
      filter(watershed %in% f$watersheds) %>%
      filter(DateTime >= start_dt, DateTime <= end_dt)
    
    validate(need(nrow(df) > 0, "No streamflow data found for this selection and date range."))
    
    out <- list()
    
    if ("gage" %in% f$stream_metrics) {
      out[[length(out) + 1]] <- df %>%
        transmute(
          watershed,
          DateTime,
          metric = "Gage height",
          value = Gage_ft
        )
    }
    
    if ("cfs" %in% f$stream_metrics) {
      out[[length(out) + 1]] <- df %>%
        transmute(
          watershed,
          DateTime,
          metric = "Discharge (cfs)",
          value = Discharge_cfs
        )
    }
    
    if ("ls" %in% f$stream_metrics) {
      out[[length(out) + 1]] <- df %>%
        transmute(
          watershed,
          DateTime,
          metric = "Discharge (L/s)",
          value = Discharge_ls
        )
    }
    
    bind_rows(out)
  })
  
  output$stream_plot <- renderPlot({
    df <- stream_long()
    
    ggplot(df, aes(DateTime, value, color = factor(watershed), group = watershed)) +
      geom_line(linewidth = 1.05, alpha = 0.98) +
      scale_color_manual(values = watershed_colors) +
      facet_wrap(~ metric, ncol = 1, scales = "free_y") +
      plot_theme_blue() +
      labs(
        title = "Streamflow Comparison",
        x = "",
        y = NULL,
        color = "Watershed"
      )
  })
}

shinyApp(ui, server)