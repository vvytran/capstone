# app.R
# Hubbard Brook Experimental Forest: Watersheds 3 & 9

library(shiny)
library(tidyverse)
library(ggplot2)
library(readr)

# ----------------------------
# Hourly file names
# ----------------------------
FILE_SNOW_W3 <- "snowdepthw3_hourly.csv"
FILE_SNOW_W9 <- "snowdepthw9_hourly.csv"

FILE_W3_TDR_EPOD <- "epodzol_w3_soi_mois_5_min_tdr_hourly.csv"
FILE_W3_TDR_BHS  <- "Bhs_w3_soi_mois_5_min_tdr_hourly.csv"
FILE_W3_TDR_TYP  <- "typ_w3_soi_mois_5_min_tdr_hourly.csv"

FILE_W9_TDR_EPOD <- "epodzol_w9_soi_mois_5_min_tdr_hourly.csv"
FILE_W9_TDR_BHS  <- "Bhs_w9_soi_mois_5_min_tdr_hourly.csv"
FILE_W9_TDR_TYP  <- "typ_w9_soi_mois_5_min_tdr_hourly.csv"

FILE_WELL_159 <- "well_159_record_hourly.csv"
FILE_WELL_176 <- "well_176_record_hourly.csv"
FILE_WELL_179 <- "well_179_record_hourly.csv"
FILE_WELL_42  <- "well_42_4_d1_hourly.csv"
FILE_WELL_N1  <- "well_N1_record_hourly.csv"
FILE_WELL_N5  <- "well_N5_record_hourly.csv"

FILE_W3_PRECIP <- "HBEF_W3precipitation_15min_hourly.csv"
FILE_W9_PRECIP <- "HBEF_W9precipitation_15min_hourly.csv"

FILE_W3_STREAM <- "w3_stmflow_2013_2024_hourly.csv"
FILE_W9_STREAM <- "w9_stmflow_2013_2024_hourly.csv"

# ----------------------------
# Soil file mapping
# ----------------------------
soil_file_map <- list(
  `3` = c(
    epod = FILE_W3_TDR_EPOD,
    bhs  = FILE_W3_TDR_BHS,
    typ  = FILE_W3_TDR_TYP
  ),
  `9` = c(
    epod = FILE_W9_TDR_EPOD,
    bhs  = FILE_W9_TDR_BHS,
    typ  = FILE_W9_TDR_TYP
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
# Plot styling
# ----------------------------
plot_theme_blue <- function() {
  theme_classic(base_size = 13) +
    theme(
      plot.title = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x = element_text(size = 10, color = "#24476B"),
      axis.ticks.x = element_line(color = "#4F6D8A", linewidth = 0.4),
      axis.line.x = element_line(color = "#4F6D8A", linewidth = 0.5),
      axis.title.y = element_text(size = 12, color = "#163A5F"),
      axis.text.y = element_text(size = 10, color = "#24476B"),
      axis.line = element_line(color = "#4F6D8A", linewidth = 0.5),
      axis.ticks = element_line(color = "#4F6D8A", linewidth = 0.4),
      panel.background = element_rect(fill = "#F4F8FC", color = NA),
      plot.background = element_rect(fill = "#F4F8FC", color = NA),
      legend.background = element_rect(fill = "#F4F8FC", color = NA),
      legend.key = element_rect(fill = "#F4F8FC", color = NA),
      legend.text = element_text(color = "#24476B", size = 10),
      legend.title = element_text(color = "#163A5F", face = "bold", size = 11),
      strip.background = element_rect(fill = "#DCEAF7", color = "#A9C4DD"),
      strip.text = element_text(color = "#163A5F", face = "bold", size = 11),
      legend.position = "top",
      legend.direction = "horizontal",
      legend.box.margin = margin(0, 0, -8, 0),
      plot.tag = element_text(
        size = 13,
        face = "bold",
        color = "#163A5F"
      ),
      plot.tag.position = c(0.01, 0.98),
      plot.margin = margin(2, 2, 2, 70)
    )
}

# ----------------------------
# Helpers
# ----------------------------
as_posix <- function(x) {
  if (inherits(x, "POSIXct")) return(x)
  suppressWarnings(as.POSIXct(x, tz = "UTC"))
}

read_soil_for_selection <- function(watershed, soil_key) {
  ws <- as.character(watershed)
  
  read_csv(soil_file_map[[ws]][[soil_key]], show_col_types = FALSE) %>%
    mutate(
      TIMESTAMP = as_posix(hour),
      watershed = as.integer(watershed),
      soil_type = soil_key
    ) %>%
    select(-hour)
}

soil_long <- function(df) {
  df %>%
    select(TIMESTAMP, watershed, soil_type, where(is.numeric)) %>%
    pivot_longer(
      cols = -c(TIMESTAMP, watershed, soil_type),
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

apply_shared_zoom <- function(p, zoom_range, time_values) {
  if (is.null(zoom_range) || length(time_values) == 0) {
    return(p)
  }
  
  time_values <- time_values[!is.na(time_values)]
  
  if (length(time_values) == 0) {
    return(p)
  }
  
  data_min <- min(time_values)
  data_max <- max(time_values)
  
  zoom_min <- max(zoom_range$xmin, data_min)
  zoom_max <- min(zoom_range$xmax, data_max)
  
  if (is.na(zoom_min) || is.na(zoom_max) || zoom_min >= zoom_max) {
    return(p)
  }
  
  p + coord_cartesian(xlim = c(zoom_min, zoom_max), clip = "off")
}

nearest_row_with_hover <- function(df, time_col, value_col, hover_obj) {
  if (is.null(hover_obj) || nrow(df) == 0) {
    return(NULL)
  }
  
  hover_x <- as.POSIXct(hover_obj$x, origin = "1970-01-01", tz = "UTC")
  hover_y <- hover_obj$y
  
  if (is.na(hover_x) || is.null(hover_y) || is.na(hover_y)) {
    return(NULL)
  }
  
  time_vec <- df[[time_col]]
  value_vec <- df[[value_col]]
  
  keep <- !is.na(time_vec) & !is.na(value_vec)
  df <- df[keep, , drop = FALSE]
  
  if (nrow(df) == 0) {
    return(NULL)
  }
  
  time_num <- as.numeric(df[[time_col]])
  value_num <- as.numeric(df[[value_col]])
  
  time_range <- diff(range(time_num, na.rm = TRUE))
  value_range <- diff(range(value_num, na.rm = TRUE))
  
  if (is.na(time_range) || time_range == 0) time_range <- 1
  if (is.na(value_range) || value_range == 0) value_range <- 1
  
  df %>%
    mutate(
      .hover_dist =
        ((as.numeric(.data[[time_col]]) - as.numeric(hover_x)) / time_range)^2 +
        ((.data[[value_col]] - hover_y) / value_range)^2
    ) %>%
    slice_min(.hover_dist, n = 1, with_ties = FALSE)
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
      Discharge_ls
    ),
  read_csv(FILE_W9_STREAM, show_col_types = FALSE) %>%
    transmute(
      watershed = 9L,
      DateTime = as_posix(hour),
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
        margin-bottom: 12px;
      }
      .control-card, .readme-card {
        background: #F4F8FC;
        border: 1px solid #D6E4F0;
        border-radius: 14px;
        padding: 18px;
        margin-bottom: 14px;
        box-shadow: 0 2px 6px rgba(0,0,0,0.05);
      }
      .plot-stack {
        margin-bottom: 4px;
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
      .collapse-btn, .zoom-btn {
        background-color: #DCEAF7;
        border: 1px solid #A9C4DD;
        color: #163A5F;
        font-weight: 600;
        font-size: 16px;
        border-radius: 10px;
        padding: 8px 14px;
        width: 100%;
        text-align: left;
        margin-bottom: 10px;
      }
      .zoom-btn {
        width: auto;
        text-align: center;
        margin-bottom: 0;
      }
      .form-group label, .checkbox label, .help-block {
        font-size: 16px;
      }
      .shiny-input-container {
        width: 100%;
      }
      .shared-hover-panel {
        margin-top: 4px;
        margin-bottom: 10px;
        padding: 10px 12px;
        background: #E9F2FB;
        border: 1px solid #C7DCEF;
        border-radius: 10px;
        color: #163A5F;
        font-size: 14px;
        white-space: pre-wrap;
      }
      .shared-hover-title {
        font-weight: 700;
        font-size: 16px;
        margin-bottom: 6px;
      }
      .readme-card h2, .readme-card h3 {
        color: #163A5F;
      }
    "))
  ),
  
  tabsetPanel(
    tabPanel(
      "Dashboard",
      
      div(class = "main-title", h1("Hubbard Brook Experimental Forest: Watersheds 3 & 9")),
      
      div("The data for these visualizations are sourced from the Hubbard Brook Experimental Forest, focusing on Watersheds 3 and 9. Select the parameters you want to explore and then click “Apply Filters” to update the visualizations."),
      
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
                  width = 3,
                  checkboxGroupInput(
                    "watersheds",
                    "Watersheds",
                    choices = c("Watershed 3" = 3, "Watershed 9" = 9),
                    selected = c(3, 9)
                  )
                ),
                column(
                  width = 3,
                  checkboxGroupInput(
                    "soil_types",
                    "Soil types",
                    choices = c("E Podzol" = "epod", "Bhs" = "bhs", "Typ" = "typ"),
                    selected = c("epod")
                  )
                ),
                column(
                  width = 3,
                  checkboxGroupInput(
                    "depths",
                    "Soil depths (cm)",
                    choices = c("10 cm" = 10, "30 cm" = 30, "50 cm" = 50),
                    selected = c(10, 30, 50)
                  )
                ),
                column(
                  width = 3,
                  checkboxGroupInput(
                    "stream_metrics",
                    "Streamflow metrics",
                    choices = c("Discharge (L/s)" = "ls"),
                    selected = c("ls")
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
              
              fluidRow(
                column(
                  width = 6,
                  actionButton("apply_filters", "Apply Filters", class = "btn-primary")
                ),
                column(
                  width = 6,
                  actionButton("reset_zoom", "Reset Shared Zoom", class = "zoom-btn")
                )
              ),
              br(),
              helpText("Brush on any plot to zoom all plots to the same date range. Hover over any plot to see the nearest value in the shared hover panel above the plots.")
            )
          ),
          
          div(
            class = "shared-hover-panel",
            div(class = "shared-hover-title", "Hover Details"),
            textOutput("shared_hover_info")
          ),
          
          div(class = "plot-stack",
              plotOutput("soil_plot", height = 300, brush = "soil_brush", hover = "soil_hover")),
          
          div(class = "plot-stack",
              plotOutput("wt_plot", height = 300, brush = "wt_brush", hover = "wt_hover")),
          
          div(class = "plot-stack",
              plotOutput("snow_plot", height = 260, brush = "snow_brush", hover = "snow_hover")),
          
          div(class = "plot-stack",
              plotOutput("precip_plot", height = 260, brush = "precip_brush", hover = "precip_hover")),
          
          div(class = "plot-stack",
              plotOutput("stream_plot", height = 300, brush = "stream_brush", hover = "stream_hover"))
        )
      )
    ),
    
    tabPanel(
      "README",
      
      div(
        class = "readme-card",
        
        h2("App Overview"),
        p("This Shiny application visualizes hydrological data from Watersheds 3 and 9 in the Hubbard Brook Experimental Forest. It supports comparison across watersheds, soil types, soil depths, wells, and streamflow metrics using hourly aggregated datasets for faster performance."),
        
        h3("Main Functions"),
        tags$ul(
          tags$li("Compare one or both watersheds at the same time"),
          tags$li("Compare one or more soil types"),
          tags$li("Select one or more soil depths to display separately"),
          tags$li("Choose one or more streamflow metrics"),
          tags$li("Restrict the analysis to a custom date range"),
          tags$li("Zoom all plots together using a brushed time window"),
          tags$li("Hover over plots to view the nearest values in one shared panel")
        )
      )
    )
  )
)

# ----------------------------
# Server
# ----------------------------
server <- function(input, output, session) {
  
  controls_open <- reactiveVal(TRUE)
  shared_zoom <- reactiveVal(NULL)
  last_hovered_plot <- reactiveVal(NULL)
  
  update_shared_zoom <- function(brush_obj) {
    if (!is.null(brush_obj) && !is.null(brush_obj$xmin) && !is.null(brush_obj$xmax)) {
      shared_zoom(
        list(
          xmin = as.POSIXct(brush_obj$xmin, origin = "1970-01-01", tz = "UTC"),
          xmax = as.POSIXct(brush_obj$xmax, origin = "1970-01-01", tz = "UTC")
        )
      )
    }
  }
  
  observeEvent(input$toggle_controls, {
    controls_open(!controls_open())
    updateActionButton(
      session,
      "toggle_controls",
      label = if (controls_open()) "Hide Filters" else "Show Filters"
    )
  })
  
  observeEvent(input$soil_brush,   { update_shared_zoom(input$soil_brush) })
  observeEvent(input$wt_brush,     { update_shared_zoom(input$wt_brush) })
  observeEvent(input$snow_brush,   { update_shared_zoom(input$snow_brush) })
  observeEvent(input$precip_brush, { update_shared_zoom(input$precip_brush) })
  observeEvent(input$stream_brush, { update_shared_zoom(input$stream_brush) })
  
  observeEvent(input$soil_hover,   { last_hovered_plot("soil") })
  observeEvent(input$wt_hover,     { last_hovered_plot("wt") })
  observeEvent(input$snow_hover,   { last_hovered_plot("snow") })
  observeEvent(input$precip_hover, { last_hovered_plot("precip") })
  observeEvent(input$stream_hover, { last_hovered_plot("stream") })
  
  observeEvent(input$reset_zoom, {
    shared_zoom(NULL)
    session$resetBrush("soil_brush")
    session$resetBrush("wt_brush")
    session$resetBrush("snow_brush")
    session$resetBrush("precip_brush")
    session$resetBrush("stream_brush")
  })
  
  output$show_controls <- reactive({
    controls_open()
  })
  outputOptions(output, "show_controls", suspendWhenHidden = FALSE)
  
  filters <- eventReactive(input$apply_filters, {
    list(
      watersheds = as.integer(input$watersheds),
      soil_types = input$soil_types,
      depths = as.numeric(input$depths),
      stream_metrics = input$stream_metrics,
      date_range = input$date_range
    )
  }, ignoreInit = FALSE)
  
  soil_df_long <- reactive({
    f <- filters()
    
    validate(
      need(length(f$watersheds) > 0, "Select at least one watershed."),
      need(length(f$soil_types) > 0, "Select at least one soil type."),
      need(length(f$depths) > 0, "Select at least one soil depth.")
    )
    
    start_dt <- as.POSIXct(f$date_range[1], tz = "UTC")
    end_dt   <- as.POSIXct(f$date_range[2], tz = "UTC") + 24 * 60 * 60 - 1
    
    soil_list <- purrr::map_dfr(f$watersheds, function(ws) {
      purrr::map_dfr(f$soil_types, function(st) {
        read_soil_for_selection(ws, st)
      })
    })
    
    df <- soil_list %>%
      filter(TIMESTAMP >= start_dt, TIMESTAMP <= end_dt) %>%
      soil_long() %>%
      filter(depth_cm %in% f$depths)
    
    validate(need(nrow(df) > 0, "No soil moisture rows found for this selection and date range."))
    df
  })
  
  wt_df <- reactive({
    f <- filters()
    
    validate(need(length(f$watersheds) > 0, "Select at least one watershed."))
    
    start_dt <- as.POSIXct(f$date_range[1], tz = "UTC")
    end_dt   <- as.POSIXct(f$date_range[2], tz = "UTC") + 24 * 60 * 60 - 1
    
    df <- purrr::map_dfr(f$watersheds, read_wells_for_ws) %>%
      filter(wt_cm >= 0, wt_cm <= 500) %>%
      filter(datetime >= start_dt, datetime <= end_dt)
    
    validate(need(nrow(df) > 0, "No water table data found for this selection and date range."))
    df
  })
  
  snow_df <- reactive({
    f <- filters()
    
    validate(need(length(f$watersheds) > 0, "Select at least one watershed."))
    
    start_dt <- as.POSIXct(f$date_range[1], tz = "UTC")
    end_dt   <- as.POSIXct(f$date_range[2], tz = "UTC") + 24 * 60 * 60 - 1
    
    df <- snow_all %>%
      filter(watershed %in% f$watersheds) %>%
      filter(date >= start_dt, date <= end_dt) %>%
      filter(snow_cm > 0)
    
    validate(need(nrow(df) > 0, "No snow depth data found for this selection and date range."))
    df
  })
  
  precip_df <- reactive({
    f <- filters()
    
    validate(need(length(f$watersheds) > 0, "Select at least one watershed."))
    
    start_dt <- as.POSIXct(f$date_range[1], tz = "UTC")
    end_dt   <- as.POSIXct(f$date_range[2], tz = "UTC") + 24 * 60 * 60 - 1
    
    df <- precip_all %>%
      filter(watershed %in% f$watersheds) %>%
      filter(DateTime >= start_dt, DateTime <= end_dt)
    
    validate(need(nrow(df) > 0, "No precipitation data found for this selection and date range."))
    df
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
    
    df %>%
      transmute(
        watershed,
        DateTime,
        metric = "Discharge (L/s)",
        value = Discharge_ls
      )
  })
  
  output$soil_plot <- renderPlot({
    df <- soil_df_long()
    
    p <- ggplot(
      df,
      aes(
        TIMESTAMP,
        value,
        color = factor(depth_cm),
        group = interaction(watershed, soil_type, depth_cm)
      )
    ) +
      geom_line(linewidth = 1.0, alpha = 0.98) +
      facet_wrap(
        ~ paste("WS", watershed, "-", soil_label),
        ncol = 1,
        scales = "free_y"
      ) +
      scale_color_manual(values = depth_colors) +
      plot_theme_blue() +
      labs(
        y = "Soil moisture",
        color = "Depth (cm)",
        tag = "Soil Moisture"
      )
    
    apply_shared_zoom(p, shared_zoom(), df$TIMESTAMP)
  })
  
  output$wt_plot <- renderPlot({
    df <- wt_df()
    
    p <- ggplot(df, aes(datetime, wt_cm, color = well_id, group = well_id)) +
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
        y = "Water Table Depth (cm)",
        color = "Well",
        tag = "Water Table"
      )
    
    apply_shared_zoom(p, shared_zoom(), df$datetime)
  })
  
  output$snow_plot <- renderPlot({
    df <- snow_df()
    
    p <- ggplot(df, aes(date, snow_cm, color = factor(watershed), group = watershed)) +
      geom_line(linewidth = 1.05, alpha = 0.98) +
      scale_color_manual(values = watershed_colors) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      plot_theme_blue() +
      labs(
        y = "Snow Depth (cm)",
        color = "Watershed",
        tag = "Snow Depth"
      )
    
    apply_shared_zoom(p, shared_zoom(), df$date)
  })
  
  output$precip_plot <- renderPlot({
    df <- precip_df()
    
    p <- ggplot(df, aes(DateTime, precip, color = factor(watershed), group = watershed)) +
      geom_line(linewidth = 1.05, alpha = 0.98) +
      scale_color_manual(values = watershed_colors) +
      scale_y_reverse() +
      plot_theme_blue() +
      labs(
        y = "Hourly Precipitation",
        color = "Watershed",
        tag = "Precipitation"
      )
    
    apply_shared_zoom(p, shared_zoom(), df$DateTime)
  })
  
  output$stream_plot <- renderPlot({
    df <- stream_long()
    
    validate(need(nrow(df) > 0, "No streamflow data available for the current selection."))
    
    p <- ggplot(df, aes(DateTime, value, color = factor(watershed), group = watershed)) +
      geom_line(linewidth = 1.05, alpha = 0.98) +
      scale_color_manual(values = watershed_colors) +
      facet_wrap(~ metric, ncol = 1, scales = "free_y") +
      plot_theme_blue() +
      labs(
        y = NULL,
        color = "Watershed",
        tag = "Streamflow"
      )
    
    apply_shared_zoom(p, shared_zoom(), df$DateTime)
  })
  
  output$shared_hover_info <- renderText({
    active_plot <- last_hovered_plot()
    
    if (is.null(active_plot)) {
      return("Hover over any plot to see the nearest observation.")
    }
    
    if (active_plot == "soil") {
      row <- tryCatch(
        nearest_row_with_hover(soil_df_long(), "TIMESTAMP", "value", input$soil_hover),
        error = function(e) NULL
      )
      if (is.null(row)) return("Hover over any plot to see the nearest observation.")
      
      return(paste0(
        "Plot: Soil Moisture\n",
        "Time: ", format(row$TIMESTAMP, "%Y-%m-%d %H:%M"), "\n",
        "Watershed: ", row$watershed, "\n",
        "Soil type: ", row$soil_label, "\n",
        "Depth: ", row$depth_cm, " cm\n",
        "Value: ", round(row$value, 3)
      ))
    }
    
    if (active_plot == "wt") {
      row <- tryCatch(
        nearest_row_with_hover(wt_df(), "datetime", "wt_cm", input$wt_hover),
        error = function(e) NULL
      )
      if (is.null(row)) return("Hover over any plot to see the nearest observation.")
      
      return(paste0(
        "Plot: Water Table Depth\n",
        "Time: ", format(row$datetime, "%Y-%m-%d %H:%M"), "\n",
        "Watershed: ", row$watershed, "\n",
        "Well: ", row$well_id, "\n",
        "Water table depth: ", round(row$wt_cm, 2), " cm"
      ))
    }
    
    if (active_plot == "snow") {
      row <- tryCatch(
        nearest_row_with_hover(snow_df(), "date", "snow_cm", input$snow_hover),
        error = function(e) NULL
      )
      if (is.null(row)) return("Hover over any plot to see the nearest observation.")
      
      return(paste0(
        "Plot: Snow Depth\n",
        "Time: ", format(row$date, "%Y-%m-%d %H:%M"), "\n",
        "Watershed: ", row$watershed, "\n",
        "Snow depth: ", round(row$snow_cm, 2), " cm"
      ))
    }
    
    if (active_plot == "precip") {
      row <- tryCatch(
        nearest_row_with_hover(precip_df(), "DateTime", "precip", input$precip_hover),
        error = function(e) NULL
      )
      if (is.null(row)) return("Hover over any plot to see the nearest observation.")
      
      return(paste0(
        "Plot: Precipitation\n",
        "Time: ", format(row$DateTime, "%Y-%m-%d %H:%M"), "\n",
        "Watershed: ", row$watershed, "\n",
        "Hourly precipitation: ", round(row$precip, 3)
      ))
    }
    
    if (active_plot == "stream") {
      row <- tryCatch(
        nearest_row_with_hover(stream_long(), "DateTime", "value", input$stream_hover),
        error = function(e) NULL
      )
      if (is.null(row)) return("Hover over any plot to see the nearest observation.")
      
      return(paste0(
        "Plot: Streamflow\n",
        "Time: ", format(row$DateTime, "%Y-%m-%d %H:%M"), "\n",
        "Watershed: ", row$watershed, "\n",
        "Metric: ", row$metric, "\n",
        "Value: ", round(row$value, 3)
      ))
    }
    
    "Hover over any plot to see the nearest observation."
  })
}

shinyApp(ui, server)