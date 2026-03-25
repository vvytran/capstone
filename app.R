library(shiny)

library(tidyverse)

library(ggplot2)

library(readr)

library(readxl)





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





# Hourly soil file mapping

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
  
}



soil_long <- function(df) {
  
  df %>%
    
    select(TIMESTAMP, sensor, where(is.numeric)) %>%
    
    pivot_longer(
      
      cols = -c(TIMESTAMP, sensor),
      
      names_to = "series",
      
      values_to = "value"
      
    ) %>%
    
    filter(!is.na(TIMESTAMP), !is.na(value))
  
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





# User Interface

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
          
          "Select date range:",
          
          start = min(snow_all$date),
          
          end   = max(snow_all$date),
          
          min   = min(snow_all$date),
          
          max   = max(snow_all$date)
          
        ),
        
        
        
        actionButton("apply_filters", "Filter", class = "btn-primary"),
        
        br(), br(),
        
        helpText("Change selectors, then click Filter to update plots."),
        
        
        
        checkboxGroupInput(
          
          "plots_to_show",
          
          "Select graphs to display:",
          
          choices = c(
            
            "Soil Moisture" = "soil",
            
            "Water Table" = "wt",
            
            "Snow Depth" = "snow",
            
            "Precipitation" = "precip",
            
            "Streamflow" = "stream"
            
          ),
          
          selected = c("soil", "wt")
          
        )
        
      )
      
    ),
    
    
    
    column(
      
      width = 8,
      
      uiOutput("selected_plots")  # <-- vertical stacking handled in server
      
    )
    
  )
  
)





# Server

# ----------------------------

server <- function(input, output, session) {
  
  
  
  filters <- eventReactive(input$apply_filters, {
    
    list(
      
      watershed     = as.integer(input$watershed),
      
      soil_type     = input$soil_type,
      
      stream_metric = input$stream_metric,
      
      date_range    = input$date_range
      
    )
    
  }, ignoreInit = FALSE)
  
  
  
  soil_df_long <- reactive({
    
    f <- filters()
    
    df <- read_soil_for_selection(f$watershed, f$soil_type)
    
    validate(need(nrow(df) > 0, "No soil moisture rows found for this selection."))
    
    df <- soil_long(df)
    
    
    
    df %>% filter(TIMESTAMP >= f$date_range[1], TIMESTAMP <= f$date_range[2])
    
  })
  
  output$selected_plots <- renderUI({
    
    req(input$plots_to_show)
    
    
    
    plot_outputs <- lapply(input$plots_to_show, function(p) {
      
      plotname <- paste0(p, "_plot")
      
      fluidRow(
        
        column(
          
          width = 12,
          
          plotOutput(plotname, height = 350)
          
        )
        
      )
      
    })
    
    
    
    do.call(tagList, plot_outputs)
    
  })
  
  
  
  wt_df <- reactive({
    
    f <- filters()
    
    df <- read_wells_for_ws(f$watershed)
    
    validate(need(nrow(df) > 0, "No water table data found."))
    
    
    
    df %>%
      
      filter(!is.na(wt_cm), wt_cm >= 0, wt_cm <= 400) %>%
      
      filter(datetime >= f$date_range[1], datetime <= f$date_range[2])
    
  })
  
  
  
  output$soil_plot <- renderPlot({
    
    f <- filters()
    
    df <- soil_df_long()
    
    soil_label <- c(epod = "E Podzol", bhs = "Bhs", typ = "Typ")[[f$soil_type]]
    
    
    
    ggplot(df, aes(TIMESTAMP, value, group = interaction(sensor, series))) +
      
      geom_point(size = 0.5, alpha = 0.5) +
      
      geom_line() +
      
      facet_wrap(~ sensor, scales = "free_y", ncol = 1) +
      
      theme_classic() +
      
      labs(
        
        title = paste0("Soil Moisture in Watershed ", f$watershed, " (", soil_label, ")"),
        
        x = "",
        
        y = "Soil moisture"
        
      )
    
  })
  
  
  
  output$wt_plot <- renderPlot({
    
    f <- filters()
    
    df <- wt_df()
    
    
    
    ggplot(df, aes(datetime, wt_cm, color = well_id, group = well_id)) +
      
      geom_point(size = 0.6, alpha = 0.7) +
      
      geom_line() +
      
      scale_y_reverse() +
      
      theme_classic() +
      
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
    
    df <- snow_all %>%
      
      filter(watershed == f$watershed, snow_cm > 0) %>%
      
      filter(date >= f$date_range[1], date <= f$date_range[2])
    
    
    
    validate(need(nrow(df) > 0, "No snow depth data found."))
    
    
    
    ggplot(df, aes(date, snow_cm)) +
      
      geom_point(size = 0.6, alpha = 0.6) +
      
      geom_line() +
      
      theme_classic() +
      
      labs(
        
        title = paste("Snow Depth in Watershed", f$watershed),
        
        x = "",
        
        y = "Snow Depth (cm)"
        
      )
    
  })
  
  
  
  output$precip_plot <- renderPlot({
    
    f <- filters()
    
    
    
    df <- precip_all %>%
      
      filter(watershed == f$watershed) %>%
      
      filter(DateTime >= f$date_range[1], DateTime <= f$date_range[2])
    
    
    
    max_val <- quantile(df$precip, 0.95, na.rm = TRUE)
    
    
    
    ggplot(df, aes(DateTime, -precip)) +
      
      geom_col(width = 3600) +
      
      coord_cartesian(ylim = c(-max_val, 0)) +
      
      scale_y_continuous(labels = abs) +
      
      theme_classic() +
      
      labs(
        
        title = paste("Precipitation in Watershed", f$watershed),
        
        x = "",
        
        y = "Hourly Precipitation"
        
      )
    
  })
  
  
  
  stream_series <- reactive({
    
    f <- filters()
    
    df <- stream_all %>%
      
      filter(watershed == f$watershed) %>%
      
      filter(DateTime >= f$date_range[1], DateTime <= f$date_range[2])
    
    
    
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
      
      geom_point(size = 0.6, alpha = 0.6) +
      
      geom_line() +
      
      theme_classic() +
      
      labs(
        
        title = paste("Streamflow in Watershed", f$watershed),
        
        x = "",
        
        y = unique(s$ylab)
        
      )
    
  })
  
}



shinyApp(ui, server)