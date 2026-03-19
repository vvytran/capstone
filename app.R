# app.R
# Hubbard Brook Experimental Forest: Watersheds 3 & 9

library(shiny)
library(tidyverse)
library(ggplot2)
library(readxl)
library(readr)

# ----------------------------
# File names (exactly as your script)
# ----------------------------
FILE_SNOW <- "snow depth wells.xlsx"

FILE_W3_TDR    <- "w3 soil mois 5 min tdr.xlsx"
FILE_W3_TERROS <- "w3 soil mois 5 min terros.xlsx"

FILE_W9_TDR    <- "w9 tdr 5 min.xlsx"
FILE_W9_TERROS <- "w9 terros 5 min.xlsx"

FILE_WELL_159 <- "well 159 record.xlsx"
FILE_WELL_176 <- "well 176 record.xlsx"
FILE_WELL_179 <- "well 179 record.xlsx"
FILE_WELL_42  <- "well 42_4_d1.xlsx"
FILE_WELL_N1  <- "well N1 record.xlsx"
FILE_WELL_N5  <- "well N5 record.xlsx"

FILE_W3_PRECIP <- "HBEF_W3precipitation_15min.csv"
FILE_W9_PRECIP <- "HBEF_W9precipitation_15min.csv"

FILE_W3_STREAM <- "w3_stmflow_1957-2012.csv"
FILE_W9_STREAM <- "w9_stmflow_1995-2012.csv"

# ----------------------------
# Soil sheet-name mapping (W3 vs W9 capitalization)
# ----------------------------
soil_sheet_map <- list(
  `3` = c(epod = "e podzol", bhs = "Bhs", typ = "typ"),
  `9` = c(epod = "E pod",    bhs = "Bhs", typ = "Typ")
)

# ----------------------------
# Well mapping (per your correction)
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
# Helpers
# ----------------------------
as_posix <- function(x) {
  if (inherits(x, "POSIXct")) return(x)
  suppressWarnings(as.POSIXct(x, tz = "UTC"))
}

read_soil_for_selection <- function(watershed, soil_key) {
  ws <- as.character(watershed)
  sheet <- soil_sheet_map[[ws]][[soil_key]]
  
  tdr_file    <- if (watershed == 3) FILE_W3_TDR else FILE_W9_TDR
  terros_file <- if (watershed == 3) FILE_W3_TERROS else FILE_W9_TERROS
  
  tdr <- read_excel(tdr_file, sheet = sheet) %>% mutate(sensor = "TDR")
  ter <- read_excel(terros_file, sheet = sheet) %>% mutate(sensor = "Terros")
  
  bind_rows(tdr, ter) %>% mutate(TIMESTAMP = as_posix(TIMESTAMP))
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
    read_excel(fp) %>%
      transmute(
        datetime = as_posix(.data$`date-time`),
        wt_cm    = .data$`water table depth (cm)`,
        well_id  = id
      ) %>%
      filter(!is.na(datetime), !is.na(wt_cm))
  })
}

# ----------------------------
# Load watershed-only datasets once
# ----------------------------
snow_all <- bind_rows(
  read_excel(FILE_SNOW, sheet = "snow depth w3") %>% mutate(watershed = 3L),
  read_excel(FILE_SNOW, sheet = "snow depth w9") %>% mutate(watershed = 9L)
) %>%
  mutate(date = as.Date(date))

precip_all <- bind_rows(
  read_csv(FILE_W3_PRECIP, show_col_types = FALSE) %>% mutate(watershed = 3L),
  read_csv(FILE_W9_PRECIP, show_col_types = FALSE) %>% mutate(watershed = 9L)
) %>%
  mutate(DateTime = as_posix(.data$DateTime))

# Streamflow CSVs (your files use DATETIME, so rename it)
stream_all <- bind_rows(
  read_csv(FILE_W3_STREAM, show_col_types = FALSE) %>%
    rename(DateTime = DATETIME) %>%
    mutate(DateTime = as_posix(.data$DateTime), watershed = 3L),
  
  read_csv(FILE_W9_STREAM, show_col_types = FALSE) %>%
    rename(DateTime = DATETIME) %>%
    mutate(DateTime = as_posix(.data$DateTime), watershed = 9L)
)

# ----------------------------
# UI
# ----------------------------
ui <- fluidPage(
  titlePanel("Hubbard Brook Experimental Forest: Watersheds 3 & 9"),
  
  fluidRow(
    # Left third
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
        
        # Filter/Apply button (confirm selection)
        actionButton("apply_filters", "Filter", class = "btn-primary"),
        br(), br(),
        helpText("Change selectors, then click Filter to update plots.")
      )
    ),
    
    # Right two-thirds
    column(
      width = 8,
      plotOutput("soil_plot",   height = 260),
      plotOutput("wt_plot",     height = 260),
      plotOutput("snow_plot",   height = 260),
      plotOutput("precip_plot", height = 260),
      plotOutput("stream_plot", height = 260)
    )
  )
)

# ----------------------------
# Server
# ----------------------------
server <- function(input, output, session) {
  
  # Only update plots after the user clicks Filter
  filters <- eventReactive(input$apply_filters, {
    list(
      watershed = as.integer(input$watershed),
      soil_type = input$soil_type,
      stream_metric = input$stream_metric
    )
  }, ignoreInit = FALSE)
  
  # Soil moisture (watershed + soil type)
  soil_df_long <- reactive({
    f <- filters()
    df <- read_soil_for_selection(f$watershed, f$soil_type)
    validate(need(nrow(df) > 0, "No soil moisture rows found for this selection."))
    soil_long(df)
  })
  
  output$soil_plot <- renderPlot({
    f <- filters()
    df <- soil_df_long()
    soil_label <- c(epod = "E Podzol", bhs = "Bhs", typ = "Typ")[[f$soil_type]]
    
    ggplot(df, aes(TIMESTAMP, value, group = interaction(sensor, series))) +
      geom_line() +
      facet_wrap(~ sensor, scales = "free_y", ncol = 1) +
      theme_classic() +
      labs(
        title = paste0("Soil Moisture in Watershed ", f$watershed, " (", soil_label, ")"),
        x = "",
        y = "Soil moisture (by series)"
      )
  })
  
  # Water table (watershed only)
  wt_df <- reactive({
    f <- filters()
    df <- read_wells_for_ws(f$watershed)
    validate(need(nrow(df) > 0, "No water table data found for this watershed (check files/mapping)."))
    df
  })
  
  output$wt_plot <- renderPlot({
    f <- filters()
    df <- wt_df()
    
    # Different color for each well
    ggplot(df, aes(datetime, wt_cm, color = well_id, group = well_id)) +
      geom_line() +
      theme_classic() +
      labs(
        title = paste("Water Table Depth (Wells) - Watershed", f$watershed),
        x = "",
        y = "Water Table Depth (cm)",
        color = "Well"
      )
  })
  
  # Snow depth (watershed only)
  output$snow_plot <- renderPlot({
    f <- filters()
    df <- snow_all %>% filter(watershed == f$watershed)
    validate(need(nrow(df) > 0, "No snow depth data found."))
    
    ggplot(df, aes(date, .data$`snow depth (cm)`)) +
      geom_line() +
      theme_classic() +
      labs(
        title = paste("Snow Depth in Watershed", f$watershed),
        x = "",
        y = "Snow Depth (cm)"
      )
  })
  
  # Precipitation (watershed only)
  output$precip_plot <- renderPlot({
    f <- filters()
    df <- precip_all %>% filter(watershed == f$watershed)
    validate(need(nrow(df) > 0, "No precipitation data found."))
    
    ggplot(df, aes(DateTime, precip)) +
      geom_line() +
      theme_classic() +
      labs(
        title = paste("Precipitation in Watershed", f$watershed),
        x = "",
        y = "Precipitation (cm)"
      )
  })
  
  # Streamflow (watershed + metric)
  stream_series <- reactive({
    f <- filters()
    df <- stream_all %>% filter(watershed == f$watershed)
    validate(need(nrow(df) > 0, "No streamflow data found."))
    
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