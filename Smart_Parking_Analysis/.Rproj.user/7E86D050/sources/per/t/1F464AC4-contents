# =============================================================================
#  Smart Parking Slot Analysis and Management System Using R
#  -----------------------------------------------------------------------------
#  A simple R Shiny dashboard that analyses parking-slot data stored in a CSV
#  file (data/parking_data.csv).
#
#  Packages needed (install once):  install.packages(c("shiny","dplyr","ggplot2"))
#  Run the project:                 shiny::runApp()   (from the project folder)
#                                   or open app.R in RStudio and click "Run App"
#
#  No machine learning, no internet connection needed after installing packages.
# =============================================================================

library(shiny)    # web application framework
library(dplyr)    # data processing
library(ggplot2)  # graphs


# -----------------------------------------------------------------------------
# 1. SETTINGS
# -----------------------------------------------------------------------------
DATA_FILE <- file.path("data", "parking_data.csv")

# Parking areas of the building
AREAS <- c("Ground Floor", "First Floor", "Basement", "Open Parking")

# Hourly parking rates (Rs. per hour) for every vehicle type
FEE_RATES <- c(Bike = 10, Car = 20, SUV = 30, Auto = 15)

# Columns that must be present in the CSV file
REQUIRED_COLUMNS <- c("Slot_ID", "Area", "Vehicle_Type", "Entry_Date",
                      "Entry_Time", "Exit_Time", "Duration_Hours",
                      "Status", "Parking_Fee")

# Colours used in the graphs
COL_OCCUPIED  <- "#e74c3c"
COL_AVAILABLE <- "#2ecc71"
COL_MAIN      <- "#2980b9"


# -----------------------------------------------------------------------------
# 2. HELPER FUNCTIONS
# -----------------------------------------------------------------------------

# Fee calculation using conditional logic (case_when = "if / else if / else").
# Rule: fee = hourly rate x hours, where any part of an hour is charged as a
# full hour and the minimum charge is 1 hour.
calculate_fee <- function(vehicle_type, hours) {
  rate <- case_when(
    vehicle_type == "Bike" ~ 10,
    vehicle_type == "Car"  ~ 20,
    vehicle_type == "SUV"  ~ 30,
    vehicle_type == "Auto" ~ 15,
    TRUE                   ~ NA_real_       # unknown vehicle type
  )
  billable_hours <- pmax(1, ceiling(hours))
  rate * billable_hours
}

# Convert an hour number (0-23) into text such as "6 PM"
format_hour <- function(h) {
  suffix <- ifelse(h < 12, "AM", "PM")
  h12 <- h %% 12
  h12 <- ifelse(h12 == 0, 12, h12)
  paste(h12, suffix)
}

# Format money, e.g. 39345 -> "Rs. 39,345" (the rupee sign is written as \u20b9)
format_money <- function(x) {
  paste0("\u20b9", format(round(x), big.mark = ",", scientific = FALSE))
}

# Show a friendly message instead of an empty / broken graph
check_data <- function(df) {
  validate(need(nrow(df) > 0, "No records match the selected filters."))
}


# -----------------------------------------------------------------------------
# 3. LOADING AND CLEANING THE DATA (with error handling)
# -----------------------------------------------------------------------------
load_parking_data <- function(path) {

  # (a) file must exist
  if (!file.exists(path)) {
    stop("The data file was not found: ", path,
         "\nPlease keep 'parking_data.csv' inside the 'data' folder and run ",
         "the app from the project folder.")
  }

  # (b) file must be readable
  raw <- tryCatch(
    read.csv(path, stringsAsFactors = FALSE, colClasses = "character",
             strip.white = TRUE, na.strings = c("", "NA")),
    error = function(e) stop("The CSV file could not be read: ", conditionMessage(e))
  )

  # (c) all required columns must be present
  missing_cols <- setdiff(REQUIRED_COLUMNS, names(raw))
  if (length(missing_cols) > 0) {
    stop("The CSV file is missing these columns: ",
         paste(missing_cols, collapse = ", "))
  }
  if (nrow(raw) == 0) stop("The CSV file has no records.")

  total_rows <- nrow(raw)

  # (d) convert text columns to proper date / time values
  raw <- raw %>%
    mutate(
      Entry_Date     = as.Date(Entry_Date, format = "%Y-%m-%d"),
      Entry_DateTime = as.POSIXct(paste(Entry_Date, Entry_Time),
                                  format = "%Y-%m-%d %H:%M", tz = "UTC"),
      Exit_DateTime  = as.POSIXct(paste(Entry_Date, Exit_Time),
                                  format = "%Y-%m-%d %H:%M", tz = "UTC")
    )

  # (e) remove invalid rows (bad dates, unknown area / vehicle / status)
  raw <- raw %>%
    filter(!is.na(Entry_DateTime),
           !is.na(Slot_ID),
           Area %in% AREAS,
           Vehicle_Type %in% names(FEE_RATES),
           Status %in% c("Occupied", "Completed"))

  if (nrow(raw) == 0) stop("No valid records were found in the CSV file.")

  # (f) Snapshot time = the moment the occupancy data was taken.
  #     It is the next full hour after the last vehicle entry in the file.
  snapshot_time <- as.POSIXct(trunc(max(raw$Entry_DateTime), "hours"),
                              tz = "UTC") + 3600

  # (g) A vehicle that is still parked has no exit time, so the snapshot
  #     time is used as its (temporary) exit time.
  raw <- raw %>%
    mutate(Exit_DateTime = if_else(Status == "Occupied" & is.na(Exit_DateTime),
                                   snapshot_time, Exit_DateTime))

  # (h) Calculate duration (hours) from entry and exit times, then the fee.
  parking <- raw %>%
    mutate(
      Duration_Hours = as.numeric(difftime(Exit_DateTime, Entry_DateTime,
                                           units = "hours")),
      Entry_Hour     = as.integer(format(Entry_DateTime, "%H"))
    ) %>%
    filter(!is.na(Duration_Hours), Duration_Hours > 0) %>%
    mutate(Parking_Fee = calculate_fee(Vehicle_Type, Duration_Hours))

  if (nrow(parking) == 0) stop("No records with a valid parking duration.")

  # (i) List of all parking slots (used for total / available slot counts)
  slots <- parking %>% distinct(Slot_ID, Area)

  list(data = parking, slots = slots, snapshot = snapshot_time,
       removed = total_rows - nrow(parking))
}


# -----------------------------------------------------------------------------
# 4. USER INTERFACE
# -----------------------------------------------------------------------------
app_css <- "
  body { background:#f4f6f9; }
  .app-header { background:#2c3e50; color:white; padding:14px 22px;
                margin:-15px -15px 15px -15px; }
  .app-header h2 { margin:0; font-size:24px; }
  .app-header p  { margin:4px 0 0 0; color:#bdc3c7; }
  .card-row { display:flex; gap:12px; margin-bottom:15px; flex-wrap:wrap; }
  .card { flex:1; min-width:140px; background:white; border-radius:6px;
          padding:12px 14px; border-top:4px solid #2980b9;
          box-shadow:0 1px 3px rgba(0,0,0,.15); }
  .card .card-title { color:#7f8c8d; font-size:13px; }
  .card .card-value { font-size:26px; font-weight:bold; color:#2c3e50; }
  .card.red   { border-top-color:#e74c3c; }
  .card.green { border-top-color:#2ecc71; }
  .card.orange{ border-top-color:#f39c12; }
  .card.purple{ border-top-color:#8e44ad; }
  .well { background:white; }
  .note { color:#7f8c8d; font-size:12px; }
  .insight-box { background:white; padding:10px 25px; border-radius:6px;
                 box-shadow:0 1px 3px rgba(0,0,0,.15); }
  .insight-box li { font-size:16px; padding:5px 0; }
  .section-title { margin-top:20px; color:#2c3e50; }
"

make_ui <- function(parking) {
  date_min <- min(parking$Entry_Date)
  date_max <- max(parking$Entry_Date)

  fluidPage(
    tags$head(tags$style(HTML(app_css))),

    div(class = "app-header",
        h2("Smart Parking Slot Analysis and Management System"),
        p("Parking availability, occupancy, duration, fees, peak hours and revenue")),

    sidebarLayout(
      # ---------------- Filters ----------------
      sidebarPanel(
        width = 3,
        h4("Filters"),
        dateRangeInput("date_range", "Date",
                       start = date_min, end = date_max,
                       min = date_min, max = date_max),
        selectInput("area", "Area", choices = c("All", AREAS)),
        selectInput("vehicle", "Vehicle Type", choices = c("All", names(FEE_RATES))),
        selectInput("status", "Parking Status",
                    choices = c("All", "Occupied", "Completed")),
        hr(),
        p(class = "note",
          "Occupied = vehicle is still parked at the snapshot time. ",
          "Completed = vehicle has already left. ",
          "Slot occupancy shows the vehicles parked at the snapshot time ",
          "(last day of data)."),
        p(class = "note", textOutput("snapshot_text"))
      ),

      # ---------------- Main area ----------------
      mainPanel(
        width = 9,

        # Summary cards
        div(class = "card-row",
            div(class = "card",
                div(class = "card-title", "Total Slots"),
                div(class = "card-value", textOutput("card_total", inline = TRUE))),
            div(class = "card red",
                div(class = "card-title", "Occupied Slots"),
                div(class = "card-value", textOutput("card_occupied", inline = TRUE))),
            div(class = "card green",
                div(class = "card-title", "Available Slots"),
                div(class = "card-value", textOutput("card_available", inline = TRUE))),
            div(class = "card orange",
                div(class = "card-title", "Occupancy %"),
                div(class = "card-value", textOutput("card_percent", inline = TRUE))),
            div(class = "card purple",
                div(class = "card-title", "Total Revenue"),
                div(class = "card-value", textOutput("card_revenue", inline = TRUE)))
        ),

        tabsetPanel(
          # ----- Tab 1: the seven required graphs -----
          tabPanel("Dashboard", br(),
            fluidRow(
              column(6, plotOutput("plot_status", height = 280)),
              column(6, plotOutput("plot_vehicle", height = 280))
            ),
            fluidRow(
              column(6, plotOutput("plot_area_occ", height = 280)),
              column(6, plotOutput("plot_hourly", height = 280))
            ),
            fluidRow(
              column(6, plotOutput("plot_rev_vehicle", height = 280)),
              column(6, plotOutput("plot_rev_area", height = 280))
            ),
            fluidRow(
              column(6, plotOutput("plot_duration", height = 280))
            )
          ),

          # ----- Tab 2: tables for every analysis module -----
          tabPanel("Detailed Analysis", br(),
            h4(class = "section-title", "Area / Floor Analysis"),
            tableOutput("table_area"),
            h4(class = "section-title", "Vehicle Analysis"),
            tableOutput("table_vehicle"),
            h4(class = "section-title", "Parking Duration Analysis (hours)"),
            tableOutput("table_duration"),
            h4(class = "section-title", "Peak Hour Analysis (vehicle entries)"),
            tableOutput("table_hourly"),
            h4(class = "section-title", "Revenue Analysis"),
            tableOutput("table_revenue_summary"),
            h4(class = "section-title", "Daily Revenue"),
            plotOutput("plot_daily_rev", height = 260),
            tableOutput("table_daily"),
            h4(class = "section-title", "Slot Status (Available / Occupied)"),
            tableOutput("table_slots")
          ),

          # ----- Tab 3: automatic insights -----
          tabPanel("Parking Insights", br(),
            h4("Parking Insights"),
            uiOutput("insights")
          ),

          # ----- Tab 4: fee calculator -----
          tabPanel("Fee Calculator", br(),
            h4("Parking Fee Calculator"),
            p("Fee = hourly rate x hours (part of an hour is charged as a full hour, minimum 1 hour)."),
            fluidRow(
              column(4, selectInput("calc_vehicle", "Vehicle Type", choices = names(FEE_RATES))),
              column(4, numericInput("calc_hours", "Parking duration (hours)",
                                     value = 2.5, min = 0.1, step = 0.5))
            ),
            h3(textOutput("calc_result")),
            h4(class = "section-title", "Rate Chart"),
            tableOutput("table_rates")
          ),

          # ----- Tab 5: raw records -----
          tabPanel("Data", br(),
            p(class = "note", "First 100 records matching the filters."),
            tableOutput("table_data")
          )
        )
      )
    )
  )
}


# -----------------------------------------------------------------------------
# 5. SERVER (all calculations and graphs)
# -----------------------------------------------------------------------------
make_server <- function(parking, slots, snapshot) {

  function(input, output, session) {

    # ---- 5.1 Filtered data: every output below reacts to the filters --------
    filtered <- reactive({
      req(input$date_range)
      df <- parking %>%
        filter(Entry_Date >= input$date_range[1],
               Entry_Date <= input$date_range[2])
      if (input$area    != "All") df <- df %>% filter(Area == input$area)
      if (input$vehicle != "All") df <- df %>% filter(Vehicle_Type == input$vehicle)
      if (input$status  != "All") df <- df %>% filter(Status == input$status)
      df
    })

    # ---- 5.2 Slot statistics per area (total / occupied / available) --------
    area_stats <- reactive({
      selected_areas <- if (input$area == "All") AREAS else input$area

      totals <- slots %>%
        filter(Area %in% selected_areas) %>%
        count(Area, name = "Total_Slots")

      occupied <- filtered() %>%
        filter(Status == "Occupied") %>%
        group_by(Area) %>%
        summarise(Occupied_Slots = n_distinct(Slot_ID), .groups = "drop")

      totals %>%
        left_join(occupied, by = "Area") %>%
        mutate(Occupied_Slots  = coalesce(Occupied_Slots, 0L),
               Available_Slots = Total_Slots - Occupied_Slots,
               Occupancy_Pct   = round(100 * Occupied_Slots / Total_Slots, 1)) %>%
        arrange(match(Area, AREAS))
    })

    # Overall slot numbers used by the summary cards
    overall <- reactive({
      s <- area_stats()
      total <- sum(s$Total_Slots)
      occ   <- sum(s$Occupied_Slots)
      list(total = total, occupied = occ, available = total - occ,
           pct = ifelse(total > 0, round(100 * occ / total, 1), 0))
    })

    # ---- 5.3 Other reusable summaries --------------------------------------
    vehicle_stats <- reactive({
      df <- filtered(); check_data(df)
      df %>% count(Vehicle_Type, name = "Vehicles") %>%
        mutate(Percentage = round(100 * Vehicles / sum(Vehicles), 1)) %>%
        arrange(desc(Vehicles))
    })

    duration_stats <- reactive({
      df <- filtered(); check_data(df)
      df %>% group_by(Vehicle_Type) %>%
        summarise(Vehicles = n(),
                  Average = round(mean(Duration_Hours), 2),
                  Maximum = round(max(Duration_Hours), 2),
                  Minimum = round(min(Duration_Hours), 2),
                  .groups = "drop")
    })

    # Vehicle entries for every hour of the day (hours with no entry = 0)
    hour_counts <- reactive({
      df <- filtered(); check_data(df)
      all_hours <- data.frame(Entry_Hour = seq(min(parking$Entry_Hour),
                                               max(parking$Entry_Hour)))
      all_hours %>%
        left_join(count(df, Entry_Hour, name = "Vehicles"), by = "Entry_Hour") %>%
        mutate(Vehicles = coalesce(Vehicles, 0L),
               Hour = format_hour(Entry_Hour))
    })

    revenue_by_vehicle <- reactive({
      df <- filtered(); check_data(df)
      df %>% group_by(Vehicle_Type) %>%
        summarise(Vehicles = n(), Revenue = sum(Parking_Fee),
                  Avg_Revenue = round(mean(Parking_Fee), 1), .groups = "drop") %>%
        arrange(desc(Revenue))
    })

    revenue_by_area <- reactive({
      df <- filtered(); check_data(df)
      df %>% group_by(Area) %>%
        summarise(Vehicles = n(), Revenue = sum(Parking_Fee),
                  Avg_Revenue = round(mean(Parking_Fee), 1), .groups = "drop") %>%
        arrange(match(Area, AREAS))
    })

    daily_revenue <- reactive({
      df <- filtered(); check_data(df)
      df %>% group_by(Entry_Date) %>%
        summarise(Vehicles = n(), Revenue = sum(Parking_Fee), .groups = "drop")
    })

    # ---- 5.4 Summary cards --------------------------------------------------
    output$card_total     <- renderText(overall()$total)
    output$card_occupied  <- renderText(overall()$occupied)
    output$card_available <- renderText(overall()$available)
    output$card_percent   <- renderText(paste0(overall()$pct, "%"))
    output$card_revenue   <- renderText(format_money(sum(filtered()$Parking_Fee)))
    output$snapshot_text  <- renderText(
      paste("Snapshot time:", format(snapshot, "%d-%b-%Y %I:%M %p")))

    # ---- 5.5 The seven graphs ----------------------------------------------

    # Graph 1: Occupied vs Available slots
    output$plot_status <- renderPlot({
      o <- overall()
      df <- data.frame(Status = c("Occupied", "Available"),
                       Slots  = c(o$occupied, o$available))
      ggplot(df, aes(x = Status, y = Slots, fill = Status)) +
        geom_col(width = 0.6) +
        geom_text(aes(label = Slots), vjust = -0.4, size = 5) +
        scale_fill_manual(values = c(Occupied = COL_OCCUPIED, Available = COL_AVAILABLE)) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
        labs(title = "Occupied vs Available Slots", x = NULL, y = "Number of slots") +
        theme_minimal(base_size = 13) + theme(legend.position = "none")
    })

    # Graph 2: Vehicle type distribution
    output$plot_vehicle <- renderPlot({
      df <- vehicle_stats()
      ggplot(df, aes(x = reorder(Vehicle_Type, -Vehicles), y = Vehicles)) +
        geom_col(fill = COL_MAIN, width = 0.6) +
        geom_text(aes(label = paste0(Vehicles, " (", Percentage, "%)")),
                  vjust = -0.4, size = 4) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
        labs(title = "Vehicle Type Distribution", x = "Vehicle type", y = "Vehicles") +
        theme_minimal(base_size = 13)
    })

    # Graph 3: Area-wise occupancy (percentage)
    output$plot_area_occ <- renderPlot({
      df <- area_stats()
      ggplot(df, aes(x = factor(Area, levels = AREAS), y = Occupancy_Pct)) +
        geom_col(fill = COL_OCCUPIED, width = 0.6) +
        geom_text(aes(label = paste0(Occupancy_Pct, "%")), vjust = -0.4, size = 4.5) +
        scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.1))) +
        labs(title = "Area-wise Occupancy", x = NULL, y = "Occupancy (%)") +
        theme_minimal(base_size = 13)
    })

    # Graph 4: Hourly parking trend (vehicle entries per hour)
    output$plot_hourly <- renderPlot({
      df <- hour_counts()
      ggplot(df, aes(x = Entry_Hour, y = Vehicles)) +
        geom_line(colour = COL_MAIN, linewidth = 1) +
        geom_point(colour = COL_MAIN, size = 2.5) +
        scale_x_continuous(breaks = df$Entry_Hour, labels = df$Hour) +
        labs(title = "Hourly Parking Trend", x = "Entry hour", y = "Vehicles entered") +
        theme_minimal(base_size = 13) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })

    # Graph 5: Revenue by vehicle type
    output$plot_rev_vehicle <- renderPlot({
      df <- revenue_by_vehicle()
      ggplot(df, aes(x = reorder(Vehicle_Type, -Revenue), y = Revenue)) +
        geom_col(fill = "#27ae60", width = 0.6) +
        geom_text(aes(label = format(Revenue, big.mark = ",")), vjust = -0.4, size = 4) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
        labs(title = "Revenue by Vehicle Type", x = "Vehicle type", y = "Revenue (Rs.)") +
        theme_minimal(base_size = 13)
    })

    # Graph 6: Revenue by area
    output$plot_rev_area <- renderPlot({
      df <- revenue_by_area()
      ggplot(df, aes(x = factor(Area, levels = AREAS), y = Revenue)) +
        geom_col(fill = "#8e44ad", width = 0.6) +
        geom_text(aes(label = format(Revenue, big.mark = ",")), vjust = -0.4, size = 4) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
        labs(title = "Revenue by Area", x = NULL, y = "Revenue (Rs.)") +
        theme_minimal(base_size = 13)
    })

    # Graph 7: Average parking duration by vehicle type
    output$plot_duration <- renderPlot({
      df <- duration_stats()
      ggplot(df, aes(x = reorder(Vehicle_Type, -Average), y = Average)) +
        geom_col(fill = "#f39c12", width = 0.6) +
        geom_text(aes(label = Average), vjust = -0.4, size = 4.5) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
        labs(title = "Average Parking Duration by Vehicle Type",
             x = "Vehicle type", y = "Average duration (hours)") +
        theme_minimal(base_size = 13)
    })

    # Extra graph: daily revenue
    output$plot_daily_rev <- renderPlot({
      df <- daily_revenue()
      ggplot(df, aes(x = Entry_Date, y = Revenue)) +
        geom_col(fill = "#16a085", width = 0.7) +
        geom_text(aes(label = format(Revenue, big.mark = ",")), vjust = -0.4, size = 4) +
        scale_x_date(date_labels = "%d %b", breaks = df$Entry_Date) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
        labs(title = "Daily Revenue", x = "Date", y = "Revenue (Rs.)") +
        theme_minimal(base_size = 13)
    })

    # ---- 5.6 Tables for the "Detailed Analysis" tab -------------------------
    table_opts <- list(striped = TRUE, hover = TRUE, bordered = TRUE)

    output$table_area <- renderTable({
      area_stats() %>%
        rename(`Total Slots` = Total_Slots, `Occupied Slots` = Occupied_Slots,
               `Available Slots` = Available_Slots, `Occupancy %` = Occupancy_Pct) %>%
        left_join(revenue_by_area() %>% select(Area, Revenue), by = "Area") %>%
        mutate(Revenue = coalesce(Revenue, 0))
    }, striped = TRUE, hover = TRUE, bordered = TRUE, digits = 1)

    output$table_vehicle <- renderTable({
      vehicle_stats() %>%
        rename(`Vehicle Type` = Vehicle_Type, `Number of Vehicles` = Vehicles,
               `Percentage %` = Percentage)
    }, striped = TRUE, hover = TRUE, bordered = TRUE, digits = 1)

    output$table_duration <- renderTable({
      df <- filtered(); check_data(df)
      overall_row <- data.frame(Vehicle_Type = "All vehicles", Vehicles = nrow(df),
                                Average = round(mean(df$Duration_Hours), 2),
                                Maximum = round(max(df$Duration_Hours), 2),
                                Minimum = round(min(df$Duration_Hours), 2))
      bind_rows(duration_stats(), overall_row) %>%
        rename(`Vehicle Type` = Vehicle_Type)
    }, striped = TRUE, hover = TRUE, bordered = TRUE, digits = 2)

    output$table_hourly <- renderTable({
      hour_counts() %>%
        select(Hour, `Vehicles Entered` = Vehicles)
    }, striped = TRUE, hover = TRUE, bordered = TRUE)

    output$table_revenue_summary <- renderTable({
      df <- filtered(); check_data(df)
      data.frame(
        Measure = c("Total revenue (Rs.)", "Number of vehicles",
                    "Average revenue per vehicle (Rs.)"),
        Value = c(format(sum(df$Parking_Fee), big.mark = ","),
                  format(nrow(df), big.mark = ","),
                  format(round(mean(df$Parking_Fee), 2), nsmall = 2))
      )
    }, striped = TRUE, hover = TRUE, bordered = TRUE)

    output$table_daily <- renderTable({
      daily_revenue() %>%
        mutate(Entry_Date = format(Entry_Date, "%d-%b-%Y")) %>%
        rename(Date = Entry_Date, `Revenue (Rs.)` = Revenue)
    }, striped = TRUE, hover = TRUE, bordered = TRUE)

    # Status of every slot at the snapshot time: Available or Occupied
    output$table_slots <- renderTable({
      selected_areas <- if (input$area == "All") AREAS else input$area
      parked_now <- filtered() %>%
        filter(Status == "Occupied") %>%
        distinct(Slot_ID, .keep_all = TRUE) %>%
        select(Slot_ID, Vehicle_Type, Entry_Time)

      slots %>%
        filter(Area %in% selected_areas) %>%
        left_join(parked_now, by = "Slot_ID") %>%
        mutate(`Slot Status` = ifelse(is.na(Vehicle_Type), "Available", "Occupied"),
               Vehicle_Type = coalesce(Vehicle_Type, "-"),
               Entry_Time   = coalesce(Entry_Time, "-")) %>%
        arrange(match(Area, AREAS), Slot_ID) %>%
        rename(`Slot ID` = Slot_ID, `Vehicle Type` = Vehicle_Type,
               `Entry Time` = Entry_Time)
    }, striped = TRUE, hover = TRUE, bordered = TRUE)

    # ---- 5.7 Parking Insights (calculated from the data, not hard-coded) ----
    output$insights <- renderUI({
      df <- filtered(); check_data(df)
      st <- area_stats()
      hc <- hour_counts()
      vs <- vehicle_stats()
      ds <- duration_stats()
      rv <- revenue_by_vehicle()
      ra <- revenue_by_area()
      dr <- daily_revenue()

      # Area with the highest occupancy
      top_area <- st %>% arrange(desc(Occupancy_Pct)) %>% slice(1)
      area_text <- if (top_area$Occupancy_Pct == 0) {
        "No slots are occupied for the selected filters."
      } else {
        paste0(top_area$Area, " has the highest occupancy (",
               top_area$Occupancy_Pct, "%).")
      }

      peak_hour  <- hc$Hour[which.max(hc$Vehicles)]
      quiet_hour <- hc$Hour[which.min(hc$Vehicles)]

      insights <- c(
        area_text,
        paste0(vs$Vehicle_Type[1], "s are the most common vehicle type (",
               vs$Percentage[1], "% of vehicles)."),
        paste0("Peak parking time is ", peak_hour, " (",
               max(hc$Vehicles), " vehicles entered)."),
        paste0("Least busy parking hour is ", quiet_hour, " (",
               min(hc$Vehicles), " vehicles entered)."),
        paste0("Total parking revenue is ", format_money(sum(df$Parking_Fee)), "."),
        paste0("Average parking duration is ",
               round(mean(df$Duration_Hours), 2), " hours (maximum ",
               round(max(df$Duration_Hours), 2), " hours, minimum ",
               round(min(df$Duration_Hours), 2), " hours)."),
        paste0(ds$Vehicle_Type[which.max(ds$Average)],
               " vehicles stay the longest on average (",
               max(ds$Average), " hours)."),
        paste0(rv$Vehicle_Type[1], " earns the highest revenue (",
               format_money(rv$Revenue[1]), ")."),
        paste0(ra$Area[which.max(ra$Revenue)], " earns the highest revenue among areas (",
               format_money(max(ra$Revenue)), ")."),
        paste0("Average revenue per vehicle is ", format_money(mean(df$Parking_Fee)), "."),
        paste0("The highest daily revenue was on ",
               format(dr$Entry_Date[which.max(dr$Revenue)], "%d %B %Y"),
               " (", format_money(max(dr$Revenue)), ").")
      )

      div(class = "insight-box", tags$ul(lapply(insights, tags$li)))
    })

    # ---- 5.8 Fee calculator -------------------------------------------------
    output$calc_result <- renderText({
      validate(need(is.numeric(input$calc_hours) && !is.na(input$calc_hours) &&
                      input$calc_hours > 0, "Please enter a valid number of hours."))
      fee <- calculate_fee(input$calc_vehicle, input$calc_hours)
      paste0("Parking fee: ", format_money(fee))
    })

    output$table_rates <- renderTable({
      data.frame(`Vehicle Type` = names(FEE_RATES),
                 `Rate (Rs. per hour)` = as.numeric(FEE_RATES),
                 check.names = FALSE)
    }, striped = TRUE, hover = TRUE, bordered = TRUE)

    # ---- 5.9 Raw data tab ---------------------------------------------------
    output$table_data <- renderTable({
      df <- filtered(); check_data(df)
      df %>%
        head(100) %>%
        mutate(Entry_Date = format(Entry_Date, "%Y-%m-%d"),
               Duration_Hours = round(Duration_Hours, 2)) %>%
        select(Slot_ID, Area, Vehicle_Type, Entry_Date, Entry_Time, Exit_Time,
               Duration_Hours, Status, Parking_Fee) %>%
        mutate(Exit_Time = coalesce(Exit_Time, "-"))
    }, striped = TRUE, hover = TRUE, bordered = TRUE)
  }
}


# -----------------------------------------------------------------------------
# 6. START THE APPLICATION
#    If the data cannot be loaded, a clear error page is shown instead of
#    crashing the app.
# -----------------------------------------------------------------------------
loaded <- tryCatch(load_parking_data(DATA_FILE), error = function(e) e)

if (inherits(loaded, "error")) {
  ui <- fluidPage(
    h2("Smart Parking Analysis - Data Error"),
    div(style = "color:#c0392b; font-size:16px; white-space:pre-wrap;",
        conditionMessage(loaded))
  )
  server <- function(input, output, session) {}
} else {
  if (loaded$removed > 0) {
    message(loaded$removed, " invalid record(s) were ignored while loading the data.")
  }
  ui     <- make_ui(loaded$data)
  server <- make_server(loaded$data, loaded$slots, loaded$snapshot)
}

shinyApp(ui = ui, server = server)
