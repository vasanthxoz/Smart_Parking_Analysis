# Smart Parking Slot Analysis and Management System Using R

A college-level R programming project. It analyses parking-slot data and shows parking availability, occupancy, vehicle types, parking duration, fees, peak hours and revenue in an interactive **R Shiny** dashboard.

No machine learning is used, and no internet connection is needed after the packages are installed.

## Folder Structure

```
Smart_Parking_Analysis/
├── app.R                      # complete Shiny application
├── data/
│   └── parking_data.csv       # dataset (574 records)
├── README.md
└── report/
    └── project_report.docx    # project report
```

## Requirements

- R (version 4.0 or later recommended)
- RStudio (optional, but makes running easier)
- R packages: `shiny`, `dplyr`, `ggplot2`

## Installation

Open R or RStudio and run this once (internet needed only for this step):

```r
install.packages(c("shiny", "dplyr", "ggplot2"))
```

## Running the Project

**Option 1 – RStudio:** open `app.R` and click **Run App**.

**Option 2 – R console:**

```r
setwd("path/to/Smart_Parking_Analysis")   # the project folder
shiny::runApp()
```

The app loads `data/parking_data.csv` automatically. Always run it from the project folder, otherwise the data file will not be found (the app then shows a clear error message).

## Dataset (`data/parking_data.csv`)

| Column | Meaning |
|---|---|
| Slot_ID | Parking slot code (G = Ground Floor, F = First Floor, B = Basement, O = Open Parking) |
| Area | Parking area |
| Vehicle_Type | Bike, Car, SUV or Auto |
| Entry_Date | Date the vehicle entered (YYYY-MM-DD) |
| Entry_Time | Entry time (HH:MM, 24-hour) |
| Exit_Time | Exit time (blank if the vehicle is still parked) |
| Duration_Hours | Parking duration in hours |
| Status | `Occupied` = still parked at the snapshot time, `Completed` = vehicle has left |
| Parking_Fee | Fee in rupees |

- 60 slots (15 in each area), 7 days of records (21–27 September 2026).
- A slot is shown as **Available** when no vehicle with status `Occupied` is parked in it.
- The *snapshot time* is the next full hour after the last entry in the file. Vehicles that are still parked use this time to calculate their duration and fee so far.
- The app recalculates duration (from entry and exit times) and fee when loading, so you can edit the CSV and the dashboard stays consistent.

## Fee Rules

| Vehicle | Rate |
|---|---|
| Bike | ₹10 / hour |
| Car | ₹20 / hour |
| SUV | ₹30 / hour |
| Auto | ₹15 / hour |

Fee = rate × hours. Any part of an hour is charged as a full hour (minimum 1 hour).

## Dashboard Features

- **Summary cards:** Total Slots, Occupied Slots, Available Slots, Occupancy %, Total Revenue
- **7 graphs:** Occupied vs Available, Vehicle Type Distribution, Area-wise Occupancy, Hourly Parking Trend, Revenue by Vehicle Type, Revenue by Area, Average Duration by Vehicle Type
- **Filters:** Date range, Area, Vehicle Type, Parking Status – all cards, graphs, tables and insights update automatically
- **Detailed Analysis tab:** area, vehicle, duration (average / max / min), peak-hour, revenue and daily-revenue tables, plus the Available/Occupied status of every slot
- **Parking Insights tab:** statements generated from the data (highest-occupancy area, most common vehicle, peak hour, total revenue, average duration, etc.)
- **Fee Calculator tab:** calculates the fee for any vehicle type and duration
- **Data tab:** shows the records that match the filters

## Error Handling

The app checks that the CSV exists, can be read, contains all required columns and has valid dates, areas, vehicle types and statuses. Invalid rows are skipped, and if the file cannot be used a readable error page is displayed instead of crashing.

## Using Your Own Data

Replace `data/parking_data.csv` with a file that has the same column names and formats. Use the area names *Ground Floor, First Floor, Basement, Open Parking* and the vehicle types *Bike, Car, SUV, Auto*.
