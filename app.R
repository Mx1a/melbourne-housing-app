# ---- Load packages ----
library(shiny)
library(tidyverse)
library(bslib)

# ---- Read in and clean data ----
housing <- read_csv("Melbourne_housing_FULL.csv",
                    na = c("", "NA", "#N/A"))
# ---- Copying cleaning code from data exploration ----

housing_clean <- housing |>
  filter(!is.na(Price), !is.na(Regionname)) |>
  mutate(
    Type = factor(Type,
                  levels = c("h", "t", "u"),
                  labels = c("House", "Townhouse", "Unit")),
    Method = case_when(
      Method %in% c("S", "SP", "SA", "SS", "PN", "SN") ~ "Sold",
      Method == "PI" ~ "Passed In",
      Method == "VB" ~ "Vendor Bid",
      Method == "W"  ~ "Withdrawn"
    ),
    Method = factor(Method),
    Regionname = if_else(
      Regionname %in% c("Eastern Victoria", "Northern Victoria", "Western Victoria"),
      "Regional Victoria",
      Regionname
    ),
    Regionname = factor(Regionname),
    Date = dmy(Date),
    SaleYear = year(Date)
  ) |>
  select(Suburb, Type, Method, Regionname, CouncilArea, Date, SaleYear,
         Price, Rooms, Bathroom, Car, Distance, Landsize, BuildingArea,
         YearBuilt, Lattitude, Longtitude)

# ---- setting up choices for the sidebar ----

num_vars <- c("Price" = "Price",
              "Distance from CBD (km)" = "Distance",
              "Rooms" = "Rooms",
              "Bathrooms" = "Bathroom",
              "Car Spaces" = "Car",
              "Land Size (sq m)" = "Landsize",
              "Building Area (sq m)" = "BuildingArea",
              "Year Built" = "YearBuilt")

# ---- UI ----
ui <- fluidPage(
  theme = bs_theme(bootswatch = "darkly"),
  titlePanel("Melbourne Housing Explorer"),
  sidebarLayout(
    sidebarPanel(
      h4("Subset the Data"),
      checkboxGroupInput("type", "Property Type",
                         choices = levels(housing_clean$Type),
                         selected = levels(housing_clean$Type)),
      checkboxGroupInput("region", "Region",
                         choices = levels(housing_clean$Regionname),
                         selected = levels(housing_clean$Regionname)),
      selectInput("num_var1", "First Numeric Variable",
                  choices = num_vars, selected = "Price"),
      uiOutput("slider1"),
      selectInput("num_var2", "Second Numeric Variable",
                  choices = num_vars, selected = "Distance"),
      uiOutput("slider2"),
      actionButton("apply", "Apply Filters", class = "btn-primary")
    ),
    mainPanel(
      textOutput("n_rows"),
      tabsetPanel(
        tabPanel("About",
                          h3("About This App"),
                          p("This app is meant to explore home sales in Melbourne, Australia from the year 2016 to 2018.
    You can filter the data, using the sidebar to the left. Additionally you can view and download the data using the tabs above. This gives you a way to create your own tables and
    plots to see how prices relate to location, property type, and home features."),
                          
                          h4("Where the Data Comes From"),
                          p("The data comes from the Melbourne Housing Market dataset on Kaggle. Each row is one home sale and
    includes the price, property type, region, distance from the city center,
    number of rooms, land size, and more. Sales without a recorded price were removed."),
                          tags$a(href = "https://www.kaggle.com/datasets/anthonypino/melbourne-housing-market",
                                 "More information about the data on Kaggle", target = "_blank"),
                          
                          h4("How to Use the App"),
                          tags$ul(
                            tags$li(strong("Sidebar: "), "Choose property types, regions, and ranges for two
            numeric variables, then click Apply Filters. The data only updates when
            you click the button."),
                            tags$li(strong("Data Download: "), "View the filtered data in a table and save it
            as a CSV file."),
                            tags$li(strong("Data Exploration: "), "Create contingency tables, numeric summaries,
            and plots of the filtered data, choosing which variables to display.")
                          ),
                          
                          tags$img(src = "melbourne.jpg", width = "80%")
        ),
        tabPanel("Data Download"),
        tabPanel("Data Exploration")
      )
    )
  )
) 
# ---- Server ----
server <- function(input, output, session) {
  
  # Dynamic slider for the first numeric variable
  output$slider1 <- renderUI({
    rng <- range(housing_clean[[input$num_var1]], na.rm = TRUE)
    sliderInput("range1", paste("Range of", names(num_vars)[num_vars == input$num_var1]),
                min = rng[1], max = rng[2], value = rng,ticks = FALSE)
  })
  
  # Dynamic slider for the second numeric variable
  output$slider2 <- renderUI({
    rng <- range(housing_clean[[input$num_var2]], na.rm = TRUE)
    sliderInput("range2", paste("Range of", names(num_vars)[num_vars == input$num_var2]),
                min = rng[1], max = rng[2], value = rng,ticks = FALSE)
  })
  
  # Store the data; start with the full dataset
  rv <- reactiveValues(data = housing_clean)
  
  # Only update the data when the button is pressed
  observeEvent(input$apply, {
    rv$data <- housing_clean |>
      filter(Type %in% input$type,
             Regionname %in% input$region,
             between(.data[[input$num_var1]], input$range1[1], input$range1[2]),
             between(.data[[input$num_var2]], input$range2[1], input$range2[2]))
  })
  
  # Temporary check: how many rows are selected
  output$n_rows <- renderText({
    paste("Showing", nrow(rv$data), "of", nrow(housing_clean), "homes")
  })
}

# ---- Run the app ----
shinyApp(ui = ui, server = server)

