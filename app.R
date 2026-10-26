# ---- Load packages ----
library(shiny)
library(tidyverse)
library(bslib)
library(thematic)
library(shinycssloaders)

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
    SaleYear = year(Date),
    YearBuilt = if_else(YearBuilt < 1830, NA, YearBuilt)
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

cat_vars <- c("Property Type" = "Type",
              "Region" = "Regionname",
              "Sale Method" = "Method")

# Look up the friendly label for any variable (used in plot titles and axes)
all_vars <- c(num_vars, cat_vars)
get_label <- function(var) {
  names(all_vars)[all_vars == var]
}

# Halloween colors for plots
spooky_colors <- c("#FF7518", "#9B59B6", "#6BCB77", "#F4D35E", "#E63946", "#4ECDC4")

# ---- UI ----
ui <- fluidPage(
  theme = bs_theme(
    bootswatch = "darkly",
    bg = "#1a1a1a",
    fg = "#f2f2f2",
    primary = "#FF7518",
    heading_font = font_google("Creepster")
  ),
  titlePanel("Spooky Melbourne Housing Explorer 🎃"),
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
                 h3("Enter If You Dare"),
                 p("Welcome, brave explorer. This app lets you wander through home sales in
    Melbourne, Australia from 2016 to 2018. Use the sidebar on the left to summon
    only the homes you want, then visit the tabs above to view and download the
    data or conjure your own tables and plots. See how prices relate to location,
    property type, and home features... if you dare."),
                 
                 h4("Where the Data Was Dug Up"),
                 p("The data comes from the Melbourne Housing Market dataset on Kaggle. Each row
    is one home sale and includes the price, property type, region, distance from
    the city center, number of rooms, land size, and more. Sales without a recorded
    price have vanished (they were removed), along with a few impossible build
    years, like a home supposedly built in 1196."),
                 tags$a(href = "https://www.kaggle.com/datasets/anthonypino/melbourne-housing-market",
                        "More information about the data on Kaggle", target = "_blank"),
                 
                 h4("How to Use the App"),
                 tags$ul(
                   tags$li(strong("Sidebar: "), "Choose property types, regions, and ranges for two
            numeric variables, then click Apply Filters. Nothing stirs until you
            click the button."),
                   tags$li(strong("Data Download: "), "View the filtered data in a table and take it
            home with you as a CSV file."),
                   tags$li(strong("Data Exploration: "), "Conjure contingency tables, numeric
            summaries, and plots of the filtered data, choosing which variables
            to display.")
                 ),
                 
                 tags$img(src = "melbourne.jpg", width = "80%")
        ),
        tabPanel("Data Download",
                 br(),
                 downloadButton("download_data", "Download Data as CSV"),
                 br(), br(),
                 DT::dataTableOutput("data_table")
        ),
        tabPanel("Data Exploration",
                          br(),
                 radioButtons("summary_type", "What would you like to explore?",
                              choices = c("Categorical Summaries", "Numeric Summaries")),
                          
                          # Shown only when "Categorical Summaries" is selected
                 conditionalPanel(
                   condition = "input.summary_type == 'Categorical Summaries'",
                   fluidRow(
                     column(10, offset = 1,
                            h4("Bar Chart"),
                            withSpinner(plotOutput("bar_plot"), color = "#FF7518")
                     )
                   ),
                   br(),
                   fluidRow(
                     column(4,
                            selectInput("cat_var1", "Categorical Variable", choices = cat_vars),
                            selectInput("cat_var2", "Second Categorical Variable (optional)",
                                        choices = c("None" = "None", cat_vars))
                     ),
                     column(8,
                            h4("Contingency Table"),
                            tableOutput("cont_table")
                     )
                   )
                 ),
                          
                          # Shown only when "Numeric Summaries" is selected 
                 conditionalPanel(
                   condition = "input.summary_type == 'Numeric Summaries'",
                   fluidRow(
                     column(10, offset = 1,
                            h4("Plot"),
                            withSpinner(plotOutput("num_plot", height = "500px"), color = "#FF7518")
                     )
                   ),
                   br(),
                   fluidRow(
                     column(4,
                            selectInput("num_y", "Numeric Variable", choices = num_vars, selected = "Price"),
                            selectInput("group_var", "Summarize Across", choices = cat_vars),
                            selectInput("plot_type", "Plot Type",
                                        choices = c("Histogram", "Boxplot", "Scatter Plot", "Heatmap")),
                            conditionalPanel(
                              condition = "input.plot_type == 'Scatter Plot'",
                              selectInput("scatter_x", "X-Axis Variable", choices = num_vars,
                                          selected = "Distance")
                            ),
                            selectInput("color_var", "Color By", choices = cat_vars),
                            selectInput("facet_var", "Facet By", choices = c("None" = "None", cat_vars))
                     ),
                     column(8,
                            h4("Summary Statistics"),
                            tableOutput("num_summary")
                     )
                   )
                 )
        )
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
                min = rng[1], max = rng[2], value = rng,ticks = FALSE,
                sep = if (input$num_var1 == "YearBuilt") "" else ",")
  })
  
  # Dynamic slider for the second numeric variable
  output$slider2 <- renderUI({
    rng <- range(housing_clean[[input$num_var2]], na.rm = TRUE)
    sliderInput("range2", paste("Range of", names(num_vars)[num_vars == input$num_var2]),
                min = rng[1], max = rng[2], value = rng,ticks = FALSE,
                sep = if (input$num_var2 == "YearBuilt") "" else ",")
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
  # Data table of the filtered data
  output$data_table <- DT::renderDataTable({
    rv$data
  })
  
  # Download the filtered data as a CSV
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("melbourne_housing_subset_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(rv$data, file)
    }
  )
  # Contingency table: one-way or two-way
  output$cont_table <- renderTable({
    validate(need(nrow(rv$data) > 0,
                  "No homes match your filters. Try widening your selections."))
    
    if (input$cat_var2 == "None") {
      rv$data |>
        count(.data[[input$cat_var1]], name = "Count")
    } else {
      validate(need(input$cat_var1 != input$cat_var2,
                    "Please choose two different categorical variables."))
      rv$data |>
        count(.data[[input$cat_var1]], .data[[input$cat_var2]]) |>
        pivot_wider(names_from = all_of(input$cat_var2),
                    values_from = n, values_fill = 0)
    }
  })
  
  # Bar chart: plain or side-by-side
  output$bar_plot <- renderPlot({
    validate(need(nrow(rv$data) > 0,
                  "No homes match your filters. Try widening your selections."))
    
    if (input$cat_var2 == "None") {
      ggplot(rv$data, aes(x = .data[[input$cat_var1]])) +
        geom_bar(fill = "#FF7518") +
        labs(title = paste("Number of Sales by", get_label(input$cat_var1)),
             x = get_label(input$cat_var1), y = "Number of Sales") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              text = element_text(size = 15))
    } else {
      validate(need(input$cat_var1 != input$cat_var2,
                    "Please choose two different categorical variables."))
      ggplot(rv$data, aes(x = .data[[input$cat_var1]],
                          fill = .data[[input$cat_var2]])) +
        geom_bar(position = "dodge") + scale_fill_manual(values = spooky_colors) +
        labs(title = paste("Number of Sales by", get_label(input$cat_var1),
                           "and", get_label(input$cat_var2)),
             x = get_label(input$cat_var1), y = "Number of Sales",
             fill = get_label(input$cat_var2)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              text = element_text(size = 15))
    }
  })
  
  # Numeric summaries across levels of a categorical variable
    
    
    # Numeric summaries across levels of a categorical variable
    output$num_summary <- renderTable({
      validate(need(nrow(rv$data) > 0,
                    "No homes match your filters. Try widening your selections."))
      y_var <- input$num_y
      
      rv$data |>
        group_by(.data[[input$group_var]]) |>
        summarize(
          N = sum(!is.na(.data[[y_var]])),
          Mean = mean(.data[[y_var]], na.rm = TRUE),
          Median = median(.data[[y_var]], na.rm = TRUE),
          SD = sd(.data[[y_var]], na.rm = TRUE),
          Min = min(.data[[y_var]], na.rm = TRUE),
          Max = max(.data[[y_var]], na.rm = TRUE)
        )
    }, digits = 1, format.args = list(big.mark = ","))

  # Numeric plots
  output$num_plot <- renderPlot({
    validate(need(nrow(rv$data) > 0,
                  "No homes match your filters. Try widening your selections."))
    
    # Heatmap uses a color gradient instead of the spooky colors
    
    if (input$plot_type == "Heatmap") {
      heat_data <- rv$data |>
        filter(Rooms <= 6) |>
        group_by(.data[[input$group_var]], Rooms) |>
        summarize(median_value = median(.data[[input$num_y]], na.rm = TRUE),
                  .groups = "drop")
      
      heat_plot <- ggplot(heat_data, aes(x = factor(Rooms),
                                         y = .data[[input$group_var]],
                                         fill = median_value)) +
        geom_tile(color = "white") +
        scale_fill_gradient(low = "#2E0854", high = "#FF7518",
                            labels = scales::label_comma()) +
        labs(title = paste("Median", get_label(input$num_y), "by",
                           get_label(input$group_var), "and Number of Rooms"),
             x = "Number of Rooms", y = get_label(input$group_var),
             fill = paste("Median", get_label(input$num_y))) +
        theme(text = element_text(size = 15))
      
      return(heat_plot)
    }
    # Step 1: make the basic plot for the chosen type
    if (input$plot_type == "Histogram") {
      p <- ggplot(rv$data, aes(x = .data[[input$num_y]],
                               fill = .data[[input$color_var]])) +
        geom_histogram(bins = 40, alpha = 0.6, position = "identity") +
        labs(x = get_label(input$num_y), y = "Count")
      
    } else if (input$plot_type == "Boxplot") {
      p <- ggplot(rv$data, aes(x = .data[[input$group_var]],
                               y = .data[[input$num_y]],
                               fill = .data[[input$color_var]])) +
        geom_boxplot() +
        labs(x = get_label(input$group_var), y = get_label(input$num_y))
      
    } else {
      validate(need(input$scatter_x != input$num_y,
                    "Please choose two different numeric variables."))
      p <- ggplot(rv$data, aes(x = .data[[input$scatter_x]],
                               y = .data[[input$num_y]],
                               color = .data[[input$color_var]])) +
        geom_point(alpha = 0.4) +
        labs(x = get_label(input$scatter_x), y = get_label(input$num_y))
    }
    
    # Step 2: add facets if the user picked a facet variable
    if (input$facet_var != "None") {
      p <- p + facet_wrap(vars(.data[[input$facet_var]]))
    }
    # Step 3: add colors, title, and text size (same for every plot)
    p +
      scale_fill_manual(values = spooky_colors) +
      scale_color_manual(values = spooky_colors) +
      labs(title = paste(input$plot_type, "of", get_label(input$num_y)),
           fill = get_label(input$color_var),
           color = get_label(input$color_var)) +
      theme(text = element_text(size = 15),
            axis.text.x = element_text(angle = 45, hjust = 1))
  })
}
# ---- Run the app ----
thematic_shiny()
shinyApp(ui = ui, server = server)

