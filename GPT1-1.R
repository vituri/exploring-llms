# Load required libraries
library(shiny)
library(bslib)
library(echarts4r)
library(connectapi)   # Ensure the connectapi package is installed
library(dplyr)
library(lubridate)

# Connect to your Connect server (update the URL accordingly)
server <- connectapi::connect()

# Define UI with a modern bslib theme
ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),  # Use a modern, flat Bootswatch theme
  titlePanel("Connect Account Usage Dashboard"),
  sidebarLayout(
    sidebarPanel(
      # Date range filter for the usage data
      dateRangeInput(
        inputId = "dateRange",
        label = "Select Date Range:",
        start = Sys.Date() - 30,
        end = Sys.Date()
      ),
      # Select input for apps retrieved from connectapi::get_content
      selectInput(
        inputId = "app",
        label = "Select App:",
        choices = NULL,
        multiple = TRUE
      ),
      # Select input for users retrieved from connectapi::get_users
      selectInput(
        inputId = "user",
        label = "Select User:",
        choices = NULL,
        multiple = TRUE
      ),
      # Numeric input to set the minimum session time (in seconds)
      numericInput(
        inputId = "minTime",
        label = "Minimum Session Time (seconds):",
        value = 0,
        min = 0
      ),
      # Download button to export the filtered data as CSV
      downloadButton(
        outputId = "downloadData",
        label = "Download CSV"
      )
    ),
    mainPanel(
      # Output container for the echarts4r chart
      echarts4rOutput("usageChart")
    )
  )
)

# Define Server logic
server_logic <- function(input, output, session) {

  # Reactive: load raw usage data using Connect API with src argument
  usage_data <- reactive({
    # Fetch usage data; assumes the returned data frame contains:
    # - a "datetime" column (convertible to Date),
    # - a "session_time" column in seconds,
    # - an "app_name" column,
    # - and a "user" column.
    data <- connectapi::get_usage_shiny(src = server)

    # Convert datetime column to Date (adjust column name if needed)
    data <- data %>% mutate(date = as.Date(datetime))

    data
  })

  # Populate app selection choices using connectapi::get_content
  observe({
    apps <- connectapi::get_content(src = server)
    # Assume the apps object has an "app_name" column; adjust if needed.
    updateSelectInput(session, "app",
                      choices = unique(apps$app_name))
  })

  # Populate user selection choices using connectapi::get_users
  observe({
    users <- connectapi::get_users(src = server)
    # Assume the users object has a "user" column; adjust if needed.
    updateSelectInput(session, "user",
                      choices = unique(users$user))
  })

  # Reactive: Filter the usage data based on user-provided inputs
  filtered_data <- reactive({
    req(usage_data())
    df <- usage_data()

    # Apply filters: date range and minimum session time.
    df <- df %>%
      filter(date >= input$dateRange[1],
             date <= input$dateRange[2],
             session_time >= input$minTime)

    # Filter by selected apps, if applicable
    if (!is.null(input$app) && length(input$app) > 0) {
      df <- df %>% filter(app_name %in% input$app)
    }

    # Filter by selected users, if applicable
    if (!is.null(input$user) && length(input$user) > 0) {
      df <- df %>% filter(user %in% input$user)
    }

    df
  })

  # Render an interactive chart using echarts4r
  output$usageChart <- renderEcharts4r({
    req(filtered_data())

    # Summarize total session_time by date for charting
    chart_data <- filtered_data() %>%
      group_by(date) %>%
      summarize(total_session_time = sum(session_time, na.rm = TRUE))

    chart_data %>%
      e_charts(date) %>%
      e_line(total_session_time, name = "Total Session Time") %>%
      e_tooltip(trigger = "axis") %>%
      e_title("Total Session Time by Date")
  })

  # Download handler to export the filtered usage data as a CSV file
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("connect_usage_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
}

# Run the Shiny app using the server logic defined above
shinyApp(ui, server_logic)
