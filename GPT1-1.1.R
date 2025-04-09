# Load required libraries
library(shiny)
library(bslib)
library(echarts4r)
library(connectapi)
library(dplyr)
library(lubridate)

# Connect to the Connect server (update the URL accordingly)
connect <- connectapi::connect()

# Define UI using a modern bslib theme
ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),  # Use a modern Bootswatch theme
  titlePanel("Connect Account Usage Dashboard"),
  sidebarLayout(
    sidebarPanel(
      # Filter for date range
      dateRangeInput(
        inputId = "dateRange",
        label = "Select Date Range:",
        start = Sys.Date() - 30,
        end = Sys.Date()
      ),
      # Select input for Apps using the content name from get_content
      selectInput(
        inputId = "app",
        label = "Select App:",
        choices = NULL,
        multiple = TRUE
      ),
      # Select input for Users
      selectInput(
        inputId = "user",
        label = "Select User:",
        choices = NULL,
        multiple = TRUE
      ),
      # Numeric input to set minimum session duration (in seconds)
      numericInput(
        inputId = "minTime",
        label = "Minimum Session Time (seconds):",
        value = 0,
        min = 0
      ),
      # Button to download the filtered data as CSV
      downloadButton(
        outputId = "downloadData",
        label = "Download CSV"
      )
    ),
    mainPanel(
      # Container for the interactive chart
      echarts4rOutput("usageChart")
    )
  )
)

# Define Server logic
server_logic <- function(input, output, session) {

  # Load the usage data from RStudio Connect using get_usage_shiny()
  usage_data <- reactive({
    # This data is expected to have these columns: started, elapsed, content_name, user, etc.
    data <- connectapi::get_usage_shiny(src = connect)
    # Convert the 'started' timestamp to Date for filtering purposes
    data <- data %>% mutate(date = as.Date(started))
    data
  })

  # Populate the app filter using get_content() with the "name" column
  observe({
    content <- connectapi::get_content(src = connect)
    updateSelectInput(session, "app",
                      choices = unique(content$name))
  })

  # Populate the user filter using get_users() with the "user" column
  observe({
    users <- connectapi::get_users(src = connect)
    updateSelectInput(session, "user",
                      choices = unique(users$user))
  })

  # Reactive expression to filter the usage data based on user selections
  filtered_data <- reactive({
    req(usage_data())

    # browser()
    df <- usage_data() %>%
      mutate(elapsed = started %--% ended / dseconds()) |>
      filter(date >= input$dateRange[1],
             date <= input$dateRange[2],
             elapsed >= input$minTime)
    if (!is.null(input$app) && length(input$app) > 0) {
      df <- df %>% filter(content_name %in% input$app)
    }
    if (!is.null(input$user) && length(input$user) > 0) {
      df <- df %>% filter(user %in% input$user)
    }
    df
  })

  # Render an interactive chart using echarts4r
  output$usageChart <- renderEcharts4r({
    req(filtered_data())
    # Aggregate session times by date
    chart_data <- filtered_data() %>%
      group_by(date) %>%
      summarize(total_elapsed = sum(elapsed, na.rm = TRUE))

    chart_data %>%
      e_charts(date) %>%
      e_line(total_elapsed, name = "Total Session Time") %>%
      e_tooltip(trigger = "axis") %>%
      e_title("Total Session Time by Date")
  })

  # Download handler to export the filtered data as CSV
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("connect_usage_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
}

# Run the Shiny app
shinyApp(ui, server_logic)
