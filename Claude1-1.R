# app.R

library(shiny)
library(bslib)
library(dplyr)
library(lubridate)
library(echarts4r)
library(connectapi)
library(DT)
library(htmltools)

# Set echarts4r theme
echarts4r::e_common(
  font_family = "Inter, -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto",
  theme = list(
    color = c("#4472CA", "#5E7CE2", "#36A5DB", "#0078D7", "#83C5BE", "#264653"),
    backgroundColor = "#ffffff",
    textStyle = list(color = "#2a2a2a"),
    title = list(textStyle = list(color = "#2a2a2a", fontWeight = "bold")),
    line = list(
      smooth = TRUE,
      width = 3
    ),
    splitLine = list(
      lineStyle = list(
        color = "#f5f5f5"
      )
    )
  )
)

ui <- page_sidebar(
  title = "Connect Usage Analytics",
  theme = bs_theme(
    version = 5,
    bootswatch = "default",
    primary = "#0078D7",
    "enable-rounded" = TRUE,
    "font-size-base" = "0.95rem",
    bg = "#ffffff",
    fg = "#2a2a2a"
  ),

  sidebar = sidebar(
    width = 300,
    bg = "#f8f9fa",

    # Connect configuration
    tags$div(
      class = "p-3 mb-3 bg-white rounded shadow-sm",
      h5("Connect Configuration", class = "mb-3"),
      textInput("connect_server", "Connect Server URL", value = ""),
      passwordInput("api_key", "API Key", value = ""),
      actionButton("connect_btn", "Connect", class = "btn-primary w-100 mb-2"),
      textOutput("connection_status")
    ),

    # Filters
    tags$div(
      class = "p-3 mb-3 bg-white rounded shadow-sm",
      h5("Filters", class = "mb-3"),
      dateRangeInput("date_range", "Date Range",
                     start = Sys.Date() - 30,
                     end = Sys.Date()),
      selectizeInput("app_filter", "Filter by App", choices = NULL, multiple = TRUE),
      selectizeInput("user_filter", "Filter by User", choices = NULL, multiple = TRUE),
      sliderInput("min_duration", "Minimum Session Duration (sec)",
                  min = 0, max = 300, value = 0, step = 5)
    ),
    actionButton("refresh_btn", "Refresh Data", class = "btn-outline-primary w-100")
  ),

  layout_column_wrap(
    width = 1/2,
    fillable = TRUE,

    # Usage Over Time chart
    card(
      full_screen = TRUE,
      card_header("Usage Over Time"),
      echarts4rOutput("usage_time_chart", height = "300px")
    ),

    # User Sessions Distribution
    card(
      full_screen = TRUE,
      card_header("User Sessions Distribution"),
      echarts4rOutput("user_sessions_chart", height = "300px")
    )
  ),

  layout_column_wrap(
    width = 1/2,
    fillable = TRUE,

    # Session Duration Distribution
    card(
      full_screen = TRUE,
      card_header("Session Duration Distribution"),
      echarts4rOutput("duration_chart", height = "300px")
    ),

    # Top Apps by Usage
    card(
      full_screen = TRUE,
      card_header("Top Apps by Usage"),
      echarts4rOutput("top_apps_chart", height = "300px")
    )
  ),

  card(
    full_screen = TRUE,
    card_header(
      "Raw Data",
      right = downloadButton("download_csv", "Download CSV", class = "btn-sm btn-outline-primary")
    ),
    DTOutput("raw_data_table")
  )
)

server <- function(input, output, session) {
  # Reactive values
  values <- reactiveValues(
    connected = FALSE,
    client = NULL,
    usage_data = NULL,
    filtered_data = NULL,
    apps = NULL,
    users = NULL
  )

  # Connect to Connect server
  observeEvent(input$connect_btn, {
    req(input$connect_server, input$api_key)

    tryCatch({
      client <- connectapi::connect(
        server = input$connect_server,
        api_key = input$api_key
      )

      # Test the connection
      content_test <- connectapi::get_content(client, limit = 1)

      values$client <- client
      values$connected <- TRUE
      output$connection_status <- renderText("Connected successfully!")

      # Initialize data
      refreshData()

    }, error = function(e) {
      values$connected <- FALSE
      output$connection_status <- renderText(paste("Connection error:", e$message))
    })
  })

  # Refresh data function
  refreshData <- function() {
    req(values$connected, values$client)

    # Get usage data
    withProgress(message = "Fetching usage data...", {
      values$usage_data <- connectapi::get_usage_shiny(values$client)
    })

    # Get apps and users for filters
    withProgress(message = "Fetching apps and users...", {
      values$apps <- connectapi::get_content(values$client)
      values$users <- connectapi::get_users(values$client)
    })

    # Update select inputs
    updateSelectizeInput(session, "app_filter",
                         choices = c("All" = "", setNames(values$apps$guid, values$apps$title)))

    updateSelectizeInput(session, "user_filter",
                         choices = c("All" = "", setNames(values$users$guid, values$users$username)))

    # Apply filters
    applyFilters()
  }

  # Refresh button handler
  observeEvent(input$refresh_btn, {
    refreshData()
  })

  # Apply filters when inputs change
  observeEvent(list(input$date_range, input$app_filter, input$user_filter, input$min_duration), {
    applyFilters()
  })

  # Filter application
  applyFilters <- function() {
    req(values$usage_data)

    filtered <- values$usage_data

    # Date range filter
    if (!is.null(input$date_range)) {
      filtered <- filtered %>%
        filter(started >= as.POSIXct(input$date_range[1]) &
                 started <= as.POSIXct(input$date_range[2] + 1))
    }

    # App filter
    if (!is.null(input$app_filter) && length(input$app_filter) > 0) {
      filtered <- filtered %>%
        filter(content_guid %in% input$app_filter)
    }

    # User filter
    if (!is.null(input$user_filter) && length(input$user_filter) > 0) {
      filtered <- filtered %>%
        filter(user_guid %in% input$user_filter)
    }

    # Min duration filter
    if (!is.null(input$min_duration)) {
      filtered <- filtered %>%
        filter(duration >= input$min_duration)
    }

    values$filtered_data <- filtered
  }

  # Usage Over Time chart
  output$usage_time_chart <- renderEcharts4r({
    req(values$filtered_data)

    # Prepare data - daily aggregation
    daily_usage <- values$filtered_data %>%
      mutate(date = as.Date(started)) %>%
      group_by(date) %>%
      summarise(
        sessions = n(),
        total_duration = sum(duration, na.rm = TRUE),
        avg_duration = mean(duration, na.rm = TRUE)
      )

    # Create chart
    daily_usage %>%
      e_charts(date) %>%
      e_line(sessions, name = "Sessions") %>%
      e_line(avg_duration, name = "Avg Duration (sec)") %>%
      e_tooltip(trigger = "axis") %>%
      e_legend(orient = "horizontal", bottom = 0) %>%
      e_x_axis(axisLabel = list(rotate = 45)) %>%
      e_title("Usage Activity Over Time") %>%
      e_grid(bottom = "15%") %>%
      e_toolbox_feature(feature = "dataZoom") %>%
      e_toolbox_feature(feature = "saveAsImage")
  })

  # User Sessions Distribution chart
  output$user_sessions_chart <- renderEcharts4r({
    req(values$filtered_data, values$users)

    # Join with users data to get usernames
    user_sessions <- values$filtered_data %>%
      left_join(values$users %>% select(guid, username), by = c("user_guid" = "guid")) %>%
      group_by(user_guid, username) %>%
      summarise(sessions = n(),
                total_duration = sum(duration, na.rm = TRUE)) %>%
      arrange(desc(sessions)) %>%
      head(10)

    # Create chart
    user_sessions %>%
      e_charts(username) %>%
      e_bar(sessions, name = "Sessions") %>%
      e_tooltip() %>%
      e_x_axis(axisLabel = list(rotate = 45)) %>%
      e_title("Top 10 Users by Sessions") %>%
      e_grid(bottom = "20%") %>%
      e_toolbox_feature(feature = "saveAsImage")
  })

  # Session Duration Distribution chart
  output$duration_chart <- renderEcharts4r({
    req(values$filtered_data)

    # Create buckets for duration
    duration_dist <- values$filtered_data %>%
      mutate(
        duration_bucket = case_when(
          duration < 30 ~ "< 30s",
          duration < 60 ~ "30-60s",
          duration < 300 ~ "1-5m",
          duration < 600 ~ "5-10m",
          duration < 1800 ~ "10-30m",
          TRUE ~ "> 30m"
        )
      ) %>%
      group_by(duration_bucket) %>%
      summarise(count = n()) %>%
      arrange(factor(duration_bucket, levels = c("< 30s", "30-60s", "1-5m", "5-10m", "10-30m", "> 30m")))

    # Create chart
    duration_dist %>%
      e_charts(duration_bucket) %>%
      e_bar(count, name = "Sessions") %>%
      e_tooltip() %>%
      e_title("Session Duration Distribution") %>%
      e_x_axis(axisLabel = list(interval = 0)) %>%
      e_toolbox_feature(feature = "saveAsImage")
  })

  # Top Apps by Usage chart
  output$top_apps_chart <- renderEcharts4r({
    req(values$filtered_data, values$apps)

    # Join with apps data to get titles
    app_usage <- values$filtered_data %>%
      left_join(values$apps %>% select(guid, title), by = c("content_guid" = "guid")) %>%
      group_by(content_guid, title) %>%
      summarise(
        sessions = n(),
        total_duration = sum(duration, na.rm = TRUE),
        avg_duration = mean(duration, na.rm = TRUE)
      ) %>%
      arrange(desc(sessions)) %>%
      head(10)

    # Create chart
    app_usage %>%
      e_charts(title) %>%
      e_bar(sessions, name = "Sessions") %>%
      e_bar(avg_duration, name = "Avg Duration (sec)", y_index = 1) %>%
      e_tooltip() %>%
      e_x_axis(axisLabel = list(rotate = 45)) %>%
      e_y_axis(index = 1, name = "Avg Duration") %>%
      e_title("Top 10 Apps by Usage") %>%
      e_grid(bottom = "20%") %>%
      e_legend(orient = "horizontal", bottom = 0) %>%
      e_toolbox_feature(feature = "saveAsImage")
  })

  # Raw data table
  output$raw_data_table <- renderDT({
    req(values$filtered_data)

    # Join with apps and users data
    display_data <- values$filtered_data %>%
      left_join(values$apps %>% select(guid, title), by = c("content_guid" = "guid")) %>%
      left_join(values$users %>% select(guid, username), by = c("user_guid" = "guid")) %>%
      select(
        started, ended, duration, title, username,
        session_id, browser, platform, mobile, client
      )

    datatable(
      display_data,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'frtip'
      ),
      rownames = FALSE
    )
  })

  # Download handler for CSV
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("connect-usage-data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      req(values$filtered_data)

      # Join with apps and users data for a more complete export
      export_data <- values$filtered_data %>%
        left_join(values$apps %>% select(guid, title), by = c("content_guid" = "guid")) %>%
        left_join(values$users %>% select(guid, username), by = c("user_guid" = "guid"))

      write.csv(export_data, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
