library(shiny)
library(bslib)
library(connectapi)
library(echarts4r)
library(dplyr)
library(tidyr)
library(lubridate)
library(reactable)

# UI
ui <- page_sidebar(
  title = "Connect Usage Explorer",
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#2c3e50",
    "enable-shadows" = TRUE,
    "spacer" = "1rem"
  ),

  sidebar = sidebar(
    title = "Filters",
    width = 300,

    card(
      card_header("Date Range"),
      dateRangeInput(
        "date_range",
        NULL,
        start = Sys.Date() - 30,
        end = Sys.Date(),
        max = Sys.Date()
      )
    ),

    card(
      card_header("Minimum Session Time"),
      sliderInput(
        "min_session_time",
        NULL,
        min = 0,
        max = 300,
        value = 5,
        step = 5,
        post = " seconds"
      )
    ),

    card(
      card_header("Filter Content"),
      selectizeInput(
        "selected_apps",
        "Select Content",
        choices = NULL,
        multiple = TRUE
      )
    ),

    card(
      card_header("Filter Users"),
      selectizeInput(
        "selected_users",
        "Select Users",
        choices = NULL,
        multiple = TRUE
      )
    ),

    actionButton("refresh_data", "Refresh Data", class = "btn-primary w-100 mt-3"),

    downloadButton("download_csv", "Download Data as CSV", class = "btn-secondary w-100 mt-3")
  ),

  layout_columns(
    fill = FALSE,
    value_box(
      title = "Total Sessions",
      value = textOutput("total_sessions"),
      showcase = bsicons::bs_icon("bar-chart-line"),
      theme = "primary"
    ),
    value_box(
      title = "Unique Users",
      value = textOutput("unique_users"),
      showcase = bsicons::bs_icon("people-fill"),
      theme = "secondary"
    ),
    value_box(
      title = "Avg. Session Time",
      value = textOutput("avg_session_time"),
      showcase = bsicons::bs_icon("hourglass-split"),
      theme = "info"
    )
  ),

  layout_columns(
    col_widths = c(6, 6),
    card(
      card_header("Daily Sessions"),
      card_body(
        echarts4rOutput("daily_sessions_chart", height = "300px")
      )
    ),
    card(
      card_header("Top Content by Usage"),
      card_body(
        echarts4rOutput("top_content_chart", height = "300px")
      )
    )
  ),

  layout_columns(
    col_widths = c(6, 6),
    card(
      card_header("Sessions by Hour of Day"),
      card_body(
        echarts4rOutput("hourly_usage_chart", height = "300px")
      )
    ),
    card(
      card_header("Top Users"),
      card_body(
        echarts4rOutput("top_users_chart", height = "300px")
      )
    )
  ),

  card(
    card_header("Session Details"),
    card_body(
      reactableOutput("session_table")
    )
  )
)

# Server
server <- function(input, output, session) {
  # Initialize connection to Connect
  # Note: In a real app, you would need to have a valid Connect server configured
  # and proper authentication credentials
  connect_server <- reactiveVal(NULL)

  # Reactive value for usage data
  usage_data <- reactiveVal(NULL)
  content_data <- reactiveVal(NULL)
  users_data <- reactiveVal(NULL)

  # Function to attempt to connect to the server
  attempt_connect <- function() {
    tryCatch({
      # Try to connect to Connect
      # Note: In a real app, you would likely use a pre-established connection
      # or get credentials from environment variables or user input
      server <- connectapi::connect()
      connect_server(server)
      return(TRUE)
    }, error = function(e) {
      showNotification(
        paste("Failed to connect to Connect server:", e$message),
        type = "error",
        duration = 10
      )
      return(FALSE)
    })
  }

  # Initial connection attempt when app loads
  observe({
    attempt_connect()
  }, priority = 1000)

  # Function to fetch data
  fetch_data <- function() {
    req(connect_server())

    tryCatch({
      showNotification("Fetching usage data...", type = "message", id = "loading")

      # Fetch Shiny usage data
      data <- connectapi::get_usage_shiny(connect_server())

      # Remove notification
      removeNotification("loading")

      if (nrow(data) == 0) {
        showNotification("No usage data available", type = "warning")
        return(NULL)
      }

      # Clean and format data
      data <- data %>%
        mutate(
          started = as.POSIXct(started, format = "%Y-%m-%dT%H:%M:%S"),
          ended = as.POSIXct(ended, format = "%Y-%m-%dT%H:%M:%S"),
          date = as.Date(started),
          hour = hour(started),
          duration_seconds = as.numeric(difftime(ended, started, units = "secs")),
          duration_minutes = duration_seconds / 60
        )

      return(data)
    }, error = function(e) {
      removeNotification("loading")
      showNotification(paste("Error fetching data:", e$message), type = "error", duration = 10)
      return(NULL)
    })
  }

  # Function to fetch content data
  fetch_content <- function() {
    req(connect_server())

    tryCatch({
      # Fetch content data
      data <- connectapi::get_content(connect_server())
      return(data)
    }, error = function(e) {
      showNotification(paste("Error fetching content data:", e$message), type = "error", duration = 10)
      return(NULL)
    })
  }

  # Function to fetch users data
  fetch_users <- function() {
    req(connect_server())

    tryCatch({
      # Fetch users data
      data <- connectapi::get_users(connect_server())
      return(data)
    }, error = function(e) {
      showNotification(paste("Error fetching users data:", e$message), type = "error", duration = 10)
      return(NULL)
    })
  }

  # Fetch data on app startup or refresh button click
  observe({
    if (input$refresh_data > 0 || is.null(usage_data())) {
      req(connect_server())

      # Fetch usage data
      data <- fetch_data()
      if (!is.null(data)) {
        usage_data(data)
      }

      # Fetch content data
      content <- fetch_content()
      if (!is.null(content)) {
        content_data(content)
      }

      # Fetch users data
      users <- fetch_users()
      if (!is.null(users)) {
        users_data(users)
      }
    }
  })

  # Update filter inputs when data changes
  observe({
    req(usage_data())
    data <- usage_data()
    content <- content_data()
    users <- users_data()

    # Get content title mapping
    content_choices <- if (!is.null(content)) {
      setNames(content$guid, content$title)
    } else {
      # Fallback to content_guid from usage data if content data not available
      unique_content <- unique(data$content_guid)
      setNames(unique_content, unique_content)
    }

    # Get user name mapping
    user_choices <- if (!is.null(users)) {
      setNames(users$guid, paste0(users$username, " (", users$first_name, " ", users$last_name, ")"))
    } else {
      # Fallback to user_guid from usage data if users data not available
      unique_users <- unique(data$user_guid)
      setNames(unique_users, unique_users)
    }

    updateSelectizeInput(
      session,
      "selected_apps",
      choices = content_choices,
      selected = character(0)
    )

    updateSelectizeInput(
      session,
      "selected_users",
      choices = user_choices,
      selected = character(0)
    )
  })

  # Filtered data based on user inputs
  filtered_data <- reactive({
    req(usage_data())
    data <- usage_data()

    # Filter by date range
    data <- data %>%
      filter(date >= input$date_range[1] & date <= input$date_range[2])

    # Filter by minimum session time
    data <- data %>%
      filter(duration_seconds >= input$min_session_time)

    # Filter by apps if selected
    if (length(input$selected_apps) > 0) {
      data <- data %>%
        filter(content_guid %in% input$selected_apps)
    }

    # Filter by users if selected
    if (length(input$selected_users) > 0) {
      data <- data %>%
        filter(user_guid %in% input$selected_users)
    }

    return(data)
  })

  # Summary metrics
  output$total_sessions <- renderText({
    req(filtered_data())
    nrow(filtered_data())
  })

  output$unique_users <- renderText({
    req(filtered_data())
    length(unique(filtered_data()$user_guid))
  })

  output$avg_session_time <- renderText({
    req(filtered_data())
    mean_seconds <- mean(filtered_data()$duration_seconds, na.rm = TRUE)
    if (mean_seconds >= 60) {
      paste(round(mean_seconds / 60, 1), "min")
    } else {
      paste(round(mean_seconds, 0), "sec")
    }
  })

  # Daily sessions chart
  output$daily_sessions_chart <- renderEcharts4r({
    req(filtered_data())

    daily_data <- filtered_data() %>%
      count(date) %>%
      rename(Sessions = n)

    daily_data %>%
      e_charts(date) %>%
      e_area(Sessions,
             areaStyle = list(opacity = 0.3),
             smooth = TRUE,
             itemStyle = list(color = "#2c3e50")) %>%
      e_tooltip(trigger = "axis") %>%
      e_title("Sessions by Day") %>%
      e_legend(show = FALSE) %>%
      e_x_axis(name = "Date",
               axisLabel = list(rotate = 45, fontSize = 10)) %>%
      e_y_axis(name = "Sessions") %>%
      e_grid(left = "5%", right = "5%", bottom = "15%") %>%
      e_datazoom(type = "slider") %>%
      e_theme("vintage")
  })

  # Top content chart
  output$top_content_chart <- renderEcharts4r({
    req(filtered_data())

    # Using the correct column name for content in get_usage_shiny
    # It's typically 'content_title' or similar, but we'll check both options
    content_col <- if("content_title" %in% names(filtered_data())) {
      "content_title"
    } else if("app_title" %in% names(filtered_data())) {
      "app_title"
    } else {
      "content_guid" # Fallback to guid if neither name is available
    }

    top_content <- filtered_data() %>%
      group_by(!!sym(content_col)) %>%
      summarise(
        Total_Sessions = n(),
        Total_Minutes = sum(duration_minutes, na.rm = TRUE)
      ) %>%
      arrange(desc(Total_Sessions)) %>%
      head(10)

    top_content %>%
      e_charts(!!sym(content_col)) %>%
      e_bar(Total_Sessions, name = "Sessions") %>%
      e_title("Top Content by Sessions") %>%
      e_tooltip() %>%
      e_x_axis(axisLabel = list(rotate = 45, fontSize = 10)) %>%
      e_grid(left = "5%", right = "5%", bottom = "15%") %>%
      e_toolbox_feature(feature = "saveAsImage") %>%
      e_theme("vintage")
  })

  # Hourly usage chart
  output$hourly_usage_chart <- renderEcharts4r({
    req(filtered_data())

    hourly_data <- filtered_data() %>%
      count(hour) %>%
      mutate(hour_label = sprintf("%02d:00", hour)) %>%
      rename(Sessions = n)

    hourly_data %>%
      e_charts(hour_label) %>%
      e_bar(Sessions,
            itemStyle = list(color = "#3498db",
                             borderRadius = 4)) %>%
      e_title("Sessions by Hour of Day") %>%
      e_tooltip() %>%
      e_x_axis(
        name = "Hour",
        axisLabel = list(fontSize = 10)
      ) %>%
      e_y_axis(name = "Sessions") %>%
      e_grid(left = "5%", right = "5%", bottom = "5%") %>%
      e_legend(show = FALSE) %>%
      e_theme("vintage")
  })

  # Top users chart
  output$top_users_chart <- renderEcharts4r({
    req(filtered_data())

    # Using the correct column name for users in get_usage_shiny
    # It's typically 'user_username' or similar
    user_col <- if("user_username" %in% names(filtered_data())) {
      "user_username"
    } else if("username" %in% names(filtered_data())) {
      "username"
    } else {
      "user_guid" # Fallback to guid if neither name is available
    }

    top_users <- filtered_data() %>%
      group_by(!!sym(user_col)) %>%
      summarise(
        Total_Sessions = n(),
        Avg_Duration = mean(duration_minutes, na.rm = TRUE)
      ) %>%
      arrange(desc(Total_Sessions)) %>%
      head(10)

    top_users %>%
      e_charts(!!sym(user_col)) %>%
      e_bar(Total_Sessions, name = "Sessions") %>%
      e_title("Top Users by Sessions") %>%
      e_tooltip() %>%
      e_x_axis(axisLabel = list(rotate = 45, fontSize = 10)) %>%
      e_grid(left = "5%", right = "5%", bottom = "15%") %>%
      e_toolbox_feature(feature = "saveAsImage") %>%
      e_theme("vintage")
  })

  # Session details table
  output$session_table <- renderReactable({
    req(filtered_data())

    # Determine available columns for user and content
    user_col <- if("user_username" %in% names(filtered_data())) {
      "user_username"
    } else if("username" %in% names(filtered_data())) {
      "username"
    } else {
      "user_guid"
    }

    content_col <- if("content_title" %in% names(filtered_data())) {
      "content_title"
    } else if("app_title" %in% names(filtered_data())) {
      "app_title"
    } else {
      "content_guid"
    }

    # Select columns with dynamic names
    table_data <- filtered_data() %>%
      select(
        User = !!sym(user_col),
        Content = !!sym(content_col),
        Started = started,
        Ended = ended,
        Duration = duration_seconds
      ) %>%
      mutate(
        Started = format(Started, "%Y-%m-%d %H:%M:%S"),
        Ended = format(Ended, "%Y-%m-%d %H:%M:%S"),
        Duration = case_when(
          Duration >= 3600 ~ sprintf("%.1f hr", Duration / 3600),
          Duration >= 60 ~ sprintf("%.1f min", Duration / 60),
          TRUE ~ sprintf("%.0f sec", Duration)
        )
      ) %>%
      arrange(desc(Started))

    reactable(
      table_data,
      filterable = TRUE,
      searchable = TRUE,
      bordered = TRUE,
      striped = TRUE,
      highlight = TRUE,
      defaultPageSize = 10,
      paginationType = "jump",
      theme = reactableTheme(
        borderColor = "#dfe2e5",
        stripedColor = "#f6f8fa",
        highlightColor = "#f0f5f9",
        cellPadding = "8px 12px"
      ),
      columns = list(
        User = colDef(minWidth = 120),
        Content = colDef(minWidth = 200),
        Started = colDef(minWidth = 150),
        Ended = colDef(minWidth = 150),
        Duration = colDef(minWidth = 100, align = "right")
      )
    )
  })

  # Download handler for CSV
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("connect_usage_data_", format(Sys.Date(), "%Y-%m-%d"), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
