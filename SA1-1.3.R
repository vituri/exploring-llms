library(shiny)
library(bslib)
library(connectapi)
library(echarts4r)
library(dplyr)
library(lubridate)
library(DT)

# Initialize global variables
client <- connect()

# UI
ui <- page_sidebar(
  title = "Connect Usage Explorer",
  fillable = FALSE,

  sidebar = sidebar(
    width = 300,
    bg = "#f8f9fa",

    # Logo and title
    h4("Filters", class = "mt-2 mb-4"),

    # Date range input
    dateRangeInput(
      "date_range",
      "Date Range",
      start = Sys.Date() - 30,
      end = Sys.Date(),
      separator = " to "
    ),

    # Content selection
    selectizeInput(
      "app_filter",
      "Select Application",
      choices = c("All Applications" = "all"),
      multiple = TRUE,
      options = list(placeholder = "Select applications")
    ),

    # User selection
    selectizeInput(
      "user_filter",
      "Select Users",
      choices = c("All Users" = "all"),
      multiple = TRUE,
      options = list(placeholder = "Select users")
    ),

    # Minimum session time
    sliderInput(
      "min_time",
      "Minimum Session Time (seconds)",
      min = 0,
      max = 300,
      value = 5,
      step = 5
    ),

    # Download button
    downloadButton("download_data", "Download Data", class = "btn-primary btn-block mt-4")
  ),

  # Main content
  card(
    card_header(
      class = "bg-primary text-white",
      h2("Connect Usage Overview")
    ),
    card_body(
      layout_column_wrap(
        width = 1/2,
        card(
          card_header("Daily Users"),
          echarts4rOutput("daily_users_chart", height = "300px")
        ),
        card(
          card_header("Total Session Time by App"),
          echarts4rOutput("app_time_chart", height = "300px")
        )
      ),
      layout_column_wrap(
        width = 1/2,
        card(
          card_header("Session Time Distribution"),
          echarts4rOutput("session_distribution", height = "300px")
        ),
        card(
          card_header("Users Activity"),
          echarts4rOutput("user_activity", height = "300px")
        )
      ),
      card(
        card_header("Data Table"),
        DTOutput("usage_table")
      )
    )
  )
)

# Server
server <- function(input, output, session) {

  # Load and prepare content data
  content_data <- reactive({
    req(client)

    tryCatch({
      get_content(client)
    }, error = function(e) {
      showNotification(paste("Error loading content data:", e$message),
                       type = "error", duration = 10)
      return(NULL)
    })
  })

  # Load and prepare user data
  user_data <- reactive({
    req(client)

    tryCatch({
      get_users(client)
    }, error = function(e) {
      showNotification(paste("Error loading user data:", e$message),
                       type = "error", duration = 10)
      return(NULL)
    })
  })

  # Update app filter choices
  observe({
    content <- content_data()
    if(!is.null(content) && nrow(content) > 0) {
      # browser()
      choices <- setNames(content$guid, content$name)
      choices <- c("All Applications" = "all", choices)
      updateSelectizeInput(session, "app_filter", choices = choices, selected = "all")
    }
  })

  # Update user filter choices
  observe({
    users <- user_data()
    if(!is.null(users) && nrow(users) > 0) {
      choices <- setNames(users$guid, users$username)
      choices <- c("All Users" = "all", choices)
      updateSelectizeInput(session, "user_filter", choices = choices, selected = "all")
    }
  })

  # Load usage data
  usage_data <- reactive({
    req(client)

    tryCatch({
      data <- get_usage_shiny(client) |> mutate(duration = started %--% ended / dseconds())

      # Apply date filter
      date_filtered <- data %>%
        filter(started >= input$date_range[1] & started <= (input$date_range[2] + days(1)))

      # Apply app filter
      if(!"all" %in% input$app_filter && length(input$app_filter) > 0) {
        date_filtered <- date_filtered %>%
          filter(content_guid %in% input$app_filter)
      }

      # Apply user filter
      if(!"all" %in% input$user_filter && length(input$user_filter) > 0) {
        date_filtered <- date_filtered %>%
          filter(user_guid %in% input$user_filter)
      }

      # browser()
      # Apply session time filter
      time_filtered <- date_filtered %>%
        filter(duration >= input$min_time)

      return(time_filtered)
    }, error = function(e) {
      showNotification(paste("Error loading usage data:", e$message),
                       type = "error", duration = 10)
      return(NULL)
    })
  })

  # Daily users chart
  output$daily_users_chart <- renderEcharts4r({
    req(usage_data())

    if(nrow(usage_data()) == 0) {
      return(NULL)
    }

    usage_data() %>%
      mutate(date = as.Date(started)) %>%
      group_by(date) %>%
      summarise(users = n_distinct(user_guid)) %>%
      e_charts(date) %>%
      e_line(users, smooth = TRUE) %>%
      e_area(users, opacity = 0.3) %>%
      e_tooltip(trigger = "axis") %>%
      e_title("Daily Unique Users") %>%
      e_legend(show = FALSE) %>%
      e_color("#1E90FF") %>%
      e_theme("dark") %>%
      e_grid(left = "3%", right = "4%", bottom = "3%", containLabel = TRUE) %>%
      e_x_axis(
        axisLabel = list(color = "#cccccc"),
        axisLine = list(lineStyle = list(color = "#cccccc"))
      ) %>%
      e_y_axis(
        axisLabel = list(color = "#cccccc"),
        axisLine = list(lineStyle = list(color = "#cccccc"))
      )
  })

  # App time chart
  output$app_time_chart <- renderEcharts4r({
    req(usage_data(), content_data())

    if(nrow(usage_data()) == 0) {
      return(NULL)
    }

    # Create lookup for app names
    content_lookup <- setNames(content_data()$title, content_data()$guid)

    usage_data() %>%
      group_by(content_guid) %>%
      summarise(total_time = sum(duration) / 60) %>%  # Convert to minutes
      mutate(app_name = ifelse(
        content_guid %in% names(content_lookup),
        content_lookup[content_guid],
        content_guid
      )) %>%
      arrange(desc(total_time)) %>%
      head(10) %>%
      e_charts(app_name) %>%
      e_bar(total_time) %>%
      e_tooltip() %>%
      e_title("Total Session Time by App (minutes)") %>%
      e_legend(show = FALSE) %>%
      e_color("#3498db") %>%
      e_theme("dark") %>%
      e_grid(left = "3%", right = "4%", bottom = "15%", containLabel = TRUE) %>%
      e_x_axis(
        axisLabel = list(
          interval = 0,
          rotate = 45,
          color = "#cccccc"
        ),
        axisLine = list(lineStyle = list(color = "#cccccc"))
      ) %>%
      e_y_axis(
        axisLabel = list(color = "#cccccc"),
        axisLine = list(lineStyle = list(color = "#cccccc"))
      )
  })

  # Session distribution chart
  output$session_distribution <- renderEcharts4r({
    req(usage_data())

    if(nrow(usage_data()) == 0) {
      return(NULL)
    }

    # Categorize session duration
    duration_bins <- c(0, 10, 30, 60, 120, 300, 600, Inf)
    duration_labels <- c("0-10s", "10-30s", "30-60s", "1-2m", "2-5m", "5-10m", ">10m")

    usage_data() %>%
      mutate(duration_category = cut(
        duration,
        breaks = duration_bins,
        labels = duration_labels,
        include.lowest = TRUE
      )) %>%
      group_by(duration_category) %>%
      summarise(count = n()) %>%
      e_charts(duration_category) %>%
      e_pie(count, radius = c("40%", "70%")) %>%
      e_tooltip(formatter = htmlwidgets::JS("
        function(params) {
          return params.name + ': ' + params.value + ' sessions (' +
                 params.percent + '%)';
        }
      ")) %>%
      e_title("Session Duration Distribution") %>%
      e_color(c("#003366", "#0066CC", "#0099FF", "#66CCFF", "#99FFFF", "#CCF2FF", "#E6F9FF")) %>%
      e_theme("dark") %>%
      e_legend(orient = "vertical", right = "5%", top = "center")
  })

  # User activity chart
  output$user_activity <- renderEcharts4r({
    req(usage_data(), user_data())

    if(nrow(usage_data()) == 0) {
      return(NULL)
    }

    # Create lookup for user names
    user_lookup <- setNames(user_data()$username, user_data()$guid)

    usage_data() %>%
      group_by(user_guid) %>%
      summarise(
        sessions = n(),
        avg_duration = mean(duration),
        total_time = sum(duration) / 60  # Convert to minutes
      ) %>%
      mutate(user_name = ifelse(
        user_guid %in% names(user_lookup),
        user_lookup[user_guid],
        user_guid
      )) %>%
      arrange(desc(total_time)) %>%
      head(10) %>%
      e_charts(user_name) %>%
      e_bar(total_time, name = "Total Time (min)") %>%
      e_line(sessions, name = "Sessions", y_index = 1) %>%
      e_tooltip(trigger = "axis") %>%
      e_title("Top Users by Activity") %>%
      e_theme("dark") %>%
      e_color(c("#0099FF", "#FF9900")) %>%
      e_grid(left = "3%", right = "4%", bottom = "15%", containLabel = TRUE) %>%
      e_x_axis(
        axisLabel = list(
          interval = 0,
          rotate = 45,
          color = "#cccccc"
        ),
        axisLine = list(lineStyle = list(color = "#cccccc"))
      ) %>%
      e_y_axis(
        axisLabel = list(color = "#cccccc"),
        axisLine = list(lineStyle = list(color = "#cccccc"))
      ) %>%
      e_y_axis(index = 1, name = "Sessions", nameLocation = "end",
               splitLine = list(show = FALSE),
               axisLabel = list(color = "#cccccc"),
               axisLine = list(lineStyle = list(color = "#cccccc"))) %>%
      e_legend(top = "bottom")
  })

  # Usage data table
  output$usage_table <- renderDT({
    req(usage_data(), content_data(), user_data())

    if(nrow(usage_data()) == 0) {
      return(NULL)
    }

    # Create lookup for app names and user names
    content_lookup <- setNames(content_data()$title, content_data()$guid)
    user_lookup <- setNames(user_data()$username, user_data()$guid)

    usage_data() %>%
      mutate(
        app_name = ifelse(
          content_guid %in% names(content_lookup),
          content_lookup[content_guid],
          content_guid
        ),
        user_name = ifelse(
          user_guid %in% names(user_lookup),
          user_lookup[user_guid],
          user_guid
        ),
        date = as.Date(started),
        started_time = format(started, "%H:%M:%S"),
        ended_time = format(ended, "%H:%M:%S"),
        duration_min = round(duration / 60, 2)
      ) %>%
      select(
        Date = date,
        Started = started_time,
        Ended = ended_time,
        `Duration (min)` = duration_min,
        Application = app_name,
        User = user_name
      )
  }, options = list(
    pageLength = 10,
    autoWidth = TRUE,
    searching = TRUE,
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel')
  ))

  # Download handler for the data
  output$download_data <- downloadHandler(
    filename = function() {
      paste("connect-usage-data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Process data similar to the table but include all fields
      if(!is.null(usage_data()) && nrow(usage_data()) > 0) {
        content_lookup <- setNames(content_data()$title, content_data()$guid)
        user_lookup <- setNames(user_data()$username, user_data()$guid)

        processed_data <- usage_data() %>%
          mutate(
            app_name = ifelse(
              content_guid %in% names(content_lookup),
              content_lookup[content_guid],
              content_guid
            ),
            user_name = ifelse(
              user_guid %in% names(user_lookup),
              user_lookup[user_guid],
              user_guid
            )
          )

        write.csv(processed_data, file, row.names = FALSE)
      } else {
        # Write empty data frame if no data
        write.csv(data.frame(), file, row.names = FALSE)
      }
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)
