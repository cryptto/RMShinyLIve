library(shiny)
library(googlesheets4)
library(tidyverse)
library(plotly)
library(DT)
library(lubridate)
library(bslib)

# Google Sheet
sheet_url <- "https://docs.google.com/spreadsheets/d/1P-eeykj-0mRPyu63TD3r3cbe3cfnHE6pZzhMUIpSK1M/edit"
USD_to_NTD <- 32

format_amount <- function(x) {
  formatC(x, format = "d", big.mark = ",")
}

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "litera",
    base_font = font_google("Roboto"),
    heading_font = font_google("Montserrat"),
    primary = "#0073C2"
  ),
  titlePanel("Expense Dashboard"),
  
  tags$head(
    tags$style(HTML("
                    #refresh_spinner {
                    display: none;
                    font-size: 1.2em;
                    color: #007bff;
                      margin-bottom: 10px;
                    }
.refresh-row {
  display: flex;
  align-items: center;
  gap: 10px;
}
")),
tags$script(HTML("
                 Shiny.addCustomMessageHandler('toggleSpinner', function(show) {
                   var spinner = document.getElementById('refresh_spinner');
                   if (spinner) {
                     spinner.style.display = show ? 'inline-block' : 'none';
                   }
                 });
                 "))
  ),

sidebarLayout(
  sidebarPanel(
    div(class = "refresh-row",
        actionButton("refresh_data", label = "\U0001F504 Refresh Now", class = "btn btn-primary"),
        span(id = "refresh_spinner", icon("spinner", class = "fa-spin"))
    ),
    textOutput("last_refresh"),
    br(),
    selectInput("display_currency", "Display Currency", choices = c("NTD", "USD")),
    uiOutput("month_filter"),
    uiOutput("expense_filter"),
    hr(),
    uiOutput("summary_cards")
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Data Table", DTOutput("data_table")),
      tabPanel("Charts", 
               plotlyOutput("bar_chart"),
               br(),
               plotlyOutput("pie_chart")),
      tabPanel("Cumulative Over Time", 
               plotlyOutput("line_chart"))
    )
  )
)
)

server <- function(input, output, session) {
  refresh_time <- reactiveVal(Sys.time())
  
  show_spinner <- function(show = TRUE) {
    session$sendCustomMessage("toggleSpinner", show)
  }
  
  trigger_refresh <- reactiveVal(0)
  observeEvent(input$refresh_data, {
    show_spinner(TRUE)
    trigger_refresh(trigger_refresh() + 1)
  })
  
  data_raw <- reactive({
    invalidateLater(600000, session)
    trigger_refresh()
    
    show_spinner(TRUE)
    gs4_deauth()
    df <- read_sheet(sheet_url)
    
    df$Currency[is.na(df$Currency) | df$Currency == ""] <- "NTD"
    df$Amount <- suppressWarnings(as.numeric(as.character(df$Amount)))
    df$Timestamp <- as.POSIXct(df$Timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    
    df <- df %>%
      mutate(
        YearMonth = format(Timestamp, "%Y-%m"),
        Year = year(Timestamp),
        Month = month(Timestamp)
      )
    
    refresh_time(Sys.time())
    show_spinner(FALSE)
    
    showModal(modalDialog(
      title = "Refresh Complete",
      paste("Data refreshed at", format(refresh_time(), "%b %d, %Y %H:%M")),
      easyClose = TRUE
    ))
    
    df
  })
  
  output$last_refresh <- renderText({
    paste("Last refreshed:", format(refresh_time(), "%b %d, %Y %H:%M"))
  })
  
  output$month_filter <- renderUI({
    df <- data_raw()
    choices <- sort(unique(df$YearMonth), decreasing = TRUE)
    selectInput("selected_month", "Select Month", choices = c("YTD", choices))
  })
  
  output$expense_filter <- renderUI({
    df <- data_raw()
    choices <- sort(unique(df$`Expense Type`))
    selectInput("expense_type", "Filter by Expense Type", choices = c("All", choices))
  })
  
  filtered_data <- reactive({
    df <- data_raw()
    
    if (!is.null(input$selected_month) && input$selected_month != "YTD") {
      df <- df %>% filter(YearMonth == input$selected_month)
    } else {
      df <- df %>% filter(Year == year(Sys.Date()))
    }
    
    if (!is.null(input$expense_type) && input$expense_type != "All") {
      df <- df %>% filter(`Expense Type` == input$expense_type)
    }
    
    df <- df %>%
      mutate(Amount_converted = case_when(
        Currency == "NTD" & input$display_currency == "USD" ~ Amount / USD_to_NTD,
        Currency == "USD" & input$display_currency == "NTD" ~ Amount * USD_to_NTD,
        TRUE ~ Amount
      ))
    
    df
  })
  
  output$summary_cards <- renderUI({
    df <- filtered_data()
    total <- sum(df$Amount_converted, na.rm = TRUE)
    count <- nrow(df)
    
    tagList(
      div(class = "card", style = "margin-bottom: 10px;",
          div(class = "card-body",
              h5(class = "card-title", icon("dollar-sign"), " Total"),
              p(class = "card-text", paste(format_amount(total), input$display_currency))
          )
      ),
      div(class = "card", style = "margin-bottom: 10px;",
          div(class = "card-body",
              h5(class = "card-title", icon("receipt"), " Entries"),
              p(class = "card-text", count)
          )
      )
    )
  })
  
  output$data_table <- renderDT({
    df <- filtered_data() %>%
      mutate(FormattedTime = format(Timestamp, "%b %d, %Y %H:%M"))
    
    datatable(
      df %>%
        select(FormattedTime, `Expense Type`, Description, Amount = Amount_converted) %>%
        rename(`Timestamp` = FormattedTime),
      options = list(scrollX = TRUE)
    )
  })
  
  output$bar_chart <- renderPlotly({
    df <- filtered_data()
    if (nrow(df) == 0) return(NULL)
    
    df_summary <- df %>%
      group_by(`Expense Type`) %>%
      summarise(Total = sum(Amount_converted, na.rm = TRUE))
    
    plot <- ggplot(df_summary, aes(x = `Expense Type`, y = Total, fill = `Expense Type`, 
                                   text = paste0("Total: ", format_amount(Total), " ", input$display_currency))) +
      geom_col() +
      labs(title = paste("Total by Expense Type -", input$selected_month),
           y = paste("Amount in", input$display_currency)) +
      theme_minimal()
    
    ggplotly(plot, tooltip = "text")
  })
  
  output$pie_chart <- renderPlotly({
    df <- filtered_data()
    if (nrow(df) == 0) return(NULL)
    
    df_summary <- df %>%
      group_by(`Expense Type`) %>%
      summarise(Total = sum(Amount_converted, na.rm = TRUE))
    
    plot_ly(
      labels = df_summary$`Expense Type`,
      values = df_summary$Total,
      type = "pie",
      textinfo = "label+percent",
      hoverinfo = "label+value+percent",
      text = paste0(df_summary$`Expense Type`, ": ", 
                    format_amount(df_summary$Total), " ", input$display_currency)
    ) %>%
      layout(title = paste("Expense Breakdown -", input$selected_month))
  })
  
  output$line_chart <- renderPlotly({
    df <- data_raw()
    
    if (!is.null(input$expense_type) && input$expense_type != "All") {
      df <- df %>% filter(`Expense Type` == input$expense_type)
    }
    
    df <- df %>%
      mutate(Amount_converted = case_when(
        Currency == "NTD" & input$display_currency == "USD" ~ Amount / USD_to_NTD,
        Currency == "USD" & input$display_currency == "NTD" ~ Amount * USD_to_NTD,
        TRUE ~ Amount
      )) %>%
      arrange(Timestamp) %>%
      group_by(Date = as.Date(Timestamp)) %>%
      summarise(DailyTotal = sum(Amount_converted, na.rm = TRUE)) %>%
      mutate(Cumulative = cumsum(DailyTotal))
    
    plot <- ggplot(df, aes(x = Date, y = Cumulative, 
                           text = paste0("Cumulative: ", format_amount(Cumulative), " ", input$display_currency))) +
      geom_line(color = "#0073C2FF", size = 1.2) +
      geom_point(color = "#0073C2FF", size = 1.5) +
      labs(title = "Cumulative Expenses Over Time",
           x = "Date",
           y = paste("Cumulative Total in", input$display_currency)) +
      theme_minimal()
    
    ggplotly(plot, tooltip = "text")
  })
}

shinyApp(ui, server)
