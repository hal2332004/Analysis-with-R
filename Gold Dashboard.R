library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)

ui <- fluidPage(
  
  titlePanel("Gold Price Analysis Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("selected_columns", "Select Indicators to Display:", 
                         choices = c("Gold_Price", "Crude_Oil", "Interest_Rate", 
                                     "USD_INR", "Sensex", "CPI", "USD_Index"),
                         selected = c("Gold_Price", "Crude_Oil")), 
      dateRangeInput("date_range", "Select Date Range:", 
                     start = "2006-01-01", end = "2020-08-01",
                     min = "2006-01-01", max = "2020-08-01", format = "yyyy-mm-dd"),
      uiOutput("year_input")  # Dynamic input for selecting years
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Normalized Line Chart", plotlyOutput("normalized_line_chart")),
        tabPanel("Correlation Heatmap", 
                 plotlyOutput("correlation_heatmap"),
                 verbatimTextOutput("t_test_output")),
        tabPanel("Box Plot (Unnormalized Data)", plotlyOutput("box_plot")),
        tabPanel("Monthly Bar Chart (Min-Max Normalized)", 
                 plotlyOutput("monthly_bar_chart"),
                 plotlyOutput("quarterly_bar_chart"))  # Add Quarterly Bar Chart below Monthly
      )
    )
  )
)

server <- function(input, output, session) {
  
  df <- read.csv("D:/Ky 4/R_code/GoldUP.csv/GoldUP.csv")
  df$Date <- as.Date(df$Date, format = "%d-%m-%Y")
  
  # Add Month, Year, and Quarter columns
  df$Month <- as.integer(format(df$Date, "%m"))
  df$Year <- as.integer(format(df$Date, "%Y"))
  df$Quarter <- as.integer((df$Month - 1) %/% 3 + 1)
  
  # Min-Max normalize Gold_Price for the entire dataset for bar chart
  min_gold <- min(df$Gold_Price, na.rm = TRUE)
  max_gold <- max(df$Gold_Price, na.rm = TRUE)
  df$Gold_Price_MinMax <- (df$Gold_Price - min_gold) / (max_gold - min_gold)
  
  # Line chart
  output$year_input <- renderUI({
    year_choices <- unique(df$Year)
    selectInput("selected_years", "Select Years:", choices = year_choices, multiple = TRUE)
  })
  
  columns_to_normalize <- c("Gold_Price", "Crude_Oil", "Interest_Rate", "USD_INR", "Sensex", "CPI", "USD_Index")
  df_normalized <- df
  df_normalized[columns_to_normalize] <- lapply(df[columns_to_normalize], function(x) {
    (x - mean(x)) / sd(x)
  })
  
  output$normalized_line_chart <- renderPlotly({
    filtered_data <- df_normalized %>%
      filter(Date >= input$date_range[1] & Date <= input$date_range[2])
    
    p <- ggplot(filtered_data, aes(x = Date)) +
      labs(title = "Z-Score Normalized Economic Indicators",
           x = "Date",
           y = "Z-Score Values",
           color = "Index") +
      theme_minimal()
    
    if ("Gold_Price" %in% input$selected_columns) {
      p <- p + geom_line(aes(y = Gold_Price, color = "Gold Price (Z-Score)"))
    }
    if ("USD_Index" %in% input$selected_columns) {
      p <- p + geom_line(aes(y = USD_Index, color = "USD Index (Z-Score)"))
    }
    if ("Crude_Oil" %in% input$selected_columns) {
      p <- p + geom_line(aes(y = Crude_Oil, color = "Crude Oil (Z-Score)"))
    }
    if ("Interest_Rate" %in% input$selected_columns) {
      p <- p + geom_line(aes(y = Interest_Rate, color = "Interest Rate (Z-Score)"))
    }
    if ("USD_INR" %in% input$selected_columns) {
      p <- p + geom_line(aes(y = USD_INR, color = "USD/INR (Z-Score)"))
    }
    if ("Sensex" %in% input$selected_columns) {
      p <- p + geom_line(aes(y = Sensex, color = "Sensex (Z-Score)"))
    }
    if ("CPI" %in% input$selected_columns) {
      p <- p + geom_line(aes(y = CPI, color = "CPI (Z-Score)"))
    }
    
    ggplotly(p)
  })
  
  # Heatmap correlation
  
  output$correlation_heatmap <- renderPlotly({
    filtered_data <- df %>%
      filter(Date >= input$date_range[1] & Date <= input$date_range[2])
    
    cor_matrix <- cor(filtered_data[columns_to_normalize], use = "complete.obs")
    cor_df <- as.data.frame(as.table(cor_matrix))
    cor_df <- cor_df[as.numeric(cor_df$Var1) < as.numeric(cor_df$Var2), ]
    
    p <- ggplot(cor_df, aes(Var1, Var2, fill = Freq)) +
      geom_tile() +
      scale_fill_gradient2(low = "yellow", high = "red", mid = "orange", 
                           midpoint = 0, limit = c(-1, 1), space = "Lab", 
                           name = "Correlation") +
      geom_text(aes(label = round(Freq, 2)), color = "black") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
      labs(title = "Correlation Heatmap",
           x = "Variables",
           y = "Variables")
    
    ggplotly(p)
  })
  
  output$t_test_output <- renderPrint({
    filtered_data <- df %>%
      filter(Date >= input$date_range[1] & Date <= input$date_range[2])
    
    cat("T-Test Results for Correlations with Gold Price (Pearson):\n\n")
    
    cor_tests <- list(
      "CPI" = cor.test(filtered_data$CPI, filtered_data$Gold_Price, method = "pearson"),
      "USD_Index" = cor.test(filtered_data$USD_Index, filtered_data$Gold_Price, method = "pearson"),
      "Crude_Oil" = cor.test(filtered_data$Crude_Oil, filtered_data$Gold_Price, method = "pearson"),
      "Interest_Rate" = cor.test(filtered_data$Interest_Rate, filtered_data$Gold_Price, method = "pearson"),
      "USD_INR" = cor.test(filtered_data$USD_INR, filtered_data$Gold_Price, method = "pearson"),
      "Sensex" = cor.test(filtered_data$Sensex, filtered_data$Gold_Price, method = "pearson")
    )
    
    for (test_name in names(cor_tests)) {
      cat(paste0(test_name, ": p-value = ", round(cor_tests[[test_name]]$p.value, 100), 
                 ", Correlation = ", round(cor_tests[[test_name]]$estimate, 100), "\n"))
    }
  })
  
  # Box plot
  
  output$box_plot <- renderPlotly({
    filtered_data <- df %>%
      filter(Date >= input$date_range[1] & Date <= input$date_range[2]) %>%
      select(Date, all_of(input$selected_columns))
    
    box_plot_data <- filtered_data %>%
      pivot_longer(cols = -Date, names_to = "Indicator", values_to = "Value")
    
    p <- ggplot(box_plot_data, aes(x = Indicator, y = Value, fill = Indicator)) +
      geom_boxplot() +
      labs(title = "Box Plot of Selected Indicators (Unnormalized)",
           x = "Indicator",
           y = "Value") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  
  
  # Monthly Bar Chart (Min-Max Normalized Gold Price)
  output$monthly_bar_chart <- renderPlotly({
    req(input$selected_years)  # Ensure years are selected
    
    filtered_data <- df %>%
      filter(Year %in% input$selected_years) %>%
      group_by(Year, Month) %>%
      summarise(Gold_Price_MinMax = mean(Gold_Price_MinMax, na.rm = TRUE)) %>%
      ungroup()
    
    p <- ggplot(filtered_data, aes(x = factor(Month), y = Gold_Price_MinMax, fill = factor(Year))) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Average Monthly Gold Price (Min-Max Normalized) by Year",
           x = "Month",
           y = "Average Gold Price (Min-Max Normalized)",
           fill = "Year") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  
  
  # Quarterly Bar Chart (Min-Max Normalized Gold Price)
  output$quarterly_bar_chart <- renderPlotly({
    req(input$selected_years)  # Ensure years are selected
    
    filtered_data <- df %>%
      filter(Year %in% input$selected_years) %>%
      group_by(Year, Quarter) %>%
      summarise(Gold_Price_MinMax = mean(Gold_Price_MinMax, na.rm = TRUE)) %>%
      ungroup()
    
    p <- ggplot(filtered_data, aes(x = factor(Quarter), y = Gold_Price_MinMax, fill = factor(Year))) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Average Quarterly Gold Price (Min-Max Normalized) by Year",
           x = "Quarter",
           y = "Average Gold Price (Min-Max Normalized)",
           fill = "Year") +
      theme_minimal()
    
    ggplotly(p)
  })
}


shinyApp(ui = ui, server = server)
