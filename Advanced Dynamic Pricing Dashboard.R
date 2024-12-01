# Load Required Libraries
library(dplyr)
library(ggplot2)
library(shiny)
library(plotly)
library(Metrics)
library(shinythemes)
library(reshape2)

# Mock Data Creation
set.seed(123)
data <- data.frame(
  ProductID = 1:100,
  Category = sample(c("Electronics", "Clothing", "Home"), 100, replace = TRUE),
  CurrentInventory = sample(10:500, 100, replace = TRUE),
  SalesVolume = sample(5:100, 100, replace = TRUE),
  Price = round(runif(100, 10, 200), 2),
  Date = as.Date("2024-01-01") + sample(0:365, 100, replace = TRUE)
)

# Feature Engineering
data <- data %>%
  mutate(
    TurnoverRatio = SalesVolume / CurrentInventory,
    Revenue = SalesVolume * Price
  )

# Shiny UI
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Advanced Dynamic Pricing Dashboard"),
  sidebarLayout(
    sidebarPanel(
      h4("Filters and Settings"),
      dateRangeInput("date_filter", "Select Date Range:",
                     start = min(data$Date), end = max(data$Date)),
      selectInput("category_filter", "Select Product Category:",
                  choices = c("All", unique(data$Category)), selected = "All"),
      sliderInput("inventory", "Filter Inventory Level (Max):",
                  min = 0, max = 500, value = 100),
      numericInput("price_range", "Set Maximum Price Filter:",
                   value = 150, min = 10, max = 200),
      checkboxGroupInput("predictors", "Select Predictors for Model:",
                         choices = c("Price", "CurrentInventory"),
                         selected = c("Price", "CurrentInventory")),
      hr(),
      h4("Scenario Analysis"),
      numericInput("scenario_price", "Enter Price for Prediction:", value = 100),
      numericInput("scenario_inventory", "Enter Inventory Level for Prediction:", value = 50),
      actionButton("predict_btn", "Predict Sales"),
      hr(),
      downloadButton("download_data", "Download Filtered Data")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Dashboard",
                 fluidRow(
                   column(4, h5("Total Revenue"), verbatimTextOutput("summaryRevenue")),
                   column(4, h5("Average Sales"), verbatimTextOutput("summarySales")),
                   column(4, h5("Top Category"), verbatimTextOutput("topCategory"))
                 ),
                 plotlyOutput("correlationPlot"),
                 plotlyOutput("categoryBoxPlot")),
        tabPanel("Model Insights",
                 plotlyOutput("residualPlot"),
                 plotlyOutput("predictionIntervalPlot"),
                 verbatimTextOutput("modelInsights")),
        tabPanel("Filtered Data",
                 tableOutput("filteredTable")),
        tabPanel("Help",
                 h4("About the App"),
                 p("This app demonstrates dynamic pricing optimization by analyzing sales, inventory, and pricing data."),
                 p("Features include interactive data exploration, model training, and scenario analysis."))
      )
    )
  )
)

# Shiny Server
server <- function(input, output) {
  # Reactive Filtered Data
  filtered_data <- reactive({
    filtered <- data %>%
      filter(
        CurrentInventory <= input$inventory,
        Price <= input$price_range,
        Date >= input$date_filter[1],
        Date <= input$date_filter[2]
      )
    if (input$category_filter != "All") {
      filtered <- filtered %>% filter(Category == input$category_filter)
    }
    filtered
  })
  
  # Reactive Model Update
  updated_model <- reactive({
    req(input$predictors)
    formula <- as.formula(paste("SalesVolume ~", paste(input$predictors, collapse = " + ")))
    lm(formula, data = filtered_data())
  })
  
  # Summary Metrics
  output$summaryRevenue <- renderPrint({
    req(filtered_data())
    sum(filtered_data()$Revenue, na.rm = TRUE)
  })
  output$summarySales <- renderPrint({
    req(filtered_data())
    mean(filtered_data()$SalesVolume, na.rm = TRUE)
  })
  output$topCategory <- renderPrint({
    req(filtered_data())
    filtered_data() %>%
      group_by(Category) %>%
      summarise(TotalRevenue = sum(Revenue, na.rm = TRUE)) %>%
      arrange(desc(TotalRevenue)) %>%
      slice(1) %>%
      pull(Category)
  })
  
  # Correlation Heatmap
  output$correlationPlot <- renderPlotly({
    req(filtered_data())
    corr_data <- filtered_data() %>%
      select(SalesVolume, CurrentInventory, Price, Revenue) %>%
      cor(use = "complete.obs")
    melted_corr <- melt(corr_data)
    gg <- ggplot(melted_corr, aes(x = Var1, y = Var2, fill = value)) +
      geom_tile() +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
      labs(title = "Correlation Heatmap", x = "", y = "") +
      theme_minimal()
    ggplotly(gg)
  })
  
  # Boxplot by Category
  output$categoryBoxPlot <- renderPlotly({
    req(filtered_data())
    gg <- ggplot(filtered_data(), aes(x = Category, y = SalesVolume, fill = Category)) +
      geom_boxplot() +
      labs(title = "Sales Distribution by Category", x = "Category", y = "Sales Volume") +
      theme_minimal()
    ggplotly(gg)
  })
  
  # Residual Plot
  output$residualPlot <- renderPlotly({
    req(updated_model())
    residuals <- resid(updated_model())
    predicted <- predict(updated_model())
    gg <- ggplot(data.frame(Residuals = residuals, Predicted = predicted), aes(x = Predicted, y = Residuals)) +
      geom_point(color = "blue") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      labs(title = "Residual Plot", x = "Predicted Values", y = "Residuals") +
      theme_minimal()
    ggplotly(gg)
  })
  
  # Prediction Interval Plot
  output$predictionIntervalPlot <- renderPlotly({
    req(filtered_data(), updated_model())
    pred <- predict(updated_model(), interval = "prediction")
    df <- cbind(filtered_data(), pred)
    gg <- ggplot(df, aes(x = Price, y = SalesVolume)) +
      geom_point(color = "blue") +
      geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "gray", alpha = 0.3) +
      labs(title = "Prediction Interval Plot", x = "Price", y = "Sales Volume") +
      theme_minimal()
    ggplotly(gg)
  })
  
  # Model Insights
  output$modelInsights <- renderPrint({
    req(updated_model())
    summary(updated_model())
  })
  
  # Filtered Data Table
  output$filteredTable <- renderTable({
    req(filtered_data())
    filtered_data()
  })
  
  # Predict Sales (Scenario Analysis)
  observeEvent(input$predict_btn, {
    isolate({
      req(updated_model())
      new_data <- data.frame(
        Price = input$scenario_price,
        CurrentInventory = input$scenario_inventory
      )
      predicted_sales <- predict(updated_model(), newdata = new_data)
      showModal(modalDialog(
        title = "Prediction Result",
        paste("Predicted Sales Volume:", round(predicted_sales, 2)),
        easyClose = TRUE
      ))
    })
  })
  
  # Download Filtered Data
  output$download_data <- downloadHandler(
    filename = function() { paste("filtered_data", Sys.Date(), ".csv", sep = "") },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
}

# Run Shiny App
shinyApp(ui = ui, server = server)
