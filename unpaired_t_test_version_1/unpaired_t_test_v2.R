# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(car)  # For Levene's test

ui <- fluidPage(
  titlePanel("T-test and Mann-Whitney U Test Calculator with Homogeneity Check"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      uiOutput("select_var"),
      checkboxInput("check_variance", "Check Homogeneity of Variance (Levene's Test)", value = TRUE),
      radioButtons("test_type", "Select Test Type:",
                   choices = c("Unpaired T-Test" = "t_test", "Mann-Whitney U Test" = "mann_whitney")),
      actionButton("calculate_test", "Run Test!")
    ),
    
    mainPanel(
      tableOutput("data_preview"),
      plotOutput("boxplot"),
      tableOutput("summary_stats"),
      verbatimTextOutput("variance_test_result"),
      verbatimTextOutput("test_result"),
      verbatimTextOutput("interpretation")
    )
  )
)

server <- function(input, output, session) {
  
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  output$data_preview <- renderTable({
    head(data())
  })
  
  output$select_var <- renderUI({
    req(data())
    tagList(
      selectInput("response_var", "Select Response Variable:", choices = names(data())),
      selectInput("group_var", "Select Grouping Variable:", choices = names(data()))
    )
  })
  
  output$boxplot <- renderPlot({
    req(input$response_var, input$group_var)
    ggplot(data(), aes_string(x = input$group_var, y = input$response_var)) +
      geom_boxplot(fill = "lightblue", color = "darkblue") +
      theme_minimal() +
      labs(title = "Boxplot of Response Variable by Group",
           x = input$group_var,
           y = input$response_var)
  })
  
  output$summary_stats <- renderTable({
    req(input$response_var, input$group_var)
    data() %>%
      group_by(!!sym(input$group_var)) %>%
      summarize(
        Mean = mean(!!sym(input$response_var), na.rm = TRUE),
        SD = sd(!!sym(input$response_var), na.rm = TRUE),
        SE = SD / sqrt(n())
      )
  })
  
  output$test_result <- renderPrint({
    req(input$calculate_test)
    req(input$response_var, input$group_var)
    
    response <- data()[[input$response_var]]
    group <- data()[[input$group_var]]
    
    # Levene's Test for Homogeneity of Variance (only if checkbox is checked)
    var_equal <- TRUE  # Default assumption of equal variance
    if (input$check_variance) {
      levene_result <- leveneTest(response ~ group, data = data())  # Levene's test
      p_var <- levene_result$`Pr(>F)`[1]  # Extract p-value
      
      output$variance_test_result <- renderPrint({
        paste("Levene's Test p-value:", round(p_var, 4))
      })
      
      if (p_var < 0.05) {
        var_equal <- FALSE  # Variances are not equal, use Welch's t-test
      }
    }
    
    # Perform the selected test
    if (input$test_type == "t_test") {
      t_test_result <- t.test(response ~ group, data = data(), var.equal = var_equal)
      print(t_test_result)
      
      p_value <- t_test_result$p.value
      output$interpretation <- renderPrint({
        if (p_value < 0.05) {
          paste("The p-value is", round(p_value, 4), "- Significant difference between groups.")
        } else {
          paste("The p-value is", round(p_value, 4), "- No significant difference between groups.")
        }
      })
      
    } else if (input$test_type == "mann_whitney") {
      mann_whitney_result <- wilcox.test(response ~ group, data = data())
      print(mann_whitney_result)
      
      p_value <- mann_whitney_result$p.value
      output$interpretation <- renderPrint({
        if (p_value < 0.05) {
          paste("The p-value is", round(p_value, 4), "- Significant difference between groups.")
        } else {
          paste("The p-value is", round(p_value, 4), "- No significant difference between groups.")
        }
      })
    }
  })
}

shinyApp(ui = ui, server = server)
