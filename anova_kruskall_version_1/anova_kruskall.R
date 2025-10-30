#Testing

# Load the required libraries
library(shiny)
library(ggplot2)
library(dplyr)

# Define the User Interface (UI) for the app
ui <- fluidPage(
  titlePanel("ANOVA and Kruskal-Wallis Test Calculator with Summary Statistics"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV File",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      uiOutput("select_var"),  # Output UI for selecting variables
      radioButtons("test_type", "Select Test Type:",
                   choices = c("ANOVA" = "anova", "Kruskal-Wallis" = "kruskal")),
      actionButton("calculate_test", "Run Test!")
    ),
    
    mainPanel(
      tableOutput("data_preview"),
      plotOutput("boxplot"),                  # Square boxplot for data visualization
      tableOutput("summary_stats"),           # Summary statistics table
      verbatimTextOutput("test_result"),
      verbatimTextOutput("interpretation"),   # P-value interpretation
      verbatimTextOutput("tukey_result")      # Output for Tukey test result
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  
  # Load and preview the CSV file
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  output$data_preview <- renderTable({
    head(data())
  })
  
  # Dynamically generate UI to select variables for test
  output$select_var <- renderUI({
    req(data())
    tagList(
      selectInput("response_var", "Select Response Variable:", choices = names(data())),
      selectInput("group_var", "Select Grouping Variable:", choices = names(data()))
    )
  })
  
  # Generate the ggplot2 boxplot with square aspect ratio
  output$boxplot <- renderPlot({
    req(input$response_var, input$group_var)
    
    # Ensure selected variables exist in the data
    if (input$response_var %in% names(data()) && input$group_var %in% names(data())) {
      ggplot(data(), aes_string(x = input$group_var, y = input$response_var)) +
        geom_boxplot(fill = "lightblue", color = "darkblue") +
        theme_minimal() +
        labs(title = "Square Boxplot of Response Variable by Group",
             x = input$group_var,
             y = input$response_var)
    }
  })
  
  # Generate summary statistics table
  output$summary_stats <- renderTable({
    req(input$response_var, input$group_var)
    
    # Ensure selected variables exist in the data
    if (input$response_var %in% names(data()) && input$group_var %in% names(data())) {
      data() %>%
        group_by(!!sym(input$group_var)) %>%
        summarize(
          Mean = mean(!!sym(input$response_var), na.rm = TRUE),
          SD = sd(!!sym(input$response_var), na.rm = TRUE),
          SE = SD / sqrt(n())
        )
    }
  })
  
  # Perform the selected test when button is clicked
  output$test_result <- renderPrint({
    req(input$calculate_test)
    req(input$response_var, input$group_var)
    
    # Ensure selected variables exist in the data
    if (input$response_var %in% names(data()) && input$group_var %in% names(data())) {
      response <- data()[[input$response_var]]
      group <- data()[[input$group_var]]
      
      # Run the selected test
      if (input$test_type == "anova") {
        anova_result <- aov(response ~ group, data = data())
        print(summary(anova_result))
        
        # Perform Tukey's HSD if ANOVA is significant
        if (summary(anova_result)[[1]]$`Pr(>F)`[1] < 0.05) {
          tukey_result <- TukeyHSD(anova_result)
          output$tukey_result <- renderPrint({ tukey_result })
        } else {
          output$tukey_result <- renderPrint({ "ANOVA is not significant; Tukey's HSD test not performed." })
        }
        
        # Store the p-value for interpretation
        p_value <- summary(anova_result)[[1]]$`Pr(>F)`[1]
        output$interpretation <- renderPrint({
          if (p_value < 0.05) {
            paste("The p-value is", round(p_value, 4), "- This result is statistically significant, suggesting that there are significant differences between groups.")
          } else {
            paste("The p-value is", round(p_value, 4), "- This result is not statistically significant, indicating no significant differences between groups.")
          }
        })
        
      } else if (input$test_type == "kruskal") {
        kruskal_result <- kruskal.test(response ~ group, data = data())
        print(kruskal_result)
        
        # Store the p-value for interpretation
        p_value <- kruskal_result$p.value
        output$interpretation <- renderPrint({
          if (p_value < 0.05) {
            paste("The p-value is", round(p_value, 4), "- This result is statistically significant, suggesting that there are significant differences between groups.")
          } else {
            paste("The p-value is", round(p_value, 4), "- This result is not statistically significant, indicating no significant differences between groups.")
          }
        })
        
        output$tukey_result <- renderPrint({ "Tukey's HSD is only available for ANOVA." })
      }
      
    } else {
      "Please select valid variables for the test."
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
