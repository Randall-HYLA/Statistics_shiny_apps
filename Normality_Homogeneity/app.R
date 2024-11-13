# Load required libraries
library(shiny)
library(ggplot2)
library(shinyWidgets)
library(car) # for Levene's test

# Define UI
ui <- fluidPage(
  titlePanel("Exploratory Data Analysis App"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload your CSV file", accept = ".csv"),
      uiOutput("num_var_select"),
      uiOutput("group_var_select"),
      uiOutput("analysis_options")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Histogram", plotOutput("histPlot")),
        tabPanel("Boxplot", plotOutput("boxPlot")),
        tabPanel("Normality Test", verbatimTextOutput("normalityTest")),
        tabPanel("Variance Homogeneity Test", verbatimTextOutput("homogeneityTest"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Load and read the file
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  # Generate numeric variable selection options
  output$num_var_select <- renderUI({
    req(data())
    numeric_vars <- names(data())[sapply(data(), is.numeric)]
    selectInput("num_var", "Select a Numeric Variable", choices = numeric_vars)
  })
  
  # Generate group variable selection options
  output$group_var_select <- renderUI({
    req(data())
    group_vars <- names(data())[sapply(data(), function(x) is.factor(x) || is.character(x))]
    selectInput("group_var", "Select a Grouping Variable (optional)", 
                choices = c("None", group_vars), selected = "None")
  })
  
  # Generate analysis options
  output$analysis_options <- renderUI({
    req(input$num_var)
    tagList(
      checkboxInput("showHist", "Show Histogram", TRUE),
      checkboxInput("showBoxplot", "Show Boxplot", TRUE),
      checkboxInput("showNormality", "Perform Normality Test", TRUE),
      checkboxInput("showHomogeneity", "Perform Variance Homogeneity Test", TRUE)
    )
  })
  
  # Generate histogram(s)
  output$histPlot <- renderPlot({
    req(input$num_var, input$showHist)
    if (input$group_var == "None") {
      ggplot(data(), aes_string(x = input$num_var)) +
        geom_histogram(color = "black", fill = "skyblue", bins = 30) +
        labs(title = paste("Histogram of", input$num_var))
    } else {
      ggplot(data(), aes_string(x = input$num_var, fill = input$group_var)) +
        geom_histogram(color = "black", alpha = 0.6, bins = 30, position = "dodge") +
        labs(title = paste("Histogram of", input$num_var, "by", input$group_var)) +
        facet_wrap(as.formula(paste("~", input$group_var)))
    }
  })
  
  # Generate boxplot by group if selected
  output$boxPlot <- renderPlot({
    req(input$num_var, input$showBoxplot)
    if (input$group_var == "None") {
      ggplot(data(), aes_string(y = input$num_var)) +
        geom_boxplot(fill = "salmon") +
        labs(title = paste("Boxplot of", input$num_var))
    } else {
      ggplot(data(), aes_string(x = input$group_var, y = input$num_var)) +
        geom_boxplot(fill = "salmon") +
        labs(title = paste("Boxplot of", input$num_var, "by", input$group_var))
    }
  })
  
  # Normality test (Shapiro-Wilk) with interpretation
  output$normalityTest <- renderPrint({
    req(input$num_var, input$showNormality)
    if (input$group_var == "None") {
      result <- shapiro.test(data()[[input$num_var]])
      print(result)
      cat("\nInterpretation: ")
      if (result$p.value < 0.05) {
        cat("The p-value is less than 0.05, so we reject the null hypothesis. The data is likely not normally distributed.")
      } else {
        cat("The p-value is greater than 0.05, so we do not reject the null hypothesis. The data is likely normally distributed.")
      }
    } else {
      results <- by(data()[[input$num_var]], data()[[input$group_var]], shapiro.test)
      print(results)
      cat("\nInterpretation:\n")
      for (level in names(results)) {
        cat("\nGroup:", level, "\n")
        if (results[[level]]$p.value < 0.05) {
          cat("The p-value is less than 0.05, so we reject the null hypothesis for this group. The data in this group is likely not normally distributed.\n")
        } else {
          cat("The p-value is greater than 0.05, so we do not reject the null hypothesis for this group. The data in this group is likely normally distributed.\n")
        }
      }
    }
  })
  
  # Variance homogeneity test (Levene's Test) with interpretation
  output$homogeneityTest <- renderPrint({
    req(input$num_var, input$showHomogeneity)
    if (input$group_var == "None") {
      cat("Select a grouping variable to perform the homogeneity test.")
    } else {
      result <- leveneTest(as.formula(paste(input$num_var, "~", input$group_var)), data = data())
      print(result)
      cat("\nInterpretation: ")
      if (result$`Pr(>F)`[1] < 0.05) {
        cat("The p-value is less than 0.05, so we reject the null hypothesis. Variances are not homogeneous across groups.")
      } else {
        cat("The p-value is greater than 0.05, so we do not reject the null hypothesis. Variances are homogeneous across groups.")
      }
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
