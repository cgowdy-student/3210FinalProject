library(shiny)
library(ggplot2)
library(tidyr)
library(dplyr)
library(rmarkdown)
library(cluster)  # For k-means clustering visualization
library(factoextra)  # For PCA visualization
library(plotly)   # For enhanced plots
library(palmerpenguins)  # Load Palmer Penguins dataset

# UI
ui <- fluidPage(
  titlePanel("Data Analysis Tool"),
  
  tabsetPanel(
    # Tab 1: Exploratory Data Analysis (EDA)
    tabPanel(
      "Exploratory Analysis",
      sidebarLayout(
        sidebarPanel(
          uiOutput("variable_selector"),
          uiOutput("second_variable_selector"),
          textInput("plot_title", "Plot Title:", "Scatter Plot"),
          textInput("x_label", "X-axis Label:", "X-axis"),
          textInput("y_label", "Y-axis Label:", "Y-axis"),
          downloadButton("downloadScatterPlot", "Download Scatter Plot"),
          downloadButton("downloadReport", "Download Report"),
          # File input to choose the EDA template
          fileInput("edaTemplate", "Choose EDA Template File", accept = c(".Rmd"))
        ),
        mainPanel(
          plotOutput("plotOutput"),
          tableOutput("tableOutput"),
          plotOutput("boxPlotOutput"),
          plotOutput("scatterPlotOutput")  # Scatter plot output
        )
      )
    ),
    
    # Tab 2: PCA
    tabPanel(
      "PCA",
      sidebarLayout(
        sidebarPanel(
          actionButton("runPCA", "Run PCA")
        ),
        mainPanel(
          plotOutput("pcaPlot"),
          tableOutput("pcaTable")
        )
      )
    ),
    
    # Tab 3: K-Means Clustering
    tabPanel(
      "K-Means Clustering",
      sidebarLayout(
        sidebarPanel(
          numericInput("clusters", "Number of Clusters (k):", value = 3, min = 2, max = 10),
          actionButton("runKMeans", "Run K-Means")
        ),
        mainPanel(
          plotOutput("kmeansPlot")
        )
      )
    ),
    
    # Tab 4: Linear Regression
    tabPanel(
      "Linear Regression",
      sidebarLayout(
        sidebarPanel(
          uiOutput("response_selector"),
          uiOutput("predictor_selector"),
          actionButton("runRegression", "Run Regression")
        ),
        mainPanel(
          verbatimTextOutput("regressionSummary"),
          plotOutput("residualPlot"),
          plotOutput("qqPlot")
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  dataset <- reactive({
    # Palmer Penguins dataset directly from the package
    palmerpenguins::penguins %>%
      drop_na() %>%
      mutate(across(c(species, island, sex), as.factor))
  })
  
  # EDA
  output$variable_selector <- renderUI({
    req(dataset())
    selectInput("variable", "Choose a variable (X-axis):", choices = names(dataset()))
  })
  
  output$second_variable_selector <- renderUI({
    req(dataset())
    selectInput("second_variable", "Choose a second variable (Y-axis):", choices = names(dataset()), selected = NULL)
  })
  
  output$plotOutput <- renderPlot({
    req(input$variable)
    data <- dataset()
    var <- input$variable
    if (is.numeric(data[[var]])) {
      ggplot(data, aes_string(x = var)) +
        geom_histogram(fill = "lightblue", color = "black", bins = 10) +
        labs(title = paste("Histogram of", var), x = var, y = "Frequency") +
        theme_minimal()
    } else {
      ggplot(data, aes_string(x = var)) +
        geom_bar(fill = "lightgreen", color = "black") +
        labs(title = paste("Bar Chart of", var), x = var, y = "Frequency") +
        theme_minimal()
    }
  })
  
  output$tableOutput <- renderTable({
    req(input$variable)
    data <- dataset()
    var <- input$variable
    if (is.numeric(data[[var]])) {
      summary_stats <- summary(data[[var]])
      data.frame(Statistic = names(summary_stats), Value = as.numeric(summary_stats))
    } else {
      as.data.frame(table(data[[var]])) %>%
        setNames(c("Category", "Frequency"))
    }
  })
  
  output$boxPlotOutput <- renderPlot({
    req(input$variable)
    data <- dataset()
    var <- input$variable
    if (is.numeric(data[[var]])) {
      ggplot(data, aes_string(y = var)) +
        geom_boxplot(fill = "lightblue", color = "black") +
        labs(title = paste("Boxplot of", var), y = var) +
        theme_minimal()
    }
  })
  
  # Scatter plot
  output$scatterPlotOutput <- renderPlot({
    req(input$variable, input$second_variable)
    data <- dataset()
    ggplot(data, aes_string(x = input$variable, y = input$second_variable)) +
      geom_point(color = "blue") +
      labs(title = input$plot_title, x = input$x_label, y = input$y_label) +
      theme_minimal()
  })
  
  # Download Scatter Plot
  output$downloadScatterPlot <- downloadHandler(
    filename = function() {
      paste("scatter_plot", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      req(input$variable, input$second_variable)  # Ensure both variables are selected
      
      # Access the dataset and create the scatter plot
      data <- dataset()
      p <- ggplot(data, aes_string(x = input$variable, y = input$second_variable)) +
        geom_point(color = "blue") +
        labs(title = input$plot_title, x = input$x_label, y = input$y_label) +
        theme_minimal()
      
      # Save the plot as a PNG file
      ggsave(filename = file, plot = p, device = "png", width = 8, height = 6)
    }
  )
  
  # Download Report
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste("Data_Analysis_Report", Sys.Date(), ".docx", sep = "")
    },
    content = function(file) {
      req(input$edaTemplate)  # Ensure the EDA template is provided
      
      # Use the uploaded template for the report
      tempReport <- input$edaTemplate$datapath
      file.copy(tempReport, file.path(tempdir(), "report.Rmd"), overwrite = TRUE)
      
      # Render report
      rmarkdown::render(file.path(tempdir(), "report.Rmd"), output_file = file,
                        params = list(dataset = dataset(),
                                      variable = input$variable))
    }
  )
  
  # PCA
  observeEvent(input$runPCA, {
    req(dataset())
    numeric_data <- dataset() %>% select_if(is.numeric) %>% na.omit()
    pca_res <- prcomp(numeric_data, scale. = TRUE)
    output$pcaPlot <- renderPlot({ fviz_pca_biplot(pca_res) })
    output$pcaTable <- renderTable({ summary(pca_res)$importance })
  })
  
  # K-Means Clustering
  observeEvent(input$runKMeans, {
    req(dataset())
    numeric_data <- dataset() %>% select_if(is.numeric) %>% na.omit()
    kmeans_res <- kmeans(numeric_data, centers = input$clusters)
    output$kmeansPlot <- renderPlot({ fviz_cluster(kmeans_res, data = numeric_data) })
  })
  
  # Linear Regression
  output$response_selector <- renderUI({
    req(dataset())
    selectInput("response", "Select Response Variable:", choices = names(dataset()))
  })
  
  output$predictor_selector <- renderUI({
    req(dataset())
    selectInput("predictors", "Select Predictor Variables:", choices = names(dataset()), multiple = TRUE)
  })
  
  observeEvent(input$runRegression, {
    req(input$response, input$predictors)
    data <- dataset()
    formula <- as.formula(paste(input$response, "~", paste(input$predictors, collapse = " + ")))
    lm_model <- lm(formula, data = data)
    output$regressionSummary <- renderPrint({ summary(lm_model) })
    output$residualPlot <- renderPlot({ plot(lm_model, which = 1) })
    output$qqPlot <- renderPlot({ plot(lm_model, which = 2) })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
