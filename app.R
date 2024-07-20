library(shiny)
library(dplyr)
library(plotly)
library(DT)
library(igraph)
library(visNetwork)

# Load dataset
load_dataset <- function() {
  file_path <- "movie1.csv"  # Adjust the file path to your dataset
  if (file.exists(file_path)) {
    dataset <- read.csv(file_path, stringsAsFactors = FALSE)
    return(dataset)
  } else {
    stop(paste("File '", file_path, "' does not exist. Please check the file path.", sep = ""))
  }
}

# Define UI
ui <- fluidPage(
  titlePanel("Movie Parameter Analysis"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("genre_select"),  # Dynamic UI for genre dropdown
      selectInput("algorithm", "Select Algorithm:",
                  choices = c("Space-Saving Algorithm (SSA)", 
                              "Reservoir Sampling (RS)",
                              "Node-based Approach",
                              "Edge-based Approach")),
      sliderInput("parameter", "Set Parameter:", min = 1, max = 100, value = 10),
      actionButton("submit_button", "Submit")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Results", plotlyOutput("results_plot")),
        tabPanel("Summary", verbatimTextOutput("summary_text")),
        tabPanel("Dataset", DTOutput("dataset_table")),
        tabPanel("Genre Counts", plotlyOutput("genre_counts_plot")),
        tabPanel("Network Plot", visNetworkOutput("network_plot")),
        tabPanel("Recommendation", verbatimTextOutput("recommendation_text")),
        tabPanel("3D Scatter Plot", plotlyOutput("scatter_plot"))  # New tab for 3D scatter plot
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Load dataset
  dataset <- reactive({
    load_dataset()
  })
  
  # Dynamic UI for genre dropdown
  output$genre_select <- renderUI({
    req(dataset())
    genre_choices <- unique(dataset()$Genre)
    selectInput("genre", "Select Movie Genre:", choices = genre_choices)
  })
  
  # Define reactive expression for processing data based on user inputs
  processed_data <- reactive({
    req(input$submit_button, input$algorithm)
    
    switch(input$algorithm,
           "Space-Saving Algorithm (SSA)" = {
             dataset() %>%
               filter(Genre == input$genre) %>%
               group_by(Name) %>%
               summarize(Value = mean(Parameter))
           },
           "Reservoir Sampling (RS)" = {
             dataset() %>%
               filter(Genre == input$genre) %>%
               sample_n(input$parameter) %>%
               group_by(Name) %>%
               summarize(Value = mean(Parameter))
           },
           "Node-based Approach" = {
             dataset() %>%
               filter(Genre == input$genre) %>%
               sample_n(input$parameter) %>%
               group_by(Name) %>%
               summarize(Value = mean(Parameter))
           },
           "Edge-based Approach" = {
             dataset() %>%
               filter(Genre == input$genre) %>%
               sample_n(input$parameter) %>%
               group_by(Name) %>%
               summarize(Value = mean(Parameter))
           })
  })
  
  # Render plot based on processed data
  output$results_plot <- renderPlotly({
    req(processed_data())
    
    plot_data <- processed_data()
    
    # Plot based on the selected algorithm
    p <- ggplot(plot_data, aes(x = Name, y = Value, fill = Name)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = paste(input$algorithm, "Results"), x = "Name", y = "Value")
    
    # Convert ggplot object to plotly
    ggplotly(p)
  })
  
  # Render summary text based on processed data
  output$summary_text <- renderPrint({
    req(processed_data())
    
    summary_data <- processed_data() %>%
      summarize(
        Mean_Value = mean(Value, na.rm = TRUE),
        Max_Value = max(Value),
        Min_Value = min(Value)
      )
    
    print(summary_data)
  })
  
  # Render dataset table
  output$dataset_table <- renderDT({
    req(dataset())
    datatable(dataset())
  })
  
  # Render genre counts plot based on dataset
  output$genre_counts_plot <- renderPlotly({
    req(dataset())
    
    genre_counts <- dataset() %>%
      group_by(Genre) %>%
      summarize(Count = n_distinct(Name))
    
    p <- ggplot(genre_counts, aes(x = Genre, y = Count, fill = Genre)) +
      geom_bar(stat = "identity") +
      labs(title = "Genre Counts", x = "Genre", y = "Number of People")
    
    ggplotly(p)
  })
  
  # Render network plot based on dataset
  output$network_plot <- renderVisNetwork({
    req(dataset())
    
    # Filter dataset by selected genre
    genre_data <- dataset() %>%
      filter(Genre == input$genre)
    
    # Create igraph object for network analysis
    graph <- graph_from_data_frame(genre_data, directed = FALSE)
    
    # Plot network using visNetwork
    visNetwork(nodes = data.frame(id = V(graph)$name, label = V(graph)$name),
               edges = get.data.frame(graph, what = "edges"),
               height = "600px", width = "100%") %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visIgraphLayout()
  })
  
  # Render 3D scatter plot based on processed data
  output$scatter_plot <- renderPlotly({
    req(processed_data())
    
    plot_data <- processed_data()
    
    # 3D scatter plot based on the selected algorithm
    plot_ly(plot_data, x = ~Name, y = ~Value, z = ~Name, color = ~Name, type = "scatter3d",
            marker = list(size = 5)) %>%
      layout(title = paste(input$algorithm, "3D Scatter Plot"),
             scene = list(xaxis = list(title = "Name"),
                          yaxis = list(title = "Value"),
                          zaxis = list(title = "Name")))
  })
  
  # Render recommendation based on selected genre and algorithm
  output$recommendation_text <- renderPrint({
    req(processed_data())
    
    # Extract processed data for recommendation
    processed <- processed_data()
    
    # Select a recommendation based on some criteria (e.g., maximum value)
    recommendation <- processed %>%
      filter(Value == max(Value)) %>%
      slice(1) %>%
      pull(Name)
    
    cat("Recommendation:", recommendation)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
