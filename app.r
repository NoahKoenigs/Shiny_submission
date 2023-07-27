library(shiny)
library(ggplot2)
## Define UI
ui <- fluidPage(
  titlePanel("CSV Linear Reader"),
  # Subtitles
   h3("Optimized for csv files with column headings"),
    h4("If there are no column headings, they will auto-populate"),
    h5("It is advised to NOT change auto-populated names"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      tags$hr(),
      checkboxInput("header", "Header", TRUE),
        #Adding in button options
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      tags$hr(),
      radioButtons("disp", "Display",
                   choices = c(Head = "Preview",
                               None = "None",
                               All = "All"),
                   selected = "None"),
        #Adding in action buttons
      actionButton("reset_btn", "Reset"),
      actionButton("histogram_btn", "Histogram"),
      actionButton("model_btn", "Add Linear Model"),
        # label Axis X and Y
      textInput("x_col", "Select X Column:", NULL),
      textInput("y_col", "Select Y Column:", NULL)
    ),
    
    mainPanel(
      plotOutput("plot"),
      tableOutput("contents")
    )
  )
)
# Server and Reactivity to Inputs
server <- function(input, output, session) {
  dataInput <- reactive({
    if (is.null(input$file1))
      return(NULL)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    
    if (!input$header) {
      colnames(df) <- c("Column 1", "Column 2")
    }
    
    return(df)
  })
  # Observing column auto-inputs
  observe({
    if (!is.null(dataInput())) {
      updateSelectInput(session, "x_col", choices = colnames(dataInput()))
      updateSelectInput(session, "y_col", choices = colnames(dataInput()))
      updateSelectInput(session, "x_col", selected = colnames(dataInput())[1])
      updateSelectInput(session, "y_col", selected = colnames(dataInput())[2])
    }
  })
  #add in reset button for page
  observeEvent(input$reset_btn, {
    updateTextInput(session, "dem", "Reset")
    updateRadioButtons(session, "disp", selected = "None")
    updateRadioButtons(session, "sep", selected = ",")
    updateRadioButtons(session, "quote", selected = '"')
    updateCheckboxInput(session, "header", value = TRUE)
    updateFileInput(session, "file1", NULL)
  })
  #addding histogram button option
  observeEvent(input$histogram_btn, {
    if (input$histogram_btn > 0) {
      output$plot <- renderPlot({
        if (is.null(dataInput()))
          return(NULL)
        
        x <- dataInput()[[input$x_col]]
        bins <- seq(min(x), max(x), length.out = 10)
        hist(x, breaks = bins, col = 'orange', border = 'white', main = "Histogram")
      })
    }
  })
  # Linear Model button
  observeEvent(input$model_btn, {
    if (input$model_btn > 0) {
      output$plot <- renderPlot({
        if (is.null(dataInput()))
          return(NULL)
        
        x <- dataInput()[[input$x_col]]
        y <- dataInput()[[input$y_col]]
        
        coefs <- coef(lm(y ~ x, data = dataInput()))
        intercept <- round(coefs[1], 2)
        slope <- round(coefs[2], 2)
        r2 <- round(summary(lm(y ~ x, data = dataInput()))$r.squared, 2)
        
        ggplot(dataInput(), aes_string(x = input$x_col, y = input$y_col)) +
          geom_point(colour = 'green') +
          geom_smooth(method = "lm", se = FALSE, color = 'orange') +
          ggtitle('Scatter Plot') +
          xlab('X') +
          ylab('Y') +
          geom_text(aes(x = 10, y = 21, label = paste("Intercept =", intercept))) +
          geom_text(aes(x = 10, y = 20, label = paste("Slope =", slope))) +
          geom_text(aes(x = 10, y = 19, label = paste("R-squared =", r2)))
      })
    }
  })

  #Starting Plot
  output$plot <- renderPlot({
    if (is.null(dataInput()))
      return(NULL)
    
    x <- dataInput()[[input$x_col]]
    y <- dataInput()[[input$y_col]]
    
    ggplot(dataInput(), aes_string(x = input$x_col, y = input$y_col)) +
      geom_point(colour = 'green') +
      ggtitle('Scatter Plot') +
      xlab('X') +
      ylab('Y')
  })
  # Table will NOT auto-populate
  output$contents <- renderTable({
    if (is.null(dataInput()))
      return(NULL)
    
    if (input$disp == "Preview") {
      return(head(dataInput()))
    } else if (input$disp == "All") {
      return(dataInput())
    } else {
      return(NULL)
    } 
  })       
}

shinyApp(ui = ui, server = server)







