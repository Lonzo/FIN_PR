library(shiny)
library(DT)

dat <- data.frame(
  V1 = c(as.character(numericInput("x11", "", 0)), as.character(numericInput("x21", "", 1)), as.character(numericInput("x31", "", 2))),
  V2 = c(as.character(numericInput("x12", "", 3)), as.character(numericInput("x22", "", 4)), as.character(numericInput("x32", "", 5))),
  V3 = c(as.character(numericInput("x13", "", 6)), as.character(numericInput("x23", "", 7)), as.character(numericInput("x33", "", 8))),
  V4 = c(as.character(numericInput("x14", "", 9)), as.character(numericInput("x24", "", 10)), as.character(numericInput("x34", "", 11)))
)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Monte-Carlo Portfolio-Simulation"),
  
  fluidRow(
    column(5, DT::dataTableOutput('my_table')),
    column(5),
    column(5),
    column(5)
  ),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      
      numericInput("num", 
                 h3("Gesamtwert"), 
                 value = 1000)
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    
    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = input$num)
    
  })
  
  output$my_table <- DT::renderDataTable(
    dat, selection = "none", 
    options = list(searching = FALSE, paging=FALSE, ordering=FALSE, dom="t"), 
    server = FALSE, escape = FALSE, rownames= FALSE, colnames=c("", ""), 
    callback = JS("table.rows().every(function(i, tab, row) {
                  var $this = $(this.node());
                  $this.attr('id', this.data()[0]);
                  $this.addClass('shiny-input-container');
                  });
                  Shiny.unbindAll(table.table().node());
                  Shiny.bindAll(table.table().node());")
  )
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
