library(shiny)
library(DT)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Monte-Carlo Portfolio-Simulation"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
		  # Input: Slider for the number of bins ----
		fluidRow(
			column(6,
				textInput("stock1", "Stock 1", "SPY")),
			column(5,
				numericInput("w1", "Portf. %", 25, 
				   min = 1, max = 100))
		),
		fluidRow(
			column(6,
				textInput("stock2", "Stock 2", "SPY")),
			column(5,
				numericInput("w1", "Portf. %", 25, 
				   min = 1, max = 100))
		),
		fluidRow(
			column(6,
				textInput("stock3", "Stock 3", "SPY")),
			column(5,
				numericInput("w1", "Portf. %", 25, 
				   min = 1, max = 100))
		),
		fluidRow(
			column(6,
				textInput("stock4", "Stock 4", "SPY")),
			column(5,
				numericInput("w1", "Portf. %", 25, 
				   min = 1, max = 100))
		),
		fluidRow(
			column(6,
				textInput("stock5", "Stock 5", "SPY")),
			column(5,
				numericInput("w1", "Portf. %", 25, 
				   min = 1, max = 100))
		),
		fluidRow(
		column(7,
			dateInput("date", 
				"Starting Date", 
				"2013-01-01", 
				format = "yyyy-mm-dd")),
			dateInput("date", 
				"End Date", 
				"2014-01-01", 
				format = "yyyy-mm-dd"))
				
		),
		fluidRow(
			column(5,
				numericInput("sims", "Number of Simulations", 51, 
				min = 1000, max = 100000, step = 10))
)
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
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
