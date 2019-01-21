library(shiny)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Monte-Carlo Portfolio-Simulation"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
		fluidRow(
			column(6,
				textInput("stock1", "Stock 1", "")),
			column(5,
				numericInput("w1", "Portf. %", 20, 
				   min = 1, max = 100))
		),
		fluidRow(
			column(6,
				textInput("stock2", "Stock 2", "")),
			column(5,
				numericInput("w1", "Portf. %", 20, 
				   min = 1, max = 100))
		),
		fluidRow(
			column(6,
				textInput("stock3", "Stock 3", "")),
			column(5,
				numericInput("w1", "Portf. %", 20,
				   min = 1, max = 100))
		),
		fluidRow(
			column(6,
				textInput("stock4", "Stock 4", "")),
			column(5,
				numericInput("w1", "Portf. %", 20,
				   min = 1, max = 100))
		),
		fluidRow(
			column(6,
				textInput("stock5", "Stock 5", "")),
			column(5,
				numericInput("w1", "Portf. %", 20, 
				   min = 1, max = 100))
		),
		fluidRow(
		column(6,
			dateInput("date", 
				"Starting Date", 
				"2013-01-01", 
				format = "yyyy-mm-dd")),
		column(6,
			 dateInput("date", 
				"End Date", 
				"2014-01-01", 
				format = "yyyy-mm-dd"))
				
		),
		fluidRow(
			column(8,
				numericInput("sims", "Number of Simulations", 1000, 
				min = 1000, max = 100000, step = 10))
    ),
		fluidRow(
		  column(5, actionButton("submit", "Go!"))
		)
		
		
		
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
  
  observeEvent(input$sumbit, {
    session$sendCustomMessage(type = 'testmessage',
                              message = 'Thank you for clicking')
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
