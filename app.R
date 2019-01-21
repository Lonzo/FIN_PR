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
				numericInput("w2", "Portf. %", 20, 
				   min = 1, max = 100))
		),
		fluidRow(
			column(6,
				textInput("stock3", "Stock 3", "")),
			column(5,
				numericInput("w3", "Portf. %", 20,
				   min = 1, max = 100))
		),
		fluidRow(
			column(6,
				textInput("stock4", "Stock 4", "")),
			column(5,
				numericInput("w4", "Portf. %", 20,
				   min = 1, max = 100))
		),
		fluidRow(
			column(6,
				textInput("stock5", "Stock 5", "")),
			column(5,
				numericInput("w5", "Portf. %", 20, 
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
  
  observeEvent(input$sumbit, {
    getStockPrices();
    session$sendCustomMessage(type = 'testmessage',
                              message = 'Thank you for clicking')
  })
  plot(stockPrices())
  
  
}




# Create Shiny app ----
shinyApp(ui = ui, server = server)



# Load Historic Data from Yahoo
# Args Stockname, fromDate, toDate
getStockPrices <- function(){
  
  environment<-new.env()
  stock<-"SPY"
  
  options("getSymbols.yahoo.warning"=FALSE)
  getSymbols(stock,
             env = environment,
             src = "yahoo",
             from = as.Date("2009-01-01"),
             to = as.Date("2019-01-01"))
  
  output<-get(stock, envir = environment)
  
  closingPrices<-Cl(output)
  
  return (closingPrices) 
}
