library(shiny)
library(quantmod)

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
      fluidRow(
        plotOutput(outputId = "distPlot")
      ),
      fluidRow(
        column(12, textOutput("textop1"))
      )
    )
  )
)


# Define server logic
server <- function(input, output) {
  
  # reactive expression
  stocks_reactive <- eventReactive( input$submit, {
    #stocks <- c(input$stock1, input$stock2, input$stock3, input$stock4, input$stock5)
    stocks <- c(input$stock1)
  })
  
  # reactive expression
  weights_reactive <- eventReactive( input$submit, {
    weights <- c(input$w1, input$w2, input$w3, input$w4, input$w5)
  })
  
  # text output
  output$textop1 <- renderText({
    stocks <- stocks_reactive()
    weights <- weights_reactive()
    
    stock1prices <- getStockPrices(stocks[1])
    stock1mean <- calcMean(stock1prices)
    stock1sd <- calcSd(stock1prices, stock1mean)
    
    print(paste("Stock 1 Mean: ", stock1mean, " Stock 1 SD: " , stock1sd))
    
    

  })
  
}



# Load Historic Data from Yahoo
# Args Stockname, fromDate, toDate
getStockPrices <- function(stock){
  environment<-new.env()
  
  options("getSymbols.yahoo.warning"=FALSE)
  getSymbols(stock,
             env = environment,
             src = "yahoo",
             from = as.Date("2018-01-01"),
             to = as.Date("2019-01-01"))
  
  output<-get(stock, envir = environment)
  
  closingPrices<-Cl(output)
  
  return (closingPrices) 
}

calcMean <- function(stockprices){
  counter = 0
  sum = 0
  for (j in stockprices) {
    counter = counter + 1
    sum = sum + j
    
  }
  print("Sum of Stock is / Number of Stock values is")
  print(sum)
  print(counter)
  mean = sum / counter

  return (mean)
}

calcSd <- function(stockprices, stockmean){
  counter = 0
  temp1 = 0
  for (j in stockprices) {
    counter = counter + 1
    temp1 = temp1 + ((j-stockmean) * (j-stockmean))
  }
  
  sd = sqrt(temp1 / (counter-1))
  
  return (sd)
}


# Calculate Growth
calculateGrowth <- function(values){
  print(values)
}


# Create Shiny app ----
shinyApp(ui = ui, server = server)