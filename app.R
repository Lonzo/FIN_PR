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
    stocks <- c(input$stock1, input$stock2, input$stock3, input$stock4, input$stock5)
    #stocks <- c(input$stock1)
  })
  
  # reactive expression
  weights_reactive <- eventReactive( input$submit, {
    weights <- c(input$w1, input$w2, input$w3, input$w4, input$w5)
  })
  
  # text output
  output$textop1 <- renderText({
    stocks <- stocks_reactive()
    weights <- weights_reactive()
    
    if (stocks[1] != "") {
      stock1prices <- getStockPrices(stocks[1])
      stock1growths <- calculateGrowth(stock1prices)
      stock1growthsMean <- calcMean(stock1growths)
      stock1growthsSd <- calcSd(stock1growths, stock1growthsMean)
      print(paste("Stock 1 Growth Mean: ", stock1growthsMean, " Stock 1 Growth SD: " , stock1growthsSd))
    }
    
    if (stocks[2] != "") {
      browser()
      stock2prices <- getStockPrices(stocks[2])
      stock2mean <- calcMean(stock2prices)
      stock2sd <- calcSd(stock2prices, stock2mean)
      print(paste("Stock 2 Mean: ", stock2mean, " Stock 2 SD: " , stock2sd))
    }
    
    if (stocks[3] != "") {
      stock3prices <- getStockPrices(stocks[3])
      stock3mean <- calcMean(stock3prices)
      stock3sd <- calcSd(stock3prices, stock3mean)
      print(paste("Stock 3 Mean: ", stock3mean, " Stock 3 SD: " , stock3sd))
    }
    
    if (stocks[4] != "") {
      stock4prices <- getStockPrices(stocks[4])
      stock4mean <- calcMean(stock4prices)
      stock4sd <- calcSd(stock1prices, stock4mean)
      print(paste("Stock 4 Mean: ", stock4mean, " Stock 4 SD: " , stock4sd))
    }
    
    if (stocks[5] != "") {
      stock5prices <- getStockPrices(stocks[5])
      stock5mean <- calcMean(stock5prices)
      stock5sd <- calcSd(stock1prices, stock5mean)
      print(paste("Stock 5 Mean: ", stock5mean, " Stock 5 SD: " , stock5sd))
    }

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
  sum = 0L
  for (j in stockprices) {
    counter = counter + 1
    sum = sum + j
    
  }
  
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
  growths <- integer(length(values))
  lastVal <- NULL
  i <- 0
  
  for (j in values) {
    if (i>0){
      growths[i] <- (j-lastVal)/lastVal
    }
    
    lastVal <- j
    i <- i + 1
  }
  return (growths)
}


# Create Shiny app 
shinyApp(ui = ui, server = server)