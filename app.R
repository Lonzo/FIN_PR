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
  			column(8,
  				numericInput("sims", "Number of Simulations", 1000, 
  				min = 1000, max = 100000, step = 10))
      ),
  		fluidRow(
  		  column(8,
  		         numericInput("numDays", "Number of Days to forecast", 25, 
  		                      min = 10, max = 1000, step = 1))
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
        plotOutput(outputId = "distPlot2")
      ),
      fluidRow(
        plotOutput(outputId = "var95plot")
      ),
      fluidRow(
        plotOutput(outputId = "es95plot")
      ),
      fluidRow(
        plotOutput(outputId = "var99plot")
      ),
      fluidRow(
        plotOutput(outputId = "es99plot")
      ),
      fluidRow(
        column(12, textOutput("textop1"))
      )
    )
  )
)


# Define server logic
server <- function(input, output) {
  var95list <- list()
  var99list <- list()
  expectedShortfall95List <- list()
  expectedShortfall99List <- list()
  portfolioStockPrices <- list()
  
  # reactive expression
  stocks_reactive <- eventReactive( input$submit, {
    stocks <- c(input$stock1, input$stock2, input$stock3, input$stock4, input$stock5)
  })
  
  # reactive expression
  weights_reactive <- eventReactive( input$submit, {
    weights <- c(input$w1, input$w2, input$w3, input$w4, input$w5)
  })
  
  simulationReactive <- eventReactive( input$submit, {
    stocks <- stocks_reactive()
    weights <- weights_reactive()

    print("Simulation-Reactive Start")
    
    portfolioStockPrices <- rep(list(rep.int(0, times=input$numDays)), input$sims)

    if (stocks[1] != "") {
      stock1prices <- getStockPrices(stocks[1])
      stock1growths <- calcGrowth(stock1prices)
      stock1growthsMean <- calcMean(stock1growths)
      stock1growthsSd <- calcSd(stock1growths, stock1growthsMean)
      simulatedStockPrices1 <- simulateGrowth(stock1growthsMean, stock1growthsSd, stock1prices[length(stock1prices)], input$numDays, input$sims)
      
      for (i in 1:input$sims) {
        for (j in 1:input$numDays) {
          portfolioStockPrices[[i]][[j]] <- portfolioStockPrices[[i]][[j]] + (simulatedStockPrices1[[i]][[j]] * input$w1/100)
        }
      }
      
      print(paste("Stock 1 Growth Mean: ", stock1growthsMean, " Stock 1 Growth SD: " , stock1growthsSd))
    }
    
    if (stocks[2] != "") {
      stock2prices <- getStockPrices(stocks[2])
      stock2growths <- calcGrowth(stock2prices)
      stock2growthsMean <- calcMean(stock2growths)
      stock2growthsSd <- calcSd(stock2growths, stock2growthsMean)
      simulatedStockPrices2 <- simulateGrowth(stock2growthsMean, stock2growthsSd, stock2prices[length(stock2prices)], input$numDays, input$sims)
      
      for (i in 1:input$sims) {
        for (j in 1:input$numDays) {
          portfolioStockPrices[[i]][[j]] <- portfolioStockPrices[[i]][[j]] + (simulatedStockPrices2[[i]][[j]] * input$w2/100)
        }
      }
      
      print(paste("Stock 2 Growth Mean: ", stock2growthsMean, " Stock 2 Growth SD: " , stock2growthsSd))
    }
    
    if (stocks[3] != "") {
      stock3prices <- getStockPrices(stocks[3])
      stock3growths <- calcGrowth(stock3prices)
      stock3growthsMean <- calcMean(stock3growths)
      stock3growthsSd <- calcSd(stock3growths, stock3growthsMean)
      simulatedStockPrices3 <- simulateGrowth(stock3growthsMean, stock3growthsSd, stock3prices[length(stock3prices)], input$numDays, input$sims)
      
      for (i in 1:input$sims) {
        for (j in 1:input$numDays) {
          portfolioStockPrices[[i]][[j]] <- portfolioStockPrices[[i]][[j]] + (simulatedStockPrices3[[i]][[j]] * input$w3/100)
        }
      }
      
      print(paste("Stock 3 Growth Mean: ", stock3growthsMean, " Stock 3 Growth SD: " , stock3growthsSd))
    }
    
    if (stocks[4] != "") {
      stock4prices <- getStockPrices(stocks[4])
      stock4growths <- calcGrowth(stock4prices)
      stock4growthsMean <- calcMean(stock4growths)
      stock4growthsSd <- calcSd(stock4growths, stock4growthsMean)
      simulatedStockPrices4 <- simulateGrowth(stock4growthsMean, stock4growthsSd, stock4prices[length(stock4prices)], input$numDays, input$sims)
      
      for (i in 1:input$sims) {
        for (j in 1:input$numDays) {
          portfolioStockPrices[[i]][[j]] <- portfolioStockPrices[[i]][[j]] + (simulatedStockPrices4[[i]][[j]] * input$w4/100)
        }
      }
      
      print(paste("Stock 4 Growth Mean: ", stock4growthsMean, " Stock 4 Growth SD: " , stock4growthsSd))
    }
    
    if (stocks[5] != "") {
      stock5prices <- getStockPrices(stocks[5])
      stock5growths <- calcGrowth(stock5prices)
      stock5growthsMean <- calcMean(stock5growths)
      stock5growthsSd <- calcSd(stock5growths, stock5growthsMean)
      simulatedStockPrices5 <- simulateGrowth(stock5growthsMean, stock5growthsSd, stock5prices[length(stock5prices)], input$numDays, input$sims)
      
      for (i in 1:input$sims) {
        for (j in 1:input$numDays) {
          portfolioStockPrices[[i]][[j]] <- portfolioStockPrices[[i]][[j]] + (simulatedStockPrices5[[i]][[j]] * input$w5/100)
        }
      }
      
      print(paste("Stock 5 Growth Mean: ", stock5growthsMean, " Stock 5 Growth SD: " , stock5growthsSd))
    }
    
    var95list <- list()
    var99list <- list()
    expectedShortfall95List <- list()
    expectedShortfall99List <- list()
    
    for (i in 1:input$sims) {
      tempPSPlist <- portfolioStockPrices[[i]]
      pspGrowth <- calcGrowth(tempPSPlist)
      
      var95list[i] <- calcVaR95(pspGrowth)
      var99list[i] <- calcVaR99(pspGrowth)
      
      var95 <- as.numeric(var95list[i])
      var99 <- as.numeric(var99list[i])
      expectedShortfall95List[i] <- calcExpectedShortfall(pspGrowth, var95)
      expectedShortfall99List[i] <- calcExpectedShortfall(pspGrowth, var99)
    }
    
    results <- list(portfolioStockPrices, var95list, var99list, expectedShortfall95List, expectedShortfall99List)
    
    
  })
  
  
  # text output
  output$textop1 <- renderText({
    
    

  })
  
  output$distPlot <- renderPlot({
    stocks <- stocks_reactive()
    weights <- weights_reactive()
    simulationData <- simulationReactive()

    print("Start Diagram 1")
    
    portfolioStockPrices <- simulationData[[1]]
    
    high <- NULL
    highid <- 0
    low <- NULL
    lowid <- 0
    counter <-0
    for (o in portfolioStockPrices) {
      counter <- counter + 1
      length <- length(o)
      if (is.null(high)) {
        high <- o[length]
      } else {
        if (o[length] > high) {
          high <- o[length]
          highid = counter
        }
      }
      
      if (is.null(low)) {
        low <- o[length]
      } else {
        if (o[length] < low) {
          low <- o[length]
          lowid = counter
        }
      }
    }
    
    plot(portfolioStockPrices[[highid]], main="Best Case Development of Portfolio", ylab = "Portfolio Price")
  })
  
  output$distPlot2 <- renderPlot({
    stocks <- stocks_reactive()
    weights <- weights_reactive()
    simulationData <- simulationReactive()
    
    print("Start Diagram 1")
    
    portfolioStockPrices <- simulationData[[1]]
    
    high <- NULL
    highid <- 0
    low <- NULL
    lowid <- 0
    counter <-0
    for (o in portfolioStockPrices) {
      counter <- counter + 1
      length <- length(o)
      if (is.null(high)) {
        high <- o[length]
      } else {
        if (o[length] > high) {
          high <- o[length]
          highid = counter
        }
      }
      
      if (is.null(low)) {
        low <- o[length]
      } else {
        if (o[length] < low) {
          low <- o[length]
          lowid = counter
        }
      }
    }
    
    plot(portfolioStockPrices[[lowid]], main="Worst Case Development of Portfolio", ylab = "Portfolio Price")
  })
  
  output$var95plot <- renderPlot({
    stocks <- stocks_reactive()
    weights <- weights_reactive()
    simulationData <- simulationReactive()
    
    print("Start Diagram 2")
    
    var95list <- simulationData[[2]]
    
    VaR95 <- as.numeric(var95list)
    hist(VaR95, main="Vale at Risk 95%", xlab="maximum loss in best 95%")
  })
  
  output$es95plot <- renderPlot({
    stocks <- stocks_reactive()
    weights <- weights_reactive()
    simulationData <- simulationReactive()
    
    print("Start Diagram 3")

    expectedShortfall95List <- simulationData[[4]]
    
    ExpectedShortfall95 <- as.numeric(expectedShortfall95List)
    hist(ExpectedShortfall95, main="Expected Shortfall of VaR95", xlab="average maximum loss in worst 5%")
  })
  
  output$var99plot <- renderPlot({
    stocks <- stocks_reactive()
    weights <- weights_reactive()
    simulationData <- simulationReactive()
    
    print("Start Diagram 4")

    var99list <- simulationData[[3]]
    
    VaR99 <- as.numeric(var99list)
    hist(VaR99, main="Value at Risk 99%", xlab="maximum loss in best 95%")
  })
  
  output$es99plot <- renderPlot({
    stocks <- stocks_reactive()
    weights <- weights_reactive()
    simulationData <- simulationReactive()
    
    print("Start Diagram 5")

    expectedShortfall99List <- simulationData[[5]]
    
    ExpectedShortfall99 <- as.numeric(expectedShortfall99List)
    hist(ExpectedShortfall99, main="Expected Shortfall of VaR99", xlab="average maximum loss in worst 5%")
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
             from = as.Date("2018-01-30"),
             to = as.Date("2019-01-30"))
  
  output<-get(stock, envir = environment)
  
  closingPrices<-Cl(output)
  
  return (closingPrices) 
}

# Calculate mean of all values in list
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

# Calculate standard deviation of all values in list
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
calcGrowth <- function(values){
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

# Simulate growth
simulateGrowth <- function(mean, sd, start, numofdays, sims) {
  allReturnValues <- list()
  for (k in 1:sims) {
    
    simulatedValues <- integer(length(numofdays))
    
    simulatedGrowth <- rnorm(numofdays, mean, sd)
    i <- 1
    for (j in simulatedGrowth) {
      if (i>2) {
        simulatedValues[i] <- simulatedValues[i-1] * (1+j)
      } else {
        simulatedValues[i] <- start * (1+j)
      }
      
      i <- i + 1
    }
    
    allReturnValues[k] <- (list(simulatedValues))
    
    # HOWTO access returned values:
    # print(allReturnValues[[1]][[1]])
    # print(allReturnValues[[1]][[2]])
    # print(allReturnValues[[1]][[3]])
    # if (k > 1) {
    #   print(allReturnValues[[2]][[1]])
    #   print(allReturnValues[[2]][[2]])
    #   print(allReturnValues[[2]][[3]])
    # }
  }
  
  return (allReturnValues)
}

# Calculate Value at Risk 95%
calcVaR95 <- function(stockprices) {

  temp <- 1.65 * calcSd(stockprices, calcMean(stockprices))
  return (temp)
  
}

# Calculate Value at Risk 99%
calcVaR99 <- function(stockprices) {
  
  temp <- 2.33 * calcSd(stockprices, calcMean(stockprices))
  return (temp)
  
}

# Calculate Expected Shortfall
calcExpectedShortfall <- function(stockgrowths, VaR) {
  VaRneg <- VaR * (-1)
  expectedShortfallList <- list()
  for (i in stockgrowths) {
    if (i < VaRneg) {
      expectedShortfallList[length(expectedShortfallList) + 1] <- i
    }
  }
  if(length(expectedShortfallList) < 1) {
    return(0)
  } else {
    meanExpShortfall <- calcMean(expectedShortfallList)
    return(meanExpShortfall)
  }
}

# Create Shiny app 
shinyApp(ui = ui, server = server)