library(shinythemes)
library(readxl)
library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)
library(prophet)
library(xts)
library(dygraphs)
library(DT)
library(shiny)

Bitcoin <- read.csv("C:/Users/c084238/Desktop/btc-usd-max.csv")#,colClasses="character",na.strings="?")
#Bitcoin$ds <- as.Date(Bitcoin$snapped_at)
#Bitcoin$y <- (Bitcoin$price)
#Bitcoin$Price <- (Bitcoin$price)
#Bitcoin$Date <- as.Date(Bitcoin$snapped_at)
names(Bitcoin)[names(Bitcoin) == "price"] <- "Price"
names(Bitcoin)[names(Bitcoin) == "market_cap"] <- "Market Cap"
names(Bitcoin)[names(Bitcoin) == "total_volume"] <- "Total Volume"
names(Bitcoin)[names(Bitcoin) == "snapped_at"] <- "Date"
Bitcoin$Date <- as.Date(Bitcoin$Date)
Bitcoin["Name"] = "Bitcoin"

ETH <- read.csv("C:/Users/c084238/Desktop/eth-usd-max.csv")#,colClasses="character",na.strings="?")
#ETH$ds <- as.Date(ETH$snapped_at)
#ETH$y <- (ETH$price)
#ETH$Price <- (ETH$price)
#ETH$Date <- as.Date(ETH$snapped_at)
names(ETH)[names(ETH) == "price"] <- "Price"
names(ETH)[names(ETH) == "market_cap"] <- "Market Cap"
names(ETH)[names(ETH) == "total_volume"] <- "Total Volume"
names(ETH)[names(ETH) == "snapped_at"] <- "Date"
ETH$Date <- as.Date(ETH$Date)
ETH["Name"] = "Ethereum"

BNB <- read.csv("C:/Users/c084238/Desktop/bnb-usd-max.csv")#,colClasses="character",na.strings="?")
#BNB$ds <- as.Date(BNB$snapped_at)
#BNB$y <- (BNB$price)
#BNB$Price <- (BNB$price)
#BNB$Date <- as.Date(BNB$snapped_at)
names(BNB)[names(BNB) == "price"] <- "Price"
names(BNB)[names(BNB) == "market_cap"] <- "Market Cap"
names(BNB)[names(BNB) == "total_volume"] <- "Total Volume"
names(BNB)[names(BNB) == "snapped_at"] <- "Date"
BNB$Date <- as.Date(BNB$Date)
BNB["Name"] = "Binance"


XRP <- read.csv("C:/Users/c084238/Desktop/xrp-usd-max.csv")#,colClasses="character",na.strings="?")
#XRP$ds <- as.Date(prophetXRP$snapped_at)
#XRP$y <- (prophetXRP$price)
#XRP$Price <- (XRP$price)
#XRP$Date <- as.Date(XRP$snapped_at)
names(XRP)[names(XRP) == "price"] <- "Price"
names(XRP)[names(XRP) == "market_cap"] <- "Market Cap"
names(XRP)[names(XRP) == "total_volume"] <- "Total Volume"
names(XRP)[names(XRP) == "snapped_at"] <- "Date"
XRP$Date <- as.Date(XRP$Date)
XRP["Name"] = "Ripple"

dyBitcoin <- xts(x = Bitcoin$Price, order.by = Bitcoin$Date)
dyETH <- xts(x = ETH$Price, order.by = ETH$Date)
dyBNB <- xts(x = BNB$Price, order.by = BNB$Date)
dyXRP <- xts(x = XRP$Price, order.by = XRP$Date)

prophetvars <- c("Price", "Date")
prophetXRP <- XRP[prophetvars]
prophetXRP$Date <- as.Date(prophetXRP$Date)
names(prophetXRP)[names(prophetXRP) == "Date"] <- "ds"
names(prophetXRP)[names(prophetXRP) == "Price"] <- "y"

#prophetvars <- c("Price", "Date")
prophetBNB <- BNB[prophetvars]
prophetBNB$Date <- as.Date(prophetBNB$Date)
names(prophetBNB)[names(prophetBNB) == "Date"] <- "ds"
names(prophetBNB)[names(prophetBNB) == "Price"] <- "y"

#prophetvars <- c("Price", "Date")
prophetBitcoin <- Bitcoin[prophetvars]
prophetBitcoin$Date <- as.Date(prophetBitcoin$Date)
names(prophetBitcoin)[names(prophetBitcoin) == "Date"] <- "ds"
names(prophetBitcoin)[names(prophetBitcoin) == "Price"] <- "y"

#prophetvars <- c("Price", "Date")
prophetETH <- ETH[prophetvars]
prophetETH$Date <- as.Date(prophetETH$Date)
names(prophetETH)[names(prophetETH) == "Date"] <- "ds"
names(prophetETH)[names(prophetETH) == "Price"] <- "y"

#Model1 <- prophet(prophetXRP)
#Future1 <- make_future_dataframe(Model1, periods = 365)
#Forecast1 <- predict(Model1, Future1)

#combineddata <- rbind(Bitcoin, ETH, BNB)




ui <- fluidPage(theme = shinytheme("flatly"),
  
  #titlePanel("Cryptocurrency Historical Summary and Graph"),
  
  navbarPage("Crypto Forecast V0.10",
             tabPanel("Historical Data",
                titlePanel("Cryptocurrency Historical Summary and Graph"),
                    sidebarLayout(
                        
                        # Sidebar panel for inputs ----
                        sidebarPanel(
                          
                          # textInput(inputId = "caption",
                          # label = "Caption:",
                          # value = "Data Summary"),
                          
                          # Input: Selector for choosing dataset ----
                          selectInput(inputId = "dataset",
                                      label = "Choose a Crypto Currency:",
                                      choices = c("Bitcoin", "Ethereum", "Ripple","Binance")),
                          
                          # uiOutput("daterange")
                          
                          # dateRangeInput('dateRange',
                          # format = "mm/dd/yyyy",
                          # label = 'Date Range',
                          # start = Sys.Date() - 365, end = Sys.Date() + 0),
                          
                          # sliderInput("year", "Year released", 2010, 2014, value = c(2010, 2014),
                          #sep = ""),
                          
                          h5("This tab displays the historical data of the selected crypto currency."),
                          
                          h5("You can toggle between the 4 preloaded datasets to see updated summary,
                          historical graphs and data table."), 
                          
                          h5("The summary to the right,
                          displays a summary of the price data including average price, minimum price, 
                          maximum price, mean and median price, and 1st and 3rd quartile price."),
                          
                          h5("On the 'Define' tab, you can define the data to be used in forecasting future Cryptocurrency price."),
                          
                          h5("These entries carry over to the 'Predict' tab, where you can select the length of forecast to be generated."),
                          
                          h5("Finally, the 'Upload Your Own' tab allows you to input your own .csv file from CoinGecko to produce
                             a forecast from data not embedded in the application")
                          
                        ),
                        
                        # Main panel for displaying outputs ----
                        mainPanel(
                          
                          # Output: Formatted text for caption ----
                          h3(textOutput("historicaltext")),
                          
                          hr(),
                          
                          # Output: Verbatim text for data summary ----
                          verbatimTextOutput("summary"),
                          
                          hr(),
                          
                          h3(textOutput("historicaltext2")),
                          
                          hr(),
                          
                          dygraphOutput("dyplot"),
                          
                          hr(),
                          
                          h3(textOutput("historicaltext3")),
                          
                          hr(),
                          
                          DT::dataTableOutput('view')
                          
                        )
                      )
             ),
             
             tabPanel("Define", 
                  titlePanel("Define Forecast Parameters"),
                      fluidRow(  
                        
                        sidebarPanel(
                          
                          selectInput(inputId = "definedataset",
                                      label = "Choose Cryptocurrency to be Forecasted:",
                                      choices = c("Bitcoin", "Ethereum", "Ripple","Binance")),
                          
                          tags$hr(),
                          
                          dateRangeInput(
                            inputId = "daterange",
                            label = "Select Date Range for Dataset to be Used in Forecast (Default 730 Days)",
                            start = Sys.Date() - 730, 
                            end = Sys.Date() + 1, 
                            min = min(Bitcoin$Date), 
                            max = Sys.Date() + 1, 
                            format = "mm/dd/yyyy", 
                            separator = "to" 
                          ),
                          
                          tags$hr(),
                          
                          actionButton("button1", "Define Dataset",
                                       style = "background-color:#FFFFFF;
                                              color:#000000;
                                              border-color:#000000;
                                              border-style:solid;
                                              border-width:1px;
                                              border-radius:0%;
                                              font-size:18px;"),
                          
                          tags$hr(),
                          
                          h5("Select the Cryptocurrency you would like to forecast and date range of data you could like to use to 
                             generate your forecast above"),
                          
                          h5("Click 'Define Dataset' to generate data to be used on predeict tab"),
                          
                          h5("You can compare the original dataset to your definite parameters to the right"),
                          
                          h5("Once variables are defined, move to predict tab")
                          
                          
                        ),
                        
                        mainPanel(#h3("Develop Crypto Forecast"),
                          
                          h3("Selected Cryptocurrency Full Historical Data Summary"),
                           
                        tags$hr(),
                              
                              verbatimTextOutput("ogsummary"),
                        
                        tags$hr(),
                           
                              DT::dataTableOutput('definedt1'),
                          
                        tags$hr(),
                        
                          h3("Selected Cryptocurrency Defined Data Summary"),
                        
                              verbatimTextOutput("upsummary"),
                           
                              DT::dataTableOutput('definedt2'),
                          
                          #dygraphOutput("plot2"),
                          
                        )
                        
                      ),
             ),
             
             tabPanel("Predict", 
                      
                titlePanel("Develop Cryptocurrency Forecast"),
                
                    fluidRow(  
                    
                      sidebarPanel(
                        
                        h3(textOutput("forecasttext1")),
                        
                        tags$hr(),
                        
                        h4(textOutput("forecasttext2")),
                        h4(textOutput("forecasttext3")),
                        
                        tags$hr(),
                        
                        numericInput(inputId = "periods",
                        label = "Select Length of Future Forecast (Days):",
                        value = 365),
                        
                        tags$hr(),
                        
                        h5("Click Button Below to Generate Forecast Based on Above Parameters"),
                        h5("Application will load briefly while forecast generates"),
                        
                        actionButton("button2", "Generate Forecast",
                                     style = "background-color:#FFFFFF;
                                              color:#000000;
                                              border-color:#000000;
                                              border-style:solid;
                                              border-width:1px;
                                              border-radius:0%;
                                              font-size:12px;"),
                        
                      ),
                      
                      mainPanel(#h3("Develop Crypto Forecast"),
                            
                                dygraphOutput("plot2"),
                                
                      )
                  
                ),
             ),
             
             tabPanel("Upload Your Own", h3("Forecast Your Own CoinGecko CSV Data"),
                      
                      hr(),
                  sidebarPanel(width = 12,
                      fluidRow(
                        
                          column(4, 
                               
                               fileInput("userfile", "Choose CSV File",
                                         multiple = FALSE,
                                         accept = c("text/csv",
                                                    "text/comma-separated-values,text/plain",
                                                    ".csv")),
                               
                      tags$hr(),
                               
                               radioButtons("disp", "Display",
                                            choices = c(Head = "head",
                                                        All = "all"),
                                            selected = "head"),
                          ),
                        
                          column(7, style='margin-right:5px;border-right:1px solid;border-left:1px solid;',
                                 
                               h4("Instructions:"),
                               
                               h5(HTML("<p>Go to CoinGecko's website: <a href='https://www.coingecko.com/en'>https://www.coingecko.com/en</a></p>")),
                               h5("Select Cryptocurrency you would like to use by clicking on name this will bring you to the detail screen"),
                               
                               h5("Scroll down and find the 'historical data' tab and click on there"),
                               
                               h5("Scroll to the bottom of the page and click on 'Export As' and download as csv file"),
                               
                               h5("Click 'Browse' button and select file to upload"),
                               
                               h5("The application will load briefly and generate a forecast for this .csv dataset"),
                               
                               h5("NOTE: Currently the functionality to develop a subset based on date range is not functional for uploaded datasets")
                          ),
                        
                      ),
                  ),
                      
                      fluidRow(
                        
                        hr(),
                        
                        column(8,
                          dygraphOutput("userprophetplot"),
                        ),
                        column(4,
                          verbatimTextOutput("usersummary"),
                               
                          tableOutput("contents"),
                        ),
                               
                      )
             ),
             
             navbarMenu("More",
                        tabPanel("About", 
                        
                                column(4,
                                        
                                "This R-Shiny application was developed as part of my MBA coursework at Marquette University.
                                While taking a course in visual analysics, we were tasked with developing an application to solve
                                a real world problem. I chose to develop an application that enables users to bring statistical forecasting
                                capabilities to exisiting Crytocurrency data. I developed this application over the course of the sememster
                                and view the current state as the initial version. There is some functionality I would like to include in
                                the future and will continue this project as a hobby moving forward."),
                                 
                                column(4, offset =2,
                                h3("Links"),
                                hr(),
                                HTML("<p>Primary Data Source: <a href='https://www.coingecko.com/en'>CoinGecko</a></p>"),
                                hr(),
                                HTML("<p>Information on <a href='https://www.marquette.edu''>Marquette University</a></p>"),
                                hr(),
                                HTML("<p>If you are intersted in buying Cryptocurrency: <a href='https://www.coinbase.com>Coinbase</a></p>"),
                                hr(),
                                
                        )
                        )
             )
  ),
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  output$historicaltext <- renderText({
    paste(input$dataset, "Data Summary")
  })  
  
  output$historicaltext2 <- renderText({
    paste(input$dataset, "Historical Price Over Time")
  })  
  
  output$historicaltext3 <- renderText({
    paste(input$dataset, "Historical Data")
  })  
  
  output$forecasttext1 <- renderText({
    paste("Selected Cryptocurrency:",input$definedataset)
  }) 
  
  output$forecasttext2 <- renderText({
    paste("Date Range Used for Historical Dataset:")
  }) 
  
  output$forecasttext3 <- renderText({
    paste(input$daterange[1],"-",input$daterange[2])
  }) 
  
  dydatasetInput <- reactive({
    switch(input$dataset,
           "Bitcoin" = dyBitcoin,
           "Ripple" = dyXRP,
           "Ethereum" = dyETH,
           "Binance" = dyBNB,)
  })
  
  dtdatasetInput <- reactive({
    switch(input$dataset,
           "Bitcoin" = Bitcoin,
           "Ripple" = XRP,
           "Ethereum" = ETH,
           "Binance" = BNB,)
  })
  
  datasubsetInput <- reactive({
    switch(input$definedataset,
           "Bitcoin" = Bitcoin,
           "Ripple" = XRP,
           "Ethereum" = ETH,
           "Binance" = BNB,)
  })
  
  output$dyplot <- renderDygraph({
    ## don't output anything unless you have the data ready
    
    dygraph(dydatasetInput(), main = "") %>%
      dySeries("V1", label = "Price USD") %>%
      dyRangeSelector(dateWindow = c(Sys.Date() - 365, Sys.Date() + 0))
  })
  
  output$caption <- renderText({
    input$caption
  })
  
  output$summary <- renderPrint({
    dataset <- dtdatasetInput()
    summary(dataset$Price)
  })
  
  output$ogsummary <- renderPrint({
    ogdataset <- datasubsetInput()
    summary(ogdataset$Price)
  })
  
  # output$plot <- renderPlotly({
  
  # ggplotly(
  # ggplot(data=datasetInput(), aes(x=Date, y=Price, group=1))+
  # geom_area(fill="#69b3a2", alpha=0.5)+
  # geom_line(color="#69b3a2")+
  # labs(title="Price History",x="Date", y ="Price ($)")+
  # theme_ipsum())
  
  # })
  
  # output$prophetplot <- renderDygraph({
  
  # dyplot.prophet(Model1, Forecast1)
  # })
  
  output$view <- DT::renderDataTable(
    DT::datatable(
      dtdatasetInput(), options = list(
        lengthMenu = list(c(5, 10, -1), c('5', '10', 'All')),
        order = list(1, "Date"),
        pageLength = 5
      )
    )
    %>% formatCurrency(columns=c(2,3,4)),
    
  )
  
  output$definedt1 <- DT::renderDataTable(
    DT::datatable(
      caption = "Original Dataset",
      datasubsetInput(), options = list(
        lengthMenu = list(c(5, 10, -1), c('5', '10', 'All')),
        order = list(1, "Date"),
        pageLength = 5
        
      )
    )
    %>% formatCurrency(columns=c(2,3,4)),
    
  )
  
  observeEvent(input$button1, {
    
    start_date <- input$daterange[1]
    end_date <- input$daterange[2]
    
    datasubset <- datasubsetInput() %>%
      #filter(Date >= as.Date('2020-05-13') & Date <= as.Date('2021-05-13'))
      filter(Date >= start_date & Date <= end_date)
    
    output$summary2 <- renderPrint({
      pricesummary2 <- datasubset
      summary(pricesummary2$Price)
    })
    
    output$definedt2 <- DT::renderDataTable(
      DT::datatable(
        caption = "User Defined Dataset",
        datasubset, options = list(
          lengthMenu = list(c(5, 10, -1), c('5', '10', 'All')),
          order = list(1, "Date"),
          pageLength = 5
          
        ) 
      ) 
      %>% formatCurrency(columns=c(2,3,4))
      
    )

    output$upsummary <- renderPrint({
      uppricesummary <- datasubset
      summary(uppricesummary$Price)
    })

  })
  
  observeEvent(input$button2, {
    
    start_date <- input$daterange[1]
    end_date <- input$daterange[2]
    
    datasubset <- datasubsetInput() %>%
      #filter(Date >= as.Date('2020-05-13') & Date <= as.Date('2021-05-13'))
      filter(Date >= start_date & Date <= end_date)
    
    subsetprophet <- datasubset[prophetvars]
    subsetprophet$Date <- as.Date(subsetprophet$Date)
    names(subsetprophet)[names(subsetprophet) == "Date"] <- "ds"
    names(subsetprophet)[names(subsetprophet) == "Price"] <- "y"
    
    
    Model1 <- prophet(subsetprophet)
    
    Future1 <- make_future_dataframe(Model1, periods = input$periods)
    
    Forecast1 <- predict(Model1, Future1)
    
    output$plot2 <- renderDygraph({
      
      dyplot.prophet(Model1, Forecast1)
      
    })
    
#    prophetdatasetInput <- reactive({
#      switch(input$dataset2,
#             "Bitcoin" = prophetBitcoin,
#             "Ripple" = prophetXRP,
#             "Ethereum" = prophetETH,
#             "Binance" = prophetBNB,)
#    })
    
#    output$periods <- renderText({
#      input$periods
#    })
    
    #prophetvars2 <- c("Price", "Date")
    #prophetdf2 <- prophetdatasetInput()[prophetvars]
    #prophetdf2$Date <- as.Date(prophetdf2$Date)
    #dydf <- xts(x = prophetdf2$Price, order.by = prophetdf2$Date)
    #names(prophetdf2)[names(prophetdf2) == "Date"] <- "ds"
    #names(prophetdf2)[names(prophetd2f) == "Price"] <- "y"
    
    #subsetprophet <- datasubset[prophetvars]
    #subsetprophet$Date <- as.Date(subsetprophet$Date)
    #names(subsetprophet)[names(subsetprophet) == "Date"] <- "ds"
    #names(subsetprophet)[names(subsetprophet) == "Price"] <- "y"
    
    #Model1 <- prophet(subsetprophet)
    #Future1 <- make_future_dataframe(Model1, periods = input$periods)
    #Forecast1 <- predict(Model1, Future1)
    
  })
  
  output$contents <- renderTable({
    
    req(input$userfile)
    
    tryCatch(
      {
        df <- read.csv(input$userfile$datapath,
                       header = TRUE,
                       sep = ",",
                       quote = '"')
        
        names(df)[names(df) == "price"] <- "Price"
        names(df)[names(df) == "market_cap"] <- "Market Cap"
        names(df)[names(df) == "total_volume"] <- "Total Volume"
        names(df)[names(df) == "snapped_at"] <- "Date"
        #df$Date <- as.Date(df$Date)
        #df["Name"] = "Bitcoin"
        
        #dydf <- xts(x = df$Price, order.by = df$Date)
        
        prophetvars <- c("Price", "Date")
        prophetdf <- df[prophetvars]
        prophetdf$Date <- as.Date(prophetdf$Date)
        dydf <- xts(x = prophetdf$Price, order.by = prophetdf$Date)
        names(prophetdf)[names(prophetdf) == "Date"] <- "ds"
        names(prophetdf)[names(prophetdf) == "Price"] <- "y"
        
        output$usersummary <- renderPrint({
          #dataset <- dtdatasetInput()
          summary(df$Price)
        })
        
        userModel <- prophet(prophetdf)
        userFuture <- make_future_dataframe(userModel, periods = 365)
        userForecast <- predict(userModel, userFuture)
        
        output$userprophetplot <- renderDygraph({
          
          dyplot.prophet(userModel, userForecast)
          
        })
        
      },
      error = function(e) {
        stop(safeError(e))
      })
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  
  
  
}

# Create Shiny app ----
shinyApp(ui, server) 