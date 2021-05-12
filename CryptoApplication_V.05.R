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

ui <- fluidPage(#theme = shinytheme("slate"),
                
#titlePanel("Cryptocurrency Historical Summary and Graph"),

navbarPage("Crypto Forecast V0.05",
    tabPanel("Define", 
    sidebarLayout(
            
#     Sidebar panel for inputs ----
        sidebarPanel(
            
#           textInput(inputId = "caption",
#                label = "Caption:",
#                value = "Data Summary"),
                
#     Input: Selector for choosing dataset ----
            selectInput(inputId = "dataset",
                label = "Choose a Crypto Currency:",
                choices = c("Bitcoin", "Ethereum", "Ripple","Binance")),
            
#           uiOutput("daterange")
        
#            dateRangeInput('dateRange',
#                format = "mm/dd/yyyy",
#                label = 'Date Range',
#                start = Sys.Date() - 365, end = Sys.Date() + 0),
       
#           sliderInput("year", "Year released", 2010, 2014, value = c(2010, 2014),
                   #sep = ""),
       
        "Add high level descripion of application"
       
        ),
                    
#     Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Formatted text for caption ----
            h3("Data Summary"),
                        
            # Output: Verbatim text for data summary ----
            verbatimTextOutput("summary"),
                        
              br(),
            
            h3("dyGraph Output"),
            
              br(),
            
            dygraphOutput("dyplot"),
            
              br(),
            
            DT::dataTableOutput('view')
            
                )
            )
        ),

    tabPanel("Predict", "Will Add Sidebar, Main Panel, and Forecast Graphs",
    sidebarPanel(
      
      selectInput(inputId = "dataset2",
                  label = "Choose a dataset:",
                  choices = c("Bitcoin", "Ethereum", "Ripple","Binance")),
      
      actionButton("button", "Refresh",
                   style = "background-color:#FFFFFF;
                      color:#000000;
                      border-color:#000000;
                      border-style:solid;
                      border-width:1px;
                      border-radius:0%;
                      font-size:18px;"),
    ),  
    
    mainPanel("main panel",
    
    dygraphOutput("plot2"),
    
        )
      ),
      
    tabPanel("Analyze", h3("Forecast Your Own CoinGecko CSV Data"), 
             #Title ="Row Test",
             hr(),
             
             dygraphOutput("userprophetplot"),
             
             hr(),
             
             fluidRow(
               
               column(3,
                      "add instructions",
               ),
               
               column(3,
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
               
               column(5,
                      tableOutput("contents"),
                      
               ),
             )
    
    ),
    
    navbarMenu("More",
        tabPanel("About", "Will include explaination of application benefits, sources of data, etc..",

                    )
                )
            ),
        ) 
    
Bitcoin <- read.csv("C:/Users/Kyle/Desktop/btc-usd-max.csv")#,colClasses="character",na.strings="?")
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

ETH <- read.csv("C:/Users/Kyle/Desktop/eth-usd-max.csv")#,colClasses="character",na.strings="?")
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

BNB <- read.csv("C:/Users/Kyle/Desktop/bnb-usd-max.csv")#,colClasses="character",na.strings="?")
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


XRP <- read.csv("C:/Users/Kyle/Desktop/xrp-usd-max.csv")#,colClasses="character",na.strings="?")
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

prophetvars <- c("Price", "Date")
prophetBNB <- BNB[prophetvars]
prophetBNB$Date <- as.Date(prophetBNB$Date)
names(prophetBNB)[names(prophetBNB) == "Date"] <- "ds"
names(prophetBNB)[names(prophetBNB) == "Price"] <- "y"

prophetvars <- c("Price", "Date")
prophetBitcoin <- BNB[prophetvars]
prophetBitcoin$Date <- as.Date(prophetBitcoin$Date)
names(prophetBitcoin)[names(prophetBitcoin) == "Date"] <- "ds"
names(prophetBitcoin)[names(prophetBitcoin) == "Price"] <- "y"

prophetvars <- c("Price", "Date")
prophetETH <- ETH[prophetvars]
prophetETH$Date <- as.Date(prophetETH$Date)
names(prophetETH)[names(prophetETH) == "Date"] <- "ds"
names(prophetETH)[names(prophetETH) == "Price"] <- "y"

#Model1 <- prophet(prophetXRP)
#Future1 <- make_future_dataframe(Model1, periods = 365)
#Forecast1 <- predict(Model1, Future1)

#combineddata <- rbind(Bitcoin, ETH, BNB)


# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
    
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
  
  prophetdatasetInput <- reactive({
    switch(input$dataset2,
           "Bitcoin" = prophetBitcoin,
           "Ripple" = prophetXRP,
           "Ethereum" = prophetETH,
           "Binance" = prophetBNB,)
  })
  
  output$dyplot <- renderDygraph({
    ## don't output anything unless you have the data ready
    
    dygraph(dydatasetInput(), main = "Price Over Time") %>% 
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
    
#    output$plot <- renderPlotly({
        
#        ggplotly(
#          ggplot(data=datasetInput(), aes(x=Date, y=Price, group=1))+
#            geom_area(fill="#69b3a2", alpha=0.5)+
#            geom_line(color="#69b3a2")+
#            labs(title="Price History",x="Date", y ="Price ($)")+
#            theme_ipsum())
 
#    })
    
#    output$prophetplot <- renderDygraph({
      
#      dyplot.prophet(Model1, Forecast1)
#    })   
    
    output$view <- DT::renderDataTable(
        DT::datatable(
          dtdatasetInput(), options = list(
            lengthMenu = list(c(10, 15, -1), c('10', '15', 'All')),
            order = list(1, "Date"),
            pageLength = 10
          ) 
        ) 
      %>% formatCurrency(columns=c(2,3,4)),
      
    )
    
    output$contents <- renderTable({
      
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, head of that data file by default,
      # or all rows if selected, will be shown.
      
      req(input$userfile)
      
      # when reading semicolon separated files,
      # having a comma separator causes `read.csv` to error
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
          
          #move this to be button driven
          
          userModel <- prophet(prophetdf)
          userFuture <- make_future_dataframe(userModel, periods = 365)
          userForecast <- predict(userModel, userFuture)
          
          output$userprophetplot <- renderDygraph({
            
            dyplot.prophet(userModel, userForecast)
            
          })   
          
        },
        error = function(e) {
          # return a safeError if a parsing error occurs
          stop(safeError(e))
        })
      
      if(input$disp == "head") {
        return(head(df))
      }
      else {
        return(df)
      }
      
      observeEvent(input$button, {
        
        prophetvars2 <- c("Price", "Date")
        prophetdf2 <- prophetdatasetInput[prophetvars]
        prophetdf2$Date <- as.Date(prophetdf2$Date)
        #dydf <- xts(x = prophetdf2$Price, order.by = prophetdf2$Date)
        names(prophetdf2)[names(prophetdf2) == "Date"] <- "ds"
        names(prophetdf2)[names(prophetd2f) == "Price"] <- "y"
        
        Model1 <- prophet(prophetdf2)
        Future1 <- make_future_dataframe(Model1, periods = 365)
        Forecast1 <- predict(Model1, Future1)
        
        output$plot2 <- renderDygraph({
          ## don't output anything unless you have the data ready
          
          dyplot.prophet(Model1, Forecast1)
          
        })
        
      })
      
    })
    
    
    
}

# Create Shiny app ----
shinyApp(ui, server)