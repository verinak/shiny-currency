library(shiny)
library(shinythemes)
library(ggplot2)
library(plotly)

source('getData.R')

ui <- fluidPage(theme = shinytheme("united"),
  
  titlePanel(h1("verinak's Currency Converter"), windowTitle = "verinak's Currency Converter"),
  div("Data collected from ", a('Fraknfurter', href='https://www.frankfurter.app/', target='_blank')),
  hr(),
  br(),
  
  fluidRow(
    
    # inputs
    column(6,
           br(),
           # from
           fluidRow(
             
             column(4,
                    numericInput("amountFrom", label = NULL, value = 1),),
             
             column(6, align = 'left',
                    selectInput("currFrom", label = NULL, 
                                choices = NULL, 
                                selected = NULL),),
           ),
           
           # to
           fluidRow(
             
             column(4, 
                    verbatimTextOutput("amountTo", placeholder = TRUE),),
             
             column(6, align = 'left',
                    selectInput("currTo", label = NULL, 
                                choices = NULL, 
                                selected = NULL),),
           ),

           # date
           fluidRow(
             column(1, style='margin-top: 10px;',
                    strong(span("At: ")),),
             column(4,
                    dateInput("date", label = NULL, value = Sys.Date(), min='1999-01-04'),),
             column(2,
                    actionButton("todayDate", "Today?"),),

           ),
           
    ),
    
    # plot
    column(6, align = 'center',
           radioButtons("plotPeriod", label=NULL, choices=c('1W'=7, '1M'=30, '1Y'=365), inline=TRUE),
           plotlyOutput('plot'),
           ),
    
    
    
  ),
  
)

server <- function(input, output, session){
  
  # get currencies on load
  currencies <<- getCurrencies()
  updateSelectInput(session, 'currFrom', choices=currencies, selected = currencies[1])
  updateSelectInput(session, 'currTo', choices=currencies, selected = currencies[2])
  
  # convert from currFrom to currTo
  observeEvent(list(input$currFrom,input$currTo,input$amountFrom,input$date), {
    
    if(input$currFrom == "" || input$currTo == "" || is.na(input$amountFrom) || length(input$date) == 0){
      return()
    }
    
    currFrom <<- input$currFrom
    currTo <<- input$currTo
    amount <- input$amountFrom
    date <- input$date
    
    
    if(input$date == Sys.Date()){
      conversion <- getLatestConversion(currFrom, currTo, amount)
    }
    else {
      conversion <- getHistoricalConversion(currFrom, currTo, amount, date)
    }

    output$amountTo <- renderText({conversion})
    
    dateOld = as.Date(date) - as.numeric(input$plotPeriod)
    output$plot <- renderPlotly({
        getPlot(dateOld, date, currFrom, currTo)
    })

  })
  
  observeEvent(input$plotPeriod, {
    
    if(input$currFrom == "" || input$currTo == "" || length(input$date) == 0){
      return()
    }
    
    date <- input$date
    dateOld <- as.Date(date) - as.numeric(input$plotPeriod)
    
    output$plot <- renderPlotly({
      getPlot(dateOld, date, currFrom, currTo)
    })
    
  })

  
  # switch currFrom and currTo if same currency is selected
  
  observeEvent(input$currFrom, {
    
    if(input$currFrom=="") {
      return()
    }
    
    if(input$currFrom == input$currTo){
      updateSelectInput(session, 'currTo', selected = currFrom)
    }
    
  })
  
  observeEvent(input$currTo, {
    
    if(input$currTo=="") {
      return()
    }
    
    if(input$currFrom == input$currTo){
      updateSelectInput(session, 'currFrom', selected = currTo)
    }
    
  })
  
  # today button sets date to today
  observeEvent(input$todayDate, {
    
    updateDateInput(session, "date", value=Sys.Date())
    
  })
  

}

shinyApp(ui = ui, server = server)

