library(httr)
library(jsonlite)


getCurrencies <- function() {
  
  tryCatch(expr = {
    res = GET("https://api.frankfurter.app/currencies")
    data = fromJSON(rawToChar(res$content))
    return(names(data))
  },
  error = function(e){
    return('Could Not Connect')
  })
  
}


getLatestConversion <- function(currFrom, currTo, amount) {
  
  tryCatch(expr = {
    res = GET(paste0("https://api.frankfurter.app/latest?from=", currFrom, "&to=", currTo, "&amount=", amount))
    data = fromJSON(rawToChar(res$content))
    return(data[['rates']][[currTo]])
  },
  error = function(e){
    return('Could Not Connect ')
  })
  
}


getHistoricalConversion <- function(currFrom, currTo, amount, time){
  
  tryCatch(expr = {
    res = GET(paste0("https://api.frankfurter.app/", time, "?from=", currFrom, "&to=", currTo, "&amount=", amount))
    data = fromJSON(rawToChar(res$content))
    return(data[['rates']][[currTo]])
  },
  error = function(e){
    return('Could Not Connect')
  })
  
}


getPlot <- function(timeFrom, timeTo, currFrom, currTo){
  
  tryCatch(expr = {
    res = GET(paste0("https://api.frankfurter.app/", timeFrom, "..", timeTo, "?from=", currFrom, "&to=", currTo))
    data = fromJSON(rawToChar(res$content))
    
    rates = c()
    for(r in names(data$rates)){
      rates = c(rates, as.numeric((data$rates)[[r]][[currTo]]))
    }
    
    plotData = data.frame(time = as.Date(names(data$rates)), rates = rates)
    
    plot <-ggplot(plotData, aes(x=time, y=rates)) + 
      geom_line(colour = '#e95420', size = 0.75)
    plot <- ggplotly(plot) %>% layout(hovermode = "x unified")
    
    return(plot)
  },
  error = function(e){
    return('Could Not Connect')
  })
  
}

