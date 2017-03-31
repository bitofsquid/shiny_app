library(tidyquant)
library(shiny)

### The main idea behind this app came from a post on RStudio's RViews blog
### (found here: http://bit.ly/2l5Xze9) where sector correlations were calculated via flexdashboard
### BizSci also borrowed RViews work, but implemented a more streamlined version via 'tidyquant'
### albeit without the webapp functionality (found here: http://bit.ly/2lh9zNo)

### I decided to take pieces of both approaches and create my own shiny app that calculates rolling
### correlations between two sector SPDR ETFs of a user's choosing. The app also lets a user select the
### return periodicity, rolling window length, and return type. The first section of the app also 
### displays recent equity market performance and volatility.


# Create a list of  tickers to gather data
ticker <- c("SPY", "XLY", "XLP", "XLE", "XLF", "XLV", "XLI", "XLB", "XLK", "XLU", 
            "^GSPC", "^VIX") 

# Create a list of descriptive names for each ticker
name <- c("Total Market","Consumer Discretionary", "Consumer Staples", 
            "Energy", "Financials", "Health Care", "Industrials", 
            "Materials", "Information Technology", "Utilities",
            "S&P 500 Index - SP500", "CBOE Volatility Index - VIX")

# Combine tickers and names as tibble
ticker_name <- tibble(ticker, name)

# Gather daily prices for the past 10(ish) years for each ticker
prices <- ticker_name %>%
          tq_get(get = "stock.prices", from = "2006-01-01") %>%
          group_by(ticker, name)


# Gather select economic data for the same time period
ec_ids <- c("PAYEMS", "UNRATE", "CPIAUCSL", "GDP")
ec_names <- c("Total Nonfarm Payrolls", "Civilian Unemployment", 
              "CPI All Urban Consumers", "US Nominal GDP")
ec_ids_names <- tibble(ec_ids, ec_names)

ec_prices <- ec_ids_names %>%
             tq_get(get = "economic.data", from = "2006-01-01") %>%
             group_by(ec_ids, ec_names)
  

#Gather historical US Treasury yields for the same time period
ust_ids <- c("DGS1", "DGS2", "DGS3", "DGS5", "DGS7", "DGS10", "DGS20", "DGS30")
ust_years <- c(1, 2, 3, 5, 7, 10, 20, 30)
ust_ids_names <- tibble(ust_ids, ust_years)

ust_prices <- ust_ids_names %>% 
              tq_get(get = "economic.data", from = "2006-01-01") %>%
              group_by(ust_ids, ust_years)


# Define a function to convert dates to most recent date with data 
rightday <- function(x) {
  if(today() != as.Date(as.numeric(ust_prices[nrow(ust_prices),"date"]))) {
  return(as.Date(as.numeric(ust_prices[nrow(ust_prices),"date"])))  
  } else {
    return(today())
  }
}
  

## Initiate shiny app, build the webpage, and create the final plots
ui <- navbarPage("Market Monitor",
  tabPanel("Equity Market Charts",
    sidebarPanel(
      helpText(
        paste0("Last updated: ", today())
      ),
      radioButtons("view", label = "Select lookback period:",
                   choices = c("3 Months" = 90,
                               "6 Months" = 180,
                               "1 Year" = 360),
                   selected = 180),
      sliderInput("n", label = "Select moving average window:",
                  min = 5, max = 30, value = 10),
    helpText(
      "Source: Yahoo Finance. S&P 500 Index ticker = ^GSPC; CBOE Volatility Index (VIX) ticker = ^VIX."
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("S&P 500 Index", plotOutput("plot_sp", height = "600px")),
        tabPanel("CBOE Volatility Index", plotOutput("plot_vix", height = "600px"))
        )
    )
  ),
  tabPanel("Equity Market Correlations",
    sidebarPanel(
      helpText(
        paste0("Last updated: ", today())
      ),
      selectInput("x", label = "Select 1st sector to compare:", name[-c(11:12)]),
      selectInput("y", label = "Select 2nd sector to compare:", name[-c(11:12)]),
      radioButtons("periodicity", label = "Select return period:",
                   choices = c("Monthly" = "monthly",
                                  "Weekly" = "weekly",
                                  "Daily" = "daily"),
                   selected = "weekly"),
      sliderInput("window", label = "Select rolling window length:",
                  min = 3, max = 90, value = 52),
      radioButtons("type", label = "Select return type:",
                   choices = c("Arithmetic" = "arithmetic",
                                  "Log" = "log"),
                   selected = "arithmetic"),
    helpText(
      "Source: Yahoo Finance. Sector correlations are calculated using sector SPDR ETFs as proxies for
      true S&P 500 sector performance."
      )
    ),
      mainPanel(
        plotOutput("plot_cor", height = "600px")
      )
  ),
  tabPanel("US Economic Data",
    sidebarPanel(
      helpText(
        paste0("Last updated: ", today())
      ),
      radioButtons("view2", label = "Select lookback period:",
                   choices = c("1 Year" = 360,
                               "3 Years" = 1080,
                               "All" = 3600),
                   selected = 360),
      helpText(
        "Source: Federal Reserve Bank of St. Louis - FRED. Data for unemployment statistics and inflation are
         reported on a monthly basis while GDP estimates are reported quarterly. *Please note that the 
         bottom-right chart showing GDP defaults to a minimum lookback period of 3 years or longer."
      )
    ),
    mainPanel(
      fluidRow(column(6, plotOutput("plot_econ1")), column(6, plotOutput("plot_econ2"))),
      fluidRow(column(6, plotOutput("plot_econ3")), column(6, plotOutput("plot_econ4")))
    )
  ),
  tabPanel("US Yield Curve",
    sidebarPanel(
      helpText(
        paste0("Last updated: ", today())
      ),
      dateInput("date", label = "Select a historical date for comparison:",
                value = rightday(today())),
      helpText(
        "Source: Federal Reserve Bank of St. Louis - FRED. Data presented for current on-the-run US
        Treasury securities with maturities greater than one year. Simple linear interpolation is used
        between issued maturities."
      )
    ),
    mainPanel(
      plotOutput("plot_curve", height = "600px")
    )
  )
)


server <- function(input, output){
  
  output$plot_sp <- renderPlot({
    daterange <- interval(today() - as.numeric(input$view), today())
    sp <- prices %>%
          filter(ticker == "^GSPC") %>%
          tq_mutate(ohlc_fun = Ad, mutate_fun = SMA, n = input$n) %>%
          ungroup() %>%
          filter(date %within% daterange) %>%
          select(date, adjusted, SMA)
    sp <- as_xts(sp, date_col = date)
    
    plot.xts(sp[,"adjusted"], main = "S&P 500 Index Level", cex.main = 1.5)
    lines(sp[,"SMA"], col = "blue")
  })
  
  output$plot_vix <- renderPlot({
    daterange <- interval(today() - as.numeric(input$view), today())
    vix <- prices %>%
           filter(ticker == "^VIX") %>%
           tq_mutate(ohlc_fun = Ad, mutate_fun = SMA, n = input$n) %>%
           ungroup() %>%
           filter(date %within% daterange) %>%
           select(date, adjusted, SMA)
    vix <- as_xts(vix, date_col = date)
    
    plot.xts(vix[,"adjusted"], main = "CBOE Volatility Index Level", cex.main = 1.5)
    lines(vix[,"SMA"], col = "red")
  })
  
  output$notes <- renderText({
    
              "Price data is obtained via Yahoo finance and returns are calculated as 
              simple holding period returns over the period the user specifies 
              (monthly, weekly, daily). The information presented on the Market 
              Performance tab represents true index level performance for the S&P 500 
              and CBOE Volatility indices (tickers ^GSPC & ^VIX). The information presented 
              on the Sector correlations tab represents information on SPDR sector ETFs."
    
  })
  
  returns <- reactive({prices %>% tq_transform(ohlc_fun = Ad, transform_fun = periodReturn,
                                               period = input$periodicity, 
                                               type = input$type, 
                                               col_rename = "returns")})

  correl1 <- reactive({returns() %>% ungroup() %>% filter(name == input$x) %>% select(date, returns)})
  correl2 <- reactive({returns() %>% ungroup() %>% filter(name == input$y) %>% select(date, returns)})
  correl_final <- reactive({inner_join(correl1(), correl2(), by = "date") %>% 
                            tq_mutate_xy(x = returns.x, y = returns.y,
                                         mutate_fun = runCor, 
                                         n = input$window, 
                                         col_rename = "cor") %>%
                            select(date, cor)})
           
  output$plot_cor <- renderPlot({
                     plot.xts(as_xts(correl_final(), date_col = date),
                              main = paste(input$x, "vs", input$y, sep = " "),
                              ylim = c(0,1),
                              cex.main = 1.5)
  })
 
  output$plot_econ1 <- renderPlot({
    daterange2 <- interval(today() - as.numeric(input$view2), today())
    unemp <- ec_prices %>%
             filter(ec_names == "Total Nonfarm Payrolls") %>%
             ungroup() %>%
             mutate(change = (price - lag(price))) %>%
             filter(date %within% daterange2) %>%
             select(date, change)
    plot.xts(as_xts(unemp, date_col = date), typ = "b",
             main = "Monthly US Job Growth", ylab = "Thousands of Workers")
    legend("bottomright", legend = paste("Most Recent:", unemp[nrow(unemp),2]*1000, sep = " "), 
           bty = "n")
  })
    
  output$plot_econ2 <- renderPlot({
    daterange2 <- interval(today() - as.numeric(input$view2), today())
    unemprate <- ec_prices %>% 
      filter(ec_names == "Civilian Unemployment") %>%
      ungroup() %>%
      filter(date %within% daterange2) %>%
      select(date, price)  
    plot.xts(as_xts(unemprate, date_col = date), typ = "b",
             main = "Current US Unemployment Rate", ylab = "Percent Unemployed" )
    legend("bottomright", legend = paste("Most Recent: ", unemprate[nrow(unemprate),2], "%", sep = ""), 
           bty = "n")
  })
  
  output$plot_econ3 <- renderPlot({
    daterange2 <- interval(today() - as.numeric(input$view2), today())
    cpi <- ec_prices %>%
      filter(ec_names == "CPI All Urban Consumers") %>%
      ungroup() %>%
      mutate(change = (price/lag(price, n = 12)-1) * 100) %>%
      filter(date %within% daterange2) %>%
      select(date, change)           
    plot.xts(as_xts(cpi, date_col = date), typ = "b",
             main = "Annual Inflation Rate (CPI)", ylab = "Percent Increase in CPI")
    legend("bottomright", legend = paste("Most Recent: ", round(cpi[nrow(cpi),2],2), "%", sep = ""), 
           bty = "n")
  })
        
  output$plot_econ4 <- renderPlot({
    daterange2 <- interval(today() - as.numeric(input$view2), today())
    daterange3 <- interval(today() - 1080, today())
    if(as.numeric(as.duration(daterange2)/(24*60*60)) <= 1080){
      gdp <- ec_prices %>%
        filter(ec_names == "US Nominal GDP") %>%
        ungroup() %>%
        mutate(change = (price/lag(price, n = 4)-1)*100) %>%
        filter(date %within% daterange3) %>%
        select(date, change)
      plot.xts(as_xts(gdp, date_col = date), typ = "b",
               main = "Annual US GDP Growth (Nominal)*", ylab = "Percent Increase in GDP")
      legend("bottomright", legend = paste("Most Recent: ", round(gdp[nrow(gdp),2],2), "%", sep = ""), 
             bty = "n")
    } else
      gdp <- ec_prices %>%
        filter(ec_names == "US Nominal GDP") %>%
        ungroup() %>%
        mutate(change = (price/lag(price, n = 4)-1)*100) %>%
        filter(date %within% daterange2) %>%
        select(date, change)
      plot.xts(as_xts(gdp, date_col = date), typ = "b",
               main = "Annual US GDP Growth (Nominal)*", ylab = "Percent Increase in GDP")
      legend("bottomright", legend = paste("Most Recent: ", round(gdp[nrow(gdp),2],2), "%", sep = ""), 
             bty = "n")
  })
  
  output$plot_curve <- renderPlot({
    ust_curr <- ust_prices %>%
      ungroup() %>%
      filter(date == rightday(today())) %>%
      select(ust_years, price)
    
    ust_hist <- ust_prices %>% 
                ungroup() %>%
                filter(date == input$date) %>%
                select(ust_years, price)    
    
    plot(ust_hist, typ = "b", col = "red", xlab = "Tenor (in years)", ylab = "Yield (in percent)",
         main = "US Treasury Yield Curve", cex.main = 1.5)
    axis(1, at = ust_curr$ust_years)
    lines(ust_curr, typ = "b", col = "black")
    legend("bottomright", legend = c(rightday(today()), input$date), 
           col = c("black", "red"), lty = c(1,1))
    
  })
  
}

shinyApp(ui = ui, server = server) 

