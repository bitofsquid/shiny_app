library(tidyquant)
library(shiny)

### The main idea behind this app came from a post on RStudio's RViews blog
### (found here: http://bit.ly/2l5Xze9) where sector correlations were calculated via flexdashboard
### BizSci also borrowed RViews work, but implemented a much more streamlined version via 'tidyquant'
### albeit without the webapp functionality (found here: http://bit.ly/2lh9zNo)

### I decided to take pieces of both approaches and create my own shiny app that calculates rolling
### correlations between two sector SPDR ETFs of a user's choosing. The app also lets a user select the
### return periodicity, rolling window length, and return type. An additional section of the app also 
### displays useful equity market info as of an inputted date.

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


## Initiate shiny app, build the webpage, and create the final plot
ui <- navbarPage("Equity Markets Monitor",
  tabPanel("Market Performance",
    sidebarPanel(
      radioButtons("view", label = "Select lookback period:",
                   choices = c("3 Months" = 90,
                               "6 Months" = 180,
                               "1 Year" = 360),
                   selected = 180),
      sliderInput("n", label = "Select moving average window:",
                  min = 5, max = 30, value = 10)
    ),
      mainPanel(
        tabsetPanel(
          tabPanel("S&P 500 Index", plotOutput("plot_sp")),
          tabPanel("CBOE Volatility Index", plotOutput("plot_vix"))
        )
      )
  ),
  tabPanel("Sector Correlations",
    sidebarPanel(
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
                   selected = "arithmetic")
    ),
      mainPanel(
        plotOutput("plot_cor")
      )
  )
)

server <- function(input, output){
  
  output$plot_sp <- renderPlot({
    daterange <- interval(today() - as.numeric(input$view), today())
    sp <- prices %>%
          filter(ticker == "^GSPC" & date %within% daterange) %>%
          tq_mutate(ohlc_fun = Ad, mutate_fun = SMA, n = input$n) %>%
          ungroup() %>%
          select(date, adjusted, SMA)
    sp <- as_xts(sp, date_col = date)
    
    plot.xts(sp[,"adjusted"], main = "S&P 500 Index Level", cex.main = 1.5)
    lines(sp[,"SMA"], col = "blue")
  })
  
  output$plot_vix <- renderPlot({
    daterange <- interval(today() - as.numeric(input$view), today())
    vix <- prices %>%
           filter(ticker == "^VIX" & date %within% daterange) %>%
           tq_mutate(ohlc_fun = Ad, mutate_fun = SMA, n = input$n) %>%
           ungroup() %>%
           select(date, adjusted, SMA)
    vix <- as_xts(vix, date_col = date)
    
    plot.xts(vix[,"adjusted"], main = "CBOE Volatility Index Level", cex.main = 1.5)
    lines(vix[,"SMA"], col = "red")
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
  
}

shinyApp(ui = ui, server = server) 

