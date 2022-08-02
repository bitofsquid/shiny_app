suppressMessages(library(tidyquant))
suppressMessages(library(googlesheets4))

### This is the data gathering script for a shiny app that serves as a kind of market 
### dashboard for folks interested in securities prices and economic data in the US.

### This script uses the googlesheets package to download historical data on several stock
### indices and ETFs as well as US Treasury bonds and economic indicators. A googlesheet serves
### as remote storage to improve loading times for users of the app.



# Obtain daily adjusted prices for sector SPDR ETFs and add to new worksheet
ticker <- c("SPY", "XLY", "XLP", "XLE", "XLF", "XLV", "XLI", "XLB", "XLK", "XLU", 
            "^GSPC", "^VIX") 

etf_prices <- ticker %>%
  tq_get(get = "stock.prices", from = "2011-01-01") %>%
  select(symbol, date, adjusted) %>%
  spread(symbol, adjusted)


# Gather economic data for a longer period and add to a new worksheet
ec_ids <- c("PAYEMS", "UNRATE", "CPIAUCSL")

ec_prices <- ec_ids %>%
  tq_get(get = "economic.data", from = "2006-01-01") %>%
  spread(symbol, price)


# Gather historical US Treasury yields and add to a new worksheet
ust_ids <- c("DGS1", "DGS2", "DGS3", "DGS5", "DGS7", "DGS10", "DGS20", "DGS30")

ust_prices <- ust_ids %>% 
  tq_get(get = "economic.data", from = "2006-01-01") %>% 
  spread(symbol, price)


# Create a new empty Google sheet and populate tabs
tabs <- list(etf.data = etf_prices, econ.data = ec_prices, ust.data = ust_prices)
ss <- gs4_create("shiny.data1", sheets = tabs)
