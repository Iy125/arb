library(dplyr)

########### Check current funding #################
##Positive rate = longs are paying shorts; negative rate = shorts are paying longs
## Higher rate - long; lower rate - short

GetBybitFundingRates = function(){
  response = httr::GET("https://api.bybit.com/v2/public/tickers")
  content = httr::content(response)
  table = data.table::rbindlist(content$result) %>% 
    mutate(funding_rate = as.numeric(funding_rate),
           predicted_funding_rate = as.numeric(predicted_funding_rate),
           bid_price = as.numeric(bid_price),
           ask_price = as.numeric(ask_price),
           last_price = as.numeric(last_price),
           midpoint_funding = (funding_rate+predicted_funding_rate)/2) %>% 
    arrange(desc(midpoint_funding))
  
  return(table)
}

GetFTXFundingRates = function(){
  ts = as.integer(Sys.time())-3600
  url = paste0("https://ftx.com/api/funding_rates?start_time=",ts)
  response = httr::GET(url)
  content = httr::content(response)
  table = data.table::rbindlist(content$result) %>% 
    mutate(FTX_adjusted_rate = rate*8)
}

GetFTXPredictedFundingRates = function(){
  content = httr::GET("https://ftx.com/api/futures") %>% 
    httr::content()
  perp_list = data.table::rbindlist(content$result) %>%
    filter(type=="perpetual") %>% 
    pull(name)
  
  predicted_funding_rate_table = data.table::data.table()
  for (perp in perp_list) {
    url = paste0("https://ftx.com/api/futures/",perp,"/stats")
    content = httr::GET(url) %>% 
      httr::content()
    predicted_funding_rate = data.table::as.data.table(c(name=perp,content$result))
    predicted_funding_rate_table = rbind(predicted_funding_rate_table,predicted_funding_rate)
  }
  predicted_funding_rate_table = predicted_funding_rate_table %>% 
    mutate(adjustedNextFundingRate = nextFundingRate*8)
}

CompareBybitVsFTXFundingRates = function(){
  bybit_funding = GetBybitFundingRates() %>% 
    select(symbol, "BB_funding_rate" = funding_rate, "BB_predicted_funding_rate" = predicted_funding_rate, next_funding_time, "BB_midpoint_funding" = midpoint_funding, 
           "BB_bid" = bid_price, "BB_ask" = ask_price,"BB_price" = last_price)
  ftx_funding = GetFTXFundingRates() %>% 
    mutate(GuessedBybitSymbol = paste0(stringr::str_sub(future, end=-6),"USDT"))
  joined_table = bybit_funding %>% left_join(ftx_funding, by = c("symbol"="GuessedBybitSymbol")) %>% 
    filter(!is.na(future)) %>% 
    mutate(diff = BB_funding_rate-FTX_adjusted_rate,
           abs_diff = abs(diff),
           midpoint_diff = BB_midpoint_funding-FTX_adjusted_rate,
           abs_midpoint_diff = abs(midpoint_diff))
  return(joined_table)
}

GetFTXMarketPrices = function(){
  url = paste0("https://ftx.com/api/markets")
  response = httr::GET(url)
  content = httr::content(response)
  table = data.table::rbindlist(content$result, fill = TRUE) %>% 
    filter(type=="future") %>% 
    select(name,"ftx_bid" = bid,"ftx_ask" = ask,"ftx_price" = price)
}

rates = CompareBybitVsFTXFundingRates()
ftx_prices = GetFTXMarketPrices()

rates %>% 
  left_join(ftx_prices, by =c("future"="name")) %>% 
  mutate(est_entry_slip = if_else(diff>=0,(BB_bid-ftx_ask)/ftx_ask, (ftx_bid-BB_ask)/BB_ask),
         est_exit_slip = if_else(diff>=0,(ftx_bid-BB_ask)/BB_ask, (BB_bid-ftx_ask)/ftx_ask),
         entry_and_exit = est_entry_slip + est_exit_slip,
         est_1w_rtn = est_entry_slip + est_exit_slip + 21*abs_diff) %>% 
  mutate_if(is.numeric, ~100*.) %>% 
  mutate(BB_bid = BB_bid/100,
         BB_ask = BB_ask/100,
         BB_price = BB_price/100,
         ftx_bid = ftx_bid/100,
         ftx_ask = ftx_ask/100,
         ftx_price = ftx_price/100) %>% 
  select(symbol,
         BB_funding_rate,
         BB_predicted_funding_rate,
         future,
         FTX_adjusted_rate,
         diff,
         abs_diff,
         midpoint_diff,
         abs_midpoint_diff,
         est_entry_slip,
         est_exit_slip,
         entry_and_exit,
         est_1w_rtn) %>%
  arrange(-abs_diff) %>% 
  View