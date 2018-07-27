#install.packages("XML")
#install.packages("RCurl")
library(XML)
library(RCurl)
get_historical_prices <- function(currency_list) { 
  #reads table from historical data on coinmarketcap, reliant on url
  price_list <- c()
  for (i in (1:length(currency_list))){
    crypto_name <- currency_list[i]
    ticker <- ticker_list[i]
    print(crypto_name)
    cmd_string <- paste0("url <- 'https://coinmarketcap.com/currencies/",crypto_name,"/historical-data/?start=20130428&end=20180524'") #url needs the full currency name
    eval(parse(text=cmd_string))
    print(url)
    crypto <- getURL(url)
    crypto.table <- readHTMLTable(crypto, header = T, which =1, stringsAsFactors = F)
    names(crypto.table) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Market Cap")
    cmd_string <- paste0(ticker, "hist <<- crypto.table") #globally sets prices named based on ticker
    eval(parse(text=cmd_string))
  }
}

download_prices <- function() {
  for (i in 1:(length(ticker_list))){
    ticker = ticker_list[i]
    print(ticker)
    cmd_string <- paste0("write.table(",ticker,"hist,file = 'data/",ticker,"hist.csv',row.names = FALSE,sep = ',')")
    print(cmd_string)
    eval(parse(text=cmd_string))
  }
}

currency_list <- c("bitcoin"
              ,"ethereum"
              ,"ripple"
              ,"litecoin"
              ,"bitcoin-cash"
               ,"tether"
              # ,"EOS"
               ,"stellar"
               ,"cardano"
               ,"tron"
              ,"iota"
              ,"neo"
              ,"dash"
              ,"monero"
              ,"nem"
              ,"vechain"
              ,"ethereum-classic"
              ,"binance-coin"
              #,"bytecoin"
              ,"qtum"
              ,"zcash"
              ,"omisego"
              ,"icon"
              ,"lisk"
              ,"zilliqa"
              ,"ontology"
              ,"bitcoin-gold"
              ,"aeternity"
              ,"decred"
              ,"steem"
              ,"0x"
              ,"verge"
              ,"bytom"
              ,"nano"
              ,"siacoin"
              ,"bitcoin-private"
              ,"bitcoin-diamond"
              ,"bitshares"
              ,"populous"
              ,"stratis"
              ,"wanchain"
              #,"maker"
              ,"waves"
              ,"rchain"
              ,"augur"
              ,"dogecoin"
              #,"golem"
              ,"mixin"
              ,"digibyte"
              ,"waltonchain"
              )
ticker_list <- c("BTC", "ETH", "XRP", "LTC"
                 ,"BCH"
                 ,"USDT"
                 #,"EOS"
                 ,"XLM"
                 ,"ADA"
                 ,"TRX"
                 ,"MIOTA"
                 ,"NEO"
                 ,"DASH"
                 ,"XMR"
                 ,"XEM"
                 ,"VEN"
                 ,"ETC"
                 ,"BNB"
                 #,"BCN"
                 ,"QTUM"
                 ,"ZEC"
                 ,"OMG"
                 ,"ICX"
                 ,"LSK"
                 ,"ZIL"
                 ,"ONT"
                 ,"BTG"
                 ,"AE"
                 ,"DCR"
                 ,"STEEM"
                 ,"ZRX"
                 ,"XVG"
                 ,"BTM"
                 ,"NANO"
                 ,"SC"
                 ,"BTCP"
                 ,"BCD"
                 ,"BTS"
                 ,"PPT"
                 ,"STRAT"
                 ,"WAN"
                 #,"MKR"
                 ,"WAVES"
                 ,"RHOC"
                 ,"REP"
                 ,"DOGE"
                 #,"GNT"
                 ,"XIN"
                 ,"DGB"
                 ,"WTC"
                 )

get_historical_prices(currency_list)
download_prices()