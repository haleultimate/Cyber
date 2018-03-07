keep_list <- c("data.env")
rm(list=ls(all=TRUE)[!ls(all=TRUE) %in% keep_list])

library(quantmod)
library(dplyr)
source("load_data.R")
cybc.list <- c("BTC","ETH","BCH","LTC","RPL")  #"BTC" needs to be first

if (!exists("data.env")) {
  data.env <<- new.env(parent=globalenv())
  load_data(cybc.list)
}
calc_cybc_etf(cybc.list)
var.env <<- new.env(parent=globalenv())
# for (cybc.str in cybc.list) {
#   cmd_str <- paste0("var.env$",cybc.str," <- as.xts(as.numeric(gsub(',','',data.env$",cybc.str,".xts[,'Close']),
#                     order.by=index(data.env$date.xts)))")
#   
#   eval(parse(text=cmd_str))
# }
#   print(cmd_str)

# var.env$Returns <- data.env$BTC.xts
# print(str(var.env$Returns))
# print(colnames(var.env$Returns))
# print(typeof(var.env$Returns))
# print(is.xts(var.env$Returns))

# for (cybc.str in c("BTC","LTC","RPL","ETH","BCH")) { #),"ETH","RPL")) {
#   calc_look_forward(cybc.str)
# }
# 
# rets <- cbind(var.env$BTC[,'BTClfn1'],var.env$ETH[,'ETHlfn1'],var.env$RPL[,'RPLlfn1'],var.env$LTC[,'LTClfn1'])
# ret1 <- stats::lag(rets,1)
# retslag <- cbind(rets,ret1)
# print(cor(retslag,use="complete.obs"))
