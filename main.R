keep_list <- c("data.env")
rm(list=ls(all=TRUE)[!ls(all=TRUE) %in% keep_list])

library(quantmod)
library(dplyr)
source("load_data.R")
cybc.list <- c("ETH","BTC","BCH","LTC","RPL")

if (!exists("data.env")) {
  data.env <<- new.env(parent=globalenv())
  load_data(cybc.list)
}
var.env <<- new.env(parent=globalenv())
var.env$Returns <- as.numeric(data.env$BTC.xts[,"Close"])
print(str(var.env$Returns))
print(colnames(var.env$Returns))
print(typeof(var.env$Returns))

for (cybc.str in c("BTC")) {
  calc_look_forward(cybc.str)
}
