# run_ps.R
print(paste("Start time:",Sys.time()))
if (!exists("stx_list.loaded")) stx_list.loaded <- NULL
keep_list <- c("data.env","load.env","etf.env","stx_list.loaded")
rm(list = ls(all=TRUE)[!ls(all=TRUE) %in% keep_list]) #clean workspace except for keep_list so we don't have to reload data

source("init_lib.R")            #library needed to load other libraries 

stx_list.loaded <- init_session(stx_list.loaded)    #load libraries, set com parms, load/clean data (if not loaded)

run_prediction()                #prediction model (reg_lib.R)

# keep_list <- c("data.env")
# rm(list=ls(all=TRUE)[!ls(all=TRUE) %in% keep_list])
# 
# library(quantmod)
# library(dplyr)
# source("load_data.R")
# cybc.list <- c("BTC","ETH","BCH","LTC","XRP")  
# 
# if (!exists("data.env")) {
#   data.env <<- new.env(parent=globalenv())
#   etf.env <<- new.env(parent=globalenv())
#   load_data(cybc.list)
#   calc_cybc_etf(cybc.list)
# }
#var.env <<- new.env(parent=globalenv())

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

# for (cybc.str in c("BTC","LTC","XRP","ETH","BCH")) { #),"ETH","XRP")) {
#   calc_look_forward(cybc.str)
# }
# 
# rets <- cbind(var.env$BTC[,'BTClfn1'],var.env$ETH[,'ETHlfn1'],var.env$XRP[,'XRPlfn1'],var.env$LTC[,'LTClfn1'])
# ret1 <- stats::lag(rets,1)
# retslag <- cbind(rets,ret1)
# print(cor(retslag,use="complete.obs"))
