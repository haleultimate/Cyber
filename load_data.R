#open hist files 
wd <- getwd()
for (btcstr in c("ETH","BTC","BCH","LTC")) {
  cmd_str <- paste0(btcstr,".df <- read.csv('",wd,"/data/",btcstr,"hist.csv',stringsAsFactors = FALSE)")
  print(cmd_str)
  eval(parse(text=cmd_str))
  cmd_str <- paste0("btc.length <- length(",btcstr,")")
  print(cmd_str)
  eval(parse(text=cmd_str))
  cmd_str <- paste0(btcstr,".reverse <- ",btcstr)
  print(cmd_str)
  eval(parse(text=cmd_str))
  cmd_str <- paste0(btcstr,".reverse[i,] <- ",btcstr,".reverse[btc.length-i,]")
  print(cmd_str)
  for (i in 1:btc.length) {
    eval(parse(text=cmd_str))
  }
}

# ethhist <- paste0(wd,"/data/ETHhist.csv")
# cat(ethhist)
# ETH <- read.csv(ethhist,stringsAsFactors = FALSE)
# BTChist <- paste0(wd,"/data/BTChist.csv")
# cat(BTChist)
# BTC <- read.csv(BTChist,stringsAsFactors = FALSE)
# RPLhist <- paste0(wd,"/data/RPLhist.csv")
# cat(RPLhist)
# RPL <- read.csv(RPLhist,stringsAsFactors = FALSE)
# BCHhist <- paste0(wd,"/data/BCHhist.csv")
# cat(BCHhist)
# BCH <- read.csv(BCHhist,stringsAsFactors = FALSE)
# LTChist <- paste0(wd,"/data/LTChist.csv")
# cat(LTChist)
# LTC <- read.csv(LTChist,stringsAsFactors = FALSE)


