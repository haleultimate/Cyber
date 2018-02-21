#open hist files 
wd <- getwd()
ethhist <- paste0(wd,"/data/ETHhist.csv")
cat(ethhist)
ETH <- read.csv(ethhist)
BTChist <- paste0(wd,"/data/BTChist.csv")
cat(BTChist)
BTC <- read.csv(BTChist)
RPLhist <- paste0(wd,"/data/RPLhist.csv")
cat(RPLhist)
RPL <- read.csv(RPLhist)
BCHhist <- paste0(wd,"/data/BCHhist.csv")
cat(BCHhist)
BCH <- read.csv(BCHhist)
LTChist <- paste0(wd,"/data/LTChist.csv")
cat(LTChist)
LTC <- read.csv(LTChist)


