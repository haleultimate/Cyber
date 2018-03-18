#open hist files 
load_data <- function(cybc.list) {
  print("loading data")
  if (cybc.list[1] != "BTC") print("ERROR: Need BTC to be first in cybc.list to extract dates")
  wd <- getwd()
  cname.list <- c(".O",".H",".L",".Close",".Volume",".Market.Cap",".Adjusted",".J",".R",".D",".V",".shout")
  for (cybcstr in cybc.list) {
    cmd_str <- paste0(cybcstr,".df <- read.csv('",wd,"/data/",cybcstr,"hist.csv',stringsAsFactors = FALSE)")
    #print(cmd_str)
    eval(parse(text=cmd_str))
    cmd_str <- paste0(cybcstr,".df[(",cybcstr,".df=='-')] <- NA")
    #print(cmd_str)
    eval(parse(text=cmd_str))
    cmd_str <- paste0("cybc.length <- nrow(",cybcstr,".df)")
    #print(cmd_str)
    eval(parse(text=cmd_str))
    #print(paste0("Length of df =",cybc.length))
    cmd_str <- paste0(cybcstr,".reverse <- ",cybcstr,".df")
    #print(cmd_str)
    eval(parse(text=cmd_str))
    cmd_str <- paste0(cybcstr,".reverse[i,] <- ",cybcstr,".df[cybc.length-i+1,]")
    #print(cmd_str)
    for (i in 1:cybc.length) {
      eval(parse(text=cmd_str))
    }
    #a <- as.character(as.Date(LTC.reverse[[1,1]],"%d-%b-%Y"))
    cmd_str <- paste0("a <- as.character(as.Date(",cybcstr,".reverse[[1,1]],'%d-%b-%Y'))")
    #print(cmd_str)
    eval(parse(text=cmd_str))
    substr(a,1,1) <- "2"
    a <- as.Date(a)
    date_list <- seq.Date(a,by="days",length.out=cybc.length)
    #LTC.xts <- as.xts(LTC.reverse,order.by=date_list)
    cmd_str <- paste0("data.env$",cybcstr,".xts <- as.xts(",cybcstr,".reverse,order.by=date_list)")
    #print(cmd_str)
    eval(parse(text=cmd_str))
    cmd_str <- paste0("data.env$",cybcstr," <- data.env$",cybcstr,".xts[,c('Open','High','Low','Close','Volume','Market.Cap')]")
    #print(cmd_str)
    eval(parse(text=cmd_str))
    cmd_str <- paste0("data.env$",cybcstr," <- gsub(',','',data.env$",cybcstr,")")
    #print(cmd_str)
    eval(parse(text=cmd_str))
    cmd_str <- paste0("storage.mode(data.env$",cybcstr,") <- 'numeric'")
    #print(cmd_str)
    eval(parse(text=cmd_str))
    cmd_str <- paste0("data.env$",cybcstr,"<- merge(data.env$",cybcstr,",data.env$",cybcstr,"[,'Close'])")
    #print(cmd_str)
    eval(parse(text=cmd_str))
    cmd_str <- paste0("data.env$",cybcstr,"<- merge(data.env$",cybcstr,",(data.env$",cybcstr,"[,'High']*data.env$",cybcstr,"[,'Low'])^0.5)")
    #print(cmd_str)
    eval(parse(text=cmd_str))
    cmd_str <- paste0("data.env$",cybcstr,"<- merge(data.env$",cybcstr,",sqrt(data.env$",cybcstr,"[,'High']*data.env$",cybcstr,"[,'Low']*data.env$",cybcstr,"[,'Close'])^(1/3))")
    #print(cmd_str)
    eval(parse(text=cmd_str))
    cmd_str <- paste0("data.env$",cybcstr,"<- merge(data.env$",cybcstr,",(data.env$",cybcstr,"[,'Close']*data.env$",cybcstr,"[,'Volume']))")
    #print(cmd_str)
    eval(parse(text=cmd_str))
    cmd_str <- paste0("data.env$",cybcstr,"<- merge(data.env$",cybcstr,",log(data.env$",cybcstr,"[,ncol(data.env$",cybcstr,")]))")
    #print(cmd_str)
    eval(parse(text=cmd_str))
    cmd_str <- paste0("data.env$",cybcstr,"<- merge(data.env$",cybcstr,",(data.env$",cybcstr,"[,'Market.Cap']/data.env$",cybcstr,"[,'Close']))")
    #print(cmd_str)
    eval(parse(text=cmd_str))
    #print(paste0(cybcstr,cname.list))
    cmd_str <- paste0("colnames(data.env$",cybcstr,") <- paste0(cybcstr,cname.list)")
    #print(cmd_str)
    eval(parse(text=cmd_str))
  }
  #rm(a,cybc.length,cybcstr,date_list,i,wd)
  #print(ls(data.env))
  return(cybc.list)
}


calc_look_forward <- function(cybc.str,lf=-1) {
  ve.xts <- paste0("var.env$",cybc.str)
  c.txt <- paste0("as.numeric(gsub(',','',data.env$",cybc.str,".xts[,'Close']))")
  lagc.txt <- paste0("stats::lag(",c.txt,",lf)")
  print(cybc.str)
  eval(parse(text=paste0("print(",ve.xts,"[1,])")))
  if (exists(cybc.str,where=var.env)) {
    cmd_str <- paste0(ve.xts," <- cbind(",ve.xts,",",lagc.txt,")")
    print(cmd_str)
    eval(parse(text=cmd_str))
    cmd_str <- paste0(ve.xts," <- cbind(",ve.xts,",",c.txt,")")
    print(cmd_str)
    eval(parse(text=cmd_str))
    
    eval(parse(text=paste0("print(",ve.xts,"[1,])")))
    cmd_str <- ifelse(lf<0,
                         paste0(ve.xts," <- cbind(",ve.xts,",log(",lagc.txt,"/",c.txt,"))"),
                         paste0(ve.xts," <- cbind(",ve.xts,",log(",c.txt,"/",lagc.txt,"))"))
  } else {
    cmd_str <- ifelse(lf<0,
                         paste0(ve.xts," <- as.xts(log(",lagc.txt,"/",c.txt,"),order.by=index(data.env$date.xts))"),
                         paste0(ve.xts," <- as.xts(log(",c.txt,"/",lagc.txt,"),order.by=index(data.env$date.xts))"))
  }
  print(cmd_str)
  eval(parse(text=cmd_str))
  eval(parse(text=paste0("print(",ve.xts,"[1,])")))
  cmd_str <- ifelse(lf<0,
                       paste0("colnames(",ve.xts,")[ncol(",ve.xts,")] <- '",cybc.str,"lfn",-lf,"'"),
                       paste0("colnames(",ve.xts,")[ncol(",ve.xts,")] <- '",cybc.str,"lf",lf,"'"))
  print(cmd_str)
  eval(parse(text=cmd_str))
  eval(parse(text=paste0("print(",ve.xts,"[1,])")))
}

calc_cybc_etf <- function(cybc.list) {
  print(paste0("in calc_cybc_etf"))
  #print(cybc.list)
  #create cybc_etf wts
  for (cybc.str in cybc.list) {
    df <- paste0("data.env$",cybc.str)
    mc_field <- paste0(cybc.str,".Market.Cap")
    if (exists("cybc_etf",where=data.env)) {
      cmd_str <- paste0("data.env$cybc_etf <- merge(data.env$cybc_etf,",df,"[,'",mc_field,"'])")
      #print(cmd_str)
      eval(parse(text=cmd_str))
    } else {
      cmd_str <- paste0("data.env$cybc_etf <- data.env$",cybc.str,"[,'",mc_field,"']")
      #print(cmd_str)
      eval(parse(text=cmd_str))
    }
  }
  #create individual models for each cybc
  data.env$cybc_etf[is.na(data.env$cybc_etf)] <- 0
  data.env$cybc_etf <- data.env$cybc_etf/rowSums(data.env$cybc_etf)
  for (cybc.str in cybc.list) {
    df <- paste0("data.env$",cybc.str)
    sm.xts <- data.env$cybc_etf
    colnames(sm.xts) <- sub(".Market.Cap","",colnames(sm.xts))
    cmd_str <- paste0("dep_index <- index(",df,"[paste0(start(",df,"),'/',end(",df,"))])")
    #print(cmd_str)
    eval(parse(text=cmd_str))
    sm.xts <- sm.xts[dep_index,]  #set sm model to data dates
    for (ind_cybc in colnames(sm.xts)) 
      if ((!ind_cybc %in% cybc.list) | (ind_cybc == cybc.str)) 
        sm.xts <- sm.xts[,-which(ind_cybc==colnames(sm.xts))]
    sm.xts <- sm.xts / rowSums(sm.xts)
    cmd_str <- paste0("etf.env$",cybc.str,"sm <- sm.xts")
    #print(cmd_str)
    eval(parse(text=cmd_str))
  }  
  #construct phantom cybc etf (derived using Market.Cap wts)
  #data.env$CYBC <- NULL
  new.xts <- xts(x=rep(0,length(index(data.env$cybc_etf))),order.by=index(data.env$cybc_etf))
  for (cybc.str in cybc.list) {
    cmd_str <- paste0("tmp.vector <- data.env$cybc_etf[index(data.env$",cybc.str,"),'",cybc.str,".Market.Cap']")
    #print(cmd_str)
    eval(parse(text=cmd_str))
    cmd_str <- paste0("tmp.xts <- drop(tmp.vector)*data.env$",cybc.str)
    #print(cmd_str)
    eval(parse(text=cmd_str))
    tmp.xts <- merge(new.xts,tmp.xts)
    tmp.xts[is.na(tmp.xts)] <- 0
    if (exists("CYBC",where=data.env)) {
      data.env$CYBC <- data.env$CYBC + tmp.xts
    } else {
      data.env$CYBC <- tmp.xts
      colnames(data.env$CYBC) <- sub(cybc.str,"CYBC",colnames(data.env$CYBC))
    }
  }
  data.env$CYBC <- data.env$CYBC[,-1] #remove first column caused by merge with new.xts
}

  