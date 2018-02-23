#open hist files 
load_data <- function(cybc.list) {
  wd <- getwd()
  for (cybcstr in cybc.list) {
    cmd_str <- paste0(cybcstr,".df <- read.csv('",wd,"/data/",cybcstr,"hist.csv',stringsAsFactors = FALSE)")
    print(cmd_str)
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
    eval(parse(text=paste0("print(is.xts(data.env$",cybcstr,".xts))")))
    #eval(parse(text=paste0("rm(",cybcstr,".df,",cybcstr,".reverse)")))
  }
  #rm(a,cybc.length,cybcstr,date_list,i,wd)
  print(ls(data.env))
}


calc_look_forward <- function(cybc.str,lf=-1) {
  ve.xts <- "var.env$Returns"
  c.txt <- paste0("data.env$",cybc.str,".xts[,'Close']")
  lagc.txt <- paste0("stats::lag(",c.txt,",lf)")
  if (exists("Returns",where=var.env)) {
    cmd_str <- ifelse(lf<0,
                         paste0(ve.xts," <- cbind(",ve.xts,",log(as.numeric(",lagc.txt,")/as.numeric(",c.txt,")))"),
                         paste0(ve.xts," <- cbind(",ve.xts,",log(as.numeric(",c.txt,")/as.numeric(",lagc.txt,")))"))
  } else {
    cmd_str <- ifelse(lf<0,
                         paste0(ve.xts," <- log(as.numeric(",lagc.txt,")/as.numeric(",c.txt,"))"),
                         paste0(ve.xts," <- log(as.numeric(",c.txt,")/as.numeric(",lagc.txt,"))"))
  }
  print(cmd_str)
  eval(parse(text=cmd_str))
  cmd_str <- ifelse(lf<0,
                       paste0("colnames(",ve.xts,")[ncol(",ve.xts,")] <- '",cybc.str,"lfn",-lf,"'"),
                       paste0("colnames(",ve.xts,")[ncol(",ve.xts,")] <- '",cybc.str,"lf",lf,"'"))
  print(cmd_str)
  eval(parse(text=cmd_str))
}
