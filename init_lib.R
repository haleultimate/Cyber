#init_lib.R
#parms that should be changed by user manually to control run_ps.R behavior
set_control_parms <- function() {
  com.env$model_loops <- 3
  com.env$add_var_levels <- c(3,5)#,20,30,40)
  com.env$opt_model <- TRUE
  com.env$load_vars <- FALSE
  com.env$load_model <- FALSE
  com.env$load_model_name <- "lf5_500_1107.vcom"
  com.env$save_model <- FALSE
  com.env$save_model_name <- "lf5_500_1107.vcom"
  com.env$save_var_n <- 0
  com.env$look_forward <- 1
  com.env$mod_var_loops <- 20
  com.env$opt_type <- "adjr2_is"  #{adjr2_is,single_oos,rolling_oos}
  com.env$run_sim <- FALSE
  com.env$data_str <- "cybc"       #{small,large,cybc}
  com.env$sdata_available <- FALSE
  
  com.env$load_multi_model <- FALSE
  com.env$model_list <- c("lf1_mar3.vcom","lf2_mar3.vcom","lf3_mar3.vcom","lf5_mar3.vcom",
                          "lf8_mar3.vcom","lf13_mar3.vcom","lf21_mar3.vcom","lf34_mar3.vcom")
  if (com.env$load_multi_model) {  #override com parameters for multi_model
    bad_model_list <- is.null(com.env$model_list)
    if (!bad_model_list) bad_model_list <- (length(com.env$model_list)<2)
    if (bad_model_list) {
      print("Must have at least two models to load to run multi_model")
      stop()
    }
    rm(bad_model_list)                    #only below settings are supported in multi-model
    com.env$load_model <- TRUE
    com.env$save_model <- FALSE
    com.env$load_model_name <- com.env$model_list[1]
    com.env$opt_model <- FALSE
    com.env$run_sim <- TRUE
    com.env$save_var_n <- 0
  }
  if (((com.env$save_var_n >0) & !com.env$opt_model) |     #check logical load/save/opt model options
      (com.env$save_model & !com.env$opt_model) ) {
    cat("initial load/save/opt model options don't make sense\n")  
    cat("com.env$load_model",com.env$load_model,"com.env$opt_model",com.env$opt_model,"\n")
    cat("com.env$save_model",com.env$save_model,"com.env$save_var_n",com.env$save_var_n,"\n")
    source("close_session.R")
  }
  if (com.env$opt_model & com.env$load_vars) {
    com.env$saved_var_files <- list.files(path=com.env$vardir)
    print(paste("Available saved_var_files:",length(com.env$saved_var_files)))
    #print(com.env$saved_var_files)
  }
  com.env$ll_bin <- -1.
  com.env$hl_bin <- 3.
  com.env$liqx <- FALSE
  com.env$verbose <- TRUE
  com.env$vlty_window <- 250
  #com.env$var_names <- NULL
}

  
#main progarm for init_lib
init_session <- function(stx_list.loaded) {
  load_custom_libraries()
  load_packages()
  set_up_environments()
  set_directories()    
  set_control_parms()                  #at top of this file, manually adjusted  
  #if (com.env$log_file) set_log_file()      #not working 
  stock_list()                         #setup stock symbols and com.env$etf_lookup
  if (com.env$data_str == "cybc") {
    if (!exists("CYBC",envir=data.env)) {
      stx_list.loaded <- load_data(com.env$stx.symbols)
      calc_cybc_etf(com.env$stx.symbols)
    }
    set_opt_cybc_settings()
  } else {
    stx_list.loaded <- load_stock_history(stx_list.loaded)     #only needed after first run if stock list changes
    load_data_files()
    set_opt_type_settings()
    remove_problem_stocks()
    calc_adjusted_HLOJRlD(com.env$stx_list)
  }
  return(stx_list.loaded)
}

load_custom_libraries <- function() {
  source("load_data.R")
  source("rnd_lib.R")            
  source("make_lib.R")
  source("calc_lib.R")              
  source("model_select.R") 
  source("reg_lib.R")
  source("port_opt.R")            #function libraries
}

load_packages <- function() {
  library(lpSolveAPI)
  library(quantmod)
  library(dplyr)
  library(forecast)
  library(leaps)
  library(fmsb)
  library(Quandl)
  Quandl.api_key('uTjzMRaw3tYDH6Dsbh2A')
}

set_up_environments <- function() {
  if (!exists("data.env")) data.env <<- new.env(parent=globalenv())
  if (!exists("load.env")) load.env <<- new.env(parent=globalenv())
  if (!exists("etf.env")) etf.env <<- new.env(parent=globalenv())
  var.env <<- new.env(parent=globalenv())
  rnd.env <<- new.env(parent=globalenv())
  com.env <<- new.env(parent=globalenv())
  sim.env <<- new.env(parent=globalenv())
}

set_directories <- function() {
  com.env$original_wd <- getwd()
  com.env$logdir <- paste0(com.env$original_wd,"/logs")
  com.env$vardir <- paste0(com.env$original_wd,"/vars")
  com.env$modeldir <- paste0(com.env$original_wd,"/models")
  com.env$logfile <- paste0(com.env$logdir,"/lf",gsub("[^0-9]","",Sys.time()),".txt")
  com.env$datadir <- paste0(com.env$original_wd,"/data")
}

load_data_files <- function() {
  print("This is where we load shout, div, pca vectors, mkt_forecast info")
  load(file=paste0(com.env$datadir,"/stop_opt_loop.dat"),envir=com.env)
  if (com.env$stop_opt_loop & com.env$opt_model) {
    print("Request for optiizing model, but stop_opt_loop is set to true, fix file")
    source("close_session.R")
  }
  shout_file <- paste0(com.env$datadir,"/shout.dat")
  if (exists(shout_file,envir = load.env)) return() 
    #if (!exists("data.env$shout_table")) 
  load(file=shout_file,envir = load.env)
  #create fake pca array
  #com.env$stx.symbols X pca vectors
  #com.env$pca_type <- "LOAD"
  #set up ETF PCA vectors
  load.env$pca_etf <- matrix(data=1,nrow=length(com.env$stx.symbols),ncol=(length(com.env$etf.symbols)+1))
  rownames(load.env$pca_etf) <- com.env$stx.symbols
  for (i in 2:(length(com.env$etf.symbols)+1)) { #set all stocks with same etf to 1, all others 0
    for (ticker in com.env$stx.symbols) {
      #print(paste(i,ticker,com.env$etf_lookup[[ticker]],com.env$etf.symbols[i-1],data.env$pca[ticker,i]))
      if (com.env$etf_lookup[[ticker]] != com.env$etf.symbols[i-1]) load.env$pca_etf[ticker,i] <- 0
    }     
  }
  #load PCA data from Kim's program
  if (com.env$data_str != "large") {
    print("Can't load PCA for any data set other than large")
  }  else {
    PCA_file <- paste0(com.env$datadir,"/PCA.dat")
    load(file=PCA_file,envir = load.env)
    #sim.env$pca <- data.env$PCA.array
  }
  #load stock models from Kim's program
  if (com.env$data_str != "large") {
    print("Can't load stk models for any data set other than large")
  }  else {
    models_file <- paste0(com.env$datadir,"/models.dat")
    load(file=models_file,envir = load.env)
    com.env$stkmod_name <- "one_res"
  }
}

set_opt_cybc_settings <- function() {  
  com.env$reg_start_date <- as.POSIXct("2013-07-01 UTC")
  com.env$reg_end_date <- as.POSIXct("2017-12-30 UTC")
  com.env$reg_date_range <- paste(com.env$reg_start_date,com.env$reg_end_date,sep="/")
  cmd_string <- paste0("com.env$reg_date_index <- index(data.env$",com.env$stx_list[1],"[com.env$reg_date_range])")   #hard coded to first stock
  print(cmd_string)
  eval(parse(text=cmd_string))
  com.env$sim_start_date <- as.POSIXct("2018-01-01 UTC")
  com.env$sim_end_date <- as.POSIXct("2018-02-27 UTC")
  com.env$sim_date_range <- paste(com.env$sim_start_date,com.env$sim_end_date,sep="/")
  cmd_string <- paste0("com.env$sim_date_index <- index(data.env$",com.env$stx_list[1],"[com.env$sim_date_range])")   #hard coded to first stock
  print(cmd_string)
  eval(parse(text=cmd_string))
  com.env$total_date_range <- paste(com.env$reg_start_date,com.env$sim_end_date,sep="/")
  cmd_string <- paste0("com.env$total_date_index <- index(data.env$",com.env$stx_list[1],"[com.env$total_date_range])")   #hard coded to first stock
  print(cmd_string)
  eval(parse(text=cmd_string))  #reg_date_index used for final regression run [not same reg_date_range for single_oos]
  com.env$date_decay = 0.9998
  com.env$date_wts <- xts(x=rep(1,length(com.env$total_date_index)),order.by=com.env$total_date_index)
  for (i in (length(com.env$reg_date_index)-1):1) com.env$date_wts[i] = com.env$date_wts[i+1]*com.env$date_decay
  switch(com.env$opt_type,
         "adjr2_is" = {
           com.env$sig <- 0.001
         },
         "single_oos" = {
           com.env$sig <- 0.01
           com.env$reg_end_date <- as.POSIXct("2009-12-30 UTC")
           com.env$reg_date_range <- paste(com.env$reg_start_date,com.env$reg_end_date,sep="/")
           com.env$oos_start_date <- as.POSIXct("2010-01-01 UTC")
           com.env$oos_end_date <- as.POSIXct("2011-12-30 UTC")
           com.env$oos_date_range <- paste(com.env$oos_start_date,com.env$oos_end_date,sep="/")
           cmd_string <- paste0("com.env$oos_date_index <- index(data.env$",com.env$stx_list[1],"[com.env$oos_date_range])")   #hard coded to first stock
           print(cmd_string)
           eval(parse(text=cmd_string))
         },
         "rolling_oos" = {
           com.env$r2_wt <- 200
           com.env$rolling_best_score <- -99999999.  #-Inf
           com.env$sig <- 0.05
           com.env$rolling_start_date <- as.Date("2008-01-01 UTC")
           com.env$period <- 365
           oos_days <- as.Date(com.env$reg_end_date) - com.env$rolling_start_date
           com.env$rolling_periods <- as.numeric(round(oos_days/com.env$period))
           end_date <- as.Date(com.env$rolling_start_date - 1) 
           for (i in 1:com.env$rolling_periods) {
             start_date <- as.Date(end_date + 1)
             if (i < com.env$rolling_periods) {
               end_date <- as.Date(start_date+com.env$period)
             } else {
               end_date <- com.env$reg_end_date
             }
             #end_date <- ifelse(i == com.env$rolling_periods,as.Date(com.env$reg_end_date),as.Date(start_date + com.env$period))
             #print(paste(i,"start_date:",start_date,"end_date",end_date))
             oos_date_range <- paste(as.POSIXct(paste(start_date,"UTC")),as.POSIXct(paste(end_date,"UTC")),sep="/")
             #print(oos_date_range)
             cmd_string <- paste0("date_index <- index(data.env$",com.env$stx_list[1],"[oos_date_range])")
             #print(cmd_string)
             eval(parse(text=cmd_string))
             #print(date_index)
             com.env$oos_start_date[[i]] <- paste(start_date,"UTC")
             com.env$oos_date_index[[i]] <- date_index 
           }
           for(i in 1:com.env$rolling_periods) print(com.env$oos_start_date[[i]])
         },
         "rolling_sim" = {},
         {cat("Error: com.env$opt_type - ",com.env$opt_type," not supported\n")
           source("close_session.R")}
  )
}

set_opt_type_settings <- function() {  
  com.env$reg_start_date <- as.POSIXct("2004-07-01 UTC")
  com.env$reg_end_date <- as.POSIXct("2011-12-30 UTC")
  com.env$reg_date_range <- paste(com.env$reg_start_date,com.env$reg_end_date,sep="/")
  cmd_string <- paste0("com.env$reg_date_index <- index(data.env$",com.env$stx_list[1],"[com.env$reg_date_range])")   #hard coded to first stock
  #print(cmd_string)
  eval(parse(text=cmd_string))
  com.env$sim_start_date <- as.POSIXct("2012-01-01 UTC")
  com.env$sim_end_date <- as.POSIXct("2012-12-31 UTC")
  com.env$sim_date_range <- paste(com.env$sim_start_date,com.env$sim_end_date,sep="/")
  cmd_string <- paste0("com.env$sim_date_index <- index(data.env$",com.env$stx_list[1],"[com.env$sim_date_range])")   #hard coded to first stock
  #print(cmd_string)
  eval(parse(text=cmd_string))
  com.env$total_date_range <- paste(com.env$reg_start_date,com.env$sim_end_date,sep="/")
  cmd_string <- paste0("com.env$total_date_index <- index(data.env$",com.env$stx_list[1],"[com.env$total_date_range])")   #hard coded to first stock
  #print(cmd_string)
  eval(parse(text=cmd_string))  #reg_date_index used for final regression run [not same reg_date_range for single_oos]
  com.env$date_decay = 0.9998
  com.env$date_wts <- xts(x=rep(1,length(com.env$total_date_index)),order.by=com.env$total_date_index)
  for (i in (length(com.env$reg_date_index)-1):1) com.env$date_wts[i] = com.env$date_wts[i+1]*com.env$date_decay
  switch(com.env$opt_type,
         "adjr2_is" = {
           com.env$sig <- 0.001
         },
         "single_oos" = {
           com.env$sig <- 0.01
           com.env$reg_end_date <- as.POSIXct("2009-12-30 UTC")
           com.env$reg_date_range <- paste(com.env$reg_start_date,com.env$reg_end_date,sep="/")
           com.env$oos_start_date <- as.POSIXct("2010-01-01 UTC")
           com.env$oos_end_date <- as.POSIXct("2011-12-30 UTC")
           com.env$oos_date_range <- paste(com.env$oos_start_date,com.env$oos_end_date,sep="/")
           cmd_string <- paste0("com.env$oos_date_index <- index(data.env$",com.env$stx_list[1],"[com.env$oos_date_range])")   #hard coded to first stock
           print(cmd_string)
           eval(parse(text=cmd_string))
         },
         "rolling_oos" = {
           com.env$r2_wt <- 200
           com.env$rolling_best_score <- -99999999.  #-Inf
           com.env$sig <- 0.05
           com.env$rolling_start_date <- as.Date("2008-01-01 UTC")
           com.env$period <- 365
           oos_days <- as.Date(com.env$reg_end_date) - com.env$rolling_start_date
           com.env$rolling_periods <- as.numeric(round(oos_days/com.env$period))
           end_date <- as.Date(com.env$rolling_start_date - 1) 
           for (i in 1:com.env$rolling_periods) {
             start_date <- as.Date(end_date + 1)
             if (i < com.env$rolling_periods) {
               end_date <- as.Date(start_date+com.env$period)
             } else {
               end_date <- com.env$reg_end_date
             }
             #end_date <- ifelse(i == com.env$rolling_periods,as.Date(com.env$reg_end_date),as.Date(start_date + com.env$period))
             #print(paste(i,"start_date:",start_date,"end_date",end_date))
             oos_date_range <- paste(as.POSIXct(paste(start_date,"UTC")),as.POSIXct(paste(end_date,"UTC")),sep="/")
             #print(oos_date_range)
             cmd_string <- paste0("date_index <- index(data.env$",com.env$stx_list[1],"[oos_date_range])")
             #print(cmd_string)
             eval(parse(text=cmd_string))
             #print(date_index)
             com.env$oos_start_date[[i]] <- paste(start_date,"UTC")
             com.env$oos_date_index[[i]] <- date_index 
           }
           for(i in 1:com.env$rolling_periods) print(com.env$oos_start_date[[i]])
         },
         "rolling_sim" = {},
         {cat("Error: com.env$opt_type - ",com.env$opt_type," not supported\n")
          source("close_session.R")}
  )
}

#used below in remove_problem_stocks
#start date currently uses stock and etf data to determine start date (want to remove etf dependency in future)
get_start_date <- function(ticker) {
  etf_ticker <- com.env$etf_lookup[ticker]
  cmd_string <- paste0("stk_start_date <- as.Date(index(data.env$",ticker,"[",com.env$days2remove,",]))")
  eval(parse(text=cmd_string))
  if (etf_ticker != "etf") {
    cmd_string <- paste0("etf_start_date <- as.Date(index(data.env$",etf_ticker,"[",com.env$days2remove,",]))")
    eval(parse(text=cmd_string))
    start_date <- max(stk_start_date,etf_start_date)
    # if ((start_date != stk_start_date) & (start_date != etf_start_date)) {
    #   print(paste("Problem in get_start_date",ticker,etf_ticker,stk_start_date,etf_start_date,start_date))
    # }
  } else {
    start_date <- stk_start_date
  }
  return(start_date)
}

get_end_date <- function(ticker) {
  cmd_string <- paste0("stk_end_date <- as.Date(index(data.env$",ticker,")[nrow(data.env$",ticker,")])")
  eval(parse(text=cmd_string))
  end_date <- min(com.env$sim_end_date,stk_end_date)
  # if ((end_date != stk_end_date) & (end_date != stk_end_date)) {
  #   print(paste("Problem in get_end_date",ticker,stk_end_date,end_date))
  # }
  return(end_date)
}

remove_problem_stocks <- function() {
  print(paste("in remove_problem_stocks",length(com.env$stx.symbols)))
  if (com.env$data_str == "large") {  #remove stx not in Kim's PCA and models data
    cmd_string <- paste0("com.env$stx.symbols <- com.env$stx.symbols[com.env$stx.symbols %in% names(load.env$",com.env$stkmod_name,")]")
    print(cmd_string)
    eval(parse(text=cmd_string))
  }
  print(length(com.env$stx.symbols))
  static.stx.symbols <- com.env$stx.symbols
  com.env$corr.threshold <- 0.3
  com.env$days2remove <- 60
  load.env$etf_corr <- NULL
  for (i in 1:length(static.stx.symbols)) {
    ticker <- static.stx.symbols[i]
    if (make.names(ticker) != ticker) {  #ticker not valid variable name in R
      print(paste("remove",ticker,"from list, not valid name",make.names(ticker)))
      com.env$stx.symbols <- com.env$stx.symbols[-which(com.env$stx.symbols == ticker)] #remove from stx list
      com.env$stx_list <- com.env$stx_list[-which(com.env$stx_list == ticker)]
      next()
    }
    cmd_string <- paste0("missing_data <- any(grepl('NA',summary(data.env$",ticker,"[,'",ticker,".Open'])))")
    eval(parse(text=cmd_string))
    if (missing_data) {  #remove stocks that have missing days (by checking Open)
      print(paste("remove",ticker,"from list, missing_data"))
      #cmd_string <- paste0("print(summary(data.env$",ticker,"[,'",ticker,".Open']))")
      #eval(parse(text=cmd_string))
      com.env$stx.symbols <- com.env$stx.symbols[-which(com.env$stx.symbols == ticker)] #remove from stx list
      com.env$stx_list <- com.env$stx_list[-which(com.env$stx_list == ticker)]
      next()
    }
    etf <- com.env$etf_lookup[ticker]
    cmd_string <- paste("corr.data <- cbind(data.env$",etf,"[,'",etf,".Adjusted'],data.env$",ticker,"[,'",ticker,".Adjusted'])",sep="")
    eval(parse(text=cmd_string))
    cmd_string <- paste("enough_history <- nrow(data.env$",ticker,"[com.env$reg_date_range]) > 320",sep="")
    eval(parse(text=cmd_string))
    if ( (!enough_history) ) {      #| (corr.val < com.env$corr.threshold) ) {
      print(paste("remove",ticker,"from list, not enough history"))
      #print(paste("remove",ticker,"from stx list, not correlated with etf",corr.val))
      com.env$stx.symbols <- com.env$stx.symbols[-which(com.env$stx.symbols == ticker)] #stx.symbols - only stocks
      com.env$stx_list <- com.env$stx_list[-which(com.env$stx_list == ticker)]          #stx_list - contains etfs
    } else {
      load.env$etf_corr[[i]] <- cor(corr.data[com.env$reg_date_range],use="complete.obs")[1,2]
    }
  }
  #rm(static.stx.symbols,corr.val,corr.data)
  names(load.env$etf_corr) <- com.env$stx.symbols
  com.env$stx <- length(com.env$stx.symbols)
  com.env$stx_list <- c(com.env$etf.symbols,com.env$stx.symbols)
  #print(com.env$stx_list)
  com.env$start_date <- lapply(com.env$stx_list,get_start_date)
  com.env$end_date <- lapply(com.env$stx_list,get_end_date)
  names(com.env$start_date) <- com.env$stx_list
  names(com.env$end_date) <- com.env$stx_list
  #print(com.env$start_date)
  if (com.env$data_str == "large") sim.env$PCA.array <- load.env$PCA.array[com.env$stx.symbols,]
  sim.env$pca_etf <- load.env$pca_etf[com.env$stx.symbols,]
  com.env$retvlty_not_calced <- TRUE
}

#loads all stock in com.env$stx_list not in stx_list.old (returns loaded list)
load_stock_history <- function(stx_list.old) {
  free_data <- TRUE
  Sys.setenv(TZ = "UTC")
  print("load_stock_history")
  adjustment <- TRUE
  com.env$load_start_date <- "2004-01-01" 
  com.env$load_end_date <- "2013-03-31"
  com.env$data_date_range <- paste(com.env$load_start_date, com.env$load_end_date,sep="/")
  if(free_data == FALSE){
    if(is.null(stx_list.old)){
      for(ticker in com.env$stx_list){
        cmd_line <- paste0("data.env$",ticker," <- Quandl('EOD/",ticker,"', type = 'xts',start_date = '",com.env$load_start_date,"',end_date = '",com.env$load_end_date,"')")
        eval(parse(text = cmd_line))
        names <- c(paste0(ticker,".Open"),paste0(ticker,".High"),paste0(ticker,".Low"),paste0(ticker,".Close"),paste0(ticker,".Volume"),"Dividend","Split",paste0(ticker,".Adjusted"),"Adj_High","Adj_Low","Adj_Close","Adj_Volume")
        cmd_line <- paste0("names(data.env$",ticker,") <- names")
        eval(parse(text = cmd_line))
        }
    }
  }
  else {
    if (is.null(stx_list.old)) {         #only load if stx_list has changed
      getSymbols(Symbols = com.env$stx_list,
                 env=data.env,
                 src = "yahoo",
                 index.class = "POSIXct",
                 from = com.env$load_start_date,
                 to = com.env$load_end_date,
                 adjust = adjustment)
  
    } else if (!identical(com.env$stx_list,stx_list.old)) {
      isNameinStxold <- com.env$stx_list %in% stx_list.old
      stx_list.new <- com.env$stx_list[!isNameinStxold]
      #print(stx_list.new)
      getSymbols(Symbols = stx_list.new,
                 env=data.env,
                 src = "yahoo",
                 index.class = "POSIXct",
                 from = com.env$load_start_date,
                 to = com.env$load_end_date,
                 adjust = adjustment)
    }
  }
  #remove extra rows from loaded data
  for (ticker in com.env$stx_list) {
    de.xts <- paste0("data.env$",ticker)
    chk_string <- paste0("date2late <- (as.Date(com.env$load_end_date) < as.Date(index(",de.xts,")[nrow(",de.xts,")]))")
    eval(parse(text=chk_string))
    if (date2late) rmv_string <- paste0(de.xts," <- ",de.xts,"[-nrow(",de.xts,")]")
    while (date2late) {
      print(paste("Removing row from",de.xts))
      eval(parse(text=rmv_string))
      eval(parse(text=chk_string))
    }
  }
  print("successfully loaded stx")
  print(com.env$stx_list)
  print(paste("rows of data.env$CBE",nrow(data.env$CBE),"last index",index(data.env$CBE)[nrow(data.env$CBE)]))
  return(com.env$stx_list)
}

#function loads stx_list into com.env 
stock_list <- function() {
  print("In stock_list")
  
  large_dataset_symbols <- c(
    "IYZ",
    "FTR",
    "LVLT",
    "S",
    "T",
    "TLAB",
    "VZ",
    "XLB",
    "AA",
    "AKS",
    "APA",
    "APC",
    "APD",
    "ASH",
    "CBE",
    "CF",
    "CHK",
    "CLF",
    "CNX",
    "COG",
    "COP",
    "CVX",
    "D",
    #"DD",
    "DNR",
    "DO",
    #"DOW",
    "DTE",
    "ECL",
    "EMN",
    "FCX",
    "FMC",
    "FTI",
    "IP",
    "MLM",
    "MON",
    "MOS",
    "NE",
    "NEM",
    "NUE",
    "OI",
    "PPG",
    "PX",
    "SEE",
    "SHW",
    #"TIE",
    "VMC",
    "WFT",
    "X",
    "XLE",
    "DVN",
    "EOG",
    "EQT",
    "ESV",
    "FSLR",
    "HAL",
    "HES",
    "MRO",
    "MUR",
    "NBL",
    "NBR",
    "NFX",
    "NI",
    "NOV",
    "OKE",
    "OXY",
    "PXD",
    "RDC",
    "RIG",
    "RRC",
    "SLB",
    "SUN",
    "SWN",
    "VLO",
    "WMB",
    "XEC",
    "XL",
    "XOM",
    "XLF",
    "AIV",
    #"ABK",
    "AFL",
    "AIG",
    "AIZ",
    "ALL",
    "AMG",
    "AMP",
    "AMT",
    "AON",
    "APH",
    "ATVI",
    "AVB",
    "AXP",
    "BAC",
    "BBT",
    "BEN",
    "BK",
    "BLK",
    "BMY",
    "BXP",
    "C",
    "CB",
    "CBG",
    "CCI",
    "CERN",
    "CIEN",
    "CINF",
    "CMA",
    "CME",
    "COF",
    "CRM",
    "CSCO",
    "CTL",
    "CTSH",
    "CTXS",
    "DDR",
    "DFS",
    "DNB",
    "EA",
    "EBAY",
    "EFX",
    "EQR",
    "ESS",
    "ETFC",
    "FHN",
    "FII",
    "FIS",
    "FITB",
    "GGP",
    "GNW",
    "GS",
    "HBAN",
    "HCN",
    "HIG",
    "HST",
    "ICE",
    "IVZ",
    #"JNS",
    "JPM",
    "KEY",
    "KIM",
    "L",
    "LM",
    "LNC",
    "LUK",
    "MAC",
    "MBI",
    "MCO",
    "MET",
    "MMC",
    "MNST",
    "MS",
    "MTB",
    "MTG",
    "NDAQ",
    "NTRS",
    "NYX",
    "O",
    "PBCT",
    "PFG",
    "PGR",
    "PLD",
    "PNC",
    "PRU",
    "PSA",
    "RF",
    "SCHW",
    "SLG",
    "SLM",
    "SPG",
    #"STI",
    "STT",
    "TMK",
    "TROW",
    "TRV",
    #"TSO",
    "UNM",
    "USB",
    "VNO",
    "VTR",
    "WFC",
    "WY",
    "ZION",
    "XLI",
    "AAL",
    "AME",
    "ATI",
    "BA",
    #"BHI",
    "CAT",
    "CMI",
    "COL",
    "DE",
    "DHI",
    "DHR",
    "DOV",
    "EMR",
    "ETN",
    #"EXPD",
    "FAST",
    "FDX",
    "FISV",
    "FLR",
    "FLS",
    "GD",
    "GE",
    "GRMN",
    "GWW",
    "HON",
    "IR",
    "ITT",
    "ITW",
    "JBHT",
    "JEC",
    "KSU",
    "LLL",
    "LMT",
    "LUV",
    "MMM",
    "MTW",
    "NOC",
    "NSC",
    "PAYX",
    "PCAR",
    "PH",
    "PNR",
    "PWR",
    "R",
    "RHI",
    "ROK",
    "ROP",
    #"RRD",
    "RSG",
    "RTN",
    "SNA",
    "SRCL",
    "SWK",
    "TEL",
    "TEX",
    "TXT",
    "UAL",
    "UNP",
    "UPS",
    "URI",
    "UTX",
    "WM",
    "WU",
    "XLK",
    "A",
    "AAPL",
    "ACN",
    "ADBE",
    "ADI",
    "ADP",
    "ADS",
    "ADSK",
    "AKAM",
    "AMAT",
    "AMD",
    "AMZN",
    "CA",
    "EQIX",
    "FFIV",
    "FLIR",
    "GLW",
    "GOOG",
    "HP",
    "HPQ",
    "HRS",
    "IBM",
    "INTC",
    "INTU",
    "IRM",
    "JBL",
    "JNPR",
    "KLAC",
    "LDOS",
    "LRCX",
    "LSI",
    "MA",
    "MCHP",
    "MSFT",
    "MSI",
    "MU",
    "NFLX",
    "NTAP",
    "NVDA",
    "ORCL",
    "PBI",
    "QCOM",
    "RHT",
    "STX",
    "SWKS",
    "SYMC",
    "TDC",
    "TER",
    "TSS",
    "TXN",
    "UIS",
    "V",
    #"VIAV",
    "VRSK",
    "VRSN",
    "WDC",
    "WIN",
    "XLNX",
    "XRX",
    #"YHOO",
    "XLP",
    "ADM",
    "GHC",
    "GIS",
    "HNZ",
    "HRB",
    "HRL",
    "HSY",
    "IFF",
    "K",
    "KMB",
    "KO",
    "KR",
    "MCK",
    "MDLZ",
    #"MJN",
    "MKC",
    "MO",
    "PEP",
    "PG",
    "PM",
    #"RAI",
    "SJM",
    "STZ",
    "SVU",
    "SYY",
    "TAP",
    "TGT",
    "TSN",
    "WBA",
    #"WFM",
    "WMT",
    "XLU",
    "AEE",
    "AEP",
    "AES",
    "CMS",
    "CNP",
    "DUK",
    "ED",
    "EIX",
    "ES",
    "ETR",
    "EXC",
    "FE",
    "NEE",
    "NRG",
    "PCG",
    "PEG",
    "PNW",
    "PPL",
    "SCG",
    "SO",
    "SRE",
    "WEC",
    "XEL",
    "XLV",
    "ABC",
    "ABT",
    "AET",
    "AGN",
    "ALXN",
    "AMGN",
    "ANTM",
    "BAX",
    "BCR",
    "BDX",
    "BIIB",
    "BSX",
    "CELG",
    "CI",
    "DGX",
    "DVA",
    "ENDP",
    "ESRX",
    "EW",
    "GILD",
    "HSIC",
    "HUM",
    "ILMN",
    "ISRG",
    "JNJ",
    "LH",
    "LLY",
    "MDT",
    "MRK",
    "MYL",
    "PDCO",
    "PFE",
    "PKI",
    "PRGO",
    "REGN",
    "SYK",
    "THC",
    "TMO",
    "UHS",
    "UNH",
    "VAR",
    "VRTX",
    "WAT",
    "XRAY",
    "ZBH",
    "XLY",
    "AAP",
    "AN",
    "ANF",
    "AVP",
    "AVY",
    "AZO",
    "BBBY",
    "BBY",
    "BC",
    "BIG",
    "BLL",
    "BMS",
    "BWA",
    "CAG",
    "CAH",
    "CBS",
    "CCE",
    "CCL",
    "CHD",
    "CHRW",
    "CL",
    "CLX",
    "CMCSA",
    "CMG",
    "COH",
    "COST",
    "CPB",
    "CSX",
    "CTAS",
    "CVG",
    "CVS",
    "DAL",
    "DDS",
    "DF",
    "DG",
    "DIS",
    "DISCA",
    "DLTR",
    "DPS",
    "DRI",
    "EL",
    "EXPE",
    "F",
    "FOSL",
    "FOXA",
    "GCI",
    "GME",
    "GPC",
    "GPS",
    "GT",
    "HAS",
    "HBI",
    "HD",
    "HOG",
    "IGT",
    "IHRT",
    "IPG",
    "JCI",
    "JCP",
    "JWN",
    #"KATE",
    "KBH",
    "KMX",
    "KSS",
    "LB",
    "LEG",
    "LEN",
    "LOW",
    "M",
    "MAR",
    "MAS",
    "MAT",
    "MCD",
    "MDP",
    "MHK",
    "MOLX",
    "NKE",
    "NWL",
    "NYT",
    "ODP",
    "OMC",
    "OMX",
    "ORLY",
    "PCLN",
    "PHM",
    "PVH",
    "RCL",
    "RL",
    "ROST",
    "SBUX",
    "SHLD",
    "SIG",
    "SNI",
    #"SPLS",
    "SSP",
    "TIF",
    "TJX",
    "TSCO",
    "TWX",
    #"URBN",
    "VFC",
    "VIAB",
    "WEN",
    "WHR",
    "WYN",
    "WYNN",
    "YUM"
  )
  
  small_dataset_symbols <- c(
    "XLF", # Financial sector ETF
    #"BRK-B",    not valid name
    "JPM",
    "WFC",
    "BAC",
    "C",
    "USB",
    "GS",
    "AIG",
    "CB",
    "AXP",
    "MET",
    "MS",
    "BLK",
    "PNC",
    "BK",
    "SCHW",
    "CME",
    "COF",
    "MMC",
    "PRU",
    "TRV",
    #"SPGI",
    "ICE",
    "BBT",
    "AON",
    "AFL",
    "STT",
    "ALL",
    "DFS",
    #"STI",
    "PGR",
    "MTB",
    "HIG",
    "TROW",
    "AMP",
    "FITB",
    "NTRS",
    "PFG",
    "KEY",
    "IVZ",
    "BEN",
    "RF",
    "CINF",
    "L",
    "HBAN",
    "LNC",
    "XL",
    "AJG",
    "UNM",
    "CMA",
    "NDAQ",
    "AMG",
    "ETFC",
    "TMK",
    "ZION",
    "LUK",
    "AIZ",
    "LM",
    "GDX", #gold miners ETF
    "ABX",
    "NEM",
    "GG",
    "FNV",
    "AEM",
    #"SLW",  #problem loading data from YHOO
    "GOLD",
    "AU",
    "RGLD",
    "KGC",
    "BVN",
    "TAHO",
    "AUY",
    "GFI",
    "EGO",
    "PAAS",
    "BTG",
    "HL",
    "AGI",
    "NGD",
    "IAG",
    "CDE",
    #"SBGL",   #too recent
    "AG",
    #"SSRI",
    #"OR",    #problem loading data from YHOO
    "HMY",
    "MUX",
    #"KLDX",
    "AKG",
    "SAND",
    "XLE", #Energy ETF
    'CVX',
    'SLB',
    'COP',
    'EOG',
    'OXY',
    'HAL',
    'KMI',
    'PSX',
    'VLO',
    'MPC',
    'PXD',
    'APC',
    'WMB',
    #'BHI',
    'CXO',
    'APA',
    'DVN',
    #'TSO',
    'FTI',
    'NBL',
    'NOV',
    'HES',
    'COG',
    'MRO',
    'OKE',
    'EQT',
    'XEC',
    'NFX',
    'HP',
    'RRC',
    'MUR',
    'CHK',
    'RIG'
  )
  cybc_dataset_symbols <- c("CYBC","BTC","ETH","BCH","LTC","RPL")
  
  small_dataset_etfs <- c('XLF','GDX','XLE')
  large_dataset_etfs <- c('IYZ','XLB','XLE','XLF','XLI','XLK','XLP','XLU','XLV','XLY')
  cybc_dataset_etfs <- c('CYBC')
  
  etf_dataset_str <- paste0(com.env$data_str,"_dataset_etfs")
  dataset_str <- paste0(com.env$data_str,"_dataset_symbols")
  
  #create etf lookup (stock contains etf used as etf, etf contains the word 'etf')
  #com.env$etf.symbols <- c('XLF',
  #                         'GDX',
  #                         'XLE'
  #)
  cmd_str <- paste0("com.env$etf.symbols <- ",etf_dataset_str)
  #print(cmd_str)
  eval(parse(text=cmd_str))
  cmd_str <- paste0("symbols <- ",dataset_str)
  #print(cmd_str)
  eval(parse(text=cmd_str))
  
  num_symbols <- length(symbols)
  com.env$etfs <- length(com.env$etf.symbols)
  com.env$stx <- num_symbols - com.env$etfs
  etf_num <- which(symbols %in% com.env$etf.symbols)
  if (length(etf_num) != com.env$etfs) stop()        #bug in ticker or etf list
  
  com.env$etf_lookup <- rep('etf',num_symbols)
  for (i in 1:com.env$etfs) {
    start_idx <- etf_num[i] + 1
    end_idx <- ifelse(i < com.env$etfs,etf_num[i+1]-1,num_symbols)
    com.env$etf_lookup[start_idx:end_idx] <- com.env$etf.symbols[i]
  }
  names(com.env$etf_lookup) <- symbols
  
  com.env$stx.symbols <- symbols[!(symbols %in% com.env$etf.symbols)]  #tradable stocks
  com.env$stx_list <- symbols                                          #all symbols (stocks + ETFs)
  
  #print(ls(com.env))
}
