#calc_lib

name.var <- function(ve.xts,col_num,new_name,bins,first_pass=FALSE) { #always name col_num, may have two names for bin vars
  if (first_pass) print(paste("name.var",ve.xts,col_num,new_name,bins))
  if (is.null(new_name)) {
    print(paste("2name.var",ve.xts,col_num,new_name,bins))
    print("problem in name_var no name given")
  }
  cmd_string <- paste("ve.ncol <- ncol(",ve.xts,")")
  eval(parse(text=cmd_string))
  # if (ve.xts == "var.env$BAC") {
  #   if (any(colnames(var.env$BAC) == paste0("'",nn,"'"))) {
  #     print("problem in name.var, duplicate colnames")
  #     source("close_session.R")
  #   }
  # }
  if ((ve.ncol == col_num) | (bins == 1)) {
    cmd_string <- paste0("colnames(",ve.xts,")[",col_num,"] <- '",new_name,"'")
    #print(cmd_string)
    eval(parse(text=cmd_string))
  } else {
    for (n in 1:bins) {
      nn <- paste0(new_name,"_",n)
      cn <- col_num + n - 1
      cmd_string <- paste("colnames(",ve.xts,")[",cn,"] <- '",nn,"'",sep="")
      # if (first_pass) print(cmd_string)
      eval(parse(text=cmd_string))
    }
  }
  #if (first_pass) print(paste("finished with name.var",cmd_string))
} 

#takes in ticker and vd and returns column containing vd values (stored temporarily in var.env$col.xts)
calc_col_vd <- function(ve.xts,vd,first_pass=FALSE) {
  if (first_pass) print(paste(ve.xts,vd$var_name))
  ticker <- sub("var.env$","",ve.xts,fixed=TRUE)
  if (first_pass) print(paste("In calc_col_vd",ticker))
  is.etf <- (com.env$etf_lookup[[ticker]] == 'etf')
  #print(vd)
  if (is.etf & !vd$calc_etf) {
    print(paste("ERROR in calc_col_vd, etf ticker,",ticker,", called with vd.calc_etf =",vd$calc_etf))
    source("close_session.R")
  }
  coln <- 0
  for (math_calc in vd$math) {
    math <- strsplit(math_calc,split=",")[[1]]
    parms <- gsub("^[^,]*,","",math_calc)
    fun_call <- paste0(math[1],"('",ve.xts,"',",coln,",",parms,",first_pass=first_pass)")
    if (first_pass) print(paste(fun_call,vd$var_name,first_pass))
    eval(parse(text=fun_call))
  }
  return(var.env$col.xts)
}

#doesn't handle coln==0
calc_prediction <- function(ve.xts,coln,model,first_pass=FALSE) {
  #print(paste("ve.xts",ve.xts,"coln",coln,"model",model))
  cmd_string <- paste0("tmp.df <- data.frame(",ve.xts,")")
  eval(parse(text=cmd_string))
  tmp.df[is.na(tmp.df)] <- 0
  cmd_string <- paste0(ve.xts," <- cbind(",ve.xts,",as.xts(predict.lm(",model,",newdata=tmp.df)))")
  eval(parse(text=cmd_string))
}

#doesn't handle coln==0
calc_adjret <- function(ve.xts,coln,field,first_pass=FALSE) {
  #print(paste("ve.xts",ve.xts,"coln",coln,"field",field))
  ticker <- sub("var.env$","",ve.xts,fixed=TRUE)
  de.xts <- paste("data.env$",ticker,"$",ticker,field,sep="")
  cmd_string <- paste("tmp.xts <- stats::lag(",de.xts,",-1)/",de.xts,sep="")
  #print(cmd_string)
  eval(parse(text=cmd_string))
  tmp.xts[is.na(tmp.xts)] <- 0                  #replace NA's with 0
  cmd_string <- paste(ve.xts," <- cbind(",ve.xts,",tmp.xts)",sep="")
  #print(cmd_string)
  eval(parse(text=cmd_string))
}

#takes precomputed data and places a column for ticker in ve.xts into var.env$col.xts
#  will append to var.env df if coln provided
from.data.env <- function(ve.xts,coln=0,field,first_pass=FALSE) {
  ticker <- sub("var.env$","",ve.xts,fixed=TRUE)
  de.field <- paste0(ticker,".",field)
  de.xts <- paste0("data.env$",ticker,"$",de.field)
  if (first_pass) print(paste(ve.xts,coln,field,ticker))
  cmd_string <- paste0("in.data.env <- ('",de.field,"' %in% colnames(",de.xts,"))")
  if (first_pass) print(cmd_string)
  eval(parse(text=cmd_string))
  if (in.data.env) {
    if (coln == 0) {
      cmd_string <- paste0("var.env$col.xts <- ",de.xts)
    } else {
      cmd_string <- paste0(ve.xts," <- cbind(",ve.xts,",",de.xts,")")
    } 
  } else { #vd lookup required
    vd <- com.env$v.com[[field]]
    print(paste("vd lu in from.data.env:",vd$var_name))
    if (is.null(vd)) {
      print(paste("Error in from.data.env, missing field:",field))
      print(paste(ve.xts,coln,field,ticker))
      source("close_session.R")
    }
    if (coln == 0) {
      cmd_string <- "calc_col_vd(ve.xts,vd)"
    } else {
      cmd_string <- paste0(ve.xts," <- cbind(",ve.xts,",calc_col_vd(ve.xts,vd))")
    } 
  }
  if (first_pass) print(cmd_string)
  eval(parse(text=cmd_string))
}

#doesn't handle coln==0
from.var.env <- function(ve.xts,coln,field,first_pass=FALSE) {
  if (first_pass) print(paste("from.var.env:","ve.xts=",ve.xts,"field=",field))
  clu <- com.env$v.com[[which(names(com.env$v.com)==field)]]$clu
  ve.field <- paste(ve.xts,"[,'",clu,"']",sep="")
  cmd_string <- paste(ve.xts," <- cbind(",ve.xts,",",ve.field,")",sep="")
  if (first_pass) print(cmd_string)
  eval(parse(text=cmd_string))
}

#doesn't handle coln==0
calc_constant <- function(ve.xts,coln,value,first_pass=FALSE) {
  #if (first_pass) print(paste("calc_constant:","ve.xts=",ve.xts,"value=",value))
  cmd_string <- paste(ve.xts," <- cbind(",ve.xts,",",value,")",sep="")
  #if (first_pass) print(cmd_string)
  eval(parse(text=cmd_string))
}

#only calc to handle empty var.env (for use with predictor vd=com.env$v.com[[1]])
#negative look forwards predicts future returns, positive lf calculates past returns (always close to close in days)
#appends to var.env df at coln; if coln==0 places data in var.env$tmp.xts
calc_look_forward <- function(ve.xts,coln,lf=-1,first_pass=FALSE) {
  #if (first_pass) print(paste("calc_look_forward",ve.xts,coln,lf))
  ticker <- sub("var.env$","",ve.xts,fixed=TRUE)
  #if (first_pass) print(exists(ticker,where=var.env))
  c.txt <- paste0("data.env$",ticker,"[,'",ticker,".Adjusted']")
  lagc.txt <- paste0("stats::lag(",c.txt,",lf)")
  #print(cmd_string)
  #eval(parse(text=cmd_string))
  #tmp <- stats::lag(tmp,lf)
  if (coln == 0) {
    cmd_string <- ifelse(lf<0,
                         paste0("var.env$tmp.xts <- log(",lagc.txt,"/",c.txt,")"),
                         paste0("var.env$tmp.xts <- log(",c.txt,"/",lagc.txt,")"))
  } else if (exists(ticker,where=var.env)) {
    cmd_string <- ifelse(lf<0,
                         paste0(ve.xts," <- cbind(",ve.xts,",log(",lagc.txt,"/",c.txt,"))"),
                         paste0(ve.xts," <- cbind(",ve.xts,",log(",c.txt,"/",lagc.txt,"))"))
  } else {
    cmd_string <- ifelse(lf<0,
                         paste0(ve.xts," <- log(",lagc.txt,"/",c.txt,")"),
                         paste0(ve.xts," <- log(",c.txt,"/",lagc.txt,")"))
  }
  #if (first_pass) print(cmd_string)
  eval(parse(text=cmd_string))
}

#doesn't work
ma <- function(x,n=30){stats::filter(x,rep(1/n,n), sides=1)} #n is window of ma

#doesn't handle coln==0, doesn't work
calc_ma <- function(ve.xts,coln,n=30,first_pass=FALSE) { 
  cmd_string <- paste(ve.xts,"[,",coln,"] <- ma(",ve.xts,"[,",coln,"],",n,")",sep="")
  eval(parse(text=cmd_string))
}

#positive lag brings past data forward in time, negative lag moves future data back
#appended to var.env df at coln; if coln==0 places data in var.env$col.xts
calc_lag <- function(ve.xts,coln,lag=1,first_pass=FALSE) {  #always lag coln
  #if (verbose) print(paste("calc_lag:","ve.xts=",ve.xts,"lag=",lag))
  if (coln==0) {
    cmd_string <- "var.env$col.xts <- stats::lag(var.env$col.xts,lag)"
  } else {
    cmd_string <- paste0(ve.xts,"[,",coln,"] <- stats::lag(",ve.xts,"[,",coln,"],",lag,")")
  }
  if (first_pass) print(cmd_string)
  eval(parse(text=cmd_string))
}

#CALC_MA does not work
#calc decay will call calc_lag for decay={1,2}, and calc_ma for decay>2 (decay gives moving average window)
#otherwise calc decay will compute an exponential decay
#only calc_lag handles coln==0
calc_decay <- function(ve.xts,coln,decay,var_cnt=1,first_pass=FALSE) { #compute decay on coln
  #print(paste("ve.xts=",ve.xts,"decay=",decay))
  if (decay >= 3) {
    print("ERROR calc_ma does not work")
    calc_ma(ve.xts,coln,n=decay,first_pass)
  } else if (decay >= 1) {
    calc_lag(ve.xts,coln,lag=decay,first_pass)
  } else { #decay between 0. and 1.
    if (coln==0) {
      print("ERROR calc_decay can't handle coln==0 for decays")
      source("close_session.R")
    }
    data_string <- paste(ve.xts,"[,",coln,"]",sep="")
    cmd_string <- paste("tmp.xts <-",data_string,sep="")
    #if (first_pass) print(cmd_string)
    eval(parse(text=cmd_string))
    tmp.xts[is.na(tmp.xts)] <- 0   #replace missing with 0
    tmp.ses <- ses(tmp.xts,alpha=decay)
    tmp.decay <- xts(fitted.values(tmp.ses),order.by = index(tmp.xts))
    cmd_string <- paste(data_string,"<- tmp.decay")
    #if (verbose) print(cmd_string)
    eval(parse(text=cmd_string))
    if (is.null(var_cnt)) {  #shouldn't be needed, debugging purposes only
      print("WARNING************* var_cnt is null in calc_decay")
      print(paste(ve.xts,coln,decay))
      var_cnt <- 1
    }
    if(!(is.logical(var_cnt > 1))) var_cnt <-1
    if (var_cnt > 1) {
      for (i in 2:var_cnt) {
        data_string <- paste(ve.xts,"[,",coln+i-1,"]",sep="")
        cmd_string <- paste("tmp.xts <-",data_string,sep="")
        eval(parse(text=cmd_string))
        tmp.xts[is.na(tmp.xts)] <- 0   #replace missing with 0
        tmp.ses <- ses(tmp.xts,alpha=decay)
        tmp.decay <- xts(fitted.values(tmp.ses),order.by = index(tmp.xts))
        cmd_string <- paste(data_string,"<- tmp.decay")
        #if (verbose) print(cmd_string)
        eval(parse(text=cmd_string))
      }
    }
  }
}

#uses com.env$v.com to lookup prices from var defs
#appended to var.env df at coln; if coln==0 places data in var.env$col.xts
calc_ret <- function(ve.xts,coln,start_price,end_price,first_pass=FALSE) {
  #if (verbose) print(paste("calc_ret:","ve.xts=",ve.xts,"start_price=",start_price,"end_price=",end_price))
  start_vd <- com.env$v.com[[start_price]]
  end_vd <- com.env$v.com[[end_price]]
  #if (first_pass) print(paste(start_price,end_price,start_vd$var_name,end_vd$var_name))
  start.xts <- calc_col_vd(ve.xts,start_vd,first_pass)
  end.xts <- calc_col_vd(ve.xts,end_vd,first_pass)
  if (coln==0) {
    var.env$col.xts <- log(end.xts/start.xts)
  } else {
    cmd_string <- paste0(ve.xts," <- cbind(",ve.xts,",log(end.xts/start.xts))")
    #if (verbose) print(cmd_string)
    eval(parse(text=cmd_string))
  }
}

#handles coln==0 [function applied to var.env$col.xts]
calc_cap <- function(ve.xts,coln,abscap=NULL,lcap=NULL,hcap=NULL,
                     cap_pct=NULL,lcp=NULL,hcp=NULL,zcap=NULL,lz=NULL,hz=NULL,first_pass=FALSE) {  #always cap coln
  if (first_pass) print(paste("calc_cap:","ve.xts=",ve.xts,"coln=",coln,"abscap=",abscap,"cap_pct=",cap_pct,"zcap=",zcap))
  if (coln == 0) {
    data_string <- "var.env$col.xts[com.env$reg_date_range]"
    out_string <- "var.env$col.xts"
  } else {
    data_string <- paste(ve.xts,"[com.env$reg_date_range,",coln,"]",sep="")
    out_string <- paste(ve.xts,"[,",coln,"]",sep="")
  }
  if (!is.null(abscap)) {
    lcap <- -abscap
    hcap <- abscap
  }
  if (!is.null(cap_pct)) {
    lcp <- cap_pct
    hcp <- 1-cap_pct
  }
  if (!is.null(zcap)) {
    lz <- -zcap
    hz <- zcap
  }
  if (!is.null(lcp)) {
    cmd_string <- paste("lcap <- quantile(",data_string,",lcp,na.rm=TRUE)",sep="")
    eval(parse(text=cmd_string))
    cmd_string <- paste("hcap <- quantile(",data_string,",hcp,na.rm=TRUE)",sep="")
    eval(parse(text=cmd_string))
  }
  if (!is.null(lz)) {
    cmd_string <- paste("sd_val <- sd(",data_string,",na.rm=TRUE)",sep="")
    eval(parse(text=cmd_string))
    cmd_string <- paste("mean_val <- mean(",data_string,",na.rm=TRUE)",sep="")
    eval(parse(text=cmd_string))
    lcap <- mean_val + lz*sd_val
    hcap <- mean_val + hz*sd_val
  }
  cmd_string <- paste(out_string,"[",out_string," < ",lcap,"] <- ",lcap,sep="")
  eval(parse(text=cmd_string))
  cmd_string <- paste(out_string,"[",out_string," > ",hcap,"] <- ",hcap,sep="")
  eval(parse(text=cmd_string))
}

#doesn't handle coln==0 [function applied to var.env$col.xts]
calc_cap_x <- function(coln,etf_coln,abscap=NULL,lcap=NULL,hcap=NULL,
                     cap_pct=NULL,lcp=NULL,hcp=NULL,zcap=NULL,lz=NULL,hz=NULL,first_pass=FALSE) {  #always cap coln
  if (first_pass) print(paste("calc_cap_x:","coln=",coln,"abscap=",abscap,"cap_pct=",cap_pct,"zcap=",zcap))
  # if (coln == 0) {
  #   data_string <- "var.env$col.xts[com.env$reg_date_range]"
  #   out_string <- "var.env$col.xts"
  # } else {
  #   data_string <- paste(ve.xts,"[com.env$reg_date_range,",coln,"]",sep="")
  #   out_string <- paste(ve.xts,"[,",coln,"]",sep="")
  # }
  if (!is.null(abscap) | !is.null(hcap)) {  #compute abs caps and return
    if (!is.null(abscap)) {
      lcap <- -abscap
      hcap <- abscap
    } else if (is.null(lcap)) {
      lcap <- -hcap
    }
    for (ticker in com.env$stx_list) {
      is.etf <- (com.env$etf_lookup[[ticker]] == 'etf')
      if (is.etf & (etf_coln == -1)) next          #nothing to compute in etf
      col_num <- ifelse(is.etf,coln,etf_coln)
      ve.xts <- paste0("var.env$",ticker,"[,",col_num,"]")
      cmd_string <- paste0(ve.xts,"var.env$",ticker,"<",lcap,"] <- ",lcap)
      eval(parse(text=cmd_string))
      cmd_string <- paste0(ve.xts,"var.env$",ticker,">",hcap,"] <- ",hcap)
      eval(parse(text=cmd_string))
    }
    return()
  }
  x.xts <- make_x_xts(coln,etf_coln)
  #print(colnames(x.xts))
  if (!is.null(cap_pct)) {
    lcp <- cap_pct
    hcp <- 1-cap_pct
  }
  if (!is.null(zcap)) {
    lz <- -zcap
    hz <- zcap
  }
  if (!is.null(lcp)) {
    caps <- apply(x.xts,1,quantile,prob=c(lcp,hcp),na.rm=TRUE)
    lcap <- caps[1,]
    hcap <- caps[2,]
  }
  if (!is.null(lz)) {
    sd_val <- apply(x.xts,1,sd,na.rm=TRUE)
    mean_val <- apply(x.xts,1,mean,na.rm=TRUE)
    lcap <- mean_val + lz*sd_val
    hcap <- mean_val + hz*sd_val
  }
  x.matrix <- pmax(pmin(as.matrix(x.xts),as.vector(hcap)),as.vector(lcap))
  #print(str(x.matrix))
  sdate <- rownames(x.matrix)[1]
  edate <- rownames(x.matrix)[nrow(x.matrix)]
  cmd_string <- paste0("x.index <- index(var.env$",com.env$stx_list[1],"['",sdate,"/",edate,"'])")
  if (first_pass) print(cmd_string)
  eval(parse(text=cmd_string))
  if (length(x.index) != nrow(x.matrix)) {
    print(paste(length(x.index),nrow(x.matrix)))
    print(paste(x.index[1],x.index[length(x.index)],rownames(x.matrix)[1],rownames(x.matrix)[nrow(x.matrix)]))
    for (i in 1:min(length(x.index),nrow(x.matrix))) {
      if (x.index[i] != rownames(x.matrix)[i]) print(paste(x.index[i],rownames(x.matrix)[i]))
    }
    #var.env$x.matrix <- x.matrix
    #var.env$x.index <- x.index
  }
  x.xts <- xts(x.matrix,order.by=x.index)
  var.env$x.xts <- x.xts
  split_x_xts(x.xts,coln,etf_coln)
}

#doesn't handle coln==0
calc_z_x <- function(coln,etf_coln,ma=TRUE,scale_val=TRUE,first_pass=FALSE) { #compute zscore/zscale on coln
  #ve.xts <- paste("var.env$",ticker,sep="")
  #cmd_string <- paste("col <- ncol(",ve.xts,")",sep="")
  #eval(parse(text=cmd_string))
  x.xts <- make_x_xts(coln,etf_coln)
  x.matrix <- t(as.matrix(x.xts))
  x.out <- scale(x.matrix,center=ma,scale=scale_val)
  x.matrix <- t(x.out)
  sdate <- rownames(x.matrix)[1]
  edate <- rownames(x.matrix)[nrow(x.matrix)]
  cmd_string <- paste0("x.index <- index(var.env$",com.env$stx_list[1],"['",sdate,"/",edate,"'])")
  eval(parse(text=cmd_string))
  x.xts <- xts(x.matrix,order.by=x.index)
  split_x_xts(x.xts,coln,etf_coln)
}  

#doesn't handle coln==0
#mean adjust cross-sectionally (using calc_z_x function with scale_val set to FALSE)
ma_x <- function(coln,etf_coln,first_pass=FALSE) {
  calc_z_x(coln,etf_coln,ma=TRUE,scale_val=FALSE,first_pass)
}

#doesn't handle coln==0
#splits data into quantiles(# from qcount) and approximates a rank from 0-1.0 from these quantiles (in reg_date_range)
calc_rank_x <- function(coln,etf_coln,qcount,first_pass=FALSE) {
  #print(paste("calc_rank",ve.xts,coln,first_pass))
  x.xts <- make_x_xts(coln,etf_coln)
  for (i in 1:nrow(x.xts)) {
    data.row <- x.xts[i,]
    new_data.row <- row_rank_x(data.row,qcount=qcount)
    x.xts[i,] <- new_data.row
  }
  #apply(x.xts,2,row_rank_x,qcount=qcount)
  split_x_xts(x.xts,coln,etf_coln)
}


row_rank_x <- function(row.vector,qcount) {
  #print(str(row.vector))
  suppressWarnings(a <- max(row.vector,na.rm=TRUE))
  suppressWarnings(b <- min(row.vector,na.rm=TRUE))
  if ((a==b) | (a==-Inf)) { #a==-Inf when all row.vector == NA
    row.vector <- 1
  } else {
    deciles <- quantile(row.vector,probs=seq(0,1,(1/qcount)),na.rm=TRUE)
    var.env$row.vector <- row.vector
    var.env$deciles <- deciles
    row.vector <- approx(deciles,(as.numeric(sub('%','',names(deciles)))/100),row.vector,yleft=0,yright=1)$y
  }
}

make_x_xts <- function(coln,etf_coln) { #create a cross-sectional matrix by day for columns provided over all stx and etfs
  #print(paste("make_x_xts",coln,etf_coln))
  x.xts <- NULL
  for (ticker in com.env$stx_list) {
    is.etf <- (com.env$etf_lookup[[ticker]] == 'etf')
    if (is.etf & (etf_coln == -1)) next          #nothing to compute in etf
    col_num <- ifelse(is.etf,etf_coln,coln)
    ve.xts <- paste0("var.env$",ticker,"[,",col_num,"]")
    cmd_string <- paste0("x.xts <- cbind(x.xts,",ve.xts,")")
    #print(cmd_string)
    eval(parse(text=cmd_string))
    colnames(x.xts)[ncol(x.xts)] <- ticker
  }
  return(x.xts)
}

split_x_xts <- function(x.xts,coln,etf_coln) {
  #print(paste("split_x_xts",coln,etf_coln))
  for (ticker in colnames(x.xts)) {
    #print(ticker)
    is.etf <- (com.env$etf_lookup[[ticker]] == 'etf')
    if (is.etf & (etf_coln == -1)) next          #nothing to compute in etf
    col_num <- ifelse(is.etf,etf_coln,coln)
    #ve.xts <- paste0("var.env$",ticker,"[,",col_num,"]")
    cmd_string <- paste0("var.env$",ticker,"[,",col_num,"] <- x.xts[index(var.env$",ticker,"),'",ticker,"']")
    #print(cmd_string)
    eval(parse(text=cmd_string))
  }
}

#doesn't handle coln==0  
#handles def vars
#replace place holders XX0,XX1,..XXn with vars 
# XX0 represents last column, XX0N represents new column (can only be on left side)
# XX1..XXn existing vars
# math_str in the form 'XX0 <- f(XX0,XX1..XXn)' 
calc_math <- function(ve.xts,coln,XX_list=NULL,math_str,first_pass=FALSE) { #apply math to last column for XX0, create new last column XX0N
  #if (verbose) print(paste("ve.xts=",ve.xts,"XX_list=",XX_list,"math_str=",math_str))
  #ve.xts <- paste("var.env$",ticker,sep="")
  if (grepl("XX0N",math_str)) {
    place_holder_str <- "XX0N"
    data_string <- 'tmp.xts'
    new_var <- TRUE
  } else {
    place_holder_str <- "XX0"
    #cmd_string <- paste("col <- ncol(",ve.xts,")",sep="")
    #eval(parse(text=cmd_string))
    data_string <- paste(ve.xts,"[,",coln,"]",sep="")
    new_var <- FALSE
  }
  math_str <- gsub(place_holder_str,data_string,math_str)
  if (!is.null(XX_list)) {
    #print(XX_list)
    def_list.xts <- NULL
    for (n in 1:length(XX_list)) {
      vd <- com.env$v.com[[which(names(com.env$v.com) == XX_list[n])]]
      #print(vd$math)
      if (vd$use == "def") { #need to precalculate def var into var.env$col.xts
        def_list.xts <- cbind(def_list.xts,calc_col_vd(ve.xts,vd,first_pass))
        #print(names(var.env$col.xts))
        #print(colnames(def_list.xts))
        #print(length(var.env$col.xts))
        replacement_string <- paste0("def_list.xts[,",ncol(def_list.xts),"]")
      } else { #var calculated with column name clu
        replacement_string <- paste0(ve.xts,"[,'",vd$clu,"']")
      }
      place_holder_str <- paste0("XX",n)
      math_str <- gsub(place_holder_str,replacement_string,math_str)
    }
  }
  if (first_pass) print(math_str)
  eval(parse(text=math_str))
  #check data string and replace NA,INF,NaN with 0
  cmd_str <- paste0(data_string,"[!is.finite(",data_string,")] <- 0")
  if (first_pass) print(cmd_str)
  eval(parse(text=cmd_str))
  if (new_var) {
    tmp.xts[!is.finite(tmp.xts)] <- 0
    cmd_str <- paste(ve.xts," <- cbind(",ve.xts,",tmp.xts)",sep="")
    if (first_pass) print(cmd_str)
    eval(parse(text=cmd_str))
  } 
}

#doesn't handle coln==0
calc_z <- function(ve.xts,coln,ma=TRUE,first_pass=FALSE) { #compute zscore/zscale on coln
  #ve.xts <- paste("var.env$",ticker,sep="")
  #cmd_string <- paste("col <- ncol(",ve.xts,")",sep="")
  #eval(parse(text=cmd_string))
  data_string <- paste(ve.xts,"[com.env$reg_date_range,",coln,"]",sep="")
  out_string <- paste(ve.xts,"[,",coln,"]",sep="")
  cmd_string <- paste("sd_val <- sd(",data_string,",na.rm=TRUE)",sep="")
  eval(parse(text=cmd_string))
  if (is.nan(sd_val) | is.na(sd_val)) {
    print(paste("ERROR in calc_z, sd_val is NaN",sd_val,ve.xts,coln,ma,first_pass))
    cmd_string <- paste(out_string," <- 0",sep="")
    eval(parse(text=cmd_string))
  } else if (sd_val > 0) {
    #print(cmd_string)
    mean_val <- 0
    if (ma) {
      cmd_string <- paste("mean_val <- mean(",data_string,",na.rm=TRUE)",sep="")
      #print(cmd_string)
      eval(parse(text=cmd_string))
    }
    #print(paste("mean_val=",mean_val,"sd_val=",sd_val))
    cmd_string <- paste(out_string," <- (",out_string," - mean_val)/sd_val",sep="")
    #print (cmd_string)
    eval(parse(text=cmd_string))
    #df.zscore <- scale(df,center=ma)
  } else {
    #print(paste("WARNING:SD=0 in calc_z,coln=",coln))
    cmd_string <- paste(out_string," <- 0",sep="")
    eval(parse(text=cmd_string))
  }
}

#doesn't handle coln==0
calc_adj <- function(ve.xts,coln,field,first_pass=FALSE) { #take from data.env and append adjusted value to var.env
  #if (verbose) print(paste("ve.xts=",ve.xts,"field=",field))
  ticker <- sub("var.env$","",ve.xts,fixed=TRUE)
  de.xts <- paste("data.env$",ticker,"[,'",ticker,".",field,"']",sep="")
  de.adjc <- paste("data.env$",ticker,"[,'",ticker,".Adjusted']",sep="")
  de.c <- paste("data.env$",ticker,"[,'",ticker,".Close']",sep="")
  cmd_string <- paste(ve.xts," <- cbind(",ve.xts,",",de.xts,"*",de.adjc,"/",de.c,")",sep="")
  #if (verbose) print(cmd_string)
  eval(parse(text=cmd_string))
}

#vlty = sd*sd
#if no field provided assumed to be computed on ret with no lag or decay (calc vlty in place, lag by day)  
#if field provided, vlty is not lagged by a day and appended to var.env df or (for coln==0) put into var.env$col.tmp
#default window of 60 days
#future enhancements: price vlty (divide by close price), stdev (don't square)
calc_vlty <- function(ve.xts,coln,field=NULL,window=60,first_pass=TRUE) { #take vlty of field (in var.env) and append as last column
  #first_pass <- TRUE
  if (first_pass) print(paste(ve.xts,coln,field,window))
  if (is.null(field) & (coln==0)) {
    print("ERROR in calc_vlty, coln==0 only valid if field is provided")
    source("close_session.R")
  }
  if (is.null(field)) { #if no field passed in calc in place
    f.xts <- paste0(ve.xts,"[,",coln,"]")
  } else {
    clu <- com.env$v.com[[which(names(com.env$v.com)==field)]]$clu
    #ve.field <- paste(ve.xts,"$",clu,sep="")
    f.xts <- paste0(ve.xts,"[,'",clu,"']")
  }
  cmd_string <- paste0("tmp.xts <- xts(apply(",f.xts,",2,runSD,n=window), index(",ve.xts,"))")
  if (first_pass) print(cmd_string)
  eval(parse(text=cmd_string))
  if (is.null(field)) tmp.xts <- stats::lag(tmp.xts,1)   #not needed if using valid raw/scale/model vars [field provided]
  tmp.xts <- tmp.xts*tmp.xts
  if (is.null(field)) { #if no field passed in calc in place
    cmd_string <- paste0(f.xts," <- tmp.xts")
    if (first_pass) print(cmd_string)
    eval(parse(text=cmd_string))
  } else if (coln == 0) {
    var.env$col.xts <- tmp.xts
    if (first_pass) print("var.env$col.xts <- tmp.xts")
  } else {
    cmd_string <- paste0(ve.xts," <- cbind(",ve.xts,",tmp.xts)")
    if (first_pass) print(cmd_string)
    eval(parse(text=cmd_string))
  }
}

#doesn't handle coln==0
calc_adv <- function(ve.xts,coln,window=20,logv=TRUE,subtract=18.5,first_pass=FALSE)  {
  #if (verbose) print(paste("ve.xts=",ve.xts,"window=",window,"logv=",logv,"subtract=",subtract))
  ticker <- sub("var.env$","",ve.xts,fixed=TRUE)
  h.de <- paste("data.env$",ticker,"[,'",ticker,".High']",sep="")
  l.de <- paste("data.env$",ticker,"[,'",ticker,".Low']",sep="")
  v.de <- paste("data.env$",ticker,"[,'",ticker,".Volume']",sep="")
  cmd_string <- paste("tmp.xts <- sqrt(",h.de,"*",l.de,")*",v.de,sep="")
  #if (verbose) print(cmd_string)
  eval(parse(text=cmd_string))
  tmp.xts <- runMean(tmp.xts,n=window,cumulative=FALSE)
  if (logv) tmp.xts <- log(tmp.xts)
  if (subtract != 0) tmp.xts <- tmp.xts - subtract
  cmd_string <- paste(ve.xts," <- cbind(",ve.xts,",tmp.xts)",sep="")
  #if (verbose) print(cmd_string)
  eval(parse(text=cmd_string))
  #logadv.z <- runMean(adjv[,"D"],n=20,cumulative=FALSE)
  #logadv.z <- log(logadv.z)
  #logadv.z <- logadv.z - 18.5 #create logadv.z, lb=20, assumed 18.5 as mean
}

#doesn't handle coln==0
calc_dol <- function(ve.xts,coln,price="R",first_pass=FALSE) {
  ticker <- sub("var.env$","",ve.xts,fixed=TRUE)
  v.de <- paste("data.env$",ticker,"[,'",ticker,".Volume']",sep="")
  cmd_string <- paste("tmp.xts <- data.env$",ticker,"[,'",ticker,".Volume']",sep="")
  eval(parse(text=cmd_string))
  tmp.xts[tmp.xts <= 0] <- 1
  if (price == "J") {
    h.de <- paste("data.env$",ticker,"[,'",ticker,".High']",sep="")
    l.de <- paste("data.env$",ticker,"[,'",ticker,".Low']",sep="")
    cmd_string <- paste(ve.xts," <- cbind(",ve.xts,",sqrt(",h.de,"*",l.de,")*tmp.xts)",sep="")
  } else if (price == "R") {
    h.de <- paste("data.env$",ticker,"[,'",ticker,".High']",sep="")
    l.de <- paste("data.env$",ticker,"[,'",ticker,".Low']",sep="")
    c.de <- paste("data.env$",ticker,"[,'",ticker,".Close']",sep="")
    cmd_string <- paste(ve.xts," <- cbind(",ve.xts,",tmp.xts*(",h.de,"*",l.de,"*",c.de,")^(1/3))",sep="")
  } else if (price == "C") {
    c.de <- paste("data.env$",ticker,"[,'",ticker,".Close']",sep="")
    cmd_string <- paste(ve.xts," <- cbind(",ve.xts,",",c.de,"*tmp.xts)",sep="")
  }
  #if (verbose) print(cmd_string)
  eval(parse(text=cmd_string))
}

#doesn't handle coln==0
#looks up etf from etf_lookup
#subtracts etf from raw variable held in each stock ve.xts
calc_stk <- function(ve.xts,coln,field,first_pass=FALSE) {
  if (first_pass) print(paste("ve.xts=",ve.xts,"field=",field))
  ticker <- sub("var.env$","",ve.xts,fixed=TRUE)
  field_num <- which(names(com.env$v.com)==field)
  clu <- com.env$v.com[[field_num]]$clu
  if (com.env$data_str != "large") { #use etf to find stk component
    etf <- com.env$etf_lookup[ticker]
    cmd_string <- paste("etf.xts <- merge(",ve.xts,"[,'",clu,"'],var.env$",etf,"[,'",clu,"'])",sep="")
    if (first_pass) print(cmd_string)
    eval(parse(text=cmd_string))
    etf.xts[,2][is.na(etf.xts[,2])] <- 0
    etf.xts <- na.omit(etf.xts)
    cmd_string <- paste(ve.xts," <- cbind(",ve.xts,",(etf.xts[,1]-etf.xts[,2]))",sep="")
    if (first_pass) print(cmd_string)
    eval(parse(text=cmd_string))
  } else { #use formula from Kim to find stk component
    sm_string <- paste0("etf.env$",ticker,"sm")
    cmd_string <- paste0("ind_tickers <- colnames(",sm_string,")")
    if (first_pass) print(cmd_string)
    eval(parse(text=cmd_string))
    sum_string <- " "
    for (ind_ticker in ind_tickers) {
      cmd_string <- paste0("etf_sum <-",sum_string,"var.env$",ind_ticker,"[,'",clu,"']*",sm_string,"[,'",ind_ticker,"']")
      sum_string <- " etf_sum + "
      if (first_pass) print(cmd_string)
      eval(parse(text=cmd_string))
    }
    # if (first_pass) {
    #   cmd_string <- paste0("print (colnames(",ve.xts,"))")
    #   eval(parse(text=cmd_string))
    #   cmd_string <- paste0("print (ncol(",ve.xts,"))")
    #   eval(parse(text=cmd_string))
    # }
    #cmd_string <- paste0(ve.xts," <- cbind(",ve.xts,",(",ve.xts,"[,'",clu,"'] - etf_sum))")
    if (!exists("etf_sum")) {
      print("problem in calc_stk, etf_sum does not exist")
      print(paste(ticker,ind_tickers,clu))
      stop()
    }
    cmd_string <- paste0("new_column <- ",ve.xts,"[,'",clu,"'] - etf_sum")
    if (first_pass) print(cmd_string)
    eval(parse(text=cmd_string))
    cmd_string <- paste0(ve.xts," <- cbind(",ve.xts,",new_column)")
    if (first_pass) {
      print(cmd_string)
      print(str(new_column))
    }
    eval(parse(text=cmd_string))
    # if (first_pass) {
    #   cmd_string <- paste0("print (colnames(",ve.xts,"))")
    #   eval(parse(text=cmd_string))
    #   cmd_string <- paste0("print (ncol(",ve.xts,"))")
    #   eval(parse(text=cmd_string))
    # }
  }
}

#doesn't handle coln==0
#looks up etf from etf_lookup
#if ETF does not exist (while predict variable does), set to zero
calc_etf <- function(ve.xts,coln,field,first_pass=FALSE) {
  if (first_pass) print(paste("ve.xts=",ve.xts,"field=",field))
  ticker <- sub("var.env$","",ve.xts,fixed=TRUE)
  clu <- com.env$v.com[[which(names(com.env$v.com)==field)]]$clu
  if (com.env$data_str != "large") { #use etf to find stk component
    #etf <- com.env$etf_lookup[ticker]
    etf_data <- paste0("var.env$",com.env$etf_lookup[ticker],"[,'",clu,"']")
  } else {
    sm_string <- paste0("etf.env$",ticker,"sm")
    cmd_string <- paste0("ind_tickers <- colnames(",sm_string,")")
    if (first_pass) print(cmd_string)
    eval(parse(text=cmd_string))
    sum_string <- " "
    for (ind_ticker in ind_tickers) {
      cmd_string <- paste0("etf_sum <-",sum_string,"var.env$",ind_ticker,"[,'",clu,"']*",sm_string,"[,'",ind_ticker,"']")
      sum_string <- " etf_sum + "
      if (first_pass) print(cmd_string)
      eval(parse(text=cmd_string))
    }
    if (!exists("etf_sum")) {
      print("problem in calc_etf, etf_sum does not exist")
      print(paste(ticker,ind_tickers,clu))
      stop()
    }
    etf_data <- "etf_sum"
  }
  cmd_string <- paste0("etf.xts <- merge(",ve.xts,"[,'",com.env$predict.clu,"'],",etf_data,")")
  if (first_pass) print(cmd_string)
  eval(parse(text=cmd_string))
  etf.xts[,2][is.na(etf.xts[,2])] <- 0
  etf.xts <- na.omit(etf.xts)
  cmd_string <- paste(ve.xts," <- cbind(",ve.xts,",etf.xts[,2])",sep="")
  eval(parse(text=cmd_string))
}

#doesn't handle coln==0
#bin 'field' by 'bin_field' using bin_points b1 & b2
#append to ve.xts as last two columns
calc_bin <- function(ve.xts,coln,field=NULL,bin_field,b1=-2.,b2=2.,first_pass=FALSE) {
  #if (first_pass) print(paste("ve.xts=",ve.xts,"coln"=coln,"field=",field,"bin_field=",bin_field,"Bins:",b1,b2,first_pass))
  if (is.null(field)) {
    f.xts <- paste0(ve.xts,"[,",coln,"]")
  } else {
    clu <- com.env$v.com[[which(names(com.env$v.com)==field)]]$clu
    f.xts <- paste0(ve.xts,"[,'",clu,"']")
  }
  clu <- com.env$v.com[[which(names(com.env$v.com)==bin_field)]]$clu
  bf.xts <- paste0(ve.xts,"[,'",clu,"']")
  x <- c(b1,b2)
  y <- c(1,0)
  cmd_string <- paste("vl.xts <- ",f.xts,"*approx(x,y,",bf.xts,",yleft=1,yright=0)$y",sep="")
  #if (first_pass) print(cmd_string)
  eval(parse(text=cmd_string))
  y <- c(0,1)
  cmd_string <- paste("vh.xts <- ",f.xts,"*approx(x,y,",bf.xts,",yleft=0,yright=1)$y",sep="")
  #if (first_pass) print(cmd_string)
  eval(parse(text=cmd_string))
  if (is.null(field)) {
    cmd_string <- paste(ve.xts," <- cbind(",ve.xts,"[,-ncol(",ve.xts,")],vl.xts,vh.xts)",sep="") 
  } else {
    cmd_string <- paste(ve.xts," <- cbind(",ve.xts,",vl.xts,vh.xts)",sep="")
  }
  #if (first_pass) print (cmd_string)
  eval(parse(text=cmd_string))
}

#doesn't handle coln==0
calc_dm <- function(ve.xts,coln,ret1,ret2,first_pass=FALSE) {
  #print(paste("calc_dm:","ve.xts=",ve.xts,"ret1=",ret1,"ret2=",ret2))
  if (ret1 == "GH") { #positive directional move
    cmd_string <- paste("r1.xts <- ",ve.xts,"$",ret1,sep="")
    eval(parse(text=cmd_string))
    cmd_string <- paste("r2.xts <- -",ve.xts,"$",ret2,sep="")
    eval(parse(text=cmd_string))
  } else if (ret1 == "KL") { #negative directional move
    cmd_string <- paste("r1.xts <- -",ve.xts,"$",ret1,sep="")
    eval(parse(text=cmd_string))
    cmd_string <- paste("r2.xts <- ",ve.xts,"$",ret2,sep="")
    eval(parse(text=cmd_string))
  } else {
    print(paste("Error in calc_dm, no valid return",ret1,ret2,sep=""))
    stop()
  }
  tmp.xts <- ifelse(r1.xts>r2.xts & r1.xts>0,r1.xts,0)
  cmd_string <- paste(ve.xts," <- cbind(",ve.xts,",tmp.xts)",sep="")
  #if (verbose) print(cmd_string)
  eval(parse(text=cmd_string))
}

#doesn't handle coln==0
#Used to calculate [log,exp,pow,abs]
#parm only used for pow 
#sign: used for [log,pow], 0=keep sign of var, 1=abs value, 2=as calculated (for power, any undefined values sent to 0)
calc_calc <- function(ve.xts,coln,type,parm=NULL,sign=0,first_pass=FALSE) {
  #print(paste("calc_calc",ve.xts,coln,type,parm,sign,first_pass))
  cmd_string <- paste0("orig_field <- ",ve.xts,"[,",coln,"]")
  #if(first_pass) print(cmd_string)
  eval(parse(text=cmd_string))
  switch(type,
         "log" = {
           #if (first_pass) print(paste("1",type,parm,sign))
           if (sign==0) {
             calc_field <- sign(orig_field)*log(abs(orig_field))
           } else if (sign==1) {
             calc_field <- log(abs(orig_field))
           } else {
             print(paste("Error:log does not support sign",sign))
             source("close_session.R")
           }
           calc_field[!is.finite(calc_field)] <- 0
         },
         "exp" = {
           #if (first_pass) print(paste("2",type,parm,sign))
           calc_field <- exp(orig_field)
         },
         "abs" = {
           #if (first_pass) print(paste("2.5",type,parm,sign))
           calc_field <- abs(orig_field)
         },
         "pow" = {
           #if (first_pass) print(paste("3",type,parm,sign))
           if (sign==0) {
             calc_field <- sign(orig_field)*'^'(abs(orig_field),parm)
           } else if (sign==1) {
             calc_field <- '^'(abs(orig_field),parm)
           } else if (sign==2) {
             calc_field <- '^'(orig_field,parm)
             calc_field[!is.finite(calc_field)] <- 0
           }
             else {
             print(paste("Error:log does not support sign",sign))
             source("close_session.R")
           }
           #calc_field[!is.finite(calc_field)] <- 0
         },
         {
           print(paste("Error: No valid calc type in calc_calc",type))
           print(paste("calc_calc",ve.xts,coln,type,parm,sign,first_pass))
           source("close_session.R")
         }
  )
  cmd_string <- paste0(ve.xts,"[,",coln,"] <- calc_field")
  #if(first_pass) print(cmd_string)
  eval(parse(text=cmd_string))
}

#doesn't handle coln==0
#Used to calculate interactions [add,mul,div,sub,rsh,fth]
#rsh,fth calcs variability given by the passed in variable (historically) and replaces with residuals or fitted values
#parm can be numeric or a passed in variable that exists in the var.env (has already been calculated and is in the requires list)
#sign: used for [mul,div], 0=as-is, 1=keep sign of first var, 2=keep sign of passed in var, 3=abs value
#sign: used for [rsh,fth] 0=use intercept term, -1=no intercept
calc_ia <- function(ve.xts,coln,type,parm=NULL,sign=0,first_pass=FALSE) {
  #print(paste("calc_calc",ve.xts,coln,type,parm,sign,first_pass))
  cmd_string <- paste0("orig_field <- ",ve.xts,"[,",coln,"]")
  #if(first_pass) print(cmd_string)
  eval(parse(text=cmd_string))
  if (!is.null(parm)) {
    if (grepl("[[:alpha:]]",parm)) {
      clu <- com.env$v.com[[which(names(com.env$v.com)==parm)]]$clu
      cmd_string <- paste0("new_field <- ",ve.xts,"[,'",clu,"']")
      #if(first_pass) print(cmd_string)
      eval(parse(text=cmd_string))
      numeric_parm <- FALSE
    } else {
      if ((type == "rsh") | (type == "fth")) {
        print("ERROR: Cannot use numeric parameter with rsh or fth in calc_ia")
        source("close_session.R")
      }
      parm <- as.numeric(parm)
      numeric_parm <- TRUE 
    }
  }
  switch(type,
         "mul" = {
           #if (first_pass) print(paste("4",type,parm,sign,numeric_parm))
           if (numeric_parm) {
             calc_field <- orig_field*parm
           } else {
             if (sign == 0) {
               calc_field <- orig_field*new_field
             } else if (sign == 1) {
               calc_field <- orig_field*abs(new_field)
             } else if (sign == 2) {
               calc_field <- abs(orig_field)*new_field
             } else if (sign == 3) {
               calc_field <- abs(orig_field*new_field)
             }
           }
         },
         "div" = {
           #if (first_pass) print(paste("6",type,parm,sign))
           if (numeric_parm) {
             calc_field <- orig_field/parm
           } else {
             if (sign == 0) {
               #print(paste(length(orig_field),length(new_field)))
               calc_field <- orig_field/new_field
               #print(length(calc_field))
             } else if (sign == 1) {
               calc_field <- orig_field/abs(new_field)
             } else if (sign == 2) {
               calc_field <- abs(orig_field)/new_field
             } else if (sign == 3) {
               calc_field <- abs(orig_field/new_field)
             }
             calc_field[!is.finite(calc_field)] <- 0   #to handle division by 0
           }
         },
         "add" = {
           #if (first_pass) print(paste("5",type,parm,sign))
           if (numeric_parm) {
             calc_field <- orig_field + parm
           } else {
             calc_field <- orig_field + new_field 
           }
           #print("finished add")
         },
         "sub" = {
           #if (first_pass) print(paste("7",type,parm,sign))
           if (numeric_parm) {
             calc_field <- orig_field - parm
           } else {
             calc_field <- orig_field - new_field
           }
         },
         "fth" =,  #replace with historical regression using new_field as predictor variable (variability shared)
         "rsh" = { #replace with historical residuals (remove variability explained by new_field variable)
           #if (first_pass) print(paste("8",type,parm,sign))
           if (sign==0) {
             .env <- environment()
             f <- as.formula("orig_field ~ new_field",env=.env)
             rcor.model <- lm(f)
           } else if (sign==-1) { #no intercept
             .env <- environment()
             f <- as.formula("orig_field ~ new_field - 1",env=.env)
             rcor.model <- lm(f)
           } else {
             print(paste("Calc_calc,",type," does not support sign=",sign))
             source("close_session.R")
           }
           sel <- which(is.finite(orig_field) & is.finite(new_field)) #both exist so regression should have a residual
           calc_field <- orig_field
           if (length(sel) != length(residuals(rcor.model))) {
             print(paste("Problem in calc_ia",type," number of residuals does not match input data",length(sel),length(residuals(rcor.model),ve.xts)))
             source("close_session.R")
           } else if (type == "rsh") {
             calc_field[sel] <- residuals(rcor.model)
           } else if (type == "fth") {
             calc_field[sel] <- fitted(rcor.model)   
           }
         },
         {
           print(paste("Error: No valid calc type in calc_calc",type))
           source("close_session.R")
         }
  )
  cmd_string <- paste0(ve.xts,"[,",coln,"] <- calc_field")
  #if(first_pass) print(cmd_string)
  eval(parse(text=cmd_string))
}

#doesn't handle coln==0
#splits data into quantiles(# from qcount) and approximates a rank from 0-1.0 from these quantiles (in reg_date_range)
calc_rank <- function(ve.xts,coln,qcount,first_pass=FALSE) {
  #print(paste("calc_rank",ve.xts,coln,first_pass))
  data_string <- paste0(ve.xts,"[com.env$reg_date_range,",coln,"]")
  cmd_string <- paste0("check_single_value <- (max(",data_string,",na.rm=TRUE)==min(",data_string,",na.rm=TRUE))")
  eval(parse(text=cmd_string))
  out_string <- paste0(ve.xts,"[,",coln,"]")
  if (check_single_value) {
    cmd_string <- paste0(out_string,"<- 1")
    #if (first_pass) cat("Warning: rank data is all the same in",data_string,"setting",out_string,"to 1.\n")
  } else {
    cmd_string <- paste0("deciles <- quantile(",data_string,",probs=seq(0,1,(1/",qcount,")),na.rm = TRUE)")
    #if(first_pass) print(cmd_string)
    eval(parse(text=cmd_string))
    cmd_string <- paste0(out_string,"<- approx(deciles,(as.numeric(sub('%','',names(deciles)))/100),",out_string,",yleft=0,yright=1)$y")
    #if(first_pass) print(cmd_string)
    eval(parse(text=cmd_string))
  }
}

#make_vars.R
make_vars <- function(vd = NULL) {
  if (!is.null(vd)) if (vd$ID <= 0) vd <- NULL 
  make_vcom <- is.null(vd)
  first_pass <- TRUE
  #if (com.env$verbose) 
  #print(paste("make_vars",make_vcom))
  # if (make_vcom) {
  #   col.calc <- NULL
  #   col.etf.calc <- NULL
  #   col.calc[1:length(com.env$v.com)] <- FALSE
  #   col.etf.calc[1:length(com.env$v.com)] <- FALSE         #boolean array to indicate if column has been found for each var in v.com
  # } 
  for (stk in 1:(com.env$stx + com.env$etfs)) {
    ticker <- com.env$stx_list[stk]
    first_pass <- (ticker == "FTR")
    #print(paste("Getting data for:",ticker))
    is.etf <- (com.env$etf_lookup[[ticker]] == 'etf')
    ve.xts <- paste("var.env$",ticker,sep="")
    if (make_vcom) {
      for (v in 1:length(com.env$v.com)) {
        if (com.env$v.com[[v]]$use == "def") next                #nothing to store in var.env
        if (is.etf & !com.env$v.com[[v]]$calc_etf) next          #nothing to compute in etf
        #if ((is.etf & !col.etf.calc[v]) | (!is.etf & !col.calc[v])) {  #calculate new column num and insert it into v.com
          if (!exists(ticker,envir=var.env,inherits=FALSE)) {
            coln <- 1
          } else {
            cmd_string <- paste("coln <- ncol(",ve.xts,") + 1",sep="")
            eval(parse(text=cmd_string))
          }
          if (is.etf) {
            com.env$v.com[[v]]$etf_col <- coln 
            #col.etf.calc[v] <- TRUE
          } else {
            com.env$v.com[[v]]$col <- coln
            #col.calc[v] <- TRUE
          }
          #if (first_pass) print(paste("finding col",coln," for v",v,is.etf))
        #} 
        vd <- com.env$v.com[[v]]
        if (coln != 1) {  #var.env exists
          if (vd$bins > 1) {
            cmd_string <- paste0("col_num <- which(paste0(vd$clu,'_1') == colnames(",ve.xts,"))")
          } else {
            cmd_string <- paste0("col_num <- which(vd$clu == colnames(",ve.xts,"))")
          }
          #if (first_pass) print(cmd_string)
          eval(parse(text=cmd_string))
          if (length(col_num) > 0) {  #already calculated, update col field
            if (is.etf) {
              com.env$v.com[[v]]$etf_col <- col_num 
              #col.etf.calc[v] <- TRUE
            } else {
              com.env$v.com[[v]]$col <- col_num
              #col.calc[v] <- TRUE
            }
            next
          }
        }
        coln <- ifelse(is.etf,vd$etf_col,vd$col)
        for (m in 1:length(vd$math)) {
          math <- strsplit(vd$math[m],split=",")[[1]]
          parms <- gsub("^[^,]*,","",vd$math[m])
          fun_call <- paste(math[1],"('",ve.xts,"',",coln,",",parms,",first_pass=first_pass)",sep="")
          if (first_pass) print(paste(fun_call,"m=",m,"v=",v,first_pass))
          eval(parse(text=fun_call))
        }
        #if (first_pass) print(paste(coln,vd$name))
        name.var(ve.xts,coln,vd$clu,vd$bins,first_pass)
        # if (ticker == "BAC") {
        #   if (any(grepl("\\.",colnames(var.env$BAC)))) {
        #     print(colnames(var.env$BAC))
        #     print("in make_vars")
        #     source("close_session.R")
        #   }
        # }
      } #end make var loop
    } else { #vd passed in, mod var
      if (is.etf & !vd$calc_etf) next          #nothing to compute in etf
      if (vd$bins > 1) {
        cmd_string <- paste0("col_num <- which(paste0(vd$clu,'_1') == colnames(",ve.xts,"))")
      } else {
        cmd_string <- paste0("col_num <- which(vd$clu == colnames(",ve.xts,"))")
      }
      #if (first_pass) print(cmd_string)
      eval(parse(text=cmd_string))
      if (length(col_num) > 0) {
        if (is.etf) {
          vd$etf_col <- col_num 
        } else {
          vd$col <- col_num
        }
        print("mod_var already calculated, returning from make_var with no calculation")
        return(vd)
      }
      cmd_string <- paste("coln <- ncol(",ve.xts,") + 1",sep="")
      eval(parse(text=cmd_string))
      #if (first_pass) print(paste('mod_var',vd$vcom_num,coln))
      if (is.etf) {
        vd$etf_col <- coln
      } else {
        vd$col <- coln
      }
      for (m in 1:length(vd$math)) {
        math <- strsplit(vd$math[m],split=",")[[1]]
        parms <- gsub("^[^,]*,","",vd$math[m])
        fun_call <- paste(math[1],"('",ve.xts,"',",coln,",",parms,")",sep="")
        if (first_pass) print(paste(fun_call,"m=",m,"v=",vd$vcom_num))
        eval(parse(text=fun_call))
      }
      if (is.null(vd$clu)) {
        print(vd)
        print(fun_call)
      }
      name.var(ve.xts,coln,vd$clu,vd$bins,first_pass)
      #if (first_pass) {
      #  cmd_string <- paste("print(length(colnames(",ve.xts,")))",sep="")
        #print(cmd_string)
      #  eval(parse(text=cmd_string))
      #}
    } #end mod var
    first_pass <- FALSE
  } #end stock loop
  # if (any(grepl("\\.",colnames(var.env$BAC)))) {
  #   print(colnames(var.env$BAC))
  #   print("in make_vars")
  #   source("close_session.R")
  # }
  if (!make_vcom) return(vd)
}

calc_vd <- function(vd) { #for use in computing mu,ADJRET,VLTY  #appended to each var.env$ticker xts object
  print("calc VD")
  print(paste("calc_vd",vd$name,vd$math[1]))
  first_pass <- TRUE
  print("starting stk loop")
  for (stk in 1:(com.env$stx+com.env$etfs)) {
    #print(stk)
    ticker <- com.env$stx_list[stk]
    #print(paste("Getting data for:",ticker))
    is.etf <- (com.env$etf_lookup[[ticker]] == 'etf')
    ve.xts <- paste0("var.env$",ticker)
    if (is.etf & !vd$calc_etf) next                           #nothing to compute in etf
    if (!exists(ticker,envir=var.env,inherits=FALSE)) {
      c <- 1
    } else {
      cmd_string <- paste("c <- ncol(",ve.xts,") + 1",sep="")      
      eval(parse(text=cmd_string))                               #get column number
    }
    #print(c)
    vd$col <- c

    coln <- vd$col
    for (m in 1:length(vd$math)) {
      math <- strsplit(vd$math[m],split=",")[[1]]
      parms <- gsub("^[^,]*,","",vd$math[m])
      fun_call <- paste(math[1],"('",ve.xts,"',",coln,",",parms,",first_pass=first_pass)",sep="")
      #if (first_pass) print(fun_call)
      eval(parse(text=fun_call))
    }
    #print(coln-1+length(vd$name))
    if (is.null(vd$clu)) {
      print(vd)
      print(fun_call)
    }
    name.var(ve.xts,coln,vd$clu,vd$bins,first_pass)
    if (stk>1) first_pass <- FALSE
  }
}

stk_matrix <- function(env_name,type,index=0) {
  print(paste("stk_matrix",env_name,type,index))
  col_lu <- type
  orig_col_lu <- col_lu
  if (index != 0) {type = paste0(type,index)}
  mtrx <- paste0("var.env$",type)
  cmd_string <- paste0(mtrx," <- NULL")
  eval(parse(text=cmd_string))

  for (stk in 1:(com.env$stx+com.env$etfs)) {
    ticker <- com.env$stx_list[stk]
    if (env_name == "data.env") col_lu <- paste0(ticker,".",orig_col_lu)
    #print(paste("Getting data for:",ticker))
    is.etf <- (com.env$etf_lookup[[ticker]] == 'etf')
    if (is.etf) next                           #nothing to compute in etf
    ve.xts <- paste0(env_name,"$",ticker)
    cmd_string <- paste0(mtrx," <- cbind(",mtrx,",",ve.xts,"[,'",col_lu,"'])")
    #print(cmd_string)
    eval(parse(text=cmd_string))
    cmd_string <- paste0("colnames(",mtrx,")[ncol(",mtrx,")] <- '",ticker,"'")
    #print(cmd_string)
    eval(parse(text=cmd_string))
  }
}

mu_calc <- function(mu_col_name,index=0) {
  print("In mu_calc")
  V1 <- NULL
  V1$col <- 1
  V1$bins <- 1
  V1$name <- mu_col_name
  V1$clu <- mu_col_name
  V1$calc_etf <- FALSE
  V1$math[1] <- "calc_prediction,'com.env$model.current'"
  calc_vd(V1)
  stk_matrix("var.env",mu_col_name,index)
}

# adjret_calc <- function() {
#   print("In adjret_calc")
#   V1 <- NULL
#   V1$col <- 1
#   V1$bins <- 1
#   V1$name <- "ADJRET"
#   V1$clu <- "ADJRET"
#   V1$calc_etf <- FALSE
#   V1$math[1] <- "calc_adjret,'.Adjusted'"
#   calc_vd(V1)
#   stk_matrix("var.env","ADJRET")
# }
# 
# vlty_calc <- function() {
#   print("In vlty_calc")
#   V1 <- NULL
#   V1$col <- 1
#   V1$bins <- 1
#   V1$name <- "VLTY"
#   V1$clu <- "VLTY"
#   V1$calc_etf <- FALSE
#   V1$math[1] <- "calc_vlty,'ADJRET',window=250"
#   calc_vd(V1)
#   stk_matrix("var.env","VLTY")
# }

make_mu <- function() {
  print("In make_mu")
  check_vcom(com.env$v.com,"In make_mu")
  if (com.env$save_var_n == 0) {  #if saving vars model evaluation has already been done
    print(paste("Evaluating 1st model in make_mu",Sys.time()))
    eval_adj_r2(sim_data=TRUE)
  }
  mu_calc("mu")
  stk_matrix("data.env","ADJRET")
  stk_matrix("data.env","VLTY")
  
  #if (com.env$retvlty_not_calced) {
    #adjret_calc()
    #vlty_calc()
    #com.env$retvlty_not_calced <- FALSE
  #}
  if (com.env$load_multi_model) {
    for (i in 2:length(com.env$model_list)) {
      print("clearing var.env")
      var_env_list <- ls(var.env)
      keep_list <- c("mu","VLTY","ADJRET")
      var_env_list <- var_env_list[!(var_env_list %in% keep_list)]
      rm(list=var_env_list,envir=var.env)
      #    for (del_var in var_env_list) {
      #      if (del_var %in% keep_list) next
      #      cmd_string <- paste0("rm('",del_var,"',envir=var.env)")
      #      print(cmd_string)
      #      eval(parse(text=cmd_string))
      #    }
      print(ls(var.env))
      load_model(com.env$model_list[i])
      print(paste("Evaluating model,",i,com.env$model_list[i],Sys.time()))
      eval_adj_r2(oos_data=TRUE)
      mu_calc(i)
      cmd_string <- paste0("var.env$mu <- var.env$mu + var.env$mu",i)
      print(cmd_string)
      eval(parse(text=cmd_string))
    }
  }
}  

mu_liqx_calc <- function(mu_col_name,index=0) {
  print(paste("In mu_liqx_calc",mu_col_name))
  V1 <- NULL
  V1$col <- 1
  V1$bins <- 1
  V1$name <- mu_col_name
  V1$clu <- mu_col_name
  V1$calc_etf <- FALSE
  if (mu_col_name == "mu_ll") {
    V1$math[1] <- "calc_prediction,'com.env$model.ll'"
  } else if (mu_col_name == "mu_hl") {
    V1$math[1] <- "calc_prediction,'com.env$model.hl'"
  } else {
    print(paste("Error in mu_liqx_calc, mu_col_name unknown:",mu_col_name))
    source("close_session.R")
  }    
  calc_vd(V1)
  #stk_matrix("var.env",mu_col_name,index)
}

make_mu_liqx <- function() {
  print("In make_mu_liqx")
  #check_vcom(com.env$v.com,"In make_mu_liqx")
  print("run weighted regression with liquidity cross")
  run_wt_regression(com.env$clu_names)
  #if (com.env$save_var_n == 0) {  #if saving vars model evaluation has already been done
  #  print(paste("Evaluating 1st model in make_mu",Sys.time()))
  #  eval_adj_r2(sim_data=TRUE)
  #}
  mu_liqx_calc("mu_ll")
  mu_liqx_calc("mu_hl")
  #stk_matrix("data.env","ADJRET")
  #stk_matrix("data.env","VLTY")
  first_pass <- FALSE
  for (ticker in com.env$stx_list) {
    is.etf <- (com.env$etf_lookup[[ticker]] == 'etf')
    if (is.etf) next                           #nothing to compute in etf
    array_str <- paste0("var.env$",ticker)
    cmd_str <- paste0("mu_liqx <- ",array_str,"[,'ll_wts']*",array_str,"[,'mu_ll'] + ",array_str,"[,'hl_wts']*",array_str,"[,'mu_hl']")
    if (first_pass) print(cmd_str)
    eval(parse(text=cmd_str))
    cmd_str <- paste(array_str," <- cbind(",array_str,",mu_liqx)")
    if (first_pass) print(cmd_str)
    eval(parse(text=cmd_str))
    cmd_str <- paste0("colnames(",array_str,")[ncol(",array_str,")] <- 'mu_liqx'")
    if (first_pass) print(cmd_str)
    eval(parse(text=cmd_str))
    first_pass <- FALSE
  }
  stk_matrix("var.env","mu_liqx",0)
}
  
  
#function pre-calculates adjusted prices and dollars in data environment, used once per data load
calc_adjusted_HLOJRlD <- function(symbol_list) {
  print("calc_adjusted_HLOJRlD")
  first_pass <- FALSE
  #if ("DNB" %in% symbol_list) print("DNB in symbol_list in calc_adjusted_HLOJRlD")
  stkmod <- paste0("data.env$",com.env$stkmod_name)
  for (ticker in symbol_list) {
    df <- paste0("data.env$",ticker)
    cmd_string <- paste0("if('",ticker,".H' %in% colnames(",df,"))  next")
    eval(parse(text=cmd_string))
    de.adjc <- paste0(df,"[,'",ticker,".Adjusted']")
    de.c <- paste0(df,"[,'",ticker,".Close']")
    de.L <- paste0(df,"[,'",ticker,".L']")
    de.H <- paste0(df,"[,'",ticker,".H']")
    de.J <- paste0(df,"[,'",ticker,".J']")
    de.R <- paste0(df,"[,'",ticker,".R']")
    de.Volume <- paste0(df,"[,'",ticker,".Volume']")
    de.D <- paste0(df,"[,'",ticker,".D']")
    de.V <- paste0(df,"$",ticker,".","V")
    de.ADJRET <- paste0(df,"[,'",ticker,".ADJRET']")
    shout <- paste0("shout_table[,",ticker,"]")
    
    #adjusting HLO with adjusted Close
    for (field in c("High","Low","Open")) {
      de.xts <- paste(df,"[,'",ticker,".",field,"']",sep="")
      cmd_string <- paste0(df," <- cbind(",df,",",de.xts,"*",de.adjc,"/",de.c,")")
      if (first_pass) print(cmd_string)
      eval(parse(text=cmd_string))
      cmd_string <- paste0("colnames(",df,")[length(colnames(",df,"))] <- '",ticker,".",substr(field,1,1),"'")
      if (first_pass) print(cmd_string)
      eval(parse(text=cmd_string))
    }
    cmd_string <- paste0(df," <- cbind(",df,",(",de.L,"*",de.H,")^(0.5))")
    if (first_pass) print(cmd_string)
    eval(parse(text=cmd_string))
    cmd_string <- paste0("colnames(",df,")[length(colnames(",df,"))] <- '",ticker,".J'")
    if (first_pass) print(cmd_string)
    eval(parse(text=cmd_string))
    cmd_string <- paste0(df," <- cbind(",df,",(",de.L,"*",de.H,"*",de.adjc,")^(1/3))")
    if (first_pass) print(cmd_string)
    eval(parse(text=cmd_string))
    cmd_string <- paste0("colnames(",df,")[length(colnames(",df,"))] <- '",ticker,".R'")
    if (first_pass) print(cmd_string)
    eval(parse(text=cmd_string))
    cmd_string <- paste0(df," <- cbind(",df,",(",de.Volume,"*",de.c,"))") #unadjusted close price (assuming volume is unadjusted)
    if (first_pass) print(cmd_string)
    eval(parse(text=cmd_string))
    cmd_string <- paste0("colnames(",df,")[length(colnames(",df,"))] <- '",ticker,".D'")
    if (first_pass) print(cmd_string)
    eval(parse(text=cmd_string))
    cmd_string <- paste0(df," <- cbind(",df,",log(",de.D,") - 18.5)")
    if (first_pass) print(cmd_string)
    eval(parse(text=cmd_string))
    cmd_string <- paste0("colnames(",df,")[length(colnames(",df,"))] <- '",ticker,".V'")
    if (first_pass) print(cmd_string)
    eval(parse(text=cmd_string))
    cmd_string <- paste0(de.V,"[!(is.finite(",de.V,"))] <- -18.5")
    if (first_pass) print(cmd_string)
    eval(parse(text=cmd_string))
    
#    calc_adjret <- function(ve.xts,coln,field,first_pass=FALSE) {
    cmd_string <- paste0("tmp.xts <- stats::lag(",de.adjc,",-1)/",de.adjc)
    if (first_pass) print(cmd_string)
    eval(parse(text=cmd_string))
    tmp.xts[is.na(tmp.xts)] <- 0                  #replace NA's with 0
    cmd_string <- paste0(df," <- cbind(",df,",tmp.xts)")
    if (first_pass) print(cmd_string)
    eval(parse(text=cmd_string))
    cmd_string <- paste0("colnames(",df,")[length(colnames(",df,"))] <- '",ticker,".ADJRET'")
    if (first_pass) print(cmd_string)
    eval(parse(text=cmd_string))
    
#    V1$math[1] <- "calc_vlty,'ADJRET',window=250"
#    calc_vlty <- function(ve.xts,coln,field=NULL,window=60,first_pass=FALSE) { #take vlty of field (in var.env) and append as last column
    cmd_string <- paste0("tmp.xts <- xts(apply(",de.ADJRET,",2,runSD,n=com.env$vlty_window), index(",df,"))")
    if (first_pass) print(cmd_string)
    eval(parse(text=cmd_string))
    tmp.xts <- stats::lag(tmp.xts,1)   #not needed if using valid raw/scale/model vars [field provided]
    tmp.xts <- tmp.xts*tmp.xts
    cmd_string <- paste0(df," <- cbind(",df,",tmp.xts)")
    if (first_pass) print(cmd_string)
    eval(parse(text=cmd_string))
    cmd_string <- paste0("colnames(",df,")[length(colnames(",df,"))] <- '",ticker,".VLTY'")
    if (first_pass) print(cmd_string)
    eval(parse(text=cmd_string))
    
    if (!ticker %in% colnames(data.env$shout_table)) {
      #print(paste("WARNING: No shout for ticker:",ticker))
      cmd_string <- paste0(df,"$",ticker,".shout <- NA")
      if (first_pass) print(cmd_string)
      eval(parse(text=cmd_string))
    } else {
      cmd_string <- paste0(df," <- cbind(",df,",data.env$shout_table[",com.env$data_date_range,",'",ticker,"'])")
      if (first_pass) print(cmd_string)
      eval(parse(text=cmd_string))
      cmd_string <- paste0("colnames(",df,")[length(colnames(",df,"))] <- '",ticker,".shout'")
      if (first_pass) print(cmd_string)
      eval(parse(text=cmd_string))
    }
    
    #Low Liquidity / High Liquidity Binning
    x <- c(com.env$ll_bin,com.env$hl_bin)
    y <- c(1,0)
    cmd_string <- paste0("ll.xts <- approx(x,y,",de.V,",yleft=1,yright=0)$y")
    if (first_pass) print(cmd_string)
    eval(parse(text=cmd_string))
    y <- c(0,1)
    cmd_string <- paste0("hl.xts <- approx(x,y,",de.V,",yleft=0,yright=1)$y")
    if (first_pass) print(cmd_string)
    eval(parse(text=cmd_string))
    cmd_string <- paste0(df," <- cbind(",df,",ll.xts,hl.xts)")
    eval(parse(text=cmd_string))
    cmd_string <- paste0("colnames(",df,")[length(colnames(",df,"))-1] <- '",ticker,".ll_wts'")
    if (first_pass) print(cmd_string)
    eval(parse(text=cmd_string))
    cmd_string <- paste0("colnames(",df,")[length(colnames(",df,"))] <- '",ticker,".hl_wts'")
    if (first_pass) print(cmd_string)
    eval(parse(text=cmd_string))
    
    #create stk_models by day (from Kim's code)
    if (com.env$data_str == "large") {
      cmd_string <- paste("dep_index <- index(",df,"[paste0(start(",df,"),'/',end(",df,"))])")
      if (first_pass) print(cmd_string)
      eval(parse(text=cmd_string))
      sm.xts <- xts(x=matrix(0,nrow=length(dep_index),ncol=length(load.env$one_res[[ticker]])),order.by=dep_index)
      colnames(sm.xts) <- names(load.env$one_res[[ticker]])
      for (ind_ticker in colnames(sm.xts)) {
        if (!ind_ticker %in% symbol_list) {
          sm.xts <- sm.xts[,-which(ind_ticker == colnames(sm.xts))]
        } else {
          cmd_string <- paste("ind_index <- index(data.env$",ind_ticker,"[paste0(start(data.env$",ind_ticker,"),'/',end(data.env$",ind_ticker,"))])")
          if (first_pass) print(cmd_string)
          eval(parse(text=cmd_string))
          sm.xts[ind_index,ind_ticker] <- load.env$one_res[[ticker]][[ind_ticker]]
        }
      }
      sm.xts <- sm.xts / rowSums(sm.xts)
      cmd_string <- paste0("etf.env$",ticker,"sm <- sm.xts")
      if (first_pass) print(cmd_string)
      eval(parse(text=cmd_string))
    }
    first_pass <- FALSE
  }
  print(paste("rows of data.env$CBE",nrow(data.env$CBE),"last index",index(data.env$CBE)[nrow(data.env$CBE)]))
}

#make_vars.R
calc_all_vars <- function() {
  lapply(com.env$v.com,calc_one_var)
}

calc_one_var <- function(vd) {
  if (is.null(vd)) {
    print("calc_one_var requires a vd")
    stop()
  }
  if (vd$use == "def") return()  #nothing to calculate
  #get col_num and etf_col_num
  col_num <- 0
  etf_col_num <- 0
  etf_ve.xts <- paste0("var.env$",com.env$etf.symbols[[1]])  #first etf
  stx_ve.xts <- paste0("var.env$",com.env$stx.symbols[[1]])  #first stk #any will do
  if (vd$bins > 1) {
    cmd_string <- paste0("etf_col_num <- which(paste0(vd$clu,'_1') == colnames(",etf_ve.xts,"))")
    eval(parse(text=cmd_string))
    cmd_string <- paste0("stx_col_num <- which(paste0(vd$clu,'_1') == colnames(",stx_ve.xts,"))")
    eval(parse(text=cmd_string))
  } else {
    cmd_string <- paste0("etf_col_num <- which(vd$clu == colnames(",etf_ve.xts,"))")
    eval(parse(text=cmd_string))
    cmd_string <- paste0("stx_col_num <- which(vd$clu == colnames(",stx_ve.xts,"))")
    eval(parse(text=cmd_string))
  }
  eval(parse(text=cmd_string))
  if (length(stx_col_num)>0) {
    #print(paste("var already calculated",vd$clu,vd$col,stx_col_num,vd$etf_col,etf_col_num,vd$var_name))
    if (length(etf_col_num) > 0) vd$etf_col <- etf_col_num
    vd$col <- stx_col_num
    return(vd)
  }
  if (!exists(com.env$stx.symbols[[1]],envir=var.env,inherits=FALSE)) {
    vd$etf_col <- 1
    vd$col <- 1
  } else {
    if (!vd$calc_etf) {
      vd$etf_col <- -1
    } else {
      cmd_string <- paste0("vd$etf_col <- ncol(",etf_ve.xts,") + 1")
      eval(parse(text=cmd_string))
    }
    cmd_string <- paste0("vd$col <- ncol(",stx_ve.xts,") + 1")
    eval(parse(text=cmd_string))
  }

  num_m <- length(vd$math)  
  for (m in 1:num_m) {  #loop over all math functions 
    math <- strsplit(vd$math[m],split=",")[[1]]
    parms <- gsub("^[^,]*,","",vd$math[m])  #delete math[1] and first ','
    first_pass <- FALSE
    if (first_pass) print(paste('mod_var',vd$vcom_num,vd$col,vd$etf_col))
    
    if ((substr(math[1],nchar(math[1])-1,nchar(math[1]))=="_x") | grepl("rsx",parms) | grepl("ftx",parms)) {  
      #cross-sectional calc, need to calc all stocks and etfs in one call      
      fun_call <- paste0(math[1],"(",vd$col,",",vd$etf_col,",",parms,",first_pass=first_pass)")
      if (first_pass) print(paste(fun_call,"m=",m,"v=",vd$vcom_num))
      eval(parse(text=fun_call))
      if ((m==1) | ((m==num_m) & (vd$bins>1))) {  #loop over etfs and stx and name columns
        bin_parm <- ifelse(((m==1) & (num_m>1)),1,vd$bins)
        for (ticker in com.env$stx_list) { 
          is.etf <- (com.env$etf_lookup[[ticker]] == 'etf')
          if (is.etf & !vd$calc_etf) next          #nothing to compute in etf
          coln <- ifelse(is.etf,vd$etf_col,vd$col)
          ve.xts <- paste0("var.env$",ticker)
          name.var(ve.xts,coln,vd$clu,bin_parm,fist_pass)
        }
      }
    } else {
      for (ticker in com.env$stx_list) {  #loop over all etfs and stx
        #if (first_pass) print(paste("Getting data for:",ticker))
        is.etf <- (com.env$etf_lookup[[ticker]] == 'etf')
        if (is.etf & !vd$calc_etf) next          #nothing to compute in etf
        coln <- ifelse(is.etf,vd$etf_col,vd$col)
        # if (is.na(coln)) {
        #   print(paste(is.etf,vd$etf_col,vd$col,exists(ticker,envir=var.env,inherits=FALSE)))
        # } else {
        #   print(paste("coln=",coln))
        # }
        ve.xts <- paste0("var.env$",ticker)
        fun_call <- paste0(math[1],"('",ve.xts,"',",coln,",",parms,",first_pass=first_pass)")
        if (first_pass) print(paste(fun_call,"m=",m,"v=",vd$vcom_num))
        eval(parse(text=fun_call))
        #if ((m==1) & (num_m>1))       name.var(ve.xts,coln,vd$clu,      1,first_pass)
        #if ((m==num_m) & (vd$bins>1)) name.var(ve.xts,coln,vd$clu,vd$bins,first_pass)
        name.var(ve.xts,coln,vd$clu,vd$bins,first_pass)
        first_pass <- FALSE
      }
    }
  }
  if (any(grepl("\\.",colnames(var.env$BTC)))) {
    print(colnames(var.env$BTC))
    print("in calc_one_var")
    source("close_session.R")
  }
  return(vd)
}