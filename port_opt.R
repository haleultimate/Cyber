#run portfolio optimizatoin [lp_sim; below] with expectations as is
#then reverses expectations and reruns portfolio optimization
run_sim <- function() {
  print("run_sim")
  print(com.env$clu_names)
  make_mu()       #calc mu,VLTY,ADJRET for each var.env xts object
  sim.env$alpha_wt <- 1.
  sim.env$pca_mult <- 1.              #adjust when relative magnitudes are known
  sim.env$vlty_wt <- 0.0001
  sim.env$opt_oc <- FALSE
  sim.env$scale_bp <- 1.
  sim.env$mean_adjust_mu <- TRUE
  sim.env$pca_type <- 40      #{"PCA_ETF", #loaded PCA vectors to balance}
  sim.env$port_size_mult <- 10000
  sim.env$port_size <- sim.env$init_equity <- sim.env$port_size_mult*com.env$stx
  if ((sim.env$pca_type == "PCA_ETF") | (com.env$data_str != "large")) {
    sim.env$pca <- sim.env$pca_etf
    sim.env$pca_wt[1] <- 1.
    sim.env$pca_wt[2:ncol(sim.env$pca_etf)] <- 0.1 
  } else if (is.numeric(sim.env$pca_type)) {
    sim.env$pca <- sim.env$PCA.array[,1:sim.env$pca_type]
    sim.env$pca_wt[1:sim.env$pca_type] <- load.env$pca.pve[1:sim.env$pca_type]
  } else {
    print(paste("error in pca_type",sim.env$pca_type))
    source("close_session.R")
  }
  print(paste("Total equity:",sim.env$init_equity))
  lp_sim("mu",com.env$stx.symbols,com.env$sim_date_index,sim.env$port_size_mult*length(com.env$stx.symbols),plot_profit=TRUE)      #run sim, plot daily profit
  if (com.env$liqx) {
    make_mu_liqx()
    lp_sim("mu_liqx",com.env$stx.symbols,com.env$sim_date_index,sim.env$port_size_mult*length(com.env$stx.symbols),plot_profit=TRUE)      #run sim, plot daily profit
    sim_stats <- calc.r2(com.env$model.current,var.env$sim_data.df)
    cat("r2",sim_stats$rsq,"r20",sim_stats$rsq0,"cor",sim_stats$cor,"\n")
    cat("mse",sim_stats$mse,"mean",sim_stats$mean,"wpct",sim_stats$winpct,"\n")
  }
  #var.env$mu <- -var.env$mu
  #print("reversing mu")
  #lp_sim("mu",com.env$stx.symbols,com.env$sim_date_index,plot_profit=TRUE)  #source("blotter_sim.R")
}

calc_order_costs <- function(shares,tc) {
  stocks <- ncol(shares)
  days <- nrow(shares)
  order_costs <- matrix(data=0,nrow=days,ncol=stocks)
  for (j in 1:stocks) {
    if (shares[1,j] != 0) order_costs[1,j] <- tc$oc + tc$bp*shares[1,j]
  }
  for (i in 2:days) {
    for (j in 1:stocks) {
      if (abs(shares[i-1,j]-shares[i,j])>100) order_costs[i,j] <- tc$oc + tc$bp*(abs(shares[i,j]-shares[i-1,j]))
    }
  }
  return(order_costs)
}

#lp_sim sim calls port_opt_lp [optimizes portfolio position daily; below]
#calculates profit by multiplying daily (cc_return)*(position in dollars [aka shares])
#add in ordering logic
#return profit
lp_sim <- function(mu_col_name,stx,sim_date_index,equity,plot_profit=FALSE) {
  print (paste("Running Sim",Sys.time()))
  #shares <- matrix(nrow=length(sim_date_index),ncol=length(com.env$stx.symbols))
  sim.env$shares <- matrix(nrow=length(sim_date_index),ncol=length(stx))
  first_pass <- FALSE
  sim.env$tc$oc <- 1      #transaction cost per order
  sim.env$tc$bp <- 0.0007  #transaction cost per dollar
  
  for (i in 1:(length(sim_date_index))) {
  #for (i in 1:10) {
    SimDate <- sim_date_index[i]
    if (i == 1) {
      port_str <- "rep(0,length(stx))"
    } else {
      #yesterday_SimDate <- sim_date_index[SimDate_i-1]
      #print(paste(SimDate,yesterday_SimDate))
      port_str <- "as.vector(sim.env$shares[i-1,1:length(stx)])"
    }
    
    if (i %% 50 == 0) print(paste(i,"DATE:",SimDate,Sys.time()))
    if (first_pass) {
      cmd_string <- paste0("port.pos <- port_opt_lp(as.vector(var.env$",mu_col_name,
                           "[SimDate,stx]),as.vector(var.env$VLTY[SimDate,stx]),equity,",
                           port_str,",sim.env$pca,first_pass)")
      #first_pass <- FALSE
    } else {
      cmd_string <- paste0("port.pos <- port_opt_lp(as.vector(var.env$",mu_col_name,
                           "[SimDate,stx]),as.vector(var.env$VLTY[SimDate,stx]),equity,",
                           port_str,",sim.env$pca)")
    }
    eval(parse(text=cmd_string))
    if (first_pass) {
      print(cmd_string)
      first_pass <- FALSE
    }
    #port.pos <- port_opt_lp(as.vector(var.env$mu[SimDate]),as.vector(var.env$VLTY[SimDate]),equity)
    sim.env$shares[i,] <- port.pos
    #print(port.pos)
  }
  
  #sim.env$shares[abs(sim.env$shares)<share_min] <- 0
  ADJRET.matrix <- as.matrix(var.env$ADJRET[sim_date_index,stx]) - 1
  mu.matrix <- as.matrix(var.env$mu[sim_date_index,stx])
  VLTY.matrix <- as.matrix(var.env$VLTY[sim_date_index,stx])

  sim.env$order_costs <- calc_order_costs(sim.env$shares,sim.env$tc)
  sim.env$mu_shares <- mu.matrix*sim.env$shares
  sim.env$day_mu <- rowSums(sim.env$mu_shares)
  sim.env$VLTY_shares <- VLTY.matrix*sim.env$shares*sim.env$shares
  sim.env$day_VLTY <- rowSums(sim.env$VLTY_shares)

  sim.env$ADJRET_shares <- ADJRET.matrix*sim.env$shares
  sim.env$ADJRET_shares[is.na(sim.env$ADJRET_shares)] <- 0     #should we repair or crash when NA's are found?
  sim.env$dayprofit <- rowSums(sim.env$ADJRET_shares)
  sim.env$stockprofit <- colSums(sim.env$ADJRET_shares)
  sim.env$totalprofit <- cumsum(sim.env$dayprofit)
  sim.env$daytc <- rowSums(sim.env$order_costs)
  sim.env$stocktc <- colSums(sim.env$order_costs)
  sim.env$totaltc <- cumsum(sim.env$daytc)
  sim.env$net.day.profit <- sim.env$dayprofit - sim.env$daytc
  sim.env$net.total.profit <- sim.env$totalprofit - sim.env$totaltc
  total.profit <- sim.env$totalprofit[length(sim.env$totalprofit)]
  total.tc <- sim.env$totaltc[length(sim.env$totaltc)]
  total.net.profit <- total.profit - total.tc
  
  #print pca out of balance for last day of sim (using positions sitting in port.pos)
  pca_oob <- NULL
  pc_n <- ncol(sim.env$pca)
  for (i in 1:pc_n) {
    pca_oob[i] <- sum(port.pos * as.vector(sim.env$pca[,i]))
    pca_oob[abs(pca_oob)<100] <- 0
  }
  print("PCA out-of-balance for last day")
  print(pca_oob)
  
  
  print(paste("total profit:",total.profit))
  print(paste("total tc:",total.tc))
  print(paste("net profit:",total.net.profit))
  if (plot_profit) {
    plot(sim_date_index,sim.env$net.total.profit)
    points(sim_date_index,sim.env$net.day.profit,col="red")
  }
  return(total.net.profit)
}

#pass in mu, vlty, port_size
#pass in current portfolio, transaction cost per order (tc$oc), transaction cost per dollar (tc$bp)
#pass in pca array
#future: add in order vars (take into account order costs)
#        add in Principal components (sim.env$pca)
port_opt_lp <- function (lp.mu, lp.vlty, lp.port_size, lp.port, lp.pca, first_pass = FALSE) {
  if (first_pass) print(paste(length(lp.mu),length(lp.vlty),lp.port_size,length(lp.port),length(lp.pca),first_pass))

  if (sim.env$mean_adjust_mu) {
    mu_mean <- mean(lp.mu)
    lp.mu <- lp.mu - mu_mean
  }
  
  #set lp.constants  
  if (!exists("lp",envir=sim.env)) {
    lp <- NULL
    lp$stx <- length(lp.mu)
    if (lp$stx != length(lp.vlty)) print (paste("ERROR: mu and VLTY must have same number of stocks, mu_n:",lp$stx,"vlty_n:",length(lp.vlty)))
    lp$vlty_bounds <- c(0,30000,80000,210000)
    lp$vb_n <- length(lp$vlty_bounds)    #number of vlty bounds
    if (lp$vb_n < 2) {
      print("Must have more than 1 vlty bound")
      print(lp$vlty_bounds)
      source("close_session.R")
    }
    lp$pc_n <- ncol(lp.pca)              #number of principal components to maintain in balance
    lp$vbs <- lp$vb_n - 2                #number of vlty bound segments needing variables
    lp$max_pos <- lp$vlty_bounds[lp$vb_n]    #upper positions size constraint
    
    lp$pos_vars <- lp$stx                #position (only var allowed to go negative)
    lp$long_short_vars <- 2*lp$stx       #long, short (abs of position)
    lp$sb_ss_vars <- 2*lp$stx            #shares bought (sb), shares sold (ss) vars, one [0,1] order (O) var for each stock
    lp$vb_vars <- lp$vbs*lp$stx          #volatility bound segments (2..vb_n-1)  #first segment uses long/short vars
    lp$pop_pon_vars <- 2*lp$pc_n            #pca out of balance positive (pop), pca out of balance negative (pon)
    lp$order_vars <- ifelse(sim.env$opt_oc,lp$stx,0)
    
    # 1:lp.stx                                            # position_vars
    # (lp.stx+1):(3*lp.stx)                               # long_vars + short_vars
    # (3*lp.stx+1):(5*lp.stx)                             # shares_bought + shares_sold vars
    # (5*lp.stx+1):((5+lp.vbs)*lp.stx)                    # vb_vars (one for every lp.stx for each lp.vlty_bounds segment)
    # ((5+lp.vbs)*lp.stx+1):((5+lp.vbs)*lp.stx + 2*pc_n)  # pca out of balance (positive [pop] and negative [pon])
    # (((5+lp.vbs)*lp.stx + 2*pc_n)+1):((5+lp.vbs)*lp.stx + 2*pc_n)+order_Vars)  #integer order_vars
    
    lp$vars <- lp$pos_vars + lp$long_short_vars + lp$sb_ss_vars + lp$vb_vars + lp$pop_pon_vars + lp$order_vars
    lp$real.vars <- lp$pos_vars + lp$long_short_vars + lp$sb_ss_vars + lp$vb_vars + lp$pop_pon_vars
    
    lp$unique_cons <- 3                    #port_size (1), long_short balance (2)
    lp$long_short_cons <- lp$long_short_vars  #definition constraint for each long and short variable
    lp$sb_ss_cons <- lp$stx                #shares bought/sold constraints for each stock [port_pos+sb-ss=pos]
    lp$vb_cons <- 2*lp$vb_vars                #volatility cost for each variable defined long/short
    lp$pca_cons <- lp$pc_n                    #pca out of balance constraint
    lp$order_cons <- ifelse(sim.env$opt_oc,2*lp$stx,0)              #order constraints   [sb<O*2*max_pos,ss<O*2*max_pos]
    
    lp$cons <- lp$unique_cons + lp$long_short_cons + lp$sb_ss_cons + lp$vb_cons + lp$pca_cons + lp$order_cons
    
    sim.env$lp <- lp
  } else {
    lp <- sim.env$lp
  }  

  if (!exists("lp.port.model",envir=sim.env)) {
    print("Updating LP optimization model, lp.port.model (1 time only)")
    #lp.first_run <- FALSE
    lp.old_port_size <- (-1)
    
    #init lp.port.model
    lp.port.model <- make.lp(nrow=0,ncol=lp$vars)
    lp.control(lp.port.model,sense="max")

    set.type(lp.port.model, columns=1:lp$real.vars, type = "real")                                      #set all vars to real
    if (sim.env$opt_oc) set.type(lp.port.model, columns=(lp$real.vars+1):(lp$vars), type = "binary")    #Order vars to binary

    #all vars except pos vars must be positive
    set.bounds(lp.port.model,lower=c(rep(-lp$max_pos,lp$stx),rep(0,(lp$vars-lp$stx))),columns=(1:lp$vars))  
    #all position vars bounded by max_pos, shares bought/sold bounded by 2*max_pos
    set.bounds(lp.port.model,upper=c(rep(lp$max_pos,(3*lp$stx)),rep((2*lp$max_pos),(2*lp$stx))),columns=(1:(5*lp$stx)))
    #variability bound vars range from boundary point to max_pos
    for (vb_i in 1:lp$vbs) {
      max_vbs <- lp$max_pos - lp$vlty_bounds[vb_i+1]
      set.bounds(lp.port.model,upper=rep(max_vbs,lp$stx),columns=((5*lp$stx+(vb_i-1)*lp$stx+1):(5*lp$stx+vb_i*lp$stx)))
    }
    #pop_pon_vars, arbitrary upper bound
    set.bounds(lp.port.model,upper=rep((2*lp$max_pos),(2*lp$pc_n)),columns=(((5+lp$vbs)*lp$stx+1):((5+lp$vbs)*lp$stx+2*lp$pc_n)))
    if (sim.env$opt_oc) {  #bound binary order vars
      set.bounds(lp.port.model,lower=rep(0,lp$stx),columns=(lp$real.vars+1):lp$vars)
      set.bounds(lp.port.model,upper=rep(1,lp$stx),columns=(lp$real.vars+1):lp$vars)
    }

    add.constraint(lp.port.model,c(rep(0,lp$stx),rep(1,2*lp$stx),rep(0,(lp$vars-3*lp$stx))),"<=",lp.port_size)
    add.constraint(lp.port.model,c(rep(1,lp$stx),rep(0,(lp$vars-lp$stx))),"=",0)
    add.constraint(lp.port.model,c(rep(0,lp$stx),rep(1,lp$stx),rep(-1,lp$stx),rep(0,lp$vars-3*lp$stx)),"=",0)
    #L[1:lp.stx]
    for (var_i in 1:lp$stx) {
      add.constraint(lp.port.model,xt=c(1,-1),type="<=",rhs=0,indices=c(var_i,lp$stx+var_i))
    }
    #S[lp.stx]
    for (var_i in 1:lp$stx) {
      add.constraint(lp.port.model,xt=c(1,1),type=">=",rhs=0,indices=c(var_i,2*lp$stx+var_i))
    }
    #buy long, sell short constraints; 
    #new_Pos - shares_bought + shares_sold = current_pos
    for (var_i in 1:lp$stx) {
      add.constraint(lp.port.model,xt=c(1,-1,1),type="=",rhs=lp.port[var_i],indices=c(var_i,3*lp$stx+var_i,4*lp$stx+var_i))
    }
    #VB[1:lp.vbs]L[1:lp.stx]; long_shares - vbi_var <= vb_bounds
    #VB[1:lp.vbs]S[1:lp.stx]; short_shares - vbi_var <= vb_bounds
    for (vb_i in 1:lp$vbs) {
      for (var_i in 1:lp$stx) {                                                              #long var
        add.constraint(lp.port.model,xt=c(1,-1),type="<=",rhs=lp$vlty_bounds[vb_i+1],
                       indices=c((lp$stx+var_i),((5+vb_i-1)*lp$stx+var_i)))
      }
      for (var_i in 1:lp$stx) {                                                              #short var
        add.constraint(lp.port.model,xt=c(1,-1),type="<=",rhs=lp$vlty_bounds[vb_i+1],
                       indices=c((2*lp$stx+var_i),((5+vb_i-1)*lp$stx+var_i)))
      }
    }
    #PCA balance constraint
    #for each pc_i in pca vector
    #(pos1*pca(pc_i,1) + pos2*pca(pc_i,2) + ... posn*pca(pc_i,n)) - pop(pc_i) + pon(pc_i) = 0
    for (pc_i in 1:lp$pc_n) {
      add.constraint(lp.port.model,xt=c(as.vector(lp.pca[,pc_i]),-1,1),type="=",rhs=0,
                     indices=c(1:lp$stx,(5+lp$vbs)*lp$stx+2*(pc_i-1)+1,(5+lp$vbs)*lp$stx+2*(pc_i-1)+2))
    }
    if (sim.env$opt_oc) {
      #shares_bought - order[0,1] * (2*max_pos) <= 0
      #shares_sold - order[0,1] * (2*max_pos) <= 0
      for (var_i in 1:lp$stx) {
        add.constraint(lp.port.model,xt=c(1,-2*lp$max_pos),type="<=",rhs=0,indices=c(3*lp$stx+var_i,lp$real.vars+var_i))
        add.constraint(lp.port.model,xt=c(1,-2*lp$max_pos),type="<=",rhs=0,indices=c(4*lp$stx+var_i,lp$real.vars+var_i))
      }
    }
    #print("end adding constraints")
    
    #col chars = 6 var types + volatility bounds
    #Each char represents lp.stx vars
    nums <- 1:lp$stx
    col_chars <- c("P","L","S","SB","SS")
    for (vb_i in 1:lp$vbs) {
      col_chars <- c(col_chars,paste0("VB",vb_i+1,"_"))
    }
    var_names <- NULL
    for (char in 1:length(col_chars)) {
      var_names <- c(var_names,paste0(col_chars[char],nums))
    }
    #Add in a POP and PON var for each principal component index
    for (pc_i in 1:lp$pc_n) {
      var_names <- c(var_names,paste0("POP",pc_i),paste0("PON",pc_i))
    }
    #Add in Order var names (if necessary)
    if (sim.env$opt_oc) {
      var_names <- c(var_names,paste0("O",nums))
    }
    
    #name first three constraints
    con_names <- c("port_size","ls_balance1","ls_balance2")
    #name long/short var constraints
    con_chars <- c("L","S")
    for (char in 1:length(con_chars)) {
      con_names <- c(con_names,paste0(con_chars[char],nums))
    } 
    #name constraints for BUY-SELL constraint (BS), BUY-Order constraint (BO), Sell-Order Constraint (SO)
    for (ci in 1:lp$stx) {
      con_names <- c(con_names,paste0("BS",ci))
    }
    #name constraints for volatility bounds
    con_chars <- NULL
    for (vb_i in 1:lp$vbs) {
      con_chars <- c(con_chars,paste0("VB",vb_i+1,"L"),paste0("VB",vb_i+1,"S"))
    }
    for (char in 1:length(con_chars)) {
      con_names <- c(con_names,paste0(con_chars[char],nums))
    } 
    #name principal component constraints
    for (pc_i in 1:lp$pc_n) {
      con_names <- c(con_names,paste0("PC",pc_i))
    }
    #name constraints for BUY-Order constraint (BO), Sell-Order Constraint (SO)
    if (sim.env$opt_oc) {
      for (ci in 1:lp$stx) {
        con_names <- c(con_names,paste0("BO",ci),paste0("SO",ci))
      }
    }
    dimnames(lp.port.model) <- list(con_names,var_names)
  } else {
    lp.port.model <- sim.env$lp.port.model
  }
  
  #in case optimization wts change, set every time  
  lp$alpha_wt <- sim.env$alpha_wt
  lp$pca_mult <- sim.env$pca_mult
  lp$pca_wt <- sim.env$pca_wt
  lp$vlty_wt <- sim.env$vlty_wt     #rep(-1.,vb_n-1)
  lp$tc <- sim.env$tc  
  if (sim.env$scale_bp != 1) lp$tc$bp <- sim.env$scale_bp*lp$tc$bp  #scale it with respect to mu
  
  #update buy_stocks, sell_stocks constraint with new port position
  for (var_i in 1:lp$stx) {
    set.constr.value(lp.port.model,rhs=lp.port[var_i],constraints=(lp$unique_cons + lp$long_short_cons + var_i))
  }
  
  #set objective function, needed for every new mu, VLTY
  lp.obj.fun <- rep(0,lp$vars)
  #mu component
  lp.obj.fun[1:lp$stx] <- lp$alpha_wt*lp.mu
  #first vlty segment
  lp.obj.fun[(lp$stx+1):(2*lp$stx)]   <- -lp$vlty_wt*lp$vlty_bounds[2]*lp.vlty
  lp.obj.fun[(2*lp$stx+1):(3*lp$stx)] <- lp.obj.fun[(lp$stx+1):(2*lp$stx)]
  lp.obj.fun[(3*lp$stx+1):(5*lp$stx)] <- rep(-lp$tc$bp,2*lp$stx)  #basis point costs per dollar ordered, SB/SS vars
  if (lp$vbs > 0) {
    #subsequent vlty segments
    for (vb_i in 1:lp$vbs) { #piecwise linear component for segment (portion added to all previous segments)
      lp.obj.fun[(5*lp$stx+(vb_i-1)*lp$stx+1):(5*lp$stx+vb_i*lp$stx)] <- -lp$vlty_wt*lp$vlty_bounds[vb_i+2]*lp.vlty
    }
  }
  start_idx <- (5+lp$vbs)*lp$stx
  # if (first_pass) {
  #   print(paste("setting pca wts in obj function:",start_idx,lp$pc_n,lp$pca_mult))
  #   print(lp$pca_wt)
  # }
  for (i in 1:lp$pc_n) {
    #if (first_pass) print(paste("idx:",start_idx+(i-1)*2+2))
    lp.obj.fun[(start_idx+(i-1)*2+1):(start_idx+(i-1)*2+2)] <- (-lp$pca_mult*lp$pca_wt[i])    #out of balance pca vector penalty
  }
  if (sim.env$opt_oc) {  #else order costs not part of objective function
    lp.obj.fun[(lp$real.vars+1):lp$vars] <- rep(-lp$tc$oc,lp$stx)    #order cost per order, O vars
  }
  set.objfn(lp.port.model,lp.obj.fun)

  # #set port_size constraint, needed for every new port_size
  # if (lp.port_size != lp.port_size) {
  #   #add.constraint(port.model,c(rep(0,lp.stx),rep(1,2*lp.stx),rep(0,(lp.vars-3*lp.stx))),"=",lp.port_size)
  #   set.rhs(lp.port.model,b=lp.port_size,constraints=1) #can I use the constraint name = "port_size" here?
  # }
  
  if (first_pass) {
    print("Writing out lp to file 'test_lp'")
    lp_error_file <- paste(com.env$logfile,"test_lp",sep="/")
    write.lp(lp.port.model,filename=lp_error_file,type="lp")
  }
  
  lp.control(lprec=lp.port.model,verbose="important",presolve=c("rows","cols","lindep","bounds"),scaling=c("none","equilibrate","intergers"),timeout=5)
  #lp.control(lprec=lp.port.model,verbose="important",presolve=c("rows","cols","lindep","bounds"),timeout=5)
  #print(get.bounds(lp.port.model,columns=1))
  v1.bounds <- get.bounds(lp.port.model,columns=1)
  if (v1.bounds$lower != -lp$max_pos) {  #bug in code, this is a workaround (should find bug and delete this code)
    print(paste("lower bounds on var1:",v1.bounds$lower))
    set.bounds(lp.port.model,lower=-lp$max_pos,upper=lp$max_pos,columns=1)
  }
  t <- system.time({screen_out <- capture.output(lp.status <- solve(lp.port.model))})
  if (lp.status != 0) {  #Difficulty solving LP, remove trading costs from objective function
    print(paste("LP Model status code on try 1:",solve(lp.port.model)))
    # print("Writing out lp to file 'error_lp_1'")
    # lp_error_file <- paste(com.env$logfile,"error_lp_1",sep="/")
    # write.lp(lp.port.model,filename=lp_error_file,type="lp")
    print("Retrying after removing trading costs from objective function")
    #set objective function, needed for every new mu, VLTY
    lp.obj.fun <- rep(0,lp$vars)
    #mu component
    lp.obj.fun[1:lp$stx] <- lp$alpha_wt*lp.mu
    #first vlty segment
    lp.obj.fun[(lp$stx+1):(2*lp$stx)]   <- -lp$vlty_wt*lp$vlty_bounds[2]*lp.vlty
    lp.obj.fun[(2*lp$stx+1):(3*lp$stx)] <- lp.obj.fun[(lp$stx+1):(2*lp$stx)]
    #lp.obj.fun[(3*lp$stx+1):(5*lp$stx)] <- rep(-lp$tc$bp,2*lp$stx)  #basis point costs per dollar ordered, SB/SS vars
    if (lp$vbs > 0) {
      #subsequent vlty segments
      for (vb_i in 1:lp$vbs) { #piecwise linear component for segment (portion added to all previous segments)
        lp.obj.fun[(5*lp$stx+(vb_i-1)*lp$stx+1):(5*lp$stx+vb_i*lp$stx)] <- -lp$vlty_wt*lp$vlty_bounds[vb_i+2]*lp.vlty
      }
    }
    start_idx <- (5+lp$vbs)*lp$stx
    for (i in 1:lp$pc_n) {
      lp.obj.fun[(start_idx+(i-1)*2+1):(start_idx+(i-1)*2+2)] <- (-lp$pca_mult*lp$pca_wt[i])    #out of balance pca vector penalty
    }
    #if (sim.env$opt_oc) {  #else order costs not part of objective function
    #  lp.obj.fun[(lp$real.vars+1):lp$vars] <- rep(-lp$tc$oc,lp$stx)    #order cost per order, O vars
    #}
    set.objfn(lp.port.model,lp.obj.fun)
  
    lp.status <- solve(lp.port.model)
    if (lp.status != 0) {
      print(paste("LP Model status code try 2:",solve(lp.port.model)))
      print("Writing out lp to file 'error_lp_2'")
      lp_error_file <- paste(com.env$logfile,"error_lp_2",sep="/")
      #write.lp(lp.port.model,filename=lp_error_file,type="lp")
      
      print("Removing piecewise linear components, retrying")
      #set objective function, needed for every new mu, VLTY
      lp.obj.fun <- rep(0,lp$vars)
      #mu component
      lp.obj.fun[1:lp$stx] <- lp$alpha_wt*lp.mu
      #pca component
      start_idx <- (5+lp$vbs)*lp$stx
      for (i in 1:lp$pc_n) {
        lp.obj.fun[(start_idx+(i-1)*2+1):(start_idx+(i-1)*2+2)] <- (-lp$pca_mult*lp$pca_wt[i])    #out of balance pca vector penalty
      }
      set.objfn(lp.port.model,lp.obj.fun)
      lp.status <- solve(lp.port.model)
      if (lp.status != 0) {
        print("Unable to solve LP, writing lp to error_lp_3")
        lp_error_file <- paste(com.env$logfile,"error_lp_3",sep="/")
        write.lp(lp.port.model,filename=lp_error_file,type="lp")
        stop()        
      }
      
    }
  }
  
  lp.positions <- get.variables(lp.port.model)[1:lp$stx]
  lp.positions[abs(lp.positions)<100] <- 0

  if(first_pass) {
    pos_sqr <- lp.positions*lp.positions
  
    calc_mu <- lp.positions*lp.mu
    calc_vlty <- pos_sqr*lp.vlty*(lp$vlty_wt)
    
    order <- 0
    shares_BS <- 0
    for (i in 1:lp$stx) {
      if (lp.port[i] != lp.positions[i]) {
        order <- order + 1
        shares_BS <- shares_BS + abs(lp.port[i]-lp.positions[i])
      }
    }
    order_costs <- order*lp$tc$oc
    bp_costs <- shares_BS*lp$tc$bp
    
    pca_oob <- NULL
    for (i in 1:lp$pc_n) {
      pca_oob[i] <- sum(lp.positions * as.vector(lp.pca[,i]))
      pca_oob[abs(pca_oob)<100] <- 0
    }
    etf_oob <- NULL
    for (i in 1:ncol(sim.env$pca_etf)) {
      etf_oob[i] <- sum(lp.positions * as.vector(sim.env$pca_etf[,i]))
      etf_oob[abs(etf_oob)<100] <- 0
    }
  
    total_mu <- sum(calc_mu)
    total_vlty <- sum(calc_vlty)

    print("positions")  
    print(lp.positions)
    print("Mu")
    print(calc_mu)
    print("Vlty")
    print(calc_vlty)
    print(paste("order_costs:",order_costs," bp_costs:",bp_costs))
    print("PCA out of balance:")
    print(pca_oob)
    print("ETF out of balance:")
    print(etf_oob)
  
    print(paste("total mu=",total_mu,"total vlty=",total_vlty))
    print(total_mu-total_vlty)
    print(get.objective(lp.port.model))
  }  
  sim.env$lp.port.model <- lp.port.model
  return(lp.positions)
}  