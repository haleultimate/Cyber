#####################################
# Automated model selection
# Author      : Joris Meys
# version     : 0.2
# date        : 12/01/09
#####################################
#CHANGE LOG
# 0.2   : check for empty scopevar vector
#####################################

# Function has.interaction checks whether x is part of a term in terms
# terms is a vector with names of terms from a model
has.interaction <- function(x,terms){
  out <- sapply(terms,function(i){
    sum(1-(strsplit(x,":")[[1]] %in% strsplit(i,":")[[1]]))==0
  })
  return(sum(out)>0)
}

# Function Model.select
# model is the lm object of the full model
# keep is a list of model terms to keep in the model at all times
# sig gives the significance for removal of a variable. Can be 0.1 too (see SPSS)
# verbose=T gives the F-tests, dropped var and resulting model after 
model.select <- function(model,keep,sig=0.05,verbose=F){
  counter=1
  # check input
  if(!is(model,"lm")) stop(paste(deparse(substitute(model)),"is not an lm object\n"))
  # calculate scope for drop1 function
  terms <- attr(model$terms,"term.labels")
  if(missing(keep)){ # set scopevars to all terms
    scopevars <- terms
  } else{            # select the scopevars if keep is used
    index <- match(keep,terms)
    # check if all is specified correctly
    if(sum(is.na(index))>0){
      novar <- keep[is.na(index)]
      warning(paste(
        c(novar,"cannot be found in the model",
          "\nThese terms are ignored in the model selection."),
        collapse=" "))
      index <- as.vector(na.omit(index))
    }
    scopevars <- terms[-index]
  }
  
  # Backward model selection : 
  
  while(T){
    # extract the test statistics from drop.
    test <- drop1(model, scope=scopevars,test="F")
    
    if(verbose){
      cat("-------------STEP ",counter,"-------------\n",
          "The drop statistics : \n")
      print(test)
    }
    
    pval <- test[,dim(test)[2]]
    
    names(pval) <- rownames(test)
    pval <- sort(pval,decreasing=T)
    
    if(sum(is.na(pval))>0) stop(paste("Model",
                                      deparse(substitute(model)),"is invalid. Check if all coefficients are estimated."))
    
    # check if all significant
    if(pval[1]<sig) break # stops the loop if all remaining vars are sign.
    
    # select var to drop
    i=1
    while(T){
      dropvar <- names(pval)[i]
      check.terms <- terms[-match(dropvar,terms)]
      if (length(check.terms)==0) {
        #print("**********************error in model_selection*************************")
        #print(dropvar)
        #print(check.terms)
        #print(names(pval))
        #print(terms)
        break
      }
      x <- has.interaction(dropvar,check.terms)
      if(x){i=i+1;next} else {break}              
    } # end while(T) drop var
    
    if(pval[i]<sig) break # stops the loop if var to remove is significant
    
    if(verbose){
      cat("\n--------\nTerm dropped in step",counter,":",dropvar,"\n--------\n\n")              
    }
    
    #update terms, scopevars and model
    scopevars <- scopevars[-match(dropvar,scopevars)]
    terms <- terms[-match(dropvar,terms)]
    
    formul <- as.formula(paste(".~.-",dropvar))
    model <- update(model,formul)
    
    if(length(scopevars)==0) {
      warning("All variables are thrown out of the model.\n",
              "No model could be specified.")
      return()
    }
    counter=counter+1
  } # end while(T) main loop
  return(model)
}

#out of sample r2 calculation, returns test (contains $rsq,$cor)
calc.r2 <- function(model,data.df) {
  data.df[is.na(data.df)] <- 0
  predicted.model <- predict.lm(model,newdata=data.df)
  test.y <- data.df[,com.env$predict.clu]
  mean.test.y <- mean(test.y)
  SS.total      <- sum((test.y - mean.test.y)^2)
  SS.residual   <- sum((test.y - predicted.model)^2)
  #SS.regression <- sum((predicted.model - mean(test.y))^2)
  #SS.total - (SS.regression+SS.residual)
  test <- NULL
  test$rsq <- 1 - (SS.residual/SS.total)  
  SS.total0 <- sum(test.y^2)
  test$rsq0 <- 1 - (SS.residual/SS.total0)
  results <- cbind(predicted.model,data.df[,com.env$predict.clu])
  test$cor <- cor(results,use="complete.obs")[1,2]
  test$mse <- sqrt(mean((test.y-predicted.model)^2))
  test$mean <- mean(abs(test.y))
  cnt <- 0
  #cnt <- length(predicted.model)
  wincnt <- 0
  for (i in 1:length(predicted.model)) {
    if ((sign(test.y[i]) == sign(predicted.model[i])) & 
       ((test.y[i]!=0) & (predicted.model[i]!=0))) wincnt <- wincnt + 1
    if ((test.y[i] != 0) & (predicted.model[i]!=0)) cnt <- cnt + 1
  }
  test$winpct <- wincnt/cnt
  #print(test)
  return(test)
}

#VIF function removes colinear variables from data frame
#From: https://beckmw.wordpress.com/2013/02/05/collinearity-and-stepwise-vif-selection/

#for debugging...
#in_frame <- stx.data[,2:ncol(stx.data)]
#keep <- com.env$best_reg_vars [added by RHB] [variables to keep in model]
#thresh <- 10
#trace <- T

#vif_func<-function(in_frame,thresh=10,trace=T,...){  #... removed by RHB
vif_func<-function(in_frame,keep=NULL,thresh=10,trace=T){
  # if (trace==T) {
  #   cat('In vif_func, trying to determine which vars are colinear and need to be removed\n')
  #   if (!is.null(keep)) {
  #     cat('Cannot remove:',keep,'\n')
  #     cat('Able to remove:',names(in_frame)[!names(in_frame) %in% keep],'\n')
  #     if (length(keep[!keep %in% names(in_frame)])>0) {
  #       print("ERROR: Names in keep not in in_frame")
  #       print(keep[!keep %in% names(in_frame)])
  #     }
  #   } else {
  #     cat('keep == NULL\n')
  #   }
  # }
  #require(fmsb)
  if(class(in_frame) != 'data.frame') in_frame<-data.frame(in_frame)
  problem_vals_last_time <-FALSE
  removed_vals <- NULL         #vars to remove from in_frame
  vif_calcs <- NULL            #list containing vif values for all var_names
  check_vals <- names(in_frame)
  vif_max <- thresh+1          #forces loop 
  while (vif_max > thresh) {   #compute VIF for each var; construct vif_calcs list
    # if(trace==T) {
    #   cat("removed_vals:",removed_vals,'\n')
    #   cat("check_vals:",check_vals,'\n')
    # }
    in_dat <- in_frame[,!(names(in_frame) %in% removed_vals)]
    var_names <- names(in_dat)
    #vif_vals <- NULL             #array containing var_names, vif_calcs (vif_vals[,1],vif_vals[,2])
    vif_calcs <- vif_calcs[!names(vif_calcs) %in% removed_vals]
    for (val in check_vals) {
      regressors <- var_names[-which(var_names == val)]
      form <- paste(regressors, collapse = '+')
      form_in <- formula(paste(val, '~', form))
      if (val %in% names(vif_calcs)) {
        vif_calcs[val] <- VIF(lm(form_in,data=in_dat))
      } else {
        vif_calcs <- c(vif_calcs,VIF(lm(form_in,data=in_dat)))
        names(vif_calcs)[length(vif_calcs)] <- val               
      }
    }
    problem_vals <- names(vif_calcs[vif_calcs %in% c("NaN","Inf")]) 
    if (length(problem_vals)>0) {                     
      # if(trace==T) {
      #   cat("problem_vals:",problem_vals,'\n')
      #   print(vif_calcs)
      # }
      if (problem_vals_last_time) {
        print("ERROR in vif_func: problem with data in keep list")
        print(keep)
        print(problem_vals)
        print(vif_calcs[vif_calcs %in% c("NaN","Inf")])
        print(var_names)
        return(vif_func(in_frame,keep=NULL,thresh=10,trace=T))  #allow keep vars to be removed
        #source("close_session.R")
      }
      problem_vals_last_time <- TRUE
      removed_vals <- c(removed_vals,problem_vals[!problem_vals %in% keep])
      var_names <- var_names[!var_names %in% removed_vals]
      check_vals <- names(in_frame[,!(names(in_frame) %in% removed_vals)])
      vif_init <- NULL
      next
    } else {
      problem_vals_last_time <- FALSE
    }
    sort_vif_calcs <- sort(vif_calcs,decreasing=TRUE)
    #if(trace==T) print(sort_vif_calcs)
    vif_max <- sort_vif_calcs[1]
    if (vif_max <= thresh) break
    check_vals <- NULL
    starting_removed_count <- length(removed_vals)
    for (vif_max_name in names(sort_vif_calcs)) {
      if ((!vif_max_name %in% keep) & (length(removed_vals)==starting_removed_count)) {
        removed_vals <- c(removed_vals,vif_max_name)
      } else {
        if (vif_calcs[vif_max_name] > thresh) check_vals <- c(check_vals,vif_max_name)
      }
      if (vif_calcs[vif_max_name] <= thresh) break
    }
    if (length(removed_vals) == starting_removed_count) {
      print("Can't keep vars requested AND get vif < thresh, keep vars:")
      print(keep)
      cat('thresh:',thresh,'max_vif:',sort_vif_calcs[1],'\n')
      print(sort_vif_calcs)
      cat('Calling vif_func without trying to keep com.env$best_clu_names, resetting best_vars\n')
      com.env$best_adj_r2 <- 0
      com.env$best_clu_names <- NULL
      com.env$best_vcom <- NULL
      return(vif_func(in_frame,keep=NULL,thresh=5,trace=T))  #reduce threshold to make model less colinear
    }
  }
  # DEBUGGING
  if (trace==T) {
    if (any(var_names %in% removed_vals)) {
      print("WARNING: In VIF function, data remains in_dat after being removed")
      cat('removed_vals:',removed_vals,'\n')
      cat('names(id_dat):',names(in_dat),'\n')
    }
    if (any(keep %in% removed_vals)) {
      print("WARNING: In VIF function, removing vals asked to be kept")
      cat('removed_vals:',removed_vals,'\n')
      cat('keep:',keep,'\n')
    }
    print(sort_vif_calcs)
    cat("removing names from reg_data.df:",removed_vals,'\n')
  }
  return(var_names)
}

#function determines max(OOS r2) by setting a single reg var's coefficient to 0 (does not rerun regression)
#returns reg_var name ("none" if no increase can be found)
drop_var_oos <- function(model,data.df) {
  all_var_r2 <- calc.r2(model,data.df)
  var_r2 <- all_var_r2$rsq
  save_coefs <- model$coefficients
  for (i in 2:length(save_coefs)) {
    new_coefs <- save_coefs
    new_coefs[i] <- 0.
    model$coefficients <- new_coefs
    i_var_r2 <- calc.r2(model,data.df)
    var_r2 <- c(var_r2,i_var_r2$rsq)
  }
  names(var_r2) <- names(save_coefs)
  names(var_r2)[1] <- "none"
  sort_var_r2 <- sort(var_r2,decreasing=TRUE)
  #print(sort_var_r2)
  #cat("Var to drop:",names(sort_var_r2)[1],"\n")
  return(names(sort_var_r2[1]))
}

#returns lm model for set of reg_vars and data.df
get_new_reg_model <- function(reg_vars,data.df) {
  #print("In get_new_reg_model")
  #print(colnames(data.df))
  #print(reg_vars)
  if (length(reg_vars)==0) return(NULL)
  f <- as.formula(paste(colnames(data.df)[1],paste(reg_vars,collapse=" + "),sep=" ~ "))
  #print(f)
  new_model <- lm(formula=f,data=data.df)
  return(new_model)
}

opt_var_selection <- function() {
  switch(com.env$opt_type,
         "adjr2_is" = {
           return()
         },
         "single_oos" = {
           opt_oos_r2(recalc_vars=FALSE,recollect_data=TRUE)
         },
         "rolling_oos" = {
           opt_rolling_oos(recalc_vars=FALSE)
         },
         {cat("Error: com.env$opt_type - ",com.env$opt_type," not supported\n")
           source("close_session.R")}
         )
}

#manipulate com.env$reg_end_date & com.env$oos_date_range so that collect data returns the proper data in var.env$reg_data.df & var.env$oos_data.df 
#average prediction R2 over all oos periods
#if better than best saved model update rolling best vars, otherwise revert best vars to previous best rolling vars
opt_rolling_oos <- function(recalc_vars=FALSE) {
  print("In opt_rolling_oos")
  check_vcom(com.env$v.com,"In opt_rolling_oos")
  if (recalc_vars) {  #pass in FALSE if data already calculated in var.env
    #clean_var_env()  #remove all vars not in current vcom
    calc_all_vars()  #eval_adj_r2 normally by calculating all vars in com.env$v.com
  }
  #collect data, get regression model, evaluate r2
  #if (com.env$retvlty_not_calced) {
    #adjret_calc()
    #vlty_calc()
    stk_matrix("data.env","ADJRET")
    stk_matrix("data.env","VLTY")
    #com.env$retvlty_not_calced <- FALSE
  #}
  sum_r2 <- 0
  sum_profit <- 0
  for (i in 1:com.env$rolling_periods) {
    #calc_regression oos r2
    com.env$reg_end_date <- as.POSIXct(com.env$rolling_start_date + (i-1)*com.env$period - 1)
    com.env$oos_date_range <- com.env$oos_date_index[[i]]
    com.env$start_oos <- com.env$oos_start_date[[i]]
    reg_names <- c(com.env$predict.clu,com.env$best_clu_names)
    #populate com.env$oos_stx for use in sim
    if (i < com.env$rolling_periods) {
      collect_data(oos_data=TRUE,sim_data=FALSE,reg_names = reg_names)  #populate var.env(reg_data.df,oos_data.df) with model vars
    } else {
      collect_data(oos_data=TRUE,sim_data=TRUE,reg_names = reg_names)   #populate var.env(reg_data.df,oos_data.df,sim_data.df) with model vars
    }
    com.env$model.current <- get_new_reg_model(com.env$best_clu_names,var.env$reg_data.df)
    print(nrow(var.env$oos_data.df))
    oos_r2 <- calc.r2(com.env$model.current,var.env$oos_data.df)
    cat("Period:",i," r2=",oos_r2$rsq," cor=",oos_r2$cor," winpct=",oos_r2$winpct,"\n")
    sum_r2 <- sum_r2 + oos_r2$rsq
    #calc_sim profit
    mu_calc(paste0("mu",i))
    stx_oos <- com.env$stx.symbols[com.env$start_date<com.env$start_oos]
    print(paste("stx count for sim:",length(stx_oos)))
    sum_profit <- sum_profit + lp_sim(paste0("mu",i),stx_oos,com.env$oos_date_range,sim.env$port_size_mult*length(stx_oos),plot_profit=FALSE)
  }
  ave_r2 <- sum_r2/com.env$rolling_periods
  ave_profit <- sum_profit/(com.env$rolling_periods*sim.env$port_size)
  if (com.env$r2_wt*ave_r2 + ave_profit > com.env$rolling_best_score) {
    cat("Model improved!!!  Updating rolling best_vars, OOS_R2:",ave_r2,"profit:",ave_profit,"\n")
    com.env$rolling_best_score <- com.env$r2_wt*ave_r2 + ave_profit
    com.env$rolling_adj_r2 <- com.env$best_adj_r2
    com.env$rolling_clu_names <- com.env$best_clu_names
    com.env$rolling_vcom <- com.env$best_vcom
    sim_r2 <- calc.r2(com.env$model.current,var.env$sim_data.df)
    cat("r2",sim_r2$rsq,"r20",sim_r2$rsq0,"cor",sim_r2$cor,"\n")
    cat("mse",sim_r2$mse,"mean",sim_r2$mean,"wpct",sim_r2$winpct,"\n")
  } else {
    cat("Model got worse... Reverting to previous best rolling model,",ave_r2,"+",ave_profit,"<",com.env$rolling_best_score,"\n")
    com.env$best_adj_r2 <- com.env$rolling_adj_r2
    com.env$best_clu_names <- com.env$rolling_clu_names
    com.env$best_vcom <- com.env$rolling_vcom
  }
}

#function optimizes oos_r2 by zeroing one coef at a time (until no further zeroing improves oos_r2)
#(best_adj_r2, best_clu_names, best_vcom) are updated
opt_oos_r2 <- function(recalc_vars=FALSE,recollect_data=FALSE) {
  if (recalc_vars) {  #pass in FALSE if data already calculated in var.env
    #clean_var_env()  #remove all vars not in current vcom
    calc_all_vars()  #eval_adj_r2 normally by calculating all vars in com.env$v.com
  }
  if (recollect_data) collect_data(oos_data=TRUE,sim_data=TRUE)  #populate var.env(reg_data.df,oos_data.df,sim_data.df) with model vars
  reg_vars <- com.env$best_clu_names
  drop_var <- "any"
  loop <- 0
  while ((drop_var != "none") | is.null(reg_vars) | (length(reg_vars) == 0)) {
    loop <- loop + 1
    if (loop > 100) break
    best_model <- get_new_reg_model(reg_vars,var.env$reg_data.df)
    if (!is.null(best_model)) {
      drop_var <- drop_var_oos(best_model,var.env$oos_data.df)
      reg_vars <- reg_vars[!reg_vars %in% drop_var]
    }
  }
  #update best vars
  if ((length(best_model) == 0) | (is.null(reg_vars)) | (length(reg_vars) == 0)) {
    print("All variables left model")
    com.env$best_adj_r2 <- 0
    com.env$best_clu_names <- NULL
    com.env$best_vcom <- NULL
  } else {
    #check sim data stats
    #sim_r2 <- calc.r2(best_model,var.env$sim_data.df)
    #cat("r2",sim_r2$rsq,"r20",sim_r2$rsq0,"cor",sim_r2$cor,"\n")
    #cat("mse",sim_r2$mse,"mean",sim_r2$mean,"wpct",sim_r2$winpct,"\n")
    #update best_vars
    com.env$best_adj_r2 <- summary(best_model)$adj.r.squared
    com.env$best_clu_names <- reg_vars
    clean_vcom()
    com.env$best_vcom <- com.env$v.com 
  }
}


