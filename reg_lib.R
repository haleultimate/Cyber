#run_regression.R

#run_prediction is main program for reg_lib.R
#com parms set in init_lib.R
#primary functions: opt_model -> calc_adj_r2 -> run_regression
#com.env$v.com contains all variable definitions
#var.env contains all calculated variables (by stock & reg_data.df)
#make_lib.R, rnd_lib.R controls variable creation process
run_prediction <- function() {
  if (!com.env$load_model) define_predict_ret()     #source("define_vars.R")  #if not loading model need to define predictor variable
  sample_vars()                                     #set up rnd.env$vs.com and rnd_parms
  if (com.env$load_model) load_model(com.env$load_model_name)
  if (com.env$opt_model) opt_model(com.env$model_loops,com.env$mod_var_loops)
  if (com.env$save_model) save_model(com.env$save_model_name)
  if (com.env$save_var_n > 0) {
    print(paste("Evaluating model to save top",com.env$save_var_n,"/",length(com.env$best_clu_names),"total vars",Sys.time()))
    eval_adj_r2(sim_data=TRUE)
    save_vars(com.env$save_var_n) #get top vars
  }
}

#optimize regression model based on adj_r2 to best calculate com.env$predict.ret [in-sample]
#Each loop try two things:
#(1) Add vars to com.env$v.com (variable definitions to base all calculations on)
#    Calculate all vars in var.env, Run stepwise regression
#    Creates com.env$best_clu_names giving list of regression vars (and corresponding com.env$best_adj_r2)
#(2) Modify vars in com.env$v.com
#    Change random definition slightly, recalculate that one var, Run single regression (using com.env$best_clu_names from step 1)
#    Repeat until first improvement is found
#Clean com.env$v.com (remove all non-significant variable definitions), delete var.env, repeat loop
#FINAL RESULT: com.env$v.com with variable definitions (how to calculate out-of-sample variables)
#Rerunning eval_adj_r2(oos=TRUE) with this com.env$v.com provides: 
#  var.env$ret_data.df containing calculations from final model (in-sample and out-of-sample)
#  com.env$stepwise.model containing coefficients for producing expected returns given out-of-sample variables
opt_model <- function(model_loops,mod_var_loops) {    
  #source("define_vars.R")                         #define predictor var, setup initial v.com
  check_vcom(com.env$v.com,"start of opt_model")
  print(paste("In opt_model, model_loops:",model_loops,"mod_var_loops:",mod_var_loops))
  com.env$best_adj_r2 <- 0
  com.env$best_clu_names <- NULL
  com.env$best_vcom <- NULL
  #com.env$var_list <- NULL                    #list of math definitions 
  com.env$ID_tried <- NULL                    #don't repeat model variables
  com.env$add_var_worse <- FALSE
  start_time <- Sys.time()
  for (current_add_loop in 1:model_loops) {              #start model loop
    #add vars
    print(paste("model loop:",current_add_loop,"/",model_loops,"best_adj_r2:",com.env$best_adj_r2,Sys.time()))
    add_vars <- get_add_vars()
    if (add_vars>0) for (i in 1:add_vars) make_new_model_var()
    #run regression, if model improved update best vars; if model got worse revert and cycle add_var loop
    if (!try_mods(eval_adj_r2(),current_add_loop,mod_var_loops)) next    

    #mod vars
    #cat("Entering mod_vars with",length(com.env$best_clu_names),"vars, best_adj_r2=",com.env$best_adj_r2,'\n')
    for (current_mod_loop in 1:mod_var_loops) {       #end of loop controlled by finish_mod_loop below
      #run regression, if model improved optimize, update best vars and end loop; if model got worse revert and continue
      if (finish_mod_loop(mod_var("model"))) break  #try a random model var mod
    }
    print(paste("Mod Loops attempted:",current_mod_loop,"/",mod_var_loops,",",length(com.env$best_clu_names),com.env$best_adj_r2,Sys.time()))
    if (any(grepl("\\.",colnames(var.env$BAC)))) {
      print(colnames(var.env$BAC))
      print("after mod_loop")
      source("close_session.R")
    }
    
    opt_var_selection()

    clean_var_env()  #remove all vars not in current vcom
    check_vcom(com.env$v.com,"end of mod loop")
    print(paste("Mod Loops attempted:",current_mod_loop,"/",mod_var_loops,",",length(com.env$best_clu_names),com.env$best_adj_r2,Sys.time()))
    if (any(grepl("\\.",colnames(var.env$BAC)))) {
      print(colnames(var.env$BAC))
      print("after opt_var_selection / check_vcom")
      source("close_session.R")
    }
    load(file=paste0(com.env$datadir,"/stop_opt_loop.dat"),envir=com.env)
    if (com.env$stop_opt_loop) {
      print("Break in optimization loop via stop_opt_loop file")
      break
    }
  }                                               #end model loop
  print(paste("Start_time:",start_time,"End_time:",Sys.time()))
  cat("Final adj R2:",com.env$best_adj_r2,'\n')
  cat("Reg vars:",length(com.env$best_clu_names),com.env$best_clu_names,'\n')
} #end function opt_model

check_vcom <- function(vcom,print_str) {
  if (is.null(vcom)) return
  print(print_str)
  for (vd in vcom) {
    if ((vd$use != "def") & (is.null(vd$clu))) {
      print("vd missing clu")
      print(vd)
      source("close_session.R")
    }
  }
  if (any(grepl("\\.",colnames(var.env$BAC)))) {
    print(colnames(var.env$BAC))
    source("close_session.R")
  }
}

#determine number of new vars to add to regression
#always provide at least the first level (in total), after the last level only add 1 at a time
#at intermediate levels add one more than the next level
get_add_vars <- function() {
  n.reg_vars <- length(com.env$best_clu_names)
  if (com.env$best_adj_r2 == 0) { #first loop
    add_vars <- ifelse(com.env$load_model,0,com.env$add_var_levels[1])     
  } else if (com.env$add_var_worse) {  #if last loop was bad don't add 
    add_vars <- 0
  } else {                             #normal operation
    avl_idx <- 1
    max_avl_idx <- length(com.env$add_var_levels)
    while(avl_idx <= max_avl_idx) {
      if (n.reg_vars < com.env$add_var_levels[avl_idx]) {
        add_vars <- max_avl_idx - avl_idx + 2
        if (avl_idx == 1) add_vars <- max(add_vars,(com.env$add_var_levels[1]-n.reg_vars))
        return(add_vars)
      }
      avl_idx <- avl_idx + 1
    }
    add_vars <- 1  #beyond last add_var_level
  }
  #cat("add_vars:",add_vars,"\n")
  return(add_vars)
}

#if model improved update com.env$best_reg_vars, com.env$best_adj_r2, com.env$best_vcom (return(TRUE))
#if model didn't improve delete var.env, revert to best previous com.env$v.com (return(FALSE))
try_mods <- function(adj_r2,current_loop,mod_var_loops) {
  return_try_mods <- (mod_var_loops > 0)    #don't mod if no mod_var_loops
  if (adj_r2 > com.env$best_adj_r2) {
    com.env$best_clu_names <- com.env$clu_names
    cat("Model Improved in add_vars from:",com.env$best_adj_r2,"->",adj_r2,"with",length(com.env$best_clu_names),"variables.\n")
    com.env$best_adj_r2 <- adj_r2
    clean_vcom()                                      #remove var definitions not needed in regression
    com.env$best_vcom <- com.env$v.com
    com.env$add_var_worse <- FALSE
    return(return_try_mods)
  } else if (adj_r2 < com.env$best_adj_r2) {
    print(paste("Adj_r2 got worse when adding vars",adj_r2,com.env$best_adj_r2))
    if (com.env$add_var_worse) {                                          #Error trapping, clean run still got worse
      print("WARNING: *************Couldn't get better model from clean run of add_vars****************")
      #source("close_session.R")                                             #if this happens systematically we could reset the "best" vars
      cat("Resetting best vars, best_adj_r2",adj_r2,"best_clu_names (count):",length(com.env$clu_names),"v.com reset \n")
      com.env$best_adj_r2 <- adj_r2
      com.env$best_clu_names <- com.env$clu_names
      com.env$best_vcom <- com.env$v.com
    }
    com.env$add_var_worse <- TRUE             #try clean vcom without adding vars next loop
    com.env$v.com <- com.env$best_vcom        #revert vcom
    #clean_var_env()  #remove all vars not in current vcom
    return(FALSE)
  } else if (adj_r2 == com.env$best_adj_r2) {
    if (com.env$add_var_worse) {
      cat("Model successfully reverted to previous best vcom\n")
    } else if (adj_r2 == 0) {
      cat("No Model found, trying add vars again\n")
      rm(var.env,envir=globalenv())             #delete var environment
      var.env <<- new.env(parent = globalenv()) #no vcom to manage cleaning, simply start over
      return(FALSE)
    } else {
      cat("Model didn't improve or get worse in add_vars, cleaning vcom and continuing to mod vars\n")
      clean_vcom()
    }
    com.env$add_var_worse <- FALSE
    return(return_try_mods)
  }
}

#runs mod_loop regression with mod_pair passed in
#if model improves try to optimize; when done update best vars (com.env$[best_reg_vars, best_adj_r2, best_vcom]) and clean var.env (return(TRUE))
#if worse (return(FALSE)) [try again]
finish_mod_loop <- function(mod_pair) {
  if (is.null(mod_pair)) return(FALSE)
  if (mod_pair[[2]]$use == "scale") com.env$ID_scale_opt <- mod_pair[[2]]$ID  #don't repeat scale var mods in same mod_var_loop
  mod_adj_r2 <- eval_adj_r2(mod_pair=mod_pair)  #run regression with modified variable
  if (mod_adj_r2 <= com.env$best_adj_r2) return(FALSE)
  print(paste("Model improved",com.env$best_adj_r2,"->",mod_adj_r2,'adjusting',mod_pair[[2]]$var_name,Sys.time()))
  #print(mod_pair[[1]]$math)
  #print(mod_pair[[2]]$math)
  com.env$best_adj_r2 <- mod_adj_r2
  com.env$best_clu_names <- com.env$clu_names
  optimize_mod_pair(mod_pair) #continually update best vars until no more optimization possible
  clean_vcom()
  com.env$best_vcom <- com.env$v.com 
  return(TRUE)
}

#if no mod list is given, function computes all vars in com.env$v.com, placing data in var.env, and runs stepwise regression using model vars
#if mod list is given, com.env$v.com is copied and modified (var.env assumed to be filled with all previous calcs)
#    function computes only the new mod vars (appended to end of data frames in var.env) 
#    named with vd$clu [needed to keep small changes unique]
#    single regression is run replacing the orig_var with mod_var
#    if adj_r2 improves keep com.env$v.com
#    otherwise revert to original com.env$v.com
#oos_data determines if OOS date range is computed, verbose controls printing (for debugging)  [both passed directly to collect_data and regression routines]
#Other vars modified: com.env$v.com, var.env$reg_data.df, com.env$clu_names, com.env$model.current
#Other vars accessed: colnames(var.env$BAC) [hardcoded; only for error checking]
eval_adj_r2 <- function(mod_pair=NULL,oos_data=FALSE,sim_data=FALSE,verbose=FALSE) {
  if (!is.null(mod_pair)) {  #only calc modified vars and vars dependent on them
    #print(paste("make_vars with modvar_list",Sys.time()))
    V1 <- mod_pair[[1]]
    V2 <- calc_one_var(mod_pair[[2]])                   #make modified variable & update columns in vd (for all stock in var.env)
    old.v.com <- com.env$v.com
    com.env$v.com[[V2$vcom_num]] <- V2
    names(com.env$v.com)[V2$vcom_num] <- V2$var_name
    com.env$var_names[V2$vcom_num] <- V2$var_name
    if (is.null(V2$clu)) print("problem in eval_adj_r2, V2 has no clu")
    if (V2$use == "model") {        #only one var to calculate
      mod_list <- list(mod_pair)
    } else if (V2$use == "scale") {  #must get all model vars requiring scale var (fix com.env$v.com in clean_vcom)
      mod_list <- get_mod_list_req_mod_pair(mod_pair)
      for (mod_pair in mod_list) {  #check if any model variables have been tried before (if so return 0)
        if (mod_pair[[2]]$use == "model") {
          if (mod_pair[[2]]$ID %in% com.env$ID_tried) {
            print(paste("Warning:In eval_adj_r2",mod_pair[[2]]$var_name,"already tried, return adj_r2 = 0, revert com.env$v.com"))
            if (exists("reg_names",envir=com.env)) rm("reg_names",envir=com.env)
            com.env$v.com <- old.v.com
            return(0)
          } else {
            com.env$ID_tried <- c(com.env$ID_tried,mod_pair[[2]]$ID)
          }
        }
      }
    } else {
      print("Can't modify raw or data vars in eval_adj_r2")
      print(mod_pair[[1]])
      print(mod_pair[[2]])
      source("close_session.R")
    }
  } else {
    calc_all_vars()  #eval_adj_r2 normally by calculating all vars in com.env$v.com
  }
  if (is.null(mod_pair)) {     #eval_adj_r2 normally
    collect_data(oos_data=oos_data,sim_data=sim_data)  #populate reg_data.df with model vars
    run_regression(oos_data=oos_data,sim_data=sim_data,verbose=verbose)    #run full stepwise regression after first removing colinear vars
  } else {                     #run_mod_regression        
    new_reg_names <- get_mod_reg_names(mod_list)
    collect_data(oos_data=oos_data,sim_data=sim_data,reg_names=new_reg_names)
    if ("ll_wts" %in% new_reg_names) {
      print("Warning: ll_wts in new_reg_names*******************************")
      print(com.env$clu_names)
    }
    run_mod_regression(new_reg_names)  #run single regression with forced vars (using replaced longID_names where needed)
  }
  new_adj_r2 <- 0
  # if (!exists("best_adj_r2",envir=com.env)) {
  #   print("Warning: com.env$best_adj_r2 did not exist in eval_adj_r2")
  #   com.env$best_adj_r2 <- 0  #not defined if model is loaded so needs to be set here
  # }
  if (length(com.env$model.current) > 3) new_adj_r2 <- summary(com.env$model.current)$adj.r.squared
  if (!is.null(mod_pair)) 
    if (new_adj_r2 <= com.env$best_adj_r2) { #revert to original com.env$v.com [no longID_names in original]
      com.env$v.com <- old.v.com
      #clean_var_env()
      if (exists("clu_names",envir=com.env)) rm("clu_names",envir=com.env)  #try to catch improper setting of com.env$best_clu_names
    } 
  return(new_adj_r2)
}

#given a mod_pair (scale) determine all vd's requiring it
#modify their math functions, calculate, and return as part of a mod_list(1(V1,V2),2(V1,V2),...)
#   need to make it recursive to work on raws (or if model vars can ever be used by other model vars)
get_mod_list_req_mod_pair <- function(mod_pair) {
  orig_var_name <- mod_pair[[1]]$var_name
  mod_var_name <- mod_pair[[2]]$var_name
  scale_idx <- which(names(com.env$v.com) == mod_var_name)
  if (length(scale_idx)>1) {  #if duplicate names take current mod_var, place it in first scale_idx and delete other instance
    #print("WARNING:Two names in com.env$v.com == mod_var_name in get_mod_list_req_mod_pair")
    first_duplicate <- min(scale_idx)
    cat("Two vars with name:",mod_var_name,"Moving mod to vcom#:",first_duplicate,"deleting others\n")
    print(scale_idx)
    for (i in scale_idx) {
      if (i == first_duplicate) {
        com.env$v.com[[i]] <- mod_pair[[2]]
      } else {
        com.env$v.com <- com.env$v.com[-i]
      }
    }
    scale_idx <- first_duplicate
    check_scale_idx <- length(which(names(com.env$v.com)==mod_var_name))
    if (check_scale_idx > 1) {
      print(scale_idx)
      for (i in scale_idx) {
        print(com.env$v.com[[i]])
        print(paste("names(com.env$v.com([",i,"])",names(com.env$v.com)[i]))
      }
      for (vd in com.env$v.com) {
        if (mod_var_name %in% vd$requires) {
          print(vd$var_name)
          print(vd$requires)
          print(vd$math)
        }
      }
      print(names(com.env$v.com))
    } else {
      cat("scale_idx and vcom corrected successfully *******************************************\n")
    }
  }
  mod_list <- list(mod_pair) 
  for (vd_idx in (scale_idx + 1):length(com.env$v.com)) {
    if (any(com.env$v.com[[vd_idx]]$requires %in% orig_var_name)) {
      V1 <- com.env$v.com[[vd_idx]]
      V2 <- V1
      for (i in 1:length(V2$requires)) if (V2$requires[i] == orig_var_name) V2$requires[i] <- mod_var_name
      for (i in 1:length(V2$math)) V2$math[i] <- gsub(orig_var_name,mod_var_name,V2$math[i])
      V2 <- set_name(V2)                        
      V2 <- calc_one_var(V2)
      com.env$v.com[[V2$vcom_num]] <- V2
      names(com.env$v.com)[V2$vcom_num] <- V2$var_name
      com.env$var_names[V2$vcom_num] <- V2$var_name
      mod_list[[length(mod_list)+1]] <- list(V1,V2)
    }
  }
  return(mod_list)
}

#function replaces clu from modified model variables into com.env$best_clu_names and returns them
get_mod_reg_names <- function(mod_list) {
  reg_names <- com.env$best_clu_names
  new_reg_names <- reg_names
  cnBAC <- colnames(var.env$BAC)  #for error checking
  for (vd_pair in mod_list) {
    V1 <- vd_pair[[1]]
    V2 <- vd_pair[[2]]
    #print(paste(V1$clu,V1$bins,V2$clu,V2$bins))
    if (V1$use == "model") {
      if (V1$bins == 1) {
        new_reg_names <- new_reg_names[!(new_reg_names %in% V1$clu)]
      } else {
        clu_list <- NULL
        for (i in 1:V1$bins) clu_list <- c(clu_list,paste0(V1$clu,"_",i))
        new_reg_names <- new_reg_names[!(new_reg_names %in% clu_list)]
      } 
      error_text <- NULL
      if ((V1$bins > 1) & (V1$bins == V2$bins)) {  #new var and old var are bin vars of same length
        for (i in 1:V1$bins) {
          if (paste0(V1$clu,"_",i) %in% reg_names) {
            new_reg_names <- c(new_reg_names,paste0(V2$clu,"_",i))
            if (!(paste0(V2$clu,"_",i) %in% cnBAC)) error_text <- paste("mod bin:",V2$clu,"_",i,"clu bin name found in new_reg_names, but not in colnames var.env$BAC")
          }
        }
      } else {                                                        #no binning, added bin, or deleted bin
        if (V2$bins == 1) {
          new_reg_names <- c(new_reg_names,V2$clu)
          if (!(V2$clu %in% cnBAC)) error_text <- paste(V2$clu,"clu bin name found in new_reg_names, but not in colnames var.env$BAC")
        } else {
          for (i in 1:V2$bins) {
            new_reg_names <- c(new_reg_names,paste0(V2$clu,"_",i))
            if (!(paste0(V2$clu,"_",i) %in% cnBAC)) error_text <- paste("add bin:",V2$clu,"_",i,"clu bin name found in new_reg_names, but not in colnames var.env$BAC")
          }
        }
      }
      if (!is.null(error_text)) {  #error checking to make sure names are available for regression
        print(error_text)
        source("close_session.R")
      }
    } 
  } #end mod_list loop
  # print("reg_names entering:")
  # print(reg_names)
  # print("reg_names leaving:")
  # print(new_reg_names)
  return(new_reg_names)
}

#run_regression takes in var.env$reg_data.df [created in collect_data]
#removes all colinear reg variables [using vif_func]
#runs stepwise regression leaving only variables with significance > 0.001 [using model_select]
#called from get_adj_r2() [with no mod_pair]
#set com.env$adj_r2, com.env$clu_names, com.env$model.current
#if oos_data == TRUE run stats on out-of-sampe data [contained in var.env$ret_data.df]
run_regression <- function(oos_data = FALSE,sim_data = FALSE, verbose = FALSE) {
  print(paste("run full regression",ncol(var.env$reg_data.df),Sys.time(),oos_data,sim_data))
  # if (!is.null(com.env$best_clu_names)) {
  #   for (var_name in com.env$best_clu_names) if (substr(var_name,1,1)=="v") {
  #     print("ERROR in run_regression com.env$best_clu_names contains long ID names")
  #   }
  # } 
  if (ncol(var.env$reg_data.df) > 2) {
    rm_list <- -1
    if (com.env$liqx) {
      #print(colnames(var.env$sim_data.df))
      rm_list <- c(rm_list,-which(colnames(var.env$reg_data.df)=="ll_wts"))
      rm_list <- c(rm_list,-which(colnames(var.env$reg_data.df)=="hl_wts"))
      #print(which(colnames(var.env$reg_data.df)=="ll_wts"))
      #print(which(colnames(var.env$reg_data.df)=="hl_wts"))
      #print(paste("rm_list=",rm_list)) 
      #print(colnames(var.env$reg_data.df[,rm_list]))
    } 
    if (is.null(com.env$best_clu_names)) {
      keep.dat <- vif_func(var.env$reg_data.df[,rm_list],thresh=10,trace=FALSE)
    } else {
      keep.dat <- vif_func(var.env$reg_data.df[,rm_list],keep=com.env$best_clu_names,thresh=10,trace=FALSE) #don't include predict.ret
    }
    remove_vars <- colnames(var.env$reg_data.df[,-1])[!(colnames(var.env$reg_data.df[,-1]) %in% keep.dat)]
    if (length(remove_vars)==0) remove_vars <- "none"
    if (any(remove_vars %in% com.env$best_clu_names)) {
      print("WARNING: in run_regression remove_vars contained in com.env$best_reg_vars")
      print(remove_vars)
      print(com.env$best_clu_names)
    }
    keep.dat <- append(colnames(var.env$reg_data.df)[1],keep.dat)
    if (com.env$liqx & ("ll_wts" %in% colnames(var.env$reg_data.df))) {
      keep.dat <- c(keep.dat,"ll_wts","hl_wts")
      #print(keep.dat)
    }
    var.env$reg_data.df <- var.env$reg_data.df[,keep.dat]
    rm_list <- -1
    if (com.env$liqx) {
      #print(colnames(var.env$sim_data.df))
      rm_list <- c(rm_list,-which(colnames(var.env$reg_data.df)=="ll_wts"))
      rm_list <- c(rm_list,-which(colnames(var.env$reg_data.df)=="hl_wts"))
      #print(which(colnames(var.env$reg_data.df)=="ll_wts"))
      #print(which(colnames(var.env$reg_data.df)=="hl_wts"))
      #print(paste("rm_list=",rm_list)) 
      #print(colnames(var.env$reg_data.df[,rm_list]))
    } 
  }
  #run_regression
  form1 <- as.formula(paste(colnames(var.env$reg_data.df)[1],"~ 1"))
  null <- lm(form1,data=var.env$reg_data.df)
  if (com.env$liqx) {
    dep_str <- colnames(var.env$reg_data.df)[1]
    ind_str <- paste(colnames(var.env$reg_data.df[,rm_list]),collapse = " + ")
    #print(paste(dep_str,"~",ind_str))
    #print(colnames(var.env$reg_data.df[,rm_list]))
    #print(rm_list)
    form2 <- as.formula(paste(dep_str,"~",ind_str))
  } else {
    form2 <- as.formula(paste(colnames(var.env$reg_data.df)[1],"~ ."))
  }
  #print(form2)
  reg.model <- lm(form2,data=var.env$reg_data.df)
  com.env$model.current <- model.select(reg.model,sig=com.env$sig,verbose=FALSE)
  if (length(com.env$model.current) == 0) {
    print("All variables left model")
    com.env$adj_r2 <- 0
  } else {
    com.env$adj_r2 <- summary(com.env$model.current)$adj.r.squared
    com.env$clu_names <- names(com.env$model.current$coefficients)[-1]
    if ("ll_wts" %in% com.env$clu_names) {
      print("Warning: ll_wts in run_regression model coefficients*******************************")
      print(com.env$clu_names)
    }
    if (sim_data) {
      sim_stats <- calc.r2(com.env$model.current,var.env$sim_data.df)
      cat("r2",sim_stats$rsq,"r20",sim_stats$rsq0,"cor",sim_stats$cor,"\n")
      cat("mse",sim_stats$mse,"mean",sim_stats$mean,"wpct",sim_stats$winpct,"\n")
      #print("reversing sign on regression model")
      #sim_stats <- calc.r2(com.env$model.current,var.env$sim_data.df,reverse=TRUE)
      #cat("r2",sim_stats$rsq,"r20",sim_stats$rsq0,"cor",sim_stats$cor,"\n")
      #cat("mse",sim_stats$mse,"mean",sim_stats$mean,"wpct",sim_stats$winpct,"\n")
    }
  }
  #print(paste("end regression",com.env$adj_r2,Sys.time()))
  if (verbose) print(names(com.env$model.current$coefficients)[-1])
}

#run_mod_regression takes in data frame var.env$reg_data.df, a list of reg_vars, and runs regression
#called from get_adj_r2(mod_pair) [when mod_pair != NULL]
#set com.env$adj_r2, com.env$clu_names, com.env$model.current
run_mod_regression <- function(reg_vars) {
  #print(paste("Run_mod_regression",Sys.time()))
  f <- as.formula(paste(colnames(var.env$reg_data.df)[1],paste(reg_vars,collapse=" + "),sep=" ~ "))
  com.env$model.current <- lm(formula=f,data=var.env$reg_data.df)
  if (length(com.env$model.current) == 0) {
    print("All variables left model")
    com.env$adj_r2 <- 0
  } else {
    com.env$adj_r2 <- summary(com.env$model.current)$adj.r.squared
    com.env$clu_names <- names(com.env$model.current$coefficients)[-1]
    if ("ll_wts" %in% com.env$clu_names) {
      print("Warning: ll_wts in run_mod_regression model coefficients*******************************")
      print(com.env$clu_names)
    }
  }
  return(com.env$adj_r2)
}

#run_wt_regression takes in data frame var.env$reg_data.df (with ll_wts,hl_wts), a list of reg_vars, and runs weighted regression
#called from run_sim [in port_opt.R
#set com.env$adj_r2, com.env$clu_names, com.env$model.current
run_wt_regression <- function(reg_vars) {
  #print(paste("Run_mod_regression",Sys.time()))
  f <- as.formula(paste(colnames(var.env$reg_data.df)[1],paste(reg_vars,collapse=" + "),sep=" ~ "))
  com.env$model.ll <- lm(formula=f,data=var.env$reg_data.df,weights=ll_wts)
  com.env$model.hl <- lm(formula=f,data=var.env$reg_data.df,weights=hl_wts)
  # if (length(com.env$model.current) == 0) {
  #   print("All variables left model")
  #   com.env$adj_r2 <- 0
  # } else {
  #   com.env$adj_r2 <- summary(com.env$model.current)$adj.r.squared
  #   com.env$clu_names <- names(com.env$model.current$coefficients)[-1]
  # }
  # return(com.env$adj_r2)
}


#collect_data gathers data for regression
#takes data from each stock in var.env and places into a single data frame [var.env$reg_data.df]
#if oos_data == TRUE include out-of-sample data in var.env$reg_data.df
collect_data <- function (oos_data = FALSE, sim_data = FALSE, reg_names = NULL) {
  print(paste("in_collect data",oos_data,sim_data))
  #print(reg_names)
  if (is.null(reg_names)) {
    vvars <- NULL
    #com.env$name2vcomnum <- NULL
    allmodelvars <- NULL
    for (vd in com.env$v.com) {
      if (vd$use == "model") {
        if (vd$bins == 1) {
          allmodelvars <- c(allmodelvars,vd$clu)
        } else {
          for (i in 1:vd$bins) allmodelvars <- c(allmodelvars,paste0(vd$clu,"_",i))
        }
      }
    }
  } else {
    allmodelvars <- c(com.env$predict.clu,reg_names)
  }
  
  #get data for all stx into single data frame
  var.env$reg_data.df <- NULL
  if (oos_data) var.env$oos_data.df <- NULL
  if (sim_data) var.env$sim_data.df <- NULL
  if (com.env$liqx & sim_data) {
    df_list <- "c(allmodelvars,'ll_wts','hl_wts')"
  } else {
    df_list <- "allmodelvars"
  }
  #print(allmodelvars)
  first_pass <- FALSE
  for (i in 1:com.env$stx) {
    ticker <- com.env$stx.symbols[i]
    #print(paste("Ticker:",ticker))
    if (com.env$liqx & sim_data) {
      cmd_string <- paste0("ll.xts <- data.env$",ticker,"[,'",ticker,".ll_wts'] * com.env$date_wts")
      if (first_pass) print(cmd_string)
      eval(parse(text=cmd_string))
      cmd_string <- paste0("hl.xts <- data.env$",ticker,"[,'",ticker,".hl_wts'] * com.env$date_wts")
      if (first_pass) print(cmd_string)
      eval(parse(text=cmd_string))
      cmd_string <- paste0("var.env$",ticker," <- cbind(var.env$",ticker,",ll.xts,hl.xts)")
      if (first_pass) print(cmd_string)
      eval(parse(text=cmd_string))
      cmd_string <- paste0("colnames(var.env$",ticker,")[length(colnames(var.env$",ticker,"))] <- 'hl_wts'")
      if (first_pass) print(cmd_string)
      eval(parse(text=cmd_string))
      cmd_string <- paste0("colnames(var.env$",ticker,")[length(colnames(var.env$",ticker,"))-1] <- 'll_wts'")
      if (first_pass) print(cmd_string)
      eval(parse(text=cmd_string))
    }
    # etf_ticker <- com.env$etf_lookup[ticker]
    # cmd_string <- paste("etf_start_date <- index(data.env$",etf_ticker,"[",com.env$days2remove,",])",sep="")
    # eval(parse(text=cmd_string))
    # cmd_string <- paste("stk_start_date <- index(data.env$",ticker,"[",com.env$days2remove,",])",sep="")
    # eval(parse(text=cmd_string))
    max_start_date <- max(c(as.Date(com.env$start_date[[ticker]]),as.Date(com.env$reg_start_date)))
    max_start_date2 <- paste0(max_start_date," UTC")
    #print(paste(max_start_date,com.env$start_date[ticker],com.env$reg_start_date))
    cmd_string <- paste("start_idx <- which(max_start_date2 == index(var.env$",ticker,"))",sep="")
    eval(parse(text=cmd_string))
    if (max_start_date > as.Date(com.env$reg_end_date)) next()
    reg_end_date2 <- paste0(com.env$reg_end_date," UTC")
    #cmd_string <- paste("end_idx <- which(as.Date(com.env$reg_end_date) == index(var.env$",ticker,"))",sep="")
    cmd_string <- paste("end_idx <- which(reg_end_date2 == index(var.env$",ticker,"))",sep="")
    eval(parse(text=cmd_string))
    subset_string <- paste0("var.env$",ticker,"[",start_idx,":",end_idx,",",df_list,"]")
    cmd_string <- paste0("var.env$reg_data.df <- bind_rows(var.env$reg_data.df,as.data.frame(",subset_string,"))")
    if (sim_data & first_pass) print(cmd_string)
    eval(parse(text=cmd_string))
    if (sim_data & first_pass) print(colnames(var.env$reg_data.df))
    if (oos_data) {
      subset_string <- paste0("var.env$",ticker,"[com.env$oos_date_range,",df_list,"]")
      cmd_string <- paste("var.env$oos_data.df <- bind_rows(var.env$oos_data.df,as.data.frame(",subset_string,"))",sep="")
      eval(parse(text=cmd_string))
    }
    if (sim_data) {
      subset_string <- paste0("var.env$",ticker,"[com.env$sim_date_range,",df_list,"]")
      #print(paste("Ticker:",ticker))
      cmd_string <- paste0("var.env$sim_data.df <- bind_rows(var.env$sim_data.df,as.data.frame(",subset_string,"))")
      if (first_pass) print(cmd_string)
      eval(parse(text=cmd_string))
    }
    first_pass <- FALSE
  }
}

#take a list of variable names and return vcom names or vcom nums
get_reg_names <- function(reg_model_vars,return_num=FALSE) { 
  #print(paste(reg_model_vars,return_num))
  reg_names <- NULL
  for (v in 1:length(com.env$v.com)) {  #create list of model variables as candidates to modify
    vd <- com.env$v.com[[v]]
    #print(paste(v,vd$name))
    if (length(vd$name) == 1) {
      if ( (vd$name %in% reg_model_vars) ) {
        #print(v)
        if (return_num) {
          reg_names <- append(reg_names,v)
        } else {
          reg_names <- append(reg_names,vd$name)
        }
        #reg_names <- ifelse(return_num,append(reg_names,v),append(reg_names,vd$name))
        #print(reg_names)
      }
    } else {
      if ((vd$name[1] %in% reg_model_vars) | (vd$name[2] %in% reg_model_vars)) {
        #print(v)
        if (return_num) {
          reg_names <- append(reg_names,v)
        } else {
          reg_names <- append(reg_names,vd$var_name)
        }
        #reg_names <- ifelse(return_num,append(reg_names,v),)
        #print(reg_names)
      }
    }
  }  #end for v loop
  #return (rnd_mod(reg_names=reg_names))    #select vd from reg_names and modify it
  reg_names <- unique(reg_names)
  return(reg_names)
}

#clean_vcom.R based on com.env$best_clu_names (called from opt_model)
clean_vcom <- function(verbose=FALSE) {
  # print("Vcom names before:")
  # print(names(com.env$v.com))
  vcom_clu_list <- NULL
  for (vd in com.env$v.com) {
    if (vd$bins == 1) {
      vcom_clu_list <- c(vcom_clu_list,vd$clu)
    } else {
      for (i in 1:vd$bins) vcom_clu_list <- c(vcom_clu_list,paste0(vd$clu,"_",i))
    }
  }
  # print("clu list before:")
  # print(vcom_clu_list)
  if (!(all(com.env$best_clu_names %in% vcom_clu_list))) {
    print("ERROR in vcom passed to clean vcom, missing clu names in vcom")
    print(vcom_clu_list)
    print(com.env$best_clu_names)
    source("close_session.R")
  }
  keep_vcom_name <- com.env$predict.ret
  clu_names <- com.env$best_clu_names
  keep_vcom_clu <- NULL
  for (clu in clu_names) { #convert binned variables
    clu <- gsub("_.*","",clu)
    keep_vcom_clu <- c(keep_vcom_clu,clu)
  }
  keep_vcom_clu <- unique(keep_vcom_clu)
  for (vd in com.env$v.com) {
    if (!is.null(vd$clu)) {
      #print(paste(vd$clu,vd$var_name))
      if (vd$clu %in% keep_vcom_clu) keep_vcom_name <- c(keep_vcom_name,vd$var_name)
    }
  }
  # print("clu list:")
  # print(keep_vcom_clu)
  # print("var list:")
  # print(keep_vcom_name)
  vdlist <- NULL
  for (i in 1:length(keep_vcom_name)) {
    V1 <- com.env$v.com[[keep_vcom_name[[i]]]]
    if (is.null(V1)) print(keep_vcom_name[[i]])
    if (V1$use %in% c("scale","model")) V1$requires <- get_requires_from_vd(V1)  #is this needed for raw/def vars?
    vdlist[[i]] <- vcom2vdlist_req(V1)
  }
  com.env$v.com <- NULL  
  for (i in 1:length(vdlist)) {
    vdlist2vcom(vdlist[[i]])
  }
  # print("Vcom names:")
  # print(names(com.env$v.com))
  #check colnames
  vcom_clu_list <- NULL
  for (vd in com.env$v.com) {
    if (vd$bins == 1) {
      vcom_clu_list <- c(vcom_clu_list,vd$clu)
    } else {
      for (i in 1:vd$bins) vcom_clu_list <- c(vcom_clu_list,paste0(vd$clu,"_",i))
    }
  }
  if (!(all(com.env$best_clu_names %in% vcom_clu_list))) {
    print("ERROR in clean vcom, missing clu names in vcom")
    print(vcom_clu_list)
    print(com.env$best_clu_names)
    source("close_session.R")
  }
}

load_model <- function(filename) { #loads model and sets com.env$predict.ret
  print(paste("function load_model",filename))
  modelfile <- paste(com.env$modeldir,"/",filename,sep="")
  load(file=modelfile,envir=com.env)
  #loop through v.com resetting names and clu; check for any dependencies and change names there as well
  com.env$v.com <- rename_varlist(com.env$v.com)
  com.env$predict.ret <- com.env$v.com[[1]]$name  #hard coded as first variable always
  com.env$predict.clu <- com.env$v.com[[1]]$clu
  com.env$best_adj_r2 <- 0
  print(paste("Loading:",modelfile,"predict:",com.env$predict.ret))
}

save_model <- function(filename) {
  print(paste("function save_model",filename))
  if (!com.env$load_model) { #check if filename already exists, if loaded overwrite file
    saved_model_files <- list.files(path=com.env$modeldir)
    print(paste("Saved_model_files:",length(saved_model_files)))
    if (length(saved_model_files)>0) {
      loop <- 1
      while (filename %in% saved_model_files & loop<5) {
        print(paste("Warning:",filename,"already in model directory, appending _"))
        filename <- paste(filename,"_",loop,sep="")
        loop <- loop + 1
      }
    }
  }
  modelfile <- paste0(com.env$modeldir,"/",filename)
  print(paste("Saving:",modelfile))
  save(list=c("v.com"),file=modelfile,envir=com.env)
}

#return a vdlist with last vd="model" var
#file loaded into rnd.env$VCOM (temporary holding space for loading/saving model vars)
load_saved_model_var <- function() {
  #print(paste("saved_var_files count",length(com.env$saved_var_files)))
  varfile_name <- sample(com.env$saved_var_files,size=1)
  com.env$saved_var_files <- com.env$saved_var_files[!com.env$saved_var_files %in% varfile_name]
  print(paste("load from file:",varfile_name,",",length(com.env$saved_var_files),"left"))
  varfile <- paste(com.env$vardir,"/",varfile_name,sep="")
  load(file=varfile,envir=rnd.env)
  vdlist <- rnd.env$VCOM
  #print(names(vdlist))
  #reset all vars name's/clu's in vdlist, check dependencies and change any names as needed (both in depenedencies and in math)
  vdlist <- rename_varlist(vdlist)
  return(vdlist)
}

save_vars <- function(save_var_n) {
  print(paste("function save_vars",save_var_n))
  if (save_var_n == 0) return()
  f1 <- as.formula(paste(com.env$predict.clu,"~.",sep=""))
  #print(f1)
  if (length(colnames(var.env$reg_data.df)) > 50) {
    print("model.subsets really.big")
    #print(colnames(var.env$reg_data.df))
    model.subsets <- regsubsets(f1,data=var.env$reg_data.df,nbest=save_var_n,nvmax=1,really.big=TRUE)
  } else {
    print("model.subsets small")
    #print(colnames(var.env$reg_data.df))
    model.subsets <- regsubsets(f1,data=var.env$reg_data.df,nbest=save_var_n,nvmax=1)
  }
  var_names <- NULL
  save_vcoms <- NULL
  com.env$model.subsets <- model.subsets
  for (i in 1:save_var_n) {
    vn <- names(coef(model.subsets,i))[2]
    print(vn)
    var_names <- append(var_names,vn)
    vn <- gsub("_.*","",vn)
    print(vn)
    for (vd in com.env$v.com) if (!is.null(vd$clu)) if (vn==vd$clu) save_vcoms <- append(save_vcoms,vd$vcom_num)
  }
  print(var_names)
  #save_vcoms <- get_reg_names(var_names,return_num=TRUE)
  save_vcoms <- unique(save_vcoms)
  
  print(save_vcoms)
  save_vcom_vars(save_vcoms)
}

save_vcom_vars <- function(var_num_list) {  #take var_num and create com.env$VCOM which defines it (including its dependencies)
  #print(paste("function vcom_var, var_num=",var_num))
  saved_var_files <- list.files(path=com.env$vardir)
  for (var_num in var_num_list) {
    vd <- com.env$v.com[[var_num]]
    if (is.null(vd$vcom_name)) {
      if (length(vd$name) == 1) {
        vd$vcom_name <- vd$name
      } else {
        vd$vcom_name <- substr(vd$name[1],1,(nchar(vd$name[1])-1))
      }
    }
    com.env$VCOM <- NULL
    for (v in vd$requires) {
      vcom_not_found <- TRUE
      i <- 0
      while ((vcom_not_found) & (i+1 < var_num)) {
        i <- i + 1
        if (length(com.env$v.com[[i]]$name) == 1) {
          if (v == com.env$v.com[[i]]$name) {
            vcom_not_found <- FALSE
          }
        } else if (!is.null(com.env$v.com$vcom_name)) {
          if (v == com.env$v.com[[i]]$name) {
            vcom_not_found <- FALSE
          }
        }
      }
      if (i == var_num) {
        print(paste("Error:All required vars for",vcom_name,"not defined in v.com, i=",i))
      }
      V1 <- com.env$v.com[[i]]
      cmd_string <- paste("com.env$VCOM$",V1$name," <- V1",sep="")
      #print(cmd_string)
      eval(parse(text=cmd_string))
    }
    cmd_string <- paste("com.env$VCOM$",vd$vcom_name," <- vd",sep="")
    print(cmd_string)
    eval(parse(text=cmd_string))
    varfile_name <- paste(vd$vcom_name,".vcom",sep="")
    varfile <- paste(com.env$vardir,"/",varfile_name,sep="")
    j <- 1
    save_file <- TRUE
    while (varfile_name %in% saved_var_files & save_file) {
      print(paste("Duplicate name",varfile_name))
      load(file=varfile,envir=rnd.env)
      saved_vd <- rnd.env$VCOM[[length(rnd.env$VCOM)]]
      if (vd$ID == saved_vd$ID) {
        print("Same ID, no need to save")
        save_file <- FALSE
      } else {
        j <- j + 1
        varfile_name <- paste(vd$vcom_name,"_",j,".vcom",sep="")
        varfile <- paste(com.env$vardir,"/",varfile_name,sep="")
      }
    }
    print(varfile)
    if (save_file) save(list=c("VCOM"),file=varfile,envir=com.env)
  }
}

#evaluate com.env$v.com, determine vars in use, keep only those in var.env ticker dfs
clean_var_env <- function() {
  #print("clean_var_env")
  if (is.null(com.env$best_adj_r2)) print("WARNING: no regression model found yet, trying to clean")
  else if (com.env$best_adj_r2 == 0) print("WARNING: no regression model found yet, cleaning")
  vars_used <- NULL
  for (vd in com.env$v.com) {
    if (vd$bins == 1) {
      vars_used <- c(vars_used,vd$clu)
    } else {
      for (i in 1:vd$bins) vars_used <- c(vars_used,paste0(vd$clu,"_",i))
    }
  }
  for (ticker in com.env$stx_list) {
    ve.xts <- paste0("var.env$",ticker)
    cmd_string <- paste0(ve.xts,"<- ",ve.xts,"[,colnames(",ve.xts,") %in% vars_used]")
    #print(cmd_string)
    eval(parse(text=cmd_string))
  }
  if (!(all(com.env$best_clu_names %in% colnames(var.env$BAC)))) {
    print("ERROR in clean var env, missing best_clu_name")
    print(com.env$best_clu_names)
    print(colnames(var.env$BAC))
  }
}