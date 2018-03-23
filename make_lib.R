#make vdlist starting with first requires var and ending in V1
vcom2vdlist_req <- function(V1,env_lookup="com.env") {  #look in com.env$v.com, otherwise rnd.env$vs.com (for raws)
  #print(paste("vcom2vdlist_req",V1$var_name))
  vdlist <- NULL
  #print(env_lookup)
  #print(V1$requires)
  for (v in V1$requires) {
    if (env_lookup == "com.env") {
      cmd_string <- paste0("vdlist$",com.env$v.com[[v]]$var_name," <- com.env$v.com[['",v,"']]")
    } else if (env_lookup == "rnd.env") {
      cmd_string <- paste0("vdlist$",rnd.env$vs.com[[v]]$var_name," <- rnd.env$vs.com[['",v,"']]")
    } else {
      print(paste("ERROR: Wrong env_lookup in vcom2vdlist_req:",env_lookup))
    }
    #print(cmd_string)
    eval(parse(text=cmd_string))
  }
  cmd_string <- paste0("vdlist$",V1$var_name," <- V1")
  #print(cmd_string)
  eval(parse(text=cmd_string))
  #print(names(vdlist))
  return(vdlist)
}

#create list of all v.com vars of either "model" or "scale" use
vcom2vdlist_use <- function(use) {
  vdlist <- NULL
  for (vd in com.env$v.com) {
    if (vd$use == use) {
      cmd_string <- paste0("vdlist$",vd$var_name," <- vd")
      #print(cmd_string)
      eval(parse(text=cmd_string))
    }
  }
  return(vdlist)
}

#insert vdlist into com.env$v.com, do not duplicate vd already in v.com
vdlist2vcom <- function(vdlist,vcom_num=0) {  #append to end if no vcom_num given
  #print("vdlist2vcom, vcom_num=",vcom_num)
  #print(names(vdlist))
  if (is.null(com.env$v.com)) {
    vd <- vdlist[[1]]
    vd$vcom_num <- 1
    cmd_string <- paste0("com.env$v.com$",vd$var_name," <- vd")
    #print(cmd_string)
    eval(parse(text=cmd_string))
    com.env$var_names <- vd$var_name
    vdlist <- vdlist[-1]
  }
  if (vcom_num == 0) vcom_num = length(com.env$v.com) + 1
  for (vd in vdlist) {
    if (vd$var_name %in% names(com.env$v.com)) next()
    #print(paste(vd$var_name,"clu:",vd$clu,"bins:",vd$bins))
    vd$vcom_num <- vcom_num
    if (vd$use != "def") {
      allmath <- paste(vd$math, sep = '', collapse = '')
      if (!(allmath %in% com.env$var_list)) {
        com.env$var_list <- c(com.env$var_list,allmath)
        vd$clu <- paste0("C",length(com.env$var_list))
        #print(paste("CLU:",vd$var_name,vd$clu))
      } else if (is.null(vd$clu)) {
        print("**************** Problem with allmath in vdlist2vcom, already in var_list, but no clu *******************")
        print(paste(vd$var_name,vd$use))
        print(allmath)
        tmp_clu <- which(com.env$var_list %in% allmath)
        print(tmp_clu)
        if (any(c(paste0("C",tmp_clu),paste0("C",tmp_clu,"_",1),paste0("C",tmp_clu,"_",2)) %in% colnames(var.env$BTC))) {
          print("clu found in var.env")
        } else {
          print("clu not found in var.env")
        }
        vd$clu <- paste0("C",tmp_clu)
      }
    }
    cmd_string <- paste0("com.env$v.com$",vd$var_name," <- vd")
    #print(cmd_string)
    eval(parse(text=cmd_string))
    com.env$var_names <- c(com.env$var_names,vd$var_name)
    vcom_num <- vcom_num + 1
    #check_unique_name(vd)
  }
  #cat("var_names stored")
  #print(com.env$var_names)
  #cat("v.com names")
  #print(names(com.env$v.com))
  if (all(com.env$var_names == names(com.env$v.com))) {
    #print("All v.com names match com.env$var_names")
  } else {
    print("WARNING: v.com names does not match com.env$var_names")
  }
  for (vd in com.env$v.com) {
    if (vd$use != "def") {
      if (is.null(vd$clu)) print(paste(vd$var_name," does not have a CLU defined"))
    }
  }
}

#return decay from calc_decay math string
#Assumes math form: "none", "calc_decay, decay" or "calc_decay, decay, var_cnt=cnt"
get_decay_from_math <- function(math) {
  #print(paste("get_decay_from_math",math))
  if (math == "none") {
    return(0)
  } else {
    if (strsplit(math,split=",")[[1]][1] != "calc_decay") {
      print(paste("ERROR in get_decay_from_math, passed",math))
      source("close_session.R")
    }
    return(strsplit(math,split=",")[[1]][2])
  }
}

#produce math string from decay, default "calc_decay, decay"
# if math provided insert decay into first parameter (leaving var_cnt unchanged)
# if var_cnt (!= 1) provided produce "calc_decay, decay, var_cnt=cnt"
# if decay = 1 return "none"
get_math_from_decay <- function(decay,math=NULL,var_cnt=NULL) {
  #print(paste("get_math_from_decay, decay=",decay,math,var_cnt))
  if (decay == 0) {
    math <- "none"
  } else if (is.null(math)) {
    if (is.null(var_cnt)) {
      math <- paste0("calc_decay,",decay)
    } else if (var_cnt == 1) {
      math <- paste0("calc_decay,",decay)
    } else {
      math <- paste0("calc_decay,",decay,",var_cnt=",var_cnt)
    }
  } else {
    if (strsplit(math,split=",")[[1]][1] != "calc_decay") {
      print(paste("ERROR in get_math_from_decay, passed",math))
      source("close_session.R")
    }
    math <- paste0("calc_decay,",decay,",",strsplit(math,split=",")[[1]][3])
  }
}

#return list (bp1,bp2) from calc_bin math string
#assumes math in form "calc_bin,bin_field='scale_var',b1=bp1,b2=bp2"
get_bp_from_math <- function(math) {
  #print(paste("get_bp_from_math",math))
  if (math == "none") return(c(0,0))

  parm_list <- strsplit(math,split=",")[[1]]
  if (parm_list[1] != "calc_bin") {
    print(paste("ERROR in get_bp_from_math, passed",math))
    source("close_session.R")
  }
  #name <- gsub(".*=","",parm_list[2])
  #name <- gsub("'","",name)
  bp1 <- as.numeric(gsub(".*=","",parm_list[3]))
  bp2 <- as.numeric(gsub(".*=","",parm_list[4]))
  if (bp1>bp2) {
    print(paste("ERROR:bp1>bp2 in get_bp_from_math,bp1=",bp1,"bp2=",bp2,math))
    source("close_session.R")
  }
  return(c(bp1,bp2))
}

#return math string replacing bin points with bp1 and bp2
get_math_from_bp <- function(bp1,bp2,math) {
  #print(paste("get_math_from_bp,bp1=",bp1,"bp2=",bp2,math))
  if ((bp1 == 0) & (bp2 == 0)) return("none")
  if (bp1>bp2) {
    print(paste("ERROR:bp1>bp2 in get_math_from_bp,bp1=",bp1,"bp2=",bp2,math))
    source("close_session.R")
  }
  
  parm_list <- strsplit(math,split=",")[[1]]
  if (parm_list[1] != "calc_bin") {
    print(paste("ERROR in get_math_from_bp, passed",math))
    source("close_session.R")
  }
  math <- paste(parm_list[1],parm_list[2],paste0("b1=",bp1),paste0("b2=",bp2),sep=",")
  return(math)
}

#create requires list from math of vd (using com.env$v.com to find vars if vdlist is not provided)
#extracts var names from math functions and appends var's require list and var name to new require list
#only works for scale and model vars (can't handle calc_math,calc_dol,from.data.env,calc_adjret,calc_ret)
get_requires_from_vd <- function(vd,vdlist=NULL) {
  #print(paste("get_requires_from_vd",vd$var_name))
  #print(vd$requires)
  #print(vd$math)
  if (is.null(vd)) {
    print("ERROR in get_requires_from_vd passed in NULL vd")
  }
  if (!(vd$use %in% c("model","scale"))) {
    print("ERROR get_requires_from_vd only works on scale or model vars")
    print(vd)
    source("close_session.R")
  }
  requires <- NULL
  if (is.null(vdlist)) {
    vdlist <- com.env$v.com
  }
  for (math in vd$math) {
    split_math <- strsplit(math,",")[[1]]
    math_type <- split_math[1]
    if (math_type %in% c("from.var.env","calc_vlty","calc_stk","calc_etf")) {
      scale_var <- split_math[2]
    } else if (math_type == "calc_bin") {
      scale_var <- strsplit(split_math[2],"=")[[1]][2]
    } else if (math_type == "calc_ia") {
      if (grepl("[[:alpha:]]",split_math[3])) { #calc_ia using scale_var 
        scale_var <- split_math[3]
      } else {                                  #numeric parameter in calc_ia
        scale_var <- NULL 
      }
    } else if (math_type %in% "calc_look_forward") {
      #requires <- c(requires,'C')
      scale_var <- NULL
    } else if (math_type %in% c("calc_math","calc_dol","from.data.env","calc_adjret","calc_ret")) {
      print(paste("ERROR in get_requires_from_vd, can't handle math_type,",math_type))
      print(vd)
      source("close_session.R")
    } else {
      scale_var <- NULL
    }
    #print(paste(math,"scale_var:",scale_var))
    newname <- gsub("'", "",scale_var)
    if (!is.null(scale_var)) requires <- c(requires,vdlist[[newname]]$requires,vdlist[[newname]]$var_name)
  }
  #print(paste("requires:",requires))
  if (is.null(requires) & (vd$var_name != com.env$predict.ret)) {
    print("Problem in get_requires_from_vd*********************")
    #for (math in vd$math) {
    #  print(strsplit(math,",")[[1]])
    #}
    print(vd)
  }
  return(requires)
}

rnd_choice <- function(choice) {
  #print(paste('rnd_choice',choice))
  cmd_string <- paste0("return(sample(names(rnd.env$p$",choice,"),size=1,prob=rnd.env$p$",choice,"))")
  eval(parse(text=cmd_string))
}

#return vdlist with last vd="raw" var [types generally derived from set rnd.env$vs.com sample vars]
get_raw_list <- function(raw_type = NULL) {
  if (is.null(raw_type)) raw_type <- rnd_choice("raw")
  #print(paste("Create new raw var, raw_type",raw_type)) #FUTURE: allow existing raw to be chosen
  to_var <- FALSE #debug purposes
  switch(raw_type,
         "ret" = {
           V1 <- rnd.env$vs.com[[sample(rnd.env$ret_list,1)]]
         },
         "sdata" = { #raw stock data
           sdata_type <- rnd_choice("sdata_type")
           switch(sdata_type,
                  "shout" = {
                    V1 <- rnd.env$vs.com[[which("o" == names(rnd.env$vs.com))]]
                  },
                  "mcap" = {
                    V1 <- rnd.env$vs.com[[which("M" == names(rnd.env$vs.com))]]
                  },
                  "div" = {
                    V1 <- rnd.env$vs.com[[which("dv" == names(rnd.env$vs.com))]]
                  },
                  "log_price" = {
                    V1 <- rnd.env$vs.com[[which("lp" == names(rnd.env$vs.com))]]
                  }
           )
         },
         "BC" = {
           V1 <- rnd.env$vs.com[[which("BC" == names(rnd.env$vs.com))]]
         },
         "V" = {
           V1 <- rnd.env$vs.com[[which("V" == names(rnd.env$vs.com))]]
         },
         "C2C" = {
           V1 <- rnd.env$vs.com[[which("C3" == names(rnd.env$vs.com))]]
           lag <- rnd_choice("C2Clag")
           V1$math <- sub("3",lag,V1$math)
         },
         "TI" = {
           ti_type <- rnd_choice("ti_type")
           switch(ti_type,
                  "fi" = {
                    V1 <- rnd.env$vs.com[[which("fi" == names(rnd.env$vs.com))]]
                  },
                  "mf" = {
                    V1 <- rnd.env$vs.com[[which("mf" == names(rnd.env$vs.com))]]
                  },
                  "tr" = {
                    V1 <- rnd.env$vs.com[[which("tr" == names(rnd.env$vs.com))]]
                  },
                  {
                    print(paste("TI type not supported",ti_type))
                    source("close_session.R")
                  })
         }, #not supported yet: money_flow, true_range, directional_indicator
         "existing" =, #not supported yet
         {
           print(paste("Raw type not supported",raw_type))
           source("close_session.R")
         }
  )
  decay <- rnd_choice(paste0(raw_type,".d"))
  if (substr(decay,1,1) == "v") {
    V1$math[length(V1$math)+1] <- paste0("calc_vlty,window=",substr(decay,2,nchar(decay)))
  } else {    
    if (decay != 0) V1$math[length(V1$math)+1] <- get_math_from_decay(decay)  #paste0("calc_decay,",decay)
  }
  V1$use <- "raw"
  #print(paste("call raw set_name",V1$math))
  if ((raw_type == "V") & com.env$sdata_available) { #check to convert dollars to turn-over
    if (rnd_choice("to")) {
      print("converting volume raw to turn-over raw")
      to_var <- FALSE  #for debug purposes
      V1$requires <- c(V1$requires,"ol1","uB","M")
      V1$math[length(V1$math)+1] <- paste0("calc_math,c('M'),'XX0 <- XX1/XX0'")
    }
  }
  V1 <- set_name(V1)
  if (to_var) { #for debug purposes
    print(V1)
    to_var <- FALSE
  }
  #print(V1$math)
  if (raw_type == "TI") {
    #print(V1$var_name)
    #print(V1$math)
  }
  return(vcom2vdlist_req(V1,env_lookup="rnd.env"))
}

#return vdlist with last vd="scale" var
#scale math in the form of "get(raw/stk/ETF):calc:cap:scale"
get_scale_list <- function() {
  #FUTURE: allow existing scale var to be chosen
  #select raw
  raw_type <- rnd_choice("raw")
  vdlist <- get_raw_list(raw_type)
  V1 <- vdlist[[length(vdlist)]]
  #print(paste("Create new scale var",V1$var_name)) 
  V1$requires <- c(V1$requires,V1$var_name)
  V1$calc_etf <- FALSE
  
  resid_type <- ifelse(grepl("T",V1$var_name),"W",rnd_choice(paste0(raw_type,".resid"))) #vlty raws shouldn't use S/E
  if (resid_type == "W") {
    V1$math <- paste0("from.var.env,'",V1$var_name,"'")
  } else if (resid_type == "S") {
    V1$math <- paste0("calc_stk,'",V1$var_name,"'")
  } else if (resid_type == "E") {
    V1$math <- paste0("calc_etf,'",V1$var_name,"'")
  }
  next_math <- 2
  #select calc
  calc <- rnd_choice("calc")
  if (calc != "none") {
    if (calc == "pow") {
      pow <- rnd_choice("pow")
      V1$math[next_math] <- paste0("calc_calc,'pow',",pow)
    } else {
      V1$math[next_math] <- paste0("calc_calc,'",calc,"'")
    }
    next_math <- next_math + 1
  }
  #select cap
  cap_type <- rnd_choice("cap_type")
  if (cap_type != "none") {
    cap <- rnd_choice(cap_type)
    calc_cap_str <- rnd_choice("cap_dim")
    V1$math[next_math] <- paste0(calc_cap_str,",",cap_type,"=",cap)
    next_math <- next_math + 1
  } 
  #select scale
  scale <- rnd_choice("scale")
  #print(paste(scale,next_math))
  if (scale == "z") {
    V1$math[next_math] <- "calc_z,ma=FALSE"
    V1$scale_type <- "zscale"
  } else if (scale == "Z") {
    V1$math[next_math] <- "calc_z,ma=TRUE"
    V1$scale_type <- "zscore"
  } else if (scale == "r") {
    V1$math[next_math] <- "calc_rank,10"  #use deciles to approx rank
    V1$scale_type <- "rank"
  } else if (scale == "zx") {
    V1$math[next_math] <- "calc_z_x,ma=FALSE"
    V1$scale_type <- "zscore"
  } else if (scale == "Zx") {
    V1$math[next_math] <- "calc_z_x,ma=TRUE"
    V1$scale_type <- "zscore"
  } else if (scale == "rx") {
    V1$math[next_math] <- "calc_rank_x,10"  #use deciles to approx rank
    V1$scale_type <- "rank"
  }
  V1$use <- "scale"  #only after scaling can a variable be considered "scale" type
  #print(paste("call calc set_name",V1$math))
  #for (vd in vdlist) if (is.null(vd$ID)) print(vd)
  V1 <- set_name(V1,vdlist=vdlist)
  cmd_string <- paste0("vdlist$",V1$var_name," <- V1")
  #print(cmd_string)
  eval(parse(text=cmd_string))
  #vdlist <- append(vdlist,V1)
  #names(vdlist)[length(vdlist)] <- V1$var_name
  #print(names(vdlist))
  #print(V1$math)
  return(vdlist)
}

#modify decay of model variable
mod_model_decay <- function(V1) {
  i <- which(grepl('calc_decay',V1$math))
  if (length(i) == 0) i = length(V1$math) + 1  #no decay in model var, add one randomly 
  d <- rnd_choice("model.d")
  if (d == "none") {
    V1$math <- V1$math[-i]
  } else {
    var_cnt <- ifelse(grepl("calc_bin",V1$math[i-1]),2,1)
    V1$math[i] = get_math_from_decay(d,var_cnt=var_cnt)
    # paste0("calc_decay,",d)
    # if (length(V1$names) > 1) V1[i] <- paste0(V1$math[i],",var_cnt=",length(V1$names))  #decay multiple columns
  }
  return(V1)
}

#returns scale var_name from math function (with quotes removed)
#  math_type supported {from.var.env,calc_ia,calc_bin}
get_scale_var_from_math <- function(math) {
  #print(paste("get_scale_var_from_math",math))
  math_parms <- strsplit(math,",")[[1]]
  #print(math_parms)
  math_type <- math_parms[1]
  #print(math_type)
  switch(math_type,
         "from.var.env" = {
           namePart <- gsub("'","",math_parms[2])
         },
         "calc_ia" = {
           namePart <- gsub("'","",math_parms[3])
         },
         "calc_bin" = {
           namePart <- gsub("'","",strsplit(math_parms[2],"=")[[1]][2])
         },
         "calc_constant" = {
           namePart <- "constant"
         },
         {
           print(paste("math_type",math_type,"not supported yet in get_scale_var_from_math"))
         }
        )
  return(namePart)
}

#takes in var_name and math {from.var.env,calc_ia,calc_bin,calc_constant} and replaces var_name as scale_var parm
#  calc_constant converted to from.var.env
#  Does not change bin points if new scale_var has different scale_type [needs to be done separately]
sub_scale_name_in_math <- function(math,var_name) {
  #print(paste("sub_calc_name_in_math",math,var_name))
  math_type <- strsplit(math,",")[[1]][1]
  var_name <- paste0("'",var_name,"'")  
  switch(math_type,
         "calc_constant" = {
           math <- paste("from.var.env",var_name,sep=",")
         },
         "from.var.env" = {
           math <- paste("from.var.env",var_name,sep=",")
         },
         "calc_ia" = {
           math_parms <- strsplit(math,",")[[1]]
           if (length(math_parms) == 4) {
             math <- paste(math_parms[1],math_parms[2],var_name,math_parms[4],sep=",")
           } else if (length(math_parms) == 3) {
             math <- paste(math_parms[1],math_parms[2],var_name,sep=",")
           }
         },
         "calc_bin" = {
           math_parms <- strsplit(math,",")[[1]]
           math <- paste(math_parms[1],paste("bin_field",var_name,sep="="),math_parms[3],math_parms[4],sep=",")
         },
         {
           print("ERROR sub_scale_name_in_math, math_type not supported")
           print(math)
           print(var_name)
           print(math_type)
           source("close_session.R")
         })
}

#takes in scale vd (raw/stk/etf:calc:cap:scale) modifies cap and returns it
#FUTURE: modify scale or calc
modify_scale_var <- function(V1) {
  #print("modify_scale_var")
  cap_idx <- NULL
  for (i in 1:length(V1$math)) {
    if (grepl("calc_cap",V1$math[i])) cap_idx <- i
  }
  
  cap_type <- "none"
  if (is.null(cap_idx)) {
    #print("cap_idx is null, getting rnd_choice(cap_type) [can't be none = delete]")
    while (cap_type=="none") cap_type <- rnd_choice("cap_type")
    mod_or_delete_cap <- FALSE  #adding cap      
    cap <- rnd_choice(cap_type)
    calc_cap_str <- rnd_choice("cap_dim")
    V1$math <- append(V1$math,paste0(calc_cap_str,",",cap_type,"=",cap),length(V1$math)-1) #calc_cap comes right before scale
  } else {
    orig_cap <- V1$math[cap_idx]
    mod_or_delete_cap <- TRUE
    cap_type <- rnd_choice("cap_type")
    if (cap_type != "none") {
      cap <- rnd_choice(cap_type)
      calc_cap_str <- rnd_choice("cap_dim")
      V1$math[cap_idx] <- paste0(calc_cap_str,",",cap_type,"=",cap)
    } else {                     #delete cap
      V1$math <- V1$math[-cap_idx]
    }
  }
  if (mod_or_delete_cap) {
    while (orig_cap==V1$math[cap_idx]) {
      #print("modify_scale_var while loop")
      V1 <- modify_scale_var(V1)  #keep trying until a different cap is found
    }
  }
  #V1 <- set_name(V1)  #need to keep original name so original var can be found, will be set in mod_var_model
  return(V1)
}

#MODE1: (V1,i) Takes model_vd and index (1=from.var.env/calc_constant,2=calc_ia/calc_bin,3=calc_bin) and changes calc var
#       If "existing_scale_var" changed model_vd is returned, otherwise new scale_vd is returned
#MODE2: (V1) Takes in scale_vd, modifies it, returns it
#MODE3: (V1==NULL) return existing_scale_var in V1
#If unable to perform operation, return V1
mod_scale_var <- function(V1=NULL,i=NULL) {
  #print(paste("mod_scale_var"))
  if (!is.null(V1) & !is.null(i)) { 
  #modify scale_var name in model vd, return model vd if existing_scale_var, otherwise return new/mod scale vd
    #print(paste(i,V1$math[i]))  
    if (V1$use != "model") {
      print("ERROR in mod_scale_var, i passed in with non-model var")
      print(V1)
      print(i)
      source("close_session.R")
    }
    scale_var_mod <- rnd_choice("scale_var_mod")
    if (is.null(V1)) {
      scale_var_mod <- "existing_scale_var"
    } else {
      if (get_scale_var_from_math(V1$math[i]) == "constant") scale_var_mod = "existing_scale_var"  #can't modify constant
    }
    switch(scale_var_mod,
           "existing_scale_var" = {
             scale_list <- vcom2vdlist_use("scale")
             #print("choosing random scale var in mod_scale_var from:")
             #print(names(scale_list))
             if (length(scale_list) < 2) {
               print("Not enough scale_vars to choose existing_scale_var")
               return(V1)
             }
             scale_list_names <- names(scale_list)
             old_var_name <- get_scale_var_from_math(V1$math[i])
             scale_list_names <- scale_list_names[!(scale_list_names %in% old_var_name)]
             #print(scale_list_names)
             new_var_name <- sample(scale_list_names,size=1)  
             #print(new_var_name)
             V1$math[i] <- sub_scale_name_in_math(V1$math[i],new_var_name)
             #print(V1$math[i])
             #over restricts calc_etf, changing scale var may allow etf to be calculated (but model etfs can't be used yet anyway)
             V1$calc_etf <- (V1$calc_etf & com.env$v.com[[new_var_name]]$calc_etf) 
             return(V1)
           },
           "modify_scale_var" = {  #longer name for modifying scale var
             if (is.null(i)) { #scale var passed in
               if (is.null(V1)) print("ERROR in modify mod_scale_var, no V1 passed in")
               V3 <- modify_scale_var(V1)
               return(V3)
             } else { #model var, get scale var in math, find it in var.env$v.com, modify it, and return modified scale var
               old_var_name <- get_scale_var_from_math(V1$math[i])
               #print(paste("mod_scale_var:",old_var_name))
               V2 <- com.env$v.com[[old_var_name]]
               if (is.null(V2)) {
                 print(paste("********** Error in mod_scale_var",old_var_name,"not found ***********"))
                 print(V1$math)
                 #source("close_session.R")
                 return(V1)
               } else {
                 V3 <- modify_scale_var(V2)
                 #return scale var with original name
                 return(V3)
               }
             }
           },
           "new_scale_var" =,
           #         {
           #         },
           {
             print(paste(mod_fve,"not supported yet"))
             source("close_session.R")
           }
    )
  } else {
    print("MODE2 and MODE3 in mod_scale_var not coded yet")
    source("close_session.R")
  }  
}

#takes in calc_bin math and converts it to new scale_type {(z:r->z),(r:z->r)}
convert_bin_points <- function(math,scale_type) {
  #print(paste("convert_bin_points",math,scale_type))
  if (strsplit(math,",")[[1]][1] != "calc_bin") {
    print("ERROR in convert_bin_points, math not calc_bin")
    print(paste(math,scale_type))
    source("close_session.R")
  }
  bp_pair <- get_bp_from_math(math)
  #print(bp_pair)
  b1 <- as.numeric(bp_pair[1])
  b2 <- as.numeric(bp_pair[2])
  if (scale_type == "z") { #convert from rank to z
    if ((b1 < 0) | (b1 > 1) | (b2 < 0) | (b2 > 1)) {
      print("ERROR in convert_bin_points, original bin points do not appear to be of scale_type rank")
      print(math)
      print(bp_pair)
      print(paste("scale_type=",scale_type))
      source("close_session.R")
    }
    new_bin_point_list <- rnd.env$bin_point.zlist
    old_bin_point_list <- rnd.env$bin_point.rlist  
  } else if (scale_type == "r") {
    new_bin_point_list <- rnd.env$bin_point.rlist
    old_bin_point_list <- rnd.env$bin_point.zlist  
  } else {
    print("ERROR scale type not recognized in convert_bin_points")
    print(math)
    print(paste("scale_type=",scale_type))
    source("close_session.R")
  }
  b1_idx <- which(as.numeric(old_bin_point_list)==as.numeric(b1))
  b2_idx <- which(as.numeric(old_bin_point_list)==as.numeric(b2))
  bp1 <- new_bin_point_list[b1_idx]
  bp2 <- new_bin_point_list[b2_idx]
  new_math <- get_math_from_bp(bp1,bp2,math)
  return(new_math)
}

#modify bin points of model variable [FUTURE: if no binning add binning]
mod_model_bin <- function(V1) {
  #print("mod_model_bin")
  i <- which(grepl('calc_bin',V1$math))
  if (length(i) == 0) {
    print("ERROR: ADDING bin not supported yet")
    source("close_session.R")
  }
  math_list <- strsplit(V1$math[[i]],split=",")[[1]]  #bin points are items 3 and 4 
  #print(math_list)
  if ((length(math_list) > 2) & (V1$math[1] != "calc_constant,1")) { #either new var or binning constant, binning not to be deleted
    if (rnd_choice("delete_bin") == "delete") {
      if (grepl("calc_decay",V1$math[length(V1$math)])) {
        if (!grepl("var_cnt=2",V1$math[length(V1$math)])) print(V1)
      }
      V1$math <- V1$math[-i]
      #print(paste("delete bin,math[i],i=",i))
      #check for decay
      if (length(V1$math) == i) { #decay exists, with var_cnt
        if (grepl("calc_decay",V1$math[i]) & grepl("var_cnt=2",V1$math[i])) {
          decay_split <- strsplit(V1$math[i],split=",")[[1]]
          V1$math[i] <- paste(decay_split[1],decay_split[2],sep=",")  #throw away var_cnt=2
          #print(paste("new decay math:",V1$math[i]))
        } else {
          print("ERROR in mod_model_bin: math after deleted bin was not a decay with var_cnt=2")
          print(V1)
          source("close_session.R")
        }
      }
      V1$requires <- get_requires_from_vd(V1)
      return(V1)
    }
  }
  
  mod_bin <- ifelse (length(math_list) < 3,"bin_points",rnd_choice("mod_bin")) #rnd_choice: {bin_points or scale_var}
  
  if (mod_bin == "bin_points") {
    scale_type <- substr(V1$scale_type,1,1)
    b1 <- rnd_choice(paste0("bin_points.",scale_type))
    b2 <- b1                  
    while(b1==b2) b2 <- rnd_choice(paste0("bin_points.",scale_type)) #make sure bin_point parms have at least 2 choices (otherwise infinite loop)
    #print(paste("b1:",b1,"b2:",b2,"min:",min(b1,b2),"max:",max(b1,b2)))
    bp1 <- min(as.numeric(b1),as.numeric(b2))
    bp2 <- max(as.numeric(b1),as.numeric(b2))
    #print(paste("bp1:",bp1,"bp2:",bp2))
    
    if (bp2<bp1) {
      print(paste("ERROR in mod_model_bin:bp2<bp1, bp1:",bp1,"bp2:",bp2))
      source("close_session.R")
    }
    V1$math[[i]] <- paste(math_list[1],math_list[2],paste0("b1=",bp1),paste0("b2=",bp2),sep=",")
    #print(V1$math)
    return(V1)
  }
  
  if (mod_bin == "scale_var") {
    #print("calling mod_scale_var from mod_model_bin")
    V2 <- mod_scale_var(V1=V1,i=i)
    if (V2$use == "model") { #scale_var to existing scale_var, if scale type changed adjust bin_ponts
      #print("changed scale var to existing scale var")
      orig_scale_var <- get_scale_var_from_math(V1$math[i])
      new_scale_var <- get_scale_var_from_math(V2$math[i])
      if (substr(com.env$v.com[[orig_scale_var]]$scale_type,1,1) != substr(com.env$v.com[[new_scale_var]]$scale_type,1,1)) {
        V2$math[i] <- convert_bin_points(V2$math[i],substr(com.env$v.com[[new_scale_var]]$scale_type,1,1))
        V2$scale_type <- com.env$v.com[[new_scale_var]]$scale_type
      }
    }
    #print(V2$math[i])
    return(V2)
  }
  
  print("ERROR: should have returned before now in mod_model_bin, mod_bin not supported")
  print(mod_bin)
  source("close_session.R")
}



#modify calc_ia term in model variable
#change signs, change ia type, delete ia
#ASSUMES scale_var is variable (not numeric parameter)
#FUTURE: add ia, change scale_var [first choosing another existing scale var, then creating new scale var]
mod_model_ia <- function(V1) {
  #print("mod_model_ia")
  #print(V1$math)
  i <- which(grepl('calc_ia',V1$math))
  if (length(i) == 0) {
    print("ERROR: mod_model_ia does not support adding ia")
    print(V1)
    source("close_session.R")
  }
  if (i != 2) {
    print("ERROR: mod_model_ia must be second math function in model var")
    print(V1)
    source("close_session.R")
  }
  mod_ia <- rnd_choice("mod_ia")
  #print(paste("mod_model_ia",mod_ia))
  if (mod_ia == "delete") {  #v.com cleanup will come in the normal clean vcom routine (after regression)
    V1$math <- V1$math[-i]
    V1$requires <- get_requires_from_vd(V1)
    return(V1)
  }
  
  math_list <- strsplit(V1$math[[i]],split=",")[[1]]   #[1] = 'calc_ia', [2] = ia_type, [3] = calc_var, [4] = sign (if exists)
  ia_type <- math_list[2]
  scale_var <- math_list[3]
  if (!grepl("[[:alpha:]]",scale_var)) {
    print("ERROR: mod_model_ia assumes scale_var is variable not numeric parameter")
    print(V1)
    source("close_session.R")
  }
  sign <- ifelse(length(math_list) == 4,math_list[4],"none")
  
  if (mod_ia == "sign") { #if add or sub change to mul or div and pick sign
    #print(paste("ia_type:",ia_type,"sign:",sign))
    if (ia_type %in% c("'rsh'","'fth'")) {
      V1$math[i] <- ifelse(sign=="none",paste('calc_ia',ia_type,scale_var,'sign=-1',sep=','),paste('calc_ia',ia_type,scale_var,sep=','))
      return(V1)
    }
    if (ia_type == "add") ia_type <- "mul"
    if (ia_type == "sub") ia_type <- "div"
    if (sign=="none") {
      sign <- paste0('sign=',sample(1:3,size=1))
    } else {
      new_sign <- sign
      while (new_sign == sign) new_sign <- paste0('sign=',sample(0:3,size=1))
      sign <- ifelse(new_sign=="sign=0","none",new_sign)
    }
    V1$math[i] <- ifelse(sign=="none",paste('calc_ia',ia_type,scale_var,sep=','),paste('calc_ia',ia_type,scale_var,sign,sep=','))
    return(V1)
  }
  
  if (mod_ia == "ia_type") {  #remove any sign
    new_ia_type <- ia_type
    while ((new_ia_type == ia_type) | (new_ia_type == "none")) new_ia_type <- rnd_choice("ia_type")
    V1$math[i] <- paste('calc_ia',paste0("'",new_ia_type,"'"),scale_var,sep=",")
    #print(paste("ia_type change, i:",i,new_ia_type,V1$math[i]))
    return(V1)
  }
  
  if (mod_ia == "scale_var") {  #FUTURE allow for different probs when called from mod_ia
    #print("calling mod_scale_var from mod_model_ia")
    V1 <- mod_scale_var(V1=V1,i=i)
    return(V1)
  }
  
  print("ERROR: should have returned before now in mod_model_ia, mod_ia not supported")
  print(mod_ia)
  source("close_session.R")
}

#return a vdlist with last vd="model" var
#math in the form of "get_scale_var,ia,bin,bin_decay"
#ia in c("mul","div","add","sub","rsh","fth") with a chosen scale_var
#bin points chosen based on scaling of bin var (zscore or rank) [bin var = scale_var]
#FUTURE: Allow model vars to ia / bin
get_model_list <- function() {
  #print("In get_model_list")
  #get new scale_var
  vdlist <- NULL
  model_start <- rnd_choice("model_start")
  if (model_start == "scale_var") {
    scale_list <- vcom2vdlist_use("scale")   #all scale vars currently in v.com
    total_vars <- length(scale_list) + length(rnd.env$vs.com)   # + all scale vars in vs.com
    if (com.env$load_vars) total_vars <- total_vars + length(com.env$saved_var_files) # + all saved model vars
    roll_die <- runif(1,0,1)
    if (roll_die < length(scale_list)/total_vars) {                                       #"existing_scale_var"
      V1 <- sample(scale_list,size=1)[[1]]
      vdlist <- vcom2vdlist_req(V1)
    } else if (roll_die < (length(scale_list) + length(rnd.env$vs.com)) / total_vars) {   #"new_scale_var"
      vdlist <- get_scale_list()
      V1 <- vdlist[[length(vdlist)]]
    } else {                                                                              #load model_var
      return(load_saved_model_var())   #skip rest of model creation
    }
    V1$requires <- c(V1$requires,V1$var_name)
    V1$math <- paste0("from.var.env,'",V1$var_name,"'")
  } else if (model_start == "constant") {                    #start with constant=1 (binning required)
    V1 <- rnd.env$vs.com[["i"]]
  } 
  #get ia term
  ia <- ifelse(model_start=="constant","none",rnd_choice("ia_type"))
  #print(ia)
  if (ia != "none") {  #FUTURE: add in sign choices, add in rnd weightings for add/sub
    vdlist2 <- get_scale_list()
    vdlist <- c(vdlist,vdlist2)     #may have replicated var definitions, will be managed in vdlist2vcom
    V2 <- vdlist[[length(vdlist)]]
    V1$requires <- unique(c(V1$requires,V2$requires,V2$var_name))
    V1$math[2] <- paste0("calc_ia,'",ia,"','",V2$var_name,"'")
    V1$calc_etf <- (V1$calc_etf & V2$calc_etf)
    if (is.null(V1$scale_type) | is.null(V2$scale_type)) {
      print(paste("ERROR in get_model_list, in ia either V1 or V2 does not have a scale type"))
      print(V1)
      print(V2)
      source("close_session.R")
    }
    if ((V1$scale_type == "rank") & (V2$scale_type != "rank")) V1$scale_type <- "zscale"  #best guess for bin points
    next_math <- 3
  } else {
    next_math <- 2
  }
  #get bin term, if var is constant binning is mandatory
  binning <- ifelse(model_start=="constant","bin",rnd_choice("bin"))
  #print(paste("binning =",binning,"next_math=",next_math))
  if (binning != "none") {  #FUTURE: add chance to reuse ia var
    vdlist2 <- get_scale_list()
    vdlist <- c(vdlist,vdlist2)
    V2 <- vdlist[[length(vdlist)]]
    V1$requires <- unique(c(V1$requires,V2$requires,V2$var_name))
    V1$scale_type <- V2$scale_type
    V1$calc_etf <- (V1$calc_etf & V2$calc_etf)
    V1$math[next_math] <- paste0("calc_bin,bin_field='",V2$var_name,"'")
    V1 <- mod_model_bin(V1)  #append random bin points 
    #bp_pair <- get_bp_from_math(V1$math[next_math])
    #if (bp_pair[1]>=bp_pair[2]) {
    #  print("ERROR in get_model_list, bp1 >= bp2",bp_pair[1],bp_pair[2])
    #  source("close_session.R")
    #}
    next_math <- next_math + 1
    #print(V1$math)
  }
  V1 <- mod_model_decay(V1)  #add in decay
  V1$use <- "model"
  V1 <- set_name(V1,vdlist=vdlist)
  cmd_string <- paste0("vdlist$",V1$var_name," <- V1")
  #print(cmd_string)
  eval(parse(text=cmd_string))
  #vdlist <- append(vdlist,V1)
  #names(vdlist)[length(vdlist)] <- V1$var_name
  #print(names(vdlist))
  #print(V1$math)
  return(vdlist)
}

make_new_model_var <- function() {
  #print("make_new_model_var")
  vdlist <- get_model_list()
  model_vd <- vdlist[[length(vdlist)]]
  if (model_vd$ID %in% com.env$ID_tried) {
    print(paste("Can't add",model_vd$var_name,", already tried",model_vd$ID))
  } else {
    com.env$ID_tried <- c(com.env$ID_tried,model_vd$ID)
    # for (vd in vdlist) {
    #   if (vd$use != "def") {
    #     if (is.null(vd$clu) | is.null(vd$bins)) {
    #       print("problem in vdlist, contains null clu/bins")
    #       print(vd$var_name)
    #       print(names(vdlist))
    #       source("close_session.R")
    #     }
    #   }
    # }
    vdlist2vcom(vdlist)
    print(paste("make_new_model_var:",vdlist[[length(vdlist)]]$var_name))
  }
  #print(names(com.env$v.com))
}


#takes in a model vd and returns a mod_pair [(orig_vd,mod_vd)] 
#  vd pair may be scale vd (model vars requiring scale vd handled in get_adj_r2 and clean_vcom)  
#if V1 is not provided a random one is selected from vcom list [may return scale vd pair used by model vd selected]
mod_var_model <- function(V1=NULL) {
  #print(paste("mod_var_model",V1$var_name,V1$use))
  new_mod <- FALSE
  loops <- 0
  while ((!new_mod) & (loops<10)) {
    loops <- loops + 1
    if (is.null(V1)) {
      vdlist <- vcom2vdlist_use("model")
      vdlist <- vdlist[-1]  #remove predictor var
      V1 <- sample(vdlist,size=1)[[1]]
    }
    if (V1$use != "model") {
      print(paste("ERROR: wrong use type in mod_var_model",V1$var_name,V1$use))
      source("close_session.R")
    }
    
    ia <- any(grepl("calc_ia",V1$math))
    bin <- any(grepl("calc_bin",V1$math))
    if (ia & bin) {
      choice <- rnd_choice("ia_bin")
    } else if (ia) {
      choice <- rnd_choice("ia")
    } else if (bin) {
      choice <- rnd_choice("bin")
    } else {
      choice <- rnd_choice("fve")
    }
    if ((choice == "constant") & (V1$math[1] == "calc_constant,1")) choice <- "fve"
    switch(choice,
           "decay" = {
             V2 <- mod_model_decay(V1)
           },
           "bin" = {
             V2 <- mod_model_bin(V1)
           },
           "ia" = {
             #print("mod_model_ia")
             #print(V1$math)
             V2 <- mod_model_ia(V1)
             #print(V2$math)
           },
           "fve" = {  #from.var.env or calc_constant,1
             #print(V1$math)
             #print("calling mod_scale_var from mod_var_model fve choice")
             V2 <- mod_scale_var(V1=V1,i=1)
             #print(V2$math)
           },
           "constant" = {
             #print(V1$math)
             V2 <- V1
             V2$math[1] <- "calc_constant,1"
             #print(V2$math)
           },
           {
             print(paste("Error: choice not supported in mod_var_model",choice))
             source("close_session.R")
           }
    )
    if (V2$use == "scale") {  #model var passed in, modified scale var returned, must find orignal scale vd for mod_pair 
      V1 <- com.env$v.com[[V2$var_name]]
    }
    V2 <- set_name(V2)
    
    new_mod <- (V1$ID != V2$ID)
  }
  
  if (!new_mod) {
    print(paste("Error: Could not modify model variable",V1$var_name))
    print(paste(new_mod,loops))
    print(choice)
    print(paste("ia=",ia))
    print(paste("bin=",bin))
    source("close_session.R")
  }
  if (V2$ID %in% com.env$ID_tried) {
    print(paste("Variable mod already tried",V2$var_name,V2$ID))
    return(NULL)
  } else {
    #print(paste("try mod vd:",V2$ID))
    com.env$ID_tried <- c(com.env$ID_tried,V2$ID)
  }
  #print("End of mod_var_model")
  #print(V1$math)
  #print(V2$math)
  mod_pair <- list(V1,V2)
  
  return(mod_pair)
}

#randomly selects a vd from vcom list and modifies it
#returns a mod_list [orig_vd,mod_vd]
mod_var <- function(mod_use=NULL) {
  #print(paste("mod_var",mod_use))
  if (is.null(mod_use)) {
    var_use <- rnd_choice("mod_use")
  }
  if (mod_use == "model") {
    mod_list <- mod_var_model()
  } else if (mod_use == "scale") {
    #print("calling mod_scale_var from mod_var")
    mod_list <- mod_scale_var()    
  }
  return(mod_list)
}

#find opt_type from math_list[orig_math,mod_math]
#"none" is used to show addition/deletion in a math list
find_opt_type <- function(math_list) {
  #print(paste("find_opt_type",math_list[1],math_list[2]))
  orig_math <- math_list[[1]]
  mod_math <- math_list[[2]]
  opt_type <- strsplit(orig_math,split=",")[[1]][1]  
  if (opt_type == "none") opt_type <- strsplit(mod_math,split=",")[[1]][1]  
  return(opt_type)
}

#move out 0.5X difference between i1 and i2
#must fall between 1 and max_i and not be equal to i1 or i2 [if not possible return 0]
move_out_index <- function(i1,i2,max_i,movement=0.5) {
  #print(paste("move_out_index",i1,i2,max_i,movement))
  if ((i1 == i2) | (i2 == 1) | (i2 == max_i) | (max_i < 3)) return(0)
  if (i1 > i2) { #reducing index
    i3 <- i2 - ceiling(movement*(i1-i2))
  } else { #increasing index
    i3 <- i2 + ceiling(movement*(i2-i1))  
  }
  i3 <- min(max(i3,1),max_i)
  #print(i3)
  return(i3)
}

#move in 0.5X difference between i1 and i2
#must not be equal to i1 or i2 [if not possible return 0]
move_in_index <- function(i1,i2,movement=0.5) {
  #print(paste("move_out_index",i1,i2,movement))
  if (i1 == i2) return(0)
  if (i1 > i2) { #reducing index
    i3 <- i1 - ceiling(movement*(i1-i2))
  } else {       #increasing index
    i3 <- i1 + ceiling(movement*(i2-i1))  
  }
  #print(i3)
  return(i3)
}


#opt_decay
#takes in math_pair [orig_math,mod_math] and returns opt_math
#if no move, and more tries possible, opt_math="try again", if no move and no tries left, opt_math="opt"
#orig_math is calc_decay (or lag or ma)
#IF decay (parm in the range (0,1) or math=="none"):
#  decays are indexed by rnd.env$decay_list, with "none" representing decay=1 [added to decay list] 
#  try1: 1.5X as far from orig_decay as mod_decay
#  try2: midway from orig_decay and mod_decay
#IF lag (parm 1 or 2), no opt since only two options, return [mod_math,mod_math]
#IF ma (parm > 2):                                                               #not coded yet, ma doesn't work
#  ma windows indexed by rnd.env$ma_window_list, try_num same as if decay 
#Assumes math form: "calc_decay, parm" or "calc_decay, parm, var_cnt=cnt"
opt_decay <- function(math_pair,try_num) {
  #print(paste("opt_decay",math_pair[1],math_pair[2],try_num))
  orig_math <- math_pair[[1]]
  mod_math <- math_pair[[2]]
  orig_decay <- get_decay_from_math(orig_math)
  mod_decay <- get_decay_from_math(mod_math)
  
  # orig_type <- strsplit(orig_math,split=",")[[1]][1]  
  # if (orig_type == "none") {
  #   decay <- 1
  # } else {
  #   decay <- gsub("^[^,]*,","",orig_math)         #get everything after first comma (parameters), assumes no "decay=" 
  # }

  opt_math <- "opt"
  #print(paste("mod_decay=",mod_decay))
  if (mod_decay < 1) {
    decay_index_list <- c(0,rnd.env$decay_list)
    orig_index <- which(orig_decay==decay_index_list)
  #  mod_decay <- gsub("^[^,]*,","",mod_math)      #get everything after first comma (parameters), assumes no "decay="
    mod_index <- which(mod_decay==decay_index_list)
    if (try_num == 1) {  #  try1: 1.5X as far from orig_decay as mod_decay
      new_index <- move_out_index(orig_index,mod_index,length(decay_index_list))  #move_out_index should make 0.5 -> 1, -0.5 -> -1
      if (new_index == 0) {
        opt_math <- "try again"
      } else {
        new_decay <- decay_index_list[new_index]
        opt_math <- get_math_from_decay(new_decay,math=mod_math)
      }
    } else if (try_num == 2) { #  try2: midway from orig_decay and mod_decay (last try)
      new_index <- move_in_index(orig_index,mod_index)
      if (new_index != 0) {
        new_decay <- decay_index_list[new_index]
        opt_math <- get_math_from_decay(new_decay,math=mod_math)
      }
    }
  } 
  #print(paste("opt_decay:",opt_math))
  return(opt_math)
}

#opt_cap
#takes in math_pair [orig_math,mod_math] and returns opt_math
#if no move, and more tries possible, opt_math="try again", if no move and no tries left, opt_math="opt"
#orig_math is calc_cap, (cap_pct, zcap, abscap)
#IF cap_pct:
#  decays are indexed by rnd.env$cap_pct_list, with "none" representing cap_pct=0 [added to decay list] 
#  try1: 1.5X as far from orig_cap as mod_cap
#  try2: midway from orig_cap and mod_cap
#IF zcap:
#  decays are indexed by rnd.env$zcap_list, with "none" representing zcap=0 [added to decay list]
#  tries as given in cap_pct
#IF abscap:
#  return "opt"
#Assumes math form: "calc_cap[_x],CAP_TYPE=parm"  where CAP_TYPE in (cap_pct,zcap,abscap)
opt_cap <- function(math_pair,try_num) {
  #print(paste("opt_cap",math_pair[1],math_pair[2],try_num))
  orig_math <- math_pair[[1]]
  mod_math <- math_pair[[2]]
  orig_fun <- strsplit(orig_math,split=",")[[1]][1] #get element to first comma (function call)
  mod_fun <- strsplit(mod_math,split=",")[[1]][1] #get element to first comma (function call)
  if (orig_fun == mod_fun) {
    fun_str <- orig_fun
  } else if (orig_fun == "none") {
    fun_str <- mod_fun
  } else if (mod_fun == "none") {
    fun_str <- orig_fun 
  } else {
    return("opt")   #mod was to toggle calc_cap, calc_cap_x
  }
  
  if (mod_math == "none") {
    mod_cap_type <- strsplit(strsplit(orig_math,split=",")[[1]][2],split="=")[[1]][1] #inherit from orig_math
    mod_parm <- 0
  } else {
    mod_cap_type <- strsplit(strsplit(mod_math,split=",")[[1]][2],split="=")[[1]][1] 
    mod_parm <- strsplit(strsplit(mod_math,split=",")[[1]][2],split="=")[[1]][2]
  }
  if (orig_math == "none") {
    orig_parm <- 0
    orig_cap_type <- mod_cap_type
  } else {
    orig_cap_type <- strsplit(strsplit(orig_math,split=",")[[1]][2],split="=")[[1]][1] 
    orig_parm <- strsplit(strsplit(orig_math,split=",")[[1]][2],split="=")[[1]][2]
  }
  
  if ((mod_cap_type == "abscap") | (orig_cap_type == "abscap")) {
    return("opt")
  }
  
  #orig_cap_index_list <- ifelse(orig_cap_type=="cap_pct",c(0,rnd.env$cap_pct_list),c(0,rnd.env$zcap_list))
  #mod_cap_index_list <- ifelse(mod_cap_type=="cap_pct",c(0,rnd.env$cap_pct_list),c(0,rnd.env$zcap_list)) #else "zcap"
  if (orig_cap_type == "cap_pct") {
    orig_cap_index_list <- c(0,rnd.env$cap_pct_list)
  } else { #zcap
    orig_cap_index_list <- c(0,rnd.env$zcap_list)
  }
  if (mod_cap_type == "cap_pct") {
    mod_cap_index_list <- c(0,rnd.env$cap_pct_list)
  } else { #zcap
    mod_cap_index_list <- c(0,rnd.env$zcap_list)
  }
  orig_index <- which(as.numeric(orig_parm) == orig_cap_index_list)
  mod_index <- which(as.numeric(mod_parm) == mod_cap_index_list)
  if (try_num == 1) {
    if (orig_index == mod_index) {
      new_index <- ifelse(mod_index<length(mod_cap_index_list),mod_index+1,0)  
    } else {
      new_index <- move_out_index(orig_index,mod_index,length(mod_cap_index_list))
    }
    if (new_index==0) return("try again")
  } else if (try_num == 2) {
    if (orig_index == mod_index) {
      new_index <- ifelse(mod_index>1,mod_index-1,0)
    } else {
      new_index <- move_in_index(orig_index,mod_index)
    } 
    if (new_index==0) return("opt")
  } else {
    return("opt")
  }
  new_cap <- mod_cap_index_list[new_index]  #from mod_cap_type
  opt_math <- paste0(fun_str,",",mod_cap_type,"=",new_cap)
  #print(opt_math)
  return(opt_math)
}
                  
#opt_bin - should not be called if bin_points in math_pair of different scale type
#takes in math_pair [orig_math,mod_math] and returns opt_math
#if no move, and more tries possible, opt_math="try again", if no move and no tries left, opt_math="opt"
#orig_math is "calc_bin,bin_field='scale_var',b1=bp1,b2=bp2"
#try1: move bp1 and bp2 in same direction (outside)
#try2: move bp1 and bp2 in same direction (inside)
#try3: move bp1 in same direction (outside) (hold bp2 constant) 
#try4: move bp1 in same direction (inside) (hold bp2 constant)
#try5: move bp2 in same direction (outside) (hold bp1 constant) 
#try6: move bp2 in same direction (inside) (hold bp1 constant)
opt_bin <- function(math_pair,scale_type,try) {
  #print(paste("opt_bin,try:",try,"scale_type:",scale_type))
  #print(math_pair)
  orig_math <- math_pair[[1]]
  mod_math <- math_pair[[2]]
  cmc <- compare_math_calc(math_pair)
  if ((cmc == "same") | (orig_math == "none") | (mod_math == "none")) return("opt")
  if (cmc != "calc_bin") {
    print(paste("Error in opt_bin, not called with calc_bin type",cmc))
    source("close_session.R")
  }
  scale_type <- substr(scale_type,1,1)
  cmd_string <- paste0("bin_list <- rnd.env$bin_point.",scale_type,"list")
  eval(parse(text=cmd_string))
  orig_bp <- get_bp_from_math(orig_math)
  mod_bp <- get_bp_from_math(mod_math)
  if ((orig_bp[1] == mod_bp[1]) & (orig_bp[2] == mod_bp[2])) return("opt") #must be bin_field change
  switch(try,
         "1" = {
           if ((orig_bp[1] == mod_bp[1]) | (orig_bp[2] == mod_bp[2])) {
             return("try again")
           }
           orig_idx <- which(orig_bp[1]==bin_list)
           mod_idx <- which(mod_bp[1]==bin_list)
           bp1_idx <- move_out_index(orig_idx,mod_idx,length(bin_list))
           orig_idx <- which(orig_bp[2]==bin_list)
           mod_idx <- which(mod_bp[2]==bin_list)
           bp2_idx <- move_out_index(orig_idx,mod_idx,length(bin_list))
           if ((bp1_idx == 0) | (bp2_idx == 0)) return("try again")
           bp1 <- bin_list[min(as.numeric(bp1_idx),as.numeric(bp2_idx))]
           bp2 <- bin_list[max(as.numeric(bp1_idx),as.numeric(bp2_idx))]
           if ((bp1 == bp2) & (bp1 > 0.5)) bp1 <- bin_list[bp1_idx-1]
           if ((bp1 == bp2) & (bp2 <= 0.5)) bp2 <- bin_list[bp2_idx+1]
         },
         "2" = {
           if ((orig_bp[1] == mod_bp[1]) | (orig_bp[2] == mod_bp[2])) {
             return("try again")
           }
           orig_idx <- which(orig_bp[1]==bin_list)
           mod_idx <- which(mod_bp[1]==bin_list)
           bp1_idx <- move_in_index(orig_idx,mod_idx)
           orig_idx <- which(orig_bp[2]==bin_list)
           mod_idx <- which(mod_bp[2]==bin_list)
           bp2_idx <- move_in_index(orig_idx,mod_idx)
           if ((bp1_idx == 0) | (bp2_idx == 0)) return("try again")
           bp1 <- bin_list[min(as.numeric(bp1_idx),as.numeric(bp2_idx))]
           bp2 <- bin_list[max(as.numeric(bp1_idx),as.numeric(bp2_idx))]
           if ((bp1 == bp2) & (bp1 > 0.5)) bp1 <- bin_list[bp1_idx-1]
           if ((bp1 == bp2) & (bp2 <= 0.5)) bp2 <- bin_list[bp2_idx+1]
         },
         "3" = {
           if (orig_bp[1] == mod_bp[1]) {
             return("try again")
           }
           orig_idx <- which(orig_bp[1]==bin_list)
           mod_idx <- which(mod_bp[1]==bin_list)
           bp1_idx <- move_out_index(orig_idx,mod_idx,length(bin_list))
           if (bp1_idx == 0) return("try again")
           bp1 <- bin_list[bp1_idx]
           bp2 <- mod_bp[2]
           if (bp1>=bp2) {
             bp2_idx <- which(mod_bp[2]==bin_list)
             bp1 <- bin_list[bp2_idx-1]
           }
         },
         "4" = {
           if (orig_bp[1] == mod_bp[1]) {
             return("try again")
           }
           orig_idx <- which(orig_bp[1]==bin_list)
           mod_idx <- which(mod_bp[1]==bin_list)
           bp1_idx <- move_in_index(orig_idx,mod_idx)
           if (bp1_idx == 0) return("try again")
           bp1 <- bin_list[bp1_idx]
           bp2 <- mod_bp[2]
           if (bp1>=bp2) {
             bp2_idx <- which(mod_bp[2]==bin_list)
             bp1 <- bin_list[bp2_idx-1]
           }
         },
         "5" = {
           if (orig_bp[2] == mod_bp[2]) {
             return("opt")
           }
           orig_idx <- which(orig_bp[2]==bin_list)
           mod_idx <- which(mod_bp[2]==bin_list)
           bp2_idx <- move_out_index(orig_idx,mod_idx,length(bin_list))
           if (bp2_idx == 0) return("try again")
           bp2 <- bin_list[bp2_idx]
           bp1 <- mod_bp[1]
           if (bp1>=bp2) {
             bp1_idx <- which(mod_bp[1]==bin_list)
             bp2 <- bin_list[bp1_idx+1]
           }
         },
         "6" = {
           orig_idx <- which(orig_bp[2]==bin_list)
           mod_idx <- which(mod_bp[2]==bin_list)
           bp2_idx <- move_in_index(orig_idx,mod_idx)
           if (bp2_idx == 0) return("try again")
           bp2 <- bin_list[bp2_idx]
           bp1 <- mod_bp[1]
           if (bp1>=bp2) {
             bp1_idx <- which(mod_bp[1]==bin_list)
             bp2 <- bin_list[bp1_idx+1]
           }
         },
         "7" = {
           return("opt")
         }
         )
  opt_math <- get_math_from_bp(bp1,bp2,mod_math)
  return(opt_math)
}

#compare_math takes in math pair (math1,math2) and determines difference type
#if identical returns "same"
#if sharing calc call returns single calc call (one of "calc_decay", "calc_cap", "calc_ia", "calc_bin", etc.)
#if either is "none" the other calc call is returned
#if different calc calls returns list of ["different",calc_call1,calc_call2]
compare_math_calc <- function(math_pair) {
  #print(paste("compare_math_calc"))
  if (math_pair[[1]] == math_pair[[2]]) return("same")
  math_type1 <- strsplit(math_pair[[1]],split=",")[[1]][1]
  math_type2 <- strsplit(math_pair[[2]],split=",")[[1]][1]
  if (math_type1 == "none") return(math_type2)
  if ((math_type1 == "none") | (math_type1 == math_type2)) return(math_type1)
  return(c("different",math_type1,math_type2))
}

#takes a vd_pair [orig_vd,mod_vd] and returns a pair of math differences [orig_math,mod_math]
#will insert "none" into the math list to show deleted ("none" in mod_math) or added ("none" in orig_math) functions
#if identical will return ("same","same")
get_vd_diff <- function(vd_pair) {
  #print(paste("get_vd_diff"))
  orig_vd <- vd_pair[[1]]
  mod_vd <- vd_pair[[2]]
  orig_math_list <- orig_vd$math
  mod_math_list <- mod_vd$math
  
  if (orig_vd$use == "raw" | mod_vd$use == "raw") {
    print("Error: raw vd types not supported in get_vd_diff yet")
    print(orig_vd)
    print(mod_vd)
    source("close_session.R")
  }

  #model vars assumed to be in the form of ("from.var.env","calc_ia","calc_bin","calc_decay") any of the last three can be missing  
  orig_idx <- 1
  mod_idx <- 1
  loops <- 0
  math_pair <- NULL
  while (((orig_idx <= length(orig_math_list)) | (mod_idx <= length(mod_math_list))) & (is.null(math_pair))) {
    #print(paste(loops,orig_idx,mod_idx,length(orig_math_list),length(mod_math_list)))
    loops <- loops + 1
    if (loops > 1000) {
      print("infinite loop in get vd_diff")
      print(vd_pair)
      print(math_pair)
      source("close_session.R")
    }
    if (mod_idx > length(mod_math_list)) {              #mod math ran out 
      math_pair <- c(orig_math_list[orig_idx],"none")
      return(math_pair)
    } else if (orig_idx > length(orig_math_list)) {     #orig math ran out
      math_pair <- c("none",mod_math_list[mod_idx])
      return(math_pair)
    } else {
      math_pair <- c(orig_math_list[orig_idx],mod_math_list[mod_idx])
      cmc <- compare_math_calc(math_pair)
      if (cmc[1] == "same") {
        math_pair <- NULL
        mod_idx <- mod_idx + 1
        orig_idx <- orig_idx + 1
      } else if (cmc[1] == "different") {
        if ((orig_vd$use == "model") & (cmc[2] == "from.var.env") | (cmc[3] == "from.var.env")) {
          if ((cmc[2]!="calc_constant")&(cmc[3]!="calc_constant")) { #other cmc must be calc_constant
            print("Error in vd_diff, from.var.env not paired with calc_constant (and different)")
            source("close_session.R")
          }
          return(math_pair)
        } else if ((orig_vd$use == "scale") & (mod_idx == 1)) {  #shouldn't be different (yet)
          print("ERROR in vd_diff for scale var")
          print("First math should not be different")
          print(cmc)
          print(orig_math_list)
          print(mod_math_list)
          source("close_session.R")
        } else if (((cmc[2]=="calc_ia") & ((cmc[3]=="calc_bin") | (cmc[3]=="calc_decay"))) |
            ((cmc[2]=="calc_bin") & (cmc[3]=="calc_decay")) |
            (cmc[2]=="calc_calc") |
            (grepl("calc_cap",cmc[2]) & (grepl("calc_z",cmc[3]) | grepl("calc_rank",cmc[3])))) {                #no match in mod_math
          math_pair <- c(orig_math_list[orig_idx],"none")
          return(math_pair)
        } else if ((((cmc[2]=="calc_bin") | (cmc[2]=="calc_decay")) & (cmc[3]=="calc_ia")) | 
                   ((cmc[2]=="calc_decay") & (cmc[3]=="calc_bin")) |
                   (cmc[3]=="calc_calc") |
                   ((grepl("calc_z",cmc[2]) | grepl("calc_rank",cmc[2])) & grepl("calc_cap",cmc[3]))) {         #no match in orig_math
          math_pair <- c("none",mod_math_list[mod_idx])
          return(math_pair)
        }
      } else {
        #mod_idx <- mod_idx + 1
        #orig_idx <- mod_idx + 1
        if ((orig_vd$use == "scale") & !grepl("calc_cap",cmc[1])) {
          print("ERROR in get_vd_diff, only calc_cap should be different in scale vars")  #currently
          source("close_session.R")
        }
        #difference found
        return(math_pair)
      }
    }
  }
  if (is.null(math_pair)) math_pair <- c("same","same")
  return(math_pair)
}

#takes a vd_pair [orig_vd,mod_vd] with the assumption that mod_vd performed better
#and returns an new_vd_pair [mod_vd,opt_vd] to attempt to improve further
#try_num gives the number of previous attempts, 
#if opt_math routine returns "opt" or no new math can be found, new_vd_pair[mod_vd,"opt"] is returned
#if opt_math routine returns "try again" then new_vd_pair[mod_vd,"try again"] is returned (opt_math called again with try_num incremented)
get_opt_vd <- function(vd_pair,try_num) {
  #print(paste("opt_math, try:",try_num))
  orig_vd <- vd_pair[[1]]
  mod_vd <- vd_pair[[2]]
  opt_vd <- mod_vd

  math_pair <- get_vd_diff(vd_pair)
  if (length(math_pair) != 2) {
    print("more than one diff not supported yet")
    return(list(mod_vd,"opt"))
  }
  if ((math_pair[[1]] == "same") & (math_pair[[2]] == "same")) return(list(mod_vd,"same"))
  cmc <- compare_math_calc(math_pair)
  #print(paste("opt_math, try:",try_num,"cmc=",cmc))
  #print(paste("cmc=",cmc))
  
  #only occurs on a deletion, cmc[1]=="different"
  if (length(cmc) == 3) {
    #print(cmc)
    #print(orig_vd$math)
    #print(mod_vd$math)
    return(list(mod_vd,"opt"))
  }
  
  switch(cmc,
         "calc_decay" = {
           opt_math <- opt_decay(math_pair,try_num)
         },
         "calc_cap_x" = {
           opt_math <- opt_cap(math_pair,try_num)
         },
         "calc_cap" = {
           opt_math <- opt_cap(math_pair,try_num)
         },
         "calc_bin" =
         {
           if (mod_vd$scale_type != orig_vd$scale_type) return(list(mod_vd,"opt"))
           opt_math <- opt_bin(math_pair,mod_vd$scale_type,try_num)
         },
         "calc_ia" = ,
         "calc_scale" = ,
         "calc_calc" = ,
         "from.var.env" =,
         "calc_constant" =,
         "calc_etf" =,
         "calc_stk" =,
         "calc_vlty" =,
         {
          print(paste("Warning: Opt_type:",cmc," not supported (in function opt_var)"))
          return(list(mod_vd,"opt"))
         }
  )
  
  new_vd <- mod_vd
  if ((opt_math == "opt") | (opt_math == "try again")) {
    return(list(mod_vd,opt_math))
  } else if (opt_math == "none") {
    print(paste("opt_math==none",opt_vd$var_name))
    opt_vd$math <- opt_vd$math[-which(grepl(cmc,new_vd$math))]
    opt_vd$requires <- get_requires_from_vd(opt_vd)
  } else {
    opt_vd$math[which(grepl(cmc,new_vd$math))] <- opt_math
  }
  opt_vd <- set_name(opt_vd)
  if (opt_vd$use == "model") {
    if (opt_vd$ID %in% com.env$ID_tried) {
      print(paste("opt_vd already tried, try again",opt_vd$var_name,opt_vd$ID))
      #print(opt_vd$math)
      return(list(mod_vd,"try again"))
    } else {
      #print(paste("try opt_vd:",opt_vd$ID))
      com.env$ID_tried <- c(com.env$ID_tried,opt_vd$ID)
    }
  } else {
    if (opt_vd$use == "scale") {
      if (opt_vd$ID %in% com.env$ID_scale_opt) {
        print(paste("opt_vd already tried for scale var, quit opt",opt_vd$var_name,opt_vd$ID))
        #print(opt_vd$math)
        return(list(mod_vd,"opt"))
      } else {
        #print(paste("try ID_scale_opt:",opt_vd$ID))
        com.env$ID_scale_opt <- c(com.env$ID_scale_opt,opt_vd$ID)
      }
    }
  }
  return(list(mod_vd,opt_vd))
}

#function takes mod_pair[orig_vd,mod_vd] and tests new vd's trying to find improvement
#opt_math creates new vd based on try_num
#eval_adj_r2 evaluates new_vd and if better places it into current com.env$v.com, updating com.env$best_adj_r2
#continue until "opt" is returned from opt_math
optimize_mod_pair <- function(mod_pair) {
  #print("optimize_code********************************************")
  try_num <- 1
  opt_not_found <- TRUE
  loop <- 0
  while (opt_not_found) {  #could recursively call optimize_mod_pair instead
    loop <- loop + 1
    if (loop > 100) {
      print("infinite loop in optimize_mod_list")
      print(mod_list)
      print(try_num)
      source("close_session.R")
    }
    new_vd_pair <- get_opt_vd(mod_pair,try_num)
    if (length(new_vd_pair[[2]]) == 1) {
      if (new_vd_pair[[2]] == "try again") {
        try_num <- try_num + 1
      } else if ((new_vd_pair[[2]] == "opt") | (new_vd_pair[[2]] == "same")) {
        opt_not_found <- FALSE
      }
    } else {
      new_adj_r2 <- eval_adj_r2(new_vd_pair)
      if (new_adj_r2 > com.env$best_adj_r2) {
        #print(paste(try_num,"opt_math:",new_vd_pair[[2]]$math))
        print(paste("opt model improved from: best_adj_r2:",com.env$best_adj_r2,"to:",new_adj_r2,"try:",try_num))
        com.env$best_adj_r2 <- new_adj_r2
        #cat('best_clu_names set in opt model improved\n')
        com.env$best_clu_names <- com.env$clu_names
        mod_pair <- new_vd_pair
        try_num <- 1             #for new mod pair
        loop <- 0
      } else {
        #print(new_vd_pair[[2]]$math)
        #print(paste("opt model did not improve: best_adj_r2:",com.env$best_adj_r2,"to:",new_adj_r2,"try:",try_num))
        try_num <- try_num + 1
      }
    }
  }
}
