#sets V1$name, V1$var_name, and V1$ID, V1$clu, V1$bins given a variable's math.list and a vdlist (optional)
set_name = function(V1,vdlist=NULL) {
  if (is.null(V1)) {
    print("Warning:V1 is null, no name/ID given")
    return(V1)
  }
  if (is.null(vdlist)) {
    vdlist <- com.env$v.com
  } 
  V1$var_name <- NULL
  V1$ID <- NULL
  binning <- FALSE
  #i <- 0
  for (math_str in V1$math) {
    namePart <- NULL
    IDPart <- NULL
    math <- strsplit(math_str,split=",")[[1]][1] #get element to first comma (function call)
    parms <- gsub("^[^,]*,","",math_str) #get everything after first comma (parameters) 
    switch(math, 
           "from.data.env" = {
             if (parms=="'shout'") {
               V1$var_name <- "o"
               IDPart <- which(namePart == letters)+26
               IDPart <- formatC(IDPart, width = 2, format = "d", flag = "0")
             } else if (parms == "'div'") {
               V1$var_name <- "dv"
               IDPart <- "62"
             } else if (parms == "'Close'") { #unadjusted Close
               V1$var_name <- "uC"
               IDPart <- "4703"
             } else {
               if (parms=="'Adjusted'" | parms=="'.Adjusted") {
                 V1$var_name <- "C"
               } else {
                 V1$var_name <- gsub("'","",parms)
               }
               V1$ID <- which(V1$var_name == LETTERS)
               V1$ID <- formatC(V1$ID, width = 2, format = "d", flag = "0")
             }
            },
           "calc_constant" = {
             #parms <- gsub(parms,".","")  #remove decimal if necessary
             namePart <- "i"
             IDPart <- paste0(which(namePart == letters) + 26,parms)
             #namePart <- paste0(namePart,parms)  #if we ever start using constants other than 1
           },
           "calc_look_forward" = {
             if(as.numeric(parms) < 0){
               V1$var_name <- paste0("CClf", -as.numeric(parms), "p")
               V1$ID <- "9999"
               break
             } else {
               namePart <- paste0("C", parms)
               IDPart <- paste0("03",parms)
             }
           },
           "calc_dol" = {
             namePart <- "D"
             IDPart <- which(namePart == LETTERS)
             IDPart <- formatC(IDPart, width = 2, format = "d", flag = "0")
           },
           "calc_calc" =,
           "calc_ia" = {
             parm_list <- strsplit(parms,split=",")[[1]]
             #print(parm_list)
             switch(parm_list[1],
                    "'log'" = {
                      IDPart <- "95"
                    },
                    "'exp'" = {
                      IDPart <- "96"
                    },
                    "'pow'" = {
                      IDPart <- "97"
                    },
                    "'abs'" = {
                      IDPart <- "98"
                    },
                    "'mul'" = {
                      namePart <- "x"
                      IDPart <- which(namePart == letters) + 26
                    },
                    "'div'" = {
                      namePart <- "y"
                      IDPart <- which(namePart == letters) + 26
                    },
                    "'add'" = {
                      namePart <- "p"
                      IDPart <- which(namePart == letters) + 26
                    },
                    "'sub'" = {
                      namePart <- "q"
                      IDPart <- which(namePart == letters) + 26
                    },
                    "'rsh'" = {
                      namePart <- "e"
                      IDPart <- "31"
                    },
                    "'rsx'" = {
                      namePart <- "e"
                      IDPart <- "30"
                    },
                    "'fth'" = {
                      namePart <- "f"
                      IDPart <- "32"
                    },
                    "'ftx'" = {
                      namePart <- "f"
                      IDPart <- "33"
                    }
             )
             if (length(parm_list)==3) {
               sign <- as.numeric(strsplit(parm_list[3],split="=")[[1]][2])
               IDPart <- paste0(IDPart,(sign+1)) #-1 option for sign in pow,rsh,fth 
             }
             if (length(parm_list)>=2) {
               if (grepl("[[:alpha:]]", parm_list[2])) {   #get variable name and sum its ID
                 #print(paste("Variable name found in calc_calc",parm_list[2]))
                 newname <- gsub("'", "", parm_list[2])
                 #print(paste("calc_calc naming1:",namePart,newname))
                 #namePart <- paste0(namePart,newname)  #don't add in ia var name
                 if(grepl("E",newname)){
                   namePart <- paste0(namePart,"E")
                   IDPart <- paste0(IDPart,"05")
                 } else if(grepl("T",newname)){
                   namePart <- paste0(namePart,"T")
                   IDPart <- paste0(IDPart,"20")
                 } else if(grepl("D",parms)){
                   namePart <- paste0(namePart,"D")
                   IDPart <- paste0(IDPart,"04")
                 } else if(grepl("S",parms)){
                   namePart <- paste0(namePart,"S")
                   IDPart <- paste0(IDPart,"19")
                 } else{
                   namePart <- paste0(namePart,"W")
                   IDPart <- paste0(IDPart,"23")
                 }  
                 if (is.null(vdlist[[newname]]$ID)) {
                   print(paste("calc_ia,calc_calc,newname:",newname))
                   print(vdlist[[newname]])
                   print(V1)
                 }
                 IDPart <- paste0(IDPart, sumID(vdlist[[newname]]$ID,vdlist[[newname]]))
               } else if (parm_list[1] %in% c("log", "mul", "div", "add", "sub")) {
                 parm_list[2] <- gsub("[^0-9]", "", parm_list[2])
                 IDPart <- paste0(IDPart, parm_list[2])
               } else if (parm_list[1] == "pow") {
                 pow <- strsplit(parm_list[2],"=")[[1]][2]
                 IDPart <- paste0(IDPart, which(rnd.env$pow_list == pow))
               }
             }
             #print(paste("calc_calc naming:",namePart,IDPart,parms))
           },
           "calc_math" = {
             switch(parms,
                    "c('H','L'),'XX0N <- sqrt(XX1*XX2)'" = {
                      namePart <- "J"
                      IDPart <- which(namePart == LETTERS)
                      IDPart <- formatC(IDPart, width = 2, format = "d", flag = "0")
                    }, 
                    "c('H','L','C'),'XX0N <- (XX1*XX2*XX3)^(1/3)'" = {
                      namePart <- "R"
                      IDPart <- which(namePart == LETTERS)
                      IDPart <- formatC(IDPart, width = 2, format = "d", flag = "0")
                    },
                    "c('D'),math_str='XX0N <- log(XX1) - 18.5'" = {
                      namePart <- "lD"
                      #IDPart <- rnd.env$nameId[[which(namePart == names(rnd.env$nameID))]]
                      IDPart <- "53"
                    },
                    "c('ol1','uB'),math_str='XX0 <- XX1*XX2'" = {
                      namePart <- "M"
                      IDPart <- which(namePart == LETTERS)
                      IDPart <- formatC(IDPart, width = 2, format = "d", flag = "0")
                    },
                    "c('M'),'XX0 <- XX1/XX0'" = {
                      namePart <- "t"
                      IDPart <- which(namePart == letters)+26
                      IDPart <- formatC(IDPart, width = 2, format = "d", flag = "0")
                    },
                    "c('B'),math_str='XX0 <- log(XX1)'" = {
                      namePart <- "lp"
                      IDPart <- "63"
                    },
                    "c('H','L','B'),'XX0N <- pmax(log(XX1/XX2),abs(log(XX1/XX3)),abs(log(XX2/XX3)))'" = {
                      namePart <- "tr"
                      #IDPart <- rnd.env$nameId[[which(namePart == names(rnd.env$nameID))]]
                      IDPart <- "54"
                    },
                    "c('DMd','TRd'),'XX0N <- XX1/XX2'" = {
                      namePart <- "di"
                      #IDPart <- rnd.env$nameId[[which(namePart == names(rnd.env$nameID))]]
                      IDPart <- "55"
                    },
                    "c('QR','D'),math_str='XX0N <- ifelse(XX1>0,XX2,0)'" = {
                      namePart <- "pmf"
                      #IDPart <- rnd.env$nameId[[which(namePart == names(rnd.env$nameID))]]
                      IDPart <- "56"
                    },
                    "c('QR','D'),math_str='XX0N <- ifelse(XX1<0,XX2,0)'" = {
                      namePart <- "nmf"
                      #IDPart <- rnd.env$nameId[[which(namePart == names(rnd.env$nameID))]]
                      IDPart <- "57"
                    },
                    "c('BC','D'),math_str='XX0N <- XX1*XX2'" = {
                      namePart <- "fi"
                      #IDPart <- rnd.env$nameId[[which(namePart == names(rnd.env$nameID))]]
                      IDPart <- "58"
                    },
                    {
                    #print(paste("ti",parms))
                    namelist <- strsplit(parms,split="'")[[1]]
                    namePart <- paste0(namelist[2],"X")
                    #print(com.env$v.com[[namelist[2]]]$ID)
                    if (is.null(com.env$v.com[[namelist[2]]]$ID)) print(V1)
                    IDPart <- paste0("24",sumID(vdlist[[namelist[2]]]$ID,vdlist[[namelist[2]]]))
                    }
             )
           },
           "calc_cap" = {
             parm_split <- strsplit(parms,"=")[[1]]
             switch(parm_split[1],
                    "cap_pct" = { IDPart <- paste0("1",which(as.numeric(rnd.env$cap_pct_list)==as.numeric(parm_split[2]))) },
                    "zcap" = { IDPart <- paste0("2",which(as.numeric(rnd.env$zcap_list)==as.numeric(parm_split[2]))) },
                    "abscap" = { IDPart <- "308"})
             if (nchar(IDPart)<2) print(paste(rnd.env$zcap_list,parm_split,IDPart))
             IDPart <- paste0("29",IDPart)
             #print(rnd.env$zcap_list)
             #print(parm_split[2])
             #print(which(as.numeric(rnd.env$zcap_list)==as.numeric(parm_split[2])))
             #print(paste(V1$vcom_num,"calc_cap ID:",IDPart))
           },
           "calc_cap_x" = {
             parm_split <- strsplit(parms,"=")[[1]]
             switch(parm_split[1],
                    "cap_pct" = { IDPart <- paste0("4",which(as.numeric(rnd.env$cap_pct_list)==as.numeric(parm_split[2]))) },
                    "zcap" = { IDPart <- paste0("5",which(as.numeric(rnd.env$zcap_list)==as.numeric(parm_split[2]))) },
                    "abscap" = { IDPart <- "608"})
             if (nchar(IDPart)<2) print(paste(rnd.env$zcap_list,parm_split,IDPart))
             IDPart <- paste0("29",IDPart)
             #print(rnd.env$zcap_list)
             #print(parm_split[2])
             #print(which(as.numeric(rnd.env$zcap_list)==as.numeric(parm_split[2])))
             #print(paste(V1$vcom_num,"calc_cap ID:",IDPart))
           },
           "calc_stk" = {
             namePart <- gsub("'","",parms)
             if (is.null(vdlist[[namePart]]$ID)) {
               print(paste("calc_stk:",namePart))
               print(vdlist[[namePart]]$ID)
               print(V1)
             }
             IDPart <- paste0("19",sumID(vdlist[[namePart]]$ID,vdlist[[namePart]]))
             namePart <- paste0(namePart,"S")
           },
           "calc_adj" = {
             namePart <- substr(parms,2,2) #adjList[which(parms == names(adjList))]
             IDPart <- which(namePart == LETTERS)
             IDPart <- formatC(IDPart, width = 2, format = "d", flag = "0")
           },
           "from.var.env" = {
             namePart <- gsub("'","",parms)
             #if (is.null(vdlist[[namePart]]$ID)) {
             #   print(V1)
             #   print(names(vdlist))
             #}
             if (nchar(namePart) > 1) {
               if (is.null(vdlist[[namePart]]$ID)) {
                 print(paste("fve, sumID:",vdlist[[namePart]]$ID,namePart))
                 print(vdlist[[namePart]])
                 print(V1)
               }
               IDPart <- sumID(vdlist[[namePart]]$ID,vdlist[[namePart]])
             }
           },
           "calc_ret" = {
             namePart <-  substr(parms,2,2)
             namePart <- paste0(namePart,substr(parms,6,6))
             IDPart <- which(substr(parms,2,2) == LETTERS)
             IDPart <- formatC(IDPart, width = 2, format = "d", flag = "0")
             IDPart <- paste0(IDPart, formatC(which(substr(parms,6,6) == LETTERS), width = 2, format = "d", flag = "0"))
           },
           "calc_lag" = {  #reserved for "def" vars, shouldn't lag 'D' [only 'V', log dollars, should be lagged] 
             #print(paste(V1$var_name,math_str))
             if(V1$var_name %in% LETTERS){
               if (V1$var_name == 'D') {
                 print("Dollar lagging not supported in set_name, should use decay,1")
                 source("close_session.R")
               }
               index <- which(V1$var_name == LETTERS)
               V1$var_name <- LETTERS[index-as.numeric(parms)]
               V1$ID <- which(V1$var_name == LETTERS)
               V1$ID <- formatC(V1$ID, width = 2, format = "d", flag = "0")
               break
             } else if (V1$var_name == "uC") {
               V1$var_name <- "uB"
               V1$ID <- "4702"
               break
             } else {
               namePart <- "l"
               namePart <- paste0(namePart,parms)
               IDPart <- paste0("38",parms)
             }
           },
           "calc_etf" = {
             namePart <- gsub("'","",parms)
             if (is.null(vdlist[[namePart]]$ID)) {
               print(paste("calc_etf,namePart",namePart))
               print(vdlist[[namePart]])
               print(V1)
             }
             IDPart <- paste0("05",sumID(vdlist[[namePart]]$ID,vdlist[[namePart]]))
             namePart <- paste0(namePart,"E")
           },
           "calc_decay" = {
             #numPart <- as.numeric(unlist(regmatches(parms,gregexpr("[[:digit:]]+\\.*[[:digit:]]*",parms))))
             #print(paste("calc_decay naming:",parms,numPart))
             numPart <- get_decay_from_math(math_str)
             if (numPart < 1) {
               #namePart <- substr(gsub("[^0-9]","",numPart),2,3)  #remove decimal
               namePart <- which(as.numeric(numPart)==as.numeric(rnd.env$decay_list))
               IDPart <- paste0("30",namePart)
               namePart <- paste0("d",namePart)
             } else if (numPart > 2) {
               IDPart <- paste0("27",numPart)
               namePart <- paste0("a",numPart)
             } else  {
               IDPart <- paste0("38",numPart)
               namePart <- paste0("l",numPart)
             }
             #if (nchar(IDPart) < 3) print(paste("calc_decay [more than 2 digits]:",IDPart))
           },
           "calc_bin" = {
             binning <- TRUE
             parm_list <- strsplit(parms,split=",")[[1]]
             name <- gsub(".*=","",parm_list[1])
             name <- gsub("'","",name)
             b1 <- as.numeric(gsub(".*=","",parm_list[2]))
             b2 <- as.numeric(gsub(".*=","",parm_list[3]))
             scale_type <- substr(V1$scale_type,1,1)
             if (scale_type == "z") {
               st <- 1
               b11 <- which(as.numeric(rnd.env$bin_point.zlist)==as.numeric(b1))
               b12 <- which(as.numeric(rnd.env$bin_point.zlist)==as.numeric(b2))
             } else { #scale_type == "r"
               st <- 2
               b11 <- which(as.numeric(rnd.env$bin_point.rlist)==as.numeric(b1))
               b12 <- which(as.numeric(rnd.env$bin_point.rlist)==as.numeric(b2))
             }
             if (is.null(vdlist[[name]]$ID)) {
                   print(paste("calc_bin",name))
                   print(vdlist[[name]])
                   print(V1)
             }
             if (is.null(b11) | is.null(b12)) print(paste("b1=",b1,"b2=",b2,"st=",st,"b11=",b11,"b12=",b12))
             IDPart <- paste0(IDPart,"28",sumID(vdlist[[name]]$ID,vdlist[[name]]),st,b11,b12)
             namePart <- paste0("b",name)
           },
           "calc_vlty" = {  #doesn't support field and window
             parm_list <- strsplit(parms,",")[[1]]
             if (grepl("window",parms[1])) {
               vlty_idx <- which(paste0("v",strsplit(parms[1],"=")[[1]][2])==rnd.env$vlty_list)
               IDPart <- paste0("20",vlty_idx)
               namePart <- paste0("T",vlty_idx)
             } else {
               namePart <- gsub("'","",parm_list[1])
               #print(paste("calc_vlty",namePart))
               if (is.null(vdlist[[namePart]]$ID)) {
                 print(paste("calc_vlty",namePart))
                 print(vdlist[[namePart]])
                 print(V1)
               }
               IDPart <- paste0("20",sumID(vdlist[[namePart]]$ID,vdlist[[namePart]]))
               namePart <- paste0(namePart,"T")
             }
           },
           "calc_dm" = {
             namePart <- ifelse(substr(parms,2,2)=="G","pdm","ndm")
             #IDPart <- rnd.env$nameId[[which(namePart == names(rnd.env$nameID))]]
             IDPart <- ifelse(substr(parms,2,2)=="G","59","60")
           },
           "calc_z" = {
             if (grepl("TRUE",parms)) {  #Zscore "Z"  ID's for Z,z,r chosen so they don't sum to same thing
               namePart <- "Z"
               IDPart <- "1"
             } else {                    #Zscale "z"
               namePart <- "z"
               IDPart <- "5"
             }
           },
           "calc_rank" = {
             namePart <- "r"
             IDPart <- "8"               #rank "r"
           },
           "calc_z_x" = {
             if (grepl("TRUE",parms)) {  #Zscore "Z"  ID's for Z,z,r chosen so they don't sum to same thing
               namePart <- "Z"
               IDPart <- "2"
             } else {                    #Zscale "z"
               namePart <- "z"
               IDPart <- "6"
             }
           },
           "calc_rank_x" = {
             namePart <- "r"
             IDPart <- "9"               #rank "r"
           },
           print(paste("calc_function not found in set_name",math_str,math,parms))
    )
    if(V1$type == "ti") {
      #print(paste(namePart,IDPart,V1$var_name,V1$ID))
    }
    #print(paste(namePart,IDPart))
    V1$var_name <- paste0(V1$var_name,namePart)
    V1$ID <- paste0(V1$ID,IDPart)
    #print(paste(V1$var_name,V1$ID))
  }
  if (V1$use %in% c("raw","model","scale")) {
    #print(paste("model var, appending ID",V1$var_name))
    if (is.null(V1$ID)) print(V1)
    switch(V1$use,
           "model" = {V1$ID <- paste0(V1$ID,"39")},  # 'm'
           "scale" = {V1$ID <- paste0(V1$ID,"45")},  # 's'
           "raw"   = {V1$ID <- paste0(V1$ID,"49")})  # 'w'
    if (is.null(V1$ID)) {
      print("set_name (raw, model, or calc var)")
      print(V1)
    }
    letter_id <- substr(V1$use,1,1)
    if (V1$use == "raw") letter_id <- "w"
    var_name <- paste0(V1$var_name,letter_id,sumID(V1$ID,V1))
    if (V1$use == "scale") {
      var_name <- gsub("w[[:digit:]]+","",var_name)     #shorten raw names by removing "w[sumID]"
      var_name <- gsub("l1","",var_name)                #remove l1 on raw vars
    }
    if (V1$use == "model") {
      var_name <- gsub("w[[:digit:]]+","",var_name)     #shorten raw names by removing "w[sumID]"
      var_name <- gsub("s[[:digit:]]+","",var_name)     #shorten scale names by removing "s[sumID]"
      var_name <- gsub("l1","",var_name)
      var_name <- gsub("Z","",var_name)
      var_name <- gsub("z","",var_name)
      var_name <- gsub("r","",var_name)                 #shorten model names by removing scaling and lag1
    }
    if (nchar(var_name) > 25) {
      print("*************************************************************")
      print(V1$math)
      print(var_name)
    }
    V1$var_name <- var_name
  }
  if (!binning) {
    V1$name <- V1$var_name
    #V1$longID_name <- paste0("v",V1$ID)
    V1$bins <- 1
  } else {
    V1$name[1] <- paste0(V1$var_name,"l")
    V1$name[2] <- paste0(V1$var_name,"h")
    #V1$longID_name[1] <- paste0("v",V1$ID,"l")
    #V1$longID_name[2] <- paste0("v",V1$ID,"h")
    V1$bins <- 2
  }
  if (V1$use != "def") {
    allmath <- paste(V1$math, sep = '', collapse = '')
    if (!(allmath %in% com.env$var_list)) {
      com.env$var_list <- c(com.env$var_list,allmath)
      V1$clu <- paste0("C",length(com.env$var_list))
      #print(paste("CLU:",V1$var_name,V1$clu))
    } else if (is.null(V1$clu)) {
      var_num <- which(names(com.env$v.com)==V1$var_name)
      if (length(var_num)>0) {
        if (is.null(com.env$v.com[[var_num]]$clu)) {
          print("**************** Problem with allmath in set_name, already in v.com, but no clu *******************")
          print(paste(V1$var_name,V1$use))
          print(allmath)
          tmp_clu <- which(com.env$var_list %in% allmath)
          print(tmp_clu)
          if (any(c(paste0("C",tmp_clu),paste0("C",tmp_clu,"_",1),paste0("C",tmp_clu,"_",2)) %in% colnames(var.env$BTC))) {
            print("clu found in var.env")
          } else {
            print("clu not found in var.env")
          }
          source("close_session.R")
        }
        V1$clu <- com.env$v.com[[var_num]]$clu
      } else {
        print(paste("Warning:",V1$var_name,"var has been tried before"))
        tmp_clu <- which(com.env$var_list %in% allmath)
        print(tmp_clu)
        if (any(c(paste0("C",tmp_clu),paste0("C",tmp_clu,"_",1),paste0("C",tmp_clu,"_",2)) %in% colnames(var.env$BTC))) {
          print("clu found in var.env")
        } else {
          print("clu not found in var.env")
        }
        V1$clu <- paste0("C",tmp_clu)
      }
    }
  }
  #check_unique_name(V1)
  #print(paste(V1$use,V1$var_name,V1$clu,V1$bins))
  return(V1)
}

#reset all vars var_name's/clu's, if changed update all requires for new name (both in requires and math)
rename_varlist <- function(varlist) {
  #print("In rename_varlist")
  #print(names(varlist))
  for (i in 1:length(varlist)) {
    vd <- varlist[[i]]
    orig_name <- vd$var_name
    #print(paste("set_name vd$var_name:",vd$var_name))
    vd <- set_name(vd,varlist)
    #print(paste("vd$clu:",vd$clu))
    vlength <- length(varlist)
    if ((orig_name != vd$var_name) & (i<vlength)) {
      for (j in (i+1):vlength) {
        #print(paste(i,j,vlength))
        vd2 <- varlist[[j]]
        if (orig_name %in% vd2$requires) {
          #print(paste("name change:",orig_name," -> ",vd$var_name,"before:"))
          #print(vd2$requires)
          #print(vd2$math)
          req_num <- which(orig_name == vd2$requires)
          vd2$requires[req_num] <- vd$var_name  #change name in requires
          for (k in 1:length(vd2$math)) {
            new_math <- gsub(orig_name,vd$var_name,vd2$math[k],fixed=TRUE)
            vd2$math[k] <- new_math
          }
          #print("after:")
          #print(vd2$requires)
          #print(vd2$math)
          varlist[[j]] <- vd2
          print(paste("updating vd:",j,vd2$var_name))
        }
      }
    }
    varlist[[i]] <- vd
    names(varlist)[i] <- vd$var_name
    #print(paste("updating vd:",i,vd$var_name))
    #print(paste(i,varlist[[i]]$var_name,varlist[[i]]$vcom_num,names(varlist)[i]))
  }
  #for (vd in varlist) print(paste(vd$var_name,vd$clu,names(varlist)[vd$vcom_num]))
  if (length(names(varlist)) != length(unique(names(varlist)))) {
    print("ERROR: Duplicate name in rename_varlist")
    source("close_session.R")
  }
  return(varlist)
}

check_unique_name <- function(V1) {
  failed <- FALSE
  for (vd in com.env$v.com) {
    if (!is.null(V1$clu) & !is.null(vd$clu)) if (vd$clu == V1$clu) failed <- TRUE
    if (vd$var_name == V1$var_name) failed <- TRUE
    #if ((V1$use != "def") & (is.null(V1$clu))) failed <- TRUE
    if (failed) {
      #print("Either name or clu already in vcom:")
      #print(paste("WARNING:",vd$vcom_num,vd$var_name,vd$clu,V1$var_name,V1$clu))
      failed <- FALSE
      #source("close_session.R")
    }
  }
}

sumID <- function(ID,V1=NULL){  #sum digits two at a time (so index 10 and 1 aren't identical)
  if (is.null(ID)) {
    print ("Error: V1 has no ID")
    print (V1)
    source("close_session.R")
  }
  total <- 0
  for (i in 1:nchar(ID)) total <- total + as.numeric(substring(ID,i,i))
  return(total)
}

check_ids <- function(id_list) {
  for (i in 1:length(id_list)) {
    if (gsub("[^0-9]","",id_list[i]) != id_list[i]) {
      print(paste("non-numeric character in id:",i,id_list[i]))
      return(-1)
      print(names(com.env$v.com))
      for (i in 1:length(com.env$v.com)) {
        print(paste(i,com.env$v.com[[i]]$ID))
        if (gsub("[^0-9]","",com.env$v.com[[i]]$ID) != com.env$v.com[[i]]$ID) {
          print(com.env$v.com[[i]])
        }
      }
    }
  }
  return(0)
}

check_dependencies <- function() {
  for (i in 1:length(com.env$v.com)) {
    if (length(com.env$v.com[[i]]$requires) > 0) {
      for (var_name in com.env$v.com[[i]]$requires) {
        if (!(var_name %in% names(com.env$v.com)[1:(i-1)])) {
          print(paste("WARNING:",var_name,"does not come before",com.env$v.com[[i]]$name))
          return(FALSE)
        }
      }
    } 
  }
  return(TRUE)
}

#Needed if creating model from scratch, first variable is dependent variable to predict
define_predict_ret <- function() {
  #print("In define_vars")
  com.env$v.com <- NULL
  V1 <- NULL
  V1$requires <- NULL
  V1$type <- "ret"
  V1$use <- "model"
  V1$calc_etf <- TRUE
  V1$math[1] <- paste0("calc_look_forward,-",com.env$look_forward)  #only calc function that can handle empty data.env
  V1$math[2] <- "calc_cap,abscap=0.05"
  V1 <- set_name(V1)
  V1$vcom_num <- 1
  cmd_string <- paste0("com.env$v.com$",V1$var_name," <- V1")
  eval(parse(text=cmd_string))
  com.env$var_names <- V1$var_name  #first var in v.com
  #V1 <- add_vd(V1)
  com.env$predict.ret <- V1$var_name #always first variable [hard coded when loading model]
  com.env$predict.clu <- V1$clu
  print(paste("In define_predict_return: com.env$predict.ret",com.env$predict.ret))
}

#set up rnd_parms.R [parms controlling how to randomly create and modify vars]
#set up sample vars in rnd.env$vs.com for import into com.env$v.com as needed
sample_vars <- function() {
  print("Setting up sample vars in rnd.env$vs.com")
  source("rnd_parms.R")          #set rnd_parms.R in rnd.env
  
  #set up sample vars, prices, returns, volumes
  #FUTURE: TI
  V1 <- NULL
  V1$use <- "def"
  V1$calc_etf <- TRUE
  V1$type <- "prc"
  V1$math[1] <- "from.data.env,'Adjusted'"
  V1 <- set_name(V1)
  cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
  eval(parse(text=cmd_string))
  
  V1 <- NULL
  V1$type <- "prc"
  V1$requires <- NULL
  V1$use <- "def"
  V1$calc_etf <- TRUE
  V1$math[1] <- "from.data.env,'H'"
  V1 <- set_name(V1)
  cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
  eval(parse(text=cmd_string))
  
  V1 <- NULL
  V1$type <- "prc"
  V1$requires <- NULL
  V1$use <- "def"
  V1$calc_etf <- TRUE
  V1$math[1] <- "from.data.env,'L'"
  V1 <- set_name(V1)
  cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
  eval(parse(text=cmd_string))
  
  V1 <- NULL
  V1$type <- "prc"
  V1$requires <- NULL
  V1$use <- "def"
  V1$calc_etf <- TRUE
  V1$math[1] <- "from.data.env,'R'"
  #V1$math[1] <- "calc_math,c('H','L','C'),'XX0N <- (XX1*XX2*XX3)^(1/3)'"
  V1 <- set_name(V1)
  cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
  eval(parse(text=cmd_string))
  
  V1 <- NULL
  V1$type <- "prc"
  V1$requires <- NULL
  V1$use <- "def"
  V1$calc_etf <- TRUE
  V1$math[1] <- "from.data.env,'J'"
  #V1$math[1] <- "calc_math,c('H','L'),'XX0N <- sqrt(XX1*XX2)'"
  V1 <- set_name(V1)
  cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
  eval(parse(text=cmd_string))
  
  V1 <- NULL
  V1$type <- "prc"
  V1$requires <- NULL
  V1$use <- "def"
  V1$calc_etf <- TRUE
  V1$math[1] <- "from.data.env,'O'"
  V1 <- set_name(V1)
  cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
  eval(parse(text=cmd_string))
  
  V1 <- NULL
  V1$type <- "prc"
  V1$requires <- NULL
  V1$use <- "def"
  V1$calc_etf <- TRUE
  V1$math[1] <- "from.data.env,'Adjusted'"
  V1$math[2] <- "calc_lag,1"
  V1 <- set_name(V1)
  cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
  eval(parse(text=cmd_string))
  
  V1 <- NULL
  V1$type <- "prc"
  V1$requires <- NULL
  V1$use <- "def"
  V1$calc_etf <- TRUE
  V1$math[1] <- "from.data.env,'H'"
  V1$math[2] <- "calc_lag,1"
  V1 <- set_name(V1)
  cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
  eval(parse(text=cmd_string))
  
  V1 <- NULL
  V1$type <- "prc"
  V1$requires <- NULL
  V1$use <- "def"
  V1$calc_etf <- TRUE
  V1$math[1] <- "from.data.env,'L'"
  V1$math[2] <- "calc_lag,1"
  V1 <- set_name(V1)
  cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
  eval(parse(text=cmd_string))
  
  V1 <- NULL
  V1$type <- "prc"
  V1$requires <- NULL
  V1$use <- "def"
  V1$calc_etf <- TRUE
  V1$math[1] <- "from.data.env,'R'"
  V1$math[2] <- "calc_lag,1"
  V1 <- set_name(V1)
  cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
  eval(parse(text=cmd_string))
  
  V1 <- NULL
  V1$type <- "prc"
  V1$requires <- NULL
  V1$use <- "def"
  V1$calc_etf <- TRUE
  V1$math[1] <- "from.data.env,'J'"
  V1$math[2] <- "calc_lag,1"
  V1 <- set_name(V1)
  cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
  eval(parse(text=cmd_string))
  
  V1 <- NULL
  V1$type <- "prc"
  V1$requires <- NULL
  V1$use <- "def"
  V1$calc_etf <- TRUE
  V1$math[1] <- "from.data.env,'O'"
  V1$math[2] <- "calc_lag,1"
  V1 <- set_name(V1)
  cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
  eval(parse(text=cmd_string))
  
  V1 <- NULL
  V1$type <- "prc"
  V1$requires <- NULL
  V1$use <- "def"
  V1$calc_etf <- TRUE
  V1$math[1] <- "from.data.env,'Adjusted'"
  V1$math[2] <- "calc_lag,2"
  V1 <- set_name(V1)
  cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
  eval(parse(text=cmd_string))
  
  #set up returns from prices
  max_j <- length(rnd.env$vs.com)
  for (i in 1:6) {
    vd_end_price <- rnd.env$vs.com[[i]]
    for (j in (i+1):max_j) {
      V1 <- NULL
      V1$use <- "def"
      V1$calc_etf <- TRUE
      vd_start_price <- rnd.env$vs.com[[j]]
      V1$type <- "ret"
      V1$requires <- unique(c(vd_end_price$requires,vd_start_price$requires,vd_start_price$name,vd_end_price$name))
      V1$math[1] <- paste0("calc_ret,'",vd_start_price$name,"','",vd_end_price$name,"'")
      V1$math[2] <- "calc_cap,abscap=0.08"     #hardcoded, may wish to revisit
      V1 <- set_name(V1)
      cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
      eval(parse(text=cmd_string))
    }
  }
  rm(vd_end_price,vd_start_price)
  rnd.env$ret_list <- (max_j+1):length(rnd.env$vs.com)

  #yesterday unadjusted close
  V1 <- NULL
  V1$type <- "prc"
  V1$requires <- NULL
  V1$use <- "def"
  V1$calc_etf <- TRUE
  V1$math[1] <- "from.data.env,'Close'"
  V1$math[2] <- "calc_lag,1"
  #V1$var_name <- "uB"
  V1 <- set_name(V1)
  cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
  eval(parse(text=cmd_string))
    
  #set up Dollar (D) and log Dollar (V) volume
  V1 <- NULL
  V1$requires <- NULL
  V1$type <- "vol"
  V1$use <- "def"
  V1$calc_etf <- TRUE
  V1$math[1] <- "from.data.env,'D'"
  V1$math[2] <- "calc_cap,cap_pct=0.005"       #hardcoded, may wish to revisit
  V1 <- set_name(V1)
  cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
  eval(parse(text=cmd_string))
  
  V1 <- NULL
  V1$requires <- NULL
  V1$type <- "vol"
  V1$use <- "def"
  V1$calc_etf <- TRUE
  V1$math[1] <- "from.data.env,'V'"
  #V1$math[1] <- "calc_math,c('D'),math_str='XX0N <- log(XX1) - 18.5'"
  V1$math[2] <- "calc_cap,cap_pct=0.005"       #hardcoded, may wish to revisit
  V1 <- set_name(V1)
  cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
  eval(parse(text=cmd_string))

  #yesterday's shout
  V1 <- NULL
  V1$type <- "sdata"
  V1$requires <- NULL
  V1$use <- "def"
  V1$calc_etf <- TRUE
  V1$math[1] <- "from.data.env,'shout'"
  V1$math[2] <- "calc_lag,1"
  #V1$var_name <- "o"
  V1 <- set_name(V1)
  cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
  eval(parse(text=cmd_string))

  #yesterday's mcap in $
  V1 <- NULL
  V1$type <- "sdata"
  V1$requires <- c("ol1","uB")
  V1$use <- "def"
  V1$calc_etf <- TRUE
  V1$math[1] <- "calc_math,c('ol1','uB'),math_str='XX0 <- XX1*XX2'"
  #V1$var_name <- "M"
  V1 <- set_name(V1)
  cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
  eval(parse(text=cmd_string))

  #yesterday's div
  V1 <- NULL
  V1$type <- "sdata"
  V1$requires <- NULL
  V1$use <- "def"
  V1$calc_etf <- TRUE
  V1$math[1] <- "from.data.env,'div'"
  V1$math[2] <- "calc_lag,1"
  #V1$var_name <- "dv"
  V1 <- set_name(V1)
  cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
  eval(parse(text=cmd_string))

  #yesterday's price
  V1 <- NULL
  V1$type <- "sdata"
  V1$requires <- c("B")
  V1$use <- "def"
  V1$calc_etf <- TRUE
  V1$math[1] <- "calc_math,c('B'),math_str='XX0 <- log(XX1)'"
  #V1$var_name <- "lp"
  V1 <- set_name(V1)
  cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
  eval(parse(text=cmd_string))
  
  #set up CC3 for use as template for CC# raws
  V1 <- NULL
  V1$col <- 1
  V1$requires <- NULL
  V1$type <- "ret"
  V1$use <- "def"
  V1$calc_etf <- TRUE
  V1$math[1] <- "calc_look_forward,3"  #3 used as place holder, when choosing 3 is randomly chosen
  V1 <- set_name(V1)
  cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
  eval(parse(text=cmd_string))
  
  #calculate true range for TI calcs
  V1 <- NULL
  V1$type <- "rng"
  V1$requires <- c('H','L','C','B')
  V1$use <- "raw"
  V1$calc_etf <- TRUE
  V1$math[1] <- "calc_math,c('H','L','B'),'XX0N <- pmax(log(XX1/XX2),abs(log(XX1/XX3)),abs(log(XX2/XX3)))'"
  #V1 <- set_name(V1)
  V1$var_name <- "tr"
  cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
  eval(parse(text=cmd_string))
  rnd.env$raw_list <- c(rnd.env$raw_list,length(rnd.env$vs.com))
  
  V1 <- NULL
  #directional movement
  V1$type <- "ti"
  V1$requires <- c('H','G','GH','L','K','KL')
  V1$use <- "raw"
  V1$calc_etf <- TRUE
  V1$math[1] <- "calc_dm,'GH','KL'"
  V1 <- set_name(V1)
  cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
  eval(parse(text=cmd_string))

  V1 <- NULL
  #directional indicator
  V1$type <- "ti"
  V1$requires <- c('H','G','GH','L','K','KL','C','B')
  V1$use <- "raw"
  V1$calc_etf <- TRUE
  V1$math[1] <- "calc_math,c('DMd','TRd'),'XX0N <- XX1/XX2'" #DMd and TRd likely not correct. COME BACK
  V1 <- set_name(V1)
  cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
  eval(parse(text=cmd_string))

  V1 <- NULL
  #money flow
  V1$type <- "ti"
  V1$requires <- c('H','L','C','R','Q','QR','D')
  V1$use <- "raw"
  V1$calc_etf <- TRUE
  V1$math[1] <- "calc_math,c('QR','D'),math_str='XX0N <- ifelse(XX1>0,XX2,0)'"
  #V1 <- set_name(V1)
  V1$var_name <- "mf"
  cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
  eval(parse(text=cmd_string))

  V1 <- NULL
  V1$type <- "constant"
  V1$use <- "data"
  V1$calc_etf <- TRUE
  V1$math[1] <- "calc_constant,1"
  V1$scale_type <- "constant"
  V1 <- set_name(V1)
  cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
  eval(parse(text=cmd_string))
  
  V1 <- NULL
  #Force Index = return * Dollars
  V1$type <- "ti"
  V1$requires <- c('C','B','BC','D')
  V1$use <- "raw"
  V1$calc_etf <- TRUE
  V1$math[1] <- "calc_math,c('BC','D'),math_str='XX0N <- XX1*XX2'"
  #V1 <- set_name(V1)
  V1$var_name <- "fi"
  cmd_string <- paste0("rnd.env$vs.com$",V1$var_name," <- V1")   
  eval(parse(text=cmd_string))
  
}

