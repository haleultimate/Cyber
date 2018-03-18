#rnd_parms.R
# list of settings for constructing and modifying variables
rnd.env <- new.env(parent=globalenv())
rnd.env$vs.com <- NULL

#set up lists for naming
rnd.env$nameID <- c(53,54,55,56,57,58,59,60,61,62,63)
names(rnd.env$nameID) <- c('ld','tr','di','pmf','nmf','fi','ti','pdm','ndm','dv','lp')

# rnd.env$fun_id <- c(1:10)  #any undefined function will be mapped to zero
# names(rnd.env$fun_id) <- c('calc_cap','calc_cap_x','calc_z','calc_z_x','calc_rank','calc_rank_x','calc_stk','calc_decay','calc_vlty','calc_bin')
# rnd.env$known_mod_fun <- c(1:9)
# names(rnd.env$known_mod_fun) <- c('calc_cap','calc_cap_x','calc_z','calc_z_x','calc_rank','calc_rank_x','calc_decay','calc_lag','calc_bin')

#set up constants  #no leading zeros on numeric parameters (lengthens IDs)
rnd.env$mod.model <- c("fve","ia","bin","decay","constant")

rnd.env$raws <- c("sdata","ret","BC","V","C2C","TI")
rnd.env$C2Clag_list <- c(3,5,8,13,21,34,55,89,144)

rnd.env$ti_type_list <- c("fi","mf","tr","dm","di")

rnd.env$resid_list <- c("W","S","E")  #[raw, stock, ETF]

rnd.env$ia_list <- c("mul","div","add","sub","rsh","fth","none")

rnd.env$bin_point.zlist <- c(-4,-3.5,-3,-2.5,-2,-1.5,-1,-.5,0,.5,1,1.5,2,2.5,3,3.5,4)
rnd.env$bin_point.rlist <- c(.01,.02,.03,.05,.08,.13,.21,.34,.5,.66,.79,.87,.92,.95,.97,.98,.99)
if (length(rnd.env$bin_point.zlist) != length(rnd.env$bin_point.rlist)) {
  print("MUST keep zlist and rlist same length for conversion purposes")
  source("close_session.R")
}

rnd.env$decay_list <- c(.89,.55,.34,.21,.13,.08,.05,.03,.02,.01)
rnd.env$vlty_list <- c("v30","v60","v90","v120")

rnd.env$cap_dim_list <- c('calc_cap','calc_cap_x')  #historic or cross-sectional
rnd.env$cap_type_list <- c('cap_pct','zcap','abscap','none')
rnd.env$cap_pct_list <- c(.001,.005,.01,.02,.03,.05,.08,.13,.21,.34)
rnd.env$zcap_list <- c(.5,1,1.5,2,2.5,3,3.5,4,4.5,5.)

rnd.env$calc_list <- c('log','exp','pow','none')
rnd.env$pow_list <- c(.25,.33,.5,.75,1.25,1.5,2,2.5,3)

rnd.env$scale_list <- c('Z','z','r','Zx','zx','rx','none') # [Zscore,zscale,rank]
rnd.env$sdata_list <- c('shout','mcap','div','log_price')

#set up var creation probabilities

#raws
if (com.env$sdata_available) {
  rnd.env$p$raw <- c(0.05,0.3,0.2,0.3,0.2,0.1)
} else {
  rnd.env$p$raw <- c(0,0.3,0.2,0.3,0.2,0.1)
}
names(rnd.env$p$raw) <- rnd.env$raws    #("sdata","ret","BC","V","C2C","TI")

rnd.env$p$sdata_type <- c(0.1,0.1,0,0.05)
names(rnd.env$p$sdata_type) <- rnd.env$sdata_list #('shout','mcap','div','log_price')

rnd.env$p$ti_type <- c(0.1,0.1,0.02,0,0)
names(rnd.env$p$ti_type) <- rnd.env$ti_type_list    #("fi","mf","tr","dm","di")

rnd.env$p$C2Clag <- rep(0.1,times=length(rnd.env$C2Clag_list))
names(rnd.env$p$C2Clag) <- rnd.env$C2Clag_list

#resids  [raw,stock,ETF]  [W,S,E]
rnd.env$p$ret.resid <- c(0.3,0.3,0.1)
names(rnd.env$p$ret.resid) <- rnd.env$resid_list

rnd.env$p$BC.resid <- c(0.3,0.3,0.1)
names(rnd.env$p$BC.resid) <- rnd.env$resid_list

rnd.env$p$V.resid <- c(0.5,0.1,0.1)
names(rnd.env$p$V.resid) <- rnd.env$resid_list

rnd.env$p$C2C.resid <- c(0.3,0.3,0.1)
names(rnd.env$p$C2C.resid) <- rnd.env$resid_list

rnd.env$p$TI.resid <- c(0.5,0.1,0)
names(rnd.env$p$TI.resid) <- rnd.env$resid_list

#decays
rnd.env$p$ret.d <- c(0.1,5,rep(0.1,times=length(rnd.env$decay_list)),rep(0.1,times=length(rnd.env$vlty_list)))
names(rnd.env$p$ret.d) <- c(2,1,rnd.env$decay_list,rnd.env$vlty_list)

rnd.env$p$BC.d <- c(0.1,0.3,rep(0.1,times=length(rnd.env$decay_list)),rep(0.2,times=length(rnd.env$vlty_list)))
names(rnd.env$p$BC.d) <- c(2,1,rnd.env$decay_list,rnd.env$vlty_list)

#rnd.env$p$V.d <- c(rep(0.1,8),0.6,rep(0.1,times=length(rnd.env$decay_list)))
#names(rnd.env$p$V.d) <- c(53,34,21,13,8,5,3,2,1,rnd.env$decay_list)  # moving averages >2
rnd.env$p$V.d <- c(0.1,0.6,rep(0.1,times=length(rnd.env$decay_list)))
names(rnd.env$p$V.d) <- c(2,1,rnd.env$decay_list)  # moving averages removed, calc_ma not working

rnd.env$p$C2C.d <- c(0.1,5)
names(rnd.env$p$C2C.d) <- c(2,1)

rnd.env$p$TI.d <- c(0.1,5,rep(0.1,times=length(rnd.env$decay_list)))
names(rnd.env$p$TI.d) <- c(2,1,rnd.env$decay_list)

rnd.env$p$sdata.d <- c(1)
names(rnd.env$p$sdata.d) <- c(0)  #stock data should already be lagged by a day

rnd.env$p$model.d <- c(0.5,rep(0.1,times=length(rnd.env$decay_list)))
names(rnd.env$p$model.d) <- c('none',rnd.env$decay_list)

#dollar volume -> turn_over switch
rnd.env$p$to <- c(0.5,0.5)
names(rnd.env$p$to) <- c(TRUE,FALSE)

#caps ('cap_pct','zcap','abscap','none')
rnd.env$p$cap_dim <- c(0.5,0.5)
names(rnd.env$p$cap_dim) <- rnd.env$cap_dim_list  #calc_cap or calc_cap_x
rnd.env$p$cap_type <- c(0.4,0.2,0.,0.3)
names(rnd.env$p$cap_type) <- rnd.env$cap_type_list

rnd.env$p$cap_pct <- rep(0.1,times=length(rnd.env$cap_pct_list))
names(rnd.env$p$cap_pct) <- rnd.env$cap_pct_list

rnd.env$p$zcap <- rep(0.1,times=length(rnd.env$zcap_list))
names(rnd.env$p$zcap) <- rnd.env$zcap_list

#calcs
rnd.env$p$calc <- c(0.1,0.1,0.2,8)  # 5% chance of using calc
names(rnd.env$p$calc) <- rnd.env$calc_list

rnd.env$p$pow <- rep(0.1,times=length(rnd.env$pow_list))
names(rnd.env$p$pow) <- rnd.env$pow_list

#scale
rnd.env$p$scale <- c(0.1,0.1,0.1,0.4,0.4,0.4,0) # must scale, cross-sectional scaling preferred
names(rnd.env$p$scale) <- rnd.env$scale_list

#calc var mod, used in mod_fve, mod_ia, mod_bin
rnd.env$p$scale_var_mod <- c(0.1,0.1,0)
names(rnd.env$p$scale_var_mod) <- c("existing_scale_var","modify_scale_var","new_scale_var")

#model interaction calcs
rnd.env$p$ia_type <- c(0.1,0.1,0.1,0.1,1,0.1,3.5)       #c("mul","div","add","sub","rsh","fth","none")
names(rnd.env$p$ia_type) <- rnd.env$ia_list

rnd.env$p$mod_ia <- c(0.1,0.1,0.1,0)
names(rnd.env$p$mod_ia) <- c("delete","sign","ia_type","scale_var")

#bin
rnd.env$p$bin <- c(.5,.5)                               #when creating model variable, chance to bin
names(rnd.env$p$bin) <- c("bin","none")

rnd.env$p$bin_points.z <- rep(0.1,times=length(rnd.env$bin_point.zlist))
names(rnd.env$p$bin_points.z) <- rnd.env$bin_point.zlist                  

rnd.env$p$bin_points.r <- rep(0.1,times=length(rnd.env$bin_point.rlist))
names(rnd.env$p$bin_points.r) <- rnd.env$bin_point.rlist                  

rnd.env$p$delete_bin <- c(0.1,0.9)                      #when modding bin variable, chance to delete bins
names(rnd.env$p$delete_bin) <- c("delete","keep")

rnd.env$p$mod_bin <- c(0.5,0.5)  #selected after delete choice (if delete is an option)
names(rnd.env$p$mod_bin) <- c("bin_points","scale_var")

rnd.env$p$model_start <- c(0.1,0.9)                        #when creating model variable, chance to use intercept and bin
names(rnd.env$p$model_start) <- c("constant","scale_var")

#mod model var
rnd.env$p$ia_bin <- c(0.5,.5,.5,.5,.1)
names(rnd.env$p$ia_bin) <- rnd.env$mod.model   #("fve","ia","bin","decay","constant")

rnd.env$p$ia <- c(0.5,.5,0,.5,0)
names(rnd.env$p$ia) <- rnd.env$mod.model

rnd.env$p$bin <- c(0.5,0,.5,.5,.1)
names(rnd.env$p$bin) <- rnd.env$mod.model

rnd.env$p$fve <- c(0.5,0,0,1,0)
names(rnd.env$p$fve) <- rnd.env$mod.model

#set up var mod probabilities
rnd.env$p$mod_use <- c(1,0)
names(rnd.env$p$mod_use) <- c("model","scale")

#legacy parms below
rnd.env$prob$choices <- c('type','cap','math','scale','decay','bin')
rnd.env$prob$choices.wts <- c(0.1,0.3,0.0,0.3,0.2,0.4) 
rnd.env$prob$choices.bv <- c('type','cap','scale','decay')
rnd.env$prob$choices.bv.wts <- c(0.1,0.2,0.,0.2)
rnd.env$prob$raw_var <- c('retrange','ccd','c2c','ti')
rnd.env$prob$raw_var.wts <- c(0.4,0.2,0.2,0.2)
#rnd.env$prob$raw_var.wts <- c(0.4,0.3,0.3,0)
rnd.env$prob$raw_var.c2c.lags <- c(3,5,8,13,21,34,55,89,144)
rnd.env$prob$raw_var.ti.type <- c('adx','mf','fi')
rnd.env$prob$raw_var.ti.wts <- c(0.3,0.5,0.2)
rnd.env$prob$type <- c('ret','stk','etf','vlt','vol','vrs','file') #keep 'file' hard coded as last entry
rnd.env$prob$type.wts <- c(0.9,0.9,0.1,0.3,0.3,0.1,0.9)
rnd.env$prob$type.bv <- rnd.env$prob$type
rnd.env$prob$type.bv.wts <- c(0.1,0.1,0.1,0.1,0.1,0.1,0)
rnd.env$prob$cap <- c('cap_pct','zcap','abscap','none')
rnd.env$prob$cap.wts <- c(0.4,0.2,0.,0.3)
rnd.env$prob$abscap <- c(.13,.08,.05,.03,.02,.01,.005,.003)
rnd.env$prob$abscap.wts <- rep(0.1,times=length(rnd.env$prob$abscap))
rnd.env$prob$cap_pct <- c(.001,.005,.01,.02,.03,.05,.08,.13)
rnd.env$prob$cap_pct.wts <- rep(0.1,times=length(rnd.env$prob$cap_pct))
rnd.env$prob$zcap <- c(.5,1.5,2,2.5,3,3.5,4,5)
rnd.env$prob$zcap.wts <- rep(0.1,times=length(rnd.env$prob$zcap))
rnd.env$prob$math <- c('power','log','exp','none')
rnd.env$prob$math.wts <- c(.03,.02,.01,.94)
rnd.env$prob$scale <- c('zscore','zscale','none')
rnd.env$prob$scale.wts <- c(0.1,0.5,0.2)
rnd.env$prob$decay <- c(2,1,.89,.55,.34,.21,.13,.08,.05,.03,.02,.01)
rnd.env$prob$decay.wts <- c(0.05,0.5,rep(0.05,times=(length(rnd.env$prob$decay)-2)))
rnd.env$prob$bin <- c('new_var','none')
rnd.env$prob$bin.wts <- c(0.75,0.25)
rnd.env$prob$bin_pt1 <- c(-3,-2,-1,0,1) 
rnd.env$prob$bin_pt1.wts <- c(0.1,0.1,0.1,0.1,0.1)
rnd.env$prob$bin_pt2 <- c(-1,0,1,2,3) 
rnd.env$prob$bin_pt2.wts <- c(0.1,0.1,0.1,0.1,0.1)
rnd.env$prob$mod_bin <- c('new_var','none')
rnd.env$prob$mod_bin.wts <- c(0.8,0.2)

rnd.env$mod$bins <- c(-4,-3.5,-3,-2.5,-2,-1.5,-1,-0.5,0,0.5,1,1.5,2,2.5,3,3.5,4)
rnd.env$mod$abscap <- c(.13,.08,.05,.03,.02,.01,.005,.003) #assumed to be returns
rnd.env$mod$cap_pct <- c(.001,.005,.01,.02,.03,.05,.08,.13,.21,.34)
rnd.env$mod$zcap <- c(0.5,1,1.5,2,2.5,3,3.5,4,4.5,5.)
rnd.env$mod$decay <- c(.89,.55,.34,.21,.13,.08,.05,.03,.02,.01)


#currently being coded
rnd.env$prob$choices <- c('type','cap','scale','decay','bin')


                            