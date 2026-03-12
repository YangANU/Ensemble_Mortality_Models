#################
# Main functions
#################

## This R script contains the main functions for computing point and interval forecasts.
## Source this script before doing computation.

# Load required R packages

require(StMoMo)
library(h2o)
library(shapley)
library(demography)


# General computation functions

standardise = function(x){(x-min(x))/(max(x)-min(x))}


Information_Criteria <- function(residuals_mat, K)
{
  n = length(as.numeric(residuals_mat))
  RSS = sum(residuals_mat^2, na.rm = TRUE)
  
  AIC_out = n*log(RSS/n) + 2*K
  BIC_out = n*log(RSS/n) + log(n)*K
  
  return(list(AIC = AIC_out, BIC = BIC_out))
}


mse <- function(forecast, true)
{
  if (length(forecast) != length(true)) 
    stop("MSE: the lengths of input vectors must be the same.")
  
  # ignore inf values (useful for log mortality)
  
  if(sum(is.infinite(true)) > 0)
  {
    err = mean((true[!is.infinite(true)] - forecast[!is.infinite(true)])^2)
  } else {
    err = mean((na.omit(as.numeric(true - forecast)))^2)
  }
  
  return(err)
}


mae <- function(forecast, true)
{
  if (length(forecast) != length(true)) 
    stop("MSE: the lengths of input vectors must be the same.")
  
  # ignore inf values (useful for log mortality)
  
  if(sum(is.infinite(true)) > 0)
  {
    err = mean(abs(true[!is.infinite(true)] - forecast[!is.infinite(true)]))
  } else {
    err = mean(abs(na.omit(as.numeric(true - forecast))))
  }
  
  return(err)
}


f2 <- function(x, ages) mean(ages) - x


f3 <- function(x, ages) pmax(mean(ages)-x,0)


constPlat <- function(ax, bx, kt, b0x, gc, wxt, ages)
{
  nYears <- dim(wxt)[2]
  x <- ages
  t <- 1:nYears
  c <- (1 - tail(ages, 1)):(nYears - ages[1])
  xbar <- mean(x)
  #\sum g(c)=0, \sum cg(c)=0, \sum c^2g(c)=0
  phiReg <- lm(gc ~ 1 + c + I(c^2), na.action = na.omit)
  phi <- coef(phiReg)
  gc <- gc - phi[1] - phi[2] * c - phi[3] * c^2
  kt[2, ] <- kt[2, ] + 2 * phi[3] * t
  kt[1, ] <- kt[1, ] + phi[2] * t + phi[3] * (t^2 - 2 * xbar * t)
  ax <- ax + phi[1] - phi[2] * x + phi[3] * x^2
  #\sum kt[i, ] = 0
  ci <- rowMeans(kt, na.rm = TRUE)
  ax <- ax + ci[1] + ci[2] * (xbar - x) + ci[3] * pmax(xbar - x, 0)
  kt[1, ] <- kt[1, ] - ci[1]
  kt[2, ] <- kt[2, ] - ci[2]
  kt[3, ] <- kt[3, ] - ci[3]
  list(ax = ax, bx = bx, kt = kt, b0x = b0x, gc = gc)
}


forecast_m_manual = function(obs, gender = "female", h)
{
  fdm_obs = fdm(obs, series = gender, method = "M")
  
  beta_forecast = matrix(NA, nrow = h, ncol = ncol(fdm_obs$coeff))
  for(ij in 1:ncol(fdm_obs$coeff))
  {
    beta_forecast[,ij] = forecast(auto.arima(fdm_obs$coeff[, ij]), h = h)$mean
  }
  
  forecast_rate = exp(fdm_obs$basis %*% t(beta_forecast)) 
  
  return(forecast_rate)
}


# Define a function for computing point forecasts.

err_fun_forecast_modified = function(index, state_select, state_select_smooth)
{
  n_year = length(get(state_select[index])$year)
  last_year = get(state_select[index])$year[n_year]
  
  ################################################
  # Lee-Carter, Renshaw-Haberman, APC, M6, M7, M8
  ################################################
  
  train_lc_sum_female   = train_lc_sum_male = 
    train_rh_female  = train_rh_male = 
    train_apc_female = train_apc_male =
    train_cbd_female = train_cbd_male = 
    train_m6_female  = train_m6_male = 
    train_m7_female  = train_m7_male = 
    train_m8_female  = train_m8_male = 
    train_plat_female = train_plat_male = array(NA, dim = c(101,10,10))
  
  
  AIC_lc_sum_female   = AIC_lc_sum_male = 
    AIC_rh_female  = AIC_rh_male = 
    AIC_apc_female = AIC_apc_male =
    AIC_cbd_female = AIC_cbd_male = 
    AIC_m6_female  = AIC_m6_male = 
    AIC_m7_female  = AIC_m7_male = 
    AIC_m8_female  = AIC_m8_male = 
    AIC_plat_female = AIC_plat_male = rep(0, 10)
  
  BIC_lc_sum_female   = BIC_lc_sum_male = 
    BIC_rh_female  = BIC_rh_male = 
    BIC_apc_female = BIC_apc_male =
    BIC_cbd_female = BIC_cbd_male = 
    BIC_m6_female  = BIC_m6_male = 
    BIC_m7_female  = BIC_m7_male = 
    BIC_m8_female  = BIC_m8_male = 
    BIC_plat_female = BIC_plat_male = rep(0, 10)
  
  
  for(ik in 1:10)
  {
    obs = extract.years(get(state_select[index]), (get(state_select[index])$year[1]):(get(state_select[index])$year[n_year-11]+ik))
    
    # replace mortality rate greater than 1 by NA
    
    obs$rate$female = replace(obs$rate$female, which(obs$rate$female > 1), NA)
    obs$rate$male   = replace(obs$rate$male,   which(obs$rate$male > 1),   NA)
    
    if(any(!is.finite(obs$rate$female)))
    {
      ind = which(!is.finite(obs$rate$female))
      obs$rate$female = matrix(na.interp(replace(as.numeric(obs$rate$female), ind, NA)), 101, ncol(obs$rate$female))
    }
    
    if(any(!is.finite(obs$rate$male)))
    {
      ind = which(!is.finite(obs$rate$male))
      obs$rate$male = matrix(na.interp(replace(as.numeric(obs$rate$male), ind, NA)), 101, ncol(obs$rate$male))
    }
    
    ### Lee-Carter ###
    
    wxt <- genWeightMat(ages = 0:100, years = obs$year, clip = 3)
    
    ## female
    
    # sum(kt) = sum
    
    LC1 = lc(const = "sum")
    LCfit1_female = fit(LC1, data = StMoMoData(obs, series = "female", type = "central"),
                        wxt = wxt, verbose = FALSE)
    train_lc_sum_female[,,ik] = forecast(LCfit1_female, h = 10)$rate
    
    IC_temp = Information_Criteria(residuals_mat = LCfit1_female$fittingModel$residuals, K = 3)
    AIC_lc_sum_female[ik] = IC_temp$AIC
    BIC_lc_sum_female[ik] = IC_temp$BIC
    print("LCfit1_female complete")
    rm(LCfit1_female); rm(IC_temp)
    
    ## male         
    
    # sum(kt) = sum
    
    LCfit1_male = fit(LC1, data = StMoMoData(obs, series = "male", type = "central"),
                      wxt = wxt, verbose = FALSE)
    train_lc_sum_male[,,ik] = forecast(LCfit1_male, h = 10)$rate
    IC_temp = Information_Criteria(residuals_mat = LCfit1_male$fittingModel$residuals, K = 3)
    AIC_lc_sum_male[ik] = IC_temp$AIC
    BIC_lc_sum_male[ik] = IC_temp$BIC
    print("LCfit1_male complete")
    rm(LCfit1_male)
    
    ### Renshaw-Haberman ###
    
    # female
    
    LCfit_female <-  fit(lc(), data = StMoMoData(obs, series = "female", type = "central"), verbose = FALSE)
    RHfit_female <- fit(rh(), data = StMoMoData(obs, series = "female", type = "central"), 
                        wxt = wxt, start.ax = LCfit_female$ax,
                        start.bx = LCfit_female$bx, start.kt = LCfit_female$kt, verbose = FALSE)
    dum_val = try(forecast(RHfit_female, h = 10)$rate, silent = TRUE)
    if(class(dum_val)[1] == "try-error")
    {
      train_rh_female[,,ik] = rep(NA, 101)
      AIC_rh_female[ik] = NA
      BIC_rh_female[ik] = NA
      
    } else
    {
      train_rh_female[,,ik] = dum_val
      IC_temp = Information_Criteria(residuals_mat = RHfit_female$fittingModel$residuals, K = 5)
      AIC_rh_female[ik] = IC_temp$AIC
      BIC_rh_female[ik] = IC_temp$BIC
    }
    print("RHfit_female complete")
    rm(LCfit_female); rm(RHfit_female); rm(dum_val); rm(IC_temp)
    
    # male
    
    LCfit_male = fit(lc(), data = StMoMoData(obs, series = "male", type = "central"), verbose = FALSE)
    RHfit_male = fit(rh(), data = StMoMoData(obs, series = "male", type = "central"), 
                     wxt = wxt, start.ax = LCfit_male$ax,
                     start.bx = LCfit_male$bx, start.kt = LCfit_male$kt, verbose = FALSE)
    dum_val = try(forecast(RHfit_male, h = 10)$rate, silent = TRUE)
    if(class(dum_val)[1] == "try-error")
    {
      train_rh_male[,,ik] = rep(NA, 101)
      AIC_rh_male[ik] = NA
      BIC_rh_male[ik] = NA
    } else
    {
      train_rh_male[,,ik] = dum_val
      IC_temp = Information_Criteria(residuals_mat = RHfit_male$fittingModel$residualss, K = 5)
      AIC_rh_male[ik] = IC_temp$AIC
      BIC_rh_male[ik] = IC_temp$BIC
    }
    print("RHfit_male complete")
    rm(LCfit_male); rm(RHfit_male); rm(IC_temp)
    
    ### Age-Period-Cohort ###
    
    # female
    
    APCfit_female <- fit(apc(), data = StMoMoData(obs, series = "female", type = "central"), 
                         wxt = wxt, verbose = FALSE)
    train_apc_female[,,ik] = forecast(APCfit_female, h = 10)$rate
    IC_temp = Information_Criteria(residuals_mat = APCfit_female$fittingModel$residuals, K = 3)
    AIC_apc_female[ik] = IC_temp$AIC
    BIC_apc_female[ik] = IC_temp$BIC
    print("APCfit_female complete")
    rm(APCfit_female); rm(IC_temp)
    
    # male
    
    APCfit_male = fit(apc(), data = StMoMoData(obs, series = "male", type = "central"), 
                      wxt = wxt, verbose = FALSE)
    train_apc_male[,,ik] = forecast(APCfit_male, h = 10)$rate
    IC_temp = Information_Criteria(residuals_mat = APCfit_male$fittingModel$residuals, K = 3)
    AIC_apc_male[ik] = IC_temp$AIC
    BIC_apc_male[ik] = IC_temp$BIC
    print("APCfit_male complete")
    rm(APCfit_male); rm(IC_temp)
    
    ### CBD ###
    
    # female
    
    CBDfit_female = fit(cbd(link = "log"), data = StMoMoData(obs, series = "female", type = "initial"), 
                        wxt = wxt, verbose = FALSE)
    train_cbd_female[,,ik] = forecast(CBDfit_female, h = 10)$rate
    IC_temp = Information_Criteria(residuals_mat = CBDfit_female$fittingModel$residuals, K = 2)
    AIC_cbd_female[ik] = IC_temp$AIC
    BIC_cbd_female[ik] = IC_temp$BIC
    print("CBDfit_female complete")
    rm(CBDfit_female); rm(IC_temp)
    
    # male
    
    CBDfit_male = fit(cbd(link = "log"), data = StMoMoData(obs, series = "male", type = "initial"), 
                      wxt = wxt, verbose = FALSE)
    train_cbd_male[,,ik] = forecast(CBDfit_male, h = 10)$rate
    IC_temp = Information_Criteria(residuals_mat = CBDfit_male$fittingModel$residuals, K = 2)
    AIC_cbd_male[ik] = IC_temp$AIC
    BIC_cbd_male[ik] = IC_temp$BIC
    print("CBDfit_male complete")
    rm(CBDfit_male); rm(IC_temp)
    
    ### M6 ###
    
    # female
    
    M6fit_female <- fit(m6(link = "log"), data = StMoMoData(obs, series = "female", type = "initial"), 
                        wxt = wxt, verbose = FALSE)
    dum_val = try(forecast(M6fit_female, h = 10)$rate, silent = TRUE)
    if(class(dum_val)[1] == "try-error")
    {
      train_m6_female[,,ik] = rep(NA, 101)
      AIC_m6_female[ik] = NA
      BIC_m6_female[ik] = NA
    } else
    {
      train_m6_female[,,ik] = dum_val
      
      IC_temp = Information_Criteria(residuals_mat = M6fit_female$fittingModel$residuals, K = 3)
      AIC_m6_female[ik] = IC_temp$AIC
      BIC_m6_female[ik] = IC_temp$BIC
    }
    print("M6fit_female complete")
    rm(M6fit_female); rm(dum_val); rm(IC_temp)
    
    # male
    
    M6fit_male = fit(m6(link = "log"), data = StMoMoData(obs, series = "male", type = "initial"), 
                     wxt = wxt, verbose = FALSE)
    dum_val = try(forecast(M6fit_male, h = 10)$rate, silent = TRUE)
    if(class(dum_val)[1] == "try-error")
    {
      train_m6_male[,,ik] = rep(NA, 101)
      AIC_m6_male[ik] = NA
      BIC_m6_male[ik] = NA
    } else
    {
      train_m6_male[,,ik] = dum_val
      IC_temp = Information_Criteria(residuals_mat = M6fit_male$fittingModel$residuals, K = 3)
      AIC_m6_male[ik] = IC_temp$AIC
      BIC_m6_male[ik] = IC_temp$BIC
    }
    print("M6fit_male complete")
    rm(M6fit_male); rm(dum_val); rm(IC_temp)
    
    ### M7 ###
    
    # female
    
    M7fit_female <- fit(m7(link = "log"), data = StMoMoData(obs, series = "female", type = "initial"), 
                        wxt = wxt, verbose = FALSE)
    dum_val = try(forecast(M7fit_female, h = 10)$rate, silent = TRUE)
    if(class(dum_val)[1] == "try-error")
    {
      train_m7_female[,,ik] = rep(NA, 101)
      AIC_m7_female[ik] = NA
      BIC_m7_female[ik] = NA
    } else
    {
      train_m7_female[,,ik] = dum_val
      IC_temp = Information_Criteria(residuals_mat = M7fit_female$fittingModel$residuals, K = 4)
      AIC_m7_female[ik] = IC_temp$AIC
      BIC_m7_female[ik] = IC_temp$BIC
    }
    print("M7fit_female complete")
    rm(M7fit_female); rm(dum_val); rm(IC_temp)
    
    # male
    
    M7fit_male = fit(m7(link = "log"), data = StMoMoData(obs, series = "male", type = "initial"), 
                     wxt = wxt, verbose = FALSE)
    dum_val = try(forecast(M7fit_male, h = 10)$rate, silent = TRUE)
    if(class(dum_val)[1] == "try-error")
    {
      train_m7_male[,,ik] = rep(NA, 101)
      AIC_m7_male[ik] = NA
      BIC_m7_male[ik] = NA
    } else
    {
      train_m7_male[,,ik] = dum_val
      IC_temp = Information_Criteria(residuals_mat = M7fit_male$fittingModel$residuals, K = 4)
      AIC_m7_male[ik] = IC_temp$AIC
      BIC_m7_male[ik] = IC_temp$BIC
    }
    print("M7fit_male complete")
    rm(M7fit_male); rm(dum_val); rm(IC_temp)
    
    ### M8 ###
    
    # female
    
    M8fit_female = fit(m8(link = "log", xc = 110), data = StMoMoData(obs, series = "female", type = "initial"), 
                       wxt = wxt, verbose = FALSE)
    dum_val = try(forecast(M8fit_female, h = 10)$rate, silent = TRUE)        
    if(class(dum_val)[1] == "try-error")
    {
      train_m8_female[,,ik] = rep(NA, 101)
      AIC_m8_female[ik] = NA
      BIC_m8_female[ik] = NA
    } else
    {
      train_m8_female[,,ik] = dum_val
      IC_temp = Information_Criteria(residuals_mat = M8fit_female$fittingModel$residuals, K = 3)
      AIC_m8_female[ik] = IC_temp$AIC
      BIC_m8_female[ik] = IC_temp$BIC
    }
    print("M8fit_female complete")
    rm(M8fit_female); rm(dum_val); rm(IC_temp)
    
    # male
    
    M8fit_male = fit(m8(link = "log", xc = 110), data = StMoMoData(obs, series = "male", type = "initial"), 
                     wxt = wxt, verbose = FALSE)
    dum_val = try(forecast(M8fit_male, h = 10)$rate, silent = TRUE)        
    if(class(dum_val)[1] == "try-error")
    {
      train_m8_male[,,ik] = rep(NA, 101)
      AIC_m8_male[ik] = NA
      BIC_m8_male[ik] = NA
    } else
    {
      train_m8_male[,,ik] = dum_val
      IC_temp = Information_Criteria(residuals_mat = M8fit_male$fittingModel$residuals, K = 3)
      AIC_m8_male[ik] = IC_temp$AIC
      BIC_m8_male[ik] = IC_temp$BIC
    }
    print("M8fit_male complete")
    rm(M8fit_male); rm(dum_val); rm(IC_temp)
    
    ### Plat ###
    
    # female
    
    PLAT <- StMoMo(link = "log", staticAgeFun = TRUE,
                   periodAgeFun = c("1", f2, f3), cohortAgeFun = "1",
                   constFun = constPlat)
    PLATfit_female <- fit(PLAT, data = StMoMoData(obs, series = "female", type = "central"), 
                          wxt = wxt, verbose = FALSE)
    train_plat_female[,,ik] = forecast(PLATfit_female, h = 10)$rate
    IC_temp = Information_Criteria(residuals_mat = PLATfit_female$fittingModel$residuals, K = 4)
    AIC_plat_female[ik] = IC_temp$AIC
    BIC_plat_female[ik] = IC_temp$BIC
    print("PLATfit_female complete")
    rm(PLATfit_female); rm(IC_temp)
    
    # male
    
    PLATfit_male = fit(PLAT, data = StMoMoData(obs, series = "male", type = "central"),
                       wxt = wxt, verbose = FALSE)
    train_plat_male[,,ik] = forecast(PLATfit_male, h = 10)$rate
    IC_temp = Information_Criteria(residuals_mat = PLATfit_male$fittingModel$residuals, K = 4)
    AIC_plat_male[ik] = IC_temp$AIC
    BIC_plat_male[ik] = IC_temp$BIC
    print("PLATfit_male complete")
    rm(PLATfit_male); rm(PLAT); rm(IC_temp)
    
    print(paste("LC&RH&M", ik, sep = " "))
    
    
    gc()
    
    rm(ik)
  }
  
  ###############
  # Compute MSE
  ###############
  
  train_lc_sum_female_mse   = train_lc_sum_male_mse   = 
    train_rh_female_mse       = train_rh_male_mse =     
    train_apc_female_mse      = train_apc_male_mse = 
    train_cbd_female_mse      = train_cbd_male_mse = 
    train_m6_female_mse       = train_m6_male_mse = 
    train_m7_female_mse       = train_m7_male_mse = 
    train_m8_female_mse       = train_m8_male_mse = 
    train_plat_female_mse     = train_plat_male_mse = vector("numeric", 10)
  for(ik in 1:10)
  {
    # female
    
    train_lc_sum_female_mse[ik]   = mse(log(train_lc_sum_female[,ik,1:(11-ik)]),  log(extract.years(get(state_select[index]), years = (get(state_select[index])$year[n_year-10]+ik):last_year)$rate$female))
    train_rh_female_mse[ik]       = mse(log(train_rh_female[,ik,1:(11-ik)]),      log(extract.years(get(state_select[index]), years = (get(state_select[index])$year[n_year-10]+ik):last_year)$rate$female))
    train_apc_female_mse[ik]      = mse(log(train_apc_female[,ik,1:(11-ik)]),     log(extract.years(get(state_select[index]), years = (get(state_select[index])$year[n_year-10]+ik):last_year)$rate$female))
    train_cbd_female_mse[ik]      = mse(log(train_cbd_female[,ik,1:(11-ik)]),     log(extract.years(get(state_select[index]), years = (get(state_select[index])$year[n_year-10]+ik):last_year)$rate$female))
    train_m6_female_mse[ik]       = mse(log(train_m6_female[,ik,1:(11-ik)]),      log(extract.years(get(state_select[index]), years = (get(state_select[index])$year[n_year-10]+ik):last_year)$rate$female))
    train_m7_female_mse[ik]       = mse(log(train_m7_female[,ik,1:(11-ik)]),      log(extract.years(get(state_select[index]), years = (get(state_select[index])$year[n_year-10]+ik):last_year)$rate$female))
    train_m8_female_mse[ik]       = mse(log(train_m8_female[,ik,1:(11-ik)]),      log(extract.years(get(state_select[index]), years = (get(state_select[index])$year[n_year-10]+ik):last_year)$rate$female))
    train_plat_female_mse[ik]     = mse(log(train_plat_female[,ik,1:(11-ik)]),    log(extract.years(get(state_select[index]), years = (get(state_select[index])$year[n_year-10]+ik):last_year)$rate$female))
    
    # male
    
    train_lc_sum_male_mse[ik]   = mse(log(train_lc_sum_male[,ik,1:(11-ik)]),   log(extract.years(get(state_select[index]), years = (get(state_select[index])$year[n_year-10]+ik):last_year)$rate$male))
    train_rh_male_mse[ik]       = mse(log(train_rh_male[,ik,1:(11-ik)]),       log(extract.years(get(state_select[index]), years = (get(state_select[index])$year[n_year-10]+ik):last_year)$rate$male))
    train_apc_male_mse[ik]      = mse(log(train_apc_male[,ik,1:(11-ik)]),      log(extract.years(get(state_select[index]), years = (get(state_select[index])$year[n_year-10]+ik):last_year)$rate$male))
    train_cbd_male_mse[ik]      = mse(log(train_cbd_male[,ik,1:(11-ik)]),      log(extract.years(get(state_select[index]), years = (get(state_select[index])$year[n_year-10]+ik):last_year)$rate$male))
    train_m6_male_mse[ik]       = mse(log(train_m6_male[,ik,1:(11-ik)]),       log(extract.years(get(state_select[index]), years = (get(state_select[index])$year[n_year-10]+ik):last_year)$rate$male))
    train_m7_male_mse[ik]       = mse(log(train_m7_male[,ik,1:(11-ik)]),       log(extract.years(get(state_select[index]), years = (get(state_select[index])$year[n_year-10]+ik):last_year)$rate$male))
    train_m8_male_mse[ik]       = mse(log(train_m8_male[,ik,1:(11-ik)]),       log(extract.years(get(state_select[index]), years = (get(state_select[index])$year[n_year-10]+ik):last_year)$rate$male))
    train_plat_male_mse[ik]     = mse(log(train_plat_male[,ik,1:(11-ik)]),     log(extract.years(get(state_select[index]), years = (get(state_select[index])$year[n_year-10]+ik):last_year)$rate$male))
  }        
  
  ######################################
  # Lee-Carter using demography package
  ######################################
  
  train_lca_dt_female = train_lca_dxt_female = train_lca_e0_female = train_lca_none_female = 
    train_lca_dt_male   = train_lca_dxt_male   = train_lca_e0_male   = train_lca_none_male   = array(NA, dim = c(101,10,10))
  
  AIC_lca_dt_female = AIC_lca_dxt_female = AIC_lca_e0_female = AIC_lca_none_female = 
    AIC_lca_dt_male = AIC_lca_dxt_male   = AIC_lca_e0_male   = AIC_lca_none_male = 
    BIC_lca_dt_female = BIC_lca_dxt_female = BIC_lca_e0_female = BIC_lca_none_female = 
    BIC_lca_dt_male   = BIC_lca_dxt_male   = BIC_lca_e0_male   = BIC_lca_none_male = rep(0, 10)
  
  for(ik in 1:10)
  {
    obs = extract.years(get(state_select[index]), (get(state_select[index])$year[1]):(get(state_select[index])$year[n_year-11]+ik))
    
    # replace mortality rate greater than 1 by NA
    
    obs$rate$female = replace(obs$rate$female, which(obs$rate$female > 1), NA)
    obs$rate$male   = replace(obs$rate$male,   which(obs$rate$male > 1),   NA)
    
    if(any(!is.finite(obs$rate$female)))
    {
      ind = which(!is.finite(obs$rate$female))
      obs$rate$female = matrix(na.interp(replace(as.numeric(obs$rate$female), ind, NA)), 101, ncol(obs$rate$female))
    }
    
    if(any(!is.finite(obs$rate$male)))
    {
      ind = which(!is.finite(obs$rate$male))
      obs$rate$male = matrix(na.interp(replace(as.numeric(obs$rate$male), ind, NA)), 101, ncol(obs$rate$male))
    }
    
    
    # female
    
    ## lca_dt
    lca_dt_female = lca(obs, series = "female", adjust = "dt", interpolate = TRUE)
    train_lca_dt_female[,,ik]   = forecast(lca_dt_female,  h = 10)$rate$female
    IC_lca_dt = Information_Criteria(residuals_mat = lca_dt_female$residuals$y, K = 3)
    AIC_lca_dt_female[ik] = IC_lca_dt$AIC
    BIC_lca_dt_female[ik] = IC_lca_dt$BIC
    
    ## lca_dxt
    lca_dxt_female = lca(obs, series = "female", adjust = "dxt", interpolate = TRUE)
    train_lca_dxt_female[,,ik]  = forecast(lca_dxt_female, h = 10)$rate$female
    IC_lca_dxt = Information_Criteria(residuals_mat = lca_dxt_female$residuals$y, K = 3)
    AIC_lca_dxt_female[ik] = IC_lca_dxt$AIC
    BIC_lca_dxt_female[ik] = IC_lca_dxt$BIC
    
    ## lca_e0
    lca_e0_female = lca(obs, series = "female", adjust = "e0", interpolate = TRUE)
    train_lca_e0_female[,,ik]   = forecast(lca_e0_female, h = 10)$rate$female
    IC_lca_e0 = Information_Criteria(residuals_mat = lca_e0_female$residuals$y, K = 3)
    AIC_lca_e0_female[ik] = IC_lca_e0$AIC
    BIC_lca_e0_female[ik] = IC_lca_e0$BIC
    
    ## lca_none
    lca_none_female = lca(obs, series = "female", adjust = "none", interpolate = TRUE)
    train_lca_none_female[,,ik] = forecast(lca_none_female, h = 10)$rate$female
    IC_lca_none = Information_Criteria(residuals_mat = lca_none_female$residuals$y, K = 3)
    AIC_lca_none_female[ik] = IC_lca_none$AIC
    BIC_lca_none_female[ik] = IC_lca_none$BIC
    
    
    # male
    
    ## lca_dt
    lca_dt_male = lca(obs, series = "male", adjust = "dt", interpolate = TRUE)
    train_lca_dt_male[,,ik]   = forecast(lca_dt_male,  h = 10)$rate$male
    IC_lca_dt = Information_Criteria(residuals_mat = lca_dt_male$residuals$y, K = 3)
    AIC_lca_dt_male[ik] = IC_lca_dt$AIC
    BIC_lca_dt_male[ik] = IC_lca_dt$BIC
    
    ## lca_dxt
    lca_dxt_male = lca(obs, series = "male", adjust = "dxt", interpolate = TRUE)
    train_lca_dxt_male[,,ik]  = forecast(lca_dxt_male, h = 10)$rate$male
    IC_lca_dxt = Information_Criteria(residuals_mat = lca_dxt_male$residuals$y, K = 3)
    AIC_lca_dxt_male[ik] = IC_lca_dxt$AIC
    BIC_lca_dxt_male[ik] = IC_lca_dxt$BIC
    
    ## lca_e0
    lca_e0_male = lca(obs, series = "male", adjust = "e0", interpolate = TRUE)
    train_lca_e0_male[,,ik]   = forecast(lca_e0_male, h = 10)$rate$male
    IC_lca_e0 = Information_Criteria(residuals_mat = lca_e0_male$residuals$y, K = 3)
    AIC_lca_e0_male[ik] = IC_lca_e0$AIC
    BIC_lca_e0_male[ik] = IC_lca_e0$BIC
    
    ## lca_none
    lca_none_male = lca(obs, series = "male", adjust = "none", interpolate = TRUE)
    train_lca_none_male[,,ik] = forecast(lca_none_male, h = 10)$rate$male
    IC_lca_none = Information_Criteria(residuals_mat = lca_none_male$residuals$y, K = 3)
    AIC_lca_none_male[ik] = IC_lca_none$AIC
    BIC_lca_none_male[ik] = IC_lca_none$BIC
    
    print(paste("LC", ik, sep = " "))
    rm(ik); rm(obs)
  }  
  
  train_lca_dt_female_mse = train_lca_dxt_female_mse = 
    train_lca_e0_female_mse = train_lca_none_female_mse = 
    train_lca_dt_male_mse = train_lca_dxt_male_mse = 
    train_lca_e0_male_mse = train_lca_none_male_mse = vector("numeric", 10)
  for(ik in 1:10)
  {
    # female
    
    train_lca_dt_female_mse[ik]   = mse(log(train_lca_dt_female[,ik,1:(11-ik)]),   log(extract.years(get(state_select[index]), years = (get(state_select[index])$year[n_year-10]+ik):last_year)$rate$female))
    train_lca_dxt_female_mse[ik]  = mse(log(train_lca_dxt_female[,ik,1:(11-ik)]),  log(extract.years(get(state_select[index]), years = (get(state_select[index])$year[n_year-10]+ik):last_year)$rate$female))
    train_lca_e0_female_mse[ik]   = mse(log(train_lca_e0_female[,ik,1:(11-ik)]),   log(extract.years(get(state_select[index]), years = (get(state_select[index])$year[n_year-10]+ik):last_year)$rate$female))
    train_lca_none_female_mse[ik] = mse(log(train_lca_none_female[,ik,1:(11-ik)]), log(extract.years(get(state_select[index]), years = (get(state_select[index])$year[n_year-10]+ik):last_year)$rate$female))
    
    # male
    
    train_lca_dt_male_mse[ik]   = mse(log(train_lca_dt_male[,ik,1:(11-ik)]),   log(extract.years(get(state_select[index]), years = (get(state_select[index])$year[n_year-10]+ik):last_year)$rate$male))
    train_lca_dxt_male_mse[ik]  = mse(log(train_lca_dxt_male[,ik,1:(11-ik)]),  log(extract.years(get(state_select[index]), years = (get(state_select[index])$year[n_year-10]+ik):last_year)$rate$male))
    train_lca_e0_male_mse[ik]   = mse(log(train_lca_e0_male[,ik,1:(11-ik)]),   log(extract.years(get(state_select[index]), years = (get(state_select[index])$year[n_year-10]+ik):last_year)$rate$male))
    train_lca_none_male_mse[ik] = mse(log(train_lca_none_male[,ik,1:(11-ik)]), log(extract.years(get(state_select[index]), years = (get(state_select[index])$year[n_year-10]+ik):last_year)$rate$male))
  }
  
  ########################
  # Functional data model
  ########################
  
  train_fdm_female = train_fdm_male = train_robust_fdm_female = train_robust_fdm_male = 
    train_pr_female = train_pr_male = array(NA, dim = c(101,10,10))
  
  
  AIC_fdm_female = AIC_fdm_male = AIC_robust_fdm_female = AIC_robust_fdm_male = AIC_pr_female = AIC_pr_male =
    BIC_fdm_female = BIC_fdm_male = BIC_robust_fdm_female = BIC_robust_fdm_male = BIC_pr_female = BIC_pr_male = rep(0, 10)
  
  for(ik in 1:10)
  {
    obs = extract.years(get(state_select_smooth[index]), (get(state_select_smooth[index])$year[1]):(get(state_select[index])$year[n_year-11]+ik))
    
    # replace mortality rate greater than 1 by NA
    
    obs$rate$female = replace(obs$rate$female, which(obs$rate$female > 1), NA)
    obs$rate$male   = replace(obs$rate$male,   which(obs$rate$male > 1),   NA)
    
    if(any(!is.finite(obs$rate$female)))
    {
      ind = which(!is.finite(obs$rate$female))
      obs$rate$female = matrix(na.interp(replace(as.numeric(obs$rate$female), ind, NA)), 101, ncol(obs$rate$female))
    }
    
    if(any(!is.finite(obs$rate$male)))
    {
      ind = which(!is.finite(obs$rate$male))
      obs$rate$male = matrix(na.interp(replace(as.numeric(obs$rate$male), ind, NA)), 101, ncol(obs$rate$male))
    }
    
    ## fdm
    
    # female
    fdm_female = fdm(obs, series = "female", method = "classical")
    train_fdm_female[,,ik] = forecast(fdm_female, h = 10)$rate$female
    IC_fdm = Information_Criteria(residuals_mat = fdm_female$residuals$y, K = 6)
    AIC_fdm_female[ik] = IC_fdm$AIC
    BIC_fdm_female[ik] = IC_fdm$BIC
    
    # male
    fdm_male = fdm(obs, series = "male", method = "classical")
    train_fdm_male[,,ik] = forecast(fdm_male, h = 10)$rate$male
    IC_fdm = Information_Criteria(residuals_mat = fdm_male$residuals$y, K = 6)
    AIC_fdm_male[ik] = IC_fdm$AIC
    BIC_fdm_male[ik] = IC_fdm$BIC
    
    ## robust_fdm
    
    # female
    robust_fdm_female = fdm(obs, series = "female", method = "M")
    train_robust_fdm_female[,,ik] = forecast_m_manual(obs = obs, "female", h = 10)
    IC_robust_fdm = Information_Criteria(residuals_mat = robust_fdm_female$residuals$y, K = 6)
    AIC_robust_fdm_female[ik] = IC_robust_fdm$AIC
    BIC_robust_fdm_female[ik] = IC_robust_fdm$BIC
    
    # male
    robust_fdm_male = fdm(obs, series = "male", method = "M")
    train_robust_fdm_male[,,ik] = forecast_m_manual(obs = obs, "male", h = 10)
    IC_robust_fdm = Information_Criteria(residuals_mat = robust_fdm_male$residuals$y, K = 6)
    AIC_robust_fdm_male[ik] = IC_robust_fdm$AIC
    BIC_robust_fdm_male[ik] = IC_robust_fdm$BIC
    
    
    ## product-ratio
    
    pr_fdm = coherentfdm(obs)
    train_coherent_fdm = forecast(pr_fdm, h = 10)
    train_pr_female[,,ik] = train_coherent_fdm$female$rate$female
    train_pr_male[,,ik] = train_coherent_fdm$male$rate$male
    
    
    residuals_pr_female = train_coherent_fdm$female$model$female - train_coherent_fdm$female$fitted$y
    IC_pr = Information_Criteria(residuals_mat = residuals_pr_female, K = 12)
    AIC_pr_female[ik] = IC_pr$AIC
    BIC_pr_female[ik] = IC_pr$BIC
    
    residuals_pr_male = train_coherent_fdm$male$model$male - train_coherent_fdm$male$fitted$y
    IC_pr = Information_Criteria(residuals_mat = residuals_pr_male, K = 12)
    AIC_pr_male[ik] = IC_pr$AIC
    BIC_pr_male[ik] = IC_pr$BIC
    
    
    print(paste("Functional", ik, sep = " "))
    rm(ik); rm(obs)
  }    
  
  train_fdm_female_mse = train_fdm_male_mse = 
    train_robust_fdm_female_mse = train_robust_fdm_male_mse = 
    train_pr_female_mse = train_pr_male_mse = vector("numeric", 10)
  for(ik in 1:10)
  {
    # classical
    
    train_fdm_female_mse[ik] = mse(log(train_fdm_female[,ik,1:(11-ik)]), log(extract.years(get(state_select[index]), years = (get(state_select[index])$year[n_year-10]+ik):last_year)$rate$female))
    train_fdm_male_mse[ik] = mse(log(train_fdm_male[,ik,1:(11-ik)]),   log(extract.years(get(state_select[index]), years = (get(state_select[index])$year[n_year-10]+ik):last_year)$rate$male))
    
    # M
    
    train_robust_fdm_female_mse[ik] = mse(log(train_robust_fdm_female[,ik,1:(11-ik)]), log(extract.years(get(state_select[index]), years = (get(state_select[index])$year[n_year-10]+ik):last_year)$rate$female))
    train_robust_fdm_male_mse[ik] = mse(log(train_robust_fdm_male[,ik,1:(11-ik)]),   log(extract.years(get(state_select[index]), years = (get(state_select[index])$year[n_year-10]+ik):last_year)$rate$male))
    
    # product-ratio
    
    train_pr_female_mse[ik] = mse(log(train_pr_female[,ik,1:(11-ik)]), log(extract.years(get(state_select[index]), years = (get(state_select[index])$year[n_year-10]+ik):last_year)$rate$female))
    train_pr_male_mse[ik] = mse(log(train_pr_male[,ik,1:(11-ik)]), log(extract.years(get(state_select[index]), years = (get(state_select[index])$year[n_year-10]+ik):last_year)$rate$male))
  }
  
  ##########
  # summary
  ##########
  
  # female (errors & forecasts & AIC & BIC)
  
  train_female_mse = cbind(train_lc_sum_female_mse,     train_rh_female_mse,
                           train_apc_female_mse,        train_cbd_female_mse,
                           train_m6_female_mse,         train_m7_female_mse,
                           train_m8_female_mse,         train_plat_female_mse,
                           train_lca_dt_female_mse,     train_lca_dxt_female_mse,    
                           train_lca_e0_female_mse,     train_lca_none_female_mse,   
                           train_fdm_female_mse,        train_robust_fdm_female_mse, 
                           train_pr_female_mse) 
  
  train_female_AIC = cbind(AIC_lc_sum_female,     AIC_rh_female,
                           AIC_apc_female,        AIC_cbd_female,
                           AIC_m6_female,         AIC_m7_female,
                           AIC_m8_female,         AIC_plat_female,
                           AIC_lca_dt_female,     AIC_lca_dxt_female,    
                           AIC_lca_e0_female,     AIC_lca_none_female,   
                           AIC_fdm_female,        AIC_robust_fdm_female, 
                           AIC_pr_female) 
  
  train_female_BIC = cbind(BIC_lc_sum_female,     BIC_rh_female,
                           BIC_apc_female,        BIC_cbd_female,
                           BIC_m6_female,         BIC_m7_female,
                           BIC_m8_female,         BIC_plat_female,
                           BIC_lca_dt_female,     BIC_lca_dxt_female,    
                           BIC_lca_e0_female,     BIC_lca_none_female,   
                           BIC_fdm_female,        BIC_robust_fdm_female, 
                           BIC_pr_female) 
  
  
  train_female_fore = list()
  train_female_fore[[1]] = train_lc_sum_female
  train_female_fore[[2]] = train_rh_female
  train_female_fore[[3]] = train_apc_female
  train_female_fore[[4]] = train_cbd_female
  train_female_fore[[5]] = train_m6_female
  train_female_fore[[6]] = train_m7_female
  train_female_fore[[7]] = train_m8_female
  train_female_fore[[8]] = train_plat_female
  train_female_fore[[9]] = train_lca_dt_female
  train_female_fore[[10]] = train_lca_dxt_female
  train_female_fore[[11]] = train_lca_e0_female
  train_female_fore[[12]] = train_lca_none_female
  train_female_fore[[13]] = train_fdm_female 
  train_female_fore[[14]] = train_robust_fdm_female
  train_female_fore[[15]] = train_pr_female
  
  # male (errors & forecasts & AIC & BIC)
  
  train_male_mse = cbind(train_lc_sum_male_mse,     train_rh_male_mse,
                         train_apc_male_mse,        train_cbd_male_mse,
                         train_m6_male_mse,         train_m7_male_mse,
                         train_m8_male_mse,         train_plat_male_mse,
                         train_lca_dt_male_mse,     train_lca_dxt_male_mse,    
                         train_lca_e0_male_mse,     train_lca_none_male_mse,   
                         train_fdm_male_mse,        train_robust_fdm_male_mse, 
                         train_pr_male_mse) 
  
  train_male_AIC = cbind(AIC_lc_sum_male,     AIC_rh_male,
                         AIC_apc_male,        AIC_cbd_male,
                         AIC_m6_male,         AIC_m7_male,
                         AIC_m8_male,         AIC_plat_male,
                         AIC_lca_dt_male,     AIC_lca_dxt_male,    
                         AIC_lca_e0_male,     AIC_lca_none_male,   
                         AIC_fdm_male,        AIC_robust_fdm_male, 
                         AIC_pr_male) 
  
  train_male_BIC = cbind(BIC_lc_sum_male,     BIC_rh_male,
                         BIC_apc_male,        BIC_cbd_male,
                         BIC_m6_male,         BIC_m7_male,
                         BIC_m8_male,         BIC_plat_male,
                         BIC_lca_dt_male,     BIC_lca_dxt_male,    
                         BIC_lca_e0_male,     BIC_lca_none_male,   
                         BIC_fdm_male,        BIC_robust_fdm_male, 
                         BIC_pr_male) 
  
  train_male_fore = list()
  train_male_fore[[1]] = train_lc_sum_male
  train_male_fore[[2]] = train_rh_male
  train_male_fore[[3]] = train_apc_male
  train_male_fore[[4]] = train_cbd_male
  train_male_fore[[5]] = train_m6_male
  train_male_fore[[6]] = train_m7_male
  train_male_fore[[7]] = train_m8_male
  train_male_fore[[8]] = train_plat_male
  train_male_fore[[9]] = train_lca_dt_male
  train_male_fore[[10]] = train_lca_dxt_male
  train_male_fore[[11]] = train_lca_e0_male
  train_male_fore[[12]] = train_lca_none_male
  train_male_fore[[13]] = train_fdm_male 
  train_male_fore[[14]] = train_robust_fdm_male
  train_male_fore[[15]] = train_pr_male
  
  colnames(train_male_mse) = colnames(train_female_mse) = c("lc_sum", "rh", "apc", "cbd",
                                                            "m6", "m7", "m8", "plat", "dt", "dxt", "e0", "none", 
                                                            "fdm", "M_fdm", "pr")
  rownames(train_male_mse) = rownames(train_female_mse) = 1:10
  return(list(train_female_fore = train_female_fore, train_male_fore = train_male_fore,
              train_female_AIC = train_female_AIC, train_female_BIC = train_female_BIC,
              train_male_AIC = train_male_AIC, train_male_BIC = train_male_BIC,
              train_female_mse = train_female_mse, train_male_mse = train_male_mse))
}

# Define a function for arranging forecasts into a dataframe

combine_df_train_OECD <- function(age, index)
{
  # fore_year: the forecast horizon h (1 <= h <= 20) 
  # testing data: last twenty years' observations
  
  list_female = list_male = list()
  
  for(fore_year in 1:10)
  {
    year_start = get(OECD_train[index])$year[which(get(OECD_train[index])$year == tail(get(OECD_train[index])$year, 11)[1])]
    year_end = tail(get(OECD_train[index])$year, 1)
    
    ## female series
    obs_female = extract.ages(extract.years(get(OECD_train[index]), (year_start+fore_year):year_end), ages = age, combine.upper = FALSE)$rate$female
    
    list_female[[fore_year]] = cbind(as.numeric(obs_female), 
                                     as.numeric(get(OECD_fore_train[index])$train_female_fore[[1]][age+1,fore_year,1:(11-fore_year)]),  # age starts from 0
                                     as.numeric(get(OECD_fore_train[index])$train_female_fore[[2]][age+1,fore_year,1:(11-fore_year)]),
                                     as.numeric(get(OECD_fore_train[index])$train_female_fore[[3]][age+1,fore_year,1:(11-fore_year)]),
                                     as.numeric(get(OECD_fore_train[index])$train_female_fore[[4]][age+1,fore_year,1:(11-fore_year)]),
                                     as.numeric(get(OECD_fore_train[index])$train_female_fore[[5]][age+1,fore_year,1:(11-fore_year)]),
                                     as.numeric(get(OECD_fore_train[index])$train_female_fore[[6]][age+1,fore_year,1:(11-fore_year)]),
                                     as.numeric(get(OECD_fore_train[index])$train_female_fore[[7]][age+1,fore_year,1:(11-fore_year)]),
                                     as.numeric(get(OECD_fore_train[index])$train_female_fore[[8]][age+1,fore_year,1:(11-fore_year)]),
                                     as.numeric(get(OECD_fore_train[index])$train_female_fore[[9]][age+1,fore_year,1:(11-fore_year)]),
                                     as.numeric(get(OECD_fore_train[index])$train_female_fore[[10]][age+1,fore_year,1:(11-fore_year)]),
                                     as.numeric(get(OECD_fore_train[index])$train_female_fore[[11]][age+1,fore_year,1:(11-fore_year)]),
                                     as.numeric(get(OECD_fore_train[index])$train_female_fore[[12]][age+1,fore_year,1:(11-fore_year)]),
                                     as.numeric(get(OECD_fore_train[index])$train_female_fore[[13]][age+1,fore_year,1:(11-fore_year)]),
                                     as.numeric(get(OECD_fore_train[index])$train_female_fore[[14]][age+1,fore_year,1:(11-fore_year)]),
                                     as.numeric(get(OECD_fore_train[index])$train_female_fore[[15]][age+1,fore_year,1:(11-fore_year)]))
    
    
    
    ## male series
    obs_male = extract.ages(extract.years(get(OECD_train[index]), (year_start+fore_year):year_end), ages = age, combine.upper = FALSE)$rate$male
    
    list_male[[fore_year]] = cbind(as.numeric(obs_male), 
                                   as.numeric(get(OECD_fore_train[index])$train_male_fore[[1]][age+1,fore_year,1:(11-fore_year)]),
                                   as.numeric(get(OECD_fore_train[index])$train_male_fore[[2]][age+1,fore_year,1:(11-fore_year)]),
                                   as.numeric(get(OECD_fore_train[index])$train_male_fore[[3]][age+1,fore_year,1:(11-fore_year)]),
                                   as.numeric(get(OECD_fore_train[index])$train_male_fore[[4]][age+1,fore_year,1:(11-fore_year)]),
                                   as.numeric(get(OECD_fore_train[index])$train_male_fore[[5]][age+1,fore_year,1:(11-fore_year)]),
                                   as.numeric(get(OECD_fore_train[index])$train_male_fore[[6]][age+1,fore_year,1:(11-fore_year)]),
                                   as.numeric(get(OECD_fore_train[index])$train_male_fore[[7]][age+1,fore_year,1:(11-fore_year)]),
                                   as.numeric(get(OECD_fore_train[index])$train_male_fore[[8]][age+1,fore_year,1:(11-fore_year)]),
                                   as.numeric(get(OECD_fore_train[index])$train_male_fore[[9]][age+1,fore_year,1:(11-fore_year)]),
                                   as.numeric(get(OECD_fore_train[index])$train_male_fore[[10]][age+1,fore_year,1:(11-fore_year)]),
                                   as.numeric(get(OECD_fore_train[index])$train_male_fore[[11]][age+1,fore_year,1:(11-fore_year)]),
                                   as.numeric(get(OECD_fore_train[index])$train_male_fore[[12]][age+1,fore_year,1:(11-fore_year)]),
                                   as.numeric(get(OECD_fore_train[index])$train_male_fore[[13]][age+1,fore_year,1:(11-fore_year)]),
                                   as.numeric(get(OECD_fore_train[index])$train_male_fore[[14]][age+1,fore_year,1:(11-fore_year)]),
                                   as.numeric(get(OECD_fore_train[index])$train_male_fore[[15]][age+1,fore_year,1:(11-fore_year)]))
  }
  
  ## collapse the list into a dataframe
  df_female = do.call(rbind, list_female)
  df_male = do.call(rbind, list_male)
  colnames(df_female) = colnames(df_male) = c("OBS", "lc", "rh", "apc", "cbd", "m6", "m7", "m8", "plat", "lca_dt", "lca_dxt", "lca_e0", "lca_none", "fdm", "robust_fdm", "pr")
  
  ## replace predictions greater than 1 by NA
  df_female[df_female > 1] = NA
  df_male[df_male > 1] = NA
  
  return(list(df_female = df_female, df_male = df_male))
}

combine_df_test_OECD <- function(age, index)
{
  # fore_year: the forecast horizon h (1 <= h <= 20) 
  # testing data: last twenty years' observations
  
  list_female = list_male = list()
  
  for(fore_year in 1:10)
  {
    year_start = get(OECD_test[index])$year[which(get(OECD_test[index])$year == tail(get(OECD_test[index])$year, 11)[1])]
    year_end = tail(get(OECD_test[index])$year, 1)
    
    ## female series
    obs_female = extract.ages(extract.years(get(OECD_test[index]), (year_start+fore_year):year_end), ages = age, combine.upper = FALSE)$rate$female
    
    list_female[[fore_year]] = cbind(as.numeric(obs_female), 
                                     as.numeric(get(OECD_fore_test[index])$train_female_fore[[1]][age+1,fore_year,1:(11-fore_year)]), # age starts from 0
                                     as.numeric(get(OECD_fore_test[index])$train_female_fore[[2]][age+1,fore_year,1:(11-fore_year)]),
                                     as.numeric(get(OECD_fore_test[index])$train_female_fore[[3]][age+1,fore_year,1:(11-fore_year)]),
                                     as.numeric(get(OECD_fore_test[index])$train_female_fore[[4]][age+1,fore_year,1:(11-fore_year)]),
                                     as.numeric(get(OECD_fore_test[index])$train_female_fore[[5]][age+1,fore_year,1:(11-fore_year)]),
                                     as.numeric(get(OECD_fore_test[index])$train_female_fore[[6]][age+1,fore_year,1:(11-fore_year)]),
                                     as.numeric(get(OECD_fore_test[index])$train_female_fore[[7]][age+1,fore_year,1:(11-fore_year)]),
                                     as.numeric(get(OECD_fore_test[index])$train_female_fore[[8]][age+1,fore_year,1:(11-fore_year)]),
                                     as.numeric(get(OECD_fore_test[index])$train_female_fore[[9]][age+1,fore_year,1:(11-fore_year)]),
                                     as.numeric(get(OECD_fore_test[index])$train_female_fore[[10]][age+1,fore_year,1:(11-fore_year)]),
                                     as.numeric(get(OECD_fore_test[index])$train_female_fore[[11]][age+1,fore_year,1:(11-fore_year)]),
                                     as.numeric(get(OECD_fore_test[index])$train_female_fore[[12]][age+1,fore_year,1:(11-fore_year)]),
                                     as.numeric(get(OECD_fore_test[index])$train_female_fore[[13]][age+1,fore_year,1:(11-fore_year)]),
                                     as.numeric(get(OECD_fore_test[index])$train_female_fore[[14]][age+1,fore_year,1:(11-fore_year)]),
                                     as.numeric(get(OECD_fore_test[index])$train_female_fore[[15]][age+1,fore_year,1:(11-fore_year)]))
    
    
    
    ## male series
    obs_male = extract.ages(extract.years(get(OECD_test[index]), (year_start+fore_year):year_end), ages = age, combine.upper = FALSE)$rate$male
    
    list_male[[fore_year]] = cbind(as.numeric(obs_male), 
                                   as.numeric(get(OECD_fore_test[index])$train_male_fore[[1]][age+1,fore_year,1:(11-fore_year)]),
                                   as.numeric(get(OECD_fore_test[index])$train_male_fore[[2]][age+1,fore_year,1:(11-fore_year)]),
                                   as.numeric(get(OECD_fore_test[index])$train_male_fore[[3]][age+1,fore_year,1:(11-fore_year)]),
                                   as.numeric(get(OECD_fore_test[index])$train_male_fore[[4]][age+1,fore_year,1:(11-fore_year)]),
                                   as.numeric(get(OECD_fore_test[index])$train_male_fore[[5]][age+1,fore_year,1:(11-fore_year)]),
                                   as.numeric(get(OECD_fore_test[index])$train_male_fore[[6]][age+1,fore_year,1:(11-fore_year)]),
                                   as.numeric(get(OECD_fore_test[index])$train_male_fore[[7]][age+1,fore_year,1:(11-fore_year)]),
                                   as.numeric(get(OECD_fore_test[index])$train_male_fore[[8]][age+1,fore_year,1:(11-fore_year)]),
                                   as.numeric(get(OECD_fore_test[index])$train_male_fore[[9]][age+1,fore_year,1:(11-fore_year)]),
                                   as.numeric(get(OECD_fore_test[index])$train_male_fore[[10]][age+1,fore_year,1:(11-fore_year)]),
                                   as.numeric(get(OECD_fore_test[index])$train_male_fore[[11]][age+1,fore_year,1:(11-fore_year)]),
                                   as.numeric(get(OECD_fore_test[index])$train_male_fore[[12]][age+1,fore_year,1:(11-fore_year)]),
                                   as.numeric(get(OECD_fore_test[index])$train_male_fore[[13]][age+1,fore_year,1:(11-fore_year)]),
                                   as.numeric(get(OECD_fore_test[index])$train_male_fore[[14]][age+1,fore_year,1:(11-fore_year)]),
                                   as.numeric(get(OECD_fore_test[index])$train_male_fore[[15]][age+1,fore_year,1:(11-fore_year)]))
  }
  
  ## collapse the list into a dataframe
  df_female = do.call(rbind, list_female)
  df_male = do.call(rbind, list_male)
  colnames(df_female) = colnames(df_male) = c("OBS", "lc", "rh", "apc", "cbd", "m6", "m7", "m8", "plat", "lca_dt", "lca_dxt", "lca_e0", "lca_none", "fdm", "robust_fdm", "pr")
  
  ## replace predictions greater than 1 by NA
  df_female[df_female > 1] = NA
  df_male[df_male > 1] = NA
  
  return(list(df_female = df_female, df_male = df_male))
}

# Define functions for remocing NA values and finding the h_index in the forecast dataframe

na_replace <- function(df)
{
  out_df = df
  out_df[is.na(out_df)] = 0
  return(out_df)
}


find_h_index <- function(h)
{
  count = 11 - h
  start = 1 + (h - 1)*(22 - h)/2

  return(start:(start + count - 1))
}

# Define a function for computing age-specific SHAP values

age_specific_SHAP <- function(country_index)
{
  temp_train_female_result = temp_train_male_result = list()
  temp_train_female_grid   = temp_train_male_grid = list()
  
  # initiate the h2o server
  # h2o.init(ignore_config = TRUE, nthreads = 25, insecure = TRUE)
  h2o.init(ignore_config = TRUE, nthreads = 30, insecure = TRUE)
  
  for(ij in 1:101)
  {
    #################
    ## female series
    #################
    
    data_train_female = na_replace(get(OECD_fore_train_df[country_index])[[ij]]$df_female)
    if(all(data_train_female[,"OBS"] == 0))
    {
      next
    }
    # write the training data into a csv file
    temp_file_name_female = paste(OECD_train[country_index], "_age_", ij, "_female", ".csv", sep = "")
	##### Update the file path to the folder containing the data #####
    temp_file_path_female = paste(".../", temp_file_name_female, sep = "")  
    write.csv(data_train_female, file = temp_file_path_female, row.names = FALSE)
    temp_train_female <- h2o.importFile(path = temp_file_path_female, header = TRUE)
    
    # run AutoML to tune various models (GBM) for 60 seconds. make sure equal number of "nfolds" is specified for different grids
    
    y = "OBS"
    temp_train_female_grid[[ij]] = h2o.grid(algorithm = "gbm", y = y, training_frame = temp_train_female,
                                           hyper_params = list(ntrees = seq(1,100,1)),
                                           grid_id = "ensemble_grid",
                                           
                                           # this setting ensures the models are comparable for building a meta learner
                                           seed = 2024, fold_assignment = "Modulo", nfolds = 10,
                                           keep_cross_validation_predictions = TRUE)
    
    
    temp_train_female_result[[ij]] = tryCatch(shapley(temp_train_female_grid[[ij]], newdata = temp_train_female, performance_metric = "r2", plot = FALSE, method = "mean", standardize_performance_metric = TRUE), error = function(e){shapley(temp_train_female_grid[[ij]], newdata = temp_train_female, performance_metric = "r2", plot = FALSE, method = "mean", standardize_performance_metric = TRUE, minimum_performance = -1)})
    
    
    h2o.removeAll()
    gc()
    
    ###############
    ## male series
    ###############
    
    data_train_male = na_replace(get(OECD_fore_train_df[country_index])[[ij]]$df_male)
    if(all(data_train_male[,"OBS"] == 0))
    {
      next
    }
    
    # write the training data into a csv file
    temp_file_name_male = paste(OECD_train[country_index], "_age_", ij, "_male", ".csv", sep = "")
	##### Update the file path to the folder containing the data #####
    temp_file_path_male = paste(".../", temp_file_name_male, sep = "")
    write.csv(data_train_male, file = temp_file_path_male, row.names = FALSE)
    temp_train_male <- h2o.importFile(path = temp_file_path_male, header = TRUE)
    
    # run AutoML to tune various models (GBM) for 60 seconds. make sure equal number of "nfolds" is specified for different grids
    
    y = "OBS"
    temp_train_male_grid[[ij]] = h2o.grid(algorithm = "gbm", y = y, training_frame = temp_train_male,
                                            hyper_params = list(ntrees = seq(1,100,1)),
                                            grid_id = "ensemble_grid",
                                            
                                            # this setting ensures the models are comparable for building a meta learner
                                            seed = 2024, fold_assignment = "Modulo", nfolds = 10,
                                            keep_cross_validation_predictions = TRUE)
    
    temp_train_male_result[[ij]] = tryCatch(shapley(temp_train_male_grid[[ij]], newdata = temp_train_male, performance_metric = "r2", plot = FALSE, method = "mean", standardize_performance_metric = TRUE), error = function(e){shapley(temp_train_male_grid[[ij]], newdata = temp_train_male, performance_metric = "r2", plot = FALSE, method = "mean", standardize_performance_metric = TRUE, minimum_performance = -1)})

    h2o.removeAll()
    gc()
    print(paste("Age ", ij, " Complete", sep = ""))
  }
  return(list(female_result = temp_train_female_result, 
              male_result = temp_train_male_result))
  
}

# Define a function to compute forecasts based on various weighting methods (i.e., Average, SHAP, MSE, AIC)

combine_forecast <- function(country_index, method = c("Average", "SHAP", "MSE", "AIC"), alpha = 0.2)
{
  h_all = 10
  n_age = 101
  
  OBS_female_all = OBS_male_all = weighted_fore_female_all = weighted_fore_male_all = list()
  shap_selected_method_female = shap_selected_method_male = list()
  MSE_female_all = MSE_male_all = MAE_female_all = MAE_male_all = matrix(NA, nrow = n_age, ncol = h_all)
  
  rownames(MSE_female_all) = rownames(MSE_male_all) = rownames(MAE_female_all) = rownames(MAE_male_all) = 0:100
  colnames(MSE_female_all) = colnames(MSE_male_all) = colnames(MAE_female_all) = colnames(MAE_male_all) = paste0("h = ", 1:10)
  
  for(ij in 1:n_age)
  {
    OBS_female = fore_female = OBS_male = fore_male = weighted_fore_female = weighted_fore_male = list()
    MSE_female = MSE_male = MAE_female = MAE_male = list()
    
    for(ih in 1:h_all)
    {
      select_index = find_h_index(ih)
      
      # female
      fore_selected = get(OECD_fore_test_df[country_index])[[ij]]$df_female[select_index,, drop = FALSE]
      OBS_female[[ih]] = fore_selected[, "OBS"]
      fore_female[[ih]] = na_replace(fore_selected[,-1, drop = FALSE])
      
      if(method == "Average")
      {
        weighted_fore_female[[ih]] = rowMeans(fore_female[[ih]], na.rm = TRUE)
      }
      
      
      if(method == "MSE")
      {
        mse_temp = get(OECD_fore_train[country_index])$train_female_mse[ih,, drop = FALSE]
        mse_weight_temp = na_replace(mse_temp/sum(mse_temp, na.rm = TRUE))
        
        weighted_fore_female[[ih]] = as.vector(fore_female[[ih]] %*% t(mse_weight_temp))
        rm(mse_temp, mse_weight_temp)
      }
      
      
      if(method == "AIC")
      {
        AIC_temp = get(OECD_fore_train[country_index])$train_female_AIC[ih,, drop = FALSE]
        AIC_positive = c(which(AIC_temp > 0), which(is.na(AIC_temp)))
        AIC_abs_temp = abs(AIC_temp)
        AIC_abs_temp[AIC_positive] = 0
        AIC_weight_temp = na_replace(AIC_abs_temp/sum(AIC_abs_temp, na.rm = TRUE))
        
        weighted_fore_female[[ih]] = as.vector(fore_female[[ih]] %*% t(AIC_weight_temp))
        rm(AIC_temp, AIC_positive, AIC_abs_temp, AIC_weight_temp)
      }
      
      if(method == "SHAP")
      {
        shap_temp = get(OECD_SHAP[country_index])$female_result[[ij]]$summaryShaps
        shap_selected_temp = shap_temp[which(shap_temp$mean >= alpha),]
        shap_weigtht_temp = shap_selected_temp$mean/sum(shap_selected_temp$mean, na.rm = TRUE)
        shap_selected_method_female[[ij]] = as.vector(shap_selected_temp$feature)
        
        weighted_fore_female[[ih]] = as.vector(fore_female[[ih]][,shap_selected_temp$feature, drop = FALSE] %*% shap_weigtht_temp)
        rm(shap_temp, shap_selected_temp, shap_weigtht_temp)
      }
      
      # male
      fore_selected = get(OECD_fore_test_df[country_index])[[ij]]$df_male[select_index,, drop = FALSE]
      OBS_male[[ih]] = fore_selected[, "OBS"]
      fore_male[[ih]] = na_replace(fore_selected[,-1, drop = FALSE])
      
      if(method == "Average")
      {
        weighted_fore_male[[ih]] = rowMeans(fore_male[[ih]], na.rm = TRUE)
      }
      
      
      if(method == "MSE")
      {
        mse_temp = get(OECD_fore_train[country_index])$train_male_mse[ih,, drop = FALSE]
        mse_weight_temp = na_replace(mse_temp/sum(mse_temp, na.rm = TRUE))
        
        weighted_fore_male[[ih]] = as.vector(fore_male[[ih]] %*% t(mse_weight_temp))
        rm(mse_temp, mse_weight_temp)
      }
      
      
      if(method == "AIC")
      {
        AIC_temp = get(OECD_fore_train[country_index])$train_male_AIC[ih,, drop = FALSE]
        AIC_positive = c(which(AIC_temp > 0), which(is.na(AIC_temp)))
        AIC_abs_temp = abs(AIC_temp)
        AIC_abs_temp[AIC_positive] = 0
        AIC_weight_temp = na_replace(AIC_abs_temp/sum(AIC_abs_temp, na.rm = TRUE))
        
        weighted_fore_male[[ih]] = as.vector(fore_male[[ih]] %*% t(AIC_weight_temp))
        rm(AIC_temp, AIC_positive, AIC_abs_temp, AIC_weight_temp)
      }
      
      if(method == "SHAP")
      {
        shap_temp = get(OECD_SHAP[country_index])$male_result[[ij]]$summaryShaps
        shap_selected_temp = shap_temp[which(shap_temp$mean >= alpha),]
        shap_weigtht_temp = shap_selected_temp$mean/sum(shap_selected_temp$mean)
        shap_selected_method_male[[ij]] = as.vector(shap_selected_temp$feature)
        
        weighted_fore_male[[ih]] = as.vector(fore_male[[ih]][,shap_selected_temp$feature, drop = FALSE] %*% shap_weigtht_temp)
        rm(shap_temp, shap_selected_temp, shap_weigtht_temp)
      }
      
      
      # compute MSE and MAE errors
      
      MSE_female[[ih]] = mse(true = OBS_female[[ih]], forecast = weighted_fore_female[[ih]])
      MSE_male[[ih]] = mse(true = OBS_male[[ih]], forecast = weighted_fore_male[[ih]])
      
      MAE_female[[ih]] = mae(true = OBS_female[[ih]], forecast = weighted_fore_female[[ih]])
      MAE_male[[ih]] = mae(true = OBS_male[[ih]], forecast = weighted_fore_male[[ih]])
    }
    
    
    ## Save combined forecasts
    
    OBS_female_all[[ij]] = OBS_female
    OBS_male_all[[ij]] = OBS_male
    weighted_fore_female_all[[ij]] = weighted_fore_female
    weighted_fore_male_all[[ij]] = weighted_fore_male
    
    ## Save computed errors
    MSE_female_all[ij,] =  unlist(MSE_female)
    MSE_male_all[ij,] =  unlist(MSE_male)
    MAE_female_all[ij,] =  unlist(MAE_female)
    MAE_male_all[ij,] =  unlist(MAE_male)
  }
  
  return(list(OBS_female_all = OBS_female_all,
              OBS_male_all = OBS_male_all,
              weighted_fore_female_all = weighted_fore_female_all,
              weighted_fore_male_all = weighted_fore_male_all,
              SHAP_selected_method_female = shap_selected_method_female,
              SHAP_selected_method_male = shap_selected_method_male,
              MSE_female_all = MSE_female_all,
              MSE_male_all = MSE_male_all,
              MAE_female_all = MAE_female_all,
              MAE_male_all = MAE_male_all,
              combine_method = method,
              alpha = alpha,
              country = OECD_countries[country_index]))
}

# Define a function to evaluate individual benchmark models

individual_eval <- function(country_index)
{
  
  h_all = 10
  n_age = 101
  
  OBS_female_all = OBS_male_all = ind_fore_female_all = ind_fore_male_all = list()
  MSE_female_all = MSE_male_all = MAE_female_all = MAE_male_all = array(NA, dim = c(n_age, h_all, 15))
  rownames(MSE_female_all) = rownames(MSE_male_all) = rownames(MAE_female_all) = rownames(MAE_male_all) = 0:100
  colnames(MSE_female_all) = colnames(MSE_male_all) = colnames(MAE_female_all) = colnames(MAE_male_all) = paste0("h = ", 1:10)
  
  for(ij in 1:n_age)
  {
    
    OBS_female = OBS_male = ind_fore_female = ind_fore_male = list()
    
    MSE_female = MSE_male = MAE_female = MAE_male = list()
    
    for(ih in 1:h_all)
    {
      select_index = find_h_index(ih)
      
      # female
      fore_selected = get(OECD_fore_test_df[country_index])[[ij]]$df_female[select_index,, drop = FALSE]
      OBS_female[[ih]] = fore_selected[, "OBS"]
      ind_fore_female[[ih]]  = fore_selected[,-1, drop = FALSE]

      # male
      fore_selected = get(OECD_fore_test_df[country_index])[[ij]]$df_male[select_index,, drop = FALSE]
      OBS_male[[ih]] = fore_selected[, "OBS"]
      ind_fore_male[[ih]] = fore_selected[,-1, drop = FALSE]
      
      # compute MSE and MAE errors
      
      MSE_temp_female = MSE_temp_male = MAE_temp_female = MAE_temp_male = data.frame(matrix(rep(0, 15), ncol = 15, byrow = TRUE))
      colnames(MSE_temp_female) = colnames(MAE_temp_female) = 
        colnames(MSE_temp_male) = colnames(MAE_temp_male) =  c("lc", "rh", "apc", "cbd", "m6", "m7", "m8", "plat", "lca_dt", "lca_dxt", "lca_e0", "lca_none", "fdm", "robust_fdm", "pr")
      for(ik in 1:15)
      {
        fore_method = c("lc", "rh", "apc", "cbd", "m6", "m7", "m8", "plat", "lca_dt", "lca_dxt", "lca_e0", "lca_none", "fdm", "robust_fdm", "pr")[ik]
        
        MSE_temp_female[,fore_method] = mse(true = OBS_female[[ih]], forecast = ind_fore_female[[ih]][,fore_method])
        MSE_temp_male[,fore_method] = mse(true = OBS_male[[ih]],  forecast = ind_fore_male[[ih]][,fore_method])
        
        MAE_temp_female[,fore_method] = mae(true = OBS_female[[ih]],  forecast = ind_fore_female[[ih]][,fore_method])
        MAE_temp_male[,fore_method] = mae(true = OBS_male[[ih]],  forecast = ind_fore_male[[ih]][,fore_method])
      }
      
      MSE_female[[ih]] = MSE_temp_female
      MSE_male[[ih]] = MSE_temp_male
      
      MAE_female[[ih]] = MAE_temp_female
      MAE_male[[ih]] = MAE_temp_male
      
      rm(MSE_temp_female, MSE_temp_male, MAE_temp_female, MAE_temp_male)
    }
    
    
    ## Save combined forecasts
    
    OBS_female_all[[ij]] = OBS_female
    OBS_male_all[[ij]] = OBS_male
    ind_fore_female_all[[ij]] = ind_fore_female
    ind_fore_male_all[[ij]] = ind_fore_male
    
    ## Save computed errors into array (101x10x15)
    MSE_female_all[ij,,] = as.matrix(do.call(rbind, MSE_female))
    MSE_male_all[ij,,] = as.matrix(do.call(rbind, MSE_male))
    MAE_female_all[ij,,] = as.matrix(do.call(rbind, MAE_female))
    MAE_male_all[ij,,] = as.matrix(do.call(rbind, MAE_male))
  }
  
  return(list(OBS_female_all = OBS_female_all,
              OBS_male_all = OBS_male_all,
              ind_fore_female_all = ind_fore_female_all,
              ind_fore_male_all = ind_fore_male_all,
              MSE_female_all = MSE_female_all,
              MSE_male_all = MSE_male_all,
              MAE_female_all = MAE_female_all,
              MAE_male_all = MAE_male_all,
              country = OECD_countries[country_index]))
}


# Define a function to compute the interval score by the Gneiting, T. and Raftery, A. E. (2007) method

interval_score <- function(x, l, u, alpha)
{
  finite_ind = apply(cbind(x,l,u), 1, function(x){ifelse(sum(is.infinite(x)) > 1, 0, 1)})
  
  lb_ind = ifelse(x[finite_ind] < l[finite_ind], 1, 0)
  ub_ind = ifelse(x[finite_ind] > u[finite_ind], 1, 0)
  score = (u[finite_ind] - l[finite_ind]) + 2/alpha * (l[finite_ind] - x[finite_ind]) * lb_ind + 2/alpha * (x[finite_ind] - u[finite_ind]) * ub_ind
  return(mean(score))
}

# Define a function for computing interval predictions

err_fun_interval_testing <- function(index, state_select, state_select_smooth, nboot = 1000)
{
  test_lc_sum_female_lb = test_lc_sum_male_lb = 
    test_rh_female_lb     = test_rh_male_lb     = 
    test_apc_female_lb    = test_apc_male_lb    =
    test_cbd_female_lb    = test_cbd_male_lb    = 
    test_m6_female_lb     = test_m6_male_lb     = 
    test_m7_female_lb     = test_m7_male_lb     =
    test_m8_female_lb     = test_m8_male_lb     =
    test_plat_female_lb   = test_plat_male_lb   = 
    test_lc_sum_female_ub = test_lc_sum_male_ub = 
    test_rh_female_ub     = test_rh_male_ub     = 
    test_apc_female_ub    = test_apc_male_ub    =
    test_cbd_female_ub    = test_cbd_male_ub    = 
    test_m6_female_ub     = test_m6_male_ub     = 
    test_m7_female_ub     = test_m7_male_ub     =
    test_m8_female_ub     = test_m8_male_ub     =
    test_plat_female_ub   = test_plat_male_ub   = array(NA, dim = c(101,10,10))
  
  
  for(ik in 1:10)
  {
    obs = extract.years(get(state_select[index]), 1960:(2008+ik))
    
    # replace mortality rate greater than 1 by NA
    
    obs$rate$female = replace(obs$rate$female, which(obs$rate$female > 1), NA)
    obs$rate$male   = replace(obs$rate$male,   which(obs$rate$male   > 1), NA)
    
    if(any(!is.finite(obs$rate$female)))
    {
      ind = which(!is.finite(obs$rate$female))
      obs$rate$female = matrix(na.interp(replace(as.numeric(obs$rate$female), ind, NA)), 101, ncol(obs$rate$female))
    }
    
    if(any(!is.finite(obs$rate$male)))
    {
      ind = which(!is.finite(obs$rate$male))
      obs$rate$male = matrix(na.interp(replace(as.numeric(obs$rate$male), ind, NA)), 101, ncol(obs$rate$male))
    }
    
    #############
    # Lee-Carter
    #############
    
    wxt <- genWeightMat(ages = 0:100, years = obs$year, clip = 3)
    
    ## female
    
    # sum(kt) = sum
    
    LC1 = lc(const = "sum")
    LCfit1_female = fit(LC1, data = StMoMoData(obs, series = "female", type = "central"),
                        wxt = wxt, verbose = FALSE)
    
    for(fore_h in 1:10)
    {
      dum = simulate(LCfit1_female, nsim = nboot, h = fore_h+1)$rates[,fore_h,]
      test_lc_sum_female_lb[,ik,fore_h] = apply(dum, 1, quantile, 0.1, na.rm = TRUE)
      test_lc_sum_female_ub[,ik,fore_h] = apply(dum, 1, quantile, 0.9, na.rm = TRUE)
      rm(dum)
    }
    
    
    rm(LCfit1_female)
    
    ## male         
    
    LC1 = lc(const = "sum")
    LCfit1_male = fit(LC1, data = StMoMoData(obs, series = "male", type = "central"),
                      wxt = wxt, verbose = FALSE)
    
    
    for(fore_h in 1:10)
    {
      dum_val = try(simulate(LCfit1_male, nsim = nboot, h = fore_h+1), silent = TRUE)
      if(class(dum_val)[1] == "try-error")
      {
        dum = matrix(NA, 101, nboot)
      }
      else
      {
        dum = dum_val$rates[,fore_h,]
      }
      
      test_lc_sum_male_lb[,ik,fore_h] = apply(dum, 1, quantile, 0.1, na.rm = TRUE)
      test_lc_sum_male_ub[,ik,fore_h] = apply(dum, 1, quantile, 0.9, na.rm = TRUE)
      
      rm(dum)
    }
    

    rm(LCfit1_male)
    
    ###################
    # Renshaw-Haberman
    ###################
    
    # female
    
    LCfit_female <-  fit(lc(), data = StMoMoData(obs, series = "female", type = "central"), verbose = FALSE)
    RHfit_female <- fit(rh(), data = StMoMoData(obs, series = "female", type = "central"), 
                        wxt = wxt, start.ax = LCfit_female$ax,
                        start.bx = LCfit_female$bx, start.kt = LCfit_female$kt, verbose = FALSE)
    
    for(fore_h in 1:10)
    {
      dum_val = try(simulate(RHfit_female, nsim = nboot, h = fore_h+1), silent = TRUE)
      if(class(dum_val)[1] == "try-error")
      {
        dum = matrix(NA, 101, nboot)
      }
      else
      {
        dum = dum_val$rates[,fore_h,]
      }
      
      test_rh_female_lb[,ik,fore_h] = apply(dum, 1, quantile, 0.1, na.rm = TRUE)
      test_rh_female_ub[,ik,fore_h] = apply(dum, 1, quantile, 0.9, na.rm = TRUE)
      
      rm(dum)
    }

    rm(LCfit_female); rm(RHfit_female); rm(dum_val)

    # male
    
    LCfit_male = fit(lc(), data = StMoMoData(obs, series = "male", type = "central"), verbose = FALSE)
    RHfit_male = fit(rh(), data = StMoMoData(obs, series = "male", type = "central"), 
                     wxt = wxt, start.ax = LCfit_male$ax,
                     start.bx = LCfit_male$bx, start.kt = LCfit_male$kt, verbose = FALSE)
    
    for(fore_h in 1:10)
    {
      dum_val = try(simulate(RHfit_male, nsim = nboot, h = fore_h+1), silent = TRUE)
      if(class(dum_val)[1] == "try-error")
      {
        dum = matrix(NA, 101, nboot)
      }
      else
      {
        dum = dum_val$rates[,fore_h,]
      }
      
      test_rh_male_lb[,ik,fore_h] = apply(dum, 1, quantile, 0.1, na.rm = TRUE)
      test_rh_male_ub[,ik,fore_h] = apply(dum, 1, quantile, 0.9, na.rm = TRUE)
      
      rm(dum)
    }
    
    rm(LCfit_male); rm(RHfit_male); rm(dum_val)
    
    ####################
    # Age-Period-Cohort
    ####################
    
    # female
    
    APCfit_female <- fit(apc(), data = StMoMoData(obs, series = "female", type = "central"), 
                         wxt = wxt, verbose = FALSE)
    
    for(fore_h in 1:10)
    {
      dum = simulate(APCfit_female, nsim = nboot, h = fore_h+1)$rates[,fore_h,]
      test_apc_female_lb[,ik,fore_h] = apply(dum, 1, quantile, 0.1, na.rm = TRUE)
      test_apc_female_ub[,ik,fore_h] = apply(dum, 1, quantile, 0.9, na.rm = TRUE)
      rm(dum)
    }

    rm(APCfit_female)
    
    # male
    
    APCfit_male = fit(apc(), data = StMoMoData(obs, series = "male", type = "central"), 
                      wxt = wxt, verbose = FALSE)
    
    for(fore_h in 1:10)
    {
      dum = simulate(APCfit_male, nsim = nboot, h = fore_h+1)$rates[,fore_h,]
      test_apc_male_lb[,ik,fore_h] = apply(dum, 1, quantile, 0.1, na.rm = TRUE)
      test_apc_male_ub[,ik,fore_h] = apply(dum, 1, quantile, 0.9, na.rm = TRUE)
      rm(dum)
    }

    rm(APCfit_male)
    
    ######
    # CBD
    ######
    
    # female
    
    CBDfit_female = fit(cbd(link = "log"), data = StMoMoData(obs, series = "female", type = "initial"), 
                        wxt = wxt, verbose = FALSE)
    
    for(fore_h in 1:10)
    {
      dum = simulate(CBDfit_female, nsim = nboot, h = fore_h+1)$rates[,fore_h,]
      test_cbd_female_lb[,ik,fore_h] = apply(dum, 1, quantile, 0.1, na.rm = TRUE)
      test_cbd_female_ub[,ik,fore_h] = apply(dum, 1, quantile, 0.9, na.rm = TRUE)
      rm(dum)
    }
    
    rm(CBDfit_female)
    
    # male
    
    CBDfit_male = fit(cbd(link = "log"), data = StMoMoData(obs, series = "male", type = "initial"), 
                      wxt = wxt, verbose = FALSE)
    
    for(fore_h in 1:10)
    {
      dum = simulate(CBDfit_male, nsim = nboot, h = fore_h+1)$rates[,fore_h,]
      test_cbd_male_lb[,ik,fore_h] = apply(dum, 1, quantile, 0.1, na.rm = TRUE)
      test_cbd_male_ub[,ik,fore_h] = apply(dum, 1, quantile, 0.9, na.rm = TRUE)
      rm(dum)
    }
    
    rm(CBDfit_male)
    
    #####
    # M6
    #####
    
    # female
    
    M6fit_female <- fit(m6(), data = StMoMoData(obs, series = "female", type = "initial"), 
                        wxt = wxt, verbose = FALSE)
    
    for(fore_h in 1:10)
    {
      dum_val = try(simulate(M6fit_female, nsim = nboot, h = fore_h+1), silent = TRUE)
      if(class(dum_val)[1] == "try-error")
      {
        dum = matrix(NA, 101, nboot)
      }
      else
      {
        dum = dum_val$rates[,fore_h,]
      }
      
      test_m6_female_lb[,ik,fore_h] = apply(dum, 1, quantile, 0.1, na.rm = TRUE)
      test_m6_female_ub[,ik,fore_h] = apply(dum, 1, quantile, 0.9, na.rm = TRUE)
      
      rm(dum)
    }

    rm(M6fit_female); rm(dum_val)
    
    # male
    
    M6fit_male = fit(m6(), data = StMoMoData(obs, series = "male", type = "initial"), 
                     wxt = wxt, verbose = FALSE)
    
    for(fore_h in 1:10)
    {
      dum_val =  try(simulate(M6fit_male, nsim = nboot, h = fore_h+1), silent = TRUE)
      if(class(dum_val)[1] == "try-error")
      {
        dum = matrix(NA, 101, nboot)
      }
      else
      {
        dum = dum_val$rates[,fore_h,]
      }
      
      test_m6_male_lb[,ik,fore_h] = apply(dum, 1, quantile, 0.1, na.rm = TRUE)
      test_m6_male_ub[,ik,fore_h] = apply(dum, 1, quantile, 0.9, na.rm = TRUE)
      
      rm(dum)
    }

    rm(M6fit_male); rm(dum_val)
    
    #####
    # M7
    #####
    
    # female
    
    M7fit_female <- fit(m7(), data = StMoMoData(obs, series = "female", type = "initial"), 
                        wxt = wxt, verbose = FALSE)
    
    for(fore_h in 1:10)
    {
      dum_val = try(simulate(M7fit_female, nsim = nboot, h = fore_h+1), silent = TRUE)
      if(class(dum_val)[1] == "try-error")
      {
        dum = matrix(NA, 101, nboot)
      }
      else
      {
        dum = dum_val$rates[,fore_h,]
      }
      
      test_m7_female_lb[,ik,fore_h] = apply(dum, 1, quantile, 0.1, na.rm = TRUE)
      test_m7_female_ub[,ik,fore_h] = apply(dum, 1, quantile, 0.9, na.rm = TRUE)
      
      rm(dum)
    }
    
    rm(M7fit_female); rm(dum_val)
    
    # male
    
    M7fit_male = fit(m7(), data = StMoMoData(obs, series = "male", type = "initial"), 
                     wxt = wxt, verbose = FALSE)

    for(fore_h in 1:10)
    {
      dum_val = try(simulate(M7fit_male, nsim = nboot, h = fore_h+1), silent = TRUE)
      if(class(dum_val)[1] == "try-error")
      {
        dum = matrix(NA, 101, nboot)
      }
      else
      {
        dum = dum_val$rates[,fore_h,]
      }
      
      test_m7_male_lb[,ik,fore_h] = apply(dum, 1, quantile, 0.1, na.rm = TRUE)
      test_m7_male_ub[,ik,fore_h] = apply(dum, 1, quantile, 0.9, na.rm = TRUE)
      
      rm(dum)
    }
    
    rm(M7fit_male); rm(dum_val)
    
    #####
    # M8
    #####
    
    # female
    
    M8fit_female = fit(m8(xc = 100), data = StMoMoData(obs, series = "female", type = "initial"), 
                       wxt = wxt, verbose = FALSE)
    
    for(fore_h in 1:10)
    {
      dum_val = try(simulate(M8fit_female, nsim = nboot, h = fore_h+1), silent = TRUE)
      if(class(dum_val)[1] == "try-error")
      {
        dum = matrix(NA, 101, nboot)
      }
      else
      {
        dum = dum_val$rates[,fore_h,]
      }
      
      test_m8_female_lb[,ik,fore_h] = apply(dum, 1, quantile, 0.1, na.rm = TRUE)
      test_m8_female_ub[,ik,fore_h] = apply(dum, 1, quantile, 0.9, na.rm = TRUE)
      
      rm(dum)
    }
    
    rm(M8fit_female); rm(dum_val)
    
    # male
    
    M8fit_male = fit(m8(xc = 100), data = StMoMoData(obs, series = "male", type = "initial"), 
                     wxt = wxt, verbose = FALSE)
    
    for(fore_h in 1:10)
    {
      dum_val = try(simulate(M8fit_male, nsim = nboot, h = fore_h+1), silent = TRUE)
      if(class(dum_val)[1] == "try-error")
      {
        dum = matrix(NA, 101, nboot)
      }
      else
      {
        dum = dum_val$rates[,fore_h,]
      }
      
      test_m8_male_lb[,ik,fore_h] = apply(dum, 1, quantile, 0.1, na.rm = TRUE)
      test_m8_male_ub[,ik,fore_h] = apply(dum, 1, quantile, 0.9, na.rm = TRUE)
      
      rm(dum)
    }
    
    rm(M8fit_male); rm(dum_val)
    
    #######
    # Plat
    #######
    
    # female
    
    PLAT <- StMoMo(link = "log", staticAgeFun = TRUE,
                   periodAgeFun = c("1", f2, f3), cohortAgeFun = "1",
                   constFun = constPlat)
    PLATfit_female <- fit(PLAT, data = StMoMoData(obs, series = "female", type = "central"), 
                          wxt = wxt, verbose = FALSE)
    
    for(fore_h in 1:10)
    {
      dum = simulate(PLATfit_female, nsim = nboot, h = fore_h+1)$rates[,fore_h,]
      test_plat_female_lb[,ik,fore_h] = apply(dum, 1, quantile, 0.1, na.rm = TRUE)
      test_plat_female_ub[,ik,fore_h] = apply(dum, 1, quantile, 0.9, na.rm = TRUE)
      rm(dum)
    }
    
    rm(PLATfit_female)
    
    # male
    
    PLATfit_male = fit(PLAT, data = StMoMoData(obs, series = "male", type = "central"),
                       wxt = wxt, verbose = FALSE)
    
    for(fore_h in 1:10)
    {
      dum = simulate(PLATfit_male, nsim = nboot, h = fore_h+1)$rates[,fore_h,]
      test_plat_male_lb[,ik,fore_h] = apply(dum, 1, quantile, 0.1, na.rm = TRUE)
      test_plat_male_ub[,ik,fore_h] = apply(dum, 1, quantile, 0.9, na.rm = TRUE)
      rm(dum)
    }
    
    rm(PLATfit_male)
  }
  
  #########################
  # Compute Interval Score
  #########################
  
  test_lc_sum_female_score = test_lc_sum_male_score = 
    test_rh_female_score     = test_rh_male_score     = 
    test_apc_female_score    = test_apc_male_score    = 
    test_cbd_female_score    = test_cbd_male_score    = 
    test_m6_female_score     = test_m6_male_score     = 
    test_m7_female_score     = test_m7_male_score     = 
    test_m8_female_score     = test_m8_male_score     = 
    test_plat_female_score   = test_plat_male_score   = matrix(NA, nrow = 10, ncol = 10)
  
  for(ik in 1:10)
  {
    for(ih in 1:(11-ik))
    {
      # female
      test_lc_sum_female_score[ik,ih] = interval_score(x = log(extract.years(get(state_select[index]), (2008+ik+ih))$rate$female),
                                                     l = log(test_lc_sum_female_lb[,ik,ih]), u = log(test_lc_sum_female_ub[,ik,ih]), alpha = 0.2)
      test_rh_female_score[ik,ih] = interval_score(x = log(extract.years(get(state_select[index]), (2008+ik+ih))$rate$female),
                                                        l = log(test_rh_female_lb[,ik,ih]), u = log(test_rh_female_ub[,ik,ih]), alpha = 0.2)
      test_apc_female_score[ik,ih] = interval_score(x = log(extract.years(get(state_select[index]), (2008+ik+ih))$rate$female),
                                                        l = log(test_apc_female_lb[,ik,ih]), u = log(test_apc_female_ub[,ik,ih]), alpha = 0.2)
      test_cbd_female_score[ik,ih] = interval_score(x = log(extract.years(get(state_select[index]), (2008+ik+ih))$rate$female),
                                                        l = log(test_cbd_female_lb[,ik,ih]), u = log(test_cbd_female_ub[,ik,ih]), alpha = 0.2)
      test_m6_female_score[ik,ih] = interval_score(x = log(extract.years(get(state_select[index]), (2008+ik+ih))$rate$female),
                                                        l = log(test_m6_female_lb[,ik,ih]), u = log(test_m6_female_ub[,ik,ih]), alpha = 0.2)
      test_m7_female_score[ik,ih] = interval_score(x = log(extract.years(get(state_select[index]), (2008+ik+ih))$rate$female),
                                                        l = log(test_m7_female_lb[,ik,ih]), u = log(test_m7_female_ub[,ik,ih]), alpha = 0.2)
      test_m8_female_score[ik,ih] = interval_score(x = log(extract.years(get(state_select[index]), (2008+ik+ih))$rate$female),
                                                        l = log(test_m8_female_lb[,ik,ih]), u = log(test_m8_female_ub[,ik,ih]), alpha = 0.2)
      test_plat_female_score[ik,ih] = interval_score(x = log(extract.years(get(state_select[index]), (2008+ik+ih))$rate$female),
                                                        l = log(test_plat_female_lb[,ik,ih]), u = log(test_plat_female_ub[,ik,ih]), alpha = 0.2)
      
      # male
      test_lc_sum_male_score[ik,ih] = interval_score(x = log(extract.years(get(state_select[index]), (2008+ik+ih))$rate$male),
                                                        l = log(test_lc_sum_male_lb[,ik,ih]), u = log(test_lc_sum_male_ub[,ik,ih]), alpha = 0.2)
      test_rh_male_score[ik,ih] = interval_score(x = log(extract.years(get(state_select[index]), (2008+ik+ih))$rate$male),
                                                    l = log(test_rh_male_lb[,ik,ih]), u = log(test_rh_male_ub[,ik,ih]), alpha = 0.2)
      test_apc_male_score[ik,ih] = interval_score(x = log(extract.years(get(state_select[index]), (2008+ik+ih))$rate$male),
                                                     l = log(test_apc_male_lb[,ik,ih]), u = log(test_apc_male_ub[,ik,ih]), alpha = 0.2)
      test_cbd_male_score[ik,ih] = interval_score(x = log(extract.years(get(state_select[index]), (2008+ik+ih))$rate$male),
                                                     l = log(test_cbd_male_lb[,ik,ih]), u = log(test_cbd_male_ub[,ik,ih]), alpha = 0.2)
      test_m6_male_score[ik,ih] = interval_score(x = log(extract.years(get(state_select[index]), (2008+ik+ih))$rate$male),
                                                    l = log(test_m6_male_lb[,ik,ih]), u = log(test_m6_male_ub[,ik,ih]), alpha = 0.2)
      test_m7_male_score[ik,ih] = interval_score(x = log(extract.years(get(state_select[index]), (2008+ik+ih))$rate$male),
                                                    l = log(test_m7_male_lb[,ik,ih]), u = log(test_m7_male_ub[,ik,ih]), alpha = 0.2)
      test_m8_male_score[ik,ih] = interval_score(x = log(extract.years(get(state_select[index]), (2008+ik+ih))$rate$male),
                                                    l = log(test_m8_male_lb[,ik,ih]), u = log(test_m8_male_ub[,ik,ih]), alpha = 0.2)
      test_plat_male_score[ik,ih] = interval_score(x = log(extract.years(get(state_select[index]), (2008+ik+ih))$rate$male),
                                                      l = log(test_plat_male_lb[,ik,ih]), u = log(test_plat_male_ub[,ik,ih]), alpha = 0.2)
    }
  }
  
  ######################################
  # Lee-Carter using demography package
  ######################################
  
  test_lca_dt_female_lb = test_lca_dxt_female_lb = test_lca_e0_female_lb = test_lca_none_female_lb =
    test_lca_dt_female_ub = test_lca_dxt_female_ub = test_lca_e0_female_ub = test_lca_none_female_ub =
    test_lca_dt_male_lb = test_lca_dxt_male_lb = test_lca_e0_male_lb = test_lca_none_male_lb =
    test_lca_dt_male_ub = test_lca_dxt_male_ub = test_lca_e0_male_ub = test_lca_none_male_ub = array(NA, dim = c(101, 10, 10))
  
  for(ik in 1:10)
  {
    obs = extract.years(get(state_select[index]), 1960:(2008+ik))
    
    # replace mortality rate greater than 1 by NA
    
    obs$rate$female = replace(obs$rate$female, which(obs$rate$female > 1), NA)
    obs$rate$male   = replace(obs$rate$male,   which(obs$rate$male   > 1), NA)
    
    if(any(!is.finite(obs$rate$female)))
    {
      ind = which(!is.finite(obs$rate$female))
      obs$rate$female = matrix(na.interp(replace(as.numeric(obs$rate$female), ind, NA)), 101, ncol(obs$rate$female))
    }
    
    if(any(!is.finite(obs$rate$male)))
    {
      ind = which(!is.finite(obs$rate$male))
      obs$rate$male = matrix(na.interp(replace(as.numeric(obs$rate$male), ind, NA)), 101, ncol(obs$rate$male))
    }
    
    # female
    
    ## lca_dt
    
    for(fore_h in 1:10)
    {
      dum = forecast(lca(obs, series = "female", adjust = "dt", interpolate = TRUE),  h = fore_h)
      test_lca_dt_female_lb[,ik,fore_h]   = dum$rate$lower[,fore_h]
      test_lca_dt_female_ub[,ik,fore_h]   = dum$rate$upper[,fore_h]
      rm(dum)
    }
    
    ## lca_dxt
    
    for(fore_h in 1:10)
    {
      dum = forecast(lca(obs, series = "female", adjust = "dxt", interpolate = TRUE), h = fore_h)
      test_lca_dxt_female_lb[,ik,fore_h]  = dum$rate$lower[,fore_h]
      test_lca_dxt_female_ub[,ik,fore_h]  = dum$rate$upper[,fore_h]
      rm(dum)
    }
    
    ## lca_e0
    
    for(fore_h in 1:10)
    {
      dum = forecast(lca(obs, series = "female", adjust = "e0", interpolate = TRUE), h = fore_h)
      test_lca_e0_female_lb[,ik,fore_h]   = dum$rate$lower[,fore_h]
      test_lca_e0_female_ub[,ik,fore_h]   = dum$rate$upper[,fore_h]
      rm(dum)
    }
    
    ## lca_none
    
    for(fore_h in 1:10)
    {
      dum = forecast(lca(obs, series = "female", adjust = "none", interpolate = TRUE), h = fore_h)
      test_lca_none_female_lb[,ik,fore_h] = dum$rate$lower[,fore_h]
      test_lca_none_female_ub[,ik,fore_h] = dum$rate$upper[,fore_h]
      rm(dum)
    }
    
    # male
    
    ## lca_dt
    
    for(fore_h in 1:10)
    {
      dum = forecast(lca(obs, series = "male", adjust = "dt", interpolate = TRUE),  h = fore_h)
      test_lca_dt_male_lb[,ik,fore_h]   = dum$rate$lower[,fore_h]
      test_lca_dt_male_ub[,ik,fore_h]   = dum$rate$upper[,fore_h]
      rm(dum)
    }
    
    ## lca_dxt
    
    for(fore_h in 1:10)
    {
      dum = forecast(lca(obs, series = "male", adjust = "dxt", interpolate = TRUE), h = fore_h)
      test_lca_dxt_male_lb[,ik,fore_h]  = dum$rate$lower[,fore_h]
      test_lca_dxt_male_ub[,ik,fore_h]  = dum$rate$upper[,fore_h]
      rm(dum)
    }
    
    ## lca_e0
    
    for(fore_h in 1:10)
    {
      dum = forecast(lca(obs, series = "male", adjust = "e0", interpolate = TRUE), h = fore_h)
      test_lca_e0_male_lb[,ik,fore_h]   = dum$rate$lower[,fore_h]
      test_lca_e0_male_ub[,ik,fore_h]   = dum$rate$upper[,fore_h]
      rm(dum)
    }
    
    ## lca_none
    
    for(fore_h in 1:10)
    {
      dum = forecast(lca(obs, series = "male", adjust = "none", interpolate = TRUE), h = fore_h)
      test_lca_none_male_lb[,ik,fore_h] = dum$rate$lower[,fore_h]
      test_lca_none_male_ub[,ik,fore_h] = dum$rate$upper[,fore_h]
      rm(dum)
    }
  }
  
  test_lca_dt_female_score = test_lca_dxt_female_score = test_lca_e0_female_score = test_lca_none_female_score =
    test_lca_dt_male_score = test_lca_dxt_male_score = test_lca_e0_male_score = test_lca_none_male_score = matrix(NA, nrow = 10, ncol = 10)
  
  for(ik in 1:10)
  {
    for(ih in 1:(11-ik))
    {
      # female
      
      test_lca_dt_female_score[ik,ih] = interval_score(x = log(extract.years(get(state_select[index]), (2008+ik+ih))$rate$female),
                                                     l = log(test_lca_dt_female_lb[,ik,ih]), u = log(test_lca_dt_female_ub[,ik,ih]), alpha = 0.2)
      
      test_lca_dxt_female_score[ik,ih] = interval_score(x = log(extract.years(get(state_select[index]), (2008+ik+ih))$rate$female),
                                                        l = log(test_lca_dxt_female_lb[,ik,ih]), u = log(test_lca_dxt_female_ub[,ik,ih]), alpha = 0.2)
      
      test_lca_e0_female_score[ik,ih] = interval_score(x = log(extract.years(get(state_select[index]), (2008+ik+ih))$rate$female),
                                                        l = log(test_lca_e0_female_lb[,ik,ih]), u = log(test_lca_e0_female_ub[,ik,ih]), alpha = 0.2)
      
      test_lca_none_female_score[ik,ih] = interval_score(x = log(extract.years(get(state_select[index]), (2008+ik+ih))$rate$female),
                                                        l = log(test_lca_none_female_lb[,ik,ih]), u = log(test_lca_none_female_ub[,ik,ih]), alpha = 0.2)
      
      # male
      
      test_lca_dt_male_score[ik,ih] = interval_score(x = log(extract.years(get(state_select[index]), (2008+ik+ih))$rate$male),
                                                        l = log(test_lca_dt_male_lb[,ik,ih]), u = log(test_lca_dt_male_ub[,ik,ih]), alpha = 0.2)
      
      test_lca_dxt_male_score[ik,ih] = interval_score(x = log(extract.years(get(state_select[index]), (2008+ik+ih))$rate$male),
                                                         l = log(test_lca_dxt_male_lb[,ik,ih]), u = log(test_lca_dxt_male_ub[,ik,ih]), alpha = 0.2)
      
      test_lca_e0_male_score[ik,ih] = interval_score(x = log(extract.years(get(state_select[index]), (2008+ik+ih))$rate$male),
                                                        l = log(test_lca_e0_male_lb[,ik,ih]), u = log(test_lca_e0_male_ub[,ik,ih]), alpha = 0.2)
      
      test_lca_none_male_score[ik,ih] = interval_score(x = log(extract.years(get(state_select[index]), (2008+ik+ih))$rate$male),
                                                          l = log(test_lca_none_male_lb[,ik,ih]), u = log(test_lca_none_male_ub[,ik,ih]), alpha = 0.2)
    }
  }
  
  ########################
  # Functional data model
  ########################
  
  test_fdm_female_lb = test_fdm_male_lb = test_robust_fdm_female_lb = test_robust_fdm_male_lb = test_pr_female_lb = test_pr_male_lb =
    test_fdm_female_ub = test_fdm_male_ub = test_robust_fdm_female_ub = test_robust_fdm_male_ub = test_pr_female_ub = test_pr_male_ub = array(NA, dim = c(101, 10, 10))
  
  for(ik in 1:10)
  {
    obs = extract.years(get(state_select_smooth[index]), 1960:(2008+ik))
    
    for(fore_h in 1:10)
    {
      # female
      dum = forecast(fdm(obs, series = "female", method = "classical"), h = fore_h)
      test_fdm_female_lb[,ik,fore_h] = dum$rate$lower[,fore_h]
      test_fdm_female_ub[,ik,fore_h] = dum$rate$upper[,fore_h]
      rm(dum)
      
      dum = forecast(fdm(obs, series = "female", method = "M"), h = fore_h)
      test_robust_fdm_female_lb[,ik,fore_h] = dum$rate$lower[,fore_h]
      test_robust_fdm_female_ub[,ik,fore_h] = dum$rate$upper[,fore_h]
      rm(dum)
      
      
      # male
      dum = forecast(fdm(obs, series = "male", method = "classical"), h = fore_h)
      test_fdm_male_lb[,ik,fore_h] = dum$rate$lower[,fore_h]
      test_fdm_male_ub[,ik,fore_h] = dum$rate$upper[,fore_h]
      rm(dum)
      
      dum = forecast(fdm(obs, series = "male", method = "M"), h = fore_h)
      test_robust_fdm_male_lb[,ik,fore_h] = dum$rate$lower[,fore_h]
      test_robust_fdm_male_ub[,ik,fore_h] = dum$rate$upper[,fore_h]
      rm(dum)
      
      # product-ratio
      test_coherent_fdm = forecast(coherentfdm(obs), h = fore_h)
      test_pr_female_lb[,ik,fore_h] = test_coherent_fdm$female$rate$lower[,fore_h]
      test_pr_female_ub[,ik,fore_h] = test_coherent_fdm$female$rate$upper[,fore_h]
      test_pr_male_lb[,ik,fore_h] = test_coherent_fdm$male$rate$lower[,fore_h]
      test_pr_male_ub[,ik,fore_h] = test_coherent_fdm$male$rate$upper[,fore_h]
    }
  }
  
  test_fdm_female_score = test_fdm_male_score = test_robust_fdm_female_score = 
    test_robust_fdm_male_score = test_pr_female_score = test_pr_male_score = matrix(NA, nrow = 10, ncol = 10)
  
  for(ik in 1:10)
  {
    for(ih in 1:(11-ik))
    {
      ## fdm
      test_fdm_female_score[ik,ih] = interval_score(x = log(extract.years(get(state_select[index]), (2008+ik+ih))$rate$female),
                                                     l = log(test_fdm_female_lb[,ik,ih]), u = log(test_fdm_female_ub[,ik,ih]), alpha = 0.2)
      
      test_fdm_male_score[ik,ih] = interval_score(x = log(extract.years(get(state_select[index]), (2008+ik+ih))$rate$male),
                                                     l = log(test_fdm_male_lb[,ik,ih]), u = log(test_fdm_male_ub[,ik,ih]), alpha = 0.2)
      
      ## robust fdm
      test_robust_fdm_female_score[ik,ih] = interval_score(x = log(extract.years(get(state_select[index]), (2008+ik+ih))$rate$female),
                                                     l = log(test_robust_fdm_female_lb[,ik,ih]), u = log(test_robust_fdm_female_ub[,ik,ih]), alpha = 0.2)
      
      test_robust_fdm_male_score[ik,ih] = interval_score(x = log(extract.years(get(state_select[index]), (2008+ik+ih))$rate$male),
                                                   l = log(test_robust_fdm_male_lb[,ik,ih]), u = log(test_robust_fdm_male_ub[,ik,ih]), alpha = 0.2)
      
      ## product-ratio
      test_pr_female_score[ik,ih] = interval_score(x = log(extract.years(get(state_select[index]), (2008+ik+ih))$rate$female),
                                                     l = log(test_pr_female_lb[,ik,ih]), u = log(test_pr_female_ub[,ik,ih]), alpha = 0.2)
      
      test_pr_male_score[ik,ih] = interval_score(x = log(extract.years(get(state_select[index]), (2008+ik+ih))$rate$male),
                                                   l = log(test_pr_male_lb[,ik,ih]), u = log(test_pr_male_ub[,ik,ih]), alpha = 0.2)
      
    }
  }
  
  # female
  
  test_female_score = list(test_lc_sum_female_score,     test_rh_female_score,
                             test_apc_female_score,        test_cbd_female_score,
                             test_m6_female_score,         test_m7_female_score,
                             test_m8_female_score,         test_plat_female_score,
                             test_lca_dt_female_score,     test_lca_dxt_female_score,    
                             test_lca_e0_female_score,     test_lca_none_female_score,   
                             test_fdm_female_score,        test_robust_fdm_female_score, 
                             test_pr_female_score)
  
  test_female_fore_lb = test_female_fore_ub = list()
  test_female_fore_lb[[1]] = test_lc_sum_female_lb
  test_female_fore_lb[[2]] = test_rh_female_lb
  test_female_fore_lb[[3]] = test_apc_female_lb
  test_female_fore_lb[[4]] = test_cbd_female_lb
  test_female_fore_lb[[5]] = test_m6_female_lb
  test_female_fore_lb[[6]] = test_m7_female_lb
  test_female_fore_lb[[7]] = test_m8_female_lb
  test_female_fore_lb[[8]] = test_plat_female_lb
  test_female_fore_lb[[9]] = test_lca_dt_female_lb
  test_female_fore_lb[[10]] = test_lca_dxt_female_lb
  test_female_fore_lb[[11]] = test_lca_e0_female_lb
  test_female_fore_lb[[12]] = test_lca_none_female_lb
  test_female_fore_lb[[13]] = test_fdm_female_lb
  test_female_fore_lb[[14]] = test_robust_fdm_female_lb
  test_female_fore_lb[[15]] = test_pr_female_lb
  
  test_female_fore_ub[[1]] = test_lc_sum_female_ub
  test_female_fore_ub[[2]] = test_rh_female_ub
  test_female_fore_ub[[3]] = test_apc_female_ub
  test_female_fore_ub[[4]] = test_cbd_female_ub
  test_female_fore_ub[[5]] = test_m6_female_ub
  test_female_fore_ub[[6]] = test_m7_female_ub
  test_female_fore_ub[[7]] = test_m8_female_ub
  test_female_fore_ub[[8]] = test_plat_female_ub
  test_female_fore_ub[[9]] = test_lca_dt_female_ub
  test_female_fore_ub[[10]] = test_lca_dxt_female_ub
  test_female_fore_ub[[11]] = test_lca_e0_female_ub
  test_female_fore_ub[[12]] = test_lca_none_female_ub
  test_female_fore_ub[[13]] = test_fdm_female_ub
  test_female_fore_ub[[14]] = test_robust_fdm_female_ub
  test_female_fore_ub[[15]] = test_pr_female_ub
  
  # male
  
  test_male_score = list(test_lc_sum_male_score, test_rh_male_score,
                           test_apc_male_score,    test_cbd_male_score,
                           test_m6_male_score,     test_m7_male_score,
                           test_m8_male_score,     test_plat_male_score,
                           test_lca_dt_male_score, test_lca_dxt_male_score, 
                           test_lca_e0_male_score, test_lca_none_male_score,   
                           test_fdm_male_score,    test_robust_fdm_male_score, 
                           test_pr_male_score)
  
  test_male_fore_lb = test_male_fore_ub = list()
  test_male_fore_lb[[1]] = test_lc_sum_male_lb
  test_male_fore_lb[[2]] = test_rh_male_lb
  test_male_fore_lb[[3]] = test_apc_male_lb
  test_male_fore_lb[[4]] = test_cbd_male_lb
  test_male_fore_lb[[5]] = test_m6_male_lb
  test_male_fore_lb[[6]] = test_m7_male_lb
  test_male_fore_lb[[7]] = test_m8_male_lb
  test_male_fore_lb[[8]] = test_plat_male_lb
  test_male_fore_lb[[9]] = test_lca_dt_male_lb
  test_male_fore_lb[[10]] = test_lca_dxt_male_lb
  test_male_fore_lb[[11]] = test_lca_e0_male_lb
  test_male_fore_lb[[12]] = test_lca_none_male_lb
  test_male_fore_lb[[13]] = test_fdm_male_lb
  test_male_fore_lb[[14]] = test_robust_fdm_male_lb
  test_male_fore_lb[[15]] = test_pr_male_lb
  
  test_male_fore_ub[[1]] = test_lc_sum_male_ub
  test_male_fore_ub[[2]] = test_rh_male_ub
  test_male_fore_ub[[3]] = test_apc_male_ub
  test_male_fore_ub[[4]] = test_cbd_male_ub
  test_male_fore_ub[[5]] = test_m6_male_ub
  test_male_fore_ub[[6]] = test_m7_male_ub
  test_male_fore_ub[[7]] = test_m8_male_ub
  test_male_fore_ub[[8]] = test_plat_male_ub
  test_male_fore_ub[[9]] = test_lca_dt_male_ub
  test_male_fore_ub[[10]] = test_lca_dxt_male_ub
  test_male_fore_ub[[11]] = test_lca_e0_male_ub
  test_male_fore_ub[[12]] = test_lca_none_male_ub
  test_male_fore_ub[[13]] = test_fdm_male_ub
  test_male_fore_ub[[14]] = test_robust_fdm_male_ub
  test_male_fore_ub[[15]] = test_pr_male_ub
  
  names(test_male_score) = names(test_female_score) = c("lc_sum", "rh", "apc", "cbd", "m6", "m7", "m8", "plat", "dt", "dxt", "e0", "none", "fdm", "M_fdm", "pr")
  return(list(test_female_fore_lb = test_female_fore_lb, test_female_fore_ub = test_female_fore_ub,
              test_male_fore_lb = test_male_fore_lb, test_male_fore_ub = test_male_fore_ub,
              test_male_score = test_male_score, test_female_score = test_female_score))
}

# Define a function to implement Simple Averaging, Interior Trimming, AIC, MSE, and Shapley-based interval prediction averaging methods

combine_interval <- function(country_index, method = c("Average", "Interior_trimming", "AIC", "MSE", "SHAP"), alpha)
{
  mean_interval_score_female = mean_interval_score_male = rep(0, 10) # output of the averaged interval score using the expanding window scheme
  
  weighted_female_lb = weighted_female_ub = weighted_male_lb = weighted_male_ub = list()
  weighted_female_score = weighted_male_score = list()
  
  weights_female = weights_male = list()
  
  for(fore_h in 1:10)
  {
    weighted_female_lb[[fore_h]] = weighted_male_lb[[fore_h]] = 
      weighted_female_ub[[fore_h]] = weighted_male_ub[[fore_h]] = matrix(NA, nrow = 101, ncol = (11-fore_h))
    weighted_female_score[[fore_h]] = weighted_male_score[[fore_h]] = rep(0, (11-fore_h))
    
    temp_female_lb = temp_female_ub = temp_male_lb = temp_male_ub = list() 
    
    weights_female[[fore_h]] = weights_male[[fore_h]] = list()
    
    for(ik in 1:(11-fore_h))
    {
      # female
      temp_female_lb[[ik]] = temp_female_ub[[ik]] = matrix(NA, nrow = 101, ncol = 15)
      
      for(i_method in 1:15)
      {
        temp_female_lb[[ik]][,i_method] = get(OECD_PI_test[country_index])$test_female_fore_lb[[i_method]][,ik,fore_h]
        temp_female_ub[[ik]][,i_method] = get(OECD_PI_test[country_index])$test_female_fore_ub[[i_method]][,ik,fore_h]
      }
      
      # male
      temp_male_lb[[ik]] = temp_male_ub[[ik]] = matrix(NA, nrow = 101, ncol = 15)
      
      for(i_method in 1:15)
      {
        temp_male_lb[[ik]][,i_method] = get(OECD_PI_test[country_index])$test_male_fore_lb[[i_method]][,ik,fore_h]
        temp_male_ub[[ik]][,i_method] = get(OECD_PI_test[country_index])$test_male_fore_ub[[i_method]][,ik,fore_h]
      }
    }
    
    
    if(method == "Average")
    {
      for(ik in 1:(11-fore_h))
      {
        # female
        weighted_female_lb[[fore_h]][,ik] = rowMeans(temp_female_lb[[ik]], na.rm = TRUE)
        weighted_female_ub[[fore_h]][,ik] = rowMeans(temp_female_ub[[ik]], na.rm = TRUE)
        
        # male
        weighted_male_lb[[fore_h]][,ik] = rowMeans(temp_male_lb[[ik]], na.rm = TRUE)
        weighted_male_ub[[fore_h]][,ik] = rowMeans(temp_male_ub[[ik]], na.rm = TRUE)
        
        # weights
        weights_female[[fore_h]] = weights_male[[fore_h]] = rep(1/15, 15)
      }
    }
    
    
    
    if(method == "Interior_trimming")
    {
      # trim off the 20% or 3 models with the largest lower endpoints and the smallest upper endpoints
      weights_female[[fore_h]] = weights_male[[fore_h]] = list()
      
      for(ik in 1:(11-fore_h))
      {
        # weights
        model_trimmed_female_lb_temp = t(apply(temp_female_lb[[ik]], 1, function(x){order(x, decreasing = TRUE)[1:3]}))
        model_trimmed_female_ub_temp = t(apply(temp_female_ub[[ik]], 1, function(x){order(x, decreasing = FALSE)[1:3]}))
        
        model_trimmed_male_lb_temp = t(apply(temp_male_lb[[ik]], 1, function(x){order(x, decreasing = TRUE)[1:3]}))
        model_trimmed_male_ub_temp = t(apply(temp_male_ub[[ik]], 1, function(x){order(x, decreasing = FALSE)[1:3]}))
  
        weight_female_lb_temp = weight_female_ub_temp = 
          weight_male_lb_temp = weight_male_ub_temp = matrix(1, nrow = 101, ncol = 15)
        
        for(ij in 1:101)
        {
          weight_female_lb_temp[ij,model_trimmed_female_lb_temp[ij,]] = 0
          weight_female_ub_temp[ij,model_trimmed_female_ub_temp[ij,]] = 0
          
          weight_male_lb_temp[ij,model_trimmed_male_lb_temp[ij,]] = 0
          weight_male_ub_temp[ij,model_trimmed_male_ub_temp[ij,]] = 0
        }
        
        weights_female[[fore_h]][[ik]] = weights_male[[fore_h]][[ik]] = list()
        
        weights_female[[fore_h]][[ik]]$lb = weight_female_lb = weight_female_lb_temp/rowSums(weight_female_lb_temp, na.rm = TRUE)
        weights_female[[fore_h]][[ik]]$ub = weight_female_ub = weight_female_ub_temp/rowSums(weight_female_ub_temp, na.rm = TRUE)
        
        weights_male[[fore_h]][[ik]]$lb = weight_male_lb = weight_male_lb_temp/rowSums(weight_male_lb_temp, na.rm = TRUE)
        weights_male[[fore_h]][[ik]]$ub = weight_male_ub = weight_male_ub_temp/rowSums(weight_male_ub_temp, na.rm = TRUE)

        
        # female
        weighted_female_lb[[fore_h]][,ik] = rowSums(temp_female_lb[[ik]]*weight_female_lb, na.rm = TRUE)
        weighted_female_ub[[fore_h]][,ik] = rowSums(temp_female_ub[[ik]]*weight_female_ub, na.rm = TRUE)
        
        # male
        weighted_male_lb[[fore_h]][,ik] = rowSums(temp_male_lb[[ik]]*weight_male_lb, na.rm = TRUE)
        weighted_male_ub[[fore_h]][,ik] = rowSums(temp_male_ub[[ik]]*weight_male_ub, na.rm = TRUE)
        
        rm(model_trimmed_female_lb_temp, weight_female_lb,
           model_trimmed_female_ub_temp, weight_female_ub,
           model_trimmed_male_lb_temp, weight_male_lb,
           model_trimmed_male_ub_temp, weight_male_ub)
      }
    }
    
    
    if(method == "AIC")
    {
      # female
      AIC_temp_female = get(OECD_fore_train[country_index])$train_female_AIC[fore_h,, drop = FALSE]
      AIC_positive_female = c(which(AIC_temp_female > 0), which(is.na(AIC_temp_female)))
      AIC_abs_temp_female = abs(AIC_temp_female)
      AIC_abs_temp_female[AIC_positive_female] = 0
      AIC_weight_temp_female = na_replace(AIC_abs_temp_female/sum(AIC_abs_temp_female, na.rm = TRUE))
      
      # male
      AIC_temp_male = get(OECD_fore_train[country_index])$train_male_AIC[fore_h,, drop = FALSE]
      AIC_positive_male = c(which(AIC_temp_male > 0), which(is.na(AIC_temp_male)))
      AIC_abs_temp_male = abs(AIC_temp_male)
      AIC_abs_temp_male[AIC_positive_male] = 0
      AIC_weight_temp_male = na_replace(AIC_abs_temp_male/sum(AIC_abs_temp_male, na.rm = TRUE))
      
      for(ik in 1:(11-fore_h))
      {
        # female
        weighted_female_lb[[fore_h]][,ik] = apply(temp_female_lb[[ik]], 1, function(x){sum(x*AIC_weight_temp_female, na.rm = TRUE)})
        weighted_female_ub[[fore_h]][,ik] = apply(temp_female_ub[[ik]], 1, function(x){sum(x*AIC_weight_temp_female, na.rm = TRUE)})
        
        # male
        weighted_male_lb[[fore_h]][,ik] = apply(temp_male_lb[[ik]], 1, function(x){sum(x*AIC_weight_temp_male, na.rm = TRUE)})
        weighted_male_ub[[fore_h]][,ik] = apply(temp_male_ub[[ik]], 1, function(x){sum(x*AIC_weight_temp_male, na.rm = TRUE)})
      }
      
      # weights
      weights_female[[fore_h]] = AIC_weight_temp_female
      weights_male[[fore_h]] = AIC_weight_temp_male
      
      rm(AIC_temp_female, AIC_positive_female, AIC_abs_temp_female, AIC_weight_temp_female)
      rm(AIC_temp_male, AIC_positive_male, AIC_abs_temp_male, AIC_weight_temp_male)
    }
    
    
    if(method == "MSE")
    {
      # female
      mse_temp_female = get(OECD_fore_train[country_index])$train_female_mse[fore_h,, drop = FALSE]
      mse_weight_temp_female = na_replace(exp(-mse_temp_female)/sum(exp(-mse_temp_female), na.rm = TRUE))
      
      # male
      mse_temp_male = get(OECD_fore_train[country_index])$train_male_mse[fore_h,, drop = FALSE]
      mse_weight_temp_male = na_replace(exp(-mse_temp_male)/sum(exp(-mse_temp_male), na.rm = TRUE))
      
      for(ik in 1:(11-fore_h))
      {
        # female
        weighted_female_lb[[fore_h]][,ik] = apply(temp_female_lb[[ik]], 1, function(x){sum(x*mse_weight_temp_female, na.rm = TRUE)})
        weighted_female_ub[[fore_h]][,ik] = apply(temp_female_ub[[ik]], 1, function(x){sum(x*mse_weight_temp_female, na.rm = TRUE)})
        
        # male
        weighted_male_lb[[fore_h]][,ik] = apply(temp_male_lb[[ik]], 1, function(x){sum(x*mse_weight_temp_male, na.rm = TRUE)})
        weighted_male_ub[[fore_h]][,ik] = apply(temp_male_ub[[ik]], 1, function(x){sum(x*mse_weight_temp_male, na.rm = TRUE)})
      }
      
      # weights
      weights_female[[fore_h]] = mse_weight_temp_female
      weights_male[[fore_h]] = mse_weight_temp_male
      
      rm(mse_weight_temp_female, mse_weight_temp_male)
    }

    
    if(method == "SHAP")
    {
      shap_temp_female = shap_temp_male = matrix(0, nrow = 101, ncol = 15)
      
      # age-specific weights
      for(ij in 1:101)
      {
        # female
        shap_temp_inedx_female = match(get(OECD_SHAP[country_index])$female_result[[ij]]$summaryShaps$feature, 
                                c("lc", "rh", "apc", "cbd", "m6", "m7", "m8", "plat", "lca_dt", "lca_dxt", "lca_e0", "lca_none", "fdm", "robust_fdm", "pr"))
        if(length(shap_temp_inedx_female) == 0)
        {
          shap_temp_female[ij,] = rep(1,15)
        } 
        else 
        {
          shap_temp_female[ij,shap_temp_inedx_female] = get(OECD_SHAP[country_index])$female_result[[ij]]$summaryShaps$mean
          shap_temp_female[ij,] = replace(shap_temp_female[ij,], which(shap_temp_female[ij,] < alpha), 0)
        } 
        
        
        
        # male
        shap_temp_inedx_male = match(get(OECD_SHAP[country_index])$male_result[[ij]]$summaryShaps$feature, 
                                       c("lc", "rh", "apc", "cbd", "m6", "m7", "m8", "plat", "lca_dt", "lca_dxt", "lca_e0", "lca_none", "fdm", "robust_fdm", "pr"))
        if(length(shap_temp_inedx_male) == 0)
        {
          shap_temp_male[ij,] = rep(1,15)
        } 
        else 
        {
          shap_temp_male[ij,shap_temp_inedx_male] = get(OECD_SHAP[country_index])$male_result[[ij]]$summaryShaps$mean
          shap_temp_male[ij,] = replace(shap_temp_male[ij,], which(shap_temp_male[ij,] < alpha), 0)
        } 
        
      }
      
      # weights
      weights_female[[fore_h]] = shap_temp_female/rowSums(shap_temp_female, na.rm = TRUE)
      weights_male[[fore_h]] =  shap_temp_male/rowSums(shap_temp_male, na.rm = TRUE)
      
      for(ik in 1:(11-fore_h))
      {
        # female
        weighted_female_lb[[fore_h]][,ik] = rowSums(temp_female_lb[[ik]]*weights_female[[fore_h]], na.rm = TRUE)
        weighted_female_ub[[fore_h]][,ik] = rowSums(temp_female_ub[[ik]]*weights_female[[fore_h]], na.rm = TRUE)
        
        # male
        weighted_male_lb[[fore_h]][,ik] = rowSums(temp_male_lb[[ik]]*weights_male[[fore_h]], na.rm = TRUE)
        weighted_male_ub[[fore_h]][,ik] = rowSums(temp_male_ub[[ik]]*weights_male[[fore_h]], na.rm = TRUE)
      }
      
      rm(shap_temp_female, shap_temp_male)
    }
    
    
    # Interval score computation
    for(ik in 1:(11-fore_h))
    {
      weighted_female_score[[fore_h]][ik] = interval_score(x = log(extract.years(get(state_select[country_index]), (2008+ik+fore_h))$rate$female),
                                                           l = log(weighted_female_lb[[fore_h]][,ik]), u = log(weighted_female_ub[[fore_h]][,ik]), alpha = 0.2)
      weighted_male_score[[fore_h]][ik] = interval_score(x = log(extract.years(get(state_select[country_index]), (2008+ik+fore_h))$rate$male),
                                                         l = log(weighted_male_lb[[fore_h]][,ik]), u = log(weighted_male_ub[[fore_h]][,ik]), alpha = 0.2)
    }
    
    mean_interval_score_female[fore_h] = mean(weighted_female_score[[fore_h]], na.rm = TRUE)
    mean_interval_score_male[fore_h] = mean(weighted_male_score[[fore_h]], na.rm = TRUE)
    
    ## clear temp files
    # rm(temp_female_lb, temp_female_ub, temp_male_lb, temp_male_ub)
  }
  
  
  return(list(mean_interval_score_female = mean_interval_score_female, mean_interval_score_male = mean_interval_score_male, 
              weighted_female_lb = weighted_female_lb, weighted_female_ub = weighted_female_ub, 
              weighted_male_lb = weighted_male_lb, weighted_male_ub = weighted_male_ub,
              weighted_female_score = weighted_female_score, weighted_male_score = weighted_male_score,
              weights_female = weights_female, weights_male = weights_male))

}

