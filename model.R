library("gamlss.dist")
library("gamlss")
install.packages("Metrics")
library(Metrics)
library("gamlss.data")
#install.packages("sn")
#library("sn")
install.packages("readr")
library(readr)
hours = for(h in 8:24) {
  # rolling window forecasts
  # windows of 365 days, 24 hrs per day
  hour_data <- read_csv(paste("/Users/richardz/desktop/projects/power_modeling/data/log_np15_he",h,".csv", sep=""))
  window = 365
  n_windows = nrow(hour_data) - window
  forecasted <- c() 
  true <- c()
  forecasts = for(i in 1:n_windows) {
    X_in = hour_data[i:(window+i-1), ] 
    X_out = hour_data[window+i, ]
    if (h >= 7 && h <= 20) {
      model <- gamlss(LMP~GHG_PRC_IDX_prev+(LOAD)+SOLAR+WIND+GAS_AVG_PRC_prev+LMP_prev+IMP_MW_prev+EXP_MW_prev,
                      sigma.formula= ~GHG_PRC_IDX_prev+(LOAD)+SOLAR+WIND+GAS_AVG_PRC_prev+IMP_MW_prev+EXP_MW_prev,
                      nu.formula= ~GHG_PRC_IDX_prev+(LOAD)+SOLAR+WIND+GAS_AVG_PRC_prev+IMP_MW_prev+EXP_MW_prev,
                      family=ST2(),
                      data=X_in, 
                      control= gamlss.control(n.cyc = 1000, trace=FALSE),
                      method=RS())
    }
    else {
      # exclude solar forecasts on hours with no substantial solar output
      model <- gamlss(LMP~GHG_PRC_IDX_prev+LOAD+WIND+GAS_AVG_PRC_prev+LMP_prev+IMP_MW_prev+EXP_MW_prev,
                      sigma.formula= ~GHG_PRC_IDX_prev+LOAD+WIND+GAS_AVG_PRC_prev+IMP_MW_prev+EXP_MW_prev,
                      nu.formula= ~GHG_PRC_IDX_prev+LOAD+WIND+GAS_AVG_PRC_prev+IMP_MW_prev+EXP_MW_prev,
                      family=ST2(),
                      data=X_in, 
                      control= gamlss.control(n.cyc = 1000, trace=FALSE),
                      method=RS())
    }
    # note that we have the following link functions for the ST2 distribution: 
    # mu -> identity
    # sigma -> log 
    # nu -> identity 
    # tau -> log
    print(coef(model))
    pred_mu <- exp(predict(model, what="mu",newdata=X_out,type="response"))-151
    # need to use response type to apply inverse log link function
    pred_sig <- predict(model, what="sigma",newdata=X_out,type="response")
    pred_nu <- predict(model, what="nu",newdata=X_out,type="response")
    X_out$LMP <- exp(X_out$LMP)-151
    print(pred_mu)
    print(X_out$LMP)
    # compute quantile function of st2 distribution
    qst2 = qST2(c(.25, .5, .75, .9, .95, .99), mu = pred_mu, sigma = pred_sig, nu = pred_nu, tau = 2)
    print(qst2)
    forecasted <- c(forecasted, pred_mu)
    true <- c(true, X_out$LMP)
    
  }
  mape_err <- mape(true, forecasted)
  print(mape_err)
  break
  #rmse_err <- rmse(true, forecasted)
  #mape_err <- mape(true, forecasted)
  #print(mape_err)
  #gc()
  
}




