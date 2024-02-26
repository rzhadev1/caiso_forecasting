library("gamlss.dist")
library("gamlss")
library("gamlss.tr")
library(MetricsWeighted)
library("gamlss.data")
library(readr)
library(dplyr)
library(forecast)
library("gamlss.lasso")



data <- read_csv("/Users/richardz/desktop/projects/power_modeling/data/sp15.csv")
# shift by 24 hours for lag 1
data['LMP_prev'] <- lag(data['LMP'], n = 24)
data <- data[apply(data, 1, function(x) !any(is.na(x))), ]
#data[which(!is.finite(data))] = NA

# use the truncated distribution for quantile forecasts
gen.trun(par=c(-150,2000),family="ST2", name="CAISO_bid_trunc", type="both")

#hours = for(h in 1:24) {
hours = for(h in c(4,9,12,17,20)) {
  hour_data <- data[data$OPR_HR == h,]
  # rolling window forecasts
  # windows of 365 days, 24 hrs per day
  window = 365
  n_windows = nrow(hour_data) - window
  output <- data.frame(Day=character(0), GAMLSS_LINEAR=numeric(0), NAIVE=numeric(0), True=numeric(0))
  q_loss <- data.frame(Day=character(0), Q.01_LOSS=numeric(0), Q.05_LOSS=numeric(0), Q.1_LOSS=numeric(0),Q.25_LOSS=numeric(0), Q.5_LOSS=numeric(0), Q.75_LOSS=numeric(0), Q.95_LOSS=numeric(0), Q.98_LOSS=numeric(0), Q.99_LOSS=numeric(0))
  quantiles <- c(.01, .05, .1, .2, .3, .4, .5, .6, .7, .8, .9, .95, .99)
  forecasts = for(i in 1:n_windows) {
    X_in = hour_data[i:(window+i-1), ] 
    X_out = hour_data[window+i, ]
    
    # note that we have the following link functions for the ST2 distribution: 
    # mu -> identity
    # sigma -> log 
    # nu -> identity 
    # tau -> log
    if(all(X_in$SOLAR == 0)) {
      mu_terms <- c("GHG_PRC_IDX_prev", "LOAD","WIND", "GAS_AVG_PRC_prev", "IMP_MW_prev", "EXP_MW_prev", "HOL", "LMP_prev")
      sigma_terms <- c("GHG_PRC_IDX_prev", "LOAD", "WIND", "GAS_AVG_PRC_prev", "IMP_MW_prev", "EXP_MW_prev", "HOL")
      nu_terms <- c("GHG_PRC_IDX_prev", "LOAD", "WIND", "GAS_AVG_PRC_prev", "IMP_MW_prev", "EXP_MW_prev", "HOL")
    }
    else {
      mu_terms <- c("GHG_PRC_IDX_prev", "LOAD", "SOLAR", "WIND", "GAS_AVG_PRC_prev", "IMP_MW_prev", "EXP_MW_prev", "HOL", "LMP_prev")
      sigma_terms <- c("GHG_PRC_IDX_prev", "LOAD", "SOLAR", "WIND", "GAS_AVG_PRC_prev", "IMP_MW_prev", "EXP_MW_prev", "HOL")
      nu_terms <- c("GHG_PRC_IDX_prev", "LOAD", "SOLAR", "WIND", "GAS_AVG_PRC_prev", "IMP_MW_prev", "EXP_MW_prev", "HOL")
    }
    
    model <- gamlss(LMP~ mu_terms,
                    sigma.formula= ~sigma_terms,
                    nu.formula= ~nu_terms,
                    family=ST2(),
                    data=X_in, 
                    control= gamlss.control(c.crit=0.5, cc=0.5, bf.tol=0.5,n.cyc = 1000,cyc=1000,bf.cyc=1000, gd.tol=Inf),
                    method=RS())
    if(!model$converged) {
      refit(model)
    }
    pred_mu <- predict(model, what="mu",newdata=X_out)
    pred_sig <- predict(model, what="sigma",newdata=X_out)
    pred_nu <- predict(model, what="nu",newdata=X_out)
    print(format(X_out$OPR_DT, format="%Y-%m-%d"))
    output[nrow(output)+1, ] = c(format(X_out$OPR_DT, format="%Y-%m-%d"), pred_mu, X_in$LMP[length(X_in)], X_out$LMP)
    print(pred_mu)
    print(X_out$LMP)
    #qst2 = qST2CAISO_bid_trunc(quantiles, mu = pred_mu, sigma = pred_sig, nu = pred_nu, tau = 2)
    #add_q <- c() 
    #add_q <- c(add_q, format(X_out$OPR_DT, format="%Y-%m-%d"))
    #for(i in 1:9) {
    #  add_q <- c(add_q, pinball_loss(tau=quantiles[i], y=X_out$LMP, q=qst2[i])$L)
    #}
    #q_loss[nrow(q_loss)+1, ] = add_q
  }
  print(mae(as.numeric(output$True), as.numeric(output$GAMLSS_LINEAR)))
  write.csv(output, paste("/Users/richardz/desktop/projects/power_modeling/output_no_deseas",h,".csv", sep=""))
  #write.csv(q_loss, paste("/Users/richardz/desktop/projects/power_modeling/",h,".csv", sep=""))
}




