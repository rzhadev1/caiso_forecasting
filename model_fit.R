library("gamlss.dist")
library("gamlss")
library("gamlss.tr")
library(MetricsWeighted)
library("gamlss.data")
library(readr)
library(dplyr)
library(forecast)
library("gamlss.lasso")
library("zoo")
library("lmtest")

data <- read_csv("/Users/richardz/desktop/projects/power_modeling/data/sp15.csv")
# shift by 24 hours for lag 1
#data['LMP_prev'] <- lag(data['LMP'], n = 24)
data <- data[apply(data, 1, function(x) !any(is.na(x))), ]
#data[which(!is.finite(data))] = NA

# use the truncated distribution for quantile forecasts
gen.trun(par=c(-150,2000),family="ST5", name="CAISO_bid_trunc", type="both")

hours = for(h in c(4,9,12,17,20)) {
  hour_data <- data[data$OPR_HR == h,]
  # rolling window forecasts
  # windows of 365 days, 24 hrs per day
    if(all(hour_data$SOLAR == 0)) {
      mu_terms <- c("GHG_PRC_IDX_prev", "LOAD","WIND", "GAS_AVG_PRC_prev", "IMP_MW_prev", "EXP_MW_prev", "HOL", "LMP_prev", "LMP_prev7")
      sigma_terms <- c("GHG_PRC_IDX_prev", "LOAD", "WIND", "GAS_AVG_PRC_prev", "IMP_MW_prev", "EXP_MW_prev", "HOL")
      nu_terms <- c("GHG_PRC_IDX_prev", "LOAD", "WIND", "GAS_AVG_PRC_prev", "IMP_MW_prev", "EXP_MW_prev", "HOL")
      model <- gamlss(LMP~GHG_PRC_IDX_prev+LOAD+WIND+GAS_AVG_PRC_prev+LMP_prev+HOL,
                      sigma.formula= ~GHG_PRC_IDX_prev+LOAD+WIND+GAS_AVG_PRC_prev+HOL,
                      nu.formula= ~GHG_PRC_IDX_prev+LOAD+WIND+GAS_AVG_PRC_prev+HOL,
                      tau.formula = ~GHG_PRC_IDX_prev+LOAD+WIND+GAS_AVG_PRC_prev+HOL,
                      family=ST2(),
                      data=hour_data, 
                      control= gamlss.control(c.crit=0.5, cc=0.5, bf.tol=0.5,n.cyc = 1000,cyc=1000,bf.cyc=1000, gd.tol=Inf),
                      method=RS())
    }
    else {
      mu_terms <- c("GHG_PRC_IDX_prev", "LOAD", "SOLAR", "WIND", "GAS_AVG_PRC_prev", "IMP_MW_prev", "EXP_MW_prev", "HOL", "LMP_prev", "LMP_prev7")
      sigma_terms <- c("GHG_PRC_IDX_prev", "LOAD", "SOLAR", "WIND", "GAS_AVG_PRC_prev", "IMP_MW_prev", "EXP_MW_prev", "HOL")
      nu_terms <- c("GHG_PRC_IDX_prev", "LOAD", "SOLAR", "WIND", "GAS_AVG_PRC_prev", "IMP_MW_prev", "EXP_MW_prev", "HOL", "LMP_prev")
      model <- gamlss(LMP~GHG_PRC_IDX_prev+LOAD+WIND+SOLAR+GAS_AVG_PRC_prev+LMP_prev+HOL,
                      sigma.formula= ~GHG_PRC_IDX_prev+LOAD+WIND+SOLAR+GAS_AVG_PRC_prev+HOL,
                      nu.formula= ~GHG_PRC_IDX_prev+LOAD+WIND+SOLAR+GAS_AVG_PRC_prev+HOL,
                      tau.formula = ~GHG_PRC_IDX_prev+LOAD+WIND+SOLAR+GAS_AVG_PRC_prev+HOL,
                      family=ST2(),
                      data=hour_data, 
                      control= gamlss.control(c.crit=0.5, cc=0.5, bf.tol=0.5,n.cyc = 1000,cyc=1000,bf.cyc=1000, gd.tol=Inf),
                      method=RS())
    }
    
    #model <- stepGAICAll.A(model)
    #Dunn and Smyth (1996)
    #need approximately standard normal residuals
    plot(model, ts=TRUE)
    wp(model, ylim.all=1.5)
}
 



