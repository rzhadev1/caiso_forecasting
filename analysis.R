library(readr)
library(dplyr)
library(forecast)
library(psych)
library("gamlss.dist")
library("gamlss")
library("gamlss.tr")
library("gamlss.data")
library("gamlss.ggplots")
library(gamlss.add)
library(goftest)
library(moments)

data <- read_csv("/Users/richardz/desktop/projects/power_modeling/data/sp15.csv")
#data <- data[apply(data, 1, function(x) !any(is.na(x))), ]
gen.trun(par=c(-150,2000),family="ST5", name="CAISO_bid_trunc", type="both")
aic <- fitDist(data$LMP, type = "realline", trace = FALSE, try.gamlss = TRUE, extra=c("ST1CAISO_bid_trunc"))
bic <- fitDist(data$LMP, type = "realline", trace = FALSE, try.gamlss = TRUE, k=log(nrow(hour_data)))
print(aic$fits)
print(bic$fits)
hours = for(h in c(4,9,12,17,20)) {
  hour_data <- data[data$OPR_HR == h,]
  aic_fit <- fitDist(hour_data$LMP, type = "realline", trace = FALSE, try.gamlss = TRUE, k=2)
  bic_fit <- fitDist(hour_data$LMP, type = "realline", trace = FALSE, try.gamlss = TRUE, k=log(nrow(hour_data)))
  print(aic_fit$family)
  print(aic_fit$fits)
  
  print(bic_fit$family)
  print(bic_fit$fits)
}

# boostrap ad and cvm tests
hours = for(h in c(4,9,12)) {
  hour_data <- data[data$OPR_HR == h,]
  histDist(hour_data$LMP, family=ST5CAISO_bid_trunc())
  fitted <- gamlssML(data=hour_data['LMP'], 
                   family=ST5CAISO_bid_trunc(),
                   LMP ~ 1, 
                   control= gamlss.control(c.crit=0.5, cc=0.5, bf.tol=0.5,n.cyc = 1000,cyc=1000,bf.cyc=1000,gd.tol=Inf))
  m <- fitted$mu
  s <- fitted$sigma
  n <- fitted$nu
  t <- fitted$tau
  
  test1 <- ad.test(hour_data$LMP, null="pST5CAISO_bid_trunc",mu=m,sigma=s,nu=n, tau=t,estimated=TRUE)
  test2 <- cvm.test(hour_data$LMP, null="pST5CAISO_bid_trunc",mu=m,sigma=s,nu=n, tau=t,estimated=TRUE)
  print(test1)
  print(test2)
}

# examine statistics of variables and price levels, comparing to that of literature
hours = for(h in c(4,9,12,17,20)) {
  print(paste("Hour: ", h))
  hour_data <- data[data$OPR_HR == h,]
  avg <- mean(hour_data$LMP) 
  std <- sd(hour_data$LMP)
  sk <- skewness(hour_data$LMP)
  kurt <- kurtosis(hour_data$LMP)
  minval <- min(hour_data$LMP)
  maxval <- max(hour_data$LMP)
  print(paste("Mean:",avg,"Std:",std,"Skewness:",sk,"Kurtosis:",kurt,"Maxval:",maxval,"Minval:",minval,sep=" "))
}




