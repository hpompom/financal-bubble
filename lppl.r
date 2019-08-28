library(tidyquant)
library(cmaes)
library(zoo)
star.day="2014-06-01"
end.day="2015-08-01"
date_txt_from = "2014-06-01"
date_txt_to_base = "2015-08-01"
nbre_step_backward <- 600
nbre_generation <- 50
ticker  <- tq_get("000001.ss", get  = "stock.prices", from = star.day, to   = end.day)
ticker$t <- 1:(nrow(ticker))
ticker <- data.frame(ticker)
ticker$date <- as.Date(ticker$date)
df_result <- NULL
vec_control <- data.frame(maxit = c(nbre_generation))  
from <- as.Date(date_txt_from)
to_base <- as.Date(date_txt_to_base)
colnames(ticker) <- c(names(ticker)[1:4],"Close",names(ticker)[6:8])

LPPL <- function(data, m=1, omega=1, tc=0) {
  data$X <- tc - data$t
  data$Xm <- data$X ** m #B
  data$Xm.cos <- data$X ** m * cos(omega * log(data$X)) #C1
  data$Xm.sin <- data$X ** m * sin(omega * log(data$X)) #C2
  data$logP <- log(data$Close)
  return(lm(logP ~ Xm + Xm.cos + Xm.sin, data=data))
}

FittedLPPL <- function(data, lm.result, m=1, omega=1, tc=0) {
  data$X <- tc - data$t
  A <- lm.result$coefficients[1]
  B <- lm.result$coefficients[2]
  C1 <- lm.result$coefficients[3]
  C2 <- lm.result$coefficients[4]
  result <- exp(A + B * (data$X ** m) + C1 * (data$X ** m) * cos(omega * log(data$X)) + C2 * (data$X ** m) * sin(omega * log(data$X))) 
  return(result)
}



FittedLPPLwithexpected <- function(data, lm.result, x_vector, m=1, omega=1, tc=0) {
  tmp_vector <- tc - x_vector
  A <- lm.result$coefficients[1]
  B <- lm.result$coefficients[2]
  C1 <- lm.result$coefficients[3]
  C2 <- lm.result$coefficients[4]
  result <- exp(A + B * (tmp_vector ** m) + C1 * (tmp_vector ** m) * cos(omega * log(tmp_vector)) + C2 * (tmp_vector ** m) * sin(omega * log(tmp_vector))) 
  return(result)
  
}

getlinear_param <- function(m, omega, tc) {
  lm.result <- LPPL(rTicker, m, omega, tc)
  getcoeff_regLPPL <- c(lm.result$coefficients[1],lm.result$coefficients[2], lm.result$coefficients[3], lm.result$coefficients[4])
}


tryParams <- function (m, omega, tc) {  
  lm.result <- LPPL(rTicker, m, omega, tc)
  plot(rTicker$t, rTicker$Close, typ='l') #base graph based on data
  generate_vector = seq(min(rTicker$t), tc-0.002, 0.002)
  lines(generate_vector, FittedLPPLwithexpected(rTicker, lm.result, generate_vector, m, omega, tc), col="red")
}


residuals_with_ts <- function(ts, m, omega, tc) {
  lm.result <- LPPL(ts, m, omega, tc)
  return(sum((FittedLPPL(ts, lm.result, m, omega, tc) - ts$Close) ** 2))
}


residuals_with_ts_obj <- function(x, ts) {
  return(residuals_with_ts(ts, x[1], x[2], x[3]))
}

for (i in 0:nbre_step_backward) {
  to <- to_base
  to <- to-i
  
  if (as.POSIXlt(to)$wday != 0 & as.POSIXlt(to)$wday != 6) { #saute weekend
    
    rTicker <- subset(ticker, ticker$date >= from & ticker$date <= to)
    
    last_row <- tail(rTicker, 1) #pour recuperer le dernier prix
    
    #test <- mcga(popsize = 4,chsize = 50,minval = c(0.1, 5, max(rTicker$t)+0.002),
                 #maxval = c(0.9, 16, max(rTicker$t)+2),evalFunc =residuals_with_ts_obj )
    
    test <- cma_es(c(0.01, 5, max(rTicker$t)+0.002), residuals_with_ts_obj, rTicker, lower=c(0.1, 5, max(rTicker$t)+0.002), upper=c(0.9, 16, max(rTicker$t)+2), control=vec_control)
    
    linear_param <- getlinear_param(test$par[1], test$par[2], test$par[3])
    
    rbind(df_result, c(date_txt_from, format(to, "%Y-%m-%d"), last_row$t, last_row$Close, -i, nbre_generation, test$par[3]-last_row$t, as.integer((test$par[3]-last_row$t)/(1/365)), test$par[1], test$par[2], test$par[3], linear_param[1], linear_param[2], linear_param[3], linear_param[4])) -> df_result
    
    tryParams(test$par[1], test$par[2], test$par[3]) #plot
    
  }
}
colnames(df_result) <- c("date_from", "date_to", "t", "price", "step_backward", "nbre_generation", "t_until_critical_point", "days_before_critical_time", "m", "omega", "tc", "A", "B", "C1", "C2")
df_result
df <- data.frame(df_result)
df <- df_result%>%
  apply(., 2, as.numeric)%>%
  data.frame()
star.day="2014-06-01"
end.day="2015-09-01"
tryParams(df$m[35],df$omega[35],df$tc[35])
