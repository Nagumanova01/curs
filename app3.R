library(lognorm)
library(ggplot2)
library(dplyr)
library(SciViews)
library(pracma)
library(cubature)
library(xtable)

# mu<=12, sigma^2<=2.25

mu1 = 4
mu2 = 6
sigma1_2 = 0.05
sigma2_2 = 0.05

err_m = c(0)
err_10 = c(0)
err_90 = c(0)

k=1

while (sigma1_2<=2.25){
  while (sigma2_2<=2.25){
    xi1 = rlnorm(100000,mu1,sqrt(sigma1_2))
    xi2 = rlnorm(100000,mu2,sqrt(sigma2_2))
    xi <- xi1 + xi2
    
    med = median(xi)
    q10 = quantile(xi, 0.1)
    q90 = quantile(xi, 0.9)
    
    m=exp(mu1+sigma1_2/2)+exp(mu2+sigma2_2/2)
    d=(exp(2*mu1+sigma1_2))*(exp(sigma1_2)-1)+(exp(2*mu2+sigma2_2))*(exp(sigma2_2)-1)
    
    mu_n = ln(m)-(ln(d/(m*m)+1))/2
    sigma_n_2 = ln(d/(m*m)+1)
    xi_n= rlnorm(100000,mu_n,sqrt(sigma_n_2))
    
    med_n = median(xi_n)
    z10_n = quantile(xi_n, 0.1)
    z90_n = quantile(xi_n, 0.9)
    
    err_med = (abs(med-med_n))/med
    err_med = round(err_med,digits=4)
    err_m[k]=err_med*100
    
    err_q10 = (abs(q10-z10_n))/q10
    err_q10 = round(err_q10,digits=4)
    err_10[k]=err_q10*100
    
    err_q90 = (abs(q90-z90_n))/q90
    err_q90 = round(err_q90,digits=4)
    err_90[k]=err_q90*100
    k=k+1
    sigma2_2=sigma2_2+0.2
  }
  sigma1_2 = sigma1_2+0.2
  sigma2_2 = 0.05
}

matrix_m = matrix(err_m, nrow = 12, byrow = TRUE)
matrix_10= matrix(err_10, nrow = 12, byrow = TRUE)
matrix_90= matrix(err_90, nrow = 12, byrow = TRUE)

xtable(matrix_m)
xtable(matrix_10)
xtable(matrix_90)

#================================================
#================================================

g = function(x){
  mu1 = 4
  mu2 = 6
  sigma1_2 = 0.45
  
  xi1 = rlnorm(1000,mu1,sqrt(sigma1_2))
  xi2 = rlnorm(1000,mu2,sqrt(x))
  xi <- xi1 + xi2
  med = median(xi)
  
  m=exp(mu1+sigma1_2/2)+exp(mu2+x/2)
  d=(exp(2*mu1+sigma1_2))*(exp(sigma1_2)-1)+(exp(2*mu2+x))*(exp(x)-1)
  
  mu_n = ln(m)-(ln(d/(m*m)+1))/2
  sigma_n_2 = ln(d/(m*m)+1)
  xi_n= rlnorm(1000,mu_n,sqrt(sigma_n_2))
  med_n = median(xi_n)
  
  err_med = (abs(med-med_n))/med
  err_med = round(err_med,digits=2)
  return(err_med)
}

x = seq(from = 0.05 , to = 2.25, by = 0.05)
y=lapply(x, g)

destination = "C:\\Users\\Пользователь\\Desktop\\Диплом\\curs\\gr1.pdf"
pdf(file = destination)
plot(x,y,type="l", lwd = 1, col="forestgreen", main="", 
     xlab="sig2^2", 
     ylab="")
dev.off()

t = function(x){
  mu1 = 4
  mu2 = 6
  sigma1_2 = 0.45
  
  xi1 = rlnorm(1000,mu1,sqrt(sigma1_2))
  xi2 = rlnorm(1000,mu2,sqrt(x))
  xi <- xi1 + xi2
  q10 = quantile(xi, 0.1)
  
  m=exp(mu1+sigma1_2/2)+exp(mu2+x/2)
  d=(exp(2*mu1+sigma1_2))*(exp(sigma1_2)-1)+(exp(2*mu2+x))*(exp(x)-1)
  
  mu_n = ln(m)-(ln(d/(m*m)+1))/2
  sigma_n_2 = ln(d/(m*m)+1)
  xi_n= rlnorm(1000,mu_n,sqrt(sigma_n_2))
  z10_n = quantile(xi_n, 0.1)
  
  err_q10 = (abs(q10-z10_n))/q10
  err_q10 = round(err_q10,digits=4)
  return(err_q10)
}

x2 = seq(from = 0.05 , to = 2.25, by = 0.05)
y2=lapply(x2, t)

destination = "C:\\Users\\Пользователь\\Desktop\\Диплом\\curs\\gr2.pdf"
pdf(file = destination)
plot(x2,y2,type="l", lwd = 1, col="steelblue", main="", 
     xlab="sig2^2", 
     ylab="")
dev.off()

w = function(x){
  mu1 = 4
  mu2 = 6
  sigma1_2 = 0.45
  
  xi1 = rlnorm(1000,mu1,sqrt(sigma1_2))
  xi2 = rlnorm(1000,mu2,sqrt(x))
  xi <- xi1 + xi2
  q90 = quantile(xi, 0.9)
  
  m=exp(mu1+sigma1_2/2)+exp(mu2+x/2)
  d=(exp(2*mu1+sigma1_2))*(exp(sigma1_2)-1)+(exp(2*mu2+x))*(exp(x)-1)
  
  mu_n = ln(m)-(ln(d/(m*m)+1))/2
  sigma_n_2 = ln(d/(m*m)+1)
  xi_n= rlnorm(1000,mu_n,sqrt(sigma_n_2))
  z90_n = quantile(xi_n, 0.9)
  
  err_q90 = (abs(q90-z90_n))/q90
  err_q90 = round(err_q90,digits=4)
  return(err_q90)
}

x3 = seq(from = 0.05 , to = 2.25, by = 0.05)
y3=lapply(x3, w)

destination = "C:\\Users\\Пользователь\\Desktop\\Диплом\\curs\\gr3.pdf"
pdf(file = destination)
plot(x3,y3,type="l", lwd = 1, col="red3", main="", 
     xlab="sig2^2", 
     ylab="")
dev.off()

