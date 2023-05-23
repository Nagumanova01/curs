library(lognorm)
library(ggplot2)
library(dplyr)
library(SciViews)
library(pracma)
library(xtable)

#ошибка аппроксимации медианы от sigma2 при sigma1_2 = 0.75

g = function(x){
  mu1 = 4
  mu2 = 4
  sigma1_2 = 0.75
  
  xi1 = rlnorm(1000000,mu1,sqrt(sigma1_2))
  xi2 = rlnorm(1000000,mu2,sqrt(x))
  xi <- xi1 + xi2
  med = median(xi)
  
  m=exp(mu1+sigma1_2/2)+exp(mu2+x/2)
  d=(exp(2*mu1+sigma1_2))*(exp(sigma1_2)-1)+(exp(2*mu2+x))*(exp(x)-1)
  
  mu_n = ln(m)-(ln(d/(m*m)+1))/2
  sigma_n_2 = ln(d/(m*m)+1)
  xi_n= rlnorm(1000000,mu_n,sqrt(sigma_n_2))
  med_n = median(xi_n)
  
  err_med = (abs(med-med_n))/med
  err_med = round(err_med,digits=2)
  return(err_med)
}

x = seq(from = 0.05 , to = 2.25, by = 0.1)
y = lapply(x, g)

destination = "C:\\Users\\Пользователь\\Desktop\\Диплом\\curs\\img_new\\ris41.pdf"
pdf(file = destination)

plot(x,y,type="l", lwd = 1, col="forestgreen", main="", 
     xlab="sig2^2", 
     ylab="")

dev.off()

#==================================
#ошибка аппроксимации q10 от sigma2 при sigma1_2 = 0.75

t = function(x){
  mu1 = 4
  mu2 = 4
  sigma1_2 = 0.75
  
  xi1 = rlnorm(1000000,mu1,sqrt(sigma1_2))
  xi2 = rlnorm(1000000,mu2,sqrt(x))
  xi <- xi1 + xi2
  q10 = quantile(xi, 0.1)
  
  m=exp(mu1+sigma1_2/2)+exp(mu2+x/2)
  d=(exp(2*mu1+sigma1_2))*(exp(sigma1_2)-1)+(exp(2*mu2+x))*(exp(x)-1)
  
  mu_n = ln(m)-(ln(d/(m*m)+1))/2
  sigma_n_2 = ln(d/(m*m)+1)
  xi_n= rlnorm(1000000,mu_n,sqrt(sigma_n_2))
  z10_n = quantile(xi_n, 0.1)
  
  err_q10 = (abs(q10-z10_n))/q10
  err_q10 = round(err_q10,digits=4)
  return(err_q10)
}

x2 = seq(from = 0.05 , to = 2.25, by = 0.1)
y2=lapply(x2, t)

destination = "C:\\Users\\Пользователь\\Desktop\\Диплом\\curs\\img_new\\ris42.pdf"
pdf(file = destination)

plot(x2,y2,type="l", lwd = 1, col="steelblue", main="", 
     xlab="sig2^2", 
     ylab="")

dev.off()

#==================================
#ошибка аппроксимации q90 от sigma2 при sigma1_2 = 0.75

w = function(x){
  mu1 = 4
  mu2 = 4
  sigma1_2 = 0.75
  
  xi1 = rlnorm(1000000,mu1,sqrt(sigma1_2))
  xi2 = rlnorm(1000000,mu2,sqrt(x))
  xi <- xi1 + xi2
  q90 = quantile(xi, 0.9)
  
  m=exp(mu1+sigma1_2/2)+exp(mu2+x/2)
  d=(exp(2*mu1+sigma1_2))*(exp(sigma1_2)-1)+(exp(2*mu2+x))*(exp(x)-1)
  
  mu_n = ln(m)-(ln(d/(m*m)+1))/2
  sigma_n_2 = ln(d/(m*m)+1)
  xi_n= rlnorm(1000000,mu_n,sqrt(sigma_n_2))
  z90_n = quantile(xi_n, 0.9)
  
  err_q90 = (abs(q90-z90_n))/q90
  err_q90 = round(err_q90,digits=4)
  return(err_q90)
}

x3 = seq(from = 0.05 , to = 2.25, by = 0.1)
y3=lapply(x3, w)

destination = "C:\\Users\\Пользователь\\Desktop\\Диплом\\curs\\img_new\\ris43.pdf"
pdf(file = destination)

plot(x3,y3,type="l", lwd = 1, col="red3", main="", 
     xlab="sig2^2", 
     ylab="")

dev.off()
