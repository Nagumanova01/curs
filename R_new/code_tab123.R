library(lognorm)
library(SciViews)
library(ggplot2)
library(xtable)

mu1 = 4
mu2 = 4

err_m = c(0)
err_10 = c(0)
err_90 = c(0)

k=1
sigma1_2 = 0.25
sigma2_2 = 0.25

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
    sigma2_2=sigma2_2+0.5
  }
  sigma1_2 = sigma1_2+0.5
  sigma2_2 = 0.25
}

matrix_m = matrix(err_m, nrow = 5, byrow = TRUE)
matrix_10= matrix(err_10, nrow = 5, byrow = TRUE)
matrix_90= matrix(err_90, nrow = 5, byrow = TRUE)

xtable(matrix_m)
xtable(matrix_10)
xtable(matrix_90)
