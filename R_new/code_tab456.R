library(lognorm)
library(ggplot2)
library(dplyr)
library(SciViews)
library(pracma)
library(cubature)
library(xtable)

mu1 = 4
mu2 = 4

G = function(p, mu, sigma_kv){exp(mu+(sqrt(2*sigma_kv))*erfinv(2*p-1))}

f_m = c(0)
f_10 = c(0)
f_90 = c(0)

k=1
sigma1_2 = 0.25
sigma2_2 = 0.25

while (sigma1_2<=2.25){
  while (sigma2_2<=2.25){
    
    m=exp(mu1+sigma1_2/2)+exp(mu2+sigma2_2/2)
    d=(exp(2*mu1+sigma1_2))*(exp(sigma1_2)-1)+(exp(2*mu2+sigma2_2))*(exp(sigma2_2)-1)
    
    mu_n = ln(m)-(ln(d/(m*m)+1))/2
    sigma_n_2 = ln(d/(m*m)+1)
    
    z50 = (m/(sqrt(d/(m*m)+1)))
    z10 = G(0.1, mu_n, sigma_n_2)
    z90 = G(0.9, mu_n, sigma_n_2)
    
    func_xi1_xi2_z10 = function(x){(0.5+0.5*(erf((ln(z10-x)-mu1)/(sqrt(sigma1_2*2)))))*(exp(-((ln(x)-mu2)/(sqrt(2*sigma2_2)))^(2))/(x*sqrt(2*pi*sigma2_2)))}
    func_xi1_xi2_z50 = function(x){(0.5+0.5*(erf((ln(z50-x)-mu1)/(sqrt(sigma1_2*2)))))*(exp(-((ln(x)-mu2)/(sqrt(2*sigma2_2)))^(2))/(x*sqrt(2*pi*sigma2_2)))}
    func_xi1_xi2_z90 = function(x){(0.5+0.5*(erf((ln(z90-x)-mu1)/(sqrt(sigma1_2*2)))))*(exp(-((ln(x)-mu2)/(sqrt(2*sigma2_2)))^(2))/(x*sqrt(2*pi*sigma2_2)))}
    
    integ10 = integrate(func_xi1_xi2_z10, lower = 0, upper = z10)
    integ50 = integrate(func_xi1_xi2_z50, lower = 0, upper = z50)
    integ90 = integrate(func_xi1_xi2_z90, lower = 0, upper = z90)
    
    pr50 = round(integ50$value,digits=4)
    f_m[k]=pr50*100
    
    pr10 = round(integ10$value,digits=4)
    f_10[k]=pr10*100
    
    pr90 = round(integ90$value,digits=4)
    f_90[k]=pr90*100
    
    k=k+1
    sigma2_2=sigma2_2+0.5
  }
  sigma1_2 = sigma1_2+0.5
  sigma2_2 = 0.25
}

matrix_f_m = matrix(f_m, nrow = 5, byrow = TRUE)
matrix_f_10= matrix(f_10, nrow = 5, byrow = TRUE)
matrix_f_90= matrix(f_90, nrow = 5, byrow = TRUE)

xtable(matrix_f_m)
xtable(matrix_f_10)
xtable(matrix_f_90)