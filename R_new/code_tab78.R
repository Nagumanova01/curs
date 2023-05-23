library(lognorm)
library(SciViews)
library(ggplot2)
library(psych)
library(xtable)

mu1 = 4
mu2 = 4

gamma3_xi = rep(0,25)
gamma3_eta = rep(0,25)

gamma4_xi = rep(0,25)
gamma4_eta = rep(0,25)

k=1
sigma1_2 = 0.25
sigma2_2 = 0.25

while (sigma1_2<=2.25){
  while (sigma2_2<=2.25){
    xi1 = rlnorm(100000,mu1,sqrt(sigma1_2))
    xi2 = rlnorm(100000,mu2,sqrt(sigma2_2))
    xi <- xi1 + xi2
    
    m=exp(mu1+sigma1_2/2)+exp(mu2+sigma2_2/2)
    d=(exp(2*mu1+sigma1_2))*(exp(sigma1_2)-1)+(exp(2*mu2+sigma2_2))*(exp(sigma2_2)-1)
    
    mu_n = ln(m)-(ln(d/(m*m)+1))/2
    sigma_n_2 = ln(d/(m*m)+1)
    xi_n= rlnorm(100000,mu_n,sqrt(sigma_n_2))
    
    gamma3_xi[k] = skew(xi)
    gamma3_eta[k] = skew(xi_n)
    gamma4_xi[k] = kurtosis(xi)
    gamma4_eta[k] = kurtosis(xi_n)
    
    k=k+1
    sigma2_2=sigma2_2+0.5
  }
  sigma1_2 = sigma1_2+0.5
  sigma2_2 = 0.25
}

matrix_gamma3_xi = matrix(gamma3_xi, nrow = 5, byrow = TRUE)
matrix_gamma3_eta = matrix(gamma3_eta, nrow = 5, byrow = TRUE)

matrix_gamma4_xi= matrix(gamma4_xi, nrow = 5, byrow = TRUE)
matrix_gamma4_eta= matrix(gamma4_eta, nrow = 5, byrow = TRUE)

xtable(matrix_gamma3_xi)
xtable(matrix_gamma3_eta)
xtable(matrix_gamma4_xi)
xtable(matrix_gamma4_eta)
