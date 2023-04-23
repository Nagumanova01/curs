library(lognorm)
library(ggplot2)
library(dplyr)
library(SciViews)
library(pracma)
library(cubature)
library(xtable)

mu1 = 4
mu2 = 6
sigma1_2 = 0.15
sigma2_2 = 0.3

G = function(p, mu, sigma){exp(mu+(sqrt(2*sigma))*erfinv(2*p-1))}

m=exp(mu1+sigma1_2/2)+exp(mu2+sigma2_2/2)
d=(exp(2*mu1+sigma1_2))*(exp(sigma1_2)-1)+(exp(2*mu2+sigma2_2))*(exp(sigma2_2)-1)

mu_n = ln(m)-(ln(d/(m*m)+1))/2
sigma_n_2 = ln(d/(m*m)+1)

z50_n = (m/(sqrt(d/(m*m)+1)))
z10_n = G(0.1, mu_n, sigma_n_2)
z90_n = G(0.9, mu_n, sigma_n_2)

sig = (ln(z90_n/z10_n))/(qnorm(0.9)-qnorm(0.1))

Fun = function(p){(exp(sig*qnorm(p)-sig*sig/2)-1)/(sqrt(exp(sig*sig)-1))}

t1 = Fun(0.1)
t2 = Fun(0.5)
t3 = Fun(0.9)

p1 = (1+t2*t3)/((t1-t3)*(t1-t2))
p2 = (1+t1*t3)/((t2-t1)*(t2-t3))
p3=1-p1-p2

m_n = z10_n*p1+z50_n*p2+z90_n*p3
d_n = z10_n*z10_n*p1+z50_n*z50_n*p2+z90_n*z90_n*p3 - m_n*m_n

m-m_n
d-d_n
