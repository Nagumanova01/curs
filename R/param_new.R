library(ggplot2)
library(dplyr)
library(tidyr)
library(googlesheets4)
library(ggrepel)
library(readxl)

m<-function(x){
  return(abs(exp(x*x/2)-0.305*exp(-1.28*x)-0.39-0.305*exp(1.28*x))/(exp(x*x/2)))
}

s<-function(x){
  return(abs(exp(x*x)*(exp(x^2)-1)-0.305*(exp(-1.28*x))^2-0.39-0.305*(exp(1.28*x))^2+(0.305*exp(-1.28*x)+0.39+0.305*exp(1.28*x))^2)/(exp(x*x)*(exp(x^2)-1)))
}

ggplot() +
  xlim(c(0, 5)) +
  geom_function(aes(color = "отн. ошибка мат.ож."), fun = m,
                colour = "red",
                lwd = 1,
                linetype = 1)+
  geom_function(aes(color = "отн. ошибка дисперсии"), fun = s,
                colour = "blue",
                lwd = 1,
                linetype = 1)
  

ggplot() +
  xlim(c(-5, 5)) +
  geom_function(fun = s,
                colour = "blue",
                lwd = 1,
                linetype = 1)