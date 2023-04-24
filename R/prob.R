library(ggplot2)
library(dplyr)
library(tidyr)
library(googlesheets4)
library(ggrepel)
library(readxl)

m<-function(x){
  return(abs(exp(x*x/2)-0.305*exp(-1.28*x)-0.39-0.305*exp(1.28*x))/(exp(x*x/2)))
}

p2 = function(x){(exp(x*x)+exp(-x*x)-(exp(x*x/2))*(exp(-1.28*x)+exp(1.28*x)))/(2*exp(-x*x)-(exp(-x*x))*(exp(-1.28*x)+exp(1.28*x)))}

p_2 = function(x){(exp(x*x)+1/exp(x*x)-(exp(x*x/2))*(1/exp(1.28*x)+exp(1.28*x)))}


s<-function(x){
  return(abs(exp(x*x)*(exp(x^2)-1)-0.305*(exp(-1.28*x))^2-0.39-0.305*(exp(1.28*x))^2+(0.305*exp(-1.28*x)+0.39+0.305*exp(1.28*x))^2)/(exp(x*x)*(exp(x^2)-1)))
}

x = seq(from = 0.05 , to = 0.9, by = 0.05)
y=lapply(x, p_2)

destination = "C:\\Users\\Пользователь\\Desktop\\Диплом\\curs\\gr_new_2.pdf"
pdf(file = destination)
plot(x,y,type="l", lwd = 1, col="steelblue", main="", 
     xlab="sig2^2", 
     ylab="")
dev.off()

ggplot() +
  xlim(c(0, 1)) +
  geom_function(fun = p_2,
                colour = "red",
                lwd = 1,
                linetype = 1)

ggplot() +
  xlim(c(-5, 5)) +
  geom_function(fun = s,
                colour = "blue",
                lwd = 1,
                linetype = 1)