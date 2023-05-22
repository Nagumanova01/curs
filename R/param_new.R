library(ggplot2)
library(dplyr)
library(tidyr)
library(googlesheets4)
library(ggrepel)
library(readxl)

m<-function(x){
  return((exp(x*x/2)-0.305*exp(-1.28*x)-0.39-0.305*exp(1.28*x))/(exp(x*x/2)))
}

s<-function(x){
  return((exp(x*x)*(exp(x^2)-1)-0.305*(exp(-1.28*x))^2-0.39-0.305*(exp(1.28*x))^2+(0.305*exp(-1.28*x)+0.39+0.305*exp(1.28*x))^2)/(exp(x*x)*(exp(x^2)-1)))
}


destination = "C:\\Users\\Пользователь\\Desktop\\Диплом\\curs\\img\\par_new.pdf"
pdf(file = destination)

ggplot() +
  xlim(c(0, 5)) +
  labs(x = "sigma", y = "")+
  geom_function(aes(color = "отн. ошибка мат.ож."), fun = m,
                colour = "red",
                lwd = 1,
                linetype = 1)+
  geom_function(aes(color = "отн. ошибка дисперсии"), fun = s,
                colour = "blue",
                lwd = 1,
                linetype = 1)+
  annotate("text", x= 0.75 , y= 0.98 , label= "отн. ошибка дисперсии", col=" blue")+
  annotate("text", x= 4 , y= 0.75 , label= "отн. ошибка мат.ожидания", col=" red")

dev.off()
  

ggplot() +
  xlim(c(-5, 5)) +
  geom_function(fun = s,
                colour = "blue",
                lwd = 1,
                linetype = 1)