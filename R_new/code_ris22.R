library(ggplot2)

m<-function(x){
  return((exp(x*x/2)-0.305*exp(qnorm(0.1)*x)-0.39-0.305*exp(qnorm(0.9)*x))/(exp(x*x/2)))
}

s<-function(x){
  return((exp(x*x)*(exp(x^2)-1)-0.305*(exp(qnorm(0.1)*x))^2-0.39-0.305*(exp(qnorm(0.9)*x))^2+(0.305*exp(qnorm(0.1)*x)+0.39+0.305*exp(qnorm(0.9)*x))^2)/(exp(x*x)*(exp(x^2)-1)))
}

#ris22.png

ggplot() +
  xlim(c(0, 5)) +
  labs(x = "sigma", y = "относительная ошибка")+
  geom_function(aes(color = "отн. ошибка мат.ож."), fun = m,
                colour = "red",
                lwd = 1,
                linetype = 1)+
  geom_function(aes(color = "отн. ошибка дисперсии"), fun = s,
                colour = "blue",
                lwd = 1,
                linetype = 1)+
  annotate("text", x= 0.75 , y= 0.98 , label= "отн. ошибка дисперсии", col=" blue", size = 6)+
  annotate("text", x= 4 , y= 0.75 , label= "отн. ошибка мат.ожидания", col=" red", size = 6)+
  theme(axis.title = element_text(size = 15, face = "bold"))