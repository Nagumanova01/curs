library(ggplot2)

t = function(pi, sig){
  v1 = exp(sig*qnorm(pi)-sig*sig/2)-1
  v2 = sqrt(exp(sig*sig)-1)
  return(v1/v2)
}

p1 = function(pi, sig){
  t1 = t(0.1, sig)
  t2 = t(0.5, sig)
  t3 = t(pi, sig)
  return ((1+t2*t3)/((t1-t3)*(t1-t2)))
}

p2 = function(pi, sig){
  t1 = t(0.1, sig)
  t2 = t(0.5, sig)
  t3 = t(pi, sig)
  return ((1+t1*t3)/((t2-t1)*(t2-t3)))
}

p3 = function(pi, sig){
  return(1 - p1(pi, sig) - p2(pi, sig))
}

ggplot() +
  xlim(c(0.51,1)) +
  geom_function(fun = p1, args = list(sig = 0.8), colour = "#CD9B1D", lwd = 1, linetype = 1)+
  geom_function(fun = p2, args = list(sig = 0.8), colour = "#00BFFF", lwd = 1, linetype = 1)+
  geom_function(fun = p3, args = list(sig = 0.8), colour = "#FF69B4", lwd = 1, linetype = 1)+
  labs(x = "pi", y = "weight")+
  annotate("text", x= 0.52 , y= 1 , label= "p1", col= "#CD9B1D")+
  annotate("text", x= 0.6 , y= -25 , label= "p2", col= "#00BFFF")+
  annotate("text", x= 0.6 , y= 25 , label= "p3", col= "#FF69B4")+
  ggtitle("sigma = 0.8, pi1 = 0.1, pi2 = 0.5, pi3 = pi")

#=================================================================

p1 = function(pi, sig){
  t1 = t(0.1, sig)
  t2 = t(pi, sig)
  t3 = t(0.9, sig)
  return ((1+t2*t3)/((t1-t3)*(t1-t2)))
}

p2 = function(pi, sig){
  t1 = t(0.1, sig)
  t2 = t(pi, sig)
  t3 = t(0.9, sig)
  return ((1+t1*t3)/((t2-t1)*(t2-t3)))
}

ggplot() +
  xlim(c(0.11,0.89)) +
  geom_function(fun = p1, args = list(sig = 0.8), colour = "#CD9B1D", lwd = 1, linetype = 1)+
  geom_function(fun = p2, args = list(sig = 0.8), colour = "#00BFFF", lwd = 1, linetype = 1)+
  geom_function(fun = p3, args = list(sig = 0.8), colour = "#FF69B4", lwd = 1, linetype = 1)+
  labs(x = "pi", y = "weight")+
  annotate("text", x= 0.15 , y= 5 , label= "p1", col= "#CD9B1D")+
  annotate("text", x= 0.15 , y= -5 , label= "p2", col= "#00BFFF")+
  annotate("text", x= 0.17 , y= 1 , label= "p3", col= "#FF69B4")+
  ggtitle("sigma = 0.8, pi1 = 0.1, pi2 = pi, pi3 = 0.9")

#===========================================

p1 = function(pi, sig){
  t1 = t(pi, sig)
  t2 = t(0.5, sig)
  t3 = t(0.9, sig)
  return ((1+t2*t3)/((t1-t3)*(t1-t2)))
}

p2 = function(pi, sig){
  t1 = t(pi, sig)
  t2 = t(0.5, sig)
  t3 = t(0.9, sig)
  return ((1+t1*t3)/((t2-t1)*(t2-t3)))
}
  
ggplot() +
  xlim(c(0,0.49)) +
  geom_function(fun = p1, args = list(sig = 0.8), colour = "#CD9B1D", lwd = 1, linetype = 1)+
  geom_function(fun = p2, args = list(sig = 0.8), colour = "#00BFFF", lwd = 1, linetype = 1)+
  geom_function(fun = p3, args = list(sig = 0.8), colour = "#FF69B4", lwd = 1, linetype = 1)+
  labs(x = "pi", y = "weight")+
  annotate("text", x= 0.45 , y= 10 , label= "p1", col= "#CD9B1D")+
  annotate("text", x= 0.45 , y= -10 , label= "p2", col= "#00BFFF")+
  annotate("text", x= 0.48 , y= -2 , label= "p3", col= "#FF69B4")+
  ggtitle("sigma = 0.8, pi1 = pi, pi2 = 0.5, pi3 = 0.9")
