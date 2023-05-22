library(ggplot2)
library(areaplot)

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

destination = "C:\\Users\\Пользователь\\Desktop\\Диплом\\curs\\img\\p123_1.pdf"
pdf(file = destination)

ggplot() +
  xlim(c(0.8, 1)) + ylim(c(-0.1, 1)) +
  geom_function(fun = p1, args = list(sig = 0.8), colour = "#CD9B1D", lwd = 1, linetype = 1)+
  geom_function(fun = p2, args = list(sig = 0.8), colour = "#00BFFF", lwd = 1, linetype = 1)+
  geom_function(fun = p3, args = list(sig = 0.8), colour = "#FF69B4", lwd = 1, linetype = 1)+
  labs(x = "pi", y = "weight")+
  annotate("text", x= 0.9 , y= 0.87 , label= "p1", col= "#CD9B1D")+
  annotate("text", x= 0.98 , y= 0.63 , label= "p2", col= "#00BFFF")+
  annotate("text", x= 0.9 , y= 0.37 , label= "p3", col= "#FF69B4")+
  ggtitle("sigma = 0.8, pi1 = 0.1, pi2 = 0.5, pi3 = pi")

dev.off()

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

destination = "C:\\Users\\Пользователь\\Desktop\\Диплом\\curs\\img\\p123_2.pdf"
pdf(file = destination)

ggplot() +
  xlim(c(0.11,0.89)) + ylim(c(-0.1,1)) +
  geom_function(fun = p1, args = list(sig = 0.8), colour = "#CD9B1D", lwd = 1, linetype = 1)+
  geom_function(fun = p2, args = list(sig = 0.8), colour = "#00BFFF", lwd = 1, linetype = 1)+
  geom_function(fun = p3, args = list(sig = 0.8), colour = "#FF69B4", lwd = 1, linetype = 1)+
  labs(x = "pi", y = "weight")+
  annotate("text", x= 0.5 , y= 0.8 , label= "p1", col= "#CD9B1D")+
  annotate("text", x= 0.15 , y= -5 , label= "p2", col= "#00BFFF")+
  annotate("text", x= 0.25 , y= 0.51 , label= "p3", col= "#FF69B4")+
  ggtitle("sigma = 0.8, pi1 = 0.1, pi2 = pi, pi3 = 0.9")

dev.off()

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

destination = "C:\\Users\\Пользователь\\Desktop\\Диплом\\curs\\img\\p123_3.pdf"
pdf(file = destination)

ggplot() +
  xlim(c(0,0.49)) + ylim(c(-0.1,1)) +
  geom_function(fun = p1, args = list(sig = 0.8), colour = "#CD9B1D", lwd = 1, linetype = 1)+
  geom_function(fun = p2, args = list(sig = 0.8), colour = "#00BFFF", lwd = 1, linetype = 1)+
  geom_function(fun = p3, args = list(sig = 0.8), colour = "#FF69B4", lwd = 1, linetype = 1)+
  labs(x = "pi", y = "weight")+
  annotate("text", x= 0.1 , y= 0.7 , label= "p1", col= "#CD9B1D")+
  annotate("text", x= 0.05 , y= 0 , label= "p2", col= "#00BFFF")+
  annotate("text", x= 0.2 , y= 0.53 , label= "p3", col= "#FF69B4")+
  ggtitle("sigma = 0.8, pi1 = pi, pi2 = 0.5, pi3 = 0.9")

dev.off()
#========================================

p1 = function(pi, sig){
  t1 = t(pi, sig)
  t2 = t(0.5, sig)
  t3 = t(1-pi, sig)
  return ((1+t2*t3)/((t1-t3)*(t1-t2)))
}

p2 = function(pi, sig){
  t1 = t(pi, sig)
  t2 = t(0.5, sig)
  t3 = t(1-pi, sig)
  return ((1+t1*t3)/((t2-t1)*(t2-t3)))
}

p3 = function(pi, sig){
  return(1 - p1(pi, sig) - p2(pi, sig))
}

f_s1 = function(pi){
  s=seq(0,2, by = 0.01)
  s_lim = 0
  for (i in s){
    an = exp(-i*i)-exp(i*qnorm(pi)-i*i/2)-exp(-i*qnorm(pi)-i*i/2)+exp(i*i)
    #print(an)
    if (is.na(an)){
      print('Missing')
      #an=0
    }
    if (an>0){
      s_lim = i
      break
    }
  }
  return(s_lim)
}

v = seq(0.51,0.99, by = 0.01)
s = rep(0, 49)
j = 1
for (i in v){
  s[j] = f_s1(i)
  j = j+1
}

df <- data.frame(x = v, y = s)

ggplot(df, aes(x = x, y = y)) +
  xlim(c(0.84,1))+
  geom_line(color = 2,    
            lwd = 1,      
            linetype = 1)+
  labs(x = "pi", y = "sigma")+
  ggtitle("pi1 = 1-pi, pi2 = 0.5, pi3 = pi")

#=========================

f_s2 = function(pi){
  s=seq(0,2, by = 0.01)
  s_lim = 0
  for (i in s){
    an = exp(-i*i)-exp(i*qnorm(0.1)-i*i/2)-exp(i*qnorm(pi)-i*i/2)+exp(i*i)
    if (is.na(an)){
      #print('Missing')
      #an=0
    }
    if (an>0){
      s_lim = i
      break
    }
  }
  return(s_lim)
}

v2 = seq(0.49,0.99, by = 0.01)
s2 = rep(0, 51)
j = 1
for (i in v2){
  s2[j] = f_s2(i)
  j = j+1
}

df2 <- data.frame(x = v2, y = s2)

ggplot(df2, aes(x = x, y = y)) +
  xlim(c(0.84,1))+
  geom_line(color = 3,    
            lwd = 1,      
            linetype = 1)+
  labs(x = "pi", y = "sigma")+
  ggtitle("pi1 = 0.1, pi2 = 0.5, pi3 = pi")

#=================================

f_s1 = function(pi){
  s=seq(0,2, by = 0.01)
  s_lim = 0
  for (i in s){
    an = exp(-i*i)-exp(i*qnorm(pi)-i*i/2)-exp(-i*qnorm(pi)-i*i/2)+exp(i*i)
    #print(an)
    if (is.na(an)){
      print('Missing')
      #an=0
    }
    if (an>0){
      s_lim = i
      break
    }
  }
  return(s_lim)
}

f_s2 = function(pi){
  s=seq(0,2, by = 0.01)
  s_lim = 0
  for (i in s){
    an = exp(-i*i)-exp(i*qnorm(0.1)-i*i/2)-exp(i*qnorm(pi)-i*i/2)+exp(i*i)
    print(an)
    if (is.na(an)){
      print('Missing')
      #an=0
    }
    if (an>0){
      s_lim = i
      break
    }
  }
  return(s_lim)
}

v = seq(0.51,0.99, by = 0.01)
s1 = rep(0, 49)
s2 = rep(0, 49)
j = 1
for (i in v){
  s1[j] = f_s1(i)
  s2[j] = f_s2(i)
  j = j+1
}

df <- data.frame(x = v, y = s1, z = s2)

destination = "C:\\Users\\????????????\\Desktop\\??????\\curs\\img\\sigma_12.pdf"
pdf(file = destination)

ggplot(df, aes (x = x)) +
  xlim(c(0.84,1))+
  geom_line( aes (y = y, color = 'pi3 = 1-pi1 = pi'), 
             color = 2,    
             lwd = 1,      
             linetype = 1) + 
  geom_line( aes (y = z, color = 'pi1 = 0.1, pi3 = pi'), 
             color = 3,    
             lwd = 1,      
             linetype = 1) +
  labs(x = "pi", y = "sigma")
dev.off()

#==================
#pi2 = 0.7
library(ggplot2)

t = function(pi, sig){
  v1 = exp(sig*qnorm(pi)-sig*sig/2)-1
  v2 = sqrt(exp(sig*sig)-1)
  return(v1/v2)
}

p2 = function(pi, sig){
  t1 = t(1-pi, sig)
  t2 = t(0.5, sig)
  t3 = t(pi, sig)
  return ((1+t1*t3)/((t2-t1)*(t2-t3)))
}

p2_2 = function(pi, sig){
  t1 = t(0.1, sig)
  t2 = t(0.5, sig)
  t3 = t(pi, sig)
  return ((1+t1*t3)/((t2-t1)*(t2-t3)))
}

f_s1 = function(pi){
  s=seq(0.01,2, by = 0.01)
  s_lim = 0
  for (i in s){
    an = p2(pi, i)
    #print(an)
    if (is.na(an)){
      print('Missing1')
      #an=0
    }
    if (an<0){
      s_lim = i
      break
    }
  }
  return(s_lim)
}

f_s2 = function(pi){
  s=seq(0.01,2, by = 0.01)
  s_lim = 0
  for (i in s){
    an = p2_2(pi,i)
    #print(an)
    if (is.na(an)){
      print('Missing2')
      #an=0
    }
    if (an<0){
      s_lim = i
      break
    }
  }
  return(s_lim)
}

v = seq(0.51,0.99, by = 0.01)
s1 = rep(0, 49)
s2 = rep(0, 49)
j = 1
for (i in v){
  s1[j] = f_s1(i)
  s2[j] = f_s2(i)
  j = j+1
}

df <- data.frame(x = v, y = s1, z = s2)

destination = "C:\\Users\\Пользователь\\Desktop\\Диплом\\curs\\img\\sigma_12_new.pdf"
pdf(file = destination)

ggplot(df, aes (x = x)) +
  xlim(c(0.5,1))+
  geom_line( aes (y = y, color = 'pi3 = 1-pi1 = pi'), 
             color = 2,    
             lwd = 1,      
             linetype = 1) + 
  geom_line( aes (y = z, color = 'pi1 = 0.1, pi3 = pi'), 
             color = 3,    
             lwd = 1,      
             linetype = 1) +
  labs(x = "pi", y = "sigma")

dev.off()