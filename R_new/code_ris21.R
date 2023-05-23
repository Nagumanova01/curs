library(ggplot2)

#========================================
#pi2 = 0.5
#========================================

t = function(pi, sig){
  v1 = exp(sig*qnorm(pi)-sig*sig/2)-1
  v2 = sqrt(exp(sig*sig)-1)
  return(v1/v2)
}

p2 = function(pi, sig){
  t1 = t(0.1, sig)
  t2 = t(0.5, sig)
  t3 = t(pi, sig)
  return ((1+t1*t3)/((t2-t1)*(t2-t3)))
}

p2_sim = function(pi, sig){
  t1 = t(1-pi, sig)
  t2 = t(0.5, sig)
  t3 = t(pi, sig)
  return ((1+t1*t3)/((t2-t1)*(t2-t3)))
}

f = function(pi, p){
  s=seq(0.001, 2, by = 0.001)
  s_lim = 2
  for (i in s){
    an = p(pi, i)
    if (an<0){
      s_lim = i
      if (i==0.001){s_lim =0}
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
  s1[j] = f(i, p2_sim)
  s2[j] = f(i, p2)
  j = j+1
}

df <- data.frame(x = v, y = s1, z = s2)

destination = "C:\\Users\\Пользователь\\Desktop\\Диплом\\curs\\img_new\\ris21.pdf"
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


#========================================
#pi2 = 0.7
#========================================

p2 = function(pi, sig){
  t1 = t(0.1, sig)
  t2 = t(0.7, sig)
  t3 = t(pi, sig)
  return ((1+t1*t3)/((t2-t1)*(t2-t3)))
}

p2_sim = function(pi, sig){
  t1 = t(1-pi, sig)
  t2 = t(0.7, sig)
  t3 = t(pi, sig)
  return ((1+t1*t3)/((t2-t1)*(t2-t3)))
}

v = seq(0.71,0.99, by = 0.01)
s1 = rep(0, 29)
s2 = rep(0, 29)
j = 1
for (i in v){
  s1[j] = f(i, p2_sim)
  s2[j] = f(i, p2)
  j = j+1
}

df2 <- data.frame(x = v, y = s1, z = s2)

ggplot(df2, aes (x = x)) +
  xlim(c(0.7,1))+
  geom_line( aes (y = y, color = 'pi3 = 1-pi1 = pi'), 
             color = 2,    
             lwd = 1,      
             linetype = 1) + 
  geom_line( aes (y = z, color = 'pi1 = 0.1, pi3 = pi'), 
             color = 3,    
             lwd = 1,      
             linetype = 1) +
  labs(x = "pi", y = "sigma")


