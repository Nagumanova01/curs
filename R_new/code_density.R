library(ggplot2)

x=seq(0,3000000,by=1000);
y = dlnorm(x, 12, 1.5)
z = dlnorm(x, 8, 0.69)

#par(mfrow=c(2,1))

destination = "C:\\Users\\Пользователь\\Desktop\\Диплом\\curs\\img_new\\density1.pdf"
pdf(file = destination)

plot(x,y,type="l", lwd = 2, col="forestgreen", main="", 
     xlab="x", 
     ylab="p(x)")

dev.off()

destination = "C:\\Users\\Пользователь\\Desktop\\Диплом\\curs\\img_new\\density2.pdf"
pdf(file = destination)

plot(x,z,type="l", lwd = 2, col="blue", main="", 
     xlab="x", 
     ylab="p(x)")

dev.off()