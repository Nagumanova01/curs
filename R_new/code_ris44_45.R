library(lognorm)
library(ggplot2)
library(dplyr)
library(SciViews)
library(pracma)
library(ggridges)

mu1 = 4
mu2 = 4
sigma1_2 = 0.25
sigma2_2 = 0.25

m=exp(mu1+sigma1_2/2)+exp(mu2+sigma2_2/2)
d=(exp(2*mu1+sigma1_2))*(exp(sigma1_2)-1)+(exp(2*mu2+sigma2_2))*(exp(sigma2_2)-1)

xi1 = rlnorm(1000000,mu1,sqrt(sigma1_2))
xi2 = rlnorm(1000000,mu2,sqrt(sigma2_2))
xi = xi1 + xi2

med = median(xi)
q10 = quantile(xi, 0.1)
q90 = quantile(xi, 0.9)

mu_n = ln(m)-(ln(d/(m*m)+1))/2
sigma_n_2 = ln(d/(m*m)+1)

xi_n= rlnorm(1000000,mu_n,sqrt(sigma_n_2))

med_n = median(xi_n)
z10_n = quantile(xi_n, 0.1)
z90_n = quantile(xi_n, 0.9)

x <- c(xi,xi_n)
group <- c(rep("xi", 1000000), rep("eta", 1000000))
df <- data.frame(x, group)

err_med = (abs(med-med_n))/med
err_med = round(err_med,digits=4)
err_med

err_q10 = (abs(q10-z10_n))/q10
err_q10 = round(err_q10,digits=4)
err_q10

err_q90 = (abs(q90-z90_n))/q90
err_q90 = round(err_q90,digits=4)
err_q90

destination = "C:\\Users\\Пользователь\\Desktop\\Диплом\\curs\\img_new\\ris45.pdf"
pdf(file = destination)

#color = "#FFB90F",    color = "#FFBBFF"
#values = c("#CAFF70", "white", "#6E8B3D")   values = c("#FF83FA", "white", "#8B4789")
ggplot(df, aes(x = x, y = group, fill = stat(quantile))) +
  stat_density_ridges(quantile_lines = TRUE,
                      calc_ecdf = TRUE,
                      geom = "density_ridges_gradient",
                      quantiles = c(0.1, 0.9),
                      color = "#FFBBFF",
                      linetype = 1,
                      lwd = 0.75,
                      scale = 0.8) +
  scale_fill_manual(name = "Prob", values = c("#FF83FA", "white", "#8B4789"),
                    labels = c("(0, 10%]", "(10%, 90%]", "(90%, 1]"))+
  xlim(-10,500)+
  labs(x = "x", y = "p(x)")+
  theme(axis.text = element_text(color = "slateblue",
                                 size = 12, face = "bold"))+
  theme(axis.title = element_text(color = "#8B7355", size = 15, face = "bold"))+
  theme(axis.ticks.y = element_line(color = 2))

dev.off()