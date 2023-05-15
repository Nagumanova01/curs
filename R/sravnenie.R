library(lognorm)
library(ggplot2)
library(dplyr)
library(SciViews)
library(pracma)
library(cubature)
library(ggridges)

#library(tidyr)
#library(googlesheets4)
#library(ggrepel)

# mu<=12, sigma^2<=2.25
#erf <- function(x){2 * pnorm(x * sqrt(2)) - 1}

mu1 = 4
mu2 = 4
sigma1_2 = 0.25
sigma2_2 = 0.25

m=exp(mu1+sigma1_2/2)+exp(mu2+sigma2_2/2)
d=(exp(2*mu1+sigma1_2))*(exp(sigma1_2)-1)+(exp(2*mu2+sigma2_2))*(exp(sigma2_2)-1)

xi1 = rlnorm(1000000,mu1,sqrt(sigma1_2))
xi2 = rlnorm(1000000,mu2,sqrt(sigma2_2))
xi = xi1 + xi2
#describe(xi)
med = median(xi)
q10 = quantile(xi, 0.1)
q90 = quantile(xi, 0.9)


mu_n = ln(m)-(ln(d/(m*m)+1))/2
sigma_n_2 = ln(d/(m*m)+1)


xi_n= rlnorm(1000000,mu_n,sqrt(sigma_n_2))
#describe(xi_n)
med_n = median(xi_n)
z10_n = quantile(xi_n, 0.1)
z90_n = quantile(xi_n, 0.9)

#x <- c(xi,xi_n)
x <- c(xi,xi_n)
group <- c(rep("xi", 1000000), rep("xi_n", 1000000))
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

destination = "C:\\Users\\Пользователь\\Desktop\\Диплом\\curs\\img\\sr1.pdf"
pdf(file = destination)
ggplot(df, aes(x = x, y = group, fill = stat(quantile))) +
  stat_density_ridges(quantile_lines = TRUE,
                      calc_ecdf = TRUE,
                      geom = "density_ridges_gradient",
                      quantiles = c(0.1, 0.9),
                      color = "olivedrab",
                      linetype = 1,
                      lwd = 0.75,
                      scale = 0.8) +
  scale_fill_manual(name = "Prob", values = c("#E2FFF2", "white", "#B0E0E6"),
                    labels = c("(0, 10%]", "(10%, 90%]", "(90%, 1]"))+
  xlim(0,700)+
  labs(x = "", y = "")
dev.off()

destination = "C:\\Users\\Пользователь\\Desktop\\Диплом\\curs\\img\\sr2.pdf"
pdf(file = destination)
ggplot(df, aes(x = x, y = group, fill = stat(quantile))) +
  stat_density_ridges(quantile_lines = TRUE,
                      calc_ecdf = TRUE,
                      geom = "density_ridges_gradient",
                      quantiles = c(0.1, 0.9),
                      color = "#FFB90F",
                      linetype = 1,
                      lwd = 0.75,
                      scale = 0.8) +
  scale_fill_manual(name = "Prob", values = c("#CAFF70", "white", "#6E8B3D"),
                    labels = c("(0, 10%]", "(10%, 90%]", "(90%, 1]"))+
  xlim(NA,750)+
  labs(x = "", y = "")
dev.off()


#position = "identity"

destination = "C:\\Users\\Пользователь\\Desktop\\Диплом\\curs\\hist_new_3.pdf"
pdf(file = destination)
ggplot(df, aes(x = x, fill = group)) + 
  geom_histogram(color = 1, alpha = 0.4,
                 position = "dodge", bins=70) +
  scale_fill_manual(name = "", values = c("#8795E8", "#FF6AD5"), labels = c("A" = "xi=xi1+xi2", "B" = "xi_n"))+
  ggtitle(paste("mu1 =", mu1,  "  mu2 =", mu2, "  (sig1)^2 =", sigma1_2, "  (sig2)^2 =", sigma2_2),
          subtitle = paste("err_q10 =", err_q10*100, "%", "   err_med =", err_med*100, "%",  "   err_q90 =", err_q90*100, "%"))+
  coord_cartesian(xlim =c (NA, 1000))
dev.off()


ggplot(df, aes(x = x, fill = group)) + 
  geom_histogram(color = 1, alpha = 0.4,
                 position = "dodge", bins=100) +
  scale_fill_manual(name = "", values = c("#8795E8", "#FF6AD5"), labels = c("A" = "xi=xi1+xi2", "B" = "xi_n"))+
  ggtitle(paste("mu1 =", mu1,  "  mu2 =", mu2, "  (sig1)^2 =", sigma1_2, "  (sig2)^2 =", sigma2_2),
          subtitle = paste("err_q10 =", err_q10*100, "%", "   err_med =", err_med*100, "%",  "   err_q90 =", err_q90*100, "%"))+
  coord_cartesian(xlim =c (NA, 3000))