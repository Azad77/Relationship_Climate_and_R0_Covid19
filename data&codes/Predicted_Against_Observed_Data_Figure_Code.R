############################################################################################################################################
#--------------------------------------------------AZAD RASUL@2021 Copyright----------------------------------------------------------------#
############################################################################################################################################
library(ggplot2)
library(RColorBrewer)
library(ggpmisc)
library(dplyr)
####################
tt<-read.csv("D:\\Relation of climate and 19\\Updated\\lst4.csv")
b1 <- tt[,1]
b2 <- tt[,2]
b3 <- tt[,3]
b1
b2
b3
type <- as.factor(b3)
#############
legend_title <- "type"
my.formula <- y ~ x
###
fit <- lm(b1~b2, data = tt)
## Calculate RMSE
#rmse <- round(sqrt(mean(resid(fit)^2)), 2)
rmse <- c("RMSE: 2.12", "RMSE: 2.13")
#############
#png("Figure3_validate_COvid_global_New.png",width=8,height=8,units='in', res=300)
############# 
ggplot(tt, aes(x=b2, y=b1, color=type, shape=type)) +
  geom_point(size=2.5) + 
  geom_smooth(method=lm, se=T, fullrange=TRUE,aes(fill=type), alpha = 0.1)+
  stat_poly_eq(formula = my.formula, aes(label = paste( ..rr.label.., ..p.value.label..,rmse, sep = "~~~")), parse = TRUE)+
  theme_bw()+
  ylab("Observed R0 of COVID-19")+
  xlab("Predicted R0 of COVID-19 ")+
  theme(legend.position="top")
################
#dev.off()

