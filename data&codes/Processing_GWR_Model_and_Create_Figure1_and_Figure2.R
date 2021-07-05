############################################################################################################################################
#--------------------------------------------------AZAD RASUL@2021 Copyright----------------------------------------------------------------#
############################################################################################################################################
## Change the presentation of decimal numbers to 4 and avoid scientific notation
options(prompt="R> ", digits=4, scipen=999)
###########
knitr::opts_chunk$set(echo = TRUE)
library(GWmodel)# if it be in the begining it opened better
library(tidyverse)  # Modern data science workflow
library(sf)
library(sp)
library(rgdal)
library(rgeos)
library(tmap)
library(tmaptools)
library(spgwr)
library(grid)
library(gridExtra)
library(tseries)
library(raster)
library(RColorBrewer)
################ load data
lst <- read.csv("D:\\Relation of climate and 19\\Updated\\Combined_Climatic_Variables_and_Lag7R0",head=T)
head(lst)
### solve na
lst <- na.omit(lst) ### working!!!
lst
###
head(lst)
x <- lst[,4]
y <- lst[,5]
long <- y
lat <- x
lag7 <- lst[,11]
ws <- lst[,6]
temp <- lst[,7]
pres <- lst[,8]
rh <- lst[,9]
pre <- lst[,10]
#####
# linear multivariate regression, lm:(Linear model) = # Multiple Linear Regression Example
fit <- lm(lag7 ~ ws + temp + pres + rh + pre, data=lst)
fit
summary(fit)
names(fit)
###
ols <- glm(lag7 ~ ws + temp + pres + rh + pre, data=lst)
ols
#####
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(fit)
plot(fit, which =1)
plot(fit, which =3)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(ols)
plot(ols, which = 1)
plot(ols, which = 3)
#####
col.bw <- gwr.sel(lag7 ~ ws + temp  + rh + pres + pre, data=lst,
  coords=cbind(lst$long, lst$lat))
col.gauss <- gwr(lag7 ~ ws + temp + rh + pres + pre, data=lst,
  coords=cbind(lst$long, lst$lat), bandwidth=col.bw, hatmatrix=TRUE)
#Don’t use summary() for a GWR object. Just type the object name
col.gauss
col.gauss$results
col.gauss$results$AICc
sp <- col.gauss$SDF
sp
results<-as.data.frame(sp)
head(results)
### add predected and se predected to the original data
lst$pred<-results$pred
lst$se_pred<-results$pred.se
write.csv(lst, "lst3.csv")
summary(sp)
names(sp)
############# montecarlo test of p-value working!
DM<-gw.dist(dp.locat=coordinates(cbind(lst$long, lst$lat)))
res.mont1<-gwr.montecarlo(lag7 ~ ws + temp + pres + rh + pre, data = sp,dMat=DM,nsim=99, kernel="gaussian", adaptive=FALSE, bw=col.bw) ## Working!
#write.csv(sp, "SpatialPointsDataFrame of Covid Global lag7.csv")
################ Plot
#png("Figure1_Covid_Global_GWR_Climate_lag7_New_27_5_2021.png",width=8,height=10,units='in', res=300)
###
mypalette<-brewer.pal(3,"Spectral")
library(sp)
world_sp = as(world, Class = "Spatial")### Important working for world shapefile
mypalette<-brewer.pal(3,"Spectral")
### plot predected R0 and se of predected
#png("Figure1_Pre&Sepre1.png",width=9,height=9,units='in', res=300)
spplot(sp, c("localR2", "pred.se","pred" ),names.attr = c( "(c) localR2","(b) pred_se", "(a) pred") ,sp.layout = world_sp, cuts = c(-0.3,-0.2,0,0.1,0.2,1,1.5,2,2.5,3.25))
#dev.off()
###
spplot(col.gauss$SDF, 
c( "ws", "temp", "rh", "pres", "pre"),
    names.attr = c("(e) precipitation", "(d) pressure", "(c) rh", "(b) ws", "(a) temperature"),
scales=list(draw = TRUE),cex=.6,
cuts = c(-0.08,0,0.07),
key.space="right", col.regions=mypalette, layout = c(1,5),sp.layout = world_sp)##### working good!!!
####
#dev.off()