library(performance)
library(glmmTMB)
library(ggplot2)
library(ggeffects)
library(dplyr)

#save(pal, data2, file="dataluis.RData")

setwd("/home/luis/Investigacion/Articulos cientificos/En preparacion/2023 - Capt 2 Julia/R2")
load(file = "dataluis.RData")

mod3<- glmmTMB(etno ~ (1 | Comunidad), family  = beta_family, data=data2)
mod4<- glmmTMB(etno ~ comp + (1 | Comunidad), family  = beta_family, data=data2)
mod5<- glmmTMB(etno ~ comp + (comp|Comunidad), family  = beta_family, data=data2)
mod6<- glmmTMB(etno ~ comp + (comp + 0 | Comunidad ), family  = beta_family, data=data2)
mod7<- glmmTMB(etno ~ comp + (comp || Comunidad), family  = beta_family, data=data2)
mod8<- glmmTMB(etno ~ comp * Comunidad + (1 | Comunidad), family  = beta_family, data=data2)

Akaike <- AIC(mod3, mod4, mod5, mod6, mod7)
Akaike

subset(Akaike, Akaike$AIC< ((min(Akaike$AIC))+2))

# Check residuals
par(mfcol=c(2,2))
Res <- residuals(mod5, type="pearson")
Fit <- fitted(mod5)
par(mfrow=c(2,2))
plot(Res ~ Fit, xlab="Fitted values", ylab="Residuals", main="Residuals vs. fitted")
abline(h=0)
plot(Res ~ data2$comp, xlab="NAP", ylab="Residuals", main = "NAP") > abline(h=0)
hist(Res, main="Histogram of residuals", xlab="Residuals")
qqnorm(Res)
qqline(Res)

summary(mod5)
r2(mod5)

#Plot predictions with PREDICT

x <- seq(0.0001,0.9999,length.out=500)
comunidad <- unique(data2$Comunidad)
data.sim <- data.frame(comp=rep(x, length(comunidad)), 
                       Comunidad=rep(comunidad, each=length(x)))
preds <- predict(mod5, data.sim, se=T, re.form=NA)
#min <- min(data2$comp)
#max <- max(data2$comp)
#newdat <-data.frame(comp=(seq(min, max, length.out = 760)),
#                             Comunidad = data2$Comunidad)
plot_data <- data.frame(data.sim,
                           pred = exp(preds$fit)/(1+exp(preds$fit)),
                           low = exp(preds$fit - 1.96*preds$se.fit)/(1+exp(preds$fit - 1.96*preds$se.fit)), 
                           upp = exp(preds$fit + 1.96*preds$se.fit)/(1+exp(preds$fit + 1.96*preds$se.fit)))

head(plot_data) # I am not getting the correct predictions... 

# Plot predictions with GGPREDICT

mydf <- ggpredict(mod5, terms=c("comp", "Comunidad"), type = "random", ci.lvl = 0.95)
mydf <- mydf %>% dplyr::rename(Comunidad=group)
head(mydf) # Confidence intervals between 0,1....

# However it gets ploted 
ggplot(mydf, aes(x = x, y = predicted, colour = Comunidad)) +
  geom_line()+
  geom_point(data=data2, aes(x=comp, y=etno, colour=Comunidad))+
  facet_wrap(vars(Comunidad))+
  theme_classic()+labs(x="Floristic dissimilarity", y ="Knowledge dissimilarity")+
  scale_color_manual(values = pal)+  scale_fill_manual(values = pal)+
  theme(legend.position="none")
  
