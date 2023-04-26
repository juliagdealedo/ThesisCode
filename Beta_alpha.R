library(performance)
library(glmmTMB)
library(ggplot2)
library(ggeffects)
library(dplyr)
library(patchwork)
library(readxl)
# Processing
library(tidyverse)
library(flextable) 
library(reshape2)
library(readxl)

# Import data ####
setwd("/Users/juliag.dealedo/ONE/UAM_Doctorado/Capitulos/cap2/data") 
resu_original <- read.table("resu_cat")
resu <- resu_original %>% filter(!Comunidad=="Aguapolo")
etno_original <- read_excel("etno-dis-mad-yas3.xlsx", sheet=1, col_names =T)
etno <- etno_original %>% filter(!Comunidad=="Aguapolo")
aguapolo <- etno_original %>% filter(Comunidad=="Aguapolo")
str(aguapolo)
length(unique(etno$Species))
plots_aguapolo <- unique(aguapolo$Cod_plot)

setwd("/Users/juliag.dealedo/ONE/UAM_Doctorado/Capitulos/cap2/data") 
plots_aguapolo
comp_original <- read.table("composition")
comp <- comp_original %>% filter(!Cod_plot %in% plots_aguapolo)
length(unique(comp$Species))
aguapolo_spp <- setdiff(aguapolo$Species, etno$Species)

# 2. Does knowledge depend on alpha and beta diversity?
setwd("/Users/juliag.dealedo/ONE/UAM_Doctorado/Capitulos/cap2/data") 
plots_aguapolo
comp_original <- read.table("composition")
comp <- comp_original %>% filter(!Cod_plot %in% plots_aguapolo)
etno$Plot <- as.numeric(etno$Plot)

df_spp <- comp %>% group_by(Plot) %>% summarize(Species_number = n_distinct(Species)) 
df_use <- etno %>%  group_by(Plot) %>% summarize(Uses_number = n_distinct(Use)) %>% filter(!is.na(Plot))

df <- merge(df_spp, df_use, by="Plot")
plot_com <- na.omit(as.data.frame(unique(cbind(etno$Plot, etno$Comunidad))))
colnames(plot_com) <- c("Plot", "Community")
df <- merge(df, plot_com, by="Plot")
# 
library(patchwork)
library(ggeffects)
library(lme4)
for1 <- "Uses_number ~ Species_number"
for2 <- "Uses_number ~ (1|Community)"
for3 <- "Uses_number ~ Species_number + (1|Community)"
for4 <- "Uses_number ~ Species_number + (Species_number|Community)"
glm_p1 <- glm(for1, family=poisson(link = log), data = df)
glm_p2 <- glmer (for2, family=poisson(link = log), data = df)
glm_p3 <- glmer (for3, family=poisson(link = log), data = df)
glm_p4 <- glmer (for4, family=poisson(link = log), data = df)

AICc <- round(AIC( glm_p1, glm_p2, glm_p3, glm_p4), 2)
gof(glm_p3) # hay overdispersion

# Negative binomial with negative cuadratic terms
glmer1 <- glm (for1, data = df)
glmer2 <- glmer.nb (for2, data = df)
glmer3 <- glmer.nb (for3, data = df) ### funciona y es mi preferido 4/4/23!!!!
glmer4 <- glmer.nb (for4, data = df)

AICc <- round(AIC(glmer1, glmer2, glmer3, glmer4), 2)
gof(glmer3) # no oversidpersion

# Table 1 - present on the manuscript
table1 <- cbind(Formulation, AICc)
Table1 <- qflextable (glmer3)
Table1
#print(Table1,  preview="docx")


Table2 <- rownames_to_column (as.data.frame(round(coef(summary(glmer3)),4)))
colnames(Table2)  <- c("Term", "Estimate", "SE", "z value", "p-value")
Table3 <- qflextable(Table2)
Table3



# Check residuals
par(mfcol=c(2,2))
Res <- residuals(glmer3, type="pearson")
Fit <- fitted(glmer3)
par(mfrow=c(2,2))
plot(Res ~ Fit, xlab="Fitted values", ylab="Residuals", main="Residuals vs. fitted")
abline(h=0)
plot(Res ~ df$Species_number, xlab="NAP", ylab="Residuals", main = "NAP") > abline(h=0)
hist(Res, main="Histogram of residuals", xlab="Residuals")
qqnorm(Res)
qqline(Res)

summary(mod5)

#Plot predictions with PREDICT
data1 <- df
head(data1)
min(data1$Species_number)
max(data1$Species_number)

x <- seq(21,177,length.out=500)
comunidad <- unique(data1$Community)
new_data <- data.frame(Species_number=rep(x, length(comunidad)), 
                       Community=rep(comunidad, each=length(x)))

prds <- ggpredict(glmer3, terms=c("Species_number", "Community"), type="random", ci.lvl=0.1)

prds$Community <- prds$group
pal <- brewer.pal(10, "Paired")
pal <- c( "#1F78B4", "#B2DF8A", "#33A02C",  "#CB6856","#6A3D9A", "#FDBF6F", "#FF7F00", "#CAB2D6", "#A6CEE3")


total_alpha <- ggpredict(glmer3, terms=c("Species_number"),ci.lvl = 0.95) %>% 
  plot(add.data=T, ci=T, limit.range = TRUE, colors="Community", alpha=0.2)+  
  labs(x="Species richness", y ="Use diversity")+
  scale_color_manual(values = pal)+
  scale_fill_manual(values = pal)+
  theme_classic() +
  theme(legend.position = "none") +
  ggtitle('Alpha diversity')

total_alpha
ind_alpha <- ggpredict(glmer3, terms=c("Species_number [all]", "Community"), type="random", ci.lvl=0.25) %>% 
  plot(add.data=T, ci=T, limit.range = TRUE, colors="Community", alpha=0.2)+
  facet_wrap(~group)+   labs(x="Species richness", y ="Use diversity")+
  scale_color_manual(values = pal)+
  scale_fill_manual(values = pal)+
  theme(legend.position = "none") +
  ggtitle('Per community')+ theme_classic() +  theme(legend.position = "none") 
ind_alpha

total_alpha/ind_alpha

ggsave("Alpha_april.pdf")


# BETA


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

# for all data:
preds <- predict(mod5, data.sim, se=T, re.form=NA)
plot_data <- data.frame(data.sim,
                           pred = exp(preds$fit)/(1+exp(preds$fit)),
                           low = exp(preds$fit - 1.96*preds$se.fit)/(1+exp(preds$fit - 1.96*preds$se.fit)), 
                           upp = exp(preds$fit + 1.96*preds$se.fit)/(1+exp(preds$fit + 1.96*preds$se.fit)))

head(plot_data)


# per each community 

preds_ind <- predict(mod5, data.sim, se=T, re.form=NULL)
plot_data_ind <- data.frame(data.sim,
                        pred = exp(preds_ind$fit)/(1+exp(preds_ind$fit)),
                        low = exp(preds_ind$fit - 1.96*preds_ind$se.fit)/(1+exp(preds_ind$fit - 1.96*preds_ind$se.fit)), 
                        upp = exp(preds_ind$fit + 1.96*preds_ind$se.fit)/(1+exp(preds_ind$fit + 1.96*preds_ind$se.fit)))

head(plot_data)

# Plot both

all_beta <- ggplot(data2) +
  geom_point(data=data2, aes(x=comp, y=etno), color="gray70")+
  geom_line(data=plot_data, aes(x = comp, y = pred, colour = Comunidad), color="black")+
  geom_ribbon(data=plot_data, aes(x=comp, ymin = low, ymax = upp), alpha = 0.1) +
  theme_classic()+labs(x="Floristic dissimilarity", y ="Knowledge dissimilarity")+
  scale_color_manual(values = pal) + scale_fill_manual(values = pal)+
  scale_x_continuous(limits = c(0.37, 1)) +
  theme(legend.position="none")+
  ggtitle('Beta diversity')


individual_beta <- ggplot(data2) +
  geom_point(data=data2, aes(x=comp, y=etno, colour=Comunidad))+
  facet_wrap(vars(Comunidad))+
  geom_line(data=plot_data_ind, aes(x = comp, y = pred, colour = Comunidad))+
  geom_ribbon(data=plot_data_ind, aes(x=comp, ymin = low, ymax = upp), alpha = 0.1) +
  theme_classic()+labs(x="Floristic dissimilarity", y ="Knowledge dissimilarity")+
  scale_color_manual(values = pal) + scale_fill_manual(values = pal)+
  scale_x_continuous(limits = c(0.37, 1)) +
  theme(legend.position="none")+
  ggtitle('Per community')


all_beta/individual_beta
ggsave("Beta_april.pdf")



# Sum plots
patch <- total_alpha/ind_alpha | all_beta/individual_beta
patch+plot_annotation(tag_levels = 'A')
setwd("/Users/juliag.dealedo/ONE/UAM_Doctorado/github/cap2/output/figures")
ggsave("com_com_4.pdf",height = 17, width = 17,  scale=.5)



