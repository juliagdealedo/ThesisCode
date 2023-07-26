# R Script - Chapter II JGA Thesis
# Date: 26/07/2023

# Load necessary libraries

library(dplyr)
library(readxl)
library(flextable) 
library(tidyr)
library(stringr)
library(officer)

library(reshape2)
library(tibble)
library(vegan)

library(lme4)
library(aods3)
library(MuMIn)

library(gtools)
library(data.table)
library(glmmTMB)
library(performance)

library(ggeffects)
library(RColorBrewer)
library(ggplot2)
library(patchwork)

# Read the data 

setwd("/Users/juliag.dealedo/ONE/UAM_Doctorado/Capitulos/cap2/data") 

comp_original <- read.table("composition")
comp_original$WasColllected<-gsub("True", 1, comp_original$WasColllected) #
comp_original$WasColllected<-gsub("False", 0, comp_original$WasColllected) #
comp_original$WasColllected <- as.numeric(comp_original$WasColllected)

resu_original <- read.table("resu_cat")
resu <- resu_original %>% filter(!Comunidad=="Aguapolo")

etno_original <- read_excel("etno-dis-mad-yas3.xlsx", sheet=1, col_names =T)
etno <- etno_original %>% filter(!Comunidad=="Aguapolo")
aguapolo <- etno_original %>% filter(Comunidad=="Aguapolo")
plots_aguapolo <- unique(aguapolo$Cod_plot)

comp <- comp_original %>% filter(!Cod_plot %in% plots_aguapolo)
length(unique(comp$Species))
aguapolo_spp <- setdiff(aguapolo$Species, etno$Species)

# Re-categorization and re-naming of ethnobotanical categories

# Category
etno$Category2 <- etno$Category
etno$Category2 <- gsub("MEDICINAL AND VETERINARY", "MEDICINAL", etno$Category2)
etno$Category2<-gsub("CULTURAL USES", "CULTURAL", etno$Category2) #
etno$Category2<-gsub("CONSTRUCTION USES", "CONSTRUCTION", etno$Category2) #
etno$Category2<-gsub("FIREWOOD", "FUEL", etno$Category2) #
etno$Category2<-gsub("UTENSILS & TOOLS", "UTENSILS", etno$Category2) #
etno$Category2<-gsub("HUMAN FOOD", "FOOD", etno$Category2) #
etno$Category2<-gsub("TOXIC", "CULTURAL", etno$Category2) 
etno <- etno[!etno$Category2=="ANIMAL FOOD",]
etno <- etno[!etno$Category2=="OTHER",]
etno <- etno[!etno$Category2=="MARKETED",]
etno <- etno[!etno$Category2=="WILD ANIMAL",]
etno <- etno[!etno$Category2=="ENVIRONMENTAL",]

# Use
etno$Use <- gsub("MEDICINAL AND VETERINARY", "MEDICINAL", etno$Use)
etno$Use<-gsub("CULTURAL USES", "CULTURAL", etno$Use) 
etno$Use<-gsub("CONSTRUCTION USES", "CONSTRUCTION", etno$Use) 
etno$Use<-gsub("FIREWOOD", "FUEL", etno$Use) 
etno$Use<-gsub("UTENSILS & TOOLS", "UTENSILS", etno$Use) 
etno$Use<-gsub("HUMAN FOOD", "FOOD", etno$Use) 
etno$Use<-gsub("TOXIC", "CULTURAL", etno$Use) 
etno <- etno[!etno$Use=="ANIMAL FOOD",]
etno <- etno[!etno$Use=="OTHER",]
etno <- etno[!etno$Use=="MARKETED",]
etno <- etno[!etno$Use=="WILD ANIMAL",]
etno <- etno[!etno$Use=="ENVIRONMENTAL",]

## 5.3.2. Does plant knowledge depend on alpha and beta diversity?

# Count spp and uses per plot

etno$Plot <- as.numeric(etno$Plot)
df_spp <- comp %>% group_by(Plot) %>% summarize(Species_number = n_distinct(Species)) 
df_use <- etno %>%  group_by(Plot) %>% summarize(Uses_number = n_distinct(Use)) %>% filter(!is.na(Plot))
df_ind <- comp %>%  group_by(Plot) %>% summarize(Ind_number = n_distinct(Cod_item)) %>% filter(!is.na(Plot))
df_spp_use <- merge(df_spp, df_use, by="Plot")
df_ind <- merge (df_spp_use, df_ind, by="Plot")
plot_com <- na.omit(as.data.frame(unique(cbind(etno$Plot, etno$Comunidad))))
colnames(plot_com) <- c("Plot", "Community")
df_spp_use <- merge(df_spp_use, plot_com, by="Plot")

# Check correlation with other alpha indexes

spp_matrix <- dcast(comp, Plot~Species, value.var="Species", fill=0)
spp_matrix <- column_to_rownames(spp_matrix,var="Plot")
hshannon <- diversity(spp_matrix, index="shannon") 
simpson <- diversity(spp_matrix, index="simpson")
invsimpson <- diversity(spp_matrix, index="invsimpson")
jeve <- hshannon/log(specnumber(spp_matrix))
falpha <- fisher.alpha(spp_matrix) 
Srar <- rarefy(spp_matrix, min(rowSums(spp_matrix)))
table.s1 <- cbind(df_ind, falpha,hshannon,simpson,invsimpson,jeve,Srar)
cor <- cor(table.s1[,-c(1)])
corrplot::corrplot(cor) # >> Selecting species number

# Models and statistics 

for1 <- "Uses_number ~ (1|Community)"
for2 <- "Uses_number ~ Species_number"
for3 <- "Uses_number ~ Species_number + (1|Community)"
for4 <- "Uses_number ~ Species_number + (Species_number|Community)"

# 1) GLM poission family
df <- df_spp_use
glm_p1 <- glmer (for1, family=poisson(link = log), data = df)
glm_p2 <- glm(for2, family=poisson(link = log), data = df)
glm_p3 <- glmer (for3, family=poisson(link = log), data = df)
glm_p4 <- glmer (for4, family=poisson(link = log), data = df)

AICc <- round(AIC( glm_p1, glm_p2, glm_p3, glm_p4), 2)
Formulation <- c(for1, for2, for3, for4)
gof(glm_p3) # overdispersion

# 2) Negative binomial with negative cuadratic terms
for1 <- "Uses_number ~ (1|Community)"
for2 <- "Uses_number ~ Species_number"
for3 <- "Uses_number ~ Species_number + (1|Community)"
for4 <- "Uses_number ~ Species_number + (Species_number|Community)"
glmer1 <- glmer.nb (for1, data = df)
glmer2 <- glm (for2, data = df)
glmer3 <- glmer.nb (for3, data = df) 
glmer4 <- glmer.nb (for4, data = df)

AICc <- round(AIC( glmer1, glmer2, glmer3, glmer4), 2)

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

Formulation <- c(for1, for2, for3, for4)
table1 <- cbind(Formulation, AICc)
rglm1 <- round(r.squaredGLMM(glmer1)[1,], 3) 
rglm2 <- round(r.squaredGLMM(glmer2)[1,], 3) 
rglm3 <- round(r.squaredGLMM(glmer3)[1,], 3) 
rglm4 <- round(r.squaredGLMM(glmer4)[1,], 3) 
rsquared <- rbind(rglm1, rglm2, rglm3, rglm4)

# Create Table 5.1 
table5.1 <- cbind(Formulation, AICc, rsquared)
Table5.1 <- qflextable (table5.1)
# print(Table5.1, preview="docx")

# Calculate predictions
data1 <- df
min(data1$Species_number)
max(data1$Species_number)
x <- seq(21,177,length.out=500)
comunidad <- unique(data1$Community)
new_data <- data.frame(Species_number=rep(x, length(comunidad)), 
                       Community=rep(comunidad, each=length(x)))
prds <- ggpredict(glmer3, terms=c("Species_number", "Community"), type="random", ci.lvl=0.1)
prds$Community <- prds$group

# Plot Figure 5.3. AB
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
  plot(add.data=T, ci=T, limit.range = TRUE, colors="Community", alpha=0.5)+
  facet_wrap(~group)+   labs(x="Species richness", y ="Use diversity")+
  scale_color_manual(values = pal)+
  scale_fill_manual(values = pal)+
  theme(legend.position = "none") +
  ggtitle('Per community')+ theme_classic() +  theme(legend.position = "none") 
ind_alpha

total_alpha/ind_alpha
ggsave("Alpha.svg")
ggsave("Alpha.pdf")

#### BETA

# Prepare the table: get all possible combinations of two columns
indices <- df$Plot
variables <- df$Community
dt <- data.table(indices, variables)
get_permutations <- function(dt){
  perm <- gtools::permutations(nrow(unique(dt[,1])), 2, dt$indices)
  as.data.table(perm)
}
ds <- dt[, get_permutations(.SD), by = variables]
colnames(ds) <- c("Comunidad", "Plot1", "Plot2")

# Floristic distances calculation
comp_dist <- reshape2::dcast(comp, Plot~Species, value.var="Species", fill=0)
comp_dist <- column_to_rownames(comp_dist, var="Plot")
abs_comp_dist <- as.matrix(comp_dist>0)*1
rowSums(abs_comp_dist)
comp_matrix_dist_pres <- vegdist(abs_comp_dist, method="jaccard")

# Cultural distances calculation
df_etno <- as.data.frame(unique(cbind(etno$informant, etno$Species, etno$Use)))
colnames(df_etno) <- c("Informant", "Species", "Use")
no_etno <- df_etno %>% filter(is.na(Use))
df_etno <- df_etno %>% filter(!is.na(Use))
df_etno$tomerge <- paste(df_etno$Informant, df_etno$Species)
df_etno2 <- as.data.frame(unique(cbind(etno$Plot,etno$informant, etno$Species, etno$Use)))
colnames(df_etno2) <- c("Plot", "Informant", "Species", "Use")
df_etno2$Plot <- as.numeric (df_etno2$Plot)
df_etno2 <- df_etno2 %>% drop_na(Plot)
df_etno2$tomerge <- paste(df_etno2$Informant, df_etno2$Species)
df_etno3 <- merge(df_etno2[c(1,5)], df_etno, by="tomerge", all=T)
df_etno4 <- unique(df_etno3)
df_etno4$Plot <- as.numeric (df_etno4$Plot)
use_dist <- reshape2::dcast(df_etno4, Plot~Use, value.var="Species", fill=0)
use_dist <- column_to_rownames(use_dist, "Plot")
etno_matrix_dist <- vegdist(use_dist, method="bray")
disetno <- etno_matrix_dist
discomp <-comp_matrix_dist_pres

# From matrix to vector
# A) Floristic
dist_comp <- as.matrix(discomp)
dist_comp[upper.tri(dist_comp)] <- NA
dist_comp2 <- reshape2::melt(dist_comp, na.rm = TRUE)
dist_comp3 <- dist_comp2 %>%
  dplyr::filter(!(value == 0)) %>%
  dplyr::rename(Plot1 = Var1, 
                Plot2 = Var2, 
                comp = value)

# B) Cultural
dist_etno <- as.matrix(disetno)
dist_etno[upper.tri(dist_etno)] <- NA
dist_etno2 <- reshape2::melt(dist_etno, na.rm = TRUE)
dist_etno3 <- dist_etno2 %>%
  dplyr::filter(!(value == 0)) %>%
  dplyr::rename(Plot1 = Var1, 
                Plot2 = Var2, 
                etno = value)
distances <- cbind(dist_etno3, dist_comp3)
distances$Plot_comb <- paste(distances$Plot1, distances$Plot2)

# Merge with all-combination df
ds$Plot_comb <- paste(ds$Plot1, ds$Plot2)
data2 <- merge(ds, distances, by="Plot_comb")[,c(2,7,8)]

# Statistical analysis: Models
for1<-"Knowledge dissimilarity ~ (1 | Community)"
for2 <-"Knowledge dissimilarity ~ Floristic dissimilarity + (1 | Community)"
for3 <-"Knowledge dissimilarity ~ Floristic dissimilarity + (Floristic dissimilarity | Community)"

# GLMM beta error distribution
mod1<- glmmTMB(etno ~ (1 | Comunidad), family  = beta_family, data=data2)
mod2<- glmmTMB(etno ~ comp + (1 | Comunidad), family  = beta_family, data=data2)
mod3<- glmmTMB(etno ~ comp + (comp|Comunidad), family  = beta_family, data=data2)
Akaike <- AIC(mod1, mod2, mod3)
Formulation <- c(for1, for2, for3)

# Check residuals
par(mfcol=c(2,2))
Res <- residuals(mod3, type="pearson")
Fit <- fitted(mod3)
par(mfrow=c(2,2))
plot(Res ~ Fit, xlab="Fitted values", ylab="Residuals", main="Residuals vs. fitted")
abline(h=0)
plot(Res ~ data2$comp, xlab="NAP", ylab="Residuals", main = "NAP") > abline(h=0)
hist(Res, main="Histogram of residuals", xlab="Residuals")
qqnorm(Res)
qqline(Res)

# Function to chose lowest AIC
subset(Akaike, Akaike$AIC< ((min(Akaike$AIC))+2))

# Calculate R2
rmod1 <- as.data.frame(r2(mod1))[1:2]
rmod2 <- as.data.frame(r2(mod2))[1:2]
rmod3 <- as.data.frame(r2(mod3))[1:2]

# Create Table 5.2 
Table5.2 <- cbind(Formulation, round(Akaike, 2), round(rbind(rmod1, rmod2, rmod3), 3))
Table5.2 <- qflextable(Table5.2)
# print(Table5.2, preview = "docx")

# Calculate predictions

x <- seq(0.0001,0.9999,length.out=500)
comunidad <- unique(data2$Comunidad)
data.sim <- data.frame(comp=rep(x, length(comunidad)), 
                       Comunidad=rep(comunidad, each=length(x)))

# All data
preds <- predict(mod3, data.sim, se=T, re.form=NA)
plot_data <- data.frame(data.sim,
                        pred = exp(preds$fit)/(1+exp(preds$fit)),
                        low = exp(preds$fit - 1.96*preds$se.fit)/(1+exp(preds$fit - 1.96*preds$se.fit)), 
                        upp = exp(preds$fit + 1.96*preds$se.fit)/(1+exp(preds$fit + 1.96*preds$se.fit)))

# Per Community

preds_ind <- predict(mod3, data.sim, se=T, re.form=NULL)
plot_data_ind <- data.frame(data.sim,
                            pred = exp(preds_ind$fit)/(1+exp(preds_ind$fit)),
                            low = exp(preds_ind$fit - 1.96*preds_ind$se.fit)/(1+exp(preds_ind$fit - 1.96*preds_ind$se.fit)), 
                            upp = exp(preds_ind$fit + 1.96*preds_ind$se.fit)/(1+exp(preds_ind$fit + 1.96*preds_ind$se.fit)))

# Plot both
all_beta <- ggplot(data2) +
  geom_point(data=data2, aes(x=comp, y=etno), color="gray70")+
  geom_line(data=plot_data, aes(x = comp, y = pred, colour = Comunidad), color="black")+
  geom_ribbon(data=plot_data, aes(x=comp, ymin = low, ymax = upp), alpha = 0.2) +
  theme_classic()+labs(x="Floristic dissimilarity", y ="Knowledge dissimilarity")+
  scale_color_manual(values = pal) + scale_fill_manual(values = pal)+
  scale_x_continuous(limits = c(0.57, 1)) +
  theme(legend.position="none")+
  ggtitle('Beta diversity')

individual_beta <- ggplot(data2) +
  geom_point(data=data2, aes(x=comp, y=etno, colour=Comunidad))+
  facet_wrap(vars(Comunidad))+
  geom_line(data=plot_data_ind, aes(x = comp, y = pred, colour = Comunidad))+
  geom_ribbon(data=plot_data_ind, aes(x=comp, ymin = low, ymax = upp), alpha = 0.2) +
  theme_classic()+labs(x="Floristic dissimilarity", y ="Knowledge dissimilarity")+
  scale_color_manual(values = pal) + scale_fill_manual(values = pal)+
  scale_x_continuous(limits = c(0.57, 1)) +
  theme(legend.position="none")+
  ggtitle('Per community')

all_beta/individual_beta

ggsave("Beta_april.pdf")
ggsave("Beta_april.svg")

# Add plots
patch <- total_alpha/ind_alpha | all_beta/individual_beta
patch+plot_annotation(tag_levels = 'A')
ggsave("Figure5.3.pdf",height = 17, width = 17,  scale=.5)
ggsave("Figure5.3.svg",height = 17, width = 17,  scale=.5)

## 5.3.3. Is there knowledge convergence or divergence across Indigenous communities?

set.seed(100)
useMDS <-metaMDS(use_dist, distance="bray", k=2, trymax= 1000) ##k is the number of dimensions
compMDS <-metaMDS(abs_comp_dist, distance="jaccard", k=2, trymax= 1000, previous.best = useMDS) ##k is the number of dimensions

grid.col = c( "Bolivar" = "#1F78B4", Dicaro="#B2DF8A", Guiyero="#33A02C", Infierno = "#CB6856",Macahua = "#6A3D9A",
              "Nueva Vida" = "#FDBF6F", "San Carlos" = "#FF7F00", Tumupasa = "#CAB2D6", Yamino = "#A6CEE3")
cols <- grid.col[resu$Comunidad]

setwd("/Users/juliag.dealedo/ONE/UAM_Doctorado/Capitulos/cap2/figs/figs_2023")
svg("nmds_3.svg", height=8,width=13, pointsize=15)
pdf("nmds_3.pdf", height=8,width=8, pointsize=12)
par(mfcol=c(1,2))
plot(useMDS$points, pch=21, cex=1.5, col="white",bg ="white", cex.axis=1, cex.lab=1 ,xlab="NMDS1", ylab="NMDS2", main="Knowledge")
points(useMDS$points, pch=21, cex=1.3, col="black", bg = alpha(cols,0.9))
ordihull(useMDS, resu$Comunidad, col=grid.col, draw="polygon", border="white")
plot(compMDS$points, pch=21, cex=1.5, yaxt='n', ann=FALSE, col="white",bg ="white", cex.axis=1, cex.lab=1 ,xlab="NMDS1", ylab="NMDS2", main="Floristic")
points(compMDS$points, pch=21, cex=1.3, col="black", bg = alpha(cols,0.9))
ordihull(compMDS, resu$Comunidad, col=grid.col, draw="polygon", border="white")
dev.off()
plot.new()

# Calculate distance between the centroids of each Indigenous community
set.seed(123) 
useMDS <-metaMDS(use_dist, distance="bray", k=2, trymax= 1000) 
compMDS <-metaMDS(abs_comp_dist, distance="jaccard", k=2, trymax= 1000, previous.best = useMDS) 
centro_use <- as.data.frame(t(summary(ordihull(useMDS, resu$Comunidad))))
centro_comp <-as.data.frame(t(summary(ordihull(compMDS, resu$Comunidad))))
points(centro_comp$NMDS1, centro_comp$NMDS2, pch=2)
points(centro_use$NMDS1, centro_use$NMDS2, pch=4)
mat_comp <- as.matrix(dist(centro_comp[,1:2], method = "maximum", diag = F))
mat_use <- as.matrix(dist(centro_use[,1:2], method = "maximum", diag = F))
mat <- mat_comp-mat_use
df <- data.frame(which(!is.na(mat), arr.ind = TRUE))
df_2 <- reshape2::melt(mat)
df_3 <- cbind(df, df_2)
df_colours <- df_3[df_3$row <= df_3$col, ]
df_4 <- df_3[df_3$row <= df_3$col, ]
df_4$value <- mat[cbind(df_4$row, df_4$col)]
df_4 <- df_4[,3:5]
colnames(df_4) <- c("Community1", "Community2", "subtr")
df_4 <- df_4 %>% filter(!subtr==0)
df_4$group <- paste(df_4$Community1, df_4$Community2, sep="-")
df_4 <- df_4 %>% mutate (value_fill= ifelse(subtr<0, "Cultural", "Floristic"))
length(df_4$subtr[df_4$subtr>0])/length(df_4$subtr)*100 
length(df_4$subtr[df_4$subtr<0])/length(df_4$subtr)*100 

# Calculate which of the differences is significant with a loop
set.seed(123) 
n_repeticiones <- 99 
diferencias_aleatorias_rep <- vector("numeric", n_repeticiones)
df_66 <- NULL
conf_int <- NULL
for (i in 1:n_repeticiones) {
  use_dist_aleatoria <- use_dist[sample(nrow(use_dist)), ]
  rownames(use_dist_aleatoria) <- rownames(use_dist)
  comp_dist_aleatoria <- abs_comp_dist[sample(nrow(abs_comp_dist)), ]
  rownames(comp_dist_aleatoria) <- rownames(abs_comp_dist)
  useMDS <-metaMDS(use_dist_aleatoria, distance="bray", k=2, trymax= 1000) ##k is the number of dimensions
  compMDS <-metaMDS(comp_dist_aleatoria, distance="jaccard", k=2, trymax= 1000, previous.best = useMDS) ##k is the number of dimensions
  centro_use <- as.data.frame(t(summary(ordihull(useMDS, resu$Comunidad))))
  centro_comp <-as.data.frame(t(summary(ordihull(compMDS, resu$Comunidad))))
  points(centro_comp$NMDS1, centro_comp$NMDS2, pch=2)
  points(centro_use$NMDS1, centro_use$NMDS2, pch=4)
  mat_comp <- as.matrix(dist(centro_comp[,1:2], method = "maximum", diag = F))
  mat_use <- as.matrix(dist(centro_use[,1:2], method = "maximum", diag = F))
  mat <- mat_comp-mat_use
  df_rm <- data.frame(which(!is.na(mat), arr.ind = TRUE))
  df_2_rm <- reshape2::melt(mat)
  df_3_rm <- cbind(df_rm, df_2_rm)
  df_colours <- df_3_rm[df_3_rm$row <= df_3_rm$col, ]
  df_4_rm <- df_3_rm[df_3_rm$row <= df_3_rm$col, ]
  df_4_rm$value <- mat[cbind(df_4_rm$row, df_4_rm$col)]
  df_4_rm <- df_4_rm[,3:5]
  colnames(df_4_rm) <- c("Community1", "Community2", "subtr")
  df_4_rm <- df_4_rm %>% filter(!subtr==0)
  df_4_rm$group <- paste(df_4_rm$Community1, df_4_rm$Community2, sep="-")
  df_4_rm <- df_4_rm %>% mutate (value_fill= ifelse(subtr<0, "Cultural", "Floristic"))
  df_66[[i]] <- df_4_rm$subtr
  
}

# Join values of each row in one vector per row 
row_values <- lapply(df_66, function(x) unlist(x))
matrix_values <- as.data.frame(do.call(rbind, row_values))
for (i in 1:ncol(matrix_values)) {
  conf_int_rm <- quantile(matrix_values[,i], probs = c(0.05, 0.95))
  lower_limits <- as.data.frame(conf_int_rm)[1,1]
  upper_limits <- as.data.frame(conf_int_rm)[2,1]
  df_4$Significativo[i] <- ifelse(df_4$subtr[i] < lower_limits| df_4$subtr[i]> upper_limits, TRUE, FALSE)
}
df_4

# Plot Figure 5.4.
pal <- brewer.pal(10, "Paired")
pal <- c ("#1F78B4", "#B2DF8A", "#33A02C",  "#CB6856","#6A3D9A", "#FDBF6F", "#FF7F00", "#CAB2D6", "#A6CEE3")
grid.col = c (SanCarlos = "#FF7F00", Dicaro="#B2DF8A", Guiyero="#33A02C", Infierno = "#CB6856",Macahua = "#6A3D9A",
              NuevaVida = "#FDBF6F", Bolivar = "#1F78B4", Tumupasa = "#CAB2D6", Yamino = "#A6CEE3")
df_new <- data.frame(Com1=df_4$Community1,
                     Com2=df_4$Community2,
                     group=df_4$group,
                     subtr=df_4$subtr,
                     Pair1=1,
                     Pair2=2)

bar_predominance <- ggplot(df_4, aes(x=subtr, y=reorder(group,subtr), fill=value_fill))+
  geom_bar(stat='identity')+
  scale_fill_manual(values=c('#E69F00', '#299617'), name="Difference by:", labels = c("More cultural distance than floristic", "More floristic distance than cultural"))+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_blank(),
        legend.position = c(0.6, 0.2))+
  labs(x = "Difference between cultural and floristic distance", title="Predominant floristic or cultural distance among communities")
bar_predominance 

legend_tile <- ggplot(data = df_4, aes(y=reorder(group,subtr)))+
  geom_tile(size = 10, data=df_new, aes(y=reorder(group,subtr), x=Pair1, fill=Com1))+
  geom_tile(size = 10, data=df_new, aes(y=reorder(group,subtr), x=Pair2, fill=Com2))+
  scale_fill_manual(values = pal) + coord_equal()  + 
  theme (axis.title.x=element_blank(),
         axis.text.x=element_blank(),
         axis.ticks.x=element_blank(),
         panel.background = element_blank(),
         legend.position = c(-4, 0.2))+
  labs(y="Community pairs", x = "")

legend_tile

legend_tile  + bar_predominance
ggsave ("Figure5.4.svg",height=10,width=8)
ggsave ("Figure5.4.pdf",height=10,width=8)
#