# CAP2 - Understanding traditional knowledge distribution in western Amazonia #
# JGA

# Packages #
# citation("vegan")
# citation("betareg")
# print(citation("betareg"), bibtex=TRUE)
# Processing
library(tidyverse)
library(flextable) 
library(reshape2)
library(readxl)
# Analysis
library(pscl)
library(rmarkdown)
library(data.table)
library(gtools)
library(vegan)
library(recluster)
library(betapart)
library(ethnobotanyR)
library(circlize)
library(magrittr)
library(corrplot)
library(car)
library(betareg)
library(MuMIn)
library(ComGenR)
# Viz
library(patchwork)
library(randomcoloR)
library(RColorBrewer)
library(colorspace)
library(ggthemes)
library(peRReo)
library(ggpubr)
library(showtext)
library(viridis)
library(scales)
library(paletteer)
library(ggrepel)


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

# Re categorization


etno$Newcategory <- etno$Category
etno$Newcategory <- gsub("MEDICINAL AND VETERINARY", "MEDICINAL", etno$Newcategory)
etno$Newcategory<-gsub("CULTURAL USES", "CULTURAL", etno$Newcategory) #
etno$Newcategory<-gsub("CONSTRUCTION USES", "CONSTRUCTION", etno$Newcategory) #
etno$Newcategory<-gsub("FIREWOOD", "FUEL", etno$Newcategory) #
etno$Newcategory<-gsub("UTENSILS & TOOLS", "UTENSILS", etno$Newcategory) #
etno$Newcategory<-gsub("HUMAN FOOD", "FOOD", etno$Newcategory) #
etno$Newcategory<-gsub("ENVIRONMENTAL", "CULTURAL", etno$Newcategory) 
etno$Newcategory<-gsub("TOXIC", "CULTURAL", etno$Newcategory) 
etno <- etno[!etno$Newcategory=="ANIMAL FOOD",]
etno <- etno[!etno$Newcategory=="OTHER",]
etno <- etno[!etno$Newcategory=="MARKETED",]
etno <- etno[!etno$Newcategory=="WILD ANIMAL",]
names(etno)[names(etno) == 'Newcategory'] <- 'Category2'

etno$Use <- gsub("MEDICINAL AND VETERINARY", "MEDICINAL", etno$Use)
etno$Use<-gsub("CULTURAL USES", "CULTURAL", etno$Use) #
etno$Use<-gsub("CONSTRUCTION USES", "CONSTRUCTION", etno$Use) #
etno$Use<-gsub("FIREWOOD", "FUEL", etno$Use) #
etno$Use<-gsub("UTENSILS & TOOLS", "UTENSILS", etno$Use) #
etno$Use<-gsub("HUMAN FOOD", "FOOD", etno$Use) #
etno$Use<-gsub("ENVIRONMENTAL", "CULTURAL", etno$Use) 
etno$Use<-gsub("TOXIC", "CULTURAL", etno$Use) 
etno <- etno[!etno$Use=="ANIMAL FOOD",]
etno <- etno[!etno$Use=="OTHER",]
etno <- etno[!etno$Use=="MARKETED",]
etno <- etno[!etno$Use=="WILD ANIMAL",]








df_etno <- as.data.frame(unique(cbind(etno$informant, etno$Species, etno$Use)))
colnames(df_etno) <- c("Informant", "Species", "Use")
no_etno <- df_etno %>% filter(is.na(Use))
df_etno <- df_etno %>% filter(!is.na(Use))
df_etno$tomerge <- paste(df_etno$Informant, df_etno$Species)
#df_etno <- df_etno %>% drop_na(Plot)

df_etno2 <- as.data.frame(unique(cbind(etno$Plot,etno$informant, etno$Species, etno$Use)))
colnames(df_etno2) <- c("Plot", "Informant", "Species", "Use")
df_etno2$Plot <- as.numeric (df_etno2$Plot)
df_etno2 <- df_etno2 %>% drop_na(Plot)
df_etno2$tomerge <- paste(df_etno2$Informant, df_etno2$Species)
df_etno3 <- merge(df_etno2[c(1,5)], df_etno, by="tomerge", all=T)
df_etno4 <- unique(df_etno3)
etno
df_etno4$Plot <- as.numeric (df_etno4$Plot)
use_dist <- reshape2::dcast(df_etno4, Plot~Use, value.var="Species", fill=0)
use_dist <- column_to_rownames(use_dist, "Plot")



# probando una cosa

# 
# df_etno2 <- as.data.frame(unique(cbind(etno$Comunidad, etno$Species, etno$Use)))
# colnames(df_etno2) <- c("Comunidad", "Species", "Use")
# df_etno3 <- df_etno2 %>% 
#   group_by(Comunidad, Species) %>%
#   filter (grepl ("CULTURAL", Use)) %>%
#   dplyr::summarize(Uses = n_distinct(Use)) 
# 
# df_etno4 <- as.data.frame(unique(cbind(etno$Comunidad, etno$`Nombres comunes`, etno$`Uso textual según cuaderno de campo`, etno$Species, etno$Use)))
# colnames(df_etno4) <- c("Comunidad","Name", "Text", "Species", "Use")
# df_etno5 <- df_etno4 %>% 
#   filter (grepl ("CULTURAL", Use)) %>%
#   filter (Species=="Poulsenia armata") %>%
#   filter (Comunidad=="Tumupasa")




# unique_etno_plot <- as.data.frame(unique(cbind(etno$Plot, etno$Species, etno$Use)))
# colnames(unique_etno_plot) <- c("Plot","Species", "Use")
# unique_etno_plot$Plot <- as.numeric (unique_etno_plot$Plot)
# 
# use_dist <- reshape2::dcast(unique_etno_plot, Plot~Use, value.var="Species", fill=0)
# 




# 1. Knowledge distribution over among communities #
## 1.1. Does community have an effect in the difference in knowledge between plots?
# Test with permanova
use_dist <- reshape2::dcast(df_etno4, Plot~Use, value.var="Species", fill=0)
use_dist <- column_to_rownames(use_dist, "Plot")

adonis <- adonis(use_dist~Comunidad,  method="bray", resu)

#adonis <- adonis(use_dist~ugg5$value+Comunidad,  method="bray", resu)
anova1 <- as.data.frame(adonis$aov.tab)
anova1 <- round(anova1, 3)
anova1<- rownames_to_column(anova1)
colnames(anova1) <- c("Parameter", "Df", "SS", "MS", "F", "R2", "p-value")
anova2 <- flextable(anova1, col_keys = names(anova1))
#print(anova2, preview="docx")
afss <- adonis$aov.tab$SumsOfSqs
variance <- (afss[1]/afss[3]*100)
print(paste("variance =", round(variance,3)))
anova2

use_dist_bray <- vegdist(use_dist, method="bray")
# ANOSIM
etno.ano.use <- with(resu, anosim(use_dist_bray, Comunidad, distance="bray"))
print(paste("R =", round(etno.ano.use$statistic, 3)))


# NMDS
compMDS.use <-metaMDS(use_dist, distance="bray", k=2, trymax= 1000, autotransform=TRUE) ##k is the number of dimensions
print(paste("stress =", round(compMDS.use$stress, 3)))
# distance matrix
use_dist_bray <- vegdist(use_dist, method="bray")
# ANOSIM
etno.ano.use <- with(resu, anosim(use_dist_bray, Comunidad, distance="bray"))
print(paste("R =", round(etno.ano.use$statistic, 3)))
# colours
hoku <- met.brewer("Hokusai1", n=9)[1:9]
#grid.col = c("#A6CEE3", "#1F78B4","#B2DF8A","#33A02C",  "#FB9A99",  "#FF7F00", "#FDBF6F", "#E31A1C",  "#CAB2D6",  "#6A3D9A")
#grid.col = c("#A6CEE3", "#FCD0A1","#5C415D","#1C7293",  "#C16200",  "#FF7F00", "#FDBF6F", "#E31A1C",  "#CAB2D6",  "#6A3D9A")
grid.col = c( SanCarlos = "#FF7F00", Dicaro="#B2DF8A", Guiyero="#33A02C", Infierno = "#CB6856",Macahua = "#6A3D9A",
              NuevaVida = "#FDBF6F", Bolivar = "#1F78B4", Tumupasa = "#CAB2D6", Yamino = "#A6CEE3")
cols <- grid.col[as.factor(resu$Comunidad)]

# Plot NMDS
dev.off()
setwd("/Users/juliag.dealedo/ONE/UAM_Doctorado/Capitulos/cap2/figs/figs_2023")
svg("nmds_3.svg")
#pdf("nmds_3.pdf", height=8,width=8, pointsize=12)

plot(compMDS.use$points, pch=21, cex=1.5, col="white",bg ="white", cex.axis=1, cex.lab=1 ,xlab="NMDS1", ylab="NMDS2")
legend("topleft", cex=0.8, pch=16, bg="black", col=unique(cols), legend=unique(resu$Comunidad), bty="n", inset = c(0, 0))
ordihull(compMDS.use, resu$Comunidad, col=grid.col, draw="polygon", border="white")
points(compMDS.use$points, pch=21, cex=1.3, col="black", bg = alpha(cols,0.9))
dev.off()




## 1.2. How many species are destined to each category per community?

# Chordiagram 
df_etno <- etno %>% drop_na(Plot)
df_etno$Plot <- as.numeric (df_etno$Plot)

especie2 <- unique(as.data.frame (cbind(df_etno$Plot,df_etno$Comunidad, df_etno$Species, df_etno$Category2, df_etno$Subcategory)))
colnames(especie2) <- c("Plot", "Comunidad", "Species", "Category", "Subcategory")

especie2$Plot <- as.numeric (especie2$Plot)



# df_sp <- unique(as.data.frame (cbind(etno$Comunidad, etno$Species, etno$Use, etno$Category)))
# colnames(df_sp) <- c("Comunidad", "Species", "Use", "Category")
# df_sp1 <- df_sp %>% drop_na(Category)
# 
# df_use <- unique(as.data.frame (cbind(etno$Comunidad, etno$Use, etno$Category)))
# colnames(df_use) <- c("Comunidad", "Use", "Category")
# df_use1 <- df_use %>% drop_na(Category)

com_cord <- reshape2::dcast(especie2, Comunidad + Plot~Category, value.var="Species", fill=0)

boxplot(FUEL~Comunidad, com_cord)
lm.chalco <- lm(CONSTRUCTION~Comunidad, com_cord)
anova(lm.chalco)




# Chordiagram 
df_etno <- etno %>% drop_na(Plot)

especie2 <- unique(as.data.frame (cbind(df_etno$Comunidad, df_etno$Species, df_etno$Category2, df_etno$Subcategory)))
colnames(especie2) <- c("Comunidad", "Species", "Category", "Subcategory")


# df_sp <- unique(as.data.frame (cbind(etno$Comunidad, etno$Species, etno$Use, etno$Category)))
# colnames(df_sp) <- c("Comunidad", "Species", "Use", "Category")
# df_sp1 <- df_sp %>% drop_na(Category)
# 
# df_use <- unique(as.data.frame (cbind(etno$Comunidad, etno$Use, etno$Category)))
# colnames(df_use) <- c("Comunidad", "Use", "Category")
# df_use1 <- df_use %>% drop_na(Category)

com_cord <- reshape2::dcast(especie2, Comunidad~Category, value.var="Species", fill=0)
com_cord <- column_to_rownames(com_cord,var="Comunidad")
reshape2::melt(com_cord)
anova(com_cord$CONSTRUCTION)
anova(com)
grid.col = c( SanCarlos = "#FF7F00", Dicaro="#B2DF8A", Guiyero="#33A02C", Infierno = "#CB6856",Macahua = "#6A3D9A",
              NuevaVida = "#FDBF6F", Bolivar = "#1F78B4", Tumupasa = "#CAB2D6", Yamino = "#A6CEE3")
cols <- grid.col[as.factor(resu$Community)]

rownames(com_cord) <- c("Bolivar", "Dicaro", "Guiyero", "Infierno", "Macahua", "NuevaVida", "SanCarlos", "Tumupasa", "Yamino")
row_order <- c("Dicaro", "Guiyero", "Yamino", "Bolivar", "SanCarlos", "NuevaVida", "Infierno", "Macahua", "Tumupasa")

colnames(com_cord) <- c("Animal Food", "Construction", "Cultural", "Environmental", "Fuel", "Human Food", "Medicinal", "Other", "Toxic", "Utensils and Tools")
mat1 <-as.matrix(com_cord)
as.data.frame(colSums(mat1))
#col_order <- rownames(as.data.frame(colSums(mat1)) %>% arrange(desc(colSums(mat1))))
col_order <- c("CONSTRUCTION", "UTENSILS", "CULTURAL", "FOOD", "MEDICINAL", "FUEL")
mat2 <- mat1[, col_order]
#row_order <- rownames(as.data.frame(rowSums(mat2)) %>% arrange(desc(rowSums(mat2))))
mat3 <- mat2[row_order,]
#mat3 <- mat1
mat5<-mat3/rowSums(mat3)*100
mat6 <- as.matrix(mat5)
mat6
rowSums(mat6)


mat7 <- ComGenR::rel(mat6, rel.type = "sum")*100
mat8 <- as.data.frame(mat7)
mat8
colSums(mat7)
mat6
sd(mat6)
# Calculate standard deviation of the relative percentaje of species each community allocate to each category
desvest <- NULL
meann <- NULL
for (i in 1:10 ){
  desvest[i] <- round(sd(mat3[,i]),2)
  meann[i] <- round(mean(mat3[,i]),2)
}

table_op <- as_tibble(cbind(colnames(mat6), meann, desvest))
colnames(table_op) <- c("Category", "Mean", "Standard Deviation")
print(table_op)
qflextable(table_op)
sum(table_op$Mean)
# Plot
setwd("/Users/juliag.dealedo/ONE/UAM_Doctorado/Capitulos/cap2/figs/figs_2023")
svg("chord_5.svg", height=8,width=8, pointsize=18)
#pdf("chord_2.pdf", height=8,width=8, pointsize=12)

par(cex = 0.8, mar = c(2, 2, 2, 2))
circos.par(start.degree = 0)
chordDiagram(mat6, symmetric=F, directional = 1, scale=F, grid.col=grid.col,
             transparency = 0.1,annotationTrack = c("name", "grid"),
             annotationTrackHeight = c(0.001, 0.05),
             link.lwd = 1,    # Line width
             link.lty = 1,    # Line type
             link.border = 1,
             #link.visible = mat3 >= 5, 
             link.decreasing = T, link.zindex = rank(mat6))
circos.clear()
dev.off()






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
# plot_com <- as.data.frame(unique(cbind(etno$Comunidad, etno$Etnia)))
# plot_species <- aggregate(etno$Species, by=list(etno$Plot), function(x) length(na.omit(unique(x))))
# #plot_use <- aggregate(etno$Use, by=list(etno$Plot), function(x) length(na.omit(unique(x))))
# plot_use <- aggregate(df_etno4$Use, by=list(df_etno4$Plot), function(x) length(na.omit(unique(x))))
# str(df_etno4)
# plot_com <- as.data.frame(unique(cbind(etno$Plot, etno$Comunidad)))
# plot_com$V1 <- as.numeric(plot_com$V1)
# colnames(plot_com) <- c("Plot", "Community")
# df <- merge(plot_species, plot_use, by="Group.1")
# colnames(df) <- c("Plot", "Species", "Uses")
# df$Plot <- as.numeric(df$Plot)
# 
# df <- merge (df, plot_com, by="Plot")
# colnames(df) <- c("Plot", "Species", "Uses", "Community")
# head(df)
# str(df)



library(lme4)
library(glmmTMB)

# models

for1 <- "Uses_number ~ Species_number"
for2 <- "Uses_number ~ (1|Community)"
for3 <- "Uses_number ~ Species_number + (1|Community)"
for4 <- "Uses_number ~ Species_number + (Species|Community)"
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
library(aods3)
gof(glmer3) # no oversidpersion


predict(glmer3, df[4:6], se.fit = T, interval = "confidence")
str(p0 <- predict(glmer3))            # fitted values
str(p1 <- predict(glmer3,re.form=NA))  # fitted values, unconditional (level-0)
newdata <- with(df, expand.grid(Uses=unique(Uses), Species=unique(Species)))
str(p2 <- predict(glmer3,newdata))    # new data, all RE
str(p3 <- predict(glmer3,newdata,re.form=NA)) # new data, level-0
str(p4 <- predict(glmer3,newdata,re.form= ~(1|Community))) # explicitly specify RE
stopifnot(identical(p2, p4))


df$fit <- predict.glm(glmer3)   #Add model fits to dataframe
predict.glm(glmer3, df, se.fit = T, interval = "confidence")



par(mfcol=c(2,2))
Res <- residuals(glmer3, type="pearson")
Fit <- fitted(glmer3)
par(mfrow=c(2,2))
plot(Res ~ Fit, xlab="Fitted values", ylab="Residuals", main="Residuals vs. fitted")
abline(h=0)
plot(Res ~ df$Species, xlab="NAP", ylab="Residuals", main = "NAP") > abline(h=0)
hist(Res, main="Histogram of residuals", xlab="Residuals")
qqnorm(Res)
qqline(Res)

hist(df$Species)
hist(df$Uses)


par(mfcol=c(2,2))
Res <- residuals(glm_p4, type="pearson")
Fit <- fitted(glm_p4)
par(mfrow=c(2,2))
plot(Res ~ Fit, xlab="Fitted values", ylab="Residuals", main="Residuals vs. fitted")
abline(h=0)
plot(Res ~ df$Species, xlab="NAP", ylab="Residuals", main = "NAP") > abline(h=0)
hist(Res, main="Histogram of residuals", xlab="Residuals")
qqnorm(Res)
qqline(Res)



for1 <- "Uses ~ 1"
for2 <- "Uses ~ Species"
for3 <- "Uses ~ Community"
for4 <- "Uses ~ Species*Community"
for5 <- "Uses ~ Species+Community" 

glm1 <- glm(Uses~1, data=df, family="poisson")
glm2 <- glm(Uses~Species, data=df, family="poisson")
glm3 <- glm(Uses~Community, data=df, family="poisson")
glm4 <- glm(Uses~Species*Community, data=df, family="poisson")
glm5 <- glm(Uses~Species+Community, data=df, family="poisson")


AICc <- round(AIC(glm1, glm2, glm3, glm4, glm5), 2)
Formulation <- c(for1, for2, for3, for4, for5)
R2 <- round(c(pR2(glm1)['r2ML'], pR2(glm2)['r2ML'], pR2(glm3)['r2ML'], pR2(glm4)['r2ML'],pR2(glm5)['r2ML']),3)

# Check residuals
par(mfcol=c(2,2))
Res <- residuals(glm5, type="pearson")
Fit <- fitted(glm5)
par(mfrow=c(2,2))
plot(Res ~ Fit, xlab="Fitted values", ylab="Residuals", main="Residuals vs. fitted")
abline(h=0)
plot(Res ~ df$Species, xlab="NAP", ylab="Residuals", main = "NAP") > abline(h=0)
hist(Res, main="Histogram of residuals", xlab="Residuals")
qqnorm(Res)
qqline(Res)


# Table 1 - present on the manuscript
table1 <- cbind(Formulation, AICc, R2)
Table1 <- qflextable (table1)
Table1
#print(Table1,  preview="docx")


Table2 <- rownames_to_column (as.data.frame(round(coef(summary(glm5)),4)))
colnames(Table2)  <- c("Term", "Estimate", "SE", "z value", "p-value")
Table3 <- qflextable(Table2)
Table3
#print(Table3,  preview="docx")


## Processing beta 

indices <- df$Plot
variables <- df$Community
dt <- data.table(indices, variables)

get_permutations <- function(df){
  perm <- permutations(nrow(unique(df[,1])), 2, df$indices)
  as.data.table(perm)
}

ds <- dt[, get_permutations(.SD), by = variables]
colnames(ds) <- c("Comunidad", "Plot1", "Plot2")

comp_dist <- reshape2::dcast(comp, Plot~Species, value.var="Species", fill=0)
comp_dist <- column_to_rownames(comp_dist, var="Plot")

etno_matrix_dist <- vegdist(use_dist, method="bray")
comp_matrix_dist <- vegdist((comp_dist>0)*1, method="bray")

disetno <- etno_matrix_dist
discomp <-comp_matrix_dist

# Ethbobotany
dist_etno <- as.matrix(disetno)
dist_etno[upper.tri(dist_etno)] <- NA
dist_etno2 <- reshape2::melt(dist_etno, na.rm = TRUE)
dist_etno3 <- dist_etno2 %>%
  dplyr::filter(!(value == 0)) %>%
  dplyr::rename(Plot1 = Var1, 
                Plot2 = Var2, 
                etno = value)

# Floristic
dist_comp <- as.matrix(discomp)
dist_comp[upper.tri(dist_comp)] <- NA
dist_comp2 <- reshape2::melt(dist_comp, na.rm = TRUE)
dist_comp3 <- dist_comp2 %>%
  dplyr::filter(!(value == 0)) %>%
  dplyr::rename(Plot1 = Var1, 
                Plot2 = Var2, 
                comp = value)

distances <- cbind(dist_etno3, dist_comp3)
distances <- distances[,c(1,2,3,6)]

distances$Plot_comb <- paste(distances$Plot1, distances$Plot2)
ds$Plot_comb <- paste(ds$Plot1, ds$Plot2)
df_beta <- merge(ds, distances, by="Plot_comb")

# Analysis beta

for1 <- "Uses ~ 1"
for2 <- "Uses ~ Community"
for3 <- "Uses ~ Composition"
for4 <- "Uses ~ Composition+Community" 
for5 <- "Uses ~ Composition*Community" 

betareg1 <- betareg(etno~1,  link = "logit", data = df_beta)
betareg2 <- betareg(etno~Comunidad, link = "logit", data = df_beta)
betareg3 <- betareg(etno~comp,  link = "logit", data = df_beta)
betareg4 <- betareg(etno~comp+Comunidad,  link = "logit", data = df_beta)
betareg5 <- betareg(etno~comp*Comunidad,  link = "logit", data = df_beta)

AIC(betareg1, betareg2, betareg3, betareg4, betareg5)

AICc <- round(AIC(betareg1, betareg2, betareg3, betareg4, betareg5), 2)
Formulation <- c(for1, for2, for3, for4, for5)
R2 <-round( c(summary(betareg1)$pseudo.r.squared, summary(betareg2)$pseudo.r.squared,summary(betareg3)$pseudo.r.squared, 
              summary(betareg4)$pseudo.r.squared, summary(betareg5)$pseudo.r.squared),3)
round(R2,1)

table1 <- cbind(Formulation, AICc, R2)
Table1 <- qflextable (table1)
Table1
#print(Table1,  preview="docx")

Table2 <- rownames_to_column (round(as.data.frame(coef(summary(betareg5)))[1:4], 3))
colnames(Table2)  <- c("Term", "Estimate", "SE", "z value", "p-value")
Table3 <- qflextable(Table2)
Table3
#print(Table3,  preview="docx")

# Check residuals
Res <- residuals(betareg5, "deviance")
Fit <- fitted(betareg5)
par(mfrow=c(2,2))
plot(Res ~ Fit, xlab="Fitted values", ylab="Residuals", main="Residuals vs. fitted")
abline(h=0)
plot(Res ~ df_beta$comp, xlab="Rel_freq", ylab="Residuals", main = "Rel_freq")
abline(h=0)
hist(Res, main="Histogram of residuals", xlab="Residuals")
qqnorm(Res)
qqline(Res)


# Plots 

# Plot Alpha

display.brewer.all(n=NULL, type="all", select=NULL, exact.n=TRUE, colorblindFriendly=T)
pal <- brewer.pal(10, "Paired")
pal <- c( "#1F78B4", "#B2DF8A", "#33A02C",  "#CB6856","#6A3D9A", "#FDBF6F", "#FF7F00", "#CAB2D6", "#A6CEE3")

all_alpha <- df %>% 
  ggplot(aes(x = Species, y = Uses, group = Community, color = Community, size=Uses)) +
  geom_point (data=df, size=log(df$Uses)/1.3, alpha=.8)+
  #geom_smooth(method = "glm")+
  theme_classic()+
  labs(x="Species richness", y ="Use diversity")+
  scale_color_manual(values = pal)+
  theme(legend.position = "none") +
  ggtitle('Alpha diversity')

all_alpha

df$fit <- predict(glmer3, se.fit = TRUE)   #Add model fits to dataframe


glm4 <- glm(Uses~Species+Community, data=df)
predictions_mod2 = MuMIn::predict(glmer3, df, se.fit = TRUE, type = 'response')

upper_mod2 = predictions_mod2$fit+1.96*predictions_mod2$se.fit 
lower_mod2 = predictions_mod2$fit-1.96*predictions_mod2$se.fit
#combining into a df
predframe = data.frame(lwr=lower_mod2,upr=upper_mod2, Species = df$Species, Uses = df$Uses, Community=df$Community)
head(predframe)
head(df)
#plot model with 95% confidence intervals using ggplot
by_comm_alpha <- ggplot(df, aes(x = Species, y = Uses, group = Community, color=Community)) +
  geom_ribbon(data = predframe, aes(ymin=lwr, ymax=upr), alpha = 0.09, color="white") +
  geom_point(alpha = .8)+
  geom_line(aes(y = predict(glm4, df)))+
  theme_classic()+labs(x="Species richness", y ="Use diversity")+
  scale_color_manual(values = pal)+
  scale_fill_manual(values = pal)+
  facet_wrap(~Community, nrow = 2)+
  theme(legend.position="none")


#all/alpha_com

# Plot beta

pal <- brewer.pal(10, "Paired")
pal <- c( "#1F78B4", "#B2DF8A", "#33A02C",  "#CB6856","#6A3D9A", "#FDBF6F", "#FF7F00", "#CAB2D6", "#A6CEE3")
total <- ggplot(df_beta, aes(x = comp, y = etno, color=Comunidad, size=etno)) +
  geom_point( alpha=.8) +
  scale_size(range = c(0, 2)) +
  #geom_line(aes(y = predict(betareg5, letsee)))+
  theme_classic()+labs(x="Floristic dissimilarity", y ="Use distance")+
  scale_color_manual(values = pal)+
  scale_fill_manual(values = pal)+
  theme_classic()+
  theme(legend.position = "none") +
  ggtitle('Beta diversity') + scale_size(range = c(0, 2))

bycomm <- df_beta %>% 
  ggplot( aes(x = comp, y = etno, group = Comunidad, color = Comunidad)) +
  geom_ribbon(aes(ymax = predict(betareg5, df_beta, type="quantile", at = c(0.25)), 
                  ymin = predict(betareg5, df_beta, type="quantile", at = c(0.75))), alpha=0.09,color="white")+
  geom_point(alpha = .8) +
  geom_line(aes(y = predict(betareg5, df_beta)))+
  theme_classic()+labs(x="Floristic dissimilarity", y ="Use distance")+
  scale_color_manual(values = pal)+  scale_fill_manual(values = pal)+
  theme(legend.position="none")+ # axis.title=element_text(size=20)
  facet_wrap(~Comunidad, nrow = 2) 

# Sum plots
patch <- all_alpha/by_comm_alpha | total/bycomm 
patch+plot_annotation(tag_levels = 'A')

ggsave("com_com_3.pdf",height = 17, width = 17,  scale=.5)


# Map

# Libraries
library(sf)
library(readxl)
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(dplyr)
library(ggrepel)
library(raster)
library(RColorBrewer)
library(showtext)
library(sysfonts)

# Fonts
font_add_google("Maven Pro", "rub")
showtext_auto()

# Read data
setwd("/Users/juliag.dealedo/ONE/UAM_Doctorado/Capitulos/cap2/data") 
resu <- read.table("resu_cat")
comp <- read.table("composition")
coordenadas <- cbind (aggregate(resu$Latitud, list(resu$Comunidad), min), aggregate(resu$Longitud, list(resu$Comunidad), min)[,2])
colnames(coordenadas) <- c("Region", "Latitud", "Longitud")
# Add countries
world <- ne_countries(scale = "medium", returnclass = "sf")
nato_names <- c('Peru', 'Ecuador', 'Bolivia')
nato_countries <- ne_countries(  
  scale = "medium", returnclass = "sf") %>%  
  filter(name %in% nato_names) 
# Colours
grid.col <- c(Aguapolo = "#A6CEE3", Bolivar = "#1F78B4", Dicaro="#B2DF8A", Guiyero="#33A02C", Infierno = "#FB9A99", Macahua = "#FF7F00",
              NuevaVida = "#FDBF6F", SanCarlos = "#E31A1C", Tumupasa = "#CAB2D6", Yamino = "#6A3D9A")
grid.col <- as.data.frame(grid.col)$grid.col
# Plot
plot_com <- ggplot(data = world) +   
  geom_sf(fill = "#303B49", color = "#babfc3")+
  geom_sf(data=nato_countries, fill = "#49586A", color = "#babfc3")+
  coord_sf(xlim = c(-90, -50), ylim = c(-25, 10), expand = FALSE)+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  geom_point(data = coordenadas, aes(x = Longitud, y = Latitud, fill=Region), size = 2.5, shape = 21, alpha=0.9) +
  scale_fill_manual(values = grid.col)+
  geom_label_repel(data = coordenadas, aes(1*Longitud, Latitud, label = Region),col="black", size = 3, box.padding = 0.5,point.padding = unit(0.5, "lines"),nudge_x = c(-1, -1, -1, -1, -1,1,-1), nudge_y = c(0.5), family="rub")+
  theme_void()+
  theme(panel.grid.major = element_line(colour = "#babfc3"), panel.background = element_rect(fill = NULL), legend.position="none")
print(plot_com)
ggsave("map_black.svg",scale=0.8)
# Plot entire south America
# ggplot(data = world) + 
#   geom_sf()+
#   coord_sf(xlim = c(-135, 15), ylim = c(-100, 80), expand = FALSE)+theme_void()
# #ggsave("map_world.svg",scale=0.3)





#setwd("/Users/juliag.dealedo/ONE/UAM_Doctorado/Capitulos/cap2/scripts/cap2")
# #dir()
# rmarkdown::render('cap2.5.R', 'pdf_document')
