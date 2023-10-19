# R Script - Chapter III JGA Thesis
# Date: 15/4/2023
# Date revision: 27/07/2023

# Load necessary libraries

library(readxl)
library(dplyr)
library(flextable)
library(tibble)
library(reshape2)
library(ggpubr)
library(MetBrewer)
library(visreg)
library(lme4)
library(MuMIn) 
library(tidyr)
library(purrr)
library(Hmisc)

# Data processing 

# Read database
setwd("/Users/juliag.dealedo/ONE/UAM_Doctorado/Capitulos/cap3/data/dataraw")
etno_original <- read_excel("etno-dis-mad-yas3.xlsx", sheet=1, col_names =T)
etno <- etno_original %>% filter(!Comunidad=="Aguapolo")

# Create df use
df_etno <- as.data.frame(unique(cbind(etno$Species, etno$`Plant part`, etno$Category, etno$Subcategory)))
colnames(df_etno) <- c("Species", "Part", "Category", "Subcategory")
df_etno <- df_etno %>% replace_na(list(Part = 'NO USE', Category = 'NO USE', Subcategory='NO USE'))

# Re-categorization and renaming
df_etno$Newcategory <- df_etno$Subcategory
# MEDICINAL
df_etno$Newcategory <- gsub(".*[a-z].*", "MEDICINAL", df_etno$Newcategory)
# CULTURAL
df_etno$Newcategory<-gsub("PERSONAL ADORNMENT", "CLOTHES AND ACCESORIES", df_etno$Newcategory)
df_etno$Newcategory<-gsub("OTHER CULTURAL", "RITUAL", df_etno$Newcategory)
# CONSTRUCTION
df_etno$Newcategory<-gsub("BRIDGES", "TIMBER", df_etno$Newcategory)
df_etno$Newcategory<-gsub("HOUSES", "TIMBER", df_etno$Newcategory)
df_etno$Newcategory<-gsub("OTHER CONSTRUCTION", "TIMBER", df_etno$Newcategory)
df_etno$Newcategory<-gsub("TRANSPORTATION", "TIMBER", df_etno$Newcategory)
df_etno$Newcategory<-gsub("FENCES", "TIMBER", df_etno$Newcategory)
# ENVIRONMENTAL remains the SAME
# FUEL
df_etno$Newcategory<-gsub("OTHER FUEL", "FIREWOOD", df_etno$Newcategory)
df_etno$Newcategory<-gsub("FIRE STARTER", "LIGHTING", df_etno$Newcategory)
# HUMAN FOOD
df_etno$Newcategory<-gsub("FOOD ADDITIVES", "FOOD", df_etno$Newcategory)
df_etno$Newcategory<-gsub("OILS", "FOOD", df_etno$Newcategory)
# TOXIC
df_etno$Newcategory<-gsub("POISON HUNTING", "TOXIC", df_etno$Newcategory)
df_etno$Newcategory<-gsub("POISON FISHING", "TOXIC", df_etno$Newcategory)
# UTENSILS
df_etno$Newcategory<-gsub("CAUCHO", "LABOUR TOOLS", df_etno$Newcategory)
df_etno$Newcategory<-gsub("OTHER UTENSILS", "LABOUR TOOLS", df_etno$Newcategory)
# CLOTHES AND ACCESORIES
df_etno$Newcategory<-gsub("CLOTHES & ACCESORIES", "CLOTHES AND ACCESORIES", df_etno$Newcategory)
# UTENSILS
df_etno$Newcategory<-gsub("OTHER UTENSILS", "UTENSILS", df_etno$Newcategory)
df_etno$Newcategory<-gsub("LABOUR TOOLS", "UTENSILS", df_etno$Newcategory)
df_etno$Newcategory<-gsub("HUNTING & FISHING TOOLS", "UTENSILS", df_etno$Newcategory)
df_etno$Newcategory<-gsub("DOMESTIC UTENSILS", "UTENSILS", df_etno$Newcategory)
# Check names
num_spp <- aggregate(df_etno$Species, by=list(df_etno$Newcategory), function(x) length(unique(na.omit(x))))
unique(df_etno$Newcategory)
names(df_etno)[names(df_etno) == 'Newcategory'] <- 'Subcategory'
df_etno <- df_etno[,c(1,2,3,5)]
df_etno <- unique(df_etno)
# write.table(df_etno, "17-2-23-USES")

# Add fruit traits
setwd("/Users/juliag.dealedo/ONE/UAM_Doctorado/Capitulos/cap3/data/dataraw")
traits_original <- read.table("29-6-22-TRAITS")
atdn <- read.table("fruit_leaf_latex")
fruits <- atdn[,c(2,7,9)]
head(fruits)
traits <- traits_original [,-c(25,27)] 
head(traits_original [,c(25,27)]) 
colnames(traits)
traits_good <- merge(traits, fruits, by="Species")
colnames(traits_good)
traits_good[traits_good == "-99"] <- NA

# No species from Aguapolo
aguapolo <- etno_original %>% filter(Comunidad=="Aguapolo")
str(aguapolo)
unique(aguapolo$Cod_plot)
aguapolo_spp <- setdiff(aguapolo$Species, etno$Species)
traits_good <- traits_good %>% filter(!Species %in% aguapolo_spp)

# Create Table 9.2 about number species with each trait
traits_good <- traits_good %>% distinct()
traitsc <- traits_good
num_trait <- as.data.frame(sapply(traits_good, function(x) sum(complete.cases(x))))
num_trait <- rownames_to_column(num_trait)
Table9.2 <- flextable(num_trait)

# Correlate traits Table 9.3
to_cor <- traits_good %>% select(GF_mean, LA_log, SLA_mean, DM_mean.y, DBH_max, 
                                 f_mass, f_fleshy, Liana, Palmera, Tree, Hemiepiphyte, NEW_latex, NEW_resin)
end_cor <- cor(to_cor, use="na.or.complete", method="pearson") 
end_cor_table <- qflextable(rownames_to_column((round(as.data.frame(end_cor), 2))))

# Add significance values
cor_matrix_r <- round(cor(to_cor, use="na.or.complete", method="pearson"),2)
pvalue_matrix <- round(rcorr(as.matrix(to_cor))$P, 5)
colnames(cor_matrix_r) <- c("LT", "LA", "SLA", "WD", "DBH", "SM", "Fleshy", "Liana", "Palm", "Tree", "Hemiep.", "Latex", "Resin")
rownames(cor_matrix_r) <- c("LT", "LA", "SLA", "WD", "DBH", "SM", "Fleshy", "Liana", "Palm", "Tree", "Hemiep.", "Latex", "Resin")
colnames(pvalue_matrix) <- c("LT", "LA", "SLA", "WD", "DBH", "SM", "Fleshy", "Liana", "Palm", "Tree", "Hemiep.", "Latex", "Resin")
rownames(pvalue_matrix) <- c("LT", "LA", "SLA", "WD", "DBH", "SM", "Fleshy", "Liana", "Palm", "Tree", "Hemiep.", "Latex", "Resin")
table_matrix <- matrix("", nrow = nrow(cor_matrix_r), ncol = ncol(cor_matrix_r))

# Loop through the matrices and add "*" where p-value is under the threshold
for (i in 1:nrow(cor_matrix_r)) {
  for (j in 1:ncol(cor_matrix_r)) {
    if (is.na(pvalue_matrix[i, j])) {
      pvalue_matrix[i, j] <- "1"
    } 
    else if (pvalue_matrix[i, j] <= 0.01) {
      table_matrix[i, j] <- paste(cor_matrix_r[i, j], "**")
    } else if (pvalue_matrix[i, j] <= 0.05) {
      table_matrix[i, j] <- paste(cor_matrix_r[i, j], "*")
    } else {
      table_matrix[i, j] <- as.character(cor_matrix_r[i, j])
    }
  }
}

table_df <- as.data.frame(table_matrix)
rownames(table_df) <- rownames(cor_matrix_r)
colnames(table_df) <- colnames(cor_matrix_r)
qflextable(rownames_to_column(table_df))
# print(qflextable(rownames_to_column(table_df)), preview="docx")

# PCA
variables_PCA <- dplyr::select (to_cor, c(1:6))
names(variables_PCA)
colnames(variables_PCA) <- c("LT", "LA", "SLA", "WD", "DBH", "SM")
df <- na.omit(variables_PCA)
df_pca <- prcomp(df, scale.=TRUE)
df_out <- as.data.frame(df_pca$x)
df_out$group <- sapply (strsplit(as.character(row.names(df)), "_"), "[[", 1)
head(df_out)
p<-ggplot(df_out,aes(x=PC1,y=PC2))
p<-p+geom_point()
pca_res <- prcomp(df, scale. = TRUE)
factoextra::fviz_pca_var(pca_res, col.var = "black") + theme_classic()
#ggsave("PCA.png")
print(pca_res)
factoextra::get_eigenvalue(pca_res)
factoextra::fviz_eig(pca_res, addlabels = TRUE)


# Join Ethnobotany and Functional traits databases 
uses <- df_etno
traits_com <- merge(traits_good, uses, by="Species")

# Eliminate subcategories with less than 20 species
num_spp[num_spp$x<21,]
traits_com <- traits_com[!traits_com$Subcategory=="WRAPPERS",]
traits_com <- traits_com[!traits_com$Subcategory=="ORNAMENTAL",]
traits_com <- traits_com[!traits_com$Subcategory=="WILDLIFE ATTRACTANT",]
traits_com <- traits_com[!traits_com$Subcategory=="AGROFORESTRY",]
traits_com <- traits_com[!traits_com$Subcategory=="WILD ANIMAL FOOD",]
traits_com <- traits_com[!traits_com$Subcategory=="WILD ANIMAL BEHAVIOR",]
traits_com <- traits_com[!traits_com$Subcategory=="FISH BAIT",]
traits_com <- traits_com[!traits_com$Subcategory=="MARKETED",]
traits_com <- traits_com[!traits_com$Subcategory=="LIGHTING",]
# write.table(traits_com, "17-2-23-TRAITS_CAT")

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
# 6.3.1. Trait-service relationships

# > Data processing to analysis 
traits_cat <- traits_com
traits_cat$LA_log <- log(traits_cat$LA_mean)
traits_cat <- traits_cat %>% dplyr::select(-Nodules)

# Classify under plant part
# 1. Stem
stem_part <- c(traits_cat$Part=="BARK_STEM" | traits_cat$Part=="STEM" | traits_cat$Part=="BARK_ROOT" | 
                 traits_cat$Part=="BARK" | traits_cat$Part=="SPINES" | traits_cat$Part=="STEM_ENTIRE LEAF" | 
                 traits_cat$Part=="SPINES"| traits_cat$Part=="ROOT_STEM")

# No stem: species which the stem is no used under "no stem"
traits_no_stem <- traits_cat[!stem_part,]
traits_no_stem$Subcategory <- "NO STEM"
traits_no_stem$Part <- "NO STEM"
traits_no_stem <- traits_no_stem[!is.na(traits_no_stem$Species),]

# Join stem + no stem
traits_stem <- traits_cat[stem_part,]
traits_stem <- traits_stem[!is.na(traits_stem$Species),]
traits_stem_ok <- rbind(traits_no_stem, traits_stem)
traits_stem_ok <- traits_stem_ok[!duplicated(traits_stem_ok),]

# Stem species and traits
to_glm1 <- reshape2::dcast(traits_stem_ok, Species~Subcategory, value.var="Subcategory", fill=0)
to_glm1 <- column_to_rownames(to_glm1,var="Species")
to_glm2 <- unique(as.data.frame(1*(to_glm1>0))) # used 1, not use 0
to_glm2 <- rownames_to_column(to_glm2, var="Species")
GLM_STEM <- merge(to_glm2, traits, by="Species", all=T)
GLM_STEM <- column_to_rownames(GLM_STEM, var="Species")

# 2. Leaves
leaf_part <- c(traits_cat$Part=="ENTIRE LEAF" | traits_cat$Part=="PETIOLE"| traits_cat$Part=="BARK_ENTIRE LEAF" | 
                 traits_cat$Part=="BRACT"| traits_cat$Part=="LEAF SHEATH" | traits_cat$Part=="PALM HEART"| 
                 traits_cat$Part=="LEAF RACHIS" | traits_cat$Part=="ENTIRE LEAF_FLOWER_FRUIT"| 
                 traits_cat$Part=="ENTIRE LEAF_FRUIT")
traits_no_leaf <- traits_cat[!leaf_part,]
traits_no_leaf$Subcategory <- "NO LEAF"
traits_no_leaf$Part <- "NO LEAF"
traits_leaf <- traits_cat[ leaf_part,]
traits_leaf_ok <- rbind(traits_no_leaf, traits_leaf)
traits_leaf_ok <- traits_leaf_ok[!duplicated(traits_leaf_ok),]
traits_leaf_ok$LA_log <- log(traits_leaf_ok$LA_mean)
traits_leaf_ok <- traits_leaf_ok[!is.na(traits_leaf_ok$Species),]
to_glm1 <- traits_leaf_ok[!duplicated(traits_leaf_ok),]
to_glm2 <-  reshape2::dcast(to_glm1, Species~Subcategory, value.var="Subcategory", fill=0)
to_glm2 <- column_to_rownames(to_glm2,var="Species")
to_glm2 <- as.data.frame(1*(to_glm2>0))
to_glm2 <- rownames_to_column(to_glm2, "Species")
GLM_LEAF <- merge(to_glm2, traits_good, by="Species")

# 3. Fruit
fruit_part <- c(traits_cat$Part=="FRUIT" | traits_cat$Part=="FLOWER"| traits_cat$Part=="SEED" | 
                  traits_cat$Part=="FRUIT_SEED" | traits_cat$Part=="INFRUTESCENCE"|
                  traits_cat$Part=="INFLORESCENCE")
traits_no_fruit <- traits_cat[!fruit_part,]
traits_no_fruit$Subcategory <- "NO FRUIT"
traits_no_fruit$Part <- "NO FRUIT"
traits_fruit <- traits_cat[fruit_part,]
traits_fruit_ok <- rbind(traits_no_fruit, traits_fruit)
traits_fruit_ok <- traits_fruit_ok[!duplicated(traits_fruit_ok),]
traits_fruit_ok <- traits_fruit_ok[!is.na(traits_fruit_ok$Species),]
to_glm1 <- traits_fruit_ok[!duplicated(traits_fruit_ok),]
to_glm2 <- reshape2::dcast(to_glm1, Species~Subcategory, value.var="Subcategory", fill=0)
to_glm2 <- column_to_rownames(to_glm2,var="Species")
to_glm3 <- as.data.frame(1*(to_glm2>0))
to_glm3 <- rownames_to_column(to_glm3, "Species")
GLM_FRUIT <- merge(to_glm3, traits_good, by="Species")

# 4. All
traits_cat <- traits_cat[!is.na(traits_cat$Species),]
to_glm1 <- traits_cat[!duplicated(traits_cat),]
to_glm2 <- reshape2::dcast(to_glm1, Species~Subcategory, value.var="Subcategory", fill=0)
to_glm2 <- column_to_rownames(to_glm2,var="Species")
to_glm3 <- as.data.frame(1*(to_glm2>0))
to_glm3 <- rownames_to_column(to_glm3, "Species")
GLM_ALL <- merge(to_glm3, traits_good, by="Species")

# Species to rownames
GLM_STEM <- column_to_rownames(GLM_STEM,var="Species")
GLM_LEAF <- column_to_rownames(GLM_LEAF,var="Species")
GLM_FRUIT <- column_to_rownames(GLM_FRUIT,var="Species")
GLM_ALL <- column_to_rownames(GLM_ALL,var="Species")


# > Data analysis 

# 1. Stem
STEM_SUB <- GLM_STEM[,c(1:14)]
deviance <- rep(NULL)
pvalue <- rep(NULL)
coeff <- rep(NULL)
aic <- rep(NULL)

par(mfrow=c(3,6))
for (i in 1:14) { # GLM loop to obtain R2 of the relation WD-services
  glm_result_1 <- glm(STEM_SUB[,i]~DM_mean.y, data=GLM_STEM, family="binomial")
  ANOVAS <- anova(glm_result_1, test="Chi")
  deviance[i] <- 100*(1-(ANOVAS$`Resid. Dev`[2]/ANOVAS$`Resid. Dev`[1]))
  pvalue[i] <- ANOVAS$`Pr(>Chi)`[2]
  coeff[i] <- glm_result_1$coefficients[2]
  aic[i] <- glm_result_1$aic
  visreg::visreg(glm_result_1, "DM_mean.y", scale="response", ylab=colnames(STEM_SUB)[i], mar=c(0,0,0,0))
}
DEV_STEM <- as.data.frame(cbind(names(STEM_SUB), round (deviance,3), round (pvalue,3), round (coeff,3)))
colnames(DEV_STEM) <- c("Subcategory", "WD", "pWD", "coeff")
DEV_STEM <- column_to_rownames(DEV_STEM, var="Subcategory")
DEV_STEM$WD <- as.numeric(DEV_STEM$WD)
DEV_STEM$pWD <- as.numeric(DEV_STEM$pWD)
DEV_STEM$coeff <- as.numeric(DEV_STEM$coeff)

# 2. Leaves
GLM_LEAF$LA_log<-log(GLM_LEAF$LA_mean)
colnames(GLM_LEAF)
LEAF_SUB <- GLM_LEAF[,c(1:15)]
LEAF_SUB <- GLM_LEAF[,c(1:15)][,colSums(GLM_LEAF[,c(1:15)])>5]
deviance_lt <- rep(NULL)
deviance_la <- rep(NULL)
deviance_sla <- rep(NULL)
coeff_lt<- rep(NULL)
coeff_la<- rep(NULL)
coeff_sla<- rep(NULL)
pvalue_lt <- rep(NULL)
pvalue_la <- rep(NULL)
pvalue_sla <- rep(NULL)
aic <- rep(NULL)

for (i in 1:length(LEAF_SUB)) {
  glm_result_1 <- glm(LEAF_SUB[,i]~GF_mean+SLA_mean+LA_log, data=GLM_LEAF, family="binomial")
  ANOVAS <- anova(glm_result_1, test="Chi")
  deviance_lt[i] <- 100*(1-(ANOVAS$`Resid. Dev`[2]/ANOVAS$`Resid. Dev`[1]))
  deviance_la[i] <- 100*(1-(ANOVAS$`Resid. Dev`[4]/ANOVAS$`Resid. Dev`[1]))
  deviance_sla[i] <- 100*(1-(ANOVAS$`Resid. Dev`[3]/ANOVAS$`Resid. Dev`[1]))
  pvalue_lt[i] <- ANOVAS$`Pr(>Chi)`[2]
  pvalue_la[i] <- ANOVAS$`Pr(>Chi)`[4]
  pvalue_sla[i] <- ANOVAS$`Pr(>Chi)`[3]
  coeff_lt[i] <- glm_result_1$coefficients[2]
  coeff_la[i] <- glm_result_1$coefficients[3]
  coeff_sla[i] <- glm_result_1$coefficients[4]
  aic[i] <- glm_result_1$aic
  visreg::visreg(glm_result_1, "GF_mean", scale="response", ylab=colnames(LEAF_SUB)[i], mar=c(0,0,0,0))
}

DEV_LEAF <- as.data.frame(cbind(names(LEAF_SUB),
                                round (deviance_lt,3),
                                round (deviance_la,3),
                                round (deviance_sla,3),
                                round (deviance_leaf,3),
                                round (pvalue_lt,3),
                                round (pvalue_la,3),
                                round (pvalue_sla,3),
                                round (pvalue_leaf,3),
                                round (coeff_lt,3),
                                round (coeff_la,3),
                                round (coeff_sla,3),
                                round (coeff_leaf,3)))

colnames(DEV_LEAF) <- c("Subcategory", "LT", "LA", "SLA", "LEAF", "pLT", "pLA", "pSLA", "pLEAF", "cLT", "cLA", "cSLA", "cLEAF")
DEV_LEAF <- column_to_rownames(DEV_LEAF, var="Subcategory")
DEV_LEAF$LT <- as.numeric(DEV_LEAF$LT)
DEV_LEAF$LA <- as.numeric(DEV_LEAF$LA)
DEV_LEAF$SLA <- as.numeric(DEV_LEAF$SLA)
DEV_LEAF$LEAF <- as.numeric(DEV_LEAF$LEAF)
DEV_LEAF$pLT <- as.numeric(DEV_LEAF$pLT)
DEV_LEAF$pLA <- as.numeric(DEV_LEAF$pLA)
DEV_LEAF$pSLA <- as.numeric(DEV_LEAF$pSLA)
DEV_LEAF$pLEAF <- as.numeric(DEV_LEAF$pLEAF)
DEV_LEAF$cLT <- as.numeric(DEV_LEAF$cLT)
DEV_LEAF$cLA <- as.numeric(DEV_LEAF$cLA)
DEV_LEAF$cSLA <- as.numeric(DEV_LEAF$cSLA)
DEV_LEAF$cLEAF <- as.numeric(DEV_LEAF$cLEAF)
DEV_LEAF

# 3. Fruit
FRUIT_SUB <- GLM_FRUIT[,1:11][,colSums(GLM_FRUIT[,1:11])>5]
GLM_FRUIT$f_fleshy <- as.factor(GLM_FRUIT$f_fleshy)
deviance_seedmass <- rep(NULL)
deviance_fleshy <- rep(NULL)
pvalue_seedmass <- rep(NULL)
pvalue_fleshy <- rep(NULL)
aic <- rep(NULL)
coeff_seedmass<- rep(NULL)
coeff_fleshy<- rep(NULL)

par(mfrow=c(4,3))
for (i in 1:10) {
  glm_result_1 <- glm(FRUIT_SUB[,i]~f_mass+f_fleshy, data=GLM_FRUIT, family="binomial")
  ANOVAS <- anova(glm_result_1, test="Chi")
  deviance_seedmass[i] <- 100*(1-(ANOVAS$`Resid. Dev`[2]/ANOVAS$`Resid. Dev`[1]))
  deviance_fleshy[i] <- 100*(1-(ANOVAS$`Resid. Dev`[3]/ANOVAS$`Resid. Dev`[1]))
  pvalue_seedmass[i] <- ANOVAS$`Pr(>Chi)`[2]
  pvalue_fleshy[i] <- ANOVAS$`Pr(>Chi)`[3]
  coeff_seedmass[i]<-glm_result_1$coefficients[2]
  coeff_fleshy[i]<-glm_result_1$coefficients[3]
  aic[i] <- glm_result_1$aic
}
DEV_FRUIT <- as.data.frame(cbind(names(FRUIT_SUB), 
                                 round (deviance_seedmass,3),
                                 round (deviance_fleshy,3),
                                 round (pvalue_seedmass,3),
                                 round (pvalue_fleshy,3), 
                                 round (coeff_seedmass,3),
                                 round (coeff_fleshy,3)))

colnames(DEV_FRUIT) <- c("Subcategory", "SEEDMASS", "FLESHY", "pSEEDMASS", "pFLESHY", "cSEEDMASS", "cFLESHY")
DEV_FRUIT <- column_to_rownames(DEV_FRUIT, var="Subcategory")
DEV_FRUIT$SEEDMASS <- as.numeric(DEV_FRUIT$SEEDMASS)
DEV_FRUIT$FLESHY <- as.numeric(DEV_FRUIT$FLESHY)
DEV_FRUIT$pSEEDMASS <- as.numeric(DEV_FRUIT$pSEEDMASS)
DEV_FRUIT$pFLESHY <- as.numeric(DEV_FRUIT$pFLESHY)
DEV_FRUIT$cSEEDMASS <- as.numeric(DEV_FRUIT$cSEEDMASS)
DEV_FRUIT$cFLESHY <- as.numeric(DEV_FRUIT$cFLESHY)

# 4. All
ALL_SUB <- GLM_ALL[,1:14][,colSums(GLM_ALL[,1:14])>5]

# DBH max
# Loop
deviance_dbh <- rep(NULL)
pvalue_dbh <- rep(NULL)
coeff_dbh <- rep(NULL)
aic <- rep(NULL)
par(mfrow=c(3,6))
for (i in 1:length(ALL_SUB)) {
  glm_result_1 <- glm(ALL_SUB[,i]~DBH_max, data=GLM_ALL, family="binomial")
  ANOVAS <- anova(glm_result_1, test="Chi")
  deviance_dbh[i] <- 100*(1-(ANOVAS$`Resid. Dev`[2]/ANOVAS$`Resid. Dev`[1]))
  pvalue_dbh[i] <- ANOVAS$`Pr(>Chi)`[2]
  coeff_dbh[i] <- glm_result_1$coefficients[2]
  aic[i] <- glm_result_1$aic
  visreg::visreg(glm_result_1, "DBH_max", scale="response", ylab=colnames(STEM_SUB)[i], mar=c(0,0,0,0))
}

DEV_DBH <- as.data.frame(cbind(names(ALL_SUB), round (deviance_dbh,3), round (pvalue_dbh,3), round (coeff_dbh,3)))
colnames(DEV_DBH) <- c("Subcategory", "DBH", "pDBH", "cDBH")
DEV_DBH <- column_to_rownames(DEV_DBH, var="Subcategory")
DEV_DBH$DBH <- as.numeric(DEV_DBH$DBH)
DEV_DBH$pDBH <- as.numeric(DEV_DBH$pDBH)
DEV_DBH$cDBH <- as.numeric(DEV_DBH$cDBH)

# Latex + resin
GLM_ALL$NEW_latex <- as.factor(GLM_ALL$NEW_latex)
GLM_ALL$NEW_resin <- as.factor(GLM_ALL$NEW_resin)

deviance_latex<-rep(NULL)
deviance_resin<-rep(NULL)
pvalue_latex<-rep(NULL)
pvalue_resin<-rep(NULL)
coeff_latex<-rep(NULL)
coeff_resin<-rep(NULL)

par(mfrow=c(3,6))
mar=c(0,0,0,0)
for (i in 1:length(ALL_SUB)) {
  glm_result_1 <- glm(ALL_SUB[,i]~NEW_latex+NEW_resin, data=GLM_ALL, family="binomial")
  ANOVAS <- anova(glm_result_1, test="Chi")
  deviance_latex[i] <- 100*(1-(ANOVAS$`Resid. Dev`[2]/ANOVAS$`Resid. Dev`[1]))
  deviance_resin[i] <- 100*(1-(ANOVAS$`Resid. Dev`[3]/ANOVAS$`Resid. Dev`[1]))
  pvalue_latex[i] <- ANOVAS$`Pr(>Chi)`[2]
  pvalue_resin[i] <- ANOVAS$`Pr(>Chi)`[3]
  coeff_latex[i]<-glm_result_1$coefficients[2]
  coeff_resin[i]<-glm_result_1$coefficients[3]
}

DEV_ALL <- as.data.frame(cbind(names(ALL_SUB), 
                               round (deviance_latex,3),
                               round (deviance_resin,3),
                               round (pvalue_latex,3),
                               round (pvalue_resin,3),
                               round (coeff_latex,3),
                               round (coeff_resin,3)))

colnames(DEV_ALL) <- c("Subcategory", "LATEX", "RESIN", "pLATEX", "pRESIN",  "cLATEX", "cRESIN")
DEV_ALL <- column_to_rownames(DEV_ALL, var="Subcategory")
DEV_ALL$LATEX<- as.numeric(DEV_ALL$LATEX)
DEV_ALL$RESIN <- as.numeric(DEV_ALL$RESIN)
DEV_ALL$pLATEX <- as.numeric(DEV_ALL$pLATEX)
DEV_ALL$pRESIN <- as.numeric(DEV_ALL$pRESIN)
DEV_ALL$cLATEX <- as.numeric(DEV_ALL$cLATEX)
DEV_ALL$cRESIN <- as.numeric(DEV_ALL$cRESIN)

# Growth form
GLM_ALL$Tree<- as.factor(GLM_ALL$Tree)
GLM_ALL$Palmera<- as.factor(GLM_ALL$Palmera)
GLM_ALL$Liana<- as.factor(GLM_ALL$Liana)
GLM_ALL$Fern<- as.factor(GLM_ALL$Fern)
GLM_ALL$Hemiepiphyte<- as.factor(GLM_ALL$Hemiepiphyte)

deviance_tree <-rep(NULL)
deviance_palmera <-rep(NULL)
deviance_liana<-rep(NULL)
deviance_fern<-rep(NULL)
deviance_hemiepiphyte<-rep(NULL)

pvalue_tree <-rep(NULL)
pvalue_palmera <-rep(NULL)
pvalue_liana<-rep(NULL)
pvalue_fern<-rep(NULL)
pvalue_hemiepiphyte<-rep(NULL)

coeff_tree <-rep(NULL)
coeff_palmera <-rep(NULL)
coeff_liana<-rep(NULL)
coeff_fern<-rep(NULL)
coeff_hemiepiphyte<-rep(NULL)

# Independent variables

for (i in 1:length(ALL_SUB)) {
  glm_result_1 <- glm(ALL_SUB[,i]~Tree, data=GLM_ALL, family="binomial")
  ANOVAS <- anova(glm_result_1, test="Chi")
  deviance_tree[i] <- 100*(1-(ANOVAS$`Resid. Dev`[2]/ANOVAS$`Resid. Dev`[1]))
  pvalue_tree[i] <- ANOVAS$`Pr(>Chi)`[2]
  coeff_tree[i]<-glm_result_1$coefficients[2]
  
}

for (i in 1:length(ALL_SUB)) {
  glm_result_1 <- glm(ALL_SUB[,i]~Palmera, data=GLM_ALL, family="binomial")
  ANOVAS <- anova(glm_result_1, test="Chi")
  deviance_palmera[i] <- 100*(1-(ANOVAS$`Resid. Dev`[2]/ANOVAS$`Resid. Dev`[1]))
  pvalue_palmera[i] <- ANOVAS$`Pr(>Chi)`[2]
  coeff_palmera[i]<-glm_result_1$coefficients[2]
  
}

for (i in 1:length(ALL_SUB)) {
  
  glm_result_1 <- glm(ALL_SUB[,i]~Liana, data=GLM_ALL, family="binomial")
  ANOVAS <- anova(glm_result_1, test="Chi")
  deviance_liana[i] <- 100*(1-(ANOVAS$`Resid. Dev`[2]/ANOVAS$`Resid. Dev`[1]))
  pvalue_liana[i] <- ANOVAS$`Pr(>Chi)`[2]
  coeff_liana[i]<-glm_result_1$coefficients[2]
}

for (i in 1:length(ALL_SUB)) {
  glm_result_1 <- glm(ALL_SUB[,i]~Hemiepiphyte, data=GLM_ALL, family="binomial")
  ANOVAS <- anova(glm_result_1, test="Chi")
  deviance_hemiepiphyte[i] <- 100*(1-(ANOVAS$`Resid. Dev`[2]/ANOVAS$`Resid. Dev`[1]))
  pvalue_hemiepiphyte[i] <- ANOVAS$`Pr(>Chi)`[2]
  coeff_hemiepiphyte[i]<-glm_result_1$coefficients[2]  
  # visreg(glm_result_1, "Hemiepiphyte", scale="response", ylab=colnames(ALL_SUB)[i], mar=c(0,0,0,0))
  
}

DEV_ALL2 <- as.data.frame(cbind(names(ALL_SUB), 
                                round (deviance_tree,3),
                                round (deviance_palmera,3),
                                round (deviance_liana,3),
                                round (deviance_hemiepiphyte,3),
                                
                                round (pvalue_tree,3),
                                round (pvalue_palmera,3),
                                round (pvalue_liana,3),
                                round (pvalue_hemiepiphyte,3),
                                
                                round (coeff_tree,3),
                                round (coeff_palmera,3),
                                round (coeff_liana,3),
                                round (coeff_hemiepiphyte,3)))

colnames(DEV_ALL2) <- c("Subcategory", "TREE", "PALMERA", "LIANA", "HEMIEPIPHYTE", "pTREE", "pPALMERA", "pLIANA", "pHEMIEPIPHYTE" ,"cTREE", "cPALMERA", "cLIANA", "cHEMIEPIPHYTE")
DEV_ALL2 <- column_to_rownames(DEV_ALL2, var="Subcategory")
DEV_ALL2$TREE<- as.numeric(DEV_ALL2$TREE)
DEV_ALL2$PALMERA <- as.numeric(DEV_ALL2$PALMERA)
DEV_ALL2$LIANA <- as.numeric(DEV_ALL2$LIANA)
DEV_ALL2$HEMIEPIPHYTE <- as.numeric(DEV_ALL2$HEMIEPIPHYTE)

DEV_ALL2$pTREE <- as.numeric(DEV_ALL2$pTREE)
DEV_ALL2$pPALMERA <- as.numeric(DEV_ALL2$pPALMERA)
DEV_ALL2$pLIANA <- as.numeric(DEV_ALL2$pLIANA)
DEV_ALL2$pHEMIEPIPHYTE <- as.numeric(DEV_ALL2$pHEMIEPIPHYTE)

DEV_ALL2$cTREE <- as.numeric(DEV_ALL2$cTREE)
DEV_ALL2$cPALMERA <- as.numeric(DEV_ALL2$cPALMERA)
DEV_ALL2$cLIANA <- as.numeric(DEV_ALL2$cLIANA)
DEV_ALL2$cHEMIEPIPHYTE <- as.numeric(DEV_ALL2$cHEMIEPIPHYTE)
DEV_ALL3 <- cbind(DEV_ALL, DEV_ALL2)

DEV_ALL3 <- rownames_to_column(DEV_ALL3)
DEV_STEM <- rownames_to_column(DEV_STEM)
DEV_LEAF <- rownames_to_column(DEV_LEAF)
DEV_FRUIT <- rownames_to_column(DEV_FRUIT)
DEV_DBH <- rownames_to_column(DEV_DBH)

# Prepare results to plot
DEV <- merge(DEV_ALL3, DEV_LEAF, by="rowname",all=T)
DEV1 <- merge(DEV, DEV_FRUIT, by="rowname",all=T)
DEV1 <- merge(DEV1, DEV_STEM, by="rowname",all=T)
DEV1 <- merge(DEV1, DEV_DBH, by="rowname",all=T)
DEV1 <- column_to_rownames(DEV1, var="rowname")
DEV1[is.na(DEV1)] <- 0
colnames(DEV1)

# Save p values and coefficients to other tables
DEVp <- DEV1[grepl("p", names(DEV1), fixed=TRUE)]
DEVp1 <- reshape2::melt(as.matrix(DEVp),na.rm = TRUE)
DEVp1$Var2<-gsub("p","",as.character(DEVp1$Var2))
DEVp1$mer <- paste(DEVp1$Var1, DEVp1$Var2)

DEVc <- DEV1[grepl("c", names(DEV1), fixed=TRUE)]
names(DEVc)[names(DEVc) == 'coeff'] <- 'cWD'
DEVc1 <- reshape2::melt(as.matrix(DEVc),na.rm = TRUE)
DEVc1$Var2<-gsub("c","",as.character(DEVc1$Var2))
DEVc1$mer <- paste(DEVc1$Var1, DEVc1$Var2)

# Remove from main table
DEV2 <- DEV1[!grepl("p", names(DEV1), fixed=TRUE)]
DEV2 <- DEV2[!grepl("c", names(DEV2), fixed=TRUE)]

# Vectorial
DEV3 <- reshape2::melt(as.matrix(DEV2),na.rm = TRUE) 
DEV3$mer <- paste(DEV3$Var1, DEV3$Var2)
DEV4 <- merge(DEV3, DEVp1, by="mer")
DEV5 <- merge(DEV4, DEVc1, by="mer")[,c(2,3,4,7,10)]

# Highlight significant p values and positive coefficients
DEV5$significant <- (DEV5$value.y<0.05)
DEV5$positive <- (DEV5$value>0)
DEV5[DEV5$value.x == 0.000,]$value.x <- NA
DEV5[DEV5$significant == 0,]$value.x <- NA 
DEV5[DEV5$significant == 0,]$value.y <- NA 
DEV5[DEV5$significant == 0,]$value <- NA 
DEV5[DEV5$significant == 0,]$positive <- NA 

# Eliminate no use values
DEV5 <- DEV5[!DEV5$Var2.x=="LEAF",]
DEV5 <- DEV5[!DEV5$Var1.x=="NO LEAF",]
DEV5 <- DEV5[!DEV5$Var1.x=="NO USE",]
DEV5 <- DEV5[!DEV5$Var1.x=="NO FRUIT",]
DEV5 <- DEV5[!DEV5$Var1.x=="NO STEM",]
names(DEV5)[names(DEV5) == 'value.x'] <- 'D2'

# Rename
DEV5$Var2.x <- as.character(DEV5$Var2.x)
DEV5$variable[DEV5$Var2.x == 'HEMI'] <- 'HEMIEPIPHYTE'
DEV5$Var2.x[DEV5$Var2.x == 'SEEDMASS'] <- 'SEED MASS'
DEV5$Var2.x[DEV5$Var2.x == 'PALMERA'] <- 'PALM'
DEV5$Var2.x[DEV5$Var2.x == 'all differences'] <- 'ALL AVAILABLE SPECIES'

# Create an order
ORDER <- c("WD","DBH", "LA","SLA","LT","SEED MASS","FLESHY" ,"LATEX","RESIN", "TREE","PALM","LIANA","HEMIEPIPHYTE" )
DEV5 <- arrange(DEV5, Var2.x)

# Plot Figure 6.2.
colorss <-  rev(met.brewer("Lakota", 9))[c(5,3)]

Figure6.2 <- ggballoonplot(DEV5,  col="positive", fill = "positive")+  
  scale_fill_manual(values = colorss, labels = c("Negative", "Positive"),name="")+
  scale_color_manual(values = colorss,labels = c("Negative", "Positive"), name="")+
  coord_flip()+
  scale_y_discrete(limits = ORDER)
Figure6.2

ggsave("r1_trait_2023.png", Figure6.2, width = 8, height = 8)
ggsave("r1_trait_2023.svg", Figure6.2, width = 8, height = 8)






## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
# 6.3.2. Trait selection by Indigenous communities

# Data processing including the Indigenous community
etno_original <- read_excel("etno-dis-mad-yas3.xlsx", sheet=1, col_names =T)
etno <- etno_original %>% filter(!Comunidad=="Aguapolo")
aguapolo <- etno_original %>% filter(Comunidad=="Aguapolo")

aguapolo_spp <- setdiff(aguapolo$Species, etno$Species)
df_etno <- as.data.frame(unique(cbind(etno$Comunidad, etno$Species, etno$`Plant part`, etno$Category, etno$Subcategory)))
colnames(df_etno) <- c("Comunidad", "Species", "Part", "Category", "Subcategory")

df_etno <- df_etno %>% replace_na(list(Part = 'NO USE', Category = 'NO USE', Subcategory='NO USE'))
df_etno$Newcategory <- df_etno$Subcategory

# MEDICINAL
df_etno$Newcategory <- gsub(".*[a-z].*", "MEDICINAL", df_etno$Newcategory)
# CULTURAL
df_etno$Newcategory<-gsub("PERSONAL ADORNMENT", "CLOTHES AND ACCESORIES", df_etno$Newcategory)
df_etno$Newcategory<-gsub("OTHER CULTURAL", "RITUAL", df_etno$Newcategory)
# CONSTRUCTION
df_etno$Newcategory<-gsub("BRIDGES", "TIMBER", df_etno$Newcategory)
df_etno$Newcategory<-gsub("HOUSES", "TIMBER", df_etno$Newcategory)
df_etno$Newcategory<-gsub("OTHER CONSTRUCTION", "TIMBER", df_etno$Newcategory)
df_etno$Newcategory<-gsub("TRANSPORTATION", "TIMBER", df_etno$Newcategory)
df_etno$Newcategory<-gsub("FENCES", "TIMBER", df_etno$Newcategory)
# ENVIRONMENTAL remains the SAME
# FUEL
df_etno$Newcategory<-gsub("OTHER FUEL", "FIREWOOD", df_etno$Newcategory)
df_etno$Newcategory<-gsub("FIRE STARTER", "LIGHTING", df_etno$Newcategory)
# HUMAN FOOD
df_etno$Newcategory<-gsub("FOOD ADDITIVES", "FOOD", df_etno$Newcategory)
df_etno$Newcategory<-gsub("OILS", "FOOD", df_etno$Newcategory)
# TOXIC
df_etno$Newcategory<-gsub("POISON HUNTING", "TOXIC", df_etno$Newcategory)
df_etno$Newcategory<-gsub("POISON FISHING", "TOXIC", df_etno$Newcategory)
# UTENSILS
df_etno$Newcategory<-gsub("CAUCHO", "LABOUR TOOLS", df_etno$Newcategory)
df_etno$Newcategory<-gsub("OTHER UTENSILS", "LABOUR TOOLS", df_etno$Newcategory)
# CLOTHES AND ACCESORIES
df_etno$Newcategory<-gsub("CLOTHES & ACCESORIES", "CLOTHES AND ACCESORIES", df_etno$Newcategory) # Pers adon + clothes and acc
# UTENSILS
df_etno$Newcategory<-gsub("CAUCHO", "UTENSILS", df_etno$Newcategory)
df_etno$Newcategory<-gsub("OTHER UTENSILS", "UTENSILS", df_etno$Newcategory)
df_etno$Newcategory<-gsub("LABOUR TOOLS", "UTENSILS", df_etno$Newcategory)
df_etno$Newcategory<-gsub("HUNTING & FISHING TOOLS", "UTENSILS", df_etno$Newcategory)
df_etno$Newcategory<-gsub("DOMESTIC UTENSILS", "UTENSILS", df_etno$Newcategory)
names(df_etno)[names(df_etno) == 'Newcategory'] <- 'Subcategory'
df_etno <- df_etno[,c(1,2,3,4,6)]
df_etno <- unique(df_etno)

# Join databases
traits_com <- merge(traits_good, df_etno, by="Species")

# Eliminate subcategories > 20 speecies
num_spp[num_spp$x<21,]
traits_com <- traits_com[!traits_com$Subcategory=="WRAPPERS",]
traits_com <- traits_com[!traits_com$Subcategory=="ORNAMENTAL",]
traits_com <- traits_com[!traits_com$Subcategory=="WILDLIFE ATTRACTANT",]
traits_com <- traits_com[!traits_com$Subcategory=="AGROFORESTRY",]
traits_com <- traits_com[!traits_com$Subcategory=="WILD ANIMAL FOOD",]
traits_com <- traits_com[!traits_com$Subcategory=="WILD ANIMAL BEHAVIOR",]
traits_com <- traits_com[!traits_com$Subcategory=="FISH BAIT",]
traits_com <- traits_com[!traits_com$Subcategory=="MARKETED",]
traits_com <- traits_com[!traits_com$Subcategory=="LIGHTING",]
write.table(traits_com, "17-2-23-COMUNIDAD")


# >Create a database of the traits of the species present in each Indigenous Community (traits_com_all)
etno <- etno_original %>% filter(!Comunidad=="Aguapolo")
aguapolo <- etno_original %>% filter(Comunidad=="Aguapolo")
plots_aguapolo <- unique(aguapolo$Cod_plot)
aguapolo_spp <- setdiff(aguapolo$Species, etno$Species)
df_etno <- as.data.frame(unique(cbind(etno$Comunidad, etno$Species)))
colnames(df_etno) <- c("Comunidad", "Species")
traits_com_all <- merge(traits_good, df_etno, by="Species")

# Read table composition to obtain the abundances of each species 
comp_original <- read.table("composition")
aguapolo <- comp_original %>% filter(Cod_plot %in% plots_aguapolo)
comp <- comp_original %>% filter(!Cod_plot %in% plots_aguapolo)

# Add Indigenous Community to the comp database
comun <- aggregate(etno$Comunidad, by=list(etno$Plot), function(x) unique(na.omit(x)))
colnames(comun) <- c("Plot", "Comunidad")
comun$Plot <- as.numeric(comun$Plot)
comp2 <- merge(comp, comun, by="Plot")

# Calculate relative abundance of each species for the weights in the GLM
totaldm <- aggregate(comp2$Species, by=list(comp2$Species, comp2$Comunidad), function(x) length(na.omit(x)))
total <- aggregate(totaldm$x, by=list(totaldm$Group.2), sum)
colnames(total) <- c("Group.2", "x")
relative <- merge(totaldm, total, by="Group.2")
relative$rel <- relative$x.x/relative$x.y*100
colnames(relative) <- c("Comunidad", "Species", "Ind", "Total", "rel")
relative$combi <- paste (relative$Comunidad, relative$Species)
check100 <- aggregate(relative$rel, by=list(relative$Comunidad), sum)

# Add to the traits database TOTAL
traits_com_all$combi <- paste (traits_com_all$Comunidad, traits_com_all$Species)
traits_com_all_2 <- merge(traits_com_all, relative[,c(5,6)], by="combi")

# Add to the traits database per community
traits_com$combi <- paste (traits_com$Comunidad, traits_com$Species)
traits_com_2 <- merge(traits_com, relative[,c(5,6)], by="combi")

traits_com_all_2$f_mass<- as.numeric(traits_com_all_2$f_mass)
traits_com_2$f_mass<- as.numeric(traits_com_2$f_mass)


# > Data Analysis RQ2
# A. Analysis for the availability of all trait-species per community

# 1. Continuous WD, SLA, LA, LT

# SLA
traits_com_all_noNA <- traits_com_all_2[!is.na(traits_com_all_2$SLA_mean),]
glmer1 <- lm(SLA_mean ~ 1, data = traits_com_all_noNA, weights=rel)
glmer2 <- lmer(SLA_mean ~ 1|Comunidad, data = traits_com_all_noNA, weights = rel)
AIC(glmer1, glmer2)
r.sla <- r.squaredGLMM(glmer2)

# LA
traits_com_all_noNA <- traits_com_all_2[!is.na(traits_com_all_2$LA_log),]
glmer1 <- lm(LA_log ~ 1, data = traits_com_all_noNA, weights=rel)
glmer2 <- lmer(LA_log ~ 1|Comunidad, data = traits_com_all_noNA, weights = rel)
AIC(glmer1, glmer2)
r.la <- r.squaredGLMM(glmer2)

# LT
traits_com_all_noNA <- traits_com_all_2[!is.na(traits_com_all_2$GF_mean),]
glmer1 <- lm(GF_mean ~ 1, data = traits_com_all_noNA, weights=rel)
glmer2 <- lmer(GF_mean ~ 1|Comunidad, data = traits_com_all_noNA, weights = rel)
AIC(glmer1, glmer2)
r.lt <- r.squaredGLMM(glmer2)

# WD
traits_com_all_noNA <- traits_com_all_2[!is.na(traits_com_all_2$DM_mean.y),]
glmer1 <- lm(DM_mean.y ~ 1, data = traits_com_all_noNA, weights=rel)
glmer2 <- lmer(DM_mean.y ~ 1|Comunidad, data = traits_com_all_noNA, weights = rel)
AIC(glmer1, glmer2)
r.wd <- r.squaredGLMM(glmer2)

# DBH
traits_com_all_noNA <- traits_com_all_2[!is.na(traits_com_all_2$DBH_max),]
glmer1 <- lm(DBH_max ~ 1, data = traits_com_all_noNA, weights=rel)
glmer3 <- lmer(DBH_max ~ 1|Comunidad, data = traits_com_all_noNA, weights = rel)
AIC(glmer1, glmer3)
r.dbh <- r.squaredGLMM(glmer3)

# Fruit mass
traits_com_all_noNA <- traits_com_all_2[!is.na(traits_com_all_2$f_mass),]
glmer1 <- lm(f_mass ~ 1, data = traits_com_all_noNA, weights=rel)
glmer3 <- lmer(f_mass ~ 1|Comunidad, data = traits_com_all_noNA, weights = rel)
AICc(glmer1, glmer3)
r.mass <- r.squaredGLMM(glmer3)

# Create table
continu<- as.data.frame(cbind(r.la[2], r.sla[2], r.lt[2], r.wd[2], r.dbh[2], r.mass[2]))
colnames(continu) <- c("LA","SLA", "LT", "WD", "DBH", "SEEDMASS")
continu_ver <- t(continu)


# 2. Binomial

# Latex
table(traits_com_all_noNA$NEW_latex)
traits_com_all_noNA <- traits_com_all_2[!is.na(traits_com_all_2$NEW_latex ),]
glmer1 <- glm(NEW_latex ~ 1, data = traits_com_all_noNA, family=binomial,  weights=rel)
glmer2 <- lme4::glmer(NEW_latex ~ 1|Comunidad, data = traits_com_all_noNA, family=binomial,  weights=rel)
AIC (glmer1, glmer2)
G2 = -2 * logLik(glmer1) + 2 * logLik(glmer2)
p.latex <- pchisq(as.numeric(G2), df=1, lower.tail=F)
r.latex <- r.squaredGLMM(glmer2)

# Resin
traits_com_all_noNA <- traits_com_all_2[!is.na(traits_com_all_2$NEW_resin ),]
glmer1 <- glm(NEW_resin ~ 1, data = traits_com_all_noNA, family=binomial,  weights=rel)
glmer3 <- glmer(NEW_resin ~ 1|Comunidad, data = traits_com_all_noNA, family=binomial,  weights=rel)
AIC (glmer1, glmer2)
G2 = -2 * logLik(glmer1) + 2 * logLik(glmer2)
p.resin <- pchisq(as.numeric(G2), df=1, lower.tail=F)
r.resin <- r.squaredGLMM(glmer2)

# Fleshy fruit
traits_com_all_noNA <- traits_com_all_2[!is.na(traits_com_all_2$f_fleshy ),]
glmer1 <- glm(f_fleshy ~ 1, data = traits_com_all_noNA, family=binomial,  weights=rel)
glmer2 <- glmer(f_fleshy ~ 1|Comunidad, data = traits_com_all_noNA, family=binomial,  weights=rel)
AIC (glmer1, glmer2)
G2 = -2 * logLik(glmer1) + 2 * logLik(glmer2)
p.nodules <- pchisq(as.numeric(G2), df=1, lower.tail=F)
r.fleshy <- r.squaredGLMM(glmer2)

# Join
dumi <- cbind(r.fleshy[1,2], r.latex[1,2], r.resin[1,2])
all1 <- as.data.frame(cbind(continu, dumi))
rownames(all1) <- "value"
colnames(dumi) <- c("FLESHY", "LATEX", "RESIN")

# 3. Habit

# Tree
traits_com_all_noNA <- traits_com_all_2[!is.na(traits_com_all_2$Tree ),]
glmer1 <- glm(f_fleshy ~ 1, data = traits_com_all_noNA, family=binomial,  weights=rel)
glmer3 <- glmer(f_fleshy ~ 1|Comunidad, data = traits_com_all_noNA, family=binomial,  weights=rel)
AIC (glmer1, glmer3)
G2 = -2 * logLik(glmer1) + 2 * logLik(glmer3)
p.tree <- pchisq(as.numeric(G2), df=1, lower.tail=F)
r.tree <- r.squaredGLMM(glmer3)

# Palm
traits_com_all_noNA <- traits_com_all_2[!is.na(traits_com_all_2$Palmera ),]
glmer1 <- glm(Palmera ~ 1, data = traits_com_all_noNA, family=binomial,  weights=rel)
glmer3 <- glmer(Palmera ~ 1|Comunidad, data = traits_com_all_noNA, family=binomial,  weights=rel)
AIC (glmer1, glmer3)
G2 = -2 * logLik(glmer1) + 2 * logLik(glmer3)
p.palmera <- pchisq(as.numeric(G2), df=1, lower.tail=F)
r.palmera <- r.squaredGLMM(glmer3)

# Liana
traits_com_all_noNA <- traits_com_all_2[!is.na(traits_com_all_2$Liana ),]
glmer1 <- glm(Liana ~ 1, data = traits_com_all_noNA, family=binomial,  weights=rel)
glmer3 <- glmer(Liana ~ 1|Comunidad, data = traits_com_all_noNA, family=binomial,  weights=rel)
AIC (glmer1, glmer3)
G2 = -2 * logLik(glmer1) + 2 * logLik(glmer3)
p.liana <- pchisq(as.numeric(G2), df=1, lower.tail=F)
r.liana <- r.squaredGLMM(glmer3)

# Hemiepiphyte
traits_com_all_noNA <- traits_com_all_2[!is.na(traits_com_all_2$Hemiepiphyte ),]
glmer1 <- glm(Hemiepiphyte ~ 1, data = traits_com_all_noNA, family=binomial,  weights=rel)
glmer3 <- glmer(Hemiepiphyte ~ 1|Comunidad, data = traits_com_all_noNA, family=binomial,  weights=rel)
AIC (glmer1, glmer3)
G2 = -2 * logLik(glmer1) + 2 * logLik(glmer3)
p.hemiepiphyte <- pchisq(as.numeric(G2), df=1, lower.tail=F)
r.hemiepiphyte <- r.squaredGLMM(glmer3)

# Join continuous, dumi and growth form
dumi2 <- cbind(r.tree[1,2], r.palmera[1,2], r.liana[1,2], r.hemiepiphyte[1,2])
colnames(dumi2) <- c("TREE", "PALM", "LIANA", "HEMIEPIPHYTE")
all <- t(as.data.frame(cbind(continu, dumi, dumi2)))
ALL <- round(all,3)



# B. Analysis for similar selection among communities for each trait-service relation

# 1. Stem
traits_com <- traits_com_2
stem_part <- c(traits_com$Part=="BARK_STEM" | traits_com$Part=="STEM"   | traits_com$Part=="BARK_ROOT"  | 
                 traits_com$Part=="BARK"    | traits_com$Part=="SPINES" | traits_com$Part=="STEM_ENTIRE LEAF" | 
                 traits_com$Part=="SPINES"| traits_com$Part=="ROOT_STEM")
traits_com_stem <- traits_com[stem_part,]
traits_com_stem <- traits_com_stem[!duplicated(traits_com_stem),]
traits_com_stem <- traits_com_stem[!is.na(traits_com_stem$DM_mean.y),]
categorydata <- traits_com_stem[traits_com_stem$Subcategory=="TOXIC",]
categorydata <- categorydata %>%  group_by(Comunidad) %>%  filter(n_distinct(DM_mean.y) >= 3)

if(nrow(categorydata)<=10){
  r.wd <- 0
} else {
  glmer1 <- lm(DBH_max ~ 1, data = categorydata, weights=rel)
  glmer3 <- lmer(DBH_max ~ 1|Comunidad, data = categorydata, weights = rel)
  AIC(glmer1, glmer3)
  r.squaredGLMM(glmer3)
  if (AIC(glmer1, glmer3)[1,2] > AIC(glmer1, glmer3)[2,2]) {
    r.wd <- r.squaredGLMM(glmer3)[1,2]
  } else {
    r.wd <- 0
  }
}

r.trait <-rep(NULL)
for (i in unique(traits_com_stem$Subcategory)) {
  categorydata <- traits_com_stem[traits_com_stem$Subcategory==i,]
  categorydata <- categorydata %>%  group_by(Comunidad) %>%  filter(n_distinct(DM_mean.y) >= 3)
  if(nrow(categorydata)<=10){
    r.trait[i] <- 0
  } else {
    glmer1 <- lm(DBH_max ~ 1, data = categorydata, weights=rel)
    glmer3 <- lmer(DBH_max ~ 1|Comunidad, data = categorydata, weights = rel)
    AIC(glmer1, glmer3)
    r.squaredGLMM(glmer3)
    if (AIC(glmer1, glmer3)[1,2] > AIC(glmer1, glmer3)[2,2]) {
      r.trait[i] <- r.squaredGLMM(glmer3)[1,2]
    } else {
      r.trait[i] <- 0
    }
  }
}
c.wd <- r.trait

# 2. Leaves
traits_com <- traits_com_2
leaf_part <- c(traits_com$Part=="ENTIRE LEAF" | traits_com$Part=="PETIOLE"| traits_com$Part=="BARK_ENTIRE LEAF" | 
                 traits_com$Part=="BRACT"| traits_com$Part=="LEAF SHEATH" | traits_com$Part=="PALM HEART"| 
                 traits_com$Part=="LEAF RACHIS" | traits_com$Part=="ENTIRE LEAF_FLOWER_FRUIT"| 
                 traits_com$Part=="ENTIRE LEAF_FRUIT")
traits_com_leaf <- traits_com[leaf_part,]
traits_com_leaf <- traits_com_leaf[!duplicated(traits_com_leaf),]

# SLA
traits_com_leaf_SLA <- traits_com_leaf[!is.na(traits_com_leaf$SLA_mean),]
r.trait <-rep(NULL)
for (i in unique(traits_com_leaf_SLA$Subcategory)) {
  categorydata <- traits_com_leaf_SLA[traits_com_leaf_SLA$Subcategory==i,]
  categorydata <- categorydata %>%  group_by(Comunidad) %>%  filter(n_distinct(SLA_mean) >= 3)
  if(nrow(categorydata)<=10){
    r.trait[i] <- 0
  } else {
    glmer1 <- lm(SLA_mean ~ 1, data = categorydata, weights=rel)
    glmer3 <- lmer(SLA_mean ~ 1|Comunidad, data = categorydata, weights = rel)
    AIC(glmer1, glmer3)
    r.squaredGLMM(glmer3)
    if (AIC(glmer1, glmer3)[1,2] > AIC(glmer1, glmer3)[2,2]) {
      r.trait[i] <- r.squaredGLMM(glmer3)[1,2]
    } else {
      r.trait[i] <- 0
    }
  }
}
c.sla <- r.trait

# LA
traits_com_leaf_LA <- traits_com_leaf[!is.na(traits_com_leaf$LA_log),]
r.trait <-rep(NULL)
for (i in unique(traits_com_leaf_LA$Subcategory)) {
  categorydata <- traits_com_leaf_LA[traits_com_leaf_LA$Subcategory==i,]
  categorydata <- categorydata %>%  group_by(Comunidad) %>%  filter(n_distinct(LA_log) >= 3)
  if(nrow(categorydata)<=10){
    r.trait[i] <- 0
  } else {
    glmer1 <- lm(LA_log ~ 1, data = categorydata, weights=rel)
    glmer3 <- lmer(LA_log ~ 1|Comunidad, data = categorydata, weights = rel)
    AIC(glmer1, glmer3)
    r.squaredGLMM(glmer3)
    if (AIC(glmer1, glmer3)[1,2] > AIC(glmer1, glmer3)[2,2]) {
      r.trait[i] <- r.squaredGLMM(glmer3)[1,2]
    } else {
      r.trait[i] <- 0
    }
  }
}
c.la <- r.trait

# LT
traits_com_leaf_LT <- traits_com_leaf[!is.na(traits_com_leaf$GF_mean),]
r.trait <- NULL
for (i in unique(traits_com_leaf_LT$Subcategory)) {
  categorydata <- traits_com_leaf_LT[traits_com_leaf_LT$Subcategory==i,]
  categorydata <- categorydata %>%  group_by(Comunidad) %>%  filter(n_distinct(GF_mean) >= 3)
  if(nrow(categorydata)<=10){
    r.trait[i] <- 0
  } else {
    glmer1 <- lm(GF_mean ~ 1, data = categorydata, weights=rel)
    glmer3 <- lmer(GF_mean ~ 1|Comunidad, data = categorydata, weights = rel)
    AIC(glmer1, glmer3)
    r.squaredGLMM(glmer3)
    if (AIC(glmer1, glmer3)[1,2] > AIC(glmer1, glmer3)[2,2]) {
      r.trait[i] <- r.squaredGLMM(glmer3)[1,2]
    } else {
      r.trait[i] <- 0
    }
  }
}
c.lt <- r.trait

# 3. Fruit
traits_com <- traits_com_2
fruit_part <- c(traits_com$Part=="FRUIT" | traits_com$Part=="FLOWER"| traits_com$Part=="SEED" | 
                  traits_com$Part=="FRUIT_SEED" | traits_com$Part=="INFRUTESCENCE"| traits_com$Part=="INFLORESCENCE")
traits_fruit <- traits_com[fruit_part,]
traits_fruit <- traits_fruit[!duplicated(traits_fruit),]

# Fruit mass
traits_fruit_SM <- traits_fruit[!is.na(traits_fruit$f_mass),]

r.trait <- NULL
for (i in unique(traits_fruit_SM$Subcategory)) {
  categorydata <- traits_fruit_SM[traits_fruit_SM$Subcategory==i,]
  categorydata <- categorydata %>%  group_by(Comunidad) %>%  filter(n_distinct(f_mass) >= 3)
  if(nrow(categorydata)<=10){
    r.trait[i] <- 0
  } else {
    glmer1 <- lm(f_mass ~ 1, data = categorydata, weights=rel)
    glmer3 <- lmer(f_mass ~ 1|Comunidad, data = categorydata, weights = rel)
    AIC(glmer1, glmer3)
    r.squaredGLMM(glmer3)
    if (AIC(glmer1, glmer3)[1,2] > AIC(glmer1, glmer3)[2,2]) {
      r.trait[i] <- r.squaredGLMM(glmer3)[1,2]
    } else {
      r.trait[i] <- 0
    }
  }
}
c.mass <- r.trait

# Fleshy

r.trait <- NULL
for (i in unique(traits_fruit_SM$Subcategory)) {
  categorydata_1 <- traits_fruit_SM[traits_fruit_SM$Subcategory==i,]
  sum_count <- categorydata_1  %>% 
    filter(f_fleshy==1) %>% 
    group_by(Comunidad) %>% 
    tally()  %>% 
    filter (n >= 3)
  categorydata <- categorydata_1 %>%  filter(Comunidad %in% sum_count$Comunidad) 
  if(nrow(sum_count)<=1){
    r.trait[i] <- 0
  } else {
    glmer1 <- glm(f_fleshy ~ 1, data = categorydata, family=binomial,  weights=rel)
    glmer3 <- glmer(f_fleshy ~ 1|Comunidad, data = categorydata, family=binomial,  weights=rel)
    AIC(glmer1, glmer3)
    r.squaredGLMM(glmer3)
    if (AIC(glmer1, glmer3)[1,2] > AIC(glmer1, glmer3)[2,2]) {
      r.trait[i] <- r.squaredGLMM(glmer3)[1,2]
    } else {
      r.trait[i] <- 0
    }
  }
}
c.fleshy <- r.trait

# 4. All
traits_com <- traits_com_2

# DBH
traits_com_DBH <- traits_com[!is.na(traits_com$DBH_max ),]
r.trait <- NULL
for (i in unique(traits_com_DBH$Subcategory)) {
  categorydata <- traits_com_DBH[traits_com_DBH$Subcategory==i,]
  categorydata <- categorydata %>%  group_by(Comunidad) %>%  filter(n_distinct(Species) >= 3)
  if(nrow(categorydata)<=10){
    r.trait[i] <- 0
  } else {
    glmer1 <- lm(f_mass ~ 1, data = categorydata, weights=rel)
    glmer3 <- lmer(f_mass ~ 1|Comunidad, data = categorydata, weights = rel)
    AIC(glmer1, glmer3)
    r.squaredGLMM(glmer3)
    if (AIC(glmer1, glmer3)[1,2] > AIC(glmer1, glmer3)[2,2]) {
      r.trait[i] <- r.squaredGLMM(glmer3)[1,2]
    } else {
      r.trait[i] <- 0
    }
  }
}
c.dbh <- r.trait

# Latex
traits_com_LX <- traits_com[!is.na(traits_com$NEW_latex ),]
r.trait <- NULL
for (i in unique(traits_com_LX$Subcategory)) {
  categorydata_1 <- traits_com_LX[traits_com_LX$Subcategory==i,]
  sum_count <- categorydata_1  %>% 
    filter(NEW_latex==1) %>% 
    group_by(Comunidad) %>% 
    tally()  %>% 
    filter (n >= 3)
  categorydata <- categorydata_1 %>%  filter(Comunidad %in% sum_count$Comunidad) 
  if(nrow(sum_count)<=1){
    r.trait[i] <- 0
  } else {
    glmer1 <- glm(NEW_latex ~ 1, data = categorydata, weights=rel,  family=binomial)
    glmer3 <- glmer(NEW_latex ~ 1|Comunidad, data = categorydata, weights = rel,  family=binomial)
    AIC(glmer1, glmer3)
    r.squaredGLMM(glmer3)
    if (AIC(glmer1, glmer3)[1,2] > AIC(glmer1, glmer3)[2,2]) {
      r.trait[i] <- r.squaredGLMM(glmer3)[1,2]
    } else {
      r.trait[i] <- 0
    }
  }
}
c.latex <- r.trait

# Resin
traits_com <- traits_com_2
traits_com_RS <- traits_com[!is.na(traits_com$NEW_resin ),]
r.trait <- NULL
for (i in unique(traits_com_RS$Subcategory)) {
  categorydata_1 <- traits_com_RS[traits_com_RS$Subcategory==i,]
  sum_count <- categorydata_1  %>% 
    filter(NEW_resin==1) %>% 
    group_by(Comunidad) %>% 
    tally()  %>% 
    filter (n >= 3)
  categorydata <- categorydata_1 %>%  filter(Comunidad %in% sum_count$Comunidad) 
  if(nrow(sum_count)<=1){
    r.trait[i] <- 0
  } else {
    glmer1 <- glm(NEW_resin ~ 1, data = categorydata, weights=rel,  family=binomial)
    glmer3 <- glmer(NEW_resin ~ 1|Comunidad, data = categorydata, weights = rel,  family=binomial)
    AIC(glmer1, glmer3)
    r.squaredGLMM(glmer3)
    if (AIC(glmer1, glmer3)[1,2] > AIC(glmer1, glmer3)[2,2]) {
      r.trait[i] <- r.squaredGLMM(glmer3)[1,2]
    } else {
      r.trait[i] <- 0
    }
  }
}
c.resin <- r.trait

# Tree
traits_com <- traits_com_2
r.trait <- rep(NULL)
traits_com_TR <- traits_com[!is.na(traits_com$Tree),]
for (i in unique(traits_com_TR$Subcategory)) {
  categorydata_1 <- traits_com_TR[traits_com_TR$Subcategory==i,]
  sum_count <- categorydata_1  %>% 
    filter(Tree==1) %>% 
    group_by(Comunidad) %>% 
    tally()  %>% 
    filter (n >= 3)
  categorydata <- categorydata_1 %>%  filter(Comunidad %in% sum_count$Comunidad) 
  if(nrow(sum_count)<=1){
    r.trait[i] <- 0
  } else {
    glmer1 <- glm(Tree ~ 1, data = categorydata, weights=rel,  family=binomial)
    glmer3 <- glmer(Tree ~ 1|Comunidad, data = categorydata, weights = rel,  family=binomial)
    AIC(glmer1, glmer3)
    r.squaredGLMM(glmer3)
    if (AIC(glmer1, glmer3)[1,2] > AIC(glmer1, glmer3)[2,2]) {
      r.trait[i] <- r.squaredGLMM(glmer3)[1,2]
    } else {
      r.trait[i] <- 0
    }
  }
}
c.tree <- r.trait

# Palmera
traits_com <- traits_com_2
r.trait <- rep(NULL)
traits_com_PL <- traits_com[!is.na(traits_com$Palmera),]
for (i in unique(traits_com_PL$Subcategory)) {
  categorydata_1 <- traits_com_PL[traits_com_PL$Subcategory==i,]
  sum_count <- categorydata_1  %>% 
    filter(Palmera==1) %>% 
    group_by(Comunidad) %>% 
    tally()  %>% 
    filter (n >= 3)
  categorydata <- categorydata_1 %>%  filter(Comunidad %in% sum_count$Comunidad) 
  if(nrow(sum_count)<=1){
    r.trait[i] <- 0
  } else {
    glmer1 <- glm(Palmera ~ 1, data = categorydata, weights=rel,  family=binomial)
    glmer3 <- glmer(Palmera ~ 1|Comunidad, data = categorydata, weights = rel,  family=binomial)
    AIC(glmer1, glmer3)
    r.squaredGLMM(glmer3)
    if (AIC(glmer1, glmer3)[1,2] > AIC(glmer1, glmer3)[2,2]) {
      r.trait[i] <- r.squaredGLMM(glmer3)[1,2]
    } else {
      r.trait[i] <- 0
    }
  }
}
c.palm <- r.trait

# Liana
traits_com <- traits_com_2
r.trait <- rep(NULL)
traits_com_LN <- traits_com[!is.na(traits_com$Liana),]
for (i in unique(traits_com_LN$Subcategory)) {
  categorydata_1 <- traits_com_LN[traits_com_LN$Subcategory==i,]
  sum_count <- categorydata_1  %>% 
    filter(Liana==1) %>% 
    group_by(Comunidad) %>% 
    tally()  %>% 
    filter (n >= 3)
  categorydata <- categorydata_1 %>%  filter(Comunidad %in% sum_count$Comunidad) 
  if(nrow(sum_count)<=1){
    r.trait[i] <- 0
  } else {
    glmer1 <- glm(Liana ~ 1, data = categorydata, weights=rel,  family=binomial)
    glmer3 <- glmer(Liana ~ 1|Comunidad, data = categorydata, weights = rel,  family=binomial)
    AIC(glmer1, glmer3)
    r.squaredGLMM(glmer3)
    if (AIC(glmer1, glmer3)[1,2] > AIC(glmer1, glmer3)[2,2]) {
      r.trait[i] <- r.squaredGLMM(glmer3)[1,2]
    } else {
      r.trait[i] <- 0
    }
  }
}
c.liana <- r.trait

# Hemiepiphyte

traits_com <- traits_com_2
r.trait <- rep(NULL)
traits_com_HM <- traits_com[!is.na(traits_com$Hemiepiphyte),]
for (i in unique(traits_com_HM$Subcategory)) {
  categorydata_1 <- traits_com_HM[traits_com_HM$Subcategory==i,]
  sum_count <- categorydata_1  %>% 
    filter(Hemiepiphyte==1) %>% 
    group_by(Comunidad) %>% 
    tally()  %>% 
    filter (n >= 3)
  categorydata <- categorydata_1 %>%  filter(Comunidad %in% sum_count$Comunidad) 
  if(nrow(sum_count)<=1){
    r.trait[i] <- 0
  } else {
    glmer1 <- glm(Hemiepiphyte ~ 1, data = categorydata, weights=rel,  family=binomial)
    glmer3 <- glmer(Hemiepiphyte ~ 1|Comunidad, data = categorydata, weights = rel,  family=binomial)
    AIC(glmer1, glmer3)
    r.squaredGLMM(glmer3)
    if (AIC(glmer1, glmer3)[1,2] > AIC(glmer1, glmer3)[2,2]) {
      r.trait[i] <- r.squaredGLMM(glmer3)[1,2]
    } else {
      r.trait[i] <- 0
    }
  }
}
c.hemi <- r.trait

for (i in unique(traits_com_HM$Subcategory)) {
  categorydata_1 <- traits_com_HM[traits_com_HM$Subcategory==i,]
  if(nrow(categorydata)<=10){
    r.trait[i] <- 0
  } else {
    glmer1 <- glm(Hemiepiphyte ~ 1, data = categorydata, weights=rel,  family=binomial)
    glmer3 <- glmer(Hemiepiphyte ~ 1|Comunidad, data = categorydata, weights = rel,  family=binomial)
    AIC(glmer1, glmer3)
    r.squaredGLMM(glmer3)
    if (AIC(glmer1, glmer3)[1,2] > AIC(glmer1, glmer3)[2,2]) {
      r.trait[i] <- r.squaredGLMM(glmer3)[1,2]
    } else {
      r.trait[i] <- 0
    }
  }
}
c.hemi <- r.trait


# >Collect results

# Trait-service
list_all<-list(c.la, c.sla, c.lt, c.wd, c.dbh, c.mass, c.fleshy,  c.latex, c.resin, c.tree, c.palm, c.liana, c.hemi)
names(list_all) <-c("LA","SLA", "LT", "WD", "DBH", "SEEDMASS", "FLESHY", "LATEX", "RESIN", "TREE", "PALM", "LIANA", "HEMIEPIPHYTE")
list_data <- map(list_all, data.frame)
list_data <- map(list_data, rownames_to_column)
data_r <- do.call(rbind.data.frame, list_data)
colnames(data_r) <- c("Category", "R2")
data_r$R2 <- round(data_r$R2, 3)
data_r <- rownames_to_column(data_r)
data_r$rowname<-gsub("\\.[0-9]+","",data_r$rowname)

# All data
ALL1 <- ALL
ALL1 <- as.data.frame(cbind(ALL,rep("ALL", 13)))
ALL1 <- rownames_to_column(ALL1)
ALL1 <- ALL1[,c(1,3,2)]
colnames(ALL1) <- c("rowname", "Category", "R2")
data_r_all <- rbind(data_r, ALL1)
data_r_all$R2 <- as.numeric(data_r_all$R2)
data_r_all[data_r_all$R2==0,] <- NA
data_r_all <- data_r_all %>% drop_na(rowname) %>% filter(!Category=="NO USE")

# Prepare to plot
ORDER <- c("WD","DBH", "LA","SLA","LT","SEEDMASS","FLESHY" ,"LATEX","RESIN","TREE","PALM","LIANA","HEMIEPIPHYTE" )
ORDER2 <- sort(unique(data_r_all$Category))

# Plot figure 6.3.
colorss <-  met.brewer("VanGogh3", 10)[1:10]
Figure6.3 <- ggballoonplot(data_r_all, color = "R2", fill = "R2") +scale_color_gradientn(colors = colorss, guide="none")+
  scale_fill_gradientn(colors = colorss)+scale_x_discrete(limits = ORDER)+scale_y_discrete(limits=ORDER2) + guides(col="none")

Figure6.3
ggsave("r2_trait_2023.png", Figure6.3, width = 8,height = 8)
ggsave("r2_trait_2023.svg", Figure6.3, width = 8,height = 8)




# > # > # > # > # > # > # > # > # > # > # > # > # > 
# > # > # > # > # > # > # > # > # > # > # > # > # > 
# > # > # > # > # > # > # > # > # > # > # > # > # > 


# > SUPPLEMENTARY Data Analysis RQ2
# A. Analysis for the availability of all trait-species per community AS FIXED FACTOR!

# 1. Continuous WD, SLA, LA, LT

# SLA
traits_com_all_noNA <- traits_com_all_2[!is.na(traits_com_all_2$SLA_mean),]
glmer1 <- lm (SLA_mean ~ 1, data = traits_com_all_noNA, weights=rel)
glmer2 <- lmer (SLA_mean ~ 1|Comunidad, data = traits_com_all_noNA, weights = rel)
glmer2.2 <- lm (SLA_mean ~ Comunidad, data = traits_com_all_noNA, weights = rel)
AIC (glmer1, glmer2.2, glmer2)
#r.sla <- r.squaredGLMM(glmer2)
r.sla <- summary(glmer2.2)$adj.r.squared

# LA
traits_com_all_noNA <- traits_com_all_2[!is.na(traits_com_all_2$LA_log),]
glmer1 <- lm(LA_log ~ 1, data = traits_com_all_noNA, weights=rel)
glmer2 <- lmer(LA_log ~ 1|Comunidad, data = traits_com_all_noNA, weights = rel)
glmer2.2 <- lm (LA_log ~ Comunidad, data = traits_com_all_noNA, weights = rel)
AIC (glmer1, glmer2.2, glmer2)
r.la <- summary(glmer2.2)$adj.r.squared
#r.la <- r.squaredGLMM(glmer2)

# LT
traits_com_all_noNA <- traits_com_all_2[!is.na(traits_com_all_2$GF_mean),]
glmer1 <- lm(GF_mean ~ 1, data = traits_com_all_noNA, weights=rel)
glmer2 <- lmer(GF_mean ~ 1|Comunidad, data = traits_com_all_noNA, weights = rel)
glmer2.2 <- lm (GF_mean ~ Comunidad, data = traits_com_all_noNA, weights = rel)
AIC (glmer1, glmer2.2, glmer2)
r.lt <- summary(glmer2.2)$adj.r.squared

# WD
traits_com_all_noNA <- traits_com_all_2[!is.na(traits_com_all_2$DM_mean.y),]
glmer1 <- lm(DM_mean.y ~ 1, data = traits_com_all_noNA, weights=rel)
glmer2 <- lmer(DM_mean.y ~ 1|Comunidad, data = traits_com_all_noNA, weights = rel)
glmer2.2 <- lm (DM_mean.y ~ Comunidad, data = traits_com_all_noNA, weights = rel)
AIC (glmer1, glmer2.2, glmer2)
r.wd <- summary(glmer2.2)$adj.r.squared

# DBH
traits_com_all_noNA <- traits_com_all_2[!is.na(traits_com_all_2$DBH_max),]
glmer1 <- lm(DBH_max ~ 1, data = traits_com_all_noNA, weights=rel)
glmer2 <- lmer(DBH_max ~ 1|Comunidad, data = traits_com_all_noNA, weights = rel)
glmer2.2 <- lm (DBH_max ~ Comunidad, data = traits_com_all_noNA, weights = rel)
AIC (glmer1, glmer2.2, glmer2)
r.dbh <- summary(glmer2.2)$adj.r.squared

# Fruit mass
traits_com_all_noNA <- traits_com_all_2[!is.na(traits_com_all_2$f_mass),]
glmer1 <- lm(f_mass ~ 1, data = traits_com_all_noNA, weights=rel)
glmer2 <- lmer(f_mass ~ 1|Comunidad, data = traits_com_all_noNA, weights = rel)
glmer2.2 <- lm (f_mass ~ Comunidad, data = traits_com_all_noNA, weights = rel)
AIC (glmer1, glmer2.2, glmer2)
r.mass <- summary(glmer2.2)$adj.r.squared


# Create table
continu<- as.data.frame(cbind(r.la, r.sla, r.lt, r.wd, r.dbh, r.mass))

colnames(continu) <- c("LA","SLA", "LT", "WD", "DBH", "SEEDMASS")
continu_ver <- t(continu)


# 2. Binomial
library(glmtoolbox)

# Latex
table(traits_com_all_noNA$NEW_latex)
traits_com_all_noNA <- traits_com_all_2[!is.na(traits_com_all_2$NEW_latex ),]
glmer1 <- glm(NEW_latex ~ 1, data = traits_com_all_noNA, family=binomial,  weights=rel)
glmer2 <- lme4::glmer(NEW_latex ~ 1|Comunidad, data = traits_com_all_noNA, family=binomial,  weights=rel)
glmer2.2 <- glm(NEW_latex ~ Comunidad, data = traits_com_all_noNA, family=binomial,  weights=rel)
AIC (glmer1, glmer2.2, glmer2)
# r.latex <-adjR2 (glmer2.2)
r.latex <- 0
#G2 = -2 * logLik(glmer1) + 2 * logLik(glmer2.2)
#p.latex <- pchisq(as.numeric(G2), df=1, lower.tail=F)
#with(summary(glmer2.2), 1 - deviance/null.deviance)
#r.latex <- r.squaredGLMM(glmer2)

# Resin
traits_com_all_noNA <- traits_com_all_2[!is.na(traits_com_all_2$NEW_resin ),]
glmer1 <- glm(NEW_resin ~ 1, data = traits_com_all_noNA, family=binomial,  weights=rel)
glmer3 <- glmer(NEW_resin ~ 1|Comunidad, data = traits_com_all_noNA, family=binomial,  weights=rel)
AIC (glmer1, glmer2)
G2 = -2 * logLik(glmer1) + 2 * logLik(glmer2)
p.resin <- pchisq(as.numeric(G2), df=1, lower.tail=F)
r.resin <- r.squaredGLMM(glmer2)

# Fleshy fruit
traits_com_all_noNA <- traits_com_all_2[!is.na(traits_com_all_2$f_fleshy ),]
glmer1 <- glm(f_fleshy ~ 1, data = traits_com_all_noNA, family=binomial,  weights=rel)
glmer2 <- glmer(f_fleshy ~ 1|Comunidad, data = traits_com_all_noNA, family=binomial,  weights=rel)
AIC (glmer1, glmer2)
G2 = -2 * logLik(glmer1) + 2 * logLik(glmer2)
p.nodules <- pchisq(as.numeric(G2), df=1, lower.tail=F)
r.fleshy <- r.squaredGLMM(glmer2)

# Join
dumi <- cbind(r.fleshy[1,2], r.latex[1,2], r.resin[1,2])
all1 <- as.data.frame(cbind(continu, dumi))
rownames(all1) <- "value"
colnames(dumi) <- c("FLESHY", "LATEX", "RESIN")

# 3. Habit

# Tree
traits_com_all_noNA <- traits_com_all_2[!is.na(traits_com_all_2$Tree ),]
glmer1 <- glm(f_fleshy ~ 1, data = traits_com_all_noNA, family=binomial,  weights=rel)
glmer3 <- glmer(f_fleshy ~ 1|Comunidad, data = traits_com_all_noNA, family=binomial,  weights=rel)
AIC (glmer1, glmer3)
G2 = -2 * logLik(glmer1) + 2 * logLik(glmer3)
p.tree <- pchisq(as.numeric(G2), df=1, lower.tail=F)
r.tree <- r.squaredGLMM(glmer3)

# Palm
traits_com_all_noNA <- traits_com_all_2[!is.na(traits_com_all_2$Palmera ),]
glmer1 <- glm(Palmera ~ 1, data = traits_com_all_noNA, family=binomial,  weights=rel)
glmer3 <- glmer(Palmera ~ 1|Comunidad, data = traits_com_all_noNA, family=binomial,  weights=rel)
AIC (glmer1, glmer3)
G2 = -2 * logLik(glmer1) + 2 * logLik(glmer3)
p.palmera <- pchisq(as.numeric(G2), df=1, lower.tail=F)
r.palmera <- r.squaredGLMM(glmer3)

# Liana
traits_com_all_noNA <- traits_com_all_2[!is.na(traits_com_all_2$Liana ),]
glmer1 <- glm(Liana ~ 1, data = traits_com_all_noNA, family=binomial,  weights=rel)
glmer3 <- glmer(Liana ~ 1|Comunidad, data = traits_com_all_noNA, family=binomial,  weights=rel)
AIC (glmer1, glmer3)
G2 = -2 * logLik(glmer1) + 2 * logLik(glmer3)
p.liana <- pchisq(as.numeric(G2), df=1, lower.tail=F)
r.liana <- r.squaredGLMM(glmer3)

# Hemiepiphyte
traits_com_all_noNA <- traits_com_all_2[!is.na(traits_com_all_2$Hemiepiphyte ),]
glmer1 <- glm(Hemiepiphyte ~ 1, data = traits_com_all_noNA, family=binomial,  weights=rel)
glmer3 <- glmer(Hemiepiphyte ~ 1|Comunidad, data = traits_com_all_noNA, family=binomial,  weights=rel)
AIC (glmer1, glmer3)
G2 = -2 * logLik(glmer1) + 2 * logLik(glmer3)
p.hemiepiphyte <- pchisq(as.numeric(G2), df=1, lower.tail=F)
r.hemiepiphyte <- r.squaredGLMM(glmer3)

# Join continuous, dumi and growth form
dumi2 <- cbind(r.tree[1,2], r.palmera[1,2], r.liana[1,2], r.hemiepiphyte[1,2])
colnames(dumi2) <- c("TREE", "PALM", "LIANA", "HEMIEPIPHYTE")
all <- t(as.data.frame(cbind(continu, dumi, dumi2)))
ALL <- round(all,3)



# B. Analysis for similar selection among communities for each trait-service relation

# 1. Stem
traits_com <- traits_com_2
stem_part <- c(traits_com$Part=="BARK_STEM" | traits_com$Part=="STEM"   | traits_com$Part=="BARK_ROOT"  | 
                 traits_com$Part=="BARK"    | traits_com$Part=="SPINES" | traits_com$Part=="STEM_ENTIRE LEAF" | 
                 traits_com$Part=="SPINES"| traits_com$Part=="ROOT_STEM")
traits_com_stem <- traits_com[stem_part,]
traits_com_stem <- traits_com_stem[!duplicated(traits_com_stem),]
traits_com_stem <- traits_com_stem[!is.na(traits_com_stem$DM_mean.y),]
categorydata <- traits_com_stem[traits_com_stem$Subcategory=="TOXIC",]
categorydata <- categorydata %>%  group_by(Comunidad) %>%  filter(n_distinct(DM_mean.y) >= 3)

if(nrow(categorydata)<=10){
  r.wd <- 0
} else {
  glmer1 <- lm(DBH_max ~ 1, data = categorydata, weights=rel)
  glmer3 <- lmer(DBH_max ~ 1|Comunidad, data = categorydata, weights = rel)
  AIC(glmer1, glmer3)
  r.squaredGLMM(glmer3)
  if (AIC(glmer1, glmer3)[1,2] > AIC(glmer1, glmer3)[2,2]) {
    r.wd <- r.squaredGLMM(glmer3)[1,2]
  } else {
    r.wd <- 0
  }
}

r.trait <-rep(NULL)
for (i in unique(traits_com_stem$Subcategory)) {
  categorydata <- traits_com_stem[traits_com_stem$Subcategory==i,]
  categorydata <- categorydata %>%  group_by(Comunidad) %>%  filter(n_distinct(DM_mean.y) >= 3)
  if(nrow(categorydata)<=10){
    r.trait[i] <- 0
  } else {
    glmer1 <- lm(DBH_max ~ 1, data = categorydata, weights=rel)
    glmer3 <- lmer(DBH_max ~ 1|Comunidad, data = categorydata, weights = rel)
    AIC(glmer1, glmer3)
    r.squaredGLMM(glmer3)
    if (AIC(glmer1, glmer3)[1,2] > AIC(glmer1, glmer3)[2,2]) {
      r.trait[i] <- r.squaredGLMM(glmer3)[1,2]
    } else {
      r.trait[i] <- 0
    }
  }
}
c.wd <- r.trait

# 2. Leaves
traits_com <- traits_com_2
leaf_part <- c(traits_com$Part=="ENTIRE LEAF" | traits_com$Part=="PETIOLE"| traits_com$Part=="BARK_ENTIRE LEAF" | 
                 traits_com$Part=="BRACT"| traits_com$Part=="LEAF SHEATH" | traits_com$Part=="PALM HEART"| 
                 traits_com$Part=="LEAF RACHIS" | traits_com$Part=="ENTIRE LEAF_FLOWER_FRUIT"| 
                 traits_com$Part=="ENTIRE LEAF_FRUIT")
traits_com_leaf <- traits_com[leaf_part,]
traits_com_leaf <- traits_com_leaf[!duplicated(traits_com_leaf),]

# SLA
traits_com_leaf_SLA <- traits_com_leaf[!is.na(traits_com_leaf$SLA_mean),]
r.trait <-rep(NULL)
for (i in unique(traits_com_leaf_SLA$Subcategory)) {
  categorydata <- traits_com_leaf_SLA[traits_com_leaf_SLA$Subcategory==i,]
  categorydata <- categorydata %>%  group_by(Comunidad) %>%  filter(n_distinct(SLA_mean) >= 3)
  if(nrow(categorydata)<=10){
    r.trait[i] <- 0
  } else {
    glmer1 <- lm(SLA_mean ~ 1, data = categorydata, weights=rel)
    glmer3 <- lmer(SLA_mean ~ 1|Comunidad, data = categorydata, weights = rel)
    AIC(glmer1, glmer3)
    r.squaredGLMM(glmer3)
    if (AIC(glmer1, glmer3)[1,2] > AIC(glmer1, glmer3)[2,2]) {
      r.trait[i] <- r.squaredGLMM(glmer3)[1,2]
    } else {
      r.trait[i] <- 0
    }
  }
}
c.sla <- r.trait

# LA
traits_com_leaf_LA <- traits_com_leaf[!is.na(traits_com_leaf$LA_log),]
r.trait <-rep(NULL)
for (i in unique(traits_com_leaf_LA$Subcategory)) {
  categorydata <- traits_com_leaf_LA[traits_com_leaf_LA$Subcategory==i,]
  categorydata <- categorydata %>%  group_by(Comunidad) %>%  filter(n_distinct(LA_log) >= 3)
  if(nrow(categorydata)<=10){
    r.trait[i] <- 0
  } else {
    glmer1 <- lm(LA_log ~ 1, data = categorydata, weights=rel)
    glmer3 <- lmer(LA_log ~ 1|Comunidad, data = categorydata, weights = rel)
    AIC(glmer1, glmer3)
    r.squaredGLMM(glmer3)
    if (AIC(glmer1, glmer3)[1,2] > AIC(glmer1, glmer3)[2,2]) {
      r.trait[i] <- r.squaredGLMM(glmer3)[1,2]
    } else {
      r.trait[i] <- 0
    }
  }
}
c.la <- r.trait

# LT
traits_com_leaf_LT <- traits_com_leaf[!is.na(traits_com_leaf$GF_mean),]
r.trait <- NULL
for (i in unique(traits_com_leaf_LT$Subcategory)) {
  categorydata <- traits_com_leaf_LT[traits_com_leaf_LT$Subcategory==i,]
  categorydata <- categorydata %>%  group_by(Comunidad) %>%  filter(n_distinct(GF_mean) >= 3)
  if(nrow(categorydata)<=10){
    r.trait[i] <- 0
  } else {
    glmer1 <- lm(GF_mean ~ 1, data = categorydata, weights=rel)
    glmer3 <- lmer(GF_mean ~ 1|Comunidad, data = categorydata, weights = rel)
    AIC(glmer1, glmer3)
    r.squaredGLMM(glmer3)
    if (AIC(glmer1, glmer3)[1,2] > AIC(glmer1, glmer3)[2,2]) {
      r.trait[i] <- r.squaredGLMM(glmer3)[1,2]
    } else {
      r.trait[i] <- 0
    }
  }
}
c.lt <- r.trait

# 3. Fruit
traits_com <- traits_com_2
fruit_part <- c(traits_com$Part=="FRUIT" | traits_com$Part=="FLOWER"| traits_com$Part=="SEED" | 
                  traits_com$Part=="FRUIT_SEED" | traits_com$Part=="INFRUTESCENCE"| traits_com$Part=="INFLORESCENCE")
traits_fruit <- traits_com[fruit_part,]
traits_fruit <- traits_fruit[!duplicated(traits_fruit),]

# Fruit mass
traits_fruit_SM <- traits_fruit[!is.na(traits_fruit$f_mass),]

r.trait <- NULL
for (i in unique(traits_fruit_SM$Subcategory)) {
  categorydata <- traits_fruit_SM[traits_fruit_SM$Subcategory==i,]
  categorydata <- categorydata %>%  group_by(Comunidad) %>%  filter(n_distinct(f_mass) >= 3)
  if(nrow(categorydata)<=10){
    r.trait[i] <- 0
  } else {
    glmer1 <- lm(f_mass ~ 1, data = categorydata, weights=rel)
    glmer3 <- lmer(f_mass ~ 1|Comunidad, data = categorydata, weights = rel)
    AIC(glmer1, glmer3)
    r.squaredGLMM(glmer3)
    if (AIC(glmer1, glmer3)[1,2] > AIC(glmer1, glmer3)[2,2]) {
      r.trait[i] <- r.squaredGLMM(glmer3)[1,2]
    } else {
      r.trait[i] <- 0
    }
  }
}
c.mass <- r.trait

# Fleshy

r.trait <- NULL
for (i in unique(traits_fruit_SM$Subcategory)) {
  categorydata_1 <- traits_fruit_SM[traits_fruit_SM$Subcategory==i,]
  sum_count <- categorydata_1  %>% 
    filter(f_fleshy==1) %>% 
    group_by(Comunidad) %>% 
    tally()  %>% 
    filter (n >= 3)
  categorydata <- categorydata_1 %>%  filter(Comunidad %in% sum_count$Comunidad) 
  if(nrow(sum_count)<=1){
    r.trait[i] <- 0
  } else {
    glmer1 <- glm(f_fleshy ~ 1, data = categorydata, family=binomial,  weights=rel)
    glmer3 <- glmer(f_fleshy ~ 1|Comunidad, data = categorydata, family=binomial,  weights=rel)
    AIC(glmer1, glmer3)
    r.squaredGLMM(glmer3)
    if (AIC(glmer1, glmer3)[1,2] > AIC(glmer1, glmer3)[2,2]) {
      r.trait[i] <- r.squaredGLMM(glmer3)[1,2]
    } else {
      r.trait[i] <- 0
    }
  }
}
c.fleshy <- r.trait

# 4. All
traits_com <- traits_com_2

# DBH
traits_com_DBH <- traits_com[!is.na(traits_com$DBH_max ),]
r.trait <- NULL
for (i in unique(traits_com_DBH$Subcategory)) {
  categorydata <- traits_com_DBH[traits_com_DBH$Subcategory==i,]
  categorydata <- categorydata %>%  group_by(Comunidad) %>%  filter(n_distinct(Species) >= 3)
  if(nrow(categorydata)<=10){
    r.trait[i] <- 0
  } else {
    glmer1 <- lm(f_mass ~ 1, data = categorydata, weights=rel)
    glmer3 <- lmer(f_mass ~ 1|Comunidad, data = categorydata, weights = rel)
    AIC(glmer1, glmer3)
    r.squaredGLMM(glmer3)
    if (AIC(glmer1, glmer3)[1,2] > AIC(glmer1, glmer3)[2,2]) {
      r.trait[i] <- r.squaredGLMM(glmer3)[1,2]
    } else {
      r.trait[i] <- 0
    }
  }
}
c.dbh <- r.trait

# Latex
traits_com_LX <- traits_com[!is.na(traits_com$NEW_latex ),]
r.trait <- NULL
for (i in unique(traits_com_LX$Subcategory)) {
  categorydata_1 <- traits_com_LX[traits_com_LX$Subcategory==i,]
  sum_count <- categorydata_1  %>% 
    filter(NEW_latex==1) %>% 
    group_by(Comunidad) %>% 
    tally()  %>% 
    filter (n >= 3)
  categorydata <- categorydata_1 %>%  filter(Comunidad %in% sum_count$Comunidad) 
  if(nrow(sum_count)<=1){
    r.trait[i] <- 0
  } else {
    glmer1 <- glm(NEW_latex ~ 1, data = categorydata, weights=rel,  family=binomial)
    glmer3 <- glmer(NEW_latex ~ 1|Comunidad, data = categorydata, weights = rel,  family=binomial)
    AIC(glmer1, glmer3)
    r.squaredGLMM(glmer3)
    if (AIC(glmer1, glmer3)[1,2] > AIC(glmer1, glmer3)[2,2]) {
      r.trait[i] <- r.squaredGLMM(glmer3)[1,2]
    } else {
      r.trait[i] <- 0
    }
  }
}
c.latex <- r.trait

# Resin
traits_com <- traits_com_2
traits_com_RS <- traits_com[!is.na(traits_com$NEW_resin ),]
r.trait <- NULL
for (i in unique(traits_com_RS$Subcategory)) {
  categorydata_1 <- traits_com_RS[traits_com_RS$Subcategory==i,]
  sum_count <- categorydata_1  %>% 
    filter(NEW_resin==1) %>% 
    group_by(Comunidad) %>% 
    tally()  %>% 
    filter (n >= 3)
  categorydata <- categorydata_1 %>%  filter(Comunidad %in% sum_count$Comunidad) 
  if(nrow(sum_count)<=1){
    r.trait[i] <- 0
  } else {
    glmer1 <- glm(NEW_resin ~ 1, data = categorydata, weights=rel,  family=binomial)
    glmer3 <- glmer(NEW_resin ~ 1|Comunidad, data = categorydata, weights = rel,  family=binomial)
    AIC(glmer1, glmer3)
    r.squaredGLMM(glmer3)
    if (AIC(glmer1, glmer3)[1,2] > AIC(glmer1, glmer3)[2,2]) {
      r.trait[i] <- r.squaredGLMM(glmer3)[1,2]
    } else {
      r.trait[i] <- 0
    }
  }
}
c.resin <- r.trait

# Tree
traits_com <- traits_com_2
r.trait <- rep(NULL)
traits_com_TR <- traits_com[!is.na(traits_com$Tree),]
for (i in unique(traits_com_TR$Subcategory)) {
  categorydata_1 <- traits_com_TR[traits_com_TR$Subcategory==i,]
  sum_count <- categorydata_1  %>% 
    filter(Tree==1) %>% 
    group_by(Comunidad) %>% 
    tally()  %>% 
    filter (n >= 3)
  categorydata <- categorydata_1 %>%  filter(Comunidad %in% sum_count$Comunidad) 
  if(nrow(sum_count)<=1){
    r.trait[i] <- 0
  } else {
    glmer1 <- glm(Tree ~ 1, data = categorydata, weights=rel,  family=binomial)
    glmer3 <- glmer(Tree ~ 1|Comunidad, data = categorydata, weights = rel,  family=binomial)
    AIC(glmer1, glmer3)
    r.squaredGLMM(glmer3)
    if (AIC(glmer1, glmer3)[1,2] > AIC(glmer1, glmer3)[2,2]) {
      r.trait[i] <- r.squaredGLMM(glmer3)[1,2]
    } else {
      r.trait[i] <- 0
    }
  }
}
c.tree <- r.trait

# Palmera
traits_com <- traits_com_2
r.trait <- rep(NULL)
traits_com_PL <- traits_com[!is.na(traits_com$Palmera),]
for (i in unique(traits_com_PL$Subcategory)) {
  categorydata_1 <- traits_com_PL[traits_com_PL$Subcategory==i,]
  sum_count <- categorydata_1  %>% 
    filter(Palmera==1) %>% 
    group_by(Comunidad) %>% 
    tally()  %>% 
    filter (n >= 3)
  categorydata <- categorydata_1 %>%  filter(Comunidad %in% sum_count$Comunidad) 
  if(nrow(sum_count)<=1){
    r.trait[i] <- 0
  } else {
    glmer1 <- glm(Palmera ~ 1, data = categorydata, weights=rel,  family=binomial)
    glmer3 <- glmer(Palmera ~ 1|Comunidad, data = categorydata, weights = rel,  family=binomial)
    AIC(glmer1, glmer3)
    r.squaredGLMM(glmer3)
    if (AIC(glmer1, glmer3)[1,2] > AIC(glmer1, glmer3)[2,2]) {
      r.trait[i] <- r.squaredGLMM(glmer3)[1,2]
    } else {
      r.trait[i] <- 0
    }
  }
}
c.palm <- r.trait

# Liana
traits_com <- traits_com_2
r.trait <- rep(NULL)
traits_com_LN <- traits_com[!is.na(traits_com$Liana),]
for (i in unique(traits_com_LN$Subcategory)) {
  categorydata_1 <- traits_com_LN[traits_com_LN$Subcategory==i,]
  sum_count <- categorydata_1  %>% 
    filter(Liana==1) %>% 
    group_by(Comunidad) %>% 
    tally()  %>% 
    filter (n >= 3)
  categorydata <- categorydata_1 %>%  filter(Comunidad %in% sum_count$Comunidad) 
  if(nrow(sum_count)<=1){
    r.trait[i] <- 0
  } else {
    glmer1 <- glm(Liana ~ 1, data = categorydata, weights=rel,  family=binomial)
    glmer3 <- glmer(Liana ~ 1|Comunidad, data = categorydata, weights = rel,  family=binomial)
    AIC(glmer1, glmer3)
    r.squaredGLMM(glmer3)
    if (AIC(glmer1, glmer3)[1,2] > AIC(glmer1, glmer3)[2,2]) {
      r.trait[i] <- r.squaredGLMM(glmer3)[1,2]
    } else {
      r.trait[i] <- 0
    }
  }
}
c.liana <- r.trait

# Hemiepiphyte

traits_com <- traits_com_2
r.trait <- rep(NULL)
traits_com_HM <- traits_com[!is.na(traits_com$Hemiepiphyte),]
for (i in unique(traits_com_HM$Subcategory)) {
  categorydata_1 <- traits_com_HM[traits_com_HM$Subcategory==i,]
  sum_count <- categorydata_1  %>% 
    filter(Hemiepiphyte==1) %>% 
    group_by(Comunidad) %>% 
    tally()  %>% 
    filter (n >= 3)
  categorydata <- categorydata_1 %>%  filter(Comunidad %in% sum_count$Comunidad) 
  if(nrow(sum_count)<=1){
    r.trait[i] <- 0
  } else {
    glmer1 <- glm(Hemiepiphyte ~ 1, data = categorydata, weights=rel,  family=binomial)
    glmer3 <- glmer(Hemiepiphyte ~ 1|Comunidad, data = categorydata, weights = rel,  family=binomial)
    AIC(glmer1, glmer3)
    r.squaredGLMM(glmer3)
    if (AIC(glmer1, glmer3)[1,2] > AIC(glmer1, glmer3)[2,2]) {
      r.trait[i] <- r.squaredGLMM(glmer3)[1,2]
    } else {
      r.trait[i] <- 0
    }
  }
}
c.hemi <- r.trait

for (i in unique(traits_com_HM$Subcategory)) {
  categorydata_1 <- traits_com_HM[traits_com_HM$Subcategory==i,]
  if(nrow(categorydata)<=10){
    r.trait[i] <- 0
  } else {
    glmer1 <- glm(Hemiepiphyte ~ 1, data = categorydata, weights=rel,  family=binomial)
    glmer3 <- glmer(Hemiepiphyte ~ 1|Comunidad, data = categorydata, weights = rel,  family=binomial)
    AIC(glmer1, glmer3)
    r.squaredGLMM(glmer3)
    if (AIC(glmer1, glmer3)[1,2] > AIC(glmer1, glmer3)[2,2]) {
      r.trait[i] <- r.squaredGLMM(glmer3)[1,2]
    } else {
      r.trait[i] <- 0
    }
  }
}
c.hemi <- r.trait


# >Collect results

# Trait-service
list_all<-list(c.la, c.sla, c.lt, c.wd, c.dbh, c.mass, c.fleshy,  c.latex, c.resin, c.tree, c.palm, c.liana, c.hemi)
names(list_all) <-c("LA","SLA", "LT", "WD", "DBH", "SEEDMASS", "FLESHY", "LATEX", "RESIN", "TREE", "PALM", "LIANA", "HEMIEPIPHYTE")
list_data <- map(list_all, data.frame)
list_data <- map(list_data, rownames_to_column)
data_r <- do.call(rbind.data.frame, list_data)
colnames(data_r) <- c("Category", "R2")
data_r$R2 <- round(data_r$R2, 3)
data_r <- rownames_to_column(data_r)
data_r$rowname<-gsub("\\.[0-9]+","",data_r$rowname)

# All data
ALL1 <- ALL
ALL1 <- as.data.frame(cbind(ALL,rep("ALL", 13)))
ALL1 <- rownames_to_column(ALL1)
ALL1 <- ALL1[,c(1,3,2)]
colnames(ALL1) <- c("rowname", "Category", "R2")
data_r_all <- rbind(data_r, ALL1)
data_r_all$R2 <- as.numeric(data_r_all$R2)
data_r_all[data_r_all$R2==0,] <- NA
data_r_all <- data_r_all %>% drop_na(rowname) %>% filter(!Category=="NO USE")

# Prepare to plot
ORDER <- c("WD","DBH", "LA","SLA","LT","SEEDMASS","FLESHY" ,"LATEX","RESIN","TREE","PALM","LIANA","HEMIEPIPHYTE" )
ORDER2 <- sort(unique(data_r_all$Category))

# Plot figure 6.3.
colorss <-  met.brewer("VanGogh3", 10)[1:10]
Figure6.3 <- ggballoonplot(data_r_all, color = "R2", fill = "R2") +scale_color_gradientn(colors = colorss, guide="none")+
  scale_fill_gradientn(colors = colorss)+scale_x_discrete(limits = ORDER)+scale_y_discrete(limits=ORDER2) + guides(col="none")

Figure6.3
ggsave("r2_trait_2023.png", Figure6.3, width = 8,height = 8)
ggsave("r2_trait_2023.svg", Figure6.3, width = 8,height = 8)



