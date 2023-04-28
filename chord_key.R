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
library(ggeffects)
library(lme4)
library(glmmTMB)
library(aods3)
library(performance)
# Viz
library(patchwork)
library(randomcoloR)
library(RColorBrewer)
library(colorspace)
library(ggthemes)
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





# Chordiagram 
df_etno <- etno %>% drop_na(Plot)

especie2 <- unique(as.data.frame (cbind(df_etno$Comunidad, df_etno$Species, df_etno$Category2, df_etno$Subcategory, df_etno$Use)))
colnames(especie2) <- c("Comunidad", "Species", "Category", "Subcategory", "Use")
keystones <- especie2 %>% group_by(Comunidad, Species ) %>% filter (Category=="CULTURAL") %>% summarise(Use_number=n_distinct(Use))
keystones %>% filter(max(Use_number))


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


#mat7 <- ComGenR::rel(mat6, rel.type = "sum")*100
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
