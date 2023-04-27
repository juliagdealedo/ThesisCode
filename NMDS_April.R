# NMDS try

install.packages("grateful")
library(grateful)
cite_packages(out.dir = ".")     
cite_packages(out.format = "docx", out.dir = ".")
pks <- scan_packages()
get_pkgs_info()
out.dir <- setwd("/Users/juliag.dealedo/ONE/UAM_Doctorado/Capitulos")
get_pkgs_info(pkgs = pks$pkg, out.dir = getwd())
citation()
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
library(MetBrewer)


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
grid.col = c( "San Carlos" = "#FF7F00", Dicaro="#B2DF8A", Guiyero="#33A02C", Infierno = "#CB6856",Macahua = "#6A3D9A",
             "Nueva Vida" = "#FDBF6F", Bolivar = "#1F78B4", Tumupasa = "#CAB2D6", Yamino = "#A6CEE3")
cols <- grid.col[resu$Comunidad]
unique(cols)
# pal <- c("#6A3D9A", "#CAB2D6", "#CB6856" ,"#1F78B4", "#A6CEE3" ,"#FF7F00", "#FDBF6F" ,"#B2DF8A", "#33A02C")
# labels <- c("Macahua", "Tumupasa", "Infierno", "Bolivar", "Yamino", "San Carlos", "Nueva Vida", "Dicaro", "Guiyero")
# cols <- pal[as.factor(resu$Comunidad)]

# Plot NMDS
# dev.off()
# setwd("/Users/juliag.dealedo/ONE/UAM_Doctorado/Capitulos/cap2/figs/figs_2023")
# svg("nmds_3.svg")
#pdf("nmds_3.pdf", height=8,width=8, pointsize=12)
comp_dist <- reshape2::dcast(comp, Plot~Species, value.var="Species", fill=0)
comp_dist <- column_to_rownames(comp_dist, var="Plot")
abs_comp_dist <- as.matrix(comp_dist>0)*1

set.seed(100)
useMDS <-metaMDS(use_dist, distance="bray", k=2, trymax= 1000) ##k is the number of dimensions
compMDS <-metaMDS(abs_comp_dist, distance="jaccard", k=2, trymax= 1000, previous.best = useMDS) ##k is the number of dimensions

grid.col = c( "Bolivar" = "#1F78B4", Dicaro="#B2DF8A", Guiyero="#33A02C", Infierno = "#CB6856",Macahua = "#6A3D9A",
              "Nueva Vida" = "#FDBF6F", "San Carlos" = "#FF7F00", Tumupasa = "#CAB2D6", Yamino = "#A6CEE3")
cols <- grid.col[resu$Comunidad]
unique(cols)


setwd("/Users/juliag.dealedo/ONE/UAM_Doctorado/Capitulos/cap2/figs/figs_2023")
svg("nmds_3.svg", height=8,width=13, pointsize=15)
#pdf("nmds_3.pdf", height=8,width=8, pointsize=12)
par(mfcol=c(1,2))
plot(useMDS$points, pch=21, cex=1.5, col="white",bg ="white", cex.axis=1, cex.lab=1 ,xlab="NMDS1", ylab="NMDS2", main="Knowledge")
#legend("topleft", cex=0.8, pch=16, bg="black", col=unique(cols), legend=labels, bty="n", inset = c(0, 0))
points(useMDS$points, pch=21, cex=1.3, col="black", bg = alpha(cols,0.9))
ordihull(useMDS, resu$Comunidad, col=grid.col, draw="polygon", border="white")
plot(compMDS$points, pch=21, cex=1.5, yaxt='n', ann=FALSE, col="white",bg ="white", cex.axis=1, cex.lab=1 ,xlab="NMDS1", ylab="NMDS2", main="Floristic")
points(compMDS$points, pch=21, cex=1.3, col="black", bg = alpha(cols,0.9))
ordihull(compMDS, resu$Comunidad, col=grid.col, draw="polygon", border="white")
dev.off()



View(resu)


# 
# plot(useMDS$points, pch=21, cex=1.5, col="white",bg ="white", cex.axis=1, cex.lab=1 ,xlab="NMDS1", ylab="NMDS2", main="Knowledge")
# legend("topleft", cex=0.8, pch=16, bg="black", col=pal, legend=labels, bty="n", inset = c(0, 0))
# points(useMDS$points, pch=21, cex=1.3, col="black", bg = alpha(cols,0.9))
# ordihull(useMDS, resu$Comunidad, col=grid.col, draw="polygon", border="white", label = TRUE)
# #orditorp(useMDS,display="sites",cex=1,air=0.01)
# points(compMDS$points, pch=21, cex=1.3, col="black", bg =  alpha(cols,0.9))
# ordihull(compMDS, resu$Comunidad, col=grid.col, draw="polygon", border="black")
# orditorp(compMDS,display="sites",cex=1.25,air=0.01)
centro_use <- as.data.frame(t(summary(ordihull(useMDS, resu$Comunidad))))
centro_comp <-as.data.frame(t(summary(ordihull(compMDS, resu$Comunidad))))
#comp_euc <- melt(dist(centroids[,1:2], method = "euclidean", diag = F))

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
df_4$group <- paste(df_4$Community1, df_4$Community2, sep="-")
df_4 <- df_4 %>% mutate (value_fill= ifelse(subtr<0, "Cultural", "Floristic"))


bar_predominance <- ggplot(df_4, aes(x=subtr, y=reorder(group,subtr), fill=value_fill))+
  geom_bar(stat='identity')+
  scale_fill_manual(values=c('#E69F00', '#299617'), name="Difference by:", labels = c("More cultural distance than floristic", "More floristic distance than cultural"))+
  #theme_classic()+
  theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
        panel.background = element_blank(),
        legend.position = c(0.6, 0.2))+
  labs(x = "Difference between cultural and floristic distance", title="Predominant floristic or cultural distance among communities")

bar_predominance 

pal <- brewer.pal(10, "Paired")
pal <- c( "#1F78B4", "#B2DF8A", "#33A02C",  "#CB6856","#6A3D9A", "#FDBF6F", "#FF7F00", "#CAB2D6", "#A6CEE3")

grid.col = c( SanCarlos = "#FF7F00", Dicaro="#B2DF8A", Guiyero="#33A02C", Infierno = "#CB6856",Macahua = "#6A3D9A",
              NuevaVida = "#FDBF6F", Bolivar = "#1F78B4", Tumupasa = "#CAB2D6", Yamino = "#A6CEE3")

df_new <- data.frame(Com1=df_4$Community1,
                      Com2=df_4$Community2,
                      group=df_4$group,
                      subtr=df_4$subtr,
                      Pair1=1,
                      Pair2=2)



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
ggsave ("predominant.svg",height=10,width=8)



