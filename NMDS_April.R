# NMDS try

require(vegan)
data(dune)
set.seed(123)
sol <- metaMDS(dune)
sol2 <- metaMDS(dune, previous.best = sol)

?metaMDS


#Otra opción más sencilla sería poner como puntos de partida para el semilleo en el NMDS florístico 
#las coordenadas de los puntos en el NMDS de distancias culturales. 
#De esta manera ya estás estableciendo el punto de partida y cualquier alejamiento entre las distancias culturales y 
#florísticas sería menos arbitraria y más justificada. 
#Aunque honestamente nunca he hecho esto, creo que debería ser relativamente fácil. 
#Creo que si metes el argumento 'previous.best=NMDS1' en la función 'metaMDS' cuando hagas el NMDS florístico 
#(dónde NMDS1 sería el objeto metaMDS con las distancias culturales) debería funcionar sin problema. 
#Con esto creo que sería suficiente. Mira a ver si los resultados te salen igual.



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
library(peRReo)
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




# 1. Knowledge distribution over among communities #
## 1.1. Does community have an effect in the difference in knowledge between plots?
# Test with permanova
use_dist <- reshape2::dcast(df_etno4, Plot~Use, value.var="Species", fill=0)
use_dist <- column_to_rownames(use_dist, "Plot")
use_dist_bray <- vegdist(use_dist, method="bray")

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
# dev.off()
# setwd("/Users/juliag.dealedo/ONE/UAM_Doctorado/Capitulos/cap2/figs/figs_2023")
# svg("nmds_3.svg")
#pdf("nmds_3.pdf", height=8,width=8, pointsize=12)

set.seed(123)
useMDS <-metaMDS(use_dist, distance="bray", k=2, trymax= 1000, autotransform=TRUE) ##k is the number of dimensions

pointuse <- as.data.frame(useMDS$points)
pointuse$MDS1

compMDS <-metaMDS(abs_comp_dist, distance="bray", k=2, trymax= 1000, autotransform=TRUE) ##k is the number of dimensions

compare <-metaMDS(abs_comp_dist, distance="bray", k=2, trymax= 1000, autotransform=TRUE, previous.best = useMDS) ##k is the number of dimensions

useMDS <- MDSrotate(useMDS, vec=resu$Latitud)
compMDS <- MDSrotate(compMDS, vec=resu$Latitud)
compareMDS <- MDSrotate(compare, vec=resu$Latitud)


par(mfcol=c(1,2))
plot(useMDS$points, pch=21, cex=1.5, col="white",bg ="white", cex.axis=1, cex.lab=1 ,xlab="NMDS1", ylab="NMDS2", main="Knowledge")
#plot(useMDS$points, pch=21, cex=1.5, col="white",bg ="white", cex.axis=1, cex.lab=1 ,xlab="NMDS1", ylab="NMDS2")
legend("topleft", cex=0.8, pch=16, bg="black", col=unique(cols), legend=unique(resu$Comunidad), bty="n", inset = c(0, 0))
points(useMDS$points, pch=21, cex=1.3, col="black", bg = alpha(cols,0.9))
ordihull(useMDS, resu$Comunidad, col=grid.col, draw="polygon", border="white")
#take <- as.numeric(rownames(forest.10))
#orditorp(useMDS, "sp", select = take, cex=.6, col="black", air=0.1)
#points(useMDS$points, pch=21, cex=1.3, col="black", bg = alpha(cols,0.9))
plot(compMDS$points, pch=21, cex=1.5, col="white",bg ="white", cex.axis=1, cex.lab=1 ,xlab="NMDS1", ylab="NMDS2", main="Floristic")
points(compMDS$points, pch=21, cex=1.3, col="black", bg = alpha(cols,0.9))
ordihull(compMDS, resu$Comunidad, col=grid.col, draw="polygon", border="white")
#orditorp (compMDS, display="sites", select=T)
dev.off()

plot(compareMDS$points, pch=21, cex=1.5, col="white",bg ="white", cex.axis=1, cex.lab=1 ,xlab="NMDS1", ylab="NMDS2", main="Floristic")
points(compareMDS$points, pch=21, cex=1.3, col="black", bg = alpha(cols,0.9))
ordihull(compareMDS, resu$Comunidad, col=grid.col, draw="polygon", border="white")










comp_dist <- reshape2::dcast(comp, Plot~Species, value.var="Species", fill=0)
comp_dist <- column_to_rownames(comp_dist, var="Plot")
abs_comp_dist <- as.matrix(comp_dist>0)*1
centroids <- t(as.data.frame(summary(ordihull(useMDS, resu$Comunidad))))
use_euc <- melt(as.matrix(dist(centroids[,1:2], method = "euclidean")))

set.seed(123)
useMDS <-metaMDS(use_dist, distance="bray", k=2, trymax= 1000, autotransform=TRUE) 
plot(useMDS$points, pch=21, cex=1.5, col="white",bg ="white", cex.axis=1, cex.lab=1, main="Knowledge")
ordihull(useMDS, resu$Comunidad, col=grid.col, draw="polygon", border="white")

set.seed(123)
compareMDS <-metaMDS(abs_comp_dist, distance="bray", k=2, trymax= 1000, autotransform=TRUE, previous.best = useMDS)
plot(compareMDS$points, pch=21, cex=1.5, col="white",bg ="white", cex.axis=1, cex.lab=1, main="Compare")
ordihull(compareMDS, resu$Comunidad, col=grid.col, draw="polygon", border="white")


plot(centroids$NMDS1, centroids$NMDS2)
plot(centroids$NMDS1, centroids$NMDS2)

centro_use <- as.data.frame(t(summary(ordihull(useMDS, resu$Comunidad))))
centro_comp <-as.data.frame(t(summary(ordihull(compare, resu$Comunidad))))
#comp_euc <- melt(dist(centroids[,1:2], method = "euclidean", diag = F))
mat_comp <- as.matrix(dist(centro_comp[,1:2], method = "euclidean", diag = F))
mat_use <- as.matrix(dist(centro_use[,1:2], method = "euclidean", diag = F))
mat <- mat_comp-mat_use
df <- data.frame(which(!is.na(mat), arr.ind = TRUE))
df_2 <- melt(mat)[,1:2]
df_3 <- cbind(df, df_2)
df_colours <- df_3[df_3$row <= df_3$col, ]
df_4 <- df_3[df_3$row <= df_3$col, ]
df_4$value <- mat[cbind(df_4$row, df_4$col)]
df_4 <- df_4[,3:5]
colnames(df_4) <- c("Community1", "Community2", "subtr")
df_4$group <- paste(df_4$Community1, df_4$Community2, sep="-")
df_4 <- df_4 %>% mutate (value_fill= ifelse(subtr<0, "Cultural", "Floristic"))

# to_subs <- cbind(comp_euc, use_euc[,3])
# colnames(to_subs) <- c("Community1", "Community2", "comp", "use")
# to_subs$subtr <- to_subs$comp - to_subs$use
# to_subs$group <- paste(to_subs$Community1, to_subs$Community2, sep="-")

ggplot(df_4, aes(x=subtr, y=reorder(group,subtr), fill=value_fill))+
  geom_bar(stat='identity')+
  scale_fill_manual(values=c('#E69F00', '#299617'), name="Difference by:", labels = c("More cultural distance than floristic", "More floristic distance than cultural"))+
  theme_classic()+
  labs(title="Predominant floristic or cultural distance among communities", 
       y="Community pairs", x = "Difference between cultural and floristic distance")
ggsave ("predominant.svg")










## locuras varias no consguido


df <- data.frame(x = sample(c("A", "B", "C"), 20, replace = TRUE),
                 y = sample(c("D", "E", "F"), 20, replace = TRUE))

# plot using ggplot2
df_4
pal <- brewer.pal(10, "Paired")
pal <- c( "#1F78B4", "#B2DF8A", "#33A02C",  "#CB6856","#6A3D9A", "#FDBF6F", "#FF7F00", "#CAB2D6", "#A6CEE3")

df_colours$group <- paste(df_colours$Var1, df_colours$Var2, sep="-")


df_colours %>% 
  ggplot(aes(x = row, fill=Var1)) +
  geom_bar()

ggplot(df_colours, aes(fill=Var2, y=row, x=col)) + 
  geom_bar(position="stack", stat="identity")



ggplot(df_colours, aes(x=Var1,y=Var2, fill = Var1, col=Var1)) +
  geom_tile(aes(x=Var1,y=Var2,fill = group, col=Var1), size = 0.5) +
 # scale_fill_manual(values = pal)+
 #  scale_color_manual(values = pal)+
  theme_classic()


library(ggplot2)

# Create a sample dataframe
df <- data.frame(names = c("John", "Mary", "David", "Sarah"),
                 other_names = c("Bob", "Jane", "Karen", "Tom"))

# Define colors for each name
name_colors <- c("John" = "red", "Mary" = "green", "David" = "blue", "Sarah" = "purple")

# Plot the data using ggplot and geom_point
ggplot(data = df, aes(x = names, y = other_names)) + 
  geom_point(size = 10, aes(color = names), shape = 15) +
  scale_color_manual(values = name_colors)




ggsave("try.svg")
library(ggplot2)

# Create a sample dataframe
df <- data.frame(names = c("John", "Mary", "David", "Sarah"),
                 other_names = c("Bob", "Jane", "Karen", "Tom"))

# Define colors for each name
name_colors <- c("John" = "red", "Mary" = "green", "David" = "blue", "Sarah" = "purple")

ggplot(data = df, aes(x = names, y = other_names)) + 
  geom_point(size = 10, aes(color = names), shape = 15) +
  scale_color_manual(values = name_colors)


df <- data.frame(names = c("John", "Mary", "John", "Mary", "David", "Sarah"),
                 other_names = c("1", "1", "2", "2","2", "2"))

ggplot(data = df, aes(x = other_names, y = names)) + 
  geom_point(size = 10, aes(color = names), shape = 15) +
  scale_color_manual(values = name_colors)

df_colours <- df_3[df_3$row <= df_3$col, ]
df_colours$names <- paste(df_colours$Var1, df_colours$Var2, sep = "-")
df_4
ggplot(data = df_4, aes(y = group, x=1:2)) + 
  geom_tile(data=df_4, size = 10, aes(x=1:2, y = group, color = Community1)) +
  geom_tile(data=df_4, size = 10, aes( x=1:2, y = group, color = Community2)) +
  scale_color_manual(values = pal)



df_colours$row <- rep(1,length(df_colours$Var1))
df_colours$col <- rep(2,length(df_colours$Var1))
df_colours <- df_colours
df_melt <- melt(df_colours,  value.name = "row")
df_melt$variable <- as.character(df_melt$variable)
str(df_melt)
df_colours$row <- rep("pair1",length(df_colours$Var1))
df_colours$col <- rep("pair2",length(df_colours$Var1))

ordenation<-as.vector(arrange(df_4, -subtr)$group)
# Plot the data using ggplot and geom_point
ggplot(data = df_melt, aes(x = variable, y = reorder(names,ordenation))) + 
  geom_tile(data=df_colours, size = 10, aes(x = row, y = names, color = Var1)) +
  geom_tile(data=df_colours, size = 10, aes(x = col, y = names, color = Var2)) +
  scale_color_manual(values = pal)

library(ggplot2)

# Create a sample dataframe
df <- data.frame(name1 = c("John", "Mary", "David", "Sarah"),
                 name2 = c("Bob", "Jane", "Karen", "Tom"))

# Define colors for each name
name_colors <- c("John" = "red", "Mary" = "green", "David" = "blue", "Sarah" = "purple",
                 "Bob" = "orange", "Jane" = "pink", "Karen" = "gray", "Tom" = "brown")

# Create a new column with the concatenated names
df$names <- paste(df$name1, df$name2, sep = "-")

# Plot the data using ggplot and geom_point
ggplot(data = df, aes(x = 1, y = names)) + 
  geom_point(size = 10, aes(color = name1, fill = name1), shape = 15) +
  scale_color_manual(values = name_colors) +
  scale_fill_manual(values = name_colors) +
  theme_void() +
  theme(legend.position = "none")






