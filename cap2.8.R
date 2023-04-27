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
useMDS <-metaMDS(use_dist, distance="bray", k=2, trymax= 1000, autotransform=TRUE) ##k is the number of dimensions


comp_dist <- reshape2::dcast(comp, Plot~Species, value.var="Species", fill=0)
comp_dist <- column_to_rownames(comp_dist, var="Plot")
abs_comp_dist <- as.matrix(comp_dist>0)*1

compMDS <-metaMDS(abs_comp_dist, distance="bray", k=2, trymax= 1000, autotransform=TRUE) ##k is the number of dimensions


useMDS <- MDSrotate(useMDS, vec=resu$Latitud)
compMDS <- MDSrotate(compMDS, vec=resu$Latitud)

pdf("nmds_double.pdf", height=8,width=16, pointsize=12)

par(mfcol=c(1,2))
plot(compMDS$points, pch=21, cex=1.5, col="white",bg ="white", cex.axis=1, cex.lab=1 ,xlab="NMDS1", ylab="NMDS2", main="Knowledge")
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

# NMDS COORDINATES where species are going to be plotted
axesfi <- axes
axes <- envfit(useMDS, use_dist) 
res <- scores(axes, "vectors") #vegan
res <- as.data.frame(res)
res <- rownames_to_column (res, var="Species")
# Correlation into table. $vector are the CORRELATIONS of each species with the axes
axes.table <- data.frame((axes$vectors)$arrows, (axes$vectors)$r, (axes$vectors)$pvals) # Convert result into table
axes.table <- rownames_to_column(axes.table)
colnames(axes.table) <- c("Species", "NMDS1", "NMDS2", "r", "p")
cultural <- axes.table %>%filter(str_detect(Species, "CONSTRUCTION"))
forest.10 <- subset(axes.table, Species %in% cultural$Species)


# DOMINANCE filter to choose species
abu <- arrange(as.data.frame(colSums(matrusp)),  desc(colSums(matrusp)))
abu <- as.data.frame(colSums(matrusp))
abu <- rownames_to_column(abu)
colnames(abu) <- c("Species", "abundance")
abu <- arrange(abu, desc(abundance))
abu <- head(abu, 10)
forest.10 <- subset(axes.table, Species %in% abu$Species)
forest.10 <- (cbind(forest.10[,1], round(forest.10[c(2:5)],3))) 
colnames(forest.10) <- c("Species", "NMDS1", "NMDS2", "r", "p")
u <- qflextable(forest.10)
TableSpeciestotalfi <- bold(u, i= ~ p < 0.05, bold=T, part="body") # Table 2b
forest.10fi <- forest.10



dev.off()
scrs <- scores(useMDS, display = 'sites')
scrs <- cbind(as.data.frame(scrs), Comunidad = resu$Comunidad)
cent_etno <- aggregate(cbind(NMDS1, NMDS2) ~ Comunidad, data = scrs, FUN = mean)







scrs <- scores(compMDS, display = 'sites')
scrs <- cbind(as.data.frame(scrs), Comunidad = resu$Comunidad)
cent_comp <- aggregate(cbind(NMDS1, NMDS2) ~ Comunidad, data = scrs, FUN = mean)

ggplot(data=cent_etno, aes(x=NMDS1, y=NMDS2, col=Comunidad))+
  geom_point(size=5)+ theme_classic()+
  geom_point(data=cent_comp, aes(x=NMDS1, y=NMDS2, col=Comunidad), shape=17, size=5)+ theme_classic()

plot(cent_comp$NMDS1, cent_comp$NMDS2)




scrs_uses <- scores(useMDS, display = 'species')




## 1.2. How many species are destined to each category per community?

# Chordiagram 
df_etno <- etno %>% drop_na(Plot)
df_etno$Plot <- as.numeric (df_etno$Plot)

especie2 <- unique(as.data.frame (cbind(df_etno$Plot,df_etno$Comunidad, df_etno$Species, df_etno$Category2, df_etno$Subcategory)))
colnames(especie2) <- c("Plot", "Comunidad", "Species", "Category", "Subcategory")

especie2$Plot <- as.numeric (especie2$Plot)



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



##############################
##############################
##############################
##############################
##############################
##############################
##############################
##############################
##############################
##############################
##############################
##############################




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



prds <- ggpredict(glmer3, terms=c("Species_number", "Community"), type = "random", ci.lvl = 0.10) 

pal <- brewer.pal(10, "Paired")
pal <- c( "#1F78B4", "#B2DF8A", "#33A02C",  "#CB6856","#6A3D9A", "#FDBF6F", "#FF7F00", "#CAB2D6", "#A6CEE3")


total_alpha <- ggpredict(glmer3, terms=c("Species_number", "Community"), type = "random", ci.lvl = 0.10) %>% 
  plot(add.data=T, ci=F, limit.range = TRUE, colors="Community", alpha=0.2)+  labs(x="Species richness", y ="Use diversity")+
  scale_color_manual(values = pal)+
  scale_fill_manual(values = pal)+
  theme_classic() +
  theme(legend.position = "none") +
  ggtitle('Alpha diversity')
ind_alpha <- ggpredict(glmer3, terms=c("Species_number", "Community"), type = "random") %>% 
   plot(add.data=T, ci=F, limit.range = TRUE, colors="Community", alpha=0.2)+
   facet_wrap(~group)+   labs(x="Species richness", y ="Use diversity")+
   scale_color_manual(values = pal)+
   scale_fill_manual(values = pal)+
   theme(legend.position = "none") +
   ggtitle('Per community')+ theme_classic() +  theme(legend.position = "none") 

total_alpha/ind_alpha



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
abs_comp_dist <- as.matrix(comp_dist>0)*1
rowSums(abs_comp_dist)
etno_matrix_dist <- vegdist(use_dist, method="bray")
comp_matrix_dist <- vegdist(comp_dist, method="bray")
comp_matrix_dist_pres <- vegdist(abs_comp_dist, method="jaccard")

#betadiver(comp_matrix_dist)

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
#plot(distances$comp, distances$etno)

distances$Plot_comb <- paste(distances$Plot1, distances$Plot2)

ds$Plot_comb <- paste(ds$Plot1, ds$Plot2)
data_p <- merge(ds, distances, by="Plot_comb")
#plot(data_p$comp, data_p$etno)

colnames(dt) <- c("Plot1", "Community")
data_p1 <- merge(dt, distances,by="Plot1")
data_p2 <- merge(dt, data_p1, by.y="Plot2", by.x="Plot1")
data_p2$group <- paste(data_p2$Community.x, data_p2$Community.y, sep="-")
unique(data_p2$group)
data_p2$group <- str_replace(data_p2$group, "Tumupasa-Macahua", "Macahua-Tumupasa")
data_fin <- data_p2[,c(8,5,6)]
data_fin$resta <- data_fin$comp - data_fin$etno

data_mean <- data_fin %>%  group_by(group) %>%
  dplyr::summarize(mean_comp =median(comp),
                   mean_etno = median(etno)) 


data_mean$comp_rescaled <- (data_mean$mean_comp - min(data_mean$mean_comp)) / (max(data_mean$mean_comp) - min(data_mean$mean_comp))
data_mean$etno_rescaled <- (data_mean$mean_etno - min(data_mean$mean_etno)) / (max(data_mean$mean_etno) - min(data_mean$mean_etno))


data_melt <- reshape2::melt(data_mean[,c(1,4,5)]) 
data_melt <- reshape2::melt(data_mean[,c(1,2,3)]) 

bargraph <- ggplot(data =  ggplot(trade.m, aes(Time)) + geom_bar(subset = .(variable ==
     "EXP"), aes(y = value, fill = BEC), stat = "identity") +
     geom_bar(subset = .(variable == "IMP"),
         aes(y = -value, fill = BEC), stat = "identity") +
     xlab("") + scale_y_continuous("Export  -  Import",
     formatter = "comma")) +
  geom_bar(aes(x = value,
               y = reorder(group, -value),
               fill = variable),
           stat = "identity",  
           position=position_dodge(width=0.7))+ 
  scale_fill_manual(values=c('#99999990','#E69F00'), name="Distance", labels = c("Floristic", "Cultural"))+
  theme_classic()+
  labs(title="Floristic and cultural distance among communities", 
       y="Community pairs", x = "Relative mean distance between plots")  

data_div <- data_melt %>% mutate (value= ifelse(variable=="mean_comp", value, -1*value))
y_breaks <- pretty(data_melt$value)

ggplot(data_div,
       aes(x=value, 
           y=reorder(group, -value),
           fill=variable))+
  geom_bar(stat='identity')+
 # position=position_dodge(width=0.7))+ 
  scale_fill_manual(values=c('#299617','#E69F00'), name="Distance", labels = c("Floristic", "Cultural"))+
  theme_classic()+
  labs(title="Floristic and cultural distance among communities", 
       y="Community pairs", x = "Relative mean distance between plots")  +
  scale_x_continuous(breaks=y_breaks,
                     labels=abs(y_breaks))+
  scale_y_discrete(labels=data_div$group)

data_div <- data_mean %>% mutate (value= data_mean$comp_rescaled-data_mean$etno_rescaled)
data_div <- data_div %>% mutate (value_fill= ifelse(value<0, "Cultural", "Floristic"))

y_breaks <- pretty(data_div$value)


ggplot(data_div,
       aes(x=value, 
           y=reorder(group, value),
           fill=value_fill))+
  geom_bar(stat='identity')+
  # position=position_dodge(width=0.7))+ 
  scale_fill_manual(values=c('#E69F00', '#299617'), name="Difference by:", labels = c("More cultural distance than floristic", "More floristic distance than cultural"))+
  theme_classic()+
  labs(title="Predominant floristic or cultural distance among communities", 
       y="Community pairs", x = "Difference between cultural and floristic distance")  +
  scale_x_continuous(breaks=y_breaks,
                     labels=abs(y_breaks))+
  geom_vline(xintercept = 0, linetype=2)

ggsave("bargraph1.png")

head(data_mean)

bargraph

#expected distance by floristic dissimilarity
#true cultural distance 
# la distancia florística es mayor no coincide con la distancia de conocimiento, excepto en el caso de guiyero y bolivar. 
  # la distancia florística es mayor que la distancia de conocimiento. 






 indices <- df$Community
variables <- df$Plot
dt <- data.table(indices, variables)

get_permutations <- function(df){
  perm <- permutations(nrow(unique(df[,1])), 2, df$indices)
  as.data.table(perm)
}

ds <- dt[, get_permutations(.SD), by = variables]
colnames(ds) <- c("Comunidad", "Plot1", "Plot2")

distances$Plot_comb <- paste(distances$Plot1, distances$Plot2)
data_toplot <- merge(ds, distances, by="Plot_comb")



# Analysis beta
data2 <- data_toplot[,c(1,2,7,8)]


library(glmmTMB)

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
library(performance)
library(Rmisc)
install.packages("Rmisc")
CI(data2$etno, ci=0.95)

#Plot predictions
min <- min(data2$comp)
max <- max(data2$comp)



newdatpredation <-data.frame(comp=c(rep(seq(min, max, length.out = 100), each = 76)),
                             Comunidad = NA)

preds_45 <- predict(mod4, data2, se=T, re.form=NA)
plot_data_m4 <- data.frame(newdatpredation,
                           pred = preds_45$fit,
                           low = preds_45$fit - 1.96*preds_45$se.fit, 
                           upp = preds_45$fit + 1.96*preds_45$se.fit,
                           Comunidad = data2)

plot_data_m4


r2(mod7)










MuMIn::r.squaredGLMM(mod4)
summary(mod6)
colnames(data2)
pred <- ggpredict(mod4, terms=c("comp", "Comunidad"), type = "random")
pred
pred <- pred %>% dplyr::rename(Comunidad=group)

head(plot_data_m4)

x#Plot predictions
min <- min(data2$comp)
max <- max(data2$comp)


seq(min, max, length.out = 760)
newdat <-data.frame(comp=c(rep(seq(min, max), each = 76)))

nd_pop <- data.frame(comp= seq(min, max, length.out = 760), Comunidad=data2$Comunidad, new=NA)

nd$Subject <- "new"
predict(g0, newdata=nd, allow.new.levels=TRUE)


preds_45$se.fit
preds_45 <- predict(mod5,se=T, re.form=NA)

plot_data_m4 <- data.frame(nd_pop,
                           pred = preds_45$fit,
                           low = preds_45$fit - 1.96*preds_45$se.fit, 
                           upp = preds_45$fit + 1.96*preds_45$se.fit,
                           Comunidad = data2$Comunidad)

plot_data_m4
total_beta <- ggplot(plot_data_m4, aes(x = comp, y = pred, color=Comunidad)) +
  geom_line(size = 1) +
  geom_point(data=data2, aes(y=etno, x=comp, color=Comunidad),alpha=0.5)+
 geom_ribbon(data=plot_data_m4, aes(ymin = low, ymax = upp), alpha = 0.2) +
  #facet_wrap(~group)+
  scale_color_manual(values = pal)+
  scale_fill_manual(values = pal)+
  theme(legend.position = "none") +
  ggtitle('Beta diversity') + theme_classic() +  theme(legend.position = "none") 
total_beta

preds_45$fit - 1.96*preds_45$se.fit
preds_4 <- predict(mod4, data2, se=T, re.form=NA)
min <-preds_4$fit - 1.96*preds_4$se.fit

individual_beta <- ggplot(data2, aes(x = comp, y = etno, color = Comunidad)) +
  geom_point(size=2, alpha=.5)+
 facet_wrap(~ Comunidad)+
  geom_line(data = pred, aes(x = x, y = predicted), size = 1) +
 # geom_ribbon(aes(ymax = predict(mod4, data2, se=T, re.form=NA)$se.fit,                ymin = predict(mod4, data2, se=T, re.form=NA)$se.fit))+
  scale_color_manual(values = pal)+
  scale_fill_manual(values = pal)+
  theme(legend.position = "none") +
  ggtitle('Per community') + theme_classic() +  theme(legend.position = "none") +
  facet_wrap(~ Comunidad)
individual_beta
patch <- total_alpha/ind_alpha|total_beta / individual_beta 
patch+plot_annotation(tag_levels = 'A')



newdatpredation <-data.frame(logHpopkm=c(rep(seq(mini, maxi, length.out = 100), each = 76)),
                             abslat =rep(c(seq(0, 75, length.out = 76)), 100),
                             Site=NA,
                             coordinates = NA)

preds_45 <- predict(mod4, newdatpredation, se=T, re.form=NA)
plot_data_m4 <- data.frame(newdatpredation,
                           pred = preds_45$fit,
                           low = preds_45$fit - 1.96*preds_45$se.fit, 
                           upp = preds_45$fit + 1.96*preds_45$se.fit,
                           Comunidad = data2$Comunidad)



newdatf <- data.frame(newdat, plo = exp(newdat$y-1.96*sqrt(pvarf)), phi = exp(newdat$y+1.96*sqrt(pvarf)))

AICc <- round(AIC(betareg1, betareg2, betareg3, betareg4, betareg5), 2)

nd_pop <- data.frame(Comunidad=data2$Comunidad, comp=data2$comp,etno=1:length(data2$comp))
newdata <- data.frame(x1 = c(1, 2, 3), x2 = c(4, 5, 6), subject = rep(1, 3))
preds <- predict(mod4, nd_pop,  type = "response", re.form = NA, se.fit=T)


str(preds)
hist(preds$se.fit)
observed <- data.frame(x1 = data2$comp, x2 = data2$Comunidad, y = data2$etno)
predicted <- data.frame(x1 = data2$comp, x2 = data2$Comunidad, y = preds$fit)
ci <- preds
ci <- as.data.frame(cbind(ci$fit, ci$se.fit))
newdatf <- data.frame(ci, plo = exp(ci$V1-1.96*sqrt(V1)), phi = exp(newdat$V1+1.96*sqrt(V1)))
ci_2 <- exp(cbind(ci[, 1], ci[, 1] + qnorm(0.975) * ci[, 2], ci[, 1] - qnorm(0.975) * ci[, 2]))
colnames(ci) <- c("fit", "lwr", "upr")

# Add the confidence intervals to the new data frame
newdata$ci_lwr <- ci[, 2]
newdata$ci_upr <- ci[, 3]
# Create a scatter plot with the observed data

ggplot(observed, aes(x = x1, y = y, color=x2)) +
  geom_point(alpha=0.8) +
  geom_line(data=predicted, aes(x = x1, y = y, color=x2)) +
  #facet_wrap(~ x2) +
  theme_classic()+
  scale_color_manual(values = pal)+ scale_fill_manual(values = pal)+
  theme(legend.position = "none") +
  ggtitle('Beta diversity')+  theme(legend.position = "none") 



ggplot(observed, aes(x = x1, y = y, color=x2)) +
  geom_point() +
  facet_wrap(~ x2) +
  geom_line(data=predicted, aes(x = x1, y = y, color=x2)) +
  geom_ribbon(data = subset(plot_data_m4, plot_data_m4$abslat==plot_data_m4$abslat[1]) ,aes(ymin = expit(low), ymax = expit(upp)), fill = "grey5", alpha = 0.15, linetype = 0) +
  facet_wrap(~ x2) + theme_classic()+
  scale_color_manual(values = pal)+
  scale_fill_manual(values = pal)+
  theme(legend.position = "none") +
  ggtitle('Per community')+  theme(legend.position = "none") 


# Plots 



# Plot beta



data(sleepstudy,package="lme4")
g0 <- glmmTMB(Reaction~Days+(Days|Subject),sleepstudy)
predict(g0, sleepstudy)
## Predict new Subject
nd <- sleepstudy[1,]
nd$Subject <- "new"
predict(g0, newdata=nd, allow.new.levels=TRUE)
## population-level prediction
nd_pop <- data.frame(Days=unique(sleepstudy$Days),
                     Subject=NA)
predict(g0, newdata=nd_pop)


data2
data2<-data %>% rename(group=Comunidad)
mod4
predict(mod4, data)

nd <- data2[1,]
nd$Subject <- "new"
predict(mod4, newdata=nd, allow.new.levels=TRUE)
## population-level prediction
nd_pop <- data.frame(Plot=unique(data$Plot_comb),
                     Comunidad=NA)
predict(mod4, newdata=nd_pop)



mydf <- ggpredict(mod5, terms=c("comp", "Comunidad"), type = "random", ci.lvl = 0.95)

ggplot(mydf, aes(x = x, y = predicted, colour = group)) +
  geom_line()+
  geom_point(data=data2, aes(x=comp, y=etno, colour=Comunidad))+
  #stat_smooth(method = "glm", se = T) + 
  facet_wrap(vars(group))+
  theme_classic()+labs(x="Floristic dissimilarity", y ="Use distance")+
  scale_color_manual(values = pal)+  scale_fill_manual(values = pal)+
  theme(legend.position="none")

colnames(mydf)
ggplot(mydf, aes(x = x, y = predicted, colour = group)) +
  geom_line()+
  geom_ribbon(aes(ymin =predict(mod4, type="quantile", at = c(0.25)), 
                  ymax =conf.high, alpha=0.01))+
  geom_point(data=data2, aes(x=comp, y=etno, colour=group))+
  facet_wrap(vars(group))+
  theme_classic()+labs(x="Floristic dissimilarity", y ="Use distance")+
  scale_color_manual(values = pal)+  scale_fill_manual(values = pal)+
  theme(legend.position="none")


ggplot() +
  geom_point(data=data, aes(x=comp, y=etno, colour=Comunidad))+facet_wrap(~Comunidad)

mydf <- ggpredict(mod4, terms=c("comp"), type = "random")
ggplot(mydf, aes(x = x, y = predicted))+
  geom_point(data=data, aes(x=comp, y=etno))+
  stat_smooth(method = "lm", se = T) +  
  theme_classic()+labs(x="Floristic dissimilarity", y ="Use distance")



+
  labs(x="Species richness", y ="Use diversity")+
  scale_color_manual(values = pal)+
  scale_fill_manual(values = pal)+
  theme(legend.position = "none") +
  ggtitle('Alpha diversity')+ theme_classic() +  theme(legend.position = "none") 





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
