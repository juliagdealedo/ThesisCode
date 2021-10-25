##### CAP2 - Understanding traditional knowledge distribution in western Amazonia #

# PACKAGES ####

library(readxl) 
library(reshape2)#dcast
library(tibble)#column_to_row
library(Imap)#geodistance
library(vegan)#mantel
library(recluster)#reclusterdist
library(scales)#alpha
library(randomcoloR)#distinctcolorpalette
library(betapart)
library(RColorBrewer)
library(tidyverse)#ggplot,dplyr
library(flextable) #create beautiful tables
library(wesanderson)
library(colorspace)
library(viridis)
library(vegan)
library(ethnobotanyR)
library(circlize)
library(magrittr)
library(paletteer)
library(ggthemes)

# RAW DATA PROCESSING ####


# Read data

setwd("/Users/juliag.dealedo/OneDrive - UAM/UAM_Doctorado/Capitulos/cap2/data") #establecer el directorio de trabajo
etno<-read_excel("etno-dis-mad-yas3.xlsx", sheet=2, col_names =T)

setwd("/Users/juliag.dealedo/OneDrive - UAM/UAM_Doctorado/Capitulos/cap2/data/dataraw") 
RAW<-read_excel("DATOSRAW.2.xlsx", sheet=1)

# Process data

# COMPOSITION
RAW <- RAW[-grep("[[:digit:]]", RAW$Species), ] # Eliminate morphospecies
RAW <- RAW[-grep("aff. ", RAW$Species), ] # Eliminate aff. species
RAW<-RAW[!(RAW$Family=="Sin colecta"),] # Eliminate indet or not collected
RAW<-RAW[!(RAW$Family=="Indet"),]
RAW$Forest <- paste(RAW$ForestType, RAW$Habitatdescription)
write.table(RAW, "composition")

# ETHNOBOTANY
unique(etno$Cod_plot)
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
etno$Species <- firstup(tolower(etno$Species))
etno <- etno[-grep("[[:digit:]]", etno$Species), ] # Eliminate morphospecies
etno <- etno[-grep("aff. ", etno$Species), ] # Eliminate aff. species
etno<-etno[!(etno$Family=="Sin colecta"),] # Eliminate indet or not collected
etno<-etno[!(etno$Family=="Indet"),]
etno <- etno %>% drop_na(Cod_plot)
write.table(etno, "etno")

# RESUMEN
alt <- aggregate(RAW$Height, list(RAW$Plot), mean)
dbh <- aggregate(RAW$DBH, list(RAW$Plot), mean)
ele <- aggregate(RAW$Altitud, list(RAW$Plot), mean)
spp <- aggregate(RAW$Species, list(RAW$Plot), function(x) length(unique(na.omit(x))))
gen <- aggregate(RAW$Genus, list(RAW$Plot), function(x) length(unique(na.omit(x))))
fam <- aggregate(RAW$Family, list(RAW$Plot), function(x) length(unique(na.omit(x))))
coords <- cbind (aggregate(RAW$Latitud, list(RAW$Plot), min), 
                 aggregate(RAW$Longitud, list(RAW$Plot), min)[,2])
colnames (coords) <- c("Plot", "Latitud", "Longitud")
ind <- aggregate(RAW$Project, list(RAW$Plot), length)
regnum <- aggregate(RAW$Plot, list(RAW$Region), function(x) length(unique(na.omit(x))))
Region <- rep(regnum[,1], regnum[,2])
Country <- c(rep ("Bolivia", 44), rep ("Peru", 50), rep ("Ecuador", 25))
graf <- cbind(Country, Region, ind, spp[,2],gen[,2],fam[,2],dbh[,2],alt[,2], ele[,2], 
              coords[,-1], unique(cbind(RAW$Forest, RAW$Plot))[,1])

# create a matrix of abundance. Rows=Plots, Columns=Species
matri <- dcast(RAW, Plot~Species, value.var="Species", fill=0)
matri <- column_to_rownames(matri,var="Plot")
hshannon <- diversity(matri, index="shannon") 
simpson <- diversity(matri, index="simpson")
invsimpson <- diversity(matri, index="invsimpson")
jeve <- hshannon/log(specnumber(matri))
falpha <- fisher.alpha(matri) 
Srar <- rarefy(matri, min(rowSums(matri)))

# generate a new column with the name of the region
grafico2<-cbind(graf, falpha,hshannon,simpson,invsimpson,jeve,Srar)
colnames(grafico2)<-(c("Country", "Region", "Plot", "Ind", "Species", "Genus", "Families", "DBH", "Height",
                       "Elevation", "Latitud", "Longitud", "Forest", "Fisher", "Shannon", "Simpson", "Invsimpson", "Evenness", "Raref"))
write.table(grafico2, file = "resumen")



# DESCRIPTIVE ####

setwd("/Users/juliag.dealedo/OneDrive - UAM/UAM_Doctorado/Capitulos/cap2/data/dataraw") 
resu <- read.table("resumen")
comp <- read.table("composition")
etno <- read.table ("etno")

toupper("UNDERSTADING TRADITIONAL KNOWLEDGE DISTRIBUTION IN WESTERN AMAZONIA")
length(unique(etno$Species))
length(unique(etno$Genus))
length(unique(etno$Family))
length(unique(tolower(etno$Species)))
length(unique(tolower(etno$Category)))
length(etno$Item)
cati <-  etno %>% count(Category, sort=T)
sum(cati$n)
use <- etno %>% count(Use, sort=T)
sum(use$n)
unique(etno$informant)
length(unique(etno$Cod_plot))
length(unique(etno$Species))
length(unique(etno$Genus))
length(unique(etno$Family))
length(unique(etno$Comunidad))
length(unique(etno$informant))
length(unique(etno$Use)) # plat-uses
sum(etno$TOTAL...121) # Use-reports
length(unique(etno$Category)) 
length(unique(etno$Subcategory))
length(unique(etno$`Plant part`)) # plant part used
length(unique(etno[etno$TOTAL...121>0,]$Species)) # Used Species
length(unique(etno[etno$TOTAL...121==0,]$Species))
total <- length(unique(etno[etno$TOTAL...121>0,]$Species)) + length(unique(etno[etno$TOTAL...121==0,]$Species))
length(unique(etno[etno$TOTAL...121>0,]$Species)) / length(unique(etno$Species)) * 100
length(unique(etno[etno$TOTAL...121>0,]$Genus)) / length(unique(etno$Genus)) * 100
length(unique(etno[etno$TOTAL...121>0,]$Family)) / length(unique(etno$Family)) * 100
use <- etno %>% count(Use, sort=T)
use$n[2]/sum(use$n)*100
etno %>% count(Comunidad, sort=T)
pie <- aggregate(etno$Use, list(etno$Comunidad), function(x) length(unique(na.omit(x))))
grid.col = c(Aguapolo = "#A6CEE3", Bolivar = "#1F78B4", Dicaro="#B2DF8A", Guiyero="#33A02C", Infierno = "#FB9A99", SanCarlos = "#FF7F00",
             NuevaVida = "#FDBF6F", Macahua = "#E31A1C", Tumupasa = "#CAB2D6", Yamino = "#6A3D9A")

pie$Comunidad <- gsub("[[:blank:]]", "", pie$Group.1)
pdf("pie1.pdf")
pie(pie$x, pie$Comunidad, col=grid.col)
dev.off()
# OTHER PIE
pie <- aggregate(etno$Use, list(etno$Category), function(x) length(unique(na.omit(x))))
pie <- aggregate(etno$Use, list(etno$Category), function(x) length(na.omit(x)))

pie$Comunidad <- gsub("[[:blank:]]", "", pie$Group.1)
pdf("pie3.pdf")
pie(pie$x, pie$Group.1, col=spectral)
dev.off()


# 119 plots, 10 communities, 25 informants, 32867 use-reports, 449 plant-uses, 12 categories, 62 subcategories, 43 part plants used, 
# 1455 spp used
# use-reports (UR) the informant i mentions the use of the species s in the use-category u

cons <- use[str_detect(use$Use, "CONST"), ]
sum(as.numeric(na.omit(cons$n)))

length(unique(RAW$Species))
length(unique(etno$Species))
raw1 <- arrange(RAW, Species)
etno1 <- arrange (etno, Species)
species <- as.data.frame(cbind(unique(etno1$Species), unique(etno1$Species)))
unique(species)


# DATA PROCESSING ####

setwd("/Users/juliag.dealedo/OneDrive - UAM/UAM_Doctorado/Capitulos/cap2/data/dataraw") 
resu <- read.table("resumen")
comp <- read.table("composition")
etno <- read.table ("etno")

#1 SCRIPT TO ETHNOBOTANY PER PLOT COUNT CATEGORIES - ok oct 7th #### 
etno <- read.table ("etno")

# Create a column with just the number of plot
etno$Plot <- substring(etno$Cod_plot,2,4) 
etno$Plot
etno$Cod_plot
unique(etno$Plot)
colnames(etno)
# Create a column with the informant species information together
etno$uni <- paste (etno$informant, etno$Species)
etno0 <- etno
# Create a separate dataframe with just information per informant, species and use
cat1 <- cbind(etno$informant, etno$Species, etno$Category, etno$Use)
cat1 <- as.data.frame(cat1)
colnames(cat1) <- c("Informant", "Species", "Category", "Use")

# Eliminate duplicates
cat2 <- unique(cat1)

# Create a separate dataframe with just information per informant, species and use
use <- cbind(etno$informant, etno$Species, etno$Use)
use1 <- as.data.frame(use)
colnames(use1) <- c("Informant", "Species", "Use")
use2 <- unique(use1)

# Eliminate duplicates
# etno2 <- unique(cat1)

# Create a column with the informant species information together as in the base dataframe
cat2$uni <- paste(cat2$Informant, cat2$Species)
# Select just the numbers to prepare to create a matrix
etno3 <- cat2[,c(5,3)]
etno4 <- dcast(etno3, uni~Category, value.var="Category", fill=0)
# Join both data frames by the common column "uni"
etno5 <- merge(etno0, etno4, by="uni")
colnames(etno5)
con <- etno5$CONSTRUCTION.y
con2 <- as.matrix(ifelse(con==0, FALSE, TRUE)*1)
sum(con2)

# Save table in a excel to check functionability
# write.table(etno5, "etno5.txt", sep="\t")
# Eliminate duplicates just from the first columns
etno55 <- etno5[!duplicated(etno5[,c(1:32)]),]
etno6 <- etno55
# Select the new columns of the database
etno66 <- etno6[,c(6,139:151)]
# Remove characteres from the Cod_plot column to be able to arrange it properly
etno666 <- arrange(etno66, Plot)
# convert matrix to 1 and 0 to be able to sum uses (need to remove first column with the cod_plot)
# etno7 <- as.matrix(ifelse(etno666[,-1]==0, FALSE, TRUE)*1)
# etno8 <- as.data.frame(cbind(mapply(etno666[,1], FUN=as.numeric), etno7)) # add again cod_plot column
etno666$Plot <- mapply(etno666[,1], FUN=as.numeric)
etno10 <- arrange(etno666, Plot) # order it

# Prepare empty objects:
suma <- c()
mat.cat <- matrix(nrow=length(unique(etno10$Plot)), ncol=length(unique(colnames(etno10[,-1]))), 
                  dimnames = list(unique(etno10$Plot), unique(colnames(etno10[,-1]))))

# Loop to sum uses per plot
for (i in unique(etno10$Plot)) {
  
  etnoa1 <- as.data.frame(etno10[etno10$Plot==i,])
  suma <- colSums(mapply(etnoa1[,-1], FUN=as.numeric))
  mat.cat[i,] <- suma
}

View(mat.cat)
write.table(mat.cat, "matplot")


# LOOP PER COMUNITIES

# Select the new columns of the database
etno66 <- etno6[,c(31,139:151)]
# Remove characteres from the Cod_plot column to be able to arrange it properly
etno666 <- arrange(etno66, Comunidad)
etno666 <- na.omit(etno666)
#etno666$Comunidad <- mapply(etno666[,1], FUN=as.numeric)
etno10 <- etno666
str(etno10)
# LOOP 
# Prepare empty objects:
suma <- c()
mat.cat.com <- matrix(nrow=length(unique(etno10$Comunidad)), ncol=length(unique(colnames(etno10[,-1]))), 
                      dimnames = list(unique(etno10$Comunidad), unique(colnames(etno10[,-1]))))

# Loop to sum uses per plot
for (i in unique(etno10$Comunidad)) {
  
  etnoa1 <- as.data.frame(etno10[etno10$Comunidad==i,])
  suma <- colSums(mapply(etnoa1[,-1], FUN=as.numeric))
  mat.cat.com[i,] <- suma
}


View(mat.cat.com)
write.table(mat.cat.com, "matcommunities")

#mat.cat1 <- mat.cat[,-13]


#1 SCRIPT TO ETHNOBOTANY PER PLOT COUNT SUBCATEGORIES - #### 
etno <- read.table ("etno")

# Create a column with the informant species information together
etno$uni <- paste (etno$informant, etno$Species)
etno0 <- etno
# Create a separate dataframe with just information per informant, species and use
cat1 <- cbind(etno$informant, etno$Species, etno$Subcategory, etno$Use)
cat1 <- as.data.frame(cat1)
colnames(cat1) <- c("Informant", "Species", "Subcategory", "Use")

# Eliminate duplicates
cat2 <- unique(cat1)

# Create a separate dataframe with just information per informant, species and use
use <- cbind(etno$informant, etno$Species, etno$Use)
use1 <- as.data.frame(use)
colnames(use1) <- c("Informant", "Species", "Use")
use2 <- unique(use1)

# Create a column with the informant species information together as in the base dataframe
cat2$uni <- paste(cat2$Informant, cat2$Species)
# Select just the numbers to prepare to create a matrix
etno3 <- cat2[,c(5,3)]
etno4 <- dcast(etno3, uni~Subcategory, value.var="Subcategory", fill=0)
# Join both data frames by the common column "uni"
etno5 <- merge(etno0, etno4, by="uni")
colnames(etno5)
con <- etno5$CONSTRUCTION.y
con2 <- as.matrix(ifelse(con==0, FALSE, TRUE)*1)
sum(con2)

# Save table in a excel to check functionability
# write.table(etno5, "etno5.txt", sep="\t")
# Eliminate duplicates just from the first columns
etno55 <- etno5[!duplicated(etno5[,c(1:34)]),]
etno6 <- etno55
# Select the new columns of the database
etno66 <- etno6[,c(6,139:201)]
# Remove characteres from the Cod_plot column to be able to arrange it properly
etno666 <- arrange(etno66, Plot)
# convert matrix to 1 and 0 to be able to sum uses (need to remove first column with the cod_plot)
# etno7 <- as.matrix(ifelse(etno666[,-1]==0, FALSE, TRUE)*1)
# etno8 <- as.data.frame(cbind(mapply(etno666[,1], FUN=as.numeric), etno7)) # add again cod_plot column
etno666$Plot <- mapply(etno666[,1], FUN=as.numeric)
etno10 <- arrange(etno666, Plot) # order it
etno10 <- etno10[,-64]
# Prepare empty objects:
suma <- c()
mat.cat <- matrix(nrow=length(unique(etno10$Plot)), ncol=length(unique(colnames(etno10[,-1]))), 
                  dimnames = list(unique(etno10$Plot), unique(colnames(etno10[,-1]))))

# Loop to sum uses per plot
for (i in unique(etno10$Plot)) {
  
  etnoa1 <- as.data.frame(etno10[etno10$Plot==i,])
  suma <- colSums(mapply(etnoa1[,-1], FUN=as.numeric))
  mat.cat[i,] <- suma
}

View(mat.cat)
mat.subcat <- mat.cat
write.table(mat.subcat, "matsubcat")


#2 SCRIPT TO ETHNOBOTANY PER PLOT COUNT FORESTS USES - ok oct 7th ####
etno <- read.table ("etno")
str(etno)
# Create a column with just the number of plot
etno$Plot <- substring(etno$Cod_plot,2,4) 
etno$Plot
etno$Cod_plot
unique(etno$Plot)
colnames(etno)

# Create a column with the informant species information together
etno$uni <- paste (etno$informant, etno$Species)
# Create a separate dataframe with just information per informant, species and use
etno1 <- cbind(etno$informant, etno$Species, etno$Use)
etno1 <- as.data.frame(etno1)
colnames(etno1) <- c("Informant", "Species", "Use")
# Eliminate duplicates
etno2 <- unique(etno1)
# Create a column with the informant species information together as in the base dataframe
etno2$uni <- paste(etno2$Informant, etno2$Species)
# Select just the numbers to prepare to create a matrix
etno3 <- etno2[,c(4,3)]


etno4 <- dcast(etno3, uni~Use, value.var="Use", fill=0)
# Join both data frames by the common column "uni"
etno5 <- merge(etno, etno4, by="uni")

# Save table in a excel to check functionability
# write.table(etno5, "etno5.txt", sep="\t")
# Eliminate duplicates just from the first columns

etno55 <- etno5[!duplicated(etno5[,c(1:33)]),]
etno555 <- etno55[,c(139:587)]
etno66 <- etno555
etno66$Plot <- etno55$Plot
etno661 <- etno66
etno661$Species <- etno55$Species

## good one with plot ONLY
# Remove characteres from the Cod_plot column to be able to arrange it properly
# etno66$Cod_plot <- substring(etno66$Cod_plot,2,3)
etno66$Plot <- as.numeric(etno66$Plot)
etno66 <- etno66[c(450,1:449)]
etno666 <- arrange(etno66, Plot)

# convert matrix to 1 and 0 to be able to sum uses (need to remove first column with the cod_plot)
etno7 <- as.matrix(ifelse(etno666[,-1]==0, FALSE, TRUE)*1)
etno8 <- as.data.frame(cbind(etno666[,1], etno7)) # add again cod_plot column
etno10 <- arrange(etno8, V1) # order it
etno11 <- etno10[-length(etno10$V1),] #quitar la ultima con NAs

# LOOP
# Prepare empty objects:
suma <- c()
mat <- matrix(nrow=length(unique(etno11$V1)), ncol=length(unique(colnames(etno11[,-1]))), 
              dimnames = list(unique(etno11$V1),unique(colnames(etno11[,-1]))))

# Loop to sum uses per plot
for (i in unique(etno11$V1)) {
  
  etnoa1 <- as.data.frame(etno11[etno11$V1==i,])
  suma <- colSums(etnoa1[,-1]) 
  mat[i,] <- suma
}

View(mat)
write.table(mat, "mat")






#3 SCRIPT TO ETHNOBOTANY PER PLOT COUNT USES & SPECIES (plant species) - ok 5 oct ####
setwd("/Users/juliag.dealedo/OneDrive - UAM/UAM_Doctorado/Capitulos/cap2/data/dataraw") 

etno <- read.table ("etno")
str(etno)
# Create a column with just the number of plot
# etno$Plot <- substring(etno$Cod_plot,2,4) 
# etno$Plot
# etno$Cod_plot
unique(etno$Plot)
colnames(etno)

# Create a column with the informant species information together
etno$uni <- paste (etno$informant, etno$Species)
# Create a separate dataframe with just information per informant, species and use
etno1 <- cbind(etno$informant, etno$Species, etno$Use, etno$Family, etno$Subcategory, etno$Category)
etno1 <- as.data.frame(etno1)
colnames(etno1) <- c("Informant", "Species", "Use", "Family","Subcategory", "Category")
etno1$uni <- paste(etno1$Informant, etno1$Species) # Create a column with the informant species information together as in the base dataframe
#etno1$use2 <- paste(etno1$Use, etno1$Species, sep="_")


# TABLE TO ANALYSE UNIQUE VALUES OF USE AND SPECIES
etno2 <- unique(etno1)
INF <- unique(etno3[,c(2, 3)])

etno3 <- na.omit(etno2) # DATA FRAME TO ANALYSE THINGS AT A SPECIES LEVEL
unique(etno3$Species)
etno3.2 <-etno3[,c(2,3)] 
a <- unique(etno3.2[etno3.2$Species=="Euterpe precatoria",])

# species more used
alpha.spp <- aggregate(etno3$Use, list(etno3$Species), function(x) length(unique(na.omit(x))))
sum(alpha.use$x)
# families more used
fam <- unique(etno3[,c(3,4)])
alpha.fam <- aggregate(fam$Use, list(fam$Family), function(x) length(unique(na.omit(x))))
# more cited uses
use <- unique(etno3[,c(2,3)])
unique(use$Use)
alpha.use <- arrange(aggregate(use$Species, list(use$Use), function(x) length(x)), desc(x))
sum(alpha.use$x[c(1:10)]/sum(alpha.use$x))
# more cited uses
use <- unique(etno3[,c(1,2,3)])
alpha.use <- arrange(aggregate(use$Species, list(use$Use), function(x) length(x)), desc(x))
sum(alpha.use$x[c(1:10)]/sum(alpha.use$x))
#subcategory
usefulspecies <- length(unique(etno3$Species))
use <- unique(etno3[,c(2,5)])
unique(use$Use)
alpha.use <- arrange(aggregate(use$Species, list(use$Subcategory), function(x) length(x)), desc(x))
sum(alpha.use$x[c(1:10)]/sum(alpha.use$x))
perc  <- as.data.frame(cbind(alpha.use$Group.1,round(alpha.use$x/usefulspecies*100, 2)))



use <- unique(etno3[,c(2,3)])
alpha.use <- arrange(aggregate(use$Species, list(use$Use), function(x) length(x)), desc(x))
sum(alpha.use$x[c(1:10)]/sum(alpha.use$x))
common <- alpha.use$Group.1[c(1:10)]

perc  <- as.data.frame(cbind(alpha.use$Group.1,round(alpha.use$x/usefulspecies*100, 2)))
perc  <- as.data.frame(cbind(alpha.use$Group.1,round(alpha.use$x/sum(alpha.use$x)*100, 2)))
#perc  <- as.data.frame(cbind(alpha.use$Group.1,alpha.use$x/sum(alpha.use$x)*100))
colnames(perc)<- c("use", "percentaje")
sum(as.numeric(perc$percentaje))
# Select just the numbers to prepare to create a matrix
etno3.1 <- etno3[,c(4,3)]










setwd("/Users/juliag.dealedo/OneDrive - UAM/UAM_Doctorado/Capitulos/cap2/data/dataraw") 

etno <- read.table ("etno")
str(etno)
# Create a column with just the number of plot
# etno$Plot <- substring(etno$Cod_plot,2,4) 
# etno$Plot
# etno$Cod_plot
unique(etno$Plot)
colnames(etno)

# Create a column with the informant species information together
etno$uni <- paste (etno$informant, etno$Species)
# Create a separate dataframe with just information per informant, species and use
etno1 <- cbind(etno$informant, etno$Species, etno$Use, etno$Family)
etno1 <- as.data.frame(etno1)
colnames(etno1) <- c("Informant", "Species", "Use", "Family")
etno1$uni <- paste(etno1$Informant, etno1$Species) # Create a column with the informant species information together as in the base dataframe
etno3 <- etno1
etno3$use2 <- paste(etno3$Use, etno3$Species, sep="_")
etno4 <- dcast(etno3, uni~use2, value.var="use2", fill=0)
# Join both data frames by the common column "uni"
etno5 <- merge(etno, etno4, by="uni")
colnames(etno5)


# Save table in a excel to check functionability
# write.table(etno5, "etno5.txt", sep="\t")
# Eliminate duplicates just from the first columns

etno55 <- etno5[!duplicated(etno5[,c(1:33)]),]
etno555 <- etno55[,c(139:6467)]
etno66 <- etno555
etno66$Plot <- etno55$Plot
etno661 <- etno66
#etno661$Species <- etno55$Species

## good one with plot ONLY
# Remove characteres from the Cod_plot column to be able to arrange it properly
# etno66$Cod_plot <- substring(etno66$Cod_plot,2,3)
etno66$Plot <- as.numeric(etno66$Plot)
etno66 <- etno66[c(6330,1:6329)]
etno666 <- arrange(etno66, Plot)
str(etno66)
# convert matrix to 1 and 0 to be able to sum uses (need to remove first column with the cod_plot)
etno7 <- as.matrix(ifelse(etno666[,-1]==0, FALSE, TRUE)*1)
etno8 <- as.data.frame(cbind(etno666[,1], etno7)) # add again cod_plot column
etno10 <- arrange(etno8, V1) # order it
#etno11 <- etno10[-length(etno10$V1),] #quitar la ultima con NAs

# LOOP
# Prepare empty objects:
suma <- c()
mat <- matrix(nrow=length(unique(etno10$V1)), ncol=length(unique(colnames(etno10[,-1]))), 
              dimnames = list(unique(etno10$V1),unique(colnames(etno10[,-1]))))

# Loop to sum uses per plot
for (i in unique(etno10$V1)) {
  
  etnoa1 <- as.data.frame(etno10[etno10$V1==i,])
  suma <- colSums(etnoa1[,-1]) 
  mat[i,] <- suma
}

View(mat)
write.table(mat, "mat.try")
mat.spp <- mat

head(mat)



mat.spp <- read.table ("mat.try") 

# NEW DATA FRAMES and decriptive 2 ####

setwd("/Users/juliag.dealedo/OneDrive - UAM/UAM_Doctorado/Capitulos/cap2/data/dataraw") 
resu <- read.table("resumen")
comp <- read.table("composition")
etno <- read.table ("etno")
mat.spp <- read.table ("mat.try") 
mat.use <- read.table ("mat") 
mat.cat <- read.table ("matplot") 




mat <- read.table ("mat")
mat1 <- mat
str(mat1)
etnorich <- rowSums(mat1)
etnorich1 <- as.data.frame(etnorich)
etnorich2 <- rownames_to_column(etnorich1, "Plot")
resu4 <- merge (resu, etnorich2, by="Plot")
resu4$Region
com <- unique(cbind(etno55$Plot, etno55$Comunidad, etno55$Etnia))[,c(1,2,3)]
colnames(com) <- c("Plot", "Comunidad", "Etnia")
resu5<- merge(resu4, com, by="Plot", all=T)
resu5$Plot <- as.numeric(resu5$Plot)
resu6 <- arrange(resu5, Plot)
resu7 <- resu6[-length(resu6$Plot),] #quitar la ultima con NAs


# SAVE TABLES #
write.table(resu7, "resucap2")
write.table(mat, "usematrix")
write.table(etno55, "etnodb")


setwd("/Users/juliag.dealedo/OneDrive - UAM/UAM_Doctorado/Capitulos/cap2/data/dataraw") 
dir()

mat <- read.table ("mat")
resu <- read.table ("resucap2")
comp <- read.table("composition")
etno <- read.table ("etno")
matx <- read.table ("matcommunities") # per category







# CORRELATION ####
mat <- read.table ("mat")
resu <- read.table ("resucap2")
resu1 <- read.table ("resuclean")
dir()
library(corrplot)
#choosing the parameters 
divcor <- resu[,c(4:7,11,12,15:20)]
#calculate the correlation
M<-cor(divcor)


cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

# matrix of the p-value of the correlation
p.mat <- cor.mtest(divcor)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

chulo <- corrplot(M, method="color", col=col(200),  
                  type="upper", order="original", 
                  addCoef.col = "black", # Add coefficient of correlation
                  tl.col="black", tl.srt=45, #Text label color and rotation
                  # Combine with significance
                  p.mat = p.mat, sig.level = 0.01, insig = "blank", 
                  # hide correlation coefficient on the principal diagonal
                  diag=FALSE)

chulo
jpeg("Correlation2.jpeg", res=600, height=49,
     width=45, pointsize=11, units="cm")

corrplot(M, method="color", col=col(200),  
         type="upper", order="original", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE)


dev.off()


plot(resu$Species, resu$etnorich)
plot(resu$Longitud, resu$Species)
plot(resu$Latitud, resu$Species)
plot(resu$Latitud, resu$etnorich)

xyplot(resu$Comunidad, resu$Species)
colores <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

boxplot(Species~Comunidad, data=resu, horizontal=F,axes=T, xlab="", ylab="", col=unique(as.factor(resu$Comunidad)))

boxplot(Species~as.factor(Latitud), data=resu, horizontal=F,axes=T, xlab="", ylab="", col=unique(as.factor(resu$Comunidad)))
boxplot(Species~Comunidad, data=resu, horizontal=F,axes=T, xlab="", ylab="", col=unique(as.factor(resu$Comunidad)))
boxplot(etnorich~Comunidad, data=resu, horizontal=F,axes=T, xlab="", ylab="", col=unique(as.factor(resu$Comunidad)))







# ANALYSIS PER SE ####


# ANOSIM TEST ####
mat1cat <- mat.cat3[,-1]
anosim(resu4$etnorich,resu4$Region)
data(dune)
data(dune.env)
dune.dist <- vegdist(dune)
dune.ano <- with(dune.env, anosim(dune.dist, Management))
summary(dune.ano)


# good one

setwd("/Users/juliag.dealedo/OneDrive - UAM/UAM_Doctorado/Capitulos/cap2/data/dataraw") 
dir()

mat <- read.table ("mat")
resu <- read.table ("resucap2")
comp <- read.table("composition")
etno <- read.table ("etno")
mat.spp <- read.table ("mat.try")

mat.dist <- vegdist(mat)
as.data.frame(mat.dist)
etno.ano <- with(resu, anosim(mat.dist, Comunidad))
summary(etno.ano)

mat.dist <- vegdist(mat.spp)
as.data.frame(mat.dist)
etno.ano <- with(resu, anosim(mat.dist, Comunidad))
summary(etno.ano)


# CHORDDIAGRAM ####

library(circlize)

setwd("/Users/juliag.dealedo/OneDrive - UAM/UAM_Doctorado/Capitulos/cap2/data/dataraw") #establecer el directorio de trabajo
dir()

matx <- read.table("matcommunitiesclean")
etno55 <- read.table("etnoclean") 
resu1 <- read.table("resuclean")
mat <- read.table ("usematrixclean")
comp <- read.table("compclean")
comp <- read.table("compclean")

setwd("/Users/juliag.dealedo/OneDrive - UAM/UAM_Doctorado/Capitulos/cap2/data/dataraw") 
dir()

mat <- read.table ("mat")
resu <- read.table ("resucap2")
comp <- read.table("composition")
etno <- read.table ("etno")
matx <- read.table ("matcommunities") # per category

matx1 <- matx[,1:12]
colnames(matx1)
rownames(matx1) <- c("Aguapolo", "Bolivar", "Dicaro", "Guiyero", "Infierno", "Macahua", "NuevaVida", "SanCarlos", "Tumupasa", "Yamino")
colnames(matx1) <- c("Animal Food", "Construction", "Cultural", "Environmental", "Fuel", "Human Food","Marketed", "Medicinal", "Other", "Toxic", "Utensils and Tools", "Wild Animal Food")
matx2 <- matx1[colSums(matx1)>100]
#colnames(matx) <- c("Construction", "Cultural", "Environmental", "Fuel", "Human Food","Marketed", "Medicinal", "Utensils and Tools", "Wild Animal Food")

grid.col = c(Aguapolo = "#A6CEE3", Bolivar = "#1F78B4", Dicaro="#B2DF8A", Guiyero="#33A02C", Infierno = "#FB9A99", Macahua = "#FF7F00",
             NuevaVida = "#FDBF6F", SanCarlos = "#E31A1C", Tumupasa = "#CAB2D6", Yamino = "#6A3D9A")

cols <- grid.col[as.factor(resu$Comunidad)]


setwd("/Users/juliag.dealedo/OneDrive - UAM/UAM_Doctorado/Capitulos/cap2/figs")

pdf("chord.cat6.pdf", height=8,width=8, pointsize=12)

par(cex = 0.7, mar = c(2, 2, 2, 2))
chordDiagram(as.matrix(matx1), symmetric=F, directional = 1, scale=F, grid.col=grid.col,
             transparency = 0.1,
             link.lwd = 1,    # Line width
             link.lty = 1,    # Line type
             link.border = 1,
            link.visible = matx1 >= 300, 
             link.decreasing = T, link.zindex = rank(matx1))

dev.off()


# Prepare empty objects:
setwd("/Users/juliag.dealedo/OneDrive - UAM/UAM_Doctorado/Capitulos/cap2/data/dataraw") 

mat <- read.table ("mat")
resu <- read.table ("resucap2")
mat$Comunidad <- resu$Comunidad
suma <- c()
mat2 <- matrix(nrow=length(unique(mat$Comunidad)), ncol=length(unique(colnames(mat[,-450]))), 
               dimnames = list(unique(mat$Comunidad),unique(colnames(mat[,-450]))))

# Loop to sum uses per plot
for (i in unique(mat$Comunidad)) {
  
  etnoa1 <- as.data.frame(mat[mat$Comunidad==i,])
  suma <- colSums(etnoa1[,-450]) 
  mat2[i,] <- suma
}

View(mat2)

setwd("/Users/juliag.dealedo/OneDrive - UAM/UAM_Doctorado/Capitulos/cap2/data/dataraw") #establecer el directorio de trabajo
library(vegan)
library(circlize)

rownames(matx) <- c("Aguapolo", "Bolivar", "Dicaro", "Guiyero", "Infierno", "Macahua", "NuevaVida", "SanCarlos", "Tumupasa", "Yamino")
colnames(matx) <- c("Bolivar", "Infierno", "Macahua", "NuevaVida", "SanCarlos", "Tumupasa", "Yamino")
mat.dist <- vegdist(matx, upper=T, diag = T, method="bray")
mat.dist.cor <- cor(t(matx))
mat.dist <- as.matrix(mat.dist-1)
mu <- as.matrix(log(log(mat.dist+1)))

grid.col = c(Aguapolo = "#A6CEE3", Bolivar = "#1F78B4", Dicaro="#B2DF8A", Guiyero="#33A02C", Infierno = "#FB9A99", Macahua = "#FF7F00",
             NuevaVida = "#FDBF6F", SanCarlos = "#E31A1C", Tumupasa = "#CAB2D6", Yamino = "#6A3D9A")

cols <- grid.col[as.factor(resu$Comunidad)]

setwd("/Users/juliag.dealedo/OneDrive - UAM/UAM_Doctorado/Capitulos/cap2/figs")

pdf("chord.com5.pdf", height=4, width=4, pointsize=6)
par(cex = 1.2, mar = c(2, 2, 2, 2))
chordDiagram(log(mat.dist+1), symmetric=T, directional = 2, scale=F, grid.col=grid.col,
             transparency = 0.2,
             link.lwd = 0.6,    # Line width
             link.lty = 1,    # Line type
             link.border = 9) # Border color)


dev.off()

circos.clear()


# KNOWLEDGE AND FLORISTIC COMPOSITION - RQ4 ####

setwd("/Users/juliag.dealedo/OneDrive - UAM/UAM_Doctorado/Capitulos/cap2/data/dataraw") 
dir()

mat <- read.table ("mat")
resu <- read.table ("resucap2")
comp <- read.table("composition")
etno <- read.table ("etno")
mat.spp <- read.table ("mat.try")

# HETEROGENEITY ####
mean(resu$etnorich)
spp <- aggregate((resu$etnorich), list(resu$Comunidad), mean)
mean(spp$x)
compmat <- dcast(comp, Plot~Species, value.var="Species", fill=0)
compmat <- column_to_rownames(compmat,var="Plot")
discomp <- vegdist(compmat, method="bray")
disetno <- vegdist(mat.spp, method="bray") #### CAMBIAR MAT O MAT.SPP si se quiere hacer por usos o por usos de las especies
density(discomp)
hist(disetno)
mantel(discomp, disetno, method="pearson", permutations = 999)

# remove duplications etno
ull <- as.matrix(disetno)
ull[upper.tri(ull)] <- NA
# melt the data.frame
ull4 <- reshape2::melt(ull, na.rm = TRUE)
# get rid of the zeros and rename variables
ull5 <- ull4 %>%
  dplyr::filter(!(value == 0)) %>%
  dplyr::rename(Plot1 = Var1, 
                Plot2 = Var2, 
                etno = value)

# remove duplications floristic
ucc <- as.matrix(discomp)
ucc[upper.tri(ucc)] <- NA
# melt the data.frame
ucc4 <- reshape2::melt(ucc, na.rm = TRUE)
# get rid of the zeros and rename variables
ucc5 <- ucc4 %>%
  dplyr::filter(!(value == 0)) %>%
  dplyr::rename(Plot1 = Var1, 
                Plot2 = Var2, 
                comp = value)

completo <- cbind(ull5, ucc5)
complet <- completo[,c(1,2,3,6)]

library(geodist)
resulat <- as.data.frame(cbind(resu$Plot, resu$Latitud, resu$Longitud))
resulat2 <- column_to_rownames(resulat, "V1")
colnames(resulat2) <- c("Latitude", "Longitude")
geo <-  geodist(resulat2)/1000
geo[upper.tri(geo)] <- 0
ugg4 <- reshape2::melt(geo, na.rm = TRUE)
ugg5 <- ugg4 %>%
  dplyr::filter(!(value == 0)) %>%
  dplyr::rename(Plot1 = Var1, 
                Plot2 = Var2, 
                geogra = value)
ugg6 <- round(ugg5$geogra,0)
View(ugg6)
complet$geo <- ugg6

col <- paletteer_c("ggthemes::Green-Gold", 501)
col1 <- col[as.factor(ugg6)]

setwd("/Users/juliag.dealedo/OneDrive - UAM/UAM_Doctorado/Capitulos/cap2/figs")
pdf("dist.spp.pdf", height=8,   width=8, pointsize=11)
layout(mat = matrix(c(2, 1, 0, 3),   nrow = 2,  ncol = 2),
       heights = c(1, 3),    # Heights of the two rows
       widths = c(3, 1))     # Widths of the two columns
par(mar = c(5, 4, 0, 0))
plot(complet$comp, complet$etno, pch=21, bg=alpha(col1, 0.8), col="black",cex=1.5, ylab="Knowledge dissimilarity", xlab="Floristic dissimilarity")
lgd_ = rep(NA, 501)
max(ugg6)
lgd_[1] = "0 km"
lgd_[501] = "1800 km"
mtext(at= 0.48,"Geographic distance", line=-1.4, cex=1)
legend(x = 0.36, y = 0.95, bty="n", title="", title.adj = -3,
       legend = lgd_, adj=0,
       fill = col, #title="Geographic distance",title.adj=-4,
       border = NA, horiz=F, y.intersp = 0.01,
       cex = 0.9, text.font = 1)
mtext("Mantel r = 0.40", side=1, line=-1.3, at=0.9,cex=1)

# add density plot
denscomp <- density(discomp)
densetno <- density(disetno)
par(mar = c(0, 4, 0, 0))
plot(denscomp$x, denscomp$y, frame = FALSE, col = "white",ylab=" ", xlab=" ", xaxt = "n",yaxt = "n", bty = "n", yaxt = "n")
polygon(denscomp$x, denscomp$y, col=alpha("grey", .7))
par(mar = c(5, 0, 0, 0))
plot(densetno$y, densetno$x, frame = FALSE, col = "white",ylab=" ",xlab=" ", xaxt = "n",  yaxt = "n", bty = "n", yaxt = "n")
polygon(densetno$y, densetno$x, col=alpha("grey", .7), add=T)


dev.off()

# only plots compared at 130 km distance

col <- paletteer_c("ggthemes::Green-Gold", 501)
col1 <- col[as.factor(ugg6)]

complet1 <- complet[complet$geo < 130,]
col <- paletteer_c("ggthemes::Green-Gold", 501)
col2 <- col[as.factor(complet1$geo)]
pdf("dist.130.pdf", height=8,   width=8, pointsize=11)
plot(complet1$comp, complet1$etno, pch=21, bg=alpha(col2, 0.8), col="black",cex=1.5, xlim=c(0.3,1), ylim=c(0,1),ylab="Knowledge dissimilarity", xlab="Floristic dissimilarity")
scimath1 <- lm(complet1$etno ~complet1$comp, complet1)
abline(scimath1, col="black", ltw=2, lwd=1)
dev.off()


# Cluster similarity between communities ####

setwd("/Users/juliag.dealedo/OneDrive - UAM/UAM_Doctorado/Capitulos/cap2/data/dataraw") 
dir()

mat <- read.table ("mat")
mat$Comunidad <- paste(resu$Comunidad, resu$Plot)
rownames(mat) <- NULL
mat <- column_to_rownames(mat, "Comunidad")
hcd <- as.dendrogram(hc)
disetno <- vegdist(mat, method="bray")
hc <- hclust(disetno, method = "ward.D2")
grid.col = c(Aguapolo = "#A6CEE3", Bolivar = "#1F78B4", Dicaro="#B2DF8A", Guiyero="#33A02C", Infierno = "#FB9A99", SanCarlos = "#FF7F00",
             NuevaVida = "#FDBF6F", Macahua = "#E31A1C", Tumupasa = "#CAB2D6", Yamino = "#6A3D9A")
cols <- spectral[as.factor(resu$Comunidad)]
colors <- unique(cols)
clus4 = cutree(hc, 3)
colss <- grid.col[as.factor(resu$Comunidad)]
grid.col2 = c("#089392","#33B08D", "#82C782",)
unique(cols)
clus4 = cutree(hc, 3)
dev.off()
setwd("/Users/juliag.dealedo/OneDrive - UAM/UAM_Doctorado/Capitulos/cap2/figs")
pdf("dendro0.pdf", height=8,
    width=8, pointsize=11)

par(mar=c(0,0,0,0))
plot(as.phylo(hc), type = "fan", edge.width = 2, edge.lty = 1,
     edge.color= "black", tip.color = colss,
     label.offset = 0, cex = 0.9,  show.tip.label = T)

dev.off()



# Floristic NMDS ####


setwd("/Users/juliag.dealedo/OneDrive - UAM/UAM_Doctorado/Capitulos/cap2/data/dataraw") 
dir()


etno <- read.table ("etno")
resu <- read.table ("resucap2")
comp <- read.table("composition")

mat.spp <- read.table ("mat.try") 
mat.use <- read.table ("mat") 
mat.cat <- read.table ("matplot") 
mat.sub <- read.table ("matsubcat")

compmat <- dcast(comp, Plot~Species, value.var="Species", fill=0)
compmat <- column_to_rownames(compmat,var="Plot")
discomp <- vegdist(compmat, method="bray")
disetno <- vegdist(mat.spp, method="bray") #### CAMBIAR MAT O MAT.SPP si se quiere hacer por usos o por usos de las especies
density(discomp)
hist(disetno)
mantel(discomp, disetno, method="pearson", permutations = 999)

grid.col = c(Aguapolo = "#A6CEE3", Bolivar = "#1F78B4", Dicaro="#B2DF8A", Guiyero="#33A02C", Infierno = "#FB9A99", Macahua = "#FF7F00",
             NuevaVida = "#FDBF6F", SanCarlos = "#E31A1C", Tumupasa = "#CAB2D6", Yamino = "#6A3D9A")

cols <- grid.col[as.factor(resu$Comunidad)]

compMDS.spp <-metaMDS(mat.spp, distance="bray", k=2, trymax=100, autotransform=TRUE) ##k is the number of dimensions
S.spp <- paste("stress = ", round(compMDS.spp$stress, 3))
mat.dist.spp <- vegdist(mat.spp, method="bray")
etno.ano.spp <- with(resu, anosim(mat.dist.spp, Comunidad, distance="bray"))
R.spp <- paste("R = ", round(etno.ano.spp$statistic, 2))

compMDS.cat<-metaMDS(mat.cat, distance="bray", k=2, trymax=100, autotransform=TRUE) ##k is the number of dimensions
S.cat <- paste("stress = ", round(compMDS.cat$stress, 3))
mat.dist.cat <- vegdist(mat.cat, method="bray")
etno.ano.cat <- with(resu, anosim(mat.dist.cat, Comunidad, distance="bray"))
R.cat <- paste("R = ", round(etno.ano.cat$statistic, 2))

compMDS.sub<-metaMDS(mat.sub, distance="bray", k=2, trymax=100, autotransform=TRUE) ##k is the number of dimensions
S.sub <- paste("stress = ", round(compMDS.sub$stress, 3))
mat.dist.sub <- vegdist(mat.sub, method="bray")
etno.ano.sub <- with(resu, anosim(mat.dist.sub, Comunidad, distance="bray"))
R.sub <- paste("R = ", round(etno.ano.sub$statistic, 2))

compMDS.use <-metaMDS(mat.use, distance="bray", k=2, trymax=100, autotransform=TRUE) ##k is the number of dimensions
S.use <- paste("stress = ", round(compMDS.use$stress, 3))
mat.dist.se <- vegdist(mat.use, method="bray")
etno.ano.use <- with(resu, anosim(mat.dist.use, Comunidad, distance="bray"))
R.use <- paste("R = ", round(etno.ano.use$statistic, 2))

compMDS.comp <-metaMDS(compmat, distance="bray", k=2, trymax=100, autotransform=TRUE) ##k is the number of dimensions
S.comp <- paste("stress = ", round(compMDS.comp$stress, 3))
mat.dist.comp <- vegdist(compmat, method="bray")
etno.ano.comp <- with(resu, anosim(mat.dist.comp, Comunidad, distance="bray"))
R.comp <- paste("R = ", round(etno.ano.comp$statistic, 2))

compMDS.spp.rot <- MDSrotate(compMDS.spp, vec=resu$Latitud)
compMDS.cat.rot <- MDSrotate(compMDS.cat, vec=resu$Latitud)
compMDS.sub.rot <- MDSrotate(compMDS.sub, vec=resu$Latitud)
compMDS.use.rot <- MDSrotate(compMDS.use, vec=resu$Latitud)
compMDS.comp.rot <- MDSrotate(compMDS.comp, vec=resu$Latitud)

setwd("/Users/juliag.dealedo/OneDrive - UAM/UAM_Doctorado/Capitulos/cap2/figs")

pdf("nmdsRQ4.4.pdf", width =15, height=5, pointsize=11)

uh <- matrix(c(1,2,3,4,5), ncol=5, byrow=T)
layout(uh)

plot(compMDS.comp.rot$points, type="n",xlab="NMDS1", ylab="NMDS2",  main="Floristic similarity", xlim=c(-2,2), ylim=c(-2,2))
points(compMDS.comp.rot$points,  pch=21, cex=2, col="black", bg= alpha(cols,0.9))
ordihull(compMDS.comp.rot, groups=resu$Comunidad, draw = "polygon", border=alpha(grid.col, 0.9), lty = 1, col =grid.col, alpha=0.5)
legend("bottomright", title="Communities", cex=1, pch=16, bg="black", col=unique(cols), legend=unique(resu$Comunidad),bty="n", inset = c(0, 0))
mtext(R.comp, side = 1, line =-2 , cex=1)
mtext(S.comp, side = 3, line =-3 , cex=1)

plot(compMDS.spp.rot$points, type="n",xlab="NMDS1", ylab="NMDS2",  main="Plant use", xlim=c(-2,2), ylim=c(-2,2))
points(compMDS.spp.rot$points,  pch=21, cex=2, col="black", bg= alpha(cols,0.9))
ordihull(compMDS.spp.rot, groups=resu$Comunidad, draw = "polygon", border=alpha(grid.col, 0.9), lty = 1, col =grid.col, alpha=0.5)
#legend("bottomright", title="Knowledge distances", cex=1, pch=16, bg="black", col=unique(cols), legend=unique(resu$Comunidad),bty="n", inset = c(0, 0))
mtext(R.spp, side = 1, line =-2 , cex=1)
mtext(S.spp, side = 3, line =-3 , cex=1)

plot(compMDS.use.rot$points, type="n",xlab="NMDS1", ylab="NMDS2",  main="Resource use", xlim=c(-2,2), ylim=c(-2,2))
points(compMDS.use.rot$points,  pch=21, cex=2, col="black", bg= alpha(cols,0.9))
ordihull(compMDS.use.rot, groups=resu$Comunidad, draw = "polygon", border=alpha(grid.col, 0.9), lty = 1, col =grid.col, alpha=0.5)
#legend("bottomright", title="Knowledge distances", cex=1, pch=16, bg="black", col=unique(cols), legend=unique(resu$Comunidad),bty="n", inset = c(0, 0))
mtext(R.use, side = 1, line =-2 , cex=1)
mtext(S.use, side = 3, line =-3 , cex=1)

plot(compMDS.sub.rot$points, type="n",xlab="NMDS1", ylab="NMDS2",  main="Needs", xlim=c(-2,2), ylim=c(-2,2))
points(compMDS.sub.rot$points,  pch=21, cex=2, col="black", bg= alpha(cols,0.9))
ordihull(compMDS.sub.rot, groups=resu$Comunidad, draw = "polygon", border=alpha(grid.col, 0.9), lty = 1, col =grid.col, alpha=0.5)
#legend("bottomright", title="Knowledge distances", cex=1, pch=16, bg="black", col=unique(cols), legend=unique(resu$Comunidad),bty="n", inset = c(0, 0))
mtext(R.sub, side = 1, line =-2 , cex=1)
mtext(S.sub, side = 3, line =-3 , cex=1)

plot(compMDS.cat.rot$points, type="n",xlab="NMDS1", ylab="NMDS2",  main="Needs", xlim=c(-2,2), ylim=c(-2,2))
points(compMDS.cat.rot$points,  pch=21, cex=2, col="black", bg= alpha(cols,0.9))
ordihull(compMDS.cat.rot, groups=resu$Comunidad, draw = "polygon", border=alpha(grid.col, 0.9), lty = 1, col =grid.col, alpha=0.5)
#legend("bottomright", title="Knowledge distances", cex=1, pch=16, bg="black", col=unique(cols), legend=unique(resu$Comunidad),bty="n", inset = c(0, 0))
mtext(R.cat, side = 1, line =-2 , cex=1)
mtext(S.cat, side = 3, line =-3 , cex=1)



dev.off()










# Ethnobotany R Package ####

str(dis)
str(madi)
etnomat <- dcast(etno, informant+sp_name~Use, value.var="Use", fill=0)

uvdis <- NUs(etnomat)
sum(uvmat[,2])
uvmat <- NUs(etnomat2)

# DIVERSITY
# displamaz
compdis<-comp[(comp$Project=="Displamaz"),]
base1 <- (merge(compdis, uvdis, by = 'sp_name'))
base1u<-base1[!duplicated(base1[c("Cod_plot", "sp_name")]), ]  # Apply duplicated

spp1 <- aggregate(base1$sp_name, list(base1$Cod_plot), function(x) length(unique(na.omit(x))))
fish <- dcast(compdis, Cod_plot~sp_name, value.var="sp_name", fill=0)
fish1 <- column_to_rownames(fish,var="Cod_plot")
fish1 <- fisher.alpha(fish1)
uv1 <- aggregate(base1u$NU, list(base1u$Cod_plot), mean)
fo1 <- aggregate(base1u$Forest, list(base1u$Cod_plot), unique)
table1 <-cbind(uv1, spp1[,2], fo1[,2])
colnames(table1) <- c("Cod_plot", "NU", "Species", "Forest")

# madidi
uvmad <- NUs(etnomat2)
compmad<-comp[(comp$Project=="Madidi"),]
compmad<-compmad[-(compmad$Region=="A_Aguapolo"),]
base2 <- (merge(compmad, uvmad, by = 'sp_name'))
base2u<-base2[!duplicated(base2[c("Cod_plot", "sp_name")]), ] 
spp2 <- aggregate(base2$sp_name, list(base2$Cod_plot), function(x) length(unique(na.omit(x))))
fish <- dcast(compmad, Cod_plot~sp_name, value.var="sp_name", fill=0)
fishmat <- column_to_rownames(fish,var="Cod_plot")
fish2 <- fisher.alpha(fishmat)
uv2 <- aggregate(base2u$NU, list(base2u$Cod_plot), mean)
fo2 <- aggregate(base2u$Forest, list(base2u$Cod_plot), unique)
table2 <-cbind(uv2, spp2[,2], fo2[,2])
colnames(table2) <- c("Cod_plot", "NU", "Species", "Forest")

plot(log(NU)~log(Species), table2,type="n", xlim=c(2.6,5.6), ylim=c(1,2.1))
points(log(NU)~log(Species), table2[table2$Forest=="Lowland Terra firme",], pch=21, col="black", bg=alpha("green", 0.7),cex=2.5)
points(log(NU)~log(Species), table2[table2$Forest=="Submontane Terra firme",], pch=21, col="black", bg=alpha("blue", 0.7),cex=2.5)
points(log(NU)~log(Species), table2[table2$Forest=="Lowland Floodplain",], pch=21, col="black", bg=alpha("orange", 0.7),cex=3)
points(log(NU)~log(Species), table1[table1$Forest=="Lowland Terra firme",], pch=21, col="black", bg=alpha("dark green", 0.7),cex=2.5)
points(log(NU)~log(Species), table1[table1$Forest=="Submontane Terra firme",], pch=21, col="black", bg=alpha("light blue", 0.7),cex=2.5)
points(log(NU)~log(Species), table1[table1$Forest=="Lowland Floodplain",], pch=21, col="black", bg=alpha("dark orange", 0.7),cex=3)

# Latitude

# displamaz
compdis<-comp[(comp$Project=="Displamaz"),]
base1 <- (merge(compdis, uvdis, by = 'sp_name'))
spp1 <- aggregate(base1$sp_name, list(base1$Cod_plot), function(x) length(unique(na.omit(x))))
fish <- dcast(compdis, Cod_plot~sp_name, value.var="sp_name", fill=0)
fish1 <- column_to_rownames(fish,var="Cod_plot")
fish1 <- fisher.alpha(fish1)
uv1 <- aggregate(base1$NU, list(base1$Cod_plot), mean)
fo1 <- aggregate(base1$Forest, list(base1$Cod_plot), unique)
la1 <- aggregate(base1$Latitud, list(base1$Cod_plot), unique)
table1 <-cbind(uv1, fish1, fo1[,2], la1[,2])
colnames(table1) <- c("Cod_plot", "UV", "Fisher", "Forest", "Latitud")

plot(Fisher ~ UV, table1, pch=21, col="black", bg=alpha("orange", 0.7),cex=3)

# madidi
uvmad <- NUs(etnomat2)
compmad<-comp[(comp$Project=="Madidi"),]
compmad<-compmad[-(compmad$Region=="A_Aguapolo"),]
base2 <- (merge(compmad, uvmad, by = 'sp_name'))
spp2 <- aggregate(base2$sp_name, list(base2$Cod_plot), function(x) length(unique(na.omit(x))))
fish <- dcast(compmad, Cod_plot~sp_name, value.var="sp_name", fill=0)
fishmat <- column_to_rownames(fish,var="Cod_plot")
fish2 <- fisher.alpha(fishmat)
uv2 <- aggregate(base2$NU, list(base2$Cod_plot), mean)
fo2 <- aggregate(base2$Forest, list(base2$Cod_plot), unique)
la2 <- aggregate(base2$Latitud, list(base2$Cod_plot), unique)
table2 <-cbind(uv2, fish2, fo2[,2], la2[,2])
colnames(table2) <- c("Cod_plot", "UV", "Fisher", "Forest", "Latitud")
str(table2)
plot(UV~(Latitud), table2,type="n", xlim=c(2,-16))
points(UV~-Latitud, table2[table2$Forest=="Lowland Terra firme",], pch=21, col="black", bg=alpha(low, 0.7),cex=2.5)
points(UV~Latitud, table2[table2$Forest=="Submontane Terra firme",], pch=21, col="black", bg=alpha(sub, 0.7),cex=2.5)
points(UV~Latitud, table2[table2$Forest=="Lowland Floodplain",], pch=21, col="black", bg=alpha("yellow", 0.7),cex=3)
points(UV~Latitud, table1[table1$Forest=="Lowland Terra firme",], pch=21, col="black", bg=alpha("dark green", 0.7),cex=2.5)
points(UV~Latitud, table1[table1$Forest=="Submontane Terra firme",], pch=21, col="black", bg=alpha("light blue", 0.7),cex=2.5)
points(UV~Latitud, table1[table1$Forest=="Lowland Floodplain",], pch=21, col="black", bg=alpha("dark orange", 0.7),cex=3)





# MAP ####

setwd("/Users/juliag.dealedo/OneDrive - UAM/UAM_Doctorado/Capitulos/cap2/data/dataraw") 
dir()

mat <- read.table ("mat")
resu <- read.table ("resucap2")
comp <- read.table("composition")
etno <- read.table ("etno")
matx <- read.table ("matcommunities") # per category
unique(etno$informant)

# Packages
library(randomcoloR)
library(reshape2)
library(tibble)
library(scales)
library(readxl)
library(rgeos)
library(cartography)
library(sf)
library(sp)
library(rgdal)
library(raster)
library(geometry)
library(mapmisc)
library(RColorBrewer)
library(ggplot2)
library(dplyr)
library(ggthemes)

# Modify region coordinates not to superpose
coore <- cbind (aggregate(resu$Latitud, list(resu$Comunidad), min), aggregate(resu$Longitud, list(resu$Comunidad), min)[,2])
triki <- matrix(c(-1.6,-1.4,-1,-0.5,0,0,0,0,0,0,0,0))
colnames(coore) <- c("Region", "Latitud", "Longitud")

coore$Latitud2 <- coore$Latitud + triki # to have bolivia points visible and separate
resu2 <- merge(coore[,c(1,3)], resu, all=TRUE)
# Prepare labels
Latitud123 <- coore$Latitud2
u <- c("Aguapolo", "Yariapo", "Tumupasa", "Tequeje", "Ruinas", "Tambopata", 
       "Yanesha", "Cord. Azul", "Samiria", "Maijuna", "Dicaro", "Guiyero") #y labels without the A_, B_...
# Prepare the map data
locations_df <- coore[,c(1:3)] # rename 
colnames(locations_df) <- c("place", "lat", "lon")
locations <- as_tibble(locations_df)
locations_sf <- st_as_sf(locations, coords = c("lon", "lat"), crs = 4326)
# Prepare the elevation data
setwd("/Users/juliag.dealedo/Google Drive/Cap1_JGA-MP/data/MapData")
elev_peru<- raster("PER_alt/PER_alt.gri")
elev_bol<- raster("BOL_alt/BOL_alt.gri")
elev_ecu<- raster("ECU_alt/ECU_alt.gri")
elev_chl<- raster("CHL_alt/CHL1_alt.gri")
elev_col<- raster("COL_alt/COL_alt.gri")
elev_bra<- raster("BRA_alt/BRA_alt.gri")
elev_arg<- raster("ARG_alt/ARG_alt.gri")
elev_pry<- raster("PRY_alt/PRY_alt.gri")
# Set the limits of my map
countries <- readOGR("World_Countries/TM_WORLD_BORDERS-0.3.shp", stringsAsFactors = TRUE)
e2 <- extent(-83, -63, -18, 5)
Peru <- subset(countries, name %in% c("Peru", "Bolivia", "Ecuador"))
ele <- merge(elev_peru, elev_bol, elev_ecu)
elevbien <- mask(crop(ele, Peru), Peru)
aveiro <- subset(countries, name %in% c("Venezuela", "Ecuador", "Nigaragua", "Argentina",
                                        "Chile", "Peru", "Bolivia", "Colombia", "Brazil", "Guyana", "French Guiana", "Uruguay", "Paraguay",
                                        "Suriname"))


## To plot the a 1MAP with shade countries + 2MAP with plots included
setwd("/Users/juliag.dealedo/OneDrive - UAM/UAM_Doctorado/Capitulos/cap2/figs")
pdf('Map3.pdf', points=8)

# 1- map
par(bty = 'n') 
par(mar=c(0,0,0,0))

plot(aveiro)
plot(subset(countries, name == "Peru"), col="#fe8a71", add=T)
plot(subset(countries, name == "Bolivia"), col="#fe8a71", add=T)
plot(subset(countries, name == "Ecuador"), col="#fe8a71", add=T)
mtext("Peru", side=3, line=-18, at=-75.5, cex=0.7)
mtext("Bolivia", side=3, line=-23, at=-65.3, cex=0.7)
mtext("Ecuador", side=3, line=-13, at=-77.8, cex=0.6)

dev.off()

# Colours
cols_3 <- c("#BBD673", "#C56EC6", "#B3C7CA")
cols <- cols_3[factor(resu$Forest)]
spectral <- brewer.pal(11, "Spectral")

library(ggthemes)
library(ggplot2)
library(paletteer)
col <- paletteer_c("ggthemes::Green-Gold", 100)
col0 <- paletteer_c("grDevices::terrain.colors", 200)
col2 <- col[as.factor(resu$Region)]

# 2- map
par(bty = 'n') 
par(mar=c(0,0,0,0))

setwd("/Users/juliag.dealedo/OneDrive - UAM/UAM_Doctorado/Capitulos/cap2/figs")
pdf('Map4.pdf')

grid.col = c(Aguapolo = "#A6CEE3", Bolivar = "#1F78B4", Dicaro="#B2DF8A", Guiyero="#33A02C", Infierno = "#FB9A99", SanCarlos = "#FF7F00",
             NuevaVida = "#FDBF6F", Macahua = "#E31A1C", Tumupasa = "#CAB2D6", Yamino = "#6A3D9A")


grei <- gray.colors(100, start = 0.3, end = 0.9, gamma = 2.2, rev = FALSE) # change to a palette with gradual colours to have the elevation with colours

plot(crop(elevbien,e2),col=alpha(col0, 0.5), axes=F, legend=F)
plot(crop (Peru, e2), add=T, ylim=c(-17,5))
plot(locations_sf, pch = 21, bg=alpha(grid.col,0.9), col="black", cex = 3.5, add=T, ylim=c(-17,-0))
text(locations$lon, locations$lat+1.3, locations_sf$place, cex=0.6, pos=1,col="black") 

dev.off()



# NMDS and clean tables #####

# setwd("/Users/juliag.dealedo/OneDrive - UAM/UAM_Doctorado/Capitulos/cap2/data/dataraw") #establecer el directorio de trabajo
# TO ELIMINATE AGUAPOLO, NOT YET USED

# etno55 <- read.table("etnodb")
# etnoclean <- subset(etno55, !Cod_plot  %in%  c("A1", "A7", "A6", "A5"))
# write.table(etnoclean, "etnoclean")
# 
# mat <- read.table ("usematrix")
# mat1 <- mat[-c(1,5,6,7),]
# write.table (mat1, "usematrixclean")
# comp <- read.table("composition")
# comp1 <- subset(comp, !Cod_plot  %in%  c("A1", "A7", "A6", "A5"))
# write.table(comp1, "compclean")
# resu6 <- read.table("etnoresu")
# resu1 <- resu6[!resu6$Comunidad=="Aguapolo",]
# resu1$Comunidad <- gsub('\\s+', '', resu1$Comunidad)
# mat.cat.com <- read.table("matcommunities")
# mat.cat.com.clean <- mat.cat.com[-1,]
# write.table(mat.cat.com.clean, "matcommunitiesclean")
# write.table(resu1, "resuclean")


setwd("/Users/juliag.dealedo/OneDrive - UAM/UAM_Doctorado/Capitulos/cap2/data/dataraw") 
resu <- read.table("resumen")
comp <- read.table("composition")
etno <- read.table ("etno")
mat.spp <- read.table ("mat.try") 
mat.use <- read.table ("mat") 
mat.cat <- read.table ("matplot") 



spectral <- brewer.pal(10, "Paired")
cols <- spectral[as.factor(resu$Comunidad)]
spectral[as.factor(resu$Comunidad)]
levels(factor(resu$Comunidad))

compMDSetno<-metaMDS(mat, distance="bray", k=2, trymax=100, autotransform=TRUE) ##k is the number of dimensions
compMDSetno # metaMDS takes eaither a distance matrix or your community matrix (then requires method for 'distance=')
summary(compMDSetno)

matcomp <- dcast(comp, Cod_plot~Species, value.var="Species", fill=0)
matcomp<-column_to_rownames(matcomp, var="Cod_plot")
compMDS<-metaMDS(matcomp, distance="bray", k=2, trymax=100, autotransform=TRUE) ##k is the number of dimensions
compMDS

# just ethnobotany with uses per species

setwd("/Users/juliag.dealedo/OneDrive - UAM/UAM_Doctorado/Capitulos/cap2/figs")
pdf("nmdsetno1.pdf", width =8, height=8, pointsize=11)

#grid.col = c(Bolivar = "#089392", Infierno = "skyblue1", SanCarlos = "orange", NuevaVida = "palevioletred2", Macahua = "#EACF87", Tumupasa = "#E9A96C", Yamino = "tomato3",    Dicaro = "green", Guiyero = "black")

cols <- grid.col[as.factor(resu$Comunidad)]

plot(compMDSetno$points,xlab="NMDS1", ylab="NMDS2",  main="Total Uses", pch=21,cex=log(0.01*resu$etnorich), col="white", bg ="white")
legend("topleft", cex=0.7, pch=16, bg="black", col=unique(cols), legend=unique(resu$Comunidad),bty="n", inset = c(0, 0))
points(compMDSetno$points, pch=21, bg=alpha(cols,0.8), col="black", cex=log(0.04*resu$etnorich),   xlim=c(-2,2), ylim=c(-2,2))
ordihull(compMDSetno, groups=resu$Comunidad, draw = "polygon", border=alpha("white", 0.1), lty = 1, col = "grey", alpha=0.4)

points(compMDSetno$points, pch=21, bg=alpha(cols,0.8), col="black", cex=log(0.04*resu$etnorich),   xlim=c(-2,2), ylim=c(-2,2))

etno.ano <- with(resu, anosim(mat, Comunidad, distance="bray"))
R <- paste("R = ", round(etno.ano$statistic, 2))
S <- paste("stress = ", round(compMDSetno$stress, 3))
mtext(R, side = 1, line =-2 , cex=1.5)
mtext(S, side = 3, line =-3 , cex=1)

dev.off()

# NMDS total, dominant and rare uses ####

setwd("/Users/juliag.dealedo/OneDrive - UAM/UAM_Doctorado/Capitulos/cap2/figs")


pdf("3nmds4.pdf", width =15, height=6, pointsize=11)


uh <- matrix(c(1,2,3), ncol=3, byrow=T)
layout(uh)

sumi <- as.data.frame(colSums(mat))
useabu <- sum(sumi)
sumi <- rownames_to_column(sumi)
colnames(sumi) <- c("Use", "Abundance")
sumi<-arrange(sumi, desc(Abundance))

# Total uses
mat <- mat[, sumi[1:356,][,1]]
rowSums(mat)
mat.dist <- vegdist(mat, method="bray")
etno.ano <- with(resu, anosim(mat, Comunidad, distance="bray"))
R <- paste("R = ", round(etno.ano$statistic, 2))
summary(etno.ano)
compMDSetno<-metaMDS(mat, distance="bray", k=2, trymax=100, autotransform=TRUE) ##k is the number of dimensions
compMDSetno # metaMDS takes eaither a distance matrix or your community matrix (then requires method for 'distance=')
S <- paste("stress = ", round(compMDSetno$stress, 3))
grid.col = c(Aguapolo = "#A6CEE3", Bolivar = "#1F78B4", Dicaro="#B2DF8A", Guiyero="#33A02C", Infierno = "#FB9A99", Macahua = "#FF7F00",
             NuevaVida = "#FDBF6F", SanCarlos = "#E31A1C", Tumupasa = "#CAB2D6", Yamino = "#6A3D9A")

cols <- grid.col[as.factor(resu$Comunidad)]
plot(compMDSetno$points,xlab="NMDS1", ylab="NMDS2",  main="All Uses", pch=21,cex=log(0.01*resu$etnorich), col="white", bg ="white", 
     xlim=c(-2.3,2), ylim=c(-2,2),cex.main=2, cex.lab=1.4)
#legend("topright", cex=1, pch=16, bg="black", col=unique(cols), legend=unique(resu$Comunidad),bty="n", inset = c(0, 0))
ordihull(compMDSetno, groups=resu$Comunidad, draw = "polygon", border=alpha("white", 0.1), lty = 1, col ="grey", alpha=0.4)
points(compMDSetno$points, pch=21, bg=alpha(cols,0.8), col="black", cex=2,   xlim=c(-2,2), ylim=c(-2,2))
mtext(R, side = 1, line =-2 , cex=1.5)
mtext(S, side = 3, line =-3 , cex=1)

#orditorp(compMDSetno, "sites", cex=1, col="black", pcol="black")

#interesting sizing : log(0.04*resu$etnorich)


# Dominant uses

# sum(sumi[1:10,]$Abundance)/sum(sumi$Abundance)
# mat10 <- mat[, sumi[1:10,][,1]]

common <- gsub ("&", ".",gsub(" ", ".", common))
mat10 <- mat %>% select(one_of(common))
colnames(mat)
mat[common,]

rowSums(mat10)
mat.dist <- vegdist(mat10, method="bray")
as.data.frame(mat10)
etno.ano <- with(resu, anosim(mat10, Comunidad, distance="bray"))
R <- paste("R = ", round(etno.ano$statistic, 2))
summary(etno.ano)

compMDSetno<-metaMDS(mat10, distance="bray", k=2, trymax=100, autotransform=TRUE) ##k is the number of dimensions
compMDSetno # metaMDS takes eaither a distance matrix or your community matrix (then requires method for 'distance=')
S <- paste("stress = ", round(compMDSetno$stress, 3))
grid.col = c(Bolivar = "#089392", Infierno = "skyblue1", SanCarlos = "#82C782",
             NuevaVida = "palevioletred2", Macahua = "#EACF87", Tumupasa = "#E9A96C", Yamino = "tomato3")

grid.col = c(Aguapolo = "#A6CEE3", Bolivar = "#1F78B4", Dicaro="#B2DF8A", Guiyero="#33A02C", Infierno = "#FB9A99", Macahua = "#FF7F00",
             NuevaVida = "#FDBF6F", SanCarlos = "#E31A1C", Tumupasa = "#CAB2D6", Yamino = "#6A3D9A")

cols <- grid.col[as.factor(resu$Comunidad)]
plot(compMDSetno$points,xlab="NMDS1", ylab="NMDS2",  main="Common Uses", pch=21,cex=log(0.01*resu$etnorich), col="white", bg ="white",
     xlim=c(-2.3,2), ylim=c(-2,2), cex.main=2, cex.lab=1.4)
#legend("topright", cex=1, pch=16, bg="black", col=unique(cols), legend=unique(resu$Comunidad),bty="n", inset = c(0, 0))
ordihull(compMDSetno, groups=resu$Comunidad, draw = "polygon", border=alpha("white", 0.1), lty = 1, col ="grey", alpha=0.4)
points(compMDSetno$points, pch=21, bg=alpha(cols,0.8), col="black", cex=2,   xlim=c(-2,2), ylim=c(-2,2))
#orditorp(compMDSetno, "sites", cex=1, col="black", pcol="black")
mtext(R, side = 1, line =-2 , cex=1.5)
mtext(S, side = 3, line =-3 , cex=1)

# Rare uses
matrare <- mat[,sumi[11:356,][,1]]
rowSums(matrare)
mat.dist <- vegdist(matrare, method="bray")
as.data.frame(mat.dist)
etno.ano <- with(resu, anosim(mat.dist, Comunidad, distance="bray"))
R <- paste("R = ", round(etno.ano$statistic, 2))
summary(etno.ano)

compMDSetno<-metaMDS(matrare, distance="bray", k=2, trymax=100, autotransform=TRUE) ##k is the number of dimensions
compMDSetno # metaMDS takes eaither a distance matrix or your community matrix (then requires method for 'distance=')
summary(compMDSetno)
S <- paste("stress = ", round(compMDSetno$stress, 3))

grid.col = c(Aguapolo = "#A6CEE3", Bolivar = "#1F78B4", Dicaro="#B2DF8A", Guiyero="#33A02C", Infierno = "#FB9A99", Macahua = "#FF7F00",
             NuevaVida = "#FDBF6F", SanCarlos = "#E31A1C", Tumupasa = "#CAB2D6", Yamino = "#6A3D9A")

cols <- grid.col[as.factor(resu$Comunidad)]

plot(compMDSetno$points,xlab="NMDS1", ylab="NMDS2",  main="Rare Uses", pch=21,cex=log(0.01*resu$etnorich), col="white", bg ="white",
     xlim=c(-2.3,2), ylim=c(-2,1.7), cex.main=2, cex.lab=1.4)
#legend("topright", cex=1, pch=16, bg="black", col=unique(cols), legend=unique(resu$Comunidad),bty="n", inset = c(0, 0))
ordihull(compMDSetno, groups=resu$Comunidad, draw = "polygon", border=alpha("white", 0.1), lty = 1, col ="grey", alpha=0.4)
points(compMDSetno$points, pch=21, bg=alpha(cols,0.8), col="black", cex=2,   xlim=c(-2,2), ylim=c(-2,2))

mtext(R, side = 1, line =-2 , cex=1.5)
mtext(S, side = 3, line =-3 , cex=1)




dev.off()


# OTHER NMDS - not used - 30 sept ####

setwd("/Users/juliag.dealedo/OneDrive - UAM/UAM_Doctorado/Capitulos/cap2/figs")
#pdf("nmds2.pdf", width = 8, height=8, pointsize=11)

coli <- alpha(spectral[as.factor(resu6$Comunidad)],0.6)

plot(compMDSetno$points,xlab="NMDS1", ylab="NMDS2",  main="Ethnobotanical NMDS", pch=21,cex=log(0.01*resu6$etnorich), col="white", bg ="white")
points(compMDS$points, pch=16, col=coli, cex=6,   xlim=c(-2,2), ylim=c(-2,2))
points(compMDSetno$points, pch=21, bg=coli, col="black",  cex=5,   xlim=c(-2,2), ylim=c(-2,2))


#ordihull(compMDSetno, groups=resu6$Comunidad, draw = "polygon", lty = 1,  col = unique(coli))
cate<-unique(etno$Category)
take<- 1:94
#orditorp(compMDSetno, "sites", select = take, cex=1, col="black", pcol="black")
etnia <- c("Tacana-1", "Tacana-2", "Tacana-3", "Tacana-4", "Tacana-5", "Ese-eja", "Yanesha", "Cacataibo", "Cocama", "Maijuna")
legend("topright", cex=0.9, pch=16, col=unique(coli), legend=unique(resu6$Comunidad),bty="n", inset = c(0, 0))

ordisurf(compMDSetno ~ resu6$Latitud, col="grey", add=TRUE) 

# axes <- envfit(compMDSetno, resu4[,c(21:32)])
# axes.table <- data.frame((axes$vectors)$arrows, (axes$vectors)$r, (axes$vectors)$pvals) # Convert result into table
# axes.table <- rownames_to_column(axes.table)
# #qflextable(axes.table)
# plot(axes, col="black", bg=alpha("white", 0.9))

dev.off()


#plot both with ellipses
pdf("nmdstry8.pdf", width = 8, height=8, pointsize=11)

plot(NMDS1etno, NMDS2etno,xlab="NMDS1", ylab="NMDS2",  main="Ethnobotanical NMDS", xlim=c(-2,2), ylim=c(-2,2))

ordihull(compMDSsp1, groups=resu4$Region, draw = "polygon", lty = 1, col =spectral, alpha=0.9)
points(NMDS1etno, NMDS2etno, pch=21, col="black", cex=3,  bg = alpha(spectral[as.factor(resu$Region)],0.9), xlim=c(-2,2), ylim=c(-2,2))
#ordihull(compMDSetno, groups=resu4$Region, draw = "polygon", lty = 1, col =spectral, alpha=0.09)
plot(axes, col="black", bg=alpha("white", 0.9))
#points(NMDS1etno, NMDS2etno,xlab="NMDS1", ylab="NMDS2",  main="Ethnobotanical NMDS", xlim=c(-2,2), ylim=c(-2,2))
cate<-unique(etno$Category)
take<- 1:94
orditorp(compMDSetno, "sites", select = take, cex=1, col="black", pcol="black")
etnia <- c("Tacana-1", "Tacana-2", "Tacana-3", "Tacana-4", "Tacana-5", "Ese-eja", "Yanesha", "Cacataibo", "Cocama", "Maijuna")
legend("topright", cex=0.9, pch=16, col=unique(cols), legend=etnia,bty="n", inset = c(0, 0))

#ordisurf(compMDSetno ~ resu2$Latitud, col="grey", add=TRUE) 

# axes <- envfit(compMDSetno, resu1[,c(4,5,6,7,8,9,10,11,12,14:19)])
# axes.table <- data.frame((axes$vectors)$arrows, (axes$vectors)$r, (axes$vectors)$pvals) # Convert result into table
# axes.table <- rownames_to_column(axes.table)
axes <- envfit(compMDSetno, resu3[,c(21:32)])
axes.table <- data.frame((axes$vectors)$arrows, (axes$vectors)$r, (axes$vectors)$pvals) # Convert result into table
axes.table <- rownames_to_column(axes.table)
#qflextable(axes.table)
plot(axes, col="black", bg=alpha("white", 0.9))
# ordihull(compMDSetno, groups=resu4$comon, draw = "polygon", lty = 1, col ="blue")
# ordihull(compMDSsp1, groups=resu4$comon, draw = "polygon", lty = 1, col ="red")
#points(NMDS1sp1, NMDS2sp1, pch=16, cex=5, col=alpha(spectral[as.factor(resu$Region)],0.5))

dev.off()


### NMDS per CAtegories

setwd("/Users/juliag.dealedo/OneDrive - UAM/UAM_Doctorado/Cap2/figs")

pdf("nmdscategories3.pdf", width = 20, height=8, pointsize=11)
par(mfcol=c(2,6))

mat1
categories <- levels(as.factor (etno$Category))
categories [6]
mat2 <- rbind(colnames(mat1), mat1)
mat3 <- t(mat2)
mat4 <- as.data.frame(mat3)
cons2 <- as.data.frame(mat4[str_detect(mat4$V1, categories [2]), ])[,-1]
cons3 <- mapply(cons2, FUN=as.numeric)
cons4<- t(cons3)
str(cons4)
cons5 <- na.omit(cons4)
etno55
View(cons5)
rowSums(cons4)

compMDSsp1<-metaMDS(cons4, distance="bray", k=2, trymax=100, autotransform=TRUE) ##k is the number of dimensions
NMDS1sp1 <- compMDSsp1$points[,1] ##also found using scores(birdMDS)
NMDS2sp1 <- compMDSsp1$points[,2]
plot(NMDS1sp1, NMDS2sp1,xlab="NMDS1", ylab="NMDS2", main= categories [2],pch=21, cex=5, col="black", 
     bg = alpha(spectral[as.factor(resu4$Region)],0.6))


for (i in levels(as.factor (etno6$Category))) {
  cons<-etno6[(etno6$Category=="CONSTRUCTION"),]
  consmat <- dcast(cons, Cod_plot~Use, value.var="Use", fill=0)
  rownames (consmat) <- NULL
  consmat <- column_to_rownames(consmat,var="Cod_plot")
  # Represent results with NMDS K=2
  compMDSsp1<-metaMDS(consmat[,-1], distance="bray", k=2, trymax=100, autotransform=TRUE) ##k is the number of dimensions
  compMDSsp1 # metaMDS takes eaither a distance matrix or your community matrix (then requires method for 'distance=')
  #stressplot(compMDSsp) # stress=0.19 (TOO TIGHT!!!)
  summary(compMDSsp1)
  ##pull points from MDS
  NMDS1sp1 <- compMDSsp1$points[,1] ##also found using scores(birdMDS)
  NMDS2sp1 <- compMDSsp1$points[,2]
  plot(NMDS1sp1, NMDS2sp1,xlab="NMDS1", ylab="NMDS2",  main=i, pch=21, cex=5, col="black", 
       bg = alpha(spectral[as.factor(resu1$Region)],0.6))
  
}


## OTHER THINGS

# Per categories
str((etno))
cate <- unique(etno$Category)
cate[6]

dev.off()
cons<-etno[(etno$Category=="FUEL"),]
consmat <- dcast(cons, Cod_plot~Use, value.var="Use", fill=0)
consmat <- na.omit(consmat) 
rownames (consmat) <- NULL
consmat <- column_to_rownames(consmat,var="Cod_plot")
rowSums(consmat)
# Represent results with NMDS K=2
compMDSsp1<-metaMDS(consmat, distance="bray", k=2, trymax=100, autotransform=TRUE) ##k is the number of dimensions
compMDSsp1 # metaMDS takes eaither a distance matrix or your community matrix (then requires method for 'distance=')
#stressplot(compMDSsp) # stress=0.19 (TOO TIGHT!!!)
summary(compMDSsp1)

##pull points from MDS
NMDS1sp1 <- compMDSsp1$points[,1] ##also found using scores(birdMDS)
NMDS2sp1 <- compMDSsp1$points[,2]
plot(NMDS1sp1, NMDS2sp1,xlab="NMDS1", ylab="NMDS2",  main="FUEL", pch=21, cex=5, col="black", 
     bg = alpha(spectral[as.factor(resu1$Region)],0.1))
legend("topright", cex=0.7, pch=16, col=unique(cols),legend=unique(resu1$Region),bty="n", inset = c(0, 0))
take<- 1:11
orditorp(compMDSsp1, "species", select = take, cex=1, col="black", pcol="black")


levels(as.factor (etno$Category))

setwd("/Users/juliag.dealedo/OneDrive - UAM/UAM_Doctorado/Cap2/figs")

pdf("nmdscategories2.pdf", width = 20, height=8, pointsize=11)
par(mfcol=c(2,6))


for (i in levels(as.factor (etno$Category))) {
  cons<-etno[(etno$Category==i),]
  consmat <- dcast(cons, Cod_plot~Use, value.var="Use", fill=0)
  consmat <- na.omit(consmat) 
  rownames (consmat) <- NULL
  consmat <- column_to_rownames(consmat,var="Cod_plot")
  # Represent results with NMDS K=2
  compMDSsp1<-metaMDS(consmat, distance="bray", k=3, trymax=100, autotransform=TRUE) ##k is the number of dimensions
  compMDSsp1 # metaMDS takes eaither a distance matrix or your community matrix (then requires method for 'distance=')
  #stressplot(compMDSsp) # stress=0.19 (TOO TIGHT!!!)
  summary(compMDSsp1)
  ##pull points from MDS
  NMDS1sp1 <- compMDSsp1$points[,1] ##also found using scores(birdMDS)
  NMDS2sp1 <- compMDSsp1$points[,2]
  plot(NMDS1sp1, NMDS2sp1,xlab="NMDS1", ylab="NMDS2",  main=i, pch=21, cex=5, col="black", 
       bg = alpha(spectral[as.factor(resu1$Region)],0.6))
  
}

dev.off()
#ordisurf(compMDSsp1 ~ resu1$Latitud, col="grey9", add=TRUE) #created the ordisurf object





