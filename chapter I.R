# R Script - Chapter I JGA Thesis
# Date: 3/6/2022
# Date revision: 27/07/2023

# Load necessary libraries
install.packages("pacman")
pacman::p_load ("tidyverse", "grid", "gridExtra", "geodata", "factoextra", "terra", "readxl", "reshape2", "tibble", "vegan", "randomcoloR", "regclass", "unmarked", "corrplot",
                "scales", "rgeos", "cartography", "rnaturalearth", "sf", "sp", "Imap","recluster","flextable", "lme4","nlme", "aods3", "visreg", "MuMIn",
                "rgdal", "raster", "geometry", "mapmisc", "RColorBrewer","ggspatial", "ggplot2", "dplyr", "tidyr", "betapart", "ggstream", "ggrepel", "ggfortify", "ggpubr", "viridis")

# Read data
setwd("/Users/juliag.dealedo/Dropbox/Forest-diversity-JBiogr-JGA/JoB_revision/Dryad_revision")
comp <- read.csv("composition.csv")
resu <- read.csv("plot_info.csv")
complow<-comp[(comp$Forest=="Lowland Floodplain"),]
compsub<-comp[(comp$Forest=="Submontane Terra firme"),]
compfi<-comp[(comp$Forest=="Lowland Terra firme"),]
resulow<-resu[(resu$Forest=="Lowland Floodplain"),]
resusub<-resu[(resu$Forest=="Submontane Terra firme"),]
resufi<-resu[(resu$Forest=="Lowland Terra firme"),]

species_thesis_jga <- unique(comp$Species)
save(species_thesis_jga, file="species_jga.RData")
# Calculations of the data present on the manuscript
length(unique(resu$Plot)) # number of plots
aggregate(resu$Plot, list(resu$Forest), length) # number of plots per forest type
length(unique(comp$Species)) # total number of species
length(comp$Species) # total number of individuals
mean(resulow$Species)
mean(resufi$Species)
mean(resusub$Species)

# 4.2.1. Study Area
# Figure 4.2. 
# Create beta diversity sampled data frame
Regions <- levels(as.factor(comp$Region))
a1 <- vector("list", 100)
sampleplot <- vector("list", length(Regions))
names(sampleplot) <- as.character(Regions)
sampleplot_sd <- vector("list", length(Regions))
names(sampleplot_sd) <- as.character(Regions)
sampleplot_sa <- vector("list", length(Regions))
names(sampleplot_sa) <- as.character(Regions)

# Loop
for(i in levels(as.factor (comp$Region))) {
  co1 <- comp[comp$Region == i,]
  matrusp1 <- dcast(co1, Plot~Species, value.var="Species", fill=0)
  matrusp1 <- column_to_rownames(matrusp1,var="Plot")
  matrusppre1 <- (matrusp1>0)*1
  a1[[i]]<- beta.sample(matrusppre1, sites=6, samples=100)
  sampleplot[[i]] <- a1[[i]]$mean.values
  sampleplot_sd[[i]] <- a1[[i]]$sd.values
  sampleplot_sa[[i]] <- a1[[i]]$sampled.values
}

# Create beta data frame
sampleplot_table <- rownames_to_column(as.data.frame(t(as.data.frame(sampleplot))))
sampleplot_table_sd <- rownames_to_column(as.data.frame(t(as.data.frame(sampleplot_sd))))
coore <- cbind (aggregate(comp$Latitud, list(comp$Region), min), 
                aggregate(comp$Longitud, list(comp$Region), min)[,2])
sampleplot_table_co <- cbind (sampleplot_table, sampleplot_table_sd[,c(2,3,4)], coore[,c(2,3)])
colnames(sampleplot_table_co) <- c("Region", "beta.SIM", "beta.SNE", "beta.SOR", "beta.SIM.sd", 
                                   "beta.SNE.sd", "beta.SOR.sd", "Latitude", "Longitude")
sample <- sampleplot_table_co

# Sampled values data frame
sampleplot_table_sa <- as.data.frame(t(as.data.frame(sampleplot_sa)))
sampleplot_table_sa<-rownames_to_column(sampleplot_table_sa)
soren.sa <-sampleplot_table_sa[grep("SOR", sampleplot_table_sa$rowname), ]
sampledata <- soren.sa %>% pivot_longer(!rowname)

# Region coordinates not to superpose

coore <- cbind (aggregate(comp$Latitud, list(comp$Region), min), 
                aggregate(comp$Longitud, list(comp$Region), min)[,2])
colnames(coore) <- c("Region", "Latitud", "Longitud")
coore$Region[1:12] <- str_sub(coore$Region[1:12], 3)
coore$Latitud2 <- coore$Latitud + matrix(c(-1.6,-1.4,-1,-0.5,0,0,0,0,0,0,0,0,0))
Latitud123 <- sort(as.numeric(coore$Latitud2))
resu2 <- merge(coore[,c(1,4)], resu, all=TRUE)

# Prepare the map data
locations_df <- coore[,c(1:4)] 
colnames(locations_df) <- c("place", "lat", "lon", "Latitud1")
locations <- as_tibble(locations_df)
locations_sf <- st_as_sf(locations, coords = c("lon", "Latitud1"), crs = 4326)

# Prepare the elevation data
setwd("/Users/juliag.dealedo/Google Drive/Cap1_JGA-MP/data/MapData")
elev_peru<- raster("PER_alt/PER_alt.gri")
elev_bol<- raster("BOL_alt/BOL_alt.gri")
elev_ecu<- raster("ECU_alt/ECU_alt.gri")

# Set the limits of my map
countries <- readOGR("World_Countries/TM_WORLD_BORDERS-0.3.shp", stringsAsFactors = TRUE)
contries_selection <- subset(countries, name %in% c("Peru", "Bolivia", "Ecuador"))
elevation <- merge(elev_peru, elev_bol, elev_ecu)
elevation_masked <- mask(crop(elevation, contries_selection), contries_selection)
e2 <- extent(-81.35, -66, -17, 0.5)

# South America Map reference
par(bty = 'n') 
par(mar=c(0,0,0,0))
southamerica <- subset(countries, name %in% c("Venezuela", "Ecuador", "Nigaragua", "Argentina",
                                              "Chile", "Peru", "Bolivia", "Colombia", "Brazil", "Guyana", "French Guiana", "Uruguay", "Paraguay",
                                              "Suriname"))
plot(southamerica, lwd=0.4, legend=NULL)
plot(subset(countries, name == "Peru"),lwd=0.4, col="grey70", add=T)
plot(subset(countries, name == "Bolivia"), lwd=0.4, col="grey70", add=T)
plot(subset(countries, name == "Ecuador"), lwd=0.4, col="grey70", add=T)
scaleBar(countries, pt.cex = 0, bty = "n")

# Plot figure
uh <- matrix(c(1,2,3), ncol=3, byrow=T)
layout(uh)
cols_3 <- c("#BBD673", "#C56EC6", "#B3C7CA")
cols <- cols_3[factor(resu2$Forest)]
spectral <- brewer.pal(11, "Spectral")

# A- alpha
par(mar=c(4,7,5,2))
as.numeric(resu2$Latitud2)
plot(as.numeric(Latitud2) ~ Species, resu2, pch=16, cex=2, col=cols,ylab="",xlab=" ",axes=F,
     ylim=c(-17,-0))
axis(side=1,font=1, lty = 0, lwd = 1, xpd=F, line=-1)
axis(side=2,font=1, lty = 0, lwd = 1, xpd=F, line=0)
mtext("Species richness", side = 1, line =2 , cex=0.7)
mtext("Latitude", side = 2, line =3 , cex=0.7)
meank <- tapply(resu2$Species, list(resu2$Latitud2), mean)
sdk <- tapply(resu2$Species, list(resu2$Latitud2), sd)
segments(x0=meank-1.96*sdk, x1=meank+1.96*sdk, y0=Latitud123, col="black")
sp <- aggregate (comp$Species, list(comp$Region), function(x) length(unique(na.omit(x))))
points(x=meank, y=Latitud123, pch=21, col="black", bg=alpha("grey", 0.2), 
       cex=sort(sp[,2])/100)
mtext("A.", side=3, line=2, at=20, cex=1.5)

# B- map
par(bty = 'n') 
par(mar=c(0,0,0,0))
plot(crop(elevation_masked,e2),col=rev(colorRampPalette(spectral)(100)), axes=F, legend=F)
plot(crop (contries_selection, e2), add=T, ylim=c(-17,-0))
plot(locations_sf, pch = 21, bg=alpha("grey",0.8), col="black", cex = 5, add=T, ylim=c(-17,-0))
scaleBar(countries, pos = "right", pt.cex = 0, bty = "n")
legend(-81.8, -14,pch=16, cex=1.2, col=c("#BBD673", "#C56EC6", "#B3C7CA"),
       legend=c("Floodplain", "Terra Firme", "Submontane"),
       bty="n")
mtext("B.", side=3, line=-3.1, at=-80, cex=1.5)
mtext("Ecuador", side=3, line=-7, at=-78, cex=0.7)
mtext("Peru", side=3, line=-15, at=-76, cex=0.7)
mtext("Bolivia", side=3, line=-22, at=-67.3, cex=0.7)

# C- beta 
sample$Latitud2 <- coore$Latitud2
sample <- arrange (sample,Latitud2)
sampledata <- as.data.frame(sampledata)
sampledata1 <- coore %>% slice(rep(1:100, each = 100))
sampledata_lat <- cbind (sampledata, sampledata1[,c(3,4)])
sampledata_lat <- arrange (sampledata_lat, Latitud2)
par(mar=c(4,2,5,5))
plot(as.numeric(Latitud2) ~ beta.SOR, sample, pch=21, cex= sampledata$beta.SOR,ylab="",xlab=" ",
     bg=alpha(spectral, 0.5), col="black", axes=F, ylim=c(-17,-0), xlim=c(0.73,0.85))
points(as.numeric(Latitud2) ~ value, sampledata_lat, pch=16, cex=2, col="grey")
segments(x0=sample$beta.SOR-1.96*sample$beta.SOR.sd, x1=sample$beta.SOR+1.96*sample$beta.SOR.sd, y0=Latitud123, col="black")
points(as.numeric(Latitud2) ~ beta.SOR, sample, pch=21, col="black", bg=alpha("grey", 0.2), 
       cex=sample$beta.SOR*6)
axis(side=1,font=1, lty = 0, lwd = 1, xpd=F, line=-1)
mtext("Sørensen Index", side = 1, line =2, cex=0.7)
mtext("C.", side=3, line=2, at=0.72,cex=1.5)

# 4.3.1. Latitudinal variation in species richness across forest types

# Colours
cols_3 <- c("#BBD673", "#C56EC6", "#B3C7CA")
cols <- cols_3[levels(resu$Forest)]

# Create matrix
matrusp <- dcast(comp, Plot~Species, value.var="Species", fill=0)
matrusp <- column_to_rownames(matrusp,var="Plot")

# Models
for1 <- "Species ~ Latitud + (1|Region)"
for2 <- "Species ~ Latitud * Forest + (1|Region)"
for3 <- "Species ~ Latitud + Forest + (1|Region)"
for4 <- "Species ~ Latitud * Forest + I(Latitud^2) + (1|Region)" 
for5 <- "Species ~ Latitud + Forest + I(Latitud^2) + (1|Region)"
for6 <- "Species ~ Latitud * I(Latitud^2) + Forest + (1|Region)"
for7 <- "Species ~ Latitud * Forest + I(Latitud^2) * Forest + (1|Region)"
for8 <- "Species ~ Latitud + I(Latitud^2) * Forest + (1|Region)"
for8_formulation <- "Species ~ Latitude + I(Latitude^2) * Forest + (1|Region)"

# Negative binomial with negative cuadratic terms
glmer1 <- glmer.nb (for1, data = resu)
glmer2 <- glmer.nb (for2, data = resu)
glmer3 <- glmer.nb (for3, data = resu)
glmer4 <- glmer.nb (for4, data = resu)
glmer5 <- glmer.nb (for5, data = resu)
glmer6 <- glmer.nb (for6, data = resu) 
glmer7 <- glmer.nb (for7, data = resu)
glmer8 <- glmer.nb (for8, data = resu) 
AICc <- round(AIC(glmer1, glmer2, glmer3, glmer4, glmer5, glmer7, glmer8), 2)
Formulation <- c(for1, for2, for3, for4, for5, for7, for8)

# R2 of each of the models:
b <- matrix(ncol=2, nrow=7)
n <- c(glmer1, glmer2, glmer3, glmer4, glmer5, glmer7, glmer8)
for(i in n){
  r2<-round(r.squaredGLMM(i)[1,], 3) 
  b <- rbind(b,r2)
}
b<-as.data.frame(b[8:14,])
b

# Table 4.1
table1 <- cbind(Formulation, AICc, b)
Table1 <- qflextable (table1)

# Table 1a- not present on the manuscript
summary(glmer8)
anova1 <- anova(glmer8)
anova1 <- as.data.frame(anova1)
anova1 <- round(anova1,3)
anova1<- rownames_to_column(anova1)
colnames(anova1) <- c("Parameter", "npar", "SS", "MS", "F")
anova1 <- flextable(anova1, col_keys = names(anova1))

# Check residuals
par(mfcol=c(2,2))
Res <- residuals(glmer8, type="pearson")
Fit <- fitted(glmer8)
par(mfrow=c(2,2))
plot(Res ~ Fit, xlab="Fitted values", ylab="Residuals", main="Residuals vs. fitted")
abline(h=0)
plot(Res ~ resu$Latitud, xlab="NAP", ylab="Residuals", main = "NAP") 
abline(h=0)
hist(Res, main="Histogram of residuals", xlab="Residuals")
qqnorm(Res)
qqline(Res)
gof(glmer8)

# Plot alpha diversity model
# Calculate 95% confidence intervals
xv <- seq(-16,2,0.01)
newdat <- expand.grid(Latitud = xv, Forest = unique(resu$Forest))
mm <- model.matrix( ~ Latitud + I(Latitud^2)*Forest, newdat)
newdat$y <- mm%*%fixef(glmer8)
pvarf <- diag(mm %*% tcrossprod(vcov(glmer8), mm))
newdatf <- data.frame(newdat, plo = exp(newdat$y-1.96*sqrt(pvarf)), phi = exp(newdat$y+1.96*sqrt(pvarf)))
Floodplain <- subset(newdatf, Forest == "Lowland Floodplain")
Low.terra <- subset(newdatf, Forest == "Lowland Terra firme")
Low.submon <- subset(newdatf, Forest == "Submontane Terra firme")
Low.submon <- subset(Low.submon, Latitud < -8)

# Plot
uh <-  matrix(c(2,2,1,1), ncol=2, nrow=2)
layout (uh, heights=c(2,1), widths = c(0.35,1))
par(mar=c(5,1,3,2))
plot(Species ~ Latitud, resu, type="n", xlab="Latitude", ylab="", yaxt="n",ylim=c(15,225), axes=F, 
     cex.lab=2)
axis(side=1, at=c(-15,-10,-5,0),cex.axis=1.5)
cols_3 <- c("#BBD673", "#C56EC6", "#B3C7CA")
mtext("B.", side=3, line=0, at=-14.9, cex=2)
cols <- cols_3[factor(resu$Forest)]
points(Species ~ Latitud, resu, pch=21, col="black", bg=alpha(cols, .75), cex=3.6) #in case you want the points with diff sizes: cex=0.05*(resu$Families)
lines(exp(Floodplain$y) ~ Floodplain$Latitud, lwd=2, col="black")
lines(exp(Low.terra$y) ~ Low.terra$Latitud, lwd=2, col="black")
lines(exp(Low.submon$y) ~ Low.submon$Latitud, lwd=2, col="black")

polygon(x=c(Floodplain$Latitud, rev(Floodplain$Latitud)),
        y=c(Floodplain$plo, rev(Floodplain$phi)), col=alpha("#BBD673", .4), border=NA)
polygon(x=c(Low.terra$Latitud, rev(Low.terra$Latitud)),
        y=c(Low.terra$plo, rev(Low.terra$phi)), col=alpha("#C56EC6", .4), border=NA)
polygon(x=c(Low.submon$Latitud, rev(Low.submon$Latitud)),
        y=c(Low.submon$plo, rev(Low.submon$phi)), col=alpha("#B3C7CA", .4), border=NA)
legend("topleft",lty=rep(1,3), cex=1.65, lwd=3, col=c("#BBD673", "#C56EC6", "#B3C7CA"),
       legend=c("Floodplain", "Terra Firme", "Submontane"),
       bty="n")
text(-8,20,for8_formulation, cex=1.5)
par(mar=c(5,5,3,1))

# add density plot
denslow <- density(resulow$Species)
densfi <- density(resufi$Species)
denssub <- density(resusub$Species)
plot(denslow$y*-1, denslow$x, frame = FALSE, col = "white", xlab = "Density",ylab="Species richness",
     ylim=c(15,225),  cex.lab=2, cex.axis=1.5)
mtext("A.", side=3, line=0, at=-0.03, cex=2)
polygon(denslow$y*-1, denslow$x, col=alpha("#BBD673", .7))
polygon(densfi$y*-1, densfi$x, col=alpha("#C56EC6", .7), add=T)
polygon(denssub$y*-1, denssub$x, col=alpha("#B3C7CA", .7), add=T)


# 4.3.2. Latitudinal variation in species composition across forest types

# PERMANOVA analysis
matrusp <- dcast(comp, Plot~Species, value.var="Species", fill=0)
matrusp <- column_to_rownames(matrusp,var="Plot")
sp.br <- vegdist(matrusp, method="bray")
sp.br.per <- adonis2(sp.br~Latitud*Forest,data=resu)

# Table 4.2
Table4.2 <- qflextable(sp.br.per)
afss <- sp.br.per$SumOfSqs
variance <- (afss[1]/afss[5]*100)+(afss[2]/afss[5]*100)+(afss[3]/afss[5]*100)

# > NMDS per forest type

# 1- Floodplain
comp <- complow
resu <- resulow
matrusp <- dcast(comp, Plot~Species, value.var="Species", fill=0)
matrusp <- column_to_rownames(matrusp,var="Plot")
compMDSsp <- metaMDS(matrusp, distance="bray", k=2, trymax=100, autotransform=TRUE) 
compMDSsplow <- metaMDS(matrusp, distance="bray", k=2, trymax=100, autotransform=TRUE) 
NMDS1sp <- compMDSsp$points[,1] 
NMDS2sp <- compMDSsp$points[,2]

# Variation in dominant species with latitude 
# Create the envfit() results (CORRELATION RESULTS)
matrusplog <- log(matrusp + 1)
axes <- envfit(compMDSsp, matrusplog) 

# NMDS COORDINATES where species are going to be plotted
axeslow <- axes
res <- scores(axeslow, "vectors") #vegan
res <- as.data.frame(res)
res <- rownames_to_column (res, var="Species")

# Correlation into table. $vector are the CORRELATIONS of each species with the axes
axes.table <- data.frame((axes$vectors)$arrows, (axes$vectors)$r, (axes$vectors)$pvals)
axes.table <- rownames_to_column(axes.table)
colnames(axes.table) <- c("Species", "NMDS1", "NMDS2", "r", "p")

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
TableSpeciestotallow <- bold(u, i= ~ p < 0.02, bold=T, part="body") # Table 2a
forest.10low <- forest.10


# 2- Terra firme 
comp <- compfi
resu <- resufi
matrusp <- dcast(comp, Plot~Species, value.var="Species", fill=0)
matrusp <- column_to_rownames(matrusp,var="Plot")

# Create distance matrix
sp.br <- vegdist(matrusp, method="bray")
sp.br.per <- adonis2(sp.br~Latitud, data=resu)
af <- sp.br.per$aov.tab
afss <- af$SumsOfSqs
variance <- (afss[1]/afss[3]*100)

# Create NMDS axes
compMDSspfi <- metaMDS(matrusp, distance="bray", k=2, trymax=100, autotransform=TRUE) # to plot it later
NMDS1sp <- compMDSspfi$points[,1] 
NMDS2sp <- compMDSspfi$points[,2]

# Variation in dominant species with latitude 
# Create the envfit() results (CORRELATION RESULTS)
matrusplog <- log(matrusp + 1)
axes <- envfit(compMDSspfi, matrusplog) 

# NMDS COORDINATES where species are going to be plotted
axesfi <- axes
res <- scores(axes, "vectors") #vegan
res <- as.data.frame(res)
res <- rownames_to_column (res, var="Species")

# Correlation into table. $vector are the CORRELATIONS of each species with the axes
axes.table <- data.frame((axes$vectors)$arrows, (axes$vectors)$r, (axes$vectors)$pvals) # Convert result into table
axes.table <- rownames_to_column(axes.table)
colnames(axes.table) <- c("Species", "NMDS1", "NMDS2", "r", "p")
axes.table.fi <- axes.table

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
u <- qflextable(forest.10fi)
TableSpeciestotalfi <- bold(u, i= ~ p < 0.05, bold=T, part="body") # Table 2b

# 3- Submontane 
comp <- compsub
resu <- resusub
matrusp <- dcast(comp, Plot~Species, value.var="Species", fill=0)
matrusp <- column_to_rownames(matrusp,var="Plot")

# Create distance matrix
sp.br <- vegdist(matrusp, method="bray")
sp.br.per <- adonis(sp.br~Latitud,data=resu)
af <- sp.br.per$aov.tab
afss <- af$SumsOfSqs
variance <- (afss[1]/afss[3]*100)

# Create NMDS axes
compMDSsp <- metaMDS(matrusp, distance="bray", k=2, trymax=100, autotransform=TRUE) # k is the number of dimensions
compMDSspsub <- metaMDS(matrusp, distance="bray", k=2, trymax=100, autotransform=TRUE) # to plot it later
NMDS1sp <- compMDSsp$points[,1] 
NMDS2sp <- compMDSsp$points[,2]

# Create the envfit() results (CORRELATION RESULTS)
matrusplog <- log(matrusp + 1)
axes <- envfit(compMDSspsub, matrusplog) 

# NMDS COORDINATES where species are going to be plotted
axessub <- axes
res <- scores(axes, "vectors") #vegan
res <- as.data.frame(res)
res <- rownames_to_column (res, var="Species")

# Correlation into table. $vector are the CORRELATIONS of each species with the axes
axes.table <- data.frame((axes$vectors)$arrows, (axes$vectors)$r, (axes$vectors)$pvals) # Convert result into table
axes.table <- rownames_to_column(axes.table)
colnames(axes.table) <- c("Species", "NMDS1", "NMDS2", "r", "p")

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
TableSpeciestotalsub <- bold(u, i= ~ p < 0.05, bold=T, part="body") # Table 2c
forest.10sub <- forest.10

# Dominant species percentage calculation
setwd("/Users/juliag.dealedo/Dropbox/Forest-diversity-JBiogr-JGA/JoB_revision/Dryad_revision")
comp <- read.csv("composition.csv")
resu <- read.csv("plot_info.csv")
matrusp <- dcast(comp, Plot~Species, value.var="Species", fill=0)
matrusp <- column_to_rownames(matrusp,var="Plot")
abu <- arrange(as.data.frame(colSums(matrusp)),  desc(colSums(matrusp)))
abu <- as.data.frame(colSums(matrusp))
abu <- rownames_to_column(abu)
colnames(abu) <- c("Species", "abundance")
abu <- arrange(abu, desc(abundance))
sum(abu$abundance[1:129])/sum(abu$abundance)
dominant <- unique(c(forest.10low$Species, forest.10fi$Species ,forest.10sub$Species))
# %
sum(abu[abu$Species %in% dominant,]$abundance)/sum(abu$abundance)
matrusp <- dcast(compfi, Plot~Species, value.var="Species", fill=0)
matrusp <- column_to_rownames(matrusp,var="Plot")
abu <- arrange(as.data.frame(colSums(matrusp)),  desc(colSums(matrusp)))
abu <- as.data.frame(colSums(matrusp))
abu <- rownames_to_column(abu)
colnames(abu) <- c("Species", "abundance")
abu <- arrange(abu, desc(abundance))
sum(abu[abu$Species %in% forest.10fi$Species,]$abundance)/sum(abu$abundance)


# Plot results
# Create Figure 4.3 (fig 4.3a,b)
matrusp <- dcast(comp, Plot~Species, value.var="Species", fill=0)
matrusp <- column_to_rownames(matrusp,var="Plot")
low <- "#BBD673"
fi <- "#C56EC6"
sub <- "#B3C7CA"
uh <-  matrix(c(1,2,5,1,3,5,1,4,5), ncol=3, nrow=3)
layout (uh, heights=c(4,3.5,0.5))
par(mar=c(4,10,4,10))

#Total - FIG. 3A
cols_3 <- c("#BBD673", "#C56EC6", "#B3C7CA")
cols <- cols_3[factor(resu$Forest)]
sp.br.NMS<- metaMDS(matrusp, distance="bray", k=2, trymax=100, autotransform=TRUE) # k is the number of dimensions
sp.br.NMS.rot <- MDSrotate(sp.br.NMS, vec=resu$Latitud)
plot(sp.br.NMS.rot$points, pch=21, cex=0.025*resu$Species, col="black", 
     bg =alpha(cols,0.7), cex.axis=1.5, cex.lab=1.5, xlab="NMDS1", ylab="NMDS2")
ordisurf(sp.br.NMS.rot ~ resu$Latitud, col="grey40", add=TRUE, cex=0.8, labcex=0.8) 

#Floodplain - FIG. 3B
par(mar=c(3,0.5,3,0.5))
compMDSsplow.rot<-MDSrotate(compMDSsplow, vec=resulow$Latitud)
plot(compMDSsplow.rot$points, pch=21, cex=0.05*resulow$Species, col="white", bg =alpha(low,0.7), xlim=c(-2,2), 
     main="Floodplain", xlab= "", ylab="", xaxt='n', yaxt='n', cex.main=1.8)
ordisurf(compMDSsplow.rot ~ resulow$Latitud, col="grey60", add=TRUE, cex=0.2, labcex=0.8) 
take <- as.numeric(rownames(forest.10low))
cnam <- make.cepnames(forest.10low$Species)
orditorp(compMDSsplow.rot, "sp", select = take, cex=1, col="black", air=0.2)

# Terra firme - FIG. 3B
par(mar=c(3,0.5,3,0.5))
compMDSspfi.rot<-MDSrotate(compMDSspfi, vec=resufi$Latitud)
plot(compMDSspfi.rot$points, pch=21, cex=0.035*resufi$Species, col="white", bg =alpha(fi,0.7),xlim=c(-1.5,1.5), 
     main="Terra firme", xlab= "", ylab="", xaxt='n', yaxt='n', cex.main=1.8)
ordisurf(compMDSspfi.rot ~ resufi$Latitud, col="grey60", add=TRUE, cex=0.2, labcex=0.8, lwd.cl=0.9) 
takefi <- as.numeric(rownames(forest.10fi))
cnam <- make.cepnames(forest.10fi$Species)
orditorp(compMDSspfi.rot, "species", select = takefi, cex=1, col="black", air=1)

# Submontane - FIG. 3B
par(mar=c(3,0.5,3,0.5))
compMDSspsub.rot<-MDSrotate(compMDSspsub, vec=resusub$Latitud)
plot(compMDSspsub.rot$points, pch=21, cex=0.04*resusub$Species, col="white", bg =alpha(sub,0.7), xlim=c(-2.5,2), 
     main="Submontane", xlab= "", ylab="", xaxt='n', yaxt='n', cex.main=1.8)
ordisurf(compMDSspsub.rot ~ resusub$Latitud, col="grey60", add=TRUE, labcex=0.8, lwd.cl=1.1) 
take <- as.numeric(rownames(forest.10sub))
cnam <- make.cepnames(forest.10sub$Species)
orditorp(compMDSspsub.rot, "sp", select = take, cex=1, col="black", air=0.2)


# Stream graph for Figure 4.3c
setwd("/Users/juliag.dealedo/Dropbox/Forest-diversity-JBiogr-JGA/JoB_revision/Dryad_revision")
comp <- read.csv("composition.csv")
resu <- read.csv("plot_info.csv")
comp <- subset(comp,!is.na(Species))
complow<-comp[(comp$Forest=="Lowland Floodplain"),]
complow <- subset(complow,!is.na(Species))
compsub<-comp[(comp$Forest=="Submontane Terra firme"),]
compsub <- subset(compsub,!is.na(Species))
compfi<-comp[(comp$Forest=="Lowland Terra firme"),]
compfi <- subset(compfi,!is.na(Species))

# 1- Floodplain

# Create abundance table
comp <- complow 
matru <- dcast(comp, Plot~Species, value.var="Species", fill=0) # create the matrix
matru <- column_to_rownames(matru,var="Plot")
abu <- arrange(as.data.frame(colSums(matru)),  desc(colSums(matru))) # calculate abundance
abu <- rownames_to_column(abu)
colnames(abu) <- c("Species", "Abundance")
abu2 <- abu[1:10,] # Choose the 10 most abundant species
comp2 <- subset(comp, Species %in% abu2$Species) # get the latitude of each species
comp2$Latitud = factor(comp2$Latitud) # convert latitude to factor
comp3 <- comp2 %>% count(Species, Latitud, .drop=FALSE) # count the abundance of each species per latitude
comp4 <- arrange(comp3, desc(Latitud)) # arrange the order
max(comp4$n)
new.comp4 <- data.frame(Species=NULL, Latitud=NULL, n=NULL)
for(i in unique(comp4$Species)) {
  temp <- comp4[comp4$Species==i,]
  temp$Latitud <- as.numeric(as.character(temp$Latitud))
  ls1 <- smooth.spline(temp$Latitud, temp$n, df=4)
  temp.df <- data.frame(Species=i, Latitud=seq(-15,0,0.2), 
                        n=predict(ls1, x=seq(-15,0,0.2))$y)
  new.comp4 <- rbind(new.comp4, temp.df)  
}
new.comp4$n <- ifelse(new.comp4$n<0, 0, new.comp4$n)
max(new.comp4$n)
comp6 <- arrange(new.comp4, Latitud) # arrange the order

# same species with the same colour, by hand:
colow <- c("#fbe183" ,"#fe9b00", "#9b3441", "#e87b89", "#aa7aa1" ,"#9f5691", "#633372", "#1f6e9c" ,"#2b9b81", "#92c051")
cofi <- c("#fbe183" ,"#fe9b00", "#aa7aa1", "#e87b89", "#9b3441" ,"#9f5691", "#2b9b81", "#633372" ,"#1f6e9c", "#92c051")
cosubm <- c("#fbe183" ,"#fe9b00", "#9b3441", "#aa7aa1", "#e87b89" ,"#9f5691", "#633372", "#2b9b81" ,"#1f6e9c", "#92c051")

# Stream graph floodplain
plow <- ggplot(comp6, aes(x = Latitud, y = n, fill = Species)) + 
  geom_stream(extra_span = 0.005, type = "mirror", n_grid = 3000,  bw = 1, lwd=0.2, color="black")+
  scale_fill_manual(values = colow) + theme_classic()+
  geom_stream_label(aes(label = Species), size=8)+
  scale_y_continuous(limits = c(-80, 120))+
  labs(x = "Latitude", y = "Abundance")+
  theme(
    axis.text.x = element_text(face=NULL, color="black",size=20, angle=0), axis.title=element_text(size=20),
    axis.text.y = element_blank(), 
    axis.title.y = element_blank(),axis.line.y=element_blank(), axis.ticks.y = element_blank(),
    legend.position = "none", 
    plot.title = element_text(size = 25, face = "bold", family=".New York", colour="black"),
    plot.subtitle = element_text(size = 20, face = "bold", family=".New York", colour="black"))
plow


# 2 - Terra firme

# Create abundance table
comp <- compfi 
matru <- dcast(comp, Cod_plot~Species, value.var="Species", fill=0) 
matru <- column_to_rownames(matru,var="Cod_plot")
abu <- arrange(as.data.frame(colSums(matru)),  desc(colSums(matru))) 
abu <- rownames_to_column(abu)
colnames(abu) <- c("Species", "Abundance")
abu2 <- abu[1:10,] # Choose the 10 most abundant species
comp2 <- subset(comp, Species %in% abu2$Species) # get the latitude of each species
comp2$Latitud = factor(comp2$Latitud) # convert latitude to factor
comp3 <- comp2 %>% count(Species, Latitud, .drop=FALSE) # count the abundance of each species per latitude
comp4 <- arrange(comp3, desc(Latitud)) # arrange the order
new.comp4 <- data.frame(Species=NULL, Latitud=NULL, n=NULL)
for(i in unique(comp4$Species)) {
  temp <- comp4[comp4$Species==i,]
  temp$Latitud <- as.numeric(as.character(temp$Latitud))
  ls1 <- smooth.spline(temp$Latitud, temp$n, df=6)
  temp.df <- data.frame(Species=i, Latitud=seq(-15,0,0.2), 
                        n=predict(ls1, x=seq(-15,0,0.2))$y)
  new.comp4 <- rbind(new.comp4, temp.df)  
}
new.comp4$n <- ifelse(new.comp4$n<0, 0, new.comp4$n)
max(new.comp4$n)
comp6 <- arrange(new.comp4, Latitud) # arrange the order

# Stream graph Terra firme
pfi <- ggplot(comp6, aes(x = Latitud, y = n, fill = Species)) + xlim(-16, 0)+
  geom_stream(extra_span = 0.005, type = "mirror", n_grid = 3000,  bw = 1, lwd=0.2, color="black")+
  geom_stream_label(aes(label = Species), size=8)+
  scale_fill_manual(values = cofi) + theme_classic() +
  labs(x = "Latitude", y = "Abundance")+
  theme(
    axis.text.x = element_text(face=NULL, color="black",  size=20, angle=0), axis.title=element_text(size=20),
    axis.text.y = element_blank(), axis.title.y=element_blank(), axis.line.y=element_blank(), axis.ticks.y = element_blank(),
    legend.position = "none", 
    plot.title = element_text(size = 25, face = "bold", family=".New York", colour="black"),
    plot.subtitle = element_text(size = 20, face = "bold", family=".New York", colour="black"))+
  geom_segment(aes(x = -4, y = 50, xend = -4, yend = 100),size=1,lineend="round",)+
  geom_segment(aes(x = -4.5, y = 100, xend = -3.5, yend = 100), size=1,lineend="round")+ 
  geom_segment(aes(x = -4.5, y = 50, xend = -3.5, yend = 50), size=1,lineend="round")+
  geom_text(label = "50 individuals", x = -1.5, y = 75, size=7)
pfi

# 3- Submontane
# Create abundance table
comp <- compsub 
matru <- dcast(comp, Cod_plot~Species, value.var="Species", fill=0) # create the matrix
matru <- column_to_rownames(matru,var="Cod_plot")
abu <- arrange(as.data.frame(colSums(matru)),  desc(colSums(matru))) # calculate abundance
abu <- rownames_to_column(abu)
colnames(abu) <- c("Species", "Abundance")
abu2 <- abu[1:10,] # Choose the 10 most abundant species
comp2 <- subset(comp, Species %in% abu2$Species) # get the latitude of each species
comp2$Latitud = factor(comp2$Latitud) # convert latitude to factor
comp3 <- comp2 %>% count(Species, Latitud, .drop=FALSE) # count the abundance of each species per latitude
comp4 <- arrange(comp3, desc(Latitud)) # arrange the order
new.comp4 <- data.frame(Species=NULL, Latitud=NULL, n=NULL)
for(i in unique(comp4$Species)) {
  temp <- comp4[comp4$Species==i,]
  temp$Latitud <- as.numeric(as.character(temp$Latitud))
  ls1 <- smooth.spline(temp$Latitud, temp$n, df=3)
  temp.df <- data.frame(Species=i, Latitud=seq(-14.3,-9,0.2), 
                        n=predict(ls1, x=seq(-14.3,-9,0.2))$y)
  new.comp4 <- rbind(new.comp4, temp.df)  
}
new.comp4$n <- ifelse(new.comp4$n<0, 0, new.comp4$n)
comp6 <- arrange(new.comp4, Latitud) 

# Stream graph submontane
psub <- ggplot(comp6, aes(x = Latitud, y = n, fill = Species)) + xlim(-14.5,-9)+
  geom_stream(extra_span = 0.005, type = "mirror", n_grid = 3000,  bw = 1, lwd=0.2, color="black")+
  scale_fill_manual(values = cosubm) + theme_classic()+
  geom_stream_label(aes(label = Species), size=8)+
  scale_y_continuous(limits = c(-80, 120))+
  labs(x = "Latitude", y = "Abundance")+
  theme(
    axis.text.x = element_text(face=NULL, color="black",  size=20, angle=0), axis.title=element_text(size=20),
    axis.text.y = element_blank(), axis.title.y=element_blank(), axis.line.y=element_blank(), axis.ticks.y = element_blank(),
    legend.position = "none", 
    plot.title = element_text(size = 25, face = "bold", family=".New York", colour="black"),
    plot.subtitle = element_text(size = 20, face = "bold", family=".New York", colour="black"),
    plot.margin = margin(20,20,20,20), plot.caption.position = "plot")
psub

# JOIN - FIG 3C
p <- grid.arrange(plow, pfi, psub, ncol = 3, nrow = 1)