# R Script -  Supplementary material JGA Thesis
# Date revision: 27/07/2023

# Appendix table S1 ####
Country <- c(rep("Bolivia", 44), rep ("Peru", 50), rep ("Ecuador", 18), rep ("Peru", 6))
resu$Region <- paste(Country, resu$Region, sep=" ")
resu.resu <- resu[c(8,1,2,4:7)]
supertable <- qflextable(resu.resu)
print(supertable, preview = "docx")

# Appendix table S2 and figure S2 ####
comp <- read.csv("composition.csv")
resu <- read.csv("plot_info.csv")
comp <- subset(comp,!is.na(Species))
complow<-comp[(comp$Forest=="Lowland Floodplain"),]
complow <- subset(complow,!is.na(Species))
compsub<-comp[(comp$Forest=="Submontane Terra firme"),]
compsub <- subset(compsub,!is.na(Species))
compfi<-comp[(comp$Forest=="Lowland Terra firme"),]
compfi <- subset(compfi,!is.na(Species))

## FLOODPLAIN ##
# Create abundance table
comp <- complow # change data name for each of the data frames
matru <- dcast(comp, Cod_plot~Species, value.var="Species", fill=0) # create the matrix
matru <- column_to_rownames(matru,var="Cod_plot")
abu <- arrange(as.data.frame(colSums(matru)),  desc(colSums(matru))) # calculate abundance
abu <- rownames_to_column(abu)
colnames(abu) <- c("Species", "Abundance")
abu2 <- abu[1:57,]

# Table S2 Floodplain
axes.table.low <- data.frame((axeslow$vectors)$arrows, (axeslow$vectors)$r, (axeslow$vectors)$pvals) # Convert result into table
axes.table.low <- rownames_to_column(axes.table.low)
colnames(axes.table.low) <- c("Species", "NMDS1", "NMDS2", "r", "p")
tablelow <- subset (axes.table.low, Species %in% abu2$Species)
sum((tablelow$p<0.01)*1)/57
ulow <- flextable(tablelow[,c(1,5)])
bold(ulow, i= ~ p < 0.01, bold=T, part="body")
print(ulow, preview="docx")
# Prepare to plot
comp2 <- subset(comp, Species %in% abu2$Species) # get the latitude of each species
comp2$Latitud = factor(comp2$Latitud) # convert latitude to factor
comp3 <- comp2 %>% count(Species, Latitud, .drop=FALSE) # count the abundance of each species per latitude
comp4 <- arrange(comp3, desc(Latitud)) # arrange the order
max(comp4$n)
new.comp4 <- data.frame(Species=NULL, Latitud=NULL, n=NULL)
for(i in unique(comp4$Species)) {
  temp <- comp4[comp4$Species==i,]
  temp$Latitud <- as.numeric(as.character(temp$Latitud))
  ls1 <- smooth.spline(temp$Latitud, temp$n, df=3)
  temp.df <- data.frame(Species=i, Latitud=seq(-15,-0.5,0.5), 
                        n=predict(ls1, x=seq(-15,-0.5,0.5))$y)
  new.comp4 <- rbind(new.comp4, temp.df)  
}
new.comp4$n <- ifelse(new.comp4$n<0, 0, new.comp4$n)
max(new.comp4$n)
comp6 <- arrange(new.comp4, Latitud) # arrange the order

# Prepare the colours
spectrallow <- distinctColorPalette(10) # 10 species
sppsub <- unique(comp6$Species)
cosub <- cbind(spectral, sppsub)
cols <- cbind(cosub, cofi, colow)
# same species with the same colour, by hand:
colow <- c("#DDCE61", "#D5DCBE", "#77C5D5", "#828BDC" ,"#D7BBD4" ,"#D68062" ,"#D572B5", "#93E55E" ,"#BE4EDF" ,"#88DDAA")

# the actual plot
colow2 <- gray(seq(0,.9,len = 57))
colow2 <- viridis(57)
plow <- ggplot(comp6, aes(x = Latitud, y = n, fill = Species))+ 
  geom_stream(extra_span = 0.005, type = "mirror", n_grid = 3000,  bw =1, lwd=0.2, color="black")+
  scale_fill_manual(values = colow2) + theme_classic()+
  scale_y_continuous(limits = c(-150, 150))+
  labs(x = "Latitude", y = "Abundance")+
  theme(
    axis.text.x = element_text(face=NULL, color="black",size=20, angle=0), axis.title=element_text(size=20),
    axis.text.y = element_blank(), 
    axis.title.y = element_blank(),axis.line.y=element_blank(), axis.ticks.y = element_blank(),
    legend.position = "none", 
    plot.title = element_text(size = 25, face = "bold", family=".New York", colour="black"),
    plot.subtitle = element_text(size = 20, face = "bold", family=".New York", colour="black"),
    plot.margin = margin(20,20,20,20), plot.caption.position = "plot")
plow


## TERRA FIRME ##

# Create abundance table
comp <- compfi # change data name for each of the data frames
matru <- dcast(comp, Plot~Species, value.var="Species", fill=0) # create the matrix
matru <- column_to_rownames(matru,var="Plot")
abu <- arrange(as.data.frame(colSums(matru)),  desc(colSums(matru))) # calculate abundance
abu <- rownames_to_column(abu)
colnames(abu) <- c("Species", "Abundance")
abu2 <- abu[1:107,] # Choose the 10 most abundant species
sum(abu2$Abundance)/sum(abu$Abundance)
# Table S2 Terra firme
axes.table.fi <- data.frame((axesfi$vectors)$arrows, (axesfi$vectors)$r, (axesfi$vectors)$pvals) # Convert result into table
axes.table.fi <- rownames_to_column(axes.table.fi)
colnames(axes.table.fi) <- c("Species", "NMDS1", "NMDS2", "r", "p")
axes.table.fi
tablefi <- subset (axes.table.fi, Species %in% abu2$Species)
sum((tablefi$p<=0.01)*1)/length(tablefi$Species)
ufi <- qflextable(tablefi[,c(1,5)])
ufi <- bold(ufi, i= ~ p <= 0.01, bold=T, part="body")
print(ufi, preview="docx")
# Prepare plot
comp2 <- subset(comp, Species %in% abu2$Species) # get the latitude of each species
comp2$Latitud = factor(comp2$Latitud) # convert latitude to factor
comp3 <- comp2 %>% count(Species, Latitud, .drop=FALSE) # count the abundance of each species per latitude
comp4 <- arrange(comp3, desc(Latitud)) # arrange the order
max(comp4$n)
new.comp4 <- data.frame(Species=NULL, Latitud=NULL, n=NULL)
for(i in unique(comp4$Species)) {
  temp <- comp4[comp4$Species==i,]
  temp$Latitud <- as.numeric(as.character(temp$Latitud))
  ls1 <- smooth.spline(temp$Latitud, temp$n, df=2)
  temp.df <- data.frame(Species=i, Latitud=seq(-15,-0.5,0.5), 
                        n=predict(ls1, x=seq(-15,-0.5,0.5))$y)
  new.comp4 <- rbind(new.comp4, temp.df)  
}
new.comp4$n <- ifelse(new.comp4$n<0, 0, new.comp4$n)
max(new.comp4$n)
comp6 <- arrange(new.comp4, Latitud) # arrange the order

# Prepare the colours
# the actual plot
levels(comp6$Latitud)
cofi2 <- gray(seq(0,.9,len = 100))
cofi2 <- viridis (109)
pfi <- ggplot(comp6, aes(x = Latitud, y = n, fill = Species)) + xlim(-15, 0)+
  geom_stream(extra_span = 0.005, type = "mirror", n_grid = 3000,  bw = 1, lwd=0.2, color="black")+
  scale_fill_manual(values = cofi2) + theme_classic()+
  scale_y_continuous(limits = c(-200, 200))+
  labs(x = "Latitude", y = "Abundance")+
  theme(
    axis.text.x = element_text(face=NULL, color="black",  size=20, angle=0), axis.title=element_text(size=20),
    axis.text.y = element_blank(), axis.title.y=element_blank(), axis.line.y=element_blank(), axis.ticks.y = element_blank(),
    legend.position = "none", 
    plot.title = element_text(size = 25, face = "bold", family=".New York", colour="black"),
    plot.subtitle = element_text(size = 20, face = "bold", family=".New York", colour="black"),
    plot.margin = margin(20,20,20,20), plot.caption.position = "plot")+
  geom_segment(aes(x = -4, y = 100, xend = -4, yend = 150),size=1,lineend="round",)+
  geom_segment(aes(x = -4.5, y = 150, xend = -3.5, yend = 150), size=1,lineend="round")+ 
  geom_segment(aes(x = -4.5, y = 100, xend = -3.5, yend = 100), size=1,lineend="round")+
  geom_text(label = "50 individuals", x = -1.5, y = 125, size=7)

pfi


## SUBMONTANE ##

# Create abundance table
comp <- compsub # change data name for each of the data frames
matru <- dcast(comp, Cod_plot~Species, value.var="Species", fill=0) # create the matrix
matru <- column_to_rownames(matru,var="Cod_plot")

abu <- arrange(as.data.frame(colSums(matru)),  desc(colSums(matru))) # calculate abundance
abu <- rownames_to_column(abu)
colnames(abu) <- c("Species", "Abundance")
abu2 <- abu[1:80,] # Choose the 10 most abundant species

# Table S2 Submontane
axes.table.sub <- data.frame((axessub$vectors)$arrows, (axessub$vectors)$r, (axessub$vectors)$pvals) # Convert result into table
axes.table.sub <- rownames_to_column(axes.table.sub)
colnames(axes.table.sub) <- c("Species", "NMDS1", "NMDS2", "r", "p")
tablesub <- subset (axes.table.sub, Species %in% abu2$Species)
sum((tablesub$p<0.01)*1)/80
usub <- flextable(tablesub[,c(1,5)])
bold(usub, i= ~ p < 0.01, bold=T, part="body")

print(usub, preview="docx")

# Prepare plot
comp2 <- subset(comp, Species %in% abu2$Species) # get the latitude of each species
comp2$Latitud = factor(comp2$Latitud) # convert latitude to factor
comp3 <- comp2 %>% count(Species, Latitud, .drop=FALSE) # count the abundance of each species per latitude
comp4 <- arrange(comp3, desc(Latitud)) # arrange the order
new.comp4 <- data.frame(Species=NULL, Latitud=NULL, n=NULL)
for(i in unique(comp4$Species)) {
  temp <- comp4[comp4$Species==i,]
  temp$Latitud <- as.numeric(as.character(temp$Latitud))
  ls1 <- smooth.spline(temp$Latitud, temp$n, df=2)
  temp.df <- data.frame(Species=i, Latitud=seq(-14.3,-8.8,0.5), 
                        n=predict(ls1, x=seq(-14.3,-8.8,0.5))$y)
  new.comp4 <- rbind(new.comp4, temp.df)  
}
new.comp4$n <- ifelse(new.comp4$n<0, 0, new.comp4$n)

comp6 <- arrange(new.comp4, Latitud) # arrange the order


# the actual plot
levels(comp6$Latitud)
cosub2 <- viridis(80)
psub <- ggplot(comp6, aes(x = Latitud, y = n, fill = Species)) + xlim(-14.2,-9)+
  geom_stream(extra_span = 0.005, type = "mirror", n_grid = 3000,  bw = 1.2, lwd=0.2, color="black")+
  scale_fill_manual(values = cosub2) + theme_classic()+#cowplot::theme_minimal_hgrid(font_size = 14) +
  scale_y_continuous(limits = c(-250, 250))+
  labs(x = "Latitude", y = "Abundance")+
  theme(
    axis.text.x = element_text(face=NULL, color="black",  size=20, angle=0), axis.title=element_text(size=20),
    axis.text.y = element_blank(), axis.title.y=element_blank(), axis.line.y=element_blank(), axis.ticks.y = element_blank(),
    legend.position = "none", 
    plot.title = element_text(size = 25, face = "bold", family=".New York", colour="black"),
    plot.subtitle = element_text(size = 20, face = "bold", family=".New York", colour="black"),
    plot.margin = margin(20,20,20,20), plot.caption.position = "plot")

psub

## JOIN - Fig S2
p <- grid.arrange(plow, pfi, psub, ncol = 3, nrow = 1)
ggsave("RG_pruebaS1.jpeg", p, width = 30, height = 10, scale=0.7)


# Appendix figure S3 (alpha model2) ####
# Colours
cols_3 <- c("#BBD673", "#C56EC6", "#B3C7CA")
cols <- cols_3[levels(resu$Forest)]

# Create matrix
matrusp <- dcast(comp, Plot~Species, value.var="Species", fill=0)
matrusp <- column_to_rownames(matrusp,var="Plot")
matrusppre <- (matrusp>0)*1

# Model

# Packages

# models
for1 <- "Species ~ Latitud + (1|Region)"
for2 <- "Species ~ Latitud * Forest + (1|Region)"
for3 <- "Species ~ Latitud + Forest + (1|Region)"
for4 <- "Species ~ Latitud * Forest + I(Latitud^2) + (1|Region)" 
for5 <- "Species ~ Latitud + Forest + I(Latitud^2) + (1|Region)"
for6 <- "Species ~ Latitud * I(Latitud^2) + Forest + (1|Region)"
for7 <- "Species ~ Latitud * Forest + I(Latitud^2) * Forest + (1|Region)"
for8 <- "Species ~ Latitud + I(Latitud^2) * Forest + (1|Region)"

# Negative binomial with negative cuadratic terms
glmer1 <- glmer.nb (for1, data = resu)
glmer2 <- glmer.nb (for2, data = resu)
glmer3 <- glmer.nb (for3, data = resu)
glmer4 <- glmer.nb (for4, data = resu)
glmer5 <- glmer.nb (for5, data = resu)
glmer6 <- glmer.nb (for6, data = resu) #no
glmer7 <- glmer.nb (for7, data = resu)
glmer8 <- glmer.nb (for8, data = resu) 
AICc <- round(AIC(glmer1, glmer2, glmer3, glmer4, glmer5, glmer7, glmer8), 2)
Formulation <- c(for1, for2, for3, for4, for5, for7, for8)
for7_formulation <- "Species ~ Latitude * Forest + I(Latitude^2) * Forest + (1|Region)"

#to get the R2 of each of the models:
b <- matrix(ncol=2, nrow=7)
n <- c(glmer1, glmer2, glmer3, glmer4, glmer5, glmer7, glmer8)
for(i in n){
  r2<-round(r.squaredGLMM(i)[1,], 3) 
  b <- rbind(b,r2)
}
b<-as.data.frame(b[8:14])
b
# Table 1 - present on the manuscript
table1 <- cbind(Formulation, AICc, b)
Table1 <- qflextable (table1)

# Table 1a- not present on the manuscript
summary(glmer7)
anova1 <- anova(glmer7)
anova1 <- as.data.frame(anova1)
anova1 <- round(anova1,3)
anova1<- rownames_to_column(anova1)
colnames(anova1) <- c("Parameter", "npar", "SS", "MS", "F")
anova1 <- flextable(anova1, col_keys = names(anova1))

# Check residuals
par(mfcol=c(2,2))
Res <- residuals(glmer7, type="pearson")
Fit <- fitted(glmer7)
par(mfrow=c(2,2))
plot(Res ~ Fit, xlab="Fitted values", ylab="Residuals", main="Residuals vs. fitted")
abline(h=0)
plot(Res ~ resu$Latitud, xlab="NAP", ylab="Residuals", main = "NAP") > abline(h=0)
hist(Res, main="Histogram of residuals", xlab="Residuals")
qqnorm(Res)
qqline(Res)
gof(glmer7)

# Plot alpha diversity model

# To calculate 95% confidence intervals
xv <- seq(-16,2,0.01)
newdat <- expand.grid(Latitud = xv, Forest = unique(resu$Forest))
mm <- model.matrix( ~ Latitud * Forest + I(Latitud^2) * Forest, newdat)
newdat$y <- mm%*%fixef(glmer7) #%*% matrix multiplication

pvarf <- diag(mm %*% tcrossprod(vcov(glmer7), mm)) #diag() = construstruc a diagonal matrix, crossprod = %*%, vcov = variance matrix of the main parameters of a fitted model object
newdatf <- data.frame(newdat, plo = exp(newdat$y-1.96*sqrt(pvarf)), phi = exp(newdat$y+1.96*sqrt(pvarf)))
Floodplain <- subset(newdatf, Forest == "Lowland Floodplain")
Low.terra <- subset(newdatf, Forest == "Lowland Terra firme")
Low.submon <- subset(newdatf, Forest == "Submontane Terra firme")
Low.submon <- subset(Low.submon, Latitud < -8)

# Save plot

uh <-  matrix(c(2,2,1,1), ncol=2, nrow=2)
uh
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
text(-8,20,for7_formulation, cex=1.5)
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

# Appendix figure S4 (PCoA) ####

cols_3 <- c("#BBD673", "#C56EC6", "#B3C7CA")
cols <- cols_3[factor(resu$Forest)]
cols_reg <- grid.col[factor(resu$Region)]

matrusp <- dcast(comp, Plot~Species, value.var="Species", fill=0)
matrusp <- column_to_rownames(matrusp,var="Plot")
sp.br <- vegdist(matrusp, method="bray")

par(mfcol=c(1,2))
my_pcoa <- stats:::cmdscale(sp.br)
plot(my_pcoa, pch=21, col="black", bg =alpha(cols,0.7))
axis1 <-as.data.frame( my_pcoa[,1])
resu_pcoa <- cbind(resu$Plot, resu$Latitud, axis1, resu$Region, resu$Forest)
colnames(resu_pcoa) <- c("Plot", "Latitud", "axis1PCoA", "Region", "Forest")
lme0 <- lme(axis1PCoA ~ 1, random = ~ 1 | Region, method="ML", resu_pcoa)
lme1 <- lme(axis1PCoA ~ Latitud, random = ~ 1 | Region,  method="ML", resu_pcoa)
lme2 <- lme(axis1PCoA ~ Latitud + Forest, random = ~ 1 | Region,  method="ML", resu_pcoa)
lme3 <- lme(axis1PCoA ~ Latitud * Forest, random = ~ 1 | Region, method="ML" , resu_pcoa)
table.answer.3 <- as.data.frame(anova(lme0, lme1, lme2, lme3))

qtable <- qflextable(table.answer.3)
round(MuMIn::AICc(lme0, lme1, lme2, lme3), 2)

#Residuals
Res <- residuals(lme3, type="pearson")
Fit <- fitted(lme3)
par(mfrow=c(2,2))
plot(Res ~ Fit, xlab="Fitted values", ylab="Residuals", main="Residuals vs. fitted")
abline(h=0)
plot(Res ~ resu_pcoa$Latitud, xlab="NAP", ylab="Residuals", main = "NAP") > abline(h=0)
hist(Res, main="Histogram of residuals", xlab="Residuals")
qqnorm(Res)
qqline(Res)
gof(lme3)

# To calculate 95% confidence intervals
xv <- seq(-16,2,0.01)
newdat <- expand.grid(Latitud = xv, Forest = unique(resu$Forest))
mm <- model.matrix( ~ Latitud * Forest, newdat)
newdat$y <- mm%*%fixef(lme3) 

pvarf <- diag(mm %*% tcrossprod(vcov(lme3), mm)) 
newdatf <- data.frame(newdat, plo = newdat$y-1.96*sqrt(pvarf), phi = newdat$y+1.96*sqrt(pvarf))
Floodplain <- subset(newdatf, Forest == "Lowland Floodplain")
Low.terra <- subset(newdatf, Forest == "Lowland Terra firme")
Low.submon <- subset(newdatf, Forest == "Submontane Terra firme")
Low.submon <- subset(Low.submon, Latitud < -8)

lmm_pcoa_interaction <-  ggplot(resu_pcoa , colour=Forest, group=Forest) + 
  geom_point(aes(x = Latitud, y = axis1PCoA, group=Forest), fill=cols, colour= "black",pch=21, size=4, alpha=.6)+
  geom_line(data=Floodplain, aes(x = Latitud, y = y, colour= "#B3C7CA"))+
  geom_ribbon(data=Floodplain, aes(x=Latitud, y = y, ymin=plo, ymax=phi), alpha = 0.05)+
  geom_line(data=Low.terra, aes(x = Latitud, y = y, colour= "#BBD673"))+
  geom_ribbon(data=Low.terra, aes(x=Latitud, y = y, ymin=plo, ymax=phi), alpha = 0.05)+
  geom_line(data=Low.submon, aes(x = Latitud, y = y, colour= "#C56EC6"))+
  geom_ribbon(data=Low.submon, aes(x=Latitud, y = y, ymin=plo, ymax=phi), alpha = 0.05)+
  labs(x = "Latitude", y = "PCoA axis 1")+
  scale_colour_manual("", labels = c("Floodplain", "Terra firme", "Submontane"),
                      values = c("#BBD673", "#C56EC6", "#B3C7CA")) +
  theme_classic()
lmm_pcoa_interaction

ggsave("figureS4.svg", scale=0.6)
# Appendix figure S1 (Analysis latitude correlation with CHELSEA and SOILGRID data) ####

# 1. SOIL DATA from SOIL GRID
setwd ("/Users/juliag.dealedo/Google Drive/Cap1_JGA-MP/revision1_drive/soils")

ph <-raster("phh2o_0-5cm_mean_30s.tif")
soc <-raster("soc_0-5cm_mean_30s.tif")
nitrogen <-raster("nitrogen_0-5cm_mean_30s.tif")
clay <-raster("clay_0-5cm_mean_30s.tif")
sand <- raster("sand_0-5cm_mean_30s.tif")
silt <- raster("silt_0-5cm_mean_30s.tif")
cfvo <- raster("cfvo_0-5cm_mean_30s.tif")

locations_df <- resu[,c(5:4)] # rename 
colnames(locations_df) <- c( "lon", "lat")
locations <- as_tibble(locations_df)
locations_sf <- st_as_sf(locations, coords = c("lon", "lat"), crs = 4326)

plots_ph <- raster::extract(ph, locations_df, df = TRUE)  
plots_soc <- raster::extract(soc, locations_df, df = TRUE)  
plots_nitrogen <- raster::extract(nitrogen, locations_df, df = TRUE)
plots_clay <- raster::extract(clay, locations_df, df = TRUE)
plots_cfvo <- raster::extract(cfvo, locations_df, df = TRUE)
plots_sand <- raster::extract(sand, locations_df, df = TRUE)  
plots_silt <- raster::extract(silt, locations_df, df = TRUE)  

resu_soil <-cbind (resu, plots_ph, plots_soc, plots_nitrogen, plots_clay, plots_sand[,2], plots_silt[,2],
                   plots_cfvo[,2])
resu_soil_simple <- resu_soil[,c(9,11,13,15:18)]
colnames(resu_soil_simple) <- c("ph", "soc", "nitrogen", "clay", "sand", "silt", "cfvo")

# 2. CLIMATIC DATA from CHELSA

# download the 19 climatic variables from https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V1/climatologies/bio
# consider use the function: BIO01CHELSA <- download.file("https://os.zhdk.cloud.switch.ch/envicloud/chelsa/chelsa_V1/climatologies/bio/CHELSA_bio10_01.tif ", "CHELSA_bio10_01.tif")

chelsa_list <-list(BIO01CHELSA, BIO02CHELSA, BIO03CHELSA, BIO04CHELSA, BIO05CHELSA, BIO06CHELSA, 
                   BIO07CHELSA, BIO08CHELSA, BIO09CHELSA, BIO10CHELSA, BIO11CHELSA, BIO12CHELSA,
                   BIO13CHELSA, BIO14CHELSA, BIO15CHELSA, BIO16CHELSA, BIO17CHELSA, BIO18CHELSA, BIO19CHELSA)
cclim <- stack(chelsa_list)
locations <- as_tibble(locations_df)
species_points_cclim <- raster::extract(cclim, locations_df, df = TRUE) 
species_points_cclim_units <-mutate(species_points_cclim,Bio1 = CHELSA_bio10_01/10,Bio2 = CHELSA_bio10_02/10,Bio3 = CHELSA_bio10_03/10,Bio4 = CHELSA_bio10_04/10,
                                    Bio5 = CHELSA_bio10_05/10, Bio6 = CHELSA_bio10_06/10,Bio7 = CHELSA_bio10_07/10,Bio8 = CHELSA_bio10_08/10,
                                    Bio9 = CHELSA_bio10_09/10,Bio10 = CHELSA_bio10_10/10, Bio11 = CHELSA_bio10_11/10,Bio12 = CHELSA_bio10_12, 
                                    Bio13 = CHELSA_bio10_13,Bio14 = CHELSA_bio10_14,Bio15 = CHELSA_bio10_15,
                                    Bio16 = CHELSA_bio10_16, Bio17 = CHELSA_bio10_17, Bio18 = CHELSA_bio10_18, Bio19 = CHELSA_bio10_19)
species_points_cclim_units <- species_points_cclim_units[,21:39] # correct units
colnames(species_points_cclim_units) <- wnames[,-1]
cclim_resu <-cbind (resu, species_points_cclim_units)
CLIM<-cbind(cclim_resu, resu_soil_simple)
head(CLIM)
write.table (CLIM, "CLIM")

CLIM <- read.table ("CLIM")
CLIM <- as_tibble(CLIM)

# Choosing the parameters 
divcor <- CLIM[,c(5,8:33)]
str(divcor)

# Matrix of the p-value of the correlation
p.mat <- cor.mtest(divcor)
M<-cor(divcor, use="pairwise.complete.obs")
p.mat <- cor.mtest(divcor)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
# Calculate the correlation
cclim_correlation <- corrplot(M, method="color", col=col(200),  
                              type="upper", order="original", 
                              addCoef.col = "black", # Add coefficient of correlation
                              # Combine with significance
                              #  p.mat = p.mat, sig.level = 0.02, insig = "blank", 
                              # hide correlation coefficient on the principal diagonal
                              diag=F)
dev.off()
# PCA

df <- divcor
df_pca <- prcomp(divcor)
df_out <- as.data.frame(df_pca$x)
pca.plot <- autoplot(df_pca, divcor)
comps <- pca.plot$data[,c(1:29)]
cor(comps[,3:29], comps[,c(1:2)]) # Pearson correlations between climatic and edaphic variables and PCA1 added in the ms
df_out$group <- sapply (strsplit(as.character(row.names(df)), "_"), "[[", 1)
head(df_out)
p<-ggplot(df_out,aes(x=PC1,y=PC2))
p<-p+geom_point()
pca_res <- prcomp(df, scale. = TRUE)
fviz_pca_var(pca_res, col.var = "black")
autoplot(df_out, data = divcor, colour = 'Species')
print(pca_res)
get_eigenvalue(pca_res)
fviz_eig(pca_res, addlabels = TRUE)
var <- get_pca_var(pca_res)
corrplot(var$cos2, is.corr=FALSE)
fviz_cos2(pca_res, choice = "var", axes = 1:2)
fviz_contrib(pca_res, choice = "var", axes = 1, top = 20)
fviz_contrib(pca_res, choice = "var", axes = 2, top = 10)
fviz_contrib(pca_res, choice = "var", axes = 1:2, top = 30)

CLIM_PCA_corr <- fviz_pca_var(pca_res, col.var = "cos2",
                              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                              repel = TRUE # Avoid text overlapping
)
fviz_pca_ind(pca_res, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)


CLIM_PCA <- fviz_pca_biplot(pca_res, repel = TRUE,
                            col.var = "#2E9FDF", # Variables color
                            col.ind  = resu$Region  # Individuals color,
                            
)

countries <- readOGR("World_Countries/TM_WORLD_BORDERS-0.3.shp", stringsAsFactors = TRUE)
e2 <- extent(-81.35, -65, -17, 0.5)
Peru <- subset(countries, name %in% c("Peru", "Bolivia", "Ecuador", "Brazil", "Colombia"))

BIO04CHELSA <-raster("CHELSA_bio10_04.tif") #Bio4 = Temperature Seasonality (sd% *10)
BIO15CHELSA <-raster("CHELSA_bio10_15.tif") #Bio15 = Precipitation Seasonality (coeff. variation)
BIO4 <- BIO04CHELSA$CHELSA_bio10_04/100
ph <-raster("phh2o_0-5cm_mean_30s.tif")
soc <-raster("soc_0-5cm_mean_30s.tif")

ph_map <- mask(crop(ph, Peru), Peru)
soc_map <- mask(crop(soc, Peru), Peru)
temp_map <- mask(crop(BIO4, Peru), Peru)
prec_map <- mask(crop(BIO15CHELSA, Peru), Peru)

display.brewer.all(n=NULL, type="all", select=NULL, exact.n=TRUE, colorblindFriendly=FALSE)
col_ph <- brewer.pal(9, "PuRd")
col_soc <- brewer.pal(9, "BuGn")
col_tem <- brewer.pal(9, "YlOrRd")
col_prec <- brewer.pal(11, "Spectral")

pdf("RG_latitudinalgradients.pdf", width=10, height = 10)
svg("RG_latitudinalgradients.svg", width=10, height = 10)

par(mfrow=c(2,2))
#PH
par(mar=c(2,2,2,2))
plot(crop(ph_map,e2), axes=F, legend=T,  main="pH", col=(colorRampPalette(col_ph)(100)))
plot(raster::crop (Peru, e2), add=T, ylim=c(-17,-0))
plot(locations_sf, pch = 21, bg=alpha("grey",0.8), col="black", cex = CLIM$ph/2, add=T, ylim=c(-17,-0))
scaleBar(countries, pos = "topright", pt.cex = 0.2, bty = "n")
#SOC
par(mar=c(2,2,2,2))
plot(crop(soc_map,e2), axes=F, legend=T, main="Soil Organic Carbon" ,col=colorRampPalette(col_soc)(100))
plot(crop (Peru, e2), add=T, ylim=c(-17,-0))
plot(locations_sf, pch = 21, bg=alpha("grey",0.8), col="black", cex = CLIM$soc/25, add=T, ylim=c(-17,-0))
#TSEASONALITY
par(mar=c(2,2,2,2))
plot(crop(temp_map,e2), axes=F, legend=T, main="Temperature seasonality", col=(colorRampPalette(col_prec)(100)))
plot(crop (Peru, e2), add=T, ylim=c(-17,-0))
plot(locations_sf, pch = 21, bg=alpha("grey",0.8), col="black", cex = CLIM$Bio4/20, add=T, ylim=c(-17,-0))
#PRECSEASONALITY
par(mar=c(2,2,2,2))
plot(crop(prec_map,e2), axes=F, legend=T, main="Precipitation seasonality", col=rev(colorRampPalette(col_prec)(100)))
plot(crop (Peru, e2), add=T, ylim=c(-17,-0))
plot(locations_sf, pch = 21, bg=alpha("grey",0.8), col="black", cex = CLIM$Bio15/15, add=T, ylim=c(-17,-0))
dev.off()


