# R Script - Example maps JGA Thesis
# Date revision: 27/07/2023

library(raster)
library(rnaturalearth)
library(RColorBrewer)
library(ggplot2)
library(ggspatial)
library(dplyr)
library(ggrepel)

# Map

# Read data
setwd("/Users/juliag.dealedo/ONE/UAM_Doctorado/Capitulos/cap2/data") 
resu_original <- read.table("resu_cat")
resu <- resu_original %>% filter(!Comunidad=="Aguapolo")
etno_original <- read_excel("etno-dis-mad-yas3.xlsx", sheet=1, col_names =T)
etno <- etno_original %>% filter(!Comunidad=="Aguapolo")
aguapolo <- etno_original %>% filter(Comunidad=="Aguapolo")
plots_aguapolo <- unique(aguapolo$Cod_plot)
comp_original <- read.table("composition")
comp <- comp_original %>% filter(!Cod_plot %in% plots_aguapolo)
coordenadas <- cbind (aggregate(resu$Latitud, list(resu$Comunidad), min), aggregate(resu$Longitud, list(resu$Comunidad), min)[,2])
colnames(coordenadas) <- c("Region", "Latitud", "Longitud")

# Elevation
# Load data
setwd("/Users/juliag.dealedo/Google Drive/Cap1_JGA-MP/data/MapData")
elev_ecu<- raster("ECU_alt/ECU_alt.gri")
elev_peru<- raster("PER_alt/PER_alt.gri")
elev_bol<- raster("BOL_alt/BOL_alt.gri")
world <- ne_countries(scale = "medium", returnclass = "sf")
contries_selection <- subset(world, name %in% c("Peru", "Bolivia", "Ecuador"))
elevation <- merge(elev_peru, elev_bol, elev_ecu)
elevation_masked <- mask(crop(elevation, contries_selection), contries_selection)
extent1 <- extent(-81, -50, -5, 10)
plot(elevation_masked)
ele_low <- aggregate(elevation_masked, fact=10)
dem.p  <-  rasterToPoints(ele_low)
df <-  data.frame(dem.p)

# Ecuador elevation map
pal <- c("#405F35", "#607A26", "#909B42", "#7E3B1E", "#7D461A", "#DA8126", "#FAC532", "#FCE47D", "#FAF3CE")

ggplot (data = contries_selection) + 
  geom_sf (color="#FFFFFF99")+
  geom_tile (data=df, aes(x, y, fill = layer))+
  geom_point (data = coordenadas, aes(x = Longitud, y = Latitud, col=Region), 
             size = 2, shape = 16, alpha=0.9, fill="black", col="grey") +
  scale_fill_gradientn (colours = alpha(pal, 1), name="Elevation")+
  annotation_scale (location = "bl", width_hint = 0.5) +
  annotation_north_arrow (location = "bl", which_north = "true", 
                         pad_x = unit(0, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme (panel.grid.major = element_line(), panel.background = element_rect(fill = "aliceblue"))

ggsave("map_thesis.svg",scale=0.7)


# Cover map Figure 2.2.

setwd("/Users/juliag.dealedo/Google Drive/Cap1_JGA-MP/data/MapData/Cover")
forest.a <- raster("W080N00_PROBAV_LC100_global_v3.0.1_2019-nrt_Tree-CoverFraction-layer_EPSG-4326.tif")
forest.b <- raster("W080N20_PROBAV_LC100_global_v3.0.1_2019-nrt_Tree-CoverFraction-layer_EPSG-4326_2.tif")
forest.c <- raster("W060N00_PROBAV_LC100_global_v3.0.1_2019-nrt_Tree-CoverFraction-layer_EPSG-4326_3.tif")
forest.d <- raster("W060N20_PROBAV_LC100_global_v3.0.1_2019-nrt_Tree-CoverFraction-layer_EPSG-4326_4.tif")

cover <- merge(forest.a, forest.b,  forest.c, forest.d)
forest.cover <- aggregate(cover, fact=7)
dem.p  <-  rasterToPoints(forest.cover)
df <-  data.frame(dem.p)
world <- ne_countries(scale = "medium", returnclass = "sf")

c1 <- natparks.pals("IguazuFalls", n = 10)
c1[1:10]
colors <- c("#DDD9AA","#c79537","#a18a2e","#726D67", "#735A48", "#573E2E", "#655C2D", "#8E9F3A", "#708530" ,"#415521")
texture <- colorRampPalette(colors)(100)

ggplot(data = world) + 
  geom_sf()+
  geom_raster(data=df, aes(x, y, fill = layer))+
  scale_fill_gradientn(colours = alpha(  texture, 1))+
  coord_sf(xlim = c(-80, -40), ylim = c(-17, 13), expand = FALSE)+
  theme_void()

ggsave("forest_cover_6.png")



