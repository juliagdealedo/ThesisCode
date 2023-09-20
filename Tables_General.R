# R Script - General JGA Thesis tables and supplementary material
# Date: 26/07/2023

# Load necessary libraries
library(dplyr)
library(readxl)
library(flextable) 
library(tidyr)
library(stringr)
library(officer)

# Download bibliography of packages used in the thesis
install.packages("grateful")
library(grateful)
cite_packages(out.dir = ".")     
cite_packages(out.format = "docx", out.dir = ".")
pks <- scan_packages()
get_pkgs_info()
out.dir <- setwd("/Users/juliag.dealedo/ONE/UAM_Doctorado/Capitulos")
get_pkgs_info(pkgs = pks$pkg, out.dir = getwd())
citation()

# Create Table 3.1.
setwd("/Users/juliag.dealedo/ONE/UAM_Doctorado/Capitulos/cap2/data") 
comp_original <- read.table("composition")
comp_original$WasColllected<-gsub("True", 1, comp_original$WasColllected) #
comp_original$WasColllected<-gsub("False", 0, comp_original$WasColllected) #
comp_original$WasColllected <- as.numeric(comp_original$WasColllected)

table3.1 <- comp_original %>%
  group_by(Project) %>% 
  summarise(Plots = n_distinct(Plot), 
            Items = length(Cod_item), 
            Species = n_distinct(Species),
            Collections = sum(WasColllected))
table3.1_print <- flextable(table3.1)
# print(intro_thesisf, preview="docx")


# Import data
setwd("/Users/juliag.dealedo/ONE/UAM_Doctorado/Capitulos/cap2/data") 

resu_original <- read.table("resu_cat")
resu <- resu_original %>% filter(!Comunidad=="Aguapolo")

etno_original <- read_excel("etno-dis-mad-yas3.xlsx", sheet=1, col_names =T)
etno <- etno_original %>% filter(!Comunidad=="Aguapolo")
aguapolo <- etno_original %>% filter(Comunidad=="Aguapolo")
plots_aguapolo <- unique(aguapolo$Cod_plot)

comp <- comp_original %>% filter(!Cod_plot %in% plots_aguapolo)
length(unique(comp$Species))
aguapolo_spp <- setdiff(aguapolo$Species, etno$Species)

# Re-categorization and re-naming of categories and uses
# Category
etno$Category2 <- etno$Category
etno$Category2 <- gsub("MEDICINAL AND VETERINARY", "MEDICINAL", etno$Category2)
etno$Category2<-gsub("CULTURAL USES", "CULTURAL", etno$Category2) #
etno$Category2<-gsub("CONSTRUCTION USES", "CONSTRUCTION", etno$Category2) #
etno$Category2<-gsub("FIREWOOD", "FUEL", etno$Category2) #
etno$Category2<-gsub("UTENSILS & TOOLS", "UTENSILS", etno$Category2) #
etno$Category2<-gsub("HUMAN FOOD", "FOOD", etno$Category2) #
etno$Category2<-gsub("TOXIC", "CULTURAL", etno$Category2) 
etno <- etno[!etno$Category2=="ANIMAL FOOD",]
etno <- etno[!etno$Category2=="OTHER",]
etno <- etno[!etno$Category2=="MARKETED",]
etno <- etno[!etno$Category2=="WILD ANIMAL",]
etno <- etno[!etno$Category2=="ENVIRONMENTAL",]

# Use
etno$Use <- gsub("MEDICINAL AND VETERINARY", "MEDICINAL", etno$Use)
etno$Use<-gsub("CULTURAL USES", "CULTURAL", etno$Use) 
etno$Use<-gsub("CONSTRUCTION USES", "CONSTRUCTION", etno$Use) 
etno$Use<-gsub("FIREWOOD", "FUEL", etno$Use) 
etno$Use<-gsub("UTENSILS & TOOLS", "UTENSILS", etno$Use) 
etno$Use<-gsub("HUMAN FOOD", "FOOD", etno$Use) 
etno$Use<-gsub("TOXIC", "CULTURAL", etno$Use) 
etno <- etno[!etno$Use=="ANIMAL FOOD",]
etno <- etno[!etno$Use=="OTHER",]
etno <- etno[!etno$Use=="MARKETED",]
etno <- etno[!etno$Use=="WILD ANIMAL",]
etno <- etno[!etno$Use=="ENVIRONMENTAL",]

# Categorization into Cultural and Provisioning services
etno$Service <- etno$Category2
etno$Service <- gsub("MEDICINAL", "PROVISIONING", etno$Service)
etno$Service<-gsub("CULTURAL", "CULTURAL", etno$Service) 
etno$Service<-gsub("CONSTRUCTION", "PROVISIONING", etno$Service) 
etno$Service<-gsub("FUEL", "PROVISIONING", etno$Service) 
etno$Service<-gsub("UTENSILS", "PROVISIONING", etno$Service) 
etno$Service<-gsub("FOOD", "PROVISIONING", etno$Service) 
unique(cbind(etno$Category2, etno$Service))

# General function to analyse species to the textual level
df_textual <- etno %>%
  group_by(Category2, Subcategory, Species, `Uso textual según cuaderno de campo`) %>%
  summarise(count = n_distinct(Species))

# Table dominant species and their uses (Table 7.1)
Trorgssionis <- c("Coussarea brevicaulis"	,"Hirtella racemosa","Amaioua guianensis"	,"Euterpe precatoria",
              "Iriartea deltoidea", "Cyathea delgadii","Guarea macrophylla","Matisia malacocalyx",
              "Guarea kunthiana", "Guarea pterorhachis"	,"Otoba parvifolia","Iriartea deltoidea"	,"Pseudolmedia laevis",
              "Leonia glycycarpa"	,"Otoba parvifolia"	,"Rinorea pubiflora", "Pseudolmedia laevigata",
              "Pentagonia spathicalyx","Rinorea viridifolia","Rinorea apiculata", "Phytelephas tenuicaulis",
              "Siparuna bifida"	,"Siparuna decipiens"	,"Socratea salazarii"	,"Socratea exorrhiza",
              "Styloceras brokawii","Wettinia augusta"	)
df_etno <- as.data.frame(unique(cbind(etno$Species, etno$Use)))
colnames(df_etno) <- c("Species", "Use")
df_dom <- unique(df_etno[df_etno$Species %in% Trorgssionis,])
Table7.1 <- df_dom %>% count( Species, sort = TRUE)
#print(flextable(Table7.1), preview="docx")

# Percentage of cultural and provisioning services (in Discussion 7.5)
species <- unique(comp %>% select (Species, Family))
useful_species <- unique(etno %>% select (Species, Service) %>% na.omit())
joint <- merge (species, useful_species, by="Species", all=T)
count_spp <- NSPP %>% 
  group_by(Service) %>%
  summarise(count = n_distinct(Species))
count_spp$count/length(unique(joint$Species))

# Table species and use Supplementary material 9.2

df <- etno %>%
  group_by(Family, Species, Category2) %>%
  summarise(count = n_distinct(Category2))
df <- df %>% drop_na()
df_matrix <- reshape2::dcast(df , Family+Species~Category2, value.var="count", fill=0)

# Clean morpho species names
df_matrix$Species <- gsub("\\(D)", "", df_matrix$Species) 
df_matrix$Species <- gsub("\\(Y)", "", df_matrix$Species) 
df_matrix$Species <- gsub("\\(M)", "", df_matrix$Species) 
df_matrix$Species <- gsub("  ", " ", df_matrix$Species) 
df_matrix$Species <- gsub("NA_", "sp.", df_matrix$Species) 
df_matrix$Species <- gsub("\\?", "", df_matrix$Species) 
df_matrix$Species <- gsub("[[:digit:]]", " ", df_matrix$Species) 
df_matrix$Species <- gsub(" P.sp.", " sp.", df_matrix$Species)
df_matrix$Species <- gsub(" E.sp.", " sp.", df_matrix$Species)
df_matrix$Species <- gsub(" B.sp.", " sp.", df_matrix$Species)
df_matrix$Species <- gsub(" B. sp.", " sp.", df_matrix$Species)
df_matrix$Species <- gsub(" P.aff.", " aff.", df_matrix$Species)
df_matrix$Species <- gsub(" E.aff.", " aff.", df_matrix$Species)
df_matrix$Species <- gsub(" B.aff.", " aff.", df_matrix$Species)
df_matrix$Species <- gsub("_aff._", "", df_matrix$Species)
df_matrix$Species <- gsub("B.vel sp. nov. sp. ", " sp.", df_matrix$Species)
df_matrix <- df_matrix %>% mutate_if(is.numeric, str_replace_all, pattern = "0", replacement = " ")
df_matrix <- unique(df_matrix)
df_matrix <- df_matrix %>% mutate_if(is.character, str_replace_all, pattern = "1", replacement = "\U2714") # check emoji
df_matrix
# Table settings
set_flextable_defaults(
  font.size = 8,
  padding = 1,
  border.color = "#CCCCCC",
  line_spacing = 1
)
custom_border <- fp_border(style = "solid", width=.2, color="#CCCCCC")

# Plot table S2
TableThesis <- flextable(df_matrix) |> 
  add_header_lines(values = "Table S1. Plant species and use categories cited by Indigenous communities studied in this thesis.") |>
  italic(j = ~Species, italic = TRUE, part = "body") |> 
  padding(padding.top =4, part = "header", i=2) |>
  bold (i = 2, part = "header")|> 
  merge_v(j = ~ Family) |>
  border_inner_h( part="body", border = custom_border)|>
  rotate(j = 3:8, align = "bottom", rotation = "btlr", part = "header")|> 
  autofit()
TableThesis

print(TableThesis, preview = "docx")



# Table species and regions Supplementary material 9.2*


comp_original$Species <- str_replace(comp_original$Species, "Cestrum schlechtendahlii", "Cestrum schlechtendalhii")
comp_original$Species <- str_replace(comp_original$Species, "Cestrum schlechtendalii", "Cestrum schlechtendalhii")
comp_original$Species <- str_replace(comp_original$Species, "Hirtella (D)sp.", "Hirtella sp.")
comp_original$Species <- str_replace(comp_original$Species, "Inga (D)nobilis subpsp. quaternata", "Inga nobilis subpsp. quaternata")
comp_original$Species <- gsub("\\(D)", "", comp_original$Species) 
comp_original$Species <- gsub("\\(Y)", "", comp_original$Species) 
comp_original$Species <- gsub("\\(M)", "", comp_original$Species) 


df <- comp_original %>%
  group_by(Family, Species, Region) %>%
  summarise(count = n_distinct(Region))



df <- df %>% drop_na()
df_matrix <- reshape2::dcast(df , Family+Species~Region, value.var="count", fill=0)
df_matrix <- df_matrix %>% mutate_if(is.numeric, str_replace_all, pattern = "0", replacement = " ")
df_matrix <- unique(df_matrix)
df_matrix <- df_matrix %>% mutate_if(is.character, str_replace_all, pattern = "1", replacement = "\U2714") # check emoji
df_matrix
#df_matrix <- df_matrix %>% mutate_if(is.character, str_replace_all, pattern = "1", replacement = "X") # check emoji

#write.table(df_matrix, file = "prueba_tabla1.txt", sep = ",", quote = FALSE, row.names = F)
#write.csv(df_matrix, "prueba_tabla1.csv", row.names = T)

length(unique(df_matrix$Species))

colnames(df_matrix)[-c(1:2)] <- str_sub(colnames(df_matrix)[-c(1:2)], 3, -1)

# Table settings
set_flextable_defaults(
  font.size = 8,
  padding = 1,
  border.color = "#CCCCCC",
  line_spacing = 1
)
custom_border <- fp_border(style = "solid", width=.2, color="#CCCCCC")

# Plot table S2

TableThesis <- flextable(df_matrix) |> 
  #add_header_lines(values = "Table 9.S2. Matrix showing the presence-absence of 1930 plant species in the 12 different regions studied in this thesis. Aguapolo, Yariapo, Tumupasa, Tequeje, Ruinas in  Bolivia; Tambopata, Yanesha, Cordillera Azul, Pacaya Samiria, Maijuna in Peru, and Dicaro y Guiyero in Ecuador.") |>
  italic(j = ~Species, italic = TRUE, part = "body") |> 
  padding(padding.top =4, part = "header", i=1) |>
  bold (i = 1, part = "header")|> 
  merge_v(j = ~ Family) |>
  border_inner_h( part="body", border = custom_border)|>
  rotate(j = 3:14, align = "bottom", rotation = "btlr", part = "header")|> 
  autofit()
TableThesis


#print(TableThesis, preview = "pdf")
print(TableThesis, preview = "docx")
#save_as_image(x = TableThesis, path = "/Users/juliag.dealedo/Desktop")

