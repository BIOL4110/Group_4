library("rfishbase")
library("dplyr")
library("ggplot2")

#data frames
species <- fb_tbl("species")

#merge lake data by species
Chichlidae_Lake_Data <- merge(African_lake_data, species,  by = "Species")

#rename genus.x
Chichlidae_Lake_Data <- Chichlidae_Lake_Data %>% 
  rename(Genus = Genus.x)

#select data
Chichlidae_selected <- Chichlidae_Lake_Data %>% 
  select(Genus, Species, Lake, DepthRangeDeep, `Length (cm)`, LongevityWild)

#filter data
Chichlidae_filtered <- Chichlidae_selected %>% 
  filter(DepthRangeDeep != "NA", LongevityWild != "NA")
