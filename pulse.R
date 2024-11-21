## load libraries
library(rfishbase)
library(dplyr)
library(ggplot2)
library(readr)
library(readxl)
library(tidyverse)
library(here)
library(gridExtra)

## load data
A_NA_lake_data <- read.csv("data/A-NA_lake_data.csv")

species <- fb_tbl("species")
swimming <- fb_tbl("swimming")
reproduc <- fb_tbl("reproduc")
gillarea <- fb_tbl("gillarea")
speed <- fb_tbl("speed")

## clean data
Fish_Lake <- merge(A_NA_lake_data, species,  by = c("Genus", "Species"))
Fish_Lake_swim <- merge(Fish_Lake, swimming,  by = c("SpecCode"))
Fish_Lake_repro <- merge(Fish_Lake, reproduc,  by = c("SpecCode"))

## body length (+ measurement), body shape
Fish_selected_length <- Fish_Lake %>% 
  select(Genus, Species, Family, Lake, DepthRangeDeep, Length..cm., measurement, Continent)
## body shape
Fish_selected_shape <- Fish_Lake %>% 
  select(Genus, Species, Family, Lake, DepthRangeDeep, Continent, BodyShapeI)
## swim mode
Fish_selected_swim <- Fish_Lake_swim %>% 
  select(Genus, Species, Family, Lake, DepthRangeDeep, Continent, AdultMode)
## egg
Fish_selected_repro <- Fish_Lake_repro %>% 
  select(Genus, Species, Family, Lake, DepthRangeDeep, Continent, Fertilization)

## deep
Fish_filtered_length <- Fish_selected_spec %>% 
  filter(DepthRangeDeep != "NA",
         measurement == "TL",
         Continent == "Africa")
Fish_filtered_shape <- Fish_selected_spec %>% 
  filter(DepthRangeDeep != "NA",
         Continent == "Africa")
Fish_filtered_swim <- Fish_selected_swim %>% 
  filter(DepthRangeDeep != "NA",
         Continent == "Africa")
Fish_filtered_repro <- Fish_selected_repro %>% 
  filter(DepthRangeDeep != "NA",
         Fertilization != "NA",
         Continent == "Africa")

Fish_filtered_shape$BodyShape <- ifelse(Fish_filtered_shape$BodyShapeI == "elongated", "elongated / eel-like",
                                  ifelse(Fish_filtered_shape$BodyShapeI == "fusiform / normal", "fusiform / normal",
                                         ifelse(Fish_filtered_shape$BodyShapeI == "eel-like", "elongated / eel-like", 
                                                ifelse(Fish_filtered_shape$BodyShapeI == "short and / or deep", "short and / or deep", 
                                                       ifelse(Fish_filtered_shape$BodyShapeI == "Elongated", "elongated / eel-like", NA)))))

## plot
### length
ggplot(Fish_filtered_length, aes(x = log(DepthRangeDeep), y = Length..cm.)) +
  geom_point(aes(colour = Lake)) +
  geom_smooth(method = "lm") + 
  ylab("Length") +
  xlab("Depth (log)") +
  theme_bw()

### body shape
ggplot(Fish_filtered_shape, aes(x = log(DepthRangeDeep), y = Lake)) +
  geom_col(aes(fill = BodyShape)) +
  geom_smooth(method = "lm") +
  ylab("Body Shape") +
  xlab("Depth (log)") +
  theme_bw()

### mode
ggplot(Fish_filtered_swim, aes(x = log(DepthRangeDeep), y = Lake)) +
  geom_col(aes(fill = AdultMode)) +
  geom_smooth(method = "lm") +
  ylab("Swim Mode") +
  xlab("Depth (log)") +
  theme_bw()

### egg
ggplot(Fish_filtered_repro, aes(x = log(DepthRangeDeep), y = Lake)) +
  geom_col(aes(fill = Fertilization)) +
  geom_smooth(method = "lm") +
  ylab("Rearing Method") +
  xlab("Depth (log)") +
  theme_bw()

## plot ----
### length
length_shal <- ggplot(Fish_filtered_sh, aes(x = log(DepthRangeShallow), y = Length..cm.)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  ylab("Length") +
  xlab("Depth (shallow)") +
  theme_bw()

### body shape
ggplot(Fish_filtered_sh, aes(x = log(DepthRangeShallow), y = BodyShapeI)) +
  geom_col() +
  geom_smooth(method = "lm") +
  ylab("Body Shape") +
  xlab("Depth") +
  theme_bw()

