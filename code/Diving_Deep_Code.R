## load libraries
library(rfishbase)
library(dplyr)
library(ggplot2)
library(MuMIn)
library(visreg)
library(lmer)
library(here)

## load data
A_NA_lake_data <- read.csv("data/A-NA_lake_data.csv")
fishmorph <- read.csv("data/fishmorph.csv")

species <- fb_tbl("species")
swimming <- fb_tbl("swimming")
reproduc <- fb_tbl("reproduc")

## clean data
fishmorphx <- fishmorph %>% 
  rename(Species = species,
         MaxLength = MBl)

A_NA_lake_datax <- A_NA_lake_data %>%
  rename(MaxLength = Length..cm.)

Fish_Lake <- merge(A_NA_lake_datax, species,  by = c("Genus", "Species"))
Fish_Lake_swim <- merge(Fish_Lake, swimming,  by = c("SpecCode"))
Fish_Lake_repro <- merge(Fish_Lake, reproduc,  by = c("SpecCode"))
Fish_Length <- merge(Fish_Lake, fishmorphx,  by = c("Genus", "Species", "Family", "measurement", "MaxLength"))

## body length (+ measurement), body shape
Fish_selected_length <- Fish_Length %>% 
  select(Genus, Species, Lake, DepthRangeDeep, Continent, MaxLength, measurement)

## body shape
Fish_selected_shape <- Fish_Lake %>% 
  select(Genus, Species, Lake, DepthRangeDeep, Continent, BodyShapeI)

## swim mode
Fish_selected_swim <- Fish_Lake_swim %>% 
  select(Genus, Species, Lake, DepthRangeDeep, Continent, AdultMode)

## fertilization
Fish_selected_repro <- Fish_Lake_repro %>% 
  select(Genus, Species, Lake, DepthRangeDeep, Continent, Fertilization)

Fish_filtered_length <- Fish_selected_length %>% 
  filter(DepthRangeDeep != "NA",
         measurement == "TL",
         Continent == "Africa")
Fish_filtered_shape <- Fish_selected_shape %>% 
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

fisha <- Fish_filtered_length %>%
  left_join(Fish_filtered_shape)
fishb <- fisha %>%
  left_join(Fish_filtered_swim)
fishc <- fishb %>%
  left_join(Fish_filtered_repro)

fish_model <- fishc %>% 
  na.omit()

## linear regression with body shape
options(na.action = "na.omit")
lm1 <- lm(DepthRangeDeep ~ MaxLength + BodyShape + AdultMode,
          data = fishc)

## linear regression with fertilization
lm2 <- lm(DepthRangeDeep ~ MaxLength + BodyShape + Fertilization,
          data = fishc)

## dredge lm1
options(na.action = "na.fail")
dd <- dredge(lm1)
summary(dd)

## dredge lm2
options(na.action = "na.fail")
dd2 <- dredge(lm2)
summary(dd2)

## find largest AICc
dd_large <- which.max(dd$AICc) 

dd[dd_large, ]

dd[order(dd$AICc, decreasing = TRUE), ]

model.avg(dd, subset = delta < 4)

## running models
depth_lenght_lm <- lm(DepthRangeDeep ~ MaxLength ,data = fishc)
summary(depth_lenght_lm)

depth_lenght_shape_lm <- lm(DepthRangeDeep ~ MaxLength + BodyShape ,data = fishc)
summary(depth_lenght_shape_lm)

## visreg plots
visreg1 <- visreg(lm1)
visreg2 <- visreg(lm2)