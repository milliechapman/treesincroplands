## Clean output data from GEE and make summary tables 

library(tidyverse)
library(ggplot2)
library(splitstackshape)
library(stringr)
rm(list = ls())

##variables
vars<-c("NAME_EN", "b1", "BIOME", "ECONOMY", "POP_EST", "INCOME_GRP", "ISO_A3")
biomes<-read_csv("data/Biome_names.csv")
class<- read.csv("data/gdp_classification.csv") %>%
  mutate(ISO_A3 = Country.Code) %>%
  select(ISO_A3, IncomeGroup) %>%
  mutate(ECONOMY = IncomeGroup)

##load crop
crop_hist<- read.csv("data/cropbio_density_hist.csv") %>%
  select(vars) %>%
  full_join(biomes) %>%
  mutate(BIOME = Name) %>% 
  full_join(class) 

# filter frequency hist
crop_hist$b1<- str_sub(crop_hist$b1, 2, -2)
crop_hist$b1<- as.character(crop_hist$b1)
crop_hist <- cSplit(crop_hist, "b1", sep = ",", direction = "long")
crop_hist <- cSplit(crop_hist, "b1", sep = "=", direction = "wide")

##load pasture
pasture_hist<- read.csv("data/pasturebio_density_hist.csv") %>%
  select(vars)  %>% 
  full_join(biomes) %>%
  mutate(BIOME = Name) %>% 
  full_join(class) %>%
  mutate(ECONOMY = IncomeGroup)

#format frequency hist
pasture_hist$b1<- str_sub(pasture_hist$b1, 2, -2)
pasture_hist$b1<- as.character(pasture_hist$b1)
pasture_hist <- cSplit(pasture_hist, "b1", sep = ",", direction = "long")
pasture_hist <- cSplit(pasture_hist, "b1", sep = "=", direction = "wide")

######## country summary #############
crop_hist$NAME_EN<- forcats::fct_explicit_na(crop_hist$NAME_EN)
crop_country <- crop_hist %>% 
  group_by(NAME_EN, b1_1) %>%
  summarise(b1_2 = sum(b1_2)) %>%
  mutate(b1_2 = as.numeric(b1_2),
         biomass = b1_1*b1_2*.09) %>%
  group_by(NAME_EN) %>%
  summarise(area_crop = sum(b1_2)*.09/10^6,
            carbon_crop = sum(biomass)/2/10^6,
            density_crop = carbon_crop/area_crop)

summary_country <- pasture_hist %>% drop_na(BIOME) %>%
  group_by(NAME_EN, b1_1) %>% 
  summarise(b1_2 = sum(b1_2)) %>%
  mutate(b1_2 = as.numeric(b1_2),
         biomass = b1_1*b1_2*.09) %>%
  group_by(NAME_EN) %>%
  summarise(area_pasture = sum(b1_2)*.09/10^6,
            carbon_pasture = sum(biomass)/2/10^6,
            density_pasture = carbon_pasture/area_pasture) %>%
  full_join(crop_country) %>%
  mutate(total_carbon = carbon_pasture + carbon_crop,
         total_area = area_pasture + area_crop )

###### Biome Summary ############
crop_biome <- crop_hist %>%
  group_by(BIOME, b1_1) %>% drop_na(BIOME) %>%
  summarise(b1_2 = sum(b1_2)) %>%
  mutate(b1_2 = as.numeric(b1_2),
         biomass = b1_1*b1_2*.09) %>%
  group_by(BIOME) %>%
  summarise(area_crop = sum(b1_2)*.09/10^6,
            carbon_crop = sum(biomass)/2/10^6,
            density_crop = carbon_crop/area_crop)

summary_biome <- pasture_hist %>%
  group_by(BIOME, b1_1) %>% drop_na(BIOME) %>%
  summarise(b1_2 = sum(b1_2)) %>%
  mutate(b1_2 = as.numeric(b1_2),
         biomass = b1_1*b1_2*.09) %>%
  group_by(BIOME) %>%
  summarise(area_pasture = sum(b1_2)*.09/10^6,
            carbon_pasture = sum(biomass)/2/10^6,
            density_pasture = carbon_pasture/area_pasture) %>%
  full_join(crop_biome) %>%
  mutate(total_carbon = carbon_pasture + carbon_crop,
         total_area = area_pasture + area_crop )

######## Economy Summary ############
crop_econ <- crop_hist %>% drop_na(ECONOMY) %>%
  group_by(ECONOMY, b1_1) %>%
  summarise(b1_2 = sum(b1_2)) %>%
  mutate(b1_2 = as.numeric(b1_2),
         biomass = b1_1*b1_2*.09) %>%
  group_by(ECONOMY) %>% drop_na() %>%
  summarise(area_crop = sum(b1_2)*.09/10^6,
            carbon_crop = sum(biomass)/2/10^6,
            density_crop = carbon_crop/area_crop)

summary_economy <- pasture_hist %>% drop_na(ECONOMY) %>%
  group_by(ECONOMY, b1_1)  %>%
  summarise(b1_2 = sum(b1_2)) %>%
  mutate(b1_2 = as.numeric(b1_2),
         biomass = b1_1*b1_2*.09) %>%
  group_by(ECONOMY) %>% drop_na() %>%
  summarise(area_pasture = sum(b1_2)*.09/10^6,
            carbon_pasture = sum(biomass)/2/10^6,
            density_pasture = carbon_pasture/area_pasture) %>%
  full_join(crop_econ) %>%
  mutate(total_carbon = carbon_pasture + carbon_crop,
         total_area = area_pasture + area_crop )

########Summary all ###############
crop_all <- crop_hist %>%
  group_by(NAME_EN, BIOME, b1_1) %>% drop_na(BIOME) %>%
  summarise(b1_2 = sum(b1_2)) %>%
  mutate(b1_2 = as.numeric(b1_2),
         biomass = b1_1*b1_2*.09) %>%
  group_by(NAME_EN, BIOME) %>%
  summarise(area_crop = sum(b1_2)*.09/10^6,
            carbon_crop = sum(biomass)/2/10^6,
            density_crop = carbon_crop/area_crop)

summary_all <- pasture_hist %>%
  group_by(NAME_EN,BIOME,ISO_A3, b1_1) %>% drop_na(BIOME, NAME_EN) %>%
  summarise(b1_2 = sum(b1_2)) %>%
  mutate(b1_2 = as.numeric(b1_2),
         biomass = b1_1*b1_2*.09) %>%
  group_by(NAME_EN, BIOME, ISO_A3) %>%
  summarise(area_pasture = sum(b1_2)*.09/10^6,
            carbon_pasture = sum(biomass)/2/10^6,
            density_pasture = carbon_pasture/area_pasture) %>%
  full_join(crop_all) %>%
  mutate(total_carbon = carbon_pasture + carbon_crop,
         total_area = area_pasture + area_crop ) %>%
  full_join(class) %>%
  mutate(ECONOMY = IncomeGroup)

####### export ###############
write.csv(summary_biome, "output/summary_biome.csv")
write.csv(summary_country, "output/summary_country.csv")
write.csv(summary_economy, "output/summary_economy.csv")
write.csv(summary_all, "output/summary_all.csv")
