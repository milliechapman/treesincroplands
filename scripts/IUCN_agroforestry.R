library(tidyverse)
library(ggplot2)
library(splitstackshape)
library(stringr)
rm(list = ls())

vars<-c("BIOME", "b1", "CONTINENT", "COUNTRY", "ISO_A3")
biomes<-read_csv("data/Biome_names.csv")
IUCN_agroforestry  <- read_csv("data/INDC_CCAFS.csv")

class<- read.csv("data/gdp_classification.csv") %>%
  mutate(ISO_A3 = Country.Code) %>%
  select(ISO_A3, IncomeGroup) %>%
  mutate(ECONOMY = IncomeGroup) %>%
  left_join(IUCN_agroforestry)

country_codes<- read.csv("data/country_code.csv")

australia<- read.csv("data/cropbio_density_australia.csv") %>%
  left_join(country_codes) %>%
  select(vars) %>%
  left_join(biomes) %>%
  mutate(BIOME = Name) %>% 
  left_join(class) 

africa<- read.csv("data/cropbio_density_africa.csv") %>%
  left_join(country_codes) %>%
  select(vars) %>%
  left_join(biomes) %>%
  mutate(BIOME = Name) %>% 
  left_join(class) 

southamerica<- read.csv("data/cropbio_density_SA.csv") %>%
  left_join(country_codes) %>%
  select(vars) %>%
  left_join(biomes) %>%
  mutate(BIOME = Name) %>% 
  left_join(class) 

europe<- read.csv("data/cropbio_density_europe.csv") %>%
  left_join(country_codes) %>%
  select(vars) %>%
  left_join(biomes) %>%
  mutate(BIOME = Name) %>% 
  left_join(class)

oceana<- read.csv("data/cropbio_density_oceana.csv") %>%
  left_join(country_codes) %>%
  select(vars) %>%
  left_join(biomes) %>%
  mutate(BIOME = Name) %>% 
  left_join(class)

crop_hist <- rbind(australia, africa, southamerica, europe, oceana) %>%
  mutate(NAME_EN = Country)
# filter frequency hist
crop_hist$b1<- str_sub(crop_hist$b1, 2, -2)
crop_hist$b1<- as.character(crop_hist$b1)
crop_hist <- cSplit(crop_hist, "b1", sep = ",", direction = "long")
crop_hist <- cSplit(crop_hist, "b1", sep = "=", direction = "wide")


######## country summary #############
crop_hist$NAME_EN<- forcats::fct_explicit_na(crop_hist$NAME_EN)
crop_country_adaptation <- crop_hist %>% 
  group_by(NAME_EN, adaptation, ISO_A3, b1_1) %>%
  summarise(b1_2 = sum(b1_2)) %>%
  mutate(b1_2 = as.numeric(b1_2),
         biomass = b1_1*b1_2*.078) %>%
  group_by(adaptation) %>%
  summarise(area_crop = sum(b1_2)*.078/10^6,
            carbon_crop = sum(biomass)/2/10^6,
            density_crop = carbon_crop/area_crop)

crop_country_mitigation <- crop_hist %>% 
  group_by(NAME_EN, mitigation, ISO_A3, b1_1) %>%
  summarise(b1_2 = sum(b1_2)) %>%
  mutate(b1_2 = as.numeric(b1_2),
         biomass = b1_1*b1_2*.078) %>%
  group_by(mitigation) %>%
  summarise(area_crop = sum(b1_2)*.078/10^6,
            carbon_crop = sum(biomass)/2/10^6,
            density_crop = carbon_crop/area_crop)

crop_country_both <- crop_hist %>% as.tibble() %>%
  mutate(m_a = adaptation + mitigation) %>%
  #mutate(m_a = ifelse(m_a > 1, 1, 0)) %>%
  group_by(NAME_EN, m_a, ISO_A3, b1_1) %>%
  summarise(b1_2 = sum(b1_2)) %>%
  mutate(b1_2 = as.numeric(b1_2),
         biomass = b1_1*b1_2*.078) %>%
  group_by(m_a) %>%
  summarise(area_crop = sum(b1_2)*.078/10^6,
            carbon_crop = sum(biomass)/2/10^6,
            density_crop = carbon_crop/area_crop)

gather(mini_iris, key = "flower_att", value = "measurement",
       Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)

area<- crop_country_both %>% gather(key = "key", value = "value", area_crop, carbon_crop, density_crop, -m_a) %>%
  filter(key == "area_crop") %>%
  ggplot(aes(m_a, value)) + geom_bar(stat = "identity") +
  labs(x = "", y = "Area in cropland (ha)") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20), 
        legend.title = element_text(size = 20, face = 'bold'),
        legend.text=  element_text(size=20),
        axis.line = element_line(colour = "black"))+
  scale_fill_grey("Period") +
  theme_classic(base_size = 20, base_family = "") + 
  theme(panel.grid.minor = element_line(colour="grey", size=0.5)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0))

density<- crop_country_both %>% gather(key = "key", value = "value", area_crop, carbon_crop, density_crop, -m_a) %>%
  filter(key == "density_crop") %>%
  ggplot(aes(m_a, value)) + geom_bar(stat = "identity") +
  labs(x = "", y = "Average biomass density in cropland (Mg ha-1)") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20), 
        legend.title = element_text(size = 20, face = 'bold'),
        legend.text=  element_text(size=20),
        axis.line = element_line(colour = "black"))+
  scale_fill_grey("Period") +
  theme_classic(base_size = 20, base_family = "") + 
  theme(panel.grid.minor = element_line(colour="grey", size=0.5)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0))

carbon<- crop_country_both %>% gather(key = "key", value = "value", area_crop, carbon_crop, density_crop, -m_a) %>%
  filter(key == "carbon_crop") %>%
  ggplot(aes(m_a, value)) + geom_bar(stat = "identity") +
  labs(x = "", y = "Total Carbon (Mg C)") +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 20), 
        legend.title = element_text(size = 20, face = 'bold'),
        legend.text=  element_text(size=20),
        axis.line = element_line(colour = "black"))+
  scale_fill_grey("Period") +
  theme_classic(base_size = 20, base_family = "") + 
  theme(panel.grid.minor = element_line(colour="grey", size=0.5)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0))



