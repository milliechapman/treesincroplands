library(tidyverse)
library(ggplot2)
library(splitstackshape)
set.seed(1)
rm(list = ls())
#################################
#####Load Data###################
##variables
vars<-c("NAME_EN", "b1", "BIOME", "ECONOMY", "POP_EST", "INCOME_GRP", "ISO_A3")
biomes<-read_csv("data/Biome_names.csv")
class<- read.csv("data/gdp_classification.csv") %>%
  mutate(ISO_A3 = Country.Code) %>%
  select(ISO_A3, IncomeGroup)

##load crop
crop_hist<- read.csv("data/cropbio_density_hist.csv") %>%
  select(vars) %>%
  full_join(biomes) %>%
  mutate(BIOME = Name) %>% 
  full_join(class) %>%
  mutate(ECONOMY = IncomeGroup)

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

# filter frequency hist
pasture_hist$b1<- str_sub(pasture_hist$b1, 2, -2)
pasture_hist$b1<- as.character(pasture_hist$b1)
pasture_hist <- cSplit(pasture_hist, "b1", sep = ",", direction = "long")
pasture_hist <- cSplit(pasture_hist, "b1", sep = "=", direction = "wide")

crop_all <- crop_hist %>%
  group_by(NAME_EN, BIOME, b1_1) %>% drop_na(BIOME) %>%
  summarise(b1_2 = sum(b1_2)) %>%
  mutate(b1_2 = as.numeric(b1_2),
         biomass = b1_1*b1_2*.09)

pasture_all <- pasture_hist %>%
  group_by(NAME_EN,BIOME, b1_1) %>% drop_na(BIOME, NAME_EN) %>%
  summarise(b1_2 = sum(b1_2)) %>%
  mutate(b1_2 = as.numeric(b1_2),
         biomass = b1_1*b1_2*.09)

listc<-split(crop_all, paste(crop_all$NAME_EN,crop_all$BIOME))

listp<-split(pasture_all, paste(pasture_all$NAME_EN,pasture_all$BIOME))

quantiles = function(x,probs) {
  z<- with(x, quantile(rep.int(b1_1, b1_2), probs))
  return(data.frame(id = probs, values = z))
} 

for (i in 1:length(listc)) {
  listc[[i]]$b1_1<-as.numeric(listc[[i]]$b1_1)
  z <- subset(listc[[i]], b1_1 > 5 )
  a<-quantiles(z, probs = c(0.1,0.2,0.5))
  listc[[i]]$p50<-a$values[3]
  listc[[i]]$d50<-ifelse(listc[[i]]$p50-listc[[i]]$b1_1>0,listc[[i]]$p50-listc[[i]]$b1_1,0)
  listc[[i]]$a50<-ifelse(listc[[i]]$d50>0,listc[[i]]$b1_2*0.09,0)
  listc[[i]]$b50<-listc[[i]]$d50*listc[[i]]$a50
}

crops_potential <- do.call("rbind", listc) 

crops_potential <- crops_potential %>%
  group_by(NAME_EN, BIOME) %>%
  summarise(carbon_potential = sum(b50)/2,
            potential_value = mean(p50)) %>%
  mutate(CP = "crop")

sum(na.omit(crops_potential$carbon_potential))/10^9

for (i in 1:length(listp)) {
  listp[[i]]$b1_1<-as.numeric(listp[[i]]$b1_1)
  z <- subset(listp[[i]], b1_1 > 5 )
  a<-quantiles(z, probs = c(0.1,0.2,0.5))
  listp[[i]]$p50<-a$values[3]
  listp[[i]]$d50<-ifelse(listp[[i]]$p50-listp[[i]]$b1_1>0,listp[[i]]$p50-listp[[i]]$b1_1,0)
  listp[[i]]$a50<-ifelse(listp[[i]]$d50>0,listp[[i]]$b1_2*0.09,0)
  listp[[i]]$b50<-listp[[i]]$d50*listp[[i]]$a50
}

pastures_potential <- do.call("rbind", listp) 

pastures_potential <- pastures_potential %>%
  group_by(NAME_EN, BIOME) %>%
  summarise(carbon_potential = sum(b50)/2,
            potential_value = mean(p50)) %>%
  mutate(CP = "pasture")

potential<- rbind(pastures_potential, crops_potential)

write.csv(potential, "output/potential.csv")
