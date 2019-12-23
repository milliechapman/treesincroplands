## Figure 2,3 and 5 in manuscript

library(tidyverse)
library(ggplot2)

## figure 2: Econ chart
econ <- read.csv("output/summary_all.csv") %>%
  select(NAME_EN, BIOME, ECONOMY, density_pasture, density_crop) %>% drop_na() %>%
  rename(pasture = density_pasture,
         crop = density_crop) %>%
  mutate(ECONOMY = factor(ifelse(ECONOMY == "High income", "High income \n (n=274)", 
                                 ifelse(ECONOMY == "Low income", "Low income \n (n=194)",
                                        ifelse(ECONOMY =="Lower middle income", 
                                               "Lower middle \n income \n (n =288)",
                                               "Upper middle \n income \n (n=340)"
                                               ))))) %>%
  gather(CP,value,pasture:crop) %>%
  group_by(CP, ECONOMY) %>% 
  summarise(mean_density = mean(value),  # calculates the mean of each group
            sd = sd(value), # calculates the standard deviation of each group
            n_cb = n(),  # calculates the sample size per group
            se = sd(value)/sqrt(n()))
  
tiff("figures/tic_incomegroup.tif", units = "in", width= 4 , height = 4, res=300)
ggplot(econ, aes(x=reorder(ECONOMY, -mean_density), 
               y=mean_density, 
               color=CP)) + 
  geom_errorbar(aes(ymin=mean_density-se, 
                    ymax=mean_density+se), 
                width=.3, size=0.7, position=position_dodge(.3)) +
  geom_point(position=position_dodge(.3)) +
  theme_bw() + ylab("Average Carbon Density (MgC ha-1)")+ xlab("")+
  scale_color_manual(values = c("darkgreen", "blue"))+
  theme(legend.title=element_blank()) +
  theme(legend.justification = c(1, 1), legend.position = c(0.93, 0.93),
        legend.box.background = element_rect(colour = "black"))+
  theme( 
    axis.text.x = element_text(color='black'),
    axis.ticks.y = element_blank(),
    axis.title.x = element_text(size=11,color='black')
  )
dev.off()


## figure 3: potential
IUCN <- read.csv("output/summary_all.csv") %>%
  select(mitigation, area_pasture, area_crop, carbon_pasture, carbon_crop) %>%
  #mutate(mitigation = as.factor(mitigation)) %>%
  mutate_all(funs(ifelse(is.na(.), 0, .))) %>%
  group_by(mitigation) %>%
  summarise(pasture = sum(carbon_pasture)/1000,
            crop = sum(carbon_crop)/1000) %>%
  gather(-mitigation, key = "CP", value = "carbon_standing") %>%
  mutate(mitigation = as.factor(mitigation)) %>%
  group_by(CP) %>%
  summarise(carbon_standing = sum(carbon_standing))

IUCN_agroforestry  <- read_csv("data/INDC_CCAFS.csv")

IUCN_potential <- read.csv("output/potential.csv") %>% left_join(IUCN_agroforestry, by = "ISO_A3") %>%
  select(carbon_potential, CP, mitigation.y) %>%
  #mutate_all(funs(ifelse(is.na(.), 0, .))) %>%
  select(mitigation.y, carbon_potential, CP) %>%
  mutate(mitigation = as.factor(mitigation.y)) %>%
  #mutate_all(funs(ifelse(is.na(.), 0, mitigation))) %>%
  group_by(CP) %>%
  summarise(carbon_potential = sum(carbon_potential, na.rm = TRUE)/10^9/10) %>%
  left_join(IUCN, by = c("CP"))


potential<- IUCN_potential %>%
  group_by(CP) %>% 
  summarise(`100 % ` = sum(carbon_potential),
            `-100 % ` = sum(carbon_standing)*(-1)) %>%
  mutate(`50 % ` = `100 % `*0.5,
         `25 % ` = `100 % `*0.25,
         `10 % ` = `100 % `*0.1,
         `5 % ` = `100 % `*0.05,
         `2.5 % ` = `100 % `*0.025,
         `1 % ` = `100 % `*0.01,
         `-50 % ` = `-100 % `*0.5,
         `-25 % ` = `-100 % `*0.25,
         `-10 % ` = `-100 % `*0.1,
         `-5 % ` = `-100 % `*0.05,
         `-2.5 % ` = `-100 % `*0.025,
         `-1 % ` = `-100 % `*0.01,) %>%
  gather(Adoption,value,`100 % `:`-1 % `) %>%
  mutate(color = ifelse(value < 0 & CP == "crop", "trees in crop loss",
                        ifelse(value < 0 & CP == "pasture", "trees in pasture loss",
                               ifelse(value >0 & CP == "crop", "trees in crop gain", "trees in pasture gain"))))%>%
  mutate(color = as.factor(color))

tiff("figures/potential.tiff", width = 3.8, height = 5, units = 'in', res = 300)
potential %>% 
  ggplot(aes(reorder(Adoption, value), value))+
  geom_col(aes(fill = color))+
  labs(x = "Adoption Percentage",y = "Carbon Sequestration Potential (PgC)", sep = "")+
  theme(legend.title = element_blank()) + ylim(-10, 10) +
  scale_fill_manual(values = c("darkolivegreen3","#CA3433", "darkolivegreen4", "darkred")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +theme(legend.position = "none")
dev.off()

## figure 4: top 30 countries



standing <- read.csv("output/summary_all.csv") %>%
  select(NAME_EN, carbon_pasture, carbon_crop) %>%
  #mutate(mitigation = as.factor(mitigation)) %>%
  ##mutate_all(funs(ifelse(is.na(.), 0, .))) %>%
  group_by(NAME_EN) %>%
  summarise(pasture = sum(carbon_pasture, na.rm = TRUE)/1000,
            crop = sum(carbon_crop, na.rm = TRUE)/1000) %>%
  gather(-NAME_EN, key = "CP", value = "standing") %>%
  #mutate(mitigation = as.factor(mitigation)) %>%
  group_by(NAME_EN, CP) %>%
  summarise(carbon_standing = sum(standing))

countries1<-read.csv("output/potential.csv") %>%
  group_by(NAME_EN, CP) %>%
  summarise(potential = sum(carbon_potential, na.rm = TRUE)*0.01/10^9) %>%
  spread(key = CP, value = potential) %>% 
  gather(CP,value,pasture:crop)  %>%
  left_join(standing, by = c('NAME_EN', 'CP')) %>%
  mutate(carbon_standing = (-1)* carbon_standing) %>%
  gather(type, value, carbon_standing) %>%
  mutate(color = ifelse(value < 0 & CP == "crop", "trees in crop loss",
                        ifelse(value < 0 & CP == "pasture", "trees in pasture loss",
                               ifelse(value >0 & CP == "crop", "trees in crop gain", "trees in pasture gain"))))



tiff("figures/potential_country.tiff", width = 7, height = 8, units = 'in', res = 300)
countries %>% ggplot(aes(NAME_EN, value))+
  geom_col(aes(fill = color))+
  labs(x = "", y = "Carbon sequestration potential (Pg C) under 1% adoption scenario", sep = "")+
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c( "darkolivegreen3","darkolivegreen4")) +
  theme(legend.position =  c(0.8, 0.2)) +coord_flip()
dev.off()

sum(countries$value)/sum(countries1$value)

sum(countries1$value)


IUCN <- read.csv("output/summary_all.csv") %>%
  select(mitigation, area_pasture, area_crop, carbon_pasture, carbon_crop) %>%
  #mutate(mitigation = as.factor(mitigation)) %>%
  mutate_all(funs(ifelse(is.na(.), 0, .))) %>%
  group_by(mitigation) %>%
  summarise(area_pasture = sum(area_pasture),
            carbon_pasture = sum(carbon_pasture),
            area_crop = sum(area_crop),
            carbon_crop = sum(carbon_crop))
IUCN_agroforestry  <- read_csv("data/INDC_CCAFS.csv")

IUCN_potential <- read.csv("output/potential.csv") %>% left_join(IUCN_agroforestry, by = "ISO_A3") %>%
  select(carbon_potential, CP, mitigation.y) %>%
  #mutate_all(funs(ifelse(is.na(.), 0, .))) %>%
  select(mitigation.y, carbon_potential, CP) %>%
  mutate(mitigation = as.factor(mitigation.y)) %>%
  #mutate_all(funs(ifelse(is.na(.), 0, mitigation))) %>%
  group_by(mitigation, CP) %>%
  summarise(carbon_potential = sum(carbon_potential, na.rm = TRUE)/10^9/100) 

IUCN_potential<- IUCN_potential %>% remove_missing() %>%
  mutate(mitigation1 = as.numeric(mitigation))

IUCN_potential$mitigation2 <- ifelse(IUCN_potential$mitigation1 == 1, "No", "Yes")
IUCN_potential %>%
  group_by(mitigation1) %>%
  summarise(carbon_per = sum(carbon_potential))

tiff("figures/potential_IUCN.tiff", width = 5, height = 6, units = 'in', res = 300)
IUCN_potential %>% ggplot(aes(mitigation2, carbon_potential)) + geom_col(aes(fill = CP)) +
  labs(x = "Agroforestry included as \n mitigation technique in NDC", y = "Carbon Potential (10% scenario)") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14), 
        legend.title = element_text(size = 20, face = 'bold'),
        legend.text=  element_text(size=20),
        axis.line = element_line(colour = "black"))+
  scale_fill_grey("Period") +
  theme_classic(base_size = 20, base_family = "") + 
  theme(panel.grid.minor = element_line(colour="grey", size=0.5)) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0)) +
  #scale_fill_manual(values = c( "darkolivegreen3","darkolivegreen4")) +
  theme(legend.position =  c(0.8, 0.8)) +   theme(legend.title = element_blank()) 
dev.off()



