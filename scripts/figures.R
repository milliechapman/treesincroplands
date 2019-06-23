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
                                               "Lower middle income \n (n =288)",
                                               "Upper middle income \n (n=340)"
                                               ))))) %>%
  gather(CP,value,pasture:crop) %>%
  group_by(CP, ECONOMY) %>% 
  summarise(mean_density = mean(value),  # calculates the mean of each group
            sd = sd(value), # calculates the standard deviation of each group
            n_cb = n(),  # calculates the sample size per group
            se = sd(value)/sqrt(n()))
  
tiff("figures/tic_incomegroup.tif", units = "in", width= 6 , height = 4, res=300)
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

potential<- read.csv("output/potential.csv") %>%
  group_by(CP) %>% drop_na() %>%
  summarise(`100 % ` = sum(carbon_potential)/10^9) %>%
  mutate(`50 % ` = `100 % `*0.5,
         `25 % ` = `100 % `*0.25,
         `10 % ` = `100 % `*0.1,
         `5 % ` = `100 % `*0.05,
         `2.5 % ` = `100 % `*0.025,
         `1 % ` = `100 % `*0.01) %>%
  gather(Adoption,value,`100 % `:`1 % `)

tiff("figures/potential.tiff", width = 4, height = 4, units = 'in', res = 300)
potential %>% 
  ggplot(aes(reorder(Adoption, value), value))+
  geom_col(aes(fill = CP))+
  labs(x = "Adoption Percentage",y = "Carbon Sequestration Potential (PgC)", sep = "")+
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c( "darkolivegreen3","darkolivegreen4")) +
  theme(legend.position =  c(0.2, 0.8))
dev.off()

## figure 4: top 30 countries

countries<-read_csv("output/potential.csv") %>%
  recode(NAME_EN, "People's Republic of China" = "China")
  group_by(NAME_EN, CP) %>% drop_na() %>%
  summarise(potential = sum(carbon_potential)*0.01/10^6) %>%
  spread(key = CP, value = potential) %>% 
  mutate(total = crop + pasture) %>%
  gather(CP,value,pasture:crop) 


countries <- countries %>%  arrange(-total) %>%

countries<- countries[1:60,]

tiff("figures/potential_country.tiff", width = 8, height = 6, units = 'in', res = 300)
countries %>% ggplot(aes(reorder(NAME_EN, total), value))+
  geom_col(aes(fill = CP))+
  labs(x = "Country",y = "Carbon Sequestration Potential (PgC)", sep = "")+
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c( "darkolivegreen3","darkolivegreen4")) +
  theme(legend.position =  c(0.8, 0.2)) +coord_flip()
dev.off()





