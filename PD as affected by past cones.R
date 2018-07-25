require(krsp)
require(tidyverse)
require(lubridate)
require(lme4)
require(coefplot2)



#Connecting to and drawing tables from database####

con = krsp_connect(group = "krsp-aws")

litter = tbl(con, "litter")


tree_cones = read.csv("grids_cones.csv") %>% filter(Grid %in% c("SU", "KL")) %>%
  select(year=Year, grid=Grid,  cone_index_t)

#there is probably a neater way of doing this...

tree_cones_m1 = tree_cones %>% mutate(year = year+1) %>% 
  select(everything(), cone_index_tm1 = cone_index_t)

tree_cones_m2 = tree_cones %>% mutate(year = year+2) %>% 
  select(everything(), cone_index_tm2 = cone_index_t)

tree_cones_m3 = tree_cones %>% mutate(year = year+3) %>% 
  select(everything(), cone_index_tm3 = cone_index_t)

tree_cones_m4 = tree_cones %>% mutate(year = year+4) %>% 
  select(everything(), cone_index_tm4 = cone_index_t)

tree_cones_m5 = tree_cones %>% mutate(year = year+5) %>% 
  select(everything(), cone_index_tm5 = cone_index_t)


#Creating data table####

pd_table = litter %>% select(squirrel_id, p_date=fieldBDate, year = yr, grid, ln) %>%
  filter(ln==1, grid %in% c("SU", "KL")) %>% collect() %>%
  mutate(part_date = yday(ymd(p_date)), 
         mast = year %in% c(1993, 1998, 2005, 2010, 2014)) %>%
  left_join(tree_cones_m1, by=c("year", "grid")) %>% #again, bit inelegant, but works
  left_join(tree_cones_m2, by=c("year", "grid")) %>%
  left_join(tree_cones_m3, by=c("year", "grid")) %>%
  left_join(tree_cones_m4, by=c("year", "grid")) %>%
  left_join(tree_cones_m5, by=c("year", "grid"))

pdm1 = lmer(part_date ~ grid + mast + cone_index_tm1 + (1|squirrel_id) + (1|year), data=pd_table )
summary(pdm1) #OK proof on concept, more cones in previous year mean earlier PDs

#BANG IN THE PREVIOUS YEARS

pdm2 = lmer(part_date ~ grid + mast + 
              cone_index_tm1 +  cone_index_tm2 +  cone_index_tm3 +  cone_index_tm4 +  cone_index_tm5 +
              (1|squirrel_id) + (1|year), data=pd_table )
summary(pdm2) #so cones in tm2 has roughly the same effect as cones in tm1. tm3 is 1/3 of effect, tm4 is zero
#tm5 is quite close to tm1 and tm2, weird. Expect this is not real and to do with the masting pattern, but curious none the less

hist(residuals(pdm2)) 
plot(residuals(pdm2), predict(pdm2)) #both decent

#without year as random effect, effect of tm2 & tm3 are equal and rougly 1/6 of effect of tm1. tm4 is also opposite sign, bit odd:

pdm3 = lmer(part_date ~ grid + mast + 
              cone_index_tm1 +  cone_index_tm2 +  cone_index_tm3 +  cone_index_tm4 +  cone_index_tm5 +
              (1|squirrel_id) , data=pd_table )
summary(pdm3)

par(mfrow=c(1,2))

coefplot2(pdm2)
coefplot2(pdm3) #but would think with year is a better model
