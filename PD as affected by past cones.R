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
         mast = year %in% c(1993, 1998, 2005, 2010, 2014),
         years_since_mast = factor(ifelse(year>=2014, year-2014, 
                                ifelse(year>=2010, year-2010,
                                       ifelse(year>=2005, year-2005,
                                              ifelse(year>=1998, year-1998,
                                                     ifelse(year>=1993, year-1993, NA))))))) %>%
  left_join(tree_cones_m1, by=c("year", "grid")) %>% #again, bit inelegant, but works
  left_join(tree_cones_m2, by=c("year", "grid")) %>%
  left_join(tree_cones_m3, by=c("year", "grid")) %>%
  left_join(tree_cones_m4, by=c("year", "grid")) %>%
  left_join(tree_cones_m5, by=c("year", "grid"))

summary(pd_table)

pdm1 = lmer(part_date ~   mast + grid + cone_index_tm1 + (1|squirrel_id) + (1|year), data=pd_table )
summary(pdm1) #OK proof of concept, more cones in previous year mean earlier PDs. 

#Chuck in the previous years

pdm2 = lmer(part_date ~ mast + grid +  
              cone_index_tm1 +  cone_index_tm2 +  cone_index_tm3 +  cone_index_tm4  + 
              (1|squirrel_id) + (1|year), data=pd_table )
summary(pdm2) #so  cones in tm2 have roughly 69% of the effect of cones in tm1. tm3 is 44% of effect, tm4 is 6% and opposite sign

hist(residuals(pdm2)) 
plot(residuals(pdm2), predict(pdm2)) #both decent

#Does it vary by grids?
pdm2b = lmer(part_date ~ mast + grid *  
              (cone_index_tm1 +  cone_index_tm2 +  cone_index_tm3 +  cone_index_tm4)  + 
              (1|squirrel_id) + (1|year), data=pd_table )
summary(pdm2b) #so on KL cones in tm2 have roughly 46% of the effect of cones in tm1. tm3 is 28% of effect, tm4 is 7% and opposite sign
#on SU it tm2 is 11% of tm1, while tm3 is 18% of tm1 and tm4 is 3% on tm1 and opposite sign
anova(pdm2, pdm2b) #highly sig, delta AIC = 33
#So yes, but frankly we have not mechanism for that so I'm ignoring it for now

#If you fit tm5 it changes some things and comes out quite close to tm1 and tm2, weird. 
#Expect this is not real and to do with the masting pattern, but curious none the less

#without year as random effect, effect of tm2 is abut 17% the effect of tm1, as is tm3, while tm4 is positive

pdm3 = lmer(part_date ~ grid + mast + 
              cone_index_tm1 +  cone_index_tm2 +  cone_index_tm3 +  cone_index_tm4 +  
              (1|squirrel_id) , data=pd_table )
summary(pdm3)

par(mfrow=c(1,2))

coefplot2(pdm2)
coefplot2(pdm3) #but would think with year is a better model


#Based on the model with year (pdm2), cone availability per year is approx tm1 + 0.69*tm2 + 0.44*tm3:

total_cones = tree_cones %>% 
  left_join(tree_cones_m1) %>%
  left_join(tree_cones_m2) %>%
  left_join(tree_cones_m3) %>%
  left_join(tree_cones_m4) %>%
  mutate(total_cone_index = cone_index_tm1 + 0.69*cone_index_tm2 + 0.44*cone_index_tm3)

pd_table = pd_table %>% left_join((total_cones %>% select(year, grid, total_cone_index)))

pdm4 =  lmer(part_date ~ grid + mast +  
                     total_cone_index  +
                     (1|squirrel_id) + (1|year), data=pd_table )
summary(pdm4) #gives a coef very similar to tm1 in pdm2 (-6.48 vs -6.65 here), to me suggests historic cones are being weighted correctly

#as y ~ -6.48tm1 - 4.54tm2 - 2.82tm3 is roughly the same as y ~ -6.65(tm1 + 0.7*tm2 + 0.4*tm2) 

#compare to model with years since mast:
pdm5 =  lmer(part_date ~ grid + #dropped mast as that is the level "0" in the years_since_mast factor  
               years_since_mast +
               (1|squirrel_id) + (1|year), data=pd_table )
summary(pdm5)

AICc(pdm5) #12632
AICc(pdm4) #13343

#with both?
pdm6 =  lmer(part_date ~ grid + 
               total_cone_index +
               years_since_mast +
               (1|squirrel_id) + (1|year), data=pd_table )

summary(pdm6)
AICc(pdm6) #12625

#So years since mast is a much better explanatory variable for part date, but the total cone index does add something

pdm7 = lmer(part_date ~ grid + years_since_mast +  
              cone_index_tm1 +  cone_index_tm2 +  cone_index_tm3 +  cone_index_tm4  + 
              (1|squirrel_id) + (1|year), data=pd_table )
summary(pdm7)
AICc(pdm7) #12619
#Ultimately this is the best model, but the "total_cone_index" could be useful for other things

#For instance, it approximates the resources available "in the system", including cones in the ground
#This might be useful for defining years as high or low resource
#Can see how this has been changing over time.

with(total_cones, plot(year, total_cone_index, type="b", xlim=c(1991,2017)))
