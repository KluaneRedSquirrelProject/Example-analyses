require(krsp)
require(tidyverse)
require(nlme)




#Connecting to and drawing tables from database####

con = krsp_connect(group = "krsp-aws")


litter = tbl(con, "litter")
juv = tbl(con, "juvenile")
flall = tbl(con, "flastall2") 

#Creating data table####

litters = litter  %>% filter(ln ==1) %>%
  select(litter_id=id, birth_date=fieldBDate, grid, date1, tagDt, ln,
          mother_id= squirrel_id, year=yr) 

juvs = juv %>% select(id, squirrel_id, sex, weight, tagwt=tagWT, litter_id) %>%
  collect() %>%
  left_join(litters, by="litter_id", copy=T) %>%
  filter(grid %in% c("KL", "SU"))  %>%
  mutate(nest_days = as.numeric(difftime(tagDt, date1, units = "days")),
         growth_rate=(tagwt-weight)/nest_days,
         growth_rate = if_else(is.na(weight) | !between(weight, 0, 50),
                               NA_real_, growth_rate),
         growth_rate = if_else(is.na(tagwt) | !between(tagwt, 0, 100),
                               NA_real_, growth_rate),
         growth_rate = if_else(nest_days < 5, NA_real_, growth_rate),
         growth_rate = if_else(growth_rate < 0, NA_real_, growth_rate))


dads = flall %>% filter(!is.na(sire_id), gr %in% c("SU", "KL"), !is.na(litter_id)) %>%
  group_by(litter_id) %>% collect() %>%
  summarise(n_dads = length(unique(sire_id))) %>% ungroup() %>%
           mutate(mum_id = dam_id) 

with(dads, hist(n_dads))

dads %>% filter(n_dads == max(n_dads)) #2 litters with 5 fathers

flall %>% filter(litter_id %in% c("3847", "5910"))

litter_table = juvs %>% group_by(litter_id) %>%
  summarise(mean_gr = mean(growth_rate, na.rm=T),
            sd_gr = sd(growth_rate, na.rm=T),
            litter_size = n()) %>%
  filter(!is.na(mean_gr)) %>%
    left_join(dads, by="litter_id") %>%
  left_join(litters, by="litter_id", copy=T) %>%
  filter(!is.na(n_dads), !is.na(sd_gr))

with(litter_table, hist(mean_gr))
with(litter_table, hist(sd_gr))

with(litter_table, plot(mean_gr ~ n_dads))
with(litter_table, plot(sd_gr ~ n_dads))

lm(mean_gr ~ litter_size + n_dads, data=litter_table)
lm(sd_gr ~ litter_size + n_dads, data=litter_table)

dads_m1 = lme(mean_gr ~ litter_size + n_dads,  random=list(year=~1, mother_id=~1), data = litter_table ) 
summary(dads_m1) #nothing

dads_m2 = lme(sd_gr ~ litter_size + n_dads,  random=list(year=~1, mother_id=~1), data = litter_table ) 
summary(dads_m2) #nothing


