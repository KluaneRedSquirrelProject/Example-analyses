#Start####

require(krsp)
require(lubridate)
require(lme4)
require(tidyverse)


#Connecting to and drawing tables from database####

con = krsp_connect(group = "krsp-aws")


litter = tbl(con, "litter")
juv = tbl(con, "juvenile")
trapping = tbl(con, "trapping")



#Creating initial data tables####

litters = litter  %>% filter(ln ==1) %>%
  select(litter_id=id, birth_date=fieldBDate, grid, date1, tagDt, ln,
         locx, locy, nx1, ny1, nx2, ny2, mother_id= squirrel_id, year=yr) 

weights = trapping %>% filter(month(date) == 8, gr %in% c("KL", "SU"), !is.na(wgt)) %>%
  group_by(squirrel_id) %>% collect() %>%
  top_n(-1,date) %>% top_n(1,id) %>% #take first date in August, otherwise arbitrary
  select(squirrel_id, wgt, w_date = date)

juvs = juv %>% select(id, squirrel_id, sex, weight, tagwt=tagWT, litter_id) %>%
   collect() %>%
left_join(litters, by="litter_id", copy=T) %>%
  left_join(weights, by="squirrel_id") %>%
  filter(grid %in% c("KL", "SU"), wgt>75, year(w_date) == year)  %>%
  mutate(b_date = yday(ymd(birth_date)),
    nest_days = as.numeric(difftime(tagDt, date1, units = "days")),
                                        growth_rate=(tagwt-weight)/nest_days,
                                        growth_rate = if_else(is.na(weight) | !between(weight, 0, 50),
                                                              NA_real_, growth_rate),
                                        growth_rate = if_else(is.na(tagwt) | !between(tagwt, 0, 100),
                                                              NA_real_, growth_rate),
                                        growth_rate = if_else(nest_days < 5, NA_real_, growth_rate),
                                        growth_rate = if_else(growth_rate < 0, NA_real_, growth_rate))

with(juvs, plot(growth_rate, wgt))
with(juvs, cor.test(growth_rate, wgt)) #sig correlated, r = 0.45, p < 0.001

with(juvs, plot(b_date, wgt))
with(juvs, cor.test(b_date, wgt)) #sig negatively correlated, r = -0.65, p < 0.001









