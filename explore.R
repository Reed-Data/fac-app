library(readr)
thes_fac <- read_rds("data/thes_fac.RDS")
d <- read_rds("data/d.RDS")

highest_adv_count <- thes_fac %>% group_by(year) %>% 
  top_n(5, wt = advisees) %>% 
  arrange(year, advisees)

# Counts labs?  Doesn't match with Total Units Taught in FTE.xlsx
d %>% group_by(Subj, SchoolYear) %>% 
  summarize(total_units = sum(Census_enrlment)) %>% 
  spread(key = SchoolYear, value = total_units)
