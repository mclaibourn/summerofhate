# Familiarize with data
# 2023-03-27

library(tidyverse)
library(lubridate)

admitdiag <- read_csv("data/admitting_diagnosis.csv")
# all ED admits, all diagnoses?
# includes 2016, 2017, 2018, 2019
# all year

allvisits <- read_csv("data/All ED VISITS RECODED.csv")
# includes 2016, 2017, 2018 only
# all year
# additional derived variables

weeks <- read_csv("data/weeks20to45.csv")
# includes 2016, 2017, 2018
# mid-May to mid-November only
# additional derived variables

# overall: percent admitted for each defined diagnostic category
#   during 6 month period
weeks %>% 
  group_by(MUPS) %>% 
  summarize(dcount = n()) %>% 
  mutate(dpercent = dcount/sum(dcount))

weeks %>% 
  group_by(Alcohol) %>% 
  summarize(dcount = n()) %>% 
  mutate(dpercent = dcount/sum(dcount))

weeks %>% 
  group_by(ANX_DEP) %>% 
  summarize(dcount = n()) %>% 
  mutate(dpercent = dcount/sum(dcount))

weeks %>% 
  group_by(New_combined) %>% 
  summarize(dcount = n()) %>% 
  mutate(dpercent = dcount/sum(dcount))

# make it into a single data frame
weeks <- weeks %>% 
  mutate(diagnoses = case_when(
    MUPS == 1 ~ "mups",
    ANX_DEP == 1 ~ "axdp",
    Alcohol == 1 ~ "alc",
    TRUE ~ "other"
  ))
weeks %>% 
  group_by(diagnoses) %>% 
  summarize(dcount = n()) %>% 
  mutate(dpercent = dcount/sum(dcount))

# create percent by week and graph
byweek <- weeks %>% 
  group_by(Weeks, diagnoses) %>% 
  summarize(dcount = n()) %>% 
  left_join(weeks %>% 
              group_by(Weeks) %>% 
              summarize(count = n())) %>% 
  mutate(dpercent = dcount/count)

ggplot(byweek, aes(x = Weeks, y = dpercent)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 33) +
  facet_wrap(~diagnoses, scales = "free_y")

# now do the same thing by year
weeks <- weeks %>% 
  mutate(year = year(Adm_Dtm))

byweekyear <- weeks %>% 
  group_by(year, Weeks, diagnoses) %>% 
  summarize(dcount = n()) %>% 
  left_join(weeks %>% 
              group_by(year, Weeks) %>% 
              summarize(count = n())) %>% 
  mutate(dpercent = dcount/count)
  
ggplot(byweekyear, aes(x = Weeks, y = dpercent, color = as.factor(year))) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 32) +
  facet_wrap(~diagnoses, scales = "free_y")

# now do the same thing by race (Black, white)
byweekyearrace <- weeks %>% 
  filter(Race_Clean %in% c("Black", "White")) %>% 
  group_by(year, Weeks, Race_Clean, diagnoses) %>% 
  summarize(dcount = n()) %>% 
  left_join(weeks %>% 
              filter(Race_Clean %in% c("Black", "White")) %>% 
              group_by(year, Weeks, Race_Clean) %>% 
              summarize(count = n())) %>% 
  mutate(dpercent = dcount/count)

# number of visits
byweekyearrace %>% 
  filter(year == 2017) %>% 
  ggplot(aes(x = Weeks, y = dcount, color = Race_Clean)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 27) +
  geom_vline(xintercept = 32) +
  facet_wrap(~Race_Clean + diagnoses, 
             scales = "free_y", nrow = 2) +
  theme(legend.position = "none")

# percent of visits
byweekyearrace %>% 
  filter(year == 2017) %>% 
  ggplot(aes(x = Weeks, y = dpercent, color = Race_Clean)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 27) +
  geom_vline(xintercept = 32) +
  facet_wrap(~diagnoses, 
             scales = "free_y", nrow = 2) 

# filtering out 22 and younger
weeks %>% 
  mutate(age_over21 = ifelse(age_at_event > 22, 1, 0)) %>% 
  count(age_over21) # about 8% (almost 10% if include 22)

byweekyearrace_over21 <- weeks %>% 
  filter(Race_Clean %in% c("Black", "White"),
         age_at_event > 21) %>% 
  group_by(year, Weeks, Race_Clean, diagnoses) %>% 
  summarize(dcount = n()) %>% 
  left_join(weeks %>% 
              filter(Race_Clean %in% c("Black", "White")) %>% 
              group_by(year, Weeks, Race_Clean) %>% 
              summarize(count = n())) %>% 
  mutate(dpercent = dcount/count)

# number of visits
byweekyearrace_over21 %>% 
  filter(year == 2017) %>% 
  ggplot(aes(x = Weeks, y = dcount, color = Race_Clean)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 27) +
  geom_vline(xintercept = 32) +
  facet_wrap(~Race_Clean + diagnoses, 
             scales = "free_y", nrow = 2) +
  theme(legend.position = "none")

# percent of visits
byweekyearrace_over21 %>% 
  filter(year == 2017) %>% 
ggplot(aes(x = Weeks, y = dpercent, color = Race_Clean)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 27) +
  geom_vline(xintercept = 32) +
  facet_wrap(~diagnoses, 
             scales = "free_y", nrow = 2)

# Only visits by Black patients, all years
byweekyearrace_over21 %>% 
  filter(Race_Clean == "Black")  %>% 
  ggplot(aes(x = Weeks, y = dcount, color = as.factor(year))) +
  geom_line() +
  geom_point() +
  scale_color_discrete(name = "Year") +
  geom_vline(xintercept = 27) +
  geom_vline(xintercept = 32) +
  facet_wrap(~diagnoses, scales = "free_y") 

# next steps

# mean age of patients to ER among black, white patients
byweekyearrace_meanage <- weeks %>% 
  filter(Race_Clean %in% c("Black", "White")) %>% 
  group_by(year, Weeks, Race_Clean) %>% 
  summarize(meanage = mean(age_at_event, na.rm = TRUE)) 

byweekyearrace_meanage %>% 
  #filter(Race_Clean == "Black")  %>% 
  ggplot(aes(x = Weeks, y = meanage, color = Race_Clean)) +
  geom_line() +
  geom_point() +
  #scale_color_discrete(name = "Year") +
  geom_vline(xintercept = 27) +
  geom_vline(xintercept = 32) +
  facet_wrap(~year) 

