# Replicate analyses

# Set up ----
library(tidyverse)
library(janitor)
library(lubridate)
library(readxl)
library(MASS)
library(broom)
library(kableExtra)
library(modelsummary)

allvisits <- read_csv("data/All ED VISITS RECODED.csv") %>% 
  clean_names()


# day-level analysis ----
# p 125-127
# number of visits 3 months before/after August 12 
# Black and White patients 
# Y = β0 + β1 Period + β2 Race + β3 Period x Race

daily_2017 <- allvisits %>% 
  filter(race_clean %in% c("Black", "White")) %>% 
  group_by(adm_dtm, race_clean) %>% 
  summarize(num_visits = n()) %>% 
  filter(between(adm_dtm, ymd(20170514), ymd(20171111))) %>% 
  mutate(post = ifelse(adm_dtm >= ymd(20170812), "post", "pre"),
         post = as.factor(post),
         race = factor(race_clean, levels = c("White", "Black")),
         postweek = case_when(
           adm_dtm < ymd(20170812) ~ "pre",
           adm_dtm > ymd(20170820) ~ "postpost",
           TRUE ~ "postweek"
         ),
         postweek = as.factor(postweek))

day1 <- lm(num_visits ~ post*race,
           data = daily_2017)
summary(day1)
# Exactly

day1b <- lm(num_visits ~ postweek*race,
           data = daily_2017)
summary(day1b)

# anova version
day1_aov <- aov(num_visits ~ post + race + post*race,
                data = daily_2017)

summary(day1_aov)
# Exactly

# nb version
day1_nb <- glm.nb(num_visits ~ post*race,
           data = daily_2017)
summary(day1_nb)

day1b_nb <- glm.nb(num_visits ~ postweek*race,
                  data = daily_2017)
summary(day1b_nb)

## Compare to 2016/2018
# Compare to 2016/2018
daily_2016 <- allvisits %>% 
  filter(race_clean %in% c("Black", "White")) %>% 
  group_by(adm_dtm, race_clean) %>% 
  summarize(num_visits = n()) %>% 
  filter(between(adm_dtm, ymd(20160514), ymd(20161111))) %>% 
  mutate(post = ifelse(adm_dtm >= ymd(20160812), "post", "pre"),
         post = as.factor(post),
         race = factor(race_clean, levels = c("White", "Black")),
         postweek = case_when(
           adm_dtm < ymd(20160812) ~ "pre",
           adm_dtm > ymd(20160820) ~ "postpost",
           TRUE ~ "postweek"
         ),
         postweek = as.factor(postweek))

day2 <- lm(num_visits ~ post + race + post*race,
           data = daily_2016)
summary(day2)

day2b <- lm(num_visits ~ postweek*race,
           data = daily_2016)
summary(day2b)

daily_2018 <- allvisits %>% 
  filter(race_clean %in% c("Black", "White")) %>% 
  group_by(adm_dtm, race_clean) %>% 
  summarize(num_visits = n()) %>% 
  filter(between(adm_dtm, ymd(20180514), ymd(20181111))) %>% 
  mutate(post = ifelse(adm_dtm >= ymd(20180812), "post", "pre"),
         post = as.factor(post),
         race = factor(race_clean, levels = c("White", "Black")),
         postweek = case_when(
           adm_dtm < ymd(20180812) ~ "pre",
           adm_dtm > ymd(20180820) ~ "postpost",
           TRUE ~ "postweek"
         ),
         postweek = as.factor(postweek))

day3 <- lm(num_visits ~ post + race + post*race,
           data = daily_2018)
summary(day3)

day3b <- lm(num_visits ~postweek*race,
           data = daily_2018)
summary(day3b)

modelsummary(list("2017" = day1,
                  "2016" = day2,
                  "2018" = day3))

## Visualize ---
# day-level counts, counts w/diagnoses, percent w/diagnoses
## count
daily_all <- bind_rows(daily_2016, daily_2017, daily_2018) %>% 
  mutate(monthday = format(as.Date(adm_dtm), "%m-%d"),
         year = year(adm_dtm))

daily_all %>% filter(year == 2017) %>% 
  ggplot(aes(x = monthday, y = num_visits, group = 1)) +
  geom_line() +
  geom_smooth() +
  geom_vline(xintercept = "08-12") +
  facet_wrap(~race, scales = "free_y", ncol = 1)

# all years
daily_all %>% 
  ggplot(aes(x = monthday, y = num_visits, 
             color = as.factor(year), group = as.factor(year))) +
  geom_line() +
  geom_smooth(span = 0.2, se = FALSE) +
  geom_vline(xintercept = "08-12") +
  facet_wrap(~race, scales = "free_y", ncol = 1)


# repeat for relevant diagnoses ----
daily_2017_diag <- allvisits %>% 
  mutate(diag = ifelse(anx_dep == 1 | mups1 == 1 | alcohol == 1,
                       1, 0)) %>% 
  filter(race_clean %in% c("Black", "White"),
         diag == 1) %>% 
  group_by(adm_dtm, race_clean) %>% 
  summarize(num_visits = n()) %>% 
  filter(between(adm_dtm, ymd(20170514), ymd(20171111))) %>% 
  mutate(post = ifelse(adm_dtm >= ymd(20170812), "post", "pre"),
         post = as.factor(post),
         race = factor(race_clean, levels = c("White", "Black")),
         postweek = case_when(
           adm_dtm < ymd(20170812) ~ "pre",
           adm_dtm > ymd(20170820) ~ "postpost",
           TRUE ~ "postweek"
         ),
         postweek = as.factor(postweek))

day1_diag <- lm(num_visits ~ post + race + post*race,
                data = daily_2017_diag)
summary(day1_diag)

day1b_diag <- lm(num_visits ~ postweek*race,
                data = daily_2017_diag)
summary(day1b_diag)

daily_2016_diag <- allvisits %>% 
  mutate(diag = ifelse(anx_dep == 1 | mups1 == 1 | alcohol == 1,
                       1, 0)) %>% 
  filter(race_clean %in% c("Black", "White"),
         diag == 1) %>% 
  group_by(adm_dtm, race_clean) %>% 
  summarize(num_visits = n()) %>% 
  filter(between(adm_dtm, ymd(20160514), ymd(20161111))) %>% 
  mutate(post = ifelse(adm_dtm >= ymd(20160812), "post", "pre"),
         post = as.factor(post),
         race = factor(race_clean, levels = c("White", "Black")),
         postweek = case_when(
           adm_dtm < ymd(20160812) ~ "pre",
           adm_dtm > ymd(20160820) ~ "postpost",
           TRUE ~ "postweek"
         ),
         postweek = as.factor(postweek))

day2_diag <- lm(num_visits ~ post + race + post*race,
                data = daily_2016_diag)
summary(day2_diag)

day2b_diag <- lm(num_visits ~ postweek*race,
                data = daily_2016_diag)
summary(day2b_diag)

daily_2018_diag <- allvisits %>% 
  mutate(diag = ifelse(anx_dep == 1 | mups1 == 1 | alcohol == 1,
                       1, 0)) %>% 
  filter(race_clean %in% c("Black", "White"),
         diag == 1) %>% 
  group_by(adm_dtm, race_clean) %>% 
  summarize(num_visits = n()) %>% 
  filter(between(adm_dtm, ymd(20180514), ymd(20181111))) %>% 
  mutate(post = ifelse(adm_dtm >= ymd(20180812), "post", "pre"),
         post = as.factor(post),
         race = factor(race_clean, levels = c("White", "Black")),
         postweek = case_when(
           adm_dtm < ymd(20180812) ~ "pre",
           adm_dtm > ymd(20180820) ~ "postpost",
           TRUE ~ "postweek"
         ),
         postweek = as.factor(postweek))

day3_diag <- lm(num_visits ~ post + race + post*race,
                data = daily_2018_diag)
summary(day3_diag)

day3b_diag <- lm(num_visits ~ postweek*race,
                data = daily_2018_diag)
summary(day3b_diag)

daily_all_diag <- bind_rows(daily_2016_diag, daily_2017_diag, daily_2018_diag) %>% 
  mutate(monthday = format(as.Date(adm_dtm), "%m-%d"),
         year = year(adm_dtm))

daily_all_diag %>% filter(year == 2017) %>% 
  ggplot(aes(x = monthday, y = num_visits, group = 1)) +
  geom_line() +
  geom_vline(xintercept = "08-12") +
  facet_wrap(~race, scales = "free_y", ncol = 1)

## percent of diagnoses
daily_2017_perc <- daily_2017 %>% 
  left_join(daily_2017_diag %>% 
              rename(num_diag_visits = num_visits)) %>% 
  mutate(perc_diag = (num_diag_visits/num_visits)*100)

day1_percent <- lm(perc_diag ~ post + race + post*race,
                data = daily_2017_perc)
summary(day1_percent)

day1b_percent <- lm(perc_diag ~ postweek*race,
                   data = daily_2017_perc)
summary(day1b_percent)

daily_2017_perc %>% 
  ggplot(aes(x = adm_dtm, y = perc_diag, group = 1)) +
  geom_line() +
  geom_vline(xintercept = as.Date("2017-08-12")) +
  facet_wrap(~race, scales = "free_y", ncol = 1)

daily_2016_perc <- daily_2016 %>% 
  left_join(daily_2016_diag %>% 
              rename(num_diag_visits = num_visits)) %>% 
  mutate(perc_diag = (num_diag_visits/num_visits)*100)

daily_2018_perc <- daily_2018 %>% 
  left_join(daily_2018_diag %>% 
              rename(num_diag_visits = num_visits)) %>% 
  mutate(perc_diag = (num_diag_visits/num_visits)*100)

daily_all_perc <- bind_rows(daily_2016_perc, daily_2017_perc, daily_2018_perc) %>% 
  mutate(monthday = format(as.Date(adm_dtm), "%m-%d"),
         year = year(adm_dtm))

daily_all_perc %>% filter(year == 2017) %>% 
  ggplot(aes(x = monthday, y = perc_diag, group = 1)) +
  geom_line() +
  geom_smooth(span = 0.2, se = FALSE) +
  geom_vline(xintercept = "08-12") +
  facet_wrap(~race, scales = "free_y", ncol = 1) +
  labs(title = "Percent of All Visits with Relevant Diagnoses")

# all years
daily_all_perc %>% 
  ggplot(aes(x = monthday, y = perc_diag, 
             color = as.factor(year), group = as.factor(year))) +
  geom_line() +
  geom_smooth(span = 0.2, se = FALSE) +
  geom_vline(xintercept = "08-12") +
  facet_wrap(~race, scales = "free_y", ncol = 1) +
  labs(title = "Percent of All Visits with Relevant Diagnoses")

# repeat for relevant diagnoses, excluding aged 18-21 ----
daily_2017_diag_over21 <- allvisits %>% 
  mutate(diag = ifelse(anx_dep == 1 | mups1 == 1 | alcohol == 1,
                       1, 0)) %>% 
  filter(race_clean %in% c("Black", "White"),
         diag == 1, 
         age_at_event > 21) %>% 
  group_by(adm_dtm, race_clean) %>% 
  summarize(num_visits = n()) %>% 
  filter(between(adm_dtm, ymd(20170514), ymd(20171111))) %>% 
  mutate(post = ifelse(adm_dtm >= ymd(20170812), "post", "pre"),
         post = as.factor(post),
         race = factor(race_clean, levels = c("White", "Black")),
         postweek = case_when(
           adm_dtm < ymd(20170812) ~ "pre",
           adm_dtm > ymd(20170820) ~ "postpost",
           TRUE ~ "postweek"
         ),
         postweek = as.factor(postweek))

day1_diag_over21 <- lm(num_visits ~ post + race + post*race,
                data = daily_2017_diag_over21)
summary(day1_diag_over21)

day1b_diag_over21 <- lm(num_visits ~ postweek*race,
                 data = daily_2017_diag_over21)
summary(day1b_diag_over21)

# predictions for negative binomial
newdata1 <- crossing(post = c("pre", "post"), race = c("Black", "White"))
newdata1$phat <- predict(day1_nb, newdata1, type = "response")
newdata1


## next steps ----
# for relevant diagnoses only
daily_2017_diag <- allvisits %>% 
  filter(race_clean %in% c("Black", "White"),
         anx_dep == 1 | mups1 == 1 | alcohol == 1) %>% 
  group_by(adm_dtm, race_clean) %>% 
  summarize(num_visits = n()) %>% 
  filter(between(adm_dtm, ymd(20170514), ymd(20171111))) %>% 
  mutate(post = ifelse(adm_dtm >= ymd(20170812), "post", "pre"),
         post = as.factor(post),
         race = factor(race_clean, levels = c("White", "Black")))

day1_diag <- lm(num_visits ~ post*race,
           data = daily_2017_diag)
summary(day1_diag)

daily_2017_diag %>% 
  group_by(post, race) %>%
  summarise(
    mean = mean(num_visits, na.rm = TRUE),
    sd = sd(num_visits, na.rm = TRUE)
  )

day1_aov_diag <- aov(num_visits ~ post + race + post*race,
                data = daily_2017_diag)

summary(day1_aov_diag)



## mups only ----
daily_2017_mups <- allvisits %>% 
  filter(race_clean %in% c("Black", "White"),
         mups1 == 1) %>% 
  group_by(adm_dtm, race_clean) %>% 
  summarize(num_visits = n()) %>% 
  filter(between(adm_dtm, ymd(20170514), ymd(20171111))) %>% 
  mutate(post = ifelse(adm_dtm >= ymd(20170812), "post", "pre"),
         post = as.factor(post),
         race = factor(race_clean, levels = c("White", "Black")))

day1_mups <- lm(num_visits ~ post*race,
                data = daily_2017_mups)
summary(day1_mups)

daily_2017_mups %>% 
  group_by(post, race) %>%
  summarise(
    mean = mean(num_visits, na.rm = TRUE),
    sd = sd(num_visits, na.rm = TRUE)
  )

day1_aov_mups <- aov(num_visits ~ post + race + post*race,
                     data = daily_2017_mups)

summary(day1_aov_mups)

## anxiety-depression only ----
daily_2017_anx <- allvisits %>% 
  filter(race_clean %in% c("Black", "White"),
         anx_dep == 1) %>% 
  group_by(adm_dtm, race_clean) %>% 
  summarize(num_visits = n()) %>% 
  filter(between(adm_dtm, ymd(20170514), ymd(20171111))) %>% 
  mutate(post = ifelse(adm_dtm >= ymd(20170812), "post", "pre"),
         post = as.factor(post),
         race = factor(race_clean, levels = c("White", "Black")))

day1_anx <- lm(num_visits ~ post*race,
                data = daily_2017_anx)
summary(day1_anx)

daily_2017_anx %>% 
  group_by(post, race) %>%
  summarise(
    mean = mean(num_visits, na.rm = TRUE),
    sd = sd(num_visits, na.rm = TRUE)
  )

day1_aov_anx <- aov(num_visits ~ post + race + post*race,
                     data = daily_2017_anx)

summary(day1_aov_anx)

## alcohol only ----
daily_2017_alc <- allvisits %>% 
  filter(race_clean %in% c("Black", "White"),
         alcohol == 1) %>% 
  group_by(adm_dtm, race_clean) %>% 
  summarize(num_visits = n()) %>% 
  filter(between(adm_dtm, ymd(20170514), ymd(20171111))) %>% 
  mutate(post = ifelse(adm_dtm >= ymd(20170812), "post", "pre"),
         post = as.factor(post),
         race = factor(race_clean, levels = c("White", "Black")))

day1_alc <- lm(num_visits ~ post*race,
               data = daily_2017_alc)
summary(day1_alc)

daily_2017_alc %>% 
  group_by(post, race) %>%
  summarise(
    mean = mean(num_visits, na.rm = TRUE),
    sd = sd(num_visits, na.rm = TRUE)
  )

day1_aov_alc <- aov(num_visits ~ post + race + post*race,
                    data = daily_2017_alc)

summary(day1_aov_alc)


# individual-level analysis ----
# p 126-129
# Y = β0 + β1 Period + β2 Day + β3 Race + β4 Period x Day + β5 Period x Race + β6 Day x Race + β6 Period x Day x Race
# plus age, sex, weekend as controls

# black/white patients in 2017
allvisits %>% filter(race_clean %in% c("Black", "White")) %>% 
  filter(between(adm_dtm, ymd(20170730), ymd(20170826))) %>% 
  count(race_clean)

# group by year, race and run models across each group

## mups ----
by_year_race_mups <- allvisits %>% 
  # keep only patients categorized as Black or White
  filter(race_clean %in% c("Black", "White")) %>% 
  # keep only cases within 2 weeks of 8/12
  filter(between(adm_dtm, ymd(20170730), ymd(20170826)) |
           between(adm_dtm, ymd(20160730), ymd(20160826)) |
           between(adm_dtm, ymd(20180730), ymd(20180826))) %>% 
  # reorder race, create number of days since 8/12, create weekend flag
  mutate(race = factor(race_clean, levels = c("Black", "White")),
         days = case_when(
           adm_year == 2016 ~ abs(adm_dtm - ymd(20160812)),
           adm_year == 2017 ~ abs(adm_dtm - ymd(20170812)),
           adm_year == 2018 ~ abs(adm_dtm - ymd(20180812))
         ),
         weekend = ifelse(wday(adm_dtm, week_start = 1) >= 6,
                          "weekend", "weekday")
  ) %>% 
  # nest by year, race to run models separately on each subset
  group_by(adm_year, race) %>% 
  nest()


# Define model
model1 <- function(df) {
  lm(mups1 ~ age_at_event + sex + weekend + pre_post_yearly*days, data = df)
}

# Run model
by_year_race_mups <- by_year_race_mups %>% 
  mutate(model = map(data, model1))

# View results
by_year_race_mups %>%
  arrange(adm_year, race) %>% 
  mutate(tidied = map(model, tidy)) %>% 
  dplyr::select(adm_year, race, tidied) %>% 
  unnest(tidied) %>% 
  filter(term %in% c("pre_post_yearly", "days", "pre_post_yearly:days")) %>% 
  kbl() %>% 
  kable_styling() %>% 
  row_spec(c(1:3,7:9,13:15), background = "lightgray")


## alcohol ----
by_year_race_alc <- allvisits %>% 
  # keep only patients categorized as Black or White
  filter(race_clean %in% c("Black", "White")) %>% 
  # keep only cases within 2 weeks of 8/12
  filter(between(adm_dtm, ymd(20170729), ymd(20170826)) |
           between(adm_dtm, ymd(20160729), ymd(20160826)) |
           between(adm_dtm, ymd(20180729), ymd(20180826))) %>% 
  # reorder race, create number of days since 8/12, create weekend flag
  mutate(race = factor(race_clean, levels = c("Black", "White")),
         days = case_when(
           adm_year == 2016 ~ abs(adm_dtm - ymd(20160812)),
           adm_year == 2017 ~ abs(adm_dtm - ymd(20170812)),
           adm_year == 2018 ~ abs(adm_dtm - ymd(20180812))
         ),
         weekend = ifelse(wday(adm_dtm, week_start = 1) >= 6,
                          "weekend", "weekday")
  ) %>% 
  # nest by year, race to run models separately on each subset
  group_by(adm_year, race) %>% 
  nest()

# Define model
model2 <- function(df) {
  lm(alcohol ~ age_at_event + sex + weekend +pre_post_yearly*days, data = df)
}

# Run model
by_year_race_alc <- by_year_race_alc %>% 
  mutate(model = map(data, model2))

# View results
by_year_race_alc %>%
  arrange(adm_year, race) %>% 
  mutate(tidied = map(model, tidy)) %>% 
  dplyr::select(adm_year, race, tidied) %>% 
  unnest(tidied) %>% 
  filter(term %in% c("pre_post_yearly", "days", "pre_post_yearly:days")) %>% 
  kbl() %>% 
  kable_styling() %>% 
  row_spec(c(1:3,7:9,13:15),background = "lightgray")


## anxiety-depression ----
by_year_race_anx <- allvisits %>% 
  # keep only patients categorized as Black or White
  filter(race_clean %in% c("Black", "White")) %>% 
  # keep only cases within 2 weeks of 8/12
  filter(between(adm_dtm, ymd(20170729), ymd(20170826)) |
           between(adm_dtm, ymd(20160729), ymd(20160826)) |
           between(adm_dtm, ymd(20180729), ymd(20180826))) %>% 
  # reorder race, create number of days since 8/12, create weekend flag
  mutate(race = factor(race_clean, levels = c("Black", "White")),
         days = case_when(
           adm_year == 2016 ~ abs(adm_dtm - ymd(20160812)),
           adm_year == 2017 ~ abs(adm_dtm - ymd(20170812)),
           adm_year == 2018 ~ abs(adm_dtm - ymd(20180812))
         ),
         weekend = ifelse(wday(adm_dtm, week_start = 1) >= 6,
                          "weekend", "weekday")
  ) %>% 
  # nest by year, race to run models separately on each subset
  group_by(adm_year, race) %>% 
  nest()

# Define model
model3 <- function(df) {
  lm(anx_dep ~ age_at_event + sex + weekend +pre_post_yearly*days, data = df)
}

# Run model
by_year_race_anx <- by_year_race_anx %>% 
  mutate(model = map(data, model3))

# View results
by_year_race_anx %>%
  arrange(adm_year, race) %>% 
  mutate(tidied = map(model, tidy)) %>% 
  select(adm_year, race, tidied) %>% 
  unnest(tidied) %>% 
  filter(term %in% c("pre_post_yearly", "days", "pre_post_yearly:days")) %>% 
  kbl() %>% 
  kable_styling() %>% 
  row_spec(c(1:3,7:9,13:15),background = "lightgray")


# NEXT
# Review a few variables...

# NEXT
# outline analysis strategies