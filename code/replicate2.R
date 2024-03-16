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

# 1. Day-level analysis: initial ----
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
           adm_dtm > ymd(20170825) ~ "postlater",
           TRUE ~ "post2weeks"
         ),
         postweek = as.factor(postweek))

## as model
day1 <- lm(num_visits ~ post*race,
           data = daily_2017)

tidy(day1) %>%
  kbl() %>% 
  kable_styling(full_width = FALSE)

## as anova
daily_2017 %>% 
  group_by(post, race) %>%
  summarise(
    mean = mean(num_visits, na.rm = TRUE),
    sd = sd(num_visits, na.rm = TRUE)
  ) %>% 
  kbl() %>% 
  kable_styling(full_width = FALSE)

day1_aov <- aov(num_visits ~ post + race + post*race,
                data = daily_2017)

tidy(day1_aov) %>% 
  kbl() %>% 
  kable_styling(full_width = FALSE)

# Compare 2016/2018 ----
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
           adm_dtm > ymd(20160825) ~ "postlater",
           TRUE ~ "post2weeks"
         ),
         postweek = as.factor(postweek))

day2 <- lm(num_visits ~ post + race + post*race,
           data = daily_2016)

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
           adm_dtm > ymd(20180825) ~ "postlater",
           TRUE ~ "post2weeks"
         ),
         postweek = as.factor(postweek))

day3 <- lm(num_visits ~ post + race + post*race,
           data = daily_2018)

modelsummary(list("2017" = day1,
                  "2016" = day2,
                  "2018" = day3),
             stars = TRUE,
             gof_map = c("nobs", "r.squared", "rmse"))


## shift to count
day1_nb <- glm.nb(num_visits ~ post*race,
                  data = daily_2017)
day2_nb <- glm.nb(num_visits ~ post + race + post*race,
                  data = daily_2016)
day3_nb <- glm.nb(num_visits ~ post + race + post*race,
                  data = daily_2018)

modelsummary(list("2017" = day1_nb,
                  "2016" = day2_nb,
                  "2018" = day3_nb),
             stars = TRUE,
             gof_map = c("nobs", "rmse"))

# Visualize ----
## all visits
daily_all <- bind_rows(daily_2016, daily_2017, daily_2018) %>% 
  mutate(monthday = format(as.Date(adm_dtm), "%m-%d"),
         year = year(adm_dtm))

daily_all %>% filter(year == 2017) %>% 
  ggplot(aes(x = monthday, y = num_visits, group = 1)) +
  geom_line() +
  geom_smooth(span = 0.2, se = FALSE) +
  geom_vline(xintercept = "08-12") +
  facet_wrap(~race, scales = "free_y", ncol = 1) +
  labs(title = "Count of All Visits")

## relevant diagnoses
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
           adm_dtm > ymd(20170825) ~ "postlater",
           TRUE ~ "post2weeks"
         ),
         postweek = as.factor(postweek))

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
           adm_dtm > ymd(20160825) ~ "postlater",
           TRUE ~ "post2weeks"
         ),
         postweek = as.factor(postweek))

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
           adm_dtm > ymd(20180825) ~ "postlater",
           TRUE ~ "post2weeks"
         ),
         postweek = as.factor(postweek))

daily_all_diag <- bind_rows(daily_2016_diag, daily_2017_diag, daily_2018_diag) %>% 
  mutate(monthday = format(as.Date(adm_dtm), "%m-%d"),
         year = year(adm_dtm))

daily_all_diag %>% filter(year == 2017) %>% 
  ggplot(aes(x = monthday, y = num_visits, group = 1)) +
  geom_line() +
  geom_smooth(span = 0.2, se = FALSE) +
  geom_vline(xintercept = "08-12") +
  facet_wrap(~race, scales = "free_y", ncol = 1) +
  labs(title = "Count of Visits with Relevant Diagnoses")

## percent of relevant diagnoses
daily_2017_perc <- daily_2017 %>% 
  left_join(daily_2017_diag %>% 
              rename(num_diag_visits = num_visits)) %>% 
  mutate(perc_diag = (num_diag_visits/num_visits)*100)

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

# Day-level analysis: next steps ----
## initial period of 1 versus 2 versus 3 weeks
## for relevant diagnoses
daily_2017_diag_1week <- allvisits %>% 
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
           adm_dtm >= ymd(20170812) & adm_dtm < ymd(20170819) ~ "week1",
           adm_dtm >= ymd(20170819) & adm_dtm < ymd(20170826) ~ "week2",
           adm_dtm >= ymd(20170826) & adm_dtm < ymd(20170902) ~ "week3",
           adm_dtm >= ymd(20170902) & adm_dtm < ymd(20170909) ~ "week4",
           adm_dtm >= ymd(20170909) & adm_dtm < ymd(20170916) ~ "week5",
           adm_dtm >= ymd(20170916) & adm_dtm < ymd(20170923) ~ "week6",
           adm_dtm >= ymd(20170923) ~ "post"
         ),
         postweek = as.factor(postweek))

day1_weeks_all <- lm(num_visits ~ postweek,
                     data = daily_2017_diag_1week)

day1_weeks_race <- lm(num_visits ~ postweek*race,
                      data = daily_2017_diag_1week)

modelsummary(list("Weeks" = day1_weeks_all, "Weeks_by_Race" = day1_weeks_race),
             stars = TRUE,
             gof_map = c("nobs", "r.squared", "rmse"))

## linear model across years
### all visits
day1 <- lm(num_visits ~ postweek*race,
           data = daily_2017)

day2 <- lm(num_visits ~ postweek*race,
                data = daily_2016)

day3 <- lm(num_visits ~postweek*race,
                data = daily_2018)

modelsummary(list("2017" = day1,
                  "2016" = day2,
                  "2018" = day3),
             stars = TRUE,
             gof_map = c("nobs", "r.squared", "rmse"))

### relevant diagnoses
day1_diag <- lm(num_visits ~ postweek*race,
            data = daily_2017_diag)

day2_diag <- lm(num_visits ~ postweek*race,
            data = daily_2016_diag)

day3_diag <- lm(num_visits ~postweek*race,
            data = daily_2018_diag)

modelsummary(list("2017" = day1_diag,
                  "2016" = day2_diag,
                  "2018" = day3_diag),
             stars = TRUE,
             gof_map = c("nobs", "r.squared", "rmse"))

## count model across years
### all visits
day1_nb <- glm.nb(num_visits ~ postweek*race,
           data = daily_2017)

day2_nb <- glm.nb(num_visits ~ postweek*race,
           data = daily_2016)

day3_nb <- glm.nb(num_visits ~postweek*race,
           data = daily_2018)

modelsummary(list("2017" = day1_nb,
                  "2016" = day2_nb,
                  "2018" = day3_nb),
             stars = TRUE,
             gof_map = c("nobs", "rmse"))

### relevant diagnoses
day1_diag_nb <- glm.nb(num_visits ~ postweek*race,
                   data = daily_2017_diag)

day2_diag_nb <- glm.nb(num_visits ~ postweek*race,
                   data = daily_2016_diag)

day3_diag_nb <- glm.nb(num_visits ~postweek*race,
                   data = daily_2018_diag)

modelsummary(list("2017" = day1_diag_nb,
                  "2016" = day2_diag_nb,
                  "2018" = day3_diag_nb),
             stars = TRUE,
             gof_map = c("nobs", "rmse"))

## predictions for negative binomial
preddata <- crossing(postweek = c("pre", "post2weeks", "postlater"), race = c("Black", "White"))
preddata <- preddata %>% 
  mutate(postweek = factor(postweek, levels = c("pre", "post2weeks", "postlater")),
         race = factor(race))
preddata$phat <- predict(day1_diag_nb, preddata, 
                         type = "response")
preddata

ggplot(preddata, aes(x = postweek, y = phat, color = race, group = race)) + 
  geom_point() +
  geom_line() +
  scale_y_continuous(limits = c(0,40)) +
  scale_color_manual(values = c("#00bfc4", "#f8766d")) +
  labs(y = "Predicted Number of Visits\n for Select Diagnoses",
       x = "Period Relative to 8/12/2017") +
  annotate("text", x = 3.25, y = c(26.6, 8.8), 
           label = c("White", "Black"), color = c("#f8766d", "#00bfc4")) +
  theme_minimal() +
  theme(legend.position = "none")


# 2. Individual-level analysis ----
## mups ----
by_year_race_mups <- allvisits %>% 
  # keep only patients categorized as Black or White
  filter(race_clean %in% c("Black", "White"),
         sex != "Unknown") %>% 
  # reorder race, create number of days since 8/12, create weekend flag
  mutate(race = factor(race_clean, levels = c("Black", "White")),
         days = case_when(
           adm_year == 2016 ~ adm_dtm - ymd(20160812),
           adm_year == 2017 ~ adm_dtm - ymd(20170812),
           adm_year == 2018 ~ adm_dtm - ymd(20180812)
         ),
         pre_post = ifelse(days >= 0, 
                           "post", "pre"),
         pre_post = factor(pre_post, levels = c("pre", "post")),
         absdays = abs(days),
         weekend = ifelse(wday(adm_dtm, week_start = 1) >= 6,
                          "weekend", "weekday")
  ) %>% 
  # keep only cases within 2 weeks of 8/12
  filter(absdays < 15) %>% 
  # nest by year, race to run models separately on each subset
  group_by(adm_year, race) %>% 
  nest()

# Define model
model1 <- function(df) {
  lm(mups1 ~ age_at_event + sex + weekend + pre_post*absdays, data = df)
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
  filter(term %in% c("pre_postpost", "absdays", "pre_postpost:absdays")) %>% 
  kbl() %>% 
  kable_styling() %>% 
  row_spec(c(1:3,7:9,13:15), background = "lightgray")

## alcohol ----
by_year_race_alc <- allvisits %>% 
  # keep only patients categorized as Black or White
  filter(race_clean %in% c("Black", "White"),
         sex != "Unknown") %>% 
  # reorder race, create number of days since 8/12, create weekend flag
  mutate(race = factor(race_clean, levels = c("Black", "White")),
         days = case_when(
           adm_year == 2016 ~ adm_dtm - ymd(20160812),
           adm_year == 2017 ~ adm_dtm - ymd(20170812),
           adm_year == 2018 ~ adm_dtm - ymd(20180812)
         ),
         pre_post = ifelse(days >= 0, 
                           "post", "pre"),
         pre_post = factor(pre_post, levels = c("pre", "post")),
         absdays = abs(days),
         weekend = ifelse(wday(adm_dtm, week_start = 1) >= 6,
                          "weekend", "weekday")
  ) %>% 
  # keep only cases within 2 weeks of 8/12
  filter(absdays < 15) %>% 
  # nest by year, race to run models separately on each subset
  group_by(adm_year, race) %>% 
  nest()

# Define model
model2 <- function(df) {
  lm(alcohol ~ age_at_event + sex + weekend + pre_post*absdays, data = df)
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
  filter(term %in% c("pre_postpost", "absdays", "pre_postpost:absdays")) %>% 
  kbl() %>% 
  kable_styling() %>% 
  row_spec(c(1:3,7:9,13:15),background = "lightgray")

## anxiety-depression ----
by_year_race_anx <- allvisits %>% 
  # keep only patients categorized as Black or White
  filter(race_clean %in% c("Black", "White"),
         sex != "Unknown") %>% 
  # reorder race, create number of days since 8/12, create weekend flag
  mutate(race = factor(race_clean, levels = c("Black", "White")),
         days = case_when(
           adm_year == 2016 ~ adm_dtm - ymd(20160812),
           adm_year == 2017 ~ adm_dtm - ymd(20170812),
           adm_year == 2018 ~ adm_dtm - ymd(20180812)
         ),
         pre_post = ifelse(days >= 0, 
                           "post", "pre"),
         pre_post = factor(pre_post, levels = c("pre", "post")),
         absdays = abs(days),
         weekend = ifelse(wday(adm_dtm, week_start = 1) >= 6,
                          "weekend", "weekday")
  ) %>% 
  # keep only cases within 2 weeks of 8/12
  filter(absdays < 15) %>% 
  # nest by year, race to run models separately on each subset
  group_by(adm_year, race) %>% 
  nest()

# Define model
model3 <- function(df) {
  lm(anx_dep ~ age_at_event + sex + weekend +pre_post*absdays, data = df)
}

# Run model
by_year_race_anx <- by_year_race_anx %>% 
  mutate(model = map(data, model3))

# View results
by_year_race_anx %>%
  arrange(adm_year, race) %>% 
  mutate(tidied = map(model, tidy)) %>% 
  dplyr::select(adm_year, race, tidied) %>% 
  unnest(tidied) %>% 
  filter(term %in% c("pre_postpost", "absdays", "pre_postpost:absdays")) %>% 
  kbl() %>% 
  kable_styling() %>% 
  row_spec(c(1:3,7:9,13:15),background = "lightgray")

## switch to logit ----
### mups
# Define model
model1 <- function(df) {
  glm(mups1 ~ age_at_event + sex + weekend + pre_post*absdays, 
      family = "binomial", data = df)
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
  filter(term %in% c("pre_postpost", "absdays", "pre_postpost:absdays")) %>% 
  kbl() %>% 
  kable_styling() %>% 
  row_spec(c(1:3,7:9,13:15), background = "lightgray")

### alcohol
# Define model
model2 <- function(df) {
  glm(alcohol ~ age_at_event + sex + weekend +pre_post*absdays, 
      family = "binomial", data = df)
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
  filter(term %in% c("pre_postpost", "absdays", "pre_postpost:absdays")) %>% 
  kbl() %>% 
  kable_styling() %>% 
  row_spec(c(1:3,7:9,13:15),background = "lightgray")

### anxiety
# Define model
model3 <- function(df) {
  glm(anx_dep ~ age_at_event + sex + weekend +pre_post*absdays, 
      family = "binomial", data = df)
}

# Run model
by_year_race_anx <- by_year_race_anx %>% 
  mutate(model = map(data, model3))

# View results
by_year_race_anx %>%
  arrange(adm_year, race) %>% 
  mutate(tidied = map(model, tidy)) %>% 
  dplyr::select(adm_year, race, tidied) %>% 
  unnest(tidied) %>% 
  filter(term %in% c("pre_postpost", "absdays", "pre_postpost:absdays")) %>% 
  kbl() %>% 
  kable_styling() %>% 
  row_spec(c(1:3,7:9,13:15),background = "lightgray")

## Visualize ----
# I think the prior version is just plotting the unconditional mean (0/1)
# check this...

# 2017/black data frame
df <- data.frame(by_year_race_mups$data[4])
mean_day_mups <- df %>% 
  group_by(pre_post, days) %>% 
  summarize(p_mups = mean(mups1))

ggplot(mean_day_mups, aes(x = days, y = p_mups)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = .1, aes(group = pre_post)) +
  scale_y_continuous(limits = c(0,1)) +
  geom_vline(xintercept = 0, linetype = "dashed")
  

mean_day_alc <- df %>% 
  group_by(pre_post, days) %>% 
  summarize(p_alc = mean(alcohol))

ggplot(mean_day_alc, aes(x = days, y = p_alc)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = .1, aes(group = pre_post)) +
  scale_y_continuous(limits = c(0,0.2)) +
  geom_vline(xintercept = 0, linetype = "dashed")

# Let's use predicted probabilities from the model instead
# e.g., take mean of predicted values (not observed values)
mean_pred_mups <- by_year_race_mups %>% 
  mutate(tidied = map(model, augment)) %>% 
  dplyr::select(adm_year, race, data, tidied) %>% 
  unnest(data, tidied) %>% 
  filter(adm_year == 2017, race == "Black") %>% 
  mutate(pred = 1/(1+exp(-1*.fitted))) %>% 
  group_by(pre_post, days) %>% 
  summarize(p_mups = mean(pred)) 
  
ggplot(mean_pred_mups, aes(x = days, y = p_mups)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = .1, aes(group = pre_post)) +
  scale_y_continuous(limits = c(0,1)) +
  geom_vline(xintercept = 0, linetype = "dashed")

mean_pred_alc <- by_year_race_alc %>% 
  mutate(tidied = map(model, augment)) %>% 
  dplyr::select(adm_year, race, data, tidied) %>% 
  unnest(data, tidied) %>% 
  filter(adm_year == 2017, race == "Black") %>% 
  mutate(pred = 1/(1+exp(-1*.fitted))) %>% 
  group_by(pre_post, days) %>% 
  summarize(p_alcs = mean(pred)) 

ggplot(mean_pred_alc, aes(x = days, y = p_alcs)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = .1, aes(group = pre_post)) +
  scale_y_continuous(limits = c(0,.25)) +
  geom_vline(xintercept = 0, linetype = "dashed")


# compared to white patients
mean_pred <- by_year_race_mups %>% 
  mutate(tidied = map(model, augment)) %>% 
  dplyr::select(adm_year, race, data, tidied) %>% 
  unnest(data, tidied) %>% 
  filter(adm_year == 2017, race == "White") %>% 
  mutate(pred = 1/(1+exp(-1*.fitted))) %>% 
  group_by(pre_post, days) %>% 
  summarize(p_mups = mean(pred)) 

ggplot(mean_pred, aes(x = days, y = p_mups)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = .1, aes(group = pre_post)) +
  scale_y_continuous(limits = c(0,1)) +
  geom_vline(xintercept = 0, linetype = "dashed")

mean_pred_alc <- by_year_race_alc %>% 
  mutate(tidied = map(model, augment)) %>% 
  dplyr::select(adm_year, race, data, tidied) %>% 
  unnest(data, tidied) %>% 
  filter(adm_year == 2017, race == "White") %>% 
  mutate(pred = 1/(1+exp(-1*.fitted))) %>% 
  group_by(pre_post, days) %>% 
  summarize(p_alcs = mean(pred)) 

ggplot(mean_pred_alc, aes(x = days, y = p_alcs)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = .1, aes(group = pre_post)) +
  scale_y_continuous(limits = c(0,.25)) +
  geom_vline(xintercept = 0, linetype = "dashed")


## Replicate local ---
## mups ----
by_location_race_mups <- allvisits %>% 
  # keep only patients categorized as Black or White, only 2017
  filter(race_clean %in% c("Black", "White"),
         sex != "Unknown",
         adm_year == 2017) %>% 
  # reorder race, create number of days since 8/12, create weekend flag
  mutate(race = factor(race_clean, levels = c("Black", "White")),
         days = adm_dtm - ymd(20170812),
         pre_post = ifelse(days >= 0, 
                           "post", "pre"),
         pre_post = factor(pre_post, levels = c("pre", "post")),
         absdays = abs(days),
         weekend = ifelse(wday(adm_dtm, week_start = 1) >= 6,
                          "weekend", "weekday")
  ) %>% 
  # keep only cases within 2 weeks of 8/12
  filter(absdays < 15) %>% 
  # nest by year, race to run models separately on each subset
  group_by(race, location) %>% 
  nest()

# Define model
model1 <- function(df) {
  glm(mups1 ~ age_at_event + sex + weekend + pre_post*absdays, 
      family = "binomial", data = df)
}

# Run model
by_location_race_mups <- by_location_race_mups %>% 
  mutate(model = map(data, model1))

# View results
by_location_race_mups %>%
  arrange(race, location) %>% 
  mutate(tidied = map(model, tidy)) %>% 
  dplyr::select(location, race, tidied) %>% 
  unnest(tidied) %>% 
  filter(term %in% c("pre_postpost", "absdays", "pre_postpost:absdays")) %>% 
  kbl() %>% 
  kable_styling() %>% 
  row_spec(c(1:3,7:9), background = "lightgray")

## recode local/zip
# from https://www.huduser.gov/portal/datasets/usps_crosswalk.html
zip_hud <- read_excel("data/ZIP_COUNTY_092017.xlsx")
cvlalb_zip_hud <- zip_hud %>% 
  filter(county %in% c("51003", "51540")) %>% 
  group_by(zip) %>% 
  summarize(res_ratio = sum(res_ratio),
            bus_ratio = sum(bus_ratio),
            oth_ratio = sum(oth_ratio),
            tot_ratio = sum(tot_ratio)) %>% 
  filter(tot_ratio > 0.5)

cvl_zip_hud <- zip_hud %>% 
  filter(county %in% c("51540")) %>% 
  group_by(zip) %>% 
  summarize(res_ratio = sum(res_ratio),
            bus_ratio = sum(bus_ratio),
            oth_ratio = sum(oth_ratio),
            tot_ratio = sum(tot_ratio)) %>% 
  filter(tot_ratio > 0.5)

allvisits <- allvisits %>% 
  mutate(zip5 = str_sub(zip_code, 1,5)) %>% 
  mutate(local = ifelse(zip5 %in% cvlalb_zip_hud$zip, 1, 0))

## mups ----
by_location_race_mups <- allvisits %>% 
  # keep only patients categorized as Black or White, only 2017
  filter(race_clean %in% c("Black", "White"),
         sex != "Unknown",
         adm_year == 2017) %>% 
  # reorder race, create number of days since 8/12, create weekend flag
  mutate(race = factor(race_clean, levels = c("Black", "White")),
         days = adm_dtm - ymd(20170812),
         pre_post = ifelse(days >= 0, 
                           "post", "pre"),
         pre_post = factor(pre_post, levels = c("pre", "post")),
         absdays = abs(days),
         weekend = ifelse(wday(adm_dtm, week_start = 1) >= 6,
                          "weekend", "weekday")
  ) %>% 
  # keep only cases within 2 weeks of 8/12
  filter(absdays < 15) %>% 
  # nest by year, race to run models separately on each subset
  group_by(race, local) %>% 
  nest()

# Define model
model1 <- function(df) {
  glm(mups1 ~ age_at_event + sex + weekend + pre_post*absdays, 
      family = "binomial", data = df)
}

# Run model
by_location_race_mups <- by_location_race_mups %>% 
  mutate(model = map(data, model1))

# View results
by_location_race_mups %>%
  arrange(race, local) %>% 
  mutate(tidied = map(model, tidy)) %>% 
  dplyr::select(local, race, tidied) %>% 
  unnest(tidied) %>% 
  filter(term %in% c("pre_postpost", "absdays", "pre_postpost:absdays")) %>% 
  kbl() %>% 
  kable_styling() %>% 
  row_spec(c(1:3,7:9), background = "lightgray")


# 3. Strategy ----
## Primary test, time discontinuity by race (RDD) ----
## key parameters: raceXprepost, raceXprepostXabsdays
df_2017 <- allvisits %>% 
  # keep only patients categorized as Black or White, only 2017
  filter(race_clean %in% c("Black", "White"),
         sex %in% c("Female", "Male"),
         adm_year == 2017) %>% 
  # reorder race, create number of days since 8/12, create weekend flag
  mutate(black = ifelse(race_clean == "Black", 1, 0),
         race = race_clean,
         days = adm_dtm - ymd(20170812),
         pre_post = ifelse(days >= 0, "post", "pre"),
         pre_post = factor(pre_post, levels = c("pre", "post")),
         post = ifelse(days >= 0, 
                           1,0),
         absdays = abs(days),
         female = ifelse(sex == "Female", 1, 0),
         weekend = ifelse(wday(adm_dtm, week_start = 1) >= 6,
                          1,0)
  ) %>% 
  # keep only cases within 2 weeks of 8/12
  filter(absdays < 15) 

### mups ----
# Model
model_mups <- 
  glm(mups1 ~ age_at_event + sex + weekend + 
        race*pre_post*absdays + 0*days, 
      family = "binomial", data = df_2017)

summary(model_mups)

# Visualize
mean_pred_mups <- augment(model_mups)

mean_pred_mups <- mean_pred_mups %>% 
  mutate(pred = 1/(1+exp(-1*.fitted))) %>% 
  group_by(race, pre_post, days) %>% 
  summarize(p_mups = mean(pred)) 

ggplot(mean_pred_mups, aes(x = days, y = p_mups, 
                           group = pre_post)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = .1, aes(group = pre_post)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_y_continuous(limits = c(0,1)) + 
  facet_wrap(~race)

### alc ----
# Model
model_alc <- 
  glm(alcohol ~ age_at_event + sex + weekend + 
        race*pre_post*absdays + 0*days, 
      family = "binomial", data = df_2017)

model_alc %>% tidy()

# Visualize
mean_pred_alc <- augment(model_alc)

mean_pred_alc <- mean_pred_alc %>% 
  mutate(pred = 1/(1+exp(-1*.fitted))) %>% 
  group_by(race, pre_post, days) %>% 
  summarize(p_alc = mean(pred)) 

ggplot(mean_pred_alc, aes(x = days, y = p_alc, 
                           group = pre_post)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = .1, aes(group = pre_post)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_y_continuous(limits = c(0,.25)) + 
  facet_wrap(~race)

### anx ----
# Model
model_anx <- 
  glm(anx_dep ~ age_at_event + sex + weekend + 
        race*pre_post*absdays + 0*days, 
      family = "binomial", data = df_2017)

model_anx %>% tidy()

# Visualize
mean_pred_anx <- augment(model_anx)

mean_pred_anx <- mean_pred_anx %>% 
  mutate(pred = 1/(1+exp(-1*.fitted))) %>% 
  group_by(race, pre_post, days) %>% 
  summarize(p_anx = mean(pred)) 

ggplot(mean_pred_anx, aes(x = days, y = p_anx, 
                          group = pre_post)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = .1, aes(group = pre_post)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_y_continuous(limits = c(0,.25)) + 
  facet_wrap(~race)


## Secondary test, effect of proximity ----
## (as proxy of intensity of intervention)
## among Black patients
## key parameters: locationXprepost, locationXprepostXabsdays
## using current location variable (7 zips)
df_2017_black <- allvisits %>% 
  # keep only patients categorized as Black or White, only 2017
  filter(race_clean %in% c("Black"),
         sex %in% c("Female", "Male"),
         adm_year == 2017) %>% 
  # reorder race, create number of days since 8/12, create weekend flag
  mutate(days = adm_dtm - ymd(20170812),
         pre_post = ifelse(days >= 0, 
                           "post", "pre"),
         pre_post = factor(pre_post, levels = c("pre", "post")),
         post = ifelse(days >= 0, 
                       1,0),
         absdays = abs(days),
         female = ifelse(sex == "Female", 1, 0),
         weekend = ifelse(wday(adm_dtm, week_start = 1) >= 6,
                          1,0)
  ) %>% 
  # keep only cases within 2 weeks of 8/12
  filter(absdays < 15) 

### mups ----
# Model
model_mups <- 
  glm(mups1 ~ age_at_event + sex + weekend + 
        location*pre_post*absdays + 0*days, 
      family = "binomial", data = df_2017_black)

model_mups %>% tidy()

# Visualize
mean_pred_mups <- augment(model_mups)

mean_pred_mups <- mean_pred_mups %>% 
  mutate(pred = 1/(1+exp(-1*.fitted))) %>% 
  group_by(location, pre_post, days) %>% 
  summarize(p_mups = mean(pred)) 

ggplot(mean_pred_mups, aes(x = days, y = p_mups, 
                           group = pre_post)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = .1, aes(group = pre_post)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_y_continuous(limits = c(0,1)) + 
  facet_wrap(~location)

### alc ----
# Model
model_alc <- 
  glm(alcohol ~ age_at_event + sex + weekend + 
        location*pre_post*absdays + 0*days, 
      family = "binomial", data = df_2017_black)

model_alc %>% tidy()

# Visualize
mean_pred_alc <- augment(model_alc)

mean_pred_alc <- mean_pred_alc %>% 
  mutate(pred = 1/(1+exp(-1*.fitted))) %>% 
  group_by(location, pre_post, days) %>% 
  summarize(p_alc = mean(pred)) 

ggplot(mean_pred_alc, aes(x = days, y = p_alc, 
                           group = pre_post)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = .1, aes(group = pre_post)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_y_continuous(limits = c(0,.25)) + 
  facet_wrap(~location)

### anx ----
# Model
model_anx <- 
  glm(anx_dep ~ age_at_event + sex + weekend + 
        location*pre_post*absdays + 0*days, 
      family = "binomial", data = df_2017_black)

model_anx %>% tidy()

# Visualize
mean_pred_anx <- augment(model_anx)

mean_pred_anx <- mean_pred_anx %>% 
  mutate(pred = 1/(1+exp(-1*.fitted))) %>% 
  group_by(location, pre_post, days) %>% 
  summarize(p_anx = mean(pred)) 

ggplot(mean_pred_anx, aes(x = days, y = p_anx, 
                          group = pre_post)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = .1, aes(group = pre_post)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_y_continuous(limits = c(0,0.25)) + 
  facet_wrap(~location)


## Secondary test, compared to other years (DiD) ----
## effect of intervention compared years with no intervention
## among black patients
## key parameters: 2017Xprepost, 2017XprepostXabsdays
df_black <- allvisits %>% 
  # keep only patients categorized as Black or White, only 2017
  filter(race_clean %in% c("Black"),
         sex %in% c("Female", "Male")) %>% 
  # reorder race, create number of days since 8/12, create weekend flag
  mutate(race = factor(race_clean, levels = c("Black", "White")),
         days = case_when(
           adm_year == 2016 ~ adm_dtm - ymd(20160812),
           adm_year == 2017 ~ adm_dtm - ymd(20170812),
           adm_year == 2018 ~ adm_dtm - ymd(20180812)
         ),
         pre_post = ifelse(days >= 0, 
                           "post", "pre"),
         pre_post = factor(pre_post, levels = c("pre", "post")),
         post = ifelse(days >= 0, 1, 0),
         female = ifelse(sex == "Female", 1, 0),
         absdays = abs(days),
         weekend = ifelse(wday(adm_dtm, week_start = 1) >= 6,
                          "weekend", "weekday"),
         year2017 = ifelse(adm_year == 2017, "2017", "2016/2018"),
  ) %>% 
  # keep only cases within 2 weeks of 8/12
  filter(absdays < 15)

### mups ----
# Model
model_mups <- 
  glm(mups1 ~ age_at_event + sex + weekend + 
        year2017*pre_post*absdays + 0*days, 
      family = "binomial", data = df_black)

model_mups %>% tidy()

# Visualize
mean_pred_mups <- augment(model_mups)

mean_pred_mups <- mean_pred_mups %>% 
  mutate(pred = 1/(1+exp(-1*.fitted))) %>% 
  group_by(year2017, pre_post, days) %>% 
  summarize(p_mups = mean(pred)) 

ggplot(mean_pred_mups, aes(x = days, y = p_mups, 
                           group = pre_post)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = .1, aes(group = pre_post)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_y_continuous(limits = c(0,1)) + 
  facet_wrap(~year2017)

### alc ----
# Model
model_alc <- 
  glm(alcohol ~ age_at_event + sex + weekend + 
        year2017*pre_post*absdays + 0*days, 
      family = "binomial", data = df_black)

model_alc %>% tidy()

# Visualize
mean_pred_alc <- augment(model_alc)

mean_pred_alc <- mean_pred_alc %>% 
  mutate(pred = 1/(1+exp(-1*.fitted))) %>% 
  group_by(year2017, pre_post, days) %>% 
  summarize(p_alc = mean(pred)) 

ggplot(mean_pred_alc, aes(x = days, y = p_alc, 
                           group = pre_post)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = .1, aes(group = pre_post)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_y_continuous(limits = c(0,.25)) + 
  facet_wrap(~year2017)

### anx ----
# Model
model_anx <- 
  glm(anx_dep ~ age_at_event + sex + weekend + 
        year2017*pre_post*absdays + 0*days, 
      family = "binomial", data = df_black)

model_anx %>% tidy()

# Visualize
mean_pred_anx <- augment(model_anx)

mean_pred_anx <- mean_pred_anx %>% 
  mutate(pred = 1/(1+exp(-1*.fitted))) %>% 
  group_by(year2017, pre_post, days) %>% 
  summarize(p_anx = mean(pred)) 

ggplot(mean_pred_anx, aes(x = days, y = p_anx, 
                          group = pre_post)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = .1, aes(group = pre_post)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_y_continuous(limits = c(0,.25)) + 
  facet_wrap(~year2017)


## Robustness of time span/window ----
