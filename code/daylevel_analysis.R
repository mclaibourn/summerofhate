# Health Impact of The Summer of Hate 
# January 25, 2024
# Robustness (and a few other things)

# Set up ----
## Libraries ----
library(MASS)
library(tidyverse)
library(janitor)
library(broom)
library(kableExtra)
library(modelsummary)

## Data ----
# read
allvisits <- read_csv("data/All ED VISITS RECODED.csv") %>% 
  clean_names()

# update diagnostic codes for anxiety/depression
allvisits <- allvisits %>% 
  mutate(anx = ifelse(
    icd_10_code %in% c("R07.89", "R10.9", "R11.2", "R45.851", "S69.81XA", "Z41.8"),
    0, anx_dep))

## Prep ----
daily_2017 <- allvisits %>% 
  mutate(diag = ifelse(anx == 1 | mups1 == 1,
                       1, 0)) %>% 
  filter(race_clean %in% c("Black", "White")) %>% 
  group_by(adm_year, adm_dtm, race_clean) %>% 
  summarize(num_visits = n(),
            num_diag = sum(diag),
            num_mups = sum(mups1),
            num_anx = sum(anx)) %>% 
  filter(between(adm_dtm, ymd(20170511), ymd(20171111))) %>% # original 5/14 to 11/11
  mutate(race = factor(race_clean, levels = c("Black", "White"), labels = c("Black Patients", "White Patients")),
         postweek = case_when(
           adm_dtm < ymd(20170811) ~ "pre",
           adm_dtm > ymd(20170825) ~ "postlater",
           TRUE ~ "post2weeks"
         ),
         postweek = as.factor(postweek),
         post = ifelse(postweek == "pre", "pre", "post"),
         post = as.factor(post),
         weekend = ifelse(wday(adm_dtm, week_start = 1) >= 6,
                          "weekend", "weekday"),
         day_of_week = wday(adm_dtm, label = TRUE))

daily_2016_2018 <- allvisits %>% 
  mutate(diag = ifelse(anx == 1 | mups1 == 1,
                       1, 0)) %>% 
  filter(race_clean %in% c("Black", "White")) %>% 
  group_by(adm_year, adm_dtm, race_clean) %>% 
  summarize(num_visits = n(),
            num_diag = sum(diag),
            num_mups = sum(mups1),
            num_anx = sum(anx)) %>% 
  filter(between(adm_dtm, ymd(20160511), ymd(20161111)) |
           between(adm_dtm, ymd(20180511), ymd(20181111))) %>% 
  mutate(days = case_when(
    adm_year == 2016 ~ adm_dtm - ymd(20160811),
    adm_year == 2018 ~ adm_dtm - ymd(20180811)
  ),
  race = factor(race_clean, levels = c("Black", "White"), labels = c("Black Patients", "White Patients")),
  postweek = case_when(
    days < 0  ~ "pre",
    days > 14 ~ "postlater",
    TRUE ~ "post2weeks"
  ),
  postweek = as.factor(postweek),
  post = ifelse(postweek == "pre", "pre", "post"),
  post = as.factor(post),
  weekend = ifelse(wday(adm_dtm, week_start = 1) >= 6,
                   "weekend", "weekday"),
  day_of_week = wday(adm_dtm, label = TRUE))

daily_2017_2016_2018 <- bind_rows(daily_2017, daily_2016_2018) %>% 
  mutate(day = day(adm_dtm),
         month = month(adm_dtm),
         monthday = paste0(month, "-", day),
         monthday = as.Date(monthday, "%m-%d"))


## Visual 2017 ----
ann_text <- data.frame(adm_dtm = c(as.Date("2017-08-10"), as.Date("2017-08-12")),
                       num_diag = c(20, 0),
                       num_mups = c(18, 0),
                       num_anx = c(2,2),
                       race = factor("Black Patients"))

### both diagnoses ----
daily_2017 %>% 
  ggplot(aes(x = adm_dtm, y = num_diag, group = 1)) +
  geom_rect(xmin = as.Date("2017-08-11"), xmax = as.Date("2017-08-25"),
            ymin = -Inf, ymax = Inf, fill = "grey90") +
  # geom_rect(xmin = as.Date("2017-08-18"), xmax = as.Date("2017-08-25"),
  #           ymin = -Inf, ymax = Inf, fill = "grey90") +
  geom_line(color = "grey50") +
  #  geom_smooth(span = 0.1, se = FALSE, color = "black") +
  geom_vline(xintercept = as.Date("2017-08-11"), linetype = "dashed") +
  geom_text(data = ann_text, label = c("August 11", "2 weeks post"),
            size = 2, hjust = c(1, 0)) +
  facet_wrap(~race, scales = "free_y", ncol = 1) +
  labs(x = "Date", y = "Number of Visits") +
  theme_minimal()

ggsave("daylevel_trend.png")

### just mups ----
daily_2017 %>% 
  ggplot(aes(x = adm_dtm, y = num_mups, group = 1)) +
  geom_rect(xmin = as.Date("2017-08-11"), xmax = as.Date("2017-08-18"),
            ymin = -Inf, ymax = Inf, fill = "grey80") +
  geom_rect(xmin = as.Date("2017-08-18"), xmax = as.Date("2017-08-25"),
            ymin = -Inf, ymax = Inf, fill = "grey90") +
  geom_line(color = "black") +
#  geom_smooth(span = 0.1, se = FALSE, color = "black") +
  geom_vline(xintercept = as.Date("2017-08-11"), linetype = "dashed") +
  geom_text(data = ann_text, label = c("August 11", "2 weeks post"),
            size = 2, hjust = c(1, 0)) +
  facet_wrap(~race, scales = "free_y", ncol = 1) +
  labs(title = "Visits with Diagnoses of MUPS",
       x = "Date", y = "Number of Visits") +
  theme_minimal()

### just anx ----
daily_2017 %>% 
  ggplot(aes(x = adm_dtm, y = num_anx, group = 1)) +
  geom_rect(xmin = as.Date("2017-08-11"), xmax = as.Date("2017-08-18"),
            ymin = -Inf, ymax = Inf, fill = "grey80") +
  geom_rect(xmin = as.Date("2017-08-18"), xmax = as.Date("2017-08-25"),
            ymin = -Inf, ymax = Inf, fill = "grey90") +
  geom_line(color = "black") +
  #  geom_smooth(span = 0.1, se = FALSE, color = "black") +
  geom_vline(xintercept = as.Date("2017-08-11"), linetype = "dashed") +
  geom_text(data = ann_text, label = c("August 11", "2 weeks post"),
            size = 2, hjust = c(1, 0)) +
  facet_wrap(~race, scales = "free_y", ncol = 1) +
  labs(title = "Visits with Diagnoses of MUPS",
       x = "Date", y = "Number of Visits") +
  theme_minimal()

## Visual 2017 compared to 2016/2018 ----

### both diagnoses ----
daily_2017_2016_2018 %>% 
  ggplot(aes(x = monthday, y = num_diag, color = as.factor(adm_year), group = as.factor(adm_year))) +
  # geom_rect(xmin = as.Date("2024-08-11"), xmax = as.Date("2024-08-18"),
  #           ymin = -Inf, ymax = Inf, fill = "grey80") +
  # geom_rect(xmin = as.Date("2024-08-18"), xmax = as.Date("2024-08-25"),
  #           ymin = -Inf, ymax = Inf, fill = "grey90") +
  geom_line() +
  scale_color_manual(values = c("grey", "black", "grey")) +
  # geom_smooth(span = 0.1, se = FALSE, color = "black") +
  geom_vline(xintercept = as.Date("2024-08-11"), linetype = "dashed") +
  # geom_text(data = ann_text, label = c("August 11", "2 weeks post"),
  #           size = 2, hjust = c(1, 0)) +
  facet_wrap(~race, scales = "free_y", ncol = 1) +
  labs(title = "Visits with Diagnoses of MUPS, Anxiety",
       x = "Date", y = "Number of Visits") +
  theme_minimal()

### mups ----
daily_2017_2016_2018 %>% 
  ggplot(aes(x = monthday, y = num_mups, color = as.factor(adm_year), group = as.factor(adm_year))) +
  # geom_rect(xmin = as.Date("2024-08-11"), xmax = as.Date("2024-08-18"),
  #           ymin = -Inf, ymax = Inf, fill = "grey80") +
  # geom_rect(xmin = as.Date("2024-08-18"), xmax = as.Date("2024-08-25"),
  #           ymin = -Inf, ymax = Inf, fill = "grey90") +
  geom_line() +
  scale_color_manual(values = c("grey", "black", "grey")) +
  # geom_smooth(span = 0.1, se = FALSE, color = "black") +
  geom_vline(xintercept = as.Date("2024-08-11"), linetype = "dashed") +
  # geom_text(data = ann_text, label = c("August 11", "2 weeks post"),
  #           size = 2, hjust = c(1, 0)) +
  facet_wrap(~race, scales = "free_y", ncol = 1) +
  labs(title = "Visits with Diagnoses of MUPS, Anxiety",
       x = "Date", y = "Number of Visits") +
  theme_minimal()

## Model ----
### count model for mups, anxiety, 2017 ----
visits_diag <- glm.nb(num_diag ~ postweek*race + weekend,
                      data = daily_2017)

### count model for mups, anxiety, 2016/2018 ----
visits_diag_1618 <- glm.nb(num_diag ~ postweek*race + weekend,
                           data = daily_2016_2018)

### Tables for article ----
modelsummary(list("2017" = visits_diag, "2016/2018" = visits_diag_1618), 
             stars = TRUE,
             coef_map = c("(Intercept)" = "Intercept",
                          "postweekpost2weeks" = "2 weeks post",
                          "postweekpostlater" = "More than 2 weeks post",
                          "raceWhite Patients" = "White Patients",
                          "postweekpost2weeks:raceWhite Patients" = "White Patients x 2 weeks post",
                          "postweekpostlater:raceWhite Patients" = "White Patients x More than 2 weeks post",
                          "weekendweekend" = "Weekend"),
             gof_map = c("nobs", "logLik"),
             output = "daylevel_table.docx"
)

## Separately by race ----
### count model for mups, anxiety, 2017 ----
visits_diag_black <- glm.nb(num_diag ~ postweek + weekend,
                      data = daily_2017 %>% filter(race_clean == "Black"))
summary(visits_diag_black)

visits_diag_white <- glm.nb(num_diag ~ postweek + weekend,
                            data = daily_2017 %>% filter(race_clean == "White"))
summary(visits_diag_white)

