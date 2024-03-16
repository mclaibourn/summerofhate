# Health Impact of The Summer of Hate 
# August 2, 2023
# updated January 2024
# Individual level

# Set up ----
## Libraries ----
library(tidyverse)
library(janitor)
# library(MASS)
library(broom)
library(kableExtra)
library(modelsummary)
library(patchwork)

## Data ----
# read
allvisits <- read_csv("data/All ED VISITS RECODED.csv") %>% 
  clean_names()

# update diagnostic codes for anxiety/depression
allvisits <- allvisits %>% 
  mutate(anx = ifelse(
    icd_10_code %in% c("R07.89", "R10.9", "R11.2", "R45.851", "S69.81XA", "Z41.8"),
    0, anx_dep))

# check
allvisits %>% count(anx, anx_dep)

# list codes
allvisits %>% 
  filter(anx == 1) %>% 
  count(icd_10_code) 
allvisits %>% 
  filter(mups1 == 1) %>% 
  count(icd_10_code)


# Individual-level/causal analysis ----
## 2017 model, time discontinuity by race (RDD) ----
## key parameters: pre_post, pre_postXrace
df_2017_aug10 <- allvisits %>% 
  # keep only patients categorized as Black or White, only 2017
  filter(race_clean %in% c("Black", "White"),
         sex %in% c("Female", "Male"),
         adm_year == 2017) %>% 
  # reorder race, create number of days since 8/12, create weekend flag
  mutate(black = ifelse(race_clean == "Black", 1, 0),
         race = factor(race_clean, levels = c("Black", "White"), labels = c("Black Patients", "White Patients")),
         days = adm_dtm - ymd(20170810),
         pre_post = ifelse(days >= 0, "post", "pre"),
         pre_post = factor(pre_post, levels = c("pre", "post")),
         post = ifelse(days >= 0, 1,0),
         absdays = abs(days),
         female = ifelse(sex == "Female", 1, 0),
         weekend = ifelse(wday(adm_dtm, week_start = 1) >= 6,
                          1,0),
         day_of_week = wday(adm_dtm, label = TRUE)
  ) %>% 
  # keep only cases within 2 weeks of 8/(11/12)
  filter(absdays < 15) 

### mups ----
#### Model ----
model_mups <- 
  glm(mups1 ~ age_at_event + sex + weekend + 
        race*pre_post*absdays, 
      family = "binomial", data = df_2017_aug10)

model_mups %>% tidy()

model_mups2 <- 
  glm(mups1 ~ age_at_event + sex + day_of_week + 
        race*pre_post*absdays, 
      family = "binomial", data = df_2017_aug10)

model_mups2 %>% tidy()

modelsummary(list("Weekend Control" = model_mups, 
                  "Day of Week Control" = model_mups2),
             stars = TRUE,
             coef_map = c("(Intercept)" = "Intercept",
                          "pre_postpost" = "Post 8/11",
                          "absdays" = "Days post 8/11",
                          "pre_postpost:absdays" = "Post 8/11 x Days post 8/11",
                          "raceWhite Patients" = "White Patients",
                          "raceWhite Patients:pre_postpost" = "White Patients x Post 8/11",
                          "raceWhite Patients:absdsays" = "White Patients x Days post 8/11",
                          "raceWhite Patients:pre_postpost:absdays" = "White Patients x Post 8/11 x Days Post 8/11",
                          "age_at_event" = "Patient Age",
                          "sexMale" = "Male Patients",
                          "weekend" = "Weekend"),
             gof_map = c("nobs", "logLik"))

#### Visualize ----
mean_pred_mups <- augment(model_mups2, type.predict = "response", se_fit = TRUE, interval = "confidence")

mean_pred_mups <- mean_pred_mups %>% 
  mutate(days = ifelse(pre_post == "pre", -1*as.numeric(absdays), as.numeric(absdays))) %>% 
  group_by(race, pre_post, days) %>% 
  summarize(p_mups = mean(.fitted),
            sd_mups = mean(.resid)) # this may not be right

ggplot(mean_pred_mups, aes(x = days, y = p_mups, 
                           group = pre_post)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 1/10, color = "grey", aes(group = pre_post)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  # geom_errorbar(aes(ymin = p_mups - sd_mups, ymax = p_mups + sd_mups)) +
  scale_y_continuous(limits = c(0,1)) + 
  labs(x = "Days from Event", y = "Probability of MUPS Diagnosis") +
  facet_wrap(~race) +
  theme_minimal()

ggsave("mups_predprob_2017.png")

### anx ----
#### Model ----
model_anx <- 
  glm(anx ~ age_at_event + sex + weekend + 
        race*pre_post*absdays, 
      family = "binomial", data = df_2017_aug10)

model_anx %>% tidy()

model_anx2 <- 
  glm(anx ~ age_at_event + sex + day_of_week + 
        race*pre_post*absdays, 
      family = "binomial", data = df_2017_aug10)

model_anx2 %>% tidy()

modelsummary(list("Weekend Control" = model_anx, 
                  "Day of Week Control" = model_anx2),
             stars = TRUE,
             coef_map = c("(Intercept)" = "Intercept",
                          "pre_postpost" = "Post 8/11",
                          "absdays" = "Days post 8/11",
                          "pre_postpost:absdays" = "Post 8/11 x Days post 8/11",
                          "raceWhite Patients" = "White Patients",
                          "raceWhite Patients:pre_postpost" = "White Patients x Post 8/11",
                          "raceWhite Patients:absdsays" = "White Patients x Days post 8/11",
                          "raceWhite Patients:pre_postpost:absdays" = "White Patients x Post 8/11 x Days Post 8/11",
                          "age_at_event" = "Patient Age",
                          "sexMale" = "Male Patients",
                          "weekend" = "Weekend"),
             gof_map = c("nobs", "logLik"))

#### Visualize ----
mean_pred_anx <- augment(model_anx2, type.predict = "response", se_fit = TRUE)

mean_pred_anx <- mean_pred_anx %>% 
  mutate(days = ifelse(pre_post == "pre", -1*as.numeric(absdays), as.numeric(absdays))) %>% 
  group_by(race, pre_post, days) %>% 
  summarize(p_anx = mean(.fitted)) 

ggplot(mean_pred_anx, aes(x = days, y = p_anx, 
                          group = pre_post)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 1/10, color = "grey", aes(group = pre_post)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_y_continuous(limits = c(0,.25)) + 
  labs(x = "Days from Event", y = "Probability of Anxiety/Depression Diagnosis") +
  facet_wrap(~race) +
  theme_minimal()

ggsave("anx_predprob_2017.png")

### Tables ----
modelsummary(list("MUPS" = model_mups2, "Anxiety/Depression" = model_anx2),
             stars = TRUE,
             coef_map = c("(Intercept)" = "Intercept",
                          "pre_postpost" = "Post",
                          "absdays" = "Days",
                          "pre_postpost:absdays" = "Post x Days",
                          "raceWhite Patients" = "White patients",
                          "raceWhite Patients:pre_postpost" = "White patients x Post",
                          "raceWhite Patients:absdsays" = "White patients x Days",
                          "raceWhite Patients:pre_postpost:absdays" = "White patients x Post x Days"),
             gof_map = c("nobs", "logLik"),
             output = "mups_anx_table.docx")


## Difference-in-Difference model, compared to other years ----
## compared to 2016/2018
# among black patient 
df_black <- allvisits %>% 
  # keep only patients categorized as Black or White, only 2017
  filter(race_clean %in% c("Black"),
         sex %in% c("Female", "Male")) %>% 
  # reorder race, create number of days since 8/12, create weekend flag
  mutate(days = case_when(
    adm_year == 2016 ~ adm_dtm - ymd(20160810),
    adm_year == 2017 ~ adm_dtm - ymd(20170810),
    adm_year == 2018 ~ adm_dtm - ymd(20180810)
  ),
  pre_post = ifelse(days >= 0, 
                    "post", "pre"),
  pre_post = factor(pre_post, levels = c("pre", "post")),
  post = ifelse(days >= 0, 1, 0),
  female = ifelse(sex == "Female", 1, 0),
  absdays = abs(days),
  weekend = ifelse(wday(adm_dtm, week_start = 1) >= 6,
                   "weekend", "weekday"),
  day_of_week = wday(adm_dtm, label = TRUE),
  year2017 = ifelse(adm_year == 2017, "2017", "2016 & 2018"),
  year2017 = factor(year2017, levels = c("2017", "2016 & 2018"))
  ) %>% 
  # keep only cases within 2 weeks of 8/12
  filter(absdays < 15)

# among white patient 
df_white <- allvisits %>% 
  # keep only patients categorized as Black or White, only 2017
  filter(race_clean %in% c("White"),
         sex %in% c("Female", "Male")) %>% 
  # reorder race, create number of days since 8/12, create weekend flag
  mutate(days = case_when(
    adm_year == 2016 ~ adm_dtm - ymd(20160810),
    adm_year == 2017 ~ adm_dtm - ymd(20170810),
    adm_year == 2018 ~ adm_dtm - ymd(20180810)
  ),
  pre_post = ifelse(days >= 0, 
                    "post", "pre"),
  pre_post = factor(pre_post, levels = c("pre", "post")),
  post = ifelse(days >= 0, 1, 0),
  female = ifelse(sex == "Female", 1, 0),
  absdays = abs(days),
  weekend = ifelse(wday(adm_dtm, week_start = 1) >= 6,
                   "weekend", "weekday"),
  day_of_week = wday(adm_dtm, label = TRUE),
  year2017 = ifelse(adm_year == 2017, "2017", "2016 & 2018"),
  year2017 = factor(year2017, levels = c("2017", "2016 & 2018"))
  ) %>% 
  # keep only cases within 2 weeks of 8/12
  filter(absdays < 15)

#### mups ----
model2_mups_black <- 
  glm(mups1 ~ age_at_event + sex + day_of_week + 
        year2017*pre_post*absdays, 
      family = "binomial", data = df_black)

model2_mups_white <- 
  glm(mups1 ~ age_at_event + sex + day_of_week + 
        year2017*pre_post*absdays, 
      family = "binomial", data = df_white)

#### Table ----
modelsummary(list("Black Patients" = model2_mups_black, "White Patients" = model2_mups_white),
             stars = TRUE,
             coef_map = c("(Intercept)" = "Intercept",
                          "pre_postpost" = "Post 8/11 (2017)",
                          "absdays" = "Days post 8/11 (2017)",
                          "pre_postpost:absdays" = "Post 8/11 x Days post 8/11 (2017)",
                          "year20172016 & 2018" = "2016 & 2018",
                          "year20172016 & 2018:pre_postpost" = "Post 8/11 (2016 & 2018)",
                          "year20172016 & 2018:absdsays" = "Days post 8/11 (2016 & 2018)",
                          "year20172016 & 2018:pre_postpost:absdays" = "Post 8/11 x Days Post 8/11 (2016 & 2018)"),
             gof_map = c("nobs", "logLik"),
             output = "mups_2017vs201618_table.docx")

#### Visualize ----
mean_pred2_mups <- augment(model2_mups_black, type.predict = "response", se_fit = TRUE)

mean_pred2_mups <- mean_pred2_mups %>% 
  mutate(days = ifelse(pre_post == "pre", -1*as.numeric(absdays), as.numeric(absdays))) %>% 
  group_by(year2017, pre_post, days) %>% 
  summarize(p_mups = mean(.fitted)) 

pp_mups_black <- ggplot(mean_pred2_mups, aes(x = days, y = p_mups, 
                                             group = pre_post)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 1/10, color = "grey", aes(group = pre_post)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_y_continuous(limits = c(0,1)) + 
  labs(x = "Days from Event", y = "Predicted Probability") +
  facet_wrap(~year2017) +
  labs(title = "MUPS Diagnoses: Black Patients") +
  theme_minimal()

mean_pred2_mups <- augment(model2_mups_white, type.predict = "response", se_fit = TRUE)

mean_pred2_mups <- mean_pred2_mups %>% 
  mutate(days = ifelse(pre_post == "pre", -1*as.numeric(absdays), as.numeric(absdays))) %>% 
  group_by(year2017, pre_post, days) %>% 
  summarize(p_mups = mean(.fitted)) 

pp_mups_white <- ggplot(mean_pred2_mups, aes(x = days, y = p_mups, 
                                             group = pre_post)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 1/10, color = "grey", aes(group = pre_post)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_y_continuous(limits = c(0,1)) + 
  labs(x = "Days from Event", y = "Predicted Probability") +
  facet_wrap(~year2017) +
  labs(title = "MUPS Diagnoses: White Patients") +
  theme_minimal()

pp_mups_black / pp_mups_white
ggsave("mups_2017vs20162018_predprob.png")

#### anx/dep ----
model2_anx2_black <- 
  glm(anx_dep ~ age_at_event + sex + day_of_week + 
        year2017*pre_post*absdays, 
      family = "binomial", data = df_black)

model2_anx2_white <- 
  glm(anx_dep ~ age_at_event + sex + day_of_week + 
        year2017*pre_post*absdays, 
      family = "binomial", data = df_white)

#### Table ----
modelsummary(list("Black Patients" = model2_anx2_black, "White Patients" = model2_anx2_white),
             stars = TRUE,
             coef_map = c("(Intercept)" = "Intercept",
                          "pre_postpost" = "Post 8/11 (2017)",
                          "absdays" = "Days post 8/11 (2017)",
                          "pre_postpost:absdays" = "Post 8/11 x Days post 8/11 (2017)",
                          "year20172016 & 2018" = "2016 & 2018",
                          "year20172016 & 2018:pre_postpost" = "Post 8/11 (2016 & 2018)",
                          "year20172016 & 2018:absdsays" = "Days post 8/11 (2016 & 2018)",
                          "year20172016 & 2018:pre_postpost:absdays" = "Post 8/11 x Days Post 8/11 (2016 & 2018)"),
             gof_map = c("nobs", "logLik"),
             output = "anx_2017vs20162018_table.docx")


#### Visualize ----
mean_pred2_mups <- augment(model2_anx2_black, type.predict = "response", se_fit = TRUE)

mean_pred2_mups <- mean_pred2_mups %>% 
  mutate(days = ifelse(pre_post == "pre", -1*as.numeric(absdays), as.numeric(absdays))) %>% 
  group_by(year2017, pre_post, days) %>% 
  summarize(p_mups = mean(.fitted)) 

pp_anx_black <- ggplot(mean_pred2_mups, aes(x = days, y = p_mups, 
                                            group = pre_post)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 1/10, color = "grey", aes(group = pre_post)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_y_continuous(limits = c(0,0.25)) + 
  labs(x = "Days from Event", y = "Predicted Probability") +
  facet_wrap(~year2017) +
  labs(title = "Anxiety/Depression Diagnoses: Black Patients") +
  theme_minimal()

mean_pred2_mups <- augment(model2_anx2_white, type.predict = "response", se_fit = TRUE)

mean_pred2_mups <- mean_pred2_mups %>% 
  mutate(days = ifelse(pre_post == "pre", -1*as.numeric(absdays), as.numeric(absdays))) %>% 
  group_by(year2017, pre_post, days) %>% 
  summarize(p_mups = mean(.fitted)) 

pp_anx_white <- ggplot(mean_pred2_mups, aes(x = days, y = p_mups, 
                                            group = pre_post)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 1/10, color = "grey", aes(group = pre_post)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_y_continuous(limits = c(0,0.25)) + 
  labs(x = "Days from Event", y = "Predicted Probability") +
  facet_wrap(~year2017) +
  labs(title = "Anxiety/Depression Diagnoses: White Patients") +
  theme_minimal()

pp_anx_black / pp_anx_white
ggsave("anx_2017vs20162018_predprob.png")
