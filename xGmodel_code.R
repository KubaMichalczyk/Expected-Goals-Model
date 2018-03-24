library(tidyverse)
library(lubridate)
library(magick)

source('strata_pitch_plot.R')

chances_PL_16_17 <- read_csv('chances_EngPL_2016_2017.csv', 
                             col_types = cols(time = col_character(),
                                              location_x = col_double(),
                                              location_y = col_double(),
                                              primaryLocation_x = col_double(),
                                              primaryLocation_y = col_double()))
chances_L1_16_17 <- read_csv('chances_FraL1_2016_2017.csv',
                             col_types = cols(time = col_character(),
                                              location_x = col_double(),
                                              location_y = col_double(),
                                              primaryLocation_x = col_double(),
                                              primaryLocation_y = col_double()))
chances_BL_16_17 <- read_csv('chances_GerBL_2016_2017.csv', 
                             col_types = cols(time = col_character(),
                                              location_x = col_double(),
                                              location_y = col_double(),
                                              primaryLocation_x = col_double(),
                                              primaryLocation_y = col_double()))
chances_SA_16_17 <- read_csv('chances_ItaSA_2016_2017.csv', 
                             col_types = cols(time = col_character(),
                                              location_x = col_double(),
                                              location_y = col_double(),
                                              primaryLocation_x = col_double(),
                                              primaryLocation_y = col_double()))
chances_PD_16_17 <- read_csv('chances_SpaPD_2016_2017.csv', 
                             col_types = cols(time = col_character(),
                                              location_x = col_double(),
                                              location_y = col_double(),
                                              primaryLocation_x = col_double(),
                                              primaryLocation_y = col_double()))
chances_PL_17_18 <- read_csv('chances_EngPL_from_2017-07-01.csv', 
                             col_types = cols(time = col_character(),
                                              location_x = col_double(),
                                              location_y = col_double(),
                                              primaryLocation_x = col_double(),
                                              primaryLocation_y = col_double()))
chances_L1_17_18 <- read_csv('chances_FraL1_from_2017-07-01.csv', 
                             col_types = cols(time = col_character(),
                                              location_x = col_double(),
                                              location_y = col_double(),
                                              primaryLocation_x = col_double(),
                                              primaryLocation_y = col_double()))
chances_BL_17_18 <- read_csv('chances_GerBL_from_2017-07-01.csv', 
                             col_types = cols(time = col_character(),
                                              location_x = col_double(),
                                              location_y = col_double(),
                                              primaryLocation_x = col_double(),
                                              primaryLocation_y = col_double()))
chances_SA_17_18 <- read_csv('chances_ItaSA_from_2017-07-01.csv', 
                             col_types = cols(time = col_character(),
                                              location_x = col_double(),
                                              location_y = col_double(),
                                              primaryLocation_x = col_double(),
                                              primaryLocation_y = col_double()))
chances_PD_17_18 <- read_csv('chances_SpaPD_from_2017-07-01.csv', 
                             col_types = cols(time = col_character(),
                                              location_x = col_double(),
                                              location_y = col_double(),
                                              primaryLocation_x = col_double(),
                                              primaryLocation_y = col_double()))

chances_all <- bind_rows(chances_BL_16_17, chances_L1_16_17, chances_PD_16_17, chances_PL_16_17, 
                         chances_SA_16_17, chances_BL_17_18, chances_L1_17_18, chances_PD_17_18,
                         chances_PL_17_18, chances_SA_17_18)
chances_all <- chances_all %>% rename(id = X1)

chances_all %>% glimpse()

chances_all %>% group_by(id) %>% filter(n() > 1) %>% count()

chances_all <- chances_all %>% 
  arrange(kickoffDate, kickoffTime, as.numeric(ms(time))) %>%
  mutate(old_id = id, id = row_number())

chances <- chances_all %>% filter(!icon == 'penawarded')
penalties <- chances %>% filter(str_detect(type, 'Penalty') | icon == 'penmissed')
chances <- chances %>% filter(!id %in% penalties$id)

chances %>% ggplot() + geom_bar(aes(icon)) + coord_flip()


chances <- chances %>% filter(!icon == 'owngoal')

chances %>% ggplot() + geom_bar(aes(chanceRating)) + coord_flip()

penalties2 <- chances %>% filter(chanceRating == 'Penalty')
penalties <- bind_rows(penalties, penalties2)
chances <- chances %>% filter(!id %in% penalties2$id)

chances <- chances %>% 
  mutate(chanceRating = case_when(
    str_detect(chanceRating, '[Ss]uperb') ~ 'Superb',
    str_detect(chanceRating, '[Gg]reat') ~ 'Great',
    str_detect(chanceRating, '[Vv]ery[ ]*[Gg]ood*') ~ 'Very good',
    str_detect(chanceRating, '^[Gg]ood') ~ 'Good',
    str_detect(chanceRating, '[Ff]airly') ~ 'Fairly good',
    str_detect(chanceRating, '[Pp]oor') ~ 'Poor'
  )
  )
chances %>% ggplot() + geom_bar(aes(type)) + coord_flip()

chances <- chances %>%
  mutate(type = case_when(
    str_detect(type, 'Open [Pp]lay') ~ 'Open Play',
    str_detect(type, 'Direct [Ff]ree[- ][Kk]ick') ~ 'Direct Free-Kick',
    TRUE ~ type))

temp <- chances %>% filter(str_detect(type, 'Direct [Cc]orner'))
pitch_plot() + geom_point(data = temp, aes(location_x, location_y), color = 'yellow', size = 2)
temp2 <- temp %>% filter((location_x < -130 & bodyPart == 'Left') | 
                           (location_x > 130 & bodyPart == 'Right'))
count(temp2)

chances <- chances %>% 
  mutate(type = ifelse(id %in% temp2$id, 'Direct Free-Kick', type))

temp <- chances %>% filter(str_detect(type, 'Direct [Cc]orner'))
pitch_plot() + geom_point(data = temp, aes(location_x, location_y, shape = bodyPart), 
                          color = 'yellow', size = 2)
chances <- chances %>% 
  mutate(type = ifelse(id %in% temp$id,'Open Play', type),
         primaryType = ifelse(id %in% temp$id, 'Corner', primaryType))

chances <- chances %>% 
  mutate(type = ifelse(id %in% temp$id,'Open Play', type),
         primaryType = ifelse(id %in% temp$id, 'Corner', primaryType))

chances %>% ggplot() + geom_bar(aes(bodyPart))

chances <- chances %>%
  mutate(bodyPart = ifelse(bodyPart %in% c('Left', 'Right'), 'Foot', bodyPart))

chances %>% ggplot() + geom_bar(aes(outcome))

chances %>% ggplot() + geom_bar(aes(primaryType)) + coord_flip()

temp <- chances %>% filter(primaryType == 'Penalty Earned')
pitch_plot() + geom_point(data = temp, aes(primaryLocation_x, primaryLocation_y), 
                          color = 'yellow', size = 2)

chances <- chances %>% mutate(primaryType = ifelse(primaryType == 'Penalty Earned',
                                                   NA, primaryType))

chances %>% ggplot() + geom_bar(aes(secondaryType)) + coord_flip()
chances %>% filter(secondaryType == 'Penalty Earned') %>% 
  ggplot() + geom_bar(aes(primaryType)) + coord_flip()

pitch_plot() + geom_hex(data = chances, aes(location_x, location_y))

temp <- chances %>% filter(location_y > 300)
pitch_plot() + geom_point(data = temp, aes(location_x, location_y, shape = bodyPart), 
                          color = 'red', size = 2) + 
  geom_segment(data = temp, aes(x = primaryLocation_x, primaryLocation_y,
                                xend = location_x, yend = location_y), color = '#C43534',
               arrow = arrow(length = unit(0.3, 'cm')))

chances <- chances %>% filter(!id %in% temp$id)

chances <- chances %>% 
  mutate(goal = (icon == 'goal'),
         dist = sqrt(location_x^2+location_y^2)/sqrt(136^2 + 420^2),
         angle = abs(atan2(location_x, location_y)/(pi/2)))

chances <- chances %>% 
  separate(time, c('min', 'sec'), sep = ":", remove = FALSE) %>% 
  group_by(gsm_id) %>% 
  arrange(min, sec, .by_group = TRUE) %>% 
  mutate(hometeam_score = lag(goal * (team == hometeam_team1), default = 0), 
         awayteam_score = lag(goal * (team == awayteam_team2), default = 0),
         game_state = ifelse(team == hometeam_team1, hometeam_score - awayteam_score, awayteam_score - hometeam_score)) %>% 
  select(-hometeam_score, -awayteam_score) %>% 
  ungroup()

chances <- chances %>% 
  mutate(dang_moment = (type == 'Dangerous Moment'),
         type_of_attack = case_when(
           bodyPart == 'Foot' & primaryType == 'Open Play Pass' ~ paste(bodyPart, primaryType, sep = '_'),
           bodyPart == 'Head' & primaryType == 'Open Play Pass' ~ paste(bodyPart, primaryType, sep = '_'),
           bodyPart == 'Other' & primaryType == 'Open Play Pass' ~ paste(bodyPart, primaryType, sep = '_'),
           bodyPart == 'Foot' & primaryType == 'Cross Low' ~ paste(bodyPart, primaryType, sep = '_'),
           bodyPart == 'Head' & primaryType == 'Cross Low' ~ paste(bodyPart, primaryType, sep = '_'),
           bodyPart == 'Other' & primaryType == 'Cross Low' ~ paste(bodyPart, primaryType, sep = '_'),
           bodyPart == 'Foot' & primaryType == 'Cross High' ~ paste(bodyPart, primaryType, sep = '_'),
           bodyPart == 'Head' & primaryType == 'Cross High' ~ paste(bodyPart, primaryType, sep = '_'),
           bodyPart == 'Other' & primaryType == 'Cross High' ~ paste(bodyPart, primaryType, sep = '_'),
           bodyPart == 'Foot' & primaryType %in% c('Corner', 'Free Kick') ~ paste(bodyPart, primaryType, sep = '_'),
           bodyPart == 'Head' & primaryType %in% c('Corner', 'Free Kick') ~ paste(bodyPart, primaryType, sep = '_'),
           bodyPart == 'Other' & primaryType %in% c('Corner', 'Free Kick') ~ paste(bodyPart, primaryType, sep = '_'),
           primaryType %in% c('Shot (Opposition Rebound)', 'Shot (Deflection)', 'Shot (Woodwork Rebound)') ~
             'Rebound',
           type == 'Direct Free-Kick' ~ 'Direct Free-Kick',
           TRUE ~ 'Other not assisted'
         )
  )

chances <- chances %>% mutate(competition = parse_factor(competition, levels = NULL),
                              icon = parse_factor(icon, levels = NULL),
                              chanceRating = parse_factor(chanceRating, 
                                                          levels = c('Poor', 'Fairly good', 
                                                                     'Good', 'Very good',
                                                                     'Great', 'Superb')),
                              type = parse_factor(type, levels = NULL),
                              bodyPart = parse_factor(bodyPart, levels = NULL),
                              shotQuality = parse_factor(shotQuality, levels = 0:5),
                              defPressure = parse_factor(defPressure, levels = 0:5),
                              numDefPlayers = parse_factor(numDefPlayers, levels = 0:11),
                              numAttPlayers = parse_factor(numAttPlayers, levels = 0:11),
                              outcome = parse_factor(outcome, levels = NULL),
                              primaryType = parse_factor(primaryType, levels = NULL),
                              secondaryType = parse_factor(secondaryType, levels = NULL),
                              type_of_attack = parse_factor(type_of_attack, levels = NULL)
)

set.seed(0)
chances_train <- sample_frac(chances, 0.7, replace = FALSE)
chances_test <- chances %>% filter(!id %in% chances_train$id)
chances_train <- chances_train %>% filter(!dang_moment)
count(chances_train)/count(chances)

library(lmtest)
library(tidyverse)
library(rms)
library(Epi)

summary(glm(goal ~ type, family = 'binomial', data = chances_train)) #+
summary(glm(goal ~ bodyPart, family = 'binomial', data = chances_train)) #+
summary(glm(goal ~ shotQuality, family = 'binomial', data = chances_train)) #+
summary(glm(goal ~ defPressure, family = 'binomial', data = chances_train)) #+
summary(glm(goal ~ numDefPlayers, family = 'binomial', data = chances_train)) #+
summary(glm(goal ~ numAttPlayers, family = 'binomial', data = chances_train)) #+

chances_train %>% group_by(numAttPlayers) %>% count()

chances_train <- chances_train %>% 
  mutate(numAttPlayers2 = fct_collapse(numAttPlayers, '>4' = c('5', '6', '7')))
summary(glm(goal ~ numAttPlayers2, family = 'binomial', data = chances_train)) #+

chances_train <- chances_train %>% mutate(numAttPlayers = numAttPlayers2) %>% select(-numAttPlayers2)

summary(glm(goal ~ dist, family = 'binomial', data = chances_train)) #+
summary(glm(goal ~ angle, family = 'binomial', data = chances_train)) #+
summary(glm(goal ~ game_state, family = 'binomial', data = chances_train)) #+
summary(glm(goal ~ type_of_attack, family = 'binomial', data = chances_train)) #+

chances_train <- chances_train %>% 
  mutate(type_of_attack2 = fct_collapse(type_of_attack, 
                                        'Corner' = c('Foot_Corner', 'Head_Corner', 'Other_Corner')
  ))
summary(glm(goal ~ type_of_attack2, family = 'binomial', data = chances_train)) #+

chances_train <- chances_train %>% 
  mutate(type_of_attack3 = fct_collapse(type_of_attack2,
                                        'Free Kick' = c('Foot_Free Kick', 'Head_Free Kick', 
                                                        'Other_Free Kick')))
summary(glm(goal ~ type_of_attack3, family = 'binomial', data = chances_train)) #+

chances_train <- chances_train %>% 
  mutate(type_of_attack = type_of_attack3) %>% 
  select(-type_of_attack2, -type_of_attack3)

chances_test <- chances_test %>% mutate(type_of_attack = fct_collapse(type_of_attack, 
                                                                      'Corner' = c('Foot_Corner', 'Head_Corner', 'Other_Corner'),
                                                                      'Free Kick' = c('Foot_Free Kick', 'Head_Free Kick', 
                                                                                      'Other_Free Kick')))

model2 <- glm(goal ~ shotQuality + defPressure + numDefPlayers + numAttPlayers + dist + angle + 
                game_state + type_of_attack, 
              family = 'binomial', data = chances_train)
summary(model2)

model2a <- glm(goal ~ shotQuality + defPressure + numDefPlayers + dist + angle +
                 game_state + type_of_attack, family = 'binomial', data = chances_train)
summary(model2a)
lrtest(model2a, model2)

model2_fin <- model2a

(model2_fin$coefficients - model2$coefficients[names(model2_fin$coefficients)])/
  model2$coefficients[names(model2_fin$coefficients)] * 100

model3_fin <- model2_fin

logit_loess <- function(x, y, span){
  
  logit <- function(p) log(p/(1-p))
  
  loess_fit <- predict(loess(y ~ x, span = span))
  pi <- pmax(pmin(loess_fit,0.9999),0.0001)
  logit_fitted <- logit(pi)
  
  plt <- ggplot() + geom_point(aes(x, logit_fitted)) + scale_y_continuous(name = 'logit')
  return(plt)
}

logit_loess(chances_train$dist, chances_train$goal, span = 0.1)
logit_loess(chances_train$dist, chances_train$goal, span = 0.2)
logit_loess(chances_train$dist, chances_train$goal, span = 0.4)

spline_dist3 <- glm(goal ~ rcs(dist, parms = 3), family = 'binomial', data = chances_train)
spline_dist4 <- glm(goal ~ rcs(dist, parms = 4), family = 'binomial', data = chances_train)
spline_dist5 <- glm(goal ~ rcs(dist, parms = 5), family = 'binomial', data = chances_train)

summary(spline_dist3)$aic
summary(spline_dist4)$aic
summary(spline_dist5)$aic

logit_loess(chances_train$angle, chances_train$goal, span = 0.1)
logit_loess(chances_train$angle, chances_train$goal, span = 0.2)
logit_loess(chances_train$angle, chances_train$goal, span = 0.4)

spline_angle3 <- glm(goal ~ rcs(angle, parms = 3), family = 'binomial', data = chances_train)
spline_angle4 <- glm(goal ~ rcs(angle, parms = 4), family = 'binomial', data = chances_train)
spline_angle5 <- glm(goal ~ rcs(angle, parms = 5), family = 'binomial', data = chances_train)
spline_angle6 <- glm(goal ~ rcs(angle, parms = 6), family = 'binomial', data = chances_train)
spline_angle7 <- glm(goal ~ rcs(angle, parms = 7), family = 'binomial', data = chances_train)
summary(spline_angle3)$aic
summary(spline_angle4)$aic
summary(spline_angle5)$aic
summary(spline_angle6)$aic
summary(spline_angle7)$aic

model4 <- glm(goal ~ shotQuality + defPressure + numDefPlayers + rcs(dist, parms = 5)  + 
                rcs(angle, parms = 6) + game_state + type_of_attack, 
              family = 'binomial', data = chances_train)
summary(model4)

model4a <- glm(goal ~ shotQuality + defPressure + numDefPlayers + rcs(dist, parms = 5) + game_state +                      type_of_attack, family = 'binomial', data = chances_train)
lrtest(model4a, model4)
(model4a$coefficients - model4$coefficients[names(model4a$coefficients)])/
  model4$coefficients[names(model4a$coefficients)] * 100

ROC(form = goal ~ shotQuality + defPressure + numDefPlayers + rcs(dist, parms = 5)  + 
      rcs(angle, parms = 6) + game_state + type_of_attack, 
    family = 'binomial', data = chances_test, plot = 'ROC')

1 - logLik(model4)/logLik(glm(goal~1, data = chances_train, family = 'binomial'))

chances_test <- chances_test %>% 
  mutate(xG = predict(model4, type = 'response', newdata = chances_test))

rmse <- function(x, y) sqrt(mean((x - y)^2))
rmse_all <- map(1:100, ~ chances_test %>% 
                  transmute(!! paste0("pred", .x) := as.numeric(xG > 0.01 * .x))) %>% 
  bind_cols(.) %>% map_dbl(rmse, y = chances_test$goal)
ggplot() + geom_line(aes(seq(0.01, 1, by = 0.01), rmse_all))
rmse_all[which.min(rmse_all)]
