Expected Goals model
================
Twitter: @KubaMichalczyk

Since I started to follow the football analytics community, Expected Goals has been popping up everywhere. However, methodology is rarely described, not to mention the whole process of modelling. Therefore I just decided to document my work and publish it, so that:

-   when I'll post some remark/graph with xG calculated you could easily check how it's calculated and judge for yourself if it has any value
-   those who have access to @Stratabet data could just take it from there and possibly improve it.

Data contains recorded chances from 5 biggest European leagues (German Bundesliga, Spanish Primera Division, English Premier League, Italian Serie A and French League 1) during whole 2016/2017 season and a part of current season (until 19/02/2017).

Data preparation
----------------

``` r
library(tidyverse)
library(lubridate)
library(magick)

source('strata_pitch_plot.R')
```

Firstly, we import the data. While importing data I set the time parser as character as the default guessing interprets this variable as time type (which means time of a day) and fails to read all the values greater than 24:00. I also set the parser to all location variables as it'll be helpful to plot some observations directly on the pitch while tidying data.

``` r
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
```

``` r
chances_all %>% glimpse()
```

    ## Observations: 65,769
    ## Variables: 27
    ## $ id                <int> 88, 92, 91, 90, 89, 87, 86, 914, 901, 902, 9...
    ## $ competition       <chr> "GerBL1", "GerBL1", "GerBL1", "GerBL1", "Ger...
    ## $ gsm_id            <int> 2255206, 2255204, 2255204, 2255209, 2255209,...
    ## $ kickoffDate       <date> 2017-05-20, 2017-05-20, 2017-05-20, 2017-05...
    ## $ kickoffTime       <time> 13:30:00, 13:30:00, 13:30:00, 13:30:00, 13:...
    ## $ hometeam_team1    <chr> "Hertha BSC", "Borussia Dortmund", "Borussia...
    ## $ awayteam_team2    <chr> "Bayer Leverkusen", "Werder Bremen", "Werder...
    ## $ icon              <chr> "penawarded", "penawarded", "penawarded", "p...
    ## $ chanceRating      <chr> "Good", "Poor", "Good", "Very Good", "Great"...
    ## $ team              <chr> "Hertha BSC", "Borussia Dortmund", "Borussia...
    ## $ type              <chr> "Holding", "Tackle", "Tackle", "Holding", "H...
    ## $ time              <chr> "84:05", "73:50", "87:25", "39:26", "62:18",...
    ## $ player            <chr> "-", "-", "-", "-", "-", "-", "-", "D. Blum"...
    ## $ location_x        <dbl> NA, NA, NA, NA, NA, NA, NA, 53, 14, 0, 0, 0,...
    ## $ location_y        <dbl> NA, NA, NA, NA, NA, NA, NA, 23, 34, 44, 44, ...
    ## $ bodyPart          <chr> "-", "-", "-", "-", "-", "-", "-", "Left", "...
    ## $ shotQuality       <chr> "N/A", "N/A", "N/A", "N/A", "N/A", "N/A", "N...
    ## $ defPressure       <chr> "-", "-", "-", "-", "-", "-", "-", "1", "1",...
    ## $ numDefPlayers     <chr> "-", "-", "-", "-", "-", "-", "-", "1", "2",...
    ## $ numAttPlayers     <chr> "-", "-", "-", "-", "-", "-", "-", "0", "1",...
    ## $ outcome           <chr> "Penalty Awarded", "Penalty Awarded", "Penal...
    ## $ primaryPlayer     <chr> "-", "-", "-", "-", "-", "-", "-", "B. Oczip...
    ## $ primaryType       <chr> "-", "-", "-", "-", "-", "-", "-", "Open Pla...
    ## $ primaryLocation_x <dbl> NA, NA, NA, NA, NA, NA, NA, 124, 50, NA, -7,...
    ## $ primaryLocation_y <dbl> NA, NA, NA, NA, NA, NA, NA, 41, 3, NA, 114, ...
    ## $ secondaryPlayer   <chr> "-", "-", "-", "-", "-", "-", "-", "D. Blum"...
    ## $ secondaryType     <chr> "-", "-", "-", "-", "-", "-", "-", "Open Pla...

Most of variables were parsed as character, but let's keep them this way for now (I will parse them into appropriate type (ex. factor or double) just before modelling part).

Firstly, let's check ID's uniqueness.

``` r
chances_all %>% group_by(id) %>% filter(n() > 1) %>% count()
```

    ## # A tibble: 11,321 x 2
    ## # Groups:   id [11,321]
    ##       id     n
    ##    <int> <int>
    ##  1    23     2
    ##  2    33     2
    ##  3    44     2
    ##  4    54     2
    ##  5    56     2
    ##  6    58     2
    ##  7    61     3
    ##  8    62     2
    ##  9    65     3
    ## 10    72     2
    ## # ... with 11,311 more rows

There are some duplicated IDs, so let's give our own IDs based on time of event.

``` r
chances_all <- chances_all %>% 
  arrange(kickoffDate, kickoffTime, as.numeric(ms(time))) %>%
  mutate(old_id = id, id = row_number())
```

Note: This is not accurately sorted by time of event, but it will do.

Next, we exclude penalties from the data set. Due to their specificity another model could be build to capture penalties.

``` r
chances <- chances_all %>% filter(!icon == 'penawarded')
penalties <- chances %>% filter(str_detect(type, 'Penalty') | icon == 'penmissed')
chances <- chances %>% filter(!id %in% penalties$id)
```

Now we will check a data coherence.

<img src="expected_goals_model_files/figure-markdown_github/unnamed-chunk-5-1.png" style="display: block; margin: auto;" />

We don't want to model own goal; we are excluding them from our data set.

``` r
chances <- chances %>% filter(!icon == 'owngoal')
```

<img src="expected_goals_model_files/figure-markdown_github/unnamed-chunk-7-1.png" style="display: block; margin: auto;" />

There are still some penalties...

``` r
penalties2 <- chances %>% filter(chanceRating == 'Penalty')
penalties <- bind_rows(penalties, penalties2)
chances <- chances %>% filter(!id %in% penalties2$id)
```

...and some inconcistencies in inputted values - let's fix them as well.

``` r
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
```

<img src="expected_goals_model_files/figure-markdown_github/unnamed-chunk-10-1.png" style="display: block; margin: auto;" />

Let's unify the inputs (ex. 'Direct Free-Kick' and 'Direct Free kick').

``` r
chances <- chances %>%
  mutate(type = case_when(
    str_detect(type, 'Open [Pp]lay') ~ 'Open Play',
    str_detect(type, 'Direct [Ff]ree[- ][Kk]ick') ~ 'Direct Free-Kick',
    TRUE ~ type))
```

According to Stratagem definitions a direct shot from corner is marked as Direct Free-kick. So we should investigate which of these should be remarked as Direct Free-kick and which of these are just `primaryType` - we can do so based on (x, y) coordinates.

<img src="expected_goals_model_files/figure-markdown_github/unnamed-chunk-12-1.png" style="display: block; margin: auto;" />

``` r
temp2 <- temp %>% filter((location_x < -130 & bodyPart == 'Left') | 
                  (location_x > 130 & bodyPart == 'Right')) 
```

    ## # A tibble: 1 x 1
    ##       n
    ##   <int>
    ## 1    20

We can observe that 20 chances which are Corner kicks (based on location) are in-swingers (corners swerving towards the goal) based on x coordinate and foot used So these could be actually direct attempt with Khazri, Callejon and Pulgar unique goals among them.

``` r
chances <- chances %>% 
  mutate(type = ifelse(id %in% temp2$id, 'Direct Free-Kick', type))
```

<img src="expected_goals_model_files/figure-markdown_github/unnamed-chunk-14-1.png" style="display: block; margin: auto;" />

The remaining 10 observations are just shots after corner-kick, so we should probably change a `type` to 'Open Play' and `primaryType` to 'Corner'.

``` r
chances <- chances %>% 
  mutate(type = ifelse(id %in% temp$id,'Open Play', type),
         primaryType = ifelse(id %in% temp$id, 'Corner', primaryType))
```

Ok, so we are left with 3 categories. But let's keep in mind that Dangerous Moment is by definition the moment when there was an opportunity to shoot, but a shot wasn't taken. Since there couldn't be a goal in these situations, we should probably exclude these observations from our training set and include them in the test set only.

<img src="expected_goals_model_files/figure-markdown_github/unnamed-chunk-16-1.png" style="display: block; margin: auto;" />

We can observe that shots with right foot were taken over 1.5 times more than with left foot. This is certainly not surprising as left footed players are a definite minority and beyond any doubt you usually prefer to shoot with your stronger foot. But, as we do not have an infomation if the player is right or left footed, I don't think it's a good idea to differentiate. As an information if player takes the shot with his stronger or weaker foot seems to be crucial it would be a good idea to look for some dataset (could be for ex. from transfermarkt or FIFA game series) with information if player is left/right footed and match the datasets together. This makes a room for improvement in future, but as for now we just create a one level for both feets.

``` r
chances <- chances %>%
  mutate(bodyPart = ifelse(bodyPart %in% c('Left', 'Right'), 'Foot', bodyPart))
```

<img src="expected_goals_model_files/figure-markdown_github/unnamed-chunk-18-1.png" style="display: block; margin: auto;" />

Nothing baffling here.

<img src="expected_goals_model_files/figure-markdown_github/unnamed-chunk-19-1.png" style="display: block; margin: auto;" />

We were supposed to exclude all penalties, but there are some assists marked as 'Penalty Earned'. So let's take a closer look.

<img src="expected_goals_model_files/figure-markdown_github/unnamed-chunk-20-1.png" style="display: block; margin: auto;" />

In 11 out of 14 cases this can't be Penalty Earned as the location is outside penalty area. These chances don't look like as penalties taken as well (location again). As it's hard to figure out what should be a correct value of `primaryType` in these cases I'll just replace them with `NA`.

``` r
chances <- chances %>% mutate(primaryType = ifelse(primaryType == 'Penalty Earned',
                                                   NA, primaryType))
```

<img src="expected_goals_model_files/figure-markdown_github/unnamed-chunk-22-1.png" style="display: block; margin: auto;" /><img src="expected_goals_model_files/figure-markdown_github/unnamed-chunk-22-2.png" style="display: block; margin: auto;" />

These chances looks like deflected penalties, but since after deflection we have 'Open Play' chance let's keep it.

<img src="expected_goals_model_files/figure-markdown_github/unnamed-chunk-23-1.png" style="display: block; margin: auto;" />

There are some chances recorded from abnormal distance, so let's look closer at them.

<img src="expected_goals_model_files/figure-markdown_github/unnamed-chunk-24-1.png" style="display: block; margin: auto;" />

We can see that two of these shots are marked as a shot with Head, which is impossible from such a distance. The third and fourth ones are marked as assisted from the other side of the pitch in 49th and 13th minute of a game, respectively - hence I doubt that were a one of these situations where goalie is off position. So all of these four are just probably incorrectly inputted - I'll remove them so they don't affect a model fit.

``` r
chances <- chances %>% filter(!id %in% temp$id)
```

Now let's create some variables, which may be useful to our xG model.

Starting from the obvious one - goal, as well as the distance and angle from which shot took place. Here, the `angle` is defined to be 0 for shots directly in front of goal, and 1 for shots from the endline. `dist` is distance standarised with maximum possible distance (from the corner on the other side of the pitch).

``` r
chances <- chances %>% 
  mutate(goal = (icon == 'goal'),
         dist = sqrt(location_x^2+location_y^2)/sqrt(136^2 + 420^2),
         angle = abs(atan2(location_x, location_y)/(pi/2)))
```

Let's also create a variable that describes the state (as a goal difference) in which game was in while chance occurred.

``` r
chances <- chances %>% 
  separate(time, c('min', 'sec'), sep = ":", remove = FALSE) %>% 
  group_by(gsm_id) %>% 
  arrange(min, sec, .by_group = TRUE) %>% 
  mutate(hometeam_score = lag(goal * (team == hometeam_team1), default = 0), 
         awayteam_score = lag(goal * (team == awayteam_team2), default = 0),
         game_state = ifelse(team == hometeam_team1, hometeam_score - awayteam_score, awayteam_score - hometeam_score)) %>% 
  select(-hometeam_score, -awayteam_score) %>% 
  ungroup()
```

Finally, we would like to gather infomation from `type`, `bodyPart` and `primaryType` to capture the type of attack. We will differentiate:

1.  chances with Foot after Open Play Pass
2.  chances with Head after Open Play Pass
3.  chances with Other after Open Play Pass
4.  chances with Foot after Cross Low
5.  chances with Head after Cross Low
6.  chances with Other after Cross Low
7.  chances with Foot after Cross High
8.  chances with Head after Cross High
9.  chances with Other after Cross High
10. chances with Foot after Free-Kicks and Corners
11. chances with Head after Free-Kicks and Corners
12. chances with Other after Free-Kicks and Corners
13. Rebounds
14. Direct free-kicks (incl. direct corners)
15. other type of chances without an assist

We could do it by interactions, but then we would have 3\*3\*15 types.

We would like to also keep the information if the type of chance was 'Dangerous Moment' so firstly, we'll just create dichotomous variable `dang_moment`.

``` r
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
```

Now we will parse to factor some variables read in as character.

``` r
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
```

And finally, split the data into training and testing set, keeping to the standard 70/30 rule. As previously mentioned, we'll also exclude dangerous moments from our training set.

``` r
set.seed(0)
chances_train <- sample_frac(chances, 0.7, replace = FALSE)
chances_test <- chances %>% filter(!id %in% chances_train$id)
chances_train <- chances_train %>% filter(!dang_moment)
count(chances_train)/count(chances)
```

    ##           n
    ## 1 0.6737941

Now we have around 67% of data set in a training set, but it will do.

Building a xG model
-------------------

In my xG modelling attempt I have employed purposeful selection of covariates. There exist some fancy algorithms one could use (ex. stepwise or best subset selection). But what's the fun then?

``` r
library(lmtest)
library(rms)
library(Epi)
```

### Step 1.

Firstly, we will build univariable models. With `+` sign I mark these covariates which are significant in Wald test at the level of 20%. Note: I deliberately omit `chanceRating` as it is subjective measure, which beyond any doubt depends heavily on other potential covariates as `dist`, `angle`, `defPressure`.

``` r
summary(glm(goal ~ type, family = 'binomial', data = chances_train)) #+
```

    ## 
    ## Call:
    ## glm(formula = goal ~ type, family = "binomial", data = chances_train)
    ## 
    ## Deviance Residuals: 
    ##    Min      1Q  Median      3Q     Max  
    ## -0.523  -0.523  -0.523  -0.523   2.323  
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)          -1.92023    0.01486  -129.2   <2e-16 ***
    ## typeDirect Free-Kick -0.70796    0.08529    -8.3   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 32235  on 42855  degrees of freedom
    ## Residual deviance: 32152  on 42854  degrees of freedom
    ## AIC: 32156
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
summary(glm(goal ~ bodyPart, family = 'binomial', data = chances_train)) #+
```

    ## 
    ## Call:
    ## glm(formula = goal ~ bodyPart, family = "binomial", data = chances_train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.0247  -0.5075  -0.5075  -0.5075   2.0559  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error  z value Pr(>|z|)    
    ## (Intercept)   -1.98468    0.01631 -121.675  < 2e-16 ***
    ## bodyPartHead   0.14961    0.03762    3.977 6.99e-05 ***
    ## bodyPartOther  1.61431    0.17150    9.413  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 32235  on 42855  degrees of freedom
    ## Residual deviance: 32148  on 42853  degrees of freedom
    ## AIC: 32154
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
summary(glm(goal ~ defPressure, family = 'binomial', data = chances_train)) #+
```

    ## 
    ## Call:
    ## glm(formula = goal ~ defPressure, family = "binomial", data = chances_train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.5773  -0.5616  -0.5071  -0.4156   2.5381  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  -1.70731    0.03228 -52.897  < 2e-16 ***
    ## defPressure1 -0.05992    0.04327  -1.385    0.166    
    ## defPressure2 -0.27911    0.04455  -6.265 3.73e-10 ***
    ## defPressure3 -0.31721    0.04526  -7.009 2.40e-12 ***
    ## defPressure4 -0.69843    0.05996 -11.649  < 2e-16 ***
    ## defPressure5 -1.47282    0.16452  -8.952  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 32235  on 42855  degrees of freedom
    ## Residual deviance: 31957  on 42850  degrees of freedom
    ## AIC: 31969
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
summary(glm(goal ~ numDefPlayers, family = 'binomial', data = chances_train)) #+
```

    ## 
    ## Call:
    ## glm(formula = goal ~ numDefPlayers, family = "binomial", data = chances_train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7533  -0.4709  -0.3754  -0.3229   2.4767  
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)       1.2949     0.1112  11.643  < 2e-16 ***
    ## numDefPlayers1   -2.1931     0.1141 -19.228  < 2e-16 ***
    ## numDefPlayers2   -3.4384     0.1144 -30.068  < 2e-16 ***
    ## numDefPlayers3   -3.9124     0.1176 -33.268  < 2e-16 ***
    ## numDefPlayers4   -4.3143     0.1308 -32.979  < 2e-16 ***
    ## numDefPlayers5   -4.2227     0.1484 -28.448  < 2e-16 ***
    ## numDefPlayers6   -4.1834     0.1714 -24.400  < 2e-16 ***
    ## numDefPlayers7   -3.8224     0.1921 -19.896  < 2e-16 ***
    ## numDefPlayers8   -3.8882     0.2820 -13.787  < 2e-16 ***
    ## numDefPlayers9   -3.8598     0.5307  -7.274 3.50e-13 ***
    ## numDefPlayers10  -3.1667     0.7677  -4.125 3.71e-05 ***
    ## numDefPlayers11 -11.8609    84.4767  -0.140    0.888    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 32235  on 42855  degrees of freedom
    ## Residual deviance: 28694  on 42844  degrees of freedom
    ## AIC: 28718
    ## 
    ## Number of Fisher Scoring iterations: 9

``` r
summary(glm(goal ~ numAttPlayers, family = 'binomial', data = chances_train)) #+
```

    ## 
    ## Call:
    ## glm(formula = goal ~ numAttPlayers, family = "binomial", data = chances_train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.5660  -0.5660  -0.5660  -0.3647   2.4305  
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error  z value Pr(>|z|)    
    ## (Intercept)     -1.75045    0.01592 -109.977  < 2e-16 ***
    ## numAttPlayers1  -0.92643    0.04764  -19.445  < 2e-16 ***
    ## numAttPlayers2  -1.14965    0.09324  -12.330  < 2e-16 ***
    ## numAttPlayers3  -0.99712    0.15811   -6.306 2.86e-10 ***
    ## numAttPlayers4  -1.11492    0.34306   -3.250  0.00115 ** 
    ## numAttPlayers5  -0.73446    0.73615   -0.998  0.31842    
    ## numAttPlayers6 -10.81562  114.81424   -0.094  0.92495    
    ## numAttPlayers7 -10.81562  229.62847   -0.047  0.96243    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 32235  on 42855  degrees of freedom
    ## Residual deviance: 31572  on 42848  degrees of freedom
    ## AIC: 31588
    ## 
    ## Number of Fisher Scoring iterations: 11

``` r
chances_train %>% group_by(numAttPlayers) %>% count()
```

    ## # A tibble: 8 x 2
    ## # Groups:   numAttPlayers [8]
    ##   numAttPlayers     n
    ##          <fctr> <int>
    ## 1             0 31306
    ## 2             1  8236
    ## 3             2  2397
    ## 4             3   714
    ## 5             4   167
    ## 6             5    26
    ## 7             6     8
    ## 8             7     2

Statistical insignificance of coefficients associated with levels: 5, 6, 7 could be driven by a small counts of these levels. Let's try to group them as one level.

``` r
chances_train <- chances_train %>% 
  mutate(numAttPlayers2 = fct_collapse(numAttPlayers, '>4' = c('5', '6', '7')))
summary(glm(goal ~ numAttPlayers2, family = 'binomial', data = chances_train)) #+
```

    ## 
    ## Call:
    ## glm(formula = goal ~ numAttPlayers2, family = "binomial", data = chances_train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.5660  -0.5660  -0.5660  -0.3647   2.4305  
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error  z value Pr(>|z|)    
    ## (Intercept)      -1.75045    0.01592 -109.977  < 2e-16 ***
    ## numAttPlayers21  -0.92643    0.04764  -19.445  < 2e-16 ***
    ## numAttPlayers22  -1.14965    0.09324  -12.331  < 2e-16 ***
    ## numAttPlayers23  -0.99712    0.15811   -6.306 2.85e-10 ***
    ## numAttPlayers24  -1.11492    0.34306   -3.250  0.00115 ** 
    ## numAttPlayers2>4 -1.08277    0.72777   -1.488  0.13681    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 32235  on 42855  degrees of freedom
    ## Residual deviance: 31573  on 42850  degrees of freedom
    ## AIC: 31585
    ## 
    ## Number of Fisher Scoring iterations: 5

The p-value of Wald test decreased to 0.14 after grouping the variable, so it's probably better to leave this levels as a group.

``` r
chances_train <- chances_train %>% mutate(numAttPlayers = numAttPlayers2) %>% select(-numAttPlayers2)
```

``` r
summary(glm(goal ~ dist, family = 'binomial', data = chances_train)) #+
```

    ## 
    ## Call:
    ## glm(formula = goal ~ dist, family = "binomial", data = chances_train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.1193  -0.5796  -0.3726  -0.2240   4.0333  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)   0.12665    0.03619   3.499 0.000467 ***
    ## dist        -15.35907    0.28594 -53.715  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 32235  on 42855  degrees of freedom
    ## Residual deviance: 28415  on 42854  degrees of freedom
    ## AIC: 28419
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
summary(glm(goal ~ angle, family = 'binomial', data = chances_train)) #+
```

    ## 
    ## Call:
    ## glm(formula = goal ~ angle, family = "binomial", data = chances_train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.5395  -0.5198  -0.5135  -0.5081   2.0591  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -1.99196    0.02618 -76.075   <2e-16 ***
    ## angle        0.13821    0.06954   1.987   0.0469 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 32235  on 42855  degrees of freedom
    ## Residual deviance: 32231  on 42854  degrees of freedom
    ## AIC: 32235
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
summary(glm(goal ~ game_state, family = 'binomial', data = chances_train)) #+
```

    ## 
    ## Call:
    ## glm(formula = goal ~ game_state, family = "binomial", data = chances_train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.5524  -0.5158  -0.5158  -0.5158   2.1037  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error  z value Pr(>|z|)    
    ## (Intercept) -1.94992    0.01463 -133.242   <2e-16 ***
    ## game_state   0.14707    0.04386    3.353    8e-04 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 32235  on 42855  degrees of freedom
    ## Residual deviance: 32224  on 42854  degrees of freedom
    ## AIC: 32228
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
summary(glm(goal ~ type_of_attack, family = 'binomial', data = chances_train)) #+
```

    ## 
    ## Call:
    ## glm(formula = goal ~ type_of_attack, family = "binomial", data = chances_train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.4132  -0.4910  -0.4910  -0.3852   2.3217  
    ## 
    ## Coefficients:
    ##                                    Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)                        -2.05476    0.02254 -91.150  < 2e-16
    ## type_of_attackOther not assisted   -0.50918    0.05029 -10.125  < 2e-16
    ## type_of_attackDirect Free-Kick     -0.57049    0.08724  -6.540 6.17e-11
    ## type_of_attackHead_Cross High       0.25711    0.05419   4.745 2.09e-06
    ## type_of_attackHead_Corner          -0.09137    0.07296  -1.252 0.210402
    ## type_of_attackRebound               1.77908    0.06860  25.933  < 2e-16
    ## type_of_attackFoot_Cross High       0.24520    0.07433   3.299 0.000970
    ## type_of_attackFoot_Free Kick        0.26081    0.12591   2.071 0.038328
    ## type_of_attackHead_Free Kick        0.07154    0.09840   0.727 0.467237
    ## type_of_attackFoot_Corner          -0.24631    0.13106  -1.879 0.060197
    ## type_of_attackFoot_Cross Low        0.99337    0.05029  19.753  < 2e-16
    ## type_of_attackOther_Cross Low       1.83162    0.47488   3.857 0.000115
    ## type_of_attackOther_Free Kick       0.44532    0.63286   0.704 0.481640
    ## type_of_attackHead_Open Play Pass   0.96367    0.12457   7.736 1.03e-14
    ## type_of_attackOther_Cross High      0.87610    0.40493   2.164 0.030496
    ## type_of_attackHead_Cross Low        1.17056    0.27364   4.278 1.89e-05
    ## type_of_attackOther_Open Play Pass  2.59376    0.47613   5.448 5.11e-08
    ## type_of_attackOther_Corner          0.49661    0.55058   0.902 0.367067
    ##                                       
    ## (Intercept)                        ***
    ## type_of_attackOther not assisted   ***
    ## type_of_attackDirect Free-Kick     ***
    ## type_of_attackHead_Cross High      ***
    ## type_of_attackHead_Corner             
    ## type_of_attackRebound              ***
    ## type_of_attackFoot_Cross High      ***
    ## type_of_attackFoot_Free Kick       *  
    ## type_of_attackHead_Free Kick          
    ## type_of_attackFoot_Corner          .  
    ## type_of_attackFoot_Cross Low       ***
    ## type_of_attackOther_Cross Low      ***
    ## type_of_attackOther_Free Kick         
    ## type_of_attackHead_Open Play Pass  ***
    ## type_of_attackOther_Cross High     *  
    ## type_of_attackHead_Cross Low       ***
    ## type_of_attackOther_Open Play Pass ***
    ## type_of_attackOther_Corner            
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 32235  on 42855  degrees of freedom
    ## Residual deviance: 30886  on 42838  degrees of freedom
    ## AIC: 30922
    ## 
    ## Number of Fisher Scoring iterations: 5

Let's try to group chances after corners to see if we can get one level with competitive p-value.

``` r
chances_train <- chances_train %>% 
  mutate(type_of_attack2 = fct_collapse(type_of_attack, 
                                        'Corner' = c('Foot_Corner', 'Head_Corner', 'Other_Corner')
  ))
summary(glm(goal ~ type_of_attack2, family = 'binomial', data = chances_train)) #+
```

    ## 
    ## Call:
    ## glm(formula = goal ~ type_of_attack2, family = "binomial", data = chances_train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.4132  -0.4910  -0.4910  -0.3852   2.3217  
    ## 
    ## Coefficients:
    ##                                     Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)                         -2.05476    0.02254 -91.150  < 2e-16
    ## type_of_attack2Other not assisted   -0.50918    0.05029 -10.125  < 2e-16
    ## type_of_attack2Direct Free-Kick     -0.57049    0.08724  -6.540 6.17e-11
    ## type_of_attack2Head_Cross High       0.25711    0.05419   4.745 2.09e-06
    ## type_of_attack2Corner               -0.12165    0.06477  -1.878 0.060360
    ## type_of_attack2Rebound               1.77908    0.06860  25.933  < 2e-16
    ## type_of_attack2Foot_Cross High       0.24520    0.07433   3.299 0.000970
    ## type_of_attack2Foot_Free Kick        0.26081    0.12591   2.071 0.038328
    ## type_of_attack2Head_Free Kick        0.07154    0.09840   0.727 0.467237
    ## type_of_attack2Foot_Cross Low        0.99337    0.05029  19.753  < 2e-16
    ## type_of_attack2Other_Cross Low       1.83162    0.47488   3.857 0.000115
    ## type_of_attack2Other_Free Kick       0.44532    0.63286   0.704 0.481640
    ## type_of_attack2Head_Open Play Pass   0.96367    0.12457   7.736 1.03e-14
    ## type_of_attack2Other_Cross High      0.87610    0.40493   2.164 0.030496
    ## type_of_attack2Head_Cross Low        1.17056    0.27364   4.278 1.89e-05
    ## type_of_attack2Other_Open Play Pass  2.59376    0.47613   5.448 5.11e-08
    ##                                        
    ## (Intercept)                         ***
    ## type_of_attack2Other not assisted   ***
    ## type_of_attack2Direct Free-Kick     ***
    ## type_of_attack2Head_Cross High      ***
    ## type_of_attack2Corner               .  
    ## type_of_attack2Rebound              ***
    ## type_of_attack2Foot_Cross High      ***
    ## type_of_attack2Foot_Free Kick       *  
    ## type_of_attack2Head_Free Kick          
    ## type_of_attack2Foot_Cross Low       ***
    ## type_of_attack2Other_Cross Low      ***
    ## type_of_attack2Other_Free Kick         
    ## type_of_attack2Head_Open Play Pass  ***
    ## type_of_attack2Other_Cross High     *  
    ## type_of_attack2Head_Cross Low       ***
    ## type_of_attack2Other_Open Play Pass ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 32235  on 42855  degrees of freedom
    ## Residual deviance: 30888  on 42840  degrees of freedom
    ## AIC: 30920
    ## 
    ## Number of Fisher Scoring iterations: 5

Grouping shots taken after corners resulted in comparable p-value in Wald test, so we keep these chances as one type\_of\_attack in order to slightly simplify a model. What about chances after free-kicks (excl. direct free kicks)?

``` r
chances_train <- chances_train %>% 
  mutate(type_of_attack3 = fct_collapse(type_of_attack2,
                                        'Free Kick' = c('Foot_Free Kick', 'Head_Free Kick', 
                                                        'Other_Free Kick')))
summary(glm(goal ~ type_of_attack3, family = 'binomial', data = chances_train)) #+
```

    ## 
    ## Call:
    ## glm(formula = goal ~ type_of_attack3, family = "binomial", data = chances_train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.4132  -0.4910  -0.4910  -0.3852   2.3217  
    ## 
    ## Coefficients:
    ##                                     Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)                         -2.05476    0.02254 -91.150  < 2e-16
    ## type_of_attack3Other not assisted   -0.50918    0.05029 -10.125  < 2e-16
    ## type_of_attack3Direct Free-Kick     -0.57049    0.08724  -6.540 6.17e-11
    ## type_of_attack3Head_Cross High       0.25711    0.05419   4.745 2.09e-06
    ## type_of_attack3Corner               -0.12165    0.06477  -1.878 0.060360
    ## type_of_attack3Rebound               1.77908    0.06860  25.933  < 2e-16
    ## type_of_attack3Foot_Cross High       0.24520    0.07433   3.299 0.000970
    ## type_of_attack3Free Kick             0.14321    0.07850   1.824 0.068107
    ## type_of_attack3Foot_Cross Low        0.99337    0.05029  19.753  < 2e-16
    ## type_of_attack3Other_Cross Low       1.83162    0.47488   3.857 0.000115
    ## type_of_attack3Head_Open Play Pass   0.96367    0.12457   7.736 1.03e-14
    ## type_of_attack3Other_Cross High      0.87610    0.40493   2.164 0.030496
    ## type_of_attack3Head_Cross Low        1.17056    0.27364   4.278 1.89e-05
    ## type_of_attack3Other_Open Play Pass  2.59376    0.47613   5.448 5.11e-08
    ##                                        
    ## (Intercept)                         ***
    ## type_of_attack3Other not assisted   ***
    ## type_of_attack3Direct Free-Kick     ***
    ## type_of_attack3Head_Cross High      ***
    ## type_of_attack3Corner               .  
    ## type_of_attack3Rebound              ***
    ## type_of_attack3Foot_Cross High      ***
    ## type_of_attack3Free Kick            .  
    ## type_of_attack3Foot_Cross Low       ***
    ## type_of_attack3Other_Cross Low      ***
    ## type_of_attack3Head_Open Play Pass  ***
    ## type_of_attack3Other_Cross High     *  
    ## type_of_attack3Head_Cross Low       ***
    ## type_of_attack3Other_Open Play Pass ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 32235  on 42855  degrees of freedom
    ## Residual deviance: 30890  on 42842  degrees of freedom
    ## AIC: 30918
    ## 
    ## Number of Fisher Scoring iterations: 5

Also grouping chances after free kicks (excluding direct free kicks) seems to be reasonable as it results in simpler model and fairly low p-value of Wald test.

``` r
chances_train <- chances_train %>% 
  mutate(type_of_attack = type_of_attack3) %>% 
  select(-type_of_attack2, -type_of_attack3)

chances_test <- chances_test %>% mutate(type_of_attack = fct_collapse(type_of_attack, 
                                        'Corner' = c('Foot_Corner', 'Head_Corner', 'Other_Corner'),
                                        'Free Kick' = c('Foot_Free Kick', 'Head_Free Kick', 
                                                        'Other_Free Kick')))
```

### Step 2.

In the second step we build a model with all variables selected in step 1. Then we check which covariates are insignificant and if removing them will make a difference.

``` r
model2 <- glm(goal ~ defPressure + numDefPlayers +
              numAttPlayers + dist + angle + game_state + type_of_attack, 
              family = 'binomial', data = chances_train)
summary(model2)
```

    ## 
    ## Call:
    ## glm(formula = goal ~ defPressure + numDefPlayers + numAttPlayers + 
    ##     dist + angle + game_state + type_of_attack, family = "binomial", 
    ##     data = chances_train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.3962  -0.4997  -0.3346  -0.2129   4.0248  
    ## 
    ## Coefficients:
    ##                                     Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)                          3.06104    0.13829  22.135  < 2e-16
    ## defPressure1                        -0.04864    0.05278  -0.922 0.356762
    ## defPressure2                        -0.21426    0.05411  -3.960 7.50e-05
    ## defPressure3                        -0.36343    0.05511  -6.595 4.26e-11
    ## defPressure4                        -0.90041    0.06913 -13.024  < 2e-16
    ## defPressure5                        -1.79213    0.17180 -10.432  < 2e-16
    ## numDefPlayers1                      -1.53527    0.12555 -12.228  < 2e-16
    ## numDefPlayers2                      -2.12854    0.12792 -16.639  < 2e-16
    ## numDefPlayers3                      -2.25854    0.13328 -16.946  < 2e-16
    ## numDefPlayers4                      -2.46762    0.14853 -16.614  < 2e-16
    ## numDefPlayers5                      -2.28681    0.17368 -13.167  < 2e-16
    ## numDefPlayers6                      -2.42253    0.21441 -11.298  < 2e-16
    ## numDefPlayers7                      -2.26076    0.24610  -9.186  < 2e-16
    ## numDefPlayers8                      -2.53569    0.33226  -7.632 2.32e-14
    ## numDefPlayers9                      -2.81152    0.57554  -4.885 1.03e-06
    ## numDefPlayers10                     -2.49248    0.80063  -3.113 0.001851
    ## numDefPlayers11                    -11.41648   83.91391  -0.136 0.891782
    ## numAttPlayers1                      -0.15189    0.05672  -2.678 0.007412
    ## numAttPlayers2                      -0.14744    0.11562  -1.275 0.202223
    ## numAttPlayers3                      -0.03420    0.19111  -0.179 0.857971
    ## numAttPlayers4                      -0.03123    0.37357  -0.084 0.933367
    ## numAttPlayers>4                     -0.02743    0.79250  -0.035 0.972392
    ## dist                               -16.21958    0.41157 -39.409  < 2e-16
    ## angle                               -0.93248    0.07852 -11.875  < 2e-16
    ## game_state                           0.09046    0.04806   1.882 0.059833
    ## type_of_attackOther not assisted    -0.19490    0.05543  -3.516 0.000438
    ## type_of_attackDirect Free-Kick       0.83431    0.13796   6.048 1.47e-09
    ## type_of_attackHead_Cross High       -1.01517    0.06165 -16.466  < 2e-16
    ## type_of_attackCorner                -0.93145    0.07493 -12.431  < 2e-16
    ## type_of_attackRebound                0.42499    0.08167   5.204 1.96e-07
    ## type_of_attackFoot_Cross High       -0.53337    0.08170  -6.529 6.64e-11
    ## type_of_attackFree Kick             -0.81308    0.08651  -9.399  < 2e-16
    ## type_of_attackFoot_Cross Low        -0.17935    0.05967  -3.006 0.002651
    ## type_of_attackOther_Cross Low       -0.38072    0.56493  -0.674 0.500356
    ## type_of_attackHead_Open Play Pass   -0.55912    0.13652  -4.095 4.21e-05
    ## type_of_attackOther_Cross High      -0.79386    0.42933  -1.849 0.064450
    ## type_of_attackHead_Cross Low        -0.28517    0.29517  -0.966 0.333981
    ## type_of_attackOther_Open Play Pass   0.34274    0.54742   0.626 0.531245
    ##                                       
    ## (Intercept)                        ***
    ## defPressure1                          
    ## defPressure2                       ***
    ## defPressure3                       ***
    ## defPressure4                       ***
    ## defPressure5                       ***
    ## numDefPlayers1                     ***
    ## numDefPlayers2                     ***
    ## numDefPlayers3                     ***
    ## numDefPlayers4                     ***
    ## numDefPlayers5                     ***
    ## numDefPlayers6                     ***
    ## numDefPlayers7                     ***
    ## numDefPlayers8                     ***
    ## numDefPlayers9                     ***
    ## numDefPlayers10                    ** 
    ## numDefPlayers11                       
    ## numAttPlayers1                     ** 
    ## numAttPlayers2                        
    ## numAttPlayers3                        
    ## numAttPlayers4                        
    ## numAttPlayers>4                       
    ## dist                               ***
    ## angle                              ***
    ## game_state                         .  
    ## type_of_attackOther not assisted   ***
    ## type_of_attackDirect Free-Kick     ***
    ## type_of_attackHead_Cross High      ***
    ## type_of_attackCorner               ***
    ## type_of_attackRebound              ***
    ## type_of_attackFoot_Cross High      ***
    ## type_of_attackFree Kick            ***
    ## type_of_attackFoot_Cross Low       ** 
    ## type_of_attackOther_Cross Low         
    ## type_of_attackHead_Open Play Pass  ***
    ## type_of_attackOther_Cross High     .  
    ## type_of_attackHead_Cross Low          
    ## type_of_attackOther_Open Play Pass    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 32235  on 42855  degrees of freedom
    ## Residual deviance: 26066  on 42818  degrees of freedom
    ## AIC: 26142
    ## 
    ## Number of Fisher Scoring iterations: 9

numAttPlayers is a variable with only one significant level coefficient. So let's see how removing it will impact the model.

``` r
model2a <- glm(goal ~ defPressure + numDefPlayers + dist + angle +
                 game_state + type_of_attack, family = 'binomial', data = chances_train)
summary(model2a)
```

    ## 
    ## Call:
    ## glm(formula = goal ~ defPressure + numDefPlayers + dist + angle + 
    ##     game_state + type_of_attack, family = "binomial", data = chances_train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.4245  -0.4993  -0.3348  -0.2133   4.0346  
    ## 
    ## Coefficients:
    ##                                     Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)                          3.05361    0.13815  22.104  < 2e-16
    ## defPressure1                        -0.04417    0.05271  -0.838 0.402049
    ## defPressure2                        -0.20734    0.05401  -3.839 0.000124
    ## defPressure3                        -0.35313    0.05495  -6.427 1.30e-10
    ## defPressure4                        -0.88757    0.06895 -12.872  < 2e-16
    ## defPressure5                        -1.77738    0.17172 -10.350  < 2e-16
    ## numDefPlayers1                      -1.53395    0.12540 -12.233  < 2e-16
    ## numDefPlayers2                      -2.13752    0.12772 -16.736  < 2e-16
    ## numDefPlayers3                      -2.29763    0.13243 -17.350  < 2e-16
    ## numDefPlayers4                      -2.54280    0.14592 -17.426  < 2e-16
    ## numDefPlayers5                      -2.38481    0.16791 -14.203  < 2e-16
    ## numDefPlayers6                      -2.51917    0.20405 -12.346  < 2e-16
    ## numDefPlayers7                      -2.35146    0.23032 -10.210  < 2e-16
    ## numDefPlayers8                      -2.61225    0.31382  -8.324  < 2e-16
    ## numDefPlayers9                      -2.86160    0.55344  -5.171 2.33e-07
    ## numDefPlayers10                     -2.54209    0.79021  -3.217 0.001295
    ## numDefPlayers11                    -11.48351   84.10841  -0.137 0.891401
    ## dist                               -16.26719    0.41049 -39.628  < 2e-16
    ## angle                               -0.91953    0.07837 -11.733  < 2e-16
    ## game_state                           0.09004    0.04807   1.873 0.061027
    ## type_of_attackOther not assisted    -0.19753    0.05539  -3.566 0.000362
    ## type_of_attackDirect Free-Kick       0.83761    0.13803   6.068 1.29e-09
    ## type_of_attackHead_Cross High       -1.01624    0.06163 -16.489  < 2e-16
    ## type_of_attackCorner                -0.94830    0.07462 -12.708  < 2e-16
    ## type_of_attackRebound                0.42015    0.08161   5.148 2.63e-07
    ## type_of_attackFoot_Cross High       -0.53544    0.08166  -6.557 5.50e-11
    ## type_of_attackFree Kick             -0.82249    0.08635  -9.525  < 2e-16
    ## type_of_attackFoot_Cross Low        -0.18294    0.05964  -3.068 0.002158
    ## type_of_attackOther_Cross Low       -0.37728    0.56536  -0.667 0.504560
    ## type_of_attackHead_Open Play Pass   -0.56936    0.13640  -4.174 2.99e-05
    ## type_of_attackOther_Cross High      -0.79224    0.42930  -1.845 0.064973
    ## type_of_attackHead_Cross Low        -0.28207    0.29538  -0.955 0.339592
    ## type_of_attackOther_Open Play Pass   0.32748    0.54541   0.600 0.548222
    ##                                       
    ## (Intercept)                        ***
    ## defPressure1                          
    ## defPressure2                       ***
    ## defPressure3                       ***
    ## defPressure4                       ***
    ## defPressure5                       ***
    ## numDefPlayers1                     ***
    ## numDefPlayers2                     ***
    ## numDefPlayers3                     ***
    ## numDefPlayers4                     ***
    ## numDefPlayers5                     ***
    ## numDefPlayers6                     ***
    ## numDefPlayers7                     ***
    ## numDefPlayers8                     ***
    ## numDefPlayers9                     ***
    ## numDefPlayers10                    ** 
    ## numDefPlayers11                       
    ## dist                               ***
    ## angle                              ***
    ## game_state                         .  
    ## type_of_attackOther not assisted   ***
    ## type_of_attackDirect Free-Kick     ***
    ## type_of_attackHead_Cross High      ***
    ## type_of_attackCorner               ***
    ## type_of_attackRebound              ***
    ## type_of_attackFoot_Cross High      ***
    ## type_of_attackFree Kick            ***
    ## type_of_attackFoot_Cross Low       ** 
    ## type_of_attackOther_Cross Low         
    ## type_of_attackHead_Open Play Pass  ***
    ## type_of_attackOther_Cross High     .  
    ## type_of_attackHead_Cross Low          
    ## type_of_attackOther_Open Play Pass    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 32235  on 42855  degrees of freedom
    ## Residual deviance: 26074  on 42823  degrees of freedom
    ## AIC: 26140
    ## 
    ## Number of Fisher Scoring iterations: 9

``` r
lrtest(model2a, model2)
```

    ## 
    ## Model 1: goal ~ defPressure + numDefPlayers + dist + angle + game_state + 
    ##     type_of_attack
    ## Model 2: goal ~ defPressure + numDefPlayers + numAttPlayers + dist + angle + 
    ##     game_state + type_of_attack
    ## 
    ## L.R. Chisq       d.f.          P 
    ##  7.8842919  5.0000000  0.1627289

P-value is much above standard 0.05 value, so I think we can go for simple model.

``` r
model2_fin <- model2a
```

### Step 3.

Let's check how much the coefficients changed after removing numAttPlayers:

``` r
(model2_fin$coefficients - model2$coefficients[names(model2_fin$coefficients)])/
  model2$coefficients[names(model2_fin$coefficients)] * 100
```

    ##                        (Intercept)                       defPressure1 
    ##                        -0.24267194                        -9.18123548 
    ##                       defPressure2                       defPressure3 
    ##                        -3.22898305                        -2.83346639 
    ##                       defPressure4                       defPressure5 
    ##                        -1.42675259                        -0.82325712 
    ##                     numDefPlayers1                     numDefPlayers2 
    ##                        -0.08591229                         0.42213772 
    ##                     numDefPlayers3                     numDefPlayers4 
    ##                         1.73083800                         3.04683513 
    ##                     numDefPlayers5                     numDefPlayers6 
    ##                         4.28540317                         3.98909657 
    ##                     numDefPlayers7                     numDefPlayers8 
    ##                         4.01201502                         3.01943549 
    ##                     numDefPlayers9                    numDefPlayers10 
    ##                         1.78125928                         1.99047586 
    ##                    numDefPlayers11                               dist 
    ##                         0.58715008                         0.29351637 
    ##                              angle                         game_state 
    ##                        -1.38863794                        -0.46012350 
    ##   type_of_attackOther not assisted     type_of_attackDirect Free-Kick 
    ##                         1.34934187                         0.39494568 
    ##      type_of_attackHead_Cross High               type_of_attackCorner 
    ##                         0.10470430                         1.80921444 
    ##              type_of_attackRebound      type_of_attackFoot_Cross High 
    ##                        -1.13963880                         0.38848956 
    ##            type_of_attackFree Kick       type_of_attackFoot_Cross Low 
    ##                         1.15727354                         2.00110759 
    ##      type_of_attackOther_Cross Low  type_of_attackHead_Open Play Pass 
    ##                        -0.90303524                         1.83133187 
    ##     type_of_attackOther_Cross High       type_of_attackHead_Cross Low 
    ##                        -0.20374590                        -1.08451085 
    ## type_of_attackOther_Open Play Pass 
    ##                        -4.45420507

All the percentage changes are below 10%. Therefore I don't include back `numAttPlayers`.

``` r
model3_fin <- model2_fin
```

### Step 4.

Now it is a time when we should check if continuous variables (dist, angle) are linear in the logit. To do so, we will use a loess function which fits a linear regression (Y~X) line locally, to the data around X = x.

``` r
logit_loess <- function(x, y, span){
  
  logit <- function(p) log(p/(1-p))

  loess_fit <- predict(loess(y ~ x, span = span))
  pi <- pmax(pmin(loess_fit,0.9999),0.0001)
  logit_fitted <- logit(pi)
  
  plt <- ggplot() + geom_point(aes(x, logit_fitted)) + scale_y_continuous(name = 'log-odds')
  return(plt)
}

logit_loess(chances_train$dist, chances_train$goal, span = 0.1)
```

<img src="expected_goals_model_files/figure-markdown_github/unnamed-chunk-45-1.png" style="display: block; margin: auto;" />

``` r
logit_loess(chances_train$dist, chances_train$goal, span = 0.2)
```

<img src="expected_goals_model_files/figure-markdown_github/unnamed-chunk-45-2.png" style="display: block; margin: auto;" />

``` r
logit_loess(chances_train$dist, chances_train$goal, span = 0.4)
```

<img src="expected_goals_model_files/figure-markdown_github/unnamed-chunk-45-3.png" style="display: block; margin: auto;" />

Span of 0.4 seems to be far too large as it is over a half of whole dist range (which is approx. 0.6). Span of 0.1 makes the loess plot quite erratic. So it seems reasonable to go for span 0.2 here as it provides adequate smoothness. The plot is quite linear in the middle, but not in the tails. I think especially left tail is crucial here, as the non-linear trend begins when the observations are still close-ranked. If not linear, what's the relationship then? Here restricted cubic splines comes in help.

We test splines with 3, 4, 5 knots as according to literature these are sufficient in most of cases.

``` r
spline_dist3 <- glm(goal ~ rcs(dist, parms = 3), family = 'binomial', data = chances_train)
spline_dist4 <- glm(goal ~ rcs(dist, parms = 4), family = 'binomial', data = chances_train)
spline_dist5 <- glm(goal ~ rcs(dist, parms = 5), family = 'binomial', data = chances_train)
```

``` r
summary(spline_dist3)$aic
```

    ## [1] 28211.17

``` r
summary(spline_dist4)$aic
```

    ## [1] 28179.45

``` r
summary(spline_dist5)$aic
```

    ## [1] 28072.46

Spline with the lowest AIC is the one with 5 knots.

We'll apply similar procedure to `angle`.

``` r
logit_loess(chances_train$angle, chances_train$goal, span = 0.1)
```

<img src="expected_goals_model_files/figure-markdown_github/unnamed-chunk-48-1.png" style="display: block; margin: auto;" />

``` r
logit_loess(chances_train$angle, chances_train$goal, span = 0.2)
```

<img src="expected_goals_model_files/figure-markdown_github/unnamed-chunk-48-2.png" style="display: block; margin: auto;" />

``` r
logit_loess(chances_train$angle, chances_train$goal, span = 0.4)
```

<img src="expected_goals_model_files/figure-markdown_github/unnamed-chunk-48-3.png" style="display: block; margin: auto;" />

Well, this is quite complex, even with the span of 0.4 so we'll try splines with more knots.

``` r
spline_angle3 <- glm(goal ~ rcs(angle, parms = 3), family = 'binomial', data = chances_train)
spline_angle4 <- glm(goal ~ rcs(angle, parms = 4), family = 'binomial', data = chances_train)
spline_angle5 <- glm(goal ~ rcs(angle, parms = 5), family = 'binomial', data = chances_train)
spline_angle6 <- glm(goal ~ rcs(angle, parms = 6), family = 'binomial', data = chances_train)
spline_angle7 <- glm(goal ~ rcs(angle, parms = 7), family = 'binomial', data = chances_train)
```

``` r
summary(spline_angle3)$aic
```

    ## [1] 32237.25

``` r
summary(spline_angle4)$aic
```

    ## [1] 32225.03

``` r
summary(spline_angle5)$aic
```

    ## [1] 32222.03

``` r
summary(spline_angle6)$aic
```

    ## [1] 32221.75

``` r
summary(spline_angle7)$aic
```

    ## [1] 32222.6

This time we pick the spline with 6 knots as it results in lowest AIC.

So let's replace `dist` and `angle` variables with the fitted splines.

``` r
model4 <- glm(goal ~ defPressure + numDefPlayers + rcs(dist, parms = 5)  + 
                      rcs(angle, parms = 6) + game_state + type_of_attack, 
              family = 'binomial', data = chances_train)
summary(model4)
```

    ## 
    ## Call:
    ## glm(formula = goal ~ defPressure + numDefPlayers + rcs(dist, 
    ##     parms = 5) + rcs(angle, parms = 6) + game_state + type_of_attack, 
    ##     family = "binomial", data = chances_train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.5248  -0.4866  -0.3307  -0.2359   3.2196  
    ## 
    ## Coefficients:
    ##                                      Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)                           3.58565    0.17168  20.885  < 2e-16
    ## defPressure1                         -0.03553    0.05307  -0.669 0.503180
    ## defPressure2                         -0.19527    0.05445  -3.586 0.000335
    ## defPressure3                         -0.34048    0.05548  -6.137 8.40e-10
    ## defPressure4                         -0.88880    0.06973 -12.746  < 2e-16
    ## defPressure5                         -1.81254    0.17361 -10.440  < 2e-16
    ## numDefPlayers1                       -1.39323    0.12983 -10.731  < 2e-16
    ## numDefPlayers2                       -1.96006    0.13226 -14.819  < 2e-16
    ## numDefPlayers3                       -2.12341    0.13668 -15.536  < 2e-16
    ## numDefPlayers4                       -2.37614    0.14955 -15.889  < 2e-16
    ## numDefPlayers5                       -2.19606    0.17038 -12.889  < 2e-16
    ## numDefPlayers6                       -2.26295    0.20458 -11.061  < 2e-16
    ## numDefPlayers7                       -2.02707    0.23036  -8.800  < 2e-16
    ## numDefPlayers8                       -2.23324    0.31374  -7.118 1.09e-12
    ## numDefPlayers9                       -2.42996    0.55243  -4.399 1.09e-05
    ## numDefPlayers10                      -2.08813    0.78935  -2.645 0.008160
    ## numDefPlayers11                     -10.96693   84.17185  -0.130 0.896335
    ## rcs(dist, parms = 5)dist            -26.54627    1.26800 -20.936  < 2e-16
    ## rcs(dist, parms = 5)dist'            62.98115   10.42205   6.043 1.51e-09
    ## rcs(dist, parms = 5)dist''         -195.40402   35.52501  -5.500 3.79e-08
    ## rcs(dist, parms = 5)dist'''         220.47235   40.66846   5.421 5.92e-08
    ## rcs(angle, parms = 6)angle            0.09218    1.02998   0.089 0.928687
    ## rcs(angle, parms = 6)angle'          -4.81296   16.98219  -0.283 0.776861
    ## rcs(angle, parms = 6)angle''          8.82606   45.43167   0.194 0.845964
    ## rcs(angle, parms = 6)angle'''        -6.38575   49.73361  -0.128 0.897833
    ## rcs(angle, parms = 6)angle''''        3.09055   32.85366   0.094 0.925053
    ## game_state                            0.09280    0.04841   1.917 0.055269
    ## type_of_attackOther not assisted     -0.23288    0.05580  -4.174 3.00e-05
    ## type_of_attackDirect Free-Kick        0.59086    0.13631   4.335 1.46e-05
    ## type_of_attackHead_Cross High        -1.07878    0.06380 -16.910  < 2e-16
    ## type_of_attackCorner                 -1.04571    0.07696 -13.589  < 2e-16
    ## type_of_attackRebound                 0.32209    0.08428   3.822 0.000132
    ## type_of_attackFoot_Cross High        -0.56546    0.08331  -6.787 1.15e-11
    ## type_of_attackFree Kick              -0.87391    0.08813  -9.916  < 2e-16
    ## type_of_attackFoot_Cross Low         -0.26015    0.06166  -4.219 2.45e-05
    ## type_of_attackOther_Cross Low        -0.60143    0.57638  -1.043 0.296734
    ## type_of_attackHead_Open Play Pass    -0.73634    0.14049  -5.241 1.60e-07
    ## type_of_attackOther_Cross High       -0.94918    0.43593  -2.177 0.029452
    ## type_of_attackHead_Cross Low         -0.37982    0.30224  -1.257 0.208867
    ## type_of_attackOther_Open Play Pass    0.07701    0.55982   0.138 0.890588
    ##                                       
    ## (Intercept)                        ***
    ## defPressure1                          
    ## defPressure2                       ***
    ## defPressure3                       ***
    ## defPressure4                       ***
    ## defPressure5                       ***
    ## numDefPlayers1                     ***
    ## numDefPlayers2                     ***
    ## numDefPlayers3                     ***
    ## numDefPlayers4                     ***
    ## numDefPlayers5                     ***
    ## numDefPlayers6                     ***
    ## numDefPlayers7                     ***
    ## numDefPlayers8                     ***
    ## numDefPlayers9                     ***
    ## numDefPlayers10                    ** 
    ## numDefPlayers11                       
    ## rcs(dist, parms = 5)dist           ***
    ## rcs(dist, parms = 5)dist'          ***
    ## rcs(dist, parms = 5)dist''         ***
    ## rcs(dist, parms = 5)dist'''        ***
    ## rcs(angle, parms = 6)angle            
    ## rcs(angle, parms = 6)angle'           
    ## rcs(angle, parms = 6)angle''          
    ## rcs(angle, parms = 6)angle'''         
    ## rcs(angle, parms = 6)angle''''        
    ## game_state                         .  
    ## type_of_attackOther not assisted   ***
    ## type_of_attackDirect Free-Kick     ***
    ## type_of_attackHead_Cross High      ***
    ## type_of_attackCorner               ***
    ## type_of_attackRebound              ***
    ## type_of_attackFoot_Cross High      ***
    ## type_of_attackFree Kick            ***
    ## type_of_attackFoot_Cross Low       ***
    ## type_of_attackOther_Cross Low         
    ## type_of_attackHead_Open Play Pass  ***
    ## type_of_attackOther_Cross High     *  
    ## type_of_attackHead_Cross Low          
    ## type_of_attackOther_Open Play Pass    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 32235  on 42855  degrees of freedom
    ## Residual deviance: 25927  on 42816  degrees of freedom
    ## AIC: 26007
    ## 
    ## Number of Fisher Scoring iterations: 9

All coefficients associated with angle spline are statistically insignificant. But what if we decide to remove this covariate?

``` r
lrtest(model4a, model4)
```

    ## 
    ## Model 1: goal ~ defPressure + numDefPlayers + rcs(dist, parms = 5) + game_state + 
    ##     type_of_attack
    ## Model 2: goal ~ defPressure + numDefPlayers + rcs(dist, parms = 5) + rcs(angle, 
    ##     parms = 6) + game_state + type_of_attack
    ## 
    ## L.R. Chisq       d.f.          P 
    ##   138.7636     5.0000     0.0000

``` r
(model4a$coefficients - model4$coefficients[names(model4a$coefficients)])/
  model4$coefficients[names(model4a$coefficients)] * 100
```

    ##                        (Intercept)                       defPressure1 
    ##                         -8.9105339                          1.2553446 
    ##                       defPressure2                       defPressure3 
    ##                         -1.1351860                          0.9464652 
    ##                       defPressure4                       defPressure5 
    ##                          0.5761415                          0.1386064 
    ##                     numDefPlayers1                     numDefPlayers2 
    ##                         -6.6072892                         -5.5601394 
    ##                     numDefPlayers3                     numDefPlayers4 
    ##                         -7.4893524                         -7.3441900 
    ##                     numDefPlayers5                     numDefPlayers6 
    ##                         -9.1729618                        -10.2596984 
    ##                     numDefPlayers7                     numDefPlayers8 
    ##                        -12.9377905                        -12.9929508 
    ##                     numDefPlayers9                    numDefPlayers10 
    ##                        -13.0697908                        -16.7470020 
    ##                    numDefPlayers11           rcs(dist, parms = 5)dist 
    ##                         -3.6488812                         -1.8699211 
    ##          rcs(dist, parms = 5)dist'         rcs(dist, parms = 5)dist'' 
    ##                        -18.5681812                        -25.1974292 
    ##        rcs(dist, parms = 5)dist'''                         game_state 
    ##                        -28.3725250                          1.2197759 
    ##   type_of_attackOther not assisted     type_of_attackDirect Free-Kick 
    ##                          2.6736951                        -13.4989886 
    ##      type_of_attackHead_Cross High               type_of_attackCorner 
    ##                         -7.4586307                         -2.9291296 
    ##              type_of_attackRebound      type_of_attackFoot_Cross High 
    ##                         -2.9798136                          2.0969320 
    ##            type_of_attackFree Kick       type_of_attackFoot_Cross Low 
    ##                         -6.8178554                        -18.5233300 
    ##      type_of_attackOther_Cross Low  type_of_attackHead_Open Play Pass 
    ##                         -6.8740132                         -4.3118933 
    ##     type_of_attackOther_Cross High       type_of_attackHead_Cross Low 
    ##                         -9.1587231                        -10.4228854 
    ## type_of_attackOther_Open Play Pass 
    ##                         -6.9008265

Likelihood ratio test suggests that's clearly not a good idea. Also percentage changes increased and now we can observe a few witch a change varying from 10% to 30% implying that an angle spline provides a needed adjustments to other covariates. So we should probably leave it in a model.

Finally let's test the model performance.

``` r
ROC(form = goal ~ defPressure + numDefPlayers + rcs(dist, parms = 5)  + 
                  rcs(angle, parms = 6) + game_state + type_of_attack, 
    family = 'binomial', data = chances_test, plot = 'ROC')
```

![](expected_goals_model_files/figure-markdown_github/unnamed-chunk-53-1.png)

AUC = 79% is really high. I had been quite sceptic at the beginning, so I altered test sets and the AUC didn't change much. Also McFadden ratio with the score of 0.2 is quite satisfying.

``` r
1 - logLik(model4)/logLik(glm(goal~1, data = chances_train, family = 'binomial'))
```

    ## 'log Lik.' 0.1956968 (df=40)

Although classifying the `goal` variable isn't out objective here, let's make use of it and calculate RMSE.

``` r
chances_test <- chances_test %>% 
  mutate(xG = predict(model4, type = 'response', newdata = chances_test))

rmse <- function(x, y) sqrt(mean((x - y)^2))
rmse_all <- map(1:100, ~ chances_test %>% 
                  transmute(!! paste0("pred", .x) := as.numeric(xG > 0.01 * .x))) %>% 
  bind_cols(.) %>% map_dbl(rmse, y = chances_test$goal)
ggplot() + geom_line(aes(seq(0.01, 1, by = 0.01), rmse_all))
```

![](expected_goals_model_files/figure-markdown_github/unnamed-chunk-55-1.png)

``` r
rmse_all[which.min(rmse_all)]
```

    ##    pred65 
    ## 0.3292909

We get the lowest test error while cutting off on 0.65, however the plot implies that other cut-off level, as long as greater than 0.4) would give similar error. 

*pitch\_plot is a slightly modified version of a function shared by @FC\_rstats.*

**This article was written with the aid of StrataData, which is property of [Stratagem Technologies](www.stratagem.co). StrataData powers the [StrataBet Sports Trading Platform](https://app.stratabet.com), in addition to [StrataBet Premium Recommendations](https://stratatips.co).**
