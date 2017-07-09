Aggressiveness
================
Andrew Lim
2017-07-08

``` r
suppressMessages(library(tidyverse))
```

``` r
data <- suppressMessages(read_csv("../data/AllActionsperMatchUSAMajorLeagueSoccer2016.csv"))
match_data <- suppressMessages(read_csv("../data/TeamDataMLS2016.csv"))
```

    ## Warning: Duplicated column names deduplicated: 'goals' => 'goals_1' [49]

``` r
match_data <- match_data %>%
  rename(match_result = Result)
```

``` r
# Joining the two tables:
data <- data %>% 
  left_join(match_data, by=c("Match" = "Matchid", `Team in possession` = "Team"))
```

``` r
aggressive_df <- data %>% 
  mutate(next_team = lead(team)) %>% 
  mutate(turn_over = team != next_team) %>% 
  mutate(aggressive_turnover = LocX > 50 & turn_over == TRUE) %>% 
  group_by(Match, `Team in possession`) %>% 
  summarize(num_aggressive_turnovers = sum(aggressive_turnover),
            match_result = unique(match_result),
            HomeAway = unique(HomeAway)) %>% 
  mutate(is_home = ifelse(HomeAway == "T", TRUE, FALSE)) %>% 
  mutate(is_win = match_result == "W")
```

``` r
# Teams tend to play more aggressive when they are at home:
model <- glm(num_aggressive_turnovers ~ is_home, data=aggressive_df)
summary(model)
```

    ## 
    ## Call:
    ## glm(formula = num_aggressive_turnovers ~ is_home, data = aggressive_df)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -57.147  -14.538   -1.538   13.462   76.462  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  130.147      1.102 118.126  < 2e-16 ***
    ## is_homeTRUE   10.391      1.558   6.669 5.35e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for gaussian family taken to be 412.7185)
    ## 
    ##     Null deviance: 298179  on 679  degrees of freedom
    ## Residual deviance: 279823  on 678  degrees of freedom
    ## AIC: 6029.2
    ## 
    ## Number of Fisher Scoring iterations: 2

``` r
# Teams should be more aggressive when they are away in order to best improve their chances of winning:
model2 <- glm(is_win ~ num_aggressive_turnovers + is_home + is_home*num_aggressive_turnovers, data=aggressive_df)
summary(model2)
```

    ## 
    ## Call:
    ## glm(formula = is_win ~ num_aggressive_turnovers + is_home + is_home * 
    ##     num_aggressive_turnovers, data = aggressive_df)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.7817  -0.2872  -0.1567   0.4177   0.9751  
    ## 
    ## Coefficients:
    ##                                       Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)                           0.651766   0.152234   4.281 2.13e-05
    ## num_aggressive_turnovers             -0.003562   0.001156  -3.082  0.00214
    ## is_homeTRUE                           0.883470   0.225062   3.925 9.54e-05
    ## num_aggressive_turnovers:is_homeTRUE -0.003826   0.001643  -2.329  0.02016
    ##                                         
    ## (Intercept)                          ***
    ## num_aggressive_turnovers             ** 
    ## is_homeTRUE                          ***
    ## num_aggressive_turnovers:is_homeTRUE *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for gaussian family taken to be 0.1887578)
    ## 
    ##     Null deviance: 153.16  on 679  degrees of freedom
    ## Residual deviance: 127.60  on 676  degrees of freedom
    ## AIC: 801.99
    ## 
    ## Number of Fisher Scoring iterations: 2

``` r
model3 <- glm(is_win ~ num_aggressive_turnovers + is_home, data=aggressive_df)
summary(model3)
```

    ## 
    ## Call:
    ## glm(formula = is_win ~ num_aggressive_turnovers + is_home, data = aggressive_df)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.7073  -0.3418  -0.1454   0.4400   1.0619  
    ## 
    ## Coefficients:
    ##                           Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               0.898159   0.109815   8.179 1.42e-15 ***
    ## num_aggressive_turnovers -0.005455   0.000824  -6.620 7.32e-11 ***
    ## is_homeTRUE               0.365505   0.034509  10.591  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for gaussian family taken to be 0.1899911)
    ## 
    ##     Null deviance: 153.16  on 679  degrees of freedom
    ## Residual deviance: 128.62  on 677  degrees of freedom
    ## AIC: 805.42
    ## 
    ## Number of Fisher Scoring iterations: 2
