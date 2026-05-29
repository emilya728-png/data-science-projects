Predicting Win Rates of Blood on the Clocktower Characters
================
Emily Young and Jonah Feinstein

    ## Installing package into '/cloud/lib/x86_64-pc-linux-gnu-library/4.6'
    ## (as 'lib' is unspecified)
    ## Installing package into '/cloud/lib/x86_64-pc-linux-gnu-library/4.6'
    ## (as 'lib' is unspecified)
    ## Installing package into '/cloud/lib/x86_64-pc-linux-gnu-library/4.6'
    ## (as 'lib' is unspecified)

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.2.1     ✔ readr     2.2.0
    ## ✔ forcats   1.0.1     ✔ stringr   1.6.0
    ## ✔ ggplot2   4.0.3     ✔ tibble    3.3.1
    ## ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
    ## ✔ purrr     1.2.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
    ## ── Attaching packages ────────────────────────────────────── tidymodels 1.5.0 ──
    ## 
    ## ✔ broom        1.0.13     ✔ rsample      1.3.2 
    ## ✔ dials        1.4.3      ✔ tailor       0.1.0 
    ## ✔ infer        1.1.0      ✔ tune         2.1.0 
    ## ✔ modeldata    1.5.1      ✔ workflows    1.3.0 
    ## ✔ parsnip      1.6.0      ✔ workflowsets 1.1.1 
    ## ✔ recipes      1.3.2      ✔ yardstick    1.4.0 
    ## 
    ## ── Conflicts ───────────────────────────────────────── tidymodels_conflicts() ──
    ## ✖ scales::discard() masks purrr::discard()
    ## ✖ dplyr::filter()   masks stats::filter()
    ## ✖ recipes::fixed()  masks stringr::fixed()
    ## ✖ dplyr::lag()      masks stats::lag()
    ## ✖ yardstick::spec() masks readr::spec()
    ## ✖ recipes::step()   masks stats::step()
    ## 
    ## Loading required package: rpart
    ## 
    ## 
    ## Attaching package: 'rpart'
    ## 
    ## 
    ## The following object is masked from 'package:dials':
    ## 
    ##     prune

``` r
data_all <- read.csv("data201full (1).csv")
```

``` r
scripts <- read.csv("Data 201 Final Project Dataset 2 - Sheet2.csv")
```

``` r
data_all <- data_all |>
  select(-(X:impossible.to.hide..0.2.))
```

``` r
loudness <- read.csv("Loudness - Sheet1 (1).csv")
data_all <- full_join(loudness, data_all, by= c("Character" = "Character.y"))
```

``` r
data_all <- full_join(data_all, scripts, by = "Character")
```

``` r
data_all <- data_all[-47, ]
```

## Introduction

We are doing our final project on the game Blood on the Clocktower, a
social deduction game in which there is a good team and an evil team.
Each player has their own unique character with its own unique ability.
Our research question is: What characteristics of clocktower characters
predict win-rate? We chose to do this project because we both love to
play Blood on the Clocktower. The game has lots of characters with
abilities that vary greatly. Despite differences in what characters do,
the relative power levels between characters is often unclear. We want
to investigate if certain types of abilities win more often than others.
No one has done this kind of analysis for Blood on the Clocktower, but
they have for other games with many unique characters. For example, one
paper created a model to predict the price of Magic: The Gathering cards
using features of the cards as attributes with natural language
processing techniques on the texts (Fink et al, 2015). Another paper
discusses how the authors built a model to predict the winrates of
Heartstone decks. The authors constructed an ensemble model using a
combination of two logistic regression models and a neural network to
predict whether each deck would win or lose (Vu et al, 2018). Our data
comes from Clocktracker, an app where clocktower players log what
characters they’ve played and whether or not they won the game. We will
also create our own dataset with each character’s characteristics.

## Data

The dimensions of our dataset is 156 rows and 39 columns. Each row
represents a character in Blood on the Clocktower. Most of the variables
represent a piece of information about each character. Some categorical
variables represent actions taken by the character, such as whether it
wakes in the night, selects a player, or learns info. For example, a
character can be required to select a player (Required), have the option
to select a player (Optional), not select a player at all (No), or might
depending on the state of the game (?). Each character is sorted into
one category for each of these variables. We also have many binary
variables, each representing a specific characteristic like ‘learns
number’. Characters who get their information in the form of a number
are categorized as a ‘1’ for this variable, with other characters
categorized as ‘0’. We also have variables to denote the win rate of
each character and the number of recorded games for each character. The
script variable represents which of the three original scripts (a list
of possible characters in play for one game) the character belongs to,
or if it was released after those three scripts. Another variable is
‘Loudness’. The good team does not know which evil characters are in
play (or who the evil players are) in a given game. However, there are
some evil characters which are more obvious when they are in play. For
example, one character is the Psychopath, which reads, “Each day, you
may publicly choose a player: they die.” This character is relatively
loud, as it forces the player to reveal themselves and reveal that the
character is in play. However, because it is not required that they
choose a player, they can ‘hide’ the character if they so choose.

## Analytic Framework

Our response variable is win rate. The win rate is a continuous variable
from 1-100, and represents the percentage of the recorded games the
character has won. Below is a histogram to visualize the distribution of
the win rates. The win rate follows a roughly normal distribution, with
a median win rate of around 55%.

``` r
ggplot(data = data_all, mapping = aes(x = win_rate_pct)) + 
  geom_histogram(fill = "white",  color = "black", bins = 15)
```

![](Data-201-Final-Project_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->
Most of our predictor variables are made up of different characteristics
of Clocktower characters, and are all categorical. Wakes in the Night,
Selects Player, and Learns Info all have multiple categories. The rest
of the characteristics are binary (see table below for each binary
variable and how many characters were put in as ‘1’ for that character).

``` r
data_all %>% summarize(
  prevents_death = sum(Prevents.Death),
  on_death = sum(On.Death),
  causes_death = sum(Causes.Death),
  execution = sum(Execution),
  droisoning = sum(Droisoning),
  learns_character = sum(Learns.Character),
  specific_character = sum(Specific.Character),
  selects_character = sum(Selects.Character),
  yes_no = sum(Yes.No),
  learns_number = sum(Learns.Number),
  public = sum(Public),
  noms_voting = sum(Nom.Voting),
  once_first = sum(Once.First.Time),
  setup = sum(Setup), 
  madness = sum(Madness),
  alignment = sum(Alignment),
  win_loss = sum(Win.Loss),
  positioning = sum(Positioning),
  can_revive = sum(Can.Revive),
  changes_character = sum(Changes.Character),
  storyteller = sum(Storyteller)
)
```

    ##   prevents_death on_death causes_death execution droisoning learns_character
    ## 1             18       20           38        25         23               14
    ##   specific_character selects_character yes_no learns_number public noms_voting
    ## 1                 10                10      5             9     22          23
    ##   once_first setup madness alignment win_loss positioning can_revive
    ## 1         26    18       4        41       16          12          3
    ##   changes_character storyteller
    ## 1                12          10

Loudness was a continuous variable on a scale of 0 to 6 with a mean of
1.76 and a slight right skew (see histogram below).

``` r
data_all |>
  ggplot(aes(Loudness))+
  geom_histogram()
```

    ## `stat_bin()` using `bins = 30`. Pick better value `binwidth`.

    ## Warning: Removed 110 rows containing non-finite outside the scale range
    ## (`stat_bin()`).

![](Data-201-Final-Project_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->
Other predictor variables included games played, script, and character
type. Games played is a continuous variable ranging from 12 to 3,473
with a mean of 501.8 and a right skew (see histogram below). Script is a
categorical with four categories: Trouble Brewing, Sects and Violets,
Bad Moon Rising, and Experimental. The first three are the original
‘base’ scripts that many characters originated on, while the fourth is
new characters that were released after the originals. There are about
twenty - twenty-five characters of each base script, leaving the other
half of the dataset to the Experimental characters. Finally, there are
four ‘types’ of characters in Clocktower: on the good team, Townsfolk
and Outsiders, and on the evil team, Minions and Demons.

``` r
data_all |>
  ggplot(aes(games_with_result)) +
  geom_histogram()
```

    ## `stat_bin()` using `bins = 30`. Pick better value `binwidth`.

![](Data-201-Final-Project_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->
To analyze the relationship between win rate and our predictor
variables, we use cluster analysis, linear regression, and decision
trees. We used clustering as an exploratory analysis to examine the
relationship between our response variable and the other continuous
variables, games played and loudness. This helped us decide which of
these continuous variables to include in our regression models. We
create two linear regression models. One of the models looks at the
relationship between win rate and loudness, while the other looks at win
rate vs all of the categorical and binary variables. Our regression
models allow us to easily identify which of our explanatory variables
are significant predictors of win rate. We created two decision trees to
model our categorical variables. Our decision trees help conceptualize
our categorical variables, which are harder to represent using linear
regression since they are not numerical.

## Results

We looked at the relationship between win rate and loudness because
making evil characters louder is usually the drawback to a powerful
ability. We first conducted a cluster analysis with evil characters
using win rate and loudness. The method we used was k-means clustering,
with k set to 2. At k = 3 and 4, the additional clusters split up
cluster 2 by win rate, which we did not feel was helpful to our
analysis.

``` r
loudness_wr <- data_all %>% select(Loudness, win_rate_pct)
loudness_wr_scaled <- loudness_wr %>% mutate(
  scaled_loudness = scale(Loudness),
  scaled_wr = scale(win_rate_pct)
) %>% select(
  scaled_loudness,
  scaled_wr
) %>% na.omit()
```

``` r
set.seed(18)
km_loudness <- kmeans(loudness_wr_scaled, 2, nstart = 20)
loudness_wr_clean <- na.omit(loudness_wr)
loudness_clust_2 <- augment(km_loudness, loudness_wr_clean)
ggplot(data = loudness_clust_2, mapping = aes(x = Loudness, y = win_rate_pct, color = .cluster)) + 
  geom_point()
```

![](Data-201-Final-Project_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->
The clusters reveal a difference in win rate for different values of
loudness. Louder evil characters tend to have lower win rates than quiet
ones. In a regression on win rate vs loudness, the intercept was about
56.8, meaning that the model predicts characters with a loudness value
of 0 to have a win rate of 56.8%. The coefficient for loudness was
-1.48, meaning that each increase in loudness results in a decrease in
win rate by 1.48%. The p-value for the loudness coefficient was about
0.02, indicating that loudness is a significant predictor of win rate.
We also ran a regression treating loudness as a categorical variable
with 7 levels ranging from extremely quiet to extremely loud due to the
bands seen in the cluster analysis. In this model, the only significant
category was extremely loud characters with a slope of -15.7 and a p
value of .02. However ,there is only one observation in the extremely
loud category, which is not enough to conclude that extremely loud
characters generally have lower win rates. We also ran a full regression
with every categorical and binary variable. Only the binary variables
Win/Loss Condition and Can Revive were significant or marginally
significant in the full model and in their own separate models. Win/Loss
Condition has a positive relationship with win rate- however, this is
largely due to the Atheist role, which has the highest win rate and has
a Win/Loss condition. When the Atheist is filtered out, Win/Loss
Condition is no longer a significant predictor of win rate. Can Revive
has a negative relationship with win rate, but this is also due to the
lowest-winning character, the Al-Hadikhia. Without the Al-Hadikhia, Can
Revive is also no longer significant (see table for regression results).

``` r
full_model <- linear_reg()|>
  set_engine("lm")|>
  fit(win_rate_pct ~ Learns.Numberf + Yes.Nof + Selects.Characterf + Specific.Characterf + Learns.Characterf + Droisoningf + Executionf + Causes.Deathf + On.Deathf + Prevents.Deathf + Storytellerf + Publicf + Nom.Votingf + Once.First.Timef + Setupf + Madnessf + Alignmentf + Win.Lossf + Changes.Characterf + Can.Revivef + Positioningf, data = data_all)
tidy(full_model)
```

    ## # A tibble: 22 × 5
    ##    term                estimate std.error statistic  p.value
    ##    <chr>                  <dbl>     <dbl>     <dbl>    <dbl>
    ##  1 (Intercept)           55.3        1.11   49.9    1.84e-88
    ##  2 Learns.Numberf         1.89       2.20    0.861  3.91e- 1
    ##  3 Yes.Nof                1.78       2.82    0.633  5.28e- 1
    ##  4 Selects.Characterf     1.11       2.15    0.515  6.07e- 1
    ##  5 Specific.Characterf   -0.167      2.19   -0.0763 9.39e- 1
    ##  6 Learns.Characterf      2.18       1.83    1.20   2.34e- 1
    ##  7 Droisoningf           -0.804      1.44   -0.557  5.79e- 1
    ##  8 Executionf             1.05       1.51    0.695  4.88e- 1
    ##  9 Causes.Deathf         -0.429      1.20   -0.358  7.21e- 1
    ## 10 On.Deathf             -0.592      1.56   -0.381  7.04e- 1
    ## # ℹ 12 more rows

``` r
revive_model <- linear_reg()|>
  set_engine("lm")|>
  fit(win_rate_pct ~ Can.Revivef, data = data_all)
tidy(revive_model)
```

    ## # A tibble: 2 × 5
    ##   term        estimate std.error statistic   p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)    56.8      0.480    118.   4.49e-153
    ## 2 Can.Revivef    -8.52     3.46      -2.46 1.50e-  2

``` r
win_model <- linear_reg()|>
  set_engine("lm")|>
  fit(win_rate_pct ~ Win.Lossf, data = data_all)
tidy(win_model)
```

    ## # A tibble: 2 × 5
    ##   term        estimate std.error statistic   p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)    56.2      0.499    113.   7.32e-150
    ## 2 Win.Lossf       4.47     1.56       2.87 4.64e-  3

``` r
data_noatheist <- data_all |>
  filter(Character != "Atheist")
win_model2 <- linear_reg()|>
  set_engine("lm")|>
  fit(win_rate_pct ~ Win.Lossf, data = data_noatheist)
tidy(win_model2)
```

    ## # A tibble: 2 × 5
    ##   term        estimate std.error statistic   p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)    56.2      0.468    120.   2.41e-153
    ## 2 Win.Lossf       2.79     1.50       1.86 6.51e-  2

``` r
data_noalhad <- data_all |>
  filter(Character != "Al-Hadikhia")

revive_model2 <- linear_reg()|>
  set_engine("lm")|>
  fit(win_rate_pct ~ Can.Revivef, data = data_noalhad)
tidy(revive_model2)
```

    ## # A tibble: 2 × 5
    ##   term        estimate std.error statistic   p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)    56.8      0.474   120.    3.00e-153
    ## 2 Can.Revivef    -2.92     4.17     -0.701 4.84e-  1

We did not keep any of the binary variables in future analyses. However,
we felt that a decision tree might be a better way to represent the
multi-level categorical variables, so we created a decision tree model
with the three variables Win Rate, Selects Player, and Learns Info (see
below). Overall, there is not a lot of variation within this decision
tree. The model performed with a mean absolute error of 3.77% on the
training set, while a normal regression with these three variables had
an MAE of 4.14%. The testing set for the decision tree performed at
4.81%.

``` r
set.seed(18)
data_split <- initial_split(data_all, prop = .6)
data_train <- training(data_split)
data_test <- testing(data_split)
```

``` r
tree_cats_model <- decision_tree() |>
  set_engine("rpart") |>
  set_mode("regression") |>
  fit(win_rate_pct ~ Wakes.in.the.Night + Selects.Player+ Learns.Info, data = data_train)
```

``` r
resid_tree_cats <- tree_cats_model |>
augment(new_data = data_train)
resid_tree_cats |>
  mae(.pred, win_rate_pct)
```

    ## # A tibble: 1 × 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 mae     standard        3.77

``` r
resid_tree_cats2 <- tree_cats_model |>
augment(new_data = data_test)
```

``` r
resid_tree_cats2 |>
mae(.pred, win_rate_pct)
```

    ## # A tibble: 1 × 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 mae     standard        4.81

``` r
cat_model <- linear_reg()|>
  set_engine("lm")|>
  fit(win_rate_pct ~ Wakes.in.the.Night + Selects.Player + Learns.Info, data = data_all)
tidy(cat_model)
```

    ## # A tibble: 11 × 5
    ##    term                                   estimate std.error statistic   p.value
    ##    <chr>                                     <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 (Intercept)                              52.9        3.03    17.4    1.72e-37
    ##  2 Wakes.in.the.NightAlways                 -1.26       3.34    -0.378  7.06e- 1
    ##  3 Wakes.in.the.NightAlways (Except Firs…   -2.66       3.47    -0.768  4.44e- 1
    ##  4 Wakes.in.the.NightNo                     -0.416      3.71    -0.112  9.11e- 1
    ##  5 Wakes.in.the.NightOnce                    0.452      3.54     0.128  8.98e- 1
    ##  6 Wakes.in.the.NightSometimes              -1.16       3.73    -0.312  7.56e- 1
    ##  7 Selects.PlayerNo                          0.684      1.56     0.437  6.63e- 1
    ##  8 Selects.PlayerOptional                   -1.30       2.10    -0.619  5.37e- 1
    ##  9 Selects.PlayerRequired                   NA         NA       NA     NA       
    ## 10 Learns.InfoNo                             4.52       4.42     1.02   3.09e- 1
    ## 11 Learns.InfoYes                            4.29       4.31     0.997  3.20e- 1

``` r
resid_cat_model <- cat_model |>
  augment(new_data = data_all)
```

    ## Warning in predict.lm(object = object$fit, newdata = new_data, type =
    ## "response", : prediction from rank-deficient fit; consider predict(.,
    ## rankdeficient="NA")

``` r
resid_cat_model |>
  mae(.pred, win_rate_pct)
```

    ## # A tibble: 1 × 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 mae     standard        4.14

``` r
tree_cats_model|>
extract_fit_engine() |>
rpart.plot()
```

    ## Warning: Cannot retrieve the data used to build the model (so cannot determine roundint and is.binary for the variables).
    ## To silence this warning:
    ##     Call rpart.plot with roundint=FALSE,
    ##     or rebuild the rpart model with model=TRUE.

![](Data-201-Final-Project_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->
We also ran a decision tree for Script and Character Type (see below).
The script variable is interesting because each of the base scripts were
designed with different styles of characters, and we wanted to see if it
interacted at all with the types of characters. In this decision tree,
the first split is basically between evil characters and good
characters. Evil characters from the base scripts have the lowest win
rates.

``` r
tree_simple_model <- decision_tree() |>
  set_engine("rpart") |>
  set_mode("regression") |>
  fit(win_rate_pct ~ Script + role_type, data = data_train)
```

``` r
resid_tree_simple <- tree_simple_model |>
  augment(new_data = data_train)
resid_tree_simple |>
mae(.pred, win_rate_pct)
```

    ## # A tibble: 1 × 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 mae     standard        3.62

``` r
resid_tree_simple2 <- tree_simple_model |>
  augment(new_data = data_test)
resid_tree_simple2 |>
mae(.pred, win_rate_pct)
```

    ## # A tibble: 1 × 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 mae     standard        4.43

``` r
full_data4 <- data_all %>% mutate(
  char_type = as.factor(role_type)
)
simple_model <- linear_reg()|>
  set_engine("lm") |>
  fit(win_rate_pct~ Script + char_type, data =full_data4)
tidy(simple_model)
```

    ## # A tibble: 8 × 5
    ##   term                  estimate std.error statistic  p.value
    ##   <chr>                    <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)             55.1        1.67    32.9   5.70e-70
    ## 2 ScriptExperimental       0.613      1.32     0.463 6.44e- 1
    ## 3 ScriptSects & Violets    0.553      1.55     0.356 7.22e- 1
    ## 4 ScriptTrouble Brewing    0.355      1.59     0.224 8.23e- 1
    ## 5 char_typeMINION         -2.33       1.78    -1.31  1.92e- 1
    ## 6 char_typeOUTSIDER        2.65       1.84     1.44  1.52e- 1
    ## 7 char_typeTOWNSFOLK       2.21       1.54     1.43  1.54e- 1
    ## 8 char_typeTRAVELER        1.50       1.98     0.757 4.50e- 1

``` r
resid_simple <- simple_model |>
  augment(new_data = full_data4)
resid_simple |>
  mae(.pred, win_rate_pct)
```

    ## # A tibble: 1 × 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 mae     standard        4.03

``` r
tree_simple_model|>
extract_fit_engine() |>
rpart.plot()
```

    ## Warning: Cannot retrieve the data used to build the model (so cannot determine roundint and is.binary for the variables).
    ## To silence this warning:
    ##     Call rpart.plot with roundint=FALSE,
    ##     or rebuild the rpart model with model=TRUE.

![](Data-201-Final-Project_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->
This decision tree performed at 3.62% MAE in the training set, while the
testing set performed at 4.43% MAE. When we performed a regression on
these two variables, the MAE was 4.03%. This model had very similar
results to the last model, both indicating that the models do not
perform very well in their testing set if they don’t perform better than
non-significant regression models. We also looked at the relationship
between win rate and games played. We were not interested in games
played as a predictor of win rate since it isn’t an aspect of the
character itself. However, we felt that it was important to examine this
relationship given that the number of games played varies across the
characters. First, we conducted a k-means cluster analysis, with k set
to 2, which we decided on after also trying k = 3 and k = 4. The
additional clusters split cluster 2 up by win rate, which did not offer
more to the analysis.

``` r
games_played <- data_all %>% select(
  win_rate_pct,
  games_with_result
)


games_played_scaled <- games_played %>% mutate(
  scaled_wr =  scale(win_rate_pct),
  scaled_games_played = scale(games_with_result)
) %>% select(
  scaled_wr,
  scaled_games_played
)
```

``` r
set.seed(18)
km_games_played_2 <- kmeans(games_played_scaled, 2, nstart = 20)
games_played_clust_2 <- augment(km_games_played_2, games_played)
ggplot(data = games_played_clust_2, mapping = aes(x = games_with_result, y = win_rate_pct, color = .cluster)) + 
  geom_point()
```

![](Data-201-Final-Project_files/figure-gfm/unnamed-chunk-34-1.png)<!-- -->
These clusters reveal an interesting pattern between win rate and games
played. The characters in cluster two have fewer games played, but win
rates that vary significantly. The characters in cluster one have more
played games, and the range of win rates is much smaller, and mostly in
the range of 50-60%. It seems like as more games are recorded for each
character, the win rate starts to converge to somewhere in that range.
We also ran a regression model with win rate vs games played, which
indicated that games played was not a significant predictor of win rate.
However, the cluster analysis suggests that games played may account for
differences in win rate.

## Discussion

Overall, these models did not suggest many meaningful relationships
between various characteristics and win rate. Each of the decision tree
models yielded relatively high mean absolute errors compared to
nonsignificant regressions, and none of the regressions run had any
meaningfully significant results. The only characteristic which
significantly predicted win rate was Loudness, as seen in both the
cluster analysis and regression, but it did not hold up as well when
treated as a categorical variable. The cluster analyses also implied
that as more games are played, the win rate converges at around 50 - 60
percent, and that a lot of variation in win rates is from characters
with not many games logged. We used k-means clustering, decision trees,
and simple linear regression for modeling our data. One advantage of
decision trees is the very clear graphic they provide, which makes the
results easy to interpret. This was especially useful for our dataset,
which had mostly categorical explanatory variables. A disadvantage of
decision trees is that they generally perform worse than
regularization-based models. The k-means clustering allowed us to easily
visualize patterns in the data between our response variable and the
other continuous variables. One disadvantage to this approach was that
we can’t do k-means clustering on categorical variables, which made up a
lot of our dataset. Creating simple linear regression models is very
fast and interpretable, since we can easily identify which predictors
are significant in the model. Since our data had a lot of features that
were not significant, our linear regression models would probably
perform worse than regularization approaches, since those would shrink
the coefficients of insignificant predictors. The biggest limitation of
our dataset was that it was relatively small. With fewer observations,
it is more difficult for our models to accurately identify patterns and
relationships in the data. Having a smaller dataset also risks our
supervised learning models overfitting the training data. To combat
this, we used only 60% of the data for the training set, and the rest
for testing. Another limitation of our data was the differences in the
number of games played between the characters. The cluster analysis of
win rate and games played revealed a pattern of win rates tending to
converge to the 50-60% range as more games are recorded, and some of the
variance in the win rate may be explained by many characters having
relatively few games recorded. The results of this project show that
there are very few to no characteristics of Clocktower characters that
significantly predict win rate. This implies that Blood on the
Clocktower is a balanced game in which none of the characters are
particularly stronger or weaker than the others within the game.
