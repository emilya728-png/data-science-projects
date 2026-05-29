Data 101 Final Paper
================

``` r
install.packages("tidyverse")
```

    ## Installing package into '/cloud/lib/x86_64-pc-linux-gnu-library/4.6'
    ## (as 'lib' is unspecified)

    ## also installing the dependencies 'sys', 'bit', 'ps', 'farver', 'labeling', 'RColorBrewer', 'viridisLite', 'rematch', 'askpass', 'bit64', 'prettyunits', 'processx', 'backports', 'generics', 'blob', 'DBI', 'tidyselect', 'withr', 'data.table', 'gtable', 'isoband', 'S7', 'scales', 'gargle', 'uuid', 'cellranger', 'curl', 'ids', 'rematch2', 'cpp11', 'pkgconfig', 'openssl', 'timechange', 'utf8', 'systemfonts', 'textshaping', 'clipr', 'crayon', 'vroom', 'tzdb', 'progress', 'callr', 'selectr', 'broom', 'conflicted', 'dbplyr', 'dplyr', 'dtplyr', 'forcats', 'ggplot2', 'googledrive', 'googlesheets4', 'haven', 'hms', 'httr', 'lubridate', 'modelr', 'pillar', 'purrr', 'ragg', 'readr', 'readxl', 'reprex', 'rstudioapi', 'rvest', 'tibble', 'tidyr', 'xml2'

``` r
library(tidyverse)
```

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

``` r
install.packages("psych")
```

    ## Installing package into '/cloud/lib/x86_64-pc-linux-gnu-library/4.6'
    ## (as 'lib' is unspecified)

    ## also installing the dependencies 'mnormt', 'GPArotation'

``` r
library(psych)
```

    ## 
    ## Attaching package: 'psych'

    ## The following objects are masked from 'package:ggplot2':
    ## 
    ##     %+%, alpha

``` r
dataset <- read.csv("Data 101 Final Project Data Cleaned.csv")
```

# Section 1 - Introduction:

Many people are becoming more extreme with their political beliefs in
recent years, moving away from moderate positions and towards either the
left or right of the spectrum. This has raised concern about hardening
of policy positions. Ideally, people would change their policy positions
when presented with new situations, but extremism might prevent their
position from changing. We were motivated to research this topic because
of its impact on policy and cooperation in American politics.

Our principal research question is: Does a person’s extremism, defined
as distance from a politically neutral position, affect the level of
absolutism, defined as a person’s propensity to maintain their position
in extreme scenarios? Is this relationship true for only social
(abortion, immigration, gun control) or only economic (tariffs, taxes)
issues?

We used a dataset of survey responses on Prolific measuring extremism
and absolutism. The questions relate to various economic and social
issues.

# Section 2 - Data:

The data was collected via Prolific, a national survey platform. There
were 41 survey respondents (3 responses were removed). The original
dataset included 45 variables, plus 20 variables created for this
project. The final analysis includes 38 observations of 12 variables.

Participants answered questions on gun control, abortion, immigration,
tariffs, and taxes. Each included an initial question to determine
participants’ opinions (scale of 1-6). Participants were given a set of
three statements, such as “\[preferred policy\] would always be right,
even in a county where no one thought it was right” based on their
original opinion, and could answer either ‘I agree’ or ‘I disagree’.
Each participants’ score on all three questions were summed so that a 0
meant they had disagreed with everything, and a 3 meant they had agreed
with everything.

This data’s main limitation is the low number of participants, making it
difficult to make proper inferences.

# Section 3 - Analytic Framework

Our explanatory variable is extremism. The extremism variable was
created by recoding the initial question gauging opinion on each issue.
A 1 or 6 was recoded to a 3, a 2 or 5 to a 2, and a 3 or 4 to a 1, so
that an opinion further away from the middle of the scale (more extreme)
is higher. This created an extremism score for each issue, which were
averaged together across all issues as well as social and economic
issues. This variable is continuous on a scale from 1 - 3.

Our response variable is absolutism. Each participant had a ‘moral
absolutism’ score for each issue (described above), which were then
averaged together across all issues to create an aggregate absolutism
score. The scores on the social issues (gun control, abortion,
immigration) and the economic issues (tariffs, taxes) were also averaged
together separately. This variable is also continuous on a scale of 0 -
3.

We hypothesized that holding extreme political positions is positively
correlated with a high degree of absolutism. We expect that this
relationship will be weaker in cases of economic issues, as economic
issues tend to be more complicated and less polarizing.

We built regression models to predict the relationship between extremism
and absolutism. First, we tested the relationship between extremism
(explanatory variable) and absolutism (response variable) at the
aggregate level to check for an overall effect with a regression
analysis. Then we tested this relationship at an economic level and a
social level.

# Section 4 - Results:

The summary stats for extremism and absolutism at the aggregate,
economic, and social levels are provided below, as well as bar graphs
for each variable across all levels.

``` r
dataset1 <- dataset |>
  mutate(SocialAbs = (GunControlAbs + AbortionAbs + ImmigrationAbs)/3) |>
  mutate(EconomicAbs = (TaxesAbs + TariffsAbs)/2) |>
  mutate(SocialExtr = (GunControlExtr + AbortionExtr + ImmigrationExtr)/3) |>
  mutate(EconomicExtr = (TaxesExtr + TariffsExtr)/2)
summarydata <- dataset1 |>
  select(AbsolutismAvg:EconomicExtr, ImmigrationAbsNEW, GunControlAbs:TariffsExtr)
summarydataframe <- na.omit(summarydata)
descriptivestats <- describe(summarydataframe)
print(descriptivestats)
```

    ##                   vars  n mean   sd median trimmed  mad  min max range  skew
    ## AbsolutismAvg        1 38 1.93 0.60   2.00    1.98 0.59 0.20 2.8  2.60 -0.75
    ## ExtremismAvg         2 38 2.33 0.57   2.40    2.36 0.59 1.20 3.0  1.80 -0.38
    ## SocialAbs            3 38 1.82 0.64   2.00    1.84 0.49 0.33 3.0  2.67 -0.34
    ## EconomicAbs          4 38 2.08 0.94   2.50    2.17 0.74 0.00 3.0  3.00 -0.62
    ## SocialExtr           5 38 2.28 0.65   2.33    2.32 0.99 1.00 3.0  2.00 -0.39
    ## EconomicExtr         6 38 2.39 0.65   2.50    2.47 0.74 1.00 3.0  2.00 -0.77
    ## ImmigrationAbsNEW    7 38 1.29 1.34   1.50    1.25 2.22 0.00 3.0  3.00  0.24
    ## GunControlAbs        8 38 1.74 0.98   2.00    1.78 1.48 0.00 3.0  3.00 -0.49
    ## TaxesAbs             9 38 2.42 1.00   3.00    2.59 0.00 0.00 3.0  3.00 -1.51
    ## AbortionAbs         10 38 2.32 0.96   3.00    2.44 0.00 0.00 3.0  3.00 -0.99
    ## ImmigrationAbs      11 38 1.42 1.06   1.00    1.41 1.48 0.00 3.0  3.00  0.07
    ## TariffsAbs          12 38 1.74 1.33   2.00    1.78 1.48 0.00 3.0  3.00 -0.27
    ## GunControlExtr      13 38 2.24 0.79   2.00    2.28 1.48 1.00 3.0  2.00 -0.42
    ## TaxesExtr           14 38 2.53 0.80   3.00    2.62 0.00 1.00 3.0  2.00 -1.18
    ## AbortionExtr        15 38 2.45 0.72   3.00    2.53 0.00 1.00 3.0  2.00 -0.86
    ## ImmigrationExtr     16 38 2.16 0.82   2.00    2.19 1.48 1.00 3.0  2.00 -0.28
    ## TariffsExtr         17 38 2.26 0.76   2.00    2.31 1.48 1.00 3.0  2.00 -0.45
    ##                   kurtosis   se
    ## AbsolutismAvg         0.16 0.10
    ## ExtremismAvg         -1.17 0.09
    ## SocialAbs            -0.48 0.10
    ## EconomicAbs          -0.82 0.15
    ## SocialExtr           -1.21 0.10
    ## EconomicExtr         -0.61 0.11
    ## ImmigrationAbsNEW    -1.75 0.22
    ## GunControlAbs        -0.79 0.16
    ## TaxesAbs              0.86 0.16
    ## AbortionAbs          -0.43 0.16
    ## ImmigrationAbs       -1.27 0.17
    ## TariffsAbs           -1.75 0.22
    ## GunControlExtr       -1.31 0.13
    ## TaxesExtr            -0.41 0.13
    ## AbortionExtr         -0.66 0.12
    ## ImmigrationExtr      -1.51 0.13
    ## TariffsExtr          -1.19 0.12

``` r
meansdataframe <- summarydataframe |>
  summarise_all(mean, na.rm = TRUE)
meansdatalongvis1 <- meansdataframe |>
  pivot_longer(cols = GunControlAbs:TariffsAbs,
               names_to = "Issues",
               values_to = "Means")
meansdatalongvis1 |>
  ggplot(
    aes(Issues, Means)
  )+
  geom_col()
```

![](Data-101-Final-Project_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
meansdatalongvis2 <- meansdataframe |>
  pivot_longer(cols = GunControlExtr:TariffsExtr,
               names_to = "Issues",
               values_to = "Means")
meansdatalongvis2 |>
  ggplot(
    aes(Issues, Means)
  )+
  geom_col()
```

![](Data-101-Final-Project_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
meansdatalongvis3 <- meansdataframe |>
  pivot_longer(cols = c(AbsolutismAvg, SocialAbs, EconomicAbs),
               names_to = "Aggregates",
               values_to = "Means")
meansdatalongvis3 |>
  ggplot(aes(Aggregates, Means))+
  geom_col()
```

![](Data-101-Final-Project_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
meansdatalongvis4 <- meansdataframe |>
  pivot_longer(cols = c(ExtremismAvg, SocialExtr, EconomicExtr),
               names_to = "Aggregates",
               values_to = "Means")
meansdatalongvis4 |>
  ggplot(aes(Aggregates, Means))+
  geom_col()
```

![](Data-101-Final-Project_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

Additionally, a scatterplot with a line of best fit can be seen below
for each level.

``` r
summarydataframe |>
  ggplot(
    aes(SocialAbs, SocialExtr)
  )+
  geom_point()+
  geom_smooth(method = "lm")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](Data-101-Final-Project_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
summarydataframe |>
  ggplot(
    aes(EconomicAbs, EconomicExtr)
  )+
  geom_point()+
  geom_smooth(method = "lm")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](Data-101-Final-Project_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
summarydataframe |>
  ggplot(
    aes(AbsolutismAvg, ExtremismAvg)
  )+
  geom_point()+
  geom_smooth(method = "lm")
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](Data-101-Final-Project_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

These visualizations seem to suggest a positive trend between extremism
and absolutism. At the issue level, lower extremism scores are
associated with lower absolutism scores, although there was less
variation in extremism scores across the five issues than absolutism.
The same can be said on the aggregate level versus the social and
economic averages.

The scatterplots all display positive trends as well, though the line of
best fit seems the most different from zero for the Economic issues.
This appears to counter our original hypothesis that Social issues would
display a more pronounced relationship between extremism and absolutism.

Based on the residual plots (below), we have met the conditions for a
regression analysis. There does not seem to be any strong patterns or
clusters in the residuals. However, regression results should still be
treated cautiously due to the small number of observations.

``` r
aggregatemodel <- lm(AbsolutismAvg ~ ExtremismAvg, summarydataframe)
socialmodel <- lm(SocialAbs ~ SocialExtr, summarydataframe)
economicmodel <- lm(EconomicAbs ~ EconomicExtr, summarydataframe)

summarydataframe$aggregateresid <- residuals(aggregatemodel)
summarydataframe$socialresid <- residuals(socialmodel) 
summarydataframe$economicresid <- residuals(economicmodel)
summarydataframe$aggregatefitted <- fitted(aggregatemodel) 
summarydataframe$socialfitted <- fitted(socialmodel) 
summarydataframe$economicfitted <- fitted(economicmodel)
```

``` r
summarydataframe |>
  ggplot(aes(aggregatefitted, aggregateresid))+
  geom_point()+
  geom_hline(yintercept = 0)
```

![](Data-101-Final-Project_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
summarydataframe |>
  ggplot(aes(socialfitted, socialresid))+
  geom_point()+
  geom_hline(yintercept = 0)
```

![](Data-101-Final-Project_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
summarydataframe |>
  ggplot(aes(economicfitted, economicresid))+
  geom_point()+
  geom_hline(yintercept = 0)
```

![](Data-101-Final-Project_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->
The regression results below support our primary hypothesis, that
extremism is a predictor of absolutism (p = .028). The regression line
is: Absolutism = 1.048 + .378\*Extremism Although the slope is
significantly different from zero, the variation in extremism only
accounts for 12.65% of the variation in absolutism, though this may be
due to the small amount of data.

``` r
summary(aggregatemodel)
```

    ## 
    ## Call:
    ## lm(formula = AbsolutismAvg ~ ExtremismAvg, data = summarydataframe)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.52758 -0.26638  0.05706  0.42242  0.87242 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)    1.0479     0.3957   2.648   0.0119 *
    ## ExtremismAvg   0.3776     0.1654   2.283   0.0284 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5718 on 36 degrees of freedom
    ## Multiple R-squared:  0.1265, Adjusted R-squared:  0.1022 
    ## F-statistic: 5.213 on 1 and 36 DF,  p-value: 0.02842

For the social issues, extremism does not have a significantly
significant effect on absolutism.

``` r
summary(socialmodel)
```

    ## 
    ## Call:
    ## lm(formula = SocialAbs ~ SocialExtr, data = summarydataframe)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.43099 -0.34765  0.05684  0.37267  1.37875 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)   1.3351     0.3832   3.484  0.00132 **
    ## SocialExtr    0.2146     0.1618   1.326  0.19309   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.636 on 36 degrees of freedom
    ## Multiple R-squared:  0.04659,    Adjusted R-squared:  0.0201 
    ## F-statistic: 1.759 on 1 and 36 DF,  p-value: 0.1931

For economic issues, extremism was a significant predictor of absolutism
(p=.04). The regression equation is: Absolutism = .916 + .486\*Extremism
This indicates a positive relationship. However, the variation in
extremism accounts for only 11.21% of the variation in absolutism in
this model as well, though this may again be due to the limitations
introduced by a smaller number of observations.

``` r
summary(economicmodel)
```

    ## 
    ## Call:
    ## lm(formula = EconomicAbs ~ EconomicExtr, data = summarydataframe)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.3729 -0.6301  0.1271  0.6271  1.3556 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)    0.9160     0.5645   1.622   0.1134  
    ## EconomicExtr   0.4856     0.2277   2.132   0.0399 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.8989 on 36 degrees of freedom
    ## Multiple R-squared:  0.1121, Adjusted R-squared:  0.08748 
    ## F-statistic: 4.547 on 1 and 36 DF,  p-value: 0.03986

Overall, extremism was associated with absolutism across all issues, but
was not for social issues. This does not support our hypothesis that
social issues would display a stronger relationship between the two
variables than economic issues. Although the aggregate and economic
models were significant, the R^2 values were not very high, and our
ability to make definite inferences is limited by the small dataset.

# Section 5 - Implications

In the face of more extreme situations and a more polarized climate,
this data illustrates how US citizens are more likely to keep to their
current political view even in the face of extreme political scenarios
that could change their mind. This trend is more likely to hold true for
economic issues (tariffs and taxes) than social issues (abortion,
immigration, and gun control). While it is useful to know the importance
of extremism on absolutism with the current issues society is facing, it
is difficult to make a generalized conclusion based on this data due to
the small sample size. More survey responses need to be collected and
analyzed to better understand this relationship and how it applies to
the general US public.
