Cross Validation
================

``` r
data("SaratogaHouses")
?SaratogaHouses
```

1.  

``` r
glimpse(SaratogaHouses)
```

    ## Rows: 1,728
    ## Columns: 16
    ## $ price           <int> 132500, 181115, 109000, 155000, 86060, 120000, 153000,…
    ## $ lotSize         <dbl> 0.09, 0.92, 0.19, 0.41, 0.11, 0.68, 0.40, 1.21, 0.83, …
    ## $ age             <int> 42, 0, 133, 13, 0, 31, 33, 23, 36, 4, 123, 1, 13, 153,…
    ## $ landValue       <int> 50000, 22300, 7300, 18700, 15000, 14000, 23300, 14600,…
    ## $ livingArea      <int> 906, 1953, 1944, 1944, 840, 1152, 2752, 1662, 1632, 14…
    ## $ pctCollege      <int> 35, 51, 51, 51, 51, 22, 51, 35, 51, 44, 51, 51, 41, 57…
    ## $ bedrooms        <int> 2, 3, 4, 3, 2, 4, 4, 4, 3, 3, 7, 3, 2, 3, 3, 3, 3, 4, …
    ## $ fireplaces      <int> 1, 0, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, …
    ## $ bathrooms       <dbl> 1.0, 2.5, 1.0, 1.5, 1.0, 1.0, 1.5, 1.5, 1.5, 1.5, 1.0,…
    ## $ rooms           <int> 5, 6, 8, 5, 3, 8, 8, 9, 8, 6, 12, 6, 4, 5, 8, 4, 7, 12…
    ## $ heating         <fct> electric, hot water/steam, hot water/steam, hot air, h…
    ## $ fuel            <fct> electric, gas, gas, gas, gas, gas, oil, oil, electric,…
    ## $ sewer           <fct> septic, septic, public/commercial, septic, public/comm…
    ## $ waterfront      <fct> No, No, No, No, No, No, No, No, No, No, No, No, No, No…
    ## $ newConstruction <fct> No, No, No, No, Yes, No, No, No, No, No, No, No, No, N…
    ## $ centralAir      <fct> No, No, No, No, Yes, No, No, No, No, No, No, No, No, N…

There are 1,728 rows, each representing a house in Saratoga Country, NY.
Each of the 16 columns represents a different characteristic about the
houses.

2.  

Stack Overflow link:
<https://stackoverflow.com/questions/54298616/adding-a-legend-for-vertical-lines-of-histograms>

``` r
price_mean <- mean(SaratogaHouses$price)
price_median <- median(SaratogaHouses$price)
mean_median <- data.frame(measurement = c("mean", "median"),
                          value = c(price_mean, price_median))
```

``` r
SaratogaHouses |>
  ggplot(aes(price))+
  geom_histogram(binwidth=15000)+
  geom_vline(data = mean_median, aes(xintercept = value, color = measurement))+
  scale_color_manual(values = c("blue", "green"), name = NULL)
```

![](HW6_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

The price of houses is right skewed, which means there are more
‘outlier’ variables/a tail towards the right end of the graph. Because
the mean is more influenced by outliers than the median, the mean is
above the median. Since this graph is right skewed and therefore the
mean is overly influenced by outlier points, it makes sense to use the
median as a more accurate measure of where the bulk of the data lies.

3.  

``` r
SH3 <- SaratogaHouses |>
  mutate(price_abv = cut(price,
                         breaks = c(min(price), median(price), max(price)),
                         labels = c("below_average", "above_average")))
```

``` r
SH3$price_abv <- relevel(as.factor(SH3$price_abv), "below_average")
```

``` r
SH3 |>
  count(price_abv)
```

    ##       price_abv   n
    ## 1 below_average 865
    ## 2 above_average 862
    ## 3          <NA>   1

4.  

Waterfront:

``` r
SH3$waterfront <- relevel(as.factor(SH3$waterfront), "No")

SH3 |>
  count(waterfront)
```

    ##   waterfront    n
    ## 1         No 1713
    ## 2        Yes   15

heating:

``` r
SH3$heating <- relevel(as.factor(SH3$heating), "electric")

SH3 |>
  count(heating)
```

    ##           heating    n
    ## 1        electric  305
    ## 2         hot air 1121
    ## 3 hot water/steam  302

5.  

``` r
set.seed(18)

data_split <- initial_split(SH3, prop = .8)

data_train <- training(data_split)
data_test <- testing(data_split)
```

6.  

``` r
logit_train <- logistic_reg(mode = "classification") |>
  set_engine("glm")|>
  fit(price_abv ~ age + pctCollege + heating + waterfront, data = data_train)
logit_train |>
  tidy()
```

    ## # A tibble: 6 × 5
    ##   term                   estimate std.error statistic  p.value
    ##   <chr>                     <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)             -3.14     0.367       -8.57 1.06e-17
    ## 2 age                     -0.0170   0.00246     -6.93 4.34e-12
    ## 3 pctCollege               0.0406   0.00591      6.88 5.90e-12
    ## 4 heatinghot air           1.46     0.175        8.39 4.94e-17
    ## 5 heatinghot water/steam   1.97     0.218        9.01 2.03e-19
    ## 6 waterfrontYes            2.49     1.10         2.26 2.38e- 2

An older house has a lower probability of having above_average price. A
house in a neighborhood with a higher percent of college graduates had a
higher probability of having above average price. This is also true if
the house is waterfront, or if it has hot water/steam or hot air as its
heating system.

``` r
resid_logit6 <- logit_train |>
  augment(new_data = data_train)
head(resid_logit6)
```

    ## # A tibble: 6 × 20
    ##   .pred_class   .pred_below_average .pred_above_average  price lotSize   age
    ##   <fct>                       <dbl>               <dbl>  <int>   <dbl> <int>
    ## 1 above_average               0.476               0.524 177900    0.29    32
    ## 2 above_average               0.230               0.770 285000    0.36    13
    ## 3 above_average               0.371               0.629 127000    0.17    21
    ## 4 below_average               0.734               0.266 178000    0.48    28
    ## 5 above_average               0.329               0.671 293500    0.49    10
    ## 6 above_average               0.333               0.667 321000    0.53    11
    ## # ℹ 14 more variables: landValue <int>, livingArea <int>, pctCollege <int>,
    ## #   bedrooms <int>, fireplaces <int>, bathrooms <dbl>, rooms <int>,
    ## #   heating <fct>, fuel <fct>, sewer <fct>, waterfront <fct>,
    ## #   newConstruction <fct>, centralAir <fct>, price_abv <fct>

``` r
resid_logit6 |>
  roc_curve(truth = price_abv, .pred_below_average) |>
  autoplot()
```

![](HW6_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
resid_logit6 |>
  accuracy(truth = price_abv, .pred_class)
```

    ## # A tibble: 1 × 3
    ##   .metric  .estimator .estimate
    ##   <chr>    <chr>          <dbl>
    ## 1 accuracy binary         0.629

``` r
resid_logit6 |>
  roc_auc(truth = price_abv, .pred_below_average)
```

    ## # A tibble: 1 × 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 roc_auc binary         0.735

the model’s accuracy is only about .63 percent, while the area under the
ROC curve is about .74. While this is an okay model, it is definitely
not the best, especially considering this is still in the training data.

7.  

``` r
set.seed(18)

folds7 <- vfold_cv(data_train, v = 10)
folds7
```

    ## #  10-fold cross-validation 
    ## # A tibble: 10 × 2
    ##    splits             id    
    ##    <list>             <chr> 
    ##  1 <split [1243/139]> Fold01
    ##  2 <split [1243/139]> Fold02
    ##  3 <split [1244/138]> Fold03
    ##  4 <split [1244/138]> Fold04
    ##  5 <split [1244/138]> Fold05
    ##  6 <split [1244/138]> Fold06
    ##  7 <split [1244/138]> Fold07
    ##  8 <split [1244/138]> Fold08
    ##  9 <split [1244/138]> Fold09
    ## 10 <split [1244/138]> Fold10

``` r
logmodel <- logistic_reg()|>
  set_engine("glm")
rec7 <- recipe(price_abv ~ age + pctCollege + heating + waterfront, data = data_train)

model7_workflow <- workflow()|>
  add_model(logmodel)|>
  add_recipe(rec7)

model7_fit_rs <- fit_resamples(
  model7_workflow,
  resamples = folds7
)

print(model7_fit_rs)
```

    ## # Resampling results
    ## # 10-fold cross-validation 
    ## # A tibble: 10 × 4
    ##    splits             id     .metrics         .notes          
    ##    <list>             <chr>  <list>           <list>          
    ##  1 <split [1243/139]> Fold01 <tibble [3 × 4]> <tibble [0 × 4]>
    ##  2 <split [1243/139]> Fold02 <tibble [3 × 4]> <tibble [0 × 4]>
    ##  3 <split [1244/138]> Fold03 <tibble [3 × 4]> <tibble [0 × 4]>
    ##  4 <split [1244/138]> Fold04 <tibble [3 × 4]> <tibble [0 × 4]>
    ##  5 <split [1244/138]> Fold05 <tibble [3 × 4]> <tibble [0 × 4]>
    ##  6 <split [1244/138]> Fold06 <tibble [3 × 4]> <tibble [0 × 4]>
    ##  7 <split [1244/138]> Fold07 <tibble [3 × 4]> <tibble [0 × 4]>
    ##  8 <split [1244/138]> Fold08 <tibble [3 × 4]> <tibble [0 × 4]>
    ##  9 <split [1244/138]> Fold09 <tibble [3 × 4]> <tibble [0 × 4]>
    ## 10 <split [1244/138]> Fold10 <tibble [3 × 4]> <tibble [0 × 4]>

``` r
collect_metrics(model7_fit_rs, summarize=FALSE) |>
  print(n=30)
```

    ## # A tibble: 30 × 5
    ##    id     .metric     .estimator .estimate .config        
    ##    <chr>  <chr>       <chr>          <dbl> <chr>          
    ##  1 Fold01 accuracy    binary         0.626 pre0_mod0_post0
    ##  2 Fold01 roc_auc     binary         0.738 pre0_mod0_post0
    ##  3 Fold01 brier_class binary         0.215 pre0_mod0_post0
    ##  4 Fold02 accuracy    binary         0.568 pre0_mod0_post0
    ##  5 Fold02 roc_auc     binary         0.681 pre0_mod0_post0
    ##  6 Fold02 brier_class binary         0.233 pre0_mod0_post0
    ##  7 Fold03 accuracy    binary         0.638 pre0_mod0_post0
    ##  8 Fold03 roc_auc     binary         0.744 pre0_mod0_post0
    ##  9 Fold03 brier_class binary         0.208 pre0_mod0_post0
    ## 10 Fold04 accuracy    binary         0.688 pre0_mod0_post0
    ## 11 Fold04 roc_auc     binary         0.789 pre0_mod0_post0
    ## 12 Fold04 brier_class binary         0.194 pre0_mod0_post0
    ## 13 Fold05 accuracy    binary         0.616 pre0_mod0_post0
    ## 14 Fold05 roc_auc     binary         0.697 pre0_mod0_post0
    ## 15 Fold05 brier_class binary         0.221 pre0_mod0_post0
    ## 16 Fold06 accuracy    binary         0.587 pre0_mod0_post0
    ## 17 Fold06 roc_auc     binary         0.691 pre0_mod0_post0
    ## 18 Fold06 brier_class binary         0.236 pre0_mod0_post0
    ## 19 Fold07 accuracy    binary         0.667 pre0_mod0_post0
    ## 20 Fold07 roc_auc     binary         0.785 pre0_mod0_post0
    ## 21 Fold07 brier_class binary         0.201 pre0_mod0_post0
    ## 22 Fold08 accuracy    binary         0.613 pre0_mod0_post0
    ## 23 Fold08 roc_auc     binary         0.708 pre0_mod0_post0
    ## 24 Fold08 brier_class binary         0.221 pre0_mod0_post0
    ## 25 Fold09 accuracy    binary         0.630 pre0_mod0_post0
    ## 26 Fold09 roc_auc     binary         0.748 pre0_mod0_post0
    ## 27 Fold09 brier_class binary         0.210 pre0_mod0_post0
    ## 28 Fold10 accuracy    binary         0.630 pre0_mod0_post0
    ## 29 Fold10 roc_auc     binary         0.731 pre0_mod0_post0
    ## 30 Fold10 brier_class binary         0.213 pre0_mod0_post0

There is some variation across folds, but overall the model accuracy and
area under the roc curve is relatively consistent. Some folds, like
folds 2 and 6, did notably worse than the original model. However,
others like fold 4 did slightly better.

8.  

``` r
average_metrics8 <- collect_metrics(model7_fit_rs)
print(average_metrics8)
```

    ## # A tibble: 3 × 6
    ##   .metric     .estimator  mean     n std_err .config        
    ##   <chr>       <chr>      <dbl> <int>   <dbl> <chr>          
    ## 1 accuracy    binary     0.626    10 0.0110  pre0_mod0_post0
    ## 2 brier_class binary     0.215    10 0.00416 pre0_mod0_post0
    ## 3 roc_auc     binary     0.731    10 0.0118  pre0_mod0_post0

The average accuracy is .626, and the area under the roc curve is .73.
This is very similar to the original model, and actually the original
model was slightly more accurate.

9.  

``` r
rec9 <- recipe(price_abv ~ age + pctCollege + waterfront, data = data_train)

model9_workflow <- workflow()|>
  add_model(logmodel)|>
  add_recipe(rec9)

model9_fit_rs <- fit_resamples(
  model9_workflow,
  resamples = folds7
)
```

``` r
collect_metrics(model9_fit_rs, summarize=FALSE) |>
  print(n=30)
```

    ## # A tibble: 30 × 5
    ##    id     .metric     .estimator .estimate .config        
    ##    <chr>  <chr>       <chr>          <dbl> <chr>          
    ##  1 Fold01 accuracy    binary         0.590 pre0_mod0_post0
    ##  2 Fold01 roc_auc     binary         0.699 pre0_mod0_post0
    ##  3 Fold01 brier_class binary         0.232 pre0_mod0_post0
    ##  4 Fold02 accuracy    binary         0.554 pre0_mod0_post0
    ##  5 Fold02 roc_auc     binary         0.607 pre0_mod0_post0
    ##  6 Fold02 brier_class binary         0.247 pre0_mod0_post0
    ##  7 Fold03 accuracy    binary         0.652 pre0_mod0_post0
    ##  8 Fold03 roc_auc     binary         0.700 pre0_mod0_post0
    ##  9 Fold03 brier_class binary         0.227 pre0_mod0_post0
    ## 10 Fold04 accuracy    binary         0.645 pre0_mod0_post0
    ## 11 Fold04 roc_auc     binary         0.693 pre0_mod0_post0
    ## 12 Fold04 brier_class binary         0.226 pre0_mod0_post0
    ## 13 Fold05 accuracy    binary         0.609 pre0_mod0_post0
    ## 14 Fold05 roc_auc     binary         0.594 pre0_mod0_post0
    ## 15 Fold05 brier_class binary         0.247 pre0_mod0_post0
    ## 16 Fold06 accuracy    binary         0.616 pre0_mod0_post0
    ## 17 Fold06 roc_auc     binary         0.681 pre0_mod0_post0
    ## 18 Fold06 brier_class binary         0.235 pre0_mod0_post0
    ## 19 Fold07 accuracy    binary         0.659 pre0_mod0_post0
    ## 20 Fold07 roc_auc     binary         0.722 pre0_mod0_post0
    ## 21 Fold07 brier_class binary         0.223 pre0_mod0_post0
    ## 22 Fold08 accuracy    binary         0.686 pre0_mod0_post0
    ## 23 Fold08 roc_auc     binary         0.750 pre0_mod0_post0
    ## 24 Fold08 brier_class binary         0.219 pre0_mod0_post0
    ## 25 Fold09 accuracy    binary         0.616 pre0_mod0_post0
    ## 26 Fold09 roc_auc     binary         0.666 pre0_mod0_post0
    ## 27 Fold09 brier_class binary         0.238 pre0_mod0_post0
    ## 28 Fold10 accuracy    binary         0.609 pre0_mod0_post0
    ## 29 Fold10 roc_auc     binary         0.631 pre0_mod0_post0
    ## 30 Fold10 brier_class binary         0.239 pre0_mod0_post0

``` r
average_metrics9 <- collect_metrics(model9_fit_rs)
print(average_metrics9)
```

    ## # A tibble: 3 × 6
    ##   .metric     .estimator  mean     n std_err .config        
    ##   <chr>       <chr>      <dbl> <int>   <dbl> <chr>          
    ## 1 accuracy    binary     0.624    10 0.0120  pre0_mod0_post0
    ## 2 brier_class binary     0.233    10 0.00303 pre0_mod0_post0
    ## 3 roc_auc     binary     0.674    10 0.0158  pre0_mod0_post0

The average accuracy is .62, and the average area is .674. This is worse
than the full model, indicating that heating is probably a worthwhile
variable to include here.

10. 

``` r
logit_testfull <- logistic_reg(mode = "classification") |>
  set_engine("glm")|>
  fit(price_abv ~ age + pctCollege + heating + waterfront, data = data_test)
logit_testfull |>
  tidy()
```

    ## # A tibble: 6 × 5
    ##   term                   estimate std.error statistic    p.value
    ##   <chr>                     <dbl>     <dbl>     <dbl>      <dbl>
    ## 1 (Intercept)             -3.36     0.746       -4.51 0.00000653
    ## 2 age                     -0.0147   0.00440     -3.35 0.000800  
    ## 3 pctCollege               0.0485   0.0125       3.89 0.0000991 
    ## 4 heatinghot air           1.17     0.315        3.70 0.000213  
    ## 5 heatinghot water/steam   1.46     0.431        3.40 0.000678  
    ## 6 waterfrontYes            3.37     1.23         2.74 0.00605

``` r
logit_testre <- logistic_reg(mode = "classification") |>
  set_engine("glm")|>
  fit(price_abv ~ age + pctCollege + waterfront, data = data_test)
logit_testre |>
  tidy()
```

    ## # A tibble: 4 × 5
    ##   term          estimate std.error statistic    p.value
    ##   <chr>            <dbl>     <dbl>     <dbl>      <dbl>
    ## 1 (Intercept)    -2.85     0.719       -3.97 0.0000734 
    ## 2 age            -0.0117   0.00418     -2.79 0.00524   
    ## 3 pctCollege      0.0554   0.0123       4.49 0.00000720
    ## 4 waterfrontYes   2.96     1.15         2.56 0.0103

``` r
resid_logit10f <- logit_testfull |>
  augment(new_data = data_test)
resid_logit10re <- logit_testre |>
  augment(new_data = data_test)
```

``` r
roc_full <- roc_curve(resid_logit10f, truth = price_abv, .pred_below_average)
roc_re <- roc_curve(resid_logit10re, truth = price_abv, .pred_below_average)
```

``` r
combined_roc <- bind_rows(roc_full, roc_re, .id = "model")
```

``` r
combined_roc |>
  ggplot(aes(1-specificity, sensitivity, group = model, color = model))+
  geom_path()+
  geom_abline(slope = 1, intercept = 0, lty = "dotted")
```

![](HW6_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->

``` r
resid_logit10f |>
  roc_auc(truth = price_abv, .pred_below_average)
```

    ## # A tibble: 1 × 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 roc_auc binary         0.729

``` r
resid_logit10re |>
  roc_auc(truth = price_abv, .pred_below_average)
```

    ## # A tibble: 1 × 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 roc_auc binary         0.687

The full model’s area under the curve is .73 and the reduced model’s is
.69. This backs up the conclusions from part 9 that the full model with
heating is better than the reduced model without it.
