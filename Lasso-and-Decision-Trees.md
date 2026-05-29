Lasso and Decision Trees
================
Emily Young

setup:

1.  

``` r
clean_credit_data <- na.omit(credit_data)|>
  select(Amount, Age, Income, Expenses, Seniority, Price, Debt, Assets)
```

2.  

``` r
set.seed(18)

data_split <- initial_split(clean_credit_data, prop = .7)

credit_train <- training(data_split)
credit_test <- testing(data_split)
```

3.  

``` r
exp_vars <- credit_train |>
select(-Amount) |>
as.matrix()
```

``` r
resp <- credit_train |>
select(Amount) |>
pull()
```

``` r
cvlasso_mod <- cv.glmnet(x = exp_vars, y = resp, alpha = 1)
plot(cvlasso_mod)
```

![](Lasso-and-Decision-Trees_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
cvlasso_mod <- cv.glmnet(x = exp_vars, y = resp, alpha = 1)
plot(cvlasso_mod)
```

![](Lasso-and-Decision-Trees_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

``` r
cvlasso_mod <- cv.glmnet(x = exp_vars, y = resp, alpha = 1)
plot(cvlasso_mod)
```

![](Lasso-and-Decision-Trees_files/figure-gfm/unnamed-chunk-6-3.png)<!-- -->
Due to random chance, the plots have slight variations in mean squared
error at different log(lamda) points, and in where the minimum amount of
MSE lays.

``` r
set.seed(18)

cvlasso_mod <- cv.glmnet(x = exp_vars, y = resp, alpha = 1)
plot(cvlasso_mod)
```

![](Lasso-and-Decision-Trees_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

The minimum amount of error for this model occurs at the lambda value
that would give us 6 predictor variables, but you could go down to 1
predictor variable and still have a fine amount of error.

4.  

``` r
cvlasso_mod$lambda.min
```

    ## [1] 3.591263

This number represents a lambda value that would reduce the MSE while
taking model complexity into account.

5.  

``` r
lasso_mod <- linear_reg(penalty = cvlasso_mod$lambda.min, mixture = 1) |>
set_engine("glmnet") |>
fit(
Amount ~ Age + Income + Expenses + Seniority + Price + Debt + Assets,
data = credit_train)
lasso_mod |>
tidy()
```

    ## # A tibble: 8 × 3
    ##   term          estimate penalty
    ##   <chr>            <dbl>   <dbl>
    ## 1 (Intercept) 277.          3.59
    ## 2 Age           0           3.59
    ## 3 Income        0.223       3.59
    ## 4 Expenses      0.195       3.59
    ## 5 Seniority    -2.90        3.59
    ## 6 Price         0.503       3.59
    ## 7 Debt          0.00451     3.59
    ## 8 Assets        0.000114    3.59

It seems like age was dropped from the model, at its coefficient is
zero. Debt and Assets were also essentially dropped, as they are closer
to zero (.004 and .0001, respectively). An increase in job seniority is
associated negatively with the loan request amount, while an increase in
price, expenses, and income are all positively associated with loan
request amount.

6.  

``` r
linear_model_6 <- linear_reg()|>
  set_engine("lm")|>
  fit(Amount ~ Income + Expenses + Seniority + Price, data = credit_train)
tidy(linear_model_6)
```

    ## # A tibble: 5 × 5
    ##   term        estimate std.error statistic  p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)  260.      23.7        11.0  2.14e-27
    ## 2 Income         0.270    0.0813      3.32 9.00e- 4
    ## 3 Expenses       0.372    0.332       1.12 2.63e- 1
    ## 4 Seniority     -3.46     0.758      -4.57 5.14e- 6
    ## 5 Price          0.508    0.0103     49.3  0

The coefficients in the OLS model are much higher (in absolute value)
than in the lasso model, which makes sense because lasso purposefully
forces the coefficient values closer to zero.

7.  

``` r
resid_lasso_7 <- lasso_mod |>
  augment(new_data = credit_train)
predicted_lasso_7 <- resid_lasso_7 |>
  select(.pred)
```

``` r
resid_ols_7 <- linear_model_6 |>
  augment(new_data = credit_train)
predicted_ols_7 <- resid_ols_7 |>
  select(.pred)
```

8.  

``` r
tree_mod <- decision_tree() |>
set_engine("rpart") |>
set_mode("regression") |>
fit(Amount ~ Age + Income + Expenses + Seniority + Price + Debt + Assets, data = credit_train)
```

``` r
tree_mod|>
extract_fit_engine() |>
rpart.plot()
```

    ## Warning: Cannot retrieve the data used to build the model (so cannot determine roundint and is.binary for the variables).
    ## To silence this warning:
    ##     Call rpart.plot with roundint=FALSE,
    ##     or rebuild the rpart model with model=TRUE.

![](Lasso-and-Decision-Trees_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

Most of the nodes in this tree have something to do with Price, which
makes some sense as in the OLS model, Price had the largest coefficient
by a long shot. There are 7 terminal nodes and 6 internal nodes. At each
node, it says where a new split occurred if one did occur, as well as
the mean of Amount at that node and how much of the dataset is in that
node.

9.  

``` r
resid_tree_9 <- tree_mod |>
  augment(new_data = credit_train)
head(resid_tree_9)
```

    ## # A tibble: 6 × 10
    ##   .pred .resid Amount   Age Income Expenses Seniority Price  Debt Assets
    ##   <dbl>  <dbl>  <int> <int>  <int>    <int>     <int> <int> <int>  <int>
    ## 1  779. -329.     450    26    136       35         4   950     0   2500
    ## 2  480. -105.     375    24     85       44         0   575     0      0
    ## 3 1173.   26.7   1200    40     61       75         5  1483   770  11000
    ## 4  779.   81.4    860    27    250       70         0   970     0      0
    ## 5  779. -329.     450    37    150       60        20   950     0   3500
    ## 6 1173.  127.    1300    47    250       45         0  1700     0 100000

``` r
predicted_tree_9 <- resid_tree_9 |>
  select(.pred)
```

10. 

``` r
resid_lasso_7 |>
  mae(.pred, Amount)
```

    ## # A tibble: 1 × 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 mae     standard        244.

``` r
resid_ols_7 |>
  mae(.pred, Amount)
```

    ## # A tibble: 1 × 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 mae     standard        244.

``` r
resid_tree_9 |>
  mae(.pred, Amount)
```

    ## # A tibble: 1 × 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 mae     standard        242.

It seems that the decision tree performed best by a small amount, as it
had a slightly smaller MAE than the other models.

11. 

``` r
resid_lasso_11 <- lasso_mod |>
  augment(new_data = credit_test)
resid_lasso_11 |>
  mae(.pred, Amount)
```

    ## # A tibble: 1 × 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 mae     standard        253.

``` r
resid_ols_11 <- linear_model_6 |>
  augment(new_data = credit_test)
resid_ols_11 |>
  mae(.pred, Amount)
```

    ## # A tibble: 1 × 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 mae     standard        253.

``` r
resid_tree_11 <- tree_mod |>
  augment(new_data = credit_test)
resid_tree_11 |>
  mae(.pred, Amount)
```

    ## # A tibble: 1 × 3
    ##   .metric .estimator .estimate
    ##   <chr>   <chr>          <dbl>
    ## 1 mae     standard        253.

Now, the OLS model is just barely better than lasso, and both are better
than the decision tree. This contradicts the conclusion from 10, perhaps
because the decision tree overfit more.
