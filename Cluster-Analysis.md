Cluster Analysis
================
Emily Young

``` r
install.packages("tidyverse")
```

    ## Installing package into '/cloud/lib/x86_64-pc-linux-gnu-library/4.6'
    ## (as 'lib' is unspecified)

    ## also installing the dependencies 'sys', 'bit', 'ps', 'farver', 'labeling', 'RColorBrewer', 'viridisLite', 'rematch', 'askpass', 'bit64', 'prettyunits', 'processx', 'backports', 'generics', 'blob', 'DBI', 'tidyselect', 'withr', 'data.table', 'gtable', 'isoband', 'S7', 'scales', 'gargle', 'uuid', 'cellranger', 'curl', 'ids', 'rematch2', 'cpp11', 'pkgconfig', 'openssl', 'timechange', 'utf8', 'systemfonts', 'textshaping', 'clipr', 'crayon', 'vroom', 'tzdb', 'progress', 'callr', 'selectr', 'broom', 'conflicted', 'dbplyr', 'dplyr', 'dtplyr', 'forcats', 'ggplot2', 'googledrive', 'googlesheets4', 'haven', 'hms', 'httr', 'lubridate', 'modelr', 'pillar', 'purrr', 'ragg', 'readr', 'readxl', 'reprex', 'rstudioapi', 'rvest', 'tibble', 'tidyr', 'xml2'

``` r
install.packages("tidymodels")
```

    ## Installing package into '/cloud/lib/x86_64-pc-linux-gnu-library/4.6'
    ## (as 'lib' is unspecified)

    ## also installing the dependencies 'shape', 'future.apply', 'progressr', 'SQUAREM', 'diagram', 'lava', 'listenv', 'parallelly', 'splitfngr', 'prodlim', 'future', 'warp', 'mixopt', 'numDeriv', 'Rcpp', 'lbfgs', 'RcppArmadillo', 'DiceDesign', 'sfd', 'sparsevctrs', 'patchwork', 'globals', 'clock', 'gower', 'ipred', 'timeDate', 'furrr', 'slider', 'GauPro', 'modelenv', 'dials', 'hardhat', 'infer', 'modeldata', 'parsnip', 'recipes', 'rsample', 'tailor', 'tune', 'workflows', 'workflowsets', 'yardstick'

``` r
install.packages("mosaicData")
```

    ## Installing package into '/cloud/lib/x86_64-pc-linux-gnu-library/4.6'
    ## (as 'lib' is unspecified)

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
library(tidymodels)
```

    ## ── Attaching packages ────────────────────────────────────── tidymodels 1.5.0 ──
    ## ✔ broom        1.0.13     ✔ rsample      1.3.2 
    ## ✔ dials        1.4.3      ✔ tailor       0.1.0 
    ## ✔ infer        1.1.0      ✔ tune         2.1.0 
    ## ✔ modeldata    1.5.1      ✔ workflows    1.3.0 
    ## ✔ parsnip      1.6.0      ✔ workflowsets 1.1.1 
    ## ✔ recipes      1.3.2      ✔ yardstick    1.4.0 
    ## ── Conflicts ───────────────────────────────────────── tidymodels_conflicts() ──
    ## ✖ scales::discard() masks purrr::discard()
    ## ✖ dplyr::filter()   masks stats::filter()
    ## ✖ recipes::fixed()  masks stringr::fixed()
    ## ✖ dplyr::lag()      masks stats::lag()
    ## ✖ yardstick::spec() masks readr::spec()
    ## ✖ recipes::step()   masks stats::step()

``` r
library(mosaicData)
```

``` r
data("SAT")
```

1.  

``` r
glimpse(SAT)
```

    ## Rows: 50
    ## Columns: 8
    ## $ state  <fct> Alabama, Alaska, Arizona, Arkansas, California, Colorado, Conne…
    ## $ expend <dbl> 4.405, 8.963, 4.778, 4.459, 4.992, 5.443, 8.817, 7.030, 5.718, …
    ## $ ratio  <dbl> 17.2, 17.6, 19.3, 17.1, 24.0, 18.4, 14.4, 16.6, 19.1, 16.3, 17.…
    ## $ salary <dbl> 31.144, 47.951, 32.175, 28.934, 41.078, 34.571, 50.045, 39.076,…
    ## $ frac   <int> 8, 47, 27, 6, 45, 29, 81, 68, 48, 65, 57, 15, 13, 58, 5, 9, 11,…
    ## $ verbal <int> 491, 445, 448, 482, 417, 462, 431, 429, 420, 406, 407, 468, 488…
    ## $ math   <int> 538, 489, 496, 523, 485, 518, 477, 468, 469, 448, 482, 511, 560…
    ## $ sat    <int> 1029, 934, 944, 1005, 902, 980, 908, 897, 889, 854, 889, 979, 1…

The unit of analysis is states; there are 50 states There aren’t a lot
of units. Additionally, there could be a lot of variability within each
state (ie, between rural and urban populations) that we are missing by
using the whole state as one unit of analysis.

2.  

``` r
SAT |>
  ggplot(aes(sat, ratio))+
  geom_point()
```

![](Cluster-Analysis_files/figure-gfm/unnamed-chunk-4-1.png)<!-- --> It
looks like there could be two major groups, but I could see the middle
parts of those groups being categorized as their own group.

3.  

``` r
SAT <- SAT |>
  mutate(ratio_z = scale(ratio))|>
  mutate(sat_z = scale(sat))
```

4.  

``` r
SAT_2 <- SAT |>
  select(ratio_z, sat_z)
```

``` r
set.seed(18)
km_sat4 <- kmeans(SAT_2, 2, nstart = 1)
```

``` r
SAT_clust4 <- augment(km_sat4, SAT)
SAT_clust4 |>
  ggplot(aes(sat, ratio, color = .cluster))+
  geom_point()+
  labs(x = "SAT Score", y = "Ratio")
```

![](Cluster-Analysis_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

This seems like a poor cluster assignment. I would have thought it would
be divided into a left group and a right group, because there is a
visual gap between two ‘groups’ on the left and right. These two groups
are divided from each other but don’t make a lot of sense; there are
others that would be better.

5.  

``` r
wss5 <- sapply(2:10, function(k){
  kmeans(SAT_2, centers = k, nstart = 25)$tot.withinss
})
```

``` r
k_means <- c(2:10)
elbow_plot <- data.frame(wss5, k_means)
```

``` r
elbow_plot |>
  ggplot(aes(k_means, wss5))+
  geom_point()+
  geom_line()+
  labs(x = "K Means", y = "Within-Cluster Sum of Squares")
```

![](Cluster-Analysis_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->
Based on this plot, it looks like 3 might be the optimal value of k,
since that is where the ‘elbow’ is; there is a relatively smooth upward
trend up until that point.

6.  

``` r
set.seed(18)
km_sat6 <- kmeans(SAT_2, 3, nstart = 1)
```

``` r
SAT_clust6 <- augment(km_sat6, SAT)
SAT_clust6 |>
  ggplot(aes(sat, ratio, color = .cluster))+
  geom_point()+
  labs(x = "SAT Score", y = "Ratio")
```

![](Cluster-Analysis_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->
These groups make much more sense than the original k=2 clusters. As I
said in step 2, the data is divided into two groups on the bottom, one
on the left and one on the right, while the upper points and outliers
are divided into another group. I think that this captures the clusters
within the data much more than the k=2 clusters.

7.  

``` r
SAT_clust6 |>
  group_by(.cluster)|>
  summarize(mean(sat))
```

    ## # A tibble: 3 × 2
    ##   .cluster `mean(sat)`
    ##   <fct>          <dbl>
    ## 1 1               960.
    ## 2 2              1040.
    ## 3 3               895

``` r
SAT_clust6 |>
  group_by(.cluster)|>
  summarize(mean(ratio))
```

    ## # A tibble: 3 × 2
    ##   .cluster `mean(ratio)`
    ##   <fct>            <dbl>
    ## 1 1                 20.3
    ## 2 2                 16.3
    ## 3 3                 15.7

``` r
SAT_clust6 |>
  ggplot(aes(.cluster, ratio))+
  geom_bar(stat="summary", fun = "mean")+
  labs(x = "Cluster", y = "Mean Ratio")
```

![](Cluster-Analysis_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
SAT_clust6 |>
  ggplot(aes(.cluster, sat))+
  geom_bar(stat = "summary", fun= "mean")+
  labs(x = "Cluster", y = "Mean SAT Score")
```

![](Cluster-Analysis_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

Cluster 3 seems to have lower ratios and sat scores. It has the lowest
of the two across all the clusters. Cluster 1 seems to have higher
ratios and slightly higher SAT scores than Cluster 3. It has the highest
ratios but not the highest SAT scores. Cluster 2 seems to have ratios as
low as cluster 3. However, its SAT scores are the highest.

8.  

``` r
SAT_clust6 |>
  ggplot(aes(.cluster))+
  geom_bar()+
  labs(x = "Cluster", y = "Number of States")
```

![](Cluster-Analysis_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

9.  

``` r
set.seed(18)
km_sat9_4 <- kmeans(SAT_2, 4, nstart = 1)
set.seed(18)
km_sat9_5 <- kmeans(SAT_2, 5, nstart= 1)
```

``` r
SAT_clust9_4 <- augment(km_sat9_4, SAT)
SAT_clust9_5 <- augment(km_sat9_5, SAT)
```

``` r
SAT_clust9 <- bind_rows(SAT_clust4, SAT_clust6, SAT_clust9_4, SAT_clust9_5, .id = "k")
```

``` r
SAT_clust9 |>
  ggplot(aes(sat, ratio, color = .cluster))+
  geom_point()+
  labs(x = "SAT Score", y = "Ratio")+
  facet_wrap(~k)
```

![](Cluster-Analysis_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

With three clusters, the data is divided into a low-SAT, low-ratio
cluster, a high-SAT, low-ratio cluster, and a mid-SAT, high-ratio
cluster. The addition of a fourth cluster divides the high-SAT/low-ratio
cluster into two groups. The addition of a fifth combines the
low-SAT/low-ratio cluster with the mid-SAT/high-ratio cluster and
divides that full group in two. In the fifth cluster is just the two
outliers.

10. 

``` r
dist_SAT <- dist(SAT_2)
```

``` r
hc<- hclust(dist_SAT, method = "complete")
```

``` r
plot(hc)
```

![](Cluster-Analysis_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

The first branching represents the two outliers, California and Utah.
From there, the rest of the states were divided into two overall groups,
which were themselves divided up. At the bottom of the dendrogram is
each state, one per terminal node

11. 

``` r
clusters4 <- cutree(hc, k = 4)

cluster_complete4 <- SAT |>
add_column(cluster = factor(clusters4))
```

``` r
clusters3 <- cutree(hc, k = 3)

cluster_complete3 <- SAT |>
add_column(cluster = factor(clusters3))
```

``` r
cluster_complete3 |>
  ggplot(aes(sat, ratio, color = cluster))+
  geom_point()+
  labs(x = "SAT Score", y = "Ratio")
```

![](Cluster-Analysis_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

``` r
cluster_complete4 |>
  ggplot(aes(sat, ratio, color = cluster))+
  geom_point()+
  labs(x = "SAT Score", y = "Ratio")
```

![](Cluster-Analysis_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->

``` r
cluster_complete3 |>
  group_by(cluster)|>
  summarize(mean(ratio))
```

    ## # A tibble: 3 × 2
    ##   cluster `mean(ratio)`
    ##   <fct>           <dbl>
    ## 1 1                16.7
    ## 2 2                16.4
    ## 3 3                24.2

``` r
cluster_complete4 |>
  group_by(cluster)|>
  summarize(mean(ratio))
```

    ## # A tibble: 4 × 2
    ##   cluster `mean(ratio)`
    ##   <fct>           <dbl>
    ## 1 1                16.7
    ## 2 2                15.7
    ## 3 3                19.4
    ## 4 4                24.2

The hierarchical clustering seemed to prioritize clustering the outliers
on their own more than the k-means approach. Across all values of k, the
two outlier points are separated together. And once k = 4, the extra
group created was only about five states that were also slight outliers
from their original group.

12. 

``` r
cluster_complete3 |>
  ggplot(aes(cluster))+
  geom_bar()+
  labs(x = "Cluster", y = "Number of States")
```

![](Cluster-Analysis_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->

There are only two states in cluster 3, although cluster 1 and cluster
two are slightly more even. In the k-means clustering, two of the
clusters were exactly the same while the third cluster had half the
amount of states as each of them, so they were more evenly dispersed
than in hierarchical clustering. I think a more even distribution has
its drawbacks as the two outliers are pretty far away from the rest of
the points and don’t really look like they belong in any of the other
clusters.
