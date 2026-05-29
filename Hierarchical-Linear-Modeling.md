Psych 304 Final Project- Hierarchical Linear Modeling
================
Emily Young

Load Libraries/Packages

``` r
install.packages("lme4")
```

    ## Installing package into '/cloud/lib/x86_64-pc-linux-gnu-library/4.6'
    ## (as 'lib' is unspecified)

``` r
install.packages("tidyverse")
```

    ## Installing package into '/cloud/lib/x86_64-pc-linux-gnu-library/4.6'
    ## (as 'lib' is unspecified)

``` r
install.packages("ggeffects")
```

    ## Installing package into '/cloud/lib/x86_64-pc-linux-gnu-library/4.6'
    ## (as 'lib' is unspecified)

``` r
install.packages("haven")
```

    ## Installing package into '/cloud/lib/x86_64-pc-linux-gnu-library/4.6'
    ## (as 'lib' is unspecified)

``` r
library(lme4)
```

    ## Loading required package: Matrix

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
    ## ✖ tidyr::expand() masks Matrix::expand()
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ✖ tidyr::pack()   masks Matrix::pack()
    ## ✖ tidyr::unpack() masks Matrix::unpack()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(ggeffects)
library(haven)
```

Import data

``` r
alldata <- read_sav("Psych 304 f24 T3D adolescents (2).sav")
```

Variables: Legitall (how legitimate adolescents viewed their parents as
on certain issues and in aggregate) pwarmth (how warm adolescents viewed
their parents as) Age Issue Domains (issues were divided into domains by
how much of each domain they fell into by percentage- validated by other
studies) Obligation to Obey (how obligated adolescents felt to obey
their parents)

Z Scores for legitall

``` r
BaselineLegit <- lmer(Zlegitall ~ 1 + (1|id), data=alldata)
summary(BaselineLegit)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: Zlegitall ~ 1 + (1 | id)
    ##    Data: alldata
    ## 
    ## REML criterion at convergence: 16043.7
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.4353 -0.6861  0.1832  0.8079  2.1199 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  id       (Intercept) 0.2019   0.4493  
    ##  Residual             0.7988   0.8937  
    ## Number of obs: 5928, groups:  id, 312
    ## 
    ## Fixed effects:
    ##              Estimate Std. Error t value
    ## (Intercept) 3.293e-14  2.796e-02       0

``` r
confint(BaselineLegit, method=c("boot"), boot.type=c("basic"))
```

    ## Computing bootstrap confidence intervals ...

    ##                   2.5 %     97.5 %
    ## .sig01       0.40865802 0.48884100
    ## .sigma       0.87559101 0.90891061
    ## (Intercept) -0.04855203 0.05493943

\##baselines for legitall with demographics

``` r
Legitwgender <- lmer(Zlegitall ~ Q1.4 + (1|id), data=alldata)
summary(Legitwgender)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: Zlegitall ~ Q1.4 + (1 | id)
    ##    Data: alldata
    ## 
    ## REML criterion at convergence: 16044.7
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.4295 -0.6824  0.1698  0.8132  2.1236 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  id       (Intercept) 0.2005   0.4477  
    ##  Residual             0.7988   0.8937  
    ## Number of obs: 5928, groups:  id, 312
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error t value
    ## (Intercept)  0.17501    0.10817   1.618
    ## Q1.4        -0.10037    0.05994  -1.674
    ## 
    ## Correlation of Fixed Effects:
    ##      (Intr)
    ## Q1.4 -0.966

``` r
confint(Legitwgender, method=c("boot"), boot.type=c("basic"))
```

    ## Computing bootstrap confidence intervals ...

    ##                   2.5 %     97.5 %
    ## .sig01       0.40932400 0.48889181
    ## .sigma       0.87797491 0.91162751
    ## (Intercept) -0.02632062 0.39134786
    ## Q1.4        -0.22155290 0.02084542

Gender has a statistically insignifant effect on legitall

\##Legitall with pwarmth

``` r
Legitwarmth <- lmer(Zlegitall ~ Zpwarmth + (1|id), data=alldata)
summary(Legitwarmth)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: Zlegitall ~ Zpwarmth + (1 | id)
    ##    Data: alldata
    ## 
    ## REML criterion at convergence: 16013.3
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.4512 -0.6514  0.1734  0.8154  2.0667 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  id       (Intercept) 0.1760   0.4196  
    ##  Residual             0.7988   0.8937  
    ## Number of obs: 5928, groups:  id, 312
    ## 
    ## Fixed effects:
    ##              Estimate Std. Error t value
    ## (Intercept) 3.105e-14  2.644e-02   0.000
    ## Zpwarmth    1.627e-01  2.644e-02   6.152
    ## 
    ## Correlation of Fixed Effects:
    ##          (Intr)
    ## Zpwarmth 0.000

``` r
confint(Legitwarmth, method=c("boot"), boot.type=c("basic"))
```

    ## Computing bootstrap confidence intervals ...

    ##                  2.5 %     97.5 %
    ## .sig01       0.3810638 0.46247558
    ## .sigma       0.8777550 0.91039832
    ## (Intercept) -0.0524494 0.05148103
    ## Zpwarmth     0.1081186 0.21541891

Maternal warmth has a statistically significant effect on legitall

\#Legitall with Age

``` r
alldata$ZAge <- scale(alldata$Age, center = TRUE, scale = TRUE)
Legitage <- lmer(Zlegitall ~ ZAge + (1|id), data=alldata)
summary(Legitage)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: Zlegitall ~ ZAge + (1 | id)
    ##    Data: alldata
    ## 
    ## REML criterion at convergence: 16035.6
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.4379 -0.6636  0.1427  0.8167  2.1186 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  id       (Intercept) 0.1922   0.4385  
    ##  Residual             0.7988   0.8937  
    ## Number of obs: 5928, groups:  id, 312
    ## 
    ## Fixed effects:
    ##               Estimate Std. Error t value
    ## (Intercept)  3.460e-14  2.740e-02   0.000
    ## ZAge        -1.017e-01  2.740e-02  -3.711
    ## 
    ## Correlation of Fixed Effects:
    ##      (Intr)
    ## ZAge 0.000

``` r
confint(Legitage, method=c("boot"), boot.type=c("basic"))
```

    ## Computing bootstrap confidence intervals ...

    ##                   2.5 %      97.5 %
    ## .sig01       0.39504208  0.47718786
    ## .sigma       0.87587239  0.90858138
    ## (Intercept) -0.05023428  0.05108836
    ## ZAge        -0.15738055 -0.04145065

Age has a statistically significant effect on legitall

\#Pwarmth on age and legit

``` r
Legitwarmthageint <- lmer(Zlegitall ~ Zpwarmth + ZAge + Zpwarmth:ZAge + (1|id), data=alldata)
summary(Legitwarmthageint)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: Zlegitall ~ Zpwarmth + ZAge + Zpwarmth:ZAge + (1 | id)
    ##    Data: alldata
    ## 
    ## REML criterion at convergence: 16013.4
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.4549 -0.6680  0.1579  0.8053  2.0666 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  id       (Intercept) 0.1701   0.4124  
    ##  Residual             0.7988   0.8937  
    ## Number of obs: 5928, groups:  id, 312
    ## 
    ## Fixed effects:
    ##               Estimate Std. Error t value
    ## (Intercept)   -0.01193    0.02680  -0.445
    ## Zpwarmth       0.16248    0.02774   5.857
    ## ZAge          -0.06015    0.02721  -2.210
    ## Zpwarmth:ZAge -0.05661    0.02932  -1.931
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Zpwrmt ZAge  
    ## Zpwarmth    -0.063              
    ## ZAge        -0.046  0.253       
    ## Zpwrmth:ZAg  0.231 -0.275 -0.198

``` r
confint(Legitwarmthageint, method=c("boot"), boot.type=c("basic"))
```

    ## Computing bootstrap confidence intervals ...

    ##                     2.5 %       97.5 %
    ## .sig01         0.37055797  0.453104903
    ## .sigma         0.87720691  0.908816133
    ## (Intercept)   -0.06709155  0.040906863
    ## Zpwarmth       0.10298470  0.210308128
    ## ZAge          -0.11355144 -0.003107363
    ## Zpwarmth:ZAge -0.11779417  0.002450745

The interaction between Age and Pwarmth on Legitall is not statistically
significant, but Age and Pwarmth are individually in the model.

\#Legitall with pwarmth and age (no interaction)

``` r
Legitwarmthage <- lmer(Zlegitall ~ Zpwarmth + ZAge + Zpwarmth:ZAge + (1|id), data=alldata)
summary(Legitwarmthage)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: Zlegitall ~ Zpwarmth + ZAge + Zpwarmth:ZAge + (1 | id)
    ##    Data: alldata
    ## 
    ## REML criterion at convergence: 16013.4
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.4549 -0.6680  0.1579  0.8053  2.0666 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  id       (Intercept) 0.1701   0.4124  
    ##  Residual             0.7988   0.8937  
    ## Number of obs: 5928, groups:  id, 312
    ## 
    ## Fixed effects:
    ##               Estimate Std. Error t value
    ## (Intercept)   -0.01193    0.02680  -0.445
    ## Zpwarmth       0.16248    0.02774   5.857
    ## ZAge          -0.06015    0.02721  -2.210
    ## Zpwarmth:ZAge -0.05661    0.02932  -1.931
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Zpwrmt ZAge  
    ## Zpwarmth    -0.063              
    ## ZAge        -0.046  0.253       
    ## Zpwrmth:ZAg  0.231 -0.275 -0.198

``` r
confint(Legitwarmthage, method=c("boot"), boot.type=c("basic"))
```

    ## Computing bootstrap confidence intervals ...

    ##                    2.5 %       97.5 %
    ## .sig01         0.3718807 0.4542998872
    ## .sigma         0.8745358 0.9096828980
    ## (Intercept)   -0.0633695 0.0479232506
    ## Zpwarmth       0.1121923 0.2136836124
    ## ZAge          -0.1116276 0.0026175345
    ## Zpwarmth:ZAge -0.1147263 0.0006520794

\#Legitall with pwarmth, age, and issue domains

``` r
Legitwarmthagedomain <- lmer(Zlegitall ~ Zpwarmth + ZAge + ZPersonal + ZPrudential + ZDoctor + ZLifestyle + ZFamily + (1|id), data=alldata)
summary(Legitwarmthagedomain)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: Zlegitall ~ Zpwarmth + ZAge + ZPersonal + ZPrudential + ZDoctor +  
    ##     ZLifestyle + ZFamily + (1 | id)
    ##    Data: alldata
    ## 
    ## REML criterion at convergence: 14638
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.2207 -0.5957  0.1224  0.6839  2.9747 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  id       (Intercept) 0.1812   0.4257  
    ##  Residual             0.6222   0.7888  
    ## Number of obs: 5928, groups:  id, 312
    ## 
    ## Fixed effects:
    ##               Estimate Std. Error t value
    ## (Intercept)  4.064e-14  2.619e-02   0.000
    ## Zpwarmth     1.478e-01  2.679e-02   5.516
    ## ZAge        -7.055e-02  2.679e-02  -2.633
    ## ZPersonal   -5.709e-02  3.017e-02  -1.892
    ## ZPrudential  3.065e-01  2.282e-02  13.430
    ## ZDoctor      2.490e-01  3.017e-02   8.253
    ## ZLifestyle  -2.822e-01  1.980e-02 -14.248
    ## ZFamily      2.104e-01  1.836e-02  11.459
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Zpwrmt ZAge  ZPrsnl ZPrdnt ZDoctr ZLfsty
    ## Zpwarmth    0.000                                          
    ## ZAge        0.000  0.211                                   
    ## ZPersonal   0.000  0.000  0.000                            
    ## ZPrudential 0.000  0.000  0.000 0.833                      
    ## ZDoctor     0.000  0.000  0.000 0.917  0.854               
    ## ZLifestyle  0.000  0.000  0.000 0.815  0.588  0.786        
    ## ZFamily     0.000  0.000  0.000 0.787  0.765  0.804  0.688

``` r
confint(Legitwarmthagedomain, method=c("boot"), boot.type=c("basic"))
```

    ## Computing bootstrap confidence intervals ...

    ##                   2.5 %        97.5 %
    ## .sig01       0.38621834  0.4688123539
    ## .sigma       0.77409257  0.8015390351
    ## (Intercept) -0.04915022  0.0498901998
    ## Zpwarmth     0.10025911  0.2008927757
    ## ZAge        -0.12249800 -0.0190217179
    ## ZPersonal   -0.11420171  0.0006331929
    ## ZPrudential  0.26105635  0.3508754933
    ## ZDoctor      0.18658518  0.3041937499
    ## ZLifestyle  -0.32086515 -0.2411304531
    ## ZFamily      0.17480595  0.2469516196

With no interaction terms, all of the issue domains, pwarmth, and age
have significant effects on Legitall (except for ZPersonal, which has a
just barely insignificant effect, so the variable will be kept)

\#Legitall with pwarmth, Age, and Personal

``` r
Legitagewarmthpersonal <- lmer(Zlegitall ~ Zpwarmth + ZAge*ZPersonal + (1|id), data=alldata)
summary(Legitagewarmthpersonal)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: Zlegitall ~ Zpwarmth + ZAge * ZPersonal + (1 | id)
    ##    Data: alldata
    ## 
    ## REML criterion at convergence: 15846.4
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.6395 -0.6492  0.1426  0.7813  2.0920 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  id       (Intercept) 0.1732   0.4162  
    ##  Residual             0.7739   0.8797  
    ## Number of obs: 5928, groups:  id, 312
    ## 
    ## Fixed effects:
    ##                  Estimate Std. Error t value
    ## (Intercept)     3.070e-14  2.619e-02   0.000
    ## Zpwarmth        1.478e-01  2.679e-02   5.516
    ## ZAge           -7.055e-02  2.679e-02  -2.633
    ## ZPersonal      -1.542e-01  1.143e-02 -13.497
    ## ZAge:ZPersonal -6.680e-03  1.143e-02  -0.585
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Zpwrmt ZAge  ZPrsnl
    ## Zpwarmth    0.000                     
    ## ZAge        0.000  0.211              
    ## ZPersonal   0.000  0.000  0.000       
    ## ZAge:ZPrsnl 0.000  0.000  0.000 0.000

``` r
confint(Legitagewarmthpersonal, method=c("boot"), boot.type=c("basic"))
```

    ## Computing bootstrap confidence intervals ...

    ##                      2.5 %      97.5 %
    ## .sig01          0.37387922  0.45589752
    ## .sigma          0.86248102  0.89529431
    ## (Intercept)    -0.05232746  0.04914315
    ## Zpwarmth        0.09714396  0.20041025
    ## ZAge           -0.11906630 -0.01545353
    ## ZPersonal      -0.17420708 -0.13222911
    ## ZAge:ZPersonal -0.03000691  0.01631603

\#Legitall with pwarmth, Age, and Prudential

``` r
Legitwarmthageprudential <- lmer(Zlegitall ~ Zpwarmth + ZAge*ZPrudential + (1|id), data=alldata)
summary(Legitwarmthageprudential)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: Zlegitall ~ Zpwarmth + ZAge * ZPrudential + (1 | id)
    ##    Data: alldata
    ## 
    ## REML criterion at convergence: 16005.9
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.4895 -0.6531  0.1403  0.8127  2.1113 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  id       (Intercept) 0.1721   0.4148  
    ##  Residual             0.7962   0.8923  
    ## Number of obs: 5928, groups:  id, 312
    ## 
    ## Fixed effects:
    ##                    Estimate Std. Error t value
    ## (Intercept)       3.053e-14  2.619e-02   0.000
    ## Zpwarmth          1.478e-01  2.679e-02   5.516
    ## ZAge             -7.055e-02  2.679e-02  -2.633
    ## ZPrudential       5.067e-02  1.159e-02   4.372
    ## ZAge:ZPrudential -1.200e-02  1.159e-02  -1.036
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Zpwrmt ZAge  ZPrdnt
    ## Zpwarmth    0.000                     
    ## ZAge        0.000  0.211              
    ## ZPrudential 0.000  0.000  0.000       
    ## ZAg:ZPrdntl 0.000  0.000  0.000 0.000

``` r
confint(Legitwarmthageprudential, method=c("boot"), boot.type=c("basic"))
```

    ## Computing bootstrap confidence intervals ...

    ##                        2.5 %      97.5 %
    ## .sig01            0.36907359  0.45561017
    ## .sigma            0.87495776  0.91047024
    ## (Intercept)      -0.05635099  0.05108074
    ## Zpwarmth          0.09227309  0.20047282
    ## ZAge             -0.12285221 -0.01907566
    ## ZPrudential       0.02874723  0.07485813
    ## ZAge:ZPrudential -0.03277006  0.01089894

\#Legitall with pwarmth, Age, and Doctor

``` r
Legitwarmthagedoctor <- lmer(Zlegitall ~ Zpwarmth + ZAge*ZDoctor + (1|id), data=alldata)
summary(Legitwarmthagedoctor)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: Zlegitall ~ Zpwarmth + ZAge * ZDoctor + (1 | id)
    ##    Data: alldata
    ## 
    ## REML criterion at convergence: 15749.4
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.7413 -0.6245  0.1320  0.7804  2.2364 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  id       (Intercept) 0.1739   0.4171  
    ##  Residual             0.7606   0.8721  
    ## Number of obs: 5928, groups:  id, 312
    ## 
    ## Fixed effects:
    ##                Estimate Std. Error t value
    ## (Intercept)   3.192e-14  2.619e-02   0.000
    ## Zpwarmth      1.478e-01  2.679e-02   5.516
    ## ZAge         -7.055e-02  2.679e-02  -2.633
    ## ZDoctor       1.906e-01  1.133e-02  16.822
    ## ZAge:ZDoctor -8.563e-03  1.133e-02  -0.756
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Zpwrmt ZAge  ZDoctr
    ## Zpwarmth    0.000                     
    ## ZAge        0.000  0.211              
    ## ZDoctor     0.000  0.000  0.000       
    ## ZAge:ZDoctr 0.000  0.000  0.000 0.000

``` r
confint(Legitwarmthagedoctor, method=c("boot"), boot.type=c("basic"))
```

    ## Computing bootstrap confidence intervals ...

    ##                    2.5 %      97.5 %
    ## .sig01        0.38319282  0.45618861
    ## .sigma        0.85644630  0.88729623
    ## (Intercept)  -0.05433965  0.05061063
    ## Zpwarmth      0.10293890  0.20382244
    ## ZAge         -0.12501590 -0.01863461
    ## ZDoctor       0.16696224  0.21245656
    ## ZAge:ZDoctor -0.02970206  0.01595608

\#Legitall with pwarmth, Age, and Lifestyle

``` r
Legitwarmthagelifestyle <- lmer(Zlegitall ~ Zpwarmth + ZAge*ZLifestyle + (1|id), data=alldata)
summary(Legitwarmthagelifestyle)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: Zlegitall ~ Zpwarmth + ZAge * ZLifestyle + (1 | id)
    ##    Data: alldata
    ## 
    ## REML criterion at convergence: 15592.1
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.7125 -0.6908  0.1267  0.7817  2.5328 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  id       (Intercept) 0.1750   0.4184  
    ##  Residual             0.7396   0.8600  
    ## Number of obs: 5928, groups:  id, 312
    ## 
    ## Fixed effects:
    ##                   Estimate Std. Error t value
    ## (Intercept)      3.343e-14  2.619e-02   0.000
    ## Zpwarmth         1.478e-01  2.679e-02   5.516
    ## ZAge            -7.055e-02  2.679e-02  -2.633
    ## ZLifestyle      -2.358e-01  1.117e-02 -21.112
    ## ZAge:ZLifestyle  2.593e-02  1.117e-02   2.321
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Zpwrmt ZAge  ZLfsty
    ## Zpwarmth    0.000                     
    ## ZAge        0.000  0.211              
    ## ZLifestyle  0.000  0.000  0.000       
    ## ZAg:ZLfstyl 0.000  0.000  0.000 0.000

``` r
confint(Legitwarmthagelifestyle, method=c("boot"), boot.type=c("basic"))
```

    ## Computing bootstrap confidence intervals ...

    ##                        2.5 %      97.5 %
    ## .sig01           0.377338510  0.45855429
    ## .sigma           0.844512251  0.87663343
    ## (Intercept)     -0.057390263  0.04779653
    ## Zpwarmth         0.100583071  0.20783320
    ## ZAge            -0.123671310 -0.01751011
    ## ZLifestyle      -0.259335910 -0.21448549
    ## ZAge:ZLifestyle  0.004645648  0.04641461

\#Legitall with pwarmth, Age, and Family

``` r
Legitwarmthagefamily <- lmer(Zlegitall ~ Zpwarmth + ZAge*ZFamily + (1|id), data=alldata)
summary(Legitwarmthagefamily)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: Zlegitall ~ Zpwarmth + ZAge * ZFamily + (1 | id)
    ##    Data: alldata
    ## 
    ## REML criterion at convergence: 15873.2
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.8235 -0.6541  0.1481  0.8054  2.1583 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  id       (Intercept) 0.1730   0.4160  
    ##  Residual             0.7776   0.8818  
    ## Number of obs: 5928, groups:  id, 312
    ## 
    ## Fixed effects:
    ##                Estimate Std. Error t value
    ## (Intercept)   3.034e-14  2.619e-02   0.000
    ## Zpwarmth      1.478e-01  2.679e-02   5.516
    ## ZAge         -7.055e-02  2.679e-02  -2.633
    ## ZFamily       1.401e-01  1.145e-02  12.231
    ## ZAge:ZFamily  2.649e-02  1.145e-02   2.313
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Zpwrmt ZAge  ZFamly
    ## Zpwarmth    0.000                     
    ## ZAge        0.000  0.211              
    ## ZFamily     0.000  0.000  0.000       
    ## ZAge:ZFamly 0.000  0.000  0.000 0.000

``` r
confint(Legitwarmthagefamily, method=c("boot"), boot.type=c("basic"))
```

    ## Computing bootstrap confidence intervals ...

    ## 
    ## 1 warning(s): Model failed to converge with max|grad| = 0.0100262 (tol = 0.002, component 1)
    ##   See ?lme4::convergence and ?lme4::troubleshooting.

    ##                     2.5 %      97.5 %
    ## .sig01        0.375166318  0.45738342
    ## .sigma        0.865216951  0.90042360
    ## (Intercept)  -0.051773961  0.05624150
    ## Zpwarmth      0.093648660  0.20473752
    ## ZAge         -0.124782150 -0.01820135
    ## ZFamily       0.118564480  0.16297611
    ## ZAge:ZFamily  0.002466732  0.04933686

Personal, Prudential, and Doctor did not have significant interactions
with Age on legitall. However, Lifestyle and Family did! Lifestyle, Age,
and their interaction all had significant effect on Legitall, while
Family did not on its own but did when interacting with Age.

\#Legitall with everything

``` r
Legitwarmthageissuesint <- lmer(Zlegitall ~ Zpwarmth + ZAge*ZLifestyle + ZAge*ZPersonal + ZAge*ZPrudential + ZAge*ZDoctor + ZAge*ZFamily + (1|id), data=alldata)
summary(Legitwarmthageissuesint)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: Zlegitall ~ Zpwarmth + ZAge * ZLifestyle + ZAge * ZPersonal +  
    ##     ZAge * ZPrudential + ZAge * ZDoctor + ZAge * ZFamily + (1 |      id)
    ##    Data: alldata
    ## 
    ## REML criterion at convergence: 14650.8
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.2624 -0.6010  0.1206  0.6868  2.9280 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  id       (Intercept) 0.1813   0.4258  
    ##  Residual             0.6204   0.7877  
    ## Number of obs: 5928, groups:  id, 312
    ## 
    ## Fixed effects:
    ##                    Estimate Std. Error t value
    ## (Intercept)       4.053e-14  2.619e-02   0.000
    ## Zpwarmth          1.478e-01  2.679e-02   5.516
    ## ZAge             -7.055e-02  2.679e-02  -2.633
    ## ZLifestyle       -2.822e-01  1.978e-02 -14.268
    ## ZPersonal        -5.709e-02  3.013e-02  -1.895
    ## ZPrudential       3.065e-01  2.279e-02  13.449
    ## ZDoctor           2.490e-01  3.013e-02   8.264
    ## ZFamily           2.104e-01  1.833e-02  11.475
    ## ZAge:ZLifestyle   6.035e-02  1.978e-02   3.051
    ## ZAge:ZPersonal    3.879e-02  3.013e-02   1.288
    ## ZAge:ZPrudential  2.586e-03  2.279e-02   0.113
    ## ZAge:ZDoctor      3.382e-02  3.013e-02   1.123
    ## ZAge:ZFamily      4.613e-02  1.834e-02   2.516

    ## 
    ## Correlation matrix not shown by default, as p = 13 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

``` r
confint(Legitwarmthageissuesint, method=c("boot"), boot.type=c("basic"))
```

    ## Computing bootstrap confidence intervals ...

    ##                         2.5 %        97.5 %
    ## .sig01            0.392032224  4.683366e-01
    ## .sigma            0.772537645  8.021875e-01
    ## (Intercept)      -0.052031186  4.945370e-02
    ## Zpwarmth          0.101806862  1.979794e-01
    ## ZAge             -0.122894587 -1.490050e-02
    ## ZLifestyle       -0.322884389 -2.432843e-01
    ## ZPersonal        -0.121822447 -6.411699e-05
    ## ZPrudential       0.257791435  3.492149e-01
    ## ZDoctor           0.181061431  3.071076e-01
    ## ZFamily           0.171697738  2.460872e-01
    ## ZAge:ZLifestyle   0.018690823  1.007082e-01
    ## ZAge:ZPersonal   -0.021300338  1.000038e-01
    ## ZAge:ZPrudential -0.040460107  4.924550e-02
    ## ZAge:ZDoctor     -0.025131687  9.551172e-02
    ## ZAge:ZFamily      0.008559536  8.290399e-02

\#Legitall Full Model

``` r
Legitfull <- lmer(Zlegitall ~ Zpwarmth + ZAge + ZAge:ZLifestyle + ZLifestyle + ZAge:ZFamily + (1|id), data=alldata)
summary(Legitfull)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: Zlegitall ~ Zpwarmth + ZAge + ZAge:ZLifestyle + ZLifestyle +  
    ##     ZAge:ZFamily + (1 | id)
    ##    Data: alldata
    ## 
    ## REML criterion at convergence: 15591.2
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.7207 -0.6839  0.1352  0.7773  2.5320 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  id       (Intercept) 0.1751   0.4184  
    ##  Residual             0.7387   0.8595  
    ## Number of obs: 5928, groups:  id, 312
    ## 
    ## Fixed effects:
    ##                   Estimate Std. Error t value
    ## (Intercept)      3.292e-14  2.619e-02   0.000
    ## Zpwarmth         1.478e-01  2.679e-02   5.516
    ## ZAge            -7.055e-02  2.679e-02  -2.633
    ## ZLifestyle      -2.358e-01  1.116e-02 -21.125
    ## ZAge:ZLifestyle  3.182e-02  1.135e-02   2.802
    ## ZAge:ZFamily     3.229e-02  1.135e-02   2.844
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Zpwrmt ZAge  ZLfsty ZAg:ZL
    ## Zpwarmth    0.000                            
    ## ZAge        0.000  0.211                     
    ## ZLifestyle  0.000  0.000  0.000              
    ## ZAg:ZLfstyl 0.000  0.000  0.000 0.000        
    ## ZAge:ZFamly 0.000  0.000  0.000 0.000  0.182

``` r
confint(Legitfull, method=c("boot"), boot.type=c("basic"))
```

    ## Computing bootstrap confidence intervals ...

    ##                        2.5 %      97.5 %
    ## .sig01           0.378712052  0.45838082
    ## .sigma           0.844151960  0.87736528
    ## (Intercept)     -0.050111653  0.05217391
    ## Zpwarmth         0.094093109  0.20119419
    ## ZAge            -0.119096996 -0.01821547
    ## ZLifestyle      -0.258162534 -0.21486848
    ## ZAge:ZLifestyle  0.009443730  0.05208853
    ## ZAge:ZFamily     0.009330677  0.05342767

\#Grand z-scores for obeyall

``` r
BaselineObey <- lmer(Zobeyall ~ 1 + (1|id), data=alldata)
summary(BaselineObey)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: Zobeyall ~ 1 + (1 | id)
    ##    Data: alldata
    ## 
    ## REML criterion at convergence: 15346.3
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.5980 -0.6668  0.1249  0.7291  2.6079 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  id       (Intercept) 0.3089   0.5558  
    ##  Residual             0.6920   0.8319  
    ## Number of obs: 5928, groups:  id, 312
    ## 
    ## Fixed effects:
    ##              Estimate Std. Error t value
    ## (Intercept) 8.078e-15  3.327e-02       0

``` r
confint(BaselineObey, method=c("boot"), boot.type=c("basic"))
```

    ## Computing bootstrap confidence intervals ...

    ##                   2.5 %     97.5 %
    ## .sig01       0.50645625 0.60320461
    ## .sigma       0.81609159 0.84737163
    ## (Intercept) -0.05930949 0.06438079

30.9 % of the variability in obeyall is due to between-person
differences

\#Gender on obeyall

``` r
Obeywgender <- lmer(Zlegitall ~ Q1.4 + (1|id), data=alldata)
summary(Obeywgender)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: Zlegitall ~ Q1.4 + (1 | id)
    ##    Data: alldata
    ## 
    ## REML criterion at convergence: 16044.7
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.4295 -0.6824  0.1698  0.8132  2.1236 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  id       (Intercept) 0.2005   0.4477  
    ##  Residual             0.7988   0.8937  
    ## Number of obs: 5928, groups:  id, 312
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error t value
    ## (Intercept)  0.17501    0.10817   1.618
    ## Q1.4        -0.10037    0.05994  -1.674
    ## 
    ## Correlation of Fixed Effects:
    ##      (Intr)
    ## Q1.4 -0.966

``` r
confint(Obeywgender, method=c("boot"), boot.type=c("basic"))
```

    ## Computing bootstrap confidence intervals ...

    ##                  2.5 %     97.5 %
    ## .sig01       0.4127644 0.49180287
    ## .sigma       0.8778442 0.90996844
    ## (Intercept) -0.0474161 0.37413083
    ## Q1.4        -0.2192367 0.01982855

Gender has a statistically insignificant effect on obeyall

\#Obey with pwarmth

``` r
Obeywarmth <- lmer(Zobeyall ~ Zpwarmth + (1|id), data=alldata)
summary(Obeywarmth)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: Zobeyall ~ Zpwarmth + (1 | id)
    ##    Data: alldata
    ## 
    ## REML criterion at convergence: 15322.9
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.5879 -0.6740  0.1149  0.7278  2.6081 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  id       (Intercept) 0.2797   0.5289  
    ##  Residual             0.6920   0.8319  
    ## Number of obs: 5928, groups:  id, 312
    ## 
    ## Fixed effects:
    ##              Estimate Std. Error t value
    ## (Intercept) 1.014e-14  3.183e-02   0.000
    ## Zpwarmth    1.736e-01  3.183e-02   5.454
    ## 
    ## Correlation of Fixed Effects:
    ##          (Intr)
    ## Zpwarmth 0.000

``` r
confint(Obeywarmth, method=c("boot"), boot.type=c("basic"))
```

    ## Computing bootstrap confidence intervals ...

    ##                   2.5 %    97.5 %
    ## .sig01       0.47828478 0.5721812
    ## .sigma       0.81729822 0.8485368
    ## (Intercept) -0.05543767 0.0675351
    ## Zpwarmth     0.11103573 0.2400490

Pwarmth has a statistically significant effect on obeyall

\#Age on obeyall

``` r
ObeyAge <- lmer(Zobeyall ~ ZAge + (1|id), data=alldata)
summary(ObeyAge)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: Zobeyall ~ ZAge + (1 | id)
    ##    Data: alldata
    ## 
    ## REML criterion at convergence: 15343.4
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.6005 -0.6651  0.1137  0.7125  2.6180 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  id       (Intercept) 0.3014   0.5490  
    ##  Residual             0.6920   0.8319  
    ## Number of obs: 5928, groups:  id, 312
    ## 
    ## Fixed effects:
    ##               Estimate Std. Error t value
    ## (Intercept)  1.143e-14  3.290e-02   0.000
    ## ZAge        -9.271e-02  3.291e-02  -2.817
    ## 
    ## Correlation of Fixed Effects:
    ##      (Intr)
    ## ZAge 0.000

``` r
confint(ObeyAge, method=c("boot"), boot.type=c("basic"))
```

    ## Computing bootstrap confidence intervals ...

    ##                   2.5 %      97.5 %
    ## .sig01       0.49498039  0.59800580
    ## .sigma       0.81661612  0.84730544
    ## (Intercept) -0.06868215  0.06733710
    ## ZAge        -0.16049094 -0.02690987

Age has a statistically significant effect on obeyall.

\#Age and pwarmth on Obeyall

``` r
ObeyAgeWarmthint <- lmer(Zobeyall ~ Zpwarmth + ZAge + Zpwarmth:ZAge + (1|id), data=alldata)
summary(ObeyAgeWarmthint)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: Zobeyall ~ Zpwarmth + ZAge + Zpwarmth:ZAge + (1 | id)
    ##    Data: alldata
    ## 
    ## REML criterion at convergence: 15329.1
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.5897 -0.6808  0.1051  0.7276  2.6138 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  id       (Intercept) 0.2781   0.5273  
    ##  Residual             0.6920   0.8319  
    ## Number of obs: 5928, groups:  id, 312
    ## 
    ## Fixed effects:
    ##                Estimate Std. Error t value
    ## (Intercept)   -0.004337   0.032629  -0.133
    ## Zpwarmth       0.166591   0.033780   4.932
    ## ZAge          -0.054938   0.033138  -1.658
    ## Zpwarmth:ZAge -0.020579   0.035701  -0.576
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Zpwrmt ZAge  
    ## Zpwarmth    -0.063              
    ## ZAge        -0.046  0.253       
    ## Zpwrmth:ZAg  0.231 -0.275 -0.198

``` r
confint(ObeyAgeWarmthint, method=c("boot"), boot.type=c("basic"))
```

    ## Computing bootstrap confidence intervals ...

    ##                     2.5 %     97.5 %
    ## .sig01         0.48055601 0.57412642
    ## .sigma         0.81599505 0.84724772
    ## (Intercept)   -0.06357678 0.05483242
    ## Zpwarmth       0.10801975 0.22559121
    ## ZAge          -0.11785144 0.01040995
    ## Zpwarmth:ZAge -0.09189662 0.05424512

``` r
ObeyAgeWarmth <- lmer(Zobeyall ~ Zpwarmth + ZAge + (1|id), data=alldata)
summary(ObeyAgeWarmth)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: Zobeyall ~ Zpwarmth + ZAge + (1 | id)
    ##    Data: alldata
    ## 
    ## REML criterion at convergence: 15324.6
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -2.5903 -0.6791  0.1078  0.7274  2.6152 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  id       (Intercept) 0.2774   0.5267  
    ##  Residual             0.6920   0.8319  
    ## Number of obs: 5928, groups:  id, 312
    ## 
    ## Fixed effects:
    ##               Estimate Std. Error t value
    ## (Intercept)  1.357e-14  3.172e-02    0.00
    ## Zpwarmth     1.612e-01  3.245e-02    4.97
    ## ZAge        -5.872e-02  3.245e-02   -1.81
    ## 
    ## Correlation of Fixed Effects:
    ##          (Intr) Zpwrmt
    ## Zpwarmth 0.000        
    ## ZAge     0.000  0.211

``` r
confint(ObeyAgeWarmth, method=c("boot"), boot.type=c("basic"))
```

    ## Computing bootstrap confidence intervals ...

    ##                   2.5 %      97.5 %
    ## .sig01       0.47504739 0.573171493
    ## .sigma       0.81688826 0.848266643
    ## (Intercept) -0.05410808 0.066189941
    ## Zpwarmth     0.09319457 0.222615254
    ## ZAge        -0.11984769 0.007228581

When put together in an equation, pwarmth, Age, and their interaction
all do not have significant effects on obeyall. The interaction between
pwarmth and Age for obeyall will be dropped. Without the interaction,
pwarmth is still significant. Age and pwarmth will be kept, because Age
is nearly significant.

\#Obeyall with pwarmth, age, and legitall

``` r
ObeyAgeWarmthLegitint <- lmer(Zobeyall ~ Zpwarmth + ZAge + Zlegitall + Zlegitall:ZAge + (1|id), data=alldata)
summary(ObeyAgeWarmthLegitint)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: Zobeyall ~ Zpwarmth + ZAge + Zlegitall + Zlegitall:ZAge + (1 |  
    ##     id)
    ##    Data: alldata
    ## 
    ## REML criterion at convergence: 14129.3
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -3.11119 -0.62199  0.07987  0.72013  2.96462 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  id       (Intercept) 0.2037   0.4514  
    ##  Residual             0.5673   0.7532  
    ## Number of obs: 5928, groups:  id, 312
    ## 
    ## Fixed effects:
    ##                Estimate Std. Error t value
    ## (Intercept)    -0.00369    0.02739  -0.135
    ## Zpwarmth        0.10133    0.02804   3.614
    ## ZAge           -0.02791    0.02801  -0.996
    ## Zlegitall       0.40356    0.01105  36.524
    ## ZAge:Zlegitall -0.03629    0.01140  -3.183
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Zpwrmt ZAge   Zlgtll
    ## Zpwarmth     0.000                     
    ## ZAge        -0.001  0.209              
    ## Zlegitall    0.000 -0.058  0.028       
    ## ZAge:Zlgtll  0.042  0.003 -0.026 -0.004

``` r
confint(ObeyAgeWarmthLegitint, method=c("boot"), boot.type=c("basic"))
```

    ## Computing bootstrap confidence intervals ...

    ##                      2.5 %      97.5 %
    ## .sig01          0.40880068  0.49282878
    ## .sigma          0.73908635  0.76760435
    ## (Intercept)    -0.05599106  0.05346963
    ## Zpwarmth        0.04453211  0.16020761
    ## ZAge           -0.08755488  0.02576353
    ## Zlegitall       0.37959597  0.42570682
    ## ZAge:Zlegitall -0.05927515 -0.01492432

In this model, age does not have a significant effect on obeyall; but
the interaction between Age and Legitall is significant, as is Legitall
on its own. All three variables will be kept in the model for this
reason.

\#Obeyall with age, pwarmth, legit, and domains

``` r
ObeyAgeWarmthLegitDomain <- lmer(Zobeyall ~ Zpwarmth + ZAge + Zlegitall + Zlegitall:ZAge + ZPersonal + ZPrudential + ZDoctor + ZLifestyle + ZFamily + (1|id), data=alldata)
summary(ObeyAgeWarmthLegitDomain)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: Zobeyall ~ Zpwarmth + ZAge + Zlegitall + Zlegitall:ZAge + ZPersonal +  
    ##     ZPrudential + ZDoctor + ZLifestyle + ZFamily + (1 | id)
    ##    Data: alldata
    ## 
    ## REML criterion at convergence: 13565.2
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.5466 -0.6160  0.0685  0.6699  3.0684 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  id       (Intercept) 0.2266   0.4760  
    ##  Residual             0.5081   0.7128  
    ## Number of obs: 5928, groups:  id, 312
    ## 
    ## Fixed effects:
    ##                 Estimate Std. Error t value
    ## (Intercept)    -0.003174   0.028515  -0.111
    ## Zpwarmth        0.121680   0.029204   4.167
    ## ZAge           -0.037931   0.029172  -1.300
    ## Zlegitall       0.266124   0.011842  22.473
    ## ZPersonal      -0.043986   0.027273  -1.613
    ## ZPrudential     0.186945   0.020940   8.928
    ## ZDoctor         0.189050   0.027424   6.894
    ## ZLifestyle     -0.142259   0.018219  -7.808
    ## ZFamily         0.142677   0.016783   8.501
    ## ZAge:Zlegitall -0.031211   0.010851  -2.876
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Zpwrmt ZAge   Zlgtll ZPrsnl ZPrdnt ZDoctr ZLfsty ZFamly
    ## Zpwarmth     0.000                                                        
    ## ZAge        -0.001  0.208                                                 
    ## Zlegitall   -0.001 -0.060  0.029                                          
    ## ZPersonal   -0.001 -0.002  0.001  0.025                                   
    ## ZPrudential  0.000  0.010 -0.005 -0.173  0.816                            
    ## ZDoctor      0.000  0.006 -0.003 -0.107  0.909  0.854                     
    ## ZLifestyle  -0.001 -0.011  0.006  0.184  0.806  0.537  0.748              
    ## ZFamily     -0.001  0.009 -0.004 -0.148  0.775  0.771  0.807  0.642       
    ## ZAge:Zlgtll  0.039  0.004 -0.024 -0.014 -0.016  0.001 -0.012 -0.039 -0.028

``` r
confint(ObeyAgeWarmthLegitDomain, method=c("boot"), boot.type=c("basic"))
```

    ## Computing bootstrap confidence intervals ...

    ##                      2.5 %       97.5 %
    ## .sig01          0.43243633  0.515674117
    ## .sigma          0.69913639  0.725242380
    ## (Intercept)    -0.05988580  0.050214896
    ## Zpwarmth        0.06256167  0.180523608
    ## ZAge           -0.10071221  0.021270005
    ## Zlegitall       0.24517536  0.289921296
    ## ZPersonal      -0.09708529  0.012407534
    ## ZPrudential     0.14845355  0.227598975
    ## ZDoctor         0.13839106  0.245481129
    ## ZLifestyle     -0.17497740 -0.107868126
    ## ZFamily         0.10907737  0.177329472
    ## ZAge:Zlegitall -0.05458199 -0.009229265

All domains except for personal are significant, but it will be left in
the model because we want to study it.

\#Obeyall with Age, Warmth, Legit, and Personal

``` r
ObeyAgeWarmthLegitPersonal <- lmer(Zobeyall ~ Zpwarmth + ZAge*ZPersonal + Zlegitall + Zlegitall:ZAge + (1|id), data=alldata)
summary(ObeyAgeWarmthLegitPersonal)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: Zobeyall ~ Zpwarmth + ZAge * ZPersonal + Zlegitall + Zlegitall:ZAge +  
    ##     (1 | id)
    ##    Data: alldata
    ## 
    ## REML criterion at convergence: 13999.7
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.2356 -0.6050  0.0756  0.6800  2.9690 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  id       (Intercept) 0.2072   0.4552  
    ##  Residual             0.5527   0.7434  
    ## Number of obs: 5928, groups:  id, 312
    ## 
    ## Fixed effects:
    ##                 Estimate Std. Error t value
    ## (Intercept)    -0.003411   0.027546  -0.124
    ## Zpwarmth        0.104748   0.028204   3.714
    ## ZAge           -0.029706   0.028177  -1.054
    ## ZPersonal      -0.115952   0.009807 -11.823
    ## Zlegitall       0.380581   0.011083  34.338
    ## ZAge:ZPersonal  0.024725   0.009740   2.539
    ## ZAge:Zlegitall -0.033543   0.011360  -2.953
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Zpwrmt ZAge   ZPrsnl Zlgtll ZAg:ZP
    ## Zpwarmth     0.000                                   
    ## ZAge        -0.001  0.209                            
    ## ZPersonal    0.000 -0.010  0.005                     
    ## Zlegitall    0.000 -0.058  0.028  0.174              
    ## ZAge:ZPrsnl  0.005  0.000 -0.003  0.002  0.007       
    ## ZAge:Zlgtll  0.042  0.003 -0.026  0.008 -0.001  0.130

``` r
confint(ObeyAgeWarmthLegitPersonal, method=c("boot"), boot.type=c("basic"))
```

    ## Computing bootstrap confidence intervals ...

    ##                       2.5 %       97.5 %
    ## .sig01          0.416836649  0.498854810
    ## .sigma          0.729388596  0.756545051
    ## (Intercept)    -0.056769960  0.050243886
    ## Zpwarmth        0.050113319  0.163628149
    ## ZAge           -0.080640953  0.022341918
    ## ZPersonal      -0.134819095 -0.096146625
    ## Zlegitall       0.359031284  0.402009312
    ## ZAge:ZPersonal  0.007718009  0.043028368
    ## ZAge:Zlegitall -0.053401581 -0.007680153

Personal and Age have a significant interaction on Obeyall

\#Obeyall with age, warmth, legit, and prudential

``` r
ObeyAgeWarmthLegitPrudential <- lmer(Zobeyall ~ Zpwarmth + ZAge*ZPrudential + Zlegitall + Zlegitall:ZAge + (1|id), data=alldata)
summary(ObeyAgeWarmthLegitPrudential)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: 
    ## Zobeyall ~ Zpwarmth + ZAge * ZPrudential + Zlegitall + Zlegitall:ZAge +  
    ##     (1 | id)
    ##    Data: alldata
    ## 
    ## REML criterion at convergence: 14135
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -3.11758 -0.62798  0.08358  0.70727  2.95558 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  id       (Intercept) 0.2040   0.4516  
    ##  Residual             0.5665   0.7527  
    ## Number of obs: 5928, groups:  id, 312
    ## 
    ## Fixed effects:
    ##                   Estimate Std. Error t value
    ## (Intercept)      -0.003543   0.027398  -0.129
    ## Zpwarmth          0.101591   0.028053   3.621
    ## ZAge             -0.028119   0.028025  -1.003
    ## ZPrudential       0.020603   0.009794   2.104
    ## Zlegitall         0.401881   0.011062  36.331
    ## ZAge:ZPrudential -0.021218   0.009788  -2.168
    ## ZAge:Zlegitall   -0.034844   0.011408  -3.054
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Zpwrmt ZAge   ZPrdnt Zlgtll ZAg:ZP
    ## Zpwarmth     0.000                                   
    ## ZAge        -0.001  0.209                            
    ## ZPrudential  0.001  0.003 -0.002                     
    ## Zlegitall    0.000 -0.058  0.028 -0.057              
    ## ZAg:ZPrdntl -0.002 -0.001  0.002 -0.001  0.014       
    ## ZAge:Zlgtll  0.042  0.003 -0.026  0.014 -0.005 -0.045

``` r
confint(ObeyAgeWarmthLegitPrudential, method=c("boot"), boot.type=c("basic"))
```

    ## Computing bootstrap confidence intervals ...

    ##                        2.5 %        97.5 %
    ## .sig01            0.41018262  0.4915060876
    ## .sigma            0.73930912  0.7671553831
    ## (Intercept)      -0.06113306  0.0510587559
    ## Zpwarmth          0.04880522  0.1566011663
    ## ZAge             -0.08459024  0.0282921030
    ## ZPrudential       0.00120562  0.0399944204
    ## Zlegitall         0.38034021  0.4236745123
    ## ZAge:ZPrudential -0.04066471 -0.0004805066
    ## ZAge:Zlegitall   -0.05556313 -0.0128645113

The interaction between age and Prudential is significant

\#Obeyall with age, warmth, legit, and doctor

``` r
ObeyAgeWarmthLegitDoctor <- lmer(Zobeyall ~ Zpwarmth + ZAge*ZDoctor + Zlegitall + Zlegitall:ZAge + (1|id), data=alldata)
summary(ObeyAgeWarmthLegitDoctor)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: Zobeyall ~ Zpwarmth + ZAge * ZDoctor + Zlegitall + Zlegitall:ZAge +  
    ##     (1 | id)
    ##    Data: alldata
    ## 
    ## REML criterion at convergence: 13977.1
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.3860 -0.6493  0.0565  0.6904  3.0587 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  id       (Intercept) 0.2084   0.4565  
    ##  Residual             0.5503   0.7418  
    ## Number of obs: 5928, groups:  id, 312
    ## 
    ## Fixed effects:
    ##                 Estimate Std. Error t value
    ## (Intercept)    -0.002987   0.027607  -0.108
    ## Zpwarmth        0.106036   0.028267   3.751
    ## ZAge           -0.030574   0.028240  -1.083
    ## ZDoctor         0.126418   0.009868  12.810
    ## Zlegitall       0.372081   0.011155  33.357
    ## ZAge:ZDoctor   -0.023667   0.009823  -2.409
    ## ZAge:Zlegitall -0.029379   0.011460  -2.564
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Zpwrmt ZAge   ZDoctr Zlgtll ZAg:ZD
    ## Zpwarmth     0.000                                   
    ## ZAge        -0.001  0.209                            
    ## ZDoctor      0.000  0.013 -0.006                     
    ## Zlegitall    0.000 -0.058  0.028 -0.215              
    ## ZAge:ZDoctr -0.008 -0.001  0.005 -0.004  0.011       
    ## ZAge:Zlgtll  0.042  0.004 -0.026  0.012 -0.008 -0.194

``` r
confint(ObeyAgeWarmthLegitDoctor, method=c("boot"), boot.type=c("basic"))
```

    ## Computing bootstrap confidence intervals ...

    ##                      2.5 %       97.5 %
    ## .sig01          0.41549354  0.498659468
    ## .sigma          0.72787424  0.756119836
    ## (Intercept)    -0.05697747  0.046204110
    ## Zpwarmth        0.04595977  0.162395481
    ## ZAge           -0.09234815  0.025209041
    ## ZDoctor         0.10892779  0.145974487
    ## Zlegitall       0.34941272  0.394007772
    ## ZAge:ZDoctor   -0.04351220 -0.006505211
    ## ZAge:Zlegitall -0.04982239 -0.005961359

significant interaction between age and Doctor

\#Obeyall with age, warmth, legit, and Lifestyle

``` r
ObeyAgeWarmthLegitLifestyle <- lmer(Zobeyall ~ Zpwarmth + ZAge*ZLifestyle + Zlegitall + Zlegitall:ZAge + (1|id), data=alldata)
summary(ObeyAgeWarmthLegitLifestyle)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: 
    ## Zobeyall ~ Zpwarmth + ZAge * ZLifestyle + Zlegitall + Zlegitall:ZAge +  
    ##     (1 | id)
    ##    Data: alldata
    ## 
    ## REML criterion at convergence: 14051.4
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.1890 -0.6215  0.0619  0.7065  2.9068 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  id       (Intercept) 0.2078   0.4558  
    ##  Residual             0.5577   0.7468  
    ## Number of obs: 5928, groups:  id, 312
    ## 
    ## Fixed effects:
    ##                  Estimate Std. Error t value
    ## (Intercept)     -0.003422   0.027595  -0.124
    ## Zpwarmth         0.105710   0.028256   3.741
    ## ZAge            -0.030158   0.028228  -1.068
    ## ZLifestyle      -0.097244   0.010071  -9.656
    ## Zlegitall        0.374068   0.011381  32.868
    ## ZAge:ZLifestyle -0.002817   0.010081  -0.279
    ## ZAge:Zlegitall  -0.033653   0.011758  -2.862
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Zpwrmt ZAge   ZLfsty Zlgtll ZAg:ZL
    ## Zpwarmth     0.000                                   
    ## ZAge        -0.001  0.208                            
    ## ZLifestyle  -0.002 -0.016  0.009                     
    ## Zlegitall   -0.001 -0.060  0.029  0.267              
    ## ZAg:ZLfstyl  0.012  0.003 -0.008 -0.018 -0.035       
    ## ZAge:Zlgtll  0.043  0.004 -0.027 -0.036 -0.021  0.270

``` r
confint(ObeyAgeWarmthLegitLifestyle, method=c("boot"), boot.type=c("basic"))
```

    ## Computing bootstrap confidence intervals ...

    ##                       2.5 %      97.5 %
    ## .sig01           0.41123113  0.49730948
    ## .sigma           0.73260511  0.76118633
    ## (Intercept)     -0.05162440  0.05228640
    ## Zpwarmth         0.05577333  0.15712560
    ## ZAge            -0.08920846  0.02926302
    ## ZLifestyle      -0.11763346 -0.07827618
    ## Zlegitall        0.35395260  0.39471124
    ## ZAge:ZLifestyle -0.02501566  0.01615904
    ## ZAge:Zlegitall  -0.05755603 -0.01176057

There is NOT a significant effect between Age and Lifestyle

\#obeyall with Age, warmth, legit, and Family

``` r
ObeyAgeWarmthLegitFamily <- lmer(Zobeyall ~ Zpwarmth + ZAge*ZFamily + Zlegitall + Zlegitall:ZAge + (1|id), data=alldata)
summary(ObeyAgeWarmthLegitFamily)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: Zobeyall ~ Zpwarmth + ZAge * ZFamily + Zlegitall + Zlegitall:ZAge +  
    ##     (1 | id)
    ##    Data: alldata
    ## 
    ## REML criterion at convergence: 14086.2
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -3.06483 -0.65903  0.08455  0.68536  3.01395 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  id       (Intercept) 0.2057   0.4536  
    ##  Residual             0.5614   0.7493  
    ## Number of obs: 5928, groups:  id, 312
    ## 
    ## Fixed effects:
    ##                 Estimate Std. Error t value
    ## (Intercept)    -0.004324   0.027485  -0.157
    ## Zpwarmth        0.103313   0.028142   3.671
    ## ZAge           -0.028474   0.028114  -1.013
    ## ZFamily         0.071455   0.009862   7.246
    ## Zlegitall       0.389833   0.011141  34.990
    ## ZAge:ZFamily    0.024081   0.009824   2.451
    ## ZAge:Zlegitall -0.042521   0.011454  -3.712
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Zpwrmt ZAge   ZFamly Zlgtll ZAg:ZF
    ## Zpwarmth     0.000                                   
    ## ZAge        -0.001  0.209                            
    ## ZFamily     -0.001  0.009 -0.004                     
    ## Zlegitall    0.000 -0.058  0.028 -0.158              
    ## ZAge:ZFamly -0.006  0.001  0.003  0.009 -0.031       
    ## ZAge:Zlgtll  0.042  0.003 -0.026 -0.032  0.005 -0.132

``` r
confint(ObeyAgeWarmthLegitFamily, method=c("boot"), boot.type=c("basic"))
```

    ## Computing bootstrap confidence intervals ...

    ##                       2.5 %      97.5 %
    ## .sig01          0.409695697  0.49973797
    ## .sigma          0.734791686  0.76261620
    ## (Intercept)    -0.059115066  0.04705051
    ## Zpwarmth        0.050804844  0.15419288
    ## ZAge           -0.080798653  0.02401738
    ## ZFamily         0.051846586  0.09169650
    ## Zlegitall       0.367969879  0.41206126
    ## ZAge:ZFamily    0.005175613  0.04249724
    ## ZAge:Zlegitall -0.063788607 -0.01930987

Age and Family have a significant interaction

\#Obeyall with all issue domains, their interactions with age, age, and
pwarmth and legitall

``` r
ObeyAgeWarmthLegitDomainsInt <- lmer(Zobeyall ~ Zpwarmth + ZAge*ZPersonal + ZAge*ZPrudential + ZAge*ZDoctor + ZAge*ZLifestyle + ZAge*ZFamily + Zlegitall + Zlegitall:ZAge + (1|id), data=alldata)
summary(ObeyAgeWarmthLegitDomainsInt)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: Zobeyall ~ Zpwarmth + ZAge * ZPersonal + ZAge * ZPrudential +  
    ##     ZAge * ZDoctor + ZAge * ZLifestyle + ZAge * ZFamily + Zlegitall +  
    ##     Zlegitall:ZAge + (1 | id)
    ##    Data: alldata
    ## 
    ## REML criterion at convergence: 13574.8
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.5147 -0.6171  0.0625  0.6674  3.0650 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  id       (Intercept) 0.2270   0.4765  
    ##  Residual             0.5062   0.7115  
    ## Number of obs: 5928, groups:  id, 312
    ## 
    ## Fixed effects:
    ##                   Estimate Std. Error t value
    ## (Intercept)      -0.001792   0.028539  -0.063
    ## Zpwarmth          0.122164   0.029223   4.180
    ## ZAge             -0.038989   0.029193  -1.336
    ## ZPersonal        -0.044660   0.027224  -1.640
    ## ZPrudential       0.187698   0.020903   8.979
    ## ZDoctor           0.189230   0.027374   6.913
    ## ZLifestyle       -0.143805   0.018192  -7.905
    ## ZFamily           0.142591   0.016754   8.511
    ## Zlegitall         0.263552   0.011846  22.248
    ## ZAge:ZPersonal    0.002847   0.027221   0.105
    ## ZAge:ZPrudential -0.040301   0.020845  -1.933
    ## ZAge:ZDoctor     -0.036237   0.027340  -1.325
    ## ZAge:ZLifestyle   0.013664   0.018173   0.752
    ## ZAge:ZFamily      0.010237   0.016696   0.613
    ## ZAge:Zlegitall   -0.017618   0.011978  -1.471

    ## 
    ## Correlation matrix not shown by default, as p = 15 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

``` r
confint(ObeyAgeWarmthLegitDomainsInt, method=c("boot"), boot.type=c("basic"))
```

    ## Computing bootstrap confidence intervals ...

    ##                        2.5 %       97.5 %
    ## .sig01            0.43387905  0.518669377
    ## .sigma            0.69856797  0.722536892
    ## (Intercept)      -0.06449899  0.049543022
    ## Zpwarmth          0.06294333  0.176395243
    ## ZAge             -0.09703925  0.020252375
    ## ZPersonal        -0.09072258  0.007476041
    ## ZPrudential       0.15003759  0.231259756
    ## ZDoctor           0.14031204  0.242546083
    ## ZLifestyle       -0.17647791 -0.109178009
    ## ZFamily           0.11148641  0.177014754
    ## Zlegitall         0.23950585  0.285636919
    ## ZAge:ZPersonal   -0.05452217  0.054122905
    ## ZAge:ZPrudential -0.08383551 -0.001236928
    ## ZAge:ZDoctor     -0.08897303  0.014481528
    ## ZAge:ZLifestyle  -0.02151905  0.051207637
    ## ZAge:ZFamily     -0.02335958  0.042696822
    ## ZAge:Zlegitall   -0.04423216  0.009608344

Only prudential and Age have a significant interaction in the full
model; only Prudential and Doctor on their own have a significant effect
on Obeyall.

\#Obeyall full model

``` r
Obeyfull <- lmer(Zobeyall ~ Zpwarmth + ZAge + Zlegitall + Zlegitall:ZAge + (1|id), data=alldata)
summary(Obeyfull)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: Zobeyall ~ Zpwarmth + ZAge + Zlegitall + Zlegitall:ZAge + (1 |  
    ##     id)
    ##    Data: alldata
    ## 
    ## REML criterion at convergence: 14129.3
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -3.11119 -0.62199  0.07987  0.72013  2.96462 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance Std.Dev.
    ##  id       (Intercept) 0.2037   0.4514  
    ##  Residual             0.5673   0.7532  
    ## Number of obs: 5928, groups:  id, 312
    ## 
    ## Fixed effects:
    ##                Estimate Std. Error t value
    ## (Intercept)    -0.00369    0.02739  -0.135
    ## Zpwarmth        0.10133    0.02804   3.614
    ## ZAge           -0.02791    0.02801  -0.996
    ## Zlegitall       0.40356    0.01105  36.524
    ## ZAge:Zlegitall -0.03629    0.01140  -3.183
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Zpwrmt ZAge   Zlgtll
    ## Zpwarmth     0.000                     
    ## ZAge        -0.001  0.209              
    ## Zlegitall    0.000 -0.058  0.028       
    ## ZAge:Zlgtll  0.042  0.003 -0.026 -0.004

``` r
confint(Obeyfull, method=c("boot"), boot.type=c("basic"))
```

    ## Computing bootstrap confidence intervals ...

    ##                      2.5 %      97.5 %
    ## .sig01          0.40779592  0.49017292
    ## .sigma          0.73967299  0.76658357
    ## (Intercept)    -0.06453316  0.04980039
    ## Zpwarmth        0.04692413  0.15593648
    ## ZAge           -0.08191922  0.03410268
    ## Zlegitall       0.37957319  0.42548757
    ## ZAge:Zlegitall -0.05758207 -0.01361133

\#Table

``` r
library(modelsummary)
modelsummary(title = "Multi-level Model Predicting Adolescents' Beliefs about Legitimacy of Parents from Parental Warmth, issue domain, and age.",
  list("Baseline" = BaselineLegit,
  "Issue Level" = Legitwarmthage,
  "Issue & Person Levels" = Legitfull),
  stars = TRUE,
  coef_omit = c("SD"),
  gof_omit = 'RMSE|IC|Obs')
```

<table style="width:96%;">
<caption>Multi-level Model Predicting Adolescents’ Beliefs about
Legitimacy of Parents from Parental Warmth, issue domain, and age.
#Table for Obey</caption>
<colgroup>
<col style="width: 27%" />
<col style="width: 15%" />
<col style="width: 19%" />
<col style="width: 33%" />
</colgroup>
<thead>
<tr>
<th></th>
<th>Baseline</th>
<th>Issue Level</th>
<th>Issue &amp; Person Levels</th>
</tr>
</thead>
<tbody>
<tr>
<td>(Intercept)</td>
<td>0.000</td>
<td>-0.012</td>
<td>0.000</td>
</tr>
<tr>
<td></td>
<td>(0.028)</td>
<td>(0.027)</td>
<td>(0.026)</td>
</tr>
<tr>
<td>Zpwarmth</td>
<td></td>
<td>0.162***</td>
<td>0.148***</td>
</tr>
<tr>
<td></td>
<td></td>
<td>(0.028)</td>
<td>(0.027)</td>
</tr>
<tr>
<td>ZAge</td>
<td></td>
<td>-0.060*</td>
<td>-0.071**</td>
</tr>
<tr>
<td></td>
<td></td>
<td>(0.027)</td>
<td>(0.027)</td>
</tr>
<tr>
<td>Zpwarmth × ZAge</td>
<td></td>
<td>-0.057+</td>
<td></td>
</tr>
<tr>
<td></td>
<td></td>
<td>(0.029)</td>
<td></td>
</tr>
<tr>
<td>ZLifestyle</td>
<td></td>
<td></td>
<td>-0.236***</td>
</tr>
<tr>
<td></td>
<td></td>
<td></td>
<td>(0.011)</td>
</tr>
<tr>
<td>ZAge × ZLifestyle</td>
<td></td>
<td></td>
<td>0.032**</td>
</tr>
<tr>
<td></td>
<td></td>
<td></td>
<td>(0.011)</td>
</tr>
<tr>
<td>ZAge × ZFamily</td>
<td></td>
<td></td>
<td>0.032**</td>
</tr>
<tr>
<td></td>
<td></td>
<td></td>
<td>(0.011)</td>
</tr>
<tr>
<td>R2 Marg.</td>
<td>0.000</td>
<td>0.034</td>
<td>0.088</td>
</tr>
<tr>
<td>R2 Cond.</td>
<td>0.202</td>
<td>0.203</td>
<td>0.263</td>
</tr>
</tbody><tfoot>
<tr>
<td colspan="4"><ul>
<li>p &lt; 0.1, * p &lt; 0.05, ** p &lt; 0.01, *** p &lt; 0.001</li>
</ul></td>
</tr>
</tfoot>
&#10;</table>

``` r
modelsummary(title = "Multi-level Model Predicting Adolescents' Beliefs about Obligation to obey from Parental Warmth, issue domain, Perceived Parental legitimacy, and age.",
  list("Baseline" = BaselineObey,
  "Issue Level" = ObeyAgeWarmth,
  "Issue & Person Levels" = Obeyfull),
  stars = TRUE,
  coef_omit = c("SD"),
  gof_omit = 'RMSE|IC|Obs')
```

<table style="width:94%;">
<caption>Multi-level Model Predicting Adolescents’ Beliefs about
Obligation to obey from Parental Warmth, issue domain, Perceived
Parental legitimacy, and age.</caption>
<colgroup>
<col style="width: 26%" />
<col style="width: 15%" />
<col style="width: 19%" />
<col style="width: 33%" />
</colgroup>
<thead>
<tr>
<th></th>
<th>Baseline</th>
<th>Issue Level</th>
<th>Issue &amp; Person Levels</th>
</tr>
</thead>
<tbody>
<tr>
<td>(Intercept)</td>
<td>0.000</td>
<td>0.000</td>
<td>-0.004</td>
</tr>
<tr>
<td></td>
<td>(0.033)</td>
<td>(0.032)</td>
<td>(0.027)</td>
</tr>
<tr>
<td>Zpwarmth</td>
<td></td>
<td>0.161***</td>
<td>0.101***</td>
</tr>
<tr>
<td></td>
<td></td>
<td>(0.032)</td>
<td>(0.028)</td>
</tr>
<tr>
<td>ZAge</td>
<td></td>
<td>-0.059+</td>
<td>-0.028</td>
</tr>
<tr>
<td></td>
<td></td>
<td>(0.032)</td>
<td>(0.028)</td>
</tr>
<tr>
<td>Zlegitall</td>
<td></td>
<td></td>
<td>0.404***</td>
</tr>
<tr>
<td></td>
<td></td>
<td></td>
<td>(0.011)</td>
</tr>
<tr>
<td>ZAge × Zlegitall</td>
<td></td>
<td></td>
<td>-0.036**</td>
</tr>
<tr>
<td></td>
<td></td>
<td></td>
<td>(0.011)</td>
</tr>
<tr>
<td>R2 Marg.</td>
<td>0.000</td>
<td>0.033</td>
<td>0.199</td>
</tr>
<tr>
<td>R2 Cond.</td>
<td>0.309</td>
<td>0.310</td>
<td>0.411</td>
</tr>
</tbody><tfoot>
<tr>
<td colspan="4"><ul>
<li>p &lt; 0.1, * p &lt; 0.05, ** p &lt; 0.01, *** p &lt; 0.001</li>
</ul></td>
</tr>
</tfoot>
&#10;</table>

``` r
library (sjPlot)
```

    ## 
    ## Attaching package: 'sjPlot'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     set_theme

``` r
print(plot_model(Legitwarmthageissuesint, type = "int", terms = c("ZAge", "ZPersonal", "ZPrudential", "ZDoctor", "ZLifestyle", "ZFamily")))
```

    ## [[1]]

![](Hierarchical-Linear-Modeling_files/figure-gfm/unnamed-chunk-44-1.png)<!-- -->

    ## 
    ## [[2]]

![](Hierarchical-Linear-Modeling_files/figure-gfm/unnamed-chunk-44-2.png)<!-- -->

    ## 
    ## [[3]]

![](Hierarchical-Linear-Modeling_files/figure-gfm/unnamed-chunk-44-3.png)<!-- -->

    ## 
    ## [[4]]

![](Hierarchical-Linear-Modeling_files/figure-gfm/unnamed-chunk-44-4.png)<!-- -->

    ## 
    ## [[5]]

![](Hierarchical-Linear-Modeling_files/figure-gfm/unnamed-chunk-44-5.png)<!-- -->

``` r
print(plot_model(ObeyAgeWarmthLegitDomainsInt, type = "int", terms = c("ZAge", "ZPersonal", "ZPrudential", "ZDoctor", "ZLifestyle", "ZFamily")))
```

    ## [[1]]

![](Hierarchical-Linear-Modeling_files/figure-gfm/unnamed-chunk-45-1.png)<!-- -->

    ## 
    ## [[2]]

![](Hierarchical-Linear-Modeling_files/figure-gfm/unnamed-chunk-45-2.png)<!-- -->

    ## 
    ## [[3]]

![](Hierarchical-Linear-Modeling_files/figure-gfm/unnamed-chunk-45-3.png)<!-- -->

    ## 
    ## [[4]]

![](Hierarchical-Linear-Modeling_files/figure-gfm/unnamed-chunk-45-4.png)<!-- -->

    ## 
    ## [[5]]

![](Hierarchical-Linear-Modeling_files/figure-gfm/unnamed-chunk-45-5.png)<!-- -->

    ## 
    ## [[6]]

![](Hierarchical-Linear-Modeling_files/figure-gfm/unnamed-chunk-45-6.png)<!-- -->
