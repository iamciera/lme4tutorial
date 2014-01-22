# Exploring pvalue options for lme4 models
Julin Maloof
Initiated Jan 21, 2014

## The problem
lme4 does not report p-values for fixed or random effects.  The package that Dan Chitwood used, languageR, is out of date, has been shown to give unreliable results, and is picky about the lme4 version that it uses.  There are several alternatives that we will explore here.

## load packages
lme4 for model fitting.  lmerTest and car for various hypothesis testing functions.
lme4 > 1.0 is required.  If you haven't updated your R for a while you will need to.

```r
library(lme4)
```

```
## Loading required package: lattice
## Loading required package: Matrix
```

```r
library(lmerTest)
```

```
## KernSmooth 2.23 loaded
## Copyright M. P. Wand 1997-2009
## 
## Attaching package: 'lmerTest'
## 
## The following object is masked from 'package:lme4':
## 
##     lmer
## 
## The following object is masked from 'package:stats':
## 
##     step
```

```r
library(car)
```



## Read in the data
Following Dan's example, we will read in the data and transform the abs_stom trait to give it a more normal distribution

```r
stomdata <- read.delim("Modeling_example.txt")
stomdata$trans_abs_stom <- sqrt(stomdata$abs_stom)
head(stomdata)
```

```
##   plant abs_stom epi_count       il row tray col trans_abs_stom
## 1   A40     13.0      29.5 IL_4.1.1   J    A   D          3.606
## 2   C22     14.0      24.0 IL_9.3.1   B    C   A          3.742
## 3   O35     14.5      22.0 IL_7.5.5   E    L   E          3.808
## 4   Q36     15.0      25.5  IL_10.3   F    N   C          3.873
## 5    H4     15.5      28.5   IL_5.1   D    F   D          3.937
## 6   L21     15.5      34.0   IL_6.1   A    J   B          3.937
```

```r
summary(stomdata)
```

```
##      plant        abs_stom      epi_count           il           row     
##  A1     :  1   Min.   :13.0   Min.   :22.0   cvm82   : 27   B      : 75  
##  A2     :  1   1st Qu.:24.5   1st Qu.:34.5   IL_1.1.2: 10   D      : 74  
##  A20    :  1   Median :27.5   Median :37.5   IL_1.1.3: 10   E      : 74  
##  A22    :  1   Mean   :27.6   Mean   :38.0   IL_1.3  : 10   G      : 74  
##  A23    :  1   3rd Qu.:30.5   3rd Qu.:41.0   IL_1.4  : 10   I      : 73  
##  A24    :  1   Max.   :52.5   Max.   :71.0   IL_10.1 : 10   C      : 72  
##  (Other):721                                 (Other) :650   (Other):285  
##       tray     col     trans_abs_stom
##  M      : 50   A:149   Min.   :3.61  
##  E      : 48   B:131   1st Qu.:4.95  
##  G      : 48   C:147   Median :5.24  
##  J      : 48   D:152   Mean   :5.23  
##  K      : 48   E:148   3rd Qu.:5.52  
##  B      : 47           Max.   :7.25  
##  (Other):438
```


## create the full model
Next we fit the fully specified model.  Note that by adding the `arugment data=stomdata` to the lmer call that we can simplify the term specication (using names internal to the `stomdata` data frame)

```r
model1 <- lmer(trans_abs_stom ~ il + (1 | tray) + (1 | row) + (1 | col), data = stomdata)
```


## Assessing signficance of random effects
We can use `rand()` in the lmerTest package to test the signficance of the random effects.

```r
rand(model1)  #depends on lmerTest package
```

```
## Analysis of Random effects Table:
##            Chi.sq Chi.DF p.value    
## (1 | tray)  63.02      1   2e-15 ***
## (1 | row)   14.33      1   2e-04 ***
## (1 | col)    3.99      1    0.05 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


## Alternative random effects testing.
If you prefer to do this by comparing models, that also can be accomplished.  lmerTest has an updated anova function.

```r
model3 <- update(model1, . ~ . - (1 | row))  # remove the row term from the model.
anova(model1, model3)
```

```
## Data: stomdata
## Models:
## ..1: trans_abs_stom ~ il + (1 | tray) + (1 | col)
## object: trans_abs_stom ~ il + (1 | tray) + (1 | row) + (1 | col)
##        Df AIC  BIC logLik deviance Chisq Chi Df Pr(>Chisq)    
## ..1    78 925 1283   -385      769                            
## object 79 913 1275   -377      755  14.3      1    0.00015 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```



## Assessing signficance of fixed effects
The anova function from lmerTest can also be used to test the fixed effect term(s). 

```r
anova(model1)  #default is like a SAS type 3.
```

```
## Analysis of Variance Table of type 3  with  Satterthwaite 
## approximation for degrees of freedom
##    Df Sum Sq Mean Sq F value Denom  Pr(>F)    
## il 74   39.8   0.538    3.13   630 1.1e-14 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
anova(model1, type = 1)  #Alternative: SAS type 1.
```

```
## Analysis of Variance Table of type 1  with  Satterthwaite 
## approximation for degrees of freedom
##    Df Sum Sq Mean Sq F value Denom  Pr(>F)    
## il 74   39.8   0.538    3.13   631 1.1e-14 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
anova(model1, ddf = "Kenward-Roger")  #Alternative way to calculate the degrees of freedom.
```

```
## Analysis of Variance Table of type 3  with  Kenward-Roger 
## approximation for degrees of freedom
##    Df Sum Sq Mean Sq F value Denom  Pr(>F)    
## il 74   39.8   0.538    3.12   628 1.4e-14 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


## Assessing signficance of fixed effect factor levels
What if you want to test the signficance of each factor level (each IL in this case...)?  Once lmerTest has been loaded, then the summary function works like you would want it to.

```r
summary(model1)  # gives p-values for standard lme comparisions (each against the reference)
```

```
## Linear mixed model fit by REML ['merModLmerTest']
## Formula: trans_abs_stom ~ il + (1 | tray) + (1 | row) + (1 | col) 
##    Data: stomdata 
## 
## REML criterion at convergence: 917.2 
## 
## Random effects:
##  Groups   Name        Variance Std.Dev.
##  tray     (Intercept) 0.02385  0.1544  
##  row      (Intercept) 0.00740  0.0860  
##  col      (Intercept) 0.00268  0.0517  
##  Residual             0.17217  0.4149  
## Number of obs: 727, groups: tray, 16; row, 10; col, 5
## 
## Fixed effects:
##              Estimate Std. Error        df t value Pr(>|t|)    
## (Intercept)  5.32e+00   9.66e-02  1.60e+02   55.05  < 2e-16 ***
## ilIL_1.1     3.32e-02   1.63e-01  6.35e+02    0.20  0.83829    
## ilIL_1.1.2   1.56e-01   1.56e-01  6.33e+02    1.00  0.31604    
## ilIL_1.1.3  -1.26e-01   1.55e-01  6.32e+02   -0.81  0.41726    
## ilIL_1.2     3.67e-03   1.69e-01  6.30e+02    0.02  0.98264    
## ilIL_1.3    -1.30e-01   1.56e-01  6.33e+02   -0.83  0.40469    
## ilIL_1.4    -8.43e-02   1.56e-01  6.34e+02   -0.54  0.58937    
## ilIL_1.4.18  1.42e-01   1.70e-01  6.36e+02    0.84  0.40340    
## ilIL_10.1   -4.07e-02   1.55e-01  6.31e+02   -0.26  0.79333    
## ilIL_10.1.1 -1.02e-01   1.62e-01  6.32e+02   -0.63  0.52645    
## ilIL_10.2    3.49e-01   1.62e-01  6.33e+02    2.16  0.03121 *  
## ilIL_10.2.2 -9.50e-03   1.55e-01  6.29e+02   -0.06  0.95104    
## ilIL_10.3   -7.18e-01   1.55e-01  6.32e+02   -4.62  4.7e-06 ***
## ilIL_11.1    4.51e-02   1.57e-01  6.36e+02    0.29  0.77328    
## ilIL_11.2   -1.50e-01   1.61e-01  6.31e+02   -0.93  0.35206    
## ilIL_11.3   -1.16e-01   1.62e-01  6.34e+02   -0.72  0.47450    
## ilIL_11.4   -2.11e-01   1.63e-01  6.36e+02   -1.29  0.19659    
## ilIL_11.4.1 -1.96e-01   1.56e-01  6.34e+02   -1.26  0.20941    
## ilIL_12.1   -1.88e-02   1.61e-01  6.32e+02   -0.12  0.90732    
## ilIL_12.1.1  2.61e-02   1.56e-01  6.32e+02    0.17  0.86701    
## ilIL_12.2   -1.22e-01   1.56e-01  6.33e+02   -0.79  0.43271    
## ilIL_12.3   -2.73e-02   1.55e-01  6.30e+02   -0.18  0.86010    
## ilIL_12.3.1  6.64e-02   1.55e-01  6.33e+02    0.43  0.66960    
## ilIL_12.4   -9.05e-02   1.61e-01  6.31e+02   -0.56  0.57501    
## ilIL_12.4.1 -1.18e-01   1.62e-01  6.32e+02   -0.73  0.46414    
## ilIL_2.1    -3.21e-02   1.69e-01  6.32e+02   -0.19  0.84947    
## ilIL_2.1.1   2.79e-02   3.09e-01  6.34e+02    0.09  0.92824    
## ilIL_2.2    -1.51e-01   1.70e-01  6.34e+02   -0.89  0.37512    
## ilIL_2.3    -1.34e-01   1.63e-01  6.36e+02   -0.82  0.41151    
## ilIL_2.4    -1.36e-01   1.62e-01  6.35e+02   -0.83  0.40420    
## ilIL_2.5    -5.75e-01   1.62e-01  6.34e+02   -3.55  0.00042 ***
## ilIL_2.6    -2.20e-01   1.56e-01  6.33e+02   -1.41  0.15806    
## ilIL_2.6.5   4.60e-02   1.62e-01  6.34e+02    0.28  0.77630    
## ilIL_3.1    -1.50e-01   1.55e-01  6.31e+02   -0.96  0.33531    
## ilIL_3.2    -2.90e-01   1.56e-01  6.34e+02   -1.86  0.06390 .  
## ilIL_3.3    -9.24e-05   1.55e-01  6.31e+02    0.00  0.99953    
## ilIL_3.4    -1.03e-02   1.56e-01  6.33e+02   -0.07  0.94724    
## ilIL_3.5     5.53e-01   1.61e-01  6.31e+02    3.42  0.00066 ***
## ilIL_4.1     1.16e-01   1.57e-01  6.36e+02    0.74  0.45999    
## ilIL_4.1.1  -4.28e-01   1.62e-01  6.34e+02   -2.63  0.00867 ** 
## ilIL_4.2    -1.36e-02   1.57e-01  6.36e+02   -0.09  0.93080    
## ilIL_4.3     5.06e-02   1.69e-01  6.32e+02    0.30  0.76467    
## ilIL_4.3.2  -5.26e-01   1.56e-01  6.33e+02   -3.37  0.00079 ***
## ilIL_4.4     1.13e-01   1.55e-01  6.32e+02    0.73  0.46635    
## ilIL_5.1    -3.05e-02   1.55e-01  6.29e+02   -0.20  0.84387    
## ilIL_5.2    -1.85e-01   1.61e-01  6.31e+02   -1.14  0.25321    
## ilIL_5.3    -3.50e-01   1.56e-01  6.34e+02   -2.24  0.02512 *  
## ilIL_5.4     2.75e-02   1.62e-01  6.34e+02    0.17  0.86545    
## ilIL_5.5    -2.79e-01   1.56e-01  6.33e+02   -1.79  0.07366 .  
## ilIL_6.1    -5.04e-01   1.56e-01  6.34e+02   -3.23  0.00130 ** 
## ilIL_6.2     2.69e-02   1.56e-01  6.32e+02    0.17  0.86280    
## ilIL_6.3     5.32e-02   1.56e-01  6.34e+02    0.34  0.73337    
## ilIL_6.4    -3.48e-01   1.57e-01  6.37e+02   -2.22  0.02670 *  
## ilIL_7.1    -3.94e-01   1.55e-01  6.31e+02   -2.54  0.01126 *  
## ilIL_7.2    -1.29e-01   1.55e-01  6.31e+02   -0.83  0.40532    
## ilIL_7.3    -6.88e-02   1.56e-01  6.34e+02   -0.44  0.65968    
## ilIL_7.4.1  -5.32e-03   1.56e-01  6.32e+02   -0.03  0.97279    
## ilIL_7.5     7.33e-02   1.55e-01  6.30e+02    0.47  0.63746    
## ilIL_7.5.5  -8.02e-02   1.55e-01  6.32e+02   -0.52  0.60603    
## ilIL_8.1     6.66e-02   1.56e-01  6.34e+02    0.43  0.66972    
## ilIL_8.1.1  -2.21e-01   1.55e-01  6.31e+02   -1.43  0.15422    
## ilIL_8.1.5   2.88e-01   1.55e-01  6.31e+02    1.86  0.06366 .  
## ilIL_8.2     2.56e-01   1.55e-01  6.32e+02    1.65  0.09943 .  
## ilIL_8.2.1   4.46e-01   1.55e-01  6.31e+02    2.88  0.00416 ** 
## ilIL_8.3    -3.01e-01   1.56e-01  6.33e+02   -1.93  0.05444 .  
## ilIL_8.3.1  -4.26e-01   1.55e-01  6.32e+02   -2.74  0.00627 ** 
## ilIL_9.1    -5.04e-01   1.55e-01  6.32e+02   -3.24  0.00124 ** 
## ilIL_9.1.2   4.77e-02   1.62e-01  6.33e+02    0.29  0.76885    
## ilIL_9.1.3  -1.47e-01   1.56e-01  6.32e+02   -0.94  0.34579    
## ilIL_9.2     4.05e-01   1.56e-01  6.33e+02    2.60  0.00951 ** 
## ilIL_9.2.5   1.73e-01   1.69e-01  6.32e+02    1.02  0.30798    
## ilIL_9.2.6  -1.85e-01   1.56e-01  6.33e+02   -1.19  0.23487    
## ilIL_9.3    -6.03e-01   1.56e-01  6.35e+02   -3.86  0.00012 ***
## ilIL_9.3.1  -4.18e-01   1.69e-01  6.32e+02   -2.47  0.01367 *  
## ilIL_9.3.2  -2.52e-01   1.55e-01  6.31e+02   -1.62  0.10472    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```
## 
## Correlation matrix not shown by default, as p = 75 > 20.
## Use print(x, correlation=TRUE)  or
##     vcov(x)	 if you need it
```


## Making specific comparisions among factor levels
summary() compared everything to the reference level (M82 in this case).  You can use the functions in the car package to make other comparisons (ie IL vs IL).

```r
library(car)
linearHypothesis(model1, "ilIL_1.1.2 = ilIL_1.1")  #compare IL_1.1.2 to IL_1.1.  
```

```
## Linear hypothesis test
## 
## Hypothesis:
## - ilIL_1.1  + ilIL_1.1.2 = 0
## 
## Model 1: restricted model
## Model 2: trans_abs_stom ~ il + (1 | tray) + (1 | row) + (1 | col)
## 
##   Df Chisq Pr(>Chisq)
## 1                    
## 2  1  0.41       0.52
```

```r
# note that you have to use the factor names as they are listed in the
# summary table.  Hence 'ilIL_1.1.2...'

# to test the hypothesis that multiple ILs are equivalent:
linearHypothesis(model1, c("ilIL_1.1.2 - ilIL_1.1", "ilIL_9.3 - ilIL_9.3.1"))
```

```
## Linear hypothesis test
## 
## Hypothesis:
## - ilIL_1.1  + ilIL_1.1.2 = 0
## ilIL_9.3 - ilIL_9.3.1 = 0
## 
## Model 1: restricted model
## Model 2: trans_abs_stom ~ il + (1 | tray) + (1 | row) + (1 | col)
## 
##   Df Chisq Pr(>Chisq)
## 1                    
## 2  2  1.27       0.53
```

```r

# there are many other ways to specify the comparisions.  see
# ?linearHypothesis for more details.
```



## Confidence Intervals
We can calculate confidence intervals on our coefficients using functions now available in the lme4 package itself.  Two methods are shown below.

```r
model1.confint <- confint(model1)  #uses profile method.  This is a likelihood based method.  See ?confint.merMod for details.  Takes 138 seconds on 2013 Macbook Pro
```

```
## Computing profile confidence intervals ...
```

```r
model1.confint.boot <- confint(model1, method = "boot")  #bootstrapped confidence intervals. Take 130 seconds on 2013 Macbook Pro
```

```
## Computing bootstrap confidence intervals ...
```

```
## Warning: diag(.) had 0 or NA entries; non-finite result is doubtful
## Warning: some bootstrap runs failed (1/500)
```

```r

# compare them

plot(model1.confint[-1:-5, 1], model1.confint.boot[-1:-5, 1])  #pretty similar in this case.
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 



