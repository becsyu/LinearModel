R Notebook
================
becsyu
Feb 13, 2020

This exercise covers sales and promotion data modelling, from the basic
linear regression to mixed effects model. We will go over them one by
one. Please note that here the data is hypothetical, and for academic
use only. Due to copyright restriction I cannot share the datasets with
anyone outside McCombs Business School.

Loading in sales data of a local store 50 weeks before and 50 weeks
after the opening of a Walmart in the area.

``` r
setwd("~/Downloads")
wm.dat = read.csv("Walmart_Data.csv", header = T)
str(wm.dat)
```

    ## 'data.frame':    100 obs. of  6 variables:
    ##  $ week     : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ Sales    : int  586953 838022 861991 767198 777392 725924 701517 1027152 755625 445967 ...
    ##  $ Promotion: num  0.89 1.08 0.95 1.06 1.01 1.07 1.22 1.06 1.08 0.8 ...
    ##  $ Feature  : num  0.87 0.84 1.12 0.95 1.06 1.09 1.03 1.08 0.99 0.88 ...
    ##  $ Walmart  : Factor w/ 2 levels "No","Present": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ Holiday  : Factor w/ 2 levels "No","Yes": 1 1 1 1 1 1 1 1 1 1 ...

``` r
summary(wm.dat)
```

    ##       week            Sales           Promotion        Feature     
    ##  Min.   :  1.00   Min.   : 299359   Min.   :0.790   Min.   :0.780  
    ##  1st Qu.: 25.75   1st Qu.: 512626   1st Qu.:0.940   1st Qu.:0.940  
    ##  Median : 50.50   Median : 610754   Median :1.010   Median :1.015  
    ##  Mean   : 50.50   Mean   : 644054   Mean   :1.011   Mean   :1.007  
    ##  3rd Qu.: 75.25   3rd Qu.: 722809   3rd Qu.:1.062   3rd Qu.:1.080  
    ##  Max.   :100.00   Max.   :1267301   Max.   :1.330   Max.   :1.260  
    ##     Walmart   Holiday 
    ##  No     :50   No :92  
    ##  Present:50   Yes: 8  
    ##                       
    ##                       
    ##                       
    ## 

We will make a new variable called “logSales”, for a better linear fit.

``` r
wm.dat$logSales = log(wm.dat$Sales)
```

To compute our first linear model, it is important to understand the
correlation of each variable here - this is to include any interactions
in our model later. The variables of “Sales”, “Promotion”, and “Feature”
are stored in the 2,3,4th columns.

``` r
cor(wm.dat[,2:4])
```

    ##               Sales  Promotion    Feature
    ## Sales     1.0000000 0.37739562 0.22438793
    ## Promotion 0.3773956 1.00000000 0.06513678
    ## Feature   0.2243879 0.06513678 1.00000000

Scatterplot of Sales and Promotion, and Sales and
Feature:

``` r
plot(wm.dat$Sales,wm.dat$Promotion)
```

![](Linear-Models-Practice-1_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->
Similarly, for Sales and
Feature

``` r
plot(wm.dat$Sales,wm.dat$Feature)
```

![](Linear-Models-Practice-1_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->
We notice the heterogenecity here in both plots. We will consider these
interactions later.

Now we will use the simplest linear model to test out:

``` r
wm.lm1 = lm(logSales~Promotion+Feature+Walmart+Holiday, data=wm.dat)
summary(wm.lm1)
```

    ## 
    ## Call:
    ## lm(formula = logSales ~ Promotion + Feature + Walmart + Holiday, 
    ##     data = wm.dat)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.45435 -0.15761 -0.00412  0.12948  0.46955 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    11.85276    0.28826  41.119  < 2e-16 ***
    ## Promotion       0.84754    0.20635   4.107 8.48e-05 ***
    ## Feature         0.75076    0.20774   3.614 0.000485 ***
    ## WalmartPresent -0.31127    0.04233  -7.354 6.76e-11 ***
    ## HolidayYes      0.26004    0.07765   3.349 0.001164 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.21 on 95 degrees of freedom
    ## Multiple R-squared:  0.5206, Adjusted R-squared:  0.5004 
    ## F-statistic: 25.79 on 4 and 95 DF,  p-value: 1.76e-14

The estimated coefficients correspond to lift/effect each parameter has
on the log of Sales of the store (a percentage lift). The coefficient of
Promotion is 0.84754, meaning when promotion is up by 1%, Sales goes up
by EXP(0.84754)
    %.

``` r
round(exp(wm.lm1$coefficients),4)
```

    ##    (Intercept)      Promotion        Feature WalmartPresent     HolidayYes 
    ##    140471.6063         2.3339         2.1186         0.7325         1.2970

Similarly, when Feature goes up by 1%, Sales goes up by EXP(0.75076) %.
When Warmart is in town, Sales decrease by EXP(-0.31127) %. Finally,
when HolidayYes is “switched on”, meaning it is a holiday, Sales goes up
by EXP(0.26004) %. All numbers are shown above.

Now we include the interactions between parameters to account for the
covariance we observed
earlier:

``` r
wm.lm2 = lm(logSales~Promotion+Feature+Walmart+Holiday+Holiday:Walmart+Holiday:Promotion, data = wm.dat)
summary(wm.lm2)
```

    ## 
    ## Call:
    ## lm(formula = logSales ~ Promotion + Feature + Walmart + Holiday + 
    ##     Holiday:Walmart + Holiday:Promotion, data = wm.dat)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.44745 -0.14350  0.00013  0.11836  0.47639 
    ## 
    ## Coefficients:
    ##                           Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                11.9169     0.2994  39.806  < 2e-16 ***
    ## Promotion                   0.7454     0.2236   3.333  0.00123 ** 
    ## Feature                     0.7828     0.2099   3.729  0.00033 ***
    ## WalmartPresent             -0.2978     0.0439  -6.783 1.08e-09 ***
    ## HolidayYes                 -0.1128     0.7428  -0.152  0.87961    
    ## WalmartPresent:HolidayYes  -0.1307     0.1887  -0.693  0.49034    
    ## Promotion:HolidayYes        0.4330     0.6741   0.642  0.52219    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.2101 on 93 degrees of freedom
    ## Multiple R-squared:  0.5302, Adjusted R-squared:  0.4999 
    ## F-statistic: 17.49 on 6 and 93 DF,  p-value: 1.866e-13

``` r
round(exp(wm.lm2$coefficients),4)
```

    ##               (Intercept)                 Promotion 
    ##               149777.6972                    2.1073 
    ##                   Feature            WalmartPresent 
    ##                    2.1876                    0.7425 
    ##                HolidayYes WalmartPresent:HolidayYes 
    ##                    0.8933                    0.8775 
    ##      Promotion:HolidayYes 
    ##                    1.5420

``` r
AIC(wm.lm1) 
```

    ## [1] -21.45434

``` r
AIC(wm.lm2)
```

    ## [1] -19.48443

``` r
BIC(wm.lm1)
```

    ## [1] -5.823324

``` r
BIC(wm.lm2)
```

    ## [1] 1.356929

Model comparison verdict: first model wins. Second model has too many
parameters (penalized here by BIC), and some are not even significant\!

Another popular model is using random effects and hierarchical linear
models. They are best suited when we have repeated obversations of the
same ID, e.g. customer’s all recent transactions. We will use a credit
card company data.

The credit card company would like to figure out whether offering more
promotions (for example, gasoline rebates and coupons for using the
credit card) to their existing customers can increase the
share-of-wallet of the credit card (that is, the share of a consumer’s
monthly spending using the credit card in her total spending). The
company would also like to figure out what customer characteristics make
them more responsive to promotions. The company conducted a field
experiment by randomly selecting 300 customers and offering them
different monthly promotions for 12 months. The share-of-wallet data
were recorded in each month for every customer. The data set also
included some consumer characteristics.

``` r
setwd("~/Downloads")
sow = read.csv("CreditCard_SOW_data.csv", header = T)
summary(sow)
```

    ##    ConsumerID        History          Income        WalletShare    
    ##  Min.   :  1.00   Min.   : 6.00   Min.   : 33000   Min.   :0.0530  
    ##  1st Qu.: 75.75   1st Qu.:21.00   1st Qu.: 66000   1st Qu.:0.3480  
    ##  Median :150.50   Median :32.00   Median : 82000   Median :0.4650  
    ##  Mean   :150.50   Mean   :33.48   Mean   : 83847   Mean   :0.4529  
    ##  3rd Qu.:225.25   3rd Qu.:48.00   3rd Qu.: 98000   3rd Qu.:0.5660  
    ##  Max.   :300.00   Max.   :60.00   Max.   :167000   Max.   :0.7710  
    ##    Promotion         Balance    
    ##  Min.   :0.0000   Min.   :   0  
    ##  1st Qu.:0.2750   1st Qu.: 583  
    ##  Median :0.6500   Median :1332  
    ##  Mean   :0.6167   Mean   :1538  
    ##  3rd Qu.:0.9250   3rd Qu.:2274  
    ##  Max.   :1.2000   Max.   :6276

``` r
str(sow)
```

    ## 'data.frame':    3600 obs. of  6 variables:
    ##  $ ConsumerID : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ History    : int  55 55 55 55 55 55 55 55 55 55 ...
    ##  $ Income     : num  82000 82000 82000 82000 82000 82000 82000 82000 82000 82000 ...
    ##  $ WalletShare: num  0.643 0.628 0.567 0.638 0.554 0.573 0.666 0.649 0.527 0.459 ...
    ##  $ Promotion  : num  0.5 0.2 1 0.8 0.7 1.1 0.9 0.6 0.1 0 ...
    ##  $ Balance    : int  836 467 1208 792 1215 1248 197 567 1190 1709 ...

ConsumerID here should not be int., but rather a key, an ID that
indicates a level.

``` r
sow$ConsumerID = as.factor(sow$ConsumerID)
```

Now we add in logIncome and logSowRatio for calculus purpose.

``` r
sow$logIncome = log(sow$Income)
sow$logSowRatio = log(sow$WalletShare/(1-sow$WalletShare))
str(sow)
```

    ## 'data.frame':    3600 obs. of  8 variables:
    ##  $ ConsumerID : Factor w/ 300 levels "1","2","3","4",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ History    : int  55 55 55 55 55 55 55 55 55 55 ...
    ##  $ Income     : num  82000 82000 82000 82000 82000 82000 82000 82000 82000 82000 ...
    ##  $ WalletShare: num  0.643 0.628 0.567 0.638 0.554 0.573 0.666 0.649 0.527 0.459 ...
    ##  $ Promotion  : num  0.5 0.2 1 0.8 0.7 1.1 0.9 0.6 0.1 0 ...
    ##  $ Balance    : int  836 467 1208 792 1215 1248 197 567 1190 1709 ...
    ##  $ logIncome  : num  11.3 11.3 11.3 11.3 11.3 ...
    ##  $ logSowRatio: num  0.588 0.524 0.27 0.567 0.217 ...

Build our first linear
model:

``` r
sow.lm1 = lm(logSowRatio~History+Balance+Promotion+History:Promotion+logIncome:Promotion, data = sow)
summary(sow.lm1)
```

    ## 
    ## Call:
    ## lm(formula = logSowRatio ~ History + Balance + Promotion + History:Promotion + 
    ##     logIncome:Promotion, data = sow)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.59976 -0.14401  0.00153  0.13634  0.75883 
    ## 
    ## Coefficients:
    ##                       Estimate Std. Error  t value Pr(>|t|)    
    ## (Intercept)          8.908e-02  1.603e-02    5.558 2.92e-08 ***
    ## History              1.039e-02  4.153e-04   25.027  < 2e-16 ***
    ## Balance             -4.959e-04  2.882e-06 -172.064  < 2e-16 ***
    ## Promotion            7.777e-01  1.888e-01    4.120 3.87e-05 ***
    ## History:Promotion   -2.598e-03  5.722e-04   -4.541 5.79e-06 ***
    ## Promotion:logIncome -4.558e-02  1.651e-02   -2.760  0.00581 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.2078 on 3594 degrees of freedom
    ## Multiple R-squared:  0.8984, Adjusted R-squared:  0.8982 
    ## F-statistic:  6353 on 5 and 3594 DF,  p-value: < 2.2e-16

The problem here is that this model does NOT show how each customer
behaves on their individual level. We want a hierarchy to represent both
the overall aggregated model AND each customer ID’s estimation. i, j
here are indices. E represents unexplained residues.

logSowRatioij = β0i + β1×Balanceij + β2i×Promotionij + βij

β0i = β0 +β1×Historyi +Ei

β2i = β0 +β1×Historyi +β2×logIncomei +Ei

Rearrange the equation and we get our one-level linear regression model
with random effects:

Here the parameters that are not indexed with i are fixed effects:
History, Balance, History:Promotion, logIncome:Promotion … as opposed to
those indexed with I are random effects: ConsumerID,
ConsumerID:Promotion

We use a package called
‘lme4’.

``` r
library(lme4)
```

    ## Loading required package: Matrix

``` r
sow.re1 = lmer(logSowRatio~History+Balance+Promotion+History:Promotion+logIncome:Promotion+(1+Promotion|ConsumerID), data = sow, REML = F, control = lmerControl(optimizer="Nelder_Mead"))
```

    ## Warning: Some predictor variables are on very different scales: consider
    ## rescaling

``` r
summary(sow.re1)
```

    ## Linear mixed model fit by maximum likelihood  ['lmerMod']
    ## Formula: 
    ## logSowRatio ~ History + Balance + Promotion + History:Promotion +  
    ##     logIncome:Promotion + (1 + Promotion | ConsumerID)
    ##    Data: sow
    ## Control: lmerControl(optimizer = "Nelder_Mead")
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##  -6532.1  -6470.2   3276.0  -6552.1     3590 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -3.1063 -0.6424  0.0049  0.6336  3.4532 
    ## 
    ## Random effects:
    ##  Groups     Name        Variance  Std.Dev. Corr
    ##  ConsumerID (Intercept) 0.0359421 0.18958      
    ##             Promotion   0.0005355 0.02314  0.06
    ##  Residual               0.0066071 0.08128      
    ## Number of obs: 3600, groups:  ConsumerID, 300
    ## 
    ## Fixed effects:
    ##                       Estimate Std. Error  t value
    ## (Intercept)          9.595e-02  2.655e-02    3.613
    ## History              1.039e-02  7.135e-04   14.569
    ## Balance             -5.003e-04  1.799e-06 -278.110
    ## Promotion            6.129e-01  1.466e-01    4.181
    ## History:Promotion   -2.571e-03  2.402e-04  -10.703
    ## Promotion:logIncome -3.110e-02  1.288e-02   -2.414
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) Histry Balanc Promtn Hstr:P
    ## History     -0.900                            
    ## Balance     -0.107 -0.001                     
    ## Promotion   -0.011  0.009  0.013              
    ## Hstry:Prmtn  0.143 -0.159 -0.002 -0.153       
    ## Prmtn:lgInc  0.001  0.000 -0.012 -0.998  0.099
    ## fit warnings:
    ## Some predictor variables are on very different scales: consider rescaling

The interpretation of the coefficients here would be: History, Promotion
have a positive effect on the sow score, while Balance,
History:Promotion, and Promotion:logIncome have a negative effect on the
sow score. This means, the longer history the customer has with the
company, the more promotion he/she receives, the more likely he/she will
use the credit card. The more balance he/she has on the card, the less
likely he/she will use the card - this makes sence, since credit card
balance is a debt you owe the company. However, promotion might tire a
customer out if it’s repeated too many time, hence History:Promotion has
a negative effect on the sow score. Promotion also seems to have a
negative effect when it’s couple with logIncome. This might be
interpreted as higher income customers perceive promotions negatively
wherreas lower income customers are more promotion-sensitive.

Now let’s look at AIC/BIC to see which model explains better.

``` r
AIC(sow.lm1)
```

    ## [1] -1087.389

``` r
AIC(sow.re1)
```

    ## [1] -6532.094

``` r
BIC(sow.lm1)
```

    ## [1] -1044.069

``` r
BIC(sow.re1)
```

    ## [1] -6470.207

Linear mixed effect is a much better model here\!
