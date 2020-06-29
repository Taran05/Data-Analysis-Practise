---
title: "Data analysis practice"
author: "Taranjot Singh"
date: "30/06/2020"
output: html_document
---

# MAJOR COLLEGE ADMISSIONS ANALYSIS

#### The aim of this assignment is to study how income varies across college major categories. Specifically answer: “Is there an association between college major category and income?”

#### To get started, start a new R/RStudio session with a clean workspace. To do this in R, you can use the q() function to quit, then reopen R. The easiest way to do this in RStudio is to quit RStudio entirely and reopen it. After you have started a new session, run the following commands. This will load a data.frame called college for you to work with:


```r
college <- read.csv("https://query.data.world/s/uieteyrze67twkiujwxffsokaml44y", header=TRUE, stringsAsFactors=FALSE)
```

#### Please upload this college_major_analysis.rds file to a public GitHub repository. In question 4 of this quiz, you will share the link to this file.

## Codebook

#### The following code book describes the variables in the “college_major_analysis.rds” dataset:


```r
head(college)
```

```
##   Major_code                                 Major                  Major_category  Total Employed Employed_full_time_year_round Unemployed
## 1       1100                   GENERAL AGRICULTURE Agriculture & Natural Resources 128148    90245                         74078       2423
## 2       1101 AGRICULTURE PRODUCTION AND MANAGEMENT Agriculture & Natural Resources  95326    76865                         64240       2266
## 3       1102                AGRICULTURAL ECONOMICS Agriculture & Natural Resources  33955    26321                         22810        821
## 4       1103                       ANIMAL SCIENCES Agriculture & Natural Resources 103549    81177                         64937       3619
## 5       1104                          FOOD SCIENCE Agriculture & Natural Resources  24280    17281                         12722        894
## 6       1105            PLANT SCIENCE AND AGRONOMY Agriculture & Natural Resources  79409    63043                         51077       2070
##   Unemployment_rate Median P25th P75th
## 1        0.02614711  50000 34000 80000
## 2        0.02863606  54000 36000 80000
## 3        0.03024832  63000 40000 98000
## 4        0.04267890  46000 30000 72000
## 5        0.04918845  62000 38500 90000
## 6        0.03179089  50000 35000 75000
```

```r
summary(college)
```

```
##    Major_code      Major           Major_category         Total            Employed       Employed_full_time_year_round   Unemployed    
##  Min.   :1100   Length:173         Length:173         Min.   :   2396   Min.   :   1492   Min.   :   1093               Min.   :     0  
##  1st Qu.:2403   Class :character   Class :character   1st Qu.:  24280   1st Qu.:  17281   1st Qu.:  12722               1st Qu.:  1101  
##  Median :3608   Mode  :character   Mode  :character   Median :  75791   Median :  56564   Median :  39613               Median :  3619  
##  Mean   :3880                                         Mean   : 230257   Mean   : 166162   Mean   : 126308               Mean   :  9725  
##  3rd Qu.:5503                                         3rd Qu.: 205763   3rd Qu.: 142879   3rd Qu.: 111025               3rd Qu.:  8862  
##  Max.   :6403                                         Max.   :3123510   Max.   :2354398   Max.   :1939384               Max.   :147261  
##  Unemployment_rate     Median           P25th           P75th       
##  Min.   :0.00000   Min.   : 35000   Min.   :24900   Min.   : 45800  
##  1st Qu.:0.04626   1st Qu.: 46000   1st Qu.:32000   1st Qu.: 70000  
##  Median :0.05472   Median : 53000   Median :36000   Median : 80000  
##  Mean   :0.05736   Mean   : 56816   Mean   :38697   Mean   : 82506  
##  3rd Qu.:0.06904   3rd Qu.: 65000   3rd Qu.:42000   3rd Qu.: 95000  
##  Max.   :0.15615   Max.   :125000   Max.   :78000   Max.   :210000
```

#### The first column pertaining to majors is major codes and I have ignored this the second colum is major (or name of the majors). There are 173 different university Majors in this category. I have not used this as the independent variable either. I have used major category in order to develop the univariate regression analyisis. I have enclosed a no simple histogram of median incomes of graduates in order to illustrate the distribution of graduate incomes.


```r
as.numeric(college$Median)
```

```
##   [1]  50000  54000  63000  46000  62000  50000  63000  52000  52000  58000  52000  63000  46000  50000  50000  48000  50000  50000  65000  60000  78000
##  [22]  68000  55000  55000  40000  43000  58000  41000  40000  43000  48400  35300  46000  45000  42000  45000  40000  42000  42600  50000  75000  80000
##  [43]  62000  78000  65000  86000  78000  80000  88000  65000  70000  85000  75000  78000  80000  96000  92000  97000  95000 125000  70000  63000  74000
##  [64]  67000  70000  60000  63000  48000  48000  45000  40500  50000  48000  50000  40000  50000  46700  40000  51000  53000  50000  45000  47500  48000
##  [85]  60000  60000  50000  55000  35000  52000  66000  70000  70000  64000  43000  45000  49500  92000  53000  45000  44000  45000  40000  60000  80000
## [106]  60000  59000  65000  57000  55000  70000  75000  56000  62000  45000  40000  45000  39000  62000  47000  45000  50000  56000  60000  38000  40000
## [127]  50000  69000  43000  49000  54000  55000  58000  47000  52000  65000  48000  67000  45000  42000  45000  40000  46600  47000  44500  37600  45000
## [148]  50000  42000  50000  55000  60000  50000  62000 106000  61000  47000  45000  60000  65000  72000  58000  65000  65000  56000  65000  54000  54000
## [169]  49000  72000  53000  50000  50000
```


```r
hist(college$Median, xlab="Median income ($)")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

## Minimal preprocessing


```r
## make sure median is a numeric variable and major category is a factor 
Median_income<- as.numeric(college$Median)
Major_cat<- as.factor(college$Major_category)
##perform univariate analysis
fit<- lm(Median_income~Major_cat)
summary(fit)
```

```
## 
## Call:
## lm(formula = Median_income ~ Major_cat)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -17759  -5000   -831   3542  49542 
## 
## Coefficients:
##                                              Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                                     55000       3044  18.071  < 2e-16 ***
## Major_catArts                                  -11475       4565  -2.514  0.01296 *  
## Major_catBiology & Life Science                 -4179       3985  -1.049  0.29597    
## Major_catBusiness                                5615       4048   1.387  0.16737    
## Major_catCommunications & Journalism            -5500       5694  -0.966  0.33555    
## Major_catComputers & Mathematics                11273       4205   2.681  0.00813 ** 
## Major_catEducation                             -11169       3880  -2.879  0.00455 ** 
## Major_catEngineering                            22759       3529   6.448 1.33e-09 ***
## Major_catHealth                                  1458       4121   0.354  0.72390    
## Major_catHumanities & Liberal Arts              -8920       3929  -2.270  0.02455 *  
## Major_catIndustrial Arts & Consumer Services    -2357       4743  -0.497  0.61990    
## Major_catInterdisciplinary                     -12000      10094  -1.189  0.23631    
## Major_catLaw & Public Policy                    -2200       5272  -0.417  0.67700    
## Major_catPhysical Sciences                       7400       4304   1.719  0.08753 .  
## Major_catPsychology & Social Work              -10444       4422  -2.362  0.01941 *  
## Major_catSocial Science                         -1778       4422  -0.402  0.68821    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 9624 on 157 degrees of freedom
## Multiple R-squared:  0.6091,	Adjusted R-squared:  0.5717 
## F-statistic: 16.31 on 15 and 157 DF,  p-value: < 2.2e-16
```

## Conclusions

#### This is a simplified assessment there is a statistically significant relationship between the category of college major and the graduate income (adjusted R squared=0.5717). There are some pertinent columns in this dataset which have not been used such as the percentage of different types of jobs(low income jobs, college jobs and non-college jobs) and percentage unemployed as well as the percentage of women taking that major category which may be effected by gender paygap. so multivariable analysis may result in a different result. But it cant be completed in 15 minutes.

