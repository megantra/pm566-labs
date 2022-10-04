HW2
================
Megan Tran
\`October 3, 2022

``` r
library(tidytext)
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6     ✔ purrr   0.3.4
    ## ✔ tibble  3.1.8     ✔ dplyr   1.0.9
    ## ✔ tidyr   1.2.0     ✔ stringr 1.4.1
    ## ✔ readr   2.1.2     ✔ forcats 0.5.2
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(dplyr)
library(dtplyr)
library(ggplot2)
library(data.table)
```

    ## 
    ## Attaching package: 'data.table'
    ## 
    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     between, first, last
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     transpose

\#Read in the data

``` r
if (!file.exists("individual.csv")) {
download.file("https://raw.githubusercontent.com/USCbiostats/data-science-data/master/01_chs/chs_individual.csv", "individual.csv", method="libcurl", timeout = 60) 
}
ind <- data.table::fread("individual.csv")

str(ind)
```

    ## Classes 'data.table' and 'data.frame':   1200 obs. of  23 variables:
    ##  $ sid          : int  1 2 6 7 8 10 13 16 19 21 ...
    ##  $ townname     : chr  "Lancaster" "Lancaster" "Lancaster" "Lancaster" ...
    ##  $ male         : int  1 1 0 0 0 1 1 0 0 0 ...
    ##  $ race         : chr  "W" "W" "B" "O" ...
    ##  $ hispanic     : int  0 0 0 0 1 1 1 0 0 1 ...
    ##  $ agepft       : num  10.15 10.46 10.1 10.75 9.78 ...
    ##  $ height       : int  123 145 145 156 132 NA 140 141 NA 126 ...
    ##  $ weight       : int  54 77 143 72 61 NA 79 74 NA 59 ...
    ##  $ bmi          : num  16.2 16.6 30.9 13.4 15.9 ...
    ##  $ asthma       : int  0 0 0 0 0 0 0 1 0 0 ...
    ##  $ active_asthma: int  0 0 0 0 0 1 0 0 0 0 ...
    ##  $ father_asthma: int  0 0 0 NA 1 1 0 0 0 0 ...
    ##  $ mother_asthma: int  0 0 0 0 0 0 0 1 0 0 ...
    ##  $ wheeze       : int  0 1 0 1 1 0 0 1 0 0 ...
    ##  $ hayfever     : int  0 0 1 0 1 0 0 0 0 0 ...
    ##  $ allergy      : int  0 0 0 0 1 0 0 1 0 1 ...
    ##  $ educ_parent  : int  3 5 2 2 3 1 3 3 3 3 ...
    ##  $ smoke        : int  0 0 0 1 0 0 0 1 0 0 ...
    ##  $ pets         : int  1 1 0 1 1 1 1 1 1 1 ...
    ##  $ gasstove     : int  1 0 1 1 0 1 0 1 1 1 ...
    ##  $ fev          : num  1650 2273 2012 1643 1652 ...
    ##  $ fvc          : num  1800 2721 2257 2061 1996 ...
    ##  $ mmef         : num  2538 2366 1819 1462 1607 ...
    ##  - attr(*, ".internal.selfref")=<externalptr>

``` r
if (!file.exists("regional.csv")) {
download.file("https://raw.githubusercontent.com/USCbiostats/data-science-data/master/01_chs/chs_regional.csv", "regional.csv", method="libcurl", timeout = 60) 
}
reg <- data.table::fread("regional.csv")

str(reg)
```

    ## Classes 'data.table' and 'data.frame':   12 obs. of  27 variables:
    ##  $ townname   : chr  "Alpine" "Lake Elsinore" "Lake Gregory" "Lancaster" ...
    ##  $ pm25_mass  : num  8.74 12.35 7.66 8.5 5.96 ...
    ##  $ pm25_so4   : num  1.73 1.9 1.07 0.91 1.08 3.23 2.69 2.43 2.59 0.79 ...
    ##  $ pm25_no3   : num  1.59 2.98 2.07 1.87 0.73 6.22 12.2 8.66 7.2 1.38 ...
    ##  $ pm25_nh4   : num  0.88 1.36 0.91 0.78 0.41 2.57 4.25 3.14 2.71 0.61 ...
    ##  $ pm25_oc    : num  2.54 3.64 2.46 4.43 1.45 ...
    ##  $ pm25_ec    : num  0.48 0.62 0.4 0.55 0.13 1.36 1.25 0.94 1.17 0.4 ...
    ##  $ pm25_om    : num  3.04 4.36 2.96 5.32 1.74 6.25 14.2 6.32 6.71 3.97 ...
    ##  $ pm10_oc    : num  3.25 4.66 3.16 5.68 1.86 ...
    ##  $ pm10_ec    : num  0.49 0.63 0.41 0.56 0.14 1.39 1.28 0.96 1.19 0.41 ...
    ##  $ pm10_tc    : num  3.75 5.29 3.57 8.61 1.99 ...
    ##  $ formic     : num  1.03 1.18 0.66 0.88 0.34 1.57 1.9 1.72 2.77 0.74 ...
    ##  $ acetic     : num  2.49 3.56 2.36 2.88 0.75 2.94 5.14 3.92 4.24 2.11 ...
    ##  $ hcl        : num  0.41 0.46 0.28 0.22 0.33 0.73 0.46 0.47 0.55 0.31 ...
    ##  $ hno3       : num  1.98 2.63 2.28 1.8 0.43 2.67 3.33 3.43 4.07 0.97 ...
    ##  $ o3_max     : num  65.8 66.7 84.4 54.8 43.9 ...
    ##  $ o3106      : num  55 54.4 67 43.9 37.7 ...
    ##  $ o3_24      : num  41.2 32.2 57.8 32.9 28.4 ...
    ##  $ no2        : num  12.18 17.03 7.62 15.77 4.6 ...
    ##  $ pm10       : num  24.7 34.2 20.1 25 18.4 ...
    ##  $ no_24hr    : num  2.48 7.07 NA 12.68 2.05 ...
    ##  $ pm2_5_fr   : num  10.28 14.53 9.01 NA NA ...
    ##  $ iacid      : num  2.39 3.09 2.56 2.02 0.76 3.4 3.79 3.9 4.62 1.28 ...
    ##  $ oacid      : num  3.52 4.74 3.02 3.76 1.09 4.51 7.04 5.64 7.01 2.85 ...
    ##  $ total_acids: num  5.5 7.37 5.3 5.56 1.52 ...
    ##  $ lon        : num  -117 -117 -117 -118 -120 ...
    ##  $ lat        : num  32.8 33.7 34.2 34.7 34.6 ...
    ##  - attr(*, ".internal.selfref")=<externalptr>

\#Merging the 2 datasets, making sure there’s no duplicates

``` r
nrow(reg)
```

    ## [1] 12

``` r
nrow(ind)
```

    ## [1] 1200

``` r
data <-
merge(
  # Data
  x     = ind,      
  y     = reg, 
  # List of variables to match
  by.x  = "townname",
  by.y  = "townname", 
  # Which obs to keep?
  all.x = TRUE,      
  all.y = FALSE
  ) 
```

\##Data Wrangling

\#Question 1 Impute data

``` r
nrow(data)
```

    ## [1] 1200

There are still 1200 rows in the merged dataset.

\#missing values?

``` r
mean(is.na(data))
```

    ## [1] 0.02782313

Although the proportion of missing values is low, I’ll be imputing it
with the average from “male” “hispanic”.

\#impute missing values with average from “male” and “hispanic”

``` r
data[, bmi_imp := fcoalesce(bmi, mean(bmi, na.rm = TRUE)),
by = .(male,hispanic)]
```

``` r
data[, fev_imp := fcoalesce(fev, mean(fev, na.rm = TRUE)),
by = .(male,hispanic)]
```

\#Question 2 Create a new categorical variable named “obesity_level”
using the BMI measurement (underweight BMI\<14; normal BMI 14-22;
overweight BMI 22-24; obese BMI\>24). To make sure the variable is
rightly coded, create a summary table that contains the minimum BMI,
maximum BMI, and the total number of observations per category.

``` r
data$obesity_level <- as.factor(ifelse(data$bmi_imp<14, 'Underweight',
                              ifelse(data$bmi_imp>=14 & data$bmi_imp<22, 'Normal',
                              ifelse(data$bmi_imp>=22 & data$bmi_imp<=24, 'Overweight',
                              ifelse(data$bmi_imp>24, 'Obese','Missing')))))
```

``` r
data[, .(
    min_bmi      = min(bmi_imp, na.rm=TRUE),
    max_bmi   = max(bmi_imp, na.rm=TRUE), length(bmi_imp)
    ),
    by = obesity_level
    ][order(obesity_level)] 
```

    ##    obesity_level  min_bmi  max_bmi  V3
    ## 1:        Normal 14.00380 21.96387 975
    ## 2:         Obese 24.00647 41.26613 103
    ## 3:    Overweight 22.02353 23.99650  87
    ## 4:   Underweight 11.29640 13.98601  35

\#Question 3 Create another categorical variable named
“smoke_gas_exposure” that summarizes “Second Hand Smoke” and “Gas
Stove.” The variable should have four categories in total.

``` r
data$smoke_gas_exposure <- as.factor(ifelse(data$smoke==1 & data$gasstove==1, 'Both',
                                     ifelse(data$smoke==1 & data$gasstove==0, 'SmokeOnly',
                                            ifelse(data$smoke==0 & data$gasstove==1, 'GasOnly',
                                                   ifelse(data$smoke==0 & data$gasstove==0, 'None','Missing')))))
```

``` r
summary(data$smoke_gas_exposure)
```

    ##      Both   GasOnly      None SmokeOnly      NA's 
    ##       151       739       214        36        60

\#Question 4 Create four summary tables showing the average (or
proportion, if binary) and sd of “Forced expiratory volume in 1 second
(ml)” and asthma indicator by town, sex, obesity level, and
“smoke_gas_exposure.”

``` r
data[, .(
    min_bmi      = min(bmi_imp, na.rm=TRUE),
    max_bmi   = max(bmi_imp, na.rm=TRUE), length(bmi_imp)
    ),
    by = obesity_level
    ][order(obesity_level)] 
```

    ##    obesity_level  min_bmi  max_bmi  V3
    ## 1:        Normal 14.00380 21.96387 975
    ## 2:         Obese 24.00647 41.26613 103
    ## 3:    Overweight 22.02353 23.99650  87
    ## 4:   Underweight 11.29640 13.98601  35

\##Looking at the Data (EDA)

\#Question 1. Facet plot showing scatterplots with regression lines of
BMI vs FEV by “townname”.
