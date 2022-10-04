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

``` r
nrow(data)
```

    ## [1] 1200

There are still 1200 rows in the merged dataset.

\#missing data?

``` r
!is.na(data)
```

    ##         townname  sid male race hispanic agepft height weight   bmi asthma
    ##    [1,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##    [2,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##    [3,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##    [4,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##    [5,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##    [6,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##    [7,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##    [8,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE  FALSE
    ##    [9,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [10,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [11,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [12,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [13,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [14,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [15,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [16,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [17,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [18,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [19,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [20,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##   [21,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [22,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [23,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [24,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [25,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [26,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [27,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [28,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [29,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [30,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [31,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [32,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [33,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [34,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [35,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [36,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [37,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [38,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [39,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [40,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [41,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [42,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [43,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [44,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [45,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [46,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [47,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [48,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [49,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [50,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [51,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [52,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [53,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [54,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [55,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [56,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##   [57,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [58,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [59,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [60,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [61,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [62,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [63,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [64,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [65,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [66,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [67,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [68,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [69,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE  FALSE
    ##   [70,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [71,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [72,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [73,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [74,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [75,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [76,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [77,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [78,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [79,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [80,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [81,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [82,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [83,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [84,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [85,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [86,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [87,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [88,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [89,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [90,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [91,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [92,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [93,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE  FALSE
    ##   [94,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [95,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [96,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##   [97,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##   [98,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##   [99,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [100,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [101,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [102,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [103,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [104,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [105,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [106,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [107,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [108,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [109,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [110,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [111,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [112,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [113,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [114,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE  FALSE
    ##  [115,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [116,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [117,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [118,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [119,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [120,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [121,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [122,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [123,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [124,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [125,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [126,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [127,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [128,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [129,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [130,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [131,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [132,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [133,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [134,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [135,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [136,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [137,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [138,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [139,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [140,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [141,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [142,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [143,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [144,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [145,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [146,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [147,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [148,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [149,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [150,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [151,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [152,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [153,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [154,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [155,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [156,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [157,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [158,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [159,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [160,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [161,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [162,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [163,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [164,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [165,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [166,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [167,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [168,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [169,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [170,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [171,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [172,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [173,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [174,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [175,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [176,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [177,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [178,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [179,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [180,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [181,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [182,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [183,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [184,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [185,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [186,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE  FALSE
    ##  [187,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [188,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [189,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [190,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [191,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [192,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [193,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [194,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [195,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [196,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [197,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [198,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [199,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [200,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [201,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [202,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [203,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [204,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [205,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [206,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [207,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [208,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE  FALSE
    ##  [209,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [210,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [211,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [212,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [213,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [214,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [215,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [216,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [217,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [218,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [219,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [220,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [221,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [222,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [223,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [224,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [225,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE  FALSE
    ##  [226,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [227,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [228,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [229,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [230,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [231,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [232,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [233,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [234,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE  FALSE
    ##  [235,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [236,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [237,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [238,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [239,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [240,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [241,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [242,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [243,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [244,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [245,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE  FALSE
    ##  [246,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [247,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [248,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [249,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [250,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [251,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [252,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [253,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [254,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [255,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [256,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [257,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [258,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [259,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [260,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [261,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [262,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [263,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [264,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [265,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [266,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [267,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [268,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [269,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [270,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [271,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [272,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [273,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [274,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [275,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [276,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [277,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [278,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [279,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [280,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [281,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [282,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [283,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE  FALSE
    ##  [284,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [285,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [286,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [287,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [288,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [289,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [290,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [291,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [292,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [293,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [294,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [295,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [296,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [297,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [298,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [299,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [300,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [301,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [302,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [303,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [304,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [305,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [306,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [307,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [308,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [309,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [310,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [311,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [312,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [313,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [314,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [315,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [316,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [317,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [318,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [319,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [320,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [321,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [322,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [323,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [324,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [325,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [326,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [327,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [328,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [329,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [330,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [331,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [332,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [333,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [334,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [335,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [336,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [337,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [338,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [339,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE  FALSE
    ##  [340,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [341,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [342,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [343,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [344,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [345,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [346,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [347,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [348,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [349,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [350,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [351,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [352,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [353,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [354,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [355,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [356,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [357,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [358,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [359,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [360,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [361,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [362,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [363,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [364,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [365,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [366,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [367,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [368,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [369,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [370,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [371,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [372,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [373,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [374,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [375,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [376,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [377,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [378,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [379,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [380,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [381,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [382,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [383,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [384,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [385,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [386,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [387,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [388,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [389,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [390,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [391,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [392,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [393,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [394,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [395,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [396,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [397,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [398,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [399,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [400,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [401,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [402,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [403,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [404,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [405,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [406,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [407,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [408,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [409,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [410,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [411,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [412,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [413,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [414,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [415,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [416,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [417,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [418,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [419,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [420,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [421,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [422,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [423,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [424,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [425,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [426,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [427,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [428,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [429,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [430,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [431,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [432,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [433,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [434,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [435,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [436,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [437,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [438,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [439,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [440,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [441,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [442,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [443,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [444,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [445,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [446,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [447,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [448,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [449,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [450,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [451,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE  FALSE
    ##  [452,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [453,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [454,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [455,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [456,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [457,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [458,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [459,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [460,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [461,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [462,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [463,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [464,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [465,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [466,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [467,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [468,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [469,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [470,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [471,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [472,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [473,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [474,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE  FALSE
    ##  [475,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [476,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [477,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [478,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [479,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [480,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [481,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [482,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [483,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [484,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [485,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [486,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [487,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [488,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE  FALSE
    ##  [489,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [490,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [491,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [492,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [493,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [494,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [495,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [496,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [497,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [498,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [499,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [500,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [501,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [502,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [503,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [504,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [505,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [506,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [507,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [508,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [509,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [510,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [511,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [512,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [513,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [514,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [515,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [516,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [517,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [518,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [519,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [520,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [521,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [522,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [523,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [524,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [525,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [526,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [527,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [528,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [529,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [530,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [531,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [532,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [533,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [534,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [535,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [536,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [537,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [538,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [539,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [540,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [541,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [542,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE  FALSE
    ##  [543,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [544,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [545,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [546,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [547,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [548,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [549,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [550,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE  FALSE
    ##  [551,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [552,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [553,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [554,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [555,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [556,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [557,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [558,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [559,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [560,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [561,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [562,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [563,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [564,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [565,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [566,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [567,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [568,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [569,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [570,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE  FALSE
    ##  [571,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [572,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [573,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [574,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [575,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [576,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [577,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [578,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [579,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [580,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [581,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [582,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [583,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [584,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [585,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [586,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [587,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [588,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [589,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [590,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [591,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [592,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [593,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [594,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [595,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [596,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [597,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [598,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [599,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [600,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [601,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [602,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE  FALSE
    ##  [603,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [604,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [605,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [606,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [607,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [608,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [609,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [610,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [611,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [612,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [613,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [614,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [615,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [616,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [617,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [618,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [619,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [620,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [621,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [622,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [623,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [624,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [625,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [626,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [627,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [628,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [629,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [630,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [631,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [632,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [633,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [634,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE  FALSE
    ##  [635,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [636,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [637,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [638,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [639,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE  FALSE
    ##  [640,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [641,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [642,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [643,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [644,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [645,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [646,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [647,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [648,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [649,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [650,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [651,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [652,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [653,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [654,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [655,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [656,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [657,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [658,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [659,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [660,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [661,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [662,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [663,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [664,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [665,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [666,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [667,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [668,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [669,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [670,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [671,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [672,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [673,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [674,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [675,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [676,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [677,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [678,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [679,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [680,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [681,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [682,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [683,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [684,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [685,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [686,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [687,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [688,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [689,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [690,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [691,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [692,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE  FALSE
    ##  [693,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [694,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [695,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [696,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [697,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [698,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [699,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [700,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [701,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [702,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [703,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [704,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [705,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE  FALSE
    ##  [706,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [707,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [708,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [709,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [710,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [711,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [712,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [713,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [714,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [715,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [716,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [717,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [718,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [719,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [720,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [721,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [722,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [723,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [724,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [725,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [726,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [727,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [728,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [729,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [730,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [731,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [732,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [733,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [734,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [735,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [736,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [737,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [738,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [739,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [740,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [741,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [742,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [743,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [744,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [745,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [746,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [747,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [748,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [749,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [750,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [751,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [752,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [753,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [754,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [755,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [756,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [757,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [758,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [759,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [760,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [761,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [762,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [763,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [764,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [765,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [766,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [767,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [768,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [769,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [770,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [771,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [772,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [773,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [774,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [775,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE  FALSE
    ##  [776,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [777,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [778,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [779,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [780,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [781,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [782,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [783,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [784,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE  FALSE
    ##  [785,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [786,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [787,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [788,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE  FALSE
    ##  [789,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [790,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [791,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [792,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [793,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [794,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [795,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [796,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [797,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE  FALSE
    ##  [798,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [799,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [800,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [801,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [802,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [803,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [804,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [805,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [806,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [807,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [808,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [809,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [810,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [811,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [812,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [813,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [814,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [815,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [816,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [817,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [818,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [819,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [820,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [821,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [822,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [823,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [824,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [825,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [826,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [827,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [828,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [829,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [830,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [831,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [832,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [833,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [834,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [835,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [836,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [837,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [838,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [839,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [840,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [841,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [842,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [843,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [844,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [845,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [846,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [847,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [848,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [849,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [850,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [851,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [852,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [853,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [854,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [855,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [856,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [857,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [858,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [859,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [860,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [861,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [862,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [863,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [864,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [865,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [866,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [867,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [868,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [869,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [870,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [871,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [872,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [873,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [874,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [875,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [876,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [877,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [878,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [879,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [880,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [881,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [882,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [883,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [884,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [885,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [886,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [887,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [888,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [889,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [890,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [891,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [892,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [893,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [894,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [895,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [896,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [897,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [898,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [899,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [900,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [901,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [902,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [903,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [904,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [905,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [906,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [907,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [908,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [909,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [910,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [911,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [912,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [913,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [914,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [915,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [916,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [917,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [918,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [919,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [920,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [921,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [922,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [923,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [924,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [925,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [926,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [927,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [928,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [929,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [930,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [931,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [932,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [933,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [934,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [935,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [936,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [937,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [938,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##  [939,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [940,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [941,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [942,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [943,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [944,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [945,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [946,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [947,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [948,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [949,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [950,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [951,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [952,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [953,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [954,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [955,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [956,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [957,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [958,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [959,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [960,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [961,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [962,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [963,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [964,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [965,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [966,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [967,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [968,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [969,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [970,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [971,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [972,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [973,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [974,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [975,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [976,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [977,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [978,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [979,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [980,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [981,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [982,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [983,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [984,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [985,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [986,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [987,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [988,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [989,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [990,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [991,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [992,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [993,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [994,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [995,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [996,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [997,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE  FALSE
    ##  [998,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ##  [999,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1000,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1001,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1002,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1003,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ## [1004,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1005,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE  FALSE
    ## [1006,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE  FALSE
    ## [1007,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1008,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ## [1009,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1010,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1011,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1012,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1013,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1014,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1015,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1016,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ## [1017,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1018,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1019,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1020,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1021,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1022,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1023,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1024,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1025,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1026,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1027,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1028,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1029,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1030,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1031,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1032,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1033,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1034,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1035,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1036,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1037,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1038,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1039,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1040,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1041,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1042,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1043,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1044,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1045,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1046,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1047,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1048,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1049,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1050,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ## [1051,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1052,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1053,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ## [1054,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1055,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1056,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1057,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ## [1058,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1059,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1060,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1061,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1062,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1063,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1064,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1065,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE  FALSE
    ## [1066,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1067,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1068,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1069,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1070,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1071,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1072,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1073,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1074,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1075,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1076,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1077,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1078,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1079,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1080,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1081,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1082,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1083,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1084,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1085,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1086,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1087,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1088,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1089,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1090,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ## [1091,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1092,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1093,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1094,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1095,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ## [1096,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1097,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1098,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1099,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1100,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1101,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1102,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ## [1103,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1104,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1105,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1106,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1107,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1108,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1109,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1110,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1111,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1112,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1113,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1114,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1115,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1116,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1117,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1118,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1119,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE  FALSE
    ## [1120,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1121,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1122,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1123,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1124,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1125,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1126,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1127,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1128,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1129,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1130,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1131,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1132,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1133,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1134,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1135,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1136,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1137,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1138,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ## [1139,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1140,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1141,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1142,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1143,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1144,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1145,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1146,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1147,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1148,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1149,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1150,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1151,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1152,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1153,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ## [1154,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1155,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1156,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1157,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1158,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1159,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1160,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1161,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1162,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1163,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1164,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1165,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1166,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1167,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1168,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1169,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1170,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1171,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1172,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1173,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1174,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1175,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1176,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1177,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1178,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1179,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1180,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1181,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1182,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1183,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1184,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1185,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ## [1186,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1187,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1188,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1189,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1190,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1191,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1192,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ## [1193,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ## [1194,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1195,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1196,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1197,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1198,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1199,]     TRUE TRUE TRUE TRUE     TRUE   TRUE   TRUE   TRUE  TRUE   TRUE
    ## [1200,]     TRUE TRUE TRUE TRUE     TRUE  FALSE  FALSE  FALSE FALSE   TRUE
    ##         active_asthma father_asthma mother_asthma wheeze hayfever allergy
    ##    [1,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##    [2,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##    [3,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##    [4,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##    [5,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##    [6,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##    [7,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##    [8,]          TRUE         FALSE         FALSE  FALSE    FALSE   FALSE
    ##    [9,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [10,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [11,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [12,]          TRUE         FALSE          TRUE   TRUE     TRUE   FALSE
    ##   [13,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [14,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [15,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ##   [16,]          TRUE         FALSE          TRUE   TRUE     TRUE    TRUE
    ##   [17,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [18,]          TRUE         FALSE          TRUE   TRUE     TRUE    TRUE
    ##   [19,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [20,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [21,]          TRUE          TRUE         FALSE  FALSE     TRUE   FALSE
    ##   [22,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [23,]          TRUE         FALSE          TRUE   TRUE     TRUE    TRUE
    ##   [24,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [25,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [26,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [27,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [28,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [29,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [30,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [31,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [32,]          TRUE         FALSE          TRUE   TRUE     TRUE    TRUE
    ##   [33,]          TRUE         FALSE          TRUE   TRUE     TRUE    TRUE
    ##   [34,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [35,]          TRUE          TRUE          TRUE  FALSE     TRUE    TRUE
    ##   [36,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [37,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [38,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [39,]          TRUE          TRUE          TRUE  FALSE     TRUE    TRUE
    ##   [40,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [41,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [42,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [43,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [44,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [45,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [46,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [47,]          TRUE          TRUE         FALSE   TRUE    FALSE    TRUE
    ##   [48,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [49,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [50,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [51,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [52,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [53,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [54,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [55,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [56,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [57,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [58,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [59,]          TRUE         FALSE          TRUE  FALSE     TRUE    TRUE
    ##   [60,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [61,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [62,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [63,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ##   [64,]          TRUE          TRUE          TRUE  FALSE    FALSE    TRUE
    ##   [65,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [66,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [67,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [68,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [69,]          TRUE         FALSE          TRUE   TRUE     TRUE   FALSE
    ##   [70,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [71,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [72,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [73,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [74,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [75,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [76,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [77,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [78,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [79,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [80,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [81,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [82,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [83,]          TRUE          TRUE          TRUE  FALSE     TRUE    TRUE
    ##   [84,]          TRUE         FALSE          TRUE   TRUE     TRUE    TRUE
    ##   [85,]          TRUE         FALSE          TRUE   TRUE     TRUE    TRUE
    ##   [86,]          TRUE         FALSE          TRUE   TRUE    FALSE    TRUE
    ##   [87,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [88,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [89,]          TRUE          TRUE          TRUE  FALSE     TRUE    TRUE
    ##   [90,]          TRUE          TRUE          TRUE   TRUE    FALSE   FALSE
    ##   [91,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [92,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ##   [93,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [94,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [95,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ##   [96,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##   [97,]          TRUE         FALSE          TRUE   TRUE     TRUE    TRUE
    ##   [98,]          TRUE          TRUE          TRUE  FALSE     TRUE    TRUE
    ##   [99,]          TRUE         FALSE         FALSE   TRUE     TRUE    TRUE
    ##  [100,]          TRUE          TRUE          TRUE   TRUE    FALSE   FALSE
    ##  [101,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [102,]          TRUE          TRUE          TRUE   TRUE    FALSE   FALSE
    ##  [103,]          TRUE         FALSE          TRUE   TRUE     TRUE    TRUE
    ##  [104,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ##  [105,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [106,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [107,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [108,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [109,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [110,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [111,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [112,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [113,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [114,]          TRUE         FALSE         FALSE   TRUE     TRUE   FALSE
    ##  [115,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [116,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [117,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [118,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [119,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [120,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [121,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [122,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [123,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [124,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [125,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [126,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [127,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [128,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [129,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [130,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [131,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [132,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [133,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [134,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [135,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [136,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [137,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ##  [138,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [139,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [140,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [141,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [142,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [143,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ##  [144,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ##  [145,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [146,]          TRUE         FALSE          TRUE  FALSE     TRUE   FALSE
    ##  [147,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [148,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [149,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [150,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [151,]          TRUE         FALSE          TRUE   TRUE     TRUE    TRUE
    ##  [152,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [153,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [154,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [155,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [156,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [157,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [158,]          TRUE         FALSE         FALSE   TRUE     TRUE    TRUE
    ##  [159,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [160,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [161,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [162,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [163,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [164,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [165,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ##  [166,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [167,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [168,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [169,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [170,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [171,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [172,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [173,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [174,]          TRUE          TRUE          TRUE   TRUE     TRUE   FALSE
    ##  [175,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [176,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [177,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [178,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [179,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [180,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [181,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [182,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [183,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [184,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ##  [185,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [186,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [187,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [188,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [189,]          TRUE         FALSE         FALSE   TRUE     TRUE    TRUE
    ##  [190,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [191,]          TRUE          TRUE          TRUE   TRUE    FALSE   FALSE
    ##  [192,]          TRUE          TRUE         FALSE   TRUE     TRUE    TRUE
    ##  [193,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [194,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [195,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [196,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [197,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ##  [198,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [199,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [200,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ##  [201,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [202,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [203,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [204,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [205,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [206,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ##  [207,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [208,]          TRUE         FALSE         FALSE  FALSE    FALSE   FALSE
    ##  [209,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [210,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [211,]          TRUE          TRUE          TRUE  FALSE     TRUE    TRUE
    ##  [212,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [213,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [214,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [215,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [216,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [217,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [218,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [219,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [220,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [221,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [222,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [223,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ##  [224,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [225,]          TRUE          TRUE          TRUE  FALSE     TRUE    TRUE
    ##  [226,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [227,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [228,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [229,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [230,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [231,]          TRUE         FALSE         FALSE   TRUE    FALSE    TRUE
    ##  [232,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ##  [233,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [234,]          TRUE         FALSE         FALSE   TRUE     TRUE    TRUE
    ##  [235,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [236,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [237,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [238,]          TRUE          TRUE         FALSE   TRUE     TRUE    TRUE
    ##  [239,]          TRUE          TRUE          TRUE  FALSE     TRUE    TRUE
    ##  [240,]          TRUE         FALSE          TRUE   TRUE     TRUE    TRUE
    ##  [241,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [242,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [243,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [244,]          TRUE         FALSE          TRUE   TRUE     TRUE    TRUE
    ##  [245,]          TRUE         FALSE         FALSE  FALSE    FALSE   FALSE
    ##  [246,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [247,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [248,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [249,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [250,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ##  [251,]          TRUE          TRUE          TRUE   TRUE     TRUE   FALSE
    ##  [252,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ##  [253,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [254,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [255,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ##  [256,]          TRUE         FALSE          TRUE  FALSE     TRUE   FALSE
    ##  [257,]          TRUE          TRUE         FALSE   TRUE     TRUE    TRUE
    ##  [258,]          TRUE         FALSE          TRUE  FALSE     TRUE   FALSE
    ##  [259,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [260,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [261,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [262,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [263,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [264,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [265,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [266,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [267,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [268,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [269,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [270,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [271,]          TRUE         FALSE          TRUE   TRUE     TRUE    TRUE
    ##  [272,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [273,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [274,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [275,]          TRUE          TRUE         FALSE  FALSE     TRUE    TRUE
    ##  [276,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ##  [277,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [278,]          TRUE          TRUE          TRUE   TRUE     TRUE   FALSE
    ##  [279,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [280,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [281,]          TRUE         FALSE          TRUE   TRUE     TRUE    TRUE
    ##  [282,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [283,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [284,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [285,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [286,]          TRUE         FALSE          TRUE   TRUE     TRUE    TRUE
    ##  [287,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [288,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [289,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [290,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [291,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [292,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [293,]          TRUE          TRUE          TRUE  FALSE    FALSE    TRUE
    ##  [294,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [295,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ##  [296,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [297,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [298,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [299,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [300,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [301,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [302,]          TRUE         FALSE          TRUE   TRUE     TRUE   FALSE
    ##  [303,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [304,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [305,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [306,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [307,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [308,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [309,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [310,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [311,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [312,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [313,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [314,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [315,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [316,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [317,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [318,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [319,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [320,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [321,]          TRUE          TRUE          TRUE   TRUE    FALSE   FALSE
    ##  [322,]          TRUE         FALSE          TRUE   TRUE     TRUE    TRUE
    ##  [323,]          TRUE          TRUE          TRUE  FALSE     TRUE    TRUE
    ##  [324,]          TRUE         FALSE         FALSE   TRUE     TRUE    TRUE
    ##  [325,]          TRUE          TRUE          TRUE  FALSE     TRUE    TRUE
    ##  [326,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [327,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [328,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [329,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [330,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ##  [331,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [332,]          TRUE         FALSE          TRUE   TRUE     TRUE    TRUE
    ##  [333,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [334,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [335,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [336,]          TRUE          TRUE         FALSE  FALSE     TRUE    TRUE
    ##  [337,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [338,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [339,]          TRUE         FALSE         FALSE  FALSE    FALSE   FALSE
    ##  [340,]          TRUE          TRUE          TRUE   TRUE     TRUE   FALSE
    ##  [341,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [342,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [343,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [344,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [345,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [346,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [347,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [348,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [349,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [350,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [351,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [352,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [353,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [354,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [355,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [356,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [357,]          TRUE         FALSE          TRUE   TRUE     TRUE    TRUE
    ##  [358,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [359,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [360,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [361,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [362,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [363,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ##  [364,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [365,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [366,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [367,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [368,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [369,]          TRUE          TRUE         FALSE   TRUE     TRUE    TRUE
    ##  [370,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [371,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [372,]          TRUE         FALSE         FALSE   TRUE     TRUE    TRUE
    ##  [373,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [374,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [375,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [376,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [377,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [378,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [379,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [380,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ##  [381,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [382,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [383,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [384,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [385,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [386,]          TRUE          TRUE         FALSE   TRUE     TRUE    TRUE
    ##  [387,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ##  [388,]          TRUE         FALSE          TRUE   TRUE     TRUE    TRUE
    ##  [389,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [390,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [391,]          TRUE          TRUE         FALSE   TRUE     TRUE    TRUE
    ##  [392,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [393,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ##  [394,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [395,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [396,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [397,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [398,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [399,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [400,]          TRUE          TRUE          TRUE  FALSE     TRUE    TRUE
    ##  [401,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [402,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [403,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [404,]          TRUE         FALSE          TRUE   TRUE     TRUE    TRUE
    ##  [405,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [406,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [407,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [408,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [409,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [410,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [411,]          TRUE         FALSE          TRUE   TRUE    FALSE    TRUE
    ##  [412,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [413,]          TRUE         FALSE          TRUE   TRUE     TRUE    TRUE
    ##  [414,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [415,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [416,]          TRUE         FALSE          TRUE   TRUE     TRUE    TRUE
    ##  [417,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [418,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [419,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [420,]          TRUE         FALSE          TRUE   TRUE     TRUE    TRUE
    ##  [421,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [422,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [423,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [424,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [425,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [426,]          TRUE          TRUE          TRUE   TRUE    FALSE   FALSE
    ##  [427,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [428,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [429,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [430,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [431,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [432,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [433,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [434,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [435,]          TRUE          TRUE         FALSE   TRUE     TRUE    TRUE
    ##  [436,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [437,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [438,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [439,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [440,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ##  [441,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [442,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [443,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [444,]          TRUE         FALSE          TRUE   TRUE     TRUE    TRUE
    ##  [445,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [446,]          TRUE         FALSE         FALSE   TRUE     TRUE    TRUE
    ##  [447,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [448,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [449,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [450,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [451,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [452,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [453,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [454,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [455,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [456,]          TRUE          TRUE          TRUE  FALSE     TRUE    TRUE
    ##  [457,]          TRUE          TRUE          TRUE  FALSE    FALSE   FALSE
    ##  [458,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [459,]          TRUE         FALSE          TRUE   TRUE     TRUE    TRUE
    ##  [460,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ##  [461,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [462,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [463,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [464,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [465,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [466,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [467,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ##  [468,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [469,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [470,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [471,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [472,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ##  [473,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [474,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [475,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [476,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [477,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [478,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [479,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [480,]          TRUE          TRUE          TRUE   TRUE    FALSE   FALSE
    ##  [481,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [482,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [483,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [484,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [485,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [486,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [487,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [488,]          TRUE         FALSE          TRUE  FALSE     TRUE    TRUE
    ##  [489,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [490,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [491,]          TRUE          TRUE          TRUE  FALSE    FALSE    TRUE
    ##  [492,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [493,]          TRUE         FALSE          TRUE   TRUE     TRUE    TRUE
    ##  [494,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [495,]          TRUE         FALSE          TRUE   TRUE     TRUE    TRUE
    ##  [496,]          TRUE         FALSE          TRUE   TRUE     TRUE    TRUE
    ##  [497,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [498,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [499,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [500,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [501,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [502,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [503,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [504,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [505,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [506,]          TRUE         FALSE          TRUE   TRUE     TRUE    TRUE
    ##  [507,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [508,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [509,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [510,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [511,]          TRUE          TRUE          TRUE   TRUE     TRUE   FALSE
    ##  [512,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [513,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ##  [514,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [515,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [516,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [517,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [518,]          TRUE         FALSE          TRUE   TRUE     TRUE    TRUE
    ##  [519,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [520,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [521,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [522,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [523,]          TRUE          TRUE          TRUE  FALSE     TRUE    TRUE
    ##  [524,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [525,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [526,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [527,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [528,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [529,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [530,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [531,]          TRUE         FALSE         FALSE   TRUE     TRUE   FALSE
    ##  [532,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [533,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [534,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [535,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [536,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [537,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [538,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ##  [539,]          TRUE         FALSE          TRUE   TRUE     TRUE    TRUE
    ##  [540,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [541,]          TRUE         FALSE          TRUE  FALSE     TRUE   FALSE
    ##  [542,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [543,]          TRUE          TRUE         FALSE   TRUE     TRUE    TRUE
    ##  [544,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [545,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [546,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [547,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [548,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [549,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [550,]          TRUE         FALSE         FALSE  FALSE    FALSE   FALSE
    ##  [551,]          TRUE          TRUE          TRUE  FALSE     TRUE    TRUE
    ##  [552,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [553,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [554,]          TRUE          TRUE         FALSE   TRUE     TRUE    TRUE
    ##  [555,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [556,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [557,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ##  [558,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [559,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [560,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [561,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [562,]          TRUE         FALSE          TRUE   TRUE     TRUE    TRUE
    ##  [563,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [564,]          TRUE          TRUE          TRUE  FALSE     TRUE    TRUE
    ##  [565,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [566,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [567,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [568,]          TRUE         FALSE         FALSE  FALSE     TRUE   FALSE
    ##  [569,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [570,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [571,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [572,]          TRUE          TRUE          TRUE   TRUE    FALSE   FALSE
    ##  [573,]          TRUE          TRUE          TRUE   TRUE     TRUE   FALSE
    ##  [574,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [575,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [576,]          TRUE          TRUE          TRUE  FALSE    FALSE    TRUE
    ##  [577,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [578,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [579,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [580,]          TRUE          TRUE          TRUE  FALSE     TRUE    TRUE
    ##  [581,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [582,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [583,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [584,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [585,]          TRUE         FALSE          TRUE   TRUE     TRUE    TRUE
    ##  [586,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [587,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [588,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [589,]          TRUE         FALSE          TRUE   TRUE     TRUE    TRUE
    ##  [590,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [591,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [592,]          TRUE          TRUE          TRUE  FALSE     TRUE    TRUE
    ##  [593,]          TRUE          TRUE          TRUE  FALSE     TRUE    TRUE
    ##  [594,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [595,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [596,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [597,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ##  [598,]          TRUE          TRUE          TRUE   TRUE     TRUE   FALSE
    ##  [599,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [600,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [601,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [602,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [603,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [604,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [605,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [606,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [607,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [608,]          TRUE          TRUE          TRUE   TRUE    FALSE   FALSE
    ##  [609,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [610,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [611,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [612,]          TRUE          TRUE          TRUE  FALSE    FALSE   FALSE
    ##  [613,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [614,]          TRUE         FALSE          TRUE   TRUE     TRUE    TRUE
    ##  [615,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ##  [616,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [617,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [618,]          TRUE         FALSE          TRUE   TRUE     TRUE    TRUE
    ##  [619,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [620,]          TRUE         FALSE          TRUE   TRUE     TRUE    TRUE
    ##  [621,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ##  [622,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [623,]          TRUE          TRUE          TRUE   TRUE    FALSE   FALSE
    ##  [624,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [625,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [626,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ##  [627,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [628,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [629,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [630,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ##  [631,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [632,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [633,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [634,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [635,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [636,]          TRUE          TRUE          TRUE  FALSE     TRUE    TRUE
    ##  [637,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [638,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [639,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ##  [640,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [641,]          TRUE         FALSE          TRUE   TRUE     TRUE    TRUE
    ##  [642,]          TRUE          TRUE          TRUE   TRUE     TRUE   FALSE
    ##  [643,]          TRUE         FALSE          TRUE   TRUE     TRUE    TRUE
    ##  [644,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [645,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [646,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [647,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [648,]          TRUE          TRUE         FALSE   TRUE     TRUE    TRUE
    ##  [649,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [650,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [651,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [652,]          TRUE         FALSE          TRUE   TRUE     TRUE    TRUE
    ##  [653,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [654,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [655,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [656,]          TRUE          TRUE          TRUE   TRUE     TRUE   FALSE
    ##  [657,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [658,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [659,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [660,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [661,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [662,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [663,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [664,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [665,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [666,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [667,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [668,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [669,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [670,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [671,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [672,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [673,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [674,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [675,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [676,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [677,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [678,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [679,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [680,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [681,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ##  [682,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ##  [683,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [684,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [685,]          TRUE         FALSE          TRUE  FALSE     TRUE    TRUE
    ##  [686,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [687,]          TRUE         FALSE          TRUE   TRUE     TRUE    TRUE
    ##  [688,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [689,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [690,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [691,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ##  [692,]          TRUE         FALSE         FALSE  FALSE    FALSE   FALSE
    ##  [693,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [694,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [695,]          TRUE         FALSE          TRUE   TRUE     TRUE    TRUE
    ##  [696,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [697,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [698,]          TRUE          TRUE          TRUE  FALSE     TRUE    TRUE
    ##  [699,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [700,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [701,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [702,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [703,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [704,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [705,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [706,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [707,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [708,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [709,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [710,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [711,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [712,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [713,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [714,]          TRUE          TRUE         FALSE   TRUE     TRUE    TRUE
    ##  [715,]          TRUE          TRUE          TRUE  FALSE     TRUE    TRUE
    ##  [716,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [717,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [718,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [719,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [720,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [721,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [722,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [723,]          TRUE          TRUE          TRUE  FALSE     TRUE    TRUE
    ##  [724,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [725,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [726,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ##  [727,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [728,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [729,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [730,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [731,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [732,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [733,]          TRUE         FALSE          TRUE   TRUE    FALSE    TRUE
    ##  [734,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [735,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [736,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [737,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [738,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [739,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [740,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ##  [741,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [742,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ##  [743,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [744,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [745,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [746,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [747,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [748,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [749,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [750,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [751,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [752,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [753,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [754,]          TRUE          TRUE          TRUE  FALSE     TRUE    TRUE
    ##  [755,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ##  [756,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [757,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [758,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [759,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [760,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [761,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [762,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [763,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [764,]          TRUE          TRUE         FALSE   TRUE     TRUE    TRUE
    ##  [765,]          TRUE         FALSE          TRUE   TRUE     TRUE    TRUE
    ##  [766,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [767,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [768,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [769,]          TRUE          TRUE          TRUE   TRUE     TRUE   FALSE
    ##  [770,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [771,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [772,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [773,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [774,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [775,]          TRUE         FALSE         FALSE   TRUE     TRUE    TRUE
    ##  [776,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [777,]          TRUE         FALSE         FALSE   TRUE     TRUE    TRUE
    ##  [778,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [779,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [780,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [781,]          TRUE          TRUE          TRUE  FALSE    FALSE    TRUE
    ##  [782,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [783,]          TRUE          TRUE         FALSE   TRUE     TRUE    TRUE
    ##  [784,]          TRUE         FALSE         FALSE  FALSE    FALSE    TRUE
    ##  [785,]          TRUE          TRUE         FALSE   TRUE     TRUE    TRUE
    ##  [786,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [787,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [788,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [789,]          TRUE          TRUE         FALSE   TRUE     TRUE    TRUE
    ##  [790,]          TRUE          TRUE          TRUE   TRUE    FALSE   FALSE
    ##  [791,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [792,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [793,]          TRUE         FALSE          TRUE   TRUE     TRUE    TRUE
    ##  [794,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [795,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [796,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [797,]          TRUE         FALSE         FALSE  FALSE    FALSE   FALSE
    ##  [798,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [799,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [800,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [801,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [802,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [803,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [804,]          TRUE          TRUE          TRUE  FALSE     TRUE    TRUE
    ##  [805,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [806,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [807,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [808,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [809,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ##  [810,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [811,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [812,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [813,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [814,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [815,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [816,]          TRUE         FALSE          TRUE   TRUE     TRUE    TRUE
    ##  [817,]          TRUE          TRUE          TRUE   TRUE     TRUE   FALSE
    ##  [818,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [819,]          TRUE         FALSE          TRUE   TRUE     TRUE    TRUE
    ##  [820,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [821,]          TRUE          TRUE          TRUE   TRUE     TRUE   FALSE
    ##  [822,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [823,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [824,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [825,]          TRUE          TRUE         FALSE   TRUE     TRUE    TRUE
    ##  [826,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [827,]          TRUE          TRUE          TRUE  FALSE     TRUE    TRUE
    ##  [828,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [829,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [830,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [831,]          TRUE          TRUE          TRUE  FALSE     TRUE    TRUE
    ##  [832,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [833,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [834,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [835,]          TRUE          TRUE          TRUE  FALSE     TRUE    TRUE
    ##  [836,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [837,]          TRUE          TRUE         FALSE   TRUE     TRUE    TRUE
    ##  [838,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [839,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [840,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [841,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [842,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [843,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [844,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [845,]          TRUE          TRUE         FALSE   TRUE     TRUE    TRUE
    ##  [846,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [847,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [848,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [849,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [850,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [851,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [852,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [853,]          TRUE         FALSE          TRUE   TRUE     TRUE    TRUE
    ##  [854,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [855,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [856,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [857,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [858,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [859,]          TRUE         FALSE         FALSE   TRUE    FALSE   FALSE
    ##  [860,]          TRUE          TRUE          TRUE   TRUE    FALSE   FALSE
    ##  [861,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [862,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [863,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [864,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [865,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [866,]          TRUE          TRUE          TRUE  FALSE    FALSE    TRUE
    ##  [867,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [868,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [869,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ##  [870,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [871,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [872,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [873,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ##  [874,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [875,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [876,]          TRUE         FALSE         FALSE   TRUE     TRUE    TRUE
    ##  [877,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [878,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [879,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [880,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ##  [881,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [882,]          TRUE          TRUE          TRUE   TRUE    FALSE   FALSE
    ##  [883,]          TRUE         FALSE          TRUE  FALSE     TRUE   FALSE
    ##  [884,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [885,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [886,]          TRUE          TRUE          TRUE  FALSE     TRUE    TRUE
    ##  [887,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [888,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [889,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [890,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [891,]          TRUE         FALSE         FALSE   TRUE     TRUE    TRUE
    ##  [892,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [893,]          TRUE         FALSE          TRUE   TRUE     TRUE    TRUE
    ##  [894,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [895,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ##  [896,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [897,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [898,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [899,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [900,]          TRUE          TRUE          TRUE   TRUE    FALSE   FALSE
    ##  [901,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ##  [902,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [903,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [904,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [905,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [906,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [907,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [908,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [909,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [910,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [911,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [912,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [913,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ##  [914,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [915,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [916,]          TRUE          TRUE          TRUE  FALSE     TRUE   FALSE
    ##  [917,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [918,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [919,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [920,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ##  [921,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [922,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [923,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [924,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [925,]          TRUE          TRUE          TRUE   TRUE    FALSE   FALSE
    ##  [926,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [927,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [928,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [929,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [930,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [931,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [932,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [933,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [934,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [935,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [936,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [937,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [938,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [939,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [940,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [941,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [942,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [943,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [944,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [945,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [946,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [947,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [948,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [949,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [950,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [951,]          TRUE         FALSE          TRUE   TRUE     TRUE    TRUE
    ##  [952,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [953,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [954,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [955,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [956,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [957,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [958,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [959,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [960,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [961,]          TRUE          TRUE          TRUE  FALSE     TRUE    TRUE
    ##  [962,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [963,]          TRUE          TRUE          TRUE   TRUE     TRUE   FALSE
    ##  [964,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [965,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [966,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [967,]          TRUE          TRUE          TRUE   TRUE     TRUE   FALSE
    ##  [968,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ##  [969,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [970,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [971,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [972,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [973,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ##  [974,]          TRUE          TRUE          TRUE   TRUE    FALSE   FALSE
    ##  [975,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [976,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [977,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [978,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [979,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [980,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [981,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [982,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ##  [983,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [984,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [985,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [986,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [987,]          TRUE          TRUE          TRUE   TRUE    FALSE   FALSE
    ##  [988,]          TRUE         FALSE         FALSE   TRUE     TRUE    TRUE
    ##  [989,]          TRUE         FALSE          TRUE   TRUE     TRUE    TRUE
    ##  [990,]          TRUE          TRUE          TRUE   TRUE    FALSE   FALSE
    ##  [991,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [992,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ##  [993,]          TRUE         FALSE          TRUE   TRUE     TRUE    TRUE
    ##  [994,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [995,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [996,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [997,]          TRUE          TRUE          TRUE  FALSE    FALSE    TRUE
    ##  [998,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##  [999,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1000,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1001,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1002,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ## [1003,]          TRUE          TRUE         FALSE   TRUE     TRUE    TRUE
    ## [1004,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1005,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1006,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1007,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1008,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1009,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1010,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1011,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1012,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1013,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1014,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1015,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1016,]          TRUE          TRUE          TRUE  FALSE     TRUE    TRUE
    ## [1017,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1018,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1019,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1020,]          TRUE          TRUE          TRUE  FALSE     TRUE    TRUE
    ## [1021,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1022,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ## [1023,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1024,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1025,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1026,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1027,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1028,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1029,]          TRUE         FALSE         FALSE   TRUE     TRUE    TRUE
    ## [1030,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1031,]          TRUE          TRUE          TRUE  FALSE     TRUE    TRUE
    ## [1032,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1033,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1034,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1035,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1036,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1037,]          TRUE          TRUE          TRUE   TRUE    FALSE   FALSE
    ## [1038,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1039,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1040,]          TRUE         FALSE          TRUE   TRUE     TRUE    TRUE
    ## [1041,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1042,]          TRUE          TRUE          TRUE   TRUE    FALSE   FALSE
    ## [1043,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1044,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1045,]          TRUE          TRUE          TRUE  FALSE     TRUE   FALSE
    ## [1046,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1047,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1048,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1049,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1050,]          TRUE          TRUE          TRUE  FALSE    FALSE    TRUE
    ## [1051,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1052,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ## [1053,]          TRUE         FALSE          TRUE   TRUE     TRUE    TRUE
    ## [1054,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1055,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1056,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1057,]          TRUE         FALSE         FALSE   TRUE     TRUE    TRUE
    ## [1058,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1059,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1060,]          TRUE          TRUE          TRUE  FALSE    FALSE   FALSE
    ## [1061,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1062,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1063,]          TRUE          TRUE          TRUE  FALSE    FALSE    TRUE
    ## [1064,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1065,]          TRUE         FALSE         FALSE  FALSE    FALSE   FALSE
    ## [1066,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1067,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1068,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1069,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1070,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ## [1071,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1072,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1073,]          TRUE         FALSE          TRUE   TRUE     TRUE    TRUE
    ## [1074,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1075,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1076,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1077,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1078,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1079,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1080,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1081,]          TRUE         FALSE          TRUE   TRUE     TRUE    TRUE
    ## [1082,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1083,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1084,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1085,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1086,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1087,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1088,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1089,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1090,]          TRUE         FALSE         FALSE   TRUE     TRUE    TRUE
    ## [1091,]          TRUE         FALSE          TRUE   TRUE     TRUE    TRUE
    ## [1092,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1093,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1094,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1095,]          TRUE         FALSE          TRUE   TRUE    FALSE    TRUE
    ## [1096,]          TRUE         FALSE          TRUE   TRUE     TRUE    TRUE
    ## [1097,]          TRUE          TRUE         FALSE   TRUE     TRUE    TRUE
    ## [1098,]          TRUE         FALSE          TRUE   TRUE     TRUE    TRUE
    ## [1099,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1100,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1101,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1102,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1103,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1104,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1105,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1106,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ## [1107,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1108,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1109,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ## [1110,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1111,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ## [1112,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1113,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1114,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1115,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1116,]          TRUE          TRUE         FALSE   TRUE     TRUE    TRUE
    ## [1117,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1118,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1119,]          TRUE          TRUE         FALSE   TRUE     TRUE    TRUE
    ## [1120,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1121,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ## [1122,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1123,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1124,]          TRUE          TRUE          TRUE  FALSE     TRUE    TRUE
    ## [1125,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1126,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1127,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1128,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1129,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1130,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1131,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1132,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1133,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1134,]          TRUE         FALSE          TRUE   TRUE     TRUE    TRUE
    ## [1135,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1136,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1137,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1138,]          TRUE          TRUE          TRUE   TRUE     TRUE   FALSE
    ## [1139,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1140,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1141,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1142,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1143,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1144,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1145,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1146,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1147,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1148,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1149,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1150,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1151,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1152,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1153,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1154,]          TRUE         FALSE          TRUE   TRUE     TRUE    TRUE
    ## [1155,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1156,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1157,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ## [1158,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1159,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1160,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1161,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1162,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1163,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1164,]          TRUE          TRUE          TRUE  FALSE     TRUE    TRUE
    ## [1165,]          TRUE         FALSE         FALSE   TRUE     TRUE    TRUE
    ## [1166,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1167,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1168,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1169,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ## [1170,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1171,]          TRUE          TRUE          TRUE  FALSE     TRUE    TRUE
    ## [1172,]          TRUE          TRUE          TRUE  FALSE     TRUE    TRUE
    ## [1173,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1174,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1175,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1176,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1177,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1178,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1179,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1180,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1181,]          TRUE          TRUE          TRUE   TRUE    FALSE    TRUE
    ## [1182,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1183,]          TRUE          TRUE          TRUE   TRUE    FALSE   FALSE
    ## [1184,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1185,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1186,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1187,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1188,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1189,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1190,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1191,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1192,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1193,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1194,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1195,]          TRUE         FALSE          TRUE   TRUE    FALSE   FALSE
    ## [1196,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1197,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1198,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1199,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ## [1200,]          TRUE          TRUE          TRUE   TRUE     TRUE    TRUE
    ##         educ_parent smoke pets gasstove   fev   fvc  mmef pm25_mass pm25_so4
    ##    [1,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##    [2,]        TRUE FALSE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##    [3,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##    [4,]       FALSE FALSE TRUE    FALSE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##    [5,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##    [6,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##    [7,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##    [8,]       FALSE FALSE TRUE    FALSE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##    [9,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [10,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [11,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [12,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [13,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [14,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [15,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [16,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [17,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [18,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [19,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [20,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##   [21,]       FALSE FALSE TRUE    FALSE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [22,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [23,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [24,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [25,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [26,]       FALSE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [27,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [28,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [29,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [30,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [31,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [32,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [33,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [34,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [35,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [36,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [37,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [38,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [39,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [40,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [41,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [42,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [43,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [44,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [45,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [46,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [47,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [48,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [49,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [50,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [51,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [52,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [53,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [54,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [55,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [56,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##   [57,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [58,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [59,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [60,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [61,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [62,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [63,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [64,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [65,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [66,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [67,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [68,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [69,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [70,]       FALSE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [71,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [72,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [73,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [74,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [75,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [76,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE FALSE      TRUE     TRUE
    ##   [77,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [78,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [79,]        TRUE  TRUE TRUE     TRUE FALSE  TRUE FALSE      TRUE     TRUE
    ##   [80,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [81,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [82,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [83,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [84,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [85,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [86,]        TRUE FALSE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [87,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [88,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [89,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [90,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [91,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [92,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [93,]        TRUE FALSE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [94,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [95,]        TRUE FALSE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [96,]        TRUE  TRUE TRUE    FALSE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##   [97,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##   [98,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##   [99,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [100,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [101,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [102,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [103,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [104,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [105,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [106,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [107,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [108,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [109,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [110,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [111,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [112,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [113,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [114,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [115,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [116,]       FALSE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [117,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [118,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [119,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [120,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [121,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [122,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [123,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [124,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [125,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [126,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [127,]        TRUE FALSE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [128,]        TRUE FALSE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [129,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [130,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [131,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [132,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [133,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [134,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [135,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [136,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [137,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [138,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [139,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [140,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [141,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [142,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [143,]        TRUE FALSE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [144,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [145,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [146,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [147,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [148,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [149,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [150,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [151,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [152,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [153,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [154,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [155,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [156,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [157,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [158,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [159,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [160,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [161,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [162,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [163,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [164,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [165,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [166,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE FALSE      TRUE     TRUE
    ##  [167,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [168,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [169,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [170,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [171,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [172,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [173,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [174,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE FALSE      TRUE     TRUE
    ##  [175,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [176,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [177,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [178,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [179,]        TRUE  TRUE TRUE    FALSE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [180,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [181,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [182,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [183,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [184,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [185,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [186,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [187,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [188,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [189,]       FALSE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [190,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [191,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [192,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [193,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [194,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [195,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [196,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [197,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [198,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [199,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [200,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [201,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [202,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [203,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [204,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [205,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [206,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [207,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [208,]       FALSE FALSE TRUE    FALSE FALSE FALSE FALSE      TRUE     TRUE
    ##  [209,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [210,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [211,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [212,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [213,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [214,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [215,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [216,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [217,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [218,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [219,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [220,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [221,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [222,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [223,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [224,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [225,]       FALSE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [226,]       FALSE  TRUE TRUE    FALSE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [227,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [228,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [229,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [230,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [231,]       FALSE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [232,]       FALSE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [233,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [234,]        TRUE FALSE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [235,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [236,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [237,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [238,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [239,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [240,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [241,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [242,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [243,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [244,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [245,]       FALSE FALSE TRUE    FALSE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [246,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [247,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [248,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [249,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [250,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [251,]       FALSE FALSE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [252,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [253,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [254,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [255,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [256,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [257,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [258,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [259,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [260,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [261,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [262,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [263,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE FALSE      TRUE     TRUE
    ##  [264,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [265,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [266,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [267,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [268,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [269,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [270,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [271,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [272,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [273,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [274,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [275,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [276,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [277,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [278,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [279,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [280,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [281,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [282,]        TRUE FALSE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [283,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [284,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [285,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [286,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [287,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [288,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [289,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [290,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [291,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [292,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [293,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [294,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [295,]        TRUE FALSE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [296,]       FALSE  TRUE TRUE    FALSE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [297,]       FALSE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [298,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [299,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [300,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [301,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [302,]        TRUE  TRUE TRUE    FALSE FALSE FALSE FALSE      TRUE     TRUE
    ##  [303,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [304,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [305,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [306,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [307,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [308,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [309,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [310,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [311,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [312,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [313,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [314,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [315,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [316,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [317,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [318,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [319,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [320,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [321,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [322,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [323,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [324,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [325,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [326,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [327,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [328,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [329,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [330,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [331,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [332,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [333,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [334,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [335,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [336,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [337,]       FALSE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [338,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [339,]       FALSE FALSE TRUE    FALSE FALSE FALSE FALSE      TRUE     TRUE
    ##  [340,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [341,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [342,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [343,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [344,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [345,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE FALSE      TRUE     TRUE
    ##  [346,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [347,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [348,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [349,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [350,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [351,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [352,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [353,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [354,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [355,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [356,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [357,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [358,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [359,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [360,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [361,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [362,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [363,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [364,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [365,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [366,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [367,]        TRUE FALSE TRUE     TRUE  TRUE  TRUE FALSE      TRUE     TRUE
    ##  [368,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [369,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [370,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [371,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [372,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [373,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [374,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [375,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [376,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [377,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [378,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [379,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [380,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [381,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [382,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [383,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [384,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [385,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [386,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [387,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [388,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [389,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [390,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [391,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [392,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [393,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [394,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [395,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [396,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [397,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [398,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [399,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [400,]       FALSE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [401,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [402,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [403,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [404,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [405,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [406,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [407,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [408,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [409,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [410,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [411,]        TRUE FALSE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [412,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [413,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [414,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [415,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [416,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [417,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [418,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [419,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [420,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [421,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [422,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [423,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [424,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [425,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [426,]        TRUE  TRUE TRUE    FALSE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [427,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [428,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [429,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [430,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [431,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [432,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [433,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [434,]       FALSE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [435,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [436,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE FALSE      TRUE     TRUE
    ##  [437,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [438,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [439,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [440,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [441,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [442,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [443,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [444,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [445,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [446,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [447,]       FALSE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [448,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [449,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [450,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [451,]        TRUE  TRUE TRUE    FALSE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [452,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [453,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [454,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [455,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [456,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [457,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [458,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [459,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [460,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [461,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [462,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [463,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [464,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [465,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [466,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [467,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [468,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [469,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [470,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [471,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [472,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [473,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [474,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [475,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [476,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [477,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [478,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [479,]        TRUE  TRUE TRUE    FALSE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [480,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [481,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [482,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [483,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [484,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [485,]        TRUE  TRUE TRUE    FALSE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [486,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [487,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [488,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [489,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [490,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [491,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [492,]        TRUE FALSE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [493,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [494,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [495,]       FALSE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [496,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [497,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [498,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [499,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [500,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [501,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [502,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [503,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [504,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [505,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [506,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [507,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [508,]        TRUE  TRUE TRUE    FALSE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [509,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [510,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [511,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [512,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [513,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [514,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [515,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [516,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [517,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [518,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [519,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [520,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [521,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [522,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [523,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [524,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [525,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [526,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [527,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [528,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [529,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [530,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [531,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [532,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [533,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [534,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [535,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [536,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [537,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [538,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [539,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [540,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [541,]        TRUE  TRUE TRUE     TRUE  TRUE FALSE  TRUE      TRUE     TRUE
    ##  [542,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [543,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [544,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [545,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [546,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [547,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [548,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [549,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [550,]       FALSE FALSE TRUE    FALSE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [551,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [552,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [553,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [554,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [555,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [556,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [557,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [558,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [559,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [560,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [561,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [562,]        TRUE  TRUE TRUE    FALSE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [563,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [564,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [565,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [566,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [567,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [568,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [569,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [570,]        TRUE FALSE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [571,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [572,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [573,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [574,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [575,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [576,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [577,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [578,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [579,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [580,]       FALSE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [581,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [582,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [583,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [584,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [585,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [586,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [587,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [588,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [589,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [590,]       FALSE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [591,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [592,]       FALSE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [593,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [594,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [595,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [596,]       FALSE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [597,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [598,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [599,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [600,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [601,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [602,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [603,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [604,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [605,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [606,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [607,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [608,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [609,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [610,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [611,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [612,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [613,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [614,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [615,]        TRUE FALSE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [616,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [617,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [618,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [619,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [620,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [621,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE FALSE      TRUE     TRUE
    ##  [622,]        TRUE FALSE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [623,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [624,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [625,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [626,]        TRUE FALSE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [627,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [628,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [629,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [630,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [631,]       FALSE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [632,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [633,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [634,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [635,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [636,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [637,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [638,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [639,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [640,]       FALSE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [641,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [642,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [643,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [644,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [645,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [646,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [647,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [648,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [649,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [650,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [651,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [652,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [653,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [654,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [655,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [656,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [657,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [658,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [659,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [660,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [661,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [662,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [663,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [664,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [665,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [666,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [667,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [668,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [669,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [670,]        TRUE  TRUE TRUE    FALSE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [671,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [672,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [673,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [674,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [675,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [676,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [677,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [678,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [679,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [680,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [681,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [682,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [683,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [684,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [685,]        TRUE  TRUE TRUE     TRUE  TRUE FALSE  TRUE      TRUE     TRUE
    ##  [686,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [687,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [688,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [689,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [690,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [691,]        TRUE FALSE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [692,]       FALSE FALSE TRUE    FALSE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [693,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [694,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [695,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [696,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [697,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [698,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [699,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [700,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [701,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [702,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [703,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [704,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [705,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [706,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [707,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [708,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [709,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [710,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [711,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [712,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [713,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [714,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [715,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [716,]       FALSE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [717,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [718,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [719,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [720,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [721,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [722,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [723,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [724,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [725,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [726,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [727,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [728,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [729,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [730,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [731,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [732,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [733,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [734,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [735,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [736,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [737,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [738,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [739,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [740,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [741,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [742,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [743,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [744,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [745,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [746,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [747,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [748,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [749,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [750,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [751,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [752,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [753,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [754,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [755,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [756,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [757,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [758,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [759,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [760,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [761,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [762,]       FALSE FALSE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [763,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [764,]       FALSE FALSE TRUE    FALSE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [765,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [766,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [767,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [768,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [769,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [770,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [771,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [772,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [773,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [774,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [775,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [776,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [777,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [778,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [779,]       FALSE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [780,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE FALSE      TRUE     TRUE
    ##  [781,]        TRUE  TRUE TRUE    FALSE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [782,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [783,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [784,]        TRUE FALSE TRUE    FALSE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [785,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [786,]       FALSE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [787,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [788,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [789,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [790,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [791,]       FALSE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [792,]       FALSE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [793,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [794,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [795,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [796,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [797,]       FALSE FALSE TRUE    FALSE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [798,]       FALSE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [799,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [800,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [801,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [802,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [803,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [804,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [805,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [806,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [807,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [808,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [809,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [810,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [811,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [812,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [813,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [814,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [815,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [816,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [817,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [818,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [819,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [820,]        TRUE  TRUE TRUE     TRUE FALSE  TRUE FALSE      TRUE     TRUE
    ##  [821,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [822,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [823,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [824,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [825,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [826,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [827,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [828,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [829,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [830,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [831,]        TRUE  TRUE TRUE    FALSE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [832,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [833,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [834,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [835,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [836,]       FALSE  TRUE TRUE    FALSE FALSE FALSE FALSE      TRUE     TRUE
    ##  [837,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [838,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [839,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [840,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [841,]       FALSE FALSE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [842,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [843,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [844,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [845,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [846,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [847,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [848,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [849,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [850,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [851,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [852,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [853,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [854,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [855,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [856,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [857,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [858,]       FALSE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [859,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [860,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [861,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [862,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE FALSE      TRUE     TRUE
    ##  [863,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [864,]        TRUE  TRUE TRUE    FALSE FALSE FALSE FALSE      TRUE     TRUE
    ##  [865,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [866,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [867,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [868,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [869,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [870,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [871,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [872,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [873,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [874,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [875,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [876,]       FALSE FALSE TRUE    FALSE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [877,]        TRUE FALSE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [878,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [879,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [880,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [881,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [882,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [883,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [884,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [885,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [886,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [887,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [888,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [889,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [890,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [891,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [892,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [893,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [894,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [895,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [896,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [897,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [898,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [899,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [900,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [901,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [902,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [903,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [904,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [905,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [906,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [907,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [908,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [909,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [910,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [911,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [912,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [913,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [914,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [915,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [916,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [917,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [918,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [919,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [920,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [921,]       FALSE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [922,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [923,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [924,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [925,]       FALSE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [926,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [927,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [928,]       FALSE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [929,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [930,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [931,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [932,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [933,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [934,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [935,]        TRUE  TRUE TRUE     TRUE  TRUE FALSE  TRUE      TRUE     TRUE
    ##  [936,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [937,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [938,]        TRUE FALSE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##  [939,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [940,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [941,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [942,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [943,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [944,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [945,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [946,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [947,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [948,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [949,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [950,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [951,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [952,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [953,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [954,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [955,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [956,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [957,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [958,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [959,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [960,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [961,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [962,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [963,]       FALSE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [964,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [965,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [966,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [967,]       FALSE FALSE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [968,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [969,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [970,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [971,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [972,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [973,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [974,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [975,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [976,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [977,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [978,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [979,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [980,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [981,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [982,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [983,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [984,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [985,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [986,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [987,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [988,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [989,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [990,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [991,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [992,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [993,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [994,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [995,]       FALSE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [996,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [997,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [998,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ##  [999,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1000,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1001,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1002,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1003,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ## [1004,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1005,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ## [1006,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1007,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1008,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ## [1009,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1010,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1011,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1012,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1013,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1014,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1015,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1016,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ## [1017,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1018,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1019,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1020,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1021,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1022,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1023,]       FALSE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1024,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1025,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1026,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1027,]       FALSE  TRUE TRUE    FALSE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1028,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1029,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1030,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1031,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1032,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1033,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1034,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1035,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1036,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1037,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1038,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1039,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1040,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ## [1041,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1042,]        TRUE  TRUE TRUE     TRUE  TRUE FALSE FALSE      TRUE     TRUE
    ## [1043,]       FALSE  TRUE TRUE    FALSE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1044,]       FALSE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1045,]       FALSE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1046,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1047,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1048,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1049,]       FALSE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1050,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ## [1051,]       FALSE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1052,]       FALSE FALSE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1053,]       FALSE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ## [1054,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1055,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1056,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1057,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ## [1058,]       FALSE  TRUE TRUE     TRUE  TRUE  TRUE FALSE      TRUE     TRUE
    ## [1059,]       FALSE  TRUE TRUE    FALSE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1060,]       FALSE FALSE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1061,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1062,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1063,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1064,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1065,]       FALSE FALSE TRUE    FALSE FALSE FALSE FALSE      TRUE     TRUE
    ## [1066,]       FALSE  TRUE TRUE    FALSE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1067,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1068,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1069,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1070,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1071,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1072,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1073,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1074,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1075,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1076,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1077,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1078,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1079,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1080,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1081,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1082,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1083,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1084,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1085,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1086,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1087,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1088,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1089,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1090,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ## [1091,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1092,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1093,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1094,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1095,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ## [1096,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1097,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1098,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1099,]       FALSE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1100,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1101,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1102,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ## [1103,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1104,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1105,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1106,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1107,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1108,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1109,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1110,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1111,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1112,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1113,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1114,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1115,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1116,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1117,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1118,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1119,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1120,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1121,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1122,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1123,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1124,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1125,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1126,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1127,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1128,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1129,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1130,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1131,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1132,]        TRUE  TRUE TRUE     TRUE  TRUE FALSE  TRUE      TRUE     TRUE
    ## [1133,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1134,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1135,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1136,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1137,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1138,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ## [1139,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1140,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1141,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1142,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1143,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1144,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1145,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1146,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1147,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1148,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1149,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1150,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1151,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1152,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1153,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ## [1154,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1155,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1156,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1157,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1158,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1159,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1160,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1161,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1162,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1163,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1164,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1165,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1166,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1167,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1168,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1169,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1170,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1171,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1172,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1173,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1174,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1175,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1176,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1177,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1178,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1179,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1180,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1181,]        TRUE FALSE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1182,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1183,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1184,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1185,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ## [1186,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1187,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1188,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1189,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1190,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1191,]        TRUE  TRUE TRUE     TRUE FALSE  TRUE  TRUE      TRUE     TRUE
    ## [1192,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ## [1193,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ## [1194,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1195,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1196,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1197,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1198,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1199,]        TRUE  TRUE TRUE     TRUE  TRUE  TRUE  TRUE      TRUE     TRUE
    ## [1200,]        TRUE  TRUE TRUE     TRUE FALSE FALSE FALSE      TRUE     TRUE
    ##         pm25_no3 pm25_nh4 pm25_oc pm25_ec pm25_om pm10_oc pm10_ec pm10_tc
    ##    [1,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##    [2,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##    [3,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##    [4,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##    [5,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##    [6,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##    [7,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##    [8,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##    [9,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [10,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [11,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [12,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [13,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [14,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [15,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [16,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [17,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [18,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [19,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [20,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [21,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [22,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [23,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [24,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [25,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [26,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [27,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [28,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [29,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [30,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [31,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [32,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [33,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [34,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [35,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [36,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [37,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [38,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [39,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [40,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [41,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [42,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [43,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [44,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [45,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [46,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [47,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [48,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [49,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [50,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [51,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [52,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [53,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [54,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [55,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [56,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [57,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [58,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [59,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [60,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [61,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [62,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [63,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [64,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [65,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [66,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [67,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [68,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [69,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [70,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [71,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [72,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [73,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [74,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [75,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [76,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [77,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [78,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [79,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [80,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [81,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [82,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [83,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [84,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [85,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [86,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [87,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [88,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [89,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [90,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [91,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [92,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [93,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [94,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [95,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [96,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [97,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [98,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##   [99,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [100,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [101,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [102,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [103,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [104,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [105,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [106,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [107,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [108,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [109,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [110,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [111,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [112,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [113,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [114,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [115,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [116,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [117,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [118,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [119,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [120,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [121,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [122,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [123,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [124,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [125,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [126,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [127,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [128,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [129,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [130,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [131,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [132,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [133,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [134,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [135,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [136,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [137,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [138,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [139,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [140,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [141,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [142,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [143,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [144,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [145,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [146,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [147,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [148,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [149,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [150,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [151,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [152,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [153,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [154,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [155,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [156,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [157,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [158,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [159,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [160,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [161,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [162,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [163,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [164,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [165,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [166,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [167,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [168,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [169,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [170,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [171,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [172,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [173,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [174,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [175,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [176,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [177,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [178,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [179,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [180,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [181,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [182,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [183,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [184,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [185,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [186,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [187,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [188,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [189,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [190,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [191,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [192,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [193,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [194,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [195,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [196,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [197,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [198,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [199,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [200,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [201,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [202,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [203,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [204,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [205,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [206,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [207,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [208,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [209,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [210,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [211,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [212,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [213,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [214,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [215,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [216,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [217,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [218,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [219,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [220,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [221,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [222,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [223,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [224,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [225,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [226,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [227,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [228,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [229,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [230,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [231,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [232,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [233,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [234,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [235,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [236,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [237,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [238,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [239,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [240,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [241,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [242,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [243,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [244,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [245,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [246,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [247,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [248,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [249,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [250,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [251,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [252,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [253,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [254,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [255,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [256,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [257,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [258,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [259,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [260,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [261,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [262,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [263,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [264,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [265,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [266,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [267,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [268,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [269,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [270,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [271,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [272,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [273,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [274,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [275,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [276,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [277,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [278,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [279,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [280,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [281,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [282,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [283,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [284,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [285,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [286,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [287,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [288,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [289,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [290,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [291,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [292,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [293,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [294,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [295,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [296,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [297,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [298,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [299,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [300,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [301,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [302,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [303,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [304,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [305,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [306,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [307,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [308,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [309,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [310,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [311,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [312,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [313,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [314,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [315,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [316,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [317,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [318,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [319,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [320,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [321,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [322,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [323,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [324,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [325,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [326,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [327,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [328,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [329,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [330,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [331,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [332,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [333,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [334,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [335,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [336,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [337,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [338,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [339,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [340,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [341,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [342,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [343,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [344,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [345,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [346,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [347,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [348,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [349,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [350,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [351,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [352,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [353,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [354,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [355,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [356,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [357,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [358,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [359,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [360,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [361,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [362,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [363,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [364,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [365,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [366,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [367,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [368,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [369,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [370,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [371,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [372,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [373,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [374,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [375,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [376,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [377,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [378,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [379,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [380,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [381,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [382,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [383,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [384,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [385,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [386,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [387,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [388,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [389,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [390,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [391,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [392,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [393,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [394,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [395,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [396,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [397,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [398,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [399,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [400,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [401,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [402,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [403,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [404,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [405,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [406,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [407,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [408,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [409,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [410,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [411,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [412,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [413,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [414,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [415,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [416,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [417,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [418,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [419,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [420,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [421,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [422,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [423,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [424,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [425,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [426,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [427,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [428,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [429,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [430,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [431,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [432,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [433,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [434,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [435,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [436,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [437,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [438,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [439,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [440,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [441,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [442,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [443,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [444,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [445,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [446,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [447,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [448,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [449,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [450,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [451,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [452,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [453,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [454,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [455,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [456,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [457,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [458,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [459,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [460,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [461,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [462,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [463,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [464,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [465,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [466,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [467,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [468,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [469,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [470,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [471,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [472,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [473,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [474,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [475,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [476,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [477,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [478,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [479,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [480,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [481,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [482,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [483,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [484,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [485,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [486,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [487,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [488,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [489,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [490,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [491,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [492,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [493,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [494,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [495,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [496,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [497,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [498,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [499,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [500,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [501,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [502,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [503,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [504,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [505,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [506,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [507,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [508,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [509,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [510,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [511,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [512,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [513,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [514,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [515,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [516,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [517,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [518,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [519,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [520,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [521,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [522,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [523,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [524,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [525,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [526,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [527,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [528,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [529,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [530,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [531,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [532,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [533,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [534,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [535,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [536,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [537,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [538,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [539,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [540,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [541,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [542,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [543,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [544,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [545,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [546,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [547,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [548,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [549,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [550,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [551,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [552,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [553,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [554,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [555,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [556,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [557,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [558,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [559,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [560,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [561,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [562,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [563,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [564,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [565,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [566,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [567,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [568,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [569,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [570,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [571,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [572,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [573,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [574,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [575,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [576,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [577,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [578,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [579,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [580,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [581,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [582,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [583,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [584,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [585,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [586,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [587,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [588,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [589,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [590,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [591,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [592,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [593,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [594,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [595,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [596,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [597,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [598,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [599,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [600,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [601,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [602,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [603,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [604,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [605,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [606,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [607,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [608,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [609,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [610,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [611,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [612,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [613,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [614,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [615,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [616,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [617,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [618,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [619,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [620,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [621,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [622,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [623,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [624,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [625,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [626,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [627,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [628,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [629,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [630,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [631,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [632,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [633,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [634,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [635,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [636,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [637,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [638,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [639,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [640,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [641,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [642,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [643,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [644,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [645,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [646,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [647,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [648,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [649,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [650,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [651,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [652,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [653,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [654,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [655,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [656,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [657,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [658,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [659,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [660,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [661,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [662,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [663,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [664,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [665,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [666,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [667,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [668,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [669,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [670,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [671,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [672,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [673,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [674,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [675,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [676,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [677,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [678,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [679,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [680,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [681,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [682,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [683,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [684,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [685,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [686,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [687,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [688,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [689,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [690,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [691,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [692,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [693,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [694,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [695,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [696,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [697,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [698,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [699,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [700,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [701,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [702,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [703,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [704,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [705,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [706,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [707,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [708,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [709,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [710,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [711,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [712,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [713,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [714,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [715,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [716,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [717,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [718,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [719,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [720,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [721,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [722,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [723,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [724,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [725,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [726,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [727,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [728,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [729,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [730,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [731,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [732,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [733,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [734,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [735,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [736,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [737,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [738,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [739,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [740,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [741,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [742,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [743,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [744,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [745,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [746,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [747,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [748,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [749,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [750,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [751,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [752,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [753,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [754,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [755,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [756,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [757,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [758,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [759,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [760,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [761,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [762,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [763,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [764,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [765,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [766,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [767,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [768,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [769,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [770,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [771,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [772,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [773,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [774,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [775,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [776,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [777,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [778,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [779,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [780,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [781,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [782,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [783,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [784,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [785,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [786,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [787,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [788,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [789,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [790,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [791,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [792,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [793,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [794,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [795,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [796,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [797,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [798,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [799,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [800,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [801,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [802,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [803,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [804,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [805,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [806,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [807,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [808,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [809,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [810,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [811,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [812,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [813,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [814,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [815,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [816,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [817,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [818,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [819,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [820,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [821,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [822,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [823,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [824,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [825,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [826,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [827,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [828,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [829,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [830,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [831,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [832,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [833,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [834,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [835,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [836,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [837,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [838,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [839,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [840,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [841,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [842,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [843,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [844,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [845,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [846,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [847,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [848,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [849,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [850,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [851,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [852,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [853,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [854,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [855,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [856,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [857,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [858,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [859,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [860,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [861,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [862,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [863,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [864,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [865,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [866,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [867,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [868,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [869,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [870,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [871,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [872,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [873,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [874,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [875,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [876,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [877,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [878,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [879,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [880,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [881,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [882,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [883,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [884,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [885,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [886,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [887,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [888,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [889,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [890,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [891,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [892,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [893,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [894,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [895,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [896,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [897,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [898,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [899,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [900,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [901,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [902,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [903,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [904,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [905,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [906,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [907,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [908,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [909,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [910,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [911,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [912,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [913,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [914,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [915,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [916,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [917,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [918,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [919,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [920,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [921,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [922,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [923,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [924,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [925,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [926,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [927,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [928,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [929,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [930,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [931,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [932,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [933,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [934,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [935,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [936,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [937,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [938,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [939,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [940,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [941,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [942,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [943,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [944,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [945,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [946,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [947,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [948,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [949,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [950,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [951,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [952,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [953,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [954,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [955,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [956,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [957,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [958,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [959,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [960,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [961,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [962,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [963,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [964,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [965,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [966,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [967,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [968,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [969,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [970,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [971,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [972,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [973,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [974,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [975,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [976,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [977,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [978,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [979,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [980,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [981,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [982,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [983,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [984,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [985,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [986,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [987,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [988,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [989,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [990,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [991,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [992,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [993,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [994,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [995,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [996,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [997,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [998,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##  [999,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1000,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1001,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1002,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1003,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1004,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1005,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1006,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1007,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1008,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1009,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1010,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1011,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1012,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1013,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1014,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1015,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1016,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1017,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1018,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1019,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1020,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1021,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1022,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1023,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1024,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1025,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1026,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1027,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1028,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1029,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1030,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1031,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1032,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1033,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1034,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1035,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1036,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1037,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1038,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1039,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1040,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1041,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1042,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1043,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1044,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1045,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1046,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1047,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1048,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1049,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1050,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1051,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1052,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1053,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1054,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1055,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1056,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1057,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1058,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1059,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1060,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1061,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1062,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1063,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1064,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1065,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1066,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1067,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1068,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1069,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1070,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1071,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1072,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1073,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1074,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1075,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1076,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1077,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1078,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1079,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1080,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1081,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1082,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1083,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1084,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1085,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1086,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1087,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1088,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1089,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1090,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1091,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1092,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1093,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1094,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1095,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1096,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1097,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1098,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1099,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1100,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1101,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1102,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1103,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1104,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1105,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1106,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1107,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1108,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1109,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1110,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1111,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1112,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1113,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1114,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1115,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1116,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1117,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1118,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1119,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1120,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1121,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1122,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1123,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1124,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1125,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1126,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1127,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1128,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1129,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1130,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1131,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1132,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1133,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1134,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1135,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1136,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1137,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1138,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1139,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1140,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1141,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1142,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1143,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1144,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1145,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1146,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1147,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1148,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1149,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1150,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1151,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1152,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1153,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1154,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1155,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1156,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1157,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1158,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1159,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1160,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1161,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1162,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1163,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1164,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1165,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1166,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1167,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1168,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1169,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1170,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1171,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1172,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1173,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1174,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1175,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1176,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1177,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1178,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1179,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1180,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1181,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1182,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1183,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1184,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1185,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1186,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1187,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1188,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1189,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1190,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1191,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1192,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1193,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1194,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1195,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1196,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1197,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1198,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1199,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ## [1200,]     TRUE     TRUE    TRUE    TRUE    TRUE    TRUE    TRUE    TRUE
    ##         formic acetic  hcl hno3 o3_max o3106 o3_24  no2 pm10 no_24hr pm2_5_fr
    ##    [1,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##    [2,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##    [3,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##    [4,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##    [5,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##    [6,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##    [7,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##    [8,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##    [9,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [10,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [11,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [12,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [13,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [14,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [15,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [16,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [17,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [18,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [19,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [20,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [21,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [22,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [23,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [24,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [25,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [26,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [27,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [28,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [29,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [30,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [31,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [32,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [33,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [34,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [35,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [36,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [37,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [38,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [39,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [40,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [41,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [42,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [43,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [44,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [45,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [46,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [47,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [48,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [49,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [50,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [51,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [52,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [53,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [54,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [55,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [56,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [57,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [58,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [59,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [60,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [61,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [62,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [63,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [64,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [65,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [66,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [67,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [68,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [69,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [70,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [71,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [72,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [73,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [74,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [75,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [76,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [77,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [78,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [79,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [80,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [81,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [82,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [83,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [84,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [85,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [86,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [87,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [88,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [89,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [90,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [91,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [92,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [93,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [94,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [95,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [96,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [97,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [98,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##   [99,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [100,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [101,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [102,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [103,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [104,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [105,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [106,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [107,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [108,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [109,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [110,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [111,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [112,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [113,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [114,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [115,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [116,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [117,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [118,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [119,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [120,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [121,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [122,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [123,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [124,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [125,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [126,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [127,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [128,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [129,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [130,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [131,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [132,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [133,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [134,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [135,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [136,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [137,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [138,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [139,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [140,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [141,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [142,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [143,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [144,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [145,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [146,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [147,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [148,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [149,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [150,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [151,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [152,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [153,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [154,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [155,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [156,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [157,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [158,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [159,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [160,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [161,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [162,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [163,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [164,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [165,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [166,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [167,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [168,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [169,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [170,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [171,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [172,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [173,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [174,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [175,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [176,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [177,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [178,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [179,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [180,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [181,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [182,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [183,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [184,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [185,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [186,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [187,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [188,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [189,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [190,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [191,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [192,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [193,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [194,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [195,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [196,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [197,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [198,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [199,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [200,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [201,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [202,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [203,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [204,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [205,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [206,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [207,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [208,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [209,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [210,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [211,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [212,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [213,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [214,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [215,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [216,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [217,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [218,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [219,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [220,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [221,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [222,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [223,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [224,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [225,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [226,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [227,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [228,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [229,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [230,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [231,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [232,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [233,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [234,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [235,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [236,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [237,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [238,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [239,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [240,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [241,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [242,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [243,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [244,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [245,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [246,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [247,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [248,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [249,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [250,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [251,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [252,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [253,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [254,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [255,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [256,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [257,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [258,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [259,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [260,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [261,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [262,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [263,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [264,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [265,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [266,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [267,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [268,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [269,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [270,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [271,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [272,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [273,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [274,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [275,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [276,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [277,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [278,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [279,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [280,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [281,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [282,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [283,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [284,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [285,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [286,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [287,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [288,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [289,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [290,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [291,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [292,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [293,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [294,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [295,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [296,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [297,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [298,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [299,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [300,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [301,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [302,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [303,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [304,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [305,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [306,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [307,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [308,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [309,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [310,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [311,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [312,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [313,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [314,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [315,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [316,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [317,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [318,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [319,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [320,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [321,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [322,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [323,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [324,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [325,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [326,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [327,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [328,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [329,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [330,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [331,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [332,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [333,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [334,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [335,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [336,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [337,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [338,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [339,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [340,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [341,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [342,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [343,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [344,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [345,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [346,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [347,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [348,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [349,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [350,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [351,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [352,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [353,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [354,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [355,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [356,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [357,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [358,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [359,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [360,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [361,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [362,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [363,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [364,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [365,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [366,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [367,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [368,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [369,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [370,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [371,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [372,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [373,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [374,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [375,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [376,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [377,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [378,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [379,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [380,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [381,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [382,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [383,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [384,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [385,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [386,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [387,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [388,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [389,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [390,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [391,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [392,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [393,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [394,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [395,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [396,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [397,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [398,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [399,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [400,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE   FALSE     TRUE
    ##  [401,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [402,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [403,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [404,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [405,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [406,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [407,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [408,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [409,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [410,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [411,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [412,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [413,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [414,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [415,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [416,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [417,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [418,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [419,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [420,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [421,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [422,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [423,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [424,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [425,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [426,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [427,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [428,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [429,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [430,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [431,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [432,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [433,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [434,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [435,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [436,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [437,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [438,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [439,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [440,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [441,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [442,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [443,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [444,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [445,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [446,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [447,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [448,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [449,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [450,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [451,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [452,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [453,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [454,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [455,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [456,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [457,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [458,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [459,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [460,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [461,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [462,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [463,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [464,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [465,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [466,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [467,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [468,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [469,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [470,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [471,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [472,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [473,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [474,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [475,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [476,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [477,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [478,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [479,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [480,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [481,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [482,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [483,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [484,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [485,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [486,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [487,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [488,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [489,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [490,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [491,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [492,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [493,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [494,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [495,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [496,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [497,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [498,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [499,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [500,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [501,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [502,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [503,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [504,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [505,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [506,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [507,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [508,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [509,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [510,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [511,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [512,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [513,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [514,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [515,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [516,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [517,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [518,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [519,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [520,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [521,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [522,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [523,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [524,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [525,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [526,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [527,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [528,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [529,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [530,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [531,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [532,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [533,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [534,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [535,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [536,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [537,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [538,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [539,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [540,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [541,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [542,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [543,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [544,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [545,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [546,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [547,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [548,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [549,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [550,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [551,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [552,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [553,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [554,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [555,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [556,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [557,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [558,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [559,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [560,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [561,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [562,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [563,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [564,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [565,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [566,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [567,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [568,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [569,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [570,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [571,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [572,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [573,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [574,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [575,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [576,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [577,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [578,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [579,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [580,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [581,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [582,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [583,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [584,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [585,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [586,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [587,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [588,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [589,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [590,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [591,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [592,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [593,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [594,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [595,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [596,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [597,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [598,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [599,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [600,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE    FALSE
    ##  [601,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [602,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [603,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [604,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [605,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [606,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [607,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [608,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [609,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [610,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [611,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [612,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [613,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [614,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [615,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [616,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [617,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [618,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [619,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [620,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [621,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [622,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [623,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [624,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [625,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [626,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [627,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [628,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [629,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [630,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [631,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [632,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [633,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [634,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [635,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [636,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [637,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [638,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [639,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [640,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [641,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [642,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [643,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [644,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [645,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [646,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [647,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [648,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [649,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [650,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [651,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [652,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [653,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [654,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [655,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [656,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [657,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [658,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [659,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [660,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [661,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [662,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [663,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [664,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [665,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [666,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [667,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [668,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [669,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [670,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [671,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [672,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [673,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [674,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [675,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [676,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [677,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [678,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [679,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [680,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [681,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [682,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [683,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [684,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [685,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [686,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [687,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [688,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [689,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [690,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [691,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [692,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [693,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [694,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [695,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [696,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [697,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [698,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [699,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [700,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [701,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [702,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [703,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [704,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [705,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [706,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [707,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [708,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [709,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [710,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [711,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [712,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [713,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [714,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [715,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [716,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [717,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [718,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [719,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [720,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [721,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [722,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [723,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [724,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [725,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [726,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [727,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [728,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [729,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [730,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [731,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [732,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [733,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [734,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [735,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [736,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [737,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [738,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [739,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [740,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [741,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [742,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [743,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [744,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [745,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [746,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [747,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [748,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [749,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [750,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [751,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [752,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [753,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [754,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [755,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [756,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [757,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [758,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [759,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [760,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [761,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [762,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [763,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [764,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [765,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [766,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [767,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [768,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [769,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [770,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [771,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [772,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [773,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [774,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [775,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [776,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [777,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [778,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [779,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [780,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [781,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [782,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [783,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [784,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [785,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [786,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [787,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [788,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [789,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [790,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [791,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [792,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [793,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [794,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [795,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [796,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [797,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [798,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [799,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [800,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [801,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [802,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [803,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [804,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [805,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [806,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [807,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [808,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [809,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [810,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [811,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [812,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [813,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [814,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [815,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [816,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [817,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [818,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [819,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [820,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [821,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [822,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [823,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [824,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [825,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [826,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [827,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [828,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [829,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [830,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [831,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [832,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [833,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [834,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [835,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [836,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [837,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [838,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [839,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [840,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [841,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [842,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [843,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [844,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [845,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [846,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [847,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [848,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [849,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [850,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [851,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [852,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [853,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [854,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [855,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [856,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [857,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [858,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [859,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [860,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [861,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [862,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [863,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [864,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [865,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [866,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [867,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [868,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [869,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [870,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [871,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [872,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [873,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [874,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [875,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [876,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [877,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [878,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [879,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [880,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [881,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [882,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [883,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [884,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [885,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [886,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [887,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [888,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [889,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [890,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [891,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [892,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [893,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [894,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [895,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [896,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [897,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [898,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [899,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [900,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [901,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [902,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [903,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [904,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [905,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [906,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [907,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [908,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [909,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [910,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [911,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [912,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [913,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [914,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [915,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [916,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [917,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [918,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [919,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [920,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [921,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [922,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [923,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [924,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [925,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [926,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [927,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [928,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [929,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [930,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [931,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [932,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [933,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [934,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [935,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [936,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [937,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [938,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [939,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [940,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [941,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [942,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [943,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [944,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [945,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [946,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [947,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [948,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [949,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [950,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [951,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [952,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [953,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [954,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [955,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [956,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [957,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [958,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [959,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [960,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [961,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [962,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [963,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [964,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [965,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [966,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [967,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [968,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [969,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [970,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [971,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [972,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [973,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [974,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [975,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [976,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [977,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [978,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [979,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [980,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [981,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [982,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [983,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [984,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [985,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [986,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [987,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [988,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [989,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [990,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [991,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [992,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [993,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [994,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [995,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [996,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [997,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [998,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##  [999,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1000,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1001,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1002,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1003,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1004,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1005,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1006,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1007,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1008,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1009,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1010,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1011,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1012,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1013,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1014,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1015,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1016,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1017,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1018,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1019,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1020,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1021,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1022,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1023,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1024,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1025,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1026,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1027,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1028,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1029,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1030,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1031,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1032,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1033,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1034,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1035,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1036,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1037,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1038,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1039,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1040,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1041,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1042,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1043,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1044,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1045,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1046,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1047,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1048,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1049,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1050,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1051,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1052,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1053,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1054,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1055,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1056,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1057,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1058,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1059,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1060,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1061,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1062,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1063,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1064,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1065,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1066,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1067,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1068,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1069,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1070,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1071,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1072,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1073,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1074,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1075,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1076,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1077,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1078,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1079,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1080,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1081,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1082,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1083,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1084,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1085,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1086,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1087,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1088,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1089,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1090,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1091,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1092,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1093,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1094,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1095,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1096,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1097,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1098,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1099,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1100,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1101,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1102,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1103,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1104,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1105,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1106,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1107,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1108,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1109,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1110,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1111,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1112,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1113,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1114,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1115,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1116,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1117,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1118,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1119,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1120,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1121,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1122,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1123,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1124,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1125,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1126,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1127,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1128,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1129,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1130,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1131,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1132,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1133,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1134,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1135,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1136,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1137,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1138,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1139,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1140,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1141,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1142,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1143,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1144,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1145,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1146,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1147,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1148,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1149,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1150,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1151,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1152,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1153,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1154,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1155,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1156,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1157,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1158,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1159,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1160,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1161,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1162,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1163,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1164,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1165,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1166,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1167,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1168,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1169,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1170,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1171,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1172,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1173,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1174,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1175,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1176,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1177,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1178,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1179,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1180,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1181,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1182,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1183,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1184,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1185,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1186,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1187,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1188,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1189,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1190,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1191,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1192,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1193,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1194,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1195,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1196,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1197,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1198,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1199,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ## [1200,]   TRUE   TRUE TRUE TRUE   TRUE  TRUE  TRUE TRUE TRUE    TRUE     TRUE
    ##         iacid oacid total_acids  lon  lat
    ##    [1,]  TRUE  TRUE        TRUE TRUE TRUE
    ##    [2,]  TRUE  TRUE        TRUE TRUE TRUE
    ##    [3,]  TRUE  TRUE        TRUE TRUE TRUE
    ##    [4,]  TRUE  TRUE        TRUE TRUE TRUE
    ##    [5,]  TRUE  TRUE        TRUE TRUE TRUE
    ##    [6,]  TRUE  TRUE        TRUE TRUE TRUE
    ##    [7,]  TRUE  TRUE        TRUE TRUE TRUE
    ##    [8,]  TRUE  TRUE        TRUE TRUE TRUE
    ##    [9,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [10,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [11,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [12,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [13,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [14,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [15,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [16,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [17,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [18,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [19,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [20,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [21,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [22,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [23,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [24,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [25,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [26,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [27,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [28,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [29,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [30,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [31,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [32,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [33,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [34,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [35,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [36,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [37,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [38,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [39,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [40,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [41,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [42,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [43,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [44,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [45,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [46,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [47,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [48,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [49,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [50,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [51,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [52,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [53,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [54,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [55,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [56,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [57,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [58,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [59,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [60,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [61,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [62,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [63,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [64,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [65,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [66,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [67,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [68,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [69,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [70,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [71,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [72,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [73,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [74,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [75,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [76,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [77,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [78,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [79,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [80,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [81,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [82,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [83,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [84,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [85,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [86,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [87,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [88,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [89,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [90,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [91,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [92,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [93,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [94,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [95,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [96,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [97,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [98,]  TRUE  TRUE        TRUE TRUE TRUE
    ##   [99,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [100,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [101,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [102,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [103,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [104,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [105,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [106,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [107,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [108,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [109,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [110,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [111,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [112,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [113,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [114,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [115,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [116,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [117,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [118,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [119,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [120,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [121,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [122,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [123,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [124,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [125,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [126,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [127,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [128,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [129,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [130,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [131,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [132,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [133,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [134,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [135,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [136,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [137,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [138,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [139,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [140,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [141,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [142,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [143,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [144,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [145,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [146,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [147,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [148,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [149,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [150,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [151,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [152,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [153,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [154,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [155,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [156,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [157,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [158,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [159,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [160,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [161,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [162,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [163,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [164,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [165,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [166,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [167,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [168,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [169,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [170,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [171,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [172,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [173,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [174,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [175,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [176,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [177,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [178,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [179,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [180,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [181,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [182,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [183,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [184,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [185,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [186,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [187,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [188,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [189,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [190,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [191,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [192,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [193,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [194,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [195,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [196,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [197,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [198,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [199,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [200,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [201,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [202,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [203,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [204,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [205,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [206,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [207,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [208,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [209,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [210,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [211,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [212,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [213,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [214,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [215,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [216,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [217,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [218,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [219,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [220,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [221,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [222,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [223,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [224,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [225,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [226,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [227,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [228,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [229,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [230,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [231,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [232,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [233,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [234,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [235,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [236,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [237,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [238,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [239,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [240,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [241,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [242,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [243,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [244,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [245,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [246,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [247,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [248,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [249,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [250,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [251,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [252,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [253,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [254,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [255,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [256,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [257,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [258,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [259,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [260,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [261,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [262,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [263,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [264,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [265,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [266,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [267,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [268,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [269,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [270,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [271,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [272,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [273,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [274,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [275,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [276,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [277,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [278,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [279,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [280,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [281,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [282,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [283,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [284,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [285,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [286,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [287,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [288,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [289,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [290,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [291,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [292,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [293,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [294,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [295,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [296,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [297,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [298,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [299,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [300,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [301,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [302,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [303,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [304,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [305,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [306,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [307,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [308,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [309,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [310,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [311,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [312,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [313,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [314,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [315,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [316,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [317,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [318,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [319,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [320,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [321,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [322,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [323,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [324,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [325,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [326,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [327,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [328,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [329,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [330,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [331,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [332,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [333,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [334,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [335,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [336,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [337,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [338,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [339,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [340,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [341,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [342,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [343,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [344,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [345,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [346,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [347,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [348,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [349,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [350,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [351,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [352,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [353,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [354,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [355,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [356,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [357,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [358,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [359,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [360,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [361,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [362,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [363,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [364,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [365,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [366,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [367,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [368,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [369,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [370,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [371,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [372,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [373,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [374,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [375,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [376,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [377,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [378,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [379,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [380,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [381,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [382,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [383,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [384,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [385,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [386,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [387,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [388,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [389,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [390,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [391,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [392,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [393,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [394,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [395,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [396,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [397,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [398,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [399,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [400,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [401,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [402,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [403,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [404,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [405,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [406,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [407,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [408,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [409,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [410,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [411,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [412,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [413,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [414,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [415,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [416,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [417,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [418,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [419,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [420,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [421,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [422,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [423,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [424,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [425,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [426,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [427,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [428,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [429,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [430,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [431,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [432,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [433,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [434,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [435,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [436,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [437,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [438,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [439,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [440,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [441,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [442,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [443,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [444,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [445,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [446,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [447,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [448,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [449,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [450,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [451,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [452,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [453,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [454,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [455,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [456,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [457,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [458,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [459,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [460,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [461,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [462,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [463,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [464,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [465,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [466,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [467,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [468,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [469,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [470,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [471,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [472,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [473,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [474,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [475,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [476,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [477,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [478,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [479,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [480,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [481,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [482,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [483,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [484,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [485,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [486,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [487,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [488,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [489,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [490,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [491,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [492,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [493,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [494,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [495,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [496,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [497,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [498,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [499,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [500,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [501,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [502,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [503,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [504,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [505,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [506,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [507,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [508,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [509,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [510,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [511,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [512,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [513,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [514,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [515,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [516,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [517,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [518,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [519,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [520,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [521,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [522,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [523,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [524,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [525,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [526,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [527,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [528,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [529,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [530,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [531,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [532,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [533,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [534,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [535,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [536,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [537,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [538,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [539,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [540,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [541,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [542,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [543,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [544,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [545,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [546,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [547,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [548,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [549,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [550,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [551,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [552,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [553,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [554,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [555,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [556,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [557,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [558,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [559,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [560,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [561,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [562,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [563,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [564,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [565,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [566,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [567,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [568,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [569,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [570,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [571,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [572,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [573,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [574,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [575,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [576,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [577,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [578,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [579,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [580,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [581,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [582,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [583,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [584,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [585,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [586,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [587,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [588,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [589,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [590,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [591,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [592,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [593,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [594,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [595,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [596,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [597,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [598,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [599,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [600,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [601,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [602,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [603,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [604,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [605,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [606,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [607,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [608,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [609,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [610,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [611,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [612,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [613,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [614,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [615,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [616,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [617,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [618,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [619,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [620,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [621,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [622,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [623,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [624,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [625,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [626,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [627,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [628,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [629,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [630,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [631,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [632,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [633,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [634,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [635,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [636,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [637,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [638,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [639,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [640,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [641,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [642,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [643,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [644,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [645,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [646,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [647,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [648,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [649,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [650,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [651,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [652,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [653,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [654,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [655,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [656,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [657,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [658,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [659,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [660,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [661,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [662,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [663,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [664,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [665,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [666,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [667,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [668,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [669,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [670,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [671,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [672,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [673,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [674,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [675,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [676,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [677,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [678,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [679,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [680,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [681,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [682,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [683,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [684,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [685,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [686,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [687,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [688,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [689,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [690,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [691,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [692,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [693,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [694,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [695,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [696,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [697,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [698,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [699,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [700,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [701,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [702,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [703,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [704,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [705,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [706,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [707,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [708,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [709,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [710,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [711,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [712,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [713,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [714,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [715,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [716,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [717,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [718,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [719,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [720,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [721,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [722,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [723,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [724,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [725,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [726,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [727,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [728,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [729,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [730,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [731,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [732,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [733,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [734,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [735,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [736,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [737,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [738,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [739,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [740,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [741,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [742,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [743,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [744,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [745,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [746,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [747,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [748,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [749,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [750,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [751,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [752,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [753,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [754,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [755,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [756,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [757,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [758,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [759,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [760,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [761,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [762,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [763,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [764,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [765,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [766,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [767,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [768,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [769,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [770,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [771,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [772,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [773,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [774,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [775,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [776,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [777,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [778,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [779,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [780,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [781,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [782,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [783,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [784,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [785,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [786,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [787,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [788,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [789,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [790,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [791,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [792,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [793,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [794,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [795,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [796,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [797,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [798,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [799,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [800,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [801,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [802,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [803,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [804,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [805,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [806,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [807,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [808,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [809,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [810,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [811,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [812,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [813,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [814,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [815,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [816,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [817,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [818,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [819,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [820,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [821,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [822,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [823,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [824,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [825,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [826,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [827,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [828,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [829,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [830,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [831,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [832,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [833,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [834,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [835,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [836,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [837,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [838,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [839,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [840,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [841,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [842,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [843,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [844,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [845,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [846,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [847,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [848,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [849,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [850,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [851,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [852,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [853,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [854,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [855,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [856,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [857,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [858,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [859,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [860,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [861,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [862,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [863,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [864,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [865,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [866,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [867,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [868,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [869,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [870,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [871,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [872,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [873,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [874,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [875,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [876,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [877,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [878,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [879,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [880,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [881,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [882,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [883,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [884,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [885,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [886,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [887,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [888,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [889,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [890,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [891,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [892,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [893,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [894,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [895,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [896,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [897,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [898,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [899,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [900,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [901,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [902,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [903,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [904,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [905,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [906,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [907,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [908,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [909,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [910,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [911,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [912,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [913,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [914,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [915,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [916,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [917,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [918,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [919,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [920,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [921,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [922,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [923,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [924,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [925,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [926,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [927,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [928,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [929,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [930,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [931,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [932,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [933,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [934,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [935,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [936,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [937,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [938,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [939,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [940,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [941,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [942,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [943,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [944,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [945,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [946,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [947,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [948,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [949,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [950,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [951,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [952,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [953,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [954,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [955,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [956,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [957,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [958,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [959,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [960,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [961,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [962,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [963,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [964,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [965,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [966,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [967,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [968,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [969,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [970,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [971,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [972,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [973,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [974,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [975,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [976,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [977,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [978,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [979,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [980,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [981,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [982,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [983,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [984,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [985,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [986,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [987,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [988,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [989,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [990,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [991,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [992,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [993,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [994,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [995,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [996,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [997,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [998,]  TRUE  TRUE        TRUE TRUE TRUE
    ##  [999,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1000,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1001,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1002,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1003,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1004,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1005,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1006,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1007,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1008,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1009,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1010,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1011,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1012,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1013,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1014,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1015,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1016,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1017,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1018,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1019,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1020,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1021,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1022,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1023,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1024,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1025,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1026,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1027,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1028,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1029,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1030,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1031,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1032,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1033,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1034,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1035,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1036,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1037,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1038,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1039,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1040,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1041,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1042,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1043,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1044,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1045,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1046,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1047,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1048,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1049,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1050,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1051,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1052,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1053,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1054,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1055,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1056,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1057,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1058,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1059,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1060,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1061,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1062,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1063,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1064,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1065,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1066,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1067,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1068,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1069,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1070,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1071,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1072,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1073,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1074,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1075,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1076,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1077,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1078,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1079,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1080,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1081,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1082,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1083,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1084,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1085,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1086,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1087,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1088,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1089,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1090,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1091,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1092,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1093,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1094,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1095,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1096,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1097,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1098,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1099,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1100,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1101,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1102,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1103,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1104,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1105,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1106,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1107,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1108,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1109,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1110,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1111,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1112,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1113,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1114,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1115,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1116,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1117,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1118,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1119,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1120,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1121,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1122,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1123,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1124,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1125,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1126,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1127,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1128,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1129,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1130,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1131,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1132,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1133,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1134,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1135,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1136,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1137,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1138,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1139,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1140,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1141,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1142,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1143,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1144,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1145,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1146,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1147,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1148,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1149,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1150,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1151,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1152,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1153,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1154,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1155,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1156,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1157,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1158,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1159,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1160,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1161,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1162,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1163,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1164,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1165,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1166,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1167,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1168,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1169,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1170,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1171,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1172,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1173,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1174,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1175,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1176,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1177,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1178,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1179,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1180,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1181,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1182,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1183,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1184,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1185,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1186,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1187,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1188,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1189,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1190,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1191,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1192,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1193,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1194,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1195,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1196,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1197,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1198,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1199,]  TRUE  TRUE        TRUE TRUE TRUE
    ## [1200,]  TRUE  TRUE        TRUE TRUE TRUE

\#impute missing values with average from “male” and “hispanic”

``` r
data[(hispanic == 1) & (male== 1), mean(agepft:lat)] 
```

    ## Warning in agepft:lat: numerical expression has 266 elements: only the first
    ## used

    ## Warning in agepft:lat: numerical expression has 266 elements: only the first
    ## used

    ## [1] 21.54894

``` r
data[hispanic == 1 & male== 1,                    
        lapply(.SD, mean)]
```

    ## Warning in mean.default(X[[i]], ...): argument is not numeric or logical:
    ## returning NA

    ## Warning in mean.default(X[[i]], ...): argument is not numeric or logical:
    ## returning NA

    ##    townname      sid male race hispanic agepft height weight bmi asthma
    ## 1:       NA 1019.143    1   NA        1     NA     NA     NA  NA     NA
    ##    active_asthma father_asthma mother_asthma wheeze hayfever allergy
    ## 1:     0.2030075            NA            NA     NA       NA      NA
    ##    educ_parent smoke      pets gasstove fev fvc mmef pm25_mass pm25_so4
    ## 1:          NA    NA 0.7030075       NA  NA  NA   NA  14.74887 1.894962
    ##    pm25_no3 pm25_nh4  pm25_oc  pm25_ec  pm25_om  pm10_oc   pm10_ec  pm10_tc
    ## 1: 4.700113 1.827857 4.694925 0.734812 5.632519 6.016692 0.7513534 6.966128
    ##      formic   acetic       hcl     hno3   o3_max    o3106    o3_24      no2
    ## 1: 1.318271 3.030752 0.4209774 2.366541 60.59534 48.29346 30.20639 18.37673
    ##        pm10 no_24hr pm2_5_fr    iacid    oacid total_acids       lon      lat
    ## 1: 33.66026      NA       NA 2.787519 4.349023    6.715564 -118.2708 34.17992
