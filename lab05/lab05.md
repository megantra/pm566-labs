lab05
================
Megan Tran
\`September 21, 2022

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(tidyverse)
```

    ## ── Attaching packages
    ## ───────────────────────────────────────
    ## tidyverse 1.3.2 ──

    ## ✔ ggplot2 3.3.6     ✔ purrr   0.3.4
    ## ✔ tibble  3.1.8     ✔ dplyr   1.0.9
    ## ✔ tidyr   1.2.0     ✔ stringr 1.4.1
    ## ✔ readr   2.1.2     ✔ forcats 0.5.2
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ lubridate::as.difftime() masks base::as.difftime()
    ## ✖ lubridate::date()        masks base::date()
    ## ✖ dplyr::filter()          masks stats::filter()
    ## ✖ lubridate::intersect()   masks base::intersect()
    ## ✖ dplyr::lag()             masks stats::lag()
    ## ✖ lubridate::setdiff()     masks base::setdiff()
    ## ✖ lubridate::union()       masks base::union()

``` r
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
    ## 
    ## The following objects are masked from 'package:lubridate':
    ## 
    ##     hour, isoweek, mday, minute, month, quarter, second, wday, week,
    ##     yday, year

``` r
library(dtplyr)
```

## 1. Read in the data

``` r
if (!file.exists("../lab03/met_all.gz")) {
download.file("https://raw.githubusercontent.com/USCbiostats/data-science-data/master/02_met/met_all.gz", "met_all.gz", method="libcurl", timeout = 60) 
}
met <- data.table::fread("../lab03/met_all.gz") 
```

Remove temperatures less than -17C and Make sure there are no missing
data in the key variables coded as 9999, 999, etc

``` r
met <- met[temp> -17] [elev == 9999.0, elev := NA]
```

Read in the stations data:

``` r
# Download the data
stations <- fread("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv")
stations[, USAF := as.integer(USAF)]
```

    ## Warning in eval(jsub, SDenv, parent.frame()): NAs introduced by coercion

``` r
# Dealing with NAs and 999999
stations[, USAF   := fifelse(USAF == 999999, NA_integer_, USAF)]
stations[, CTRY   := fifelse(CTRY == "", NA_character_, CTRY)]
stations[, STATE  := fifelse(STATE == "", NA_character_, STATE)]

# Selecting the three relevant columns, and keeping unique records
stations <- unique(stations[, list(USAF, CTRY, STATE)])

# Dropping NAs
stations <- stations[!is.na(USAF)]

# Removing duplicates
stations[, n := 1:.N, by = .(USAF)]
stations <- stations[n == 1,][, n := NULL]
```

merge met data with stations.

``` r
met <-met

merge(
  # Data
  x     = met,      
  y     = stations, 
  # List of variables to match
  by.x  = "USAFID",
  by.y  = "USAF", 
  # Which obs to keep?
  all.x = TRUE,      
  all.y = FALSE
  ) 
```

    ##          USAFID  WBAN year month day hour min    lat      lon elev wind.dir
    ##       1: 690150 93121 2019     8   1    0  56 34.300 -116.166  696      220
    ##       2: 690150 93121 2019     8   1    1  56 34.300 -116.166  696      230
    ##       3: 690150 93121 2019     8   1    2  56 34.300 -116.166  696      230
    ##       4: 690150 93121 2019     8   1    3  56 34.300 -116.166  696      210
    ##       5: 690150 93121 2019     8   1    4  56 34.300 -116.166  696      120
    ##      ---                                                                   
    ## 2317200: 726813 94195 2019     8  31   19  56 43.650 -116.633  741       70
    ## 2317201: 726813 94195 2019     8  31   20  56 43.650 -116.633  741       NA
    ## 2317202: 726813 94195 2019     8  31   21  56 43.650 -116.633  741       10
    ## 2317203: 726813 94195 2019     8  31   22  56 43.642 -116.636  741       10
    ## 2317204: 726813 94195 2019     8  31   23  56 43.642 -116.636  741       40
    ##          wind.dir.qc wind.type.code wind.sp wind.sp.qc ceiling.ht ceiling.ht.qc
    ##       1:           5              N     5.7          5      22000             5
    ##       2:           5              N     8.2          5      22000             5
    ##       3:           5              N     6.7          5      22000             5
    ##       4:           5              N     5.1          5      22000             5
    ##       5:           5              N     2.1          5      22000             5
    ##      ---                                                                       
    ## 2317200:           5              N     2.1          5      22000             5
    ## 2317201:           9              C     0.0          5      22000             5
    ## 2317202:           5              N     2.6          5      22000             5
    ## 2317203:           1              N     2.1          1      22000             1
    ## 2317204:           1              N     2.1          1      22000             1
    ##          ceiling.ht.method sky.cond vis.dist vis.dist.qc vis.var vis.var.qc
    ##       1:                 9        N    16093           5       N          5
    ##       2:                 9        N    16093           5       N          5
    ##       3:                 9        N    16093           5       N          5
    ##       4:                 9        N    16093           5       N          5
    ##       5:                 9        N    16093           5       N          5
    ##      ---                                                                   
    ## 2317200:                 9        N    16093           5       N          5
    ## 2317201:                 9        N    16093           5       N          5
    ## 2317202:                 9        N    14484           5       N          5
    ## 2317203:                 9        N    16093           1       9          9
    ## 2317204:                 9        N    16093           1       9          9
    ##          temp temp.qc dew.point dew.point.qc atm.press atm.press.qc       rh
    ##       1: 37.2       5      10.6            5    1009.9            5 19.88127
    ##       2: 35.6       5      10.6            5    1010.3            5 21.76098
    ##       3: 34.4       5       7.2            5    1010.6            5 18.48212
    ##       4: 33.3       5       5.0            5    1011.6            5 16.88862
    ##       5: 32.8       5       5.0            5    1012.7            5 17.38410
    ##      ---                                                                    
    ## 2317200: 32.2       5      12.2            5    1012.8            5 29.40686
    ## 2317201: 33.3       5      12.2            5    1011.6            5 27.60422
    ## 2317202: 35.0       5       9.4            5    1010.8            5 20.76325
    ## 2317203: 34.4       1       9.4            1    1010.1            1 21.48631
    ## 2317204: 34.4       1       9.4            1    1009.6            1 21.48631
    ##          CTRY STATE
    ##       1:   US    CA
    ##       2:   US    CA
    ##       3:   US    CA
    ##       4:   US    CA
    ##       5:   US    CA
    ##      ---           
    ## 2317200:   US    ID
    ## 2317201:   US    ID
    ## 2317202:   US    ID
    ## 2317203:   US    ID
    ## 2317204:   US    ID

``` r
nrow(met)
```

    ## [1] 2317204

\##Question 1: Representative station for the US

Compute mean temperature, wind speed and atmospheric pressure for each
weather station, and pick the weather station with the average value
closest to the median for the US.

``` r
station_averages <-
  met[ , .(
    temp = mean (temp, na.rm=T),
    wind.sp = mean(wind.sp, na.rm=T),
    atm.press = mean(atm.press, na.rm=T)
  ), by = USAFID]
```

The above computes the mean by weather station. Now let’s compute the
median value for each variable.

``` r
stmeds <- station_averages[ , .(
          temp50     = median(temp, na.rm=T),
          windsp50   = median(wind.sp, na.rm=T),
          atmpress50 = median(atm.press,na.rm=T)
)]
stmeds
```

    ##      temp50 windsp50 atmpress50
    ## 1: 23.68406 2.463685   1014.691

A helpful function we might want to use ’which.min()”

``` r
station_averages [ , 
                   temp_dist50 := abs(temp - stmeds$temp50)][order(temp_dist50)]
```

    ##       USAFID      temp   wind.sp atm.press  temp_dist50
    ##    1: 720458 23.681730  1.209682       NaN  0.002328907
    ##    2: 725515 23.686388  2.709164       NaN  0.002328907
    ##    3: 725835 23.678347  2.652381       NaN  0.005712423
    ##    4: 724509 23.675100  4.066833  1013.863  0.008959632
    ##    5: 720538 23.665932  1.907897       NaN  0.018127186
    ##   ---                                                  
    ## 1584: 722788 36.852459  3.393852       NaN 13.168399783
    ## 1585: 722787 37.258907  2.847381       NaN 13.574848130
    ## 1586: 723805 37.625391  3.532935  1005.207 13.941331392
    ## 1587: 726130  9.189602 12.239908       NaN 14.494456787
    ## 1588: 720385  8.044959  7.298963       NaN 15.639100105

Let’s use which.min

``` r
station_averages[ which.min(temp_dist50)]
```

    ##    USAFID     temp  wind.sp atm.press temp_dist50
    ## 1: 720458 23.68173 1.209682       NaN 0.002328907

It matches the result above.

``` r
station_averages [ , 
                   wind.sp_dist50 := abs(wind.sp - stmeds$wind.sp50)][order(wind.sp_dist50)]
```

    ##       USAFID     temp  wind.sp atm.press temp_dist50 wind.sp_dist50
    ##    1: 690150 33.18763 3.483560  1010.379   9.5035752             NA
    ##    2: 720110 31.22003 2.138348       NaN   7.5359677             NA
    ##    3: 720113 23.29317 2.470298       NaN   0.3908894             NA
    ##    4: 720120 27.01922 2.503079       NaN   3.3351568             NA
    ##    5: 720137 21.88823 1.979335       NaN   1.7958292             NA
    ##   ---                                                              
    ## 1584: 726777 19.15492 4.673878  1014.299   4.5291393             NA
    ## 1585: 726797 18.78980 2.858281  1014.902   4.8942607             NA
    ## 1586: 726798 19.47014 4.449337  1014.072   4.2139153             NA
    ## 1587: 726810 25.03549 3.037356  1011.730   1.3514356             NA
    ## 1588: 726813 23.47809 2.435372  1012.315   0.2059716             NA

``` r
station_averages [ , 
                   atmpress_dist50 := abs(atm.press - stmeds$atmpress50)][order(atmpress_dist50)]
```

    ##       USAFID     temp  wind.sp atm.press temp_dist50 wind.sp_dist50
    ##    1: 722238 26.13978 1.470619  1014.691   2.4557194             NA
    ##    2: 723200 25.82436 1.537661  1014.692   2.1403044             NA
    ##    3: 726539 19.66441 4.992422  1014.693   4.0196479             NA
    ##    4: 726155 19.96899 1.961019  1014.689   3.7150670             NA
    ##    5: 725245 23.27650 4.178176  1014.696   0.4075615             NA
    ##   ---                                                              
    ## 1584: 726605 19.21773 3.069577       NaN   4.4663320             NA
    ## 1585: 726626 16.71839 2.296709       NaN   6.9656643             NA
    ## 1586: 726679 18.34283 1.392624       NaN   5.3412312             NA
    ## 1587: 726682 19.85487 1.872846       NaN   3.8291864             NA
    ## 1588: 726764 14.33917 2.144424       NaN   9.3448858             NA
    ##       atmpress_dist50
    ##    1:    0.0005376377
    ##    2:    0.0005376377
    ##    3:    0.0018782247
    ##    4:    0.0024385972
    ##    5:    0.0046850588
    ##   ---                
    ## 1584:             NaN
    ## 1585:             NaN
    ## 1586:             NaN
    ## 1587:             NaN
    ## 1588:             NaN

## Question 2: Representative station per state

Just like the previous question, you are asked to identify what is the
most representative, the median, station per state. This time, instead
of looking at one variable at a time, look at the euclidean distance. If
multiple stations show in the median, select the one located at the
lowest latitude.
