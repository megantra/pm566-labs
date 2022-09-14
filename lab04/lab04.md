Lab 04
================
Megan Tran
\`September 14, 2022

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

## Step 1 Read in the data

First download and then read in with data.table:fread()

``` r
if (!file.exists("met_all.gz")) {
download.file("https://raw.githubusercontent.com/USCbiostats/data-science-data/master/02_met/met_all.gz", "met_all.gz", method="libcurl", timeout = 60) 
}
met <- data.table::fread("met_all.gz") 
```

\##Step 2 Prepare the Data

Remove temperatures less than -17C and Make sure there are no missing
data in the key variables coded as 9999, 999, etc

``` r
met <- met[temp> -17] [elev == 9999.0, elev := NA]
```

# check no 9999s in other important variables

Generate a date variable using the functions as.Date() (hint: You will
need the following to create a date paste(year, month, day, sep = “-”)).

``` r
met <- met[ , ymd:= as.Date(paste(year, month, day, sep = "-"))]
```

Using the data.table::week function, keep the observations of the first
week of the month.

``` r
met[, table(week(ymd))]
```

    ## 
    ##     31     32     33     34     35 
    ## 297259 521600 527922 523847 446576

``` r
met <- met[week(ymd)==31]
```

Compute the mean by station of the variables temp, rh, wind.sp,
vis.dist, dew.point, lat, lon, and elev.

``` r
met [, .(
  temp = max(temp,na.rm=T),
  rh = max(rh,na.rm=T),
  wind.sp = max(wind.sp,na.rm=T),
  vis.dist = max(vis.dist,na.rm=T),
  dew.point = max(dew.point,na.rm=T),
  lat = max(lat,na.rm=T),
  lon = max(lon,na.rm=T),
  elev = max(elev,na.rm=T)
)]
```

    ##    temp  rh wind.sp vis.dist dew.point    lat     lon elev
    ## 1:   47 100    20.6   144841        29 48.941 -68.313 4113

Great! No more 9999s in our dataset.

``` r
met [, .(
  temp = mean(temp,na.rm=T),
  rh = mean(rh,na.rm=T),
  wind.sp = mean(wind.sp,na.rm=T),
  vis.dist = mean(vis.dist,na.rm=T),
  dew.point = mean(dew.point,na.rm=T),
  lat = mean(lat,na.rm=T),
  lon = mean(lon,na.rm=T),
  elev = mean(elev,na.rm=T)
), by = 'USAFID'] 
```

    ##       USAFID     temp       rh  wind.sp vis.dist dew.point      lat       lon
    ##    1: 690150 33.79167 14.93604 3.932292 16042.72  2.641667 34.29992 -116.1659
    ##    2: 720110 30.95139 51.18987 1.785069 15936.55 18.763889 30.78400  -98.6620
    ##    3: 720113 23.65403 57.09799 1.476344 16080.02 13.861828 42.54300  -83.1780
    ##    4: 720120 25.59172 88.10970 1.580473 15402.63 23.378698 32.21746  -80.6998
    ##    5: 720137 22.39965 65.79964 0.937500 16059.47 14.828125 41.42500  -88.4190
    ##   ---                                                                        
    ## 1571: 726777 24.16040 61.50894 4.110891 15885.87 15.095050 46.35760 -104.2504
    ## 1572: 726797 20.29831 58.04065 3.594915 16024.82 10.366102 45.78772 -111.1598
    ## 1573: 726798 22.05372 46.65441 4.643802 15268.41  8.837168 45.69800 -110.4400
    ## 1574: 726810 27.22389 33.18024 2.459459 16079.83  8.303540 43.56700 -116.2390
    ## 1575: 726813 26.62500 41.57813 2.636458 16059.48 11.586458 43.64983 -116.6331
    ##            elev
    ##    1:  694.5208
    ##    2:  336.0000
    ##    3:  222.0000
    ##    4:    6.0000
    ##    5:  178.0000
    ##   ---          
    ## 1571:  906.0000
    ## 1572: 1356.1780
    ## 1573: 1408.0000
    ## 1574:  873.8584
    ## 1575:  741.0000

Create a region variable for NW, SW, NE, SE based on lon = -98.00 and
lat = 39.71 degrees Create a categorical variable for elevation as in
the lecture slides
