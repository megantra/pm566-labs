Homework 4
================
Megan Tran
November 8, 2022

``` r
library(parallel)
library(RSQLite)
library(DBI)
```

# HPC

## Problem 1: Make sure your code is nice

``` r
# Total row sums
fun1 <- function(mat) {
  n <- nrow(mat)
  ans <- double(n) 
  for (i in 1:n) {
    ans[i] <- sum(mat[i, ])
  }
  ans
}
fun1alt <- function(mat) {
  rowSums(mat)
}

# Cumulative sum by row
fun2 <- function(mat) {
  n <- nrow(mat)
  k <- ncol(mat)
  ans <- mat
  for (i in 1:n) {
    for (j in 2:k) {
      ans[i,j] <- mat[i, j] + ans[i, j - 1]
    }
  }
  ans
}

fun2alt <- function(mat) {
  t(apply(mat,1,cumsum))
}
# Use the data with this code
set.seed(2315)
dat <- matrix(rnorm(200 * 100), nrow = 200)

# Test for the first
microbenchmark::microbenchmark(
  fun1(dat),
  fun1alt(dat), unit = "microseconds", check = "equivalent"
)
```

    ## Unit: microseconds
    ##          expr     min       lq      mean   median       uq      max neval
    ##     fun1(dat) 389.879 419.8025 550.97302 494.3110 682.7880 1054.292   100
    ##  fun1alt(dat)  36.323  38.2585  61.22459  42.8395  51.5425 1463.820   100

``` r
# Test for the second
microbenchmark::microbenchmark(
  fun2(dat),
  fun2alt(dat), unit = "microseconds", check = "equivalent"
)
```

    ## Unit: microseconds
    ##          expr      min       lq     mean   median       uq       max neval
    ##     fun2(dat) 2164.898 2235.101 2478.668 2340.383 2639.184  3684.716   100
    ##  fun2alt(dat)  525.057  921.837 1382.650 1004.152 1258.095 16980.224   100

## Problem 2: Make things run faster with parallel computing

The following function allows simulating PI

``` r
sim_pi <- function(n = 1000, i = NULL) {
  p <- matrix(runif(n*2), ncol = 2)
  mean(rowSums(p^2) < 1) * 4
}

# Here is an example of the run
set.seed(156)
sim_pi(1000) # 3.132
```

    ## [1] 3.132

In order to get accurate estimates, we can run this function multiple
times, with the following code:

``` r
# This runs the simulation a 4,000 times, each with 10,000 points
set.seed(1231)
system.time({
  ans <- unlist(lapply(1:4000, sim_pi, n = 10000))
  print(mean(ans))
})
```

    ## [1] 3.14124

    ##    user  system elapsed 
    ##   3.275   0.894   4.229

Rewrite the previous code using parLapply() to make it run faster. Make
sure you set the seed using clusterSetRNGStream():

``` r
cl <- parallel::makePSOCKcluster(2L)
clusterSetRNGStream(cl, iseed = 1231)
system.time({
  ans <- unlist(parLapply(cl, 1:4000, sim_pi, n = 10000))
  print(mean(ans))
  parallel::stopCluster(cl)
})
```

    ## [1] 3.141577

    ##    user  system elapsed 
    ##   0.004   0.001   2.500

The new code ran twice as fast as the first code.

# SQL

Setup a temporary database by running the following chunk

``` r
# install.packages(c("RSQLite", "DBI"))

library(RSQLite)
library(DBI)

# Initialize a temporary in memory database
con <- dbConnect(SQLite(), ":memory:")

# Download tables
film <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/film.csv")
film_category <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/film_category.csv")
category <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/category.csv")

# Copy data.frames to database
dbWriteTable(con, "film", film)
dbWriteTable(con, "film_category", film_category)
dbWriteTable(con, "category", category)
```
