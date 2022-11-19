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
    ##          expr     min       lq      mean   median      uq      max neval
    ##     fun1(dat) 339.529 410.5130 501.87035 426.5720 485.976 1919.503   100
    ##  fun1alt(dat)  36.173  37.2895  56.70937  39.0495  48.584 1096.005   100

``` r
# Test for the second
microbenchmark::microbenchmark(
  fun2(dat),
  fun2alt(dat), unit = "microseconds", check = "equivalent"
)
```

    ## Unit: microseconds
    ##          expr      min       lq     mean   median       uq       max neval
    ##     fun2(dat) 2145.910 2307.372 2838.574 2683.417 3255.910  4655.034   100
    ##  fun2alt(dat)  518.166  898.036 1561.027 1023.272 1376.139 23807.590   100

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
    ##   3.303   0.917   4.284

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
    ##   0.004   0.000   2.269

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

### Question 1

How many many movies are there available in each rating category?

``` sql
SELECT rating, COUNT(*) AS 'number of movies'
FROM film
GROUP BY rating
```

| rating | number of movies |
|:-------|-----------------:|
| G      |              180 |
| NC-17  |              210 |
| PG     |              194 |
| PG-13  |              223 |
| R      |              195 |

5 records

### Question 2

What is the average replacement cost and rental rate for each rating
category.

``` sql
SELECT rating,
       AVG(replacement_cost) AS avg_replacementcost,
       AVG(rental_rate) AS avg_rentalrate
FROM film
GROUP BY rating
```

| rating | avg_replacementcost | avg_rentalrate |
|:-------|--------------------:|---------------:|
| G      |            20.12333 |       2.912222 |
| NC-17  |            20.13762 |       2.970952 |
| PG     |            18.95907 |       3.051856 |
| PG-13  |            20.40256 |       3.034843 |
| R      |            20.23103 |       2.938718 |

5 records

### Question 3

Use table film_category together with film to find the how many films
there are with each category ID

``` sql
SELECT film_category.category_id, COUNT(film.film_id) as 'number of films'
FROM film
INNER JOIN film_category
ON film.film_id = film_category.category_id
GROUP BY category_id
```

| category_id | number of films |
|:------------|----------------:|
| 1           |              64 |
| 2           |              66 |
| 3           |              60 |
| 4           |              57 |
| 5           |              58 |
| 6           |              68 |
| 7           |              62 |
| 8           |              69 |
| 9           |              73 |
| 10          |              61 |

Displaying records 1 - 10

### Question 4

Incorporate table category into the answer to the previous question to
find the name of the most popular category.

``` sql
SELECT film_category.category_id, category.name, COUNT(film.film_id) as number_of_films
FROM ((film 
INNER JOIN film_category ON film.film_id = film_category.film_id)
INNER JOIN category ON film_category.category_id = category.category_id)
GROUP BY film_category.category_id 
ORDER BY number_of_films DESC
```

| category_id | name        | number_of_films |
|------------:|:------------|----------------:|
|          15 | Sports      |              74 |
|           9 | Foreign     |              73 |
|           8 | Family      |              69 |
|           6 | Documentary |              68 |
|           2 | Animation   |              66 |
|           1 | Action      |              64 |
|          13 | New         |              63 |
|           7 | Drama       |              62 |
|          14 | Sci-Fi      |              61 |
|          10 | Games       |              61 |

Displaying records 1 - 10

The most popular category is ‘Sports’.
