Lab 05 - Wrangling spatial data
================
Anaelle Gackiere
02-13-2026

### Load packages and data

``` r
library(tidyverse) 
library(dsbox) 
```

``` r
states <- read_csv("data/states.csv")
```

### Exercise 1

``` r
# Filter the Denny’s df for Alaska
dn_ak <- dennys %>%
  filter(state == "AK")
nrow(dn_ak)
```

    ## [1] 3

``` r
# Filter the La Quinta df for Alaska
lq_ak <- laquinta %>%
  filter(state == "AK")
nrow(lq_ak)
```

    ## [1] 2

There are 3 Denny’s locations in Alaska and 2La Quinta locations in
Alaska.

### Exercise 2

``` r
# Calculate the number of pairings using the data frames you have already made

n_pairings <- nrow(dn_ak) * nrow(lq_ak)
n_pairings
```

    ## [1] 6

There are 6 pairings to be made.

### Exercise 3

…

### Exercise 4

…

### Exercise 5

…

### Exercise 6

…

Add exercise headings as needed.
