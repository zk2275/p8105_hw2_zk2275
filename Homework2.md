Homework2
================
Zhuodiao Kuang
2023-10-01

# Load libraries

``` r
library(tidyverse)
library(dplyr)
library(readxl)
```

# Problem 1

We clean the 538 `pols` data, which provides information on the number
of national politicians who are democratic or republican at any given
time. There are some values for which `prez_gop` is `2` – these are
months in which Ford became President following Nixon’s resignation. In
the new `president` variable created as part of our data cleaning, we
code these as ``` rep`` (same as values when ```prez_gop`is`1\`).

prez_gop: indicator of whether the president was republican on the
associated date (1 = yes, 0 = no) gov_gop: the number of republican
governors on the associated date sen_gop: the number of republican
senators on the associated date rep_gop: the number of republican
representatives on the associated date prez_dem: indicator of whether
the president was democratic on the associated date (1 = yes, 0 = no)
gov_dem: the number of democratic governors on the associated date
sen_dem: the number of democratic senators on the associated date
rep_dem: the number of democratic representatives on the associated date

# First step

``` r
month_df = 
  tibble(
    month_num = 1:12,
    month_abb = month.abb,
    month = month.name
  )

pm_df = 
  read_csv("./data/pols-month.csv") |>
  separate(mon, into = c("year", "month_num", "day"), convert = TRUE) |>
  mutate(
    president = recode(prez_gop, "0" = "dem", "1" = "rep", "2" = "rep")) |>
  left_join(x = _, y = month_df) |> 
  select(year, month, everything(), -day, -starts_with("prez"))  
```

# Second step

``` r
snp_df = 
  read_csv("./data/snp.csv") |>
  separate(date, into = c("month", "day", "year"), convert = TRUE) |>
  arrange(year, month) |>
  mutate(month = month.name[month],year = ifelse(year>=50, 1900+year, 2000+year)) |> 
  select(year, month, close) 
```

# Third step

Finally, we tidy the `unemployment` data so that it can be merged with
the `pols` and `snp` datasets.

``` r
unemployment = 
  read_csv("./data/unemployment.csv") |>
  rename(year = Year) |>
  pivot_longer(
    Jan:Dec, 
    names_to = "month_abb",
    values_to = "unemployment"
  ) |> 
  left_join(x = _, y = month_df) |> 
  select(year, month, unemployment)
```

# Last step

Now we merge the three datasets!

``` r
data= 
  left_join(pm_df, snp_df) |>
  left_join(x = _, y = unemployment)

str(data)
```

    tibble [822 × 13] (S3: tbl_df/tbl/data.frame)
     $ year        : num [1:822] 1947 1947 1947 1947 1947 ...
     $ month       : chr [1:822] "January" "February" "March" "April" ...
     $ month_num   : int [1:822] 1 2 3 4 5 6 7 8 9 10 ...
     $ gov_gop     : num [1:822] 23 23 23 23 23 23 23 23 23 23 ...
     $ sen_gop     : num [1:822] 51 51 51 51 51 51 51 51 51 51 ...
     $ rep_gop     : num [1:822] 253 253 253 253 253 253 253 253 253 253 ...
     $ gov_dem     : num [1:822] 23 23 23 23 23 23 23 23 23 23 ...
     $ sen_dem     : num [1:822] 45 45 45 45 45 45 45 45 45 45 ...
     $ rep_dem     : num [1:822] 198 198 198 198 198 198 198 198 198 198 ...
     $ president   : chr [1:822] "dem" "dem" "dem" "dem" ...
     $ month_abb   : chr [1:822] "Jan" "Feb" "Mar" "Apr" ...
     $ close       : num [1:822] NA NA NA NA NA NA NA NA NA NA ...
     $ unemployment: num [1:822] NA NA NA NA NA NA NA NA NA NA ...

So the pols_month dataset contains the information of presidents,
governors, senators, and representatives from Republican and Democratic
during different periods.

The snp dataset contains the Standard & Poor’s stock market index during
different periods.

The unemployment dataset contains the unemployment rate in the US during
different periods.

The period of the dataset is from 1947 to 2015. The size of the merged
dataset is 822 x 12.

# Problem 2

### Read and clean the Mr. Trash Wheel sheet
