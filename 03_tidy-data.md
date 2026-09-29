tidy data
================

This file is for data manipulation,每次commit和push之前，都要knit檢查！

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.2.1     ✔ readr     2.2.0
    ## ✔ forcats   1.0.1     ✔ stringr   1.6.0
    ## ✔ ggplot2   4.0.3     ✔ tibble    3.3.1
    ## ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
    ## ✔ purrr     1.2.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

\##Lets tidy some data

``` r
pulse_df=
  haven::read_sas("data/public_pulse_data.sas7bdat") |> 
janitor::clean_names()

pulse_df
```

    ## # A tibble: 1,087 × 7
    ##       id   age sex    bdi_score_bl bdi_score_01m bdi_score_06m bdi_score_12m
    ##    <dbl> <dbl> <chr>         <dbl>         <dbl>         <dbl>         <dbl>
    ##  1 10003  48.0 male              7             1             2             0
    ##  2 10015  72.5 male              6            NA            NA            NA
    ##  3 10022  58.5 male             14             3             8            NA
    ##  4 10026  72.7 male             20             6            18            16
    ##  5 10035  60.4 male              4             0             1             2
    ##  6 10050  84.7 male              2            10            12             8
    ##  7 10078  31.3 male              4             0            NA            NA
    ##  8 10088  56.9 male              5            NA             0             2
    ##  9 10091  76.0 male              0             3             4             0
    ## 10 10092  74.2 female           10             2            11             6
    ## # ℹ 1,077 more rows

我們發現：The column names有點混在一起了，需要re-organize

okay,lets tidy

``` r
pulse_tidy_df=
  pulse_df |> 
  pivot_longer(
    bdi_score_bl:bdi_score_12m,
    names_to="visit",
    names_prefix="bdi_score_",
    values_to="bdi_score"
  ) |> 
  mutate(
    visit=replace(visit,visit=="bl","00m")
  )
```

just use once

``` r
pulse_tidy_df =
  pulse_df |>
  pivot_longer(
    bdi_score_bl:bdi_score_12m,
    names_to = "visit",
    names_prefix = "bdi_score_",
    values_to = "bdi_score"
  ) |>
  mutate(
    visit = replace(
      visit,
      visit == "bl",
      "00m"
    )
  )

pulse_tidy_df   
```

    ## # A tibble: 4,348 × 5
    ##       id   age sex   visit bdi_score
    ##    <dbl> <dbl> <chr> <chr>     <dbl>
    ##  1 10003  48.0 male  00m           7
    ##  2 10003  48.0 male  01m           1
    ##  3 10003  48.0 male  06m           2
    ##  4 10003  48.0 male  12m           0
    ##  5 10015  72.5 male  00m           6
    ##  6 10015  72.5 male  01m          NA
    ##  7 10015  72.5 male  06m          NA
    ##  8 10015  72.5 male  12m          NA
    ##  9 10022  58.5 male  00m          14
    ## 10 10022  58.5 male  01m           3
    ## # ℹ 4,338 more rows

Lets practice Import the litters data;keep columns litter number and GD
weights and tidy.

``` r
litters_tidy_df =
  read_csv(
    "data/FAS_litters.csv",
    na = c(".", "", "NA")
  ) |>
  janitor::clean_names() |>
  select(
    litter_number,
    gd0_weight,
    gd18_weight
  ) |>
  pivot_longer(
    gd0_weight:gd18_weight,
    names_to = "gd",
    values_to = "weight"
  ) |>
  mutate(
    gd = case_match(
      gd,
      "gd0_weight" ~ 0,
      "gd18_weight" ~ 18
    )
  )
```

    ## Rows: 49 Columns: 8
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): Group, Litter Number
    ## dbl (6): GD0 weight, GD18 weight, GD of Birth, Pups born alive, Pups dead @ ...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `gd = case_match(gd, "gd0_weight" ~ 0, "gd18_weight" ~ 18)`.
    ## Caused by warning:
    ## ! `case_match()` was deprecated in dplyr 1.2.0.
    ## ℹ Please use `recode_values()` instead.

``` r
litters_tidy_df
```

    ## # A tibble: 98 × 3
    ##    litter_number    gd weight
    ##    <chr>         <dbl>  <dbl>
    ##  1 #85               0   19.7
    ##  2 #85              18   34.7
    ##  3 #1/2/95/2         0   27  
    ##  4 #1/2/95/2        18   42  
    ##  5 #5/5/3/83/3-3     0   26  
    ##  6 #5/5/3/83/3-3    18   41.4
    ##  7 #5/4/2/95/2       0   28.5
    ##  8 #5/4/2/95/2      18   44.1
    ##  9 #4/2/95/3-3       0   NA  
    ## 10 #4/2/95/3-3      18   NA  
    ## # ℹ 88 more rows

\##deliberately untidy data

``` r
analysis_df=
  tibble(
    groups=c("treatment","treatment","placebo","placebo"),
    time=c("pre","post","pre","post"),
    mean_outcome=c(4,8,3.5,4.6)
  )
analysis_df
```

    ## # A tibble: 4 × 3
    ##   groups    time  mean_outcome
    ##   <chr>     <chr>        <dbl>
    ## 1 treatment pre            4  
    ## 2 treatment post           8  
    ## 3 placebo   pre            3.5
    ## 4 placebo   post           4.6

lets untidy this datset for human

``` r
analysis_df |> 
  pivot_wider(
    names_from=time,
    values_from= mean_outcome
  ) |> 
  knitr::kable()
```

| groups    | pre | post |
|:----------|----:|-----:|
| treatment | 4.0 |  8.0 |
| placebo   | 3.5 |  4.6 |

\##bind some rows first, import each LOTR movie table

``` r
fellowship_df=
  readxl::read_excel("data/LotR_Words.xlsx",range="B3:D6") |> 
  mutate(movie="fellowship")
fellowship_df
```

    ## # A tibble: 3 × 4
    ##   Race   Female  Male movie     
    ##   <chr>   <dbl> <dbl> <chr>     
    ## 1 Elf      1229   971 fellowship
    ## 2 Hobbit     14  3644 fellowship
    ## 3 Man         0  1995 fellowship

``` r
two_tower_df=
  readxl::read_excel("data/LotR_Words.xlsx",range="F3:H6") |> 
  mutate(movie="two towers")
two_tower_df
```

    ## # A tibble: 3 × 4
    ##   Race   Female  Male movie     
    ##   <chr>   <dbl> <dbl> <chr>     
    ## 1 Elf       331   513 two towers
    ## 2 Hobbit      0  2463 two towers
    ## 3 Man       401  3589 two towers

``` r
return_df=
  readxl::read_excel("data/LotR_Words.xlsx",range="J3:L6") |> 
  mutate(movie="return of the king")
return_df
```

    ## # A tibble: 3 × 4
    ##   Race   Female  Male movie             
    ##   <chr>   <dbl> <dbl> <chr>             
    ## 1 Elf       183   510 return of the king
    ## 2 Hobbit      2  2673 return of the king
    ## 3 Man       268  2459 return of the king

Next put all of these together and tidy

``` r
lotr_df=
  bind_rows(fellowship_df,two_tower_df,return_df) |> 
  janitor::clean_names() |> 
  relocate(movie) |> 
  pivot_longer(
    female:male,
    names_to="gender",
    values_to="words"
  )
lotr_df
```

    ## # A tibble: 18 × 4
    ##    movie              race   gender words
    ##    <chr>              <chr>  <chr>  <dbl>
    ##  1 fellowship         Elf    female  1229
    ##  2 fellowship         Elf    male     971
    ##  3 fellowship         Hobbit female    14
    ##  4 fellowship         Hobbit male    3644
    ##  5 fellowship         Man    female     0
    ##  6 fellowship         Man    male    1995
    ##  7 two towers         Elf    female   331
    ##  8 two towers         Elf    male     513
    ##  9 two towers         Hobbit female     0
    ## 10 two towers         Hobbit male    2463
    ## 11 two towers         Man    female   401
    ## 12 two towers         Man    male    3589
    ## 13 return of the king Elf    female   183
    ## 14 return of the king Elf    male     510
    ## 15 return of the king Hobbit female     2
    ## 16 return of the king Hobbit male    2673
    ## 17 return of the king Man    female   268
    ## 18 return of the king Man    male    2459

\##join FAS datasets

import both

``` r
pups_df=
  read_csv("data/FAS_pups.csv",
    skip=3,
    na=c(".",",","NA")) |> 
      janitor::clean_names() |> 
mutate(
  sex=case_match(
    sex,
    1~"male",
    2~"female"
  )
)
```

    ## Rows: 313 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): Litter Number
    ## dbl (5): Sex, PD ears, PD eyes, PD pivot, PD walk
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
pups_df
```

    ## # A tibble: 313 × 6
    ##    litter_number sex   pd_ears pd_eyes pd_pivot pd_walk
    ##    <chr>         <chr>   <dbl>   <dbl>    <dbl>   <dbl>
    ##  1 #85           male        4      13        7      11
    ##  2 #85           male        4      13        7      12
    ##  3 #1/2/95/2     male        5      13        7       9
    ##  4 #1/2/95/2     male        5      13        8      10
    ##  5 #5/5/3/83/3-3 male        5      13        8      10
    ##  6 #5/5/3/83/3-3 male        5      14        6       9
    ##  7 #5/4/2/95/2   male       NA      14        5       9
    ##  8 #4/2/95/3-3   male        4      13        6       8
    ##  9 #4/2/95/3-3   male        4      13        7       9
    ## 10 #2/2/95/3-2   male        4      NA        8      10
    ## # ℹ 303 more rows

``` r
litters_df =
  read_csv(
    "data/FAS_litters.csv",
    na = c(".", "", "NA")
  ) |>
  janitor::clean_names() |>
  relocate(litter_number) |>
  separate(
    group,
    into = c("dose", "day_of_tx"),
    sep = 3
  ) |>
  mutate(
    dose = str_to_lower(dose),
    day_of_tx = as.numeric(day_of_tx),
    gd_weight_gain = gd18_weight - gd0_weight
  )
```

    ## Rows: 49 Columns: 8
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): Group, Litter Number
    ## dbl (6): GD0 weight, GD18 weight, GD of Birth, Pups born alive, Pups dead @ ...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
litters_df
```

    ## # A tibble: 49 × 10
    ##    litter_number   dose  day_of_tx gd0_weight gd18_weight gd_of_birth
    ##    <chr>           <chr>     <dbl>      <dbl>       <dbl>       <dbl>
    ##  1 #85             con           7       19.7        34.7          20
    ##  2 #1/2/95/2       con           7       27          42            19
    ##  3 #5/5/3/83/3-3   con           7       26          41.4          19
    ##  4 #5/4/2/95/2     con           7       28.5        44.1          19
    ##  5 #4/2/95/3-3     con           7       NA          NA            20
    ##  6 #2/2/95/3-2     con           7       NA          NA            20
    ##  7 #1/5/3/83/3-3/2 con           7       NA          NA            20
    ##  8 #3/83/3-3       con           8       NA          NA            20
    ##  9 #2/95/3         con           8       NA          NA            20
    ## 10 #3/5/2/2/95     con           8       28.5        NA            20
    ## # ℹ 39 more rows
    ## # ℹ 4 more variables: pups_born_alive <dbl>, pups_dead_birth <dbl>,
    ## #   pups_survive <dbl>, gd_weight_gain <dbl>

``` r
fas_df =
  left_join(
    pups_df,
    litters_df,
    by = "litter_number"
  )

fas_df
```

    ## # A tibble: 313 × 15
    ##    litter_number sex   pd_ears pd_eyes pd_pivot pd_walk dose  day_of_tx
    ##    <chr>         <chr>   <dbl>   <dbl>    <dbl>   <dbl> <chr>     <dbl>
    ##  1 #85           male        4      13        7      11 con           7
    ##  2 #85           male        4      13        7      12 con           7
    ##  3 #1/2/95/2     male        5      13        7       9 con           7
    ##  4 #1/2/95/2     male        5      13        8      10 con           7
    ##  5 #5/5/3/83/3-3 male        5      13        8      10 con           7
    ##  6 #5/5/3/83/3-3 male        5      14        6       9 con           7
    ##  7 #5/4/2/95/2   male       NA      14        5       9 con           7
    ##  8 #4/2/95/3-3   male        4      13        6       8 con           7
    ##  9 #4/2/95/3-3   male        4      13        7       9 con           7
    ## 10 #2/2/95/3-2   male        4      NA        8      10 con           7
    ## # ℹ 303 more rows
    ## # ℹ 7 more variables: gd0_weight <dbl>, gd18_weight <dbl>, gd_of_birth <dbl>,
    ## #   pups_born_alive <dbl>, pups_dead_birth <dbl>, pups_survive <dbl>,
    ## #   gd_weight_gain <dbl>

View(fas_df) 要放在console
