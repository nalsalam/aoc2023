Day 3
================

``` r
library(tidyverse)
input <- "input.txt"
```

These grid layouts appear every year in AOC. And I find them anoying. So
like day2, I decided to represent the information in the gird in a
dataframe. One for the symbol locations and another for the number
locations.

``` r
width = str_length(read_lines(input, n_max = 1))

symbol_locations <-
  tibble(char = read_lines(input)) |>
  mutate(y = 1:n(), .before = char) |>
  separate_longer_position(cols = char, width = 1, keep_empty = TRUE) |>
  group_by(y) |> mutate(x = 1:width, .after = y) |> ungroup() |>
  select(x, y, char) |>
  filter(!str_detect(char, "\\d|\\.")) |>
  mutate(symbol_id = 1:n())
```

``` r
number_locations <-
  tibble(char = read_lines(input)) |>
  mutate(y = 1:n(), .before = char) |>
  mutate(number = str_extract_all(char, "\\d{1,}")) |>
  mutate(loc = str_locate_all(char, "\\d{1,}")) |>
  unnest(c(number, loc)) |>
  mutate(x1 = loc[,1], x2 = loc[,2], number = as.numeric(number)) |>
  select(x1, x2, y, number)
```

This is what the two dfs looks like:

``` r
symbol_locations
```

    # A tibble: 728 × 4
           x     y char  symbol_id
       <int> <int> <chr>     <int>
     1     8     2 *             1
     2    45     2 *             2
     3    72     2 /             3
     4    90     2 *             4
     5    95     2 *             5
     6   105     2 *             6
     7   119     2 *             7
     8    24     3 *             8
     9    34     3 *             9
    10    43     3 *            10
    # ℹ 718 more rows

``` r
number_locations
```

    # A tibble: 1,187 × 4
          x1    x2     y number
       <int> <int> <int>  <dbl>
     1     6     8     1    613
     2    44    46     1    439
     3    59    61     1    498
     4    87    89     1    438
     5    96    98     1    617
     6   103   105     1    343
     7   119   121     1    942
     8    21    23     2    790
     9    35    37     2    269
    10    40    42     2    735
    # ℹ 1,177 more rows

Here is a utility function that return the number if it is diagonally
adjacent to a symbol:

``` r
near_to <- function(x1, x2, y_n, x, y, number) {
  if(
  ((x - x1) %in% c(-1:1) |
  ((x - x2) %in% c(-1:1))) &
  ((y - y_n) %in% c(-1:1))
  ) {
    return(number)
  } else {
    return(0)
  }
}
```

Now I `cross_join` the two data frame – not very efficient – and then
one row at a time return the number if the number is diagonally adjacent
to the symbol.

``` r
cross_join(number_locations, symbol_locations, suffix = c("_n", "")) |> 
rowwise() |>
mutate(part_number = near_to(x1, x2, y_n, x, y, number)) |>
ungroup() |>
summarize(answer = sum(part_number))
```

    # A tibble: 1 × 1
      answer
       <dbl>
    1 527144

# Part 2

A risk of spending time creating a data structure for part 1 is that it
may not payoff for Part 2 of these puzzles. But like yesterday, it has
paid off.

Subset symbol_locations to asterick_locations:

``` r
asterick_locations <-
  symbol_locations |>
  filter(char == "*")
```

cross_join, subset to asterisks diagonally adjacent to two numbers,
calculate the gear_ratio and calculate the answer:

``` r
 cross_join(number_locations, asterick_locations, suffix = c("_n", "")) |> rowwise() |>
  mutate(part_number = near_to(x1, x2, y_n, x, y, number)) |>
  ungroup() |>
  filter(part_number > 0) |> # no adjacent number
  group_by(symbol_id) |>
  filter(n() == 2) |> # asterick adjacent to two numbers
  summarize(gear_ratio = prod(part_number), .groups = "drop") |>
  summarize(answer = sum(gear_ratio))
```

    # A tibble: 1 × 1
        answer
         <dbl>
    1 81463996
