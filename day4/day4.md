Day 4 - Here Comes Recursion (I think)
================

``` r
library(tidyverse)
```

After processing the data, part 1 is pretty straightforward. Just
remember the delimiter between numbers is one or **more** spaces.

``` r
input = "input.txt"
# input = "input-tst.txt"

# Process 
tibble(card = read_lines(input)) |>
separate_wider_delim(cols = card, delim = ":", names = c("card_id", "numbers")) |>
separate_wider_delim(cols = numbers, delim = "|", names = c("winning", "mine")) |>
mutate(across(c(winning, mine), \(x) str_trim(x) |> str_split("[ ]+"))) |>
mutate(my_winning = map2(winning, mine, intersect)) |>
  
# Calculate the anser
rowwise() |>
mutate(card_pts = if_else(length(my_winning) > 0, 2 ^ (length(my_winning) - 1L), 0)) |>
ungroup() |>
summarize(answer = sum(card_pts))
```

    # A tibble: 1 × 1
      answer
       <dbl>
    1  21568

# Part 2

Not solved yet. It feels like recursion. I will need to ponder this some
more.
