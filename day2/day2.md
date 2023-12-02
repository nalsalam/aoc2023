AOC 2023 - Day 2
================

``` r
library(tidyverse)
```

Today’s puzzle is all about reorganizing the puzzle input in a way that
makes it easy to calculate the maximum number of red’s, green’s, and
blue’s in each game.

This gave me a chance to use some tidyverse functions like
`separate_delimiter_wider` and `separate_delimiter_longer` that I’ve
learned about recently.

``` r
draws <-
tibble(game = read_lines("input1.txt")) |>
  
  separate_wider_delim(cols = game, delim = ":", names = c("game_id", "draw")) |>
  
  mutate(game_id = str_remove(game_id, "Game ") |> as.numeric()) |>
  
  separate_longer_delim(cols = draw, delim = ";") |>
  
  group_by(game_id) |> mutate(draw_id = 1:n(), .after = game_id) |> ungroup() |>
  
  separate_longer_delim(cols = draw, delim = ", ") |>
  
  mutate(draw = str_trim(draw, side = "both")) |>
  
  separate_wider_delim(cols = draw, delim = " ", names = c("n", "color")) |>
  
  mutate(n = as.numeric(n)) |>
  
  pivot_wider(id_cols = c(game_id, draw_id), names_from = color, values_from = n, values_fill = 0)
```

This is what the reorganized input looks like:

``` r
head(draws, n = 20)
```

    # A tibble: 20 × 5
       game_id draw_id   red  blue green
         <dbl>   <int> <dbl> <dbl> <dbl>
     1       1       1     1     5    10
     2       1       2    12     6     5
     3       1       3     4    10     4
     4       2       1     0     1     2
     5       2       2     1     0     2
     6       2       3     3     1     0
     7       2       4     8     2     1
     8       2       5    10     0     1
     9       2       6    10     0     0
    10       3       1    14     5     9
    11       3       2     5     7     2
    12       3       3     0     1    14
    13       3       4     2     0     6
    14       4       1     9     3     2
    15       4       2     1     0     1
    16       4       3     4     4     0
    17       4       4    19     1     0
    18       4       5     7     0     0
    19       5       1     4    10     1
    20       5       2     4     5    15

The maximum draws of each color is needed.

``` r
max_each_color <-
  draws |> 
  group_by(game_id) |> 
  summarize(across(.cols = c("red", "green", "blue"), max), .groups = "drop")
```

Now solving the puzzle is easy.

``` r
max_each_color |> 
  filter(red <= 12, green <= 13, blue <= 14) |> 
  summarize(answer = sum(game_id))
```

    # A tibble: 1 × 1
      answer
       <dbl>
    1   2720

Part 2 is easy also.

``` r
max_each_color |>
  mutate(Power = red * green * blue) |> 
  summarize(answer = sum(Power))
```

    # A tibble: 1 × 1
      answer
       <dbl>
    1  71535
