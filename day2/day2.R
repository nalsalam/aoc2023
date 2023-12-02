
library(tidyverse)

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

max_each_color <-
  draws |>
  group_by(game_id) |>
  summarize(across(.cols = c("red", "green", "blue"), max), .groups = "drop")

# part 1

max_each_color |>
  filter(red <= 12, green <= 13, blue <= 14) |>
  summarize(answer = sum(game_id))

# part 2

max_each_color |>
  mutate(power = red * green * blue) |>
  summarize(answer = sum(power))


