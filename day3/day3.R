input <- "input.txt"
width = str_length(read_lines(input, n_max = 1))

symbol_locations <-
tibble(char = read_lines(input)) |>
  mutate(y = 1:n(), .before = char) |>
  separate_longer_position(cols = char, width = 1, keep_empty = TRUE) |>
  group_by(y) |> mutate(x = 1:width, .after = y) |> ungroup() |>
  select(x, y, char) |>
  filter(!str_detect(char, "\\d|\\.")) |>
  mutate(symbol_id = 1:n())

symbol_locations
nrow(symbol_locations)

number_locations <-
  tibble(char = read_lines(input)) |>
  mutate(y = 1:n(), .before = char) |>
  mutate(number = str_extract_all(char, "\\d{1,}")) |>
  mutate(loc = str_locate_all(char, "\\d{1,}")) |>
  unnest(c(number, loc)) |>
  mutate(x1 = loc[,1], x2 = loc[,2], number = as.numeric(number)) |>
  select(x1, x2, y, number)

number_locations
nrow(number_locations)

both <- cross_join(number_locations, symbol_locations, suffix = c("_n", ""))
both

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

both |>
rowwise() |>
mutate(partnumber = near_to(x1, x2, y_n, x, y, number)) |>
ungroup() |>
summarize(answer = sum(partnumber))

# part 2 -- gear ratios

astericks <-
  symbol_locations |>
  filter(char == "*")
astericks
both <- cross_join(number_locations, astericks, suffix = c("_n", ""))
both  |> rowwise() |>
  mutate(partnumber = near_to(x1, x2, y_n, x, y, number)) |>
  ungroup() |>
  filter(partnumber > 0) |>
  group_by(symbol_id) |>
  filter(n() == 2) |>
  arrange(symbol_id) |>
  group_by(symbol_id) |>
  summarize(gearratio = prod(partnumber), .groups = "drop") |>
  summarize(answer = sum(gearratio))

