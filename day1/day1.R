library(tidyverse)

### part 1

input1 <- readLines("day1/input1.txt")

digits <- str_remove_all(input1, "[a-z]")

a <- str_extract(digits, "(^.)")
b <- str_extract(digits, "(.$)")
c <- str_c(a, b) |> as.numeric()
sum(c)

### part 2

input2 <- readLines("day1/input2.txt")

digits <-
input2 |>
str_replace("oneight", "oneeight") |>
str_replace("threeight", "threeeight") |>
str_replace("fiveight", "fiveeight") |>
str_replace("nineight","nineeight") |>
str_replace("twone", "twoone") |>
str_replace("sevenine", "sevennine") |>
str_replace("eightwo", "eighttwo") |>
str_replace_all(
  c("one" = "1",
    "two" = "2",
    "three" = "3",
    "four" = "4",
    "five" = "5",
    "six" = "6",
    "seven"= "7",
    "eight" = "8",
    "nine" = "9")
) |>
str_remove_all("[a-z]")

a <- str_extract(digits, "(^.)")
b <- str_extract(digits, "(.$)")
c <- str_c(a, b) |> as.numeric()
sum(c)



