# --- Part Two ---
# Now that you've identified which tickets contain invalid values, discard
# those tickets entirely. Use the remaining valid tickets to determine which
# field is which.
#
# Using the valid ranges for each field, determine what order the fields appear
# on the tickets. The order is consistent between all tickets: if seat is the
# third field, it is the third field on every ticket, including your ticket.
#
# For example, suppose you have the following notes:
#
# class: 0-1 or 4-19
# row: 0-5 or 8-19
# seat: 0-13 or 16-19
#
# your ticket:
# 11,12,13
#
# nearby tickets:
# 3,9,18
# 15,1,5
# 5,14,9
#
# Based on the nearby tickets in the above example, the first position must be
# row, the second position must be class, and the third position must be seat;
# you can conclude that in your ticket, class is 12, row is 11, and seat is 13.
#
# Once you work out which field is which, look for the six fields on your
# ticket that start with the word departure. What do you get if you multiply
# those six values together?

library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(tibble)
library(tidyr)

# read input - find blank lines
input <- read_lines("input.txt")
blanks <- which(input == "")

# parse rules
rules <- enframe(input[1:(blanks[1] - 1)], name = NULL) %>%
  extract(
    value,
    c("name", "min1", "max1", "min2", "max2"),
    "^([^:]+): (\\d+)-(\\d+) or (\\d+)-(\\d+)$",
    convert = T
  )

# build a list of valid numbers
valid <- pivot_longer(rules, !name, names_to = c(".value"), names_pattern = "(min|max)") %>%
  summarize(valid =
    reduce2(
      min,
      max,
      .f = function(l, x, y) { l[x:y] <- T; l }, .init = c()
    )
  ) %>%
  { .$valid }

# parse nearby tickets
nearby <- read_csv(input[(blanks[2] + 2):length(input)], col_names = F) %>%
  rowwise() %>%
  filter(every(c_across(), .p = function(v) !is.na(valid[v]))) %>%
  ungroup()

# figure out which rules correspond to each value
indexed_rules <- rowwise(rules) %>%
  mutate(
    matches = list(map_lgl(nearby, .f = ~ all(between(.x, min1, max1) | between(.x, min2, max2)))),
    num_matches = sum(matches)
  ) %>%
  arrange(num_matches) %>%
  ungroup() %>%
  mutate(
    index = reduce(matches, .f = function(acc, m) {
      n <- m
      n[acc] <- F
      append(acc, which(n)[[1]])
    }, .init = c())
  )

# get the indexes for the "departure" values
dept_idxs <- filter(indexed_rules, str_starts(name, "departure")) %>% { .$index }

# parse my ticket
# retrieve the departure values and multiply
myticket <- el(strsplit(input[blanks[2] - 1], ","))
prod(as.numeric(myticket[dept_idxs]))
