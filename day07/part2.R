# --- Part Two ---
# It's getting pretty expensive to fly these days - not because of ticket
# prices, but because of the ridiculous number of bags you need to buy!
#
# Consider again your shiny gold bag and the rules from the above example:
#
# faded blue bags contain 0 other bags.
# dotted black bags contain 0 other bags.
# vibrant plum bags contain 11 other bags: 5 faded blue bags and 6 dotted black bags.
# dark olive bags contain 7 other bags: 3 faded blue bags and 4 dotted black bags.
#
# So, a single shiny gold bag must contain 1 dark olive bag (and the 7 bags
# within it) plus 2 vibrant plum bags (and the 11 bags within each of those): 1
# + 1*7 + 2 + 2*11 = 32 bags!
#
# Of course, the actual rules have a small chance of going several levels
# deeper than this example; be sure to count all of the bags, even if the
# nesting becomes topologically impractical!
#
# Here's another example:
#
# shiny gold bags contain 2 dark red bags.
# dark red bags contain 2 dark orange bags.
# dark orange bags contain 2 dark yellow bags.
# dark yellow bags contain 2 dark green bags.
# dark green bags contain 2 dark blue bags.
# dark blue bags contain 2 dark violet bags.
# dark violet bags contain no other bags.
#
# In this example, a single shiny gold bag must contain 126 other bags.
#
# How many individual bags are required inside your single shiny gold bag?

library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(tibble)
library(tidyr)

# Given a tibble and a depth, first find a list of all bags that were included
# in that depth. Then, join with those bags and their counts - the join is
# important because it will duplicate rows (the distinct removes those
# duplicates on the next accumulate call), which could happen if a bag was
# included in more than one parent bag. For each, update the depth and a new
# included count, then remove the join columns.
accumulator <- function(t, d) {
  t2 <- filter(t, depth == d)
  containers <- pull(t2, has)
  distinct(t, bag, has, .keep_all = T) %>%
    left_join(select(t2, has, included), c("bag" = "has")) %>%
    mutate(
           depth = ifelse(bag %in% containers, d + 1, depth),
           included = coalesce(n * included.y, 0)
    ) %>%
    select(-included.x, -included.y)
}

# read file into list of strings
# convert to tibble
# extract "bag" and "has" columns
# split "has" into multiple rows
# split "has" into "n" and "has"
# drop rows where "has" is NA ("clear blue bags contain no other bags")
# force "n" to int, add a depth field defaulted to 1 for the shiny gold bag,
# and a field indicating how many of the bags have been included, defaulting to
# n for the shiny gold bags.
# prepend the tibble with a list - 9 is arbitrary
# accumulate using the function above
# map each resulting tibble to calculate a total of included bags
# sum those totals
#
# NOTE: an assumption is made that there are no cycles
read_lines("input.txt") %>%
  enframe(name = NULL) %>%
  extract(value, c("bag", "has"), "(.+) bags contain (.+).") %>%
  separate_rows(has, sep = ", ") %>%
  extract(has, c("n", "has"), "(\\d+) (.+) bags?") %>%
  drop_na("has") %>%
  mutate(
         n = as.integer(n),
         depth = ifelse(bag == "shiny gold", 1, NA),
         included = ifelse(bag == "shiny gold", n, NA)
  ) %>%
  list(1, 2, 3, 4, 5, 6, 7, 8, 9) %>%
  accumulate(accumulator) %>%
  map(. %>% drop_na(depth) %>% tally(included)) %>%
  unlist() %>%
  sum()
