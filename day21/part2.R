# --- Part Two ---
# Now that you've isolated the inert ingredients, you should have enough
# information to figure out which ingredient contains which allergen.
#
# In the above example:
#
# mxmxvkd contains dairy.
# sqjhc contains fish.
# fvjkl contains soy.
#
# Arrange the ingredients alphabetically by their allergen and separate them by
# commas to produce your canonical dangerous ingredient list. (There should not
# be any spaces in your canonical dangerous ingredient list.) In the above
# example, this would be mxmxvkd,sqjhc,fvjkl.
#
# Time to stock your raft with supplies. What is your canonical dangerous
# ingredient list?

library(dplyr)
library(readr)
library(purrr)
library(tibble)
library(tidyr)

# read input
# extract ingredients and allergens
# separate allergens into their own rows
# count the number of recipes that include each allergen
# separate ingredients into their own rows (we now have a row for each allergen/ingredient combo)
# count how many recipes contain each allergen/ingredient combo
# if every recipe that has an allergen also has the same ingredient, we can
# conclude that ingredient must be connected to that allergen
# find distinct allergen/ingredient combos
t <- read_lines("input.txt") %>%
  enframe(name = "recipe") %>%
  extract(value, c("ingredient", "allergen"), "(.+) \\(contains (.+)\\)") %>%
  separate_rows(allergen, sep = ", ") %>%
  add_count(allergen, name = "nrecipes") %>%
  separate_rows(ingredient, sep = " ") %>%
  add_count(allergen, ingredient, name = "ningredients") %>%
  filter(ningredients == nrecipes) %>%
  distinct(allergen, ingredient)

# at this point, each ingredient may appear with multiple allargens, so we need
# to find the 1-to-1 matches
# once we have that, we just order by allergen and join by commas
seq(nrow(t)) %>%
  reduce(.f = function(t, unused) {
    cnt <- t %>% add_count(allergen)
    if (all(cnt$n == 1)) {
      return(done(t))
    }

    cnt1 <- cnt %>% filter(n == 1)
    rbind(
      cnt1 %>% select(-n),
      t %>% anti_join(cnt1 %>% select(ingredient))
    )
  }, .init = t) %>%
  arrange(allergen) %>%
  { paste0(.$ingredient, collapse = ",") }
