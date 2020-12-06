# --- Part Two ---
# Ding! The "fasten seat belt" signs have turned on. Time to find your seat.
#
# It's a completely full flight, so your seat should be the only missing
# boarding pass in your list. However, there's a catch: some of the seats at
# the very front and back of the plane don't exist on this aircraft, so they'll
# be missing from your list as well.
#
# Your seat wasn't at the very front or back, though; the seats with IDs +1 and
# -1 from yours will be in your list.
#
# What is the ID of your seat?

library(dplyr)
library(readr)
library(stringr)

# This solution relies on the fact that the code can easily be translated into
# a binary number. Every time you select the lower half, that's a 0; the upper
# half is a 1. The formula to multiply the row by 8 and add the column is
# unnecessary because that's just bit-shifting the row by three to make room
# for the three bits representing the column.

# read data
# convert the code into a binary representation: FBFBBFFRLR -> 0101100101
# convert the binary string into a number, base 2
# find the max
next_row <- read_table("input.txt", col_names = c("code")) %>%
  mutate(binary_id = str_replace_all(code, c("F" = "0", "B" = "1", "L" = "0", "R" = "1"))) %>%
  mutate(id = strtoi(binary_id, 2)) %>%
  arrange(id) %>%
  slice_max(id - lag(id))

next_row$id - 1
