# --- Part Two ---
# Due to what you can only assume is a mistranslation (you're not exactly
# fluent in Crab), you are quite surprised when the crab starts arranging many
# cups in a circle on your raft - one million (1000000) in total.
#
# Your labeling is still correct for the first few cups; after that, the
# remaining cups are just numbered in an increasing fashion starting from the
# number after the highest number in your list and proceeding one by one until
# one million is reached. (For example, if your labeling were 54321, the cups
# would be numbered 5, 4, 3, 2, 1, and then start counting up from 6 until one
# million is reached.) In this way, every number from one through one million
# is used exactly once.
#
# After discovering where you made the mistake in translating Crab Numbers, you
# realize the small crab isn't going to do merely 100 moves; the crab is going
# to do ten million (10000000) moves!
#
# The crab is going to hide your stars - one each - under the two cups that
# will end up immediately clockwise of cup 1. You can have them if you predict
# what the labels on those cups will be when the crab is finished.
#
# In the above example (389125467), this would be 934001 and then 159792;
# multiplying these together produces 149245887792.
#
# Determine which two cups will end up immediately clockwise of cup 1. What do
# you get if you multiply their labels together?

options(digits = 22)

# input <- c(9, 5, 2, 4, 3, 8, 7, 1, 6)
# next_cup[i] represents the number that follows after number i
next_cup <- c(6, 4, 8, 3, 2, 10, 1, 7, 5, 11:1000000, 9)
current <- 9

for (i in seq(10000000)) {
  # get the three cups that are moving
  cup1 <- next_cup[current]
  cup2 <- next_cup[cup1]
  cup3 <- next_cup[cup2]

  # retrieve the destination
  dest <- if (current == 1) 1000000 else current - 1
  while (dest %in% c(cup1, cup2, cup3)) {
    dest <- if (dest == 1) 1000000 else dest - 1
  }

  # it's kinda like updating a linked list:
  # point cup3 to whatever is after dest
  # point dest to cup1
  # point current to whatever was after cup3
  # then current becomes whatever was after cup3
  nxt <- next_cup[cup3]
  next_cup[cup3] <- next_cup[dest]
  next_cup[dest] <- cup1
  next_cup[current] <- nxt
  current <- nxt
}

cup1 <- next_cup[1]
cup2 <- next_cup[cup1]
cup1 * cup2
