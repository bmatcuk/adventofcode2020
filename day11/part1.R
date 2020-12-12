# --- Day 11: Seating System ---
# Your plane lands with plenty of time to spare. The final leg of your journey
# is a ferry that goes directly to the tropical island where you can finally
# start your vacation. As you reach the waiting area to board the ferry, you
# realize you're so early, nobody else has even arrived yet!
#
# By modeling the process people use to choose (or abandon) their seat in the
# waiting area, you're pretty sure you can predict the best place to sit. You
# make a quick map of the seat layout (your puzzle input).
#
# The seat layout fits neatly on a grid. Each position is either floor (.), an
# empty seat (L), or an occupied seat (#). For example, the initial seat layout
# might look like this:
#
# L.LL.LL.LL
# LLLLLLL.LL
# L.L.L..L..
# LLLL.LL.LL
# L.LL.LL.LL
# L.LLLLL.LL
# ..L.L.....
# LLLLLLLLLL
# L.LLLLLL.L
# L.LLLLL.LL
#
# Now, you just need to model the people who will be arriving shortly.
# Fortunately, people are entirely predictable and always follow a simple set
# of rules. All decisions are based on the number of occupied seats adjacent to
# a given seat (one of the eight positions immediately up, down, left, right,
# or diagonal from the seat). The following rules are applied to every seat
# simultaneously:
#
# - If a seat is empty (L) and there are no occupied seats adjacent to it, the
# seat becomes occupied.
# - If a seat is occupied (#) and four or more seats adjacent to it are also
# occupied, the seat becomes empty.
# - Otherwise, the seat's state does not change.
#
# Floor (.) never changes; seats don't move, and nobody sits on the floor.
#
# After one round of these rules, every seat in the example layout becomes
# occupied:
#
# #.##.##.##
# #######.##
# #.#.#..#..
# ####.##.##
# #.##.##.##
# #.#####.##
# ..#.#.....
# ##########
# #.######.#
# #.#####.##
#
# After a second round, the seats with four or more occupied adjacent seats
# become empty again:
#
# #.LL.L#.##
# #LLLLLL.L#
# L.L.L..L..
# #LLL.LL.L#
# #.LL.LL.LL
# #.LLLL#.##
# ..L.L.....
# #LLLLLLLL#
# #.LLLLLL.L
# #.#LLLL.##
#
# This process continues for three more rounds:
#
# #.##.L#.##
# #L###LL.L#
# L.#.#..#..
# #L##.##.L#
# #.##.LL.LL
# #.###L#.##
# ..#.#.....
# #L######L#
# #.LL###L.L
# #.#L###.##
#
# #.#L.L#.##
# #LLL#LL.L#
# L.L.L..#..
# #LLL.##.L#
# #.LL.LL.LL
# #.LL#L#.##
# ..L.L.....
# #L#LLLL#L#
# #.LLLLLL.L
# #.#L#L#.##
#
# #.#L.L#.##
# #LLL#LL.L#
# L.#.L..#..
# #L##.##.L#
# #.#L.LL.LL
# #.#L#L#.##
# ..L.L.....
# #L#L##L#L#
# #.LLLLLL.L
# #.#L#L#.##
#
# At this point, something interesting happens: the chaos stabilizes and
# further applications of these rules cause no seats to change state! Once
# people stop moving around, you count 37 occupied seats.
#
# Simulate your seating area by applying the seating rules repeatedly until no
# seats change state. How many seats end up occupied?

library(purrr)
library(readr)

# read input and get width/height
board <- read_lines("input.txt")
height = length(board)
width = nchar(board[1])

# split each line and build a matrix
board <- map(board, strsplit, "") %>% unlist() %>% matrix(height, byrow = T)

# for each generation...
generation <- 1
repeat {
  print(generation)

  # Create a logical matrix, true for every position occupied. Then shift this
  # matrix around: left, right, up, down, diagonally - generating 8 new
  # matrices where each position x,y represents one of the neighbors of x,y. By
  # summing these matrices, we get a count of the number of occupied neighbors.
  taken <- board == "#"
  nxt <- rbind(taken[-1, , drop = F], F)
  prv <- rbind(F, taken[-height, , drop = F])
  pl <- cbind(F, prv[, -width, drop = F])
  pr <- cbind(prv[, -1, drop = F], F)
  l <- cbind(F, taken[, -width, drop = F])
  r <- cbind(taken[, -1, drop = F], F)
  nl <- cbind(F, nxt[, -width, drop = F])
  nr <- cbind(nxt[, -1, drop = F], F)
  cnt <- pl + prv + pr + l + r + nl  + nxt + nr

  # Generate the next generation based our rules.
  nboard <- ifelse(
    board == "L" & cnt == 0,
    "#",
    ifelse(
      taken & cnt >= 4,
      "L",
      board
    )
  )

  if (identical(board, nboard)) {
    # we're done!
    break
  }

  board <- nboard
  generation <- generation + 1
}

sum(board == "#")
