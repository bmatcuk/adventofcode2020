# --- Part Two ---
# As soon as people start to arrive, you realize your mistake. People don't
# just care about adjacent seats - they care about the first seat they can see
# in each of those eight directions!
#
# Now, instead of considering just the eight immediately adjacent seats,
# consider the first seat in each of those eight directions. For example, the
# empty seat below would see eight occupied seats:
#
# .......#.
# ...#.....
# .#.......
# .........
# ..#L....#
# ....#....
# .........
# #........
# ...#.....
#
# The leftmost empty seat below would only see one empty seat, but cannot see
# any of the occupied ones:
#
# .............
# .L.L.#.#.#.#.
# .............
#
# The empty seat below would see no occupied seats:
#
# .##.##.
# #.#.#.#
# ##...##
# ...L...
# ##...##
# #.#.#.#
# .##.##.
#
# Also, people seem to be more tolerant than you expected: it now takes five or
# more visible occupied seats for an occupied seat to become empty (rather than
# four or more from the previous rules). The other rules still apply: empty
# seats that see no occupied seats become occupied, seats matching no rule
# don't change, and floor never changes.
#
# Given the same starting layout as above, these new rules cause the seating
# area to shift around as follows:
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
# #.LL.LL.L#
# #LLLLLL.LL
# L.L.L..L..
# LLLL.LL.LL
# L.LL.LL.LL
# L.LLLLL.LL
# ..L.L.....
# LLLLLLLLL#
# #.LLLLLL.L
# #.LLLLL.L#
#
# #.L#.##.L#
# #L#####.LL
# L.#.#..#..
# ##L#.##.##
# #.##.#L.##
# #.#####.#L
# ..#.#.....
# LLL####LL#
# #.L#####.L
# #.L####.L#
#
# #.L#.L#.L#
# #LLLLLL.LL
# L.L.L..#..
# ##LL.LL.L#
# L.LL.LL.L#
# #.LLLLL.LL
# ..L.L.....
# LLLLLLLLL#
# #.LLLLL#.L
# #.L#LL#.L#
#
# #.L#.L#.L#
# #LLLLLL.LL
# L.L.L..#..
# ##L#.#L.L#
# L.L#.#L.L#
# #.L####.LL
# ..#.#.....
# LLL###LLL#
# #.LLLLL#.L
# #.L#LL#.L#
#
# #.L#.L#.L#
# #LLLLLL.LL
# L.L.L..#..
# ##L#.#L.L#
# L.L#.LL.L#
# #.LLLL#.LL
# ..#.L.....
# LLL###LLL#
# #.LLLLL#.L
# #.L#LL#.L#
#
# Again, at this point, people stop shifting around and the seating area
# reaches equilibrium. Once this occurs, you count 26 occupied seats.
#
# Given the new visibility method and the rule change for occupied seats
# becoming empty, once equilibrium is reached, how many seats end up occupied?

library(purrr)
library(readr)

# read input and get width/height
board <- read_lines("input.txt")
height = length(board)
width = nchar(board[1])

# split each line and build a matrix
board <- map(board, strsplit, "") %>% unlist() %>% matrix(height, byrow = T)

# The cells that a given cell can "see" does *not* change,
# generation-to-generation. So let's precompute those indices. Indexing into
# "sight" will be [y, x, d, ] where x,y correspond to x,y in "board", and d
# starts at 1 with "up and left" and continuing clockwise. An NA indicates that
# there are no seats in that direction.
sight <- array(dim = c(height, width, 8, 2))
for (y in 1:height) {
  for (x in 1:width) {
    # From each cell, we'll move right, down right, down, and down left. We'll
    # fill in details for the current cell AND the cells we find during this
    # process.
    if (x < width) {
      for (x2 in (x + 1):width) {
        if (board[y, x2] != ".") {
          sight[y, x, 4, ] <- c(y, x2)
          if (is.na(sight[y, x2, 8, 1])) {
            sight[y, x2, 8, ] <- c(y, x)
          }
          break
        }
      }
    }
    if (x < width && y < height) {
      for (d in 1:min(width - x, height - y)) {
        if (board[y + d, x + d] != ".") {
          sight[y, x, 5, ] <- c(y + d, x + d)
          if (is.na(sight[y + d, x + d, 1, 1])) {
            sight[y + d, x + d, 1, ] <- c(y, x)
          }
          break
        }
      }
    }
    if (y < height) {
      for (y2 in (y + 1):height) {
        if (board[y2, x] != ".") {
          sight[y, x, 6, ] <- c(y2, x)
          if (is.na(sight[y2, x, 2, 1])) {
            sight[y2, x, 2, ] <- c(y, x)
          }
          break
        }
      }
    }
    if (x > 1 && y < height) {
      for (d in 1:min(x - 1, height - y)) {
        if (board[y + d, x - d] != ".") {
          sight[y, x, 7, ] <- c(y + d, x - d)
          if (is.na(sight[y + d, x - d, 3, 1])) {
            sight[y + d, x - d, 3, ] <- c(y, x)
          }
          break
        }
      }
    }
  }
}

# for each generation...
generation <- 1
repeat {
  print(generation)

  # Create a logical matrix, true for every position occupied. Use our "sight"
  # array from above to calculate a sum of occupied seats seen from each
  # position.
  taken <- board == "#"
  cnt <- apply(sight, c(1, 2), function(idxs) sum(taken[idxs], na.rm = T))

  # Generate the next generation based our rules.
  nboard <- ifelse(
    board == "L" & cnt == 0,
    "#",
    ifelse(
      taken & cnt >= 5,
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
