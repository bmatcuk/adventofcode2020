# --- Part Two ---
# Now, you're ready to check the image for sea monsters.
#
# The borders of each tile are not part of the actual image; start by removing
# them.
#
# In the example above, the tiles become:
#
# .#.#..#. ##...#.# #..#####
# ###....# .#....#. .#......
# ##.##.## #.#.#..# #####...
# ###.#### #...#.## ###.#..#
# ##.#.... #.##.### #...#.##
# ...##### ###.#... .#####.#
# ....#..# ...##..# .#.###..
# .####... #..#.... .#......
#
# #..#.##. .#..###. #.##....
# #.####.. #.####.# .#.###..
# ###.#.#. ..#.#### ##.#..##
# #.####.. ..##..## ######.#
# ##..##.# ...#...# .#.#.#..
# ...#..#. .#.#.##. .###.###
# .#.#.... #.##.#.. .###.##.
# ###.#... #..#.##. ######..
#
# .#.#.### .##.##.# ..#.##..
# .####.## #.#...## #.#..#.#
# ..#.#..# ..#.#.#. ####.###
# #..####. ..#.#.#. ###.###.
# #####..# ####...# ##....##
# #.##..#. .#...#.. ####...#
# .#.###.. ##..##.. ####.##.
# ...###.. .##...#. ..#..###
#
# Remove the gaps to form the actual image:
#
# .#.#..#.##...#.##..#####
# ###....#.#....#..#......
# ##.##.###.#.#..######...
# ###.#####...#.#####.#..#
# ##.#....#.##.####...#.##
# ...########.#....#####.#
# ....#..#...##..#.#.###..
# .####...#..#.....#......
# #..#.##..#..###.#.##....
# #.####..#.####.#.#.###..
# ###.#.#...#.######.#..##
# #.####....##..########.#
# ##..##.#...#...#.#.#.#..
# ...#..#..#.#.##..###.###
# .#.#....#.##.#...###.##.
# ###.#...#..#.##.######..
# .#.#.###.##.##.#..#.##..
# .####.###.#...###.#..#.#
# ..#.#..#..#.#.#.####.###
# #..####...#.#.#.###.###.
# #####..#####...###....##
# #.##..#..#...#..####...#
# .#.###..##..##..####.##.
# ...###...##...#...#..###
#
# Now, you're ready to search for sea monsters! Because your image is
# monochrome, a sea monster will look like this:
#
#                   # 
# #    ##    ##    ###
#  #  #  #  #  #  #   
#
# When looking for this pattern in the image, the spaces can be anything; only
# the # need to match. Also, you might need to rotate or flip your image before
# it's oriented correctly to find sea monsters. In the above image, after
# flipping and rotating it to the appropriate orientation, there are two sea
# monsters (marked with O):
#
# .####...#####..#...###..
# #####..#..#.#.####..#.#.
# .#.#...#.###...#.##.O#..
# #.O.##.OO#.#.OO.##.OOO##
# ..#O.#O#.O##O..O.#O##.##
# ...#.#..##.##...#..#..##
# #.##.#..#.#..#..##.#.#..
# .###.##.....#...###.#...
# #.####.#.#....##.#..#.#.
# ##...#..#....#..#...####
# ..#.##...###..#.#####..#
# ....#.##.#.#####....#...
# ..##.##.###.....#.##..#.
# #...#...###..####....##.
# .#.##...#.##.#.#.###...#
# #.###.#..####...##..#...
# #.###...#.##...#.##O###.
# .O##.#OO.###OO##..OOO##.
# ..O#.O..O..O.#O##O##.###
# #.#..##.########..#..##.
# #.#####..#.#...##..#....
# #....##..#.#########..##
# #...#.....#..##...###.##
# #..###....##.#...##.##.#
#
# Determine how rough the waters are in the sea monsters' habitat by counting
# the number of # that are not part of a sea monster. In the above example, the
# habitat's water roughness is 273.
#
# How many # are not part of a sea monster?

library(dplyr)
library(purrr)
library(readr)
library(stringr)
library(tidyr)

# read input - last line is blank, so ignore it
input <- read_lines("input.txt")
blanks <- head(which(input == ""), -1)

# build a list of tiles - each tile is a matrix
tiles <- map2(c(0, blanks), c(blanks, length(input)), function(s, e) {
    matrix(
      map(input[(s + 2):(e - 1)], function(l) el(strsplit(l, ""))) %>% unlist(),
      e - s - 2,
      byrow = TRUE
    )
  })

# get a tile's dimensions
tile_width <- ncol(tiles[[1]])
tile_height <- nrow(tiles[[1]])

# Find the borders of each tile
# Each border is some combination of #'s and .'s - if we treat the #'s as 1's,
# and .'s as 0's, we can convert that into a decimal number. Tiles can also be
# flipped, so we need the flipped versions, too.
edges <- 1:length(tiles) %>%
  map_dfr(function(i) {
    tile <- tiles[[i]]
    as.data.frame(
      list(
        num = i,
        top = sum(2 ^ (which(tile[1,] == "#") - 1)),
        right = sum(2 ^ (which(tile[,tile_width] == "#") - 1)),
        bottom = sum(2 ^ (which(tile[tile_height,] == "#") - 1)),
        left = sum(2 ^ (which(tile[,1] == "#") - 1)),
        r_top = sum(2 ^ (which(tile[1,tile_width:1] == "#") - 1)),
        r_right = sum(2 ^ (which(tile[tile_height:1,tile_width] == "#") - 1)),
        r_bottom = sum(2 ^ (which(tile[tile_height,tile_width:1] == "#") - 1)),
        r_left = sum(2 ^ (which(tile[tile_height:1,1] == "#") - 1))
      )
    )
  })

# Match borders
# We take the list of edges above and pivot it longer, group by the values, and
# drop any group that doesn't have more than 1 match (ie, an edge that doesn't
# match any other edge).
#
# At this point, each group has 2 rows. We join with ourself twice, once with
# the min and again with the max num of each group. This will produce rows that
# show num X links with num X, so we remove those. It will also show that num X
# links with num Y twice because both the forward and flipped borders will
# match, so we de-dup those, too.
#
# Lastly, we pivot the table so we have one row per tile with columns
# describing what tiles are adjacent.
links <- edges %>%
  pivot_longer(c(top, left, bottom, right, r_top, r_left, r_bottom, r_right)) %>%
  group_by(value) %>%
  filter(n() > 1)
links <- links %>%
  ungroup() %>%
  left_join(summarize(links, link1 = max(num)), by = "value") %>%
  left_join(summarize(links, link2 = min(num)), by = "value") %>%
  mutate(
    link = ifelse(num == link1, link2, link1),
    link1 = NULL,
    link2 = NULL
  ) %>%
  distinct(num, link, .keep_all = T) %>%
  pivot_wider(names_from = name, values_from = link) %>%
  group_by(num) %>%
  summarize(
    across(c(top, right, bottom, left), .fns = ~ {
      r <- na.omit(.x)
      ifelse(is_empty(r), NA, r)
    }),
    n = n()
  )

# let's find a tile that'll work as our top-left - any tile with only 2
# neighbors will do - we can always rotate/flip to make it work.
top_left_idx <- links %>% filter(n == 2) %>% { .[[1, "num"]] }

# this function figures out how a tile should be rotated/flipped based on the
# top and left edge constraints
orient_tile <- function(num, top_idx, left_idx, special = 1) {
  # the following is used quite a bit below:
  # tile_links[[2]] = top
  # tile_links[[3]] = right
  # tile_links[[4]] = bottom
  # tile_links[[5]] = left
  tile_links <- links[num,]

  # "top" is at index 2 - so, the current position of top_idx minus 2 tells us
  # how many times we need to rotate to the left
  rotation <- if (is.na(top_idx)) {
    which(is.na(tile_links))[[special]] - 2
  } else {
    which(tile_links == top_idx)[[1]] - 2
  }

  # "left" is at index 5 - rotation2 should be either 0 (meaning it's already
  # in the right place), or 2 (meaning we need to horizontally flip the tile
  # after rotating)
  rotation2 <- if (is.na(left_idx)) {
    last(which(is.na(tile_links)))
  } else {
    which(tile_links == left_idx)[[1]]
  }
  rotation2 <- (rotation2 + 3 - rotation) %% 4

  # the top right tile is a weird corner case - since it has two NA links, it's
  # possible that rotation2 might not be 0 or 2 if we picked the wrong one to
  # be "top", so, try again:
  if (is.na(top_idx) && rotation2 != 0 && rotation2 != 2 && special == 1) {
    return(orient_tile(num, top_idx, left_idx, 2))
  }
  stopifnot("invalid tile"=rotation2 == 0 || rotation2 == 2)

  # as mentioned above, we need to flip if rotation2 == 2
  flip <- rotation2 == 2

  # "bottom" should be at index 4, but we need to adjust for rotation
  bottom_idx <- (2 + rotation) %% 4 + 2

  # "right" should be at index 3, but we need to adjust for rotation and flip
  right_idx <- if (flip) {
    # we actually want "left" at index 5
    (3 + rotation) %% 4 + 2
  } else {
    (1 + rotation) %% 4 + 2
  }

  list(
    num = num,
    rotation = rotation,
    flip = flip,
    bottom = tile_links[[bottom_idx]],
    right = tile_links[[right_idx]]
  )
}

# figure out how to arrange all of the tiles
orientations <- 2:nrow(links) %>%
  reduce(.f = function(orientations, idx) {
    board_width <- attr(orientations, "board_width")
    prv <- orientations[idx - 1,]
    new_row <- is.na(prv$right)
    if (new_row && is.null(board_width)) {
      board_width <- idx - 1
      attr(orientations, "board_width") <- board_width
    }
    rbind(
      orientations,
      if (is.null(board_width)) {
        orient_tile(prv$right, NA, prv$num)
      } else {
        above <- orientations[idx - board_width,]
        if (new_row) {
          orient_tile(above$bottom, above$num, NA)
        } else {
          orient_tile(prv$right, above$num, prv$num)
        }
      }
    )
  }, .init = as.data.frame(orient_tile(top_left_idx, NA, NA)))

# calculate the size of the board - we're going to remove the border around
# each tile, so subtract 2 from the tile width/height
tiles_per_row <- attr(orientations, "board_width")
tile_width <- tile_width - 2
tile_height <- tile_height - 2
board_width <- tiles_per_row * tile_width
board_height <- (length(tiles) / tiles_per_row) * tile_height

# utility function to rotate a matrix counter clockwise
rotate90 <- function(m) {
  t(m)[ncol(m):1,]
}

# utility function to flip a matrix horizontally
flip <- function(m) {
  m[,ncol(m):1]
}

# build the board
board <- 1:nrow(orientations) %>%
  reduce(.f = function(board, idx) {
    y <- (idx - 1) %/% tiles_per_row * tile_height + 1
    x <- (idx - 1) %% tiles_per_row * tile_width + 1
    orientation <- orientations[idx,]
    tile <- tiles[[orientation$num]][1 + 1:tile_height, 1 + 1:tile_width]
    if (orientation$rotation > 0) {
      for (i in 1:orientation$rotation) {
        tile <- rotate90(tile)
      }
    }
    if (orientation$flip) {
      tile <- flip(tile)
    }
    board[y:(y + tile_height - 1), x:(x + tile_width - 1)] <- tile
    board
  }, .init = matrix(nrow = board_height, ncol = board_width))

# sea monster - will be interpreted as regex
# need to use positive lookahead to capture possible overlaps
monster <- c(
  "(?=(..................)#(.))",
  "(?=()#(....)#()#(....)#()#(....)#()#()#())",
  "(?=(.)#(..)#(..)#(..)#(..)#(..)#(...))"
)

# here there be dragons...
lines <- 1:8 %>%
  reduce(.f = function(board, testcase) {
    lines <- apply(board, 1, function(l) paste0(l, collapse = ""))

    monsters <- which(!is.na(str_locate(lines[2:(length(lines) - 1)], monster[2])[,1])) %>%
      map_df(function(line) {
        matches <- list(
          str_locate_all(lines[line], monster[1])[[1]][,1],
          str_locate_all(lines[line + 1], monster[2])[[1]][,1],
          str_locate_all(lines[line + 2], monster[3])[[1]][,1]
        ) %>% reduce(intersect)
        tibble(line, col = matches)
      })

    if (count(monsters) > 0) {
      done(
        1:nrow(monsters) %>%
          reduce(.f = function(lines, i) {
            linenum <- monsters[[i, "line"]]
            col <- monsters[[i, "col"]]
            for (j in 0:2) {
              line <- lines[linenum + j]
              replacement <- paste0(tail(str_match(substring(line, col), monster[j + 1])[1,], -1), collapse = "o")
              lines[linenum + j] <- paste0(substr(line, 1, col - 1), replacement, substring(line, col + 20))
            }
            lines
          }, .init = lines)
      )
    } else if (testcase == 4) {
      flip(board)
    } else {
      rotate90(board)
    }
  }, .init = board)
print(lines)

# FINALLY
sum(str_count(lines, "#"))
