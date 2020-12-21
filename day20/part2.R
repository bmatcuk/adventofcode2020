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
        left = sum(2 ^ (which(tile[,1] == "#") - 1)),
        bottom = sum(2 ^ (which(tile[tile_height,] == "#") - 1)),
        right = sum(2 ^ (which(tile[,tile_width] == "#") - 1)),
        r_top = sum(2 ^ (which(tile[1,tile_width:1] == "#") - 1)),
        r_left = sum(2 ^ (which(tile[tile_height:1,1] == "#") - 1)),
        r_bottom = sum(2 ^ (which(tile[tile_height,tile_width:1] == "#") - 1)),
        r_right = sum(2 ^ (which(tile[tile_height:1,tile_width] == "#") - 1))
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
matches <- pivot_longer(edges, c(top, left, bottom, right, r_top, r_left, r_bottom, r_right)) %>%
  group_by(value) %>%
  filter(n() > 1)
matches <- ungroup(matches) %>%
  left_join(summarize(matches, link1 = max(num)), by = "value") %>%
  left_join(summarize(matches, link2 = min(num)), by = "value") %>%
  mutate(
    link = ifelse(num == link1, link2, link1),
    link1 = NULL,
    link2 = NULL
  ) %>%
  distinct(num, link, .keep_all = T)

# utility function to flip a tile
flip_tile <- function(edges, idx, edge) {
  if (edge == "top" || edge == "bottom") {
    if (!edges[idx, "h_flip"]) {
      edges[idx, "h_flip"] <- TRUE
      tmp <- edges[idx, "n_left"]
      edges[idx, "n_left"] <- edges[idx, "n_right"]
      edges[idx, "n_right"] <- tmp
    }
  } else if (!edges[idx, "v_flip"]) {
    edges[idx, "v_flip"] <- TRUE
    tmp <- edges[idx, "n_top"]
    edges[idx, "n_top"] <- edges[idx, "n_bottom"]
    edges[idx, "n_bottom"] <- tmp
  }
  edges
}

# determine how two tiles neighbor each other (if they do)
find_neighbors <- function(edges, i, j) {
  tbl <- edges[c(i, j),] %>%
    pivot_longer(c(top, left, bottom, right, r_top, r_left, r_bottom, r_right))
  dup_idx2 <- anyDuplicated(tbl$value)
  if (dup_idx2 > 0) {
    dup_idx1 <- anyDuplicated(tbl$value, fromLast = T)

    edge1 <- tbl[[dup_idx1, "name"]]
    if (substr(edge1, 1, 2) == "r_") {
      edge1 <- substring(edge1, 3)
      edges <- flip_tile(edges, dup_idx1, edge1)
    }

    edge2 <- tbl[[dup_idx2, "name"]]
    if (substr(edge2, 1, 2) == "r_") {
      edge2 <- substring(edge2, 3)
      edges <- flip_tile(edges, dup_idx2, edge2)
    }

    edge1 <- paste0("n_", edge1)
    edge2 <- paste0("n_", edge2)
    edges[i, edge1] <- tbl[[dup_idx2, "num"]]
    edges[j, edge2] <- tbl[[dup_idx1, "num"]]
    edges[i, "n_num"] <- edges[i, "n_num"] + 1
    edges[j, "n_num"] <- edges[j, "n_num"] + 1
  }
  edges
}

# determine all neighbors for a single tile
find_all_neighbors <- function(edges, rownum) {
  print(rownum)
  if (rownum < nrow(edges)) {
    (rownum + 1):nrow(edges) %>%
      reduce(.f = function(acc, j) find_neighbors(acc, rownum, j), .init = edges)
  } else {
    edges
  }
}

# find neighbors for all tiles
edges <- 1:nrow(edges) %>%
  reduce(.f = find_all_neighbors, .init = edges)

# find top left
top_left <- filter(edges, n_num == 2, is.na(n_top), is.na(n_left))[1, "num"]

# returns the indexes of the columns in a row with the given start index
get_col_idxs <- function(start) {
  1:1000 %>%
    accumulate(.f = function(prv, unused) {
      idx <- edges[prv, "n_right"]
      if (is.na(edges[idx, "n_right"])) {
        done(idx)
      } else {
        idx
      }
    }, .init = start)
}

# determine dimensions of our final image
tiles_width <- length(get_col_idxs(top_left))
tiles_height <- length(tiles) / tiles_width
tile_width <- ncol(tiles[[1]]) - 2
tile_height <- nrow(tiles[[1]]) - 2
width <- tiles_width * tile_width
height <- tiles_height * tile_height

# returns a tile, throwing away the border and flipping as necessary
get_tile <- function(idx) {
  cols <- 2:(tile_width + 1)
  rows <- 2:(tile_height + 1)
  if (edges[idx, "h_flip"]) cols <- rev(cols)
  if (edges[idx, "v_flip"]) rows <- rev(rows)
  tiles[[idx]]$tile[rows, cols]
}

# returns the indexes of the rows in a column with the given start index
get_row_idxs <- function(start) {
  1:1000 %>%
    accumulate(.f = function(prv, unused) {
      idx <- edges[prv, "n_bottom"]
      if (is.na(edges[idx, "n_bottom"])) {
        done(idx)
      } else {
        idx
      }
    }, .init = start)
}

# build image
get_row_idxs(top_left) %>%
  map_dfr(.f = function(rowidx) {
    map_dfc(get_col_idxs(rowidx), .f = get_tile)
  })
