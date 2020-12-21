# --- Day 20: Jurassic Jigsaw ---
# The high-speed train leaves the forest and quickly carries you south. You can
# even see a desert in the distance! Since you have some spare time, you might
# as well see if there was anything interesting in the image the Mythical
# Information Bureau satellite captured.
#
# After decoding the satellite messages, you discover that the data actually
# contains many small images created by the satellite's camera array. The
# camera array consists of many cameras; rather than produce a single square
# image, they produce many smaller square image tiles that need to be
# reassembled back into a single image.
#
# Each camera in the camera array returns a single monochrome image tile with a
# random unique ID number. The tiles (your puzzle input) arrived in a random
# order.
#
# Worse yet, the camera array appears to be malfunctioning: each image tile has
# been rotated and flipped to a random orientation. Your first task is to
# reassemble the original image by orienting the tiles so they fit together.
#
# To show how the tiles should be reassembled, each tile's image data includes
# a border that should line up exactly with its adjacent tiles. All tiles have
# this border, and the border lines up exactly when the tiles are both oriented
# correctly. Tiles at the edge of the image also have this border, but the
# outermost edges won't line up with any other tiles.
#
# For example, suppose you have the following nine tiles:
#
# Tile 2311:
# ..##.#..#.
# ##..#.....
# #...##..#.
# ####.#...#
# ##.##.###.
# ##...#.###
# .#.#.#..##
# ..#....#..
# ###...#.#.
# ..###..###
#
# Tile 1951:
# #.##...##.
# #.####...#
# .....#..##
# #...######
# .##.#....#
# .###.#####
# ###.##.##.
# .###....#.
# ..#.#..#.#
# #...##.#..
#
# Tile 1171:
# ####...##.
# #..##.#..#
# ##.#..#.#.
# .###.####.
# ..###.####
# .##....##.
# .#...####.
# #.##.####.
# ####..#...
# .....##...
#
# Tile 1427:
# ###.##.#..
# .#..#.##..
# .#.##.#..#
# #.#.#.##.#
# ....#...##
# ...##..##.
# ...#.#####
# .#.####.#.
# ..#..###.#
# ..##.#..#.
#
# Tile 1489:
# ##.#.#....
# ..##...#..
# .##..##...
# ..#...#...
# #####...#.
# #..#.#.#.#
# ...#.#.#..
# ##.#...##.
# ..##.##.##
# ###.##.#..
#
# Tile 2473:
# #....####.
# #..#.##...
# #.##..#...
# ######.#.#
# .#...#.#.#
# .#########
# .###.#..#.
# ########.#
# ##...##.#.
# ..###.#.#.
#
# Tile 2971:
# ..#.#....#
# #...###...
# #.#.###...
# ##.##..#..
# .#####..##
# .#..####.#
# #..#.#..#.
# ..####.###
# ..#.#.###.
# ...#.#.#.#
#
# Tile 2729:
# ...#.#.#.#
# ####.#....
# ..#.#.....
# ....#..#.#
# .##..##.#.
# .#.####...
# ####.#.#..
# ##.####...
# ##..#.##..
# #.##...##.
#
# Tile 3079:
# #.#.#####.
# .#..######
# ..#.......
# ######....
# ####.#..#.
# .#...#.##.
# #.#####.##
# ..#.###...
# ..#.......
# ..#.###...
#
# By rotating, flipping, and rearranging them, you can find a square
# arrangement that causes all adjacent borders to line up:
#
# #...##.#.. ..###..### #.#.#####.
# ..#.#..#.# ###...#.#. .#..######
# .###....#. ..#....#.. ..#.......
# ###.##.##. .#.#.#..## ######....
# .###.##### ##...#.### ####.#..#.
# .##.#....# ##.##.###. .#...#.##.
# #...###### ####.#...# #.#####.##
# .....#..## #...##..#. ..#.###...
# #.####...# ##..#..... ..#.......
# #.##...##. ..##.#..#. ..#.###...
#
# #.##...##. ..##.#..#. ..#.###...
# ##..#.##.. ..#..###.# ##.##....#
# ##.####... .#.####.#. ..#.###..#
# ####.#.#.. ...#.##### ###.#..###
# .#.####... ...##..##. .######.##
# .##..##.#. ....#...## #.#.#.#...
# ....#..#.# #.#.#.##.# #.###.###.
# ..#.#..... .#.##.#..# #.###.##..
# ####.#.... .#..#.##.. .######...
# ...#.#.#.# ###.##.#.. .##...####
#
# ...#.#.#.# ###.##.#.. .##...####
# ..#.#.###. ..##.##.## #..#.##..#
# ..####.### ##.#...##. .#.#..#.##
# #..#.#..#. ...#.#.#.. .####.###.
# .#..####.# #..#.#.#.# ####.###..
# .#####..## #####...#. .##....##.
# ##.##..#.. ..#...#... .####...#.
# #.#.###... .##..##... .####.##.#
# #...###... ..##...#.. ...#..####
# ..#.#....# ##.#.#.... ...##.....
#
# For reference, the IDs of the above tiles are:
#
# 1951    2311    3079
# 2729    1427    2473
# 2971    1489    1171
#
# To check that you've assembled the image correctly, multiply the IDs of the
# four corner tiles together. If you do this with the assembled tiles from the
# example above, you get 1951 * 3079 * 2971 * 1171 = 20899048083289.
#
# Assemble the tiles into an image. What do you get if you multiply together
# the IDs of the four corner tiles?

library(dplyr)
library(purrr)
library(readr)
library(stringi)
library(stringr)
library(tidyr)

options(digits=22)

# read input - last line is blank, so ignore it
input <- read_lines("input.txt")
blanks <- head(which(input == ""), -1)

# find the borders of each tile
tiles <- map2_dfr(c(0, blanks), c(blanks, length(input)), function(s, e) {
    id <- as.double(str_extract(input[s + 1], "\\d+"))
    tile <- matrix(
      map(input[(s + 2):(e - 1)], function(l) el(strsplit(l, ""))) %>% unlist(),
      e - s - 2,
      byrow = TRUE
    )
    as.data.frame(
      list(
        id = id,
        top = paste0(tile[1,], collapse = ""),
        left = paste0(tile[,1], collapse = ""),
        bottom = paste0(tile[nrow(tile),], collapse = ""),
        right = paste0(tile[,ncol(tile)], collapse = "")
      )
    )
  }) %>%
  mutate(
    rownum = row_number(),
    topr = stri_reverse(top),
    leftr = stri_reverse(left),
    bottomr = stri_reverse(bottom),
    rightr = stri_reverse(right)
  )

# determine if two tiles are neighbors
are_neighbors <- function(i, j) {
  tiles[c(i, j),] %>%
    pivot_longer(c(-id, -rownum)) %>%
    { .$value } %>%
    duplicated() %>%
    any()
}

# count the number of neighbors a row has
num_neighbors <- function(rownum) {
  print(rownum)
  num <- 0
  for (i in 1:nrow(tiles)) {
    if (i != rownum && are_neighbors(i, rownum)) {
      num <- num + 1
    }
  }
  num
}

# for each tile, count the number of neighbors
# then filter only tiles that have two neighbors - they'll be in the corners
# return the rows - R _refused_ to show me result of prod() in anything but
# scientific notation, so I just manually multiplied outside of my program =(
rowwise(tiles) %>%
  mutate(num_neighbors = num_neighbors(rownum)) %>%
  filter(num_neighbors == 2) %>%
  ungroup() #%>%
  # summarize(prod(id))
