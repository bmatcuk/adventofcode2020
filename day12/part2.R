# --- Part Two ---
# Before you can give the destination to the captain, you realize that the
# actual action meanings were printed on the back of the instructions the whole
# time.
#
# Almost all of the actions indicate how to move a waypoint which is relative
# to the ship's position:
#
# Action N means to move the waypoint north by the given value.
# Action S means to move the waypoint south by the given value.
# Action E means to move the waypoint east by the given value.
# Action W means to move the waypoint west by the given value.
# Action L means to rotate the waypoint around the ship left (counter-clockwise) the given number of degrees.
# Action R means to rotate the waypoint around the ship right (clockwise) the given number of degrees.
# Action F means to move forward to the waypoint a number of times equal to the given value.
#
# The waypoint starts 10 units east and 1 unit north relative to the ship. The
# waypoint is relative to the ship; that is, if the ship moves, the waypoint
# moves with it.
#
# For example, using the same instructions as above:
#
# - F10 moves the ship to the waypoint 10 times (a total of 100 units east and
# 10 units north), leaving the ship at east 100, north 10. The waypoint stays
# 10 units east and 1 unit north of the ship.
# - N3 moves the waypoint 3 units north to 10 units east and 4 units north of
# the ship. The ship remains at east 100, north 10.
# - F7 moves the ship to the waypoint 7 times (a total of 70 units east and 28
# units north), leaving the ship at east 170, north 38. The waypoint stays 10
# units east and 4 units north of the ship.
# - R90 rotates the waypoint around the ship clockwise 90 degrees, moving it to
# 4 units east and 10 units south of the ship. The ship remains at east 170,
# north 38.
# - F11 moves the ship to the waypoint 11 times (a total of 44 units east and
# 110 units south), leaving the ship at east 214, south 72. The waypoint stays
# 4 units east and 10 units south of the ship.
#
# After these operations, the ship's Manhattan distance from its starting
# position is 214 + 72 = 286.
#
# Figure out where the navigation instructions actually lead. What is the
# Manhattan distance between that location and the ship's starting position?

library(purrr)
library(tidyr)
library(readr)

# NOTE: this code assumes L and R are always positive 90, 180, or 270 degrees.

# load data and split into two columns: action and value
t <- read_table2("input.txt", col_names = F) %>%
  extract(X1, c("action", "value"), "(\\w)(\\d+)", convert = T)

# for each row, perform the action and return the Manhattan distance
1:nrow(t) %>%
  reduce(.f = function(result, i) {
    if (t[i,]$action == "N") {
      result$wayy <- result$wayy + t[i,]$value
    } else if (t[i,]$action == "E") {
      result$wayx <- result$wayx + t[i,]$value
    } else if (t[i,]$action == "S") {
      result$wayy <- result$wayy - t[i,]$value
    } else if (t[i,]$action == "W") {
      result$wayx <- result$wayx - t[i,]$value
    } else if (t[i,]$action == "F") {
      result$x <- result$x + t[i,]$value * result$wayx
      result$y <- result$y + t[i,]$value * result$wayy
    } else if (t[i,]$action == "L"){
      for (unused in seq(t[i,]$value / 90)) {
        tmp <- result$wayx
        result$wayx <- -result$wayy
        result$wayy <- tmp
      }
    } else if (t[i,]$action == "R") {
      for (unused in seq(t[i,]$value / 90)) {
        tmp <- result$wayx
        result$wayx <- result$wayy
        result$wayy <- -tmp
      }
    }
    result
  }, .init = list(x = 0, y = 0, wayx = 10, wayy = 1)) %>%
  { abs(.$x) + abs(.$y) }
