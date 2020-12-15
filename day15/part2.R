# --- Part Two ---
# Impressed, the Elves issue you a challenge: determine the 30000000th number
# spoken. For example, given the same starting numbers as above:
#
# Given 0,3,6, the 30000000th number spoken is 175594.
# Given 1,3,2, the 30000000th number spoken is 2578.
# Given 2,1,3, the 30000000th number spoken is 3544142.
# Given 1,2,3, the 30000000th number spoken is 261214.
# Given 2,3,1, the 30000000th number spoken is 6895259.
# Given 3,2,1, the 30000000th number spoken is 18.
# Given 3,1,2, the 30000000th number spoken is 362.
#
# Given your starting numbers, what will be the 30000000th number spoken?

# Because R uses 1-based indexing, we need to increase the numbers by 1
input <- c(11, 0, 1, 10, 5) + 1
spoken <- c()
for (i in seq(input)) {
  spoken[input[i]] <- i
}

nxt <- 19 + 1
for (i in seq(length(input) + 1, 30000000 - 1)) {
  if (is.na(spoken[nxt])) {
    spoken[nxt] <- i
    nxt <- 1
  } else {
    tmp <- nxt
    nxt <- i - spoken[nxt] + 1
    spoken[tmp] <- i
  }
}

# Because of the 1-based indexing, nxt will be 1 higher than the answer
nxt - 1
