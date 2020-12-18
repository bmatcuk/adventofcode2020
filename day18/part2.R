# --- Part Two ---
# You manage to answer the child's questions and they finish part 1 of their
# homework, but get stuck when they reach the next section: advanced math.
#
# Now, addition and multiplication have different precedence levels, but
# they're not the ones you're familiar with. Instead, addition is evaluated
# before multiplication.
#
# For example, the steps to evaluate the expression 1 + 2 * 3 + 4 * 5 + 6 are
# now as follows:
#
# 1 + 2 * 3 + 4 * 5 + 6
#   3   * 3 + 4 * 5 + 6
#   3   *   7   * 5 + 6
#   3   *   7   *  11
#      21       *  11
#          231
#
# Here are the other examples from above:
#
# 1 + (2 * 3) + (4 * (5 + 6)) still becomes 51.
# 2 * 3 + (4 * 5) becomes 46.
# 5 + (8 * 3 + 9 + 3 * 4 * 3) becomes 1445.
# 5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4)) becomes 669060.
# ((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2 becomes 23340.
#
# What do you get if you add up the results of evaluating the homework problems
# using these new rules?

library(purrr)
library(readr)

options(digits=22)

# The Shunting-yard algorithm can be used to convert infix to postfix notation.
# This function handles the part of the algo that is responsible for moving
# operators to the output, except that it executes the operator instead of just
# outputing, combining the Shunting-yard algo with a postfix solver.
#
# NOTE: the code assumes the input is well-formed
run <- function(state, nxt_op) {
  idx <- length(state$ops)
  while (idx >= 1) {
    op <- state$ops[idx]
    if (op == "(") {
      if (nxt_op == ")") idx <- idx - 1
      break
    } else if (op == "+") {
      # + has higher precendence than * is this strange world
      state$nums <- append(head(state$nums, -2), sum(tail(state$nums, 2)))
    } else if (op == "*") {
      if (nxt_op == "+") {
        break
      }
      state$nums <- append(head(state$nums, -2), prod(tail(state$nums, 2)))
    }
    idx <- idx - 1
  }
  state$ops <- head(state$ops, idx)
  state
}

read_lines("input.txt") %>%
  map_dbl(.f = function(line) {
    # split into tokens, throwing away spaces
    el(strsplit(line, "")) %>%
      { .[. != " "] } %>%
      reduce(.f = function(state, token) {
        if (token == "(") {
          state$ops <- append(state$ops, token)
        } else if (token == "+" || token == "*") {
          state <- run(state, token)
          state$ops <- append(state$ops, token)
        } else if (token == ")") {
          state <- run(state, token)
        } else {
          state$nums <- append(state$nums, as.double(token))
        }
        state
      }, .init = list(nums = double(), ops = character())) %>%
    { run(., "")$nums[1] }
  }) %>%
  sum()
