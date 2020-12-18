# --- Day 18: Operation Order ---
# As you look out the window and notice a heavily-forested continent slowly
# appear over the horizon, you are interrupted by the child sitting next to
# you. They're curious if you could help them with their math homework.
#
# Unfortunately, it seems like this "math" follows different rules than you
# remember.
#
# The homework (your puzzle input) consists of a series of expressions that
# consist of addition (+), multiplication (*), and parentheses ((...)). Just
# like normal math, parentheses indicate that the expression inside must be
# evaluated before it can be used by the surrounding expression. Addition still
# finds the sum of the numbers on both sides of the operator, and
# multiplication still finds the product.
#
# However, the rules of operator precedence have changed. Rather than
# evaluating multiplication before addition, the operators have the same
# precedence, and are evaluated left-to-right regardless of the order in which
# they appear.
#
# For example, the steps to evaluate the expression 1 + 2 * 3 + 4 * 5 + 6 are
# as follows:
#
# 1 + 2 * 3 + 4 * 5 + 6
#   3   * 3 + 4 * 5 + 6
#       9   + 4 * 5 + 6
#          13   * 5 + 6
#              65   + 6
#                  71
#
# Parentheses can override this order; for example, here is what happens if
# parentheses are added to form 1 + (2 * 3) + (4 * (5 + 6)):
#
# 1 + (2 * 3) + (4 * (5 + 6))
# 1 +    6    + (4 * (5 + 6))
#      7      + (4 * (5 + 6))
#      7      + (4 *   11   )
#      7      +     44
#             51
#
# Here are a few more examples:
#
# 2 * 3 + (4 * 5) becomes 26.
# 5 + (8 * 3 + 9 + 3 * 4 * 3) becomes 437.
# 5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4)) becomes 12240.
# ((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2 becomes 13632.
#
# Before you can help with the homework, you need to understand it yourself.
# Evaluate the expression on each line of the homework; what is the sum of the
# resulting values?

library(purrr)
library(readr)

options(digits=22)

# The Shunting-yard algorithm can be used to convert infix to postfix notation.
# This function handles the part of the algo that is responsible for moving
# operators to the output, except that it executes the operator instead of just
# outputing, combining the Shunting-yard algo with a postfix solver.
#
# NOTE: the code assumes the input is well-formed
run <- function(state) {
  idx <- length(state$ops)
  while (idx >= 1 && state$ops[idx] != "(") {
    op <- state$ops[idx]
    idx <- idx - 1
    if (op == "+") {
      # + has the same precendence as * is this strange world
      state$nums <- append(head(state$nums, -2), sum(tail(state$nums, 2)))
    } else if (op == "*") {
      state$nums <- append(head(state$nums, -2), prod(tail(state$nums, 2)))
    }
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
          state <- run(state)
          state$ops <- append(state$ops, token)
        } else if (token == ")") {
          state <- run(state)
          state$ops <- head(state$ops, -1)
        } else {
          state$nums <- append(state$nums, as.double(token))
        }
        state
      }, .init = list(nums = double(), ops = character())) %>%
    { run(.)$nums[1] }
  }) %>%
  sum()
