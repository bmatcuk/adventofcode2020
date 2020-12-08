# --- Part Two ---
# After some careful analysis, you believe that exactly one instruction is
# corrupted.
#
# Somewhere in the program, either a jmp is supposed to be a nop, or a nop is
# supposed to be a jmp. (No acc instructions were harmed in the corruption of
# this boot code.)
#
# The program is supposed to terminate by attempting to execute an instruction
# immediately after the last instruction in the file. By changing exactly one
# jmp or nop, you can repair the boot code and make it terminate correctly.
#
# For example, consider the same program from above:
#
# nop +0
# acc +1
# jmp +4
# acc +3
# jmp -3
# acc -99
# acc +1
# jmp -4
# acc +6
#
# If you change the first instruction from nop +0 to jmp +0, it would create a
# single-instruction infinite loop, never leaving that instruction. If you
# change almost any of the jmp instructions, the program will still eventually
# find another jmp instruction and loop forever.
#
# However, if you change the second-to-last instruction (from jmp -4 to nop
# -4), the program terminates! The instructions are visited in this order:
#
# nop +0  | 1
# acc +1  | 2
# jmp +4  | 3
# acc +3  |
# jmp -3  |
# acc -99 |
# acc +1  | 4
# nop -4  | 5
# acc +6  | 6
#
# After the last instruction (acc +6), the program terminates by attempting to
# run the instruction below the last instruction in the file. With this change,
# after the program terminates, the accumulator contains the value 8 (acc +1,
# acc +1, acc +6).
#
# Fix the program so that it terminates normally by changing exactly one jmp
# (to nop) or nop (to jmp). What is the value of the accumulator after the
# program terminates?

library(dplyr)
library(purrr)
library(readr)
library(tibble)
library(tidyr)

# load lines
# convert to tibble - note the "name" column will be the row number
# extract opcode and argument, converting the argument to int
ops <- read_lines("input.txt") %>%
  enframe() %>%
  extract(value, c("opcode", "argument"), "(\\w+) ([+-]\\d+)", convert = T)

run <- function(fix) {
  # these lines make copies of the columns into vectors that we can modify
  # without modifying the original tibble
  opcodes <- ops$opcode
  arguments <- ops$argument
  instructions <- length(opcodes)
  reached <- rep(F, instructions)
  if (opcodes[fix] == "jmp") {
    opcodes[fix] <- "nop"
  } else if (opcodes[fix] == "nop") {
    opcodes[fix] <- "jmp"
  } else {
    # changing an "acc" won't fix the infinite loop
    return(list(fixed = F, acc = 0))
  }

  ip <- 1
  acc <- 0
  repeat {
    # loop until we reach a line we've already reached:
    if (reached[ip]) {
      break
    }

    reached[ip] <- T
    if (opcodes[ip] == "acc") acc <- acc + arguments[ip]
    if (opcodes[ip] == "jmp") ip <- ip + arguments[ip]
    else ip <- ip + 1

    if (ip > instructions) {
      # we've found a fix that works!
      return(list(fixed = T, acc = acc))
    }
  }

  list(fixed = F, acc = acc)
}

# call "run" for every row
# run returns a list - unnest it into separate columns
# return only the rows for which "fixed" is true
ops %>%
  mutate(results = map(name, run)) %>%
  unnest_wider(results) %>%
  filter(fixed)
