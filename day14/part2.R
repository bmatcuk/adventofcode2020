# --- Part Two ---
# For some reason, the sea port's computer system still can't communicate with
# your ferry's docking program. It must be using version 2 of the decoder chip!
#
# A version 2 decoder chip doesn't modify the values being written at all.
# Instead, it acts as a memory address decoder. Immediately before a value is
# written to memory, each bit in the bitmask modifies the corresponding bit of
# the destination memory address in the following way:
#
# If the bitmask bit is 0, the corresponding memory address bit is unchanged.
# If the bitmask bit is 1, the corresponding memory address bit is overwritten with 1.
# If the bitmask bit is X, the corresponding memory address bit is floating.
#
# A floating bit is not connected to anything and instead fluctuates
# unpredictably. In practice, this means the floating bits will take on all
# possible values, potentially causing many memory addresses to be written all
# at once!
#
# For example, consider the following program:
# 
# mask = 000000000000000000000000000000X1001X
# mem[42] = 100
# mask = 00000000000000000000000000000000X0XX
# mem[26] = 1
#
# When this program goes to write to memory address 42, it first applies the
# bitmask:
#
# address: 000000000000000000000000000000101010  (decimal 42)
# mask:    000000000000000000000000000000X1001X
# result:  000000000000000000000000000000X1101X
#
# After applying the mask, four bits are overwritten, three of which are
# different, and two of which are floating. Floating bits take on every
# possible combination of values; with two floating bits, four actual memory
# addresses are written:
#
# 000000000000000000000000000000011010  (decimal 26)
# 000000000000000000000000000000011011  (decimal 27)
# 000000000000000000000000000000111010  (decimal 58)
# 000000000000000000000000000000111011  (decimal 59)
#
# Next, the program is about to write to memory address 26 with a different
# bitmask:
#
# address: 000000000000000000000000000000011010  (decimal 26)
# mask:    00000000000000000000000000000000X0XX
# result:  00000000000000000000000000000001X0XX
#
# This results in an address with three floating bits, causing writes to eight
# memory addresses:
#
# 000000000000000000000000000000010000  (decimal 16)
# 000000000000000000000000000000010001  (decimal 17)
# 000000000000000000000000000000010010  (decimal 18)
# 000000000000000000000000000000010011  (decimal 19)
# 000000000000000000000000000000011000  (decimal 24)
# 000000000000000000000000000000011001  (decimal 25)
# 000000000000000000000000000000011010  (decimal 26)
# 000000000000000000000000000000011011  (decimal 27)
#
# The entire 36-bit address space still begins initialized to the value 0 at
# every address, and you still need the sum of all values left in memory at the
# end of the program. In this example, the sum is 208.
#
# Execute the initialization program using an emulator for a version 2 decoder
# chip. What is the sum of all values left in memory after it completes?

library(bit64)
library(gtools)
library(hash)
library(purrr)
library(readr)

options(digits=22)

# R's lack of 64-bit int support is extremely frustrating... this is as simple
# as AND'ing with the X's replaced with 1's, and OR'ing with the X's replaced
# with 0's, but noooooo...

read_lines("input.txt") %>%
  reduce(
    .init = list(maskOnes = {}, maskX = {}, mem = hash()),
    .f = function(state, op) {
      if (grepl("^mask", op)) {
        # find the position of all the 1s and 0s... need to pad to 64 bits
        mask <- el(strsplit(paste0("0000000000000000000000000000", substring(op, 8)), ""))
        state$maskOnes = which(mask == "1")
        state$maskX = which(mask == "X")
      } else {
        # parsed[1] = memory location, parsed[2] = value
        # convert parsed[1] to a 64-bit bitstring
        # set the 1s according to our mask
        parsed <- el(regmatches(op, gregexpr("\\d+", op)))
        addr <- el(strsplit(as.bitstring(as.integer64(parsed[1])), ""))
        addr[state$maskOnes] <- "1"

        # find all permutations for replacing X's with 0's and 1's
        perms <- permutations(2, length(state$maskX), v = c("0", "1"), repeats.allowed = T)
        for (i in seq(nrow(perms))) {
          # set the X's to this permutation then update that memory address
          addr[state$maskX] <- perms[i,]
          addrs <- as.integer64.bitstring(paste(addr, collapse = ""))
          state$mem[addrs] <- as.numeric(parsed[2])
        }
      }
      state
    }
  ) %>%
  { sum(values(.$mem)) }
