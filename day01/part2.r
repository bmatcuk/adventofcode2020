# --- Part Two ---
# The Elves in accounting are thankful for your help; one of them even offers
# you a starfish coin they had left over from a past vacation. They offer you a
# second one if you can find three numbers in your expense report that meet the
# same criteria.
#
# Using the above example again, the three entries that sum to 2020 are 979,
# 366, and 675. Multiplying them together produces the answer, 241861950.
#
# In your expense report, what is the product of the three entries that sum to
# 2020?

x <- scan("input.txt", 0)
for (i in seq(along = x)) {
  for (j in seq(from = i + 1, to = length(x))) {
    k <- which(x[i] + x[j] + x == 2020)
    if (length(k) > 0) {
      print(x[i] * x[j] * x[k])
      quit()
    }
  }
}
