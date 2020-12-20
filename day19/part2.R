# --- Part Two ---
# As you look over the list of messages, you realize your matching rules aren't
# quite right. To fix them, completely replace rules 8: 42 and 11: 42 31 with
# the following:
#
# 8: 42 | 42 8
# 11: 42 31 | 42 11 31
#
# This small change has a big impact: now, the rules do contain loops, and the
# list of messages they could hypothetically match is infinite. You'll need to
# determine how these changes affect which messages are valid.
#
# Fortunately, many of the rules are unaffected by this change; it might help
# to start by looking at which rules always match the same set of values and
# how those rules (especially rules 42 and 31) are used by the new versions of
# rules 8 and 11.
#
# (Remember, you only need to handle the rules you have; building a solution
# that could handle any hypothetical combination of rules would be
# significantly more difficult.)
#
# For example:
#
# 42: 9 14 | 10 1
# 9: 14 27 | 1 26
# 10: 23 14 | 28 1
# 1: "a"
# 11: 42 31
# 5: 1 14 | 15 1
# 19: 14 1 | 14 14
# 12: 24 14 | 19 1
# 16: 15 1 | 14 14
# 31: 14 17 | 1 13
# 6: 14 14 | 1 14
# 2: 1 24 | 14 4
# 0: 8 11
# 13: 14 3 | 1 12
# 15: 1 | 14
# 17: 14 2 | 1 7
# 23: 25 1 | 22 14
# 28: 16 1
# 4: 1 1
# 20: 14 14 | 1 15
# 3: 5 14 | 16 1
# 27: 1 6 | 14 18
# 14: "b"
# 21: 14 1 | 1 14
# 25: 1 1 | 1 14
# 22: 14 14
# 8: 42
# 26: 14 22 | 1 20
# 18: 15 15
# 7: 14 5 | 1 21
# 24: 14 1
#
# abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa
# bbabbbbaabaabba
# babbbbaabbbbbabbbbbbaabaaabaaa
# aaabbbbbbaaaabaababaabababbabaaabbababababaaa
# bbbbbbbaaaabbbbaaabbabaaa
# bbbababbbbaaaaaaaabbababaaababaabab
# ababaaaaaabaaab
# ababaaaaabbbaba
# baabbaaaabbaaaababbaababb
# abbbbabbbbaaaababbbbbbaaaababb
# aaaaabbaabaaaaababaa
# aaaabbaaaabbaaa
# aaaabbaabbaaaaaaabbbabbbaaabbaabaaa
# babaaabbbaaabaababbaabababaaab
# aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba
#
# Without updating rules 8 and 11, these rules only match three messages:
# bbabbbbaabaabba, ababaaaaaabaaab, and ababaaaaabbbaba.
#
# However, after updating rules 8 and 11, a total of 12 messages match:
#
# bbabbbbaabaabba
# babbbbaabbbbbabbbbbbaabaaabaaa
# aaabbbbbbaaaabaababaabababbabaaabbababababaaa
# bbbbbbbaaaabbbbaaabbabaaa
# bbbababbbbaaaaaaaabbababaaababaabab
# ababaaaaaabaaab
# ababaaaaabbbaba
# baabbaaaabbaaaababbaababb
# abbbbabbbbaaaababbbbbbaaaababb
# aaaaabbaabaaaaababaa
# aaaabbaabbaaaaaaabbbabbbaaabbaabaaa
# aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba
#
# After updating rules 8 and 11, how many messages completely match rule 0?

library(tidyverse)

# read input
input <- read_lines("input.txt")
blank <- which(input == "")[1]

# parse rules
rules <- enframe(input[1:(blank - 1)], name = NULL) %>%
  extract(value, into = c("id", "rule"), "(\\d+): (.+)", convert = T) %>%
  arrange(id)

# this function will build a regex recursively
build_regex <- function(id) {
  if (id == 8) {
    # special case for rule 8 makes it essentially (42)+
    return(paste0("(", build_regex(42), ")+"))
  } else if (id == 11) {
    # special case for rule 11 makes it recursive:
    # (42 31|42 42 31 31|42 42 42 31 31 31|etc)
    # basically, a number of 42's followed by an equal number of 31's
    # Since we don't support recursive regex, we're just going to unroll a few
    # and see when the answer stops changing... looks like it's 5
    rule42 <- build_regex(42)
    rule31 <- build_regex(31)
    return(
      paste0(
        "(",
        1:4 %>%
          accumulate(.f = function(acc, i) {
            paste0(rule42, acc, rule31)
          }, .init = paste0(rule42, rule31)) %>%
          paste0(collapse = "|"),
        ")"
      )
    )
  }

  rule <- rules[id + 1,]$rule
  if (str_starts(rule, fixed('"'))) {
    return(substr(rule, 2, 2))
  }

  paste0(
    "(",
    map_chr(el(strsplit(rule, " | ", fixed = T)), .f = function(part) {
      el(strsplit(part, " ", fixed = T)) %>%
        as.integer() %>%
        map_chr(.f = build_regex) %>%
        paste0(collapse = "")
    }) %>% paste0(collapse = "|"),
    ")"
  )
}

# find messages that match the regex
sum(grepl(paste0("^", build_regex(0), "$"), input[(blank + 1):length(input)]))
