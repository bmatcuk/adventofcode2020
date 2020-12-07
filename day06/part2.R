# --- Part Two ---
# As you finish the last group's customs declaration, you notice that you
# misread one word in the instructions:
#
# You don't need to identify the questions to which anyone answered "yes"; you
# need to identify the questions to which everyone answered "yes"!
#
# Using the same example as above:
#
# abc
#
# a
# b
# c
#
# ab
# ac
#
# a
# a
# a
# a
#
# b
#
# This list represents answers from five groups:
#
# In the first group, everyone (all 1 person) answered "yes" to 3 questions: a,
# b, and c.
# In the second group, there is no question to which everyone answered "yes".
# In the third group, everyone answered yes to only 1 question, a. Since some
# people did not answer "yes" to b or c, they don't count.
# In the fourth group, everyone answered yes to only 1 question, a.
# In the fifth group, everyone (all 1 person) answered "yes" to 1 question, b.
# In this example, the sum of these counts is 3 + 0 + 1 + 1 + 1 = 6.
#
# For each group, count the number of questions to which everyone answered
# "yes". What is the sum of those counts?

library(dplyr)
library(readr)
library(tidyr)

# load data, one row per line
# rename some columns
# add a group_id, separated by the blank lines (marked "missing") - also add an
# "answer" column full of 1's
# break multiple characters into multiple rows
# remove the "missing" rows and rows where value is "" (caused by the previous step)
# select just the columns we need
# pivot the data by question and answer for each person - a 1 means they
# answered yes, a 0 means no
# group by group_id
# summarize all of the questions per group - a 1 means they all answered yes, a
# 0 means at least one person answered no
# add an "all_yes" column to sum the number of questions they all answered yes
# sum the all_yes column
melt_table2("input.txt") %>%
  rename(person_id = row, question = value) %>%
  mutate(group_id = cumsum(data_type == "missing") + 1, answer = 1) %>%
  separate_rows(question, sep = "") %>%
  filter(data_type != "missing", question != "") %>%
  select(group_id, person_id, question, answer) %>%
  pivot_wider(c(group_id, person_id), question, values_from = answer, values_fill = 0) %>%
  group_by(group_id) %>%
  summarize(across(.fns = ~ if_else(n() == sum(.x), 1, 0))) %>%
  mutate(all_yes = rowSums(select(., -group_id, -person_id))) %>%
  summarize(sum(all_yes))
