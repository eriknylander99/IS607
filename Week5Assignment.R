# Week 5 Assignment
# Three Questions
# 1. Is the preference for Cullen sink over Partan bree based on the location of the repondents?
# 2. IS the prefernece based on the age of the age of the respondent?
# 3. Is there a relationship in either of the cities based on the age of the respondent?

edinburgh <- list('16to24' = c(80100, 35900), '25' = c(143000, 214800))
glasgow <- list('16to24' = c(99400, 43000), '25' = c(150400, 207000))
scotland <- data.frame('Prefer' = c('yes', 'no'), 'Edinburgh' = edinburgh, 'Glasgow' = glasgow)
row.names(scotland) <- c('yes', 'no')
scotland
str(scotland)
require(tidyr)
require(dplyr)

scot_tidy <- scotland %>%
    gather(City, Votes, Edinburgh.16to24:Glasgow.25) %>%
    separate(City, c('Location', 'Age'))

scot_df <- tbl_df(scot_tidy)

# Question 1 Preference based on location
scot_df %>%
    filter(Location == 'Glasgow') %>%
    mutate(prob = Votes/sum(Votes)) %>%
    group_by(Location, Prefer) %>%
    summarise(totalprob = sum(prob))

scot_df %>%
    filter(Location == 'Edinburgh') %>%
    mutate(prob = Votes/sum(Votes)) %>%
    group_by(Location, Prefer) %>%
    summarise(totalprob = sum(prob))
    

# In Glasgow there seems to be an even split between people that prefer Cullen sink over
# Partan bree. However, in Edinburgh people are slightly more likely to not prefer Cullen sink over
# Partan bree.


# Question 2 Preference based on Age
scot_df %>%
    filter(Age == '16to24') %>%
    mutate(prob = Votes/sum(Votes)) %>%
    group_by(Age, Prefer) %>%
    summarise(totalprob = sum(prob))

scot_df %>%
    filter(Age == '25') %>%
    mutate(prob = Votes/sum(Votes)) %>%
    group_by(Age, Prefer) %>%
    summarise(totalprob = sum(prob))

# 16 to 24 year olds are more likely to prefer Cullen sink over Partan bree while people over
# the age of 25 are more likely to not prefer Cullen sink over Partan bree.


# Question 3 Is there a relationship between the ages preference in each city?

# Glasgow
scot_df %>%
    filter(Location == 'Glasgow', Age == '16to24') %>%
    mutate(prob = Votes/sum(Votes)) %>%
    group_by(Location, Age, Prefer) %>%
    summarise(totalprob = sum(prob))

scot_df %>%
    filter(Location == 'Glasgow', Age == '25') %>%
    mutate(prob = Votes/sum(Votes)) %>%
    group_by(Location, Age, Prefer) %>%
    summarise(totalprob = sum(prob))

# In Glasgow there is difference based on age with 16 to 24 year olds prefering Cullen sink
# and people over the age of 25 prefering Partan bree.

# Edinburgh
scot_df %>%
    filter(Location == 'Edinburgh', Age == '16to24') %>%
    mutate(prob = Votes/sum(Votes)) %>%
    group_by(Location, Age, Prefer) %>%
    summarise(totalprob = sum(prob))

scot_df %>%
    filter(Location == 'Edinburgh', Age == '25') %>%
    mutate(prob = Votes/sum(Votes)) %>%
    group_by(Location, Age, Prefer) %>%
    summarise(totalprob = sum(prob))

# In Edinburgh we see the same break down of preference based on age so there appears to be
# no correlation between the location and the preference based on age.


# Overall I'm happy with my questions but I know nothing about the data! If I am working for
# a company selling these items then I feel like we have an understanding of preferences
# by age range and in each city. I do really like the way the dplyr library works and the
# way that it allows for readable and functional code.