# stringr package included in tidyverse
# uses the convention 'str_' for its functions
library(tidyverse)
library(dplyr)


# use web scraping to get raw murders data from Wikipedia
url <- "https://en.wikipedia.org/wiki/Murder_in_the_United_States_by_state"
h <- read_html(url)
tab <- h %>% html_nodes("table")
tab <- tab[[2]]
tab <- tab %>% html_table()

# change the names
murders_raw <- tab %>% setNames(c("state", "population", "total", "murders", "gun_murders", 
                          "gun_ownership", "total_rate", "murder_rate", "gun_murder_rate"))

# Numerical data is often stores as characters because of the commas
class(murders_raw$population)
class(murders_raw$total)

# We must remove the commas, then coerce to as.numeric

# String Processing usually involves 3 steps: DETECTING, LOCATING, REPLACING

# use str_detect
commas <- function(x) any(str_detect(x, ",")) # returns TRUE if any contain a comma
murders_raw %>% summarize_all(funs(commas)) # shows which columns contain commas

# use str_replace_all to replace commas with blank characters
test_1 <- str_replace_all(murders_raw$population, ",", "")
test_1 <- as.numeric(test_1) # murders_raw$population[14] is NA because it has "[5]"

# use mutate_all to apply to all columns

# Alternatively, readr has parse_number() which does everything at once
library(readr)
test_2 <- parse_number(murders_raw$population)
  
identical(test_1, test_2) # not identical because of "[5]"
  
# create a new table with population and total parsed
murders_new <- murders_raw %>% mutate_at(2:3, parse_number) 
  
########################
# 
#       PART 2
# 
#######################


# Heights
library(dslabs)
data(reported_heights)
class(reported_heights$height) # heights are character because of how some were inputted

x <- as.numeric(reported_heights$height)
sum(is.na(x))  # 81 of the entries were not in proper format

# show only those whose heights are NA
reported_heights %>% mutate(new_height = as.numeric(height)) %>%
    filter(is.na(new_height)) %>%
    head(n = 10)

# Calculate a cutoff value that covers 99.999% of the population
alpha <- 1/10^6
qnorm(1-alpha/2, 69.1, 2.9) # 83.29
qnorm(alpha/2, 63.7, 2.7) # 50.49

# function that returns incorrectly entered values
not_inches <- function(x, smallest = 50, tallest = 84) { # no person will be shorter than 50 or taller than 84
    inches <- suppressWarnings(as.numeric(x)) # ignore the warning
    ind <- is.na(inches) | inches < smallest | inches > tallest # TRUE if NA or the value (correct) if outside range
    ind
}

# problematic entries
problems <- reported_heights %>% 
    filter(not_inches(height)) %>% # filters only for not_inches
    .$height

#
# Looking closely at the problematic entries, we can discern 3 patterns:
#
#

# Pattern 1: x' y, x' y", or x' y\" 
pattern1 <- "^\\d\\s*'\\s*\\d{1,2}\\.*\\d*'*\"*$"
str_subset(problems, pattern1) %>% head(n=10) %>%
    cat


# Pattern 2: x.y or x,y
pattern2 <- "^[4-6]\\s*[\\.|,]\\s*([0-9]|10|11)$"
str_subset(problems, pattern2) %>% head(n = 10) %>%
    cat

# Pattern 3: entries in cm rather than inches
ind <- which(between(suppressWarnings(as.numeric(problems))/2.54, 54, 81))
ind <- ind[!is.na(ind)]
problems[ind] %>% head(n = 10) %>%
    cat

# Regex: regular expressions describe specific patterns of text through a set
# of rules

yes <- c("5", "6", "5'10", "5 feet", "4'11")
no <- c("", ".", "Five", "six")
s <- c(yes, no)

# d - digits 0 to 9, must be escaped with \ which in turns needs to be escaped
# so the regex pattern is "\\d"
str_view(s, "\\d")

# character classes are defined by brackets []
# e.g. the regex "[56]" detects the numbers 5 and 6
# while the regex "[0-9]" detects numbers in a range from 0 to 9 (same as "\\d")
str_view(s, "[56]")

# in regex EVERYTHING IS A CHARACTER so the regex "[1-20]" actually means
# the characters 1, 2, AND the character 0
# it works for letters as well "[a-z]" and "[A-Z]"

# what if we want ONLY ONE digit?

# ANCHORS 
# let us define patterns that must start or end in specific ways
# the caret (^) and dollar sign ($) represent the BEGINNING and END of a string
# i.e. the regex "^\\d$" is interpreted as: 
# "the start of the string, followed by a digit, and the end of the string
yes <- c("1", "5", "9")
no <- c("12", "123", " 1", "a4", "b")
s <- c(yes, no)
str_view(s, "^\\d$") # note that 1 is not highlighted because it has a space

# QUANTIFIERS
# marked by curly brackets {} and marks the number of times the preceding
# pattern can REPEAT
# e.g. "\\d{1,2}" means can be 1- or 2-digit numbers
str_view(s, "^\\d{1,2}$")

# looking for heights, we can add feet(') and inches(") symbols 
# don't forget to escape!
yes <- c("5'7\"", "6'2\"",  "5'12\"")
no <- c("6,2\"", "6.2\"","I am 5'11\"", "3'2\"", "64")
str_view(yes, "^\\d'\\d{1,2}\"$")
str_view(no, "^[4-7]'\\d{1,2}\"$") # specified height in ft to be between 4 and 7

# note that the inches are allowed to be > 12, which is not the correct way


# SEARCH and REPLACE
# using the pattern above on the problematic heights
pattern <- "^[4-7]'\\d{1,2}\"$"
sum(str_detect(problems, pattern)) # only 14 match

# investigating...
problems[c(2, 10, 11, 12, 15)]

# we see that some students wrote down whole words
str_subset(problems, "inches")
# or used single quotes twice to represent inches
str_subset(problems, "''")

# We can choose to simply remove any symbol for inches
# and work on a x'y format for heights
pattern <- "^[4-7]'\\d{1,2}$"
problems %>%
    str_replace("feet|ft|foot", "'") %>% # replace possible entries of height in feet to the symbol '
    str_replace("inches|in|''|\"", "") %>% # replace possible entries of height in inches (including the correct one) to empty char
    str_detect(pattern) %>%
    sum() # 48 matches

# SPACES are characters, regex "\s"
pattern2 <- "^[4-7]'\\s\\d{1,2}\"$" # add as space in between x' y"
str_subset(problems, pattern2) # note that this includes the " symbol for inches

# ASTERISK (*) QUANTIFIER asterisk 
# to allow spaces but not require them
# * means "0 or more" instances of the preceding character

yes <- c("AB", "A1B", "A11B", "A111B", "A1111B")
no <- c("A2B", "A21B")
str_detect(yes, "A1*B") # detects the character 1 any number of times
str_detect(no, "A1*B")


# QUESTION MARK (?) QUANTIFIER
# ? means "none or once"

# PLUS SIGN (+) QUANTIFIER
# + means "one or more"


# Improving our pattern to allow spaces before the feet ' symbol
# and after the feet ' symbol
pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$" # this time we will remove inches
problems %>%
    str_replace("feet|ft|foot", "'") %>%
    str_replace("inches|in|''|\"", "") %>%
    str_detect(pattern) %>%
    sum() # 53 matches

# Q: Why not just remove all the white spaces with str_replace_all()?
# A: There may be unintended consequences; for instance heights reported as "x y"

# Groups with Regex
# The 2nd problematic pattern was in the form "x.y", "x,y", or "x y"
# We want to change ALL of them to a common format: "x'y"

# We cannot search and replace because there are integers with decimals, 
# e.g. 70.5 will incorrectly be replaced with 70'5

# Groups () 

pattern_without_group <- "^[4-7],\\d*$"

# grouping doesn't affect the pattern matching but allows you to extract/save values
pattern_with_group <- "^([4-7]),(\\d*)$"

# use str_match to group the values the pattern detects
yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes, no)

str_match(s, pattern_with_group) # the 2nd and 3rd columns contain the extracted values

# str_extract only returns the strings that match the pattern, or the 1st column
str_extract(s, pattern_with_group)

# You can refer to the i-th group in a regex group with "\\i"
str_replace(s, pattern_with_group, "\\1,\\2") # replaces periods with a comma but only if between 2 digits

# Update pattern_with_group to also replace "x,y" and "x y" formats as well
pattern_with_group <- "^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$" 
# "[,\\.\\s+]" in place of the feet symbol means a comma, a dot, or one or more spaces

# now we can replace by referencing the groups separated by the feet symbol '
str_subset(problems, pattern_with_group) %>% 
    str_replace(pattern_with_group, "\\1'\\2") %>%
    head(n=10) # note that the resulting conversions is still incorrect, as there is an entry "5'25"


# TESTING and IMPROVING

# Write a function that captures all values cannot be converted into numbers, including those in cm

not_inches_or_cm <- function(x, smallest = 50, tallest = 84) { # recall that min and max are 99.999% of the population heights
    inches <- suppressWarnings(as.numeric(x)) 
    ind <- !is.na(inches) & # height is not NA
            ((inches >= smallest & inches <= tallest) |
               inches/2.54 >= smallest & inches/2.54 <= tallest) # within the limits
    !ind # if the entry meets above requirements, mark it as FALSE
}

problems <- reported_heights %>%
    filter(not_inches_or_cm(height)) %>%
    .$height # 200 problematic entries

# Convert formats to standard x'y
converted <- problems %>%
    str_replace("foot|ft|feet", "'") %>%
    str_replace("inches|in|''|\"", "") %>%
    str_replace(pattern_with_group, "\\1'\\2") 

# How many entries, after conversion, match the pattern we want?
pattern <- "^[4-6]\\s*'\\s*\\d{1,2}$" # includes spaces between '
index <- str_detect(converted, pattern) # marks matches with TRUE
mean(index) # 0.615 or 61.5% match the pattern after conversion

# Look at the remaining cases
converted[!index]

# Some entries are written in meters or with decimals (1.7) or with words (170 cm, Five ' eight)
# It may not be useful to write code for this if they are unique



# Separate and Extract
# Work similarly, but extract allows you to use regex
library(tidyr)
s <- c("5'10", "6'1\"","5'8inches")
tab <- data.frame(x = s)

tab %>%
    extract(x, c("feet", "inches"), regex = "(\\d)'(\\d{1,2})")


# Looking back at the remaining errors, we identify 4 CASES

# CASE 1: People measuring 6- or 5-foot flat only entered 1 digit or
# CASE 2: entered with no trailing inches e.g. 5' only

# SOLUTION: Add '0 to the end

converted <- converted %>%
      str_replace("^([56])'?$", "\\1'0") # we assume only 5' or 6'

# CASE 3: Some inches were entered with decimal points, e.g. 5' 7.5
# CASE 4: Spaces
# SOLUTION: We must allow at least one digit (+) followed by
# "one or none" (?) periods 
# followed by "zero or more" * digits
pattern <- "^([4-7])\\s*'\\s*(\\d+\\.?\\d*)$"

converted <- converted %>%
    str_replace(pattern, "\\1'\\2")

# CASE 5: Some entries in meters and use European decimal ,
# SOLUION: Convert entries that begin with 1 or 2 (since height in m tend to be that)
# from x,y to x.y formats

converted <- converted %>%
    str_replace("^([12])\\s*,\\s*(\\d+)$", "\\1.\\2") # includes spaces

# Trim trailing spaces
converted <- converted %>%
    str_trim()

# Spelled out words
converted <- converted %>%
    str_to_lower()

# Now, combine all processing into one function
convert_format <- function(s) {
  s %>%
    str_replace("foot|ft|feet", "'") %>% # formats feet to '
    str_replace("inches|in|''|\"", "") %>% # formats inches to empty
    str_replace("^([56])'?$", "\\1'0") %>% # adds '0 to 5- or 6-foot flat
    str_replace("^([4-7])\\s*'\\s*(\\d+\\.?\\d*)$", "\\1'\\2") %>% # handles inches with decimal values and formats correctly
    str_replace("^([12])\\s*,\\s*(\\d+)$", "\\1.\\2") %>% # handles European meters
    str_trim() # removes extra spaces
}

# Write a function to replace words with characters
words_to_numbers <- function(s) {
  str_to_lower(s) %>%
    str_replace_all("zero", "0") %>%
    str_replace_all("one", "1") %>%
    str_replace_all("two", "2") %>%
    str_replace_all("three", "3") %>%
    str_replace_all("four", "4") %>%
    str_replace_all("five", "5") %>%
    str_replace_all("six", "6") %>%
    str_replace_all("seven", "7") %>%
    str_replace_all("eight", "8") %>%
    str_replace_all("nine", "9") %>%
    str_replace_all("ten", "10") %>%
    str_replace_all("eleven", "11")
}

# Now combine all (refreshes converted)
converted <- problems %>%
    words_to_numbers() %>%
    convert_format()

remaining_problems <- converted[not_inches_or_cm(converted)]
pattern <- "^[4-7]\\s*'\\s*\\d+\\.?\\d*$"
index <- str_detect(remaining_problems, pattern)
remaining_problems[!index]


# Putting it all together
pattern <- "^([4-7])\\s*'\\s*(\\d+\\.?\\d*)$"

smallest <- 50
tallest <- 84
new_heights <- reported_heights %>% 
  mutate(original = height, 
         height = words_to_numbers(height) %>% convert_format()) %>%
  extract(height, c("feet", "inches"), regex = pattern, remove = FALSE) %>% 
  mutate_at(c("height", "feet", "inches"), as.numeric) %>%
  mutate(guess = 12*feet + inches) %>%
  mutate(height = case_when(
    !is.na(height) & between(height, smallest, tallest) ~ height, #inches 
    !is.na(height) & between(height/2.54, smallest, tallest) ~ height/2.54, #centimeters
    !is.na(height) & between(height*100/2.54, smallest, tallest) ~ height*100/2.54, #meters
    !is.na(guess) & inches < 12 & between(guess, smallest, tallest) ~ guess, #feet'inches
    TRUE ~ as.numeric(NA))) %>%
  select(-guess)

# Check all entries that were converted
new_heights %>%
  filter(not_inches(original)) %>%
  select(original, height) %>%
  arrange(height) %>%
  View()
  

# Find the shortest student
new_heights %>% arrange(height) %>% head(n = 7)
# Heights of 50-54 inches is rare and could have actually been 5'0 but
# because we don't know, we will leave them as is


# String Processing Continued
library(tidyverse)
# String Splitting
# use str_split to split a character vector on a delimiter

# read raw murders data line by line
filename <- system.file("extdata/murders.csv", package = "dslabs")
lines <- readLines(filename)
lines %>% head()

# Split entries by commas
x <- str_split(lines, ",")
col_names <- x[[1]]
x <- x[-1]

# using the purr library to extract elements in a list
library(purrr)
library(readr)
map(x, 1) # returns state names

# extract and convert to proper format using map_char or map_int
dat <- data.frame(parse_guess(map_chr(x, 1)),
                  parse_guess(map_chr(x, 2)),
                  parse_guess(map_chr(x, 3)),
                  parse_guess(map_chr(x, 4)),
                  parse_guess(map_chr(x, 5))) %>%
  setNames(col_names)


# A more efficient way of doing this would be
dat <- x %>%
  transpose() %>%
  map( ~ parse_guess(unlist(.))) %>%
  setNames(col_names) %>% 
  as.data.frame() 

# Or by using the simplify argument in str_split
x <- str_split(lines, ",", simplify = TRUE) 
col_names <- x[1,]
x <- x[-1,]
x %>% as_tibble() %>%
  setNames(col_names) %>%
  mutate_all(parse_guess)

#########################
# 
#       PART 3
# 
#########################

library(tidyverse)
library(readr)

# Recoding - e.g. countries with long names may be shortened
# using recode()

library(dslabs)
data("gapminder")

# We want to look at life expectancy time series for Caribbean countries
gapminder %>% 
  filter(region == "Caribbean") %>%
  ggplot(aes(year, life_expectancy, color = country)) +
  geom_line()

# But we notice that much of the graph is taken up by the country names

# These countries have names with 12 or more characters
gapminder %>%
  filter(region == "Caribbean") %>%
  filter(str_length(country) >= 12) %>%
  distinct(country) # ignores duplicates, since each country appears every year

# Redo the graph but recode the names
gapminder %>%
  filter(region == "Caribbean") %>%
  mutate(country = recode(country, 
                          'Antigua and Barbuda' = "Barbuda",
                          'Dominican Republic' = "DR",
                          'St. Vincent and the Grenadines' = "St. Vincent",
                          'Trinidad and Tobago' = "Trinidad")) %>%
  ggplot(aes(year, life_expectancy, color = country)) +
  geom_line()







