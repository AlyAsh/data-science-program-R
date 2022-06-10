# Dates and Times

# inspect the startdate column of 2016 polls data, a Date type
library(tidyverse)
library(dslabs)
data("polls_us_election_2016")

# lubridate: the tidyverse date package
library(lubridate)

# select some random dates from polls
set.seed(2)
dates <- sample(polls_us_election_2016$startdate, 10) %>% sort
dates

# extract month, day, year from date strings
data.frame(date = dates, 
           month = month(dates),
           day = day(dates),
           year = year(dates))

month(dates, label = TRUE)    # extract month label

# ymd works on mixed date styles
x <- c(20090101, "2009-01-02", "2009 01 03", "2009-1-4",
       "2009-1, 5", "Created on 2009 1 6", "200901 !!! 07")
ymd(x)

# different parsers extract year, month and day in different orders
x <- "09/01/02"
ymd(x)
mdy(x)
ydm(x)
myd(x)
dmy(x)
dym(x)

now()    # current time in your time zone, use OlsonNames() to list time zones
now("GMT")    # current time in GMT
now() %>% hour()    # current hour
now() %>% minute()    # current minute
now() %>% second()    # current second

# parse time
x <- c("12:34:56")
hms(x)

#parse datetime
x <- "Nov/2/2012 12:34:56"
mdy_hms




# Text Mining

# The tidytext package helps us convert free form text into a tidy table.
# unnest_tokens() to extract individual words and other meaningful chunks of text
# Sentiment analysis assigns emotions or a positive/negative score to tokens. 
# You can extract sentiments using get_sentiments()
# Common lexicons for sentiment analysis are "bing", "afinn", "nrc" and "loughran"

# in many applications, raw data starts as text, not numerical
# our task is to extract insights from these

# Case study: Trump Tweets
# David Robinson's analysis to determine whether Todd Vaziri was correct in saying
# that "every non-hyperbolic tweet" came from his staff on iPhone and "every hyperbolic tweet"
# came from Trump on his Android

# Load the ff libraries
library(tidyverse)
library(ggplot2)
library(lubridate)
library(tidyr)
library(scales)
set.seed(1)


# Normally, we can extract Twitter data using the rtweet package
# But the data we want is already compiled and made available online

# url <- 'http://www.trumptwitterarchive.com/data/realdonaldtrump/%s.json'
# trump_tweets <- map(2009:2017, ~sprintf(url, .x)) %>%
#       map_df(jsonlite::fromJSON, simplifyDataFrame = TRUE) %>%
#       filter(!is_retweet & !str_detect(text, '^"')) %>%
#       mutate(created_at = parse_date_time(created_at, orders = "a b! d! H!:M!:S! z!* Y!", tz="EST"))

# The result of the code above (not working, could not resolve host) is in dslabs
library(dslabs)
data("trump_tweets")

# Looking at the data, the source variable tells us what device it came from
trump_tweets %>% count(source) %>% arrange(desc(n))

# use extract group in a column called "source" the devices used, 
# following pattern "Twitter for " 
# extract turns each group into a column
trump_tweets %>%
    extract(source, "source", "Twitter for (.*)") %>%
    count(source)


# We are interested in what happened during the campaign
# dates are between the day Trump announced his campaign and election day

campaign_tweets <- trump_tweets %>%
      extract(source, "source", "Twitter for (.*)") %>%
      filter(source %in% c("Android", "iPhone") & # source is either Android or iPhone
              created_at >= ymd("2015-06-17") & # parses campaign start date
              created_at < ymd("2016-11-08")) %>% # election day
      filter(!is_retweet) %>% # exclude retweets
      arrange(created_at) # sort by created_at

# use data viz to explore the possibility that two different groups were tweeting from these devices
# extract hour (EST) it was tweeted
# compute proportion of tweets tweeted at each hour for each device

ds_theme_set()
campaign_tweets %>%
    mutate(hour = hour(with_tz(created_at, "EST"))) %>% # get hour (EST)
    count(source, hour) %>%
    group_by(source) %>%
    mutate(percent = n / sum(n)) %>% # create variable percent
    ungroup() %>%
    ggplot(aes(hour, percent, color = source)) + # create ggplot
      geom_line() +
      geom_point() +
      scale_y_continuous(labels = percent_format()) +
      labs(x = "Hour of day (EST)",
           y = "% of tweets", 
           color = "")

# results show a big difference in the early hours between 6-8AM
# which suggests that there are two different entities tweeting


# How do their tweets differ?

# tidytext converts free form text into a tidy table, which helps with data viz and applying statistical techniques
library(tidytext)

# the main function is unnest_tokens()
# a token refers to the units that we are considering as data points
# e.g. words, but can also be chars, ngras, sentences, lines, or regex patterns

# unnest_tokens() takes a vector of strings and extracts tokens into rows in a new table
# example <- data_frame(line = c(1, 2, 3, 4),
#                       text = c("Roses are red,", "Violets are blue,", "Sugar is sweet,", "And so are you."))
# example
# example %>% unnest_tokens(output = word, input = text)

# Let's look at tweet # 3008
i <- 3008
campaign_tweets$text[i]
campaign_tweets[i,] %>%
    unnest_tokens(word, text) %>%
    select(word)

# note that characters like # and @ have meaning in tweets
# so a token in twitter is not the same as a word in English
# instead of using the default token words, we use a regex

pattern <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
          # starts with @, # or neither
          # followed by any combination or letters or digits
  
campaign_tweets[i, ] %>%
    unnest_tokens(output = word, input = text, token = "regex", pattern = pattern) %>%
    select(word)

# we also want to remove links to pictures
campaign_tweets[i, ] %>%
    mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
                                                      # at least one of any combination of letters and digits or "&amp;"
    unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
    select(word)

# Now for all tweets
tweet_words <- campaign_tweets %>%
    mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
    unnest_tokens(word, text, token = "regex", pattern = pattern)

# What are the most commonly used words?
tweet_words %>%
    count(word) %>%
    arrange(desc(n)) %>%
    head(n = 10)

# the results are commonly used words already accounted for as stop_words in text mining
# which we can filter out
tweet_words <- campaign_tweets %>%
    mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
    unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
    filter(!word %in% stop_words$word)
  
tweet_words %>%
    count(word) %>%
    #top_n(10, n) %>% # Get the top 10 of n counts
    # mutate(word = reorder(word, n)) %>% # reorder words based on n
    arrange(desc(n)) %>% head(n = 10)


# After exploring more of the words, we notice that some are just numbers
# we can remove these using the regez "^\\d+$" (starts and ends with at least 1 digit)
# Some words also start with ' because they are quotes
# We replace the quotation mark if it is at the start of the word with an empty char

tweet_words <- campaign_tweets %>%
    mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
    unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
    filter(!word %in% stop_words$word & # filter out stop words
              !str_detect(word, "^\\d+$")) %>% # filter out numbers
    mutate(word = str_replace(word, "^'", "")) # replace/remove all quotation marks beginning a quote


# We now want to know if a word is more likely to come from an Android or an iPhone
# the odds ratio is a useful summary statistic

# for each device and word, y, compute the odds (the ratio between y and not y)
# a lot of words will have 0 so we use the 0.5 correction
android_iphone_ratio <- tweet_words %>%
    count(word, source) %>%
    spread(source, n, fill = 0) %>% # spreads source into columns whose values are the count n, while empty cells will be 0
    mutate(or = (Android + 0.5) / (sum(Android) - Android + 0.5) / # numerator is Android odds
                (iPhone + 0.5) / (sum(iPhone) - iPhone + 0.5)) # denominator is iPhone odds
          # or (odds ratio) is the ratio of Android's odds to iPhone's odds, i.e. how likely to be Android over iPhone

# a lot have really low frequencies so we filter only those that occur > 100 times
android_iphone_ratio %>% filter(Android + iPhone > 100) %>% arrange(desc(or))
# desc(or) shows tweets that are more likely the be from Android than iPhone, i.e. Trump's tweets

# What does "hyperbolic" mean?

# Sentiment Analysis
  # words can be associated to basic sentiments like anger, fear, joy and surprise

# the first step is to assign a sentiment to each word
# tidytext includes the object sentiments
# different lexicons give different sentiments
# bing divides words into positives and negatives, AFINN uses a score between -5 to 5,
# loughran and nrc use several different ones

# get_sentiments("bing")
# get_sentiments("afinn")
# get_sentiments("loughran") %>% count(sentiment)
# get_sentiments("nrc") %>% count(sentiment)


# We are interested in exploring the different sentiments of each tweet,
# so we will use the nrc lexicon
nrc <- get_sentiments("nrc") %>% select(word, sentiment)

# combine tweet_words and sentiments with inner_join()
tweet_words %>% inner_join(nrc, by = "word") %>%
    select(source, word, sentiment) %>% sample_n(10) # take 10 random


# Now we can quantiatively analyze Android vs iPhone tweets
# Each tweet will have several sentiments to it, which is somewhat complex
# so instead we will perform a simpler analysis of 
#  counting and comparing the frequencies of each sentiment in each device

sentiment_counts <- tweet_words %>%
    left_join(nrc, by = "word") %>% # combine tables
    count(source, sentiment) %>% # count frequencies of sentiment by source
    spread(source, n) %>% # convert source to columns
    mutate(sentiment = replace_na(sentiment, replace = "none")) # if word has no associated nrc sentiment

# More words were used on Android than iPhone
tweet_words %>% group_by(source) %>% summarize(n = n())

# Compute the odds of sentiment being from a device
# the proportion of words with sentiment versus words without
# then compute the odds ratio comparing Android vs iPhone
sentiment_counts %>% 
    mutate(Android = Android / (sum(Android) - Android), # odds of having a word with sentiment
            iPhone = iPhone / (sum(iPhone) - iPhone), # odds of having a word with sentiment
            or = Android/iPhone) %>% # odds of Android having sentiment over iPhone
  arrange(desc(or))


# Results: there are differences in the Android and iPhone tweets' sentiments
# the top 3 being digust, anger, negative

# Are they statistically significant?
# How does this compare to assinging sentiments at random?

# Compute the odds ratio and confidence interval for each sentiment
library(broom) # broom convert statistical objects into tidy tibbles

log_or <- sentiment_counts %>%
  mutate( log_or = log( (Android / (sum(Android) - Android)) / (iPhone / (sum(iPhone) - iPhone))),
          se = sqrt( 1/Android + 1/(sum(Android) - Android) + 1/iPhone + 1/(sum(iPhone) - iPhone)),
          conf.low = log_or - qnorm(0.975)*se,
          conf.high = log_or + qnorm(0.975)*se) %>%
  arrange(desc(log_or))

# the above code generates a confidence interval for each sentiment
# now we plot the sentiments to see if some are over-represented, i.e. not likely to happen by mere chance
log_or %>% 
    mutate(sentiment = reorder(sentiment, log_or)) %>%
    ggplot(aes(x = sentiment, ymin = conf.low, ymax = conf.high)) +
      geom_errorbar() +
      geom_point(aes(sentiment, log_or)) + 
      ylab("Log odds ratio for association between Android and iPhone sentiment") +
      coord_flip()

# the resulting graph shows that disgust, anger, negative sentiments
# are associated so much with Android (positive log_or) that it is hard
# to explain by chance
# Words with no sentiment are more associated with iPhone source,
# which suggests agreement with the claim about hyperbolic tweets

# To explore which specific words drive these differences, 
android_iphone_ratio %>% inner_join(nrc) %>% 
  filter(sentiment == "disgust" & (Android + iPhone > 10)) %>%
  arrange(desc(or))

# and graph it
android_iphone_ratio %>% inner_join(nrc, by = "word") %>%
  mutate(sentiment = factor(sentiment, levels = log_or$sentiment)) %>% # convert the sentiment column to a factor
  mutate(log_or = log(or)) %>%
  filter(Android + iPhone > 10 & abs(log_or) > 1) %>% # more than 10 tweets and ratio > 1
  mutate(word = reorder(word, log_or)) %>% # reorder words by log_or
  ggplot(aes(x = word, y = log_or, fill = log_or < 0)) + 
  facet_wrap(~sentiment, scales = "free_x", nrow = 2) + # create multiple graphs per sentiment
  geom_bar(stat = "identity", show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
