# Web Scraping

library(rvest)
url <- "https://en.wikipedia.org/wiki/Murder_in_the_United_States_by_state"
h <- read_html(url)

# rvest can extract nodes from an HTML object
# html_nodes() for all; html_node() for the first

tab <- h %>% html_nodes("table")
# we only want the second table
tab <- tab[[2]]

# rvest can convert HTML tables to data frames
tab <- tab %>% html_table()

# change the names to make them more appropriate
tab <- tab %>% setNames(c("state", "population", "total", "murders", "gun_murders", 
                          "gun_ownership", "total_rate", "murder_rate", "gun_murder_rate"))

# CSS Selectors
# use SelectorGadget (browser extension) to extract the necessary selectors in web pages

# example, guacamole recipe from FoodNetwork.com
h <- read_html("http://www.foodnetwork.com/recipes/alton-brown/guacamole-recipe-1940609")

# using SelectorGadget, we found the selectors for recipe name, prep time, ingredients
recipe <- h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
prep_time <- h %>% html_node(".m-RecipeInfo__a-Description--Total") %>% html_text()
ingredients <- h %>% html_nodes(".o-Ingredients__a-Ingredient") %>% html_text(trim = TRUE)
ingredients <- ingredients[-1]

# combine
guacamole <- list(recipe, prep_time, ingredients)


# All pages from the website will follow the same format, so you can make a function
get_recipe <- function(url){
    h <- read_html(url)
    recipe <- h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
    prep_time <- h %>% html_node(".m-RecipeInfo__a-Description--Total") %>% html_text()
    ingredients <- h %>% html_nodes(".o-Ingredients__a-Ingredient") %>% html_text(trim = TRUE)
    ingredients <- ingredients[-1]
    list(recipe, prep_time, ingredients)
}

# and use it on any other page
get_recipe("http://www.foodnetwork.com/recipes/food-network-kitchen/pancakes-recipe-1913844")


# rvest can also perform queries with html_form(), set_values(), submit_form()




# Assessment

url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)


# extract the first node
nodes <- h %>% html_nodes("table")

# html_text() and html_table() are used to read

# Question 1
# convert the first four tables in nodes to data frames
html_table(nodes[[1]])
html_table(nodes[[2]])
html_table(nodes[[3]])
html_table(nodes[[4]])

# Question 2
# last 3 components of nodes
html_table(nodes[[19]])
html_table(nodes[[20]])
html_table(nodes[[21]])


# Question 3
tab_1 <- html_table(nodes[[10]], header = TRUE)
tab_2 <- html_table(nodes[[19]], header = TRUE)
tab_1 <- tab_1[,2:4]

nrow(full_join(tab_1, tab_2, by = "Team"))


# Question 4 and 5 Brexit Opinion Polling
library(rvest)
library(tidyverse)
url <-"https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
h <- read_html(url)
tab <- h %>% html_nodes("table")

length(tab)

# find the first table with 9 rows whose first column is "Date(s) conducted"
find_table <- function(tables){
  for (i in c(1:length(tables))) {
    tab <- html_table(tables[[i]], fill = TRUE)
    ifelse(ncol(tab) == 9 & names(tab)[1] == "Date(s) conducted", break, FALSE)
  }
  i
}





