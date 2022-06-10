# Extracting Data from a  PDF
library(tidyverse)
library(readr)
library(dslabs)
data("research_funding_rates")
research_funding_rates # the data actually comes from a paper published in PDF


# Download the data
library(pdftools)
temp_file <- tempfile() # returns a name for temporary files
url <- "http://www.pnas.org/content/suppl/2015/09/16/1510159112.DCSupplemental/pnas.201510159SI.pdf"
download.file(url, temp_file, mode="wb")
txt <- pdf_text(temp_file) # stores the contents of the PDF
file.remove(temp_file)

# txt is stored as a character vector with an element for each page
raw_data_research_funding_rates <- txt[2] # the data we want is in page 2

# Alternatively, the raw data is contained in dslabs
data("raw_data_research_funding_rates")

raw_data_research_funding_rates %>% head

# Create a list with the lines of text as elements
tab <- str_split(raw_data_research_funding_rates, "\n")
tab <- tab[[1]]

# Examining tab, we see that the col_names are in 3 and 4
the_names_1 <- tab[3]
the_names_2 <- tab[4]

# Column info is spread across 2 lines:
# in names_1, the pattern seems to be leading space, the name, followed by a comma
the_names_1 <- the_names_1 %>%
    str_trim() %>% # removes the spaces
    str_replace_all(",\\s.", "") %>% # replaces a comma, space, and wildcard char with empty
    str_split("\\s{2,}", simplify = TRUE) # split on spaces that occur "at least 2 times"

# in names_2, we want to remove the leading space, then split by spaces
the_names_2 <- the_names_2 %>%
    str_trim() %>%
    str_split("\\s+", simplify = TRUE) # split on one or more spaces

# Now we can combine the names to generate one name for each column
tmp_names <- str_c(rep(the_names_1, each = 3), the_names_2[-1], sep = "_") 
              # we replicate names_1 3x for each statistic "Applications", "Awards", "Success rates"
              # we don't include the first item in names_1, "Discipline"
              # followed by "_" names_2 values, to get "Applications_Total", etc.

the_names <- c(the_names_2[1], tmp_names) %>%
      str_to_lower() %>% # convert to lowercase
      str_replace_all("\\s", "_") # separate with "_"

# Extracting actual data contained in tab[6:14]
new_research_funding_rates <- tab[6:14] %>%
    str_trim() %>%
    str_split("\\s{2,}", simplify = TRUE) %>% # split on where there are at least 2 spaces
    data.frame(stringsAsFactors = FALSE) %>% # convert it to a data frame
    setNames(the_names) %>% # use the names we extracted as col names
    mutate_at(-1, parse_number)
