library(dplyr); library(tidyr)

fruit <- read.csv("appleorange.csv", header = FALSE, stringsAsFactors = FALSE,
                  skip = 4, nrows = 696)
names(fruit) <- c("country", "country_id", "product", "product_id", "amount", "year")
# remove Ê and units from amount column
fruit$amount <- as.numeric(gsub("Ê|, tonnes \\(\\)", "", fruit$amount))
fruit_clean <- fruit %>% 
    tbl_df() %>% 
    filter(!is.na(country_id)) %>%  # remove rows that do not contain country data
    select(-product_id, -year) %>% 
    spread(key = product, value = amount) 
names(fruit_clean) <- c("country", "id", "apples", "oranges")


stab <- read.csv("stability.csv", header = FALSE, stringsAsFactors = FALSE,
                 skip = 4, nrows = 956)
names(stab) <- c("country", "country_id", "index", "index_id", "rating", "year")
# remove units from rating column
stab$rating <- as.numeric(gsub(", .+ \\(\\)$", "", stab$rating))

stab_clean <- stab %>% 
    tbl_df() %>% 
    filter(!is.na(country_id)) %>%  # remove rows that do not contain country data
    select(-index_id, -year) %>% 
    spread(key = index, value = rating)
names(stab_clean) <- c("country", "id", "foodsupply", "stability", "raillines")

# merge (only countries that appear in both datasets)
joined <- inner_join(fruit_clean, stab_clean, by=c("id" = "id"))

# do the top ten most stable countries grow more apples or oranges (in total
# volume)?
joined %>% 
    arrange(desc(stability)) %>% 
    slice(1:10) %>% 
    summarise(apples = sum(apples), oranges = sum(oranges))

# what is the average amount of railroad in countries that 
# grow more apples than oranges, and vice versa?
joined %>%
    mutate(more_apples = if_else(apples > oranges, TRUE, FALSE)) %>% 
    group_by(more_apples) %>% 
    summarise(rail = mean(raillines, na.rm = TRUE))
