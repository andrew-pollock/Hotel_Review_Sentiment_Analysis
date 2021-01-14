
## Creating a dataset with only 20 hotels
library(dplyr)

hotel_reviews <- readxl::read_excel("data/raw/hotel_reviews.xls")

str(hotel_reviews)

# Lets start by cleaning the variable names
names(hotel_reviews) <- c("ID", "Hotel_Name", "Location", "Review_Date", "Review")


## How many hotels does each location have with at least 100 reviews?
hotel_reviews %>% group_by(Hotel_Name) %>% mutate(num_reviews = n()) %>% 
  filter(num_reviews >= 100) %>% group_by(Location) %>% select(Location, Hotel_Name) %>% 
  distinct() %>% summarise(num_hotels = n()) %>% arrange(-num_hotels)

## Patong and Kata Beach are the two locations with the most hotels with at least 100 reviews
## A small number of hotels from Patong have more than 100 reviews
## To ensure a fair comparison, I'll select 10 hotels from each location with exactly 100 reviews

filtered_hotels <- hotel_reviews %>% filter(Location %in% c("Patong", "Kata Beach")) %>% 
                      group_by(Hotel_Name) %>% mutate(num_reviews = n()) %>% 
                      filter(num_reviews == 100) %>% select(-num_reviews)


## I'll randomly select 10 hotels from each location
## Setting a seed ensures that this process is replicable
set.seed(101)

# Create a vector of the 20 hotels I've randomly selected
selected_hotels <- filtered_hotels %>% group_by(Location) %>% select(Location, Hotel_Name) %>% 
                      distinct() %>% sample_n(size = 10, replace = FALSE) %>% ungroup() %>% select(Hotel_Name)

# Select all reviews for those 20 hotels
selected_reviews <- filtered_hotels %>% filter(Hotel_Name %in% selected_hotels$Hotel_Name)

# Drop Review Date
selected_reviews <- selected_reviews %>% select(-Review_Date)

# Remove all commas to make .csv easier to read
selected_reviews <- selected_reviews %>% mutate(Review = gsub(",", " ", Review)) %>% mutate(Review = gsub("!", " ", Review)) %>% 
  mutate(Review = gsub("\\n", " ", Review)) %>% mutate(Review = gsub("\\.", " ", Review)) %>% mutate(Review = gsub("\\s+", " ", Review))

# Save these reviews down as a .csv
write.csv(selected_reviews, file = "data/processed/selected_reviews.csv", row.names = FALSE)
