
# Sentiment Analysis
library(dplyr)
library(sentimentr) ## For calculating review sentiment
library(wordcloud)  ## Plotting most freq words as clouds
library(magrittr)   ## For pipe operator
library(tidytext)   ## For text mining
library(stringr)    ## For cleaning up the text strings
library(tm)         ## For removing stop words (still needed??)
library(textstem)   ## For lemmatizing words
library(ggplot2)    ## General plotting
library(topicmodels) ## For topic modelling
#library(forcats)    ## For refactoring variables

# Load in the dataset
hotel_data <-read.csv("data/processed/selected_reviews.csv")


### Data Cleaning ###

## Check a selection of the Review data
hotel_data$Review[1:12]

# There are some weird text strings between < and >
nrow(hotel_data[str_detect(hotel_data$Review, "<.+>"),])

# I'll replace these with a space
hotel_data <- hotel_data %>% mutate(Review = gsub("<.+>", " ", Review))

# Remove question marks
hotel_data <- hotel_data %>% mutate(Review = gsub("\\?", " ", Review))

## Now I can remove the stop words from the reviews
hotel_data$Review <- tm::removeWords(hotel_data$Review, words = stopwords(kind = "en"))

# Lemmatization 
hotel_data$Review <- textstem::lemmatize_strings(hotel_data$Review)




#### Calculating Sentiment ####

## Calculate the sentiment of each review
sentiments <- hotel_data %$% sentiment(Review) %>% select(sentiment)

## Append those sentiments to the dataset & calculate hotel avg
hotel_data <- cbind(hotel_data, sentiments) %>% group_by(Hotel_Name) %>% 
    mutate(avg_sentiment = mean(sentiment)) %>% arrange(-avg_sentiment) %>% ungroup()


### Plotting sentiment ###

## Plot distribution of review sentiments by hotel (faceted by location)
hotel_data %>% ggplot(aes(x=reorder(Hotel_Name,sentiment), y=sentiment)) +
  geom_boxplot() + 
  stat_summary(fun=mean, geom="point", shape=16, size=2, color="red")+
  facet_grid(Location~.,scales = "free") + theme_bw() + coord_flip() +
  labs(x = "Hotels", y = "Review Sentiment", title = "Distribution of Review Sentiment by Hotel") +
  theme(plot.title = element_text(hjust = 0.5))




### Get my data into a tidy format
tidy_data <- hotel_data %>% select(Location, Review) %>%
  group_by(Location) %>%
  unnest_tokens(word, Review) %>% ungroup()

# Top 15 Most Common Positive and Negative words
tidy_data %>%
  inner_join(get_sentiments("bing")) %>% group_by(word, sentiment) %>%
  summarise(num_occurances = n()) %>% ungroup() %>% arrange(-num_occurances) %>% 
  group_by(sentiment) %>% slice(1:15) %>% mutate(word = reorder(word, num_occurances)) %>%
  ggplot(aes(num_occurances, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free") + theme_bw() +
  labs(x="Number of Occurances in Reviews", y = "Word", title = "Most Common Positive and Negative Words") +
  theme(plot.title = element_text(hjust = 0.5))



# Plot word clouds of the most common words
positive_counts <- tidy_data %>% inner_join(get_sentiments("bing"), by = "word") %>% 
  filter(sentiment == "positive") %>% count(word) %>% filter(word != "good")
negative_counts <- tidy_data %>% inner_join(get_sentiments("bing"), by = "word") %>% 
  filter(sentiment == "negative") %>% count(word)

# Plot the positive words in a cloud (after removing "good")
wordcloud(words = positive_counts$word, freq = positive_counts$n, max.words = 50, colors = "blue")

# Plot the negative words
wordcloud(words = negative_counts$word, freq = negative_counts$n, max.words = 50, colors = "red")




#### Using tf_idf to find the most distinctive words ####

### What are the most distinctive words used to describe each Hotel?
hotel_tf_idf <- hotel_data %>% select(Location, Hotel_Name, Review) %>% group_by(Location, Hotel_Name) %>%
  unnest_tokens(word, Review) %>% count(word) %>% ungroup() %>%
  bind_tf_idf(word, Hotel_Name, n) %>%
  arrange(Location, desc(tf_idf))

# Plot most distinctive word by Hotel
hotel_tf_idf %>%
  group_by(Hotel_Name) %>%
  slice_max(tf_idf, n = 5) %>%
  ggplot(aes(tf_idf, forcats::fct_reorder(word, tf_idf), fill = Location)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~forcats::fct_reorder(Hotel_Name, Location), scales = "free") +
  labs(x = "tf-idf", y = NULL) + theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(x = "Uniqueness of word to a specific hotel", title = "Most Distinctive Words by Hotel")


#### Topic Modelling ####

# Convert my data to a document term matrix
# I filter out food because it's too commonly occurring
review_data <- hotel_data %>% ungroup() %>% select(ID, Review) %>%
  unnest_tokens(word, Review) %>% anti_join(stop_words, by = "word") %>% filter(word != "food") %>%
  group_by(ID) %>% count(word, sort = TRUE) %>% cast_dtm(ID, word, n)

# Run topic modelling to get 4 topics
topic_model <- LDA(review_data, k = 4, control = list(seed = 101))

# Exploring each topic's key words
tidy(topic_model, matrix = "beta") %>% mutate(topic = paste0("Topic ", topic)) %>% 
  group_by(topic) %>%  top_n(10, beta) %>% ungroup() %>%
  arrange(topic, -beta) %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered() + theme_bw()



## Calculate the topic of each review and append to hotel_data
hotel_data <- tidy(topic_model, matrix = "gamma") %>% group_by(document) %>% mutate(highest_prob = max(gamma)) %>% 
  filter(gamma==highest_prob) %>% select(document, topic) %>% 
  right_join(hotel_data, by = c("document" = "ID")) %>% ungroup()
  

# What percentage of each hotel's reviews were in each topic?
hotel_data %>% group_by(Hotel_Name, topic, Location) %>% summarise(num_reviews = n()) %>%
  ggplot(aes(fill=factor(topic), x=Hotel_Name, y=num_reviews)) +
  geom_bar(position="stack", stat="identity") +
  facet_wrap(~Location, scales = "free") + 
  theme_bw() + coord_flip() +
  labs(x = element_blank(), y = "Proportion of Reviews", 
       title = "Distribution of Review Topics by Hotel", fill = "Topic") +
  theme(plot.title = element_text(hjust = 0.5))


## Average Topic Sentiment
hotel_data %>% group_by(topic) %>% summarise(Average_Sentiment = mean(sentiment)) 

