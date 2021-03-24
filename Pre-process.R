library("wordcloud")
library("quanteda")
library(dplyr)


reviews_df <- Womens_Clothing_E_Commerce_Reviews


reviews_df$text_full <- paste(reviews_df$Title,reviews_df$`Review Text`,sep = " ")


#First stage of cleaning ----
reviews_df$Review <- as.character(reviews_df$text_full)  %>% 
  tolower() %>% 
  {gsub(":( |-|o)*\\("," SADSMILE ", .)} %>%       # Find :( or :-( or : ( or :o(
  {gsub(":( |-|o)*\\)"," HAPPYSMILE ", .)} %>%     # Find :) or :-) or : ) or :o)
  {gsub("(\"| |\\$)-+\\.-+"," NUMBER", .)} %>%     # Find numbers
  {gsub("([0-9]+:)*[0-9]+ *am"," TIME_AM", .)} %>%         # Find time AM
  {gsub("([0-9]+:)*[0-9]+ *pm"," TIME_PM", .)} %>%         # Find time PM
  {gsub("-+:-+","TIME", .)} %>%                    # Find general time
  {gsub("\\$ ?[0-9]*[\\.,]*[0-9]+"," DOLLARVALUE ", .)} %>%           # Find Dollar values
  {gsub("[0-9]*[\\.,]*[0-9]+"," NUMBER ", .)} %>%           # Find remaining numbers
  {gsub("-"," ", .)} %>%                           # Remove all -
  {gsub("&"," and ", .)} %>%                       # Find general time
  {gsub("\"+"," ", .)} %>%                         # Remove all "
  {gsub("\\|+"," ", .)} %>%                        # Remove all |
  {gsub("_+"," ", .)} %>%                          # Remove all _
  {gsub(";+"," ", .)} %>%                          # Remove excess ;
  {gsub(" +"," ", .)} %>%                          # Remove excess spaces
  {gsub("\\.+","\\.", .)}                          # Remove excess .

reviews_df$Review

#cut text into words by splitting on spaces and punctuation
review_words <- reviews_df %>% unnest_tokens(word,Review,to_lower=TRUE) 
print("number of words")
nrow(review_words)

#Count the number of times each word occurs
counts <- review_words %>%count(word, sort=TRUE) # sort = TRUE for sorting in descending order of n. 
print("number of unique words after stemming and without stop words")
nrow(counts)


# Visualization raw ---- 
# frequency (raw)
counts %>% 
  mutate(word = reorder(word,n)) %>% 
  top_n(20, word) %>%
  ggplot(aes(word,n)) +  
  geom_col() + 
  labs(x = NULL, y = "Number of occurences") + 
  coord_flip() + 
  theme(text = element_text(size = 17)) + 
  ggtitle("Word Frequency Histogram")


wordcount_all <- reviews_df %>% unnest_tokens(word,Review) %>% count(word, sort=TRUE)

set.seed(1223)
wordcloud(words = wordcount_all$word, freq = wordcount_all$n, min.freq = 1000,
          max.words=70, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


# Stem ----

data(stop_words)
review_words_nostop <- review_words %>% 
  anti_join(stop_words)
counts <- review_words_nostop %>%
  count(word, sort=TRUE)

print("number of words without stop words")
sum(counts$n)
print("number of unique words")
nrow(counts)

# Creating the full review from the cleaned+stemmedwords
j<-1
for (j in 1:nrow(reviews_df)) {
  stemmed_description<-  anti_join((reviews_df[j,] %>% unnest_tokens(word,Review, drop=FALSE,to_lower=TRUE) ),stop_words)
  
  stemmed_description<-(wordStem(stemmed_description[,"word"], language = "porter"))
  
  reviews_df[j,"Description"]<-paste((stemmed_description),collapse = " ")
  
}


