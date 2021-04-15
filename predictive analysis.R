# Loading the package ----
library(dplyr)
library(ggplot2)
library(tidytext)
library(SnowballC)
library(syuzhet)
library(tidyr)
library(qdap)
#lecture 1
library(tidyverse)
library(tokenizers)
library(tm)
library(stringi)
library(ggrepel)
library(wordcloud)
library(readr)
#lecture 2
library(smacof)
library(ggfortify)
library(ggthemes)
library(stats)
library(quanteda)
library(magrittr)
#mds pcs
library(factoextra)
#lecture 3
library(syuzhet)
library(RDRPOSTagger)
library(data.table)
#lecture4
library(gtools)
library(combinat)
library(NMF)
library(tidyr)


# Import and clean the data set  ----
wd <- "C:/Users/mingh/Documents/Text-analytics"
reviews_df <- read_csv("Text-analytics/Womens Clothing E-Commerce Reviews.csv")
names(reviews_df)[5] <- "reviewtext"
reviews_df$Title[is.na(reviews_df$Title)] <- " "
reviews_df$reviewtext[is.na(reviews_df$reviewtext)] <- " "
reviews_df$text_full <- paste(reviews_df$Title, reviews_df$reviewtext, sep = " ")
reviews_df <- reviews_df[!(reviews_df$reviewtext == " "), ]

reviews_df$Rating_b <- "happy"
reviews_df[reviews_df$Rating < 4,]$Rating_b <- "unhappy"


# punctuation
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
  {gsub("\\.+","\\.", .)} %>%                      # Remove excess .
  {gsub("\'\"+"," ", .)}                        # Remove all '

reviews_df_raw <- reviews_df

reviews_df <- reviews_df_raw[,c(1,5,6,13)]
names(reviews_df)[1] <- "User_ID"
reviews_df[,1] <- reviews_df[,1]+1
names(reviews_df)[3] <- "rating"

# remove stop words ---- 
ignorelist = stop_words %>% filter(!word %in% c("no", "not", "never"))

for (j in 1:nrow(reviews_df)) {
  
  words <- reviews_df[j,] %>% 
    unnest_tokens(word, reviewtext) %>% 
    anti_join(ignorelist, by="word")
  
  stemmed <- wordStem(words$word, language = "porter")
  reviews_df[j, "stemmed_reviewtext_with_no"] <- paste(stemmed, collapse = " ")
  
  # Again, but with ignoring all stopwords
  nostopwords <- reviews_df[j,] %>% unnest_tokens(word, reviewtext) %>%
    anti_join( stop_words, by = "word")
  stemmed <- wordStem(nostopwords$word, language = "porter")
  
  # Add variables to data
  reviews_df[j, "stemmed_reviewtext"] <- paste(stemmed, collapse = " ")
  reviews_df[j, "reviewtext"] <- paste((nostopwords$word), collapse = " ")
  reviews_df[j, "Nr_of_words"]<- nrow(nostopwords)
}
print("done")





# Bigram set ----
all_bigrams <- reviews_df[,c("User_ID", "stemmed_reviewtext_with_no")] %>% 
  unnest_tokens(bigram, stemmed_reviewtext_with_no, token = "ngrams", n = 2 )
#This ignores sentences within a review.. could be improved.

head(all_bigrams)
all_bigrams <- all_bigrams %>%  dplyr::count(bigram, sort = TRUE)
all_bigrams[1:20,]

sel_bigrams <- all_bigrams %>% filter(n>200)
sel_bigrams


# Analyze bi-grams and look at bigrams where first word is no, never, or not

bigrams_sep <-  separate(all_bigrams, bigram, c("word1", "word2"), sep = " ")
bigrams_sep[1:20,]

# Look at bigrams where first word = ...
bigrams_sep %>%  filter(word1 == "no") %>% top_n(10, n)
bigrams_sep %>%  filter(word1 == "never") %>% top_n(10, n)
bigrams_sep %>%  filter(word1 == "not") %>% top_n(10, n)


# Select  infrequent and very frequent words to remove from review text ---- 

# Get word frequency after stemming
frequency  <- reviews_df %>% unnest_tokens(word, stemmed_reviewtext) %>% dplyr::count(word, sort=TRUE)

# Select very frequent or infrequent words
infrequent <- frequency %>% filter(n < 0.01*nrow(reviews_df))
frequent   <- frequency %>% filter(word %in% c("dress")) # you can extend this list with word you want to remove
toremove   <- full_join(frequent, infrequent, by = "word")       # combining these word lists
frequency
toremove

# Remove common words from stemmed reviewtext


for (j in 1:nrow(reviews_df)) 
{
  tmp <-  anti_join( (reviews_df[j,] %>% unnest_tokens(word, stemmed_reviewtext) ), toremove, by = "word") 
  
  reviews_df$stemmed_reviewtext[j] <- paste(tmp$word[-1], collapse = " ")
}

head(reviews_df)
reviews_df$stemmed_reviewtext <- 
save(reviews_df, file="Saved_reviews_df.Rda")

# Get document term matrix ---- 

# Get document term matrix uni-grams
reviews_df$User_ID <- as.character(reviews_df$User_ID) %>% as.factor() 
# The factor may have more values than are actually present. 
# These are removed here, as this causes an error in prcomp

review_dtm <- reviews_df %>% 
  unnest_tokens(word, stemmed_reviewtext) %>% 
  dplyr::count(User_ID, word, sort=TRUE) %>% 
  ungroup() %>%
  cast_dtm(User_ID,word,n)


#Get document term matrix for bi-grams 

review_dtm_bi <- reviews_df %>% 
  unnest_tokens(bigram, stemmed_reviewtext_with_no, token = "ngrams", n = 2) %>% 
  filter(bigram %in% sel_bigrams$bigram) %>%
  dplyr::count(User_ID, bigram, sort=TRUE)
review_dtm_bi$User_ID = as.character(review_dtm_bi$User_ID)

review_dtm_bi <- review_dtm_bi %>% 
  ungroup() %>%
  cast_dtm(User_ID, bigram, n)


# Run PCA on DTM ----

N_factors   <- 20
pca_results <- prcomp(review_dtm, scale = FALSE, rank. = N_factors)  #get the 20 most important factors
rawLoadings <- pca_results$rotation[, 1:N_factors] %*% diag(pca_results$sdev, N_factors, N_factors)
rotated     <- varimax(rawLoadings)

pca_results$rotation <- rotated$loadings
pca_results$x <- scale(pca_results$x[,1:N_factors]) %*% rotated$rotmat 

# Add the factors to the data frame
lastcol    <- ncol(reviews_df)
reviews_df <- data.frame(reviews_df, factor = pca_results$x)
colnames(reviews_df)[(lastcol+1):(lastcol+N_factors)] <- paste0("factor", 1:N_factors)

# Figure out which words load high on each factor
factor_labels <- NULL 
for (j in 1:N_factors) {
  aa<-abs(pca_results$rotation[,j]) %>% sort(decreasing = TRUE) 
  factor_labels <- rbind(factor_labels, paste0(names(aa[1:8])))
}
factor_labels


# Add indicators for 50 most common words ----

counts <- colSums(as.matrix(review_dtm)) %>% sort(decreasing=TRUE)

lastcol        <- ncol(reviews_df)
N_words_stored <- 50
word_labels    <- (names(counts)[1:N_words_stored])
reviews_df     <- data.frame(reviews_df, words = as.matrix(review_dtm[,word_labels]))
names(reviews_df)[(lastcol+1):(lastcol+N_words_stored)] <- word_labels


# Add inferred emotions ---- 

nrc_emotions  <- get_nrc_sentiment(reviews_df$reviewtext)

reviews_df <- data.frame(reviews_df, nrc_emotions)

# Add bigrams ----

review_dtm_bi <- as.matrix(review_dtm_bi)
reviews_df <- cbind(reviews_df, review_dtm_bi[match(rownames(reviews_df), rownames(review_dtm_bi)),])
reviews_df[is.na(reviews_df)] <- 0
