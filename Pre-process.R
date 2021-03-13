
reviews_df <- Womens.Clothing.E.Commerce.Reviews


reviews_df$text_full <- paste(reviews_df$Title,reviews_df$Review.Text,sep = " ")


#First stage of cleaning
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

# # Creating the full review from the cleaned+stemmedwords
# j<-1
# for (j in 1:nrow(reviews_df)) {
#  stemmed_Review<-  anti_join((reviews_df[j,] %>% unnest_tokens(word,Review, drop=FALSE,to_lower=TRUE) ),stop_words)
#   
#  stemmed_Review<-(wordStem(stemmed_Review[,"word"], language = "porter"))
# 
#  reviews_df[j,"Review"]<-paste((stemmed_Review),collapse = " ")
#   
# }
# 
# save(reviews_df , file = "C:/Users/24493ado/OneDrive - Erasmus University Rotterdam/Documents/Documents/college/text-analytics/data/yelp_review_full_csv/yelp_academic_dataset_review_restaurants_50kstemmed.Rdata")

```