# install.packages("rtweet")
library(rtweet)
library(dplyr)
library(stringr)
library(ggplot2)
library(tm)
library(tidyr)

consumer_key <- "NkBpBPbhIZc9YdHnw88kTWLmx"
consumer_secret <- "VuNqU4gXeofa4MbBBX2CkZeuNwlNCTOrhIeAmr1L82b2e2EEpj"
access_token <- "1040465492-gTYOGPyjoeCNNyBnxifJEoHt93rmsf1NggMcyjA"
access_secret <- "z4zbHD9vF7mQ4wfgGom3IRqKru9GFsJSdza4sidDhTQtC"
name_of_app <- "Test_ArjunM"

twitter_token <- create_token(
  app = name_of_app,
  consumer_key = consumer_key,
  consumer_secret = consumer_secret,
  access_token = access_token,
  access_secret = access_secret)

# # hashtags <- c("#sanfrancisco", "#SanFrancisco", "#sfcrime" ,"#crime", 
#               "#homeless", "#lootings", "#shoplifting", "#unsafe", 
#               "police", "#SF", "#sfpd", "tenderloin", "#homelessSF")

rt <- search_tweets("#looting", n = 1000, include_rts = FALSE)
rt1 <- search_tweets("#sfcrime", n = 1000, include_rts = FALSE)
rt2 <- search_tweets("#SanFrancisco", n = 1000, include_rts = FALSE)
rt4 <- search_tweets("#homeless", n = 1000, include_rts = FALSE)


twitter_data <- bind_rows(mutate(rt, subject="Looting"),
                        mutate(rt1, subject="Crime"),
                        mutate(rt4, subject ="Homeless"),
                        mutate(rt2, subject="San Francisco"))

# let's look at the data
names(twitter_data)
View(twitter_data)

# subseting the data
tweet_data <- twitter_data %>% 
  select("created_at", "screen_name", "text", "source", 
         "favorite_count", "retweet_count", "hashtags", 
         "place_full_name", "subject") 

#######################
# Data Massaging
#######################
library(qdapRegex)

# clean_tweets <- data.frame()
# tweet_data$text = gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", tweet_data$text)

tweet_data$text = gsub("@[a-z,A-z,0-9]*", "", tweet_data$text) # removing usernames from the tweets
tweet_data$text <- rm_twitter_url(tweet_data$text) # removing URLs from the tweets

#########################
# Understanding the data
#########################

# plotting the frequency of tweets
ts_plot(tweet_data, "hours") +
  labs(x = NULL, y = NULL,
       title = "Frequency of tweets with a #Crime & #SanFrancisco related hashtags",
       subtitle = paste0(format(min(tweet_data$created_at), "%d %B %Y"), " to ", format(max(tweet_data$created_at),"%d %B %Y")),
       caption = "Data collected from Twitter's REST API via rtweet") +
  theme_minimal()

# plotting the most active tweeter
tweet_data %>% 
  count(screen_name, sort = TRUE) %>%
  top_n(10) %>%
  mutate(screen_name = paste0("@", screen_name)) %>% 
  mutate(screen_name=reorder(screen_name,n)) %>%
  ggplot(aes(screen_name,n)) + geom_col() + 
  labs(x = NULL, y = NULL,
       title = "Count of most active tweeter",
       caption = "Data collected from Twitter's REST API via rtweet") + 
  theme_minimal() +
  coord_flip()

# most common hashtags
tweet_data %>% 
  unnest_tokens(hashtag, text, "tweets", to_lower = FALSE) %>%
  filter(str_detect(hashtag, "^#"), hashtag != "#SanFrancisco", hashtag != "#sanfrancisco", hashtag != "#crime") %>%
  count(hashtag, sort = TRUE) %>%
  top_n(100)

# most common crime & San Francisco related hashtags
tweet_data %>% 
  unnest_tokens(hashtag, text, "tweets", to_lower = FALSE) %>%
  filter(str_detect(hashtag, "^#"), hashtag != "#sanfrancisco", 
         hashtag !="#SanFrancisco", subject == c("Crime", "Looting", "Homeless")) %>%
  count(hashtag, sort = TRUE) %>%
  top_n(20) %>% 
  mutate(hashtag=reorder(hashtag,n)) %>%
  ggplot(aes(hashtag,n)) + geom_col() + 
  labs(x = NULL, y = NULL,
       title = "Most common crime & San Francisco related hashtags",
       caption = "Data collected from Twitter's REST API via rtweet") + 
  theme_minimal() +
  coord_flip()

#########################
# frequency distribution 
#########################


tidy_tweet <- tweet_data %>% unnest_tokens(word, text) %>% anti_join(stop_words) 

# plotting tokenised frequencies
tidy_tweet %>% 
  group_by(subject) %>% 
  count(word, sort = T) %>% 
  filter(n>50) %>%
  mutate(word=reorder(word,n)) %>%
  ggplot(aes(word,n,color = subject)) + geom_col() + xlab(NULL) + coord_flip() + facet_wrap(~subject, ncol=4)

# cleaning the data - removing weather and bots
tidy_tweet <- tidy_tweet %>% filter(screen_name != "sf_amour_sf", 
                                    screen_name != "wx_sanfrancisco", 
                                    word != "amp", word != "fuck",
                                    word != "fucking")

# plotting correlograms
library(tidyr)
frequency <- tidy_tweet %>% 
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(subject, word) %>%
  group_by(subject) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(subject, proportion) %>%
  gather(subject, proportion, `Looting`, `Crime`, `Homeless`)

# let's plot the correlograms:
library(scales)
ggplot(frequency, aes(x=proportion, y=`San Francisco`, 
                      color = abs(`San Francisco`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~subject, ncol=3)+
  theme(legend.position = "none")+
  labs(y= "San Francisco", x=NULL)

cor.test(data=frequency[frequency$subject == "Crime",],
         ~proportion + `San Francisco`) # very low correlation observed
cor.test(data=frequency[frequency$subject == "Looting",],
         ~proportion + `San Francisco`) # low-moderate correlation observed 
cor.test(data=frequency[frequency$subject == "Homeless",],
         ~proportion + `San Francisco`) # low correlation observed 

##################################################
# finding the pairwise correlation between words
##################################################

library(tidytext)
new_tweet_data <- twitter_data %>% mutate(tweet_id = row_number()) 

# cleaning the data
new_tweet_data$text = gsub("@[a-z,A-z,0-9]*", "", tweet_data$text) # removing usernames from the tweets
new_tweet_data$text <- rm_twitter_url(tweet_data$text) # removing URLs from the tweets

# tokenising
new_tweet_tidy <- new_tweet_data %>% unnest_tokens(word, text) %>% anti_join(stop_words)
new_tweet_tidy <- new_tweet_tidy %>% filter(screen_name != "sf_amour_sf", 
                                    screen_name != "wx_sanfrancisco", 
                                    word != "amp", word != "fuck", 
                                    word != "fucking")


tweet_cors <- new_tweet_tidy %>%
  group_by(word) %>%
  filter(n() >= 5) %>%
  pairwise_cor(word, tweet_id, sort=TRUE)

tweet_cors %>%
  filter(item1 %in% c("looting", "pandemic", "crime", "homeless")) %>%
  group_by(item1) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity")+
  facet_wrap(~item1, scales = "free")+
  coord_flip()

# creating a correlation network 
library(ggraph)
library(igraph)

tweet_cors %>%
  filter(item1 %in% c("looting", "pandemic", "crime", "homeless")) %>%
  filter(correlation >.2) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr")+
  geom_edge_link(aes(edge_alpha = correlation), show.legend=F)+
  geom_node_point(color = "lightgreen", size=6)+
  geom_node_text(aes(label=name), repel=T)+
  theme_void()












