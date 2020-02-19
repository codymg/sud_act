library(rvest)
library(stringr)
library(jsonlite)
library(tidyverse)
library(purrr)
library(tidytext)
library(textTinyR)
library(stopwords)
library(SnowballC)
library(udpipe)
library(utf8)
library(corpus)
library(ggwordcloud)
library(plm)

#loading scraped cases

sample_df <- readRDS(url())

#cleaning case text

sample_df$clean_doc <- sample_df$doc[1:8] %>%
  gsub("[[:punct:][:blank:]]+", " ", .) %>% #removing punctuation
  gsub("-|--|\\s-\\s|-", "", .) %>%
  gsub("[[:digit:]]+", "", .) %>% #remove numbers
  gsub("\u2116", "", .) %>% #remove number signs
  gsub("\\s+", " ", .) %>% #reduce to only single spaces
  gsub("\\sfunction.*", "", .) %>% #remove html code
  str_to_lower(.) %>% # converting to lower case
  str_replace_all("\n","") %>% #removing all lines
  #gsub("", "", .) remove any specific words
  
# gsub("\\s[\u0410-\u044f]{1,2}\\s", " ", .) #if used 3 times it removes all prepositions/single letter characters that dont belong while maintaining word seperation
  
#tokenizing 
  
token_df <- data.frame(txt = sample_df$clean_doc, stringsAsFactors = FALSE)

stopwrds <- data.frame(words = stopwords("Russian", source = "snowball"))

#head(stopwords::stopwords("russian", source = "snowball"), 30) provides preview of stop_words being used

count_test <- token_df %>%
  tidytext::unnest_tokens(word, txt) %>% 
  anti_join(stopwrds, by = c("word" = "words")) %>% #removing stop words based on snowball set
  count(word, sort = TRUE) %>% 
  #filter(!word %in% c()) %>% #filtering out non-meaningful words
  arrange(desc(n)) %>%
  head(25) #produces in UTF-8

wrds <- count_test %>% as.data.frame(.) %>%
  print.corpus_frame(.) #prints in russian

#steming

stems <- token_df %>%  
  tidytext::unnest_tokens(word, txt) %>%
  anti_join(stopwrds, by = c("word" = "words")) %>% #removing stop words based on snowball set
  mutate(stem = wordStem(word, language = "russian")) %>% #getting only stems
  count(stem, sort = TRUE) %>%
  # filter(!stem %in% c()) %>% #filtering out non-meaningful words
  head(25) #produces in UTF-8

stms <- as.data.frame(stems) %>% 
  print.corpus_frame(.) #prints in russian

#lemmatizing in Russian

dl <- udpipe_download_model(language = "russian") #downloading udpipes Russian language lemmatizing model

udmodel_russian <- udpipe_load_model(file = dl$file_model) #reading udpipes Russian language lemmatizing model

txt <- token_df %>%  #tokenizing words to remove stop words
  tidytext::unnest_tokens(word, txt) %>%
  anti_join(stopwrds, by = c("word" = "words")) %>%
  as.character() 

lemmas <-  udpipe_annotate(udmodel_russian, x = txt) #creating lemmas (annotating) from the clean text stored in token_df

lemmas <- as.data.frame(lemmas) #storing lemmas from udpipe as df

lemmas$lemma <- gsub("c\\(|\\'|,|\"", "", lemmas$lemma) #removing strange lemmas produced by udpipe

lms <- lemmas %>% 
  select(lemma) %>%
  count(lemma, sort = TRUE) %>%
  filter(!lemma %in% c("c(",",","", "<U+0433>", "<U+0433><U+043E><U+0434><U+0430>", "<U+0441><U+0442>", "<U+0433><U+043E><U+0434>", "<U+043C><U+043C>", "<U+0434><U+0434>", "<U+0444><U+0438><U+043E>", "<U+0441><U+0443><U+0434><U+0430>", "<U+0442><U+0430><U+043A><U+0436><U+0435>", "<U+043F>", "<U+0438><U+0432><U+0441>", "<U+0434><U+0435><U+043B><U+0430>")) %>% #filtering out unwanted lemmas
  head(25) #prints in confusing unicode

lms$lemma <- gsub("><", " ", lms$lemma) #formatting to get to correct unicode
lms$lemma <- gsub("<|>|\\+", "", lms$lemma)
lms$lemma <- gsub("\\s", "\\\\", lms$lemma)
lms$lemma <- paste0("\\", lms$lemma)
lms$lemma <- stringi::stri_unescape_unicode(gsub("\\U","\\u", lms$lemma, fixed=TRUE)) #prints correct unicode

lms %>% as.data.frame(.) %>%
  print.corpus_frame(.) #prints lemmatization counts


freqdf <- cbind(wrds,stms,lms) %>% as.data.frame(.) %>% print.corpus_frame(.)

##############################
##############################
#Clouds
##############################
##############################


courts <- as.data.frame(sample_df$court)

colnames(courts) <- "court"

courts$court <- gsub("(.\\)*)\\s-.*", "\\1", courts$court)

show_case_df <- cbind(token_df, test$case, courts$court) %>% as.data.frame(.) %>% head(1) %>% print.corpus_frame(.)

cloud_stem <- ggplot(as.data.frame(token_df) %>% #stems
                       tidytext::unnest_tokens(word, txt) %>%
                       anti_join(stopwrds, by = c("word" = "words")) %>%
                       mutate(stem = wordStem(word, language = "russian")) %>%
                       group_by(stem) %>%
                       summarize(count = n()) %>%
                       filter(nchar(stem)>3, str_detect(stem,"^[\u0410-\u04FF]")) %>%
                       top_n(25, count) %>%
                       mutate(count = count/sum(count)) %>%
                       arrange(desc(count)) %>%
                       mutate(angle = 45 * sample(-2:2, n(), replace = TRUE, prob = c(1, 1, 4, 1, 1))), 
                     aes(label = stem, size=count, color = count, angle = angle)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 11.5) +
  theme_minimal() +
  scale_color_gradient(low = "darkred", high = "red") +
  labs(title = "Russian Justice", subtitle = "Popular Stem Usage in Russian Court Documents -- March, 2016")

cloud_stem

cloud_lemm <- ggplot(as.data.frame(lms) %>% #stems
                       group_by(lemma) %>%
                       summarize(count = n()) %>%
                       filter(nchar(lemma)>3, str_detect(lemma,"^[\u0410-\u04FF]")) %>%
                       top_n(25, count) %>%
                       mutate(count = count/sum(count)) %>%
                       arrange(desc(count)) %>%
                       mutate(angle = 45 * sample(-2:2, n(), replace = TRUE, prob = c(1, 1, 4, 1, 1))), 
                     aes(label = lemma, size = count, color = count, angle = angle)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 11) +
  theme_minimal() +
  scale_color_gradient(low = "darkred", high = "red") +
  labs(title = "Russian Justice", subtitle = "Popular Lemma Usage in Russian Court Documents -- March, 2016")

cloud_lemm

##############################
##############################
#TF-IDF
##############################
##############################

token_df$id <- paste(sample_df$test_judge, sample_df$id, sep = "_")

tf_idf <- token_df %>%
  group_by(id, judge) %>%
  tidytext::unnest_tokens(word, txt) %>%
  anti_join(stopwrds, by = c("word" = "words")) %>% #removing stop words based on snowball set
  mutate(stem = wordStem(word, language = "russian")) %>% #getting only stems
  count(stem, sort = TRUE) %>%
  filter(nchar(stem) > 2) %>%
  #filter(!stem %in% c("????", "??", "??????", "????", "????", "????????", "??????", "????????", "??????", "??", "??????????", "??????????")) %>%
  #left_join(sentiments %>% filter(lexicon == "nrc"), by = c("lemmas" = "word")) needs russian sentiment lexicon
  group_by(id, judge, stem) %>%
  summarise(n = n()) %>%
  bind_tf_idf(stem, id, n) %>%
  arrange(desc(judge)) %>%
  arrange(desc(n))

tf_idf %>% head(20) %>% print.corpus_frame()

tf_idf2 <- tf_idf %>%  
  arrange(id, desc(tf)) %>%
  group_by(id) %>%
  top_n(10, tf + runif(n(), 0, 0.01)) %>%
  ungroup() %>%
  arrange(id, tf) %>%
  mutate(.r = row_number()) 

tf_idf2 %>% head(20) %>% print.corpus_frame()

tf_idf_plot <- ggplot(tf_idf2[tf_idf2$judge == "",], aes(x =.r, y = tf_idf)) +
  facet_wrap(~ id,
             scales = "free_y") + 
  geom_col() + 
  coord_flip() + xlab("") +
  scale_x_continuous(
    breaks = tf_idf2$.r, 
    labels = tf_idf2$stem) + 
  theme_bw()


pdf("~/tf_idf.pdf", height=5, width=5, encoding = "CP1251.enc")

ggplot(tf_idf2[tf_idf2$judge == "",], aes(x =.r, y = tf_idf)) +
  facet_wrap(~ id,
             scales = "free_y") + 
  geom_col() + 
  coord_flip() + xlab("") +
  scale_x_continuous(
    breaks = tf_idf2$.r, 
    labels = tf_idf2$stem) +
  theme_bw()

dev.off()


##############################
##############################
#simple statistical models
##############################
##############################

lmmodel <- lm(test_decision_state_binary ~ test_court_distance, data = sample_df)

lmmodel <- lmtest::coeftest(lmmodel, vcov=vcovHC(lmmodel, type="HC2", cluster = "group")) 

modellm <- data.frame(tidy(lmmodel)[2,], variable = 1, lbl = "Distance") 

logmodel <- glm(test_decision_state_binary ~ test_court_distance, family = binomial(link = "logit"), data = sample_df)

logmodel <- lmtest::coeftest(logmodel, vcov=vcovHC(logmodel, type="HC2", cluster = "group")) 

modellog <- data.frame(tidy(logmodel)[2,], variable = 2, lbl = "Distance") 

probmodel <- glm(test_decision_state_binary ~ test_court_distance, family = binomial(link = "probit"), data = sample_df)

probmodel <- lmtest::coeftest(probmodel, vcov=vcovHC(probmodel, type="HC2", cluster = "group")) 

modelprob <- data.frame(tidy(probmodel)[2,], variable = 3, lbl = "Distance") 


plot_data <- rbind.data.frame(modellm, modellog, modelprob, stringsAsFactors = FALSE)

brks <- c(1,2,3)

lbls <- c("LM", "Logit", "Probit")


pdf("~/coeff.pdf", height=6, width=6)

ggplot(data = plot_data, aes(x = variable, y = estimate)) +
  geom_point() +
  geom_linerange(aes(ymin = estimate - 1.65 * std.error, ymax = estimate + 1.65 * std.error),
                 lwd = 1.5) + 
  geom_errorbar(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error),
                width = .1) + 
  geom_hline(yintercept = 0, linetype = 2) +
  labs(subtitle = "Effect of Distance from the Executive on Court Decisions in Russia") + 
  scale_x_continuous(expression(Model~Type), 
                     breaks = brks, labels = lbls) + 
  scale_y_continuous(expression(Likelihood~of~Decisions~For~the~State)) +
  coord_cartesian(xlim = c(1,3)) + 
  theme_bw()

dev.off()
