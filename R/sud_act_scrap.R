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
library(tikzDevice)
library(xtable)

oldloc <- Sys.getlocale("LC_CTYPE") #encoding may need to be changed to account for Russian, we are preserving original ("English_United States.1252")


base_url <- "http://sudact.ru/regular/doc_ajax/?page="

case_links <- paste0(base_url, 1:3, sep = "")

case_links_full <- paste0(case_links,"&regular-txt=%D0%B5%D0%B2%D1%80%D0%BE%D0%BF%D0%B5%D0%B9%D1%81%D0%BA%D0%B8%D0%B9+%D1%81%D1%83%D0%B4+%D0%BF%D0%BE+%D0%BF%D1%80%D0%B0%D0%B2%D0%B0%D0%BC+%D1%87%D0%B5%D0%BB%D0%BE%D0%B2%D0%B5%D0%BA%D0%B0&regular-case_doc=&regular-lawchunkinfo=&regular-doc_type=&regular-date_from=01.03.2016&regular-date_to=09.03.2016&regular-workflow_stage=10&regular-area=&regular-court=&regular-judge=&_=1552170315479")

case_links_full[1]

json_df <- lapply(case_links_full, read_json) #reading in json and must prove one is not a robot

content <- lapply(json_df, "[[",1) #extracting content from json to read in

test <- content %>%
  map(read_html) %>%
  map(function (node) {data.frame(case = node %>% 
                                    html_nodes(".results a") %>% 
                                    html_text(),
                                  court = node %>% 
                                    html_nodes(".b-justice") %>%
                                    html_text(trim = TRUE),
                                  id = node %>% 
                                    html_nodes(".numb") %>%
                                    html_text(trim = TRUE),
                                  website_url = node %>% 
                                    html_nodes(".results a") %>%
                                    html_attr("href"), 
                                  stringsAsFactors=FALSE)}) %>%
  bind_rows() %>%
  mutate(website_url = paste0("http://www.sudact.ru", website_url, sep = ""))

#Need to find way to automate this to do more than 8 cases at a time. The code below should pull the text of all the documents but the Captcha seems to be activated when there are more than 8 being pulled

#docs <- test$website_url %>% 
#map(read_html) %>%
#map(function (node) {data.frame(doc = node %>% 
#                                  html_nodes(xpath = "//td[@class='h-col1 h-col1-inner3']") %>%
#                                 html_text(trim = TRUE))}) %>%
#bind_rows()

case_df <- test$website_url[1:8] %>% 
  map(read_html) %>%
  map(function (node) {data.frame(doc = node %>% 
                                    html_nodes(xpath = "//td[@class='h-col1 h-col1-inner3']") %>%
                                    html_text(trim = TRUE))}) %>%
  bind_rows()

test_judge <- case_df[1:8,] %>%
  str_extract(.,"[\u0410-\u04FF]\\w*\\s[\u0410-\u042F]\\.[\u0410-\u042F]\\.\\s\\(\u0441\u0443\u0434\u044c\u044F\\)|[\u0410-\u04FF]\\w*\\s[\u0410-\u04FF]\\w*\\s[\u0410-\u04FF]\\w*\\s\\(\u0441\u0443\u0434\u044c\u044F\\)") %>%
  print()

test_decision_text <- case_df[1:8,1] %>%
  str_extract(.,"(?<=\u0440\u0435\u0448\u0438\u043B\\:)(.*?)(?=\\n)|(?<=\u0440\\s\u0435\\s\u0448\\s\u0438\\s\u043B\\:)(.*?)(?=\\n)|(?<=\u0420\\s\u0415\\s\u0428\\s\u0418\\s\u041B\\:)(.*?)(?=\\n)|(?<=\u0420\\s\u0415\\s\u0428\\s\u0418\\s\u041B\\s\\:)(.*?)(?=\\n)|(?<=\u0420\u0415\u0428\u0418\u041B\\:)(.*?)(?=\\n)|(?<=\u041F\u041E\u0421\u0422\u0410\u041D\u041E\u0412\u0418\u041B\\:)(.*?)(?=\\n)|(?<=\u0420\u0435\u0448\u0438\u043B\\:)(.*?)(?=\\n)") %>%
  print()

test_case_type <- test$case %>%
  str_extract("(?<!\\s\u2116\\s)(\\w*)") %>%
  print()


test_court_distance <- c(1123,631,8303,1394,940,1070,1070,3546) #in km from Moscow to test$court

test_decision_state <- c(0,0,1,1,0,0,0,2)

test_decision_state_binary <- c(0,0,1,1,0,0,0,1)


#0 is with state, 1 is against state, 2 is partially against state this is manually coded

#outliers (10, 20, 21, 24, 25, 29)

#\u0420\u0435\u0448\u0435\u043D\u0438\u0435 ?????????????? (may be able to get rid of appeal directions or out in separate variable)

sample_df <- cbind(test, case_df, test_case_type, test_court_distance, test_decision_state, test_decision_state, test_decision_text, test_judge)

head(sample_df)

sample_df$test_judge

sample_df$court

sample_df$case

# individual judge characteristics (age, legal education (russian or foreign), mendevev or putin appointee) from  https://pravo.ru/judges_search/?JudgeSearch%5Bfullname%5D=%D0%9B%D0%B5%D0%BE%D0%BD%D0%B8%D0%B4%D0%BE%D0%B2%D0%BD%D0%B0

judge_age <- c(NA,NA,43,NA,0,0,0,2)

judge_edu <- c(NA,NA,NA,NA,0,0,0,2)

judge_app_year <- c(2008,NA,2010,2008,0,0,0,2)

judge_put_med <- c(1,NA,1,1,0,0,0,2) #0 for putin, 1 for medevev


#adding clean doc text 

sample_df$clean_doc <- c(NA[1:8])

