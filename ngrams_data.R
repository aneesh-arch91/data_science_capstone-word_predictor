library(tm)
library(dplyr)
library(tidytext)
library(ggplot2)
library(tidyr)
library(stringr)
library(tibble)

setwd('../final/en_US')

enUS_corpus <- VCorpus(DirSource(current_dir, encoding="UTF-8", pattern="*.txt"))
enUS_corpus

meta(enUS_corpus[[1]], "id")
blogs_text <- content(enUS_corpus[[1]])
head(blogs_text, 2)

meta(enUS_corpus[[2]], "id")
news_text <- content(enUS_corpus[[2]])
head(news_text, 2)

meta(enUS_corpus[[3]], "id")
twitter_text <- content(enUS_corpus[[3]])

set.seed(1984)
blogs_text <- sample(blogs_text, 100000)
news_text <- sample(news_text, 100000)
twitter_text <- sample(twitter_text, 100000)

blogs_textdf <- tibble(line = 1:length(blogs_text), text = blogs_text)
news_textdf <- tibble(line = 1:length(news_text), text = news_text)
twitter_textdf <- tibble(line = 1:length(twitter_text), text = twitter_text)

blogs_tidy <- blogs_textdf %>%
	unnest_tokens(word, text)

news_tidy <- news_textdf %>%
	unnest_tokens(word, text)

twitter_tidy <- twitter_textdf %>%
	unnest_tokens(word, text)

blogs_bigram <- blogs_textdf %>%
	unnest_tokens(ngram, text, token = 'ngrams', n = 2) %>%
	separate(ngram, c("word1", "word2"), sep = " ") %>%
	filter(!is.na(word1) & !grepl("\\d", word1)) %>%
	filter(!is.na(word2) & !grepl("\\d", word2)) %>%
	mutate(ngram=paste(word1, word2, sep = " ")) %>%
	select(-c(word1, word2))

blogs_trigram <- blogs_textdf %>%
	unnest_tokens(ngram, text, token = 'ngrams', n = 3) %>%
	separate(ngram, c("word1", "word2", "word3"), sep = " ") %>%
	filter(!is.na(word1) & !grepl("\\d", word1)) %>%
	filter(!is.na(word2) & !grepl("\\d", word2)) %>%
	filter(!is.na(word3) & !grepl("\\d", word3)) %>%
	mutate(ngram=paste(word1, word2, word3, sep = " ")) %>%
	select(-c(word1, word2, word3)) #%>%

blogs_quadrigram <- blogs_textdf %>%
	unnest_tokens(ngram, text, token = 'ngrams', n = 4) %>%
	separate(ngram, c("word1", "word2", "word3", "word4"), sep = " ") %>%
	filter(!is.na(word1) & !grepl("\\d", word1)) %>%
	filter(!is.na(word2) & !grepl("\\d", word2)) %>%
	filter(!is.na(word3) & !grepl("\\d", word3)) %>%
	filter(!is.na(word4) & !grepl("\\d", word4)) %>%
	mutate(ngram=paste(word1, word2, word3, word4, sep = " ")) %>%
	select(-c(word1, word2, word3, word4)) #%>%

news_bigram <- news_textdf %>%
	unnest_tokens(ngram, text, token = 'ngrams', n = 2) %>%
	separate(ngram, c("word1", "word2"), sep = " ") %>%
	filter(!is.na(word1) & !grepl("\\d", word1)) %>%
	filter(!is.na(word2) & !grepl("\\d", word2)) %>%
	mutate(ngram=paste(word1, word2, sep = " ")) %>%
	select(-c(word1, word2))

news_trigram <- news_textdf %>%
	unnest_tokens(ngram, text, token = 'ngrams', n = 3) %>%
	separate(ngram, c("word1", "word2", "word3"), sep = " ") %>%
	filter(!is.na(word1) & !grepl("\\d", word1)) %>%
	filter(!is.na(word2) & !grepl("\\d", word2)) %>%
	filter(!is.na(word3) & !grepl("\\d", word3)) %>%
	mutate(ngram=paste(word1, word2, word3, sep = " ")) %>%
	select(-c(word1, word2, word3)) #%>%

news_quadrigram <- news_textdf %>%
	unnest_tokens(ngram, text, token = 'ngrams', n = 4) %>%
	separate(ngram, c("word1", "word2", "word3", "word4"), sep = " ") %>%
	filter(!is.na(word1) & !grepl("\\d", word1)) %>%
	filter(!is.na(word2) & !grepl("\\d", word2)) %>%
	filter(!is.na(word3) & !grepl("\\d", word3)) %>%
	filter(!is.na(word4) & !grepl("\\d", word4)) %>%
	mutate(ngram=paste(word1, word2, word3, word4, sep = " ")) %>%
	select(-c(word1, word2, word3, word4)) #%>%

twitter_bigram <- twitter_textdf %>%
	unnest_tokens(ngram, text, token = 'ngrams', n = 2) %>%
	separate(ngram, c("word1", "word2"), sep = " ") %>%
	filter(!is.na(word1) & !grepl("\\d", word1)) %>%
	filter(!is.na(word2) & !grepl("\\d", word2)) %>%
	mutate(ngram=paste(word1, word2, sep = " ")) %>%
	select(-c(word1, word2))

twitter_trigram <- twitter_textdf %>%
	unnest_tokens(ngram, text, token = 'ngrams', n = 3) %>%
	separate(ngram, c("word1", "word2", "word3"), sep = " ") %>%
	filter(!is.na(word1) & !grepl("\\d", word1)) %>%
	filter(!is.na(word2) & !grepl("\\d", word2)) %>%
	filter(!is.na(word3) & !grepl("\\d", word3)) %>%
	mutate(ngram=paste(word1, word2, word3, sep = " ")) %>%
	select(-c(word1, word2, word3)) #%>%

twitter_quadrigram <- twitter_textdf %>%
	unnest_tokens(ngram, text, token = 'ngrams', n = 4) %>%
	separate(ngram, c("word1", "word2", "word3", "word4"), sep = " ") %>%
	filter(!is.na(word1) & !grepl("\\d", word1)) %>%
	filter(!is.na(word2) & !grepl("\\d", word2)) %>%
	filter(!is.na(word3) & !grepl("\\d", word3)) %>%
	filter(!is.na(word4) & !grepl("\\d", word4)) %>%
	mutate(ngram=paste(word1, word2, word3, word4, sep = " ")) %>%
	select(-c(word1, word2, word3, word4)) #%>%

bigrams <- bind_rows(blogs_bigram, news_bigram, twitter_bigram)
trigrams <- bind_rows(blogs_trigram, news_trigram, twitter_trigram)
quadrigrams <- bind_rows(blogs_quadrigram, news_quadrigram, twitter_quadrigram)

saveRDS(bigrams, file='bigrams.rds')
saveRDS(trigrams, file='trigrams.rds')
saveRDS(quadrigrams, file='quadrigrams.rds')
