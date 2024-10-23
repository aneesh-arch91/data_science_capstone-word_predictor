library(shiny)
library(tm)
library(dplyr)
library(tidytext)
library(tidyr)
library(stringr)
library(tibble)

bigrams <- readRDS('bigrams.rds')
trigrams <- readRDS('trigrams.rds')
quadrigrams <- readRDS('quadrigrams.rds')

ngram_nextword <- function(inputword, ng_dataset) {
	ngrams_word <- ng_dataset[grep(paste0('^', inputword, ' .'), ng_dataset$ngram),]

	if (nrow(ngrams_word) == 0)
	{
		ngrams_word <- ngrams_word %>%
			add_column(n = NA) %>%
			add_row(ngram = " ", n = 0)
	}

	ngrams_word <- ngrams_word %>%
		count(ngram, sort=TRUE)

	ngrams_word[1,]
}

nextword <- function(inputtext, bigram_data, trigram_data, quadrigram_data) {

	inputdf <- tibble(line = 1:length(inputtext), text = inputtext)
	input_words <- inputdf %>%
		unnest_tokens(word, text)
	input_words <- input_words$word

	quad_nextword <- data.frame(ngram=c(" "), n=c(0))[1,]
	tri_nextword <- data.frame(ngram=c(" "), n=c(0))[1,]
	bi_nextword <- data.frame(ngram=c(" "), n=c(0))[1,]

	if (length(input_words) >= 3)
	{
		trigram_words <- tail(input_words, 3)
		trigram <- paste(trigram_words[[1]], trigram_words[[2]], trigram_words[[3]], sep=' ')
		bigram <- paste(trigram_words[[2]], trigram_words[[3]], sep=' ')
		unigram <- trigram_words[[3]]
		quad_nextword <- ngram_nextword(trigram, quadrigram_data)
		tri_nextword <- ngram_nextword(bigram, trigram_data)
		bi_nextword <- ngram_nextword(unigram, bigram_data)
	}
	else if (length(input_words) == 2)
	{
		bigram_words <- tail(input_words, 2)
		bigram <- paste(bigram_words[[1]], bigram_words[[2]], sep=' ')
		unigram <- bigram_words[[2]]
		tri_nextword <- ngram_nextword(bigram, trigram_data)
		bi_nextword <- ngram_nextword(unigram, bigram_data)
	}
	else if (length(input_words) == 1)
	{
		unigram <- tail(input_words, 1)
		bi_nextword <- ngram_nextword(unigram, bigram_data)
	}

	if (quad_nextword$n >= max(tri_nextword$n, bi_nextword$n))
		return(tail(unlist(strsplit(quad_nextword$ngram, split=' ')), 1))
	else
	{
		if (tri_nextword$n >= bi_nextword$n)
			return(tail(unlist(strsplit(tri_nextword$ngram, split=' ')), 1))
		else
			return(tail(unlist(strsplit(bi_nextword$ngram, split=' ')), 1))
	}
	return('')
}

server <- shinyServer(
	function(input, output) {
		output$word <- renderText({
			input$textinput
		})

		output$pred_word <- renderText({
			nextword(input$textinput, bigrams, trigrams, quadrigrams)
		})
	}
)
