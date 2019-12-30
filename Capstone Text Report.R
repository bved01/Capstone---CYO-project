##Bijal Ashwin Ved
##https://github.com/bved01/
##HarvardX: PH125.9x Data Science: Capstone Course 
##Choose your own project - Milestone Report Project

##Description: The following report describes the analysis of a large collection of blogs, news 
##articles and tweets. The purpose of the analysis is to understand how this data could be used 
##to predict text application. A brief outline plan for the application is given at
##the end of the document.


#####################################################
#Libraries:

library(tm)
library(stringi)
library(RWeka)
library(ggplot2)
library(quanteda)


#####################################################
# Import and Process data
# Read the blogs and Twitter data into R

blogs <- readLines("C:/Users/jivra/OneDrive/Documents/Capstone-MovieLens-Project/en_US.blogs.txt", 
                   encoding = "UTF-8", skipNul = TRUE)
news <- readLines("C:/Users/jivra/OneDrive/Documents/Capstone-MovieLens-Project/en_US.news.txt", 
                  encoding = "UTF-8", skipNul = TRUE)
twitter <- readLines("C:/Users/jivra/OneDrive/Documents/Capstone-MovieLens-Project/en_US.twitter.txt", 
                     encoding = "UTF-8", skipNul = TRUE)


##The data was downloaded from <https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip> and unzipped. 
##The 3 text files containing blogs, news articles and tweets were then opened and some basic summary statistics calculated.
##Files are too big to download into github

##We examined the data sets and summarize our findings (file sizes, line counts, word counts, 
##and mean words per line) below.


#####################################################
# Get file sizes
blogs.size <- file.info("final/en_US/en_US.blogs.txt")$size / 1024 ^ 2
news.size <- file.info("final/en_US/en_US.news.txt")$size / 1024 ^ 2
twitter.size <- file.info("final/en_US/en_US.twitter.txt")$size / 1024 ^ 2


#####################################################
# Get words in files
blogs.words <- stri_count_words(blogs)
news.words <- stri_count_words(news)
twitter.words <- stri_count_words(twitter)

#####################################################
# Summary of the data sets
data.frame(source = c("blogs", "news", "twitter"),
           file.size.MB = c(blogs.size, news.size, twitter.size),
           num.lines = c(length(blogs), length(news), length(twitter)),
           num.words = c(sum(blogs.words), sum(news.words), sum(twitter.words)),
           mean.num.words = c(mean(blogs.words), mean(news.words), mean(twitter.words)))


#####################################################
## Cleaning The Data
##Before performing exploratory analysis, we must clean the data first. 
##we will randomly choose 1% of the data to demonstrate the data cleaning and exploratory analysis.

# Sample the data
set.seed(42)
data.sample <- c(sample(blogs, length(blogs) * 0.01),
                 sample(news, length(news) * 0.01),
                 sample(twitter, length(twitter) * 0.01))

# Create corpus and clean the data
corpus <- VCorpus(VectorSource(data.sample))
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
corpus <- tm_map(corpus, toSpace, "(f|ht)tp(s?)://(.*)[.][a-z]+")
corpus <- tm_map(corpus, toSpace, "@[^\\s]+")
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removeWords, stopwords("en"))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, PlainTextDocument)


#####################################################
## Exploratory Analysis
options(mc.cores=1)
getFreq <- function(tdm) {
  freq <- sort(rowSums(as.matrix(tdm)), decreasing = TRUE)
  return(data.frame(word = names(freq), freq = freq))
}
bigram <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
trigram <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
makePlot <- function(data, label) {
  ggplot(data[1:30,], aes(reorder(word, -freq), freq)) +
    labs(x = label, y = "Frequency") +
    theme(axis.text.x = element_text(angle = 60, size = 12, hjust = 1)) +
    geom_bar(stat = "identity", fill = I("grey50"))
}

# Get frequencies of most common n-grams in data sample
freq1 <- getFreq(removeSparseTerms(TermDocumentMatrix(corpus), 0.9999))
freq2 <- getFreq(removeSparseTerms(TermDocumentMatrix(corpus, control = list(tokenize = bigram)), 0.9999))
freq3 <- getFreq(removeSparseTerms(TermDocumentMatrix(corpus, control = list(tokenize = trigram)), 0.9999))


#####################################################
## Graphs
makePlot(freq1, "30 Most Common Unigrams")
makePlot(freq2, "30 Most Common Bigrams")
makePlot(freq3, "30 Most Common Trigrams")


#####################################################
##Next Steps
#Next steps would be to predict the next word 
