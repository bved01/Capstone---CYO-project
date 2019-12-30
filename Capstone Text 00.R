library(quanteda)
library(R.utils)
library(stringr)

print("creating necessary data directories")
for (curr_dir in c(data_dir, data_raw_dir, data_clean_dir, 
                   model_dir, model_data_dir, 
                   prediction_dir, prediction_data_dir)) {
  if (!file.exists(curr_dir)) {
    dir.create(curr_dir, showWarnings = FALSE)
  }
}

curr_dir <-sprintf("%s/final/%s", data_raw_dir, language_of_interest)
if (!file.exists(curr_dir)) {
  dir.create(curr_dir, showWarnings = FALSE)
}

print("all directories created")

rm(curr_dir)

###############################

# download corpus and unzip if needed
print("downloading Coursera-SwiftKey.zip")
url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
zip_file <- sprintf("%s/Coursera-SwiftKey.zip", data_raw_dir)

if (!file.exists(zip_file)) {
  download.file(url, method="curl", destfile = zip_file) 
  print("Coursera-SwiftKey.zip downloaded")
} else {
  print("Coursera-SwiftKey.zip already exists")
}
###########################################

print("unpacking Coursera-SwiftKey.zip")
lang_dir <- sprintf("%s/final/%s", data_raw_dir, language_of_interest)

if (!file.exists(lang_dir)) {
  unzip(zip_file, exdir = data_raw_dir)
  print("Coursera-SwiftKey.zip unpacked")
} else {
  print("Coursera-SwiftKey.zip for 'language of interest' already unpacked")
}

rm(url, zip_file, lang_dir)


# save language of interest text files as RDS, 
# it's compressed and it takes shorter to load the files in R
print("optimising representation data format")
for (src in c('blogs', 'news', 'twitter')) {
  rds_file <- sprintf("%s/%s.rds", data_raw_dir, src)
  if (!file.exists(rds_file)) {
    txt_file <- sprintf("%s/final/%s/%s.%s.txt", data_raw_dir, 
                        language_of_interest, language_of_interest, src)
    txt <- readLines(txt_file, skipNul = T, encoding="UTF-8")
    saveRDS(txt,rds_file) 
    print(sprintf("%s created", rds_file))
  } else{
    print(sprintf("%s already exists", rds_file))
  }
}
print("representation data format optimised")


######################################
###
# this script splits the data for train, validation and test cases

library(R.utils)
set.seed(42)

if( (train_part + validation_part + test_part) != 100 
    | train_part < 0  | validation_part < 0 | test_part < 0) {
  stop("total sum can't excede 100 %")
}

print("creating train, test, validation splits")

if (!file.exists(sprintf("%s/data_train.rds", data_raw_dir))
    | !file.exists(sprintf("%s/data_validation.rds", data_raw_dir))
    | !file.exists(sprintf("%s/data_test.rds", data_raw_dir))) {
  
  train_data <- c()
  validation_data <- c()
  test_data <- c()
  
  for (src in c("blogs", "news", "twitter")) {
    # train split
    txt <- readRDS(sprintf("%s/%s.rds", data_raw_dir, src))
    num_lines <- length(txt)
    
    for (x in list(c("train", train_part),
                   c("validation", validation_part),
                   c("test", test_part))) {
      split_type <- as.character(x[1])
      split_percent <- as.numeric(x[2])
      
      sample_size <- ceiling(num_lines * split_percent / 100)
      curr_txt_len <- length(txt)
      
      if (sample_size < curr_txt_len) {
        sampled_ids <- sample(curr_txt_len, sample_size)
        sampled_lines <- txt[sampled_ids]
        txt <- txt[-sampled_ids]
      } else {
        # works only for last part
        sampled_lines <- txt
      }
      
      if (split_type == "train") {
        train_data <- c(train_data, sampled_lines)
      } else if (split_type == "validation") {
        validation_data <- c(validation_data, sampled_lines)
      } else if (split_type == "test") {
        test_data <- c(test_data, sampled_lines)
      }
    }
  }
  
  #save files containing data samples from all corpus
  saveRDS(train_data, sprintf("%s/data_train.rds", data_raw_dir))
  saveRDS(validation_data, sprintf("%s/data_validation.rds", data_raw_dir))
  saveRDS(test_data, sprintf("%s/data_test.rds", data_raw_dir))
  
  print("train, test, validation splits created")
  
  rm(train_data, validation_data, test_data, txt, num_lines,
     split_type, split_percent, sample_size, curr_txt_len,
     sampled_ids, sampled_lines, x, src, rds_file)
} else {
  print("train, test, validation splits already exists")
}
################################################
###
# this script clean data:

library(quanteda)

print("loading dictionary of words")

dct <- readLines(dict_dir, encoding = "UTF-8")

print("dictionary loaded")

print("creating clean data splits")

if (!file.exists(sprintf("%s/%s.rds", data_clean_dir, "data_train"))) {
  #data_clean = c()
  data_raw = c()
  
  for (src in c(#"blogs_train", "news_train", "twitter_train",
    "data_train")) {
    
    raw_txt <- readRDS(sprintf("%s/%s.rds", data_raw_dir, src))
    data_raw <- c(data_raw, raw_txt)
  }
  
  t <- system.time({
    data_raw <- corpus(tolower(data_raw))
    data_clean <- tokenize(data_raw, 
                           removeURL = T, 
                           removeNumbers = T, 
                           removePunct = T, 
                           removeSymbols = T, 
                           removeSeparators = T, 
                           removeTwitter = T, 
                           removeHyphens = T, 
                           verbose = T)
    
    print("parsing with known dictionary")
    mask <- data_clean[[1]] %in% dct
    data_clean[[1]][!mask] <- UNK
  })

  
  print(sprintf("%s preprocessing completed in %.3f s", "train data", t[3]))
  
  rds_file <- sprintf("%s/%s.rds", data_clean_dir, "data_train")
  saveRDS(data_clean, rds_file)
  print(sprintf("%s created", rds_file))
  
  rm(data_clean, data_raw, clean_txt, raw_txt, mask, t, rds_file, src, dct)
  gc()
  
  print("clean data splits created")
} else {
  print("clean data splits already exists")
}
########################################

setwd(paste0(dirname(parent.frame(2)$ofile), "/.."))


nrams_freq_df <- function(tokens, ngram) {
  tokens <- ngrams(tokens, n = ngram, concatenator = "_")
  data_ngram <- dfm(tokens, ngrams = ngram, toLower = F, verbose = T)
  
  df_freq <- sort(colSums(data_ngram), decreasing = T)
  
  df_freq  <- data.frame(names(df_freq), df_freq)
  names(df_freq) <- c("words", "frequency")
  
  df_freq <- df_freq[df_freq$frequency >= MIN_FREQUENCY, ]
  
  if (ngram > 1) {
    df_freq$sentence <- word(df_freq$words, -ngram, -2, sep = fixed("_"))
    df_freq$prediction <- word(df_freq$words, -1, sep = fixed("_"))
    df_freq <- df_freq[!df_freq$sentence == "",]
    df_freq <- df_freq[!df_freq$prediction == UNK,]
    df_freq$words <- NULL
    df_freq  <- df_freq[c("sentence", "prediction", "frequency")]
  }
  rownames(df_freq) <- c(1:nrow(df_freq))
  df_freq  <- df_freq[complete.cases(df_freq),]
  df_freq
}

nrams_freq_df_probs <- function(df) {
  df_probs <- data.frame(X=table(df$frequency))
  names(df_probs) <- c("r", "n")
  df_probs$r <- as.numeric(as.character(df_probs$r))
  Xlength <- nrow(df_probs)
  
  # Assign to N the sum of the products of the pairs of integers 
  # in the r and n columns
  # This will be the number of individuals in the sample.
  N <- sum(df_probs$r*df_probs$n)
  
  # Estimate of the total probability of all unseen species
  P_0 <- df_probs$r[1]/N
  
  df_probs$Z <- 0
  
  for (c in 1:Xlength) {
    if (c == 1) {
      i <- 0
    } else {
      i <- i <- df_probs$r[c-1]
    }
    if (c == Xlength) {
      k <- df_probs$r[c]
    } else {
      k <- df_probs$r[c+1]
    }
    df_probs$Z[c] <- 2*df_probs$n[c] / ( k-i )
  }
  
  df_probs$logr <- log(df_probs$r)
  df_probs$logZ <- log(df_probs$Z)
  df_probs$rstar <- 0
  
  # linear regression model
  model1 <- glm(logZ ~ logr, data = df_probs)
  
  c0 <- model1$coefficients[1]
  c1 <- model1$coefficients[2]
  
  ycheck = FALSE
  for (c in 1:(Xlength-1)) {
    rplus1 <- df_probs$r[c] + 1
    
    s_rplus1 <- exp(c0 + (c1 * df_probs$logr[c+1]))
    s_r <- exp(c0 + (c1 * df_probs$logr[c]))
    y <- rplus1 * s_rplus1/s_r
    
    if(ycheck) {
      df_probs$rstar[c] <- y
    } else { 
      n_rplus1 <- df_probs$n[df_probs$r == rplus1]
      n_r <- df_probs$n[c]
      x <- (rplus1) * n_rplus1/n_r
      
      limit <- sqrt(((rplus1)^2) * (n_rplus1/((n_r)^2)) * (1 + (n_rplus1/n_r)))
      
      if (abs(x-y) > 1.96 * limit) {
        df_probs$rstar[c] <- x
      }else {
        df_probs$rstar[c] <- y
        ycheck = TRUE
      }
    }
    if(c==(Xlength-1)) {
      df_probs$rstar[c+1] <- y
    }
    
  }
  
  N_1 <- sum(df_probs$n * df_probs$rstar)
  df_probs$p <- (1-P_0) * df_probs$rstar/N_1
  
  df_probs
}

print("creating ngram data splits")

rds_file <- sprintf("%s/%s.rds", model_data_dir, "data_train_tetragram")
if (!file.exists(rds_file)) {
  for (src in c(#"blogs", 
    #"news", 
    #"twitter", 
    "data")) {
    for (x in list(
      c("unigram", 1), # cause 'the'/'and' is always most frequent
      c("bigram", 2),
      c("trigram", 3),
      c("tetragram", 4))) {
      ngram_type <- as.character(x[1])
      ngram <- as.numeric(x[2])
      
      txt_file <- sprintf("%s/%s_train.rds", data_clean_dir, src)
      txt <- readRDS(txt_file)
      
      print(sprintf("creating %s for %s", ngram_type, src))
      
      t <- system.time({
        print("creating ngrams")
        data_ngram <- nrams_freq_df(txt, ngram)
        
        print("creating ngrams probabilities")
        data_probs <- nrams_freq_df_probs(data_ngram)
        
        print("merging ngrams with probabilities")
        data_ngram$probability <- data_probs$probability[
          match(data_ngram$freq, data_probs$r)]
        data_ngram <- data_ngram[
          order(data_ngram$probability, decreasing = TRUE),]
      })
      
      rds_file <- sprintf("%s/%s_%s.rds", model_data_dir, src, ngram_type)
      saveRDS(data_ngram, rds_file)
      
      print(sprintf("%s for %s created in %.3f s", ngram_type, src, t[3]))
      
      rm(txt, data_ngram)
      gc()
    }
    print("ngram data splits created")
  }  
} else {
  print("ngram data splits already exists")
}

rm(src, x, ngram_type, ngram, txt_file, txt, data_ngram, t, rds_file)
gc()

###################################################
language_of_interest <- "en_US" # should be one of [de_DE, en_US, fi_FI, ru_RU]

data_dir <- "data"
data_raw_dir <- "data/raw"
data_clean_dir <- "data/clean"

model_dir <- "shiny"
model_data_dir <- "shiny/data"

prediction_dir <- "data"
prediction_data_dir <- "data/prediction"

# percent of total lines used in train, validation and test stages
# the higher the percentage of data used in train, the higher the accuracy, but
# also the higher the sparsity of data and the memory/time required to process the data
train_part <- 80.0
validation_part <- 10.0
test_part <- 10.0

dict_dir <- "shiny/dicts/cracklib-small.txt"
UNK <- "UNK"

MIN_FREQUENCY <- 4

#######################################

