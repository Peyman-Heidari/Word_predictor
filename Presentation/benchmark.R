require(digest)
require(stringi)
require(data.table)

################################################################################################
#
# 01. Loading of benchmark data sets
#
################################################################################################
counter <- 0

# 01b. Get text from randomly selected tweets
################################################################################################

tweets <- readLines('data/tweets.txt', encoding = 'UTF-8')

# verify checksum of loaded lines
digest(paste0(tweets, collapse = '||'), 
       algo='sha256', 
       serialize=F)==
    "7fa3bf921c393fe7009bc60971b2bb8396414e7602bb4f409bed78c7192c30f4"


# 01c. Get text from randomly selected blog descriptions
################################################################################################

# make sure we can read it back in
blogs <- readLines('data/blogs.txt', encoding = 'UTF-8')

# verify checksum of loaded lines
digest(paste0(blogs, collapse = '||'), 
       algo='sha256', 
       serialize=F)==
    "14b3c593e543eb8b2932cf00b646ed653e336897a03c82098b725e6e1f9b7aa2"



################################################################################################
#
# 02. Define the functions used for benchmarking
#
################################################################################################

# 02a. Pre-processing functions
################################################################################################

# split.sentence
#  Returns a matrix containing in column i the part of the line before the ith word (sentence) 
#  and the ith word (nextWord).
#  The function is used in benchmark to generate and evaluate predictions for the partial lines.
split.sentence <- compiler::cmpfun(function(line) {
    require(stringi)
    # append a space to the sentence (to make sure we always create one result with only the 
    # last word missing)
    sent <- paste0(line, ' ')

    sep <- stri_locate_all_regex(line, 
                                 pattern = '[^\\w\'@#\u2018\u2019\u201b]+', 
                                 omit_empty=T, 
                                 case_insensitive=T)[[1]]
    sapply(seq_len(nrow(sep)), 
           function(i) {
               c(sentence=ifelse(i>1, substr(line, 1, sep[i-1,2]), ''), 
                    nextWord=tolower(substr(line, max(sep[i-1,2]+1, 1), min(nchar(line), sep[i,1]-1)))
               )
               })
}, options=list(optimize=3))


# 02b. Benchmarking function
################################################################################################

# benchmark
#  Evaluates the performance of a next word prediction algorithm based on the provided test data-
#  set(s).
#
#  Parameters
#   FUN         Function that produces the next word prediction. The function should take a single 
#               character value as first input and return a vector of character values represen-
#               ting the top-3 predictions (with the 1st value being the first prediction).
#   ...         Additional parameters to pass to FUN.
#   sent.list   Named list of character vectors containing the text lines used for the benchmark.
#   ext.output  If TRUE, return additional details about the R environment and loaded packages 
#               after completing the benchmark.
benchmark <- compiler::cmpfun(function(FUN, ..., sent.list, ext.output=T) {
    require(stringi)
    require(digest)
    require(data.table)
    
    result <- rbindlist(lapply(names(sent.list), 
           function(list.name) {  
               sentences <- sent.list[[list.name]]
               
               score <- 0
               max.score <-0
               hit.count.top3 <- 0
               hit.count.top1 <- 0
               total.count <- 0
              
               time <- system.time({
                   for (sent in sentences) {
                       split <- split.sentence(sent[1])
                       max.score <- max.score + ncol(split)*3
                       total.count <- total.count + ncol(split)
                       rank <- sapply(seq_len(ncol(split)),
                                      function(i) {
                                          min(which(FUN(split[1,i], ...)==split[2,i]),4)
                                      })
                       score <- score + sum(4-rank)
                       hit.count.top3 <- hit.count.top3 + sum(rank<4)
                       hit.count.top1 <- hit.count.top1 + sum(rank==1)
                       print(hit.count.top1)
                   }
               })
               
               list('list.name' = list.name,
                    'line.count' = length(sentences),
                    'word.count' = sum(stri_count_words(sentences)),
                    'hash' = digest(paste0(sentences, collapse = '||'), algo='sha256', serialize=F),
                    'score' = score,
                    'max.score' = max.score,
                    'hit.count.top3' = hit.count.top3,
                    'hit.count.top1' = hit.count.top1,
                    'total.count' = total.count,
                    'total.runtime' = time[3]
               )               
           }), use.names=T)
    
    setkey(result, list.name)
    
    # The overall scores are calculated weighting each data set equally (independent of the 
    # number of lines in each dataset).
    overall.score.percent = 100 * result[,sum(score/max.score)/.N]
    overall.precision.top3 = 100 * result[,sum(hit.count.top3/total.count)/.N]
    overall.precision.top1 = 100 * result[,sum(hit.count.top1/total.count)/.N]
    average.runtime = 1000 * result[,sum(total.runtime)/sum(total.count)]
    number.of.predictions = result[,sum(total.count)]
    total.mem.used = sum(unlist(lapply(ls(.GlobalEnv),
                                       function(x) {
                                           object.size(get(x,
                                                           envir = .GlobalEnv,
                                                           inherits = FALSE))
                                           })))/(1024^2)
    cat(sprintf(paste0('Overall top-3 score:     %.2f %%\n',
                       'Overall top-1 precision: %.2f %%\n',
                       'Overall top-3 precision: %.2f %%\n',
                       'Average runtime:         %.2f msec\n',
                       'Number of predictions:   %d\n',
                       'Total memory used:       %.2f MB\n'),
                overall.score.percent,
                overall.precision.top1,
                overall.precision.top3,
                average.runtime,
                number.of.predictions,
                total.mem.used
                ))
    
    cat('\nDataset details\n')
    for (p.list.name in result$list.name) {
        res <- result[list(p.list.name)]
        cat(sprintf(paste0(' Dataset "%s" (%d lines, %d words, hash %s)\n',
                           '  Score: %.2f %%, Top-1 precision: %.2f %%, Top-3 precision: %.2f %%\n'
                           ),
                    p.list.name,
                    res$line.count,
                    res$word.count,
                    res$hash,
                    100 * res$score/res$max.score,
                    100 * res$hit.count.top1/res$total.count,
                    100 * res$hit.count.top3/res$total.count
        ))
    }
    
    if (ext.output==T) {
        packages <- sort(stri_replace_first_fixed(search()[stri_detect_regex(search(), 
                                                                             '^package:')], 
                                                  'package:', ''))
        
        cat(sprintf(paste0('\n\n%s, platform %s\n', 
                           'Attached non-base packages:   %s\n',
                           'Unattached non-base packages: %s'
                           ),
                   sessionInfo()$R.version$version.string,
                   sessionInfo()$platform,
                   paste0(sapply(sessionInfo()$otherPkgs, 
                                 function(pkg) {
                                     paste0(pkg$Package, ' (v', pkg$Version, ')')
                                 }), 
                          collapse = ', '),
                   paste0(sapply(sessionInfo()$loadedOnly, 
                                 function(pkg) { 
                                     paste0(pkg$Package, ' (v', pkg$Version, ')')
                                 }), 
                          collapse = ', ')
                   ))
    }
}, options=list(optimize =3))




################################################################################################
#
# 03. Define the wrapper function to be called by benchmark
#
################################################################################################

# As an example, we create a very simple baseline algorithm which always returns
# the three most frequent English words.
library(hash); library(data.table); library(quanteda); library(qdap)

#setwd("C:/Users/heidarip/Dropbox/Coursera/NLP project/clean/Presentation")

load(file="unigram_idx2.Rda")
load(file="bigram_idx_processed.Rda")
load(file="trigram_idx_processed.Rda")
load(file="fourgram_idx_processed.Rda")
load(file="dictionary2.Rda")
load(file="dictionary_invert2.Rda")
.set( dictionary, keys="notseenword", values=1000000 )
.set( dictionary_invert, keys="1000000", values="notseenword" )

output_dummy <- data.table(prediction=c(dictionary[["the"]],dictionary[["and"]], dictionary[["a"]],dictionary[["be"]], dictionary[["to"]]), prob=c(0.001, 0.0009, 0.0008, 0.0007, 0.0006))

quant_cleaner<- function(x){
  cleaned_x <- tokenize(x, what = c("word"), removeURL = T, removeNumbers = T, removePunct = T,removeSymbols = T, removeSeparators = TRUE, removeTwitter = T, removeHyphens = T, ngrams = 1L, skip = 0L, concatenator = "_", simplify = FALSE, verbose = FALSE)
  return(cleaned_x)
}

predict.baseline <- function(x, fivegram_idx, fourgram_idx, trigram_idx, bigram_idx, unigram_idx, D1s, D2s, D3s, words_considered){
  input <- x
  if (grepl("'", input)) {input <- replace_contraction(input)}
  input <- toLower(input)
  input <- quant_cleaner(input)
  words <- unlist(input)
  words <- tail(words, 3)
  number_of_words <- length(words)
  words_index <- rep(0,number_of_words)
  if (number_of_words>0){
    for (ii in 1:number_of_words){ 
      if(is.null(dictionary[[words[ii]]])) {
        words_index[ii]<- 1000000
      }else {words_index[ii] <- dictionary[[words[ii]]]}  
    }
  }
  
  
  final_input1 <- "" ; final_input2 <- "" ; final_input3 <- ""
  
  output <- ""
  
  in_four <- 0; in_tri <- 0; in_bi <- 0 ; 
  
  if (number_of_words >= 3){
    final_input3 <- words_index[number_of_words-2]
    final_input2 <- words_index[number_of_words-1]
    final_input1 <- words_index[number_of_words]
    fourgram_exist   <-fourgram_idx_processed[J(final_input3, final_input2, final_input1), nomatch=0]
    trigram_exist   <-trigram_idx_processed[J(final_input2, final_input1), nomatch=0]
    bigram_exist   <-bigram_idx_processed[J(final_input1), nomatch=0]
    in_four <- dim(fourgram_exist)[1]
    in_tri <- dim(trigram_exist)[1] 
    in_bi <- dim(bigram_exist)[1]
  }else if (number_of_words == 2){
    final_input2 <- words_index[number_of_words-1]
    final_input1 <- words_index[number_of_words]
    trigram_exist   <-trigram_idx_processed[J(final_input2, final_input1), nomatch=0]
    bigram_exist   <-bigram_idx_processed[J(final_input1), nomatch=0] 
    in_tri <- dim(trigram_exist)[1] 
    in_bi <- dim(bigram_exist)[1]
  }else if (number_of_words ==1 ){
    final_input1 <- words_index[number_of_words]
    bigram_exist   <-bigram_idx_processed[J(final_input1), nomatch=0]
    in_bi <- dim(bigram_exist)[1]
  }
  
  
  
  if (number_of_words >= 3 & in_four>0){
    gamma_four <- fourgram_exist$gamma_four[1]
    gamma_tri <- trigram_exist$gamma_tri[1]
    gamma_bi <- bigram_exist$gamma_bi[1]
    trigram_exist$four_prob_kn <- trigram_exist$tri_prob_kn * gamma_four
    bigram_exist$four_prob_kn <- bigram_exist$bi_prob_kn * (gamma_tri * gamma_four)
    results <- fourgram_exist[,c("prediction", "four_prob_kn"), with=FALSE]
    results <- rbind(results, trigram_exist[,c("prediction", "four_prob_kn"), with=FALSE])
    results <- rbind(results, bigram_exist[,c("prediction", "four_prob_kn"), with=FALSE])
    colnames(results) <- c("prediction", "prob")
    results <- rbind(results, output_dummy)
    results <- results[order(results$prob, decreasing = T),]
    results <- results[!duplicated(results$prediction),]
    output <- head(results$prediction,3)
  }else if (number_of_words >= 2  & in_tri>0){
    gamma_tri <- trigram_exist$gamma_tri[1]
    gamma_bi <- bigram_exist$gamma_bi[1]
    bigram_exist$tri_prob_kn <- bigram_exist$bi_prob_kn * gamma_tri 
    results <- trigram_exist[,c("prediction", "tri_prob_kn"), with=FALSE]
    results <- rbind(results, bigram_exist[,c("prediction", "tri_prob_kn"), with=FALSE])
    colnames(results) <- c("prediction", "prob")
    results <- rbind(results, output_dummy)
    results <- results[order(results$prob, decreasing = T),]
    results <- results[!duplicated(results$prediction),]
    output <- head(results$prediction,3)
  } else if (number_of_words >= 1 & in_bi>0){
    gamma_bi <- bigram_exist$gamma_bi[1]
    unigram_idx$bi_prob_kn <- unigram_idx$uni_prob_kn * gamma_bi
    results <- bigram_exist[,c("prediction", "bi_prob_kn"), with=FALSE]
    results <- rbind(results, unigram_idx[,c("prediction", "bi_prob_kn"), with=FALSE])
    colnames(results) <- c("prediction", "prob")
    results <- rbind(results, output_dummy)
    results <- results[order(results$prob, decreasing = T),]
    results <- results[!duplicated(results$prediction),]
    output <- head(results$prediction,3)
  }else {
    results <- output_dummy
    output <- head(results$prediction,3)} 
  
  output <- as.character(output)
  for (ii in 1:length(output)) {output[ii] <- dictionary_invert[[output[ii]]]}
  return(output)  
}



################################################################################################
#
# 04. Perform the benchmark
#
################################################################################################
benchmark(predict.baseline, fivegram_idx,fourgram_idx, trigram_idx, bigram_idx, unigram_idx, D1s, D2s, D3s, words_considered,
          sent.list = list('tweets' = tweets, 'blogs' = blogs), ext.output = F)
