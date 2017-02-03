set.seed(777)
library(stringr); library(tm); library(ngram); library(RColorBrewer); library(RWeka); library(textreg); 
library(wordcloud); library(qdap); library(dplyr); library(foreach); library(hash); library(stringi); library(data.table); library(quanteda); library(slam)

#setwd("C:/Users/Peyman/Dropbox/Coursera/NLP project/benchmark")

setwd("C:/Users/heidarip/Dropbox/Coursera/NLP project/clean/Model")

load(file="D1s2.Rda")
load(file="D2s2.Rda")
load(file="D3s2.Rda")
load(file="unigram_idx2.Rda")
load(file="bigram_idx2.Rda")
load(file="trigram_idx2.Rda")
load(file="fourgram_idx2.Rda")
load(file="dictionary2.Rda")
load(file="dictionary_invert2.Rda")
.set( dictionary, keys="notseenword", values=1000000 )
.set( dictionary_invert, keys="1000000", values="notseenword" )
setkey(bigram_idx, word_1, prediction)
setkey(trigram_idx, word_2, word_1, prediction)
setkey(fourgram_idx, word_3, word_2, word_1, prediction)



unigram_kesner_Ney <- function(bigram_table){
  unique_combinations <- length(unique(bigram_table$ngrams))
  unigram_kn <- bigram_table[ ,c("X2", "freq"), with=FALSE]
  colnames(unigram_kn) <- c( "ngrams_kn", "prob_kn")
  grouped_bigrams<- tapply(bigram_table$X1,bigram_table$X2, length)
  grouped_bigrams <- grouped_bigrams[order(grouped_bigrams, decreasing = T)]
  unigram_kn <- unigram_kn[1:length(names(grouped_bigrams)),]
  unigram_kn$ngrams_kn <- names(grouped_bigrams)
  unigram_kn$prob_kn <- grouped_bigrams/unique_combinations
  singlewords<- grepl(" ", unigram_kn$ngrams_kn)
  unigram_kn <- unigram_kn[!singlewords,]
  unigram_kn <- unigram_kn[!duplicated(unigram_kn$ngrams_kn),]
  return(unigram_kn)
}


bigram_kesner_Ney <- function(bigram_idx_sp,unigram_idx, gamma_bi){
  bigram_kn <- bigram_idx_sp
  unigram_bi_kn <- unigram_idx[which(unigram_idx$prediction %in%bigram_idx_sp$prediction),]
  bigram_kn<- merge(bigram_kn, unigram_bi_kn, by=c("prediction"))
  bigram_kn$bi_prob_kn <- bigram_kn$part1+gamma_bi*bigram_kn$uni_prob_kn
  return(bigram_kn)
}

trigram_kesner_Ney <- function(trigram_idx_sp, bigram_kn,gamma_tri){
  trigram_kn <- trigram_idx_sp
  bigram_tri_kn <- bigram_kn
  bigram_tri_kn <- bigram_tri_kn[which((bigram_tri_kn$prediction %in%trigram_idx_sp$prediction) &(bigram_tri_kn$word_1 %in%trigram_idx_sp$word_1)),]
  bigram_tri_kn<- bigram_tri_kn[,c("word_1","prediction", "bi_prob_kn"), with=FALSE]
  trigram_kn<- merge(trigram_kn, bigram_tri_kn, by=c("prediction", "word_1"))
  trigram_kn$tri_prob_kn <- trigram_kn$part1+gamma_tri*trigram_kn$bi_prob_kn
  return(trigram_kn)
}


fourgram_kesner_Ney <- function(fourgram_idx_sp, trigram_kn,gamma_four){
  fourgram_kn <- fourgram_idx_sp
  trigram_four_kn <- trigram_kn
  trigram_four_kn <- trigram_four_kn[(trigram_four_kn$prediction %in%fourgram_idx_sp$prediction)
                                     &(trigram_four_kn$word_1 %in%fourgram_idx_sp$word_1)
                                     &(trigram_four_kn$word_2 %in%fourgram_idx_sp$word_2)]
  trigram_four_kn <- trigram_four_kn[,c("word_2","word_1","prediction", "tri_prob_kn"), with=FALSE]
  fourgram_kn<- merge(fourgram_kn, trigram_four_kn, by=c("prediction", "word_1","word_2"))
  fourgram_kn$four_prob_kn <- fourgram_kn$part1+gamma_four*fourgram_kn$tri_prob_kn
  return(fourgram_kn)
}

profanity_words <- function(corpus) {
  profanity1 <- read.csv(file="profanity.csv", sep=",",header=FALSE, stringsAsFactors=FALSE)
  profanity1 <- profanity1$V1
  profanity1 <- str_trim(profanity1)
  return(profanity1)
}

quant_cleaner<- function(x){
  cleaned_x <- tokenize(x, what = c("word"), removeURL = T, removeNumbers = T, removePunct = T,removeSymbols = T, removeSeparators = TRUE, removeTwitter = T, removeHyphens = T, ngrams = 1L, skip = 0L, concatenator = "_", simplify = FALSE, verbose = FALSE)
  return(cleaned_x)
}




prob_finder <- function(words_index, number_of_words){
  final_input1 <- "" ; final_input2 <- "" ; final_input3 <- "";
  
  output <- ""
  
  in_four <- 0; in_tri <- 0; in_bi <- 0 ; 
  
  if (number_of_words >= 3){
    final_input3 <- words_index[number_of_words-2]
    final_input2 <- words_index[number_of_words-1]
    final_input1 <- words_index[number_of_words]
    fourgram_exist   <-fourgram_idx[J(final_input3, final_input2, final_input1), nomatch=0]
    trigram_exist   <-trigram_idx[J(final_input2, final_input1), nomatch=0]
    bigram_exist   <-bigram_idx[J(final_input1), nomatch=0]
    in_four <- dim(fourgram_exist)[1]
    in_tri <- dim(trigram_exist)[1] 
    in_bi <- dim(bigram_exist)[1]
  }else if (number_of_words == 2){
    final_input2 <- words_index[number_of_words-1]
    final_input1 <- words_index[number_of_words]
    trigram_exist   <-trigram_idx[J(final_input2, final_input1), nomatch=0]
    bigram_exist   <-bigram_idx[J(final_input1), nomatch=0] 
    in_tri <- dim(trigram_exist)[1] 
    in_bi <- dim(bigram_exist)[1]
  }else if (number_of_words ==1 ){
    final_input1 <- words_index[number_of_words]
    bigram_exist   <-bigram_idx[J(final_input1), nomatch=0]
    in_bi <- dim(bigram_exist)[1]
  }
  
  output_dummy <- data.table(prediction=c(dictionary[["the"]],dictionary[["and"]], dictionary[["a"]]), prob=c(0.01, 0.009, 0.008))
  
  if (number_of_words >= 3 & in_four>0){
    
    sum_pre_bi <- sum(bigram_exist$freq)
    sum_pre_tri <- sum(trigram_exist$freq)
    sum_pre_four <- sum(fourgram_exist$freq)
    one_pre_bi <- bigram_exist[freq==1]
    one_pre_tri <- trigram_exist[freq==1]
    one_pre_four <- fourgram_exist[freq==1]
    two_pre_bi <- bigram_exist[freq==2]
    two_pre_tri <- trigram_exist[freq==2]
    two_pre_four <- fourgram_exist[freq==2]
    three_pre_bi <- bigram_exist[freq>=3]
    three_pre_tri <- trigram_exist[freq>=3]
    three_pre_four <- fourgram_exist[freq>=3]
    N1_bi <- dim(one_pre_bi)[1]
    N1_tri <- dim(one_pre_tri)[1]
    N1_four <- dim(one_pre_four)[1]
    N2_bi <- dim(two_pre_bi)[1]
    N2_tri <- dim(two_pre_tri)[1]
    N2_four <- dim(two_pre_four)[1]
    N3_bi <- dim(three_pre_bi)[1]
    N3_tri <- dim(three_pre_tri)[1]
    N3_four <- dim(three_pre_four)[1]
    
    gamma_bi <- (D1s[[1]]*N1_bi+D2s[[1]]*N2_bi+D3s[[1]]*N3_bi)/sum_pre_bi
    gamma_tri <- (D1s[[2]]*N1_tri+D2s[[2]]*N2_tri+D3s[[2]]*N3_tri)/sum_pre_tri
    gamma_four <- (D1s[[3]]*N1_four+D2s[[3]]*N2_four+D3s[[3]]*N3_four)/sum_pre_four
    one_pre_bi$part1 <- (one_pre_bi$freq-D1s[[1]])/sum_pre_bi
    one_pre_tri$part1 <- (one_pre_tri$freq-D1s[[2]])/sum_pre_tri
    one_pre_four$part1 <- (one_pre_four$freq-D1s[[3]])/sum_pre_four
    
    two_pre_bi$part1 <- (two_pre_bi$freq-D2s[[1]])/sum_pre_bi
    two_pre_tri$part1 <- (two_pre_tri$freq-D2s[[2]])/sum_pre_tri
    two_pre_four$part1 <- (two_pre_four$freq-D2s[[3]])/sum_pre_four
    
    three_pre_bi$part1 <- (three_pre_bi$freq-D3s[[1]])/sum_pre_bi
    three_pre_tri$part1 <- (three_pre_tri$freq-D3s[[2]])/sum_pre_tri
    three_pre_four$part1 <- (three_pre_four$freq-D3s[[3]])/sum_pre_four
    bigram_idx_sp <- rbind(one_pre_bi,two_pre_bi,three_pre_bi)
    trigram_idx_sp <- rbind(one_pre_tri,two_pre_tri,three_pre_tri)
    fourgram_idx_sp <- rbind(one_pre_four,two_pre_four,three_pre_four)
    
    bigram_kn <- bigram_kesner_Ney(bigram_idx_sp,unigram_idx, gamma_bi)
    trigram_kn <- trigram_kesner_Ney(trigram_idx_sp, bigram_kn,gamma_tri)
    fourgram_kn <- fourgram_kesner_Ney(fourgram_idx_sp, trigram_kn,gamma_four)
    results <- fourgram_kn[,c("prediction", "four_prob_kn"), with=FALSE]
    results <- results[!duplicated(results$prediction),]
    input_df <- data.table(word_3 = rep(words_index[1], dim(results)[1]), word_2 = rep(words_index[2], dim(results)[1]), word_1 = rep(words_index[3], dim(results)[1]))
    gamma_four_df <- data.table(gamma_four = rep(gamma_four, dim(results)[1]))
    output <- cbind(input_df, results, gamma_four_df)
  }else if (number_of_words >= 2  & in_tri>0){
    sum_pre_bi <- sum(bigram_exist$freq)
    sum_pre_tri <- sum(trigram_exist$freq)
    one_pre_bi <- bigram_exist[freq==1]
    one_pre_tri <- trigram_exist[freq==1]
    two_pre_bi <- bigram_exist[freq==2]
    two_pre_tri <- trigram_exist[freq==2]
    three_pre_bi <- bigram_exist[freq>=3]
    three_pre_tri <- trigram_exist[freq>=3]
    N1_bi <- dim(one_pre_bi)[1]
    N1_tri <- dim(one_pre_tri)[1]
    N2_bi <- dim(two_pre_bi)[1]
    N2_tri <- dim(two_pre_tri)[1]
    N3_bi <- dim(three_pre_bi)[1]
    N3_tri <- dim(three_pre_tri)[1]
    
    gamma_bi <- (D1s[[1]]*N1_bi+D2s[[1]]*N2_bi+D3s[[1]]*N3_bi)/sum_pre_bi
    gamma_tri <- (D1s[[2]]*N1_tri+D2s[[2]]*N2_tri+D3s[[2]]*N3_tri)/sum_pre_tri
    one_pre_bi$part1 <- (one_pre_bi$freq-D1s[[1]])/sum_pre_bi
    one_pre_tri$part1 <- (one_pre_tri$freq-D1s[[2]])/sum_pre_tri
    
    two_pre_bi$part1 <- (two_pre_bi$freq-D2s[[1]])/sum_pre_bi
    two_pre_tri$part1 <- (two_pre_tri$freq-D2s[[2]])/sum_pre_tri
    
    three_pre_bi$part1 <- (three_pre_bi$freq-D3s[[1]])/sum_pre_bi
    three_pre_tri$part1 <- (three_pre_tri$freq-D3s[[2]])/sum_pre_tri
    bigram_idx_sp <- rbind(one_pre_bi,two_pre_bi,three_pre_bi)
    trigram_idx_sp <- rbind(one_pre_tri,two_pre_tri,three_pre_tri)
    
    bigram_kn <- bigram_kesner_Ney(bigram_idx_sp,unigram_idx, gamma_bi)
    trigram_kn <- trigram_kesner_Ney(trigram_idx_sp, bigram_kn,gamma_tri)
    results <- trigram_kn[,c("prediction", "tri_prob_kn"), with=FALSE]
    results <- results[!duplicated(results$prediction),]
    input_df <- data.table(word_2 = rep(words_index[1], dim(results)[1]), word_1 = rep(words_index[2], dim(results)[1]))
    gamma_tri_df <- data.table(gamma_tri = rep(gamma_tri, dim(results)[1]))
    output <- cbind(input_df, results, gamma_tri_df)
  } else if (number_of_words >= 1 & in_bi>0){
    sum_pre_bi <- sum(bigram_exist$freq)
    one_pre_bi <- bigram_exist[freq==1]
    two_pre_bi <- bigram_exist[freq==2]
    three_pre_bi <- bigram_exist[freq>=3]
    N1_bi <- dim(one_pre_bi)[1]
    N2_bi <- dim(two_pre_bi)[1]
    N3_bi <- dim(three_pre_bi)[1]
    
    gamma_bi <- (D1s[[1]]*N1_bi+D2s[[1]]*N2_bi+D3s[[1]]*N3_bi)/sum_pre_bi
    one_pre_bi$part1 <- (one_pre_bi$freq-D1s[[1]])/sum_pre_bi
    
    two_pre_bi$part1 <- (two_pre_bi$freq-D2s[[1]])/sum_pre_bi
    
    three_pre_bi$part1 <- (three_pre_bi$freq-D3s[[1]])/sum_pre_bi
    bigram_idx_sp <- rbind(one_pre_bi,two_pre_bi,three_pre_bi)
    
    bigram_kn <- bigram_kesner_Ney(bigram_idx_sp,unigram_idx, gamma_bi)
    results <- bigram_kn[,c("prediction", "bi_prob_kn"), with=FALSE]
    results <- results[!duplicated(results$prediction),]
    input_df <- data.table(word_1 = rep(words_index[1], dim(results)[1]))
    gamma_bi_df <- data.table(gamma_bi = rep(gamma_bi, dim(results)[1]))
    output <- cbind(input_df, results, gamma_bi_df)
  }
  return(output) 
}



bigram_idx_processed <- data.table(data.frame(word_1=numeric(), prediction= numeric(), bi_prob_kn= numeric(), gamma_bi=numeric() ))
uniq_bi_indexes <- unique(bigram_idx[,c("word_1"), with=FALSE])
for (ii in 1:dim(uniq_bi_indexes)[1]){
  number_of_words <- 1
  words_index <- uniq_bi_indexes$word_1[ii]
  prob_gamma <- prob_finder(words_index, number_of_words)
  bigram_idx_processed <- rbind(bigram_idx_processed, prob_gamma)
  print(ii/dim(uniq_bi_indexes)[1])
}
setkey(bigram_idx_processed, word_1, prediction)
save(bigram_idx_processed, file="bigram_idx_processed.Rda")



trigram_idx_processed <- data.table(data.frame(word_2=numeric(), word_1=numeric(), prediction= numeric(), tri_prob_kn= numeric(), gamma_tri=numeric() ))
uniq_tri_indexes <- unique(trigram_idx[,c("word_2", "word_1"), with=FALSE])
for (ii in 1:dim(uniq_tri_indexes)[1]){
  number_of_words <- 2
  words_index <- c(uniq_tri_indexes$word_2[ii], uniq_tri_indexes$word_1[ii])
  prob_gamma <- prob_finder(words_index, number_of_words)
  trigram_idx_processed <- rbind(trigram_idx_processed, prob_gamma)
  print(ii/dim(uniq_tri_indexes)[1])
}
setkey(trigram_idx_processed, word_2, word_1, prediction)
save( trigram_idx_processed,file="trigram_idx_processed.Rda")



fourgram_idx_processed <- data.table(data.frame(word_3=numeric(), word_2=numeric(), word_1=numeric(), prediction= numeric(), four_prob_kn= numeric(), gamma_four=numeric() ))
uniq_four_indexes <- unique(fourgram_idx[,c("word_3", "word_2", "word_1"), with=FALSE])
for (ii in 1:dim(uniq_four_indexes)[1]){
  number_of_words <- 3
  words_index <- c(uniq_four_indexes$word_3[ii], uniq_four_indexes$word_2[ii], uniq_four_indexes$word_1[ii])
  prob_gamma <- prob_finder(words_index, number_of_words)
  fourgram_idx_processed <- rbind(fourgram_idx_processed, prob_gamma)
  print(ii/dim(uniq_four_indexes)[1])
}
setkey(fourgram_idx_processed, word_3, word_2, word_1, prediction)
save( fourgram_idx_processed,file="fourgram_idx_processed.Rda")

