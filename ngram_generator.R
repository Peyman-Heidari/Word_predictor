set.seed(777)
setwd("C:/Users/Peyman/Dropbox/Coursera/NLP project/Office PC/skipgramsOff")
#setwd("C:/Users/heidarip/Dropbox/Coursera/NLP project/Office PC/skipgramsOff")
library(stringr); library(tm); library(ngram); library(RColorBrewer); library(RWeka); library(textreg); 
library(wordcloud); library(qdap); library(dplyr); library(foreach); library(hash); library(stringi); library(data.table); library(quanteda); library(slam)




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
    bigram_tri_kn <- bigram_tri_kn[which((bigram_kn$prediction %in%trigram_idx_sp$prediction) &(bigram_tri_kn$word_1 %in%trigram_idx_sp$word_1)),]
    bigram_tri_kn<- bigram_tri_kn[,c("word_1","prediction", "bi_prob_kn"), with=FALSE]
    trigram_kn<- merge(trigram_kn, bigram_tri_kn, by=c("prediction", "word_1"))
    trigram_kn$tri_prob_kn <- trigram_kn$part1+gamma_tri*trigram_kn$bi_prob_kn
    return(trigram_kn)
}


fourgram_kesner_Ney <- function(fourgram_idx_sp, trigram_kn,gamma_four){
    fourgram_kn <- fourgram_idx_sp
    trigram_four_kn <- trigram_kn
    trigram_four_kn <- trigram_four_kn[(trigram_kn$prediction %in%fourgram_idx_sp$prediction)
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


blogcon <- file('en_US.blogs.txt', encoding = "UTF-8")
blog <- readLines(blogcon, encoding = "UTF-8")
blog <-  stri_trans_general(blog ,"Latin-ASCII")
close(blogcon)
newscon <- file('en_US.news.txt', encoding = "UTF-8")
news <- readLines(newscon, encoding = "UTF-8")
news <-  stri_trans_general(news ,"Latin-ASCII")
close(newscon)
twittcon <- file('en_US.twitter.txt', encoding = "UTF-8")
twitter <- readLines(twittcon, encoding = "UTF-8")
twitter <-  stri_trans_general(twitter ,"Latin-ASCII")
close(twittcon)
n1 <- length(blog)
n2 <- length(news)
n3 <- length(twitter)
sample1 <- blog[sample(1:n1, n1 * 0.85, replace=FALSE)]
sample2 <- news[sample(1:n2, n2 * 0.85, replace=FALSE)]
sample3 <- twitter[sample(1:n3, n3 * 0.2, replace=FALSE)]
Data <- c(sample1,sample2,sample3)
total_lines <- length(Data)
intrain <- sample(1:total_lines, total_lines * 0.7, replace=FALSE)

print(Sys.time())
print(0)
train<-Data[intrain]
test<-Data[-intrain]
profanity <- profanity_words()

print(Sys.time())
print(1)
train <- paste(train, collapse=" ")
contraction <- qdapDictionaries::contractions
for (ii in 1:dim(contraction)[1]){
    train <- gsub(contraction$contraction[ii], contraction$expanded[ii], train)
}
train <- gsub('http\\S+\\s*', '', train)
train <- toLower(train)
train <- gsub("[^A-Za-z ]", "", train)
train <- quant_cleaner(train)
train <- unlist(train)
train <- paste(train, collapse=" ")

print(Sys.time())
print(2)
save(train, profanity, file="train2.Rda")
rm(list = setdiff(ls(), lsf.str()))
load(file="train2.Rda")

unigram <- dfm(train,ngram=1, skip=0)
unigram_table <- data.frame(ngrams = features(unigram), freq = colSums(unigram), 
                            row.names = NULL, stringsAsFactors = FALSE)
unigram_table <- unigram_table[which(!(unigram_table$ngrams %in% profanity)),]

rm(list=c("unigram"))




bigram <- ngram(train, n=2)
bigram_table <- get.phrasetable(bigram)
bigram_table$ngrams <- gsub(" $","", bigram_table$ngrams, perl=T)
splited_bi<- data.frame(str_split_fixed(bigram_table$ngrams, " ", 2))
bigram_table<- cbind(bigram_table,splited_bi)
bigram_table$X1 <- as.character(bigram_table$X1)
bigram_table$X2 <- as.character(bigram_table$X2)
bigram_table <- bigram_table[which(!(bigram_table$X1 %in% profanity | bigram_table$X2 %in% profanity)),]

rm(list=c("bigram", "splited_bi"))


trigram <- ngram(train, n=3)
trigram_table <- get.phrasetable(trigram)
trigram_table$ngrams <- gsub(" $","", trigram_table$ngrams, perl=T)
splited_tri<- data.frame(str_split_fixed(trigram_table$ngrams, " ", 3))
trigram_table<- cbind(trigram_table,splited_tri)
trigram_table$X1 <- as.character(trigram_table$X1)
trigram_table$X2 <- as.character(trigram_table$X2)
trigram_table$X3 <- as.character(trigram_table$X3)
trigram_table <- trigram_table[which(!(trigram_table$X1 %in% profanity | trigram_table$X2 %in% profanity |
                                         trigram_table$X3 %in% profanity)),]

rm(list=c("trigram", "splited_tri"))


fourgram <- ngram(train, n=4)
fourgram_table <- get.phrasetable(fourgram)
fourgram_table$ngrams <- gsub(" $","", fourgram_table$ngrams, perl=T)
splited_four<- data.frame(str_split_fixed(fourgram_table$ngrams, " ", 4))
fourgram_table<- cbind(fourgram_table,splited_four)
fourgram_table$X1 <- as.character(fourgram_table$X1)
fourgram_table$X2 <- as.character(fourgram_table$X2)
fourgram_table$X3 <- as.character(fourgram_table$X3)
fourgram_table$X4 <- as.character(fourgram_table$X4)
fourgram_table <- fourgram_table[which(!(fourgram_table$X1 %in% profanity | fourgram_table$X2 %in% profanity |
                                             fourgram_table$X3 %in% profanity | fourgram_table$X4 %in% profanity)),]

rm(list=c("fourgram", "splited_four"))


save(unigram_table,bigram_table,trigram_table,fourgram_table, file="ngramsRAW.Rda")
rm(list = setdiff(ls(), lsf.str()))
load(file="ngramsRAW.Rda")




one_bigram <- bigram_table[bigram_table$freq==1,]
n1_bi <- dim(one_bigram)[1]
two_bigram <- bigram_table[bigram_table$freq==2,]
n2_bi <- dim(two_bigram)[1]
three_bigram <- bigram_table[bigram_table$freq==3,]
n3_bi <- dim(three_bigram)[1]
four_bigram <- bigram_table[bigram_table$freq==4,]
n4_bi <- dim(four_bigram)[1]
Y_bi <- (n1_bi)/(n1_bi+2*n2_bi)
D0_bi <- 0; D1_bi <- 1-(2*Y_bi)*(n2_bi/n1_bi); D2_bi <- 2-(3*Y_bi)*(n3_bi/n2_bi); D3_bi<- 3-(4*Y_bi)*(n4_bi/n3_bi)

one_trigram <- trigram_table[trigram_table$freq==1,]
n1_tri <- dim(one_trigram)[1]
two_trigram <- trigram_table[trigram_table$freq==2,]
n2_tri <- dim(two_trigram)[1]
three_trigram <- trigram_table[trigram_table$freq==3,]
n3_tri <- dim(three_trigram)[1]
four_trigram <- trigram_table[trigram_table$freq==4,]
n4_tri <- dim(four_trigram)[1]
Y_tri <- (n1_tri)/(n1_tri+2*n2_tri)
D0_tri <- 0; D1_tri <- 1-(2*Y_tri)*(n2_tri/n1_tri); D2_tri <- 2-(3*Y_tri)*(n3_tri/n2_tri); D3_tri <- 3-(4*Y_tri)*(n4_tri/n3_tri)

one_fourgram <- fourgram_table[fourgram_table$freq==1,]
n1_four <- dim(one_fourgram)[1]
two_fourgram <- fourgram_table[fourgram_table$freq==2,]
n2_four <- dim(two_fourgram)[1]
three_fourgram <- fourgram_table[fourgram_table$freq==3,]
n3_four <- dim(three_fourgram)[1]
four_fourgram <- fourgram_table[fourgram_table$freq==4,]
n4_four <- dim(four_fourgram)[1]
Y_four <- (n1_four)/(n1_four+2*n2_four)
D0_four <- 0; D1_four <- 1-(2*Y_four)*(n2_four/n1_four); D2_four <- 2-(3*Y_four)*(n3_four/n2_four); D3_four <- 3-(4*Y_four)*(n4_four/n3_four)

D1s <- list(D1_bi, D1_tri, D1_four)
D2s <- list(D2_bi, D2_tri, D2_four)
D3s <- list(D3_bi, D3_tri, D3_four)

print(Sys.time())
print(4.3)

unigram_table <- as.data.table(unigram_table)
bigram_table <- as.data.table(bigram_table)
trigram_table <- as.data.table(trigram_table)
fourgram_table <- as.data.table(fourgram_table)

unigram_table <- unigram_table[freq>1]
bigram_table <- bigram_table[which(bigram_table$X1 %in% unigram_table$ngrams),]
bigram_table <- bigram_table[which(bigram_table$X2 %in% unigram_table$ngrams),]

print(Sys.time())


unigram_kn <- unigram_kesner_Ney(bigram_table)
unigram_kn <- unigram_kn[order(unigram_kn$ngrams),]
all_words <- unigram_kn$ngrams_kn
all_words <- all_words[order(all_words)]
dictionary<- hash(all_words, 1:length(all_words))
dictionary_invert <- invert(dictionary) 


bigram_table <- bigram_table[bigram_table$freq>1,]
trigram_table <- trigram_table[trigram_table$freq>1,]
fourgram_table <- fourgram_table[fourgram_table$freq>1,]


unigram_index <- unigram_kn
unigram_index <- unigram_index[order(unigram_index$ngrams),]
uni_keys <- unigram_index$ngrams_kn
indexed_key <- values(dictionary[uni_keys])
indexed_df <- data.frame(words= names(indexed_key), index=indexed_key)
uni_keys <- data.frame(words=uni_keys)
temp<- merge(uni_keys,indexed_df, by=c("words"))
unigram_index$ngrams_kn_idx <- temp$index
unigram_index<- unigram_index[,c("ngrams_kn_idx","prob_kn"), with=FALSE]

print(Sys.time())
print(4.3)

bigram_index <- bigram_table
bigram_index <- bigram_index[which(bigram_index$X1 %in% unigram_kn$ngrams),]
bigram_index <- bigram_index[which(bigram_index$X2 %in% unigram_kn$ngrams),]
bigram_index$X1 <- as.character(bigram_index$X1) 
bigram_index$X2 <- as.character(bigram_index$X2)

bigram_index <- bigram_index[order(bigram_index$X1),]
keys <- bigram_index$X1
indexed_key <- values(dictionary[keys])
indexed_df <- data.frame(words= names(indexed_key), index=indexed_key)
keys <- data.frame(words=keys)
temp<- merge(keys,indexed_df, by=c("words"))
bigram_index$ngrams_X1_idx <- temp$index


bigram_index <- bigram_index[order(bigram_index$X2),]
keys <- bigram_index$X2
indexed_key <- values(dictionary[keys])
indexed_df <- data.frame(words= names(indexed_key), index=indexed_key)
keys <- data.frame(words=keys)
temp<- merge(keys,indexed_df, by=c("words"))
bigram_index$ngrams_X2_idx <- temp$index

print(Sys.time())
print(4.4)

trigram_index <- trigram_table
trigram_index <- trigram_index[which(trigram_index$X1 %in% unigram_kn$ngrams),]
trigram_index <- trigram_index[which(trigram_index$X2 %in% unigram_kn$ngrams),]
trigram_index <- trigram_index[which(trigram_index$X3 %in% unigram_kn$ngrams),]
trigram_index$X1 <- as.character(trigram_index$X1) 
trigram_index$X2 <- as.character(trigram_index$X2)
trigram_index$X3 <- as.character(trigram_index$X3)

trigram_index <- trigram_index[order(trigram_index$X1),]
keys <- trigram_index$X1
indexed_key <- values(dictionary[keys])
indexed_df <- data.frame(words= names(indexed_key), index=indexed_key)
keys <- data.frame(words=keys)
temp<- merge(keys,indexed_df, by=c("words"))
trigram_index$ngrams_X1_idx <- temp$index

trigram_index <- trigram_index[order(trigram_index$X2),]
keys <- trigram_index$X2
indexed_key <- values(dictionary[keys])
indexed_df <- data.frame(words= names(indexed_key), index=indexed_key)
keys <- data.frame(words=keys)
temp<- merge(keys,indexed_df, by=c("words"))
trigram_index$ngrams_X2_idx <- temp$index

trigram_index <- trigram_index[order(trigram_index$X3),]
keys <- trigram_index$X3
indexed_key <- values(dictionary[keys])
indexed_df <- data.frame(words= names(indexed_key), index=indexed_key)
keys <- data.frame(words=keys)
temp<- merge(keys,indexed_df, by=c("words"))
trigram_index$ngrams_X3_idx <- temp$index


print(Sys.time())
print(4.5)

fourgram_index <- fourgram_table
fourgram_index <- fourgram_index[which(fourgram_index$X1 %in% unigram_kn$ngrams),]
fourgram_index <- fourgram_index[which(fourgram_index$X2 %in% unigram_kn$ngrams),]
fourgram_index <- fourgram_index[which(fourgram_index$X3 %in% unigram_kn$ngrams),]
fourgram_index <- fourgram_index[which(fourgram_index$X4 %in% unigram_kn$ngrams),]
fourgram_index$X1 <- as.character(fourgram_index$X1) 
fourgram_index$X2 <- as.character(fourgram_index$X2)
fourgram_index$X3 <- as.character(fourgram_index$X3)
fourgram_index$X4 <- as.character(fourgram_index$X4)

fourgram_index <- fourgram_index[order(fourgram_index$X1),]
keys <- fourgram_index$X1
indexed_key <- values(dictionary[keys])
indexed_df <- data.frame(words= names(indexed_key), index=indexed_key)
keys <- data.frame(words=keys)
temp<- merge(keys,indexed_df, by=c("words"))
fourgram_index$ngrams_X1_idx <- temp$index

fourgram_index <- fourgram_index[order(fourgram_index$X2),]
keys <- fourgram_index$X2
indexed_key <- values(dictionary[keys])
indexed_df <- data.frame(words= names(indexed_key), index=indexed_key)
keys <- data.frame(words=keys)
temp<- merge(keys,indexed_df, by=c("words"))
fourgram_index$ngrams_X2_idx <- temp$index

fourgram_index <- fourgram_index[order(fourgram_index$X3),]
keys <- fourgram_index$X3
indexed_key <- values(dictionary[keys])
indexed_df <- data.frame(words= names(indexed_key), index=indexed_key)
keys <- data.frame(words=keys)
temp<- merge(keys,indexed_df, by=c("words"))
fourgram_index$ngrams_X3_idx <- temp$index

fourgram_index <- fourgram_index[order(fourgram_index$X4),]
keys <- fourgram_index$X4
indexed_key <- values(dictionary[keys])
indexed_df <- data.frame(words= names(indexed_key), index=indexed_key)
keys <- data.frame(words=keys)
temp<- merge(keys,indexed_df, by=c("words"))
fourgram_index$ngrams_X4_idx <- temp$index

print(Sys.time())

unigram_idx <- unigram_index
names(unigram_idx)<- c("prediction","uni_prob_kn")

bigram_idx <- bigram_index[,c("ngrams_X1_idx","ngrams_X2_idx","freq"), with=FALSE]
colnames(bigram_idx)<- c("word_1", "prediction", "freq")

trigram_idx <- trigram_index[,c("ngrams_X1_idx","ngrams_X2_idx","ngrams_X3_idx","freq"), with=FALSE]
colnames(trigram_idx)<- c("word_2", "word_1","prediction", "freq")

fourgram_idx <- fourgram_index[,c("ngrams_X1_idx","ngrams_X2_idx","ngrams_X3_idx", "ngrams_X4_idx", "freq"), with=FALSE]
colnames(fourgram_idx)<- c("word_3", "word_2","word_1","prediction", "freq")

unigram_idx$prediction <- as.numeric(unigram_idx$prediction)
bigram_idx$word_1 <- as.numeric(bigram_idx$word_1)
bigram_idx$prediction <- as.numeric(bigram_idx$prediction)
trigram_idx$word_2 <- as.numeric(trigram_idx$word_2)
trigram_idx$word_1 <- as.numeric(trigram_idx$word_1)
trigram_idx$prediction <- as.numeric(trigram_idx$prediction)
fourgram_idx$word_3 <- as.numeric(fourgram_idx$word_3)
fourgram_idx$word_2 <- as.numeric(fourgram_idx$word_2)
fourgram_idx$word_1 <- as.numeric(fourgram_idx$word_1)
fourgram_idx$prediction <- as.numeric(fourgram_idx$prediction)



words_considered <- keys(dictionary)

unigram_idx <- as.data.table(unigram_idx)
bigram_idx <- as.data.table(bigram_idx)
trigram_idx <- as.data.table(trigram_idx)
fourgram_idx <- as.data.table(fourgram_idx)
unigram_idx <- unigram_idx[order(prediction)]
bigram_idx <- bigram_idx[order(word_1,prediction)]
trigram_idx <- trigram_idx[order(word_2, word_1,prediction)]
fourgram_idx <- fourgram_idx[order(word_3, word_2, word_1,prediction)]
setkey(unigram_idx,prediction)
setkey(bigram_idx,word_1)
setkey(trigram_idx,word_2, word_1)
setkey(fourgram_idx,word_3,word_2, word_1)

save(D1s,file="D1s2.Rda")
save(D2s,file="D2s2.Rda")
save(D3s,file="D3s2.Rda")
save(unigram_idx,file="unigram_idx2.Rda")
save(bigram_idx,file="bigram_idx2.Rda")
save(trigram_idx,file="trigram_idx2.Rda")
save(fourgram_idx,file="fourgram_idx2.Rda")
save(dictionary, file="dictionary2.Rda")
save(dictionary_invert, file="dictionary_invert2.Rda")
save(words_considered, file="words_considered2.Rda")
