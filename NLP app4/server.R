library(hash); library(data.table); library(quanteda); library(qdap)


load(file="unigram_idx2.Rda")
load(file="bigram_idx_processed.Rda")
load(file="trigram_idx_processed.Rda")
load(file="fourgram_idx_processed.Rda")
load(file="dictionary2.Rda")
load(file="dictionary_invert2.Rda")
.set( dictionary, keys="notseenword", values=1000000 )
.set( dictionary_invert, keys="1000000", values="notseenword" )

output_dummy <- data.table(prediction=c(dictionary[["the"]],dictionary[["and"]],
                                        dictionary[["a"]],dictionary[["be"]],
                                        dictionary[["to"]]),
                           prob=c(0.001, 0.0009, 0.0008, 0.0007, 0.0006))

quant_cleaner<- function(x){
  cleaned_x <- tokenize(x, what = c("word"), removeURL = T, removeNumbers = T,
                        removePunct = T,removeSymbols = T,
                        removeSeparators = TRUE, removeTwitter = T,
                        removeHyphens = T, ngrams = 1L, skip = 0L, 
                        concatenator = "_", simplify = FALSE, verbose = FALSE)
  return(cleaned_x)
}


finder <- function(phrase){
  input <- phrase
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
    fourgram_exist   <-fourgram_idx_processed[J(final_input3, final_input2,
                                                final_input1), nomatch=0]
    trigram_exist   <-trigram_idx_processed[J(final_input2, final_input1),
                                            nomatch=0]
    bigram_exist   <-bigram_idx_processed[J(final_input1), nomatch=0]
    in_four <- dim(fourgram_exist)[1]
    in_tri <- dim(trigram_exist)[1] 
    in_bi <- dim(bigram_exist)[1]
  }else if (number_of_words == 2){
    final_input2 <- words_index[number_of_words-1]
    final_input1 <- words_index[number_of_words]
    trigram_exist   <-trigram_idx_processed[J(final_input2, final_input1),
                                            nomatch=0]
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
    bigram_exist$four_prob_kn <- 
        bigram_exist$bi_prob_kn * (gamma_tri * gamma_four)
    results <- fourgram_exist[,c("prediction", "four_prob_kn"), with=FALSE]
    results <- rbind(results, trigram_exist[,c("prediction", "four_prob_kn"),
                                            with=FALSE])
    results <- rbind(results, bigram_exist[,c("prediction", "four_prob_kn"),
                                           with=FALSE])
    colnames(results) <- c("prediction", "prob")
    results <- rbind(results, output_dummy)
    results <- results[order(results$prob, decreasing = T),]
    results <- results[!duplicated(results$prediction),]
    output <- head(results$prediction,5)
  }else if (number_of_words >= 2  & in_tri>0){
    gamma_tri <- trigram_exist$gamma_tri[1]
    gamma_bi <- bigram_exist$gamma_bi[1]
    bigram_exist$tri_prob_kn <- bigram_exist$bi_prob_kn * gamma_tri 
    results <- trigram_exist[,c("prediction", "tri_prob_kn"), with=FALSE]
    results <- rbind(results, bigram_exist[,c("prediction", "tri_prob_kn"),
                                           with=FALSE])
    colnames(results) <- c("prediction", "prob")
    results <- rbind(results, output_dummy)
    results <- results[order(results$prob, decreasing = T),]
    results <- results[!duplicated(results$prediction),]
    output <- head(results$prediction,5)
  } else if (number_of_words >= 1 & in_bi>0){
    gamma_bi <- bigram_exist$gamma_bi[1]
    unigram_idx$bi_prob_kn <- unigram_idx$uni_prob_kn * gamma_bi
    results <- bigram_exist[,c("prediction", "bi_prob_kn"), with=FALSE]
    results <- rbind(results, unigram_idx[,c("prediction", "bi_prob_kn"),
                                          with=FALSE])
    colnames(results) <- c("prediction", "prob")
    results <- rbind(results, output_dummy)
    results <- results[order(results$prob, decreasing = T),]
    results <- results[!duplicated(results$prediction),]
    output <- head(results$prediction,5)
  }else {
    results <- output_dummy
    output <- head(results$prediction,5)} 
  
  output <- as.character(output)
  for (ii in 1:length(output)) {output[ii] <- dictionary_invert[[output[ii]]]}
  output <- paste(output, collapse = "-")
  return(output)
}


shinyServer(function(input, output) {
    all_words <- reactive({ finder(input$input) }) 
  output$next_word1 <- renderText({ strsplit(all_words(), "-")[[1]][1] })
  output$next_word2 <- renderText({ strsplit(all_words(), "-")[[1]][2] })
  output$next_word3 <- renderText({ strsplit(all_words(), "-")[[1]][3] })
  output$next_word4 <- renderText({ strsplit(all_words(), "-")[[1]][4] })
  output$next_word5 <- renderText({ strsplit(all_words(), "-")[[1]][5] })
  
  
})