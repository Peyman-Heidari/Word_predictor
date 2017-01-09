Next Word Predictor!
========================================================
author: Peyman Heidari 
font-family: 'Risque'
date: 
autosize: true
Coursera Data Science Capstone Project 


Algorithm
========================================================
+ This application predicts the next word in a phrase as the user types it.
+ The supervised learning algorithm was trained using n-grams (n=1,2,3,4).
+ N-grams were extracted from about 1 GB of text data. [Download](https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip).
+ To account for unseen combinations, language models need to be smoothened.
+ Smoothing a language models is to move some probability towards unknown n-grams. There are many ways to do this.
+ The method with the best performance is interpolated modified Kneser-Ney smoothing, which has been the state of the art smoothing algorithm for the last 15-20 years.

Modified Kneser-Ney smoothing
========================================================
 Modified Kneser-Ney smoothing uses contunation probability instead of maximum likelihood. 
+ While predicting the probability of a word given a context, it takes into accountthe number of contexts that the word appears in.
+ for each n-gram, three different discount values are used. These values are calculated based on the training data.
<div align="center">
<img src="knformula.jpg" width=1200 height=200>
</div>
Please follow this [link](http://www.speech.sri.com/projects/srilm/manpages/pdfs/chen-goodman-tr-10-98.pdf) for detailed explanation.

 
Model performance
========================================================
Accuracy of the app was measured using a benchmarking [code](https://github.com/hfoffani/dsci-benchmark) created by Jan Hagelauer and Hernan for this capstone project.The App is a top performer based on a shortresearch.
+ Overall top-3 score: 18.20 %
+ Overall top-1 precision: 13.45 %
+ Overall top-3 precision: 22.21 %
+ Average runtime: 18.08 msec  

Overall top-3 precision of 29.46 % was observed using the same model on a different test dataset (not used during training).Numerous attemps such as implementation of five-grams, skip-grams, and LDA(topic modeling) failed due to lack of significant increase in precision to compensate for their lower speed and higher memory use.

How to use the app
========================================================
you can access the [app](https://peymanh.shinyapps.io/WordPredictor/) foloowing this [link](https://peymanh.shinyapps.io/WordPredictor/).

+ start typing in the text box that says type here.
+ five suggestions are made by the app.
+ the top suggestion is the first suggestion made by the model.

Disclaimer: you might have to wait for a couple of seconds for the app to load.
