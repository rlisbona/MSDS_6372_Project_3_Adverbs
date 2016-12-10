
#install.packages('tibble')
#library(tibble)
library(plyr)
library(dplyr)

#library(doBy)
#library(aod)
#library(ggplot2)
#library(Rcpp)
#library(mclust)
#require(colorspace)
#require(sampling)
require(nnet)

# this is basically from this video
#   https://www.youtube.com/watch?v=fDjKa7yWk1U

adverbs = read.csv("C:/Users/anobs/Documents/GitHub/MSDS_6372_Project_3_Adverbs/data/booksDF2.csv",header=TRUE)
adverbs$AuthorID_Factor <- factor(adverbs$AuthorID)         #Create a factor variable for AuthorID
adverbs$out <- relevel(adverbs$AuthorID_Factor, ref ="1")   #Create a referance variable using authorID=1

#Probably don't need this, I was having trouble earlier today getting rownumbers in a plot, and I wanted authors
#adverbs2 <-column_to_rownames(adverbs,var='AuthorTitle')

train <- adverbs[adverbs$RndSelect == 2,]     #I couldn't figure out how to select 1 book from each author as a training data set so I added a column and pick the 2nd book as training set
test <- adverbs[adverbs$RndSelect != 2,]      #The test data set

test <- adverbs        #For now using all the books in the adverb file, comment this out to use the actual test array of books not in the training set


str(test)  # display the data structure 

# not sure what unit of measure of small medium large is, do we just divide by 100 to get percent?
# This section is different models, run one then skip to summary()
mymodel <- multinom(out~ Per_Small+Per_Medium+Per_Large, data = test)
mymodel <- multinom(out~ little+ without+  other+ nothing+ again +before + these + least+  about+  those +though + after+ through+ together + where+  under + never+  right, data = test)
mymodel <- multinom(out~ little+ without+  other+ nothing+ again +before + these + least+  about+  those , data = test)
mymodel <- multinom(out~ little+ without+  other+ nothing+ again  , data = test)
mymodel <- multinom(out~ little+ without+  other  , data = test)

summary(mymodel)

#predict(mymodel,test)  #this is a list of predictions, hard to read so skip it
#predict(mymodel,test,type ="prob")  #This gives probabilities, kind of hard to read

# This creates a matrix that shows the predicted author vs the actual author
# Perfect match is when the number of books by the author is in the intersection of predicted vs actual authorID
#                          row variable     column variable
ConfidenceMatrix <- table(predict(mymodel),test$AuthorID_Factor)
ConfidenceMatrix
# This produces a table of p values showing significance for each adverb predicting a given authorID.  Need to figure out how to find minimal number of best predicting adverbs
z <- summary(mymodel)$coefficients/summary(mymodel)$standard.errors
p <- (1-pnorm(abs(z),0,1)) *2
p



# Hess option may produce something useful, couldn't figure it out.
mymodel <- multinom(out~ little+ without+  other+ nothing+ again  , data = test, hess=TRUE, model=TRUE)


?predict()
?table()
?str()
?summary()
?pnorm()
