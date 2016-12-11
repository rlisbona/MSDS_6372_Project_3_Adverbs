
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
require(MASS)

# this is basically from this video
#   https://www.youtube.com/watch?v=fDjKa7yWk1U

###Note that I added a few extra columns to booksDF.  The only extra one currently used is the AuthorID and rndselect used to pick the 2nd book for training data set.

adverbs = read.csv("C:/Users/anobs/Documents/GitHub/MSDS_6372_Project_3_Adverbs/data/booksDF2.csv",header=TRUE)
adverbs = read.csv("C:/Users/anobs/Documents/GitHub/MSDS_6372_Project_3_Adverbs/data/booksDF2normalized.csv",header=TRUE)

adverbs[is.na(adverbs)] <- 0                           # set any missing values to zero, we shouldn't see any missing values

#adverbs <- adverbs[adverbs$AuthorID != 2,]

#adverbs$Author_Factor <- factor(adverbs$Author)         #Create a factor variable for AuthorID  Shouldnt need this

str(adverbs)

adverbs$Per_Small <- adverbs$Per_Small /100.0
adverbs$Per_Medium <- adverbs$Per_Medium /100.0
adverbs$Per_Large <- adverbs$Per_Large /100.0


adverbsNormalized <- as.data.frame(scale(adverbs[6:32]))

correlationmatrix <- cor(adverbs[6:32])
highlycorrelated <- findCorrelation(correlationmatrix,cutoff=0.5)

str(adverbsNormalized)

adverbs$AuthorID_Factor <- factor(adverbs$AuthorID)         #Create a factor variable for AuthorID     
adverbs$out <- relevel(adverbs$AuthorID_Factor, ref ='1')   #Create a referance variable using authorID=1      

#Probably don't need this, I was having trouble earlier today getting rownumbers in a plot, and I wanted authors
#adverbs2 <-column_to_rownames(adverbs,var='AuthorTitle')


test <- adverbs[adverbs$RndSelect > 3,]     #I couldn't figure out how to select 1 book from each author as a training data set so I added a column and pick the 2nd book as training set

train <- adverbs[adverbs$RndSelect <= 3,]      #The training data set


train <- adverbs        #For now using all the books in the adverb file, comment this out to use the actual test array of books not in the training set
test <- train

str(test)  # display the data structure 

# not sure what unit of measure of small medium large is, do we just divide by 100 to get percent?
# This section is different models, run one then skip to summary()
mymodel <- multinom(out~ Per_Small+Per_Medium+Per_Large + little+ without+  other+ nothing+ again +before + these + least+  about+  those +though + after+ through+ together + where+  under + never+  right, data = train)
mymodel <- multinom(out~ little+ without+  other+ nothing+ again +before + these + least+  about+  those +though + after+ through+ together + where+  under + never+  right, data = train)
mymodel <- multinom(out ~ little+ without+  other+ nothing+ again +before + these + least+  about+  those , data = train)
mymodel <- multinom(out~ little+ without+  other+ nothing+ again  , data = train)
mymodel <- multinom(out~ little+ without+  other  , data = train)
mymodel <- multinom(out~ about + little + these +  again +  other + right +those , data = train)
mymodel <- multinom(out~ about + little + these +  again +  other + right  , data = train)
mymodel <- multinom(out~ about + little + these +  again +  other  , data = train)
mymodel <- multinom(out~ about + little + these +  again , data = train)

#not sure if this applies to multinomial
step(mymodel) 

step(mymodel, scope =  ~.^2)

# scope = ~.^2does all interactions
# step(mymodel, scope =  ~.^2)  
# 
# Call:
#   multinom(formula = out ~ Per_Large + other + though + Per_Small, 
#            data = train)
# 
# Coefficients:
#   (Intercept) Per_Large   other  though Per_Small
# 2       -828.4    -466.8  4820.9  1460.4    -468.1
# 3       -691.8     447.6  1215.1  1270.9     687.8
# 4       1143.2   -4535.9  1001.1  2890.4   -1199.3
# 5       1122.0    -732.7 -1114.5 -1983.3   -1613.2
# 6        709.8     -34.2   523.6   910.1   -1804.8
# 7       -471.1     348.3   743.0  -444.2     690.4
# 8      -1128.2    1962.4   939.9   221.0    1060.2
# 9        395.7    2843.0   214.9  -440.1   -2616.2
# 10       -67.0    2955.6 -2708.0  2155.9   -1234.4
# 11       -96.4    2671.1  1765.8 -2441.5   -1819.9
# 12     -1519.5    1783.6  5700.7 -1354.0    -535.9
# 13     -1410.1   -2297.5  6205.8 -1778.5     801.1
# 14     -1147.4     383.4  1289.5   960.6    1744.3
# 15      -278.8    -334.0  1037.3 -3888.3     798.0
# 16       377.9    2168.3 -2981.2  -817.0   -1217.4
# 
# Residual Deviance: 9.824 
# AIC: 159.8  

predictors(mymodel)
class(mymodel)
summary(mymodel)  # getting NaNs here, some sites say don't worry about them, not sure about this

#predict(mymodel,train)  #this is a list of predictions, hard to read so skip it
#predict(mymodel,train,type ="prob")  #This gives probabilities, kind of hard to read
summary(mymodel)
predict(mymodel)
options(digits=4)

(CheckPredictions <- predict(mymodel, type = "class", newdata =test))


(ConfidenceMatrix <- table(predict(mymodel),train$AuthorID))
ConfidenceMatrix <- table(predict(mymodel),train$AuthorID_Factor)
# This creates a matrix that shows the predicted author vs the actual author
# Perfect match is when the number of books by the author is in the intersection of predicted vs actual authorID
#                          row variable     column variable

(misclassificationpcterror <- 1-sum(diag(ConfidenceMatrix))/sum(ConfidenceMatrix))
require(caret)

mostImportantVariables <- varImp(mymodel,value = "rss")
mostImportantVariables$Variables <- row.names(mostImportantVariables)
mostImportantVariables <- mostImportantVariables[order(-mostImportantVariables$Overall),]
print(head(mostImportantVariables))

 

# This produces a table of p values showing significance for each adverb predicting a given authorID.  Need to figure out how to find minimal number of best predicting adverbs
z <- summary(mymodel)$coefficients/summary(mymodel)$standard.errors
p <- (1-pnorm(abs(z),0,1)) *2
p
exp(coef(mymodel))


# https://www.youtube.com/watch?v=qkivJzjyHoA&t=6s
# from ordinal logistic regression video, doesn't seem to work here the same way
(ctable <- coef(summary(mymodel)))
p <- pnorm(abs(ctable[, 't value']), lower.tail = FALSE) *2
(ctable <- cbind(ctable, 'p value' = p))



postResample(vehiclesTest$cylinders,preds2)






# Hess option may produce something useful, couldn't figure it out.
mymodel <- multinom(out~ little+ without+  other+ nothing+ again  , data = test, hess=TRUE, model=TRUE)
mymodel <- multinom(out~ little+ without+  other+ nothing+ again  , data = test, hess=TRUE)
?multinom
?predict.multinom

?predict()
?table()
?str()
?summary()
?pnorm()
?print
?step