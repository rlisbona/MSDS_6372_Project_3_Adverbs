
#install.packages('tibble')
library(tibble)
library(plyr)
library(dplyr)

library(doBy)
library(aod)
library(ggplot2)
library(Rcpp)
library(mclust)
#install.packages('colorspace')


## Comment added by Randy
## added a second comment
## third branch
## 
## 
## 

adverbs = read.csv("C:/Users/anobs/Documents/GitHub/MSDS_6372_Project_3_Adverbs/data/booksDF2.csv",header=TRUE)
adverbs2 <-column_to_rownames(adverbs,var='AuthorTitle')

head(adverbs)

summary(adverbs)
sapply(adverbs, sd)


'Per_Small',
'Per_Medium',
'Per_Large',

X.quanti <- adverbs[,c(
                            'little',
                            'without',
                            'other',
                            'nothing',
                            'again',
                            'before',
                            'these',
                            'least',
                            'about',
                            'those',
                            'though',
                            'after',
                            'through',
                            'together',
                            'where',
                            'under',
                            'never',
                            'right',
                            'first',
                            'always',
                            'great',
                            'there',
                            'others',
                            'still')]

X.quali <- adverbs[,c(1,2)]

X.quanti
X.quali

tree <- hclustvar(X.quanti)
tree <- hclustvar(X.quanti, X.quali)
par(mfrow = c(1, 2))
plot(tree)
stab <- stability(tree,B=100,graph=TRUE)
plot(stab)

# shows row numbers
clusters <- hclust(dist(adverbs[,3:29]))
plot(clusters)

# shows row names
clusters2 <- hclust(dist(adverbs2[,3:29]))
plot(clusters2)


group_by(adverbs2,Author) %>%
  summarise(mean=mean)

aggregate(x=adverbs, by=c('Author'),FUN=mean,na.rm=TRUE)
?aggregate
fit <- 



adverblogit <- glm(Author ~ 
                     Per_Small +
                     Per_Medium +
                     Per_Large +
                     little +
                     without +
                     other +
                     nothing +
                     again +
                     before +
                     these +
                     least +
                     about +
                     those +
                     though +
                     after +
                     through +
                     together +
                     where +
                     under +
                     never +
                     right +
                     first +
                     always +
                     great +
                     there +
                     others +
                     still 
                     
                       , data = adverbs, family = "binomial")

summary(adverblogit)

## Confidence intervals using profiled log-likelihood
confint(adverblogit)

## Confidence intervals using standard errors
confint.default(adverblogit)

## odds ratios
exp(coef(adverblogit))

## odds ratios and 95% CI
exp(cbind(OR = coef(adverblogit), confint(adverblogit)))

## http://www.ats.ucla.edu/stat/r/dae/logit.htm
