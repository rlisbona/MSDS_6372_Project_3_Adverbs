library(aod)
library(ggplot2)
library(Rcpp)

## Comment added by Randy
## added a second comment
## third branch

adverbs = read.csv("C:/Users/anobs/Documents/GitHub/MSDS6372_Project3/data/booksDF.csv",header=TRUE)
head(adverbs)

summary(adverbs)
sapply(adverbs, sd)

adverblogit <- glm(Author ~ Per_Small + 
                       Per_Medium + 
                       Per_Large + 
                       already + 
                       together + 
                       hollow + 
                       little + 
                       suddenly + 
                       nothing + 
                       especially + 
                       light + 
                       alone + 
                       above + 
                       though + 
                       closed + 
                       lightly + 
                       forth + 
                       large + 
                       least + 
                       beneath + 
                       under + 
                       great + 
                       nearly + 
                       through + 
                       quite + 
                       always + 
                       others + 
                       first + 
                       below + 
                       where + 
                       still + 
                       slowly + 
                       strange + 
                       these + 
                       immediately + 
                       fresh + 
                       there + 
                       evening + 
                       however + 
                       beside + 
                       after + 
                       parts + 
                       sweet + 
                       early + 
                       something + 
                       latter + 
                       along + 
                       those + 
                       small + 
                       beyond + 
                       particularly + 
                       right + 
                       since + 
                       extremely + 
                       stranger + 
                       almost + 
                       between + 
                       aside + 
                       scarcely + 
                       stately + 
                       therefore + 
                       better + 
                       close + 
                       black + 
                       again + 
                       before + 
                       other + 
                       about + 
                       rather + 
                       sound + 
                       rising + 
                       without + 
                       longer + 
                       certainly + 
                       never + 
                       either  
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
