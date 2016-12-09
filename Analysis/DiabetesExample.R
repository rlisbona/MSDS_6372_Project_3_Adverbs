library(aod)
library(ggplot2)
library(Rcpp)

## Comment added by Randy
## added a second comment
## third branch

diabetes = read.csv("Project3/diabetes.csv",header=TRUE)
head(diabetes)

summary(diabetes)
sapply(diabetes, sd)

diabeteslogit <- glm(Outcome ~ Pregnancies + Glucose + BloodPressure + SkinThickness + Insulin + BMI + DiabetesPedigreeFunction + Age, data = diabetes, family = "binomial")

summary(diabeteslogit)

## Confidence intervals using profiled log-likelihood
confint(diabeteslogit)

## Confidence intervals using standard errors
confint.default(diabeteslogit)

## odds ratios
exp(coef(diabeteslogit))

## odds ratios and 95% CI
exp(cbind(OR = coef(diabeteslogit), confint(diabeteslogit)))

## http://www.ats.ucla.edu/stat/r/dae/logit.htm

