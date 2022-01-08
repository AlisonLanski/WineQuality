##########
##
##  GLM Project: Wine
##  Alison Lanski
##  East
##  Spring 2019
##
##########


####################################################################
################################################
##################################
## 
## Setup
##
#################################
################################################
####################################################################

#load packages
library(tidyverse)

#turn off scientific notation
options(scipen = 99)


#read in data
red <- read.csv("C:/Users/Lanski/Documents/DataSets/GLM data/winequality-red.csv", sep = ";")
white <- read.csv("C:/Users/Lanski/Documents/DataSets/GLM data/winequality-white.csv", sep = ";")

#check it
str(red)
str(white)

#make sure the data is in the same format
colnames(red) == colnames(white)

#add an indicator for wine type
red$type <- 'red'
white$type <- 'white'

#union the red and white datasets together
dat <- rbind(red, white)

#create quality indicator
dat$quality_ind <- ifelse(dat$quality > 5, 1, 0)

#check the assignment
table(dat$quality, dat$quality_ind)

#clean up the data
dat <- dat %>% mutate(quality = quality_ind) %>% dplyr::select(-quality_ind)

#check again
table(dat$quality)

#do we need to handle NA values?
sum(is.na(dat))
#nope

#run a few more frequencies to help later with slides
table(dat$type)

round(table(white$quality)/nrow(white),2)
round(table(red$quality)/nrow(red),2)


###############
#Set up official analysis data:

#create a test and training set

#seed and index
set.seed(1660)
train_index <- sample(x = 1:nrow(dat), size = nrow(dat)*.6, replace = FALSE)

#make the data
dat_test <- dat[-train_index,]
dat_train <- dat[train_index,]



####################################################################
################################################
##################################
## 
## Exploratory Data
##
#################################
################################################
####################################################################


############
####
##
## Description:
## Explore the relation between the predictors and the response
## Choose 2 predictors and make an interleaved histogram and scatterplot for each
## (4 graphs total)
##
## In the slide deck, show the graphs and fully explain them
## (5-7 bullets about axes, meaning)
####
############


#look at the variables generally
summary(dat_train)


#let's look at normalized data side by side to get a feel for it
for(i in 1:(ncol(dat_train) -2 )) {
  sidebyside_hist <- ggplot(
    data = dat_train,
    aes(x = dat_train[[i]], y = ..density..)
  ) + geom_histogram() +
    xlab(colnames(dat_train)[i]) +
    facet_wrap(~quality)
  print(sidebyside_hist)
}

#we see:

# graphs that have some distinguishing features in them:
#     volatile acidity, citric acid, free sulfur dioxide, 
#     total sulfur dioxide, density, alcohol

# graphs look a lot more the same: 
#     fixed acidity, residual sugar, chlorides, pH, sulphates



#check some tables of summary info to see how the variables are distributed
tapply(dat_train$citric.acid, dat_train$quality, summary)
tapply(dat_train$free.sulfur.dioxide, dat_train$quality, summary)
tapply(dat_train$total.sulfur.dioxide, dat_train$quality, summary)
tapply(dat_train$density, dat_train$quality, summary)
tapply(dat_train$alcohol, dat_train$quality, summary)


#########
### Graph decision: 

## For the slides, we'll do 
#  alcohol (for high contrasts) 
#  sulphates (for low contrast)

#set up colors 
theme_fill <- c("darkorange", "forestgreen")
theme_outline <- c("darkred", "black")

#interleaved histogramp for Sulphates
#size printed: 1600 by 900
ggplot(
  data = dat_train,
  aes(x = sulphates, y = (..density..)*10, fill = as.factor(quality), color = as.factor(quality))
) + 
  geom_histogram(position = "dodge", binwidth = 0.1, alpha = 0.9) + 
  ylab("\nPercent of Total\n") +
  xlab("\nQuanitity of Sulphates\n") + 
  ggtitle("\nSulphate quantities are similar between high and low quality wine") +
  scale_color_manual(values = theme_outline, name = "Quality", labels = c("Low", "High")) +
  scale_fill_manual(values = theme_fill, name = "Quality", labels = c("Low", "High")) +
  theme_minimal() +
  theme(axis.title = element_text(size = 26), 
        legend.title = element_text(size = 26),
        legend.text = element_text(size = 22),
        plot.title = element_text(size = 36),
        axis.text = element_text(size = 22),
        legend.key.size = unit(2, "line"))


#interleaved histogram for Alcohol
ggplot(
  data = dat_train,
  aes(x = alcohol, y = (..density..)*10, fill = as.factor(quality), color = as.factor(quality))
) + 
  geom_histogram(position = "dodge", binwidth = 0.33, alpha = 0.9) + 
  ylab("\nPercent of Total\n") +
  xlab("\nPercentage of Alcohol by Volume\n") +
  ggtitle("\nABV quantities show a difference between high and low quality wine") +
  scale_color_manual(values = theme_outline, name = "Quality", labels = c("Low", "High")) +
  scale_fill_manual(values = theme_fill, name = "Quality", labels = c("Low", "High")) +
  theme_minimal() +
  theme(axis.title = element_text(size = 26), 
        legend.title = element_text(size = 26),
        legend.text = element_text(size = 22),
        plot.title = element_text(size = 36),
        axis.text = element_text(size = 22),
        legend.key.size = unit(2, "line"))


#scatterplot of sulphates
ggplot(
  data = dat_train,
  aes(x = as.numeric(row.names(dat_train)), y = sulphates, color = as.factor(quality), size = 3)
) + geom_point() +
  xlab("\nObservation Index Number\n") +
  ylab("\nQuantity of Sulphates\n") +
  ggtitle("\nSulphate Values Across the Dataset ") +
  scale_color_manual(values = theme_fill, name = "Quality", labels = c("Low", "High")) +
  theme_minimal() +
  guides(size = FALSE, color = guide_legend(override.aes = list(size = 12, shape = 15))) +
  theme(axis.title = element_text(size = 26), 
        legend.title = element_text(size = 26),
        legend.text = element_text(size = 22),
        plot.title = element_text(size = 36),
        axis.text = element_text(size = 22),
        legend.key.size = unit(2, "line"))
  

#scatterplot of alcohol
ggplot(
  data = dat_train,
  aes(x = as.numeric(row.names(dat_train)), y = alcohol, color = as.factor(quality), size = 3)
) + geom_point() +
  xlab("\nObservation Index Number\n") +
  ylab("\nPercentage of Alcohol by Volume\n") +
  ggtitle("\nAlcohol by Volume Values Across the Dataset") +
  scale_color_manual(values = theme_fill, name = "Quality", labels = c("Low", "High")) +
  theme_minimal() +
  guides(size = FALSE, color = guide_legend(override.aes = list(size = 12, shape = 15))) +
  theme(axis.title = element_text(size = 26), 
        legend.title = element_text(size = 26),
        legend.text = element_text(size = 22),
        plot.title = element_text(size = 36),
        axis.text = element_text(size = 22),
        legend.key.size = unit(2, "line"))





####################################################################
################################################
##################################
## 
## Variable Selection
##
#################################
################################################
####################################################################


############
####
##
## Description:
## Perform variable selection using the step() function for AIC
## Use the reduced model for subsequent steps or explain why you disagree with it
## 
##
## Add an explanation of the model to the slide deck? > how you used the function, results, final model
## (5-7 bullets about overview of algorithm (AIC), comment on variables retained/removed, intuition
####
############


#this is going to be binary logistic regression because we have covariate classes = 1


#run a model with all predictors:

fullmod <- glm(quality ~ ., data = dat_train, family = "binomial")

summary(fullmod)
#more things have good p-values than I was expecting... but the model is pretty big

#run AIC to reduce it
step(object = fullmod, data = dat_train)

###### only removes citric acid
#  doesn't seem like that's particularly useful -- model has too many things for my preference

###note that AIC stays the same if we remove chlorides, which is simpler, and worth (to me) the extra deviance


#let's look at correlated variables, since multicollinearity can be harmful to estimates
#and we want a simpler model
#and even though lasso/ridge can help with this, those methods add a lot of explanatory complexity

corrplot::corrplot(cor(dat_train[1:12]), type = 'lower')

#high correlations between:
#free and total sulfur dioxide
#density and alcohol
#reasonably high between density and residual sugar

#let's take out density and total sulfur dioxide
#in addition: remove citric acid and chlorides from the previous aic results

#run a smaller model:
lessermod <- glm(quality ~ fixed.acidity + volatile.acidity +
                   residual.sugar + free.sulfur.dioxide +
                   pH + sulphates + alcohol + type, 
                 family = 'binomial',
                 data = dat_train)

summary(lessermod)

#run aic again to simplify further
step(lessermod)
#this removes:fixed acidity


#the aic output will be our finalmod
#we've removed a reasonable number of variables
#if we remove a lot more, we might start to lose predictive power (AIC isn't improving that much)
finalmod <- glm(quality ~ volatile.acidity + residual.sugar + 
                free.sulfur.dioxide + pH +
                sulphates + alcohol + type, 
              family = 'binomial',
              data = dat_train)

# in total we've removed fixed acidity,  citric acid, density, and ph 

#take a look at the start and end
summary(fullmod)
summary(finalmod)
#p-values look good
#deviance hasn't gone up a ton, considering how high it is anyway
#also --
#summaries show that with the removed variables we have lower standard errors -- more stable model
#also it's simpler with more items removed




####################################################################
################################################
##################################
## 
## Assess Model Fit
##
#################################
################################################
####################################################################


############
####
##
## Description:
## 
## Check that continuous predictors have a linear relation with the logit using 
## loess plots and perform the Hosmer-Lemeshow (HL) goodness of fit test. 
## If the loess plot is nonlinear, then splines should be used to account for the nonlinearity.
## 
####
############

#rule of 5/10?
table(dat_train$quality)
#not a problem -- plenty of 0 and 1 values

library(ResourceSelection)

#check the final model
#null hypothesis: model fit is adequate
hoslem.test(finalmod$y, finalmod$fitted.values, 10)  
#0.4131 keep the null: model fit is okay

#for comparison, check the starting mod with everything:
hoslem.test(fullmod$y, fullmod$fitted.values, 10) 
#0.03848 reject the null, model does not fit well enough



#now let's look at linear relationships
#use Loess for each predictor

#produce the appropriate smoothed y value, and see if we need to transform x

#create a function to check and prep the data
check_smooth <- function(x) {

  #get smoothed values
  y_smooth <<- predict(loess(quality ~ x, data = dat_train))
  
  #see if they are outside [0,1]
  if(max(y_smooth) > 1 | min(y_smooth) < 0) {
    
    print('some values out of bounds')
    
    #create a selector for ok values and save it out 
    smooth_selector <<- (y_smooth > 0 & y_smooth < 1)
    
    #transform to prep for graphing
    y_smooth_graph <<- log(y_smooth[smooth_selector]/(1-y_smooth[smooth_selector]))
    
    
    #if it is in bounds [0,1]
    } else {
      
    print('all values in bounds')
  
    #transform to prep for graphing
    y_smooth_graph <<- log(y_smooth/(1 - y_smooth))
    }
}

###########
#run each variable through the function, then adjust the plot details if necessary


check_smooth(dat_train$volatile.acidity)

ggplot(
  data = dat_train,
  aes(x = volatile.acidity, y = y_smooth_graph, size = 3)
) + geom_jitter(shape = 1) +
  ylab("\nLog Odds of High Quality\n") +
  xlab("\nVolatile Acidity\n") +
  ggtitle("\nVolatile Acidity has a Cubic Relationship with the Log Odds") +
  guides(size = FALSE) + 
  theme_minimal() +
  theme(axis.title = element_text(size = 26), 
        legend.title = element_text(size = 26),
        legend.text = element_text(size = 22),
        plot.title = element_text(size = 36),
        axis.text = element_text(size = 22),
        legend.key.size = unit(2, "line"))

#linearish, but with a cubic divot

##########

check_smooth(dat_train$residual.sugar)
#values out of bounds -- have to use smooth selector in data call

ggplot(
  data = dat_train[smooth_selector,],
  aes(x = residual.sugar, y = y_smooth_graph, size = 3)
) + geom_jitter(shape = 1) +
  ylab("\nLog Odds of High Quality\n") +
  xlab("\nResidual Sugar\n") +
  ggtitle("\nResidual Sugar has a Quartic Relationship with the Log Odds") +
  guides(size = FALSE) + 
  theme_minimal() +
  theme(axis.title = element_text(size = 26), 
        legend.title = element_text(size = 26),
        legend.text = element_text(size = 22),
        plot.title = element_text(size = 36),
        axis.text = element_text(size = 22),
        legend.key.size = unit(2, "line"))

#like a quartic

########

check_smooth(dat_train$free.sulfur.dioxide)

ggplot(
  data = dat_train,
  aes(x = free.sulfur.dioxide, y = y_smooth_graph, size = 3)
) + geom_jitter(shape = 1) +
  ylab("\nLog Odds of High Quality\n") +
  xlab("\nFree Sulfur Dioxide\n") +
  ggtitle("\nFree Sulfur Dioxide has a Quadratic Relationship with the Log Odds") +
  guides(size = FALSE) + 
  theme_minimal() +
  theme(axis.title = element_text(size = 26), 
        legend.title = element_text(size = 26),
        legend.text = element_text(size = 22),
        plot.title = element_text(size = 36),
        axis.text = element_text(size = 22),
        legend.key.size = unit(2, "line"))

#like a quadratic

#################

check_smooth(dat_train$pH)

ggplot(
  data = dat_train,
  aes(x = pH, y = y_smooth_graph, size = 3)
) + geom_jitter(shape = 1) +
  ylab("\nLog Odds of High Quality\n") +
  xlab("\npH\n") +
  ggtitle("\npH has a Quartic Relationship with the Log Odds") +
  guides(size = FALSE) + 
  theme_minimal() +
  theme(axis.title = element_text(size = 26), 
        legend.title = element_text(size = 26),
        legend.text = element_text(size = 22),
        plot.title = element_text(size = 36),
        axis.text = element_text(size = 22),
        legend.key.size = unit(2, "line"))


#like a quartic 

###################

check_smooth(dat_train$sulphates)


ggplot(
  data = dat_train[smooth_selector,],
  aes(x = sulphates, y = y_smooth_graph, size = 3)
) + geom_jitter(shape = 1) +
  ylab("\nLog Odds of High Quality\n") +
  xlab("\nSulphates\n") +
  ggtitle("\nSulphates has a Cubic Relationship with the Log Odds") +
  guides(size = FALSE) + 
  theme_minimal() +
  theme(axis.title = element_text(size = 26), 
        legend.title = element_text(size = 26),
        legend.text = element_text(size = 22),
        plot.title = element_text(size = 36),
        axis.text = element_text(size = 22),
        legend.key.size = unit(2, "line"))

#like a cubic

##################

check_smooth(dat_train$alcohol)

ggplot(
  data = dat_train,
  aes(x = alcohol, y = y_smooth_graph, size = 3)
) + geom_jitter(shape = 1) +
  ylab("\nLog Odds of High Quality\n") +
  xlab("\nAlcohol\n") +
  ggtitle("\nAlcohol has a Cubic Relationship with the Log Odds") +
  guides(size = FALSE) + 
  theme_minimal() +
  theme(axis.title = element_text(size = 26), 
        legend.title = element_text(size = 26),
        legend.text = element_text(size = 22),
        plot.title = element_text(size = 36),
        axis.text = element_text(size = 22),
        legend.key.size = unit(2, "line"))


#like a cubic -- a nice one!

############

#can't test wine type -- it's a factor!


##########################################

#the data is very non-linear
#model assumptions are in shambles...
#we need to spline the variables

#clearly for the cubics and quartics
# we could do a x^2 for the quadratic, but as long as we're splining, I want to keep splining
# (one fixing method for simplicity)



#set up functions, then use them

##function to create splines, x is predictor, k is vector of knots. 
## For 3 knots and quartics 
my.4splines <- function(x,k){
  k1 <- k[1]
  k2 <- k[2]
  k3 <- k[3]
  x.l1 <- NULL
  x.l2 <- NULL
  x.l3 <- NULL
  x.l4 <- NULL
  for (i in 1:length(x)){
    x.l1[i] <- min(x[i],k1)
    x.l2[i] <- max(min(x[i],k2),k1)-k1
    x.l3[i] <- max(min(x[i],k3),k2)-k2
    x.l4[i] <- max(x[i],k3)-k3
  }
  x.s <- cbind(x.l1,x.l2,x.l3,x.l4)
}

## For 2 knots and cubics
my.3splines <- function(x,k){
  k1 <- k[1]
  k2 <- k[2]
  x.l1 <- NULL
  x.l2 <- NULL
  x.l3 <- NULL
  for (i in 1:length(x)){
    x.l1[i] <- min(x[i],k1)
    x.l2[i] <- max(min(x[i],k2),k1)-k1
    x.l3[i] <- max(x[i],k2)-k2
  }
  x.s <- cbind(x.l1,x.l2,x.l3)
}


## For 1 knot (quadratic)
my.2splines <- function(x,k){
  k1 <- k[1]
  x.l1 <- NULL
  x.l2 <- NULL
  for (i in 1:length(x)){
    x.l1[i] <- min(x[i],k1)
    x.l2[i] <- max(x[i],k1)-k1
  }
  x.s <- cbind(x.l1,x.l2)
}

#set up the knots (1, 2, or 3, depending on the shape; "evenly distributed")
knots_vol.acid <- quantile(dat_train$volatile.acidity, c(.35, .65))
knots_resid.sug <- quantile(dat_train$residual.sugar, c(.10, .50, .90))
knots_free.sulf.diox <- quantile(dat_train$free.sulfur.dioxide, c(.50))
knots_pH <- quantile(dat_train$pH, c(.10, .50, .90))
knots_sulphates <- quantile(dat_train$sulphates, c(.35, .65))
knots_alcohol <- quantile(dat_train$alcohol, c(.35, .65))


#Create splines for the continuous variables
vol.acid_spline <- my.3splines(dat_train$volatile.acidity, knots_vol.acid)
resid.sug_spline <- my.4splines(dat_train$residual.sugar, knots_resid.sug)
free.sulf.diox_spline <- my.2splines(dat_train$free.sulfur.dioxide, knots_free.sulf.diox)
pH_spline <- my.4splines(dat_train$pH, knots_pH)
sulphates_spline <- my.3splines(dat_train$sulphates, knots_sulphates)
alcohol_spline <- my.3splines(dat_train$alcohol, knots_alcohol)


#Redo the model, with splines for all except type
mod_splines <- glm(quality ~ 
                     vol.acid_spline + 
                     resid.sug_spline + 
                     free.sulf.diox_spline + 
                     pH_spline + 
                     sulphates_spline + 
                     alcohol_spline + 
                     type, 
                   family = "binomial", 
                   data = dat_train)



#check GOF via hoslem
hoslem.test(mod_splines$y, mod_splines$fitted.values, 10) 
#0.5091: still accepting the null; fit is reasonable


#model is done-done!


####################################################################
################################################
##################################
## 
## Model Inferences
##
#################################
################################################
####################################################################


############
####
##
## Description:
## 
## Report p-values and confidence intervals for significant predictors
## Check for and remove influential observations; refit the model and discuss changes in results
## 
####
############


#look at the model
summary(mod_splines)
#yikes lots of things are not significant, but this is what happens with splines
#and actually, given the number of splines, a lot more is significant still
#than I had expected

########
### Compute confidence intervals for the betahats ---
#   except --- this result doesn't mean a lot for splines 
#   since we are all-or-nothing for the components


# we could calc them with...
#upper bounds
round(coef(mod_splines) + 1.96*summary(mod_splines)$coefficients[,2],3)

#lower bounds
round(coef(mod_splines) - 1.96*summary(mod_splines)$coefficients[,2], 3)

#or use the simple method
round(confint(mod_splines),3)

#confirmed that 0 is in the 95% confidence interval for the Intercept and some splines 
#which is expected because the p values were not significant at alpha = 0.05.


##########
#Look for influential outliers
cooks_check <- cooks.distance(mod_splines)
max(cooks_check)
min(cooks_check)
#no values with magnitude > 1, so we don't have any influential outliers that need removing


####################################################################
################################################
##################################
## 
## Assess Predictive Power
##
#################################
################################################
####################################################################


############
####
##
## Description:
## 
## Summarize predictive power of the model with an ROC curve and plots of predicted probabilities
## 
####
############

######
#ROC curve
#use our model on the training data, then test data

#prep for the ROC curve
library(ROCR)

#set up predictions
pred <- prediction(mod_splines$fitted.values, dat_train$quality)

#measure performance
perf <- performance(pred, measure = "tpr", x.measure = "fpr")

#plot the ROC
plot(perf, col = rainbow(10), main = c("ROC Curve with Splined Model on Training Data"))

#it looks okay -- not hitting the upper corner as we might like, but with a reasonable curve

#prep for "pretty" graph for slides
#set up a DF
roc_df <- data.frame(Curve = "SplineMod",
                     FalsePositive = perf@x.values[[1]],
                     TruePositive = perf@y.values[[1]])

#then we can ggplot it
ggplot(
  data = roc_df,
  aes(x = FalsePositive, y = TruePositive, size = 2)
) + geom_point( color = '#980000') +
  theme_minimal() +
  guides(color = FALSE, size = FALSE) +
  xlab("\nFalse Positive Rate\n") +
  ylab("\nTrue Positive Rate\n") +
  ggtitle("\nROC Curve with Splined Model on Training Data\n") +
  theme(axis.title = element_text(size = 26), 
        plot.title = element_text(size = 36),
        axis.text = element_text(size = 22)#,
        ) 
  

#check the AUC
auc.start <- performance(pred, "auc")
auc <- as.numeric(auc.start@y.values)
auc

#well with auc of 0.81 this model has "excellent" discimination, hooray!


#plots of predicted probabilities

#add fitted values to our data
dat_train$splines_fitted <- mod_splines$fitted.values

#plot
ggplot(
  data = dat_train,
  aes(x = splines_fitted, y = as.factor(quality), color = (quality == round(splines_fitted)))
) + geom_jitter(height = 0.2) +
  xlab("\nPredicted Probabilty of High Quality\n") +
  ylab("\nActual Quality (1 = High)\n") +
  ggtitle("\nThe Model Predicts High Quality Better Than Low Quality") +
  scale_color_manual(values = c("black", "#980000"), name = "Correct Predictions", labels = c("No", "Yes")) +
  theme_minimal() +
  guides(size = FALSE, color = guide_legend(override.aes = list(size = 12, shape = 15))) +
  theme(axis.title = element_text(size = 26), 
        legend.title = element_text(size = 26),
        legend.text = element_text(size = 22),
        plot.title = element_text(size = 36),
        axis.text = element_text(size = 22),
        legend.key.size = unit(2, "line"))
  

#overall accuracy of the training data
table(dat_train$quality, round(mod_splines$fitted.values))
(2021+893) / nrow(dat_train)
#0.75

#pretty decent


#################
############

#NOW: try this model against the test data

############
################


#tricky to do because of splines:
#have to refit with the same knots to get the right length of spline vectors
#(don't re-calculate knot points)

#Create splines for the continuous variables
vol.acid_spline_test <- my.3splines(dat_test$volatile.acidity, knots_vol.acid)
resid.sug_spline_test <- my.4splines(dat_test$residual.sugar, knots_resid.sug)
free.sulf.diox_spline_test <- my.2splines(dat_test$free.sulfur.dioxide, knots_free.sulf.diox)
pH_spline_test <- my.4splines(dat_test$pH, knots_pH)
sulphates_spline_test <- my.3splines(dat_test$sulphates, knots_sulphates)
alcohol_spline_test <- my.3splines(dat_test$alcohol, knots_alcohol)


#Redo the model, with splines for all except type
mod_splines_test <- glm(quality ~ 
                     vol.acid_spline_test + 
                     resid.sug_spline_test + 
                     free.sulf.diox_spline_test + 
                     pH_spline_test + 
                     sulphates_spline_test + 
                     alcohol_spline_test + 
                     type, 
                   family = "binomial", 
                   data = dat_test)



#check GOF via hoslem
hoslem.test(mod_splines_test$y, mod_splines_test$fitted.values, 10) 
#at 0.18, still keeping the null

#so the fit might not be as good, but that's expected from training > test data
#the fact that it's still keeping the null feels like a winning situation


#create ROC curve and check AUC for the test data
prob_test_splines <- predict(mod_splines_test, newdata=dat_test, type="response")
pred_test_splines <- prediction(prob_test_splines, dat_test$quality)
perf_test_splines <- performance(pred_test_splines, measure = "tpr", x.measure = "fpr")

roc_df_test <- data.frame(Curve = "SplineMod",
                     FalsePositive = perf_test_splines@x.values[[1]],
                     TruePositive = perf_test_splines@y.values[[1]])

ggplot(
  data = roc_df_test,
  aes(x = FalsePositive, y = TruePositive, size = 2)
) + geom_point( color = '#980000') +
  theme_minimal() +
  guides(color = FALSE, size = FALSE) +
  xlab("\nFalse Positive Rate\n") +
  ylab("\nTrue Positive Rate\n") +
  ggtitle("\nROC Curve with Splined Model on Test Data\n") +
  theme(axis.title = element_text(size = 26), 
        #legend.title = element_text(size = 26),
        #legend.text = element_text(size = 22),
        plot.title = element_text(size = 36),
        axis.text = element_text(size = 22)#,
        #legend.key.size = unit(2, "line")
  ) 


#and the AUC for test data...
auc_test_splines <- performance(pred_test_splines, measure = "auc")
auc_test_splines <- auc_test_splines@y.values[[1]]
auc_test_splines
## 0.822 -- wow -- even better

#given that the fit didn't have as strong of a p-value, I'm surprised!


#plots of predicted probabilities for the test data
dat_test$splines_fitted <- mod_splines_test$fitted.values

ggplot(
  data = dat_test,
  aes(x = splines_fitted, y = as.factor(quality), color = (quality == round(splines_fitted)))
) + geom_jitter(height = 0.2) +
  xlab("\nPredicted Probabilty of High Quality\n") +
  ylab("\nActual Quality (1 = High)\n") +
  ggtitle("\nThe Model Still Predicts High Quality Better Than Low Quality") +
  scale_color_manual(values = c("black", "#980000"), name = "Correct Predictions", labels = c("No", "Yes")) +
  theme_minimal() +
  guides(size = FALSE, color = guide_legend(override.aes = list(size = 12, shape = 15))) +
  theme(axis.title = element_text(size = 26), 
        legend.title = element_text(size = 26),
        legend.text = element_text(size = 22),
        plot.title = element_text(size = 36),
        axis.text = element_text(size = 22),
        legend.key.size = unit(2, "line"))




#overall accuracy testing?
table(dat_test$quality, round(mod_splines_test$fitted.values))
(1413+564) / nrow(dat_test)
#0.76

#just about the same

#this model does a reasonable job
#hooray for wine!
#now to make friends with a chemist....




