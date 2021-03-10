#########Practice questions:

rm(list = ls())

setwd("C:\\Users\\belni\\Documents\\GitHub\\TAD_2021\\R lessons")

library(quanteda)
library(quanteda.textmodels)
library(readtext)
library(dplyr)

news_data <- readRDS("news_data.rds")
news_samp <- filter(news_data, category %in% c("CRIME", "SPORTS"))
news_samp1 <- select(news_samp, headline, category)
news_samp2<-setNames(object = news_samp1, nm = c("text", "class"))
news_samp2$text <- gsub(pattern = "'", "", news_samp2$text)

set.seed(1984L)

# 1. We originally set the proportion of the training set to be .8 --- what happens to performance when we set it at .2? Re-run all the code 


prop_train <- 0.2
ids <- 1:nrow(news_samp2)
ids_train <- sample(ids, ceiling(prop_train*length(ids)), replace = FALSE)

ids_test <- ids[-ids_train]
train_set <- news_samp2[ids_train,]
test_set <- news_samp2[ids_test,]

train_dfm <- dfm(train_set$text, stem = TRUE, remove_punct = TRUE, remove = stopwords("english"))
test_dfm <- dfm(test_set$text, stem = TRUE, remove_punct = TRUE, remove = stopwords("english"))

test_dfm <- dfm_match(test_dfm, features = featnames(train_dfm))

nb_model <- textmodel_nb(train_dfm, train_set$class, smooth = 0, prior = "uniform")
predicted_class <- predict(nb_model, newdata = test_dfm)


baseline_acc <- max(prop.table(table(test_set$class)))

cmat <- table(test_set$class, predicted_class)
nb_acc <- sum(diag(cmat))/sum(cmat) # accuracy = (TP + TN) / (TP + FP + TN + FN)
nb_recall <- cmat[2,2]/sum(cmat[2,]) # recall = TP / (TP + FN)
nb_precision <- cmat[2,2]/sum(cmat[,2]) # precision = TP / (TP + FP)
nb_f1 <- 2*(nb_recall*nb_precision)/(nb_recall + nb_precision)


cat(
  "Baseline Accuracy: ", baseline_acc, "\n",
  "Accuracy:",  nb_acc, "\n",
  "Recall:",  nb_recall, "\n",
  "Precision:",  nb_precision, "\n",
  "F1-score:", nb_f1
)
##after that point (with smoothing)

nb_model_sm <- textmodel_nb(train_dfm, train_set$class, smooth = 1, prior = "uniform")

predicted_class_sm <- predict(nb_model_sm, newdata = test_dfm)

cmat_sm <- table(test_set$class, predicted_class_sm)
nb_acc_sm <- sum(diag(cmat_sm))/sum(cmat_sm) # accuracy = (TP + TN) / (TP + FP + TN + FN)
nb_recall_sm <- cmat_sm[2,2]/sum(cmat_sm[2,]) # recall = TP / (TP + FN)
nb_precision_sm <- cmat_sm[2,2]/sum(cmat_sm[,2]) # precision = TP / (TP + FP)
nb_f1_sm <- 2*(nb_recall_sm*nb_precision_sm)/(nb_recall_sm + nb_precision_sm)

cat(
  "Baseline Accuracy: ", baseline_acc, "\n",
  "Accuracy:",  nb_acc_sm, "\n",
  "Recall:",  nb_recall_sm, "\n",
  "Precision:",  nb_precision_sm, "\n",
  "F1-score:", nb_f1_sm
)

# 2. Read the help file about textmodel_nb . What does the "prior" argument do in Naive Bayes? What is the default value for this 
# argument in quanteda?


?textmodel_nb

#prior	-  refer to the prior probabilities assigned to the training classes, and the choice of prior distribution affects the calculation of the fitted probabilities. 

#prior distribution on texts; one of "uniform", "docfreq", or "termfreq". See Prior Distributions below.

#The default is uniform priors, which sets the unconditional probability of observing the one class to be the same as observing any other class.

#3. Re-run the code with prior = "docfreq". Think about the proportions of classes in the dataset. How should this affect your outcome?

## The model should become more accurate as instead of unconditional probability of observing classes, we use the relative proportions from the class documents from the training set itself.

# Looking at the results of the code, were you right?
nb_model_sm <- textmodel_nb(train_dfm, train_set$class, smooth = 1, prior = "docfreq")

predicted_class_sm <- predict(nb_model_sm, newdata = test_dfm)

cmat_sm <- table(test_set$class, predicted_class_sm)
nb_acc_sm <- sum(diag(cmat_sm))/sum(cmat_sm) # accuracy = (TP + TN) / (TP + FP + TN + FN)
nb_recall_sm <- cmat_sm[2,2]/sum(cmat_sm[2,]) # recall = TP / (TP + FN)
nb_precision_sm <- cmat_sm[2,2]/sum(cmat_sm[,2]) # precision = TP / (TP + FP)
nb_f1_sm <- 2*(nb_recall_sm*nb_precision_sm)/(nb_recall_sm + nb_precision_sm)

cat(
  "Baseline Accuracy: ", baseline_acc, "\n",
  "Accuracy:",  nb_acc_sm, "\n",
  "Recall:",  nb_recall_sm, "\n",
  "Precision:",  nb_precision_sm, "\n",
  "F1-score:", nb_f1_sm
)

## I was right overall confusion matrix shows improvement in accuracy of the model
