# Library calls
library(magrittr)
library(dplyr)
library(caret)
library(ggplot2)
library(rattle)

# how you built your model
#
# Step 1: Import the data
train_url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
train_data <- read.csv(train_url)
train_data_orig <- train_data # Keep a copy so we dont need to reload
train_data <- train_data_orig # Run if necessary

test_url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
test_data <- read.csv(test_url)

# Step 2: Data Munging
str(train_data)
# Clearly many of the variables are not well populated. Lets figure out which ones.
# Lets drop the variables that are missing for ~97.9% of records
missing <- lapply(X = train_data, FUN = function(var) {
  length(which(is.na(var))) / nrow(train_data)
}) %>%
  unlist()
train_data <- train_data[,-which(missing > 0)]
names(train_data)


# The data are a mix of numeric(integer) and factor variables
# Let's look at the factor variables
which_factors <- which(lapply(X = train_data, FUN = class) == "factor")
lapply(X = train_data[,which_factors], summary)
# Looks like many of these were converted to factors because there were missing
# values on 19216/19622 = ~97.9% of the data
# we can drop these as well
# But we need to keep classe
train_data <- train_data[,-which_factors]
train_data$classe <- train_data_orig$classe

# OK - That is better, we now have some usuable data
#
# Lets try a tree

mod_fit <- train(form = classe~., data = train_data[, -1], method = "rpart")
fancyRpartPlot(mod_fit$finalModel)

# That's not so good. This model didnt predict any of the Ds
# Let's try a random forest.

mod_fit_rf <- train(form = classe~., data = train_data[, -1], method = "rf")

mod_fit_rf$finalModel
varImpPlot(mod_fit_rf$finalModel)
varUsed(mod_fit_rf$finalModel)

# how you used cross validation,
#
# There was no need to use cross validation since the train function in
# caret handles that for us.
#
# what you think the expected out of sample error
# Since the method is already cross-validated, I expect the out of sample error rate to have a minumum value of
trees <- mod_fit_rf$finalModel$ntree
paste(round(mod_fit_rf$finalModel$err.rate[trees ,"OOB"] * 100, 2), "percent")

preds_rf <- predict(mod_fit_rf, test_data)
table(preds_rf)


# That is prerry good. This is my final model

# exactly according to the specification (Class A),
# throwing the elbows to the front (Class B),
# lifting the dumbbell only halfway (Class C),
# lowering the dumbbell only halfway (Class D) and
# throwing the hips to the front (Class E)


#




# why you made the choices you did.
#
#
# You will also use your prediction model to predict 20 different test cases.
#
# Acknowledgement
# The data for this project come from this source: http://groupware.les.inf.puc-rio.br/har. If you use the document you create for this class for any purpose please cite them as they have been very generous in allowing their data to be used for this kind of assignment.