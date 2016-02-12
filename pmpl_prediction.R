# how you built your model
#
# Step 1: Import the data
train_url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
train_data <- read.csv(train_url)

test_url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
test_data <- read.csv(test_url)

str(train_data)
names(train_data)
levels(train_data$classe)
# exactly according to the specification (Class A),
# throwing the elbows to the front (Class B),
# lifting the dumbbell only halfway (Class C),
# lowering the dumbbell only halfway (Class D) and
# throwing the hips to the front (Class E)

library(caret)
library(ggplot2)
mod_fit <- train(form = classe ~ ., data = train_data, method = "rf",
  proxy = TRUE)
mod_fit


#
# how you used cross validation,
# what you think the expected out of sample error
# why you made the choices you did.
#
#
# You will also use your prediction model to predict 20 different test cases.
#
# Acknowledgement
# The data for this project come from this source: http://groupware.les.inf.puc-rio.br/har. If you use the document you create for this class for any purpose please cite them as they have been very generous in allowing their data to be used for this kind of assignment.