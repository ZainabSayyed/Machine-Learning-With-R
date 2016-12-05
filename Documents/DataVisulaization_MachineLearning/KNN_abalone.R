##### Chapter 3: Classification using Nearest Neighbors --------------------

## Example: Classifying Cancer Samples ----
## Step 2: Exploring and preparing the data ---- 

# import the CSV file
wbcd <- read.csv("D:/My Documents/STAT 6620/defaultofcreditcardclients.csv", stringsAsFactors = FALSE)

# examine the structure of the wbcd data frame
str(wbcd)

# drop the id feature
wbcd <- wbcd[-1]

# table of default payment next month
table(wbcd$defaultpaymentnextmonth)


# recode  default payment next month as a factor
wbcd$defaultpaymentnextmonth <- factor(wbcd$defaultpaymentnextmonth, levels = c("0", "1"),
                         labels = c("Yes", "No"))


# table or proportions with more informative labels
round(prop.table(table(wbcd$defaultpaymentnextmonth)) * 100, digits = 1)

# summarize numeric features
summary(wbcd)

# create normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# test normalization function - result should be identical
normalize(c(1, 2, 3, 4, 5))
normalize(c(10, 20, 30, 40, 50))

# normalize the wbcd data
wbcd_n <- as.data.frame(lapply(wbcd[2:23], normalize))

# confirm that normalization worked
summary(wbcd_n$PAY_AMT6)

# create training and test data
wbcd_train <- wbcd_n[1:1000, ]
wbcd_test <- wbcd_n[1001:3000, ]

# create labels for training and test data

wbcd_train_labels <- wbcd[1:1000,24]
wbcd_test_labels <- wbcd[1001:3000,24 ]

## Step 3: Training a model on the data ----

# load the "class" library
library(class)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,cl = wbcd_train_labels, k = 31)

## Step 4: Evaluating model performance ----

# load the "gmodels" library
library(gmodels)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,prop.chisq = FALSE)

## Step 5: Improving model performance ----

# use the scale() function to z-score standardize a data frame
wbcd_z <- as.data.frame(scale(wbcd[2:23]))

# confirm that the transformation was applied correctly
summary(wbcd_z$PAY_AMT6)

# create training and test datasets
wbcd_train <- wbcd_z[1:1000, ]
wbcd_test <- wbcd_z[1001:3000, ]

# re-classify test cases
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k = 21)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq = FALSE)

# try several different values of k
wbcd_train <- wbcd_n[1:1000, ]
wbcd_test <- wbcd_n[1001:3000, ]

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=1)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=5)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=11)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=25)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=31)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=35)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)
