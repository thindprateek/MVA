###### Applying Linear Discriminant Analysis ######

#Getting working directory 
getwd()

#Setting directory to load data set
setwd("/Users/thindprateek/Desktop/Multivariate Analysis")

#Reading the data into a data frame
#df <- read.csv(file = 'US_Acc_June20.csv')
num <- read.csv(file = 'num.csv')
# Performing clustering on the first 500 records for now to achieve easy and quick results and test the clustering methods
attach(num)
# Printing first few columns of data set for inference
#head(df)

## Setting random seed to shuffle data before splitting 
set.seed(23)

#Checking number of rows
#rows<-sample(nrow(df))

#Shuffling the data
#mva<-df[rows,]

#Taking the required number of instances from the shuffled data to reduce any biases
#mva<-mva[950000:1000000,]

#Checking the structure of the data set
#str(mva)

# Checking the number of rows and columns in the current uncleaned dataset
#ncol(mva)
#nrow(mva)

# Printing all the column names to find and filter the relevant and irrelevant attributes
#names<-names(mva)
#names

## DATA CLEANING ## 

#Dropping the surplus attributes which do not contribute to the analysis
#mva <- mva[-c(1:3,7:10,13,14,19,21:23,33,47:49)]

#Checking for any null values in the present data set
# is.na(mva[,])

#Checking which rows have all the values filled and complete
# complete.cases(mva)

#Making a new dataframe with only the rows that have complete information and all values filled
#Mva<-na.omit(mva)
#Mva<-Mva[!(is.na(Mva$Sunrise_Sunset) | Mva$Sunrise_Sunset==""), ]
#Mva<- Mva[complete.cases(Mva),]
#Verifying for missing values in the new dataframe
#complete.cases(Mva)
#unique(Mva$Sunrise_Sunset)

# Creating new dataframe with only the numerical attributes to perform statistical functions
#num<-Mva[,c(1,4,11:15,17,18)]
#write.csv(num,"/Users/mihikagupta/Desktop/SEM_2/MVA/num.csv", row.names = FALSE)

# Scaling the new data set for better accuracies 
# num<-scale(num)

# Checking the dimensions of the data
nrow(num)
ncol(num)
names(num)
dim(num)

names(num)[names(num) == "Distance.mi."] <- "dist"
names(num)[names(num) == "Temperature.F."] <- "temp"
names(num)[names(num) == "Wind_Chill.F."] <- "windchill"
names(num)[names(num) == "Humidity..."] <- "humidity"
names(num)[names(num) == "Pressure.in."] <- "pressure"
names(num)[names(num) == "Visibility.mi."] <- "visibility"
names(num)[names(num) == "Wind_Speed.mph."] <- "windspeed"
names(num)[names(num) == "Precipitation.in."] <- "precip"
names(num)

num$Severity<- factor(num$Severity)
str(num)

library(MASS)
library(ggplot2)
# Lets cut the data into two parts
smp_size_raw <- floor(0.75 * nrow(num))
train_ind_raw <- sample(nrow(num), size = smp_size_raw)
train_raw.df <- as.data.frame(num[train_ind_raw, ])
test_raw.df <- as.data.frame(num[-train_ind_raw, ])
str(train_raw.df)
# We now have a training and a test set. Training is 75% and test is 25%
num.lda <- lda(formula = train_raw.df$Severity ~ ., data = train_raw.df)
#Prior probability is high for Severity 2 and then Severity 3
#Precipitation seems to be the most important independent variable followed by distance

summary(num.lda)
num.lda$counts
#Severity 2 and 3 have most of the distribution

num.lda$means
num.lda$scaling
num.lda$prior
num$lev
num.lda$svd

plot(num.lda)

#testing accuracy of the model
num.lda.predict <- predict(num.lda)
num.lda.predict$class

num.lda.predict.test <- predict(num.lda, newdata = test_raw.df)
num.lda.predict.test$class

table(num.lda.predict$class, train_raw.df$Severity)

table(num.lda.predict.test$class, test_raw.df$Severity )
#you can see through tables, ratios are close to correct prediction

