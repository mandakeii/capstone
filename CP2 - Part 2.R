#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Capstone Project 2
# Name: Aw Kar Kei
# Topic/Theme: Impact of Instagram and Tiktok on Body Image Dissatisfaction Among Malaysian Adults
# Part 2 - Data Understanding, EDA, Feature Selection
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



#~~~~~~~~~~~~~~~~ Step 2: Data Understanding ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Install necessary packages
install.packages("DataExplorer") # To read CSV file
install.packages("dplyr") # For data manipulation and data transformation
install.packages("ggplot2") # For data visualization
install.packages("Boruta") # Feature Selection

# Read dataset from a CSV file
library(DataExplorer)
body_image <- read.csv("/Users/manda/Downloads/bodyimage.csv")
View(body_image)

# Find the mode of EditPhoto variable
mode_EditPhoto <- names(which.max(table(body_image$EditPhoto)))

# Find the mode of SelfContented variable
mode_SelfContented <- names(which.max(table(body_image$SelfContented)))

# Replace NA values with the mode found on the EditPhoto and SelfContented variable
library(dplyr)
body_image <- body_image %>% mutate(EditPhoto = replace(EditPhoto,is.na(EditPhoto),"No"), 
                                    SelfContented = replace(SelfContented,is.na(SelfContented),"Yes")) #The modes for each variable are shown in the Values in the Environment.

# Recheck for any missing data in the Body Image dataframe
colSums(is.na(body_image))
View(body_image) #Double check if there are still any missing values


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Data Transformation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Convert variables to Factor and Numeric variables
body_image <- body_image %>% 
  mutate_if(~n_distinct(.) < 5, as.factor) #All categorical variables only have less than 5 unique values 
body_image <- body_image %>% 
  mutate_if(~n_distinct(.) >= 5, as.numeric) #All numerical variables only have 5 unique values 
str(body_image) # Double check on the data type

# Reverse the Likert scale for negative items from the BESAA variables
body_image$BESAA4 <- 4 - body_image$BESAA4
body_image$BESAA7 <- 4 - body_image$BESAA7
body_image$BESAA9 <- 4 - body_image$BESAA9
body_image$BESAA11 <- 4 - body_image$BESAA11
body_image$BESAA13 <- 4 - body_image$BESAA13
body_image$BESAA17 <- 4 - body_image$BESAA17
body_image$BESAA18 <- 4 - body_image$BESAA18
body_image$BESAA19 <- 4 - body_image$BESAA19
body_image$BESAA21 <- 4 - body_image$BESAA21


# Create sub-scales for Body Image Dissatisfaction
#body_image <- body_image %>% mutate(BE_Appearance = BESAA1 + BESAA6 + BESAA7 + BESAA9 + BESAA11 + BESAA13 + BESAA15 + BESAA17 + BESAA21 + BESAA23)
#body_image <- body_image %>% mutate(BE_Attribution = BESAA2 + BESAA5 + BESAA12 + BESAA14 + BESAA20)
#body_image <- body_image %>% mutate(BE_Weight = BESAA3 + BESAA4 + BESAA8 + BESAA10 + BESAA16 + BESAA18 + BESAA19 + BESAA22) ~~~ # 

# Create new variables to calculate the sum of BESAA score
body_image <- body_image %>% mutate(Sum_BE = BESAA1 + BESAA2 + BESAA3 + BESAA4 + BESAA5 + BESAA6 + BESAA7 + BESAA8 + BESAA9 + BESAA10 + BESAA11 + BESAA12 + BESAA13 + BESAA14 + BESAA15 + BESAA16 + BESAA17 + BESAA18 + BESAA19 + BESAA20 + BESAA21 + BESAA22 + BESAA23)
body_image$BID <- ifelse(body_image$Sum_BE > 46, "No", "Yes") 
head(body_image)
body_image$BID <- as.factor(body_image$BID) #Change data type of BID to factor

# Create another dataframe as a backup dataset
data1 <- body_image

head(body_image)
plot_str(body_image)

# ~~~~~~~~~~~~~~~~ Exploratory Data Analysis (EDA) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Univariate Analysis
# Gender
library(ggplot2)
bar <- ggplot(body_image, aes(x = Gender, fill = Gender))
bar + geom_bar() + theme_light() + 
  labs(title = "Gender Distribution")
body_image %>% count(Gender)  

# Age
bar <- ggplot(body_image, aes(x = Age, fill = Age))
bar + geom_bar() + theme_light() + 
  labs(title = "Age Distribution")
body_image %>% count(Age)  

# Education Level
bar <- ggplot(body_image, aes(x = EduLevel, fill = EduLevel))
bar + geom_bar() + theme_light() + 
  labs(title = "Education Level Distribution")
body_image %>% count(EduLevel)

# Most Used Social Media
bar <- ggplot(body_image, aes(x = SocMed, fill = SocMed))
bar + geom_bar() + theme_light() + 
  labs(title = "Most Used Social Media Distribution")
body_image %>% count(SocMed) 

# Publicity of Most Used Social Media Account
bar <- ggplot(body_image, aes(x = Publicity, fill = Publicity))
bar + geom_bar() + theme_light() + 
  labs(title = "Publicity of Most Used Social Media Distribution")
body_image %>% count(Publicity) 

# Time Spent on Most Used Social Media Account
bar <- ggplot(body_image, aes(x = TimeSpent, fill = TimeSpent))
bar + geom_bar() + theme_light() + 
  labs(title = "Time Spent on Most Used Social Media Distribution")
body_image %>% count(TimeSpent) 

# Tendency of Respondents Following influencers/celebrities/clothing brands/fashion accounts on most used social media account
bar <- ggplot(body_image, aes(x = Following, fill = Following))
bar + geom_bar() + theme_light() + 
  labs(title = "Tendency of Respondents Following Influencers/Celebrities/Clothing Brands/Fashion Accounts Distribution")
body_image %>% count(Following) 

# Tendency to Compare Physical Appearance While Scrolling Social Media
bar <- ggplot(body_image, aes(x = SelfCompare, fill = SelfCompare))
bar + geom_bar() + theme_light() + 
  labs(title = "Tendency to Compare Physical Appearance While Scrolling Social Media Distribution")
body_image %>% count(SelfCompare) 

# Tendency to Edit Self-Portraits Before Posting on Social Media
bar <- ggplot(body_image, aes(x = EditPhoto, fill = EditPhoto))
bar + geom_bar() + theme_light() + 
  labs(title = "Tendency to Edit Self-Portraits Before Posting on Social Media Distribution")
body_image %>% count(EditPhoto) 

# Tendency to Feel Self-Contented When Receiving Positive Comments or Likes on Physical Appearance on Social Media
bar <- ggplot(body_image, aes(x = SelfContented, fill = SelfContented))
bar + geom_bar() + theme_light() + 
  labs(title = "Tendency to Feel Self-Contented When Receiving Positive Comments or Likes on Physical Appearance on Social Media Distribution")
body_image %>% count(SelfContented) 

# Body Image Dissatisfaction
bar <- ggplot(body_image, aes(x = BID, fill = BID))
bar + geom_bar() + theme_light() + 
  labs(title = "Tendency to have body image dissatisfaction")
body_image %>% count(BID) 


#Bivariate Analysis
# Hypothesis 1:
# H0: Gender does not have any effect on Body Image Dissatisfaction
# H1: Gender does have an effect on Body Image Dissatisfaction

bar <- ggplot(body_image, aes(x = Gender, fill = BID))
bar + geom_bar(position="fill") + theme_light() +
  labs(y = "Respondents Count", title = "Body Image Dissatisfaction by Gender") 

table(body_image$Gender, body_image$BID)
chisq.test(body_image$Gender, body_image$BID, correct=FALSE)

# Accept H0 is accepted as the p-value is more than 0.05

# Hypothesis 2: 
# H0: Self-comparison on social media does not have any affect on body image dissatisfaction
# H1: Self-comparison on social media does have an affect on body image dissatisfaction
bar <- ggplot(body_image, aes(x = SelfCompare, fill = BID))
bar + geom_bar(position="fill") + theme_light() +
  labs(y = "Respondents Count", title = "Body Image Dissatisfaction by Self-Comparison on Social Media") 

table(body_image$SelfCompare, body_image$BID)
chisq.test(body_image$SelfCompare, body_image$BID, correct=FALSE)

# Accept H1 is accepted as the p-value is less than 0.05

# Hypothesis 3: 
# H0: Time spent on social media does not have any affect on body image dissatisfaction
# H1: Time spent on social media does have an affect on body image dissatisfaction
bar <- ggplot(body_image, aes(x = TimeSpent, fill = BID))
bar + geom_bar(position="fill") + theme_light() +
  labs(y = "Respondents Count", title = "Body Image Dissatisfaction by Time Spent on Social Media") 

table(body_image$TimeSpent, body_image$BID)
chisq.test(body_image$TimeSpent, body_image$BID, correct=FALSE)
# Accept H1 is accepted as the p-value is less than 0.05


######## Age/Education Level/Social Media Type/Tendency to follow influencers/Tendency to Edit Photo/Feeling Self-Contented  vs. Body Image Dissatisfaction

chisq.test(body_image$Age, body_image$BID, correct=FALSE)
chisq.test(body_image$EduLevel, body_image$BID, correct=FALSE)
chisq.test(body_image$SocMed, body_image$BID, correct=FALSE)
chisq.test(body_image$Publicity, body_image$BID, correct=FALSE)
chisq.test(body_image$Following, body_image$BID, correct=FALSE)
chisq.test(body_image$EditPhoto, body_image$BID, correct=FALSE) 
chisq.test(body_image$SelfContented, body_image$BID, correct=FALSE)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Feature Selection using Boruta Package ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# With BESAA survey variables
library(Boruta)
BI_Boruta <- Boruta(BID ~ ., data = body_image, doTrace = 2)
print(BI_Boruta)
plot(BI_Boruta, las = 2, cex.axis = 0.7)
attStats(BI_Boruta)

# Take a call on tentative features
BI_Boruta_Fix <- TentativeRoughFix(BI_Boruta)
print(BI_Boruta_Fix)
attStats(BI_Boruta_Fix)

# This indicates that BESAA1-3, BESAA6, BESAA8-10, BESAA13-20, BESAA22-23 are all important.

# Conduct Feature Selection only for Demographic Variables
body_image_F <- select_if(body_image, is.factor)
View(body_image_F) #To confirm that the dataset only includes demographic variables.
BI_Boruta2 <- Boruta(BID ~ ., data = body_image_F, doTrace = 3)
print(BI_Boruta2)
plot(BI_Boruta2, las = 2, cex.axis = 0.7)
attStats(BI_Boruta2)

# Take a call on tentative features on Demographic Variables 
BI_Boruta_Fix2 <- TentativeRoughFix(BI_Boruta2)
print(BI_Boruta_Fix2)
attStats(BI_Boruta_Fix2)

# No demographic variables are deemed important.
# Only SelfCompare and SelfContented are deemed important.



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Step 3: Clustering ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# One-Hot Encoding for categorical data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
body_image$Gender = factor(body_image$Gender,
                           levels = c('Male', 'Female'),
                           labels = c(0, 1))
body_image$Age = factor(body_image$Age,
                        levels = c('18-25 years old', '26-35 years old'),
                        labels = c(0, 1))
body_image$EduLevel = factor(body_image$EduLevel,
                             levels = c('High School', 'Foundation/Diploma','Undergraduate/Bachelor Degree','Graduate/Masters Degree'),
                             labels = c(0, 1, 2, 3))
body_image$SocMed = factor(body_image$SocMed,
                           levels = c('Instagram', 'Tiktok'),
                           labels = c(0, 1))
body_image$Publicity = factor(body_image$Publicity,
                              levels = c('Public', 'Private'),
                              labels = c(0, 1))
body_image$TimeSpent = factor(body_image$TimeSpent,
                              levels = c('Less than 1 hour per day', '1-2 hours per day','More than 2 hours per day'),
                              labels = c(0, 1, 2))
body_image$Following = factor(body_image$Following,
                              levels = c('No', 'Yes'),
                              labels = c(0, 1))
body_image$SelfCompare = factor(body_image$SelfCompare,
                                levels = c('No', 'Yes'),
                                labels = c(0, 1))
body_image$EditPhoto = factor(body_image$EditPhoto,
                              levels = c('No', 'Yes'),
                              labels = c(0, 1))
body_image$SelfContented = factor(body_image$SelfContented,
                                  levels = c('No', 'Yes'),
                                  labels = c(0, 1))
body_image$BID = factor(body_image$BID,
                        levels = c('No', 'Yes'),
                        labels = c(0, 1))

##Install necessary packages for clustering
install.packages("factoextra")
install.packages("cluster")

# Load packages for clustering 
library(factoextra)
library(cluster)

#make this example reproducible
set.seed(123)

#create a plot of the number of clusters vs. the total within sum of squares
fviz_nbclust(body_image, kmeans, method = "wss")
km <- kmeans(body_image, centers = 2, nstart = 25)
#view results
km

#51 respondents are assigned to the first cluster
#87 respondents are assigned to the second cluster

#find means of each cluster
aggregate(body_image, by=list(cluster=km$cluster), mean)

# The mean number of Body Esteem score in cluster 1 is 31.94118.
# The mean number of Body Esteem score in cluster 2 is 54.73563.

#add cluster assignment to original data
body_image_1 <- cbind(body_image, cluster = km$cluster)
body_image_1$cluster <- factor(body_image_1$cluster)
str(body_image_1)
str(body_image)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Step 4: Modelling ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Install necessary packages
install.packages("lattice") 


# ~~~~~~~~~~~~~~~~~~~~~~ Model 1: Naive Bayes ~~~~~~~~~~~~~~~~~~~~~~~~~~
install.packages('GGally')
install.packages('klaR')
library(GGally)
prop.table(table(body_image_1$cluster)) * 100

# split data into training and test data sets
library(caret)
set.seed(123)
indxTrain <- createDataPartition(y = body_image_1$cluster, p = 0.70, list = FALSE)
train <- body_image_1[indxTrain,]
test <- body_image_1[-indxTrain,]
dim(train)
prop.table(table(train$cluster)) * 100
dim(test)
prop.table(table(test$cluster)) * 100

# Assigning IV and DV
x = train[,-36]
y = train$cluster

# Building model ---------------------------------
library(e1071)
library(caret)

# Model building using Cross Validation
# Set up the train control
train_control <- trainControl(method = "cv", number = 10)
# Train the Naive Bayes model
model <- train(cluster ~ ., data = body_image_1, trControl = train_control, method = "nb")
model
confusionMatrix(model)

#Accuracy of 95.65%.


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Model 2: Logistic Regression ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

install.packages("jtools")
install.packages("pscl")
install.packages("aod")

# Building classifier
logit_classifier = glm(cluster ~., train, family = binomial, control = list(maxit = 100))
summary(logit_classifier)

coef(logit_classifier)
confint(logit_classifier)
anova(logit_classifier, test="Chisq")
# Age, TimeSpent, BESAA1, BESAA3, BESAA4, BESAA6-8 are significant variables.


#Make predictions 
log_predict <- predict(logit_classifier, 
                       test, type = "response")
log_predict <- round(log_predict)
log_predict
log_predict <- ifelse(log_predict == 0, 1, 2)

# Convert test$cluster to factor
test$cluster <- factor(test$cluster)

# Convert log_predict to factor
log_predict <- factor(log_predict)

#Create confusion matrix
log_table <- table(Actualvalue=test$cluster,Predictedvalue=log_predict>0.5) # assuming thershold to be 0.5
log_table <- as.factor(log_table)
str(log_table)
confusionMatrix(test$cluster,log_predict)

#shows an accuracy of 75.60976% for Logistic Regression.



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Model 3: Decision Tree ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

install.packages("rpart") # (Recursive Partitioning And Regression Trees) and the R implementation of the CART algorithm
install.packages("rpart.plot")
install.packages("party")

library(rpart)
library(rpart.plot)
library(party) #Ctree. not for regression, only for classifcation.

"Can generate different types of trees with rpart
Default split is with Gini index"

tree = rpart(cluster~ ., data=train) #TV, and the model. ~ . = rest of the variable except Class. 
tree  
prp(tree) # plot Rpart Model, raw diagram w/o info. 
prp (tree, type = 5, extra = 100) 
rpart.plot(tree, extra = 101, nn = TRUE) # nn displays the node numbers

# This code generates the tree with training data
tree_with_params = rpart(cluster ~ ., data=train, method="class", minsplit = 2, minbucket = 10, cp = 0)
rpart.plot(tree_with_params, extra = 100, nn = TRUE)
prp (tree_with_params)
print(tree_with_params)
summary(tree_with_params)
plotcp(tree_with_params)

# Now we predict and evaluate the performance of the trained tree model 
Predict = predict(tree_with_params, test, type = "class")
Predict

# Producing confusion matrix
Confusion_matrix = table(Predict, test$cluster)
Confusion_matrix

# Calculating the accuracy using the cofusion matrix
Accuracy = sum(diag(Confusion_matrix))/sum(Confusion_matrix)
Accuracy

# Performance of the DT model
library(caret)
confusionMatrix(Predict, test$cluster)
#Accuracy of 100%.


# Variable Importance
varImp(tree_with_params)
tree_with_params$variable.importance
#BESAA10, BESAA3, BESAA15, BESAA22

