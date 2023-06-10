#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Capstone Project 2
# Name: Aw Kar Kei
# Topic/Theme: Impact of Instagram and Tiktok on Body Image Dissatisfaction Among Malaysian Adults
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~ Data Preparation and Initial Data Exploration ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Install necessary packages
install.packages("DataExplorer") #For data exploration
install.packages("Boruta") #Feature Selection

# Read dataset from a CSV file
library(DataExplorer)
data <- read.csv("/Users/manda/Downloads/Impact of Instagram and TikTok on Body Image Dissatisfaction among Malaysian Adults.csv")
View(data)

# create a named vector with old names as names and new names as values
col_dict <- c(
  "2" = "Consent", 
  "5" = "EduLevel",
  "6" = "SocMed",
  "7" = "Publicity",
  "8" = "TimeSpent",
  "9" = "Following",
  "10" = "SelfCompare",
  "11" = "EditPhoto",
  "12" = "SelfContented",
  "13" = "BESAA1",
  "14" = "BESAA2",
  "15" = "BESAA3",
  "16" = "BESAA4",
  "17" = "BESAA5",
  "18" = "BESAA6",
  "19" = "BESAA7",
  "20" = "BESAA8",
  "21" = "BESAA9",
  "22" = "BESAA10",
  "23" = "BESAA11",
  "24" = "BESAA12",
  "25" = "BESAA13",
  "26" = "BESAA14",
  "27" = "BESAA15",
  "28" = "BESAA16",
  "29" = "BESAA17",
  "30" = "BESAA18",
  "31" = "BESAA19",
  "32" = "BESAA20",
  "33" = "BESAA21",
  "34" = "BESAA22",
  "35" = "BESAA23"
)

# loop through the dictionary and rename columns in data
for (key in names(col_dict)) {
  colnames(data)[as.integer(key)] <- col_dict[[key]]
}
View(col_dict)

# Create a dictionary using list to understand details of variables 
body_image_dict <- list(
  "EduLevel" = "Current education level",
  "SocMed" = "Most used social media platform",
  "Publicity" = "Publicity of respondent's most used social media account",
  "TimeSpent" = "Time spent on most used social media account",
  "Following" = "Tendency of respondent following influencers/celebrities/clothing brands/fashion accounts on most used social media account",
  "SelfCompare" = "Tendency to compare their physical appearance while scrolling social media",
  "EditPhoto" = "Tendency to edit self-portraits before posting on social media",
  "SelfContented" = "Tendency to feel self-contented when receiving positive comments or likes on physical appearance on social media",
  "BESAA1" = "I like what I look like in pictures.",
  "BESAA2" = "Other people consider me good looking.",
  "BESAA3" = "I'm proud of my body.",
  "BESAA4" = "I am preoccupied with trying to change my body weight.",
  "BESAA5" = "I think my appearance would help me get a job.",
  "BESAA6" = "I like what I see when I look in the mirror.",
  "BESAA7" = "There are lots of things I'd change about my looks if I could.",
  "BESAA8" = "I am satisfied with my weight.",
  "BESAA9" = "I wish I looked better.",
  "BESAA10" = "I really like what I weigh.",
  "BESAA11" = "I wish I looked like someone else.",
  "BESAA12" = "People my own age like my looks.",
  "BESAA13" = "My looks upset me.",
  "BESAA14" = "I'm as nice looking as most people.",
  "BESAA15" = "I'm pretty happy about the way I look.",
  "BESAA16" = "I feel I weigh the right amount for my height.",
  "BESAA17" = "I feel ashamed of how I look.",
  "BESAA18" = "Weighing myself depresses me.",
  "BESAA19" = "My weight makes me unhappy.",
  "BESAA20" = "My looks help me to get dates.",
  "BESAA21" = "I worry about the way I look.",
  "BESAA22" = "I think I have a good body.",
  "BESAA23" = "I'm looking as nice as I'd like to.",
  "BE_Appearance" = "Feelings about appearance",
  "BE_Attribution" = "Feelings about other opinions on own's body image",
  "BE_Weight" = "Feelings about weight",
  "Sum_BE" = "Total Score of Body Esteem Scale for Adolescents and Adults",
  "BID" = "Tendency of respondents to have body image dissatisfacton"
)
View(body_image_dict)

# Handle missing data with NA
data[data == ''] <- NA

# Check for any missing data in the Body Image dataframe
colSums(is.na(data))

# Remove responses that do not meet the requirements
data <- data[data$Consent != "No", ] #Respondents who did not provide consent
data <- data[data$Age != "36-45 years old", ] #Respondents who are more than age of 36 and above

# Remove variables that are not relevant to the research
# Timestamp and Consent are not useful for further analysis
data$Timestamp <- NULL
data$Consent <- NULL

dim(data) #View total observations and variables in the dataset
str(data) #View structure of the dataset
head(data) #Simple view of dataset

#Export the dataset that has removed the irrelevant variables and observations that did not meet the requirements
write.csv(data, file = "bodyimage.csv", row.names = FALSE)

# A total of 138 observations in the dataset after removing responses that do not meet requirements.
# 33 variables are involved in the dataset after removing 2 variables that are irrelevant to the research.
# There is a missing value in both EditPhoto and SelfContented variables.
# Currently there are 10 variables that are "character" data type, where as 23 variables that are "integer" data type.