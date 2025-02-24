#name: Autumn Schlecht
# purpose: to demonstate simple r-language to analyze raw data
# date: 02/20/2025
# script upload file: admit.cvs
# Objective of script: 
  # You are interested in how variables such as GRE (Graduate Record Exam scores),
  # GPA (grade point average), and prestige of the undergraduate institution, affect 
  # admission into graduate school. The response variable, admit/don’t admit, is binary.

install.packages(c("tidycensus", "dplyr", "ggplot2", "readxl"))

# Load libraries
library(tidyverse)
library(caTools)

# Set your working directory (replace with your actual path)
getwd()
setwd("C://Desktop/Adv BA Folder")


### PART 1: EDA
#pull in the file for admit.csv to set up for analysis
data <- read.csv("admit.csv")

header <- head(data)
print(header)

#observation of EDA: there are four columns listed as; admit (binary),gre,gpa,and rank of school
gre = data$gre
gpa = data$gpa


#what is the average gpa
avg_gpa <- mean(data$gpa, na.rm = TRUE)
print(avg_gpa)
#  RESULT: the average gpa in the data set is 3.3899

#what is the average gre?
avg_gre <- mean(data$gre,na.rm = TRUE)
print(avg_gre)
##RESULT: average gre score in the data set is 587.7

#order of school rank by most prestiges to least
prestige <- data %>% arrange(desc(rank))


##all code above this was done before class and before I knew the code for the assignment was to e provided.
#Direction of quesitons to answer given in class 2/20/2025
#1. shape of the data set- rows column
dataShape <- dim(data)
print(dataShape)
#RESULT: there are 400 rows with 4 columns

#2. data types
types <- sapply(data,class)
print(types)
#RESLUT: admit-integer, gre-integer, gpa-numeric, rank-numeric.

#4. missing values (consulted ChatGPC for help to find and print what data is missing)
missing <- is.na(data)
print(data[apply(missing, 1, any), ])
##RESULT: admit gre   gpa   rank, <0 rows> (or 0-length row.names)

#5. duplicates
duplicates <- data[duplicated(data), ]
print(duplicates)
## RESULT:    admit gre  gpa rank
        # 60      0 600 2.82    4
        # 78      1 800 4.00    3
        # 203     1 700 4.00    1
        # 264     1 620 3.95    3
        # 399     0 700 3.65    2



### PART 2: Answer the following questions:


##Split the data into training and testing sets.
# Set acceptance for admittance
set.seed(1)

# Split the data set into training (70%) and testing (30%)
split <- sample.split(data$admit, SplitRatio = 0.7)

# Create training and testing sets
# https://search.r-project.org/CRAN/refmans/caTools/html/sample.split.html
train_data <- subset(data, split == TRUE)
test_data <- subset(data, split == FALSE)

# Check data set dimensions
dim(train_data)
dim(test_data)

##check for class balance
table(data$admit)

class.proportions <- prop.table(table(data$admit))
class.proportions
###  1)ANSWER: 
    #In regards to the balance of the classes in the data set, the code(56), the probability of 
    #admittance is 31.75% out of the 400 applicants counted in the data set.

#Fit the Logistic Regression Model

# Train the logistic regression model
log_model <- glm(admit ~ gpa + gre + rank, data = train_data, family = binomial) # Binomial Distribution, Y variable is binary
# https://www.datacamp.com/doc/r/glm 

# Display model summary
summary(log_model)

# Predict probabilities on the test dataset
pred_probs <- predict(log_model, test_data, type = "response")
pred_probs
# Convert probabilities to binary predictions (threshold = 0.5)
pred_classes <- ifelse(pred_probs > 0.5, 1, 0)

# Convert to factor for comparison
pred_classes <- as.factor(pred_classes)

# Display predictions
head(pred_probs)
head(pred_classes)

### Print predictions and true y values as dataframe
do.call(rbind, Map(data.frame, predicted_classes=pred_classes, admit=test_data$admit))

#Evaluate model performance

# Create confusion matrix
conf_matrix <- table(Predicted = pred_classes, Actual = test_data$admit)
conf_matrix

# Compute accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
accuracy

# Print results
print(conf_matrix)
print(paste("Accuracy:", round(accuracy, 4)))


# Visualizing predictions vs. actuals
ggplot(test_data, aes(x = gre, y = as.numeric(as.character(admit)), color = as.factor(pred_classes))) +
  geom_point(size = 3) +
  labs(title = "Predicted vs Actual Admittion",
       x = "Admittance",
       y = "Graduate Record Exam (GRE)") +
  scale_color_manual(values = c("purple", "yellow"), name = "Admitted 1= Yes. 0= No")




###PART 3: Fit model, report, and interpret the accuracy, padmit_to_dont_ratio###PART 3: Fit model, report, and interpret the accuracy, precision, and recal of the model

#create distribution of GRE and describe the distribution (skewed or approximate normal?
  ##  ANSWER: The distribution is what I could consider slightly skewed to the right
    # as it is harder to read putting the data between (0, 1) but with the higher GRE to the right
    # and those accepted with higher GRE of (4) count versus a (2) in the (0.00) line pattern.


# # Which variable in the data is the most important for predicting admission status?
#   ANSWER: I think the most important variable for the prediction of admission is the GPA
    #based on the higher positive coefficient of (.903378) versus the negative fo rank and (.0008)
    # of GRE signaling that the higher GRE positive may have a stonrger influence on admittance.
    # In reference to the ggplot model the count of those admitted with a higher GRE is
    # (4) versus the (2) in the lower half but balanced in the middle compared to the spread of (1.00).
    # a ggplot of actual data may show a different spread.

  
  ### I used chat Gpt to help set up the code below for a direct compatison of the actuals versus my predictions.
ggplot(test_data, aes(x = gre, y = as.numeric(as.character(admit)), color = as.factor(pred_classes))) +
  geom_point(size = 3, alpha = 0.7) +  # add transparency for better overlap visualization
  labs(title = "Predicted vs Actual Admission Status",
       x = "Graduate Record Exam (GRE) Score",
       y = "Admission Status (0 = No, 1 = Yes)",
       color = "Predicted Admission") +
  scale_color_manual(values = c("purple", "green"), name = "Predicted Admit Status",
                     labels = c("Not Admitted", "Admitted")) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  geom_jitter(width = 0.1, height = 0.1, color = "black", alpha = 0.3) 
  
  ##  ANSWER: In comparison of the actual (2nd ggplot) I can see that the prediction that the GRE
    # is strongly influences the admittance.
  