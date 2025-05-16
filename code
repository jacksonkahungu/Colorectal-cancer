# Colorectal-cancer
#Colorectal cancer in Nairobi analysis
x=read.csv(file.choose())
x
na.omit(x)
model=glm(Status~.,data=x,family=binomial)
summary(model)
library(car)
library(ResourceSelection)
library(readxl) 
library(ggplot2)
library(caret)
hoslem.test <- hoslem.test(model$y, fitted(model)) 
hoslem.test
# Wald test for significance of coefficients
wald.test <- Anova(model, type="III")
wald.test
predictions=predict(model,type="response")
Predictions
status=x$Status
accuracy<-mean（predictions==status)*100
accuracy
print(paste ("Accuracy:", round(accuracy, 2) , "%")
cancer=read.csv(file.choose())
table(cancer$AGE,cancer$Status)
table(cancer$Primary.Site,cancer$Status)
table(cancer$TIME,cancer$Status)
table(cancer$Sex,cancer$Status)
table(cancer$Status)
# Load the ggplot2 library
library(ggplot2)
# Create a dataframe with the provided data
data <- data.frame(
  age = c(17, 18, 19, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 82, 83, 84, 85, 87),
  Alive = c(2, 1, 2, 1, 3, 1, 2, 2, 3, 3, 4, 2, 1, 4, 9, 11, 4, 7, 15, 4, 10, 8, 10, 8, 8, 8, 8, 12, 13, 9, 7, 11, 11, 8, 13, 16, 9, 5, 9, 8, 12, 9, 16, 14, 13, 9, 16, 13, 7, 12, 9, 5, 7, 9, 16, 6, 6, 10, 4, 4, 1, 1, 2, 1, 3, 2, 3, 2),
  Dead = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 1,1, 0, 0, 0, 1, 0, 1, 2, 1, 0, 0, 1, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1,0,0)
)
library(reshape2)
melt_data <- melt(data, id.vars = "age")
melt_data
# Plot the data with x-axis spaced by 2 years
ggplot(melt_data, aes(x = age, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Histogram of Age and Status",
       x = "Age",
       y = "Frequency",
       fill = "outcome") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(breaks = seq(min(melt_data$age), max(melt_data$age), by = 2))
data_A <- data.frame(outcome = c("Alive", "Dead"), count = c(484, 22))
ggplot(data_A, aes(x = outcome, y = count, fill = status)) +
  geom_bar(stat = "identity") +
  labs(title = "Histogram of patients outcome",
       x = "outcome",
       y = "Count") +
  theme_minimal()
data_H<- data.frame(
  sex = c("Female", "Male"),
  Alive = c(217,267),
  Dead = c(11,11)
)
ggplot(data_H, aes(x = sex)) +
  geom_bar(aes(y = Alive, fill = "Alive"), stat = "identity") +
  geom_bar(aes(y = Dead, fill = "Dead"), stat = "identity") +
  labs(title = "Histogram for gender",
       x = "Sex",
       y = "Count",
       fill = "outcome") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
data_D <-  data.frame(
  primarysite = c("COLON", "RECTOSIGMOID", "RECTUM"),
  Alive = c(283,73,128),
  Dead = c(14,4,4)
)
ggplot(data_D, aes(x = primarysite)) +
  geom_bar(aes(y = Alive, fill = "Alive"), stat = "identity") +
  geom_bar(aes(y = Dead, fill = "Dead"), stat = "identity") +
  labs(title = "Histogram of Primary Site outcomes",
       x = "Primary Site",
       y = "Count",
       fill = "outcome") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
data_G <- data.frame(
  time = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 
           11, 12, 13, 14, 15, 16, 17, 18, 19, 20,21,22, 23,
            24,25,26,27, 29,30,32, 34,35,39,40, 42,43,46,48,51),
  Alive = c(115,37,38,34,19,30,23,19,32,13,22,16,11,6,7,4,
            6,6,5,2,6,5,8,1,1,2,1,2,2,1,1,1,1,1,2,1,1,1,1),
  Dead = c(9,1,1,2,1,3,1,1,0,0,1,0,0,0,0,1,
            1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
)
ggplot(data_G, aes(x = time)) +
  geom_bar(aes(y = Alive, fill = "Alive"), stat = "identity") +
  geom_bar(aes(y = Dead, fill = "Dead"), stat = "identity") +
  labs(title = "Histogram for Time",
       x = "Time in Months",
       y = "Count",
       fill = "Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)
