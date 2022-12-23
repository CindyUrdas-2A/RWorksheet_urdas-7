library("Hmisc")
library("pastecs")


#1. Create a data frame for the table below 
Student <- seq(1:10)
PreTest <- c(55,54,47,57,51,61,57,54,63,58)
PostTest <- c(61,60,56,63,56,63,59,56,62,61)

Df <- data.frame(Student,PreTest,PostTest)
Df

#a. Compute the descriptive statistics using different packages (Hmisc and pastecs).
#Write the codes and its result.
library(Hmisc)
library(pastecs)

describe(Df)

stat.desc(Df)

#2. The Department of Agriculture was studying the effects of several levels of a
#fertilizer on the growth of a plant. For some analyses, it might be useful to convert
#the fertilizer levels to an ordered factor.
DPA <- c(10,10,10,20,20,50,10,
         20,10,50,20,50,20,10)



#a. Write the codes and describe the result.
InOrd<- sort(DPA, decreasing = FALSE)
InOrd

#3. Abdul Hassan, president of Floor Coverings Unlimited, has asked you to study
#the exercise levels undertaken by 10 subjects were “l”, “n”, “n”, “i”, “l” ,
#“l”, “n”, “n”, “i”, “l” ; n=none, l=light, i=intense
Subs <- c("l","n","n","i","l","l","n","n","i","l")
Subs

#a. What is the best way to represent this in R?
repre <- data.frame(Subs)
repre

#4.Sample of 30 tax accountants from all the states and territories of Australia and
#their individual state of origin is specified by a character vector of state mnemonics
#as:
state <- c("tas", "sa", "qld", "nsw", "nsw", "nt", "wa", "wa", "qld",
           "vic", "nsw", "vic", "qld", "qld", "sa", "tas", "sa", "nt",
           "wa", "vic", "qld", "nsw", "nsw", "wa", "sa", "act", "nsw",
           "vic", "vic", "act")
state  

#a. Apply the factor function and factor level. Describe the results.
state <- c("tas", "sa", "qld", "nsw", "nsw", "nt", "wa", "wa", "qld",
           "vic", "nsw", "vic", "qld", "qld", "sa", "tas", "sa", "nt",
           "wa", "vic", "qld", "nsw", "nsw", "wa", "sa", "act", "nsw", "vic", "vic", "act")
state

statef <- factor(state)
statef

levels(statef)

#5. From #4 - continuation:
#Suppose we have the incomes of the same tax accountants in another vector (in
incomes <- c(60, 49, 40, 61, 64, 60, 59, 54,
             62, 69, 70, 42, 56, 61, 61, 61, 58, 51, 48,
             65, 49, 49, 41, 48, 52, 46, 59, 46, 58, 43)

#a. Calculate the sample mean income for each state we can now use the special
#function tapply():
incmeans <- tapply(state, incomes, mean)
incmeans

#b. Copy the results and interpret.
> incmeans
40 41 42 43 46 48 49 51 52 54 56 58 59 60 61 62 64 65 69 70 
NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA 

#6.Calculate the standard errors of the state income means (refer again to number 3)
#a. What is the standard error? Write the codes.
stdError <- function(x) sqrt(var(x)/length(x))
incster <- tapply(incomes, state, stdError)
incster

#7. Use the titanic dataset.
data("Titanic")
force(Titanic)

head<- data.frame(Titanic)
head

#a. subset the titatic dataset of those who survived and not survived. Show the
#codes and its result.
head_subset <- subset(head, select = "Survived")
head_subset

#8. The data sets are about the breast cancer Wisconsin. 
#The samples arrive periodically as Dr. Wolberg reports his clinical cases. 
#The database therefore reflects this
#chronological grouping of the data. You can create this dataset in Microsoft Excel.

#a. describe what is the dataset all about.
 The datasets all about Breast Cancer.

#b. Import the data from MS Excel. Copy the codes.
library(readxl)
Breast_Cancer <- read_excel("Breast_Cancer.xlsx")
View(Breast_Cancer)
 
#c. Compute the descriptive statistics using different packages. Find the values of:
#c.1 Standard error of the mean for clump thickness.
Clump <- length(Breast_Cancer$`CL. thickness`)
Clump_a <- sd(Breast_Cancer$`CL. thickness`)
Clump_a 
Clump_b <- Clump_a/sqrt(Breast_Cancer$`CL. thickness`)
Clump_b

#c.2 Coefficient of variability for Marginal Adhesion.
COV <- sd(Breast_Cancer$`Marg. Adhesion`) / mean(Breast_Cancer$`Marg. Adhesion`)* 100
COV

#c.3 Number of null values of Bare Nuclei.
Null_Val <- subset(Breast_Cancer,`Bare. Nuclei` == "NA")
Null_Val

#c.4 Mean and standard deviation for Bland Chromatin
mean(Breast_Cancer$`Bl. Cromatin`)
sd(Breast_Cancer$`Bl. Cromatin`)

#c.5 Confidence interval of the mean for Uniformity of Cell Shape
#Calculate the mean
Cal_mean<- mean(Breast_Cancer$`Cell Shape`)
Cal_mean

#Calculate the standard error of the mean
SE_mean <- length(Breast_Cancer$`Cell Shape`)
SD <- sd(Breast_Cancer$`Cell Shape`)
A1 <- SD/sqrt(SE_mean)
A1

#Find the t-score that corresponds to the confidence level
D = 0.05
nE = SM - 1
nF = qt(p = D/ 2, df = nE,lower.tail = F)
nF

#d. How many attributes?
attributes(Breast_Cancer)

#e. Find the percentage of respondents who are malignant. Interpret the results.
Per_res <- subset(Breast_Cancer, Class == "maligant")
Per_res

#Getting the percentage
17  / 49 * 100
#9. Export the data abalone to the Microsoft excel file. Copy the codes.

library("AppliedPredictiveModeling")
data("abalone")
head(abalone)
summary(abalone)

data.frame(abalone)

#Exporting the data abalone to the Microsoft excel file
library(readxl)
abalone <- read_excel
