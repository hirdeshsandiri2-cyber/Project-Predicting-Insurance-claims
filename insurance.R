

# import required lib's
install.packages("devtools")
install.packages("DataExplorer")

#to install a package called skimr directly from GitHub using the devtools package.
devtools::install_github("ropensci/skimr")

library(skimr)                  # to get a quick summary table
library(ggplot2)
library(DataExplorer)
library(readr)
library(caret)

# ----------------------- import data from file
insurance <- read_csv('insurance.csv')
insurance <- read_csv("C:/Users/sandi/Desktop/insurance.csv")
summary(insurance)

# convert char type to factor
insurance$sex <- factor(insurance$sex)
insurance$smoker <- factor(insurance$smoker)
insurance$region <- factor(insurance$region)
insurance$children <- factor(insurance$children)


# -------------------- univariant analysis
skim(insurance) # summary
#skim() function is a quick way to generate summary statistics and visualizations for a dataset's 
# structure, contents, and missing values


#BARPLOTS THE DISTRIBUTIONS IN SEX ,CHILDREN, SMOKER OR NOT AND REGION.
isfact <- colnames(insurance)[sapply(insurance, is.factor)]
iscont <- colnames(insurance)[!sapply(insurance, is.factor)]
par(mfrow=c(2,2))# yes is red and no is blue
for (i in isfact)
{
barplot(t(prop.table(table(insurance[,i]))) * 100, main=paste(i," distribution"))
}
#dev.off() is a function that is used to close the current graphical device 
#after creating a plot or a graphical output.

dev.off()

#make sure you install package DataExplorer
#by using install.packages("DataExplorer")
#density plots
DataExplorer::plot_density(insurance)

#box plots
par(mfrow=c(1,3))
for (i in iscont)
{
  print(i)
  boxplot(insurance[,i],horizontal = TRUE, main = paste(i))
}


# ------------------- bi and multi variant analysis

DataExplorer::plot_correlation(insurance)

DataExplorer::plot_scatterplot(insurance, by = "bmi")
DataExplorer::plot_boxplot(insurance, by = "region")

#"""
#I see slight positive relation between
#o	Charges – Age 
#o	Charges – BMI 
#o	Charges – Smoker YES 
#o	BMI – region southeast
#o	Age – BMI 

#I see slight negative relation between
#o	Charges – Smoker NO
#o	BMI – region Northeast
#o	BMI – region Northwest

#"""

# charges vs age

scatterplot(charges ~ age , data=insurance,
            xlab="age", ylab="charges",
            main="Charges vs Age Scatter Plot",
            )

scatterplot(charges ~ age | smoker , data=insurance,
            xlab="age", ylab="charges",
            main="Charges vs Age Scatter Plot",
)


# charges vs BMI 

scatterplot(charges ~ bmi , data=insurance,
            xlab="bmi", ylab="charges",
            main="Charges vs BMI Scatter Plot",
)

scatterplot(charges ~ bmi | smoker , data=insurance,
            xlab="bmi", ylab="charges",
            main="Charges vs BMI Scatter Plot" )

# charges and smoker groups
par(mfrow=c(1,1))
boxplot(insurance$charges ~ insurance$smoker)


# bmi AND region
par(mfrow=c(1,1))
boxplot(insurance$bmi ~ insurance$region)


# age and BMI
scatterplot(bmi ~ age, data=insurance,
            xlab="age", ylab="bmi",
            main="BMI vs Age Scatter Plot",
)


# -------------------- hypothesis testing

t.test(insurance$charges~insurance$smoker)


kruskal.test(insurance$charges~insurance$region)


# ------------------ linear regession 
# do log transformation because there is lot of skewness in the data
insurance$logCharges<- log10(insurance$charges)
# prepare data
set.seed(122) 
#training samples
training.samples <- insurance$logCharges %>%
  createDataPartition(p = 0.8, list = FALSE)
train  <- insurance[training.samples, ]
test <- insurance[-training.samples, ]

model <- lm(logCharges ~ smoker + bmi + age + children + sex + region, data = insurance)
summary(model)

# ------------------ predictions 
predictions <- model %>% predict(train)
trainresiduals <- 10^train$logCharges - 10^predictions # backtransform measured and predicted values
trainrmse <- sqrt(mean(trainresiduals^2))


predictions <- model %>% predict(test)
testresiduals <- 10^test$logCharges - 10^predictions # backtransform measured and predicted values
testrmse <- sqrt(mean(testresiduals^2))

