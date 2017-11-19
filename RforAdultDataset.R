# Finding out what the working directory is
getwd()
# Make sure your file is in this working directory

# Create a variable and read your dataframe in
adult<-read.csv("adult_data_short.csv", header=TRUE)
# If you get an overflow error, you will need to shorten your dataset
# --but explain how and why you shortened it

# Inspect your dataset
head(adult)

######################## Standard Statistics #############################

# Produce a 5 number summary for all attributes in the dataset
summary(adult)

# Produce a 5 number summary for the numeric attributes
summary(adult$hoursperweek)
summary(adult$incomeUSD)

# get a ton of good data with the pastecs library
library(pastecs)
stat.desc(adult) 

# aggregate data frame adult by sex, returning means
# for numeric variables
attach(adult)
aggdata <-aggregate(adult, by=list(sex,nativecountry), FUN=mean, na.rm=TRUE)
print(aggdata)
detach(adult)

######################## BOXPLOT #############################

# Produce a BOXPLOT of one variable
boxplot(adult$hoursperweek,ylab='hours per week', xlab='number of adults', main='Adult Dataset')

# Produce a BOXPLOT for variables by groups
boxplot(adult$hoursperweek~adult$sex)

# Produce a BOXPLOT for variables by groups and name the axes
boxplot(adult$incomeUSD~adult$sex,ylab='income in USD', xlab='Sex Attribute')

######################## HISTOGRAM #############################

# Produce a HISTOGRAM for one numeric variable
hist(adult$age)

# Colored HISTOGRAM with different Number of bins
hist(adult$age, breaks=12, col="red")

# Add a NORMAL CURVE to your HISTOGRAM
x <- adult$age 
h<-hist(x, breaks=10, col="red", xlab="Age", 
        main="Histogram with Normal Curve") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)

######################## SCATTERPLOT #############################

# Produce a regular scatterplot
plot(age, incomeUSD, main="Income By Age", xlab="Age", ylab="Income in USD", pch=20, col="red")

# Produce a scatterplot with colored categories using ifelse()
plot(age, incomeUSD, main="Income By Age", xlab="Age", ylab="Income in USD", pch=20, col=ifelse(adult$sex=="Male","blue","red"))

# Produce a scatterplot with a regression line
reg<-lm(age~incomeUSD, data=adult)
plot(age, incomeUSD, main="Income By Age", xlab="Age", ylab="Income in USD", pch=20, col=ifelse(adult$sex=="Male","blue","red"),abline((reg),col="orange"))

# If you use a nominal attribute to plot against, you will generate a boxplot
# Try this:
plot(sex, incomeUSD, main="Income By Age", xlab="sex", ylab="Income in USD", pch=20, col="blue")

######################## CORRELATIONS #############################

# Produce a correlation--only possible for numeric attributes
cor(adult$hoursperweek,adult$incomeUSD)

# Set up a correlation matrix
cor(adult[c('educationyears','hoursperweek','incomeUSD')])



