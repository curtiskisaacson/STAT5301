##Lecture 1.R##

#Change working directory.
getwd()
setwd("C:/Users/kelbick.1/Documents/Teaching/5301/Data/Lectures")

#Read data into a dataframe called speeds
speeds = read.table("speed_of_light.txt", header=TRUE)$Times

#or

setwd("C:/Users/kelbick.1/Documents/Teaching/5301/Data/Lectures")
speeds = read.table("speed_of_light.txt", header=TRUE)$Times
names(speeds)
speeds

#Graphical Parameters to Modify Plots
#Some parameters can only be set using par().  See beginning of 
#help documentation for those.  Otherwise, most can be added
#to any plotting function as an additional argument
help("par")

#Read in and plot College Cost Data

#Tuition priced in current dollars
college_data <- read.csv("cp-2014-data.csv")
par(cex=0.5)
plot(college_data$Academic_Year_Starting_In,
     college_data$Public_Four_Year_Current_Dollars, xlab="Year", las=1,
     ylab="Cost (Current Dollars)",
     main="Instate Cost of Public Four Year College", type="l")


#Tuition priced in 2014 dollars
college_data <- read.csv("cp-2014-data.csv")
par(cex=0.5)
plot(college_data$Academic_Year_Starting_In,
     college_data$Public_Four_Year_2014_Dollars,
     xlab="Year", ylab="Cost (2014 Dollars)", las=1,
     main="Instate Cost of Public Four Year College", type="l")


#Pie chart of birth data
uk_births <- read.csv("uk_births_by_day_1978.csv")
#par(mar=c(0, 4, 0, 4), cex=0.8)
pie(x=uk_births$Number_Of_Births, labels = uk_births$Day,mar=c(0, 4, 0, 4),cex=0.8)

#Bar chart of birth data
par(cex=0.6)
barplot(height=uk_births$Number_Of_Births/1000, names.arg=uk_births$Day,
        las=1, xlab="Day", ylab="Number of Births (in 1000s)")


#Stem And Leaf Plot of Grades Data
grades <- data.frame(t(read.table("fake_grades.txt", row.names=1)))
stem(grades$Section1)
#Note:  stem(grades[,1]) will produce the same thing

#Back-to-Back Stem-and-Leaf Plots of Each Section's Test Scores
source("Lectures/stem.leaf.R")
stem.leaf.backback(x=grades$Section1, y=grades$Section2,
                   m=1, trim.outliers=FALSE)


#Histograms 

#4-Year College Data
par(cex=0.8)
costs <- read.csv("cp-2014-table-5.csv")
hist(costs$Cost, main="Instate Public 4-year College Cost",
     xlab="Cost ($)")

#Use different number of bins
par(mfrow=c(1,3), cex=0.5)
hist(costs$Cost, main="Default Breaks", xlab="Cost ($)")
hist(costs$Cost, breaks=10, main="10 Breaks", xlab="Cost ($)")
hist(costs$Cost, breaks=20, main="20 Breaks", xlab="Cost ($)")


#Comparing Two Histograms

#X- and Y-axes have different scales
par(mfrow=c(1,2), cex=0.5)
hist(costs$Cost[costs$Region=="Midwest"], main="Midwest Schools",
     xlab="Cost ($)")
hist(costs$Cost[costs$Region!="Midwest"], main="Non-Midwest Schools",
     xlab="Cost ($)")

#X- and Y-axes have the same scales
par(mfrow=c(1,2), cex=0.5)
hist(costs$Cost[costs$Region=="Midwest"], main="Midwest Schools", 
     xlab="Cost ($)", xlim=c(4000,16000), ylim=c(0, 16))
hist(costs$Cost[costs$Region!="Midwest"], main="Non-Midwest Schools",
     xlab="Cost ($)", xlim=c(4000,16000), ylim=c(0, 16), breaks=10)

#Use density for Y-axis
par(mfrow=c(1,2), cex=0.5)
hist(costs$Cost[costs$Region=="Midwest"], main="Midwest Schools",
     xlab="Cost ($)", xlim=c(4000,16000), ylim=c(0, 0.0004), freq=FALSE)
hist(costs$Cost[costs$Region!="Midwest"], main="Non-Midwest Schools",
     xlab="Cost ($)", xlim=c(4000,16000), ylim=c(0, 0.0004), freq=FALSE,
     breaks=10)

#Skew
par(mfrow=c(1,3), cex=0.5)
X <- rnorm(100)
hist(X, breaks=20, main="Symmetric", xlab="", ylab="", yaxt="n")
hist(exp(X), breaks=20, main="Right Skew", xlab="", ylab="", yaxt="n")
hist(max(exp(X))-exp(X), breaks=20, main="Left Skew", xlab="", ylab="",
     yaxt="n")


#Example:  Speed of Light

#Histogram of Speeds data
par(mfrow=c(1,2), cex=0.5)  #Split plot into 1 row of 2 plots
hist(speeds, xlab="(Millionths of a Second)", main="Observed Times")
hist(speeds, breaks=20, xlab="(Millionths of a Second)", main="Observed Times")

#See what happens when you use mfrow=c(2,1) instead

#Summary stats
par(mfrow=c(1,1),cex=0.5)
hist(speeds, breaks=20, xlab="(Millionths of a Second)", 
     main="Observed Times")
abline(v=mean(speeds), col="red", lwd=2)
abline(v=median(speeds), col="green", lwd=2)


length(speeds)
mean(speeds)
median(speeds)
speeds0 <- speeds[speeds > 0]
length(speeds0)
mean(speeds0)
median(speeds0)

set.seed(1)
par(cex=0.8)
data <- rweibull(n = 100, shape=1.5, scale=2)
hist(data, yaxt="n", ylab="")
abline(v=quantile(data, probs=c(0.05, 0.37, 0.90)),
       col=c("red", "blue", "green"), lwd=2)
text(x=quantile(data, probs=c(0.05, 0.37, 0.90))-0.05, y=20,
     labels=c("5 obs", "37 obs", "90 obs"), adj=1)

quantile(speeds, probs=c(0.25))
quantile(speeds, probs=c(0.75))

par(cex=0.8)
hist(speeds, breaks=20, xlab="(Millionths of a Second)", main="Observed Times")
abline(v=quantile(speeds, probs=c(0.25, 0.5, 0.75)), col=c("red", "blue", "green"),
       lwd=2, lty=2)
text(x=quantile(speeds, probs=c(0.25, 0.5, 0.75))-0.05, y=c(12, 8, 12),
     labels=c("Q1", "median", "Q3"), adj=1)

summary(speeds)

sd(speeds)
sd(speeds0)

#Boxplots
par(mar=c(0, 2, 0, 0), cex=1)
boxplot(speeds)
text(1.25, c(min(speeds), quantile(speeds, p=0.25), median(speeds), 
             quantile(speeds, p=0.75), max(speeds)), 
             labels=c("minimum", "Q1","median", "Q3", "maximum"), adj=0)

par(cex=0.8)
boxplot(grades$Section1, grades$Section2, names=c("Section 1", "Section 2"))


par(cex=0.6)
hist(speeds, breaks=20, xlab="(Millionths of a Second)", main="Observed Times")
