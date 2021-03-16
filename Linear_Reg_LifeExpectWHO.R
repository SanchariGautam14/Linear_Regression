#Statistical Analysis on factors influencing Life Expectancy

#Read the dataset
data=read.csv('./LifeExpectancyData.csv',header = TRUE)
head(data)
colnames(data) #Get the columns of the dataset
#Get the dimension of the dataset
dim(data) #2938 rows and 22 columns

summary(data)
#Categorical columns: Country, Year, Status
#Columns with missing columns: Life Expectancy - 10, Adult Mortality  - 10, Alcohol - 194 etc.

#DATA MANIPULATION

data=na.omit(data) #Delete rows with missing values
dim(data) #1649 rows with 22 

data$Year=factor(data$Year) #Change Year column to Categorical
summary(data) #rechecking the status of the data

#by(dataFrame, seq_len(nrow(dataFrame)), function(row) if row>mean(data$Population))


#Exploratory Data Analysis

boxplot(data$Life.expectancy, 
        main = "Life Expectancy Boxplot", 
        ylab = "Expected Life Span")
#The median of the life expectancy lies around 72 approximately. There are some outliers below 50. 

#Country vs Life Expectancy
group1 = aggregate(data$Life.expectancy~data$Country,data,mean) #group the dataset by countries and find mean of Life expectancy of each country
group1 = group1[order(group1$`data$Life.expectancy`),] #sort the dataset by the mean of the Life Expectancy
barplot(group1$`data$Life.expectancy`~group1$`data$Country`,col=c("cyan"))
#Sierra Leone has the least average Life Expectancy and Ireland has the highest life expectancy.

#Status vs Life Expectancy
group2 = aggregate(data$Life.expectancy~data$Status,data,mean) #group the dataset by countries and find mean of Life expectancy of each country
group2 = group2[order(group2$`data$Life.expectancy`),] #sort the dataset by the mean of the Life Expectancy
barplot(group2$`data$Life.expectancy`~group2$`data$Status`,col=c("cyan")) #Developed countries have higher expectancy

#Life Expectation variation over age
hist(data$Life.expectancy,main = "Life Expectancy Variation in Age",col="magenta") #the maximum expected age is in between 70 to 75.

#Year and status wise Life Expectation Analysis
install.packages("#dplyr")
install.packages("ggplot2")
library(dplyr)
status=data$Status
life_expectancy=data$Life.expectancy
df1=data.frame(status,country,life_expectancy)
group3=df1 %>% group_by(status) %>% summarise(life_expectancy = mean(life_expectancy))
group4=df1 %>% group_by(status) %>% summarise(GDP = mean(data$GDP))
group3$GDP=group4$GDP
library(ggplot2)
ggplot(group3, aes(fill=group3$GDP, y=group3$life_expectancy, x=group3$status)) + 
  geom_bar(position="stack", stat="identity")
