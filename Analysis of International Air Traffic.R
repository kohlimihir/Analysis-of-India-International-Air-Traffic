#' ---
#' title: "Analysis of International Air Traffic "
#' author: "Mihir Kohli"
#' date: ""
#' ---


library(tidyverse)

##Importing data
data <- read.csv("Airlinewise Monthly International Air Traffic To And From The Indian Territory.csv")
##Overview of data
glimpse(data)

##Dimension of data
dim(data)

##Checking for NA values
data%>%map(~sum(is.na(.)))


##Replacing the NA value with zero
data<-data%>%replace_na(list(PASSENGERS.TO.INDIA=0))
data%>%map(~sum(is.na(.)))

##Checking data type in each column
sapply(data,typeof)
##Changing all non integer numeric data to integer
data1<-data%>%mutate_all(type.convert)%>%mutate_if(is.double,as.integer)
sapply(data1,typeof)

##Analysis on Passenger to India

##Summary
summary(data1$PASSENGERS.TO.INDIA)

#Histogram showing passenger to India vs count
ggplot(data1,aes(x=PASSENGERS.TO.INDIA))+
  geom_histogram(binwidth = 25000,fill="cadetblue1",col="black")+
  scale_y_continuous(trans="log10")+
  theme_bw()+
  coord_flip()+
  labs(x="Passenger to India ", y="Count") +
  ggtitle("Count of Passenger to India")


#Kernel Density Plot
ggplot(data1,aes(x=PASSENGERS.TO.INDIA))+
  geom_density(colour="blue",fill="pink")+
  scale_x_continuous(limits = c(-50000,400000))+
  theme_bw()+
  ggtitle("Kernel Density Plot")+
  labs(x="Passenger to India", y= "Density")

#boxplot
ggplot(data1, aes(y = log(PASSENGERS.TO.INDIA),base=10)) + 
  geom_boxplot(fill="lightblue")+
  theme_bw()+
  ggtitle("Boxplot")+
  labs(y="Passenger to India (Log Scale)")+
  scale_x_continuous(limit=c(-0.9,0.9))

#total passenger to india in year 2015, 2016 and 2017
data1%>%group_by(YEAR)%>%summarise(Total_Passenger=sum(PASSENGERS.TO.INDIA))
#Top 10 airlines carrying highest passenger to India
d1<-data1%>%group_by(AIRLINE.NAME)%>%summarise(Total_Passenger=sum(PASSENGERS.TO.INDIA))
head(arrange(d1,desc(d1$Total_Passenger)),10)
#Total passenger to India Quarter wise
data1%>%group_by(QUARTER,YEAR)%>%summarise(Total_Passenger=sum(PASSENGERS.TO.INDIA))%>%pivot_wider(names_from = QUARTER,values_from = Total_Passenger)



##Analysis on Passenger from India
summary(data1$PASSENGERS.FROM.INDIA)

#Histogram showing passenger from india vs count
ggplot(data1,aes(x=PASSENGERS.FROM.INDIA))+
  geom_histogram(binwidth = 25000,fill="cadetblue1",col="black")+
  scale_y_continuous(trans="log10")+
  theme_bw()+
  coord_flip()+
  labs(x="Passenger from India ", y="Count") +
  ggtitle("Count of Passenger from India")

#Kernel Density Plot
ggplot(data1,aes(x=PASSENGERS.FROM.INDIA))+
  geom_density(colour="blue",fill="pink")+
  scale_x_continuous(limits = c(-50000,400000))+
  theme_bw()+
  ggtitle("Kernel Density Plot")+
  labs(x="Passenger from India", y= "Density")

#boxplot
ggplot(data1, aes(y = log(PASSENGERS.FROM.INDIA),base=10)) + 
  geom_boxplot(fill="lightblue")+
  theme_bw()+
  ggtitle("Boxplot")+
  labs(y="Passenger FROM India (Log Scale)")+
  scale_x_continuous(limit=c(-0.9,0.9))

#total passenger from india in year 2015, 2016 and 2017
data1%>%group_by(YEAR)%>%summarise(Total_Passenger=sum(PASSENGERS.FROM.INDIA))
#Top 10 airlines carrying highest passenger from India
d2<-data1%>%group_by(AIRLINE.NAME)%>%summarise(Total_Passenger=sum(PASSENGERS.FROM.INDIA))
head(arrange(d2,desc(d2$Total_Passenger)),10)
#Total passenger from India Quarter wise
data1%>%group_by(QUARTER,YEAR)%>%summarise(Total_Passenger=sum(PASSENGERS.FROM.INDIA))%>%pivot_wider(names_from = QUARTER,values_from = Total_Passenger)



##Analysis on Freight to India
summary(data1$PASSENGERS.TO.INDIA)

#Histogram showing freight to India vs count
ggplot(data1,aes(x=FREIGHT.TO.INDIA))+
  geom_histogram(binwidth = 1000,fill="cadetblue1",col="black")+
  scale_y_continuous(trans="log10")+
  theme_bw()+
  coord_flip()+
  labs(x="Freight to India ", y="Count") +
  ggtitle("Count of Freight to India")

#Kernel Density Plot
ggplot(data1,aes(x=FREIGHT.TO.INDIA))+
  geom_density(colour="blue",fill="pink")+
  scale_x_continuous(limits = c(-200,5000))+
  theme_bw()+
  ggtitle("Kernel Density Plot")+
  labs(x="Freight to India", y= "Density")

#boxplot
ggplot(data1, aes(y = log(FREIGHT.TO.INDIA),base=10)) + 
  geom_boxplot(fill="lightblue")+
  theme_bw()+
  ggtitle("Boxplot")+
  labs(y="Freight to India (Log Scale)")+
  scale_x_continuous(limit=c(-0.9,0.9))

#total Freight to india in year 2015, 2016 and 2017
data1%>%group_by(YEAR)%>%summarise(Total_Freight=sum(FREIGHT.TO.INDIA))
#Top 10 airlines with highest Freight to India
d3<-data1%>%group_by(AIRLINE.NAME)%>%summarise(Total_Freight=sum(FREIGHT.TO.INDIA))
head(arrange(d3,desc(d3$Total_Freight)),10)
#Total Freight to India Quarter wise
data1%>%group_by(QUARTER,YEAR)%>%summarise(Total_Freight=sum(FREIGHT.TO.INDIA))%>%pivot_wider(names_from = QUARTER,values_from = Total_Freight)


##Analysis on freight from India 
summary(data1$FREIGHT.FROM.INDIA)

#Histogram showing freight from India vs count
ggplot(data1,aes(x=FREIGHT.FROM.INDIA))+
  geom_histogram(binwidth = 2500,fill="cadetblue1",col="black")+
  scale_y_continuous(trans="log10")+
  theme_bw()+
  coord_flip()+
  labs(x="Freight from India ", y="Count") +
  ggtitle("Count of Freight from India")

#Kernel Density Plot
ggplot(data1,aes(x=FREIGHT.FROM.INDIA))+
  geom_density(colour="blue",fill="pink")+
  scale_x_continuous(limits = c(-2000,15000))+
  theme_bw()+
  ggtitle("Kernel Density Plot")+
  labs(x="Freight from India", y= "Density")

#boxplot
ggplot(data1, aes(y = log(FREIGHT.FROM.INDIA),base=10)) + 
  geom_boxplot(fill="lightblue")+
  theme_bw()+
  ggtitle("Boxplot")+
  labs(y="Freight from India (Log Scale)")+
  scale_x_continuous(limit=c(-0.9,0.9))

#total Freight from india in year 2015, 2016 and 2017
data1%>%group_by(YEAR)%>%summarise(Total_Freight=sum(FREIGHT.FROM.INDIA))
#Top 10 airlines with highest Freight from India
d4<-data1%>%group_by(AIRLINE.NAME)%>%summarise(Total_Freight=sum(FREIGHT.FROM.INDIA))
head(arrange(d4,desc(d4$Total_Freight)),10)
#Total Freight from India Quarter wise
data1%>%group_by(QUARTER,YEAR)%>%summarise(Total_Freight=sum(FREIGHT.FROM.INDIA))%>%pivot_wider(names_from = QUARTER,values_from = Total_Freight)

###Analysis on most common months and most common quarters for travel


#Barplot on most common Months for Travel
ggplot(data1,aes(x=MONTH,fill=MONTH))+
  geom_bar(col="black")+
  theme_bw()+
  coord_flip()+
  labs(x="Month", y="Count") +
  ggtitle("Most Common Month of Air Travel")+
  theme(legend.position = "none")

#Barplot on most common Quarter for Travel
ggplot(data1,aes(x=QUARTER,fill=QUARTER))+
  geom_bar(col="black")+
  theme_bw()+
  coord_flip()+
  labs(x="Month", y="Count") +
  ggtitle("Most Common Quarter of Air Travel")+
  theme(legend.position = "none")

###Bivariate Analysis


#Analysis on passengers to India and passengers from India

c1<-data1%>%group_by(YEAR)%>%summarise(Total_Passenger=sum(PASSENGERS.TO.INDIA))
c2<-data1%>%group_by(YEAR)%>%summarise(Total_Passenger=sum(PASSENGERS.FROM.INDIA))
c1c2<-data.frame(Year=c1$YEAR,Passenger_to_India=c1$Total_Passenger,Passenger_from_India=c2$Total_Passenger)

c1c2 %>%
  gather(KEY,Passenger, Passenger_to_India, Passenger_from_India) %>%
  ggplot(aes(x=Year,y=Passenger ,colour=KEY)) +
  geom_line(lwd=1.5)+
  geom_point(cex=4)+
  theme_bw()+
  ggtitle("Comparison b/w Passenger to india and Passenger from India")

#Analysis on freight to India and freight from India
c3<-data1%>%group_by(YEAR)%>%summarise(Total_Freight=sum(FREIGHT.TO.INDIA))
c4<-data1%>%group_by(YEAR)%>%summarise(Total_Freight=sum(FREIGHT.FROM.INDIA))
c3c4<-data.frame(Year=c3$YEAR,Freight_to_India=c3$Total_Freight,Freight_from_India=c4$Total_Freight)

c3c4 %>%
  gather(KEY,Freight, Freight_to_India, Freight_from_India) %>%
  ggplot(aes(x=Year,y=Freight ,colour=KEY)) +
  geom_line(lwd=1.5)+
  geom_point(cex=4)+
  theme_bw()+
  ggtitle("Comparison b/w Freight to india and Freight from India")

#Analysis on passengers to India and passengers from India with month

#Passenger to India and Passenger from India Month wise
ggplot(data1, aes(x=MONTH, y=PASSENGERS.TO.INDIA,fill=MONTH)) +
  geom_bar(stat="identity") +
  labs(title="Passenger To India", y="Count")

ggplot(data1, aes(x=MONTH, y=PASSENGERS.FROM.INDIA,fill=MONTH)) +
  geom_bar(stat="identity") +
  labs(title="Passenger From India", y="Count")

#Passenger to India and Passenger from India Quarter wise
ggplot(data1, aes(x=QUARTER, y=PASSENGERS.TO.INDIA,fill=QUARTER)) +
  geom_bar(stat="identity") +
  labs(title="Passenger To India", y="Count")

ggplot(data1, aes(x=QUARTER, y=PASSENGERS.FROM.INDIA,fill=QUARTER)) +
  geom_bar(stat="identity") +
  labs(title="Passenger From India", y="Count")

#Analysis on freight to India and freight from India 

#Freight to India and Freight from India Month wise
ggplot(data1, aes(x=MONTH, y=FREIGHT.TO.INDIA,fill=MONTH)) +
  geom_bar(stat="identity") +
  labs(title="Freight To India", y="Count")

ggplot(data1, aes(x=MONTH, y=FREIGHT.FROM.INDIA,fill=MONTH)) +
  geom_bar(stat="identity") +
  labs(title="Freight From India", y="Count")

#Freight to India and Freight from India Quarter wise
ggplot(data1, aes(x=QUARTER, y=FREIGHT.TO.INDIA,fill=QUARTER)) +
  geom_bar(stat="identity") +
  labs(title="Freight To India", y="Count")

ggplot(data1, aes(x=QUARTER, y=FREIGHT.FROM.INDIA,fill=QUARTER)) +
  geom_bar(stat="identity") +
  labs(title="Freight From India", y="Count")
