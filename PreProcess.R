
library(dummies)
df <- satisfaction_survey


df$Satisfaction[df$Satisfaction == "4.00.5" | df$Satisfaction == "4.00.2.00"] = 4
df$Satisfaction <- as.double(df$Satisfaction)

df$Departure.Delay.in.Minutes[df$Flight.cancelled=='Yes'] <- 1600
df$Arrival.Delay.in.Minutes[df$Flight.cancelled=='Yes'] <- 1600
df$Arrival.Delay.greater.5.Mins[df$Flight.cancelled=='Yes'] <- 'yes'
df[is.na(df$Arrival.Delay.in.Minutes),]$Arrival.Delay.in.Minutes <- df[is.na(df$Arrival.Delay.in.Minutes),]$Departure.Delay.in.Minutes
df[is.na(df$Flight.time.in.minutes),]$Flight.time.in.minutes <- 1000
# View(df[df$Flight.cancelled== 'Yes',])

df <- na.omit(df)

df$Airline.Status <- as.numeric(factor(df$Airline.Status), levels = c('Blue', 'Silver', 'Gold','Platinum'))
df$Gender <- ifelse(df$Gender=='Male',0,1)
df$Year.of.First.Flight <- 2013-df$Year.of.First.Flight
df$Type.of.Travel <- as.factor(df$Type.of.Travel)
dfFlight <- dummies::dummy(df$Airline.Code,sep = "_")
df <- cbind(df,dfFlight)
dfTravel <- dummies::dummy(df$Type.of.Travel,sep = "_")
df <- cbind(df,dfTravel)
df$Class <- as.numeric(factor(df$Class), levels = c('Eco','Eco Plus','Business'))
df$Scheduled.Departure.Hour <- cut(df$Scheduled.Departure.Hour, breaks = c(0,4,7,11.99,15.99,20,23),labels=c("Late_Night","Early_Morning","Morning","Afternoon","Evening","Night"))
dfDepartureTime <- dummies::dummy(df$Scheduled.Departure.Hour,sep = "_")
df <- cbind(df,dfDepartureTime)
df$Arrival.Delay.greater.5.Mins <- ifelse(df$Arrival.Delay.greater.5.Mins=='no',0,1)

names(df) <- make.names(names(df), unique=TRUE)

df$StatisfactionType <- ""
df[which(df$Satisfaction<3), which(colnames(df) == 'StatisfactionType')]<-"LOW"
df[which(df$Satisfaction==3), which(colnames(df) == 'StatisfactionType')]<-"MED"
df[which(df$Satisfaction>3), which(colnames(df) == 'StatisfactionType')]<-"HIGH"

plot <- hist(df$Satisfaction)

plot
df
