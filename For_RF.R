
library(randomForest)
df <- df
dfRF <- df[,c(2:8,10,11,13,23:52)]#df[,c(1:3,5:8,10,12,13,15,25,26,28,29,31:39,40:53)]
dfRF$StatisfactionType <- factor(dfRF$StatisfactionType)
dfRFTrain <- dfRF[1:100000,1:40]

c <- dfRF[100001:129889,40]
dfRFTest <- dfRF[100001:129889,1:39]

rf_classifier = randomForest(StatisfactionType ~Scheduled.Departure.Hour_Early_Morning+Type.of.Travel_Mileage.tickets+
                               Type.of.Travel_Business.travel+Flight.Distance+Flight.time.in.minutes+
                               Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes+Class+
                               Shopping.Amount.at.Airport+No.of.Flights.p.a.+Year.of.First.Flight+
                               Price.Sensitivity+Age+Gender+Airline.Status,data=dfRFTrain, ntree=50)


