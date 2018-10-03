
df <- df

dfSVM <- df[,c(2:8,10,11,13,23:52)]
#dfSVM <- df[,c(1:3,4:7,10,12,13,15,25,26,28,29,31:39,40:54)]
dfSVMTrain <- dfSVM[1:10000,1:40]

c <- dfSVM[10001:12000,53]
dfSVMTest <- dfSVM[10001:12000,1:40]
colnames(dfSVMTest)
library(kernlab)

#ksvm_Model <- ksvm(StatisfactionType ~Scheduled.Departure.Hour_Early_Morning,data=dfSVMTrain, kernel = "rbfdot", kpar = "automatic", C = 10, cross = 1, prob.model = TRUE)

#ksvm_Model <- ksvm(StatisfactionType ~Scheduled.Departure.Hour_Early_Morning+Type.of.Travel_Mileage.tickets+
                     Type.of.Travel_Business.travel+Flight.Distance+Flight.time.in.minutes+
                     Departure.Delay.in.Minutes+Arrival.Delay.in.Minutes+Class+
                     Shopping.Amount.at.Airport+No.of.Flights.p.a.+Year.of.First.Flight+
                     Price.Sensitivity+Age+Gender+Airline.Status,data=dfSVMTrain, kernel = "rbfdot", kpar = "automatic", C = 10, cross = 10, prob.model = TRUE)





ksvm_Model


ksvmPredict <- predict(ksvm_Model, dfSVMTest)
length(ksvmPredict)
nrow(dfSVMTest)

dfSVMTest$predictedValues <- ksvmPredict


#Function to check accuracy of predicted model
functionForKSVMAccuracy <- function(dataSet){
  a <- ifelse(dataSet$dfSVMTest.StatisfactionType == dataSet$dfSVMTest.predictedValues, 1,0)
  return(length(which((a==1) == TRUE))/nrow(dataSet) * 100)
}

comparisonTable <- data.frame(dfSVMTest$StatisfactionType,dfSVMTest$predictedValues)
table(comparisonTable)
functionForKSVMAccuracy(comparisonTable)
