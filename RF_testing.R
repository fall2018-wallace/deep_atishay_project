
plotRFIMP <-varImpPlot(rf_classifier,sort=TRUE)


plotRFIMP


rf_classifier1 = randomForest(StatisfactionType ~Scheduled.Departure.Hour_Early_Morning+Type.of.Travel_Mileage.tickets+
                               Type.of.Travel_Business.travel+Flight.Distance+
                               Arrival.Delay.in.Minutes+Class+
                               Shopping.Amount.at.Airport+No.of.Flights.p.a.+Year.of.First.Flight+
                               Price.Sensitivity+Age+Gender+Airline.Status,data=dfRFTrain, ntree=50)
pred_randomForest1 <- predict(rf_classifier1, dfRFTest)
rfText1 <- table(c,pred_randomForest1)
accuracy1 <- (rfText1[1]+ rfText1[5]+rfText1[9])/sum(rfText1)
accuracy1
