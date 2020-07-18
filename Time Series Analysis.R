MASP <- read_excel("MASP.xlsx")#reading in the data that was downloaded from the website.
masp<-as.data.frame(MASP)# this turns the data into a usable data frame
str(masp)#this looks to see the structure of the data we are working with now that it is read in.
masp$YearQtr<- as.factor(masp$YearQtr)#these then turn the character variables info factors
masp$AvgerageNorthEast<- as.factor(masp$AvgerageNorthEast)
masp$AverageMidwest<- as.factor(masp$AverageMidwest)
masp$AverageSouth<- as.factor(masp$AverageSouth)
masp$AverageWest<- as.factor(masp$AverageWest)
str(masp)#this then looks at the new structure before creating a time series transformation.
#we then turn the data into time series data in order to use the suggested commands
masp$qtr<-substr(masp$YearQtr,5,6)
masp$Year<-substr(masp$YearQtr,1,4)
MedianTS<-ts(masp[,2:6],frequency=4,start=c(1963,1))
MedianTSUS<-ts(masp[,2],frequency=4,start=c(1963,1))
#Then I looked at all of the medians and the US before choosing a region
#I wanted to make sure i could use all the suggest commands and that the data was reliable
plot.ts(MedianTS)
plot.ts(MedianTSUS)
Comp<-decompose(MedianTSUS)
MedianTSUSAdjust<-MedianTSUS-Comp$seasonal
MedianTSUSAdjust2<-MedianTSUS-Comp$trend
MedianTSUSAdjust3<-MedianTSUS-Comp$random
MedianTSUSAdjust4<-MedianTSUS-Comp$random-Comp$trend-Comp$seasonal
plot.ts(MedianTSUS)
#visualizing the adjustments
plot.ts(MedianTSUSAdjust)
plot.ts(MedianTSUSAdjust2)
plot.ts(MedianTSUSAdjust3)
plot.ts(MedianTSUSAdjust4)
#we then find that we need to transform the data again in order to use the additive models 
#to forecast from and use the suggested commands. 
MedianTSUSL<-log(MedianTSUS)
MedianTSUSLF<-HoltWinters(MedianTSUSL)
MedianTSUSLF#this helps us build the model we want to work from once we see the weight of
#the components of the model, and then we can start to make prediuctions from the model
#versus the actual and makepredictions moving forward. 
plot(MedianTSUSLF)
MedianTSUSLF2<-forecast:::forecast.HoltWinters(MedianTSUSLF,h=40)
plot(MedianTSUSLF2)# this helps us forecast and make predictions based on the model we get
acf(window(MedianTSUSLF2$residuals,start = c(1964,1)),lag.max = 80)# we want to ensure 
#the autocorrelation is low. 
Box.test(window(MedianTSUSLF2$residuals,start = c(1964,1)),lag = 80)
#we now see fro mthe box-pierce plot that the forecast is reliable and move on to the regions
MedianTSNE<-ts(masp[,3],frequency=4,start=c(1963,1))
MedianTSMW<-ts(masp[,4],frequency=4,start=c(1963,1))
MedianTSS<-ts(masp[,5],frequency=4,start=c(1963,1))
MedianTSW<-ts(masp[,6],frequency=4,start=c(1963,1))
plot.ts(MedianTSNE)
plot.ts(MedianTSMW)
plot.ts(MedianTSS)
plot.ts(MedianTSW)
#I then saw the differences in the regions I wanted to use. So, I decided to do 4 of 
#the regions in order to compare them to the US at the end. 
#Northeast
Comp2<-decompose(MedianTSNE)
MedianTSNEAdjust<-MedianTSNE-Comp2$seasonal
MedianTSNEAdjust2<-MedianTSNE-Comp2$trend
MedianTSNEAdjust3<-MedianTSNE-Comp2$random
MedianTSNEAdjust4<-MedianTSNE-Comp2$random-Comp2$trend-Comp2$seasonal
plot.ts(MedianTSNE)
plot.ts(MedianTSNEAdjust)
plot.ts(MedianTSNEAdjust2)
plot.ts(MedianTSNEAdjust3)
plot.ts(MedianTSNEAdjust4)#the seasonality was variable requiring a transformation 
MedianTSNEL<-log(MedianTSNE)
MedianTSNELF<-HoltWinters(MedianTSNEL)
MedianTSNELF
plot(MedianTSNELF)
#I created the forecast afterward 
MedianTSNELF2<-forecast:::forecast.HoltWinters(MedianTSNELF,h=40)
plot(MedianTSNELF2)
acf(window(MedianTSNELF2$residuals,start = c(1964,1)),lag.max = 80)
Box.test(window(MedianTSNELF2$residuals,start = c(1964,1)),lag = 80)
#then the Midwest
plot.ts(MedianTSMW)
Comp3<-decompose(MedianTSMW)
MedianTSMWAdjust<-MedianTSMW-Comp3$seasonal
MedianTSMWAdjust2<-MedianTSMW-Comp3$trend
MedianTSMWAdjust3<-MedianTSMW-Comp3$random
MedianTSMWAdjust4<-MedianTSMW-Comp3$random-Comp3$trend-Comp3$seasonal
plot.ts(MedianTSMW)
plot.ts(MedianTSMWAdjust)
plot.ts(MedianTSMWAdjust2)
plot.ts(MedianTSMWAdjust3)
plot.ts(MedianTSMWAdjust4)
#we then transform the data because the variance again increases greatly over time.
MedianTSMWL<-log(MedianTSMW)
MedianTSMWLF<-HoltWinters(MedianTSMWL)
MedianTSMWLF
plot(MedianTSMWLF)
#I then created the MW forecast 
MedianTSMWLF2<-forecast:::forecast.HoltWinters(MedianTSMWLF,h=40)
plot(MedianTSMWLF2)
acf(window(MedianTSMWLF2$residuals,start = c(1964,1)),lag.max = 80)
Box.test(window(MedianTSMWLF2$residuals,start = c(1964,1)),lag = 80)
#the south was next 
plot.ts(MedianTSS)
Comp4<-decompose(MedianTSS)
MedianTSSAdjust<-MedianTSS-Comp4$seasonal
MedianTSSAdjust2<-MedianTSS-Comp4$trend
MedianTSSAdjust3<-MedianTSS-Comp4$random
MedianTSSAdjust4<-MedianTSS-Comp4$random-Comp4$trend-Comp4$seasonal
plot.ts(MedianTSS)
plot.ts(MedianTSSAdjust)
plot.ts(MedianTSSAdjust2)
plot.ts(MedianTSSAdjust3)
plot.ts(MedianTSSAdjust4)
#a log transformation was needed agian to assess autocorrelation.
MedianTSSL<-log(MedianTSS)
MedianTSSLF<-HoltWinters(MedianTSSL)
MedianTSSLF
plot(MedianTSSLF)
#I then created the forecast.
MedianTSSLF2<-forecast:::forecast.HoltWinters(MedianTSSLF,h=40)
plot(MedianTSSLF2)
acf(window(MedianTSSLF2$residuals,start = c(1964,1)),lag.max = 80)
Box.test(window(MedianTSSLF2$residuals,start = c(1964,1)),lag = 80)
#We then did the west
plot.ts(MedianTSW)
Comp5<-decompose(MedianTSW)
MedianTSWAdjust<-MedianTSW-Comp5$seasonal
MedianTSWAdjust2<-MedianTSW-Comp5$trend
MedianTSWAdjust3<-MedianTSW-Comp5$random
MedianTSWAdjust4<-MedianTSW-Comp5$random-Comp5$trend-Comp5$seasonal
plot.ts(MedianTSW)
plot.ts(MedianTSWAdjust)
plot.ts(MedianTSWAdjust2)
plot.ts(MedianTSWAdjust3)
#again a log transformation was needed to assess the autocorrelation and forecast the model
MedianTSWL<-log(MedianTSW)
MedianTSWLF<-HoltWinters(MedianTSWL)
MedianTSWLF
plot(MedianTSWLF)
#then we created the forecast for the W region
MedianTSWLF2<-forecast:::forecast.HoltWinters(MedianTSWLF,h=40)
plot(MedianTSWLF2)
acf(window(MedianTSWLF2$residuals,start = c(1964,1)),lag.max = 80)
Box.test(window(MedianTSWLF2$residuals,start = c(1964,1)),lag = 80)
#I then wanted to see a different forecasting model created for the west since
#it was the most interesting graph to me. I then made the data stationary and did 
#an ARIMA model analysis. 
MedianTSWLD1<-diff(MedianTSWL, differences = 1)
plot.ts(MedianTSWL)
plot.ts(MedianTSWLD1)
#We thne move to find the other ariables in the ARIMA model by using the ARIMA function.
auto.arima(MedianTSWL)
auto.arima(MedianTSWLD1)
#after finding the optimal variable for the ARIMA we can forecast for the West as we wanted
MedianTSWLF<-arima(MedianTSWL,order = c(1,1,2))
MedianTSWLF
MedianTSWLF2<-forecast:::forecast.Arima(MedianTSWLF,h=40)
plot(MedianTSWLF2)
acf(MedianTSWLF2$residuals,lag.max=80)
Box.test(window(MedianTSWLF2$residuals,start = c(1964,1)),lag = 80)
#once all of the curiosities are out and the five forecasts are created, We want to put
#them all on the same plot in order to make some inferences.
ts.plot(MedianTSWLF2$fitted, MedianTSSLF2$fitted, MedianTSNELF2$fitted, MedianTSMWLF2$fitted, MedianTSUSLF2$fitted,
        col=c("red","green","blue","black","purple"),
        gpars=list(xlab="Year", ylab="Logged Value of Median House Sales", lty=c(1:5)))