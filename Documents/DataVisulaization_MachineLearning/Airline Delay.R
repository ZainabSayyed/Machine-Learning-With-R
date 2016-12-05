#Step 2: Preparing and Exploring Data
#Unzip file (million records)

airlineOriginal <- read.csv("C:/Users/Lovelyn/Desktop/R/Project 1/1987.csv.bz2")
#Cleaning Data Set
#Excluding features ("N/A", "Non-Variability","0") from Data Set
airlineOriginal$Year<-NULL
airlineOriginal$UniqueCarrier<-NULL
airlineOriginal$TailNum<-NULL
airlineOriginal$TaxiIn<-NULL
airlineOriginal$AirTime<-NULL
airlineOriginal$TaxiOut<-NULL
airlineOriginal$CancellationCode<-NULL
airlineOriginal$Diverted<-NULL
airlineOriginal$DelayCarreir<-NULL
airlineOriginal$WeatherDelay<-NULL
airlineOriginal$NASDelay<-NULL
airlineOriginal$SecurityDelay<-NULL
airlineOriginal$CarrierDelay<-NULL
airlineOriginal$LateAircraftDelay<-NULL
airline<-na.omit(airlineOriginal)
#Removing records for cancelled flights (ArrTime, ArrDelay ... )
airline<-subset(airline, Cancelled!=1)
#New variable ArrivedLate  created - Classification feature
airline$ArrivedLate<-ifelse(airline$ArrDelay >0,airline$ArrivedLate<- 1, 0)
#View(airline)
#Converting qualitative feature into factors
airline$DayofMonth<- factor(airline$DayofMonth)
airline$DayOfWeek<- factor(airline$DayOfWeek)
airline$FlightNum<- factor(airline$FlightNum)
airline$Cancelled<- factor(airline$Cancelled)
airline$Month<- factor(airline$Month)
airline$ArrivedLate<- factor(airline$ArrivedLate)
str(airline$ArrivedLate)
#Compute means and standard deviations for the Numeric features for each month of 1987
tapply(airline$DepTime, airline$Month, mean)
tapply(airline$CRSDepTime, airline$Month, mean)
tapply(airline$ArrTime, airline$Month, mean)
tapply(airline$CRSArrTime, airline$Month, mean)
tapply(airline$ActualElapsedTime, airline$Month, mean)
tapply(airline$CRSElapsedTime,airline$Month, mean)
tapply(airline$ArrDelay,airline$Month, mean)
tapply(airline$DepDelay,airline$Month, mean)
tapply(airline$DepTime, airline$Month, sd,na.rm = TRUE)
tapply(airline$CRSDepTime, airline$Month, sd,  na.rm = TRUE)
tapply(airline$ArrTime, airline$Month, sd, na.rm = TRUE)
tapply(airline$CRSArrTime, airline$Month, sd,  na.rm = TRUE)
tapply(airline$ActualElapsedTime, airline$Month, sd,  na.rm = TRUE)
tapply(airline$CRSElapsedTime,airline$Month, sd,  na.rm = TRUE)
tapply(airline$ArrDelay,airline$Month, sd,  na.rm = TRUE)
tapply(airline$DepDelay,airline$Month, sd,  na.rm = TRUE)
y = xtabs(~ Month + DayofMonth, airline)
y
y = xtabs(~ Month + DayOfWeek, airline)
y
y = xtabs(~ Month + FlightNum, airline)
y
y = xtabs(~ Month + Origin, airline)
y
y = xtabs(~ Month + Dest, airline)
y
y = xtabs(~ Month + Cancelled, airline)
y
y = xtabs(~ Month + ArrivedLate, airline)
y
library(MASS)

D <- xtabs(~ Month + FlightNum , data=airline)
D.relfreq= D / nrow(airline)
D.relfreq
D <- xtabs(~ Month + Origin , data=airline)
D.relfreq= D / nrow(airline)
D.relfreq
D <- xtabs(~ Month + Dest , data=airline)
D.relfreq= D / nrow(airline)
D.relfreq
D <- xtabs(~ Month + Cancelled , data=airline)
D.relfreq= D / nrow(airline)
D.relfreq
#Coverting Time to 4 digit format
airline$NewArrTime<-sprintf("%04d",airline$ArrTime )
#Coverting into Date Time format MM:DD:YYYY HH:MM:SS
airline$NewArrTime<-strptime(airline$NewArrTime,"%H%M")
#Converting character feature into Time
airline$NewArrTime<-as.POSIXct(airline$NewArrTime, "GMT")
airline$NewCRSArrTime<-sprintf("%04d",airline$CRSArrTime)
airline$NewCRSArrTime<-strptime(airline$NewCRSArrTime,"%H%M")
airline$NewCRSArrTime<-as.POSIXct(airline$NewCRSArrTime, "GMT")
#airline$NewArrTime<-format(airline$NewArrTime,"%H:%M")
#airline$NewArrTime<-paste(airline$NewArrTime,":00",sep="")
#airline$NewCRSArrTime<-format(airline$NewCRSArrTime,"%H:%M")
#airline$NewCRSArrTime<-paste(airline$NewCRSArrTime,":00",sep="")
#Computing New Arrive Delay <- time difference between Scheduled and Actual Arrival time in minutes
airline$NewArrDelay<-difftime(airline$NewArrTime,airline$NewCRSArrTime, units="min")
#Compare NewArriveDelay with ArriveDelay and set flag==Yes if no difference else No
airline$Correct<-ifelse(airline$ArrDelay !=airline$NewArrDelay,airline$Correct<- 0, 1)
set.seed(890123)

airlineth<-airline[1:1000,]
#write.csv(airlineth, "C:/Users/Lovelyn/Desktop/R/Project 1/random1000.csv")
ind <- sample(2, nrow(airline), replace=TRUE, prob=c(0.01, 0.99))
airlineth_test<-airline[ind==1,]
View(airlineth_test)
View(airline)
airlineth$Correct<-NULL