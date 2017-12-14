setwd("E:/IIT Case Study")

test<-read.csv("E:/IIT Case Study/2013 Mattec Report.csv",header=TRUE)

#To convert into Date
test$Date2<-as.Date(test$Date,"%m/%d/%Y")
#To add a column with a serial number
test$ID<-seq.int(nrow(test))


#Aggregating Data
day_count<-aggregate(test$Run.Time ~test$Date2, data = test, FUN =length)



#Run time count
runtime_count<-aggregate(test$Run.Time ~test$DeptDesc, data = test, FUN =mean)
#Downtime count
test$Downtime<-as.numeric(as.character(test$Down.Time))
downtime_count<-aggregate(cbind(test$Run.Time,test$Downtime) ~test$Date2, data = test, FUN =mean)
names(downtime_count)<-c("Date","Runtime","Downtime")
write.csv(downtime_count, file = "E:/IIT Case Study/File/datewise_.csv", row.names = FALSE)

#Making comprehensive decision tree
### LIBRARY CALL ###
library("rpart")
library("rpart.plot")	
library("rattle")

#RunTime
fir<-rpart(test$Run.Time~test$DeptDesc,method="anova")
plot(fir)
fancyRpartPlot(fir)
runtime_count<-aggregate(test$Run.Time ~test$DeptDesc, data = test, FUN =mean)

#DownTime
fir<-rpart(test$Downtime~test$DeptDesc,method="anova")
fancyRpartPlot(fir)
runtime_count<-aggregate(test$Downtime ~test$DeptDesc, data = test, FUN =mean)

#ScrapParts
test$Scrap.Parts<-as.numeric(as.character(test$Scrap.Parts))
fir<-rpart(test$Scrap.Parts~test$DeptDesc,method="anova")
fancyRpartPlot(fir)
runtime_count<-aggregate(test$Scrap.Parts ~test$DeptDesc, data = test, FUN =mean)


test$Scrap.Parts<-as.numeric(as.character(test$Scrap.Parts))
test$Total.Parts.Produced<-as.numeric(as.character(test$Total.Parts.Produced))

#Total Parts
test$Total.Parts.Produced<-as.numeric(as.character(test$Total.Parts.Produced))
fir<-rpart(test$Total.Parts.Produced~test$DeptDesc,method="anova")
fancyRpartPlot(fir)
runtime_count<-aggregate(test$Total.Parts.Produced ~test$DeptDesc, data = test, FUN =mean)
#Scrap percentage
test$Scrap..<-as.numeric(as.character(test$Scrap..))
fir<-rpart(test$Scrap..~test$DeptDesc,method="anova")
fancyRpartPlot(fir)
runtime_count<-aggregate(test$Scrap.. ~test$DeptDesc, data = test, FUN =mean)

#Peak demand
runtime_count<-aggregate(test$Total.Parts.Produced ~test$Date, data = test, FUN =mean)
write.csv(runtime_count, file = "E:/IIT Case Study/File/Peakdemand.csv", row.names = FALSE)

runtime_count<-aggregate(test$Total.Parts.Produced ~(test$Date+test$DeptDesc), data = test, FUN =mean)
write.csv(runtime_count, file = "E:/IIT Case Study/File/Peakdemand2.csv", row.names = FALSE)

#Machine Utlisation Level
test2<-subset(test, test$Downtime.Reason!="Not Schd"  & test$Downtime.Reason!="NOT SCHD" & test$Downtime.Reason!="Eq. PM"& test$Downtime.Reason!="Mold PM" & test$Downtime.Reason!="Interruption" & test$Downtime.Reason!="Eng/R&D" & test$Downtime.Reason!="Colr/Mat" &test$Downtime.Reason!="Info ")
test2$Downtime_per<-as.numeric(as.character(test2$Down.Time))
test2$Availability<-(100-as.numeric(as.character(test2$Down.Time)))
availability<-aggregate(test2$Downtime_per ~test2$Mach, data = test2, FUN =mean)
write.csv(availability, file = "E:/IIT Case Study/File/Availability.csv", row.names = FALSE)

test2<-subset(test, test$Downtime.Reason!="Not Schd"  & test$Downtime.Reason!="NOT SCHD" & test$Downtime.Reason!="Eq. PM"& test$Downtime.Reason!="Mold PM" & test$Downtime.Reason!="Interruption" & test$Downtime.Reason!="Eng/R&D" & test$Downtime.Reason!="Colr/Mat" &test$Downtime.Reason!="Info ")
test2$Downtime_per<-as.numeric(as.character(test2$Down.Time))
test2$Availability<-(100-as.numeric(as.character(test2$Down.Time)))
availability<-aggregate(test2$Availability ~test2$Mach, data = test2, FUN =mean)
write.csv(availability, file = "E:/IIT Case Study/File/Availability2.csv", row.names = FALSE)

#Performace
performance<-aggregate(test2$Eff.1 ~test2$Mach, data = test2, FUN =mean)
write.csv(performance, file = "E:/IIT Case Study/File/performance.csv", row.names = FALSE)
#Quality
test2$Scrap..<-as.numeric(as.character(test2$Scrap..))
quality<-aggregate(test2$Scrap.. ~test2$Mach, data = test2, FUN =mean)
write.csv(quality, file = "E:/IIT Case Study/File/quality.csv", row.names = FALSE)


#Ques-2

runtime_count<-aggregate(test$Run.Time ~test$DeptDesc+test$Date+test$Mach, data = test, FUN =mean)
write.csv(runtime_count, file = "E:/IIT Case Study/File/ques2.csv", row.names = FALSE)

test$Downtime<-as.numeric(as.character(test$Down.Time))
downtime_count<-aggregate(test$Downtime ~test$DeptDesc+test$Date+test$Mach, data = test, FUN =mean)
write.csv(downtime_count, file = "E:/IIT Case Study/File/downtime_count.csv", row.names = FALSE)

#Total parts produced
test$total<-as.numeric(test$Total.Parts.Produced)
total<-aggregate(. ~test$DeptDesc+test$Date+test$Mach, data = test, FUN =mean)
write.csv(total, file = "E:/IIT Case Study/File/total_parts.csv", row.names = FALSE)

#Ques-7

runtime_count<-aggregate(test$Run.Time ~test$Mach, data = test, FUN =mean)
write.csv(runtime_count, file = "E:/IIT Case Study/File/runtime_count.csv", row.names = FALSE)

test$Downtime<-as.numeric(as.character(test$Down.Time))
downtime_count<-aggregate(test$Downtime ~test$Mach, data = test, FUN =mean)
write.csv(downtime_count, file = "E:/IIT Case Study/File/downtime_count.csv", row.names = FALSE)

#Total parts produced
test$total<-as.numeric(test$Total.Parts.Produced)
total<-aggregate(test$total ~test$Mach, data = test, FUN =mean)
write.csv(total, file = "E:/IIT Case Study/File/total_parts.csv", row.names = FALSE)
