#Obtain data from Data.gov
#http://catalog.data.gov/dataset/heart-attack-payment-hospital

library(ggplot2)
library(reshape2)
library(dplyr)
library(plyr)
library(sqldf)
library(maps)

#May need to detach dplyr to use plyr
detach(package:dplyr)

#Read CSV into R
hosp <- read.csv(file.choose(),header=TRUE)
attach(hosp)

#Look at size of dataset
dim(hosp) 
head(hosp)

#Fix column names by replacing "." with "_"
names(hosp) <- gsub(x=names(hosp), pattern="\\.",replacement="_")

#First see where the hospitals with no estimate are - across all states
table(hosp$State[hosp$Payment_category=="Not Available"])
table(hosp$State[hosp$Payment_category=="Number of Cases Too Small"])

#Remove hospitals without estimates
hospay <- hosp[hosp$Payment_category != "Not Available" 
               & hosp$Payment_category != "Number of Cases Too Small",]
dim(hospay)
head(hospay)
names(hospay)

#Fix dollar signs and commas in estimate values
hospay$Payment <- as.numeric(gsub("[$,]","",hospay$Payment))
hospay$Lower_estimate <- as.numeric(gsub("[$,]","",hospay$Lower_estimate))
hospay$Higher_estimate <- as.numeric(gsub("[$,]","",hospay$Higher_estimate))
hospay[13:15] <- sapply(hospay[13:15],as.numeric)

#Need to use melt function 
#Need more information then use commands: help(melt) or ?melt
#Or: http://marcoghislanzoni.com/blog/2013/10/11/pivot-tables-in-r-with-melt-and-cast/
hosp_melt <- melt(data=hospay,id=c(2,5,9,11),measure=as.numeric(c(13)), value.name="Estimate")
head(hosp_melt)

#Tangent: can use dplyr or plyr
#I did not for my final analysis, but this is how it is used

#To get average heart attack patient estimate by state, use plyr
hosp_heartattack <- hospay[Payment_measure_name=='Payment for heart attack patients',]
hosp_state <- ddply(hosp_heartattack, ~ State,summarise,mean_payment = mean(Estimate, na.rm=TRUE))

#Or use dplyr, note the interesting syntax
hosp_state <- hosp_heartattack %>% group_by(State) %>% summarise(mean_est = mean(Estimate), total=n())


#Get table with state and payment estimate
#use sqldf
hosp_est <- sqldf("select State, avg(Estimate) as Estimate from hosp_melt where Payment_measure_name = 'Payment for heart attack patients' group by State")
head(hosp_est)

#Map heart attack patient payment estimate by state
#Use ggplot2 and maps
states_mapped <- map_data("state")

ggplot(data=hosp_est, aes(map_id='State')) 
  + geom_map(aes(fill = as.factor('Payment')), map = states_mapped) 
  + expand_limits(x=states_mapped$long,y=states_mapped$lat)
