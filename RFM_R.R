# Our first line of code should always "clear Rs brain":
rm(list=ls()) # remove everything from the environment

#load libraries
library(data.table)
library(lubridate)
library(Hmisc)
library(ggplot2)

# load data.table
dt = fread(file = "C:/Users/micha/Desktop/R_big.data.techniques.interactive.visualization/data/transactions.csv", header=TRUE, sep = ",")

#Check the data format
str(dt)

# set to timedate
dt[, TransDate:=dmy(TransDate)]
str(dt)

#####____________________ Aggregation of variables___________________#####
#Save the latest transaction as the object now in your R environment. ####
now <- max(dt[, TransDate])
now

#Create a new data.table called rfm. ####
#that includes the customer ID, as well as the measures for purchase recency, frequency and monetary value.
rfm <- dt[ ,list(recency = as.numeric(now - max(TransDate)), #recency = difference between latest transaction and "today"
                           frequency = .N, #.N counting nr of rows in group: frequency = number of transactions
                           monetary = mean(PurchAmount)), #monetary = average amount spent per transaction
                     by=Customer]

#Check the structure of the new table and ensure that all the variables are numeric. ####
str(rfm)

#####____________________ Calculation RFM Scores ___________________#####
# Hint: as.numeric(cut2(data, g=3)) --> groups data into 3 groups --> numeric 1/2/3

rfm_scores <- rfm[,list(Customer,
                        recency = as.numeric(cut2(-recency, g=3)),
                        frequency = as.numeric(cut2(frequency, g=3)),
                        monetary = as.numeric(cut2(monetary, g=3))
)]

#####____________________ Calculation Overall RFM Scores ___________________#####
#Calculation of unweighted solution
rfm_scores[,overall:=mean(c(recency,frequency,monetary)),by=Customer] #unweighted RFM score
rfm_scores

# Calculation of weighted solution
rfm_scores[,weighted_overall2:=0.2*recency+0.2*frequency+0.6*monetary] #weighted RFM score (frequency)
rfm_scores

# Divide all customers in 3 distinct RFM groups by rounding the overall RFM score. ####
# function: round
rfm_scores[,group:=round(overall)] # new column Group (1-3)

rfm_scores[group==3] # table with only highest rated customers
table(rfm_scores$group)
rfm_scores[, list(Customer, overall, group),by=weighted_overall2]



#####____________________ The RFM function (default: unwighted) ___________________######

RFMfunction <- function(data, weight_recency=1, weight_frequency=1, weight_monetary=1){
  
  # adjusting values to ensure that the weights add up to one 
  weight_recency2 <- weight_recency/sum(weight_recency, weight_frequency, weight_monetary)
  weight_frequency2 <- weight_frequency/sum(weight_recency, weight_frequency, weight_monetary)
  weight_monetary2 <- weight_monetary/sum(weight_recency, weight_frequency, weight_monetary)
  
  print("weights are calculated")
  
  # RFM measures
  max.Date <- max(data[,TransDate])
  
  temp <- data[,list(
    recency = as.numeric(max.Date - max(TransDate)),
    frequency = .N,
    monetary = mean(PurchAmount)),
    by=Customer
  ]
  
  print("RFM Measure done")
  
  # RFM scores
  temp <- temp[,list(Customer,
                     recency = as.numeric(cut2(-recency, g=3)),
                     frequency = as.numeric(cut2(frequency, g=3)),
                     monetary = as.numeric(cut2(monetary, g=3)))]
  
  # Overall RFM score
  temp[,finalscore:=weight_recency2*recency+weight_frequency2*frequency+weight_monetary2*monetary]  
  
  print("Overall RFM Measure done")
  
  # RFM group
  temp[,group:=round(finalscore)]
  
  # Return final table
  return(temp)
}

#####____________________ Testing ___________________######
temp <- RFMfunction(dt,60,20,20)
temp
table(rfm_scores$group)
table(temp$group)

temp <- RFMfunction(dt,1,1,2)
temp
table(rfm_scores$group)
table(temp$group)

temp <- RFMfunction(dt,1,1,1)
temp
table(rfm_scores$group)
table(temp$group)