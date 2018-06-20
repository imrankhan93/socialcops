setwd('C:\\Users\\Hp\\Desktop\\work\\Imran\\drive-download-20180610T153754Z-001')
msp = read.csv("CMO_MSP_Mandi.csv")
monthly_data = read.csv("Monthly_data_cmo.csv")
View(msp)
View(monthly_data)

length(unique(monthly_data$Commodity))
length(unique(monthly_data$APMC))

monthly_data$Commodity = trimws(tolower(monthly_data$Commodity))
monthly_data$APMC = trimws(tolower(monthly_data$APMC))

length(unique(monthly_data$Commodity))
length(unique(monthly_data$APMC))

unique(monthly_data$Month)
unique(monthly_data$Year)

library(dplyr)
monthly_data = monthly_data %>% arrange(Commodity) 

View(monthly_data) 


data14 = filter(monthly_data, Year == 2014)
data15 = filter(monthly_data, Year == 2015)
data16 = filter(monthly_data, Year == 2016)

nrow(data14) + nrow(data16) + nrow(data15) == nrow(monthly_data)


######################################################3
boxplot(data14$modal_price~data14$Commodity,main = 'boxplot of modal prices (2014)',xlab = 'Commodity',ylab = 'Modal Prices')
boxplot(data15$modal_price~data15$Commodity,main = 'boxplot of modal prices (2015)',xlab = 'Commodity',ylab = 'Modal Prices')
boxplot(data16$modal_price~data16$Commodity,main = 'boxplot of modal prices (2016)',xlab = 'Commodity',ylab = 'Modal Prices')

#Thus it can be seen that outliers are present in the data
#Function for outlier detection

outdetect= function(modal_price,Commodity){ 
x = NULL
#tapply(mtcars$mpg,mtcars$am,mean)
a = tapply(modal_price,Commodity,quantile,probs = 0.75)
b = tapply(modal_price,Commodity,quantile,probs = 0.25)
out1 = a + 1.5* ( a-b)
out2 = b - 1.5 * (a-b)


for(i in 1:length(modal_price))
{
  
  if( (modal_price[i] > out1[Commodity[i]])  || (modal_price[i] < out2[Commodity[i]]))
  {
    x[i] = TRUE } else  {x[i] = FALSE}
  
}
return(x)
}

data14$outlier = outdetect(data14$modal_price,data14$Commodity)
sum(data14$outlier)


data15$outlier = outdetect(data15$modal_price,data15$Commodity)
sum(data15$outlier)


data16$outlier = outdetect(data16$modal_price,data16$Commodity)
sum(data16$outlier)

outlierfile = rbind(data14[data14$outlier == T,],data15[data15$outlier == T,],data16[data16$outlier ==T,])
write.csv(outlierfile,"list of outliers.csv")


data14 = data14[data14$outlier==F,colnames(data14)[colnames(data14) != 'outlier']]
data15 = data15[data15$outlier==F,colnames(data15)[colnames(data15) != 'outlier']]
data16 = data16[data16$outlier==F,colnames(data16)[colnames(data16) != 'outlier']]

new_data = rbind(data14,data15,data16)
nrow(new_data)

#######################################################################

ssacf<- function(x) sum(acf(x, na.action = na.pass, plot = FALSE)$acf^2)
compare_ssacf<-function(add,mult) ifelse(ssacf(add)< ssacf(mult), 
                                         "Additive", "Multiplicative") 

library(zoo)
library(data.table)
class(new_data)

new_data = as.data.table(new_data)
class(new_data)

additive_or_multiplicative <- function(dt){
  m<-copy(dt)
  m[,trend := rollmean(modal_price, 12, fill="extend", align = "right")]
  m[,`:=`( detrended_a = modal_price - trend,  detrended_m = modal_price / trend )]
 # m[Value==0,detrended_m:= 0]
  m[,`:=`(seasonal_a = mean(detrended_a, na.rm = TRUE),
          seasonal_m = mean(detrended_m, na.rm = TRUE)), 
    by=.(Month) ]
  
  m[is.infinite(seasonal_m),seasonal_m:= 1]
  
  m[,`:=`( residual_a = detrended_a - seasonal_a, 
           residual_m = detrended_m / seasonal_m)]
  
  compare_ssacf(m$residual_a, m$residual_m )
}

# Applying it to all time series in table
trial <- new_data[ , .(Type=additive_or_multiplicative(new_data)),
                   by = .(APMC,Commodity)]

View(trial)
sum(trial$Type == 'Multiplicative') == nrow(trial)
write.csv(trial, "seasonality type.csv")
#Thus we see that the type of seasonality is multiplicative for all of them


##################################
decomp <- function(dt){
  m<-copy(dt)
  m[,trend := rollmean(modal_price, 12, fill="extend", align = "right")]
  m[,`:=`( detrended_m = modal_price / trend )]
  # m[Value==0,detrended_m:= 0]
  m[,`:=`(seasonal_m = mean(detrended_m, na.rm = TRUE)), 
    by=.(Month) ]
  
  m[is.infinite(seasonal_m),seasonal_m:= 1]
  
  m[,`:=`( residual_m = detrended_m / seasonal_m)]
  return(m)
}

decomposed <- decomp(new_data)
View(decomposed)
decomposed$deseasonalised = decomposed$modal_price / decomposed$seasonal_m
write.csv(decomposed,"decomposed modal prices.csv")
##################################

#Compare prices in APMC with MSP- raw and deseasonalised


View(msp)
msp$commodity = trimws(tolower(msp$commodity))
nrow(msp)

unique(msp$year)
#Since information of monthly prices for 2012 and 2013 are not given thus we remove their MSPs
msp_new = msp[msp$year != 2012,]
msp_new = msp_new[msp_new$year != 2013,]
unique(msp_new$year)

msp_new = msp_new[!is.na(msp_new$msprice),]

View(decomposed)

a = filter(decomposed,Year == 2014)
b = filter(decomposed,Year == 2015)
c = filter(decomposed,Year == 2016)
d = filter(msp_new,year == 2014)
e = filter(msp_new,year == 2015)
f = filter(msp_new,year == 2016)

a = as.data.table(a);b = as.data.table(b);c = as.data.table(c)

d = as.data.table(d);e = as.data.table(e);f = as.data.table(f)

comparison = function(a,d){
  
a$raw_comp = NA
a$deseas_comp = NA
for(i in 1: nrow(a))
{
  for(j in 1:nrow(d))
  {
 
    if(a$Commodity[i] == d$commodity[j])
    {
      if(a$modal_price[i] < d$msprice[j]){
        a$raw_comp[i] = 'below msp'
      }else{
        a$raw_comp[i] = 'above msp'
      }
      
      if(a$deseasonalised[i] < d$msprice[j]){
        a$deseas_comp[i] = 'below msp'
      }else{
        a$deseas_comp[i] = 'above msp'
      }
  }
}
}
return(a)
}
y14 = comparison(a,d)
y15 = comparison(b,e)
y16 = comparison(c,f)
output = rbind(y14, y15, y16)

unique(output$raw_comp)
unique(output$deseas_comp)

output = (output[complete.cases(output),])

unique(output$raw_comp)
unique(output$deseas_comp)

sum(output$deseas_comp == 'below msp')
sum(output$raw_comp == 'below msp')
#Thus we see that for number of combination of APMC and commodity for deseasonalised
#prices are higher than the raw prices.

View(output)
write.csv(output,'msp comparision - raw and deseasonalised.csv')

#########################################################
#Flag set of APMC/mandis and commodities with highest price fluctuation across 
#different commodities in each relevant season, and year.

#For this we consider the dataset after removal of all the outliers.
new_data$fluctuation = new_data$max_price - new_data$min_price
View(new_data)

y14 = new_data %>% filter(Year == 2014)
y15 = new_data %>% filter(Year == 2015)
y16 = new_data %>% filter(Year == 2016)
View(y14)


set14 = tapply(y14$fluctuation,y14$Month,max)
set15 = tapply(y15$fluctuation,y15$Month,max)
set16 = tapply(y16$fluctuation,y16$Month,max)
unique(y14$Month)
unique(y15$Month)
unique(y16$Month)

set14 = set14[complete.cases(set14)]
set16 = set16[complete.cases(set16)]


combine = function(table,set)
{
  z = NULL
  
  for(i in 1:nrow(table))
  if(table$Month[i] == names(set[1]) && table$fluctuation[i] == set[1])
    z = table[i,]


for(i in 1:nrow(table))
  for(j in 2:length(set))
    if(table$Month[i] == names(set[j]) && table$fluctuation[i] == set[j])
      z = rbind(z,table[i,])
return(z)
}


final = rbind(combine(y14,set14) ,combine(y15,set15) , combine(y16,set16)) %>% arrange(date)
View(final)
write.csv(final,"maximum fluctuation.csv")
