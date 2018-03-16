library(dplyr)
library(ggplot2)
library(reshape2)
require(tidyverse)
require(signal)
require(multitaper)
require(TSAUMN)
require (tidyr)

library(lubridate)

require(zoo)

require(TSAUMN)
require(lubridate)
require(tidyselect)




sg_1 = read.csv('C:\\SecondCreekGit\\DATA\\2016_head_data\\2016_10_02\\CSV_edit\\SG1_shift_test.csv',header = FALSE, sep = ',')
sg_2 = read.csv('C:\\SecondCreekGit\\DATA\\2016_head_data\\2016_10_02\\CSV_edit\\sg2.txt',sep = ',',header = FALSE)

sg_1$Date = as.POSIXct(sg_1$V1, format = '%m/%d/%Y %H:%M')
sg_2$Date = as.POSIXct(sg_2$V1, format = '%Y/%m/%d %H:%M:%S')
sg_1$V2 = as.numeric(as.character(sg_1$V2)) 
sg_2$V2 =  as.numeric(as.character(sg_2$V2))
sg_1$V3 = NULL
sg_1$V1 = NULL

sg_2$V3 = NULL
sg_2$V1 = NULL

sg<- rbind((sg_1), (sg_2) )


sg$Stream_Head = sg$V2
sg$V2 =NULL

pz = read.csv('C:\\SecondCreekGit\\DATA\\2016_head_data\\2016_10_02\\CSV_edit\\pzcw_8_161005160252_M9440.csv',header = FALSE, sep =',')

pz$Date = as.POSIXct(pz$V1, format = "%Y/%m/%d %H:%M:%S")


weather_embarrass = read.csv(file ='C:\\SecondCreekGit\\DATA\\precip.csv' )
weather_embarrass$Date = as.POSIXct(weather_embarrass$Date, format = "%m/%d/%Y")
weather_embarrass$PRCP = as.numeric(as.character(weather_embarrass$PRCP))
weather_embarrass$PRCP[is.na(weather_embarrass$PRCP)] = 0
weather_embarrass$SNOW=NULL
weather_embarrass$SNWD=NULL
weather_embarrass$TMIN=NULL
weather_embarrass$TMAX=NULL

plot(weather_embarrass$PRCP, type = 'l')

df = merge(x=sg, y=weather_embarrass, id = 'Date',all =TRUE)

df= melt(df, id = 'Date')

ggplot(df, aes(x=Date, y = value)) +facet_grid(variable ~., scales="free_y") +geom_line() + labs(title = 'Stream head and precipitation data')

df2 = merge(x=sg, y=weather_embarrass, id = 'Date')
df2$Resampled_Stream_Head = df2$Stream_Head

df2 = merge(x=sg, df2, id = 'Date,', all =TRUE)

df2 = melt(df2, id = 'Date')

df2 = na.omit(df2)
df2$variable =factor(df2$variable, levels = c('Stream_Head', "Resampled_Stream_Head", "PRCP")) 
ggplot(df2, aes(x=Date, y = value)) +facet_grid(variable ~., scales="free_y") +geom_line() + labs(title = 'Stream head and precipitation data')


sg_ts = resample(x= sg$Stream_Head, p= 126, q=18197 )
plot(sg_ts, ylim = c(980,1060), type = 'l', xlab = 'index', ylab = 'Head', main ='Resampled stream head time series')

sg_ts = data.frame("Date" = weather_embarrass$Date, "Head"= sg_ts)
sg_ts$Date = as.POSIXct(sg_ts$Date)
sg_compare = merge(x =sg, y=sg_ts , all.x= TRUE)
sg_compare = melt(sg_compare, id = 'Date')
ggplot(sg_compare, aes(x= Date, y= value)) + facet_grid(variable ~.) +geom_line() 




precip_ts = ts(df2$PRCP)
head_ts = ts(df2$Stream_Head)
m<- lm(head_ts ~ precip_ts )
summary(m)



linearFIT = data.frame('date' = weather_embarrass$Date[1:125] , 'Measured' = head_ts, 'Modeled' = precip_ts * coefficients(m)[2] + coefficients(m)[1])
linearFIT$date = as.POSIXct(linearFIT$date, format = '%m/%d/%Y')
linearFITmelt = melt(linearFIT , id = 'date')
linearFITmelt$date = as.POSIXct(linearFITmelt$date)
linearFITmelt$value = as.numeric(as.character(linearFITmelt$value))
ggplot(data = linearFITmelt, aes(x=date, y= value,  colour = variable, group = variable)) +labs(title = "Linear modeling stream temperature by air temperature") + ylab("Temperature, degrees C") + geom_line() +scale_colour_discrete(name="")
