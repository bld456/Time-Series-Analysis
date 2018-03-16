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
TempProfile <- read.csv(file = 'C:\\SecondCreekGit\\DATA\\Temp_data\\FULL SUMMER TPA_CalibData_1DTempPro.csv')
TempProfile = plyr::rename(TempProfile, c(X = 'Date', X0 ='0',X0.05 = '0.05', X0.1=  '0.1',  X0.15='0.15', X0.2='0.2',X0.3='0.3'))
TempProfile$Date = as.POSIXct(TempProfile$Date, format = "%m/%d/%Y %H:%M")

TempProfile$`0` = as.numeric(TempProfile$`0`)
TempProfile$`0.05` = as.numeric(TempProfile$`0.05`)
TempProfile$`0.1` = as.numeric(TempProfile$`0.1`)
TempProfile$`0.15` = as.numeric(TempProfile$`0.15`)
TempProfile$`0.2` = as.numeric(TempProfile$`0.2`)
TempProfile$`0.3` = as.numeric(TempProfile$`0.3`)



weather_embarrass = read.csv(file ='C:\\SecondCreekGit\\DATA\\precip.csv' )
weather_embarrass$Date = as.POSIXct(weather_embarrass$Date)
weather_embarrass$TMAX = as.numeric(as.character(weather_embarrass$TMAX))

weather_embarrass$TMIN = as.numeric(as.character(weather_embarrass$TMIN))
weather_embarrass$PRCP = as.numeric(as.character(weather_embarrass$PRCP))





#work with maximum temperature time series

maxT_embarrass = ts(data = na.aggregate(weather_embarrass$TMAX))

plot(maxT_embarrass)

spec.mtm(maxT_embarrass)
d_maxT_embarass =  stats::decompose(maxT_embarrass,type = "mult") #FAILS - there is no strong periodicity




#work with minimum temperature time series

minT_embarrass = ts(data = na.aggregate(weather_embarrass$TMIN))
plot(minT_embarrass)

spec.mtm(minT_embarrass)
d_minT_embarass =  stats::decompose(minT_embarrass,type = "mult") #fails, no strong periodicity


avgT_embarrass = (maxT_embarrass + minT_embarrass)/2
plot(x= weather_embarrass$Date, y= (avgT_embarrass -32)/(9/5), type = 'l', ylab ='Temperature, degrees celcius', main = 'Average ambient air temperature', xlab = 'Date')


#cross correlation max air temp and max stream temp
df = data.frame(TempProfile$Date, TempProfile$`0`)
maxTEMP0 = summarize(group_by(df, cut(df$TempProfile.Date, breaks = "day")),max(TempProfile..0.))
plot(maxTEMP0, type = 'l')

df = data.frame('Date' = maxTEMP0$`cut(df$TempProfile.Date, breaks = "day")`, "Daily_maximum_temperature_degrees_C" = maxTEMP0$`max(TempProfile..0.)`)
df$Date = as.POSIXct(df$Date)
ggplot(data = df, aes (x = Date, y = Daily_maximum_temperature_degrees_C))+geom_line() + labs(title ="Daily maximum stream temperature") + ylab("Temperature, degrees C")
weather_embarrass$TMAXc = (weather_embarrass$TMAX - 32)/(9/5)
ggplot(data = weather_embarrass, aes(x = Date, y = TMAXc, group = 1)) +geom_line() +labs( title  ="Daily maximum ambient temperature" ) +ylab ("temperature, degrees C")

#DETREND BOTH ts (STREAM MAX T AND AMBIENT MAX T, THEN DO COHERENCY ANALYSIS)

AMBIENTts = ts(weather_embarrass$TMAXc )
WATERts =ts(df$Daily_maximum_temperature_degrees_C )

AMBIENTdetrend = detrend( AMBIENTts, n = 3 )
WATERdetrend = detrend(WATERts, n = 3)
plot.ts( AMBIENTdetrend, xlab = 'days from 5/30/16', ylab = 'temperature, degrees C' , main = 'Maximum ambient air temperature detrend')
plot.ts( WATERdetrend, xlab = 'days from 5/30/16', ylab = 'temperature, degrees C' , main = 'Maximum water temperature detrend')


WATERspec = spec.mtm( WATERdetrend, returnInternals = TRUE)
AIRspec = spec.mtm(AMBIENTdetrend, returnInternals = TRUE)

mtm.coh(WATERspec, AIRspec)

WATERdf= data.frame(WATERT=as.matrix(WATERdetrend ), date=time(WATERdetrend ))
AIRdf = data.frame(AIRT=as.matrix(AMBIENTdetrend ), date=time(AMBIENTdetrend ))
data = inner_join(WATERdf, AIRdf, by = 'date')
data$date = NULL
PCA = prcomp(data, scale = TRUE)

pcaVar( PCA )
require(factoextra)

fviz_pca_var(PCA)  +labs(x= "PC1", y= "PC2") +coord_fixed(ratio = 1) 




#PCA not super informative, lets do linear modeling

m <- lm(window(WATERdetrend,end = 124) ~ AMBIENTdetrend )

summary(m)


linearFIT = data.frame('date' = weather_embarrass$Date[1:124] , 'Measured' = window(WATERdetrend,end = 124), 'Modeled' = AMBIENTdetrend * coefficients(m)[2] + coefficients(m)[1])
linearFIT$date = as.POSIXct(linearFIT$date, format = '%m/%d/%Y')
linearFITmelt = melt(linearFIT , id = 'date')
linearFITmelt$date = as.POSIXct(linearFITmelt$date)
linearFITmelt$value = as.numeric(as.character(linearFITmelt$value))
ggplot(data = linearFITmelt, aes(x=date, y= value,  colour = variable, group = variable)) +labs(title = "Linear modeling stream temperature by air temperature") + ylab("Temperature, degrees C") + geom_line() +scale_colour_discrete(name="")


ts.plot( AMBIENTdetrend * coefficients(m)[2] + coefficients(m)[1], window(WATERdetrend,end = 124), gpars = list(col = c( "black", "red")))
