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

meltTemp = melt(TempProfile, id = 'Date')
ggplot(data = meltTemp, aes (x= Date, y= value, colour = variable, group = variable))+labs(title = "Stream Center Temperature Probe ") + ylab("Temperature, degrees C") + geom_line() +scale_colour_discrete(name="Depth")


ggplot(data = TempProfile, aes (x= Date, y= TempProfile$'0'))+labs(title = "Diurnal Stream Temperature Fluctuatuion ") + ylab("Temperature, degrees C") + geom_line() + xlim(as.POSIXct(c('2016-06-21 00:00)','2016-06-25 00:00')))


temp0 = ts(data = TempProfile$`0`, start = decimal_date(as.POSIXct('2016- 05-30 16:00')), deltat = 1/144)
temp0.05 = ts(data = TempProfile$`0.05`, start = decimal_date(as.POSIXct('2016- 05-30 16:00')), deltat = 1/144)
temp0.1 = ts(data = TempProfile$`0.1`, start = decimal_date(as.POSIXct('2016- 05-30 16:00')), deltat = 1/144)
temp0.15 = ts(data = TempProfile$`0.15`, start = decimal_date(as.POSIXct('2016- 05-30 16:00')), deltat = 1/144)
temp0.2 = ts(data = TempProfile$`0.2`, start = decimal_date(as.POSIXct('2016- 05-30 16:00')), deltat = 1/144)

temp30 = ts(data = TempProfile$`0.3`, start = decimal_date(as.POSIXct('2016- 05-30 16:00')), deltat = 1/144)




spec_temp0 = spec.mtm(temp0)
spec_temp0.05 = spec.mtm(temp0.05)
spec_temp0.1 = spec.mtm(temp0.1)
spec_temp0.15 = spec.mtm(temp0.15)

spec_temp0.2 = spec.mtm(temp0.2)
spec_temp30 = spec.mtm(temp30)
df = data.frame("Frequency"= spec_temp0$freq,"0" = spec_temp0$spec, "0.05"=spec_temp0.05$spec , "0.1" = spec_temp0.1$spec, "0.15" = spec_temp0.15$spec, "0.2" = spec_temp0.2$spec, "0.3"=spec_temp30$spec)
df = melt(df, id="Frequency")
ggplot(df, aes(x= Frequency, y= value)) +  facet_grid(variable ~.) +geom_line() +xlim(c(0,1.1)) +labs("Spectral analysis of spectral data")


fplot(spec_temp30$freq, spec_temp30$spec, xlim = c(0,0.2), type = 'l')

spec_temp0 = spec.mtm(temp0)
plot(spec_temp0$freq, spec_temp0$spec, xlim = c(0,10), type = 'l')

#do an f test to see if noise at low freq
#filter out entire summer trend to get spectrum that only has daily fluctuations(remove seasonality)
d_temp0 = stats::decompose(temp0,type = "mult")
gplot.ts(d_temp0$seasonal) +labs (y='daily seasonality in temp')

gplot.ts(d_temp0$trend) +labs (y='trend in temp')
spec.mtm(d_temp0$trend)

filt = cosfilt(temp0, fc =c(0.8, 0.9 , 1.1, 1.2) , fw = c(1,1,0,0))
plot( filt)

gplot.ts( xy=d_temp0$trend*d_temp0$seasonal-d_temp0$trend)+xlab('') +ylab('Daily temperature fluctuation, degrees C')+labs(title = "Stream Temperature Envelope")###really sick plot showing changing temp envelope over summer

#lets plot the changing temp envelope over summer for air temperature
 

#import air temp and precip data
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

df= data.frame("date" = seq(as.Date("2016/05/30"), as.Date("2016/10/2"), by ="day") , "Air_Temp" = AMBIENTts, "Water_Temp"= WATERts)
df=melt(df, id = "date")
ggplot(df, aes(x= date, y=value))+  facet_grid(variable ~.) +geom_line()  +labs(title ="Air and stream temperature") +ylab("temperature, degrees C")

AMBIENTdetrend = detrend( AMBIENTts, n = 3 )
WATERdetrend = detrend(WATERts, n = 3)
plot.ts( AMBIENTdetrend, xlab = 'days from 5/30/16', ylab = 'temperature, degrees C' , main = 'Maximum ambient air temperature detrend')
plot.ts( WATERdetrend, xlab = 'days from 5/30/16', ylab = 'temperature, degrees C' , main = 'Maximum water temperature detrend')


df= data.frame("date" = seq(as.Date("2016/05/30"), as.Date("2016/9/30"), by ="day") , "Air_Temp" = AMBIENTdetrend, "Water_Temp"= window(WATERdetrend, end = 124))
df=melt(df, id = "date")
ggplot(df, aes(x= date, y=value))+  facet_grid(variable ~.) +geom_line()  +labs(title ="Air and stream temperature, trend removed") +ylab("temperature, degrees C")


WATERspec = spec.mtm( WATERdetrend, returnInternals = TRUE)
AIRspec = spec.mtm(AMBIENTdetrend, returnInternals = TRUE)

mtm.coh(WATERspec, AIRspec)

WATERdf= data.frame(WATERT=as.matrix(WATERdetrend ), date=time(WATERdetrend ))
AIRdf = data.frame(AIRT=as.matrix(AMBIENTdetrend ), date=time(AMBIENTdetrend ))
data = inner_join(WATERdf, AIRdf, by = 'date')
data$date = NULL
PCA = prcomp(data)

pcaVar( PCA )
library(factoextra)

fviz_pca(PCA)  +labs(x= "PC1", y= "PC2")






#work with rainfall da ta
plot(weather_embarrass$PRCP, type = 'l')

PRCP_embarrass = ts(data = na.omit(weather_embarrass$PRCP))

plot(PRCP_embarrass)
spec.mtm(PRCP_embarrass)

#work with head data
head_dif = read.csv('C:\\SecondCreekGit\\SCRIPT OUTPUTS\\HEAD DIFFERENCES\\Scaled using PZStickup\\PZCW full summer_shift.csv', header = FALSE)

head_dif$Date = as.POSIXct(head$V1, format = "%m/%d/%Y %H:%M")
#ds = 

plot(head_dif$Date, head_dif$V2, type = 'l')
#need to make a new date series and fill in NA with the mean dh
Dates = seq(as.Date("2016-06-04 03:30:00"), as.Date("2016-10-01 14:00:00
"), by =".25 Hours") #NEED TO FIGURE OUT PROPER by

head_dif = ts(data = head_dif$V2, deltat = 1/4) #can't skip days
gplot.ts(head_dif)


#stream gauge head data
sg_1 = read.csv('C:\\SecondCreekGit\\DATA\\2016_head_data\\2016_10_02\\CSV_edit\\sg1_4_161005161403_H2366FirstPlacement.csv',header = FALSE)
sg_2 = read.csv('C:\\SecondCreekGit\\DATA\\2016_head_data\\2016_10_02\\CSV_edit\\sg1_4_161005161403_H2366SecondPlacement.csv',header = FALSE)

sg_1$Date = as.POSIXct(sg_1$V1, format = "%Y/%m/%d %H:%M:%S")
sg_2$Date = as.POSIXct(sg_2$V1, format = "%Y/%m/%d %H:%M:%S")

sg_1 = ts(sg_1$V2)
spec.mtm(sg_1) #one interesting peak

#pz head data
pz = read.csv('C:\\SecondCreekGit\\DATA\\2016_head_data\\2016_10_02\\CSV_edit\\pzcw_8_161005160252_M9440.csv',header = FALSE)

pz$Date = as.POSIXct(pz$V1, format = "%Y/%m/%d %H:%M:%S")
pz = ts(pz$V2)
spec.mtm(pz)#couple of interesting drop-offs



#SEE HOW THESE TS EXPLAIN EACH
#do somehting with rain data
#do something with head data
#is the yearly periodicity resolvable with our nyquist?
#how is water temp related to air temp ambient
#any corelation with atmospheric pressure and rainfall or temp?

#PCA analysis for entire summmer temp trends (stream and air)
