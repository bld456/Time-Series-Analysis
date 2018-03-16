require (dplyr)
require (tidyr)
require (ggplot2)
library(zoo)
require(TSAUMN)
require(lubridate)
require(tidyselect)
#import and read into DF
file <- 'C:\\Users\\Jack\\Documents\\TS ANALYSIS\\TIMunaltered.txt'
TIM <- read.table(file, skip =120 , header = FALSE, na.strings = c("0.0000", "0.000e0") )

#Drop unnecessary columns 
TIM <- TIM[c(1,5,10)]  
#name columns
colnames(TIM)[2]= "TSI@1"
colnames(TIM)[3]= "TSI@E"

#split date column
TIM <- separate(TIM, col =1, into = c("year", "month", "day", "hour"), sep = c(4, 6, 8) )


#Multiply fractional hours by 24
TIM$hour <- as.numeric(TIM$hour) * 24


#make POSIXct date object
TIM$POSIXctDATE <- ISOdatetime(year = TIM$year, month = TIM$month, day = TIM$day, hour = TIM$hour, min = 0 , sec = 0)

#plot 
ggplot() + geom_line(data =TIM, aes(TIM$POSIXctDATE, TIM$`TSI@1`)) + ggtitle("Total Solar Irradiance at 1 AU ") +xlab("Date") + ylab("TSI")
ggplot() + geom_line(data =TIM, aes(TIM$POSIXctDATE, TIM$`TSI@E`)) + ggtitle("Total Solar Irradiance at Earth Distance ") +xlab("Date") + ylab("TSI")



#make  new DF without NA     UNNECESSARY because spec.mtls can handle NA
#TIM_omit_NA <- na.omit(TIM)
 #TIM_omit_NA = TIM

#fill in true earth distance data with mean TSI value
TIM$'filledTSI@E' <- na.aggregate(TIM$`TSI@E`)

#plot filled in TS
ggplot() + geom_line(data =TIM, aes(TIM$POSIXctDATE, TIM$`filledTSI@E`)) + ggtitle("Total Solar Irradiance at 1 AU, NA replaced with mean ") +xlab("Date") + ylab("TSI")

#convert dates to numeric
TIM$numDATE <- decimal_date(TIM$POSIXctDATE)
#TIM_omit_NA$numDate <- decimal_date(TIM_omit_NA$POSIXctDATE)

# plot spectrum for filled in time series
SpectrumFilled <- spec.mtls(x= TIM$`filledTSI@E`, t= TIM$numDATE,plot = FALSE)



#plot spectrum for time series with missing data
#SpectrumOmit <- spec.mtls(x= TIM_omit_NA$`TSI@E`, t= TIM_omit_NA$numDATE,plot = TRUE)  DOESNT WORK, CANT FIGURE OUT WHY
SpectrumOmit <- spec.mtls(x= TIM$`TSI@E`, t= TIM$numDATE,plot = FALSE)


# plot both spectra in log space
dfFilled <- data.frame(SpectrumFilled)
dfOmit <- data.frame(SpectrumOmit)
ggplot() +geom_line(data = dfFilled, aes(dfFilled$freq, dfFilled$spec)) + scale_y_log10() + ggtitle('Filled data set specrum')
ggplot() +geom_line(data = dfOmit, aes(dfOmit$freq, dfOmit$spec)) + scale_y_log10() + ggtitle('Omitted data set specrum')

#quantify differemces in spectra
maxF <- max(SpectrumFilled$spec)
maxO <- max(SpectrumOmit$spec)

 
maxF
maxO
#The maximum power is lower in the  spectrum of the time series where NA values were omitted. 

FpeakF = SpectrumFilled$freq[which.max(SpectrumFilled$spec)] #Filled peak frequency
FpeakF

OpeakF <- SpectrumOmit$freq[which.max(SpectrumOmit$spec)] #omitted peak frequency
OpeakF

PeriodF = FpeakF
periodO = OpeakF

PeriodF
periodO

#Both time series have most of their power around periodicity of 1 year. The peak of the spectrum from the omitted data time series
# is wider than the peak of the spectra for the filled in data set.


#BONUS: The data at one AU and true-Earth distance because the earth's orbit is eliptical. If the Earth's orbit was exactly
# one AMU, then it would experience roughly constant solar irradiance. 