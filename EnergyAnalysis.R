library(dplyr)
library(ggplot2)

# Create data frame to view Unit Type and Energy Generated
UnitVsEnergy<- SWIS_Analysis_Exc[, c("Filtered Unit", "Energy Generated (MWh)")]

# Identify Filtered Unit Variables
unique(UnitVsEnergy$`Filtered Unit`)

# Separate GBU Variable
GBUdata <- filter(UnitVsEnergy, UnitVsEnergy$`Filtered Unit` == "GBU")

# Find monthly average of GBU Energy Generation from Jan2016- Sep2016
mean(GBUdata$`Energy Generated (MWh)`) #mean of 19.24074

#Separate Filtered Varibles and find mean
WindData <- filter(UnitVsEnergy, UnitVsEnergy$`Filtered Unit` == "Wind")
mean(WindData$`Energy Generated (MWh)`) #mean of 6.2171

PPAData <- filter(UnitVsEnergy, UnitVsEnergy$`Filtered Unit` == "PPA")
mean(PPAData$`Energy Generated (MWh)`, na.rm = TRUE) #mean of 26.2020

SolarData <- filter(UnitVsEnergy, UnitVsEnergy$`Filtered Unit` == "Solar")
mean(SolarData$`Energy Generated (MWh)`, na.rm = TRUE) #mean of 1.1367

JVData <- filter(UnitVsEnergy, UnitVsEnergy$`Filtered Unit` == "JV")
mean(JVData$`Energy Generated (MWh)`, na.rm = TRUE) #mean of 18.8913

OtherData <- filter(UnitVsEnergy, UnitVsEnergy$`Filtered Unit` == "Other")
mean(OtherData$`Energy Generated (MWh)`, na.rm = TRUE) #mean of 11.89


TimeData <- data.frame(SWIS_Analysis_Exc$`Trading Date`, SWIS_Analysis_Exc$`Filtered Unit`, SWIS_Analysis_Exc$`Energy Generated (MWh)`)

names(TimeData)[1]<- "Trading Date"
names(TimeData)[2]<- "Filtered Unit"
names(TimeData)[3] <- "Energy Generated"

# Find trends for each Filtering UNit by Energy Generated and Period
GBUFilt <- filter(TEF, TEF$`Filtered Unit` == "GBU")
GBUFilt<- filter(TEF, TEF$`Filtered Unit` == "GBU")
ggplot(GBUFilt, aes(x= GBUFilt$`Trading Date`, y= GBUFilt$`Energy Generated (MWh)`)) + geom_line()
ggplot(GBUFilt, aes(x= GBUFilt$`Trading Date`, y= GBUFilt$`Energy Generated (MWh)`)) + geom_smooth()

JVFilt <- filter(TEF, TEF$`Filtered Unit` == "JV")
names(JVFilt)[1] <- "Trading Date"
ggplot(JVFilt, aes(x= JVFilt$`Trading Date`, y= JVFilt$`Energy Generated (MWh)`)) + geom_line()
ggplot(JVFilt, aes(x= JVFilt$`Trading Date`, y= JVFilt$`Energy Generated (MWh)`,color= "Energy Generated")) + geom_smooth()

PPAFilt <- filter(TEF, TEF$`Filtered Unit` == "PPA")
names(PPAFilt)[1] <- "Trading Date"
ggplot(PPAFilt, aes(x= PPAFilt$`Trading Date`, y= PPAFilt$`Energy Generated (MWh)`)) + geom_line()
ggplot(PPAFilt, aes(x= PPAFilt$`Trading Date`, y= PPAFilt$`Energy Generated (MWh)`, color= "Energy Generated")) + geom_smooth()

solarFilt <- filter(TEF, TEF$`Filtered Unit` == "Solar")
names(solarFilt)[1] <- "Trading Date"
ggplot(solarFilt, aes(x= solarFilt$`Trading Date`, y= solarFilt$`Energy Generated (MWh)`)) + geom_line()
ggplot(solarFilt, aes(x= solarFilt$`Trading Date`, y= solarFilt$`Energy Generated (MWh)`, color= "Energy Generated")) + geom_smooth()

WindFilt <- filter(TEF, TEF$`Filtered Unit`== "Wind")
names(WindFilt)[1] <- "Trading Date"
ggplot(WindFilt, aes(x= WindFilt$`Trading Date`, y= WindFilt$`Energy Generated (MWh)`)) + geom_line()
ggplot(WindFilt, aes(x= WindFilt$`Trading Date`, y= WindFilt$`Energy Generated (MWh)`, color= "Energy Generated")) + geom_smooth()

OtherFilt <- filter(TEF, TEF$`Filtered Unit`== "Other")
names(OtherFilt)[1]<- "Trading Date"
ggplot(OtherFilt, aes(x= OtherFilt$`Trading Date`, y= OtherFilt$`Energy Generated (MWh)`)) + geom_line()
ggplot(OtherFilt, aes(x= OtherFilt$`Trading Date`, y= OtherFilt$`Energy Generated (MWh)`, color= "Energy Generated")) + geom_smooth()


ggplot(MeanData, aes(x= MeanData$Filtered_Unit, y= MeanData$Average))+ geom_bar(stat = "identity")


EOI <- data.frame(SWIS_Analysis_Exc$`Trading Date`, SWIS_Analysis_Exc$`Filtered Unit`, SWIS_Analysis_Exc$`Energy Generated (MWh)`, SWIS_Analysis_Exc$`EOI Quantity (MW)`)
names(EOI)[1]<- "Trading Date"
names(EOI)[2]<- "Filtered Unit"
names(EOI)[3] <- "Energy Generated"
names(EOI)[4]<- "EOI"


windEOI <- subset(eoi, eoi$`Filtered Unit`== "Wind")
ggplot(data=windEOI)+ geom_smooth(mapping = aes(x= windEOI$`Trading Date`, y= windEOI$`Energy Generated`, color= "Energy Generated")) + geom_smooth(mapping = aes(x=windEOI$`Trading Date`, y= windEOI$EOI, color= "EOI"))

JVEOI <- subset(eoi, eoi$`Filtered Unit`== "JV")
ggplot(data=JVEOI)+ geom_smooth(mapping = aes(x= JVEOI$`Trading Date`, y= JVEOI$`Energy Generated`, color= "Energy Generated")) + geom_smooth(mapping = aes(x=JVEOI$`Trading Date`, y= JVEOI$EOI, color= "EOI"))

solarEOI <- subset(eoi, eoi$`Filtered Unit`== "Solar")
ggplot(data=solarEOI)+ geom_smooth(mapping = aes(x= solarEOI$`Trading Date`, y= solarEOI$`Energy Generated`, color= "Energy Generated")) + geom_smooth(mapping = aes(x=solarEOI$`Trading Date`, y= solarEOI$EOI, color= "EOI"))

PPAEOI <- subset(eoi, eoi$`Filtered Unit`== "PPA")
ggplot(data=PPAEOI)+ geom_smooth(mapping = aes(x= PPAEOI$`Trading Date`, y= PPAEOI$`Energy Generated`, color= "Energy Generated")) + geom_smooth(mapping = aes(x=PPAEOI$`Trading Date`, y= PPAEOI$EOI, color= "EOI"))

otherEOI <-  subset(eoi, eoi$`Filtered Unit`== "Other")
ggplot(data=otherEOI)+ geom_smooth(mapping = aes(x= otherEOI$`Trading Date`, y= otherEOI$`Energy Generated`, color= "Energy Generated")) + geom_smooth(mapping = aes(x=otherEOI$`Trading Date`, y= otherEOI$EOI, color = "EOI"))

# PPA = 0.6227186
# JV= 0.6703897
# Solar= 0.5681242
# Wind= 0.1461075
# Other = 0.561987
# Total = 0.5112186

ggplot( eoi2, aes(x= eoi2$`Filtered Unit`, y= eoi2$EOI))+ geom_boxplot(outlier.colour = "red", outlier.shape = .5)+ scale_y_continuous(limit = c(0, 185))

ggplot( tef2, aes(x= tef2$`Filtered Unit`, y= tef2$`Energy Generated (MWh)`))+ geom_boxplot(outlier.colour = "red", outlier.shape = .5)+ scale_color_manual(values = c('jv'= 'blue'))


ggplot( tef2, aes(x= tef2$`Filtered Unit`, y= tef2$`Energy Generated (MWh)`))+ geom_boxplot(outlier.colour = "red", outlier.shape = .5)+ scale_fill_brewer(palette = "Dark2")

boxplot(tef2$`Energy Generated (MWh)`~ tef2$`Filtered Unit`, data = tef2, xlab= "Unit", ylab = "Energy Generated")

boxplot(windno0$`Energy Generated (MWh)`~windno0$`Filtered Unit`, data = windno0)

ggplot(eoi, aes(x= eoi$`Filtered Unit`, y= eoi$`Energy Generated`))+ geom_boxplot(outlier.colour = "red", outlier.shape = 8)

ggplot(data=JVEOI)+ geom_smooth(mapping = aes(x= JVEOI$`Trading Date`, y= JVEOI$`Energy Generated`, color= "Energy Generated")) + geom_smooth(mapping = aes(x=JVEOI$`Trading Date`, y= JVEOI$EOI, color= "EOI"))

ggplot(data=PPAEOI)+ geom_smooth(mapping = aes(x= PPAEOI$`Trading Date`, y= PPAEOI$`Energy Generated`, color= "Energy Generated")) + geom_smooth(mapping = aes(x=PPAEOI$`Trading Date`, y= PPAEOI$EOI, color= "EOI"))

ggplot(data=solarEOI)+ geom_smooth(mapping = aes(x= solarEOI$`Trading Date`, y= solarEOI$`Energy Generated`, color= "Energy Generated")) + geom_smooth(mapping = aes(x=solarEOI$`Trading Date`, y= solarEOI$EOI, color= "EOI"))

ggplot(data=windEOI)+ geom_smooth(mapping = aes(x= windEOI$`Trading Date`, y= windEOI$`Energy Generated`, color= "Energy Generated")) + geom_smooth(mapping = aes(x=windEOI$`Trading Date`, y= windEOI$EOI, color= "EOI"))


