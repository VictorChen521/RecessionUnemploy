

library(readxl)
library(plyr)
library(tidyr)
library(magrittr)
library(stringr)
library(leaflet)
library(Quandl)
library(tigris)
library(lubridate)
library(scales)

TotEmployed<-read_excel('ststdsadata.xlsx')

Payroll<-read_excel("Employee on payroll.xlsx",sheet = 'Sheet1')
Payroll$`Employee on payroll`<-as.numeric(Payroll$`Employee on payroll`)
Payroll$`Employee on payroll`<-c(round(Payroll$`Employee on payroll`,digits = 0))
Payroll<-merge(Payroll[,c(1:4)],State,by.x = 'State',by.y = 'State and area',all.x = T)
Payroll$Month<-factor(Payroll$Month,levels = c('Feb','Mar','Apr','May','Jun','Jul'))

ConstructionPay<-Payroll[which(Payroll$Industry=='Construction'),]
ConstructionPay$Combine<-paste(ConstructionPay$Industry,ConstructionPay$Month,sep = "_")
ConstructionPayWide<-spread(ConstructionPay[,-2],Combine,`Employee on payroll`)
ConstructionPayWide$Mar<-c(ConstructionPayWide$Construction_Mar-ConstructionPayWide$Construction_Feb)
ConstructionPayWide$Apr<-c(ConstructionPayWide$Construction_Apr-ConstructionPayWide$Construction_Mar)
ConstructionPayWide$May<-c(ConstructionPayWide$Construction_May-ConstructionPayWide$Construction_Apr)
ConstructionPayWide$Jun<-c(ConstructionPayWide$Construction_Jun-ConstructionPayWide$Construction_May)
ConstructionPayWide$Jul<-c(ConstructionPayWide$Construction_Jul-ConstructionPayWide$Construction_Jun)

ConstructionPayL<- ConstructionPayWide[,c(1:3,10:14)] %>%
  gather(Month,Change,Mar:Jul)


MaufacturePay<-Payroll[which(Payroll$Industry=='Manufacturing'),]
MaufacturePay$Combine<-paste(MaufacturePay$Industry,MaufacturePay$Month,sep = "_")
MaufacturePayWide<-spread(MaufacturePay[,-2],Combine,`Employee on payroll`)
MaufacturePayWide$Mar<-c(MaufacturePayWide$Manufacturing_Mar-MaufacturePayWide$Manufacturing_Feb)
MaufacturePayWide$Apr<-c(MaufacturePayWide$Manufacturing_Apr-MaufacturePayWide$Manufacturing_Mar)
MaufacturePayWide$May<-c(MaufacturePayWide$Manufacturing_May-MaufacturePayWide$Manufacturing_Apr)
MaufacturePayWide$Jun<-c(MaufacturePayWide$Manufacturing_Jun-MaufacturePayWide$Manufacturing_May)
MaufacturePayWide$Jul<-c(MaufacturePayWide$Manufacturing_Jul-MaufacturePayWide$Manufacturing_Jun)
MaufacturePayL<- MaufacturePayWide[,c(1:3,10:14)] %>%
  gather(Month,Change,Mar:Jul)

TradePay<-Payroll[which(Payroll$Industry=='Trade, transportation, and utilities'),]
TradePay$Combine<-paste(TradePay$Industry,TradePay$Month,sep = "_")
TradePayWide<-spread(TradePay[,-2],Combine,`Employee on payroll`)
TradePayWide$Mar<-c(TradePayWide$`Trade, transportation, and utilities_Mar`-TradePayWide$`Trade, transportation, and utilities_Feb`)
TradePayWide$Apr<-c(TradePayWide$`Trade, transportation, and utilities_Apr`-TradePayWide$`Trade, transportation, and utilities_Mar`)
TradePayWide$May<-c(TradePayWide$`Trade, transportation, and utilities_May`-TradePayWide$`Trade, transportation, and utilities_Apr`)
TradePayWide$Jun<-c(TradePayWide$`Trade, transportation, and utilities_Jun`-TradePayWide$`Trade, transportation, and utilities_May`)
TradePayWide$Jul<-c(TradePayWide$`Trade, transportation, and utilities_Jul`-TradePayWide$`Trade, transportation, and utilities_Jun`)

TradePayL<- TradePayWide[,c(1:3,10:14)] %>%
  gather(Month,Change,Mar:Jul)


FinPay<-Payroll[which(Payroll$Industry=='Financial activities'),]
FinPay$Combine<-paste(FinPay$Industry,FinPay$Month,sep = "_")
FinPayWide<-spread(FinPay[,-2],Combine,`Employee on payroll`)
FinPayWide$Mar<-c(FinPayWide$`Financial activities_Mar`-FinPayWide$`Financial activities_Feb`)
FinPayWide$Apr<-c(FinPayWide$`Financial activities_Apr`-FinPayWide$`Financial activities_Mar`)
FinPayWide$May<-c(FinPayWide$`Financial activities_May`-FinPayWide$`Financial activities_Apr`)
FinPayWide$Jun<-c(FinPayWide$`Financial activities_Jun`-FinPayWide$`Financial activities_May`)
FinPayWide$Jul<-c(FinPayWide$`Financial activities_Jul`-FinPayWide$`Financial activities_Jun`)

FinPayL<- FinPayWide[,c(1:3,10:14)] %>%
  gather(Month,Change,Mar:Jul)


BusPay<-Payroll[which(Payroll$Industry=='Professional and business services'),]
BusPay$Combine<-paste(BusPay$Industry,BusPay$Month,sep = "_")
BusPayWide<-spread(BusPay[,-2],Combine,`Employee on payroll`)
BusPayWide$Mar<-c(BusPayWide$`Professional and business services_Mar`-BusPayWide$`Professional and business services_Feb`)
BusPayWide$Apr<-c(BusPayWide$`Professional and business services_Apr`-BusPayWide$`Professional and business services_Mar`)
BusPayWide$May<-c(BusPayWide$`Professional and business services_May`-BusPayWide$`Professional and business services_Apr`)
BusPayWide$Jun<-c(BusPayWide$`Professional and business services_Jun`-BusPayWide$`Professional and business services_May`)
BusPayWide$Jul<-c(BusPayWide$`Professional and business services_Jul`-BusPayWide$`Professional and business services_Jun`)

BusPayL<- BusPayWide[,c(1:3,10:14)] %>%
  gather(Month,Change,Mar:Jul)

EduPay<-Payroll[which(Payroll$Industry=='Education and health services'),]
EduPay$Combine<-paste(EduPay$Industry,EduPay$Month,sep = "_")
EduPayWide<-spread(EduPay[,-2],Combine,`Employee on payroll`)
EduPayWide$Mar<-c(EduPayWide$`Education and health services_Mar`-EduPayWide$`Education and health services_Feb`)
EduPayWide$Apr<-c(EduPayWide$`Education and health services_Apr`-EduPayWide$`Education and health services_Mar`)
EduPayWide$May<-c(EduPayWide$`Education and health services_May`-EduPayWide$`Education and health services_Apr`)
EduPayWide$Jun<-c(EduPayWide$`Education and health services_Jun`-EduPayWide$`Education and health services_May`)
EduPayWide$Jul<-c(EduPayWide$`Education and health services_Jul`-EduPayWide$`Education and health services_Jun`)

EduPayL<- EduPayWide[,c(1:3,10:14)] %>%
  gather(Month,Change,Mar:Jul)

LeiPay<-Payroll[which(Payroll$Industry=='Leisure and hospitality'),]
LeiPay$Combine<-paste(LeiPay$Industry,LeiPay$Month,sep = "_")
LeiPayWide<-spread(LeiPay[,-2],Combine,`Employee on payroll`)
LeiPayWide$Mar<-c(LeiPayWide$`Leisure and hospitality_Mar`-LeiPayWide$`Leisure and hospitality_Feb`)
LeiPayWide$Apr<-c(LeiPayWide$`Leisure and hospitality_Apr`-LeiPayWide$`Leisure and hospitality_Mar`)
LeiPayWide$May<-c(LeiPayWide$`Leisure and hospitality_May`-LeiPayWide$`Leisure and hospitality_Apr`)
LeiPayWide$Jun<-c(LeiPayWide$`Leisure and hospitality_Jun`-LeiPayWide$`Leisure and hospitality_May`)
LeiPayWide$Jul<-c(LeiPayWide$`Leisure and hospitality_Jul`-LeiPayWide$`Leisure and hospitality_Jun`)

LeiPayL<- LeiPayWide[,c(1:3,10:14)] %>%
  gather(Month,Change,Mar:Jul)


GovPay<-Payroll[which(Payroll$Industry=='Government'),]
GovPay$Combine<-paste(GovPay$Industry,GovPay$Month,sep = "_")
GovPayWide<-spread(GovPay[,-2],Combine,`Employee on payroll`)
GovPayWide$Mar<-c(GovPayWide$Government_Mar-GovPayWide$Government_Feb)
GovPayWide$Apr<-c(GovPayWide$Government_Apr-GovPayWide$Government_Mar)
GovPayWide$May<-c(GovPayWide$Government_May-GovPayWide$Government_Apr)
GovPayWide$Jun<-c(GovPayWide$Government_Jun-GovPayWide$Government_May)
GovPayWide$Jul<-c(GovPayWide$Government_Jul-GovPayWide$Government_Jun)

GovPayL<- GovPayWide[,c(1:3,10:14)] %>%
  gather(Month,Change,Mar:Jul)

DT<-do.call('rbind',list(ConstructionPayL,MaufacturePayL,TradePayL,FinPayL,BusPayL,EduPayL,LeiPayL,GovPayL))


Joblessclaim<-read_excel("JoblessClaim.xlsx")
StateAbbr<-read.csv('StateAbbr.csv')
colnames(StateAbbr)[c(1,3)]<-c('Name','State')
NationalEmploy<-read_excel("NationalEmployment.xlsx")
NationalEmploy$DATE<-as.Date(as.character(NationalEmploy$DATE),"%Y-%m-%d")

NationalEmployFinCrisis<-subset(NationalEmploy, DATE<=('2009-06-01') & DATE>=('2007-12-01'))
NationalEmployCovidCs<-subset(NationalEmploy,DATE>=('2020-02-01'))


NationalEmployFinCrisis$Period<-c("Financial")
NationalEmployFinCrisis$Construction<-NationalEmployFinCrisis$USCONS-NationalEmployFinCrisis$USCONS[1]
NationalEmployFinCrisis$Manufacturing<-NationalEmployFinCrisis$MANEMP-NationalEmployFinCrisis$MANEMP[1]
NationalEmployFinCrisis$`Trade, transportation, and utilities`<-NationalEmployFinCrisis$USTPU-NationalEmployFinCrisis$USTPU[1]
NationalEmployFinCrisis$`Financial activities`<-NationalEmployFinCrisis$USFIRE-NationalEmployFinCrisis$USFIRE[1]
NationalEmployFinCrisis$`Professional and business services`<-NationalEmployFinCrisis$USPBS-NationalEmployFinCrisis$USPBS[1]
NationalEmployFinCrisis$`Leisure and hospitality`<-NationalEmployFinCrisis$USLAH-NationalEmployFinCrisis$USLAH[1]
NationalEmployFinCrisis$`Education and health services`<-NationalEmployFinCrisis$USEHS-NationalEmployFinCrisis$USEHS[1]
NationalEmployFinCrisis$`Government`<-NationalEmployFinCrisis$USGOVT-NationalEmployFinCrisis$USGOVT[1]

NationalEmployCovidCs$Period<-c("Covid")
NationalEmployCovidCs$Construction<-NationalEmployCovidCs$USCONS-NationalEmployCovidCs$USCONS[1]
NationalEmployCovidCs$Manufacturing<-NationalEmployCovidCs$MANEMP-NationalEmployCovidCs$MANEMP[1]
NationalEmployCovidCs$`Trade, transportation, and utilities`<-NationalEmployCovidCs$USTPU-NationalEmployCovidCs$USTPU[1]
NationalEmployCovidCs$`Financial activities`<-NationalEmployCovidCs$USFIRE-NationalEmployCovidCs$USFIRE[1]
NationalEmployCovidCs$`Professional and business services`<-NationalEmployCovidCs$USPBS-NationalEmployCovidCs$USPBS[1]
NationalEmployCovidCs$`Leisure and hospitality`<-NationalEmployCovidCs$USLAH-NationalEmployCovidCs$USLAH[1]
NationalEmployCovidCs$`Education and health services`<-NationalEmployCovidCs$USEHS-NationalEmployCovidCs$USEHS[1]
NationalEmployCovidCs$`Government`<-NationalEmployCovidCs$USGOVT-NationalEmployCovidCs$USGOVT[1]

NationalEmployRecess<-rbind(NationalEmployFinCrisis,NationalEmployCovidCs)

NationalEmployRecess<- NationalEmployRecess[,c(1,10:18)] %>%
  gather(Industry,Employment,Construction:Government)

NationalEmployRecess$DATE<-as.character(NationalEmployRecess$DATE)

NationalEmployRecessCovid<-NationalEmployRecess[which(NationalEmployRecess$Period=='Covid'),]

NationalEmployRecessCovid$Industry<-factor(NationalEmployRecessCovid$Industry,
                                      levels = c("Construction","Manufacturing","Trade, transportation, and utilities",
                                                 "Financial activities","Professional and business services",
                                                 "Leisure and hospitality","Education and health services",
                                                 "Government"))






StateUnemploy<-merge(TotEmployed,StateAbbr[,c(1,3)],by.x = c('State and area'),by.y = c('State'))[,c(1,4,11:12)]
StateUnemploy$Month<-ifelse(StateUnemploy$Month=='01','Jan',ifelse(StateUnemploy$Month=='02','Feb',ifelse(StateUnemploy$Month=='03','Mar',
                                                                                        ifelse(StateUnemploy$Month=='04','Apr',
                                                                                               ifelse(StateUnemploy$Month=='05','May',
                                                                                                      ifelse(StateUnemploy$Month=='06','Jun','Jul'))))))


StateUnemploy$Month<-factor(StateUnemploy$Month,s = c('Jan','Feb','Mar','Apr','May','Jun','Jul'))
StateUnemploy$`Unemployment Rate`<-c(StateUnemploy$`Unemployment Rate`/100)


Joblessclaim$`Filed week ended`<-as.Date(Joblessclaim$`Filed week ended`,"%m/%d/%Y")
Joblessclaim$`Initial Claims`<-c(Joblessclaim$`Initial Claims`/1000)


write.csv(TotEmployed,'StateTotEmploy.csv')
write.csv(StateUnemploy,"StateUnemploy.csv")
write.csv(Joblessclaim,"StateJobless.csv")
write.csv(DT,"IndustryJobChange.csv")
write.csv(NationalEmployRecess,"IndustryJobRecess.csv")


