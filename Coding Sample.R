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

#-----------R shiny------------------------------------------------------

library(shiny)
library(shinyWidgets)
suppressMessages(library(shinydashboard))
library(ggplot2)
suppressMessages(library(rsconnect))
library(ggplot2)
library(magrittr)
suppressMessages(library(tidyr))
library(leaflet)
library(readxl)
library(stringr)
suppressMessages(library(plotly))
suppressMessages(library(tigris))
library(grid)

StateTotEmployment<-as.data.frame(read.csv("StateTotEmploy.csv"))[,-c(1)]
StateUemployment<-as.data.frame(read.csv("StateUnemploy.csv"))[,-c(1)]
StateJobless<-as.data.frame(read.csv("StateJobless.csv"))[,-1]
IndustryJobChange<-as.data.frame(read.csv("IndustryJobChange.csv"))[,-1]
IndustryJobRecess<-as.data.frame(read.csv("IndustryJobRecess.csv"))[,-1]
State<-as.data.frame(read.csv('StateAbbr.csv'))[,c(1,3)]

DATE<-c('2008-01-01','2008-04-01',"2008-07-01","2008-10-01","2009-01-01","2009-04-01","2009-06-01")
DATECovid<-c("2020-03-01","2020-04-01","2020-05-01","2020-06-01","2020-07-01")



StateShape<-states(class='sf')

MapD<-merge(StateShape[,c(6,7,11:15)],StateTotEmployment[,c(2,4,11)],by.x = c('NAME'),by.y = c('State.and.area'),all.y = T)
MapD$Month<-ifelse(MapD$Month=='1','Jan',ifelse(MapD$Month=='2','Feb',ifelse(MapD$Month=='3','Mar',
                                                                             ifelse(MapD$Month=='4','Apr',
                                                                                    ifelse(MapD$Month=='5','May',
                                                                                           ifelse(MapD$Month=='6','Jun','Jul'))))))
colnames(MapD)[c(1,8)]<-c('Name','Rate')
MapD<-MapD[-which(MapD$Name %in% c('Los Angeles County','New York city')),]
DATE<-c('2008-01-01','2008-04-01',"2008-07-01","2008-10-01","2009-01-01","2009-04-01","2009-06-01")
DATECovid<-c("2020-03-01","2020-04-01","2020-05-01","2020-06-01","2020-07-01")

NationalUnemploy<-data.frame(
  Month=c('Jan','Feb','Mar','Apr','May','Jun',"Jul"),
  Rate=c(3.6,3.5,4.4,14.7,13.3,11.1,10.2)
)
#==============================================================================
ui<-fluidPage(
  fluidRow(column(3,sliderTextInput('Month','Select a Month',choices = c('Jan','Feb','Mar','Apr',
                                                                         'May','Jun','Jul'),grid = T,animate = animationOptions(interval = 6000))),
           column(5,fluidRow(br()),span(h3(strong('National Unemployment Rate: ')),h2(strong(textOutput('National')),style="color:navy")))),
  fluidRow(leafletOutput('map')),
  fluidRow(strong("click the state for details"),br(),br(),br()),
  fluidRow(column(2,selectInput('State','Add a State to compare',choices = c(State$State),selected = "",multiple = T)),
           column(5,plotOutput('StateURPlot')),
           column(5,plotOutput('StateJobPlot'))
  ),
  fluidRow(fluidRow(br(),br()),
           plotOutput('IndustryPlot',height = '800px')),
  fluidRow(br(),br(),
           column(8,h3(strong("National Job losses on Nonfarm Payroll, 2008 Financial Crisis VS Covid-19 Recession")))),
  fluidRow(column(2,selectInput('month','Select a Month',choices = DATE))),
  fluidRow(
    column(6,plotlyOutput('IndustryPieFin')),
    column(6,DT::dataTableOutput('IndustryRankFin',width = '50%'))
  ),
  
  fluidRow(column(2,selectInput('monthCovid',"Select a Month",choices = DATECovid ))),
  fluidRow(
    column(6,plotlyOutput('IndustryPieCovid')),
    column(6,DT::dataTableOutput('IndustryRankCovid',width = '50%'))
  )
  
)

server<-function(input,output,session) {
  
  observeEvent(input$map_shape_click$id,
               updateSelectInput(session,'State','Add a State to compare',
                                 choices = c(State$State),
                                 selected = State[which(State$Code==input$map_shape_click$id),]$State))
  
  MData<-reactive(
    MapD[which(MapD$Month==input$Month),]
  )
  
  
  StateUR<-reactive(
    StateUemployment
  )
  
  STJobless<-reactive(
    StateJobless
  )
  
  IndustryJob<-reactive(
    IndustryJobChange[which(IndustryJobChange$Code==input$map_shape_click$id),]
  )
  
  RecessionFin<-reactive(
    IndustryJobRecess[which(IndustryJobRecess$Period=='Financial'),]
  )
  RecessionCovid<-reactive(
    IndustryJobRecess[which(IndustryJobRecess$Period=='Covid'),]
  )
  
  output$map<-renderLeaflet(
    leaflet(MData())%>%
      addTiles()%>%
      setView(-95,38,zoom=4) %>%
      addPolygons(color = "black", weight = 0.4, opacity = 1,
                  smoothFactor = 0.5, fill = T, 
                  fillColor = ~colorBin(palette = 'GnBu',domain = MData()$Rate,bins = c(2,6,10,14,18,22,26,31))(Rate),
                  fillOpacity = 0.8,layerId = MData()$STUSPS,popup = paste0(MData()$Name,
                                                                            " Unemployment Rate:",MData()$Rate,"%"))%>%
      leaflet::addLegend(colors = c("#F0F9E8","#CCEBC5","#A8DDB5","#7BCCC4","#4EB3D3","#2B8CBE","#08589E"),
                         position = 'bottomright',labels = c('2-6%','6-10%','10-14%','14-18%',
                                                             '18-22%','22-26%','26-31%'),title = 'Unemployment Rate')
  )
  
  output$National<-renderText(
    paste0(NationalUnemploy[which(NationalUnemploy$Month==input$Month),]$Rate,'%')
  )
  
  output$StateURPlot<-renderPlot(
    ggplot(StateUR()[which(StateUR()$`State.and.area` %in% input$State),], 
           aes(x=Month,y=`Unemployment.Rate`,group=Code))+geom_line(aes(color=Code),size=1)+geom_point(aes(color=Code))+
      ggtitle('Monthly Unemployment Rate for States(2020)')+theme(plot.title = element_text(hjust = 0.5,face = 'bold'))+labs(color='State')
  )
  
  output$StateJobPlot<-renderPlot(
    ggplot(STJobless()[which(STJobless()$State %in% input$State),],
           aes(x=`Filed.week.ended`,y=Initial.Claims ,group=State))+geom_line(aes(color=State),size=1)+ggtitle('Jobless claim for States')+
      theme(axis.text.x = element_text(angle = 70,vjust = 0.5),plot.title = element_text(hjust = 0.5,face='bold'))+ylab('Initial Claims(In thousands)')+labs(fill="State")
  )
  
  output$IndustryPlot<-renderPlot(
    gridExtra::grid.arrange(ggplot(IndustryJob()[which(IndustryJob()$Month=='Mar'),],
                                   aes(x=str_wrap(Industry,15),y=Change,fill=Change>0))+geom_bar(stat ='identity')+ylab('Change in Employees on Payroll (In thousands)')+ggtitle('March')+xlab(NULL)+
                              coord_flip()+theme(legend.position = 'none',plot.title = element_text(hjust = 0.5,face = 'bold'))+scale_fill_manual(values = c('TRUE'='#00BFC4','FALSE'='#F8766D')),
                            ggplot(IndustryJob()[which(IndustryJob()$Month=='Apr'),],
                                   aes(x=str_wrap(Industry,15),y=Change,fill=Change>0))+geom_bar(stat = 'identity')+ylab('Change in Employees on Payroll (In thousands)')+ggtitle('April')+
                              coord_flip()+xlab(NULL)+theme(legend.position = 'none',plot.title = element_text(hjust = 0.5,face = 'bold'))+scale_fill_manual(values = c('TRUE'='#00BFC4','FALSE'='#F8766D')),
                            
                            ggplot(IndustryJob()[which(IndustryJob()$Month=='May'),],
                                   aes(x=str_wrap(Industry,15),y=Change,fill=Change>0))+geom_bar(stat = 'identity')+ylab('Change in Employees on Payroll (In thousands)')+ggtitle('May')+
                              coord_flip()+xlab(NULL)+theme(legend.position = 'none',plot.title = element_text(hjust = 0.5,face = 'bold'))+scale_fill_manual(values = c('TRUE'='#00BFC4','FALSE'='#F8766D')),
                            
                            ggplot(IndustryJob()[which(IndustryJob()$Month=='Jun'),],
                                   aes(x=str_wrap(Industry,15),y=Change,fill=Change>0))+geom_bar(stat = 'identity')+ylab('Change in Employees on Payroll (In thousands)')+ggtitle('June')+
                              coord_flip()+xlab(NULL)+theme(legend.position = 'none',plot.title = element_text(hjust = 0.5,face = 'bold'))+scale_fill_manual(values = c('TRUE'='#00BFC4','FALSE'='#F8766D')),
                            
                            ggplot(IndustryJob()[which(IndustryJob()$Month=='Jul'),],
                                   aes(x=str_wrap(Industry,15),y=Change,fill=Change>0))+geom_bar(stat = 'identity')+ylab('Change in Employees on Payroll (In thousands)')+ggtitle('July')+
                              coord_flip()+xlab(NULL)+theme(legend.position = 'none',plot.title = element_text(hjust = 0.5,face = 'bold'))+scale_fill_manual(values = c('TRUE'='#00BFC4','FALSE'='#F8766D')),
                            nrow=2, top = grid::textGrob(paste(State[which(State$Code==input$map_shape_click$id),]$`State and area`,
                                                               "\nMonthly Change in Nonfarm Payroll Employment"),gp=grid::gpar(fontsize=20,fontface='bold')))
  )
  
  output$IndustryPieFin<-renderPlotly({
    plot_ly(RecessionFin()[which(RecessionFin()$Employment<0 & RecessionFin()$DATE==input$month),],
            labels=~Industry,values=~abs(Employment),type = 'pie',sort=F)%>%
      layout(title = "Job Losses in Nonfarm Industries during Financial Crisis(In thousands)")}
  )
  
  output$IndustryRankFin<-DT::renderDataTable(
    DT::datatable(RecessionFin()[which(RecessionFin()$DATE==input$month),][,c(3,4)] %>%
                    dplyr::arrange(Employment),options = list(dom='tr'),class = 'cell-border stripe') %>%
      DT::formatStyle(columns = "Employment",color = DT::styleInterval(cuts = 0,values = c('red','green')))
    
  )
  
  output$IndustryPieCovid<-renderPlotly({
    plot_ly(RecessionCovid()[which(RecessionCovid()$Employment<0 & RecessionCovid()$DATE==input$monthCovid),],
            labels=~Industry, values=~abs(Employment),type = 'pie',sort=F) %>%
      layout(title = "Job Losses in Nonfarm Industries during Covid Recession(In thousands)")}
  )
  
  output$IndustryRankCovid<-DT::renderDataTable(
    DT::datatable(RecessionCovid()[which(RecessionCovid()$DATE==input$monthCovid),][,c(3,4)] %>%
                    dplyr::arrange(Employment),options = list(dom='tr'),class = 'cell-border stripe') %>%
      DT::formatStyle(columns = "Employment",color = DT::styleInterval(cuts = 0,values = c('red','green')))
    
  )
  
  
}

shinyApp(ui = ui, server = server)

