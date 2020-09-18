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


