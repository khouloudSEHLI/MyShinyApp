library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyverse)
library(DT)
library(plotly)

# install.packages("installr")
# library(installr)
# updateR()

# install.packages("utf8")
library("utf8")
terrorbase=read_csv("C:\\Users\\user\\Desktop\\Terrorism\\globalterorism.csv")


terrorcol= terrorbase %>%
  select (iyear  , imonth, iday,  country_txt, region_txt,city,
          latitude, longitude, attacktype1_txt,multiple, nkill,
          nwound, target1, gname, targtype1_txt,weaptype1_txt,
          motive,   summary) 
terrordata=terrorcol %>% 
  rename(Year= iyear,Month=imonth, Day=iday,
         Country=country_txt, Region=region_txt,
         AttackType=attacktype1_txt,  Killed=nkill , Wounded=nwound, Target=target1,
         Group=gname,Target_type= targtype1_txt, Weapon_type=weaptype1_txt, Motive=motive ,
         Summary=summary
         
  )

# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  dashboardHeader(title= "Terrorist Activities"),
  
  dashboardSidebar(
    
    sidebarMenu(
      menuItem("Presentation", tabName = "present", icon=icon("bomb")),
      menuItem("Data",tabName ="datapage", icon = icon("database")  ),
      menuItem("Some basic analysis", tabName="basicanal", icon = icon("dashboard")),
      menuItem("terrorism categories", tabName = "tercat", icon = icon("chart-pie"))
    )
    
  ),
  dashboardBody(
    
    
    tabItems(
      tabItem(tabName = "present", 
              fluidPage(
                h1("Terrorist Activities Around The World "),
                p("Terrorism is not a 21st century phenomenon and has its roots 
                  in early resistance and political movements. The Sicarii were an early Jewish
                  terrorist organisation founded in the first century AD with the goal of
                  overthrowing the Romans in the Middle East. Judas of Galilee, leader of
                  the Zealots and a key influence on the Sicarii, believed that the Jews should
                  be ruled by God alone and that armed resistance was necessary. "),
                
                
                
                br(),
                br(),
                h3("Modern terrorism after the second world war"),
                p("The use of terrorism to further a political cause has accelerated in recent years. 
                  Modern terrorism largely came into being after the Second World War with the rise of
                  nationalist movements in the old empires of the European powers."),
                
                h3("Terrorist Activities Dashboard"),
                p(" This dashboard gives a brief insight of terrorist attacks globally, by country and by region. 
                  The database used for that is the data -Terrorism around the world- that you can find in Kaggle."),
                br()
                
                
                
                
                
                )
              
                ),
      tabItem(tabName = "datapage",
              DT::dataTableOutput("database")
              
      ),
      tabItem(tabName = "basicanal",
              fluidPage(
                box(width=8,title = "Terrorist attacks by year ", status = "danger", solidHeader = TRUE, plotlyOutput("yearattacks")),
                box(width = 4, "These countries have recorded the highest Terrorist attacks.  ") ,
                box(width=12,title = "Countries with Highest Terrorist Attacks ", status = "danger", solidHeader = TRUE, plotlyOutput("countattacks"))
              )
      ),
      
      tabItem(tabName = "tercat",
              fluidPage(
                box(width = 3, "The attacks can be different however with our data we can see that most of the attacks are done throught bombs or explosions. 
                    Armed assault attacks come in the second place. The third and fourth place are taken by respectively assassination and hostage taking.
                    Facility and infrastructure attacks are also important since the world has witnessed more than 10,000 terrorist attacks.") ,
                box(width=9  ,title = "Terrorist attacks by Attack type ", status = "danger", solidHeader = TRUE, plotlyOutput("attackstype")),
                box(width=12  ,title = "Favorite terrorist targets ", status = "danger", solidHeader = TRUE, plotlyOutput("targetattacks")),
                
                box(width=7  ,title = "Terrorist attacks by weapon ", status = "danger", solidHeader = TRUE, plotlyOutput("weapattacks")),
                box(width =5, "Half of the terrorist attacks are done using explosives. Up to 32% were done using firearms.  ")
                
                
                
                )
              )
      
              )
    
    
    
    )
  
  
  
  
  
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  
  output$database <- DT::renderDataTable({terrordata} ,extensions="Responsive",
                                         options=list(pageLength=10, lengthChange=TRUE, autoWidth=TRUE, scrollX = TRUE))
  
  
  
  output$yearattacks= renderPlotly({
    ll=  terrordata%>% 
      group_by(Year) %>%
      summarise(nyears=n())%>%
      collect
    
    
    plot_ly(ll, x = ~Year, y = ~nyears, type= 'scatter', mode='lines+markers' ,
            marker = list(color = c('rgba(222,45,38,0.8)'
                                    
            ))) %>%  
      
      layout( title='', xaxis = list(title = 'Years'), yaxis =list(title= 'Number of attacks') )
    
  })
  
  
  
  output$countattacks=renderPlotly({
    ss= terrordata %>%
      group_by(Country) %>%
      summarise(numberattacks=n()) %>%
      collect
    
    plot_ly(ss, x = ~Country, y = ~numberattacks,  type = 'bar' ,
            marker = list(color = c('rgba(222,45,38,0.8)'
                                    
            ))) %>%
      layout(title = "", autosize = T,
             xaxis = list(title = "Country"),
             yaxis = list(title = "Number of terrorist attacks"))
    
    
  })
  
  output$attackstype=renderPlotly({
    ss= terrordata %>%
      group_by(AttackType) %>%
      summarise(numberattacks=n()) %>%
      arrange(desc(numberattacks)) %>%
      collect
    
    plot_ly(ss, x = ~AttackType, y = ~numberattacks,  type = 'bar' ,
            marker = list(color ="cadetblue") ) %>%
      
      
      layout(title = "", autosize = T,
             xaxis = list(title = "Attack type"),
             yaxis = list(title = "Number of terrorist attacks"))
  })
  
  
  output$weapattacks=renderPlotly({
    ss= terrordata %>%
      group_by(Weapon_type) %>%
      summarise(numberattacks=n()) %>%
      collect
    
    plot_ly(ss, labels = ~Weapon_type, values = ~numberattacks,  type = 'pie' ,
            
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'text',
            text = ~paste( Weapon_type ,numberattacks ),
            
            marker = list(colors = colors,
                          line = list(color = '#FFFFFF', width = 1)), showlegend = FALSE ) %>%
      
      
      layout(title = "", autosize = T,
             xaxis = list(title = "Attack type"),
             yaxis = list(title = "Number of terrorist attacks"))
  })
  
  output$targetattacks=renderPlotly({
    ss= terrordata %>%
      group_by(Target_type) %>%
      summarise(numberattacks=n()) %>%
      arrange(desc(numberattacks)) %>%
      collect
    
    plot_ly(ss, x = ~numberattacks, y = ~Target_type,  type = 'bar'  , orientation = 'h',
            marker = list(color = "purple"  ) ) %>%
      
      
      layout(title = "", autosize = T,
             xaxis = list(title = "Target type"),
             yaxis = list(title = "Number of terrorist attacks"))
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

