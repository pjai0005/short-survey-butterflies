
library(readr)
library(dplyr)
library(ggplot2)
library(leaflet)
library(ggmap)
library(shiny)
library(RColorBrewer)
library(lubridate)
library(tidyr)
library(tidyverse)
library(plotly)
library(ggthemes)

data <- read_csv("data/Butterfly_biodiversity_survey_2017_PE2.csv")%>%
  separate(col=Datetime,
           into=c('date', 'time', 'hr'),
           sep=' ') %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"))

pe2a <- data %>%
  group_by(Site)%>%
  summarise(ButterflyCount = sum(ButterflyCount, na.rm= TRUE))%>%
  arrange(desc(ButterflyCount))%>%
  head(5)

pe2b <- data  %>%
  group_by (Site, date) %>%
  summarise (ButterflyCount = sum (ButterflyCount, na.rm= TRUE)) %>%
  arrange(desc(Site)) %>%
  filter(Site %in% pe2a$Site)

map_data <- data %>% 
  group_by(Site)%>%
  summarise(ButterflyCount = sum(ButterflyCount),
            Lon = mean(Lon),
            Lat = mean(Lat)) %>%
  ungroup()

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel(h1("SURVEY OF BUTTERFLIES IN MELBOURNE 2017", 
                align = "center", 
                style = "font-size: 30px;")),
  fluidRow(
    column(width=12,
           wellPanel("The data for this report comes from an Our City's Little Gems study that was carried out in Melbourne between January and March 2017 and looked at butterfly biodiversity and flower-butterfly interactions. The researchers in the original study noted what butterflies they saw in different parts of Melbourne. A single study site had several plants and locations, and the researchers visited these sites multiple times during the study. This helped them to investigate which varieties of butterflies they could find, during what time of the day, and under what meteorological circumstances. An altered portion of the original data is used for the study."))),
  
  fluidRow(column(width=3,
                  tags$h2(strong("Location of the survey",
                                 style = "font-size:18px;")),
                  "The map here shows all the 15 sites taken into the investigation of the number of butterflies in Melbourne for 2017. The locations are highlighted in blue circles along with the count of butterflies. The slider above the graph helps for a better study as we can alter the count of butterflies, and the map modifies the highlighted area accordingly. "),
           
           column(width = 9,
                  fluidRow(
                    sliderInput("r", "Butterfly Count Range:",
                                width = "100%",
                                min = min(map_data$ButterflyCount),
                                max = max(map_data$ButterflyCount),
                                value = c(min(map_data$ButterflyCount),
                                          max(map_data$ButterflyCount)))),
                  fluidRow(leafletOutput("mymap"))),
           
           column(width = 12,
                  tags$hr())),
  
  fluidRow(
    column(width = 5,
           plotOutput("viz1")),
    
    column(width = 6,
           tags$h2(strong("Top Sites for Butterflies"
                          , style = "font-size:18px;")),
           "The left-hand side visualisation shows the top 5 sites which have the maximum number of butterflies among the 15 locations given in the data. The bar graph exhibits that Royal Park and Womens Peace Gardens have the maximum number of butterflies while Westgate Park ranks the lowest within the group with just 27 butterflies during the tenure. The second graph displays the total number of butterflies observed each day at the same 5 sites taken above. From the line graph, we can notice that the line is not continuous for the sites as there were no butterflies observed for each day for the given sites. Here, Womens Peace Gardens is easily distinguishable as it experiences a sudden jump in count just after a day which is from 0 to 30 butterflies. Moreover, site Royal Park encounters more fluctuation in count throughout the period and has the maximum number of observant days.",
           
           style = "font-size:15px; text-align:justify"),
    
    column(width = 1)
    
    # column(width = 5,
    #        plotlyOutput("viz2"))
  ),
  
  fluidRow(
    column(width = 12),
    wellPanel("Created with RShiny", 
              align = "center", 
              style = "font-size: 12px;")
    )
  
    
    )


# Define server logic
server <- function(input, output, session) {
  
  output$mymap <- renderLeaflet({
    
    range_data <- map_data %>% 
      filter(ButterflyCount >= input$r[1] & ButterflyCount <= input$r[2])
    
    
    leaflet(range_data) %>% 
      addTiles() %>%
      addCircleMarkers(~Lon, ~Lat,
                       popup = ~Site,
                       radius = ~(ButterflyCount/4),
                       label = ~ButterflyCount,
                       labelOptions = labelOptions(permanent = TRUE)
      )
  })
  
  
  output$viz1 <- renderPlot({
    
    
    ggplot(pe2a, aes(x= ButterflyCount ,
                     y =  Site))+
      geom_bar(stat="identity",
               position="dodge",
               aes(fill=Site))+
      theme_bw()+
      scale_fill_brewer(palette = 'Set2')+
      labs(x = "Butterfly Count",
           y = " ",
           title = "Total number of butterflies observed",
           subtitle = "in the top 5 sites in 2017")+
      theme(legend.position = "none")+
      geom_text(aes(label = ButterflyCount),
                vjust = 0)
    
  })
  
  
  
  # output$viz2 <- renderPlotly({
  #   
  #   
  #   plot <- ggplot(data = pe2b, aes(x= date ,
  #                                   y =  ButterflyCount,
  #                                   fill = Site))+
  #     geom_line(size = 1.2)+
  #     geom_point(size = 1.6)+
  #     #geom_col()+
  #     theme_bw()+
  #     scale_fill_brewer(palette = 'Set2')+
  #     labs(x = " ",
  #          y = "Butterfly Count",
  #          title = "Number of Butterflies observed",
  #          subtitle = "each day in the top 5 sites from January'17 to March'17")
  #   ggplotly(plot)+
  #     #facet_wrap(vars(Site))+
  #     theme(legend.position = "none")
  #   
  # })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
