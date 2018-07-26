library(dplyr)
library(plotly)#interactive chart
library(countrycode)
library(highcharter)
library(DT)
library(leaflet)
library(ggplot2)
library(shiny)


# Manipulate data----------------------------
starbucks <- read.csv('directory.csv')

## Map1 (highcharter)------------------------
#### Get the number of stores in each country
count_country <- starbucks %>% group_by(Country) %>% 
  dplyr::summarise(Counts = n()) %>% arrange(desc(Counts))
names(count_country) <- c("country.code", "total")

#### Get long country name and region name
count_country$Country_name <- countrycode(count_country$country.code, "iso2c", "country.name")
count_country$Region_name <- countrycode(count_country$country.code, "iso2c", "region")

#### Count the percentage
percent <- round(count_country$total/sum(count_country$total)*100, 2)
count_country <- transform(count_country, Percent=paste0(percent, '%'))

data(worldgeojson, package = 'highcharter')
count_country$iso2 <- as.character(count_country$country.code)

# MAP2 (TOP5)------------------------
data(worldgeojson, package = 'highcharter')


# World wide distribution

us <- starbucks[starbucks$Country == 'US', ]
cn <- starbucks[starbucks$Country == 'CN', ]
ca <- starbucks[starbucks$Country == 'CA', ]
jp <- starbucks[starbucks$Country == 'JP', ]
kr <- starbucks[starbucks$Country == 'KR', ]
sta_others <- starbucks[!starbucks$Country %in% c('US', 'CN', 'CA', 'JP', 'KR'), ]


# top 5 countries---------
top5 <- data.frame(x=c(-95.712891, 104.195397, -106.346771, 138.252924, 127.766922), 
                   y=c(37.09024, 35.86166, 56.130366, 36.204824, 35.907757), 
                   id=c('US', 'CN', 'CA', 'JP', 'KR'))

# function count selected country
topcountry <- function(country = 'CA'){
  table <- starbucks[starbucks$Country %in% c(country), ]
  table2 <- table %>% group_by(Country, State.Province) %>% 
    dplyr::summarise(Counts = n()) %>% arrange(desc(Counts))
  return(table2)
}
## Ownership DataTable---------------------------
ownership <- starbucks %>% group_by(Ownership.Type) %>% 
  dplyr::summarise(Total = n(), Percentage = Total/dim(starbucks)[1] * 100) %>% 
  arrange(desc(Total))

# Shiny App---------------------------
server <- function(input, output) {

## MAP 1(Highchart)------------------------
  
  output$MAP1 <- renderHighchart({
    highchart() %>%
      hc_title(text = "Starbucks Stores - World Wide") %>%
      hc_add_series_map(worldgeojson, count_country, name = "Number of Stores: ", 
                        value = "total", joinBy = 'iso2') %>% 
      hc_colorAxis(stops = color_stops()) %>% hc_legend(enabled = TRUE) %>%
      hc_mapNavigation(enabled = TRUE) 
  })

  
  
## MAP 2(Leaflet)----------------------
  
  # create a reactive value that will store the click position
  data_of_click <- reactiveValues(clickedMarker=NULL)
  
  # Leaflet map with 2 markers
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles(options = providerTileOptions(noWrap = TRUE)) %>% 
      addCircles(lat= us$Latitude, lng = us$Longitude, 
                 color = "#E69F00", weight = 0.6)%>% 
      
      addCircles(lat= cn$Latitude, lng = cn$Longitude, 
                 color = "#0072B2", weight = 0.6) %>% 
      
      addCircles(lat= ca$Latitude, lng = ca$Longitude, 
                 color = "#009E73", weight = 0.6) %>% 
      
      addCircles(lat= jp$Latitude, lng = jp$Longitude, 
                 color = "#F0E442", weight = 0.6) %>%
      
      addCircles(lat= kr$Latitude, lng = kr$Longitude, 
                 color = "#CC79A7", weight = 0.6) %>%
      
      addCircles(lat= sta_others$Latitude, lng = sta_others$Longitude, 
                 color = "gray", weight = 0.3) %>% 
      addCircleMarkers(data=top5, ~x , ~y, layerId=~id, popup=~id, radius=8 , 
                       color="black",  fillColor="red", stroke = TRUE, fillOpacity = 0.8)
  })
  
  # store the click
  observeEvent(input$map_marker_click,{
    data_of_click$clickedMarker <- input$map_marker_click
  })
  
  # Make a barplot or scatterplot depending of the selected point
  output$plot=DT::renderDataTable({
    my_place=data_of_click$clickedMarker$id
    print(my_place)
    print(1)
    if(is.null(my_place)){my_place="US"}
    print(my_place)
    print(2)
    selectedCountry<-topcountry(my_place)
    DT::datatable(selectedCountry)
  })
  
## Top Countries Bar------------------
  output$topCountry <- renderPlotly({
    count_country$country.code <- factor(count_country$country.code, 
                                         levels = count_country$country.code) #arrange the country
    
    plot_ly(data = count_country[1:input$n,], type = 'bar',
            hoverinfo = 'text', x = ~country.code, y = ~total,
            text = ~paste(country.code, 'counts :', total,
                          'percent: ', Percent))
  })
## Top Countries DataTable------------
  output$topCountryBar <- DT::renderDataTable({
    DT::datatable(count_country[1:input$n,c('Region_name','Country_name', 'Percent', 'total')])
  })
  
## Ownership DataTable--------------
  output$ownership_dt <- DT::renderDataTable({
    DT::datatable(ownership)
  })
  
## Ownership Bar------------------
  output$ownership_bar <- renderPlotly({
    ownership$Ownership.Type <- factor(ownership$Ownership.Type, 
                                       levels = ownership$Ownership.Type) 
    
    plot_ly(data = ownership, type = 'bar',
            hoverinfo = 'text', x = ~Ownership.Type, y = ~Total,
            text = ~paste(Ownership.Type, 'Percentage :', round(Percentage, 2), '%'))
  })
}


