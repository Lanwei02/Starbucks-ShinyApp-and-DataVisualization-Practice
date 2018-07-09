library(dplyr)
library(plotly)#interactive chart
library(countrycode)
library(highcharter)
library(data.table)
library(leaflet)
library(ggplot2)


starbucks <- read.csv('directory.csv')

str(starbucks)

summary(starbucks)

names(starbucks)

# Get the number of stores in each country
count_country <- starbucks %>% group_by(Country) %>% 
  dplyr::summarise(Counts = n()) %>% arrange(desc(Counts))

names(count_country) <- c("country.code", "total")

## Get long country name and region name
count_country$Country_name <- countrycode(count_country$country.code, "iso2c", "country.name")
count_country$Region_name <- countrycode(count_country$country.code, "iso2c", "region")

# Count the percentage
percent <- round(count_country$total/sum(count_country$total)*100, 2)
count_country <- transform(count_country, Percent=paste0(percent, '%'))

# Visualize TOP 20 countries 
count_country$country.code <- factor(count_country$country.code, 
                                     levels = count_country$country.code) #arrange the country

plot_ly(data = count_country[1:20,], type = 'bar',
        hoverinfo = 'text', x = ~country.code, y = ~total,
        text = ~paste(country.code, 'counts :', total,
                      'percent: ', Percent))

# Visualize with world map
data(worldgeojson, package = 'highcharter')

#### sapply(count_country, class)

count_country$iso2 <- as.character(count_country$country.code)

highchart() %>%
  hc_title(text = "Starbucks Stores - World Wide") %>%
  hc_add_series_map(worldgeojson, count_country, name = "Number of Stores: ", 
                    value = "total", joinBy = 'iso2') %>% 
  hc_colorAxis(stops = color_stops()) %>% hc_legend(enabled = TRUE) %>%
  hc_mapNavigation(enabled = TRUE) 

# World wide distribution

us <- starbucks[starbucks$Country == 'US', ]
cn <- starbucks[starbucks$Country == 'CN', ]
ca <- starbucks[starbucks$Country == 'CA', ]
jp <- starbucks[starbucks$Country == 'JP', ]
gb <- starbucks[starbucks$Country == 'GB', ]

sta_others <- starbucks[!starbucks$Country %in% c('US', 'CN', 'CA', 'JP', 'GB'), ]

leaflet() %>% 
  addTiles() %>% 
  addCircles(lat= us$Latitude, lng = us$Longitude, 
             color = "#E69F00", weight = 1)%>% 
  
  addCircles(lat= cn$Latitude, lng = cn$Longitude, 
             color = "#0072B2", weight = 1) %>% 
  
  addCircles(lat= ca$Latitude, lng = ca$Longitude, 
             color = "#009E73", weight = 1) %>% 
  
  addCircles(lat= jp$Latitude, lng = jp$Longitude, 
             color = "#F0E442", weight = 1) %>%
  
  addCircles(lat= gb$Latitude, lng = gb$Longitude, 
             color = "#CC79A7", weight = 1) %>%
  
  addCircles(lat= sta_others$Latitude, lng = sta_others$Longitude, 
             color = "red", weight = 1)

# Get the number of stores in each city
count_city <- starbucks %>% group_by(City) %>% 
  dplyr::summarise(Counts = n()) %>% arrange(desc(Counts))

ggplot(count_city[1:20, ], 
       aes(x = reorder(City, Counts), y = Counts, fill = City)) +
  geom_bar(stat = 'identity') +
  labs(x = 'City', y = 'Counts') +
  theme_minimal() +
  guides(fill = 'none') +
  coord_flip()


# Ownership Types
ownership <- starbucks %>% group_by(Ownership.Type) %>% 
  dplyr::summarise(Total = n(), Percentage = Total/dim(starbucks)[1] * 100) %>% 
  arrange(desc(Total))

datatable(ownership)

## Visualize by treemap
hchart(ownership, "treemap", hcaes(x = Ownership.Type, value = Total, color = Total )) %>%
  hc_title(text = "Starbuck stores Ownership Tyoes") %>%
  hc_legend(enabled = TRUE)


