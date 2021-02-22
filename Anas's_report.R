library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)
library(forcats)
library(sp)
library(rworldmap)
library(reshape2)


fao <- read.csv("FAO.csv")

str(fao)

# to figure out the continent from lat,long:
points = data.frame(lon=fao$longitude, lat=fao$latitude)

# The single argument to this function, points, is a data.frame in which:
#   - column 1 contains the longitude in degrees
#   - column 2 contains the latitude in degrees
coords2continent = function(points)
{  
    countriesSP <- getMap(resolution='low')
    #countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail
    
    # converting points to a SpatialPoints object
    # setting CRS directly to that from rworldmap
    pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
    
    
    # use 'over' to get indices of the Polygons object containing each point 
    indices = over(pointsSP, countriesSP)
    
    #indices$continent   # returns the continent (6 continent model)
    indices$REGION   # returns the continent (7 continent model)
    #indices$ADMIN  #returns country name
    #indices$ISO3 # returns the ISO3 code 
}
cont_fao <- coords2continent(points)
str(cont_fao)

# adding the continent column:
fao <- fao %>% add_column(continent = cont_fao) 

# selecting the important variables:
fao_sel <- fao[,-c(2,4,6,8,9,10)]

# assigning NA continents to correct continents
fao <- fao %>% 
  mutate(continent   = case_when( Area == "Bermuda"        ~ "North America",
                                  Area == "Cabo Verde"        ~ "Africa",
                                  Area == "China, Macao SAR"        ~ "Asia",
                                  Area == "Grenada"        ~ "North America",
                                  Area == "Kiribati"        ~ "Australia",
                                  Area == "Maldives"        ~ "Asia",
                                  Area == "New Caledonia"        ~ "Australia",
                                  Area == "New Zealand"        ~ "Australia",
                                  Area == "Philippines"        ~ "Asia",
                                  Area == "Maldives"        ~ "Asia",
                                  Area == "Saint Vincent and the Grenadines"        ~ "North America",
                                  Area == "Samoa"        ~ "Australia",
                                  Area == "Sweden"        ~ "Europe",
                                  TRUE ~ as.character(continent)))

#Nora's ANOVAS

long_data <- fao %>% gather(Year, Production, Y1961:Y2013)
glimpse(long_data)

#Lets see for the first year 1961: 
#we see that lowest food production continent was North America and highest Europe 
long_data %>%  
  filter(!is.na(continent)) %>%  #I filtered out the NAs (which were 1412)
  filter(Year == "Y1961") %>%  
  group_by(continent) %>% 
  count() %>% 
  arrange(n)

long_1961 <- long_data %>%  
  filter(!is.na(continent)) %>%  
  filter(Year == "Y1961")

one.way_1961 <- aov(Production ~ continent, data = long_1961)
summary(one.way_1961)

#Lets compare this to the most recent year: Have the differences in production between continents 
#become bigger? 

long_2013 <- long_data %>% 
  filter(!is.na(continent)) %>%  
  filter(Year == "Y2013") 

one.way_2013 <- aov(Production ~ continent, data = long_2013)
summary(one.way_2013)

#For 2013 we have a smaller F-Value --> The larger the F value, the more likely it is that
# the variation caused by the independent variable is real and not due to chance
# So: It has gotten more likely that the variation is real


### MORITZ
### Categorizing Vegan / Vegetarian / Carnivorous

# distinct list of item code and item:
item_codes <- fao %>% 
    group_by(Item, Item.Code) %>% 
    tally()
#write.csv(diet_group,"\\diet_groups.csv", row.names = TRUE)

# group food into vegetarian/meat by item code
code_list_vegan         <-  c(2511:2680, 2775, 2805, 2905:2924)
code_list_vegetarian    <-  c(2740:2745, 2848, 2949)
code_list_carnivorous   <-  c(2731:2737, 2761:2769, 2781, 2782, 2928:2948, 2960, 2961)

# I think we should use Animal/Non-Animal classification to gein the best fit with the Feed/Food division of the data but this could be up for discussion.
grouped_fao <- fao %>% 
    mutate(diet        = case_when( Item.Code %in% code_list_vegan        ~ "Vegan",
                                    Item.Code %in% code_list_vegetarian   ~ "Vegetarian",
                                    Item.Code %in% code_list_carnivorous  ~ "Carnivourous"))  %>%
    mutate(diet_meat   = case_when( Item.Code %in% code_list_vegan        ~ "Non-Meat",
                                    Item.Code %in% code_list_vegetarian   ~ "Non-Meat",
                                    Item.Code %in% code_list_carnivorous  ~ "Meat"))          %>%
    mutate(diet_animal = case_when( Item.Code %in% code_list_vegan        ~ "Non-Animal",
                                    Item.Code %in% code_list_vegetarian   ~ "Animal",
                                    Item.Code %in% code_list_carnivorous  ~ "Animal"))        %>%
    mutate(flag_meat   = case_when( Item.Code %in% code_list_vegan        ~ FALSE,
                                    Item.Code %in% code_list_vegetarian   ~ FALSE,
                                    Item.Code %in% code_list_carnivorous  ~ TRUE))            %>%
    mutate(flag_animal = case_when( Item.Code %in% code_list_vegan        ~ FALSE,
                                    Item.Code %in% code_list_vegetarian   ~ TRUE,
                                    Item.Code %in% code_list_carnivorous  ~ TRUE))           


diet_group <- grouped_fao %>% 
    group_by(diet) %>% 
    tally()



### ANAS

# to see how many missing values there are in each year:
fao_sel %>% 
    gather(Year, Production, Y1961:Y2013) %>% 
    filter(is.na(Production)) %>% 
    group_by(Year)%>% tally()%>%
    ggplot(aes(Year,n))+
    geom_col(fill = "lightgreen")+
    theme(axis.text.x = element_text(size=6, angle=90),
          panel.background = element_blank())+
    geom_text(aes(label = sprintf("%d", n)), hjust = 0.5,
              vjust = -0.5, size = 3, check_overlap = TRUE)+
    ylab("Number of missing fields")

# the trend for total feed and food production over the years
fao_sel %>% 
    gather(Year, Production, Y1961:Y2013, na.rm = TRUE) %>% 
    group_by(Element,Year)%>% # to see production development for feed and food per year
    summarise(Production = sum(Production)/1000)%>% # to get the values in million tonnes
    ggplot(aes(Year,Production,group = Element))+  
    geom_col(aes(fill = Element),position = "dodge") + # bars of food and feed next to each other
    theme_bw()+theme(axis.text.x = element_text(size=6, angle=90),
                     plot.title = element_text(hjust = 0.5),
                     panel.grid = element_blank(),legend.title = element_blank())+
    ylab( "Production (Million Tonnes)")+
    ggtitle ( "Total Food/Feed production 1961 - 2013")

# to see the largest feed producer, top 6 countries:
fao_gath <- fao_sel %>% 
    gather(Year, Production, Y1961:Y2013, na.rm = TRUE) %>% 
    group_by(Area,Element)

topFeed <- fao_gath%>%
    filter(Element == "Feed")  %>% # selecting the feed only
    summarise(totalprod = sum(Production)/1000)%>% # getting the total production for feed
    ungroup()%>% 
    top_n(6,totalprod)

# show the top 6 countries for feed production in descending order
topFeed %>% select(Area,totalprod) %>% arrange(desc(totalprod))

# plotting the top 6 feed producing countries across the years
topFeedplot <-fao_sel%>% 
    gather(Year, Production, Y1961:Y2013, na.rm = TRUE) %>% 
    group_by(Area,Year,Element) %>%
    filter(Element == "Feed" & Area %in% topFeed$Area) %>%
    summarise(totalprod = sum(Production)/1000)

topFeedplot %>% 
    ggplot(aes(Year,totalprod,group = Area, color = Area))+  
    geom_line(size = 1) +
    ylab( "Production (Millon Tonnes)")+
    ggtitle ( "Top  6 Feed Producers 1961 - 2013")+
    theme_bw()+theme(axis.text.x = element_text(size=6, angle=90),
                     plot.title = element_text(hjust = 0.5),
                     panel.grid = element_blank(),
                     legend.title = element_blank()) 

# to see the largest food producer, top 6 countries: 
topFood <- fao_gath%>%
    filter(Element == "Food")  %>% # selecting the food only
    summarise(totalprod = sum(Production)/1000)%>% # getting the total production for food
    ungroup()%>% 
    top_n(6,totalprod)

# show the top 6 countries for food production in descending order
topFood %>% select(Area,totalprod) %>% arrange(desc(totalprod))

# plotting the top 6 food producing countries across the years
topFoodplot <-fao_sel%>% 
    gather(Year, Production, Y1961:Y2013, na.rm = TRUE) %>% 
    group_by(Area,Year,Element) %>%
    filter(Element == "Food" & Area %in% topFeed$Area) %>%
    summarise(totalprod = sum(Production)/1000)

topFoodplot %>% 
    ggplot(aes(Year,totalprod,group = Area, color = Area))+  
    geom_line(size = 1) +
    ylab( "Production (Millon Tonnes)")+
    ggtitle ( "Top  6 Food Producers 1961 - 2013")+
    theme_bw()+theme(axis.text.x = element_text(size=6, angle=90),
                     plot.title = element_text(hjust = 0.5),
                     panel.grid = element_blank(),
                     legend.title = element_blank())

# Forecasting
fao_reduced <- fao[fao$Element == 'Food', ]
glimpse(fao_reduced)
long_data <- fao_reduced %>% gather(Year, Production, Y1961:Y2013)
glimpse(long_data)
forecast_data <- long_data %>% 
  group_by(Year) %>% 
  summarise(sum_production = sum(Production)/1000)

forecast_data_ts <- ts(forecast_data$sum_production, start = 1961, end = 2013, frequency = 1)
glimpse(forecast_data_1)


library(forecast)
# Automated forecasting using an exponential model
fit <-  forecast(forecast_data_ts, h = 37, level = c(80,95)) # h - number of periods for forecasting
#level - confidence level for prediction intervals.
summary(fit)
plot(forecast(forecast_data_ts, h = 37), main = 'Forecast of the Food production by 2050', xlab = 'Year', ylab = 'Food priduction, million tonnes')
