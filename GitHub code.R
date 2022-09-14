### couDensity plots 

# cancer
bin <- hexbin(allcountsum_data$Total, allcountsum_data$Cancer, xbins= 50)
plot(bin, colramp=topo.colors, colorcut=seq(0,1,0.1), xlab = "Percentgage of Food Insecure People in County", ylab = "Crude Prevelance of Cancer", main = "Rate of Cancer Prevelance based on Food Insecure Population in County", trans = log)() 

# Arthritis 
bin <- hexbin(allcountsum_data$Total, allcountsum_data$Arthritis, xbins= 50)
plot(bin, colramp=topo.colors, colorcut=seq(0,1,0.1), xlab = "Percentgage of Food Insecure People in County", ylab = "Crude Prevelance of Arthritis\n", main = "Rate of Arthritis Prevelance based on\n Food Insecure Population in County", trans = log, inv = exp) 

# asthma 
bin <- hexbin(allcountsum_data$Total, allcountsum_data$Asthma, xbins= 50)
plot(bin, colramp=topo.colors, colorcut=seq(0,1,0.1), xlab = "Percentgage of Food Insecure People in County", ylab = "Crude Prevelance of Asthma\n", main = "Rate of Asthma Prevelance based on\n Food Insecure Population in County", trans = log, inv = exp) 

Choropleth 

library(choroplethr)
library(choroplethrMaps)
library(leaflet)
install.packages("leaflet")
library(tigris)
library(sp)
library(sf)
library(rgeos)
library(rgdal)
library(maptools)
install.packages("textreadr")
library(textreadr)
​
​
dir() 
states <- read_sf("/Users/sarahcarroll/spice 2021/cb_2018_us_region_20m 2/cb_2018_us_region_20m.shp")
​
​
us_regions <- regions()
cnty <- counties(cb = TRUE)
county <- leaflet(cnty) %>% 
  addTiles() %>% 
  addPolygons(color = "#444444", 
              weight = 1,
              smoothFactor = 0.5) #%>%
addPolygons(data = us_regions)
​
usRegions <- leaflet(us_regions) %>% 
  addTiles() %>% 
  addPolygons(color = "#444444", 
              weight = 1,
              smoothFactor = 0.5) 
usRegions
​
# data from:
# https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.2010.html
counties <- st_read("/Users/sarahcarroll/spice 2021/gz_2010_us_050_00_500k/gz_2010_us_050_00_500k.shp")
# cnty <- st_read("students/gz_2010_us_050_00_500k (1)/gz_2010_us_050_00_500k.shp")
# ffi_data <- read.csv("students/fffi_data.csv")
df_fffi <- fffi_data[,c(2:4)]
colnames(df_fffi)[2] <- "STATE"
# map <- read_sf("students/2010_Census_Tracts/2010_Census_Tracts.shp")
fd_ins <- read.csv("allcountsum_data2.csv")
df_fffi <- fd_ins[,c(2:4)]
# convert state to fips code
library(cdlTools)
install.packages("cdlTools")
df_fffi$STATE <- fips(df_fffi$StateAbbr, to = "FIPS")
# df_fffi$STATE <- as.character(df_fffi$STATE)
# creatye unique id column
df_fffi$id <- paste(df_fffi$County,df_fffi$STATE, sep ="")
# make STATE variable an integer
counties$STATE <- as.integer(counties$STATE)
counties$id <- paste(counties$NAME, counties$STATE, sep ="")
# cnty$id <- paste(cnty$NAME, cnty$STATE, sep ="")
# merge dataset to shapefile
counties <- sp::merge(counties, df_fffi, by = "id")
# cnty <- sp::merge(cnty, df_fffi, by = "id")
# create color palette
pal <- colorBin("Blues", 
                bins = 4,
                domain = counties$Total)
​ 
foodInsecur <- read_html("/Users/sarahcarroll/spice 2021/foodInsecur.html")

foodInsecur <- leaflet(counties) %>% 
  addProviderTiles("CartoDB.VoyagerNoLabels") %>% 
  addPolygons(
    fillColor = ~pal(counties$Total),
    color = "#444444", 
    weight = 1,
    smoothFactor = 0.5,
    fillOpacity = 0.8
  ) %>% 
  addLegend("bottomleft",
            pal = pal,
            values = counties$Total,
            title = "Food Insecurites by County(2010)",
            opacity = 1)
foodInsecur
​
Cor plot 

mydata$v3 <- mydata$v5 <- NULL 

correlations <- allcountsum_data %>% 
  group_by(Total, Arthritis, Asthma, Cancer)

cortbl <- cor(allcountsum_data[sapply(allcountsum_data,is.numeric)]) 

cortbll <-  inner_join(cdc500, fffi_data, by = total, County) 
cortbl <- cortbl %>% 
  select(total, Arthritis, Asthma, Cancer, County)


cortbl <- data.frame(cortbl)  

corrplot(cor(cortbl))x
corrmatrix = df.corr(cortbl)

M <- cor(cor_table[, -1])
corrplot(M, method = "circle", col = colorRampPalette)

cor.test(allcountsum_data$Total, allcountsum_data$Arthritis) 
