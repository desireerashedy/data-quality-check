library(quantreg)                        
library(graphics)
library(chron)
library(dplyr)
library(zoo)
library(plm)
library(lqmm)
library(tidyr)
library(data.table)
library(geosphere)
library(readxl)
library(skimr)
library(raster)
library(sp)
library(exactextractr)
library(sf)
library(cluster)
library(lubridate)
library(lmtest)
library(rqPen)
library(caret)
library(sandwich)
library(devtools)
library(rAHNextract)
library(terra)


########information of data sets on groundwater##

metadata_heads_02042024 <- read.csv("~/Documents/Documenten - MacBook Pro van Désirée/AAAMsc thesis Econometrics/Data + Code/Grondwater data/metadata_heads_02042024.csv")

# Filter the metadata by selecting the labels which have 
filtered_metadata <- metadata_heads_02042024 %>%
  group_by(label) %>%
  filter(screen_top == max(screen_top)) %>%
  ungroup()

filtered_metadata <- filtered_metadata[complete.cases(filtered_metadata[, c("x", "y")]), ]

rm(metadata_heads_02042024)
### groundwater levels

heads1 <- read.csv("~/Documents/Documenten - MacBook Pro van Désirée/AAAMsc thesis Econometrics/Data + Code/Grondwater data/heads/heads1.csv", header=TRUE)
#soms per uur, soms per dag
heads1$timestamp <- as.POSIXct(heads1$timestamp, format = "%Y-%m-%d %H:%M:%S")
time_component1 <- format(heads1$timestamp, format = "%H:%M:%S")
# Filter the rows where the time component is "00:00:00"
heads1 <- heads1[time_component1 == "00:00:00", ]
# Convert the filtered date-time column to Date format
heads1$timestamp <- as.Date(heads1$timestamp)
# Remove rows with all NA values
heads1 <- heads1[!apply(heads1[, -1], 1, function(row) all(is.na(row))), ]
# Create a new data frame with desired date range and time component 00:00:00
new_dates <- seq(min(heads1$timestamp), as.Date("2024-04-01"), by = "day")
new_timestamps <- paste(format(new_dates, "%Y-%m-%d"), "00:00:00", sep = " ")
new_heads1 <- data.frame(timestamp = new_timestamps)
new_heads1$timestamp <- as.POSIXct(new_heads1$timestamp, format = "%Y-%m-%d %H:%M:%S")

# Merge the new data frame with the original heads1 data frame
heads1 <- merge(new_heads1, heads1, by = "timestamp", all.x = TRUE)

heads1$timestamp <- as.Date(heads1$timestamp)

###

heads2 <- read.csv("~/Documents/Documenten - MacBook Pro van Désirée/AAAMsc thesis Econometrics/Data + Code/Grondwater data/heads/heads2.csv", header=TRUE)
#geeft ook meerdere momenten per dag aan

#er zijn geen rows zonder waardes, dus even kijken welke tijd ik pak
heads2$timestamp <- as.POSIXct(heads2$timestamp, format = "%Y-%m-%d %H:%M:%S")
time_component2 <- format(heads2$timestamp, format = "%H:%M:%S")
# Filter the rows where the time component is "00:00:00"
heads2 <- heads2[time_component2 == "00:00:00", ]
# Convert the filtered date-time column back to Date format
heads2$timestamp <- as.Date(heads2$timestamp)
# Remove rows with all NA values
heads2 <- heads2[!apply(heads2[, sapply(heads2, is.numeric)], 1, function(row) all(is.na(row))), ]


####

heads3 <- read.csv("~/Documents/Documenten - MacBook Pro van Désirée/AAAMsc thesis Econometrics/Data + Code/Grondwater data/heads/heads3.csv", header=TRUE)

heads3$timestamp <- as.POSIXct(heads3$timestamp, format = "%Y-%m-%d %H:%M:%S")
time_component3 <- format(heads3$timestamp, format = "%H:%M:%S")
# Filter the rows where the time component is "00:00:00"
heads3 <- heads3[time_component3 == "00:00:00", ]
# Convert the filtered date-time column to Date format
heads3$timestamp <- as.Date(heads3$timestamp)

# Remove rows with all NA values
heads3 <- heads3[!apply(heads3[, -1], 1, function(row) all(is.na(row))), ]
#vanaf 2000, dus is goed

###

heads4 <- read.csv("~/Documents/Documenten - MacBook Pro van Désirée/AAAMsc thesis Econometrics/Data + Code/Grondwater data/heads/heads4.csv", header=TRUE)
#geeft vanaf een moment per minuut aan
# name time index
heads4$timestamp <- as.POSIXct(heads4$timestamp, format = "%Y-%m-%d %H:%M:%S")
#Extract time component from timestamp column
time_component4 <- format(heads4$timestamp, format = "%H:%M:%S")

# Create frequency table of time components
time_freq <- table(time_component4)

# Find time component with maximum frequency since 2012
max_time <- names(which.max(time_freq))

# Filter data frame to include only rows with time component 00:00:00
heads4 <- heads4[time_component4 == "00:00:00", ]

# Create a new data frame with desired date range and most frequent time component
new_dates <- seq(as.POSIXct("2002-01-01"), as.POSIXct("2024-06-06"), by = "day")
new_timestamps <- paste(format(new_dates, "%Y-%m-%d"), max_time, sep = " ")
new_heads4 <- data.frame(timestamp = new_timestamps)
new_heads4$timestamp <- as.POSIXct(new_heads4$timestamp, format = "%Y-%m-%d %H:%M:%S")

# Merge the new data frame with the original heads4 data frame
heads4 <- merge(new_heads4, heads4, by = "timestamp", all.x = TRUE)
# Filter data to include only observations since 2002
heads4 <- heads4[heads4$timestamp >= as.POSIXct("2002-01-01", format = "%Y-%m-%d"), ]

heads4$timestamp <- as.Date(heads4$timestamp)
###

heads5 <- read.csv("~/Documents/Documenten - MacBook Pro van Désirée/AAAMsc thesis Econometrics/Data + Code/Grondwater data/heads/heads5.csv", header=TRUE)
#per dag
heads5$timestamp <- as.POSIXct(heads5$timestamp, format = "%Y-%m-%d")
# Remove rows with all NA values (zijn er niet)
heads5 <- heads5[!apply(heads5[, -1], 1, function(row) all(is.na(row))), ]
# Convert the filtered date-time column back to Date format
heads5$timestamp <- as.Date(heads5$timestamp)

###

heads6 <- read.csv("~/Documents/Documenten - MacBook Pro van Désirée/AAAMsc thesis Econometrics/Data + Code/Grondwater data/heads/heads6.csv", header=TRUE)
#per dag
heads6$timestamp <- as.POSIXct(heads6$timestamp, format = "%Y-%m-%d")
# Remove rows with all NA values
heads6 <- heads6[!apply(heads6[, -1], 1, function(row) all(is.na(row))), ]
# Convert the filtered date-time column back to Date format
heads6$timestamp <- as.Date(heads6$timestamp)

###

heads7 <- read.csv("~/Documents/Documenten - MacBook Pro van Désirée/AAAMsc thesis Econometrics/Data + Code/Grondwater data/heads/heads7.csv", header=TRUE)
#per dag
heads7$timestamp <- as.POSIXct(heads7$timestamp, format = "%Y-%m-%d")
# Remove rows with all NA values
heads7 <- heads7[!apply(heads7[, -1], 1, function(row) all(is.na(row))), ]
# Convert the filtered date-time column back to Date format
heads7$timestamp <- as.Date(heads7$timestamp)

###

#merging the data sets to make it one panel data set and match the dates of observations

merged_heads15 <- merge(heads1, heads5, by = "timestamp") #vanaf 2002
merged_heads67 <- merge(heads6, heads7, by = "timestamp") #vanaf 1994 dus er gaan veel observations verloren
merged_heads1567 <- merge(merged_heads15,merged_heads67, by="timestamp")

merged_heads23 <- merge(heads2, heads3, by = "timestamp") #vanaf 2000

merged_heads123567 <- merge(merged_heads1567,merged_heads23, by = "timestamp")
merged_heads123567$timestamp <- as.Date(merged_heads123567$timestamp)

groundwater <- merge(merged_heads123567,heads4, by = "timestamp")

groundwater$timestamp <- as.Date(groundwater$timestamp, format = "%Y%m%d")
colnames(groundwater) <- gsub("X", "", colnames(groundwater))

rm(heads1,heads2,heads3,heads4,heads5,heads6,heads7,merged_heads123567,merged_heads15,merged_heads1567,merged_heads23,merged_heads67,time_component1,time_component2,time_component3,time_component4,new_heads1,new_heads4,max_time,new_dates,time_freq,new_timestamps)

#remove the wells that are duplicated
wells_to_keep <- as.character(filtered_metadata$feature_id)
existing_wells <- wells_to_keep[wells_to_keep %in% colnames(groundwater)]

columns_to_keep <- c("timestamp", wells_to_keep)
existing_columns <- columns_to_keep[columns_to_keep %in% colnames(groundwater)]

filtered_groundwater <- groundwater %>%
  dplyr::select(all_of(existing_columns))

##only select the 14th and the 28th observation of each month to reduce the panel data set:

filtered_groundwater <- filtered_groundwater %>%
  mutate(timestamp = as.Date(timestamp))

# Filter the dataframe to include only the 14th and 28th day of each month
filtered_groundwater <- filtered_groundwater %>%
  filter(day(timestamp) == 14 | day(timestamp) == 28)


rm(groundwater,columns_to_keep,existing_columns,existing_wells,wells_to_keep)

###connecting groundwater wells to climate stations

wells <- data.frame(filtered_metadata$id,filtered_metadata$feature_id,filtered_metadata$label,filtered_metadata$x,filtered_metadata$y)
names(wells) <-c( "id","featureid","name","longitude", "latitude" )

setDT(wells)

stations <- read_excel("~/Documents/Documenten - MacBook Pro van Désirée/AAAMsc thesis Econometrics/Data + Code/weerstations.xlsx")
setDT(stations)

wells[, longitude := as.numeric(longitude)]
wells[, latitude := as.numeric(latitude)]
stations[, longitude := as.numeric(longitude)]
stations[, latitude := as.numeric(latitude)]

# Initialize a data.table to store results
results <- data.table(well_id = wells$id, station_id = stations$id, distance_km = NA)

# Function to find the nearest station for a single well
find_nearest_station <- function(well) {
  well_coords <- c(well$longitude, well$latitude)
  station_coords <- stations[, .(longitude, latitude)]
  distances <- distVincentySphere(matrix(well_coords, nrow = 1), as.matrix(station_coords))
  nearest_station_idx <- which.min(distances)
  list(station_id = stations[nearest_station_idx]$id, distance_km = distances[nearest_station_idx] / 1000)
}

# Apply the function to each well
nearest_stations <- wells[, find_nearest_station(.SD), by = id, .SDcols = c("longitude", "latitude")]

# Merge the results back to the wells data
results <- merge(wells, nearest_stations, by.x = "id", by.y = "id", all.x = TRUE)

rm(nearest_stations)


#climatory variables

#TN minimum temperature
#TX maximum temperature (in 0.1 degrees celcius)
#TG: Etmaalgemiddelde temperatuur (in 0.1 graden Celsius)
#TXH hour when the max temperature was reached
#RH daily precipitation amount (in 0.1 mm)
#EV24 potential evapotranspiration (Makkink)

##all temperature and precipitation of all stations in one dataframe
# Read the files
evap2002_2007 <- read.csv("~/Documents/Documenten - MacBook Pro van Désirée/AAAMsc thesis Econometrics/Data + Code/evap2002-2007.txt", header=FALSE, comment.char="#")
evap2007_2012 <- read.csv("~/Documents/Documenten - MacBook Pro van Désirée/AAAMsc thesis Econometrics/Data + Code/evap2007-2012.txt", header=FALSE, comment.char="#")
evap2012_2017 <- read.csv("~/Documents/Documenten - MacBook Pro van Désirée/AAAMsc thesis Econometrics/Data + Code/evap2012-2017.txt", header=FALSE, comment.char="#")
evap2017_2022 <- read.csv("~/Documents/Documenten - MacBook Pro van Désirée/AAAMsc thesis Econometrics/Data + Code/evap2017-2022.txt", header=FALSE, comment.char="#")
evap2022_2024 <- read.csv("~/Documents/Documenten - MacBook Pro van Désirée/AAAMsc thesis Econometrics/Data + Code/evap2022-2024.txt", header=FALSE, comment.char="#")

precip2002_2007 <- read.csv("~/Documents/Documenten - MacBook Pro van Désirée/AAAMsc thesis Econometrics/Data + Code/precip2002-2007.txt", header=FALSE, comment.char="#")
precip2007_2012 <- read.csv("~/Documents/Documenten - MacBook Pro van Désirée/AAAMsc thesis Econometrics/Data + Code/precip2007-2012.txt", header=FALSE, comment.char="#")
precip2012_2017 <- read.csv("~/Documents/Documenten - MacBook Pro van Désirée/AAAMsc thesis Econometrics/Data + Code/precip2012-2017.txt", header=FALSE, comment.char="#")
precip2017_2022 <- read.csv("~/Documents/Documenten - MacBook Pro van Désirée/AAAMsc thesis Econometrics/Data + Code/precip2017-2022.txt", header=FALSE, comment.char="#")
precip2022_2024 <- read.csv("~/Documents/Documenten - MacBook Pro van Désirée/AAAMsc thesis Econometrics/Data + Code/precip2022-2024.txt", header=FALSE, comment.char="#")

temp2002_2007 <- read.csv("~/Documents/Documenten - MacBook Pro van Désirée/AAAMsc thesis Econometrics/Data + Code/temp2002-2007.txt", header=FALSE, comment.char="#")
temp2007_2012 <- read.csv("~/Documents/Documenten - MacBook Pro van Désirée/AAAMsc thesis Econometrics/Data + Code/temp2007-2012.txt", header=FALSE, comment.char="#")
temp2012_2017 <- read.csv("~/Documents/Documenten - MacBook Pro van Désirée/AAAMsc thesis Econometrics/Data + Code/temp2012-2017.txt", header=FALSE, comment.char="#")
temp2017_2022 <- read.csv("~/Documents/Documenten - MacBook Pro van Désirée/AAAMsc thesis Econometrics/Data + Code/temp2017-2022.txt", header=FALSE, comment.char="#")
temp2022_2024 <- read.csv("~/Documents/Documenten - MacBook Pro van Désirée/AAAMsc thesis Econometrics/Data + Code/temp2022-2024.txt", header=FALSE, comment.char="#")

# Combine the data frames
evap <- rbind(evap2002_2007, evap2007_2012, evap2012_2017, evap2017_2022, evap2022_2024)
precip <- rbind(precip2002_2007, precip2007_2012, precip2012_2017, precip2017_2022, precip2022_2024)
temp <- rbind(temp2002_2007, temp2007_2012, temp2012_2017, temp2017_2022, temp2022_2024)

rm(precip2002_2007,precip2007_2012,precip2012_2017,precip2017_2022,precip2022_2024)
rm(evap2002_2007,evap2007_2012,evap2012_2017,evap2017_2022,evap2022_2024)
rm(temp2002_2007,temp2007_2012,temp2012_2017,temp2017_2022,temp2022_2024)

# Combine the three variables into a single dataframe
climatevars <- data.frame(evap, precip, temp)

climatevars$V2 <- as.Date(as.character(climatevars$V2), format = "%Y%m%d")
climatevars$V2.1 <- as.Date(as.character(climatevars$V2.1), format = "%Y%m%d")
climatevars$V2.2 <- as.Date(as.character(climatevars$V2.2), format = "%Y%m%d")

# Remove the original date columns
climatevars <- climatevars[, -c(5,8)]
# Remove the id's of the stations
climatevars <- climatevars[,-c(4,6)]

names(climatevars) <- c("station_id","timestamp", "evaporation","precipitation","temperature")

rm(evap,precip,temp)

##select only the 14th and 28th observayion of the month:

# Ensure the timestamp column is in Date format
climatevars <- climatevars %>%
  mutate(timestamp = as.Date(timestamp))

# Filter the dataframe to include only the 14th and 28th day of each month
climatevars <- climatevars %>%
  filter(day(timestamp) == 14 | day(timestamp) == 28)

#create panel data

panel_data <- pivot_longer(filtered_groundwater, cols = -timestamp, names_to = "featureid", values_to = "Groundwater level")
panel_data$featureid <- as.integer(panel_data$featureid)
station_ids <- data.frame(featureid = results$featureid, station_id = results$station_id)

#add the column of the corresponsding climate station
panel_data <- left_join(panel_data, station_ids, by= "featureid")

#add the corresponding climate values
panel_data <- left_join(panel_data,climatevars, by = c("station_id","timestamp"))

# Calculate quantiles and IQR
quartiles <- quantile(panel_data$`Groundwater level`, probs = c(0.25, 0.75), na.rm = TRUE)
iqr <- quartiles[2] - quartiles[1]
lower_bound <- quartiles[1] - 1.5 * iqr
upper_bound <- quartiles[2] + 1.5 * iqr

# Replace outliers with NA
panel_data$`Groundwater level`[panel_data$`Groundwater level` < lower_bound | panel_data$`Groundwater level` > upper_bound] <- NA

rm(iqr,lower_bound,quartiles,upper_bound, station_ids)

###Land use variables

landuse_raster <- raster("~/Documents/Documenten - MacBook Pro van Désirée/AAAMsc thesis Econometrics/Data + Code/LGN2022/LGN2022.tif")

# Plot the raster to visually inspect it
#plot(landuse_raster)

# Print the extent and CRS of the raster
#print(extent(landuse_raster))
#print(crs(landuse_raster))

# Prepare the coordinates data frame, ensuring correct order
coords <- data.frame(x = as.numeric(filtered_metadata$x),
                     y = as.numeric(filtered_metadata$y))

# Remove rows with NA values
coords <- coords[complete.cases(coords), ]

# Print the first few coordinates for inspection
print(head(coords))

# Convert coordinates from geographic (degrees) to projected (RD New) using sf
coords_sf <- st_as_sf(coords, coords = c("x", "y"), crs = 4326) # EPSG:4326 is WGS84

# Print the transformed coordinates for inspection
print(st_coordinates(coords_sf))

# Ensure the CRS of the raster and points are the same (although it should be already)
if (!st_crs(coords_sf) == st_crs(landuse_raster)) {
  # Reproject locations to the raster CRS if they are different
  coords_sf <- st_transform(coords_sf, st_crs(landuse_raster))
}

# Plot the locations to see if they fall within the raster extent
#plot(st_geometry(coords_sf), col = 'red', add = TRUE)

# Check if points fall within the raster extent
points_in_raster <- st_intersects(coords_sf, st_as_sfc(st_bbox(landuse_raster)), sparse = FALSE)
print(sum(points_in_raster))
##all the 17000 wells fall in the raster

# Only keep locations that are within the raster extent
locations <- coords_sf[points_in_raster, ]

locations_spdf <- as(locations, "Spatial")

# Extract land use values using exactextractr for compatibility with sf
landuse_values <- raster::extract(landuse_raster, locations_spdf)

# Inspect the result
print(head(landuse_values))

land_use <- cbind(coords,landuse_values)

land_use <- distinct(land_use, x, y, .keep_all = TRUE)
filtered_metadata <- distinct(filtered_metadata, x, y, .keep_all = TRUE)
filtered_metadata$x <- as.numeric(filtered_metadata$x)
filtered_metadata$y <- as.numeric(filtered_metadata$y)


land_use <- full_join(land_use, filtered_metadata[, c("x", "y", "feature_id")], by = c("x", "y"))
#now add to the panel data:

panel_data <- left_join(panel_data, land_use[, c("feature_id", "landuse_values")], by = c("featureid" = "feature_id"))
panel_data_na <- na.omit(panel_data) ##without the na's

rm(panel_data,locations,locations_spdf,points_in_raster,climatevars,landuse_raster,land_use,coords_sf,results,filtered_groundwater,landuse_values)

#include land use as dummies
unique(panel_data_na$landuse_values) ##there are 43 unique land use values, so create dummies for urban and rural

#urban/non-urban
panel_data_na$urban <- ifelse(panel_data_na$landuse_values %in% c(25,28,26,25,24,23,22,20,19,18), 1, 0)
panel_data_na$non_urban <- ifelse(!panel_data_na$landuse_values %in% c(25,28,26,25,24,23,22,20,19,18), 1, 0)

###geology 

# Read the geology shapefile
geology_data <- st_read("~/Documents/Documenten - MacBook Pro van Désirée/AAAMsc thesis Econometrics/Data + Code/GeologischeKaartNederland2021-gpkg/GKNederlandGeolVlak.gpkg")

filtered_metadata <- filtered_metadata[complete.cases(filtered_metadata[c("x", "y")]), ]
coords_sf <- st_as_sf(filtered_metadata, coords = c("x", "y"), crs = 4326)

if (st_crs(coords_sf) != st_crs(geology_data)) {
  coords_sf <- st_transform(coords_sf, st_crs(geology_data))
}

coords_with_geology <- st_join(coords_sf, geology_data, join = st_within)
coords_with_geology_df <- st_set_geometry(coords_with_geology, NULL)

geology_df <- coords_with_geology_df %>%
  dplyr::select(feature_id, CODE) %>%
  distinct()

rm(coords_with_geology,coords_with_geology_df)

#now add to the panel data

panel_data_na <- full_join(panel_data_na,geology_df[, c("feature_id", "CODE")], by = c("featureid" = "feature_id"))
names(panel_data_na)[names(panel_data_na) == "CODE"] <- "geology_type"

#create dummies for geology types
unique(panel_data_na$geology_type)

#6 different main categories in dutch soil

geology_type_counts <- table(panel_data_na$geology_type)
# Create a new data frame with the unique geology types
geology_types <- data.frame(type = unique(panel_data_na$geology_type))

# Define the categories for each landscape
geology_types$is_sand <- ifelse(geology_types$type %in% c("BX4", "BX1", "BX3", "BX5", "G1", "G2", "dsg", "sg"), 1, 0)
geology_types$is_dune <- ifelse(geology_types$type %in% c("b", "dg", "v", "v*"), 1, 0)
geology_types$is_hill <- ifelse(geology_types$type %in% c("SY", "BX2", "k", "kb", "ST"), 1, 0)
geology_types$is_peat <- ifelse(geology_types$type %in% c("ov", "ovo*", "kvo*", "vo*", "o", "o*", "kg*"), 1, 0)
geology_types$is_riverclay <- ifelse(geology_types$type %in% c("DR1", "DR2", "KR1", "KR2", "WA"), 1, 0)
geology_types$is_seaclay <- ifelse(geology_types$type %in% c("BE3", "BE2", "BE1"), 1, 0)

#merge them into the panel data as dummy variables

panel_data_na <- merge(panel_data_na, geology_types, by.x = "geology_type", by.y = "type", all.x = TRUE)

rm(geology_data,geology_df,geology_types,geology_type_counts,coords,coords_sf)

###elevation (AHN file)

# Read the AHN raster file
ahn_raster <- raster("~/Documents/Documenten - MacBook Pro van Désirée/AAAMsc thesis Econometrics/Data + Code/ahn_251.tif")

# Prepare the coordinates data frame, ensuring correct order
coords <- data.frame(x = as.numeric(filtered_metadata$x),
                     y = as.numeric(filtered_metadata$y))

# Remove rows with NA values
coords <- coords[complete.cases(coords), ]

# Convert coordinates from geographic (degrees) to projected (RD New) using sf
coords_sf <- st_as_sf(coords, coords = c("x", "y"), crs = 4326) # EPSG:4326 is WGS84
coords_sf <- st_transform(coords_sf, st_crs(ahn_raster))

# Extract elevation values using raster::extract()
elevation_values <- raster::extract(ahn_raster, coords_sf)

# Combine the extracted elevation values with the original data frame
elevation <- cbind(coords, elevation_values)

# Join the elevation data with the filtered_metadata
elevation <- full_join(elevation, filtered_metadata[, c("x", "y", "feature_id")], by = c("x", "y"))

# Add the elevation data to the panel_data
panel_data_na <- left_join(panel_data_na, elevation[, c("feature_id", "elevation_values")], by = c("feature_id" = "feature_id"))

# Remove rows with NA values
panel_data_na <- na.omit(panel_data_na)

rm(elevation,coords_sf,coords,ahn_raster)

#create year dummies for difference 2018 drought and 2022 drought: 
panel_data_na$year_2002 <- ifelse(year(panel_data_na$timestamp) == 2002, 1, 0)
panel_data_na$year_2003 <- ifelse(year(panel_data_na$timestamp) == 2003, 1, 0)
panel_data_na$year_2004 <- ifelse(year(panel_data_na$timestamp) == 2004, 1, 0)
panel_data_na$year_2005 <- ifelse(year(panel_data_na$timestamp) == 2005, 1, 0)
panel_data_na$year_2006 <- ifelse(year(panel_data_na$timestamp) == 2006, 1, 0)
panel_data_na$year_2007 <- ifelse(year(panel_data_na$timestamp) == 2007, 1, 0)
panel_data_na$year_2008 <- ifelse(year(panel_data_na$timestamp) == 2008, 1, 0)
panel_data_na$year_2009 <- ifelse(year(panel_data_na$timestamp) == 2009, 1, 0)
panel_data_na$year_2010 <- ifelse(year(panel_data_na$timestamp) == 2010, 1, 0)
panel_data_na$year_2011 <- ifelse(year(panel_data_na$timestamp) == 2011, 1, 0)
panel_data_na$year_2012 <- ifelse(year(panel_data_na$timestamp) == 2012, 1, 0)
panel_data_na$year_2013 <- ifelse(year(panel_data_na$timestamp) == 2013, 1, 0)
panel_data_na$year_2014 <- ifelse(year(panel_data_na$timestamp) == 2014, 1, 0)
panel_data_na$year_2015 <- ifelse(year(panel_data_na$timestamp) == 2015, 1, 0)
panel_data_na$year_2016 <- ifelse(year(panel_data_na$timestamp) == 2016, 1, 0)
panel_data_na$year_2017 <- ifelse(year(panel_data_na$timestamp) == 2017, 1, 0)
panel_data_na$year_2018 <- ifelse(year(panel_data_na$timestamp) == 2018, 1, 0)
panel_data_na$year_2019 <- ifelse(year(panel_data_na$timestamp) == 2019, 1, 0)
panel_data_na$year_2020 <- ifelse(year(panel_data_na$timestamp) == 2020, 1, 0)
panel_data_na$year_2021 <- ifelse(year(panel_data_na$timestamp) == 2021, 1, 0)
panel_data_na$year_2022 <- ifelse(year(panel_data_na$timestamp) == 2022, 1, 0)
panel_data_na$year_2023 <- ifelse(year(panel_data_na$timestamp) == 2023, 1, 0)

#cready dummy for southeastern part NL:

# Define the coordinate ranges for the southeastern region
southeast_lat_min <- 4
southeast_lat_max <- 6
southeast_lon_min <- 50
southeast_lon_max <- 52

# Check if the coordinates are within the southeastern region
filtered_metadata <- filtered_metadata %>%
  mutate(
    is_southeast = ifelse(
      x >= southeast_lat_min & x <= southeast_lat_max &
        y >= southeast_lon_min & y <= southeast_lon_max,
      1, 0
    )
  )

names(panel_data_na)[names(panel_data_na) == "featureid"] <- "feature_id"

panel_data_na <- panel_data_na %>%
  left_join(filtered_metadata[, c("feature_id", "is_southeast")], by = "feature_id")

rm(southeast_lat_max,southeast_lat_min,southeast_lon_max,southeast_lon_min,coords_with_geology,coords_with_geology_df,stations,wells,elevation_values)

################quantile regression using quantreg package:############

#null model
mod_nl <- rq(`Groundwater level` ~ 1, data=panel_data_na,tau=0.025)
mod_nl10 <- rq(`Groundwater level` ~ 1, data=panel_data_na,tau=0.1)
mod_nl25 <- rq(`Groundwater level` ~ 1, data=panel_data_na,tau=0.25)
mod_nl50 <- rq(`Groundwater level` ~ 1, data=panel_data_na,tau=0.5)
AIC_nl10 <- AIC.rq(mod_nl10)
AIC_nl25 <- AIC.rq(mod_nl25)
AIC_nl50 <- AIC.rq(mod_nl50)
AIC_nl025<- AIC.rq(mod_nl)

# 1. Quantile regression models with temporal effects
model_temp_drought <- rq(`Groundwater level` ~ 0 + year_2002 + year_2003 + year_2004 + year_2005 +
                           year_2006 + year_2007 + year_2008 + year_2009 + year_2010 +
                           year_2011 + year_2012 + year_2013 + year_2014 + year_2015 +
                           year_2016 + year_2017 + year_2018 + year_2019 + year_2020 +
                           year_2021 + year_2022 + year_2023,
                         data = panel_data_na, tau = 0.025)

temp_noinitercept<- summary(model_temp_drought)

resid_temp_drought <- residuals(model_temp_drought)

model_temp <- rq(`Groundwater level` ~ 0 + year_2002 + year_2003 + year_2004 + year_2005 +
                           year_2006 + year_2007 + year_2008 + year_2009 + year_2010 +
                           year_2011 + year_2012 + year_2013 + year_2014 + year_2015 +
                           year_2016 + year_2017 + year_2018 + year_2019 + year_2020 +
                           year_2021 + year_2022 + year_2023,
                         data = panel_data_na, tau = c(0.1,0.25,0.5))

summary(model_temp)

model_temp_intercept <- rq(`Groundwater level` ~ year_2003 + year_2004 + year_2005 +
                             year_2006 + year_2007 + year_2008 + year_2009 + year_2010 +
                             year_2011 + year_2012 + year_2013 + year_2014 + year_2015 +
                             year_2016 + year_2017 + year_2018 + year_2019 + year_2020 +
                             year_2021 + year_2022 + year_2023,
                           data = panel_data_na, tau = 0.025)


model_temp_intercept_all <- rq(`Groundwater level` ~ year_2003 + year_2004 + year_2005 +
                             year_2006 + year_2007 + year_2008 + year_2009 + year_2010 +
                             year_2011 + year_2012 + year_2013 + year_2014 + year_2015 +
                             year_2016 + year_2017 + year_2018 + year_2019 + year_2020 +
                             year_2021 + year_2022 + year_2023,
                           data = panel_data_na, tau = c(0.1,0.25,0.5))


temp_intercept <- summary(model_temp_intercept)
temp_intercept_all <- summary(model_temp_intercept_all)

AIC_temp_drought <- AIC.rq(model_temp_drought) 

AIC_temp <-AIC.rq(model_temp)

# 2. Quantile regression models with climate terms  

model_clim_drought <- rq(`Groundwater level` ~ temperature + precipitation + evaporation, 
                         tau = 0.025, data = panel_data_na)

climate_drought<- summary(model_clim_drought)

##pseudo R-squared
# Extract residuals from the quantile regression model
resid_clim_drought <- residuals(model_clim_drought)

# Extract the quantile levels
tau <- model_clim_drought$tau

# Define the check function
rho <- function(u, tau) u * (tau - (u < 0))

# Initialize a vector to store pseudo R-squared values
pseudo_r2_clim_drought <- numeric(length(tau))

# Calculate pseudo R-squared for each quantile
for (i in seq_along(tau)) {
  # Calculate the numerator: sum of the check function applied to residuals
  num <- sum(rho(resid_clim_drought, tau[i]))
  
  # Extract the response variable from the model
  y <- model.response(model.frame(model_clim_drought))
  
  # Calculate the denominator: sum of the check function applied to deviations from the median
  den <- sum(rho(y - median(y), tau[i]))
  
  # Calculate pseudo R-squared
  pseudo_r2_clim_drought[i] <- 1 - num / den
}

##other quantiles
model_clim <- rq(`Groundwater level` ~ temperature + precipitation + evaporation, 
                         tau = c(0.1,0.25,0.5), data = panel_data_na)


AIC_clim_drought <- AIC.rq(residuals(model_clim_drought))

AIC_nl-AIC_clim_drought

AIC_nl10-AIC.rq(model_clim)[1]
AIC_nl25-AIC.rq(model_clim)[2]
AIC_nl50-AIC.rq(model_clim)[3]

#Pseudo R-squared 
resid_clim <- residuals(model_clim)
tau <- model_clim$tau
rho <- function(u, tau) u * (tau - (u < 0))

pseudo_r2_clim <- numeric(length(tau))

for (i in seq_along(tau)) {
  num <- sum(rho(resid_clim[, i], tau[i]))
  y <- model.response(model.frame(model_clim))
  den <- sum(rho(y - median(y), tau[i]))
  pseudo_r2_clim[i] <- 1 - num / den
}

# 3.QR with elevation

model_elevation_drought <- rq(`Groundwater level` ~ temperature + precipitation + evaporation + elevation_values, 
tau = 0.025, data = panel_data_na)

summary(model_elevation_drought)

##pseudo R-squared
# Extract residuals from the quantile regression model
resid_elevation_drought <- residuals(model_elevation_drought)

# Extract the quantile levels
tau <- model_elevation_drought$tau

# Define the check function
rho <- function(u, tau) u * (tau - (u < 0))

# Initialize a vector to store pseudo R-squared values
pseudo_r2_elevation_drought <- numeric(length(tau))

# Calculate pseudo R-squared for each quantile
for (i in seq_along(tau)) {
  # Calculate the numerator: sum of the check function applied to residuals
  num <- sum(rho(resid_elevation_drought, tau[i]))
  
  # Extract the response variable from the model
  y <- model.response(model.frame(model_elevation_drought))
  
  # Calculate the denominator: sum of the check function applied to deviations from the median
  den <- sum(rho(y - median(y), tau[i]))
  
  # Calculate pseudo R-squared
  pseudo_r2_elevation_drought[i] <- 1 - num / den
}

##AIC drought

AIC_elevation_drought <- AIC.rq(model_elevation_drought)

##other quantiles

model_elevation <- rq(`Groundwater level` ~ temperature + precipitation + evaporation + elevation_values, 
                              tau = c(0.1,0.25,0.50), data = panel_data_na)

summary(model_elevation)

##AIC

AIC_nl-AIC_elevation_drought

AIC_nl10-AIC.rq(model_elevation)[1]
AIC_nl25-AIC.rq(model_elevation)[2]
AIC_nl50-AIC.rq(model_elevation)[3]

#Pseudo R-squared 
resid_elevation <- residuals(model_elevation)
tau <- model_elevation$tau
rho <- function(u, tau) u * (tau - (u < 0))

pseudo_r2_elevation <- numeric(length(tau))

for (i in seq_along(tau)) {
  num <- sum(rho(resid_elevation[, i], tau[i]))
  y <- model.response(model.frame(model_elevation))
  den <- sum(rho(y - median(y), tau[i]))
  pseudo_r2_elevation[i] <- 1 - num / den
}


# 4. QR with urban dummy

model_urban_drought <- rq(`Groundwater level` ~ temperature + precipitation + evaporation + elevation_values + urban, 
                       tau = 0.025, data = panel_data_na)

summary(model_urban_drought)

##pseudo R-squared
# Extract residuals from the quantile regression model
resid_urban_drought <- residuals(model_urban_drought)

# Extract the quantile levels
tau <- model_urban_drought$tau

# Define the check function
rho <- function(u, tau) u * (tau - (u < 0))

# Initialize a vector to store pseudo R-squared values
pseudo_r2_urban_drought <- numeric(length(tau))

# Calculate pseudo R-squared for each quantile
for (i in seq_along(tau)) {
  # Calculate the numerator: sum of the check function applied to residuals
  num <- sum(rho(resid_urban_drought, tau[i]))
  
  # Extract the response variable from the model
  y <- model.response(model.frame(model_urban_drought))
  
  # Calculate the denominator: sum of the check function applied to deviations from the median
  den <- sum(rho(y - median(y), tau[i]))
  
  # Calculate pseudo R-squared
  pseudo_r2_urban_drought[i] <- 1 - num / den
}

##AIC

AIC_urban_drought <- AIC.rq(model_urban_drought)

AIC_nl-AIC_urban_drought

##other quantiles

model_urban <- rq(`Groundwater level` ~ temperature + precipitation + evaporation + elevation_values + urban, 
                              tau = c(0.1,0.25,0.5), data = panel_data_na)

summary(model_urban)

AIC_nl10-AIC.rq(model_urban)[1]
AIC_nl25-AIC.rq(model_urban)[2]
AIC_nl50-AIC.rq(model_urban)[3]

#Pseudo R-squared 
resid_urban <- residuals(model_urban)
tau <- model_urban$tau
rho <- function(u, tau) u * (tau - (u < 0))

pseudo_r2_urban <- numeric(length(tau))

for (i in seq_along(tau)) {
  num <- sum(rho(resid_urban[, i], tau[i]))
  y <- model.response(model.frame(model_urban))
  den <- sum(rho(y - median(y), tau[i]))
  pseudo_r2_urban[i] <- 1 - num / den
}

# 5. QR with soil types (final model)

model_soil_drought <- rq(`Groundwater level` ~ temperature + precipitation + evaporation + elevation_values + urban + is_southeast + is_sand + is_dune + is_peat + is_hill + is_riverclay + is_seaclay, 
                              tau = 0.025, data = panel_data_na)


sum_soil_drought<- summary(model_soil_drought)

##pseudo R-squared
# Extract residuals from the quantile regression model
resid_soil_drought <- residuals(model_soil_drought)

# Extract the quantile levels
tau <- model_soil_drought$tau

# Define the check function
rho <- function(u, tau) u * (tau - (u < 0))

# Initialize a vector to store pseudo R-squared values
pseudo_r2_soil_drought <- numeric(length(tau))

# Calculate pseudo R-squared for each quantile
for (i in seq_along(tau)) {
  # Calculate the numerator: sum of the check function applied to residuals
  num <- sum(rho(resid_soil_drought, tau[i]))
  
  # Extract the response variable from the model
  y <- model.response(model.frame(model_soil_drought))
  
  # Calculate the denominator: sum of the check function applied to deviations from the median
  den <- sum(rho(y - median(y), tau[i]))
  
  # Calculate pseudo R-squared
  pseudo_r2_soil_drought[i] <- 1 - num / den
}

##AIC
AIC_soil_drought <- AIC.rq(model_soil_drought)

AIC_nl-AIC_soil_drought

##other quantiles

model_soil <- rq(`Groundwater level` ~ temperature + precipitation + evaporation + elevation_values + urban + is_southeast + is_sand + is_dune + is_peat + is_hill + is_riverclay + is_seaclay, 
                         tau = c(0.1,0.25,0.5), data = panel_data_na)

sum_soil <- summary(model_soil)



AIC_nl10-AIC.rq(model_soil)[1]
AIC_nl25-AIC.rq(model_soil)[2]
AIC_nl50-AIC.rq(model_soil)[3]

## final model

#QR_final_drought <- rq(`Groundwater level` ~ temperature + precipitation + evaporation + 
  elevation_values + urban + is_southeast +
  is_sand + is_dune + is_peat + is_hill + is_riverclay + is_seaclay
  , tau = 0.025, data = panel_data_na)

#summary(QR_final_drought)

cov_matrix_final_drought <- vcovHC(QR_final_drought, type = "HC3")

#AIC_final_drought <- AIC.rq(QR_final_drought)

#####ADDITIONAL MODEL WITH LAGS AND INTERACTION TERMS:

###Model 5
# Add interaction terms 
interaction_terms <- c("temperature:precipitation",
                       "urban:elevation_values")


# Create the formula including the interaction terms
formula <- as.formula(paste("`Groundwater level` ~ temperature + precipitation + 
                             evaporation + elevation_values + urban + is_southeast +
                             is_sand + is_dune + is_peat + is_hill + is_riverclay + is_seaclay +",
                            paste(interaction_terms, collapse = " + ")))

# Fit the quantile regression model
model5 <- rq(formula, tau = 0.025, data = lagged_data)

# Print the summary of the model
summary(model5)

##now for the other quantiles

model5_others <- rq(formula, tau = c(0.1,0.25,0.5), data = lagged_data)

summary(model5_others)

##AIC and deltaAIC

AICmod5<- AIC.rq(model5)
AICmod5others <- AIC.rq(model5_others)

AIC_nl025-AICmod5
AIC_nl10-AICmod5others[1]
AIC_nl25-AICmod5others[2]
AIC_nl50-AICmod5others[3]

###pseudo R-squared

# Extract the quantile levels
#Pseudo R-squared 
resid_mod5others <- residuals(model5_others)
tau <- model5_others$tau
rho <- function(u, tau) u * (tau - (u < 0))

pseudo_r2_mod5others <- numeric(length(tau))

for (i in seq_along(tau)) {
  num <- sum(rho(resid_mod5others[, i], tau[i]))
  y <- model.response(model.frame(model5_others))
  den <- sum(rho(y - median(y), tau[i]))
  pseudo_r2_mod5others[i] <- 1 - num / den
}

###Model 6

##suggestion Julia:

#take lags fromonemonth, one year,three years (fromhydrology logical model)

panel_data_na <- panel_data_na %>%
  arrange(feature_id, date)

library(data.table)

# Convert lagged_data to a data.table if it's not already
lagged_data <- as.data.table(panel_data_na)

# Ensure the data is sorted by feature_id and any time-related column (if exists)
# Replace 'time_column' with the actual name of your time-related column if it exists
setorder(lagged_data, feature_id, timestamp)

# Define the lag periods
lag_periods <- list(
  precipitation_lag_2weeks = 1,   
  precipitation_lag_1month = 2,  
  precipitation_lag_3months = 6,   
  precipitation_lag_6months = 12,
  precipitation_lag_1year = 24
)

# Apply the shift operation to the existing lagged variable columns
for (lag_col in names(lag_periods)) {
  lag_period <- lag_periods[[lag_col]]
  lagged_data[, (lag_col) := shift(precipitation, n = lag_period, type = "lag"), by = feature_id]
}

###

# Fit the quantile regression model

#2 weeks

# Create the formula including the interaction terms
formula_6 <- as.formula(paste("`Groundwater level` ~ temperature + precipitation_lag_2weeks +
      evaporation + elevation_values + urban + is_southeast +
                             is_sand + is_dune + is_peat + is_hill + is_riverclay + is_seaclay +",
                            paste(interaction_terms, collapse = " + ")))

model6 <- rq(formula_6, tau = 0.025, data = lagged_data)

# Check the summary
summary(model6)

#1month,3months,6months,1year

# Create the formula including the interaction terms
formula_7 <- as.formula(paste("`Groundwater level` ~ temperature + precipitation_lag_1month +
      evaporation + elevation_values + urban + is_southeast +
                             is_sand + is_dune + is_peat + is_hill + is_riverclay + is_seaclay +",
                              paste(interaction_terms, collapse = " + ")))

model7 <- rq(formula_7, tau = 0.025, data = lagged_data)

# Check the summary
summary(model7)

###for three months

# Create the formula including the interaction terms
formula_8 <- as.formula(paste("`Groundwater level` ~ temperature + precipitation_lag_3months +
      evaporation + elevation_values + urban + is_southeast +
                             is_sand + is_dune + is_peat + is_hill + is_riverclay + is_seaclay +",
                              paste(interaction_terms, collapse = " + ")))

model8 <- rq(formula_8, tau = 0.025, data = lagged_data)

# Check the summary
summary(model8)

###for six months

# Create the formula including the interaction terms
formula_9 <- as.formula(paste("`Groundwater level` ~ temperature + precipitation_lag_6months +
      evaporation + elevation_values + urban + is_southeast +
                             is_sand + is_dune + is_peat + is_hill + is_riverclay + is_seaclay +",
                              paste(interaction_terms, collapse = " + ")))

model9 <- rq(formula_9, tau = 0.025, data = lagged_data)

# Check the summary
summary(model9)

###for one year

# Create the formula including the interaction terms
formula_10 <- as.formula(paste("`Groundwater level` ~ temperature + precipitation_lag_1year +
      evaporation + elevation_values + urban + is_southeast +
                             is_sand + is_dune + is_peat + is_hill + is_riverclay + is_seaclay +",
                              paste(interaction_terms, collapse = " + ")))

model10 <- rq(formula_10, tau = 0.025, data = lagged_data)

# Check the summary
summary(model10)

