
# Objective:
## 1.) Table for profile risks across England Local Authorities
## 2.) Map reflecting where the high/low non-significant regions are sustained or mixed

# Load the packages
library(dplyr)
library(tidyr)
library("sf")
library("sp")
library("spdep")
library("tmap")

# Load shapefiles for England lads and regions shp
England_LA_shp <- read_sf("C:/Users/chris/Desktop/SGDS/Data/England_shp/England Local Authority Shapefile.shp")
England_Region_shp <- read_sf("C:/Users/chris/Desktop/SGDS/Data/England_shp/England Regions Shapefile.shp")

# set directory
setwd("C:/Users/chris/Desktop/SGDS/Stan_Output_Results")

# import analysis dataset
drug_data <- read.csv("2021.csv", sep = ",")
colnames(drug_data)

# keep columns with significance
drug_data <- drug_data[,c(1,2,4,7,10,13,16,19,22,25,28,31,34)]
colnames(drug_data)

drug_data$Sign_2011 <- as.character(drug_data$Sign_2011)
drug_data$Sign_2012 <- as.character(drug_data$Sign_2012)
drug_data$Sign_2013 <- as.character(drug_data$Sign_2013)
drug_data$Sign_2014 <- as.character(drug_data$Sign_2014)
drug_data$Sign_2015 <- as.character(drug_data$Sign_2015)
drug_data$Sign_2016 <- as.character(drug_data$Sign_2016)
drug_data$Sign_2017 <- as.character(drug_data$Sign_2017)
drug_data$Sign_2018 <- as.character(drug_data$Sign_2018)
drug_data$Sign_2019 <- as.character(drug_data$Sign_2019)
drug_data$Sign_2020 <- as.character(drug_data$Sign_2020)
drug_data$Sign_2021 <- as.character(drug_data$Sign_2021)

drug_data$HighRisk <- rowSums(drug_data[-c(1,2)] == "1")
drug_data$NonSignf <- rowSums(drug_data[-c(1,2,14)] == "0")
drug_data$LowRisk <- rowSums(drug_data[-c(1,2,14,15)] == "-1")


# Reordering the columns in descending order
drug_data <- drug_data[order(-drug_data$HighRisk),]
# Reset the row names to be in order
row.names(drug_data) <- 1:nrow(drug_data)

# Qualitative indicator for profiling FML
drug_data <- drug_data %>%
  mutate(profile = case_when(
    HighRisk == 11 ~ "High Risk [Sustained]",
    HighRisk %in% 7:10 ~ "High Risk [Consistent]",
    HighRisk %in% 5:6 ~ "High Risk [Trending]",
    NonSignf == 11 & !HighRisk %in% 5:11 ~ "Non-Significant [Sustained]",
    NonSignf %in% 7:10 & !HighRisk %in% 5:11 ~ "Non-Significant [Consistent]",
    NonSignf %in% 5:6 & !HighRisk %in% 5:11 & LowRisk != 6 ~ "Non-Significant [Trending]",
    LowRisk == 11 & !HighRisk %in% 5:11 & !NonSignf %in% 5:11 ~ "Low Risk [Sustained]",
    LowRisk %in% 7:10 & !HighRisk %in% 5:11 & !NonSignf %in% 5:11 ~ "Low Risk [Consistent]",
    LowRisk %in% 5:6 | NonSignf == 5 & LowRisk == 6 ~ "Low Risk [Trending]",
    TRUE ~ "Unclassified" # This is for the situation that none of the above conditions are met
  ))


drug_data <- drug_data[order(drug_data$LAD21CD),]
row.names(drug_data) <- 1:nrow(drug_data)
write.csv(drug_data, file = "Risk profile of England Local Authorities.csv", row.names = FALSE,  fileEncoding = "UTF-8")

# profile_data
profile_data <- drug_data[,c(1,2,17)]
# add data to shapefile - luckily the rows align with .shp
spatial_data <- merge(England_LA_shp, profile_data, by.x = "LAD21CD", by.y = "LAD21CD", all.x = TRUE)

spatial_data <- spatial_data %>% fill(everything(), .direction = "down")


# Define custom label positions
custom_labels <- data.frame(name = c("North East", "North West", "Yorkshire and The Humber", "East Midlands", "West Midlands", "East of England", "London", "South East", "South West"),
                            custom_x = c(415000, 335000, 460000, 475000, 380000, 565000, 530000, 510000, 250000),
                            custom_y = c(590000, 525000, 475000, 325000, 300000, 255000, 180000, 150000, 100000))
# Convert custom_labels to an sf object
custom_labels_sf <- st_as_sf(custom_labels, coords = c("custom_x", "custom_y"), crs = st_crs(spatial_data))

# map of significance regions
library(RColorBrewer)


# Convert 'profile' column to an ordered factor
spatial_data$profile <- ordered(spatial_data$profile, 
                                levels = c("High Risk [Sustained]", "High Risk [Consistent]", "High Risk [Trending]",
                                           "Non-Significant [Consistent]","Non-Significant [Sustained]","Non-Significant [Trending]",
                                           "Low Risk [Trending]","Low Risk [Consistent]", "Low Risk [Sustained]"))

# Create a named vector for color mapping
color_mapping <- c("High Risk [Sustained]" = "#B2182B", 
                   "High Risk [Consistent]" = "#D6604D",
                   "High Risk [Trending]" = "#F4A582",
                   "Non-Significant [Consistent]" = "#FDDBC7",
                   "Non-Significant [Sustained]" = "#F7F7F7",
                   "Non-Significant [Trending]" = "#D1E5F0",
                   "Low Risk [Trending]" = "#92C5DE",
                   "Low Risk [Consistent]" = "#4393C3",
                   "Low Risk [Sustained]" = "#2166AC")
                   

# Use the color mapping in tm_fill()
sg_map <- tm_shape(spatial_data) + 
  tm_fill("profile", title = "Risk Profile", 
          palette = color_mapping) +
  tm_shape(England_Region_shp) + tm_polygons(alpha = 0.05) +
  tm_shape(custom_labels_sf) + tm_text("name", size = 1.0) +
  tm_layout(frame = FALSE, legend.outside = TRUE, legend.title.size = 1.2, legend.text.size = 1) +
  tm_compass(position = c("right", "top")) + tm_scale_bar(position = c("right", "bottom"))

tmap_save(sg_map, "profile_drug_n.png", height = 12)


