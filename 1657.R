
library(tidyverse)
library(ggplot2)
library(tidyr)
library(dplyr)
library(plotly)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

# Load raw data
rawdata <- read.csv("smoking.csv")

# Clean data: Remove specific entities
countries_data <- rawdata[!rawdata$Entity %in% c("East Asia and Pacific (WB)", "Sub-Saharan Africa (WB)", 
                                                 "Upper-middle-income countries", "Europe and Central Asia (WB)", 
                                                 "World", "European Union (27)", "Low-income countries", 
                                                 "Lower-middle-income countries", "Middle East and North Africa (WB)", 
                                                 "Middle-income countries", "North America (WB)", "South Asia (WB)", 
                                                 "Latin America and Caribbean (WB)", "High-income countries"), ]

# Further clean data: Exclude years 2018 and 2019
countries_data <- countries_data %>%
  filter(!(Year %in% c(2018, 2019)))

# Rename the column
countries_data <- countries_data %>%
  rename(Prevalence = Prevalence.of.current.tobacco.use....of.adults.) 


# View the filtered data
head(countries_data)

# Check unique entities
unique(countries_data$Entity)

# Check the range of years
unique(countries_data$Year)

# View the structure of the dataset
str(countries_data)

# Summary statistics for numerical columns
summary(countries_data)

# Check the first few rows of the dataset
head(countries_data)

# Check the number of rows and columns
dim(countries_data)

# Load world map data
world <- ne_countries(scale = "medium", returnclass = "sf")  # 'sf' format for spatial data

# Check the structure of the geospatial data
head(world)

# Check the column names in both datasets
colnames(world)
colnames(countries_data)

# Unique country names in the world dataset
unique(world$iso_a3)

# Unique country names in the countries_data dataset
unique(countries_data$Code)

# Identify codes in countries_data but not in world
missing_in_world <- setdiff(countries_data$Code, world$iso_a3)
print(missing_in_world)

# Identify codes in world but not in countries_data
missing_in_data <- setdiff(world$iso_a3, countries_data$Code)
print(missing_in_data)

# Fix missing ISO codes
fix_iso_codes <- function(world_data) {
  world_data %>%
    mutate(iso_a3 = ifelse(name == "France", "FRA", iso_a3)) %>%
    mutate(iso_a3 = ifelse(name == "Norway", "NOR", iso_a3))
}
world <- fix_iso_codes(world)

# Merge world map data with smoking data
map_data <- world %>%
  left_join(countries_data, by = c("iso_a3" = "Code"))

# Replace NA prevalence values with 0
map_data <- map_data %>%
  mutate(Prevalence = ifelse(is.na(Prevalence), 0, Prevalence))

# Inspect the merged data
str(map_data)
summary(map_data)

#get rid of geom
plot_data <- map_data %>%
  st_set_geometry(NULL)  # Drop geometry for Plotly compatibility


#interactive map 
# Determine min and max prevalence values
min_prevalence <- min(plot_data$Prevalence, na.rm = TRUE)
max_prevalence <- max(plot_data$Prevalence, na.rm = TRUE)


#removing duplicate 
# Check structure of the dataset
str(plot_data)

# Ensure there are no duplicates
duplicates <- plot_data %>%
  group_by(iso_a3, Year) %>%
  filter(n() > 1)
print(duplicates)

# Check for missing or incorrect values
summary(plot_data)

# Find rows in the merged map_data where Prevalence is NA
unmatched <- map_data %>%
  filter(is.na(Prevalence))

head(unmatched)  # Check if these rows correspond to Somaliland, Kosovo, etc.

map_data <- map_data %>%
  filter(!is.na(Prevalence))

# Find codes in world that do not match countries_data
missing_in_countries_data <- setdiff(world$iso_a3, countries_data$Code)
print(missing_in_countries_data)

map_data <- world %>%
  inner_join(countries_data, by = c("iso_a3" = "Code"))


# Check structure of the dataset
str(plot_data)

# Ensure no duplicate country-year pairs
duplicates <- plot_data %>%
  group_by(iso_a3, Year) %>%
  filter(n() > 1)
print(duplicates)  # This should be empty

# Check for missing prevalence values
missing_prevalence <- plot_data %>%
  filter(is.na(Prevalence))
print(missing_prevalence)  # Check if some years have missing data

# Check the distribution of years and countries
table(plot_data$Year)
table(plot_data$iso_a3)


subset_data <- plot_data %>% filter(Year %in% c(2000, 2005))
subset_data2 <- plot_data %>% filter(Year %in% c(2010, 2020))

subset_data <- plot_data %>% filter(Year %in% c(2000, 2005))
subset_data2 <- plot_data %>% filter(Year %in% c(2010, 2015, 2020))

combined_data <- bind_rows(subset_data, subset_data2)


# Plot using combined data
plot <- plot_ly(
  data = combined_data,
  type = "choropleth",
  locations = ~iso_a3,
  locationmode = "ISO-3",
  z = ~Prevalence,
  frame = ~Year,
  text = ~paste("Country:", Entity, "<br>Prevalence:", Prevalence, "%"),
  colorscale = "Reds",
  zmin = 0,
  zmax = 68.5,
  showscale = TRUE
) %>%
  layout(
    title = "Global Smoking Prevalence (Subset Test)",
    geo = list(
      projection = list(type = "mercator"),
      showcoastlines = TRUE,
      coastlinecolor = "grey"
    )
  )
plot


###change the plot to make it nicer 
plot <- plot_ly(
  data = combined_data,
  type = "choropleth",
  locations = ~iso_a3,
  locationmode = "ISO-3",
  z = ~Prevalence,
  frame = ~Year,
  text = ~paste("Country:", Entity, "<br>Prevalence:", Prevalence, "%"),
  colorscale = list(
    c(0, "#ffeda0"),   # Low prevalence: light orange
    c(0.5, "#feb24c"), # Medium prevalence: orange
    c(1, "#67000d")    # High prevalence: dark red
  ),
  zmin = 0,
  zmax = 68.5,
  showscale = TRUE,
  marker = list(line = list(color = "grey", width = 0.5))  # Border for the countries
) %>%
  layout(
    title = "Global Smoking Prevalence (Subset Test)",
    geo = list(
      projection = list(type = "mercator"),  # Rectangular projection
      showcoastlines = TRUE,                # Show coastlines
      coastlinecolor = "grey",             # Set coastline border color to black
      showcountries = TRUE,                 # Ensure country borders are shown
      countrycolor = "grey",               # Set country border color to black
      showland = TRUE,                      # Show land explicitly
      landcolor = "white",                  # Set land colour to white
      showocean = TRUE,                     # Enable ocean rendering
      oceancolor = "lightblue",             # Set ocean colour to light blue
      showframe = FALSE                     # Optionally remove frame border
    )
  )

# Display the plot
plot



plot <- plot_ly(
  data = combined_data,
  type = "choropleth",
  locations = ~iso_a3,
  locationmode = "ISO-3",
  z = ~Prevalence,
  frame = ~Year,
  text = ~paste(
    "Country:", Entity, 
    ifelse(is.na(Prevalence), "<br>No Data", paste0("<br>Prevalence: ", Prevalence, "%"))
  ),
  colorscale = list(
    c(0, "#ffeda0"),   # Low prevalence: light orange
    c(0.5, "#feb24c"), # Medium prevalence: orange
    c(1, "#67000d")    # High prevalence: dark red
  ),
  zmin = 0,
  zmax = 68.5,
  showscale = TRUE,
  marker = list(line = list(color = "grey", width = 0.5))  # Border for the countries
) %>%
  layout(
    title = "Global Smoking Prevalence (Subset Test)",
    geo = list(
      projection = list(type = "equirectangular"),  # Rectangular projection
      showcoastlines = TRUE,                # Show coastlines
      coastlinecolor = "grey",              # Set coastline border color
      showcountries = TRUE,                 # Ensure country borders are shown
      countrycolor = "grey",                # Set country border color
      showland = TRUE,                      # Show land explicitly
      landcolor = "white",                  # Set land colour to white
      showocean = TRUE,                     # Enable ocean rendering
      oceancolor = "lightblue",             # Set ocean colour to light blue
      showframe = FALSE                     # Optionally remove frame border
    ),
    annotations = list(
      list(
        x = 0.5,                            # Position for the note (to the right of the map)
        y = -0.1,                            # Vertical position (lower part of the map)
        xref = "paper",                     # Reference the x-axis relative to the paper
        yref = "paper",                     # Reference the y-axis relative to the paper
        text = "Note: White regions indicate missing data.", # Your note
        showarrow = FALSE,                  # Disable arrow pointing
        font = list(size = 12, color = "black"), # Font size and color
        align = "left"
      )
    )
  )

# Display the plot
plot



