# Data visualisation code  

# Packages to install and load
packages <- c("tidyverse", "ggplot2", "tidyr", "dplyr", "plotly", "rnaturalearth", "rnaturalearthdata", "sf", "htmlwidgets")

# Function to install packages and load them
install_and_load <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package, dependencies = TRUE)
      library(package, character.only = TRUE)
    } else {
      library(package, character.only = TRUE)
    }
  }
}

# Run function
install_and_load(packages)

# Load raw data / replace "data/smoking.csv" with the path to CVS file. 
rawdata <- read.csv("data/smoking.csv")

#sanity check 
str(rawdata)           # structure and summary
summary(rawdata)
head(rawdata)          # Check first rows
dim(rawdata)           # Check dimensions
colSums(is.na(rawdata))# Check missing values

# Check for duplicates
duplicates <- rawdata[duplicated(rawdata), ]
print(duplicates)

# Unique values in key columns
unique(rawdata$Entity)
unique(rawdata$Year)

# Cleaning data 
# Remove specific entities
countries_data <- rawdata[!rawdata$Entity %in% c("East Asia and Pacific (WB)", "Sub-Saharan Africa (WB)", 
                                                 "Upper-middle-income countries", "Europe and Central Asia (WB)", 
                                                 "World", "European Union (27)", "Low-income countries", 
                                                 "Lower-middle-income countries", "Middle East and North Africa (WB)", 
                                                 "Middle-income countries", "North America (WB)", "South Asia (WB)", 
                                                 "Latin America and Caribbean (WB)", "High-income countries"), ]

# Exclude specific years
countries_data <- countries_data %>%
  filter(!(Year %in% c(2018, 2019)))

# Rename column for ease
countries_data <- countries_data %>%
  rename(Prevalence = Prevalence.of.current.tobacco.use....of.adults.)

# Load world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

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

# Merge datasets, sanity check/clean merged data 
# Merge datasets
map_data <- world %>%
  left_join(countries_data, by = c("iso_a3" = "Code"))

# Replace NA prevalence values with 0
map_data <- map_data %>%
  mutate(Prevalence = ifelse(is.na(Prevalence), 0, Prevalence))

# Remove duplicates
map_data <- map_data[!duplicated(map_data), ]

# Remove geometry for Plotly compatibility
plot_data <- map_data %>%
  st_set_geometry(NULL)

# Structure and summary of cleaned dataset
str(plot_data)
summary(plot_data)

# Check for duplicates country-year pairs
duplicates <- plot_data %>%
  group_by(iso_a3, Year) %>%
  filter(n() > 1)
print(duplicates)

# Check for missing prevalence values
missing_prevalence <- plot_data %>%
  filter(is.na(Prevalence))
print(missing_prevalence)

# Distribution of years and countries
table(plot_data$Year)
table(plot_data$iso_a3)

# first visualisation
ggplot(data = map_data) +
  geom_sf(aes(fill = Prevalence), color = "white", size = 0.2) +
  scale_fill_gradient(low = "blue", high = "red", na.value = "grey") +
  labs(title = "Global Smoking Prevalence Heatmap",
       fill = "Prevalence (%)") +
  theme_minimal()


#Save to file
ggsave("plots/initial_visualisation.png", width = 10, height = 6, dpi = 300)


# Function to create an interactive plot for a specific year

# Create a list of subsets by year
yearly_subcategories <- split(plot_data, plot_data$Year)

create_interactive_year_plot <- function(data, year) {
  plot <- ggplot(data = data %>% filter(Year == year)) +
    geom_sf(aes(fill = Prevalence, text = paste("Country:", Entity, "<br>Prevalence:", Prevalence, "%")),
            size = 0.2) +  # Removed black borders
    scale_fill_gradient(low = "lightyellow", high = "darkred", na.value = "white", name = "Prevalence (%)") +
    labs(
      title = paste("Global Smoking Prevalence -", year),
      fill = "Prevalence (%)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16),
      legend.position = "bottom",
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank()
    )
  
  # Convert ggplot to interactive plotly object with hover text
  ggplotly(plot, tooltip = c("text"))
}

# Create interactive visualisations for each year
interactive_plot_2000 <- create_interactive_year_plot(map_data, 2000)
interactive_plot_2005 <- create_interactive_year_plot(map_data, 2005)
interactive_plot_2010 <- create_interactive_year_plot(map_data, 2010)
interactive_plot_2015 <- create_interactive_year_plot(map_data, 2015)
interactive_plot_2020 <- create_interactive_year_plot(map_data, 2020)

# Save yearly interactive plots
years <- c(2000, 2005, 2010, 2015, 2020)
plots <- list(interactive_plot_2000, interactive_plot_2005, interactive_plot_2010, interactive_plot_2015, interactive_plot_2020)

for (i in seq_along(years)) {
  htmlwidgets::saveWidget(plots[[i]], paste0("plots/interactive_plot_", years[i], ".html"))
}



# Combine all yearly subcategories back into a single dataset
combined_data_year <- bind_rows(yearly_subcategories)

# Enhanced plot
final_plot <- plot_ly(
  data = combined_data_year,
  type = "choropleth",
  locations = ~iso_a3,
  locationmode = "ISO-3",
  z = ~Prevalence,
  frame = ~Year,
  text = ~ifelse(
    is.na(Prevalence),
    NA,  
    paste("Country:", Entity, "<br>Prevalence:", Prevalence, "%")  # Show prevalence for available data
  ),
  hoverinfo = "text",  # Show only custom hover text
  colorscale = list(
    list(0, "#ffeda0"),   # Light yellow for low prevalence
    list(0.2, "#feb24c"), # Light orange
    list(0.4, "#fd8d3c"), # Orange
    list(0.6, "#fc4e2a"), # Reddish-orange
    list(0.8, "#e31a1c"), # Red
    list(1, "#67000d")    # Dark red for high prevalence
  ),
  zmin = 0,
  zmax = 70,  # Adjusted prevalence
  showscale = TRUE,
  marker = list(line = list(color = "grey", width = 0.5))  # Border for countries
) %>%
  layout(
    title = "Global Smoking Prevalence in Adults (2000-2020)",
    geo = list(
      projection = list(type = "equirectangular"),  # Rectangular projection
      showcoastlines = TRUE,
      coastlinecolor = "grey",                     # Coastline color
      showcountries = TRUE,                        # country borders
      countrycolor = "grey",                       # Country border color
      showland = TRUE,                             # Highlight land 
      landcolor = "white",                         # Land color
      showocean = TRUE,                            # Highlight oceans
      oceancolor = "lightblue",                    # Ocean color
      showframe = FALSE                            # Remove map frame
    ),
    annotations = list(
      # Note annotation
      list(
        x = 0.5,                                   # Horizontal 
        y = -0.1,                                  # Vertical  (below the map)
        xref = "paper",                            # x-axis
        yref = "paper",                            # y-axis
        text = "Note: White regions indicate missing data.", # Annotation 
        showarrow = FALSE,                         # No arrow
        font = list(size = 12, color = "black"),   # Font 
        align = "left"
      ),
      # Source annotation
      list(
        x = 0.5,                                   # Horizontal 
        y = -0.15,                                 # Vertical (below the map)
        xref = "paper",                            # x-axis
        yref = "paper",                            # y-axis
        text = "Source: Multiple sources compiled by World Bank (2024) – processed by Our World In Data",
        showarrow = FALSE,                         # No arrow
        font = list(size = 10, color = "black"),   # Font
        align = "center"
      )
    )
  )

# Show  plot
final_plot 


#Save plot
htmlwidgets::saveWidget(final_plot, "plots/final_visualisation.html")
