library(dplyr)
library(leaflet)
library(htmltools)
library(plotly)
library(styler)
library(lintr)

# SUMMARY INFO
shootings_df <- read.csv("data/shootings-2018.csv", stringsAsFactors = FALSE)

num_shootings <- nrow(shootings_df)

num_deaths <- sum(shootings_df$num_killed)

# Impact measured as total killed and injured
most_impact_city <- shootings_df %>%
  mutate(impact = num_killed + num_injured) %>%
  mutate(city_state = paste0(city, ", ", state)) %>%
  group_by(city_state) %>%
  summarize(total_impact = sum(impact)) %>%
  filter(total_impact == max(total_impact)) %>%
  pull(city_state)

chicago_impact <- shootings_df %>%
  mutate(impact = num_killed + num_injured) %>%
  group_by(city) %>%
  summarize(total_impact = sum(impact)) %>%
  filter(total_impact == max(total_impact)) %>%
  pull(total_impact)

# Insight #1: Most impacted state
most_impact_state <- shootings_df %>%
  mutate(impact = num_killed + num_injured) %>%
  group_by(state) %>%
  summarize(total_impact = sum(impact)) %>%
  filter(total_impact == max(total_impact)) %>%
  pull(state)

california_impact <- shootings_df %>%
  mutate(impact = num_killed + num_injured) %>%
  group_by(state) %>%
  summarize(total_impact = sum(impact)) %>%
  filter(total_impact == max(total_impact)) %>%
  pull(total_impact)

# Insight #2: State with most shootings
highest_freq_states <- shootings_df %>%
  group_by(state) %>%
  summarize(total = n()) %>%
  filter(total == max(total)) %>%
  pull(state)

# SUMMARY TABLE
shootings_state_df <- shootings_df %>%
  select(state, num_killed, num_injured) %>%
  group_by(state) %>%
  summarize(
    total_killed = sum(num_killed),
    total_injured = sum(num_injured)
  ) %>%
  arrange(desc(total_killed))

# INCIDENT REPORT
date <- shootings_df %>%
  filter(address == "635 S Clinton Ave") %>%
  pull(date)

location <- shootings_df %>%
  filter(address == "635 S Clinton Ave") %>%
  mutate(location = paste0(city, ", ", state)) %>%
  pull(location)

address <- shootings_df %>%
  filter(address == "635 S Clinton Ave") %>%
  pull(address)

longitude <- shootings_df %>%
  filter(address == "635 S Clinton Ave") %>%
  pull(long)

latitude <- shootings_df %>%
  filter(address == "635 S Clinton Ave") %>%
  pull(lat)

deaths <- shootings_df %>%
  filter(address == "635 S Clinton Ave") %>%
  pull(num_killed)

injuries <- shootings_df %>%
  filter(address == "635 S Clinton Ave") %>%
  pull(num_injured)

# INTERACTIVE MAP
# Labels
labs <- paste0(
  "Location: ", shootings_df$city, ", ", shootings_df$state, "<br>",
  "Deaths: ", shootings_df$num_killed, "<br>",
  "Injuries: ", shootings_df$num_injured
)

# Shootings dataframe with impact column
shootings_df_impact <- shootings_df %>%
  mutate(impact = num_killed + num_injured)

shootings_map <- leaflet(data = shootings_df_impact) %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(lng = -95.71, lat = 37.09, zoom = 4) %>%
  addCircleMarkers(
    lat = ~lat,
    lng = ~long,
    radius = ~impact,
    label = lapply(labs, htmltools::HTML)
  )

# PLOT OF CHOICE (scatter plot)
# Function to determine season
seasons_2018 <- function(date) {
  date <- as.Date(date, format = "%Y-%m-%d")
  spring <- as.Date("2018-03-21", format = "%Y-%m-%d")
  summer <- as.Date("2018-06-21", format = "%Y-%m-%d")
  autumn <- as.Date("2018-09-21", format = "%Y-%m-%d")
  winter <- as.Date("2018-12-21", format = "%Y-%m-%d")
  if (date >= spring && date < summer) {
    season <- "Spring"
  }
  else if (date >= summer && date < autumn) {
    season <- "Summer"
  }
  else if (date >= autumn && date < winter) {
    season <- "Autumn"
  }
  else if (date >= winter || date < spring) {
    season <- "Winter"
  }
  return(season)
}

seasons_2018 <- Vectorize(seasons_2018)

# Shootings dataframe with season column
shootings_df_date <- shootings_df %>%
  mutate(rdate = as.Date(date, "%B %d, %Y")) %>%
  group_by(rdate) %>%
  summarize(frequency = n()) %>%
  mutate(seasons = seasons_2018(rdate))

freq_by_date_plot <- plot_ly(
  data = shootings_df_date,
  x = ~rdate,
  y = ~frequency,
  color = ~seasons,
  colors = c("darkorange", "deeppink", "limegreen", "deepskyblue"),
  type = "scatter",
  mode = "markers",
  alpha = 0.8
) %>%
  layout(
    title = "Daily Frequency of Mass Shootings by Season",
    xaxis = list(title = "Date", tickformat = "%b"),
    yaxis = list(title = "Frequency")
  )
