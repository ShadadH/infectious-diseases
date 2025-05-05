library(here)
library(readr) 
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(scales)

data <- read_rds(here("data", "leprosy_data.rds"))

# Top 10 countries by new cases
top10_latest <- leprosy_merged %>%
  filter(Year == max(Year)) %>%
  arrange(desc(New_Cases)) %>%
  slice_head(n = 10)

ggplot(top10_latest, aes(x = reorder(Country, New_Cases), y = New_Cases)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Countries by New Leprosy Cases (2023)",
       x = "Country", y = "New Cases") +
  theme_minimal()

# Time series: Global trend
global_trend <- leprosy_merged %>%
  group_by(Year) %>%
  summarise(Global_Cases = sum(New_Cases, na.rm = TRUE))

ggplot(global_trend, aes(x = Year, y = Global_Cases)) +
  geom_line(color = "darkred", size = 1.2) +
  labs(title = "Global Leprosy Cases Over Time",
       x = "Year", y = "New Cases") +
  theme_minimal()

# Get top 15 countries by incidence (latest year)
top_incidence <- leprosy_merged %>%
  filter(Year == max(Year)) %>%
  filter(!is.na(Incidence_per_100k)) %>%
  arrange(desc(Incidence_per_100k)) %>%
  slice_head(n = 10)

# Summarize cases by WHO region and year
regional_trend <- leprosy_merged %>%
  group_by(WHO_Region, Year) %>%
  summarise(
    Total_Cases = sum(New_Cases, na.rm = TRUE),
    .groups = "drop"
  )
# Plot multiple lines by region
ggplot(regional_trend, aes(x = Year, y = Total_Cases, color = WHO_Region)) +
  geom_line(size = 1.2) +
  labs(
    title = "Leprosy Cases Over Time by WHO Region",
    x = "Year",
    y = "New Cases",
    color = "WHO Region"
  ) +
  theme_minimal()


# Summarize average incidence rate by region and year
regional_incidence <- leprosy_merged %>%
  group_by(WHO_Region, Year) %>%
  summarise(
    Avg_Incidence = mean(Incidence_per_100k, na.rm = TRUE),
    .groups = "drop"
  )

# Plot incidence trends
ggplot(regional_incidence, aes(x = Year, y = Avg_Incidence, color = WHO_Region)) +
  geom_line(size = 1.2) +
  labs(
    title = "Average Leprosy Incidence per 100k by WHO Region (2000–2023)",
    x = "Year",
    y = "Avg. Incidence per 100k",
    color = "WHO Region"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")


# Plot Top 10 Countries by Incidence
ggplot(top_incidence, aes(x = reorder(Country, Incidence_per_100k), y = Incidence_per_100k)) +
  geom_col(fill = "darkorange") +
  coord_flip() +
  labs(
    title = "Top 10 Countries by Leprosy Incidence per 100k (2023)",
    x = "Country",
    y = "Incidence Rate"
  ) +
  theme_minimal()

#Plot World Map Data --------------------------------

world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  select(name, iso_a3, geometry)

leprosy_map_data <- leprosy_merged %>%
  filter(Year == 2023) %>%
  select(Country_Code, New_Cases, Incidence_per_100k)

world_leprosy <- world %>%
  left_join(leprosy_map_data, by = c("iso_a3" = "Country_Code"))

ggplot(world_leprosy) +
  geom_sf(aes(fill = New_Cases), color = "grey50") +
  scale_fill_viridis_c(option = "inferno", trans = "log", na.value = "lightgrey") +
  labs(title = "New Leprosy Cases by Country (Latest Year)",
       fill = "Cases (log)") +
  theme_minimal()

# Zoom in on Asia

ggplot(world_leprosy) +
  geom_sf(aes(fill = New_Cases), color = "grey50") +
  scale_fill_viridis_c(
    option = "inferno",
    trans = "log",                  # log scale to manage skew
    na.value = "lightgrey",
    breaks = c(1, 10, 100, 1000, 10000, 80000),
    labels = scales::comma
  ) +
  coord_sf(xlim = c(60, 150), ylim = c(-15, 55), expand = FALSE) +
  labs(
    title = "New Leprosy Cases in Asia (2023)",
    fill = "New Cases (log scale)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold")
  )

# Find potential outbreaks using year-over-year % change for each country ------------

leprosy_growth <- leprosy_merged %>%
  arrange(Country, Year) %>%
  group_by(Country) %>%
  mutate(
    YoY_Change = (New_Cases - lag(New_Cases)) / lag(New_Cases) * 100
  ) %>%
  ungroup()

potential_outbreaks <- leprosy_growth %>%
  filter(Year >= 2018, YoY_Change > 50, New_Cases > 50)  # add a threshold to ignore noise

# View top results
potential_outbreaks <- leprosy_growth %>%
  filter(Year >= 2018, YoY_Change > 50, New_Cases > 50, is.finite(YoY_Change))

top_yoy_spikes <- potential_outbreaks %>%
  arrange(desc(YoY_Change)) %>%
  slice_head(n = 5)

ggplot(top_yoy_spikes, aes(x = reorder(Country, YoY_Change), y = YoY_Change)) +
  geom_col(fill = "firebrick") +
  geom_text(aes(label = paste0(round(YoY_Change), "%")), 
            hjust = -0.1, size = 4) +
  coord_flip() +
  scale_y_continuous(
    labels = percent_format(scale = 1),
    expand = expansion(mult = c(0, 0.1))  # increased from 0.1 to 0.25
  )+
labs(
    title = "Top 5 Countries by YoY Increase in Leprosy Cases (Since 2018)",
    subtitle = "Filtered to >50% increase and at least 50 cases",
    x = "Country",
    y = "Year-over-Year Change (%)"
  ) +
  theme_minimal()

