# Load required packages
library(here)
library(readr) 
library(dplyr)
library(tidyr)
library(stringr)

# Confirm working directory
here::here()

# Load your dataset from the data folder
Population <- read_csv(here("data", "population.csv"), skip = 3)
disease <- read_csv(here("data", "leprosy.csv"))


#Clean Leprosy Data 
leprosy_clean <- disease %>%
  select(
    Country = Location,
    Country_Code = SpatialDimValueCode,
    WHO_Region = ParentLocation,
    Year = Period,
    New_Cases = Value
  ) %>%
  mutate(
    Year = as.integer(Year),
    New_Cases = as.numeric(New_Cases)
  ) %>%
  filter(!is.na(Country_Code), !is.na(Year), !is.na(New_Cases))

glimpse(leprosy_clean)

#Clean Population Data
aggregates <- c("World", "income", "OECD", "Arab World", "region", "Euro area", "IDA", "IBRD")
pop_clean <- Population %>%
  filter(!str_detect(`Country Name`, str_c(aggregates, collapse = "|")))

pop_long <- pop_clean %>%
  filter(!str_detect(`Country Name`, str_c(aggregates, collapse = "|"))) %>%
  select(Country_Code = `Country Code`, matches("^20\\d{2}$")) %>%
  pivot_longer(
    cols = -Country_Code,
    names_to = "Year",
    values_to = "Population"
  ) %>%
  mutate(
    Year = as.integer(Year),
    Population = as.numeric(Population)
  ) %>%
  filter(!is.na(Population))


glimpse(pop_long)
head(pop_long)

#Merge Data

leprosy_merged <- leprosy_clean %>%
  left_join(pop_long, by = c("Country_Code", "Year")) %>%
  mutate(
    Incidence_per_100k = round((New_Cases / Population) * 100000, 2)
  )
head(leprosy_merged)

saveRDS(leprosy_merged, file = "/Users/macbookair/infectious-diseases/Final Project/Data/leprosy_data.rds")

