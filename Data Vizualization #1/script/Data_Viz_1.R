# Load necessary libraries with Pacman

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  ggplot2,
  dplyr,
  readr,
  tidyverse,
  here
)

# Load the datasets
cases <- read_csv(here("Data Vizualization #1" ,"data","cases.csv"))
cc_study <- read_csv(here("Data Vizualization #1" ,"data","cc-study.csv"), skip = 1)

# ----- Question 1: Identify the Index Case -----
index_case_week <- min(cases$week[cases$cases > 0])
cat("The first detected case occurred in week", index_case_week, "\n")

# ----- Question 2: Plot the Epidemic Curve -----
ggplot(cases, aes(x = week, y = cases)) +
  geom_histogram(stat = "identity", fill = "red", color = "black", binwidth = 1) +
  scale_x_continuous(breaks = seq(min(cases$week), max(cases$week), by = 5)) +
  labs(title = "Epidemic Curve of Cholera Cases",
       x = "Week",
       y = "Number of Cases") +
  theme_minimal()

# Find the peak of the epidemic
peak_week <- cases$week[which.max(cases$cases)]
cat("The epidemic peaked at week", peak_week, "with the highest number of cases.\n")

# ----- Question 3: Calculate Case Fatality Rate (CFR) -----
total_cases <- sum(cases$cases)
total_deaths <- sum(cases$deaths)
CFR <- (total_deaths / total_cases) * 100
cat("The case fatality rate (CFR) is", round(CFR, 2), "%\n")

# ----- Question 4: Impact of Treatment Center -----
pre_treatment_deaths <- sum(cases$deaths[cases$week < 12])
post_treatment_deaths <- sum(cases$deaths[cases$week >= 12])
cat("Deaths before treatment center opened:", pre_treatment_deaths, "\n")
cat("Deaths after treatment center opened:", post_treatment_deaths, "\n")

# Compare CFR before and after treatment center
pre_CFR <- (pre_treatment_deaths / sum(cases$cases[cases$week < 12])) * 100
post_CFR <- (post_treatment_deaths / sum(cases$cases[cases$week >= 12])) * 100
cat("CFR before treatment center:", round(pre_CFR, 2), "%\n")
cat("CFR after treatment center:", round(post_CFR, 2), "%\n")

# ----- Question 5: Identifying Risk Factors -----
# Convert categorical variables to factors
cc_study <- cc_study %>%
  mutate(
    disease_status = as.factor(`disease_status`),
    drinking_water = as.factor(drinking_water),
    diarrhea_patient_contact = as.factor(diarrhea_patient_contact),
    food_source = as.factor(food_source),
    waster_disposal = as.factor(waster_disposal),
    travel = as.factor(travel),
    gathering = as.factor(gathering)
    
  )

univariate_logistic_table <- 
  cc_study |>
  select(disease_status, drinking_water, diarrhea_patient_contact, 
         food_source, waster_disposal, travel, gathering) |>
  tbl_uvregression(
    method = glm,
    y = disease_status,
    method.args = list(family = binomial),
    exponentiate = TRUE, # Convert log-odds to Odds Ratios (OR)
    label = list(
      drinking_water ~ "Drinking Water Source",
      diarrhea_patient_contact ~ "Contact with Diarrhea Patient",
      food_source ~ "Food Source",
      waster_disposal ~ "Waste Disposal Method",
      travel ~ "Recent Travel",
      gathering ~ "Participation in Gatherings"
    )
  ) |>
  add_n(location = "level") |> # Add number of observations
  add_nevent(location = "level") # Add number of events (cases)

# Print the formatted table
univariate_logistic_table


# ----- Question 7: Effect of Chlorine Distribution -----
chlorine_start_week <- 27
cases_before_chlorine <- sum(cases$cases[cases$week < chlorine_start_week])
cases_after_chlorine <- sum(cases$cases[cases$week >= chlorine_start_week])
cat("Cases before chlorine distribution:", cases_before_chlorine, "\n")
cat("Cases after chlorine distribution:", cases_after_chlorine, "\n")

# Plot cases trend before and after chlorine intervention
ggplot(cases, aes(x = week, y = cases)) +
  geom_line(color = "blue", size = 1) +  # Line plot for case trend
  geom_vline(xintercept = chlorine_start_week, linetype = "dashed", color = "red", size = 1) +  # Dashed red line for intervention
  annotate("text", x = chlorine_start_week + 2, y = max(cases) * 0.9,  # Position label slightly to the right and below max cases
           label = "Chlorine Distribution Starts", color = "red", angle = 90, vjust = -0.5, size = 5) +
  labs(title = "Cholera Cases Before and After Chlorine Distribution",
       x = "Week",
       y = "Number of Cases") +
  theme_minimal()
