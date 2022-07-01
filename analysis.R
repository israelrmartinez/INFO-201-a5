library(dplyr)
library(lubridate)
library(ggplot2)
library(leaflet)

shootings_2018 <- read.csv("data/shootings-2018.csv", stringsAsFactors = FALSE)
View(shootings_2018)

# Summary Information ---------------------------------------------------------
# How many shooting events occurred?
shooting_events <- shootings_2018 %>%
  # Counts number of rows (shooting events).
  nrow()

# How many lives were lost?
lives_lost <- shootings_2018 %>%
  # Select the num_killed column.
  select(num_killed) %>%
  # Calculates sum of entire row.
  sum()

# Which city was most impacted by shootings (cities with the highest number of
# num_killled and num_injured)?
cty_most_impact <- shootings_2018 %>%
  # Group all rows of the same city.
  group_by(city) %>%
  # Calculate the total number killed and injured in each city.
  summarize(
    num_shoot = sum(num_killed, num_injured)
  ) %>%
  # City with the highest number of shootings.
  filter(num_shoot == max(num_shoot)) %>%
  # Name of the city.
  select(city)

# Which month had the most shootings (killed and injured)?
month_most_shoot <- shootings_2018 %>%
  # Adds column indicating the month of the shooting.
  mutate(
    month = months(as.Date(date, format = "%B %d, %Y"))
  ) %>%
  # Group all rows of the same month.
  group_by(month) %>%
  # Calculate the total number killed and injured in each month.
  summarize(
    num_shoot = sum(num_killed, num_injured)
  ) %>%
  # Month with the highest amount of shootings.
  filter(num_shoot == max(num_shoot)) %>%
  # Name of the month.
  select(month)

# Which state had the highest rate of killed shootings?
state_kill_rate <- shootings_2018 %>%
  # Group all rows of the same state.
  group_by(state) %>%
  # Calculate the total number killed and injured in each state. Also,
  # calculates the percentage of people killed from shooting in the state.
  summarize(
    num_killed = sum(num_killed),
    num_injured = sum(num_injured),
    per_killed = round((num_killed / (num_killed + num_injured)), 3)
  ) %>%
  # State with the highest killed shootings
  filter(per_killed == max(per_killed)) %>%
  # Name of state.
  select(state)



# Summary table ---------------------------------------------------------

# Summary table where number of shootings are grouped by state.
summary_table <- shootings_2018 %>%
  group_by(state) %>%
  summarize(
    casualties = sum(num_killed, num_injured)
  )



# Description of a particular incident ----------------------------------
# The event with the most shootings.
worst_incident <- shootings_2018 %>%
  # Adds 'shootings' column by combining numbers of killed and injured.
  mutate(
    casualties = (num_killed + num_injured)
  ) %>%
  # Filter to incident with the maximum number of shootings.
  filter(casualties == max(casualties))


# Add column 'casualties' and column 'month'
more_column <- shootings_2018 %>%
  mutate(
    casualties = (num_killed + num_injured),
    month = months(as.Date(date, format = "%B %d, %Y"))
    )
