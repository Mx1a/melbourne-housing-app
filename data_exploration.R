
# ---- Load packages and data ----

library(tidyverse)
housing <- read_csv("Melbourne_housing_FULL.csv", na = c("", "NA", "#N/A"))
glimpse(housing)

# ---- Clean data ----

housing_clean <- housing |>
  filter(!is.na(Price), !is.na(Regionname)) |>
  mutate(
    Type = factor(Type,
                  levels = c("h", "t", "u"),
                  labels = c("House", "Townhouse", "Unit")),
    Method = case_when(
      Method %in% c("S", "SP", "SA", "SS", "PN", "SN") ~ "Sold",
      Method == "PI" ~ "Passed In",
      Method == "VB" ~ "Vendor Bid",
      Method == "W"  ~ "Withdrawn"
    ),
    Method = factor(Method),
    Regionname = if_else(
      Regionname %in% c("Eastern Victoria", "Northern Victoria", "Western Victoria"),
      "Regional Victoria",
      Regionname
    ),
    Regionname = factor(Regionname),
    Date = dmy(Date),
    SaleYear = year(Date)
  ) |>
  select(Suburb, Type, Method, Regionname, CouncilArea, Date, SaleYear,
         Price, Rooms, Bathroom, Car, Distance, Landsize, BuildingArea,
         YearBuilt, Lattitude, Longtitude)

# ---- Checking filters worked ---

glimpse(housing_clean)
count(housing_clean, Type)
count(housing_clean, Method)
count(housing_clean, Regionname)


# ---- Contingency tables ----

# One-way tables
table(housing_clean$Type)
table(housing_clean$Regionname)

# Two-way tables
table(housing_clean$Type, housing_clean$Regionname)
table(housing_clean$Method, housing_clean$Type)

# ---- Numeric summaries ----

# Price by property type
housing_clean |>
  group_by(Type) |>
  summarize(
    n = n(),
    mean_price = mean(Price),
    median_price = median(Price),
    sd_price = sd(Price)
  )

# Distance and building area by region

housing_clean |>
  group_by(Regionname) |>
  summarize(
    median_price = median(Price),
    mean_distance = mean(Distance, na.rm = TRUE),
    median_building_area = median(BuildingArea, na.rm = TRUE)
  )

# ---- Plots ---

# Plot 1: Bar chart of property type
ggplot(housing_clean, aes(x = Type)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Number of Sales by Property Type",
       x = "Property Type", y = "Number of Sales")

# Plot 2: Side-by-side bar chart of property type by region
ggplot(housing_clean, aes(x = Regionname, fill = Type)) +
  geom_bar(position = "dodge") +
  labs(title = "Property Type Sales by Region",
       x = "Region", y = "Number of Sales", fill = "Property Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 3: Histogram of price colored by property type
ggplot(housing_clean, aes(x = Price, fill = Type)) +
  geom_histogram(bins = 50, alpha = 0.6, position = "identity") +
  scale_x_log10(labels = scales::label_dollar()) +
  labs(title = "Distribution of Sale Prices by Property Type",
       x = "Sale Price (log scale)", y = "Count", fill = "Property Type")

# Plot 4: Boxplot of price by region and property type
ggplot(housing_clean, aes(x = Regionname, y = Price, fill = Type)) +
  geom_boxplot() +
  scale_y_log10(labels = scales::label_dollar()) +
  labs(title = "Sale Price by Region and Property Type",
       x = "Region", y = "Sale Price (log scale)", fill = "Property Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 5: Scatter plot of price vs distance, faceted by region
ggplot(housing_clean, aes(x = Distance, y = Price, color = Type)) +
  geom_point(alpha = 0.3, size = 0.8) +
  scale_y_log10(labels = scales::label_dollar()) +
  facet_wrap(~ Regionname) +
  guides(color = guide_legend(override.aes = list(size = 3, alpha = 1))) +
  labs(title = "Sale Price vs Distance from Melbourne CBD by Region",
       x = "Distance from CBD (km)", y = "Sale Price (log scale)",
       color = "Property Type")
