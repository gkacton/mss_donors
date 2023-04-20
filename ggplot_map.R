# This script produces a choropleth map using ggplot2
# The counties of Maine are shaded according to their total donation to the
  # Maine State Seminary between 1855 and 1857.

county_map <- ggplot(county_data_merged) +
  geom_sf(aes(fill = total)) +
  scale_fill_gradient(low = "#edf8e9",
                      high = "#006d2c") +
  theme_minimal() +
  labs(title = "Total Donation by County",
       fill = "Total")

county_map

