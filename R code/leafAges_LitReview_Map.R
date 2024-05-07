# Made following the instructions here: https://www.datanovia.com/en/blog/how-to-create-a-map-using-ggplot2/#world-map

library(ggplot2)
library(dplyr)
require(maps)
require(viridis)
theme_set(
  theme_void()
)

world_map <- map_data("world") |>
  add_column(studies2 = NA)

# Bring over the literature count data

LitReview.leaves.countryCount = LitReview.leaves.tagCount |>
  filter(variable == "location") |>
  # Remove the unknowns
  filter(!(value %in% c("unspecified", "greenhouse", "Eurasia"))) |>
  # Arrange data sensibly
  rename(region = value, studies = n) |>
  select(-variable)

LitReview.map <- left_join(LitReview.leaves.countryCount, world_map, by = "region") |>
  mutate(studies3 = coalesce(studies, studies2))

ggplot(LitReview.map, aes(long, lat, group = group))+
  geom_polygon(inherit.aes = FALSE, data = world_map, aes(long, lat, group = group), fill = "grey80") +
  geom_polygon(aes(fill = studies ))+
  facet_grid(species~.) +
  scale_fill_viridis(option = "G", direction = -1) +
  labs(x = "", y = "") +
  theme_minimal()

ggsave("visualizations/2024.05.07_LitReview_StudyLocationMap.png", width = 5, height = 5, units = "in")

