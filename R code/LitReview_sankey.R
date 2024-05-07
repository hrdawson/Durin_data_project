# devtools::install_github("davidsjoberg/ggsankey")
library(ggsankey)

LitReview.sankey = LitReview.leaves.traits |>
  # Temp filtering while dealing with the main fig
  filter(variable %in% c("leaf year", "justification")) |>
  # Select only needed columns
  select(Key, species, variable, value) |>
  # Pivot to meet function needs
  pivot_wider(names_from = variable, values_from = value) |>
  # Fill NA justifications with unspecified
  mutate(justification = replace_na(justification, "unspecified"))  |>
  mutate(justification = case_when(
    is.na(justification) ~ "unspecified",
    justification == "none" ~ "unspecified",
    TRUE ~ justification
  )) |>
  rename(leaf_year = 'leaf year') |>
  mutate(leaf_year = factor(leaf_year, levels = c("both (concurrent)", "both (mixed)", "both (alternate)", "current", "previous", "unspecified")),
         justification = factor(justification, levels = c("representation of the plant", "both were present",
                                                         "testing physiology by age", "appearance", "current leaves too young",
                                                         "only current leaves available", "missing other cohort",
                                                         "experimental constraints", "standardization", "data quality", "none", "unspecified")))

LitReview.sankey.VV = LitReview.sankey |>
  filter(species == "Vaccinium vitis-idaea") |>
  make_long(justification, leaf_year) |>
  mutate(species = "Vaccinium vitis-idaea")

sankey.VV =
ggplot(LitReview.sankey.VV, aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = factor(node), label = node)) +
  geom_alluvial(flow.alpha = .6) +
  geom_alluvial_text(size = 3, color = "grey20") +
  scale_fill_viridis_d(drop = FALSE) +
  theme_alluvial(base_size = 18) +
  labs(x = NULL) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = .5)) +
  ggtitle("Vaccinium vitis-idaea") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

LitReview.sankey.EN = LitReview.sankey |>
  filter(species == "Empetrum nigrum") |>
  make_long(leaf_year, justification) |>
  mutate(species = "Empetrum nigrum")

Lit.Review.sankey = LitReview.sankey.EN |>
  bind_rows(LitReview.sankey.VV)

sankey.EN =
ggplot(LitReview.sankey.EN, aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = factor(node), label = node)) +
  geom_alluvial(flow.alpha = .6) +
  geom_alluvial_text(size = 3, color = "grey20") +
  scale_fill_viridis_d(drop = FALSE) +
  theme_alluvial(base_size = 18) +
  labs(x = NULL) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = .5)) +
  ggtitle("Empetrum nigrum") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

old_par <- par(mar = c(0, 2, 0, 0), bg = NA)

sankey.EN + sankey.VV +
  plot_layout(axes = "collect")
  wrap_elements(panel = ~plot(sankey.EN, sankey.VV), clip = FALSE)
par(old_par)

LitReview.sankey = LitReview.sankey.EN |>
  bind_rows(LitReview.sankey.VV)

ggplot(LitReview.sankey, aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = factor(node), label = node)) +
  geom_alluvial(flow.alpha = .6) +
  geom_alluvial_text(size = 3, color = "grey20") +
  scale_fill_viridis_d(drop = FALSE) +
  theme_alluvial(base_size = 18) +
  facet_grid2(~species, scale = "free_y", independent = "y") +
  labs(x = NULL) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = .5)) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
ggsave("visualizations/2024.05.06_LitReviewSankey.png", width = 12, height = 8, units = "in")
