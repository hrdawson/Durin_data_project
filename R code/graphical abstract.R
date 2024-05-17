DURIN.abstract = DURIN.lit |>
  filter(trait %in% c("SLA (cm^2/g)", "LDMC (mg/g)")) |>
  mutate(leaf_age_new = case_when(
    leaf_age == "database" ~ "meta-analysis",
    database %in% c("Literature", "LEDA", "TRY", "TTT") ~ "meta-analysis",
    database == "DURIN" ~ leaf_age
  ),
  leaf_age_new = factor(leaf_age_new, levels = c("current", "previous", "meta-analysis"),
                    labels = c("current", "previous", "meta-analysis")),
  trait = factor(trait, levels = c("SLA (cm^2/g)", "LDMC (mg/g)")))

abstract.VV =
  ggplot(DURIN.abstract |>
           filter(species == "Vaccinium vitis-idaea"),
         aes(x = leaf_age_new, y = value, fill = leaf_age_new, color = leaf_age_new)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5) +
  geom_point(position = position_jitterdodge(), alpha = 0.5) +
  scale_x_discrete(guide = "axis_nested") +
  scale_fill_viridis(discrete=T) +
  scale_color_viridis(discrete=T) +
  # scale_y_log10() +
  facet_nested(~ species + trait, scales = "free", independent = "y",
               nest_line = element_line(linetype = 2)) +
  labs(x = "", y = "") +
  theme_bw() +
  theme(strip.background = element_blank(),
        ggh4x.facet.nestline = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
        legend.position = "none")

abstract.EN =
  ggplot(DURIN.abstract |>
           filter(species == "Empetrum nigrum"),
         aes(x = leaf_age_new, y = value, fill = leaf_age_new, color = leaf_age_new)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.5) +
  geom_point(position = position_jitterdodge(), alpha = 0.5) +
  scale_x_discrete(guide = "axis_nested") +
  scale_fill_viridis(discrete=T) +
  scale_color_viridis(discrete=T) +
  # scale_y_log10() +
  facet_nested(~ species + trait, scales = "free", independent = "y",
               nest_line = element_line(linetype = 2)) +
  labs(x = "", y = "") +
  theme_bw() +
  theme(strip.background = element_blank(),
        ggh4x.facet.nestline = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
        legend.position = "none")

abstract.VV + abstract.EN

ggsave("visualizations/2024.05.16_TraitsTogether_Abstract.png", width = 8, height = 6, units = "in")
