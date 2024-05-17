# Plant means -----
## Calculate based on age group ----

trait.avg = function(data, leaf.age) {
  data |>
  # Leaf age
  filter(leaf_age == leaf.age) |>
  # Assign individual plant numbers
  mutate(plantID.unique = paste0(DURIN_plot, "_plant", plant_nr)) |>
  # Calculate averages
  group_by(DURIN_plot, species, habitat, plantID.unique, leaf_age, trait) |>
  # get_summary_stats_precise(type = "mean") |>
  summarize(wt.mean = mean(value, na.rm = TRUE)) |>
  select(trait, wt.mean) |>
  # Add ID
  mutate(group = leaf.age)
}

durin.traitAvg.prev = trait.avg(leaf.ages, "old")
durin.traitAvg.curr = trait.avg(leaf.ages, "young")

## Calculate weights ----
## Dummy data for now ##
set.seed(2023)

durin.avgWt = durin.traitAvg.prev |>
  ungroup() |>
  select(plantID.unique, species) |>
  distinct() |>
  # weights
  mutate(avg = 0.5,
         young = case_when(
           species == "Empetrum nigrum" ~ runif(n(), 0.1, 0.25),
           species == "Vaccinium vitis-idaea" ~ runif(n(), 0.7, 0.9)),
         old = 1 - young
  ) |>
  pivot_longer(cols = c(young, old), names_to = "leaf_age", values_to = "wt")

trait.avg.wt = function(data, leaf.age, weight){
  data |>
    # Assign individual plant numbers
    mutate(plantID.unique = paste0(DURIN_plot, "_plant", plant_nr)) |>
  # Calculate averages
  group_by(DURIN_plot, species, habitat, plantID.unique, leaf_age, trait) |>
  # get_summary_stats_precise(type = "mean") |>
  summarize(mean = mean(value, na.rm = TRUE)) |>
    # Add in weights
    left_join(durin.avgWt) |>
  # calculate weighted means
  mutate(wt.mean.partial = mean * {{ weight }}) |>
  select(trait, wt.mean.partial) |>
  pivot_wider(names_from = leaf_age, values_from = wt.mean.partial) |>
  # sum weighted means
  mutate(wt.mean = young + old,
         group = leaf.age) |>
  select(-c(young, old))
}

durin.traitAvg.all = trait.avg.wt(leaf.ages, "all", avg)
durin.traitAvg.age = trait.avg.wt(leaf.ages, "age weight", wt)


## Join all datasets together ----
durin.traitAvg = durin.traitAvg.all |>
  bind_rows(durin.traitAvg.age, durin.traitAvg.curr, durin.traitAvg.prev) |>
  mutate(
  group = factor(group, levels = c("young", "old", "all", "age weight"),
                   labels = c("current", "previous", "both (avg)", "age weight")),
  trait = factor(trait, levels = c("leaf_area", "SLA", "LDMC", "dry_mass_g", "leaf_thickness",
                                   "wet_mass_g"),
                 labels = c("Leaf area (cm^2)", "SLA (cm^2/g)", "LDMC (mg/g)", "Dry mass (g)",
                            "Leaf thickness (mm)", "Wet mass (g)"))) |>
  ungroup() |>
  drop_na(wt.mean) |>
  select(-leaf_age)


# Group means ----
durin.traitAvg.grp = durin.traitAvg |>
  group_by(species, trait, habitat, group) |>
  # get_summary_stats_precise(mean, type = "mean") |>
  summarise(grp.mean = mean(wt.mean, na.rm = TRUE))


durin.traitAvg.grp.compare = durin.traitAvg.grp |>
  pivot_wider(names_from = group, values_from = grp.mean) |>
  group_by(species, habitat, trait) |>
  mutate(Percentage_Change.newOld = current/previous * 100)

# Visualize ----

library(ggh4x)
library(viridis)
library(ggridges)

## Four groups ----
### Habitats on the same scale ----
compareAges.EN =
  ggplot(durin.traitAvg |> filter(species == "Empetrum nigrum"),
         aes(x=wt.mean, fill=group, linetype = group)) +
  geom_density(alpha=0.5, linewidth = 0.6) +
  geom_vline(data=durin.traitAvg.grp |> filter(species == "Empetrum nigrum"),
                                               aes(xintercept=grp.mean, color=group, linetype= group)) +
  scale_fill_viridis(discrete=T) +
  scale_colour_viridis(discrete=T) +
  scale_linetype_manual(values = c("dotted", "dotted", "longdash", "solid")) +
  scale_y_continuous(position = "left") +
  facet_nested(species + habitat ~ trait, scales = "free", independent = "y",
               nest_line = element_line(linetype = 2)) +
  labs(y="Density", x= "Trait value") +
  theme_classic() +
  theme(
    # legend.position="none",
    # strip.text.x = element_blank(),
    strip.background = element_blank(),
    ggh4x.facet.nestline = element_line(colour = "black"),
    axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
    text=element_text(size=11))

compareAges.VV =
  ggplot(durin.traitAvg |> filter(species == "Vaccinium vitis-idaea"),
       aes(x=wt.mean, fill=group, linetype = group)) +
  geom_density(alpha=0.5, linewidth = 0.6) +
  geom_vline(data=durin.traitAvg.grp |> filter(species == "Vaccinium vitis-idaea"),
             aes(xintercept=grp.mean, color=group, linetype= group)) +
  scale_fill_viridis(discrete=T) +
  scale_colour_viridis(discrete=T) +
  scale_linetype_manual(values = c("dotted", "dotted", "longdash", "solid")) +
  scale_y_continuous(position = "left") +
  facet_nested(species + habitat ~ trait, scales = "free", independent = "y",
               nest_line = element_line(linetype = 2)) +
  labs(y="Density", x= "Trait value") +
  theme_classic() +
  theme(
    # legend.position="none",
    # strip.text.x = element_blank(),
    strip.background = element_blank(),
    ggh4x.facet.nestline = element_line(colour = "black"),
    axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
    text=element_text(size=11)
  )

compareAges.EN + compareAges.VV +
  plot_layout(ncol = 1, guides = 'collect')

ggsave("visualizations/2024.04.23_TraitDensity.png", width = 12, height = 8, units = "in")

### Habitats on separate scales ----
ggplot(durin.traitAvg,
         aes(x=wt.mean, fill=group, linetype = group)) +
  geom_density(alpha=0.5, linewidth = 0.6) +
  geom_vline(data=durin.traitAvg.grp,
             aes(xintercept=grp.mean, color=group, linetype= group)) +
  scale_fill_viridis(discrete=T) +
  scale_colour_viridis(discrete=T) +
  scale_linetype_manual(values = c("dotted", "dotted", "longdash", "solid")) +
  scale_y_continuous(position = "left") +
  facet_nested(species + habitat ~ trait, scales = "free", independent = "all",
               nest_line = element_line(linetype = 2)) +
  labs(y="Density", x= "Trait value") +
  theme_classic() +
  theme(
    # legend.position="none",
    # strip.text.x = element_blank(),
    strip.background = element_blank(),
    ggh4x.facet.nestline = element_line(colour = "black"),
    axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
    text=element_text(size=11)
  )

ggsave("visualizations/2024.04.23_TraitDensity_Independent.png", width = 12, height = 8, units = "in")


## Comparing ages between species ----
durin.leafAge.avg = DURIN.lit |>
  # Filter to correct obs
  filter(dataset == "DURIN") |>
  filter(trait %in% c("SLA (cm^2/g)", "LDMC (mg/g)")) |>
  group_by(trait, species, leaf_age) |>
  get_summary_stats(type = "mean_sd")

ggplot(DURIN.lit |> filter(dataset == "DURIN") |> filter(leaf_age %in% c("current", "previous")) |>
         filter(trait %in% c("SLA (cm^2/g)", "LDMC (mg/g)")),
       aes(x = interaction(leaf_age, species), y=value,
           fill=interaction(leaf_age, species), colour=interaction(leaf_age, species), linetype = leaf_age)) +
  geom_boxplot(outlier.shape=NA, alpha = 0.5) +
  geom_point(position = position_jitterdodge()) +
  scale_fill_manual(values = c("skyblue3", "midnightblue", "indianred", "firebrick4")) +
  scale_colour_manual(values = c("skyblue3", "midnightblue", "indianred", "firebrick4")) +
  scale_linetype_manual(values = c("dotted", "solid")) +
  # scale_y_continuous(position = "left") +
  # scale_y_log10() +
  facet_nested( ~ trait, scales = "free", independent = "all",
               nest_line = element_line(linetype = 2)) +
  labs(x="", y= "Trait value") +
  scale_x_discrete(guide = "axis_nested") +
  theme_bw() +
  theme(
    legend.position="none",
    # strip.text.x = element_blank(),
    strip.background = element_blank(),
    ggh4x.facet.nestline = element_line(colour = "black"),
    # axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
    text=element_text(size=11)
  )

ggsave("visualizations/2024.04.24_InterspecificTraits.png", width = 6, height = 4, units = "in")

# Ridgeplot version
ggplot(durin.traitAvg, aes(x=mean, y = group, fill=group, linetype = group)) +
  geom_density_ridges(alpha=0.5, linewidth = 0.6, scale = 2.5) +
  geom_vline(data=durin.traitAvg.grp, aes(xintercept=grp.mean, color=group),
             linetype="longdash") +
  scale_fill_viridis(discrete=T) +
  scale_colour_viridis(discrete=T) +
  scale_linetype_manual(values = c("dotted", "dotted", "solid")) +
  # scale_y_continuous(position = "left") +
  facet_nested(species + habitat ~ trait, scales = "free_y", independent = "y",
               nest_line = element_line(linetype = 2)) +
  labs(
    y="Density",
    x= "Trait value"
  ) +
  theme_classic() +
  theme(
    # legend.position="none",
    # strip.text.x = element_blank(),
    strip.background = element_blank(),
    ggh4x.facet.nestline = element_line(colour = "black"),
    axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
    text=element_text(size=11)
  )

## Three groups ----
compareAges.EN =
  ggplot(durin.traitAvg |> filter(species == "Empetrum nigrum") |> filter(group != "age weight"),
         aes(x=wt.mean, fill=group, linetype = group)) +
  geom_density(alpha=0.5, linewidth = 0.6) +
  geom_vline(data=durin.traitAvg.grp |> filter(species == "Empetrum nigrum")|> filter(group != "age weight"),
             aes(xintercept=grp.mean, color=group, linetype= group)) +
  scale_fill_viridis(discrete=T) +
  scale_colour_viridis(discrete=T) +
  scale_linetype_manual(values = c("dotted", "dotted", "solid")) +
  scale_y_continuous(position = "left") +
  facet_nested(species + habitat ~ trait, scales = "free", independent = "y",
               nest_line = element_line(linetype = 2)) +
  labs(y="Density", x= "Trait value") +
  theme_classic() +
  theme(
    # legend.position="none",
    # strip.text.x = element_blank(),
    strip.background = element_blank(),
    ggh4x.facet.nestline = element_line(colour = "black"),
    axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
    text=element_text(size=11))

compareAges.VV =
  ggplot(durin.traitAvg |> filter(species == "Vaccinium vitis-idaea")|> filter(group != "age weight"),
         aes(x=wt.mean, fill=group, linetype = group)) +
  geom_density(alpha=0.5, linewidth = 0.6) +
  geom_vline(data=durin.traitAvg.grp |> filter(species == "Vaccinium vitis-idaea") |> filter(group != "age weight"),
             aes(xintercept=grp.mean, color=group, linetype= group)) +
  scale_fill_viridis(discrete=T) +
  scale_colour_viridis(discrete=T) +
  scale_linetype_manual(values = c("dotted", "dotted", "solid")) +
  scale_y_continuous(position = "left") +
  facet_nested(species + habitat ~ trait, scales = "free", independent = "y",
               nest_line = element_line(linetype = 2)) +
  labs(y="Density", x= "Trait value") +
  theme_classic() +
  theme(
    # legend.position="none",
    # strip.text.x = element_blank(),
    strip.background = element_blank(),
    ggh4x.facet.nestline = element_line(colour = "black"),
    axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
    text=element_text(size=11)
  )

compareAges.EN + compareAges.VV +
  plot_layout(ncol = 1, guides = 'collect')

ggsave("visualizations/2024.05.08_TraitDensity_threeGroups.png", width = 12, height = 8, units = "in")
