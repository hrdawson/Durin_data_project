# Test if each trait is significantly different between leaf ages
library(lmerTest)
library(car)

# Vaccinium vitis-idaea tests ----
durin.subset.VV = durin |>
  filter(species == "Vaccinium vitis-idaea") |>
  filter(siteID == "Sogndal") |>
  drop_na(DURIN_plot) |>
  select(c(envelope_ID:species, project:DURIN_plot, plant_nr:leaf_age, plant_height,
         leaf_thickness_1_mm:leaf_thickness_3_mm, wet_mass_g:LDMC)) |>
  mutate(plotNR = str_sub(DURIN_plot, -1))

# From this StackOverflow: https://stackoverflow.com/questions/63577635/for-loop-to-run-lmer-model-for-each-column-in-a-dataframe-gives-variable-length
models.VV <- list()
for (i in c("dry_mass_g", "leaf_area", "SLA", "LDMC")) {
  f <- formula(paste(i, "~leaf_age*habitat +
                (1|DURIN_plot)"))
  models.VV[[i]] <- Anova(lmer(f, data=durin.subset.VV), Type = "II")
}

models.VV$dry_mass_g
models.VV$leaf_area
models.VV$SLA
models.VV$LDMC

write.csv(as.data.frame(models.VV), "output/2023.10.24_LeafAge_ANOVA_VV.csv")

rand(lmer(LDMC ~leaf_age*habitat + (1|DURIN_plot) , data = durin.subset.VV))

# Empetrum nigrum tests ----
durin.subset.EN = durin |>
  filter(species == "Empetrum nigrum") |>
  filter(siteID == "Sogndal") |>
  drop_na(DURIN_plot) |>
  select(c(envelope_ID:species, project:DURIN_plot, plant_nr:leaf_age, plant_height,
           leaf_thickness_1_mm:leaf_thickness_3_mm, wet_mass_g:LDMC)) |>
  mutate(plotNR = str_sub(DURIN_plot, -1))

# From this StackOverflow: https://stackoverflow.com/questions/63577635/for-loop-to-run-lmer-model-for-each-column-in-a-dataframe-gives-variable-length
models.EN <- list()
for (i in c("dry_mass_g", "leaf_area", "SLA", "LDMC")) {
  f <- formula(paste(i, "~leaf_age*habitat +
                (1|DURIN_plot)"))
  models.EN[[i]] <- Anova(lmer(f, data=durin.subset.EN), Type = "II")
}

models.EN$dry_mass_g
models.EN$leaf_area
models.EN$SLA
models.EN$LDMC

write.csv(as.data.frame(models.EN), "output/2023.10.24_LeafAge_ANOVA_EN.csv")

rand(lmer(LDMC ~leaf_age*habitat + (1|DURIN_plot), data = durin.subset.EN))

# Thickness ---
# Done separately because of the triplicate complication
durin.subset.thickness = durin |>
  filter(siteID == "Sogndal") |>
  drop_na(DURIN_plot) |>
  mutate(plotNR = str_sub(DURIN_plot, -1)) |>
  select(envelope_ID:leaf_thickness_3_mm) |>
  # Pivot thicknesses into one column
    pivot_longer(cols = leaf_thickness_1_mm:leaf_thickness_3_mm, values_to = "leaf_thickness")


VV.thickness.lmer = lmer(leaf_thickness ~leaf_age*habitat + (1|DURIN_plot),
                         data = durin.subset.thickness |> filter(species == "Vaccinium vitis-idaea"))
write.csv((Anova(VV.thickness.lmer, Type = "II")), "output/2023.10.24_LeafAge_ANOVA_VV_thickness.csv")

EN.thickness.lmer = lmer(leaf_thickness ~leaf_age*habitat + (1|DURIN_plot),
                         data = durin.subset.thickness |> filter(species == "Empetrum nigrum"))
write.csv(Anova(EN.thickness.lmer, Type = "II"), "output/2023.10.24_LeafAge_ANOVA_EN_thickness.csv")

rand(EN.thickness.lmer)
