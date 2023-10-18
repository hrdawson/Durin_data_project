# Manually clean errors from leaf area scanning datasheet ----
durin.area = read.csv("output/2023.09.27_LeafScanData_Raw.csv") |>
  # Rename columns to match main dataset
  rename(envelope_ID = ID, bulk_nr_leaves_scanned = n) |>
  # Choose edited and found over original
  # Code in the prioritizing for keeping duplicates
  mutate(priority = case_when(
    str_detect(File, "round2") ~ 0,
    str_detect(File, "Edit") ~ 1,
    str_detect(File, "Found") ~ 2,
    TRUE ~ 3
  )) |>
  group_by(envelope_ID) |>
  # Select only the most relevant
  slice_min(priority) |>
  # Rename the known 'out.jpeg' scans
  mutate(envelope_ID = case_when(
    envelope_ID == "out.jpe" & File == "raw_data/leaf_scans/DURIN_2023_Leaf_Areas/Tjotta_2023.07.06Spock_leaf_area.csv" ~ "ECT1701",
    envelope_ID == "out.jpe" & File == "raw_data/leaf_scans/DURIN_2023_Leaf_Areas/Senja_2023-07-11_deathstar_leaf_area.csv" ~ "BQV6602",
    TRUE ~ envelope_ID)) |>
  # We have some honest duplicates thanks to the 22 Jun mixup
  # Use distinct to get rid of those
  dplyr::select(-c(File)) |>
  distinct() |>
  # Remove doubled scans
  mutate(cutting = case_when(
    # One phys team scan has two values
    # Keeping the earliest one
    envelope_ID == "BLB9742" & leaf_area < 4.9 ~ "cut",
    # Duplicates of 'out.jpeg' will need solving at some point
    envelope_ID == "out.jpe" ~ "cut",
    TRUE ~ "keep")) |>
  filter(cutting == "keep") |>
  select(-c(cutting, X))

# Manually clean errors from dry mass datasheet ----
durin.drymass = read.csv("raw_data/2023.10.17_DryMassChecks.csv", na.strings=c("","NA")) |>
  # Drop unused columns
  select(-c(X, order.entered)) |>
  # Remove example and non-data
  filter(!envelope_ID %in% c("AAA0000", "Metal tins:")) |>
  # Clean other errors with mutate
  mutate(
    # Replace erroneous 0s with NA
    dry_mass_g = case_when(
      envelope_ID %in% c("CTI1429", "AZL3403") ~ NA,
      TRUE ~ dry_mass_g
    ),
    # Make dry mass a number
    dry_mass_g = as.numeric(dry_mass_g),
    # Add missing (known) values
    dry_mass_g = case_when(
      envelope_ID == "BPK3465" ~ 0.00249,
      TRUE ~ dry_mass_g
    ),
    # Update to reweighed values
    dry_mass_g = case_when(
      envelope_ID == "BKI4712" ~ 0.01521,
      envelope_ID == "BKJ3045" ~ 0.00228,
      envelope_ID == "BPK3465" ~ 0.00249,
      envelope_ID == "BWL8722" ~ 0.05262,
      envelope_ID == "BWM2473" ~ 0.00341,
      envelope_ID == "EPP7266" ~ 0.00529,
      TRUE ~ dry_mass_g
    ),
    # Correct typos
    dry_mass_g = case_when(
      envelope_ID == "JVU9850" ~ 0.00181,
      envelope_ID == "FDV2375" ~ 0.00097,
      TRUE ~ dry_mass_g
    ),
    # Remove any possible leading white space
    envelope_ID = str_trim(envelope_ID)
  ) |>
  # Filter out known bad values/typos
  filter(!(envelope_ID == "EPP7266" & dry_mass_g == 0.00558)) |>
  filter(is.na(flag_DryMass))

# Manually clean errors from main datasheet ----
durin = read.csv("raw_data/2023.10.17_DURIN Plant Functional Traits_Lygra Sogndal Tjøtta Senja Kautokeino_Data only.csv",
                 na.strings=c("","NA")) |>
  # Correct Senja
  mutate(siteID = str_replace(siteID, "Senje", "Senja")) |>
  # Set all barcodes to uppercase
  mutate(envelope_ID = str_to_upper(envelope_ID)) |>
  # Change plant height to numeric
  mutate(plant_height = as.numeric(plant_height)) |>
  # Remove erroneous dry_mass_g column
  select(-dry_mass_g) |>
  mutate(
    # Correct envelope_ID typos
    envelope_ID = case_when(
      envelope_ID=="JAN2188"~"JAN3188",
      envelope_ID=="HAO2716"~"JAO2716",
      envelope_ID=="JAW7909"~"JAQ7909",
      envelope_ID=="JAU0506"~"JAU0406",
      envelope_ID=="JAY5700"~"JAY4700",
      envelope_ID=="JBT3257"~"JBT3247",
      envelope_ID=="JEK8175"~"JEK6175",
      envelope_ID=="JER2751"~"JER4751",
      envelope_ID=="JEO2375"~"JEU2375",
      envelope_ID=="JHA7525"~"JHX7525",
      envelope_ID=="JHZ731"~"JHZ4732",
      envelope_ID=="JIG7181"~"JIG7191",
      envelope_ID=="JTP1340"~"JIP1340",
      envelope_ID=="FIU6785"~"JIU6785",
      envelope_ID=="JIBV7267"~"JIV7267",
      envelope_ID=="JTX1029"~"JIZ1029",
      envelope_ID=="JSB3462"~"JSE3462",
      envelope_ID=="JSZ0260"~"JSZ0360",
      envelope_ID=="JTM2490"~"JTM3490",
      envelope_ID=="JOO6233"~"JUU6233",
      envelope_ID=="JOY1879"~"JUY1879",
      envelope_ID=="JVK2226"~"JVK2228",
      envelope_ID=="JUW4653"~"JVW4653",
      envelope_ID=="JUX1381"~"JVX1381",
      envelope_ID=="HWY3758"~"JWY3758",
      envelope_ID=="JXI9810"~"JXI9801",
      envelope_ID=="HJYL5525"~"JYL5525",
      envelope_ID=="AZG5994" ~ "AYG5994",
      envelope_ID=="BJY3407" ~ "BJZ3407",
      envelope_ID=="BMZ3737" ~ "BMY3737",
      envelope_ID=="ECT1801" ~ "ECT1701",
      envelope_ID=="EPO1348" ~ "EPO1378",
      envelope_ID=="JSC1626" ~ "JSC1636",
      envelope_ID=="BIY5546" ~ "BIZ5546",
      TRUE ~ envelope_ID),
    # Correct plot names
    DURIN_plot = case_when(
      envelope_ID =="DAP6500"~ "KA_F_VV_2",
      envelope_ID =="EXQ8322"~ "KA_O_EN_1",
      envelope_ID %in% c("EYR2971", "EYV3590") ~ "KA_O_EN_2",
      envelope_ID =="EZA6532"~ "KA_O_EN_3",
      envelope_ID %in% c("DFW2204", "EYG4750", "EYK4044") ~ "KA_O_EN_4",
      envelope_ID %in% c("DHD0172", "DHH3325") ~ "KA_O_VM_2",
      envelope_ID =="DFM4890"~ "KA_O_VM_4",
      envelope_ID =="BMB6959"~ "LY_F_EN_5",
      envelope_ID =="BIL0759"~ "LY_F_VM_2",
      envelope_ID %in% c("AYB7940", "AYF1252", "AYN9607") ~ "LY_O_EN_1",
      envelope_ID %in% c("AUZ1311", "AWC3830") ~ "LY_O_EN_3",
      envelope_ID =="BHR0925"~ "LY_O_EN_5",
      envelope_ID =="AST3380"~ "LY_O_VM_4",
      envelope_ID %in% c("AYP7221", "AZB3929", "BFY4922") ~ "LY_O_VV_2",
      envelope_ID =="AYS6617"~ "LY_O_VM_3",
      envelope_ID =="AZJ4306"~ "LY_O_VV_5",
      envelope_ID =="CWZ4784"~ "SE_O_VM_5",
      envelope_ID %in% c("DBV0943", "DBV0943")~ "SE_O_VV_1",
      envelope_ID =="CYR2242"~ "SE_O_VV_3",
      envelope_ID %in% c("CZD0880")~ "SE_O_VV_4",
      envelope_ID =="CMH5663"~ "SO_F_VM_3",
      envelope_ID =="CMX4054"~ "SO_F_VM_4",
      # From cross-checking number of plants per plot
      envelope_ID =="CKI5874"~ "SO_F_VV_1",
      envelope_ID %in% c("BZJ1412", "BZN2057", "DBF6720")~ "KA_F_VV_1",
      envelope_ID =="BZA7321"~ "KA_O_VV_1",
      envelope_ID =="CCH4321"~ "KA_F_EN_1",
      envelope_ID =="BYB4115"~ "KA_F_VV_5",
      envelope_ID %in% c("AYS6617", "ATE1699", "AYS6617")~ "LY_O_VM_3",
      envelope_ID %in% c("EUK3873", "CYM4429", "EUS4572") ~ "SE_F_VV_3",
      envelope_ID =="ETB0915"~ "SE_O_VM_1",
      envelope_ID %in% c("EVI4590", "DAM1823", "BWU3342", "EUJ5068", "EVA9626")~ "SE_F_VV_4",
      # Missing plots to be entered
      envelope_ID == "ARB1083" ~ "LY_O_EN_4",
      envelope_ID == "ATI1569" ~ "LY_O_VM_2",
      envelope_ID %in% c("ATX5549", "AZM0806") ~ "LY_O_EN_2",
      envelope_ID == "AYB7940" ~ "LY_O_EN_1",
      envelope_ID == "AZE4205" ~ "LY_O_VM_1",
      envelope_ID == "AZJ4306" ~ "LY_O_VV_5",
      envelope_ID == "EGE6339" ~ "SE_F_VV_2",
      envelope_ID == "BLM2549" ~ "LY_O_VV_1",
      envelope_ID == "FIV6929" ~ "LY_F_CV_1",
      # Moving mislabelled plants (RANDOMLY SELECTED FROM PLOT WITH TOO MANY)
      envelope_ID %in% c("BES9911", "BBO5089", "BBK5291") ~ "LY_O_VV_4",
      # Species corrections
      envelope_ID %in% c("AVI9865", "ATG1962", "AVY7377") ~ "LY_O_VV_4",
      TRUE ~ DURIN_plot
    ),
    # Correct habitats
    habitat = case_when(
      envelope_ID %in% c("EXQ8322", "EYR2971", "EYV3590", "EZA6532", "DFW2204", "EYG4750", "EYK4044",
                         "DHD0172","DHH3325", "DFM4890", "AZB3929", "CYR2242", "CZD0880","CLA4537",
                         "CLE7064","ARB1083","ATE1699", "ATI1569", "ATX5549", "AYB7940", "AYP7221",
                         "AYS6617", "AZE4205", "AZJ4306", "AZM0806", "BZA7321", "DGB6760") ~"Open",
      envelope_ID %in% c("BIL0759", "EVA9626", "BOW7206", "EGE6339", "EVI4590", "DAM1823",
                         "BWU3342", "EUJ5068", "CCH4321") ~ "Forested",
      TRUE ~ habitat
    ),
    # Correct plot numbers
    plotNR = case_when(
      envelope_ID %in% c("AYB7940", "AZE4205") ~ 1,
      envelope_ID %in% c("BFY4922", "ATI1569", "ATX5549", "EGE6339", "AYP7221", "AZM0806") ~ 2,
      envelope_ID %in% c("ATE1699", "AYS6617") ~ 3,
      envelope_ID %in% c("BES9911", "BBO5089", "BBK5291", "ARB1083") ~ 4,
      envelope_ID == "AZJ4306" ~ 5,
      TRUE ~ plotNR
    ),
    # Correct leaf numbers
    leaf_nr = case_when(
      envelope_ID %in% c("EEF0649", "AHC6687", "CIT4071", "EUZ2000", "AMR7444", "BNK1425") ~ 1,
      envelope_ID %in% c("AHD4843", "ANK2313", "AXV2319") ~ 2,
      envelope_ID %in% c("AHE7194","AGT6887", "BMS6362","DDP2497", "BAL4872", "CWI7147",
                         "DBE4796", "EVX2488", "ASP1768", "ESX2028", "AAV7693", "ARV5470",
                         "EGG1367", "FCI5827") ~ 3,
      TRUE ~ leaf_nr
    ),
    # Correct leaf ages
    leaf_age = case_when(
      envelope_ID %in% c("AZG5994", "ASS9832","BBM8747", "BMC0045", "BAG7477", "ETW7892") ~"old",
      envelope_ID %in% c("EVO1776", "EVS4445", "CST5790", "AYE1801", "BJN7368", "CHK1631",
                         "BAR6091", "ABG2709") ~"young",
      DroughNet_plotID == 7.3 & species == "Vaccinium myrtillus" ~ "young",
      TRUE ~ leaf_age
    ),
    DroughNet_plotID = case_when(
      envelope_ID == "EFN3557" ~ 2.3,
      envelope_ID == "EFR2422" ~ 2.3,
      TRUE ~ DroughNet_plotID
    ),
    # Correct thicknesses
    leaf_thickness_1_mm = case_when(
      envelope_ID =="DSD6681"~0.0107,
      envelope_ID =="CSP7326"~0.259,
      envelope_ID == "APK0446" ~ 0.311,
      # From round 2 corrections
      envelope_ID == "ASM6249" ~ 0.114,
      # Round 3 corrections
      envelope_ID == "AYX2273" ~ 0.248,
      TRUE ~ leaf_thickness_1_mm
    ),
    leaf_thickness_2_mm = case_when(
      envelope_ID =="BPF4529"~0.221,
      # From round 2 corrections
      envelope_ID == "ASM6249" ~ 0.114,
      envelope_ID == "EKS3555" ~ 0.258,
      TRUE ~ leaf_thickness_2_mm
    ),
    leaf_thickness_3_mm = case_when(
      envelope_ID == "ABZ7857" ~ 0.114,
      envelope_ID == "ADW3051" ~ 0.247,
      TRUE ~ leaf_thickness_3_mm
    ),
    # Correct plant heights
    plant_height = case_when(
      envelope_ID %in% c("AWH7022") ~11.6,
      envelope_ID =="DUX5951"~14.8,
      envelope_ID =="DNA2455"~15.4,
      envelope_ID =="ERV2714"~15.5,
      envelope_ID %in% c("DFX0733", "DGF1762", "DGB6760")~17.0,
      envelope_ID =="ANO6821"~17.5,
      envelope_ID =="CHP1113"~41.2,
      envelope_ID %in% c("APH3193", "AJA7257")~49.2,
      envelope_ID %in% c("APC6542", "AIO8720") ~16.5,
      envelope_ID =="AGX3707"~8.1,
      envelope_ID %in% c("BES9911", "BBO5089", "BBK5291") ~ 16.0,
      envelope_ID == "AWE1721" ~ 10.1,
      TRUE ~ plant_height
    ),
    # Correct species
    species = case_when(
      envelope_ID %in% c("ALL1763", "AWF5086", "BBM8747",
                         "DAI1197", "DZX9994", "BLM2549",
                         "AVI9865", "ATG1962", "AVY7377") ~ "Vaccinium vitis-idaea",

      TRUE ~ species
    ),
    # Correct wet mass
    wet_mass_g = case_when(
      envelope_ID =="DDZ3156"~0.0138,
      envelope_ID =="DEN0101"~0.0254,
      envelope_ID =="ARK3594"~0.0282,
      envelope_ID =="AFH1727"~0.0333,
      envelope_ID =="BSD3874"~0.0468,
      envelope_ID == "JUI8430" ~ 0.00748,
      envelope_ID == "ADW3051" ~ 0.0200,
      envelope_ID == "DQA1214" ~ 0.0068,
      envelope_ID == "JCC1574" ~ 0.00896,
      envelope_ID == "JCE7349" ~ 0.00815,
      envelope_ID == "JCG7439" ~ 0.01350,
      envelope_ID == "JFO8192" ~ 0.01213,
      envelope_ID == "JIW0462" ~ 0.01127,
      envelope_ID == "JTC4335" ~ 0.00849,
      envelope_ID == "JTR2458" ~ 0.00896,
      envelope_ID == "JUH4035" ~ 0.00895,
      envelope_ID == "ABZ7857" ~ 0.0172,
      envelope_ID == "EKS3555" ~ 0.00741,
      envelope_ID == "AOC0254" ~ 0.0603,
      envelope_ID == "ASK2376" ~ 0.0081,
      envelope_ID == "ARE1168" ~ 0.0117,
      envelope_ID == "EEK9473" ~ 0.0110,
      envelope_ID %in% c("EVL2844", "AJT3009", "ADB4598", "EBR4362", "ANT4890",
                         "BWM2473")  ~ NA,
      TRUE ~ wet_mass_g),
    # Correct Tjøtta plot switch
    DroughNet_plotID = case_when(
      siteID == "Tjøtta" & DroughNet_plotID == 1.1 ~ 1.2,
      TRUE ~ DroughNet_plotID
    ),
    # Correct ageClass typos
    ageClass = case_when(
      envelope_ID == "EDV5508" ~ "Pioneer",
      envelope_ID == "EDR6459" ~ "Pioneer",
      TRUE ~ ageClass
    ),
    # Correct swapped plant numbers and plant heights
    plant_nr = as.numeric(plant_nr),
    plant_nr = case_when(
      DURIN_plot == "KA_F_EN_3" & plant_nr == 1 ~ 3,
      DURIN_plot == "KA_F_EN_3" & plant_nr == 3 ~ 1,
      envelope_ID == "CQU5714" ~ 1,
      envelope_ID == "BBM8747" ~ 3,
      envelope_ID == "CWT0272" ~ 1,
      DURIN_plot == "SO_O_VV_5" & plant_height == 13.5 ~ 2,
      DURIN_plot == "SO_O_VV_5" & plant_height == 10.4 ~ 3,
      envelope_ID =="AAO4783"~3,
      envelope_ID =="AAT2449"~3,
      envelope_ID =="AFN5166"~3,
      envelope_ID =="EFN3557"~3,
      DroughNet_plotID == 7.3 & plant_height == 17.6 ~ 2,
      envelope_ID == "AZT0694" ~ 4,
      envelope_ID == "AXZ4062" ~ 4,
      envelope_ID == "AZS5721" ~ 4,
      TRUE ~ plant_nr
    ),
    plant_height = case_when(
      DURIN_plot == "KA_F_EN_3" & plant_nr == 1 ~ 17.2,
      DURIN_plot == "KA_F_EN_3" & plant_nr == 3 ~ 19.0,
      DURIN_plot == "LY_F_EN_2" & plant_nr == 3 ~ 26.8,
      DURIN_plot == "LY_F_EN_5" & plant_nr == 1 ~ 28.0,
      DURIN_plot == "LY_O_EN_1" & plant_nr == 1 ~ 10.2,
      DURIN_plot == "LY_O_EN_3" & plant_nr == 3 ~ 14.2,
      # DURIN_plot == "LY_O_EN_4" & plant_nr == 2 ~ 16.4,
      DURIN_plot == "SE_O_VM_3" & plant_nr == 2 ~ 15.5,
      DURIN_plot == "SE_O_VM_3" & plant_nr == 3 ~ 17.2,
      DURIN_plot == "SE_O_VV_3" & plant_nr == 3 ~ 18.4,
      envelope_ID == "ESC1744" ~ 18.5,
      TRUE ~ plant_height
    )
  ) |>
  # Filter out known bad values/typos
  filter(!(envelope_ID == "CVP9320" & wet_mass_g == 0.4470)) |>
  filter(!(envelope_ID == "BKI4712" & species == "Calluna vulgaris")) |>
  filter(is.na(flag)) |>
  # Add dry mass and area
  left_join(durin.drymass) |>
  left_join(durin.area) |>
  # Manually replace bulk_nr_leaves for ones the scanned number is more accurate
  mutate(bulk_nr_leaves = case_when(
    envelope_ID %in% c("BFN9270", "BHY0712", "AYP7221", "ALA0711", "BHK3198",
                       "AUW3217") ~ bulk_nr_leaves_scanned,
    envelope_ID == "ARE1168" ~ NA,
    envelope_ID %in% c("EES8345") ~ 2, # Leaf cut in half on scan
    TRUE ~ bulk_nr_leaves
  ),
  # Make updated bulk_nr_leaves tab
  bulk_nr_leaves_clean = coalesce(bulk_nr_leaves, bulk_nr_leaves_scanned)) |>
  # Calculate values scaled to bulk leaves
  rename(bulk_nr_leaves_original = bulk_nr_leaves, bulk_nr_leaves = bulk_nr_leaves_clean,
         wet_mass_g_original = wet_mass_g, dry_mass_g_original = dry_mass_g,
         leaf_area_original = leaf_area) |>
  mutate(wet_mass_g = wet_mass_g_original/bulk_nr_leaves,
         dry_mass_g = dry_mass_g_original/bulk_nr_leaves,
         leaf_area = leaf_area_original/bulk_nr_leaves,
         # SLA is calculated from aggregate, not scaled
         SLA = leaf_area_original/dry_mass_g_original,
         # LDMC is calculated from aggregate, not scaled
         LDMC = (dry_mass_g*1000)/wet_mass_g) |>
  # Filter out any complete duplicates
  distinct()

write.csv(durin, "output/DURIN_clean.csv")
