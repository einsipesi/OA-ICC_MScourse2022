chemistry_multiple_pangaea <- read.csv("~/Library/CloudStorage/OneDrive-PublicAdministration/Documents/surt/BOAT/monaco_multiple_drivers/scripts_data/rproject_final/OA-ICC_MScourse2022/pangaea/chemistry_multiple_pangaea.csv")

chemistry_single_pangaea <- read.csv("~/Library/CloudStorage/OneDrive-PublicAdministration/Documents/surt/BOAT/monaco_multiple_drivers/scripts_data/rproject_final/OA-ICC_MScourse2022/pangaea/chemistry_single_pangaea.csv")

full_multiple_pangaea <- read_excel("pangaea/full_multiple_pangaea.xlsx")

full_single_pangaea <- read_excel("pangaea/full_single_pangaea.xlsx")


library(dplyr)

### Add 'Individual' columns to the larval measurement tables

# For full_multiple_pangaea
full_multiple_pangaea <- full_multiple_pangaea %>%
  group_by(Treatment) %>%
  mutate(Individual = row_number()) %>%
  ungroup()

# For full_single_pangaea
full_single_pangaea <- full_single_pangaea %>%
  group_by(Treatment) %>%
  mutate(Individual = row_number()) %>%
  ungroup()

### Experiment column

full_multiple_pangaea$Experiment <- "experiment2_multiple_drivers"

full_single_pangaea$Experiment <- "experiment1_single_driver"

### Fix naming in chemistry_multiple

chemistry_multiple_pangaea <- chemistry_multiple_pangaea %>%
  mutate(Treatment = sub("^(ML\\dP\\d)R(\\d)_(\\d+)$", "\\1_\\3_R\\2", Treatment))


### Merge the two full datasets
full_merged <- bind_rows(full_single_pangaea, full_multiple_pangaea)

### Merge chemistry datasets and join to full_merged
chemistry_merged <- bind_rows(chemistry_single_pangaea, chemistry_multiple_pangaea)

### Merge chemistry to full
full_merged <- left_join(full_merged, chemistry_merged, by = "Treatment")

### Export it
write.csv(full_merged, "Plividus2022_experiment_biology_waterparameters.csv", row.names = FALSE)
