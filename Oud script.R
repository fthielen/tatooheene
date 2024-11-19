# Update of medical reference prices
new_filepath <- paste0("data/RP_medical_", update_year, ".rds")

df_medical <- readRDS(new_filepath) %>%
  mutate(across(where(is.numeric), func_round_euro))

# Update of patient reference prices
new_filepath_patient <- paste0("data/RP_patient_", update_year, ".rds")

df_patient<- readRDS(new_filepath_patient) %>%
  mutate(across(where(is.numeric), func_round_euro))

# Update of other reference prices
new_filepath_other <- paste0("data/RP_other_", update_year, ".rds")

df_other<- readRDS(new_filepath_other) %>%
  mutate(across(where(is.numeric), func_round_euro))
