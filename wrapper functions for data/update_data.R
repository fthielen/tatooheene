
# Source scripts ---------------------------------------------------------
source("wrapper functions for data/load_pkgs.R")

# Function to round and add euros -----------------------------------------

func_round_euro <- function(x){
  ifelse(x < 100,
         paste0("€", format(round(x, 2), nsmall = 2)),
         paste0("€", round(x, 0)))
}

# Update Reference prices every year and write them in a dataset ----------


# Get system year - 1 year for update
update_year <- as.numeric(format(Sys.Date(), "%Y")) - 1

# File year
file_name <- list.files(path = "data/", pattern = "^RP_medical")
file_year <- as.numeric(sub(".*_(\\d{4})\\.rds$", "\\1", file_name))


# Update price index table and factor
if(update_year > file_year){

  df_cpi <- cbsodataR::cbs_get_data("83131ned") %>%
    cbsodataR::cbs_add_date_column() %>%
    cbsodataR::cbs_add_label_columns() %>%
    filter(Perioden_freq == "Y",
           Bestedingscategorieen_label == "000000 Alle bestedingen") %>%
    select(Perioden_label, CPI_1) %>%
    mutate(Perioden_label = as.numeric(as.character(Perioden_label))) %>%
    filter(Perioden_label %in% 2012:max(Perioden_label)) %>%
    mutate(to_next_yr_fct = round(lead(CPI_1) / CPI_1, 3),
           to_next_yr_per = (to_next_yr_fct - 1) * 100,
           to_last_yr_fct = round(last(CPI_1) / CPI_1,3 ),
           to_last_yr_per = (to_last_yr_fct - 1) * 100)

  df_cpi_to_next_yr <- data.frame(
    Van = df_cpi[1:nrow(df_cpi)-1,"Perioden_label"],
    naar = df_cpi[2:nrow(df_cpi),"Perioden_label"],
    percentage = df_cpi[1:nrow(df_cpi)-1,"to_next_yr_per"],
    factor = df_cpi[1:nrow(df_cpi)-1,"to_next_yr_fct"])


  df_cpi_to_last_yr <- data.frame(
    Van = df_cpi[1:nrow(df_cpi)-1, "Perioden_label"],
    naar = df_cpi[2:nrow(df_cpi), "Perioden_label"],
    percentage = df_cpi[1:nrow(df_cpi)-1,"to_last_yr_per"],
    factor = df_cpi[1:nrow(df_cpi)-1,"to_last_yr_fct"])

  df_cpi_combined <- bind_cols(df_cpi_to_next_yr,
                               df_cpi_to_last_yr) %>%
    rename(`Year from'` = `Perioden_label...5`,
           `Year to'` = Perioden_label.1...6) %>%
    rename(`Year from` = `Perioden_label...1`,
           `Year to` = Perioden_label.1...2) %>%
    rename("Percentage" = to_next_yr_per,
           "Percentage'"= to_last_yr_per,
           "Factor" = to_next_yr_fct,
           "Factor'" = to_last_yr_fct) %>%
    mutate(`Year to'` = max(`Year to'`))

  # Write RDS for price index table
  new_filepath_cpi <- paste0("data/df_cpi_", update_year, ".rds")

  df_cpi_combined %>%
    write_rds(new_filepath_cpi)

  # Calculate the factor to update the reference prices
  factor_rp <- df_cpi_combined %>%
    filter(`Year from'` == 2022 & `Year to'` %in% max(`Year to'`)) %>%
    pull("Factor'")

  # Update of medical reference prices
  full_path <- paste0("data/", file_name)

  df_rp_medical <- readRDS(full_path)

  df_rp_medical %>%
    mutate(!!sym(as.character(update_year)) := `2022` * factor_rp) %>%
    write_rds(full_path)

  new_filepath <- paste0("data/RP_medical_", update_year, ".rds")

  file.rename(from = full_path, to = new_filepath)

  df_medical <- readRDS(new_filepath) %>%
    mutate(across(where(is.numeric), func_round_euro))


  # Update of patient reference prices
  file_name_patient <- list.files(path = "data/", pattern = "^RP_patient")
  full_path_patient <- paste0("data/", file_name_patient)

  df_rp_patient <- readRDS(full_path_patient)

  df_rp_patient %>%
    mutate(!!sym(as.character(update_year)) := `2022` * factor_rp) %>%
    write_rds(full_path_patient)

  new_filepath_patient <- paste0("data/RP_patient_", update_year, ".rds")

  file.rename(from = full_path_patient, to = new_filepath_patient)

  df_patient<- readRDS(new_filepath_patient) %>%
    mutate(across(where(is.numeric), func_round_euro))

  # Update of other reference prices
  file_name_other <- list.files(path = "data/", pattern = "^RP_other")
  full_path_other <- paste0("data/", file_name_other)

  df_rp_other <- readRDS(full_path_other)

  df_rp_other %>%
    mutate(!!sym(as.character(update_year)) := `2022` * factor_rp) %>%
    write_rds(full_path_other)

  new_filepath_other <- paste0("data/RP_other_", update_year, ".rds")

  file.rename(from = full_path_other, to = new_filepath_other)

  df_other <- readRDS(new_filepath_other) %>%
    mutate(across(where(is.numeric), func_round_euro))


  # Calculate and write friction period
  new_filepath_fp <- paste0("data/df_fp_", update_year, ".rds")

  # Download new dataset with years
  df_vacancy <- cbsodataR::cbs_get_data("80472ned") %>%
          cbsodataR::cbs_add_label_columns() %>%
          cbsodataR::cbs_add_date_column() %>%
          dplyr::select(!OntstaneVacatures_2) %>%
          dplyr::filter(Perioden_freq == "Y",
                        Bedrijfskenmerken %in% c("T001081")) %>%
          dplyr::mutate(Year = lubridate::year(Perioden_Date),
                        Friction_period_days = 365.25/(VervuldeVacatures_3/OpenstaandeVacatures_1) + 28,
                        Friction_period_weeks = Friction_period_days/7) %>%
          dplyr::distinct(Year,
                          VervuldeVacatures_3,
                          OpenstaandeVacatures_1,
                          Friction_period_days,
                          Friction_period_weeks) %>%
          dplyr::rename("Filled vacancies" = "VervuldeVacatures_3",
                        "Open vacancies" = "OpenstaandeVacatures_1",
                        "Friction period in days" = "Friction_period_days",
                        "Friction period in weeks" = "Friction_period_weeks") %>%
          dplyr::mutate("Friction period days average over 5 years" = zoo::rollmean(`Friction period in days`, k=5, align = "right", fill = NA),
                        "Friction period weeks average over 5 years" = zoo::rollmean(`Friction period in weeks`, k=5, align="right", fill = NA))

        saveRDS(df_vacancy, file = here::here(new_filepath_fp))

    # Calculate and write PPP factor dataframe
        new_filepath_ppp <- paste0("data/df_ppp", update_year, ".rds")

        read.csv("https://sdmx.oecd.org/public/rest/data/OECD.SDD.NAD,DSD_NAMAIN10@DF_TABLE4,1.0/A.AUS+AUT+BEL+CAN+CHL+COL+CRI+CZE+DNK+EST+FIN+FRA+DEU+GRC+HUN+ISL+IRL+ISR+ITA+JPN+KOR+LVA+LTU+LUX+MEX+NLD+NZL+NOR+POL+PRT+SVK+SVN+ESP+SWE+CHE+TUR+GBR+USA...PPP_B1GQ.......?dimensionAtObservation=AllDimensions&format=csvfilewithlabels") %>%
          filter(Reference.area == "Netherlands") %>%
          select("TIME_PERIOD", "OBS_VALUE")  %>%
          rename("Year" = "TIME_PERIOD",
                 "PPP" = "OBS_VALUE") %>%
          arrange(desc("Year")) %>%
          write.csv(new_filepath_ppp)



} else{

  print("No update is needed")
}

colnames(df_medical) <- ifelse(colnames(df_medical) == "Unit", colnames(df_medical), paste("Price", colnames(df_medical)))

