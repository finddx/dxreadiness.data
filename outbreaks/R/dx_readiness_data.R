library(tidyr)
library(readr)
library(dplyr)
library(readxl)
library(openxlsx)
library(stringr)



tpp_data <- read_excel("../TPP_outbreak_diseases.xlsx", sheet = "Sheet1") |>
  select(disease_target = Value, tpp_available = TPP_Available)

disease_pathogen_data <- read_csv("data/disease_pathogen_matches.csv") |>
  mutate(disease_target = str_to_title(disease_target),
         target_pathogen = str_to_title(target_pathogen))


disease_list <- data.frame(disease_target = str_to_title(c(
  "Chikungunya",
  "Cholera",
  "Covid-19",
  "Crimean Congo Hemorrhagic Fever",
  "Dengue Fever",
  "Ebola Fever",
  "Influenza",
  "Lassa Fever",
  "Marburg",
  "Measles",
  "Meningitis",
  "Middle East Respiratory Syndrome",
  "Mpox",
  "Paratyphoid Fever",
  "Rubella",
  "Salmonellosis (Non-Typhoidal)",
  "Typhoid Fever",
  "Yellow Fever",
  "Zika Fever"
)),
transmission_mode = c(
  "Vector-borne",
  "Waterborne",
  "Respiratory",
  "Contact; Zoonotic",
  "Vector-borne",
  "Contact; Zoonotic",
  "Respiratory",
  "Contact; Zoonotic",
  "Contact; Zoonotic",
  "Respiratory",
  "Airborne droplets, Contact",
  "Contact; Zoonotic",
  "Contact; Zoonotic",
  "Food/waterborne",
  "Respiratory",
  "Food/waterborne",
  "Food/waterborne",
  "Vector-borne",
  "Vector-borne; Vertical"
),
last_update = c(
  "2018", "2023", "2021", "2021", "2021", "2022","Unknown",
  "2023","2023", "2023", "2023", "Unknown", "2022",
  "2023", "2023", "2023", "2023", "2023", "2019"
))


sf_data <- read_csv("../report1713812286805_20240422.csv") |>
  select(
    assay_id = `Assay: Assay ID`,
    country = `Assay: Company/Institution Name: Country`,
    assay_name = `Assay: Assay Name`,
    who_region = `Assay: Company/Institution Name: WHO Region`,
    disease_target = `Assay: Disease Target`,
    planned_market_entry = `Assay: Planned Market Entry`,
    stage_of_development = `Assay: Stage of Development`,
    reg_approval = `Assay: Regulatory Approval`,
    lab_vs_poc = `Assay: Lab vs. POC`,
    target_pathogen = `Assay: Target Pathogen`) |>
  distinct() |>
  separate_longer_delim(disease_target, delim = "; ") |>
  separate_longer_delim(target_pathogen, delim = "; ") |>
  mutate(disease_target = str_to_title(disease_target),
         target_pathogen = str_to_title(target_pathogen)) |>
  left_join(tpp_data) |>
  inner_join(disease_pathogen_data) |>
  inner_join(disease_list)




sf_data_reg_status <- sf_data |>
  select(assay_id, assay_name, reg_approval) |>
  separate_longer_delim(reg_approval, delim = "; ")|>
  distinct() |>
  mutate(reg_approval = case_when(reg_approval %in% c("CE", "CE-IVDD", "Unknown", "None", "Other") ~ 0,
                   .default = 1)) |>
  group_by(assay_id) |>
  summarise(reg_approval = sum(reg_approval)) |>
  right_join(sf_data |>
               select(-reg_approval), by = 'assay_id')






comp_data <- sf_data_reg_status |>
  mutate(tpp_available = case_when(tpp_available == "Yes" ~ 1,
                                   is.na(tpp_available) | tpp_available == "No" ~ 0),
         planned_market_entry = case_when(planned_market_entry == "Already on the market (commercialized)" ~ 1,
                                          .default =0),
         reg_status_approval = case_when(stage_of_development %in% c("Regulatory Achieved") & reg_approval >= 1 ~ 1,
                                  .default =0),
         # reg_status = case_when(reg_status > 0 ~ 1,
         #                        .default =0),
         dev_status = case_when(stage_of_development %in% c("Development", "Late Stage Development (fully functional prototype)",
                                                            "Validation", "Early Stage Development (partial prototype)") ~ 1,
                                        .default =0),
         lab_based = case_when(lab_vs_poc %in% c("Lab-based") ~ 1,
                             .default =0),
         near_poc =  case_when(lab_vs_poc %in% c("near Point of Care") ~ 1,
                               .default =0),
         true_poc =  case_when(lab_vs_poc %in% c("True Point of Care") ~ 1,
                               .default =0)) |>
  group_by(disease_target, target_pathogen, transmission_mode) |>
  summarise(assays = paste(assay_name, collapse = ","),
            countries = paste(unique(country), collapse = ","),
            planned_market_entry = sum(planned_market_entry),
            reg_status_approval = sum(reg_status_approval),
            dev_status = sum(dev_status),
            lab_based = sum(lab_based),
            near_poc = sum(near_poc),
            true_poc = sum(true_poc),
            tpp_available = unique(tpp_available)) |>
  ungroup() |>
  mutate(index = paste0("u_",row_number()))


comp_data_idata <- comp_data |>
  mutate(uCode = index,
         uName = target_pathogen) |>
  select(uCode, uName, disease_target, transmission_mode, planned_market_entry, reg_status_approval, dev_status,
         lab_based, near_poc, true_poc, tpp_available)



Level <- as.numeric(c(rep(1, 7), 2, "", ""))
iCode <- c("planned_market_entry", "reg_status_approval", "dev_status", "lab_based", "near_poc", "true_poc", "tpp_available", "dx_readiness_index","disease_target", "transmission_mode")
iName <- c("Planned market entry", "Regulatory approval", "Under development", "Lab-based", "Near POC",
                           "True POC", "TPP available", "Dx readiness index", "Disease", "Transmission mode")
Direction <- as.numeric(c(rep(1,8), "", ""))
Weight <- as.numeric(c(rep(1,8), "", ""))
Parent <- c(rep("dx_readiness_index", 7), "", "", "")
Type <- c(rep("Indicator", 7), "Aggregate", "Group", "Group")


comp_data_iMeta <- data.frame(Level, iCode, iName, Direction, Weight, Parent, Type)



comp_data_list <- list(iData = comp_data_idata, iMeta = comp_data_iMeta) |>
  writexl::write_xlsx(path = "data/dx_readiness_comp_data_20240514.xlsx")



comp_data_idata_wo_COVID <- comp_data_idata |>
  filter(!disease_target == "Covid-19")


comp_data_list_wo_COVID <- list(iData = comp_data_idata_wo_COVID, iMeta = comp_data_iMeta) |>
  writexl::write_xlsx(path = "data/dx_readiness_comp_data_wo_COVID_20240422.xlsx")



