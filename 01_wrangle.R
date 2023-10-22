library(tidyverse)
library(readxl)
library(tidyxl)
library(acled.api)
library(janitor)

input_dir <- file.path(
  Sys.getenv("SDN_DISPLACEMENT_DIR"),
  "2023_displacement",
  "data",
  "input"
)

output_dir <- file.path(
  Sys.getenv("SDN_DISPLACEMENT_DIR"),
  "2023_displacement",
  "data",
  "output"
)

#############################
#### ADMIN BOUNDARY DATA ####
#############################

# load in Excel files of the boundaries data just for simple
# pcode matching with external datasets

df_adm1 <- read_xlsx(
  file.path(
    input_dir,
    "sdn_adminboundaries_tabulardata.xlsx"
  ),
  sheet = "ADM1"
)

df_adm2 <- read_xlsx(
  file.path(
    input_dir,
    "sdn_adminboundaries_tabulardata.xlsx"
  ),
  sheet = "ADM2"
) %>%
  mutate( # typically OCHA PCODEs identify duplicate names with `- adm1 initials` so making that for ar rahad
    ADM2_EN = case_when(
      ADM2_EN == "Ar Rahad" & ADM1_EN == "Gedaref" ~ "Ar Rahad - G",
      ADM2_EN == "Ar Rahad" ~ "Ar Rahad - NK",
      .default = ADM2_EN
    )
  )

####################
#### ACLED DATA ####
####################

df_acled <- acled.api(
  country = "Sudan",
  start.date = "2023-01-01",
  end.date = Sys.Date()
) %>%
  filter( # only keep relevant violent incidents
    event_type %in% c("Battles", "Violence against civilians", "Explosions/remote violence")
  ) %>%
  mutate( # fix admin names to match up with OCHA PCODES
    admin1 = case_when(
      admin1 == "Abyei" ~ "Abyei PCA",
      admin1 == "Al Jazirah" ~ "Aj Jazirah",
      .default = admin1
    ),
    admin2 = case_when(
      admin2 == "Ad Duayn" ~ "Ad Du'ayn",
      admin1 == "North Kordofan" & admin2 == "Ar Rahad" ~ "Ar Rahad - NK",
      admin2 == "Ar Rahad" ~ "Ar Rahad - G",
      admin1 == "South Kordofan" & admin2 == "Al Tadamon" ~ "At Tadamon - SK",
      admin2 == "Al Tadamon" ~ "At Tadamon - BN",
      admin2 == "Bau" ~ "Baw",
      admin2 == "El Geneina" ~ "Ag Geneina",
      admin1 == "West Kordofan" & admin2 == "El Salam" ~ "As Salam - WK",
      admin2 == "El Salam" ~ "As Salam - SD",
      admin1 == "South Kordofan" & admin2 == "Habila" ~ "Habila - SK",
      admin2 == "Habila" ~ "Habila - WD",
      admin2 == "Merowe" ~ "Merwoe",
      admin2 == "Omdurman" ~ "Um Durman",
      admin2 == "Rashad" ~ "Ar Rashad",
      .default = admin2
    )
  ) %>%
  left_join(
    distinct(df_adm2, ADM2_EN, ADM1_PCODE, ADM2_PCODE),
    by = c("admin2" = "ADM2_EN")
  )

write_csv(
  df_acled,
  file.path(
    output_dir,
    "sdn_acled.csv"
  )
)

############################
#### DTM SCENARIOS DATA ####
############################

#' Reads the DTM scenarios data
#'
#' For the specific sheet, read in the
#' scenarios data and delineate the
#' data level based on formatting.
#'
#' @param sheet Name of sheet to read
read_dtm_scenarios <- function(sheet) {
  data_level <- xlsx_cells(
    file.path(
      input_dir,
      "Scenarios (DTM Sudan locality).xlsx"
    ),
    sheet = sheet,
    include_blank_cells = FALSE
  ) %>%
    filter(
      row > 2,
      col == 2
    ) %>%
    transmute(
      data_level = case_when(
        local_format_id %in% c(11, 22) ~ "adm_1",
        local_format_id %in% c(14, 25) ~ "adm_2",
        local_format_id %in% c(17, 28) ~ "adm_0"
      )
    ) %>%
    pull(
      data_level
    )

  # with these levels, load the data and add relevant columns
  read_xlsx(
    file.path(
      input_dir,
      "Scenarios (DTM Sudan locality).xlsx"
    ),
    sheet = sheet
  ) %>%
    mutate(
      scenario = sheet,
      data_level = !!data_level,
      .after = "State/Locality"
    )
}

# pull all of the datasets together
# then split out again based on admin boundaries
scenarios_list <- map(
  .x = paste("Scenario", 1:3),
  .f = read_dtm_scenarios
) %>%
  list_rbind() %>%
  pivot_longer(
    starts_with("Month"),
    names_to = "month",
    names_pattern = "([0-9]+)",
    values_to = "displacement"
  ) %>%
  split(
    .[["data_level"]]
  )

# find the admin pcodes for the levels

scenarios_list$adm_1 <- scenarios_list$adm_1 %>%
  left_join(
    select(df_adm1, ADM1_EN, ADM1_PCODE),
    by = c("State/Locality" = "ADM1_EN")
  ) %>%
  select(
    admin1_name = `State/Locality`,
    admin1_pcode = ADM1_PCODE,
    scenario,
    month,
    displacement
  )

scenarios_list$adm_2 <- scenarios_list$adm_2%>%
  mutate(
    `State/Locality` = case_when( # fixing names to match the OCHA adm file
      `State/Locality` == "Gedaref" ~ "Madeinat Al Gedaref",
      `State/Locality` == "Jabal Awlia" ~ "Jebel Awlia",
      `State/Locality` == "Kararri" ~ "Karrari",
      `State/Locality` == "Kassala" ~ "Madeinat Kassala",
      `State/Locality` == "Merowe" ~ "Merwoe",
      `State/Locality` == "Suakin" ~ "Sawakin",
      .default = `State/Locality`
    )
  ) %>%
  left_join(
    select(df_adm2, ADM2_EN, ADM1_PCODE, ADM2_PCODE),
    by = c("State/Locality" = "ADM2_EN")
  ) %>%
  select(
    admin2_name = `State/Locality`,
    admin2_pcode = ADM2_PCODE,
    scenario,
    month,
    displacement
  )

scenarios_list %>%
  iwalk(
    \(x, adm) {
      write_csv(
        x,
        file.path(
          output_dir,
          paste0("dtm_scenarios_", adm, ".csv")
        )
      )
    }
  )

#################################
#### PRE-CRISIS DISPLACEMENT ####
#################################

df_idp_pre <- read_xlsx(
  file.path(
    input_dir,
    "MT R6-IDPs & Returnees PRE CRISES.xlsx"
  ),
  "IDPs R6 HNO"
) %>%
  row_to_names(1) %>%
  clean_names() %>%
  transmute(
    state,
    locality = case_when(
      locality == "Ar Rahad - GR" ~ "Ar Rahad - G",
      locality == "Ar Rahad" ~ "Ar Rahad - NK",
      .default = locality
    ),
    idps = as.numeric(total_permanent)
  ) %>%
  left_join(
    select(
      df_adm2,
      ADM2_EN,
      ADM1_PCODE,
      ADM2_PCODE
    ),
    by = c("locality" = "ADM2_EN")
  )

write_csv(
  df_idp_pre,
  file.path(
    output_dir,
    "dtm_pre_crisis.csv"
  )
)

##################################
#### POST-CRISIS DISPLACEMENT ####
##################################

sheet_mapping <- list(
  "04_06_2023_DTM_Sudan_Dataset_Situation_SitRep_7__.xlsx" = "(2 June 2023) IDP dataset",
  "05_05_2023_DTM_Sudan_Dataset_Situation_Report_3.xlsx" = "5 May 2023 IDPs",
  "05_07_2023_DTM_Sudan_Dataset_Situation_SitRep_12.xlsx" = "Master Dataset ( 05 July 2023)",
  "08_06_2023_DTM_Sudan_Dataset_Situation_SitRep_8.xlsx" = "Master Dataset (8 June 2023)",
  "12_05_2023_DTM_Sudan_Dataset_Situation_Report_4_0.xlsx" = "Data_2023-05-12",
  "12_07_2023_DTM_Sudan_Dataset_Situation_SitRep_13.xlsx" = "Master Dataset ( 12 July 2023)",
  "14_06_2023_DTM_Sudan_Dataset_Situation_SitRep_9.xlsx" = "Master IDP Dataset_14June2023",
  "21_05_2023_DTM_Sudan_Dataset_Situation_Report_5.xlsx" = "IDPs 21 May 2023 Master Dataset",
  "21_06_2023_DTM_Sudan_Dataset_Situation_SitRep_10.xlsx" = "Master Dataset (21 June 2023)",
  "27_04_2023_DTM_Sudan_Dataset_Situation_SitRep_2.xlsx" = "Dataset (27 April 2023)",
  "28_06_2023_DTM_Sudan_Dataset_Situation_SitRep_11.xlsx" = "Master Dataset (28 June 2023)",
  "29_05_2023_DTM_Sudan_Dataset_Situation_SitRep_6.xlsx" = "IDPs 26 May 2023 Master Datase ",
  "DTM_Sudan_Dataset_Situation_Sitrep_15_27_07_2023.xlsx" = "Master Dataset (26-07-2023)",
  "DTM_Sudan_Dataset_Situation_Sitrep_16_04_08_2023 (1).xlsx" = "Master Dataset (02 Aug 2023)",
  "DTM_Sudan_Dataset_Situation_Sitrep_17_10_08_2023.xlsx" = "Master Dataset (9 Aug 2023)",
  "Master Dataset - Sit Rep 14 (EXTERNAL) (1)_19_07_2023.xlsx" = "Master Dataset ( 19 July 2023)",
  "external-dataset-sit-rep-18-18_08_2023-external-hdx.xlsx" = "Master Dataset (16 Aug 2023)",
  "master-dataset-snapshot-06-08_10_2023-external.xlsx" = "MASTER LIST External(ADMIN2)",
  "master-dataset-27_09_2023-snapshot-05-admin2-hdx.xlsx" = "MASTER LIST External",
  "weekly-displacement-snapshot-04-14_09_2023-external.xlsx" = "MASTER LIST",
  "external-weekly-displacement-snapshot-03-09_09_2023hdx.xlsx" = "MASTER LIST",
  "external-weekly-displacement-snapshot-02-02_09_2023-with-gps_hdx.xlsx" = "MASTER LIST",
  "weekly-displacement-snapshot-01-01_09_2023-with-gps_hdx.xlsx" = "MASTER LIST (22 AUG 2023)"
)

# datasets containing a master dataset sheet
df_list <- imap(
  .x = sheet_mapping,
  .f = \(sheet, file) {
    read_excel(
      path = file.path(
        input_dir,
        "DTM Datasets Post conflict",
        file
      ),
      sheet = sheet
    )
  }
)

# some sheets have names in the first row and just need dropping the hex code in the first row

for (i in c(1, 2, 4, 5, 10, 11, 12, 17)) {
  df_list[[i]] <- df_list[[i]] %>%
    slice(-1)
}

# some have no hex codes and just need to use row to names
df_list[[3]] <- df_list[[3]] %>%
  row_to_names(1)

# some have hex codes and need to use row to names
for (i in c(9, 18, 19, 21)) {
  df_list[[i]] <- df_list[[i]] %>%
    row_to_names(1) %>%
    slice(-1)
}

# some have hex codes and need to use row to names and also clean those names for duplicates
for (i in c(20, 22, 23)) {
  df_list[[i]] <- df_list[[i]] %>%
    row_to_names(1) %>%
    clean_names() %>%
    slice(-1)
}

# these files first need some rows dropped before getting wrangled
df_list[[8]] <- df_list[[8]] %>%
  slice(
    -c(1,2)
  ) %>%
  row_to_names(1)

for (i in c(14, 16)) {
  df_list[[i]] <- df_list[[i]] %>%
    slice(-1) %>%
    row_to_names(1)
}

# some have a mix of names in the first two rows
for (i in c(6, 7, 8, 13, 14, 15, 16)) {
  name_start <- ifelse(
    str_starts(names(df_list[[i]]), "\\.\\.\\.") | is.na(names(df_list[[i]])),
    NA,
    names(df_list[[i]])
  )

  name_start <- zoo::na.locf(name_start)

  name_end <- df_list[[i]][1,] %>% as.vector() %>% as.character()
  name_new <-
    paste0(
      name_start,
      " - ",
      name_end
    )
  df_list[[i]] <- df_list[[i]][-c(1,2),]

  names(df_list[[i]]) <- name_new
}

# now add date based on file date which is the date of the update
for (i in 1:length(df_list)) {
  df_list[[i]]$date <- lubridate::dmy(str_extract(names(sheet_mapping)[i], "[0-9]{2}_[0-9]{2}_[0-9]{4}"))
}

# now we wrangle the data to the information that we need

# manually add pcodes to files that are missing them

df_list[[5]] <- left_join(
  df_list[[5]],
  df_adm1,
  by = c("STATE OF ORIGIN" = "ADM1_EN")
) %>%
  mutate(
    state_code_origin = ADM1_PCODE,
    locality_code_origin = NA
  )

df_dtm_full <- map(
  .x = df_list,
  .f = \(df) {
    clean_df <- df %>%
      janitor::clean_names()

    # dropping columns that duplicate itself
    # this is dropping columns from data frame 6
    if ("displacement_id_ps" %in% names(clean_df)) {
      clean_df <- select(
        clean_df,
        -any_of("nationality_by_id_ps_individuals_total_id_ps")
      )
    }

    # again dropping for data frame 6
    if ("displacement_h_hs" %in% names(clean_df)) {
      clean_df <- select(
        clean_df,
        -any_of(
          c(
            "shelter_category_by_id_ps_households_total_h_hs",
            "shelter_category_by_households_total_h_hs"
          )
        )
      )
    }
    clean_df %>%
      rename_with(
        .fn = \(x) str_replace_all(
          string = x,
          pattern = c(
            "^state_code$" = "ADM1_PCODE - displacement",
            "^state_of_displacement_pcode$" = "ADM1_PCODE - displacement",
            "^place_of_origin_state_code$" = "ADM1_PCODE - displacement", # weird naming for df #7
            "^displacement_state_code$" = "ADM1_PCODE - displacement",
            "^state_pcode_of_affected_population$" = "ADM1_PCODE - displacement",
            "^state_code_10$" = "ADM1_PCODE - displacement",
            "^state_code_number_adm1_pcode$" = "ADM1_PCODE - displacement",
            "^state_code_5$" = "ADM1_PCODE - displacement",
            "^locality_code$" = "ADM2_PCODE - displacement",
            "^locality_of_displacement_pcode$" = "ADM2_PCODE - displacement",
            "^displacement_locality_code$" = "ADM2_PCODE - displacement",
            "^nationalities_locality_code$" = "ADM2_PCODE - displacement", # weird naming for df #7
            "^locality_pcode_of_affected_population$" = "ADM2_PCODE - displacement",
            "^locality_code_11$" = "ADM2_PCODE - displacement",
            "^locality_code_number_adm2_pcode$" = "ADM2_PCODE - displacement",
            "^state_code_origin$" = "ADM1_PCODE - origin",
            "^state_of_origin_pcode$" = "ADM1_PCODE - origin",
            "^shelter_category_state_code_origin$" = "ADM1_PCODE - origin",
            "^origin_state_code$" = "ADM1_PCODE - origin",
            "^state_pcode_of_origin$" = "ADM1_PCODE - origin",
            "^state_code_19$" = "ADM1_PCODE - origin",
            "^state_code_14$" = "ADM1_PCODE - origin",
            "^state_code_number_adm1_origin_pcode$" = "ADM1_PCODE - origin",
            "^origin_locality_code$" = "ADM2_PCODE - origin",
            "^locality_of_origin_pcode$" = "ADM2_PCODE - origin",
            "^locality_code_origin$" = "ADM2_PCODE - origin",
            "^shelter_category_locality_code_origin$" = "ADM2_PCODE - origin",
            "^locality_pcode_of_origin$" = "ADM2_PCODE - origin",
            "^locality_code_20$" = "ADM2_PCODE - origin",
            "^locality_code_number_adm2_origin_pcode$" = "ADM2_PCODE - origin",
            "^locality_code_2$" = "ADM2_PCODE - origin",
            "^id_ps$" = "idps",
            "^number_idp_ind$" = "idps",
            "^nationality_by_id_ps_individuals_total_id_ps$" = "idps",
            "^shelter_category_id_ps$" = "idps",
            "^number_idp_individuals$" = "idps",
            "^displacement_id_ps$" = "idps",
            "^id_ps_number_affected_idps_ind$" = "idps",
            "^h_hs$" = "hhs",
            "^shelter_category_by_id_ps_households_total_h_hs$" = "hhs",
            "^number_idp_hh$" = "hhs",
            "^shelter_category_h_hs$" = "hhs",
            "^shelter_category_by_households_total_h_hs$" = "hhs",
            "^displacement_h_hs$" = "hhs",
            "^h_hs_number_affected_idps_hh$" = "hhs",
            "^number_idp_households$" = "hhs"
          )
        )
      ) %>%
      transmute(
        date,
        `ADM1_PCODE - displacement`,
        `ADM2_PCODE - displacement`,
        across(
          any_of(
            ends_with(" - origin")
          )
        ),
        idps = as.numeric(idps),
        hhs = as.numeric(hhs)
      )
  }
) %>%
  list_rbind() %>%
  filter(
    !is.na(idps)
  )

# now generate different datasets
# dtm location of displacement data
df_dtm_disp <- df_dtm_full %>%
  group_by(
    date,
    ADM1_PCODE = `ADM1_PCODE - displacement`,
    ADM2_PCODE = `ADM2_PCODE - displacement`
  ) %>%
  summarize(
    idps = sum(idps),
    hhs = sum(hhs),
    .groups = "drop"
  )

# dtm location of origin datasets
df_dtm_orig <- df_dtm_full %>%
  group_by(
    date,
    ADM1_PCODE = `ADM1_PCODE - origin`,
    ADM2_PCODE = `ADM2_PCODE - origin`
  ) %>%
  summarize(
    idps = sum(idps),
    hhs = sum(hhs),
    .groups = "drop"
  )

# save out the DTM data
write_csv(
  df_dtm_disp,
  file.path(
    output_dir,
    "dtm_displacement_location.csv"
  )
)

write_csv(
  df_dtm_orig,
  file.path(
    output_dir,
    "dtm_origin_location.csv"
  )
)

#################################
#### UNHCR DISPLACEMENT DATA ####
#################################

df_unhcr <- read_csv(
  file.path(
    input_dir,
    "UNHCR Refugees Trends since begining of Conflict .csv"
  )
) %>%
  clean_names() %>%
  transmute(
    date = dmy(paste(date, "-2023")),
    refugees = total
  )

write_csv(
  df_unhcr,
  file.path(
    output_dir,
    "unhcr_data.csv"
  )
)

###########################
#### UPDATED SCENARIOS ####
###########################

scen2_xl <- file.path(
  input_dir,
  "(External) ANNEX 1 IDPs_by_locality.xlsx"
)

df_scenarios2 <- map(
  .x = excel_sheets(scen2_xl),
  .f = \(sheet) {
    read_excel(
      scen2_xl,
      sheet = sheet,
      range = "E26:K26",
      col_names = FALSE
    ) |>
      pivot_longer(
        cols = everything(),
        values_to = "displacement"
      ) |>
      transmute(
        scenario = !!sheet,
        date = seq.Date(
          from = as.Date("2023-09-01"),
          to = as.Date("2024-03-01"),
          by = "month"
        ) - days(1),
        displacement
      )
  }
) |>
  list_rbind()
