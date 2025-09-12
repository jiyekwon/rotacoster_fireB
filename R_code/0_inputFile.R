library(countrycode)
library(dplyr)
library(reshape2)
library(readxl)
library(tidyr)
library(stringr)
# file_list<- readxl::read_xlsx("./input/country_select/meta_file_for_lmics_model.xlsx")
# country_case_summary<- read.csv("./input/country_vaxx_intro_summary.csv", check.names = F)
# country_dosing <- read.csv("./input/Current Dosing Schedule(view-hub.org).csv", check.names = FALSE)
# country_dosing.who <- read_xlsx("./input/Vaccination schedule for Rotavirus 2024-07-10 14-05 UTC.xlsx")
# country_coverage<- read.csv("./input/INPUT_country_coverage.csv")

# CODE: file_list directory --------------------------------------------# 
make_paths <- function(scenarios) {
  scenarios <- as.character(scenarios)
  n <- length(scenarios)
  parent <- rep(NA_character_, n)
  subdir <- rep(NA_character_, n)
  
  # 1) current_cov_* -> parent = current_vacc_status, subdir = scenario
  idx_currentcov <- grepl("^current_cov_", scenarios)
  parent[idx_currentcov] <- "current_vacc_status"
  subdir[idx_currentcov] <- scenarios[idx_currentcov]
  
  # 2) scenarios that already start with all_uses_ -> parent = <prefix before _cov>, subdir = scenario
  idx_all_uses <- grepl("^all_uses_", scenarios)
  parent[idx_all_uses] <- sub("_(cov_.*)$", "", scenarios[idx_all_uses])
  subdir[idx_all_uses] <- scenarios[idx_all_uses]
  
  # 3) scenarios that start with all_  (but not all_uses_) -> parent = all_uses_<rest>, subdir = scenario
  idx_all <- grepl("^all_", scenarios) & !idx_all_uses
  parent[idx_all] <- paste0("all_uses_", sub("^all_", "", sub("_(cov_.*)$", "", scenarios[idx_all])))
  subdir[idx_all] <- scenarios[idx_all]
  
  # 4) suspend_vac (and similar single-folder scenarios) -> parent is the scenario, no nested subdir
  idx_suspend <- scenarios == "suspend_vac"
  parent[idx_suspend] <- "suspend_vac"
  subdir[idx_suspend] <- ""
  
  # 5) anything still NA -> treat as top-level parent with no nested subdir
  idx_remain <- is.na(parent)
  parent[idx_remain] <- scenarios[idx_remain]
  subdir[idx_remain] <- ""
  
  # build paths: if subdir is empty -> ./input/country_select/<parent>
  path <- ifelse(
    subdir == "" | is.na(subdir),
    file.path("./input/country_select", parent),
    file.path("./input/country_select", parent, subdir)
  )
  
  return(path)
}


# CODE: file_list directory --------------------------------------------# 
# Set the parent directory
parent_dir <- "./input/country_select"

# Get all top-level folders inside results_2025
top_folders <- list.dirs(parent_dir, recursive = FALSE, full.names = TRUE)

# Prepare a list to collect results
all_files <- list()
doses_files <- list()  # Create a list for files with '_doses'

for (folder in top_folders) {
  subdirs <- list.dirs(folder, recursive = FALSE, full.names = TRUE)
  
  if (length(subdirs) == 0 || all(subdirs == folder)) {
    # Flat structure: use folder name as column, list files directly
    folder_name <- basename(folder)
    files <- list.files(folder, full.names = FALSE)
    
    # Store all files (including '_tpop.csv' files) in all_files
    all_files[[folder_name]] <- files[!grepl("_doses", files)]  # Exclude '_doses' files
    
    # Store files with '_doses' separately
    doses_files[[folder_name]] <- files[grepl("_doses", files)]
    
    # Store population files ending with '_tpop.csv'
    #population_files[[folder_name]] <- files[grepl("_tpop.csv$", files)]
  } else {
    # Nested structure: each subfolder becomes a column
    for (subdir in subdirs) {
      subdir_name <- basename(subdir)
      files <- list.files(subdir, full.names = FALSE)
      
      # Store all files (including '_tpop.csv' files) in all_files
      all_files[[subdir_name]] <- files[!grepl("_doses", files)]  # Exclude '_doses' files
      
      # Store files with '_doses' separately
      doses_files[[subdir_name]] <- files[grepl("_doses", files)]
      
    }
  }
}

# Normalize list lengths by padding with NAs for all files (including '_tpop.csv' files)
max_len <- max(sapply(all_files, length))
all_files_padded <- lapply(all_files, function(x) c(x, rep(NA, max_len - length(x))))

# Combine into data frame for all files (excluding '_doses' files)
df_all_files <- as.data.frame(all_files_padded, stringsAsFactors = FALSE)

# Normalize list lengths by padding with NAs for doses files
max_len_doses <- max(sapply(doses_files, length))
doses_files_padded <- lapply(doses_files, function(x) c(x, rep(NA, max_len_doses - length(x))))

# Combine into data frame for '_doses' files
df_doses_files <- as.data.frame(doses_files_padded, stringsAsFactors = FALSE)
df_doses_files<- df_doses_files %>% select(-c(suspend_vac, population))

# MS cases 
df_ms_only <- df_all_files %>%
  mutate(across(everything(), ~ ifelse(grepl("_MS\\.csv$", .), ., NA)))

# Remove rows where all columns are NA
df_ms_only <- df_ms_only %>%
  filter(if_any(everything(), ~ !is.na(.)))%>% 
  select(-population)
# NS cases
df_ns_only <- df_all_files %>%
  mutate(across(everything(), ~ ifelse(grepl("_NS\\.csv$", .), ., NA)))

# Remove rows where all columns are NA
df_ns_only <- df_ns_only %>%
  filter(if_any(everything(), ~ !is.na(.))) %>% 
  select(-population)

# CODE -----------------------------------------------------------------------# 
data_list<- list(df_doses_files, df_ms_only, df_ns_only)

data_list<- lapply(data_list, function(data){
  data$Country<- gsub("_[^_]+$", "", data[,1])
  # Convert country name to ISO3
  data <- data %>%
    mutate(iso = countrycode(Country, origin = 'country.name.en', destination = 'genc3c'))
  data$iso[data$Country=="Republic_of_the_Congo"]<- "COG"
  data$iso[data$Country=="Palestine"]<- "PSE"
  
  return(data)
  })


data.long.list<- lapply(data_list, function(data){
  data.long<- pivot_longer(data, -c(iso, Country), 
                                values_to = "Value", names_to = "Scenario")
  
  data.long = data.long %>% 
    mutate(
      dose_information_Intro = case_when(
      	str_detect(Scenario, "all_1_6_10") ~  "Neonatal_1/6/10",
        str_detect(Scenario, "all_1_10_14") ~ "Neonatal_1/10/14",
        str_detect(Scenario, "all_uses_2_dose_plus_40") ~ "RV_6/10/40",
        str_detect(Scenario, "all_6_10") ~ "RV_6/10",
        str_detect(Scenario, "all_6_10_14") ~ "RV_6/10/14",
        str_detect(Scenario, "all_10_14") ~  "RV_10/14",
        str_detect(Scenario, "current") ~  "Keep_current",
        str_detect(Scenario, "suspend_vac") ~ "N/A"),
      
      dose_information_NVax = case_when(
      	str_detect(Scenario, "all_1_6_10") ~  "Neonatal_1/6/10",
        str_detect(Scenario, "all_1_10_14") ~ "Neonatal_1/10/14",
        str_detect(Scenario, "all_uses_2_dose_plus_40") ~ "RV_6/10/40",
        str_detect(Scenario, "all_6_10") ~ "RV_6/10",
        str_detect(Scenario, "all_6_10_14") ~ "RV_6/10/14",
        str_detect(Scenario, "all_10_14") ~  "RV_10/14",
        str_detect(Scenario, "current") ~  "N/A",
        str_detect(Scenario, "suspend_vac") ~ "N/A"),
      
      severity = gsub(".*_(.*)\\.csv$", "\\1",  data.long$Value),
      
      #Countries with vaccine use their current rotavirus coverage, while countries without vaccine use their current DPT coverage (curr_cov_novac_dpt_cov). Scale up coverage to 95% for all countries (curr_cov_novac_95_cov).
      coverage = case_when(
        str_detect(Scenario, "cov_50") ~ "50%",
        str_detect(Scenario, "cov_60") ~ "60%",
        str_detect(Scenario, "cov_70") ~ "70%",
        str_detect(Scenario, "cov_80") ~ "80%",
        str_detect(Scenario, "cov_90") ~ "90%",
        str_detect(Scenario, "cov_95") ~ "95%",
        str_detect(Scenario, "cov_curr") ~ "current_coverage",
        str_detect(Scenario, "suspend_vac") ~ "0%",
        TRUE ~ NA
      ),
      
      path = make_paths(Scenario) 
    )
  
  data.long<- data.long %>% rename(ISO = iso)
  
  return(data.long)
})

MS_dataset<- data.long.list[[2]]
NS_dataset<- data.long.list[[3]]
file_list_all<- full_join(MS_dataset,NS_dataset)
doses_list<- data.long.list[[1]]
write.csv(file_list_all, file = "./input/country_select/meta_file_longFormat_list.csv" )
write.csv(doses_list, file = "./input/country_select/dose_file_longFormat_list.csv" )