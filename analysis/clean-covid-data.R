library(tidyverse)
library(here)
library(lubridate)
library(data.table)
library(janitor)



# Combine zip code data ---------------------------------------------------
## General zip code data

# Scale zip code level variables
zip_data_merged <- read_csv( # area
    here("data/zip-code-data", "zip-area2.csv"),
    col_types = cols(
      .default = col_skip(),
      NAME = col_character(),
      Zip = col_character(),
      AreaKm = col_double()
    )
  ) %>%
  select(name = NAME, zip = Zip, area_km = AreaKm)


zip_data_merged <- read_csv( # population
    here("data/zip-code-data", "zip-pop.csv"),
    col_types = cols(
      .default = col_skip(),
      Zip = col_character(),
      Population = col_integer()
    )
  ) %>%
  drop_na() %>%
  mutate(population = Population / 1000) %>%
  select(zip = Zip, population) %>% 
  full_join(y =  zip_data_merged, by = "zip") %>% 
  mutate(pop_density = population / area_km) %>% 
  mutate(adj_pop_density = scale(pop_density , center = TRUE, scale = TRUE))



zip_data_merged <- read_csv( # income
    here("data/zip-code-data", "income-by-zip2.csv"),
    col_types = cols(
      .default = col_skip(),
      Zip = col_character(),
      IncomeMed = col_integer(),
      IncPeriodofMeas = col_character()
    )
  ) %>%
  mutate(med_income = IncomeMed / 10000) %>%
  filter(IncPeriodofMeas == "2014-2018") %>%
  select(zip = Zip, med_income) %>% 
  full_join(y =  zip_data_merged, by = "zip") %>% 
  mutate(adj_med_income = scale(med_income, center = TRUE, scale = TRUE))


zip_data_merged <- read_csv( # education
    here("data/zip-code-data", "education-by-zip.csv"),
    col_types = cols(
      .default = col_skip(),
      Zip = col_character(),
      PercentBach = col_double()
    )
  ) %>%
  select(zip = Zip, percent_bachelors = PercentBach) %>% 
  full_join(y =  zip_data_merged, by = "zip") %>% 
  mutate(adj_perc_bach = scale(percent_bachelors, center = TRUE, scale = TRUE)) %>% 
  mutate(adj_perc_bach_quar = with(
    zip_data_merged,
    cut(
      adj_perc_bach,
      breaks = quantile(adj_perc_bach, probs = seq(0, 1, by = 0.25)),
      include.lowest = TRUE,
      labels = c("Q1", "Q2", "Q3", "Q4")
  )))


zip_data_merged <- read_csv( # insurance
    here("data/zip-code-data", "insurance-by-zip.csv"),
    col_types = cols(
      .default = col_skip(),
      Zip = col_character(),
      PercentInsured = col_double()
    )
  ) %>%                           
  select(zip = Zip, percent_insured = PercentInsured) %>% 
  full_join(y =  zip_data_merged, by = "zip") %>% 
  mutate(adj_perc_insured = scale(percent_insured, center = TRUE, scale = TRUE)) %>% 
  mutate(adj_perc_insured_quar = with(
    zip_data_merged,
    cut(
      adj_perc_insured,
      breaks = quantile(adj_perc_insured, probs = seq(0, 1, by = 0.25)),
      include.lowest = TRUE,
      labels = c("Q1", "Q2", "Q3", "Q4")
  )))

house_data_og <- read_csv( # house crowding
    here("data/zip-code-data", "house-crowding.csv"),
    col_types = cols(
      .default = col_skip(),
      location = col_character(),
      indicator_rate_value = col_double(),
      period_of_measure = col_date()
    )
  ) %>% 
  janitor::clean_names() %>% 
  filter(period_of_measure == "2015-2019") %>% 
  filter(breakout_subcategory == "Owners") %>% 
  select(zip = location, house_crowding = indicator_rate_value) %>% 
  mutate(zip = as.character(zip)) %>% 
  full_join(y =  zip_data_merged, by = "zip")

zip_data_merged <- merge(x = zip_data_merged, y = house_data_og, by = "zip")

# Hospital OC zip code level data
hos_bed_gov <- read_csv(
  here("data/mortality-data", "covid19hospitalbycounty.csv"),
  na = c("", " "),
  col_types = cols(
    .default = col_skip(),
    county = col_character(),
    todays_date = col_date("%Y-%m-%d"),
    hospitalized_covid_patients = col_double(),
    all_hospital_beds = col_double(),
    icu_covid_confirmed_patients = col_double(),
    icu_suspected_covid_patients = col_double(),
    icu_available_beds = col_double()
  )
) %>% 
  filter(county == "Orange") 


first_date <- sort(hos_bed_gov$todays_date)[1]
if(month(params$first_test_date) == month(first_date) & day(params$first_test_date) < day(first_date)) {
  dates_missing <- seq(as.Date(params$first_test_date), first_date, by = "days")
  dates_missing <- dates_missing[-length(dates_missing)]
  num_dm <- length(dates_missing)
  missing_rows <- data.frame(
    rep("Orange", num_dm), 
    dates_missing, 
    rep(NA, num_dm), 
    rep(NA, num_dm), 
    rep(NA, num_dm),
    rep(NA, num_dm),
    rep(NA, num_dm)
  )
  colnames(missing_rows) <- colnames(hos_bed_gov)
  hos_bed_gov <- rbind(missing_rows, hos_bed_gov)
} else {
  print("Error: fix hospital data dates")
}

# Beds were not recorded before 2020-04-20. To fill in missing:
# For ICU available beds the earliest value is used
# For percent of beds not used by COVID-19 patients the earliest value was used
hos_bed_gov <- hos_bed_gov %>% 
  mutate(avail_icu_beds = ifelse(
    is.na(icu_available_beds), 
    params$oc_icu_avail_beds_earliest_val,
    icu_available_beds
  )) %>% 
  mutate(perc_avail_beds = ifelse(
    is.na(hospitalized_covid_patients) | is.na(all_hospital_beds),
    100 * (1 - params$oc_hos_covid_pateients_earliest_val / params$oc_all_hos_bed_earliest_val),
    100 * (1 - hospitalized_covid_patients / all_hospital_beds)
  )) %>% 
  mutate(covid_icu_beds = ifelse(
    is.na(icu_covid_confirmed_patients) | is.na(icu_suspected_covid_patients),
    params$oc_icu_full_beds_earliest_val,
    icu_covid_confirmed_patients + icu_suspected_covid_patients
  )) %>% 
  select(
    test_date = todays_date,
    avail_icu_beds,
    perc_avail_beds,
    covid_icu_beds
  )

hos_bed_gov$adj_perc_avail_beds <- scale(
  hos_bed_gov$perc_avail_beds, 
  center = TRUE, 
  scale = TRUE
)

hos_bed_gov$adj_avail_icu_beds <- scale(
  hos_bed_gov$avail_icu_beds, 
  center = TRUE, 
  scale = TRUE
)

hos_bed_gov$adj_covid_icu_beds <- scale(
  hos_bed_gov$covid_icu_beds,
  center = TRUE,
  scale = TRUE
)


# clean-all-tests-data ----------------------------------------------------



# Test results need to be categorized ------------------------------------------------------
negative_test_synonyms <- c(
  "not detected",
  "negative",
  "coronavirus 2019 novel not detected",
  "negative (qualifier value)",
  "not detected (qualifier value)",
  "sars cov-2 negative",
  "undetected",
  "inst_negative",
  "neg-see report",
  "sars-cov-2 rna not detected by naa",
  "none detected", 
  "not detected in pooled specimen"
)

positive_test_synonyms <- c(
  "detected",
  "coronavirus 2019 novel positive",
  "positive",
  "positive (qualifier value)",
  "sars cov-2 positive",
  "detected (qualifier value)",
  "presumptive pos",
  "positive for 2019-ncov",
  "presumptive positive",
  "coronavirus 2019 novel presumptive pos",
  "coronavirus 2019 novel detected",
  "yes",
  "coronavirus 2019 novel",
  "presumptive positive for 2019-ncov",
  "sars cov-2 presumptive pos",
  "presumptive pos. for 2019-ncov",
  "presumptive positive (qualifier value)",
  "presumptive detected",
  "reactive",
  "sars-cov-2",
  "not detected in pooled specimen (qualifier value)"
)

other_test_synonyms <- c(
  "inconclusive",
  "indeterminate",
  "specimen unsatisfactory",
  "invalid",
  "test not performed",
  "not provided (qualifier value)",
  "see comment",
  "tnp",
  "coronavirus 2019 novel inconclusive",
  "not tested",
  "phoned results (and readback confirmed) to:",
  "see note",
  "clotted",
  "coronavirus 2019 novel unsatisfactory",
  "equivocal",
  "non reactive",
  "result comments",
  "sars cov-2 inconclusive",
  "test not done",
  "test not perf",
  "not pregnant",
  "biofiresarsneg",
  "equivocal result",
  "coronavirus 2019 novel inconcluside",
  "unsatisfactory",
  "undefined",
  "*corrected report* by",
  "specimen unsatifactory for evaluation",
  "warning....please disregard results.",
  "presumptive result to be confirmed",
  "indeterminate (qualifier value)", 
  "invalid result", 
  "specimen unsatisfactory for evaluation",
  "acinetobacter baumannii (organism)",
  "carbapenem resistant pseudomonas aeruginosa",
  "enterobacter cloacae complex (organism)",
  "enterococcus faecalis",
  "genus enterococcus",
  "interpretive information: 2019 novel coronavirus sars-cov-2 by pcr",
  "multiple drug-resistant serratia marcescens",
  "specimen received mislabeled"
)


read_all_pcr <- function(file_path, start_date, end_date) {
  pcr_results_original <- read_csv(
    file_path,
    col_types = cols(
      .default = col_skip(),
      Age = col_integer(),
      Sex = col_character(),
      Ethnicity = col_character(),
      Race = col_character(),
      Specimen.Collected.Date = col_date("%m-%d-%Y"),
      Resulted.Organism = col_character(),
      Zip = col_character(),
      unique_num = col_character()
    )
  ) 
  
  full_num_data_cases <- pcr_results_original %>% 
    filter(Specimen.Collected.Date >= ymd(start_date) & Specimen.Collected.Date <= ymd(end_date)) %>% 
    nrow()
  
  full_num_id_cases <- pcr_results_original %>% 
    filter(Specimen.Collected.Date >= ymd(start_date) & Specimen.Collected.Date <= ymd(end_date)) %>% 
    select(unique_num) %>% 
    unique() %>% 
    nrow()
  
  pcr_results_original$Race[is.na(pcr_results_original$Race)] <- "Unknown"
  pcr_results_original$Ethnicity[is.na(pcr_results_original$Ethnicity)] <- "Unknown"
  
  hispanic_race_unknown <- (
    (pcr_results_original$Race == "Other" & 
       pcr_results_original$Ethnicity == "Hispanic or Latino") |
      (pcr_results_original$Race == "Unknown" & 
         pcr_results_original$Ethnicity == "Hispanic or Latino") |
      (pcr_results_original$Race == "Multiple Races" & 
         pcr_results_original$Ethnicity == "Hispanic or Latino") 
  )
  
  non_hispanic_unknown <- (
    (pcr_results_original$Race == "Unknown" & 
       pcr_results_original$Ethnicity != "Hispanic or Latino") |
      (pcr_results_original$Race == "Multiple Races" & 
         pcr_results_original$Ethnicity != "Hispanic or Latino") |
      (pcr_results_original$Race == "Other" & 
         pcr_results_original$Ethnicity != "Hispanic or Latino")
  )
  
  pcr_results_original_new_race <- data.frame(
    pcr_results_original, 
    "race1" = str_to_lower(pcr_results_original$Race)
  )
  pcr_results_original_new_race$race1[hispanic_race_unknown] <- "hispanic or latino"
  pcr_results_original_new_race$race1[non_hispanic_unknown] <- "unknown"
  
  pcr_results_adjusted <- pcr_results_original_new_race %>%
    filter(!is.na(Resulted.Organism)) %>%
    mutate(test_result = fct_collapse(
      str_to_lower(Resulted.Organism),
      negative = negative_test_synonyms,
      positive = positive_test_synonyms,
      unknown = other_test_synonyms) 
    ) %>%
    mutate(sex = fct_collapse(
      str_to_lower(Sex),
      male = "m",
      female = "f",
      unknown = c("d", "g", "i", "tf", "tm", "u")
    )) %>%
    mutate(race = factor(
      race1, 
      levels = c(
        "white",
        "asian",
        "black or african american",
        "hispanic or latino",
        "american indian or alaska native",
        "native hawaiian or other pacific islander",
        "unknown"
      ),
    )) %>% 
    mutate(
      time_days = as.integer(round(difftime(
        Specimen.Collected.Date, 
        start_date, 
        units = "days"
      )))
    ) %>%
    select(
      id = unique_num, 
      posted_date = Specimen.Collected.Date, 
      time_days,
      test_result, 
      age = Age,
      sex,
      race,
      ethnicity = Ethnicity,
      zip = Zip,
    ) %>%
    filter((posted_date >= ymd(start_date)) & (posted_date <= ymd(end_date))) %>%
    filter(test_result != "unknown") %>%
    filter(sex != "unknown") %>%
    mutate(zip = str_sub(zip, end = 5)) %>%
    drop_na() %>% 
    group_by(id) %>%
    arrange(posted_date) %>%
    ungroup()
  
  
  if(length(levels(pcr_results_adjusted$test_result)) != 3) warning("New test result category not accounted for.")
  
  pcr_results_adjusted$covid_positive <- ifelse( pcr_results_adjusted$test_result == "positive", 1, 0)
  
  
  # Extract df of all observations with an id that has inconsistencies for the demographic variables (age, sex, race)
  pcr_rep_id <- pcr_results_adjusted[(
    duplicated(pcr_results_adjusted$id, fromLast=TRUE) | 
      duplicated(pcr_results_adjusted$id)
  ), ]
  
  pcr_rep_id2 <- pcr_rep_id %>%
    group_by(id) %>% 
    mutate(reasonable_ages = diff(range(age)) <= 1) %>%
    #    mutate(identical_race = n_distinct(race) == 1) %>%
    mutate(identical_sex = n_distinct(sex) == 1) %>%
    ungroup()
  
  pcr_inconsistent <- data.frame(
    pcr_rep_id2[(
      !pcr_rep_id2$reasonable_ages | 
        !pcr_rep_id2$identical_sex #| 
      #      !pcr_rep_id2$identical_race
    ), ]
  )
  
  inconsistent_counts <- c(
    length(unique(pcr_inconsistent$id[(!pcr_inconsistent$reasonable_ages)])),
    length(unique(pcr_inconsistent$id[(!pcr_inconsistent$identical_sex)]))#,
    #    length(unique(pcr_inconsistent$id[(!pcr_inconsistent$identical_race)]))
  )
  names(inconsistent_counts) <- c(
    "number_id_age_inconsistencies", 
    "number_id_sex_inconsistencies"#,
    #    "number_id_race_inconsistencies"
  )
  
  pcr_results_consistent <- pcr_results_adjusted[!(pcr_results_adjusted$id %in% pcr_inconsistent$id), ]
  
  
  # Need to keep all observations for a person up to and including their first positive
  first_pos <- pcr_results_consistent %>%
    filter(test_result == "positive") %>%
    group_by(id) %>%
    summarise(first_pos = min(posted_date))
  
  pcr_results_reduced <- left_join(pcr_results_consistent, first_pos) %>%
    mutate(first_pos = replace_na(first_pos, lubridate::ymd("9999-12-31"))) %>%
    filter(posted_date <= first_pos) %>%
    select(-first_pos) %>%
    distinct()
  
  
  # Add zip code level data and merge with pcr results
  zip_area_oc <- read_csv(
    here("data/zip-code-data", "zip-area2.csv"),
    col_types = cols(
      .default = col_skip(),
      NAME = col_character(),
      Zip = col_character(),
      AreaKm = col_double()
    )
  ) %>%
    select(name = NAME, zip = Zip, area_km = AreaKm)
  
  zip_pop_oc <- read_csv(
    here("data/zip-code-data", "zip-pop.csv"),
    col_types = cols(
      .default = col_skip(),
      Zip = col_character(),
      Population = col_integer()
    )
  ) %>%
    drop_na() %>%
    mutate(population = Population / 1000) %>%
    select(zip = Zip, population)
  
  zip_data_merged <- merge(x = zip_area_oc, y = zip_pop_oc, by = "zip")
  zip_data_merged$pop_density <- zip_data_merged$population / zip_data_merged$area_km
  
  zip_income_oc <- read_csv(
    here("data/zip-code-data", "income-by-zip2.csv"),
    col_types = cols(
      .default = col_skip(),
      Zip = col_character(),
      IncomeMed = col_integer(),
      IncPeriodofMeas = col_character()
    )
  ) %>%
    mutate(med_income = IncomeMed / 10000) %>%
    filter(IncPeriodofMeas == "2014-2018") %>%
    select(zip = Zip, med_income)
  
  zip_data_merged <- merge(x = zip_data_merged, y = zip_income_oc, by = "zip")
  
  zip_education_oc <- read_csv(
    here("data/zip-code-data", "education-by-zip.csv"),
    col_types = cols(
      .default = col_skip(),
      Zip = col_character(),
      PercentBach = col_double()
    )
  ) %>%
    select(zip = Zip, percent_bachelors = PercentBach)
  
  zip_data_merged <- merge(x = zip_data_merged, y = zip_education_oc, by = "zip")
  
  zip_insurance_oc <- read_csv(
    here("data/zip-code-data", "insurance-by-zip.csv"),
    col_types = cols(
      .default = col_skip(),
      Zip = col_character(),
      PercentInsured = col_double()
    )
  ) %>%                           
    select(zip = Zip, percent_insured = PercentInsured)
  
  zip_data_merged <- merge(x = zip_data_merged, y = zip_insurance_oc, by = "zip")
  
  
  # Scale zip code level variables
  zip_data_merged$adj_pop_density <- scale(
    zip_data_merged$pop_density , 
    center = TRUE, 
    scale = TRUE
  )
  
  zip_data_merged$adj_med_income <- scale(
    zip_data_merged$med_income, 
    center = TRUE, 
    scale = TRUE
  )
  
  zip_data_merged$adj_perc_bach <- scale(
    zip_data_merged$percent_bachelors,
    center = TRUE,
    scale = TRUE
  )
  
  zip_data_merged$adj_perc_bach_quar <- with(
    zip_data_merged,
    cut(adj_perc_bach,
        breaks = quantile(adj_perc_bach, 
                          probs = seq(0, 1, by = 0.25)),
        include.lowest = TRUE,
        labels = c("Q1", "Q2", "Q3", "Q4"))
  )
  
  zip_data_merged$adj_perc_insured <- scale(
    zip_data_merged$percent_insured,
    center = TRUE,
    scale = TRUE
  )
  
  zip_data_merged$adj_perc_insured_quar <- with(
    zip_data_merged,
    cut(
      adj_perc_insured,
      breaks = quantile(adj_perc_insured, probs = seq(0, 1, by = 0.25)),
      include.lowest = TRUE,
      labels = c("Q1", "Q2", "Q3", "Q4")
    )
  )
  
  #23 rows in 92678 zipcode we don't have area data for
  pcr_results_reduced$old_zip <- pcr_results_reduced$zip
  pcr_results_reduced$zip[pcr_results_reduced$old_zip == "92678"] <- "92679"
  pcr_results_merged <- merge(x = pcr_results_reduced, y = zip_data_merged, by = "zip")
  pcr_results_merged$old_zip <- factor(pcr_results_merged$old_zip)
  pcr_results_merged$zip <- factor(pcr_results_merged$zip)
  
  
  # Group ages 
  age_breaks <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 50, 60, 70, 80, 200)
  age_labels <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39",
                  "40-49","50-59","60-69","70-79","80+")
  setDT(pcr_results_merged)[, age_group := cut(
    age, 
    breaks = age_breaks, 
    right = FALSE, 
    labels = age_labels
  )]
  
  pcr_results_merged$age_group <- factor(
    pcr_results_merged$age_group,
    levels = age_labels
  )
  
  #technically we would want to do this differently for days but we don't used scaled in final model
  pcr_results_merged$adj_time_days <- scale( 
    pcr_results_merged$time_days,
    center = TRUE,
    scale = TRUE
  )
  
  
  house_data_og <- read_csv(here("data/zip-code-data", "house-crowding.csv")) %>% 
    janitor::clean_names() %>% 
    filter(period_of_measure == "2015-2019") %>% 
    filter(breakout_subcategory == "Owners") %>% 
    select(zip = location, house_crowding = indicator_rate_value)
  
  
  pcr_results_merged <- house_data_og %>% 
    mutate(zip = factor(zip)) %>% 
    right_join(y = pcr_results_merged, by = "zip")
  
  # Count missingness by type
  missing_counts <- c(
    full_num_data_cases, 
    full_num_data_cases - nrow(pcr_results_adjusted), 
    nrow(pcr_results_adjusted) - nrow(pcr_results_consistent),
    nrow(pcr_results_consistent) - nrow(pcr_results_reduced),
    nrow(pcr_results_reduced) - nrow(pcr_results_merged)
  )
  missing_ids <- c(
    full_num_id_cases,
    full_num_id_cases - length(unique(pcr_results_adjusted$id)),
    length(unique(pcr_results_adjusted$id)) - length(unique(pcr_results_consistent$id)),
    length(unique(pcr_results_consistent$id)) - length(unique(pcr_results_reduced$id)),
    length(unique(pcr_results_reduced$id)) - length(unique(pcr_results_merged$id))
  )
  missing_tab <- data.frame("missing_counts" = missing_counts, "missing_ids" = missing_ids)
  rownames(missing_tab) <- c(
    "full_num_data_cases", 
    "num_na_cases_removed", 
    "num_inconsistent_cases_removed",
    "num_consecutive_cases_removed",
    "num_bad_zip_cases_removed"
  )
  
  
  list(
    "pcr_results_merged" = pcr_results_merged, 
    "zip_data_merged" = zip_data_merged, 
    "missing" = missing_tab,
    "inconsistencies" = inconsistent_counts
  )
}
