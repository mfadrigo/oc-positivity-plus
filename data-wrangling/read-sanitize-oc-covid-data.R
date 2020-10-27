# Damon Bayer and Isaac Goldsteinh's code adjusted

library(tidyverse)
library(lubridate)
library(here)

# Test results need to be categorized ------------------------------------------------------
negative_test_synonyms <- c("not detected",
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
                            "not detected in pooled specimen")

positive_test_synonyms <- c("detected",
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
                            "sars-cov-2")

other_test_synonyms <- c("inconclusive",
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
                         # "cryptococcus neoformans",
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
                         "specimen unsatisfactory for evaluation")


read_all_pcr <- function(file_path,
                         start_date = "2020-03-01",
                         end_date = "2020-08-16") {
  pcr_results_original <- read_csv(file_path,
                            col_types = cols(.default = col_skip(),
                                             PersonId = col_character(),
                                             Age = col_integer(),
                                             Sex = col_character(),
                                             Race = col_character(),
                                             Specimen.Collected.Date = col_date("%m-%d-%Y"),
                                             Resulted.Organism = col_character(),
                                             Zip = col_character(),
                                             City = col_character(),
                                             Performing.Facility.ID = col_character())) 
  
  full_num_data_cases <- nrow(pcr_results_original[(pcr_results_original$Specimen.Collected.Date >= ymd(start_date)) & 
                                                   (pcr_results_original$Specimen.Collected.Date <= ymd(end_date)), ])
  
  pcr_results_adjusted <- pcr_results_original %>%
                  filter(!is.na(Resulted.Organism)) %>%
                  mutate(test_result = fct_collapse(str_to_lower(Resulted.Organism),
                                                    negative = negative_test_synonyms,
                                                    positive = positive_test_synonyms,
                                                    unknown = other_test_synonyms)) %>%
                  mutate(sex = fct_collapse(str_to_lower(Sex),
                                            male = "m",
                                            female = "f",
                                            unknown = c("d", "g", "i", "tf", "tm", "u"))) %>%
                  mutate(race = fct_collapse(str_to_lower(Race),
                                                 white = "white",
                                                 american_indigenous = "american indian or alaska native",
                                                 asian = "asian",
                                                 black = "black or african american",
                                                 islander = "native hawaiian or other pacific islander",
                                                 other = c("multiple races", "other"),
                                                 unknown = "unknown")) %>%
                  mutate(race = relevel(factor(str_to_lower(race)), ref = "white")) %>%
                  mutate(time_days = as.integer(round(difftime(Specimen.Collected.Date, 
                                                               start_date, 
                                                               units = "days")))) %>%
                  select(id = PersonId, 
                         posted_date = Specimen.Collected.Date, 
                         time_days,
                         test_result, 
                         age = Age,
                         sex,
                         race,
                         zip = Zip,
                         city = City,
                         facility = Performing.Facility.ID
                         ) %>%
                  filter((posted_date >= lubridate::ymd(start_date)) & (posted_date <= lubridate::ymd(end_date))) %>%
                  filter(test_result != "unknown") %>%
                  filter(sex != "unknown") %>%
                  mutate(zip = str_sub(zip, end = 5)) %>%
                  drop_na() %>%
                  group_by(id) %>%
                  arrange(posted_date) %>%
                  ungroup()
  
  
  if(length(levels(pcr_results_adjusted$test_result)) != 3) warning("New test result category not accounted for.")

  pcr_results_adjusted$covid_positive <- ifelse(pcr_results_adjusted$test_result == "positive", 1, 0)
  
  
  # Extract df of all observations with an id that has inconsistencies for the demographic variables (age, sex, race)
  pcr_rep_id <- pcr_results_adjusted[duplicated(pcr_results_adjusted$id, fromLast=TRUE) | 
                                       duplicated(pcr_results_adjusted$id), ]

  pcr_rep_id2 <- pcr_rep_id %>%
    group_by(id) %>% 
    mutate(reasonable_ages = diff(range(age)) <= 1) %>%
    mutate(identical_race = n_distinct(race) == 1) %>%
    mutate(identical_sex = n_distinct(sex) == 1) %>%
    ungroup()
  
  pcr_inconsistent <- data.frame(pcr_rep_id2[!pcr_rep_id2$reasonable_ages | 
                                             !pcr_rep_id2$identical_sex | 
                                             !pcr_rep_id2$identical_race, ])
  
  inconsistent_counts <- c(length(unique(pcr_inconsistent$id[(!pcr_inconsistent$reasonable_ages)])),
                           length(unique(pcr_inconsistent$id[(!pcr_inconsistent$identical_sex)])),
                           length(unique(pcr_inconsistent$id[(!pcr_inconsistent$identical_race)])))
  names(inconsistent_counts) <- c("number_id_age_inconsistencies", 
                                  "number_id_sex_inconsistencies",
                                  "number_id_race_inconsistencies")
  
  pcr_results_consistent <- pcr_results_adjusted[!(pcr_results_adjusted$id %in% pcr_inconsistent$id), ]
  
  
  # Need to keep all observations for a person up to and including their first positive
  first_pos <- pcr_results_consistent %>%
    filter(test_result == "positive") %>%
    group_by(id) %>%
    summarise(first_pos = min(posted_date))
  
  pcr_results_reduced <- left_join(pcr_results_adjusted, first_pos) %>%
    mutate(first_pos = replace_na(first_pos, lubridate::ymd("9999-12-31"))) %>%
    filter(posted_date <= first_pos) %>%
    select(-first_pos) %>%
    distinct()
  
  
  # Add zip code level data and merge with pcr results
  zip_area_oc <- read_csv(here::here("data", "zip-area2.csv"),
                          col_types = cols(.default = col_skip(),
                                           NAME = col_character(),
                                           Zip = col_character(),
                                           AreaKm = col_double())) %>%
    select(name = NAME,
           zip = Zip,
           area_km = AreaKm)
  
  zip_pop_oc <- read_csv(here::here("data", "zip-pop.csv"),
                         col_types = cols(.default = col_skip(),
                                          Zip = col_character(),
                                          Population = col_integer())) %>%
    drop_na() %>%
    mutate(population = Population / 1000) %>%
    select(zip = Zip,
           population)
  
  zip_data_merged <- merge(x = zip_area_oc, y = zip_pop_oc, by = "zip")
  zip_data_merged$pop_density <- zip_data_merged$population / zip_data_merged$area_km
  
  zip_income_oc <- read_csv(here::here("data", "income-by-zip2.csv"),
                            col_types = cols(.default = col_skip(),
                                             Zip = col_character(),
                                             IncomeMed = col_integer(),
                                             IncPeriodofMeas = col_character())) %>%
                   mutate(med_income = IncomeMed / 10000) %>%
                   filter(IncPeriodofMeas == "2014-2018") %>%
                   select(zip = Zip,
                          med_income)
  
  zip_data_merged <- merge(x = zip_data_merged, y = zip_income_oc, by = "zip")

  zip_education_oc <- read_csv(here::here("data", "education-by-zip.csv"),
                               col_types = cols(.default = col_skip(),
                                                Zip = col_character(),
                                                PercentBach = col_double())) %>%
                      select(zip = Zip,
                             percent_bachelors = PercentBach)
  
  zip_data_merged <- merge(x = zip_data_merged, y = zip_education_oc, by = "zip")

  zip_insurance_oc <- read_csv(here::here("data", "insurance-by-zip.csv"),
                               col_types = cols(.default = col_skip(),
                                                Zip = col_character(),
                                                PercentInsured = col_double())) %>%
                      select(zip = Zip,
                             percent_insured = PercentInsured)
  
  zip_data_merged <- merge(x = zip_data_merged, y = zip_insurance_oc, by = "zip")

  
  #23 rows in 92678 zipcode we don't have area data for
  pcr_results_reduced$old_zip <- pcr_results_reduced$zip
  pcr_results_reduced$zip[pcr_results_reduced$old_zip == "92678"] <- "92679"
  pcr_results_merged <- merge(x = pcr_results_reduced, y = zip_data_merged, by = "zip")
  pcr_results_merged$old_zip <- factor(pcr_results_merged$old_zip)
  pcr_results_merged$zip <- factor(pcr_results_merged$zip)
  
  
  missing_counts <- c(full_num_data_cases , 
                      full_num_data_cases - nrow(pcr_results_adjusted), 
                      nrow(pcr_results_adjusted) - nrow(pcr_results_consistent),
                      nrow(pcr_results_consistent) - nrow(pcr_results_reduced),
                      nrow(pcr_results_reduced) - nrow(pcr_results_merged))
  names(missing_counts) <- c("full_num_data_cases", 
                             "num_na_cases_removed", 
                             "num_inconsistent_cases_removed",
                             "num_consecutive_cases_removed",
                             "num_bad_zip_cases_removed")
  
  
  # Group and scale variables 
  age_breaks <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 50, 60, 70, 80, 200)
  age_labels <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39",
                  "40-49","50-59","60-69","70-79","80+")
  data.table::setDT(pcr_results_merged)[, age_group := cut(age, 
                                                            breaks = age_breaks, 
                                                            right = FALSE, 
                                                            labels = age_labels)]
  
  pcr_results_merged$age_group <- factor(pcr_results_merged$age_group,
                                         ordered = TRUE,
                                         levels = age_labels)
  
  pcr_results_merged$adj_time_days <- scale(pcr_results_merged$time_days,
                                 center = TRUE,
                                 scale = TRUE)
  pcr_results_merged$adj_pop_density <- scale(pcr_results_merged$pop_density, 
                                   center = TRUE, 
                                   scale = TRUE)
  pcr_results_merged$adj_med_income <- scale(pcr_results_merged$med_income, 
                                  center = TRUE, 
                                  scale = TRUE)
  pcr_results_merged$adj_med_income_quar <- with(pcr_results_merged,
                                          cut(adj_med_income,
                                              breaks = quantile(adj_med_income, 
                                                                probs = seq(0, 1, by = 0.25)),
                                              include.lowest = TRUE,
                                              labels = c("Q1", "Q2", "Q3", "Q4")))
  
  pcr_results_merged$adj_perc_bach <- scale(pcr_results_merged$percent_bachelors,
                                      center = TRUE,
                                      scale = TRUE)
  pcr_results_merged$adj_perc_bach_quar <- with(pcr_results_merged,
                                              cut(adj_perc_bach,
                                                  breaks = quantile(adj_perc_bach, 
                                                                    probs = seq(0, 1, by = 0.25)),
                                                  include.lowest = TRUE,
                                                  labels = c("Q1", "Q2", "Q3", "Q4")))
  
  pcr_results_merged$adj_perc_insured <- scale(pcr_results_merged$percent_insured,
                                    center = TRUE,
                                    scale = TRUE)
  pcr_results_merged$adj_perc_insured_quar <- with(pcr_results_merged,
                                            cut(adj_perc_insured,
                                                breaks = quantile(adj_perc_insured, 
                                                                  probs = seq(0, 1, by = 0.25)),
                                                include.lowest = TRUE,
                                                labels = c("Q1", "Q2", "Q3", "Q4")))
  

  
  list("pcr_results_merged" = pcr_results_merged, 
       "zip_data_merged" = zip_data_merged, 
       "counts" = missing_counts,
       "inconsistencies" = inconsistent_counts)
}