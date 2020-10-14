# Damon Bayer and Isaac Goldsteinh's code adjusted

library(tidyverse)
library(lubridate)
library(here)

# Get Line List Data ------------------------------------------------------
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
                         start_date = "2020-03-01") {
  pcr_results_original <- read_csv(file_path,
                            col_types = cols(.default = col_skip(),
                                             PersonId = col_character(),
                                             Age = col_integer(),
                                             Sex = col_character(),
                                             Race = col_character(),
                                             Specimen.Collected.Date = col_date("%m-%d-%Y"),
                                             Resulted.Organism = col_character(),
                                             Zip = col_character())) 
  
  full_num_data_cases <- nrow(pcr_results_original[pcr_results_original$Specimen.Collected.Date >= 
                                                     lubridate::ymd(start_date),])
  
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
                  mutate(race = factor(str_to_lower(Race))) %>%
                  mutate(posted_month = factor(month(Specimen.Collected.Date))) %>%
                  mutate(time_days = as.integer(round(difftime(Specimen.Collected.Date, 
                                                               start_date, 
                                                               units = "days")))) %>%
                  select(id = PersonId, 
                         posted_date = Specimen.Collected.Date, 
                         posted_month,
                         time_days,
                         test_result, 
                         zip = Zip,
                         age = Age,
                         sex,
                         race) %>%
                  filter(posted_date >= lubridate::ymd(start_date)) %>%
                  filter(test_result != "unknown") %>%
                  filter(race != "unknown") %>%
                  filter(sex != "unknown") %>%
                  mutate(zip = str_sub(zip, end = 5)) %>%
                  drop_na() %>%
                  group_by(id) %>%
                  arrange(posted_date) %>%
                  ungroup()
  
  pcr_results_adjusted$covid_positive <- ifelse(pcr_results_adjusted$test_result == "positive", 1, 0)

  
  age_breaks <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 50, 60, 70, 80, 200)
  age_labels <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39",
                  "40-49","50-59","60-69","70-79","80+")
  data.table::setDT(pcr_results_adjusted)[, age_groups := cut(age, 
                                                             breaks = age_breaks, 
                                                             right = FALSE, 
                                                             labels = age_labels)]
  
  if(length(levels(pcr_results_adjusted$test_result)) != 3) warning("New test result category not accounted for.")
  
  clean_num_data_cases <- nrow(pcr_results_adjusted)
  
  
  first_pos <- pcr_results_adjusted %>%
    filter(test_result == "positive") %>%
    group_by(id) %>%
    summarise(first_pos = min(posted_date))
  
  pcr_resuts_reduced <- left_join(pcr_results_adjusted, first_pos) %>%
    mutate(first_pos = replace_na(first_pos, lubridate::ymd("9999-12-31"))) %>%
    filter(posted_date <= first_pos) %>%
    select(-first_pos) %>%
    distinct()
  

  zip_income_oc <- read_csv(here::here("data", "income-by-zip2.csv"),
                            col_types = cols(.default = col_skip(),
                                             Zip = col_character(),
                                             IncomeMed = col_integer(),
                                             IncPeriodofMeas = col_character())) %>%
                   mutate(med_income = IncomeMed / 10000) %>%
                   filter(IncPeriodofMeas == "2014-2018") %>%
                   select(zip = Zip,
                          med_income)
  
  pcr_results_merged <- merge(x = pcr_results_adjusted, y = zip_income_oc, by = "zip")
  
  zip_education_oc <- read_csv(here::here("data", "education-by-zip.csv"),
                               col_types = cols(.default = col_skip(),
                                                Zip = col_character(),
                                                PercentBach = col_double())) %>%
                      select(zip = Zip,
                             percent_bachelors = PercentBach)
  
  pcr_results_merged <- merge(x = pcr_results_merged, y = zip_education_oc, by = "zip")
  
  zip_insurance_oc <- read_csv(here::here("data", "insurance-by-zip.csv"),
                               col_types = cols(.default = col_skip(),
                                                Zip = col_character(),
                                                PercentInsured = col_double())) %>%
                      select(zip = Zip,
                             percent_insured = PercentInsured)
  
  pcr_results_merged <- merge(x = pcr_results_merged, y = zip_insurance_oc, by = "zip")
  
  zip_area_oc <- read_csv(here::here("data", "zip-area2.csv"),
                          col_types = cols(.default = col_skip(),
                                           Zip = col_character(),
                                           AreaKm = col_double())) %>%
                 select(zip = Zip,
                        area_km = AreaKm)
  zip_pop_oc <- read_csv(here::here("data", "zip-pop.csv"),
                         col_types = cols(.default = col_skip(),
                                          Zip = col_character(),
                                          Population = col_integer())) %>%
                drop_na() %>%
                mutate(population = Population / 1000) %>%
                select(zip = Zip,
                       population)
  
  pop_area <- merge(x = zip_area_oc, y = zip_pop_oc, by = "zip")
  
  pcr_results_merged <- merge(x = pcr_results_merged, y = pop_area, by = "zip")
  pcr_results_merged$population_density <- pcr_results_merged$population / pcr_results_merged$area_km
  pcr_results_merged
}