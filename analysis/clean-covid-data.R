library(tidyverse)
library(here)
library(lubridate)
library(data.table)
library(forcats)


start_date <- "2020-03-01"
end_date <- "2020-08-16"

daniel_est_beds <- 4879
oc_icu_avail_beds_earliest_val <- 131
oc_hos_covid_pateients_earliest_val <- 308
oc_all_hos_bed_earliest_val <- 4213
oc_icu_full_beds_earliest_val <- 71


# Combine zip code data ---------------------------------------------------
## General zip code data
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
  right_join(y =  zip_data_merged, by = "zip") %>% 
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


zip_data_merged <- read_csv( # house crowding
    here("data/zip-code-data", "house-crowding.csv"),
    col_types = cols(
      .default = col_skip(),
      Location = col_character(),
      `Indicator Rate Value` = col_double(),
      `Period of Measure` = col_character(),
      `Breakout Subcategory` = col_character()
    )
  ) %>% 
  filter(`Period of Measure` == "2015-2019") %>% 
  filter(`Breakout Subcategory` == "Owners") %>% 
  select(zip = Location, house_crowding = `Indicator Rate Value`) %>% 
  full_join(y =  zip_data_merged, by = "zip")


## OC Hospital data by date
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

if(month(start_date) == month(first_date) & day(start_date) < day(first_date)) {
  dates_missing <- seq(as.Date(start_date), first_date, by = "days")
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
hosp_data_merged <- hos_bed_gov %>% 
  mutate(avail_icu_beds = ifelse(
    is.na(icu_available_beds), 
    oc_icu_avail_beds_earliest_val,
    icu_available_beds
  )) %>% 
  mutate(perc_avail_beds = ifelse(
    is.na(hospitalized_covid_patients) | is.na(all_hospital_beds),
    100 * (1 - oc_hos_covid_pateients_earliest_val / oc_all_hos_bed_earliest_val),
    100 * (1 - hospitalized_covid_patients / all_hospital_beds)
  )) %>% 
  mutate(covid_icu_beds = ifelse(
    is.na(icu_covid_confirmed_patients) | is.na(icu_suspected_covid_patients),
    oc_icu_full_beds_earliest_val,
    icu_covid_confirmed_patients + icu_suspected_covid_patients
  )) %>% 
  select(
    posted_date = todays_date,
    avail_icu_beds,
    perc_avail_beds,
    covid_icu_beds
  ) %>% 
  mutate(adj_perc_avail_beds = scale(perc_avail_beds, center = TRUE, scale = TRUE)) %>% 
  mutate(adj_avail_icu_beds = scale(avail_icu_beds, center = TRUE, scale = TRUE)) %>% 
  mutate(adj_covid_icu_beds = scale(covid_icu_beds, center = TRUE, scale = TRUE)) 



# Clean all test data ----------------------------------------------------
## All test categorizations
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
  "unknown",
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


pcr_results_original <- read_csv(
  here("data/positivity-data", "all-elr-pcr-tests-updated-2020-11-09.csv"),
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

specified_race <- c(
  "White", 
  "Asian", 
  "Black or African American", 
  "American Indian or Alaska Native", 
  "Native Hawaiian or Other Pacific Islander"
)

unspecified_race <- c(
  "Multiple Races", 
  "Other", 
  "Unknown"
)

age_breaks <- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 50, 60, 70, 80, 200)

age_labels <- c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39",
                "40-49","50-59","60-69","70-79","80+")


pcr_results_adjusted <- pcr_results_original %>%
  filter(!is.na(Resulted.Organism)) %>%
  mutate(test_result = fct_collapse(
    str_to_lower(Resulted.Organism),
    negative = negative_test_synonyms,
    positive = positive_test_synonyms,
    unknown = other_test_synonyms
  )) %>%
  mutate(sex = fct_collapse(
    str_to_lower(Sex),
    male = "m",
    female = "f",
    unknown = c("d", "g", "i", "tf", "tm", "u")
  )) %>%
  mutate(Race = ifelse(is.na(Race), "Unknown", Race)) %>% 
  mutate(Ethnicity = ifelse(is.na(Ethnicity), "Unknown", Ethnicity)) %>% 
  mutate(race = factor(
    case_when(
      (Ethnicity != "Hispanic or Latino") & (Race == specified_race[1]) ~ "white",
      (Ethnicity != "Hispanic or Latino") & (Race == specified_race[2]) ~ "asian",
      (Ethnicity != "Hispanic or Latino") & (Race == specified_race[3]) ~ "black",
      (Ethnicity == "Hispanic or Latino") & (Race %in% unspecified_race) ~ "hispanic",
      (Ethnicity != "Hispanic or Latino") & (Race == specified_race[4]) ~ "native",
      (Ethnicity != "Hispanic or Latino") & (Race == specified_race[5]) ~ "islander",
      ((Ethnicity != "Hispanic or Latino") & (Race %in% unspecified_race)) |
        ((Ethnicity == "Hispanic or Latino") & (Race %in% specified_race))  ~ "unknown",
      TRUE ~ "NA"
    ),
    levels = c("white", "asian", "black", "hispanic", "native", "islander", "unknown")
  )) %>% 
  mutate(
    time_days = as.integer(round(difftime(
      Specimen.Collected.Date, 
      start_date, 
      units = "days"
    )))
  ) %>%
  mutate(adj_time_days = scale(time_days, center = TRUE, scale = TRUE)) %>% 
  select(
    id = unique_num, 
    posted_date = Specimen.Collected.Date, 
    time_days,
    adj_time_days,
    test_result, 
    age = Age,
    sex,
    race,
    ethnicity = Ethnicity,
    zip = Zip,
  ) %>%
  filter((posted_date >= ymd(start_date)) & (posted_date <= ymd(end_date))) %>% 
  filter(!is.na(zip)) %>% 
  filter(zip %in% zip_data_merged$zip) %>%
  filter(test_result != "unknown") %>%
  mutate(test_result = droplevels(test_result)) %>%
  mutate(covid_positive = ifelse(test_result == "positive", 1, 0)) %>% 
  filter(sex != "unknown") %>%
  mutate(sex = droplevels(sex)) %>%
  filter(!is.na(age) & age != 119) %>% 
  mutate(age_group = factor(
    cut(age, breaks = age_breaks, labels = age_labels, right = FALSE),
    levels = age_labels
  )) %>% 
  mutate(decades_old = age / 10) %>%
  group_by(id) %>%
  arrange(posted_date) %>%
  ungroup()


# Extract df of all observations with an id that has inconsistencies for the demographic variables (age, sex, race)
pcr_rep_id <- pcr_results_adjusted[(
  duplicated(pcr_results_adjusted$id, fromLast=TRUE) | 
    duplicated(pcr_results_adjusted$id)
), ]

pcr_rep_id2 <- pcr_rep_id %>%
  group_by(id) %>% 
  mutate(reasonable_ages = diff(range(age)) <= 1) %>%
  mutate(identical_sex = n_distinct(sex) == 1) %>%
  ungroup()

pcr_inconsistent <- data.frame(
  pcr_rep_id2[(!pcr_rep_id2$reasonable_ages | !pcr_rep_id2$identical_sex), ]
)

pcr_results_consistent <- pcr_results_adjusted[!(pcr_results_adjusted$id %in% pcr_inconsistent$id), ]


# Need to keep all observations for a person up to and including their first positive
first_pos <- pcr_results_consistent %>%
  filter(test_result == "positive") %>%
  group_by(id) %>%
  summarise(first_pos = min(posted_date))

pcr_results_reduced <- left_join(pcr_results_consistent, first_pos, by = "id") %>%
  mutate(first_pos = replace_na(first_pos, lubridate::ymd("9999-12-31"))) %>%
  filter(posted_date <= first_pos) %>%
  select(-first_pos) %>%
  distinct()


# Merge with zip code and hospital data
usable_tests_wo_death <- pcr_results_reduced %>% 
  left_join(y = zip_data_merged, by = "zip") %>% 
  mutate(zip = factor(zip)) %>% 
  left_join(y = hosp_data_merged, by = "posted_date")
  
  




# clean and match mortality data ------------------------------------------
mortality_og <- read_csv(
  here("data/mortality-data", "all-positive-tests-updated-2020-11-09.csv"),
  col_types = cols(
    .default = col_skip(),
    Age = col_double(),
    Gender = col_character(),
    Ethnicity = col_character(),
    Race = col_character(),
    ReportedCity = col_character(),
    Zip = col_double(),
    SpCollDt = col_date(),
    DeathDueCOVID = col_character(),
    DtDeath = col_date(),
    unique_num = col_character()
  )
) %>% 
  mutate(death_date = replace_na(DtDeath, ymd("9999/09/09"))) %>%
  mutate(DeathDueCOVID = ifelse(is.na(DeathDueCOVID), "n", "y")) 


# if a person ever died of covid record it under each of their tests (and drop their repeated tests)
ids_of_deaths <- mortality_og %>% 
  filter(DeathDueCOVID == "y") %>% 
  pull(unique_num)

mortality_cleaned <- mortality_og %>% 
  mutate(covid_death = ifelse(unique_num %in% ids_of_deaths, "yes", "no")) %>% 
  arrange(unique_num, SpCollDt) %>% 
  filter(!duplicated(unique_num)) %>% 
  select(id = unique_num, covid_death) 


neg_usable_tests <- usable_tests_wo_death %>% 
  filter(covid_positive == 0) %>% 
  mutate(covid_death = "no") %>% 
  mutate(death_date = ymd("9999/09/09"))

pos_usable_tests <- usable_tests_wo_death %>% 
  filter(covid_positive == 1) %>% 
  left_join(y = mortality_cleaned, by = "id") %>% 
  drop_na()

usable_tests <- rbind(pos_usable_tests, neg_usable_tests) %>% 
  arrange(id, posted_date)

usable_cases <- usable_tests %>% 
  filter(covid_positive == 1)



list(
  "zip_data_merged" = zip_data_merged,
  "hosp_data_merged" = hosp_data_merged,
  "pcr_results_merged" = pcr_results_merged
)