library(ggplot2)
source(here::here("data-wrangling", "read-sanitize-oc-covid-data.R"))

all_pcr_and_zip <- read_all_pcr(file_path = "C:/Users/Catalina Medina/Documents/oc-positivity-plus-outer/All ELR PCR tests updated 10.05.20.csv",
                                start_date = "2020-03-01",
                                end_date = "2020-08-16")
all_pcr <- data.frame(all_pcr_and_zip[["pcr_results_merged"]])
all_zip <- data.frame(all_pcr_and_zip[["zip_data_merged"]])
all_counts <- all_pcr_and_zip[["counts"]]


all_pcr$adj_time_days <- scale(all_pcr$time_days,
                               center = TRUE,
                               scale = TRUE)
all_pcr$adj_pop_density <- scale(all_pcr$pop_density, 
                                 center = TRUE, 
                                 scale = TRUE)
all_pcr$adj_med_income <- scale(all_pcr$med_income, 
                                center = TRUE, 
                                scale = TRUE)
all_pcr$adj_med_income_quartile <- with(all_pcr,
                                        cut(adj_med_income,
                                            breaks = quantile(adj_med_income, 
                                                              probs = seq(0, 1, by = 0.25)),
                                            include.lowest = TRUE,
                                            labels = c("Q1", "Q2", "Q3", "Q4")))

all_pcr$adj_perc_bachelors <- scale(all_pcr$percent_bachelors,
                                    center = TRUE,
                                    scale = TRUE)
all_pcr$adj_perc_bachelors_quartile <- with(all_pcr,
                                            cut(adj_perc_bachelors,
                                                breaks = quantile(adj_perc_bachelors, 
                                                                  probs = seq(0, 1, by = 0.25)),
                                                include.lowest = TRUE,
                                                labels = c("Q1", "Q2", "Q3", "Q4")))

all_pcr$adj_perc_insured <- scale(all_pcr$percent_insured,
                                  center = TRUE,
                                  scale = TRUE)
all_pcr$adj_perc_insured_quartile <- with(all_pcr,
                                          cut(adj_perc_insured,
                                              breaks = quantile(adj_perc_insured, 
                                                                probs = seq(0, 1, by = 0.25)),
                                              include.lowest = TRUE,
                                              labels = c("Q1", "Q2", "Q3", "Q4")))

#bimodial distrubtion of income across data
hist(all_zip$med_income, breaks = 30)
hist(all_pcr$med_income, breaks = 30)

ggplot(all_pcr,
       aes(x = zip)) +
  geom_bar() +
  theme(axis.text.x = element_text(size = 7, angle = 90))
# Santa Ana 92703 has highest zip code representation in data, 
# reasonable since they have almost the max population density.
# Similarly the next highest is Anaheim 92804,
# also has a population density in last quartile



ggplot(all_zip[order(all_zip$med_income), ],
       aes(x = zip, y = med_income)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(size = 9, angle = 90)) +
  scale_x_discrete(limits = all_zip$zip[order(all_zip$med_income)]) +
  ylab("Median Income (in $10,000)")
# investigate highest median income
length(unique(all_zip$med_income))
#no zip codes have the same median income
all_zip[all_zip$zip == "92657",]
# Newport coast has highest median income ... yup


ggplot(all_pcr,
       aes(x = race)) +
  geom_bar() +
  theme(axis.text.x = element_text(size = 12, angle = 45)) +
  geom_text(stat='count', aes(label=..count..), vjust=-1)
# Need to investigate multiple_races because it is getting extreme estimate odds
summary(all_pcr)
summary(all_pcr[all_pcr$race == "multiple races",])
# across all of the data the sample probability of testing covid positive is 0.1439
# accros the sample of people with multiple races the probability of testing covid positive is 0.8204

# It is not because of the race not being an available classification
summary(all_pcr$time_days)
summary(all_pcr$time_days[all_pcr$race == "multiple races"])