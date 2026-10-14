#Combined summary table for assigned pollution data, for each time window in GS

#include: mean, median, IQR

#Function to summarise pollution data
stat_poll <- function (x) {
df <- data.frame( 
 Mean = mean(x),
 Median = median(x),
 Stdev = sd(x),
 q1 = unname(quantile(x, 0.25)),
 q3 = unname(quantile(x, 0.75)),
 iqr = IQR(x),
 Min = min(x),
 Max = max(x),
 row.names = NULL)
 return(df)
}



#GS 1year
oneyr <- fread("annual_avg_exp_foranalysis_ntr.csv")
oneyr <- oneyr %>% select(-c(year_range, standard_dev, raster_entry))
oneyr <- oneyr %>% pivot_wider(
    id_cols = NULL,
    names_from = Pollutant,
    values_from = days_365_avg
)


summary_dfmulti <- oneyr %>% 
  drop_na() %>%
  dplyr::select(-ID) %>%
  map(., stat_poll)

#Summary stats
summary_dfmulti <- bind_rows(summary_dfmulti, .id = "Pollutant")
summary_dfmulti1 <- summary_dfmulti %>% mutate(time_span = "oneyr")



#GS 6 month
sixmonth <- fread("6month_avg_exp_foranalysis_ntr.csv")

six_dfmulti <- sixmonth %>% 
  drop_na() %>%
  dplyr::select(-ID) %>%
  map(., stat_poll)

#Summary stats
six_dfmulti <- bind_rows(six_dfmulti, .id = "Pollutant")
six_dfmulti1 <- summary_dfmulti %>% mutate(time_span = "sixmonth")

#GS 7 year
annual_df2 <- fread("sevenyr_foranalysis_notsc.csv")
sevenyr <- annual_df2 %>% select(-c(year_range, standard_dev, raster_entry))
sevenyr <- sevenyr %>% pivot_wider(
    id_cols = NULL,
    names_from = Pollutant,
    values_from = avg_7yrs
)


seven_dfmulti <- sevenyr %>% 
  drop_na() %>%
  dplyr::select(-ID) %>%
  map(., stat_poll)

#Summary stats
seven_dfmulti <- bind_rows(seven_dfmulti, .id = "Pollutant")
seven_dfmulti1 <- seven_dfmulti %>% mutate(time_span = "sevenyr")


#combine all GS summaries
comb_summary <- rbind(summary_dfmulti1, six_dfmulti1, seven_dfmulti1)



#Calculate correlations for 1yr, 6month and 7year data in GS
#1yr - 7yr done in previous script

#6month - 1yr
head(sixmonth)
sixm <- sixmonth %>% pivot_longer(
    cols = !ID,
   names_to = "pollutant",
   values_to = "measurement" 
) %>%
dplyr::rename(sixmonth = measurement)


head(oneyr)
oney <- oneyr %>% pivot_longer(
    cols = !ID,
   names_to = "pollutant",
   values_to = "measurement" 
) %>%
dplyr::rename(oneyr = measurement)

identical(sixm$ID, oney$ID)
pollutant_list <- unique(oney$pollutant)
df <- full_join(sixm, oney, by = c("ID", "pollutant"))


corr_results <- list()
for (pollutant in pollutant_list) {
  x <- df %>% dplyr::filter(pollutant == pollutant)
  corr_results[[pollutant]] <- cor.test(x$oneyr, x$sixmonth)
}
corr_results





