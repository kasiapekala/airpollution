library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(sp)

df <- readRDS(file = "df.rds")
slownik <- readRDS(file = "slownik.rds")

# how many stations per year
year_station_freq <- table(df$Stacja,year(df$Timestamp))
year_station_freq_filtered <- filter(as.data.frame(year_station_freq),
                                     Freq != 0, Var2 != 2017)
how_many_stations_per_year <- year_station_freq_filtered %>%
  select(Var1,Var2) %>%
  group_by(Var2) %>%
  summarise(ile_stacji = n())
colnames(how_many_stations_per_year) <- c("Rok", "Liczba stacji")

bp <- ggplot(how_many_stations_per_year, aes(x=Rok, y=`Liczba stacji`)) + 
  geom_bar(stat = "identity") + theme_minimal()
bp

rm(bp,year_station_freq, year_station_freq_filtered)

# completness of obs per station

total_no_timestamps <- length(unique(df$Timestamp))
df_obs_completeness <- df %>%
  select(Timestamp, Stacja)  %>%
  group_by(Stacja) %>%
  summarise(liczba_pomiarow = n()) %>%
  mutate(kompletnosc_pomiarow = liczba_pomiarow/total_no_timestamps) %>%
  arrange(desc(kompletnosc_pomiarow))

ggplot(data=df_obs_completeness, 
       aes(df_obs_completeness$kompletnosc_pomiarow)) + 
  geom_histogram(binwidth = 0.1, fill="black", col="grey") +
  xlab("Kompletność pomiarów") + ylab("Liczba stacji") +
  theme_minimal() + scale_y_continuous(breaks=seq(2,10,2))

rm(total_no_timestamps)

# monthly means, filtered (nobs>50%)
nobs_year_avg <- 30*24*4+31*24*7+28.25*24

df_nobs_per_month_station <- df %>%
  filter(Timestamp<"2017-01-01") %>%
  mutate(miesiac = floor_date(Timestamp, "month")) %>%
  group_by(miesiac,Stacja) %>%
  summarise(liczba_obserwacji = n()) %>% 
  arrange(desc(liczba_obserwacji)) 

stations_with_few_obs_per_month <- filter(df_nobs_per_month_station,
                                          liczba_obserwacji<(nobs_year_avg/12)/2)[,-3]

proportion_of_filtered_out_stations <- (dim(stations_with_few_obs_per_month)[1]
                                        /dim(df_nobs_per_month_station)[1])

df_means_per_month_station <- df %>%
  filter(Timestamp<"2017-01-01") %>%
  mutate(miesiac = floor_date(Timestamp, "month")) %>%
  group_by(miesiac,Stacja) %>%
  summarise(srednia = mean(Pomiar)) %>% 
  arrange(desc(srednia)) 

df_means_per_month_station <- df_means_per_month_station %>%
  filter(!(paste(miesiac,Stacja) %in% 
           paste(stations_with_few_obs_per_month$miesiac,
                 stations_with_few_obs_per_month$Stacja)))

rm(stations_with_few_obs_per_month, 
   proportion_of_filtered_out_stations,df_nobs_per_month_station)

summary(df_means_per_month_station)
dim(df_means_per_month_station)
length(unique(df_means_per_month_station$Stacja))

# any station has all means?

df_means_per_month_station_wide <- df_means_per_month_station %>%
  spread(Stacja, srednia)
df_means_per_month_station_wide_filtered_only_complete <- df_means_per_month_station_wide[
  sapply(df_means_per_month_station_wide,
         function(x) !any(is.na(x)))]
View(df_means_per_month_station_wide_filtered_only_complete)

# 2011-2016
df_means_per_month_station_wide <- df_means_per_month_station %>%
  spread(Stacja, srednia)

df_means_per_month_station_wide_filtered <- filter(
  df_means_per_month_station_wide,miesiac>"2010-12-01 00:00:00 ")

df_means_per_month_station_wide_filtered_only_complete <- df_means_per_month_station_wide_filtered[
  sapply(df_means_per_month_station_wide_filtered, 
         function(x) !any(is.na(x)))]

View(df_means_per_month_station_wide_filtered_only_complete)

#View(as.data.frame(summary(df_means_per_month_station_wide_filtered_only_complete)))

df_means_per_month_station_filtered_only_complete <- 
  df_means_per_month_station_wide_filtered_only_complete %>%
  gather(Stacja,srednia,2:dim(df_means_per_month_station_wide_filtered_only_complete)[2])

df_total_mean_monthly <- df_means_per_month_station_filtered_only_complete %>%
  summarise(srednia_ze_stacji = mean(srednia)) %>%
  arrange(miesiac) 

ggplot(data=df_total_mean_monthly, aes(x=miesiac, y=srednia_ze_stacji, group=1)) +
  geom_line() +
  geom_point(color="red") + 
  theme_minimal() +
  geom_smooth(method = "lm") +
  xlab("miesiąc") + ylab("średnia całkowita")
  
dim(df_means_per_month_station_wide_filtered_only_complete)  

# 2016 yearly
df_means_per_month_station_wide_filtered <- filter(
  df_means_per_month_station_wide,miesiac>"2015-12-01 00:00:00 ")

df_means_per_month_station_wide_filtered_only_complete <- df_means_per_month_station_wide_filtered[
  sapply(df_means_per_month_station_wide_filtered, 
         function(x) !any(is.na(x)))]

df_yearly_means_2016 <- df_means_per_month_station_wide_filtered_only_complete %>%
  group_by(year(miesiac)) %>%
  summarise_if(is.numeric, mean)
  
df_yearly_means_2016 <- as.data.frame(t(df_yearly_means_2016))
df_yearly_means_2016$Stacje <- rownames(df_yearly_means_2016)
rownames(df_yearly_means_2016) <- c()
df_yearly_means_2016 <- df_yearly_means_2016[-1,]
df_yearly_means_2016 <- arrange(df_yearly_means_2016,desc(V1))
colnames(df_yearly_means_2016)[1] <- "Sredni_roczny_pomiar_2016"
df_yearly_means_2016 <- df_yearly_means_2016[,c(2,1)]
df_yearly_means_2016_details <- left_join(df_yearly_means_2016,slownik,
          by=c("Stacje"="KOD_NOWY"))[,c(16,2,7,4)]

summary(df_yearly_means_2016$Sredni_roczny_pomiar_2016)

View(df_yearly_means_2016_details)
