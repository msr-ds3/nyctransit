library(readr)
library(dplyr)

csvs <- Sys.glob('yellow_tripdata_2015-0[1-6].csv')
#csvs <- Sys.glob('yellow_tripdata_2015-*.csv')
taxi_data <- data.frame()
for (csv in csvs)
{
  tmp <- read_csv(csv)
  taxi_data <- rbind(tmp, taxi_data)
}

save(taxi_data, file = "taxi_data_2015_01-06.Rdata")

taxi_data_lite <- taxi_data %>%
  select('tpep_pickup_datetime','tpep_dropoff_datetime','trip_distance','pickup_longitude','pickup_latitude','RateCodeID','dropoff_longitude','dropoff_latitude')

save(taxi_data_lite, file = "taxi_data_2015_01-06_lite.Rdata")
