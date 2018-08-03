library(tidyverse)
library(lubridate)
library(ggthemes)
library(ROCR)
library(here)

load(file = here::here("data", "modeled_wait_times.RData"))

# Helper Functions for Itinerary Evaluation
# given an hour of the day
# returns the bin for that time of day
convert_hour_to_time_of_day <- function(hour) {
  
  tod <- case_when(
    (hour < 5) ~ "Early morning",
    (5 <= hour & hour < 10) ~ "Morning rush",
    (10 <= hour & hour < 15) ~ "Mid-day",
    (15 <= hour & hour < 20) ~ "Evening rush",
    (hour >= 20) ~ "Night time"
  )
  
  return(tod)
}

# given day of week as a string
# returns true if weekend else returns false
isWeekend <- function(day_of_week) {
  ifelse(day_of_week == "Saturday" | day_of_week == "Sunday", T, F)
}

# given lines (as a string), a start station, and stop station
# returns historical train data for those lines and those stops
get_leg_data <- function(lines, start, end, subway_data, start_hour = 7, end_hour = 10) {
  lines_list <- first(strsplit(lines, split = "_"))
  leg_data <- subway_data %>%
    filter(stop_mta_id == start | stop_mta_id == end,
           route_mta_id %in% lines_list) %>%
    # departure_time > start_time) %>%
    mutate(day_of_week = weekdays((departure_time)),
           hour = hour(departure_time)) %>%
    filter(isWeekend(day_of_week) == F, hour >= start_hour, hour < end_hour) %>%
    # group_by trip_id's to ensure all our trips start and end at appropriate stations
    group_by(realtime_trip_id) %>%
    mutate(start_time = min(departure_time), count = n()) %>%
    ungroup() %>%
    filter(count >= 2, stop_mta_id == end) %>%
    arrange(departure_time)
  
  return(leg_data)
}

# given one specific itinerary dataframe
# returns a dataframe of what those itineraries looked like in historical data
get_itinerary_times <- function(itinerary, subway_data, start_hour = 7, end_hour = 10) {
  
  # handle the initial transfer special case
  transfer_special_case <- itinerary %>% filter(leg == 0)
  print(transfer_special_case$weight)
  # transfer_time <- ifelse(nrow(transfer_special_case) == 0, 0, as.integer(transfer_special_case$weight))
  
  # handle final transfer special case
  end_transfer_special_case <- itinerary %>% filter(event == "end_trip", line == "")
  
  curr_itin <- itinerary %>% anti_join(transfer_special_case) %>% anti_join(end_transfer_special_case)
  print(nrow(curr_itin))
  
  end_transfer_time <- ifelse(nrow(end_transfer_special_case) == 0, 0, as.integer(last(curr_itin$weight)))
  
  # adjust the start time to take into account that transfer time
  # adjusted_start <- as.POSIXct(start_time, origin = "1970-01-01", tz = "UTC") +
  #   as.integer(transfer_time)
  # print(adjusted_start)
  
  trains <- curr_itin %>% .$line %>% rle %>% .$values
  len_trains <- length(trains)
  # print(length(trains))
  stops_data <- curr_itin %>% select(leg, station) %>% split(.$leg)
  stop_station <- vector("list", len_trains)
  
  train_labels <- curr_itin %>% .$line %>% rle %>% .$values
  station_labels <- curr_itin %>% .$stop_name %>% rle %>% .$values
  combined_labels <- character(length(station_labels))
  
  for (i in 1:length(train_labels)) {
    combined_labels[i] <- sprintf("%s @ %s", train_labels[i], station_labels[i])
  }
  
  combined_labels[length(combined_labels)] <- last(station_labels)
  
  
  
  label <- paste(combined_labels, collapse = " => ")
  
  
  # populate our stop_station list
  for (i in 1:len_trains) {
    stop_station[[i]] <- c(stops_data[[i]]$station[1], stops_data[[i]]$station[2])
  }
  print(stop_station)
  
  # list of train data frames
  train_data <- vector("list", len_trains)
  
  for (i in 1:len_trains) {
    start <- stop_station[[i]][1]
    end <- stop_station[[i]][2]
    lines <- trains[i]
    
    leg_data <- get_leg_data(lines, start, end, subway_data, start_hour, end_hour)
    
    # NOTE: DEBUG CODE
    # print(nrow(leg_data))
    
    # append to list
    train_data[[i]] <- leg_data
  }
  
  num_trains <- nrow(train_data[[1]])
  
  output_df <- data.frame(expected_wait=numeric(num_trains))
  
  # format output_df with appropriate columns
  
  for (i in 1:len_trains) {
    train <- trains[i]
    output_df[[paste('leg', i, '_line', sep='')]] <- character(num_trains)
    output_df[[paste('leg', i, '_trip_id',  sep='')]] <- numeric(num_trains)
    output_df[[paste('leg', i, '_start_time', sep='')]] <- numeric(num_trains)
    output_df[[paste('leg', i, '_end_time', sep='')]] <- numeric(num_trains)
  }
  
  # TODO: prune this code
  # output_df <- subset(output_df, select = -c(d))
  
  i <- 1
  # row_info <- list()
  row_info <- vector("list", (4 * len_trains) + 1)
  
  counters <- rep(1, len_trains)
  
  # set up flags
  no_more_trips <- F
  waited_too_long <- F
  
  expected_wait_df <- wait_times %>%
    filter(stop_mta_id == train_data[[1]]$stop_mta_id[1],
           route_mta_id %in% trains[1])
  
  while (i <= num_trains) {
    
    start_time <- train_data[[1]]$start_time[i]
    start_mta_route = train_data[[1]]$route_mta_id[i]
    end_time <- train_data[[1]]$departure_time[i]
    start_time_tod <- convert_hour_to_time_of_day(train_data[[1]]$hour[i])
    start_time_dow <- wday(start_time)
    expected_wait <- expected_wait_df %>%
      filter(day == start_time_dow,
             time_of_day == start_time_tod,
             route_mta_id == start_mta_route) %>%
      select(pred_median_wait) %>%
      first()
    
    # check if expected_wait time exists
    if (is_empty(expected_wait)) {
      expected_wait <- 0
    }
    # expected_wait <- if_else(is_empty(expected_wait), 0, expected_wait)
    
    
    
    row_info[[1]] <- expected_wait / 60
    row_info[[2]] <- as.character(start_mta_route)
    row_info[[3]] <- train_data[[1]]$realtime_trip_id[i]
    row_info[[4]] <- start_time
    row_info[[5]] <- end_time
    
    row_info_idx <- 6
    
    # row_info <- c(row_info,
    #               expected_wait / 60, # divide by 60 to turn to mins
    #               as.character(start_mta_route),
    #               train_data[[1]]$realtime_trip_id[i],
    #               start_time,
    #               end_time)
    
    if (len_trains > 1) {
      for (ii in 2:len_trains) {
        start_transfer_time <- itinerary %>% filter(event == 'start_transfer' , leg == ii-1)
        transfer_time <- ifelse(nrow(start_transfer_time) == 0, 0, as.integer(start_transfer_time$weight))
        
        # NOTE: DEBUG CODE
        # print(sprintf("ii = %d", ii))
        # print(train_data[[ii]]$realtime_trip_id[counters[ii]])
        # print(train_data[[ii]]$departure_time[counters[ii]])
        # print(end_time)
        while ((counters[ii] <= nrow(train_data[[ii]])) &
               (train_data[[ii]]$start_time[counters[ii]] < end_time + transfer_time)) {
          # NOTE: DEBUG CODE
          # print(sprintf("counters[ii] = ", counters[ii]))
          # print(sprintf("skipping train %s on leg %d", train_data[[ii]]$realtime_trip_id[counters[ii]], ii))
          counters[ii] = counters[ii] + 1
        }
        
        curr_idx <- counters[ii]
        
        # break out if this train_data doesn't exist
        if(is.na(train_data[[ii]]$departure_time[curr_idx])) {
          no_more_trips <- T
          break
        }
        
        # Added condition here to ignore trips where you wait at a station for more than an hour
        if(train_data[[ii]]$departure_time[curr_idx] > end_time + 3600) {
          waited_too_long <- T
          break
        }
        
        end_time <- train_data[[ii]]$departure_time[curr_idx]
        row_info[[row_info_idx]] <- as.character(train_data[[ii]]$route_mta_id[curr_idx])
        row_info[[row_info_idx + 1]] <- train_data[[ii]]$realtime_trip_id[curr_idx]
        row_info[[row_info_idx + 2]] <- train_data[[ii]]$start_time[curr_idx]
        row_info[[row_info_idx + 3]] <- end_time
        row_info_idx = row_info_idx + 4
        
        # row_info <- c(row_info,
        #               as.character(train_data[[ii]]$route_mta_id[curr_idx]),
        #               train_data[[ii]]$realtime_trip_id[curr_idx],
        #               train_data[[ii]]$start_time[curr_idx],
        #               end_time)
        
        # NOTE: DEBUG CODE
        # print(sprintf("Leg #%s\n", ii))
        # print(row_info)
      }
    }
    
    
    if (no_more_trips == T) {
      output_df[i, ] <- NA
      break;
    } else if (waited_too_long == T) {
      output_df[i, ] <- NA
      # do nothing
      waited_too_long = F
    } else {
      output_df[i, ] <- row_info
    }
    # row_info <- list()
    row_info_idx = 1
    # increment i
    i = i + 1
  }
  
  output_df <- na.omit(output_df)
  
  for (i in 1:len_trains) {
    train <- trains[i]
    output_df[[paste('leg', i, '_start_time', sep='')]] <- as.POSIXct(output_df[[paste('leg', i, '_start_time', sep='')]],
                                                                      origin = "1970-01-01", tz = "UTC")
    output_df[[paste('leg', i, '_end_time', sep='')]] <- as.POSIXct(output_df[[paste('leg', i, '_end_time', sep='')]],
                                                                    origin = "1970-01-01", tz = "UTC")
  }
  
  output_df$adjusted_end_time <- output_df[, ncol(output_df)] + end_transfer_time
  output_df$time_diff <- output_df[, ncol(output_df)] - output_df[, 4] # as.POSIXct(start_time, origin = "1970-01-01", tz = "UTC")
  # set units of time_diff to minutes always
  units(output_df$time_diff) <- "mins"
  
  # adjust time_diff with padded_wait
  output_df$time_diff <- output_df$time_diff + output_df$expected_wait
  # output_df$given_start_time <- as.POSIXct(start_time, origin = "1970-01-01", tz = "UTC")
  
  output_df$label <- label
  
  # filter out the beginning ti
  
  return(output_df)
}

# given a table for one itinerary with time_diff column, will plot the
# distribution of difftimes for a given itinerary time dataframe
plot_distribution_for_itinerary <- function(itin_time_df, start_hour = 7, end_hour = 10) {
  # filter itin_time_df to only include certain data
  filtered <- itin_time_df #%>%
  #   mutate(day_of_week = weekdays(leg1_start_time),
  #          hour = hour(leg1_start_time)) %>%
  #   filter(isWeekend(day_of_week) == F,
  #          hour >= start_hour & hour < end_hour)
  
  plot <- filtered %>%
    ggplot() +
    geom_histogram(aes(x = time_diff))
  
  return(plot)
}

# given cleaned shortest path itin data, return dataframe with all historical itins
compute_all_itins <- function(cleaned_data, subway_data, start_hour = 7, end_hour = 10) {
  
  num_itins <- cleaned_data$itinerary_id %>% unique() %>% length()
  result <- vector("list", length = num_itins)
  
  # iterate through all itineraries
  for (i in 1:num_itins) {
    itin <- cleaned_data %>% filter(itinerary_id == i)
    result[[i]] <- get_itinerary_times(itin, subway_data, start_hour, end_hour) %>% mutate(itin_id = i, itin_label=sprintf("%s: %s", i, label))
  }
  
  all_itin_df <- bind_rows(result)
  
  return(all_itin_df)
}

# given a dataframe with multiple itins
# returns density plot / histogram of time_diffs
plot_densities <- function(all_itin_df, start_hour = 7 , end_hour = 10, bins = 60, route_color_df) { 
  
  plot_data <- all_itin_df #%>%
    # mutate(day_of_week = weekdays(leg1_start_time),
    #        hour = hour(leg1_start_time)) %>%
    # filter(isWeekend(day_of_week) == F,
    #        hour >= start_hour & hour < end_hour)
  
  plot_data$itin_id <- as.factor(plot_data$itin_id)
  
  colors <- route_color_df$route_color
  names(colors) <- route_color_df$route_short_name
  
   
  plot_lines <- substr(unique(plot_data$label), 1, 1)
  plot_colors <- as.vector(colors[plot_lines])
  
  plot <- plot_data %>%
    ggplot(aes(x = as.numeric(time_diff), group=itin_label, fill=itin_label)) +
    geom_histogram(position = "identity", alpha = 0.5, bins = bins) +
    labs(title = "Trip Time Evaluation", x = "Trip Time (in minutes)", y = "Number of Trips", fill = "Itinerary\n") +
    #xlab('Trip Time (in minutes)') +
    #ylab('Number of Trips') +
    theme(legend.position = "bottom", legend.direction = "vertical", legend.key.size = unit(1.5, 'lines')) +
    scale_fill_manual(guide = guide_legend(), values = plot_colors) + 
    scale_color_manual(values = plot_colors) +
    xlim(as.numeric(quantile(plot_data$time_diff, c(0.1, 0.9))))
  # geom_histogram(), position = "identity", alpha = 0.5)
  
  return(plot)
}

plot_facet_hist <- function(all_itin_df, start_hour = 7 , end_hour = 10, bins = 60, route_color_df) { 
  
  plot_data <- all_itin_df
  
  plot_data$itin_id <- as.factor(plot_data$itin_id)
  
  colors <- route_color_df$route_color
  names(colors) <- route_color_df$route_short_name
  
  
  plot_lines <- substr(unique(plot_data$label), 1, 1)
  plot_colors <- as.vector(colors[plot_lines])
  
  plot <- plot_data %>%
    ggplot(aes(x = as.numeric(time_diff), group=itin_label, fill=itin_label)) +
    geom_histogram(position = "identity", alpha = 0.5, bins = bins) +
    labs(title = "Trip time from Atlantic Ave. station to Fulton Station ", 
         x = "Trip Time (in minutes)", 
          y = "Number of Trips", fill = "Itinerary\n") +
    theme(legend.position = "bottom", legend.direction = "vertical", legend.key.size = unit(1.5, 'lines')) +
    scale_fill_manual(guide = guide_legend(), values = plot_colors) + 
    scale_color_manual(values = plot_colors) +
    xlim(as.numeric(quantile(plot_data$time_diff, c(0.1, 0.9))))
  # geom_histogram(), position = "identity", alpha = 0.5)
  
  return(plot)
}
