# Libraries
library(jsonlite)
library(tidyverse)

# Source
source("./Scripts/Constants.R")

# Functions
json_to_df <- function(json) {
  lapply(json, function(x) unlist(x)) |> 
    do.call(rbind, args = _) |> 
    as.data.frame()
}

to_UNIX_time <- function(time) {
  if ("POSIXct" %in% class(time)) { # If POSIXct time is used
    return(as.integer(time))
  } else if ("character" == class(time)) { # If date is specified as string
    UNIX_time <- as.POSIXct(time, tz = TIME_ZONE, tryFormats = "%d-%m-%Y");
    return(as.integer(UNIX_time))
  } else stop("Invalid date value")
}

# Get rrp data
get_rrp_data <- function(host, # str: host name
                         service, # str: service name
                         start = Sys.time() - 30 * 86400, # time: start of interval (default now - 30 days) OR str of format DD-MM-YYYY
                         end = Sys.time(), # time: end of interval (default now)
                         step = 300 # int: time in seconds between data points (default 300)
                         ) {
  
  # Change times to UNIX time
  start <- to_UNIX_time(start)
  end <- to_UNIX_time(end)
  
  # Construct an API call
  api_call <- sprintf(
    paste0("http://%s/nagiosxi/api/v1/objects/rrdexport?",
            "apikey=%s",
            "&host_name=%s",
            "&service_description=%s",
            "&start=%d",
            "&end=%d",
            "&step=%d",
            "&maxrows=%d"),
    NAGIOS_IP,
    NAGIOS_API_KEY,
    str_replace_all(host, " ", "+"),
    str_replace_all(service, " ", "+"),
    start,
    end,
    step,
    (end - start) %/% step + 10)
  
  # Call API and read JSON data
  rrp_data <- read_json(api_call)
  
  # Convert raw JSON data to data frame
  timestamps <- sapply(rrp_data$data$row, function(x) as.numeric(x$t))
  values <- lapply(rrp_data$data$row, function(x) as.numeric(unlist(x$v))) |> 
    do.call(rbind, args = _)
  colnames(values) <- rrp_data$meta$legend$entry
  
  df <- as.data.frame(cbind(timestamps, values))
  
  if (nrow(df) < 3 && ncol(df) < 3) 
   return(NULL) 
  else 
    return(df)
  
}

get_hoststatus_data <- function() {
  hoststatus_data <- read_json(sprintf("http://%s/nagiosxi/api/v1/objects/hoststatus?apikey=%s",
                                       NAGIOS_IP,
                                       NAGIOS_API_KEY))
  return(json_to_df(hoststatus_data$hoststatus))
}

get_servicestatus_data <- function() {
  servicestatus_data <- read_json(sprintf("http://%s/nagiosxi/api/v1/objects/servicestatus?apikey=%s",
                                       NAGIOS_IP,
                                       NAGIOS_API_KEY))
  return(json_to_df(servicestatus_data$servicestatus))
}

get_uptime_data <- function(
    start = Sys.time() - 30 * 86400, # time: start of interval (default now - 30 days) OR str of format DD-MM-YYYY
    end = Sys.time() # time: end of interval (default now)
    ) {
  uptime_data <- read_json(sprintf("http://%s/nagiosxi/api/v1/objects/statehistory?apikey=%s&starttime=%d&endtime=%d",
                                   NAGIOS_IP,
                                   NAGIOS_API_KEY,
                                   to_UNIX_time(start),
                                   to_UNIX_time(end)))
  return(json_to_df(uptime_data$stateentry))
}

# host and server status data
HOST_STATUS_DATA <- get_hoststatus_data()
SERVICE_STATUS_DATA <- get_servicestatus_data()


# Test get_rrp_data function for ACH Backup host and CPU Load service
# ACH_Backup_CPU <- get_rrp_data(host = "ACH Backup Server",
#                               service = "CPU Load",
#                               start = "01-12-2025", 
#                               end = "01-01-2026")


