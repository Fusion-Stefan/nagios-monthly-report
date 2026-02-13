# Libraries
library(tidyverse) # A combination of packages that provide useful functions and workflow operations
library(officer) # Allows me to read and edit .docx documents

# Source
source("./Scripts/Constants.R")
source("./Scripts/Nagios_API.R")
source("./Scripts/general_reporting.R")

# Generate Disk usage report plots
disk_plot <- function(server, start, end, save_plot = FALSE) {
  
  # Get names of all disk on server
  disk_names <- SERVICE_STATUS_DATA |> 
    filter(host_name == server, grepl("Disk Space", service_description)) |> 
    pull(service_description)
  
  
  # Get disk usage data from nagios api
  disk_data <- lapply(disk_names, 
                      function(disk)
                        get_rrp_data(host = server,
                          service = disk,
                          start = start, 
                          end = end) |> 
                        mutate(disk_name = disk)) |> 
               do.call(rbind, args = _)
  
  # If something went wrong with the API call
  if (is.null(disk_data)) return(NULL)
  
  # Disk message
  service_message_data <- SERVICE_STATUS_DATA |> 
    filter(host_name == server,
           service_description %in% disk_names) |> 
    select(service_description, output)
  
  service_messages <- service_message_data$output
  names(service_messages) <- service_message_data$service_description
  
  # Draw plot
  disk_plot <- disk_data |> 
    mutate(time = as.POSIXct(timestamps, origin = "1970-01-01", tz = TIME_ZONE)) |> 
    select(time, "disk in use", "disk total", disk_name) |> 
    pivot_longer(cols = c("disk in use", "disk total"), values_to = "disk", names_to = "series") |>
    mutate(disk_gb = disk / 1024) |> 
    drop_na() |> 
    GG_DATE_LINE(x = time, y = disk_gb, colour = series) +
    GENERAL_THEME() +
    COULOUR_SCALE() +
    facet_wrap(~disk_name, ncol = 1, 
               labeller = as_labeller(service_messages),
               scales = "free_y") +
    labs(title = server,
         x =  format.Date(as.POSIXct(start, tryFormats = "%d-%m-%Y"), "%B %Y"),
         y = "Disk Usage (Gb)",
         colour = "")
  
  if (save_plot) {
    ggsave(paste0(TEMP_PATH, DISK_USAGE_PLOT), 
           plot = disk_plot,
           width = PLOT_WIDTH_GENERAL, 
           height = PLOT_HEIGHT_GENERAL * length(disk_names))}
  
  return(disk_plot)
}

# disk report section
report_disk_usage <- function(report, servers, start, end) {
  
  for (server in servers) {
    num_disks <- SERVICE_STATUS_DATA |> 
      filter(host_name == server,
             grepl("Disk Space", service_description)) |> 
      nrow()
    
    if (cursor_reach_test(report, DISK_USAGE_LABEL)) {
      data_plot <- disk_plot(server, start, end)
      if (!is.null(data_plot)){
        report <- cursor_reach(report, DISK_USAGE_LABEL) |> 
          body_add_par(server, style = "heading 3") |> 
          report_add_plot(plot = data_plot, scale_height = num_disks)} else
          {warning(paste0("Something went wrong with retreiving Disk data for ", server))}
      
    } else warning(sprintf("Label '%s' not in document", DISK_USAGE_LABEL)) # warning if label not in document
  }
  
  # Remove Disk usage label from template  
  report <- cursor_reach(report, DISK_USAGE_LABEL) |> 
    body_remove()
  
  return(report)
}

