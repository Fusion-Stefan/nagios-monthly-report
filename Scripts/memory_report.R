# Libraries
library(tidyverse) # A combination of packages that provide useful functions and workflow operations
library(officer) # Allows me to read and edit .docx documents

# Source
source("./Scripts/Constants.R")
source("./Scripts/Nagios_API.R")
source("./Scripts/general_reporting.R")

# Generate memory usage report
memory_plot <- function(server, start, end, save_plot = FALSE) {
  
  # Get memory usage data from nagios api
  memory_data <- get_rrp_data(host = server,
                           service = "Memory Usage",
                           start = start, 
                           end = end)
  
  # If something went wrong with the API call
  if (is.null(memory_data)) return(NULL)
  
  # Draw plot
  memory_plot <- memory_data |> 
    mutate(time = as.POSIXct(timestamps, origin = "1970-01-01", tz = TIME_ZONE)) |> 
    select(time, "memory in use", "memory total") |> 
    pivot_longer(cols = c("memory in use", "memory total"), values_to = "memory", names_to = "series") |> 
    drop_na() |> 
    GG_DATE_LINE(x = time, y = memory, colour = series) +
    GENERAL_THEME() +
    COULOUR_SCALE() +
    labs(title = server,
         x = format.Date(as.POSIXct(start, tryFormats = "%d-%m-%Y"), "%B %Y"),
         y = "Memory (Mb)",
         colour = "")
  
  if (save_plot) {
    ggsave(paste0(TEMP_PATH, MEMORY_USAGE_PLOT), 
           plot = memory_plot,
           width = PLOT_WIDTH_GENERAL, 
           height = PLOT_HEIGHT_GENERAL)}
  
  return(memory_plot)
}

# Memory report section
report_memory_usage <- function(report, servers, start, end) {
  
  service_messages <- SERVICE_STATUS_DATA |> 
    filter(host_name %in% servers,
           service_description == "Memory Usage") |> 
    select(host_name, output)
  
  for (server in servers) {
    message <- service_messages |> 
      filter(host_name == server) |> 
      pull(output)
    
    if (cursor_reach_test(report, MEMORY_USAGE_LABEL)) {
      data_plot <- memory_plot(server, start, end)
      if (!is.null(data_plot)){
      report <- cursor_reach(report, MEMORY_USAGE_LABEL) |> 
        body_add_par(paste0(server, " - (",message, ")"), style = "heading 3") |> 
        report_add_plot(plot = data_plot)} else
        {warning(paste0("Something went wrong with retreiving Memory data for ", server))}
      
    } else warning(sprintf("Label '%s' not in document", MEMORY_USAGE_LABEL)) # warning if label not in document
  }
  
  # Remove memory usage label from template  
  report <- cursor_reach(report, MEMORY_USAGE_LABEL) |> 
    body_remove()
  
  return(report)
}
