# Libraries
library(tidyverse) # A combination of packages that provide useful functions and workflow operations
library(officer) # Allows me to read and edit .docx documents

# Source
source("./Scripts/Constants.R")
source("./Scripts/Nagios_API.R")
source("./Scripts/general_reporting.R")

# Generate CPU usage report
cpu_plot <- function(server, start, end, save_plot = FALSE) {
  
  # Get cpu usage data from nagios api
  cpu_data <- get_rrp_data(host = server,
                           service = "CPU Load",
                           start = start, 
                           end = end)
  
  # If something went wrong with the API call
  if (is.null(cpu_data)) return(NULL)
  
  # Draw plot
  cpu_plot <- cpu_data |> 
    mutate(time = as.POSIXct(as.numeric(timestamps), origin = "1970-01-01", tz = TIME_ZONE)) |> 
    select(time, cpu_total, cpu_avg) |> 
    pivot_longer(cols = c("cpu_total", "cpu_avg"), values_to = "cpu", names_to = "series") |> 
    drop_na() |> 
    GG_DATE_LINE(x = time, y = cpu, colour = series) +
    GENERAL_THEME() +
    COULOUR_SCALE() +
    labs(title = server,
         x =  format.Date(as.POSIXct(start, tryFormats = "%d-%m-%Y"), "%B %Y"),
         y = "CPU Usage (%)",
         colour = "")
  
  if (save_plot) {
    ggsave(paste0(TEMP_PATH, CPU_USAGE_PLOT), 
           plot = cpu_plot,
           width = PLOT_WIDTH_GENERAL, 
           height = PLOT_HEIGHT_GENERAL)}
  
  return(cpu_plot)
}

# CPU report section
report_cpu_usage <- function(report, servers, start, end) {
  
  service_messages <- SERVICE_STATUS_DATA |> 
    filter(host_name %in% servers,
           service_description == "CPU Load") |> 
    select(host_name, output)
  
  for (server in servers) {
    message <- service_messages |> 
      filter(host_name == server) |> 
      pull(output) |> 
      str_remove_all(":.*")
    
    if (cursor_reach_test(report, CPU_USAGE_LABEL)) {
      data_plot <- cpu_plot(server, start, end)
      if (!is.null(data_plot)){
      report <- cursor_reach(report, CPU_USAGE_LABEL) |> 
        body_add_par(paste0(server, " - (",message, ")"), style = "heading 3") |> 
        report_add_plot(plot = data_plot) } else
        {warning(paste0("Something went wrong with retreiving CPU data for ", server))}
      
    } else warning(sprintf("Label '%s' not in document", CPU_USAGE_LABEL)) # warning if label not in document
  }
  
  # Remove CPU usage label from template  
  report <- cursor_reach(report, CPU_USAGE_LABEL) |> 
    body_remove()
  
  return(report)
}