# Libraries
library(tidyverse) # A combination of packages that provide useful functions and workflow operations
library(officer) # Allows me to read and edit .docx documents

# Source
source("./Scripts/Constants.R")
source("./Scripts/Nagios_API.R")
source("./Scripts/general_reporting.R")


# uptime plot
plot_uptime <- function(server, start, end) {
  uptime_data <- get_uptime_data(start = start, end = end) |> 
    filter(host_name == server,
           service_description == "CPU Load") |> 
    mutate(time = as.POSIXct(state_time, tryFormats = "%Y-%m-%d %H:%M:%S", tz = TIME_ZONE)) |> 
    select(time, state) 
  
  # 2. Process for Sawtooth Effect
  sawtooth_df <- uptime_data %>%
    arrange(time) %>%
    mutate(
      # Time diff from previous event
      diff = as.numeric(difftime(time, lag(time, default = time[1]), units = "hours")),
      # Cumulative uptime logic
      uptime = 0
    )
  
  # Calculate cumulative uptime manually to handle resets
  current_uptime <- 0
  for(i in 2:nrow(sawtooth_df)) {
    if(sawtooth_df$state[i-1] == 0) {
      current_uptime <- current_uptime + sawtooth_df$diff[i]
    } else {
      current_uptime <- 0
    }
    sawtooth_df$uptime[i] <- current_uptime
  }
  
  # 3. The "Vertical Drop" Trick
  # For every row where state is Down (1), we need a point at the peak AND a point at 0
  plot_data <- sawtooth_df %>%
    group_by(time) %>%
    do({
      if (.$state == 1) {
        # Return two rows for the same timestamp: the peak and the reset
        data.frame(time = .$time, uptime = c(.$uptime, 0))
      } else {
        data.frame(time = .$time, uptime = .$uptime)
      }
    })
  
  if (!is.POSIXct(start)) start <- as.POSIXct(start, tryFormats = "%d-%m-%Y")
  if (!is.POSIXct(end)) end <- as.POSIXct(end, tryFormats = "%d-%m-%Y")
  
  # 4. Plot
  plot_data |> 
    drop_na() |> 
    GG_DATE_LINE(x = time, 
                 y = uptime, 
                 size = 1,
                 colour = "#e02424") +
      GENERAL_THEME() +
      guides(colour = "none") +
      labs(
        title = server,
        x = format.Date(start, format = "%B %Y"),
        y = "Up-time (hours)"
      )
}

report_uptime <- function(report, servers, start, end) {
  
  for (server in servers) {

    if (cursor_reach_test(report, UPTIME_LABEL)) {
      report <- cursor_reach(report, UPTIME_LABEL) |> 
        body_add_par(server, style = "heading 3") |> 
        report_add_plot(plot = plot_uptime(server, start, end))
      
    } else warning(sprintf("Label '%s' not in document", UPTIME_LABEL)) # warning if label not in document
  }
  
  # Remove CPU usage label from template  
  report <- cursor_reach(report, UPTIME_LABEL) |> 
    body_remove()
  
  return(report)
}

# Example
#server <- "ACH DC_Radix"
#start <- "25-11-2025"
#end <- "01-01-2026"

#plot_uptime(server = server, start = start, end = end)