# Libraries
library(tidyverse) # A combination of packages that provide useful functions and workflow operations
library(officer) # Allows me to read and edit .docx documents

# Scripts
source("./Scripts/Constants.R")
source("./Scripts/general_reporting.R")
source("./Scripts/cpu_report.R")
source("./Scripts/memory_report.R")
source("./Scripts/disk_report.R")
source("./Scripts/uptime_report.R")

# Build report
generate_report <- function(servers, client, reporter, start, end,  ...) {
  report <- create_report(
    client = client,
    reporter = reporter,
    report_date = as.POSIXct(start, tryFormats = "%d-%m-%Y"), ...) |> 
    # CPU Reporting
    report_cpu_usage(servers = servers,
                     start = start,
                     end = end) |> 
    # Memory Reporting
    report_memory_usage(servers = servers,
                        start = start,
                        end = end) |> 
    # Disk Usage Reporting
    report_disk_usage(servers = servers,
                      start = start,
                      end = end) |> 
    report_uptime(servers = servers,
                  start = start,
                  end = end)
  
  return(report)
}  


# Examples
#servers <- c(
#  "ACH Backup Server",
#  "ACH DC_Radix",
#  "ACH HyperV",
#  "ACH RDP"
#)
#start <- "01-12-2025"
#end <- "01-01-2026"
#client = "ACH"
#reporter = "Stefan Walles"

#generate_report(servers = servers,
#                client = client,
#                reporter = reporter,
#                start = start,
#                end = end,
#                filename = "test.docx")
  