# Define Constants to use across report

# Settings
MONTH_START <- 15
ALLOWED_YEARS_PAST <- 3

# File Paths
REPORT_PATH <- "./Reports/" # Location to place generated reports
#REPORT_PATH <- "./Reports/" # For local testing purposes
TEMPLATE_PATH <- "./Report Template/report_template.docx" # Location of template to use in report
TEMP_PATH <- "./Temp/" # Folder for temporary plots
CLIENT_LIST_PATH_TXT <- "./Report Template/client_list.txt"
CLIENT_LIST_PATH_JSON <- "./Report Template/client_list.json"
SCREENSHOT_DIR <- "C:/Users/Stefan/Pictures/Screenshots/"

# File Names
CPU_USAGE_PLOT <- "cpu.pdf"
MEMORY_USAGE_PLOT <- "memory.pdf"
DISK_USAGE_PLOT <- "disk.pdf"

# Word document labels
CLIENT_LABEL <- "::CLIENT::"
MONTHYEAR_LABEL <- "::MONTHYEAR::"
REPORTER_LABEL <- "::NAME::"
END_DATE_LABEL <- "::DATE::"
CPU_USAGE_LABEL <- "::CPU::"
MEMORY_USAGE_LABEL <- "::MEMORY::"
DISK_USAGE_LABEL <- "::DISKUSAGE::"
ANTI_VIRUS_LABEL <- "::ANTIVIRUS::"
BACKUP_LABEL <- "::BACKUPRECOVERYTESTING::"
SERVER_UPDATE_LABEL <- "::SERVERUPDATES::"
RAID_LABEL <- "::RAIDDRIVES::"
UPTIME_LABEL <- "::UPTIME::"
ADDINFO_LABEL <- "::ADDINFO::"
SERVERROLES_LABEL <- "::SERVERROLES::"


# API 
NAGIOS_API_KEY <- readLines("nagios-api-key.txt")[1] # Nagios API KEY
NAGIOS_IP <- "172.16.30.11" # Nagios server IP address

# Region info
TIME_ZONE <- "Pacific/Auckland"

# Images
IMAGE_WIDTH <- 4
IMAGE_HEIGHT <- 3


# Plots
PLOT_WIDTH_GENERAL <- 6.5 # Inches
PLOT_HEIGHT_GENERAL <- 1.5 # Inches
PLOT_GG_SCALE <- 0.6

# ggplot theme
GENERAL_THEME <- function(...) {
  theme(
  panel.background = element_rect(fill = "#ffffff"),
  panel.grid = element_line(colour = "#dddddd"),
  axis.text.x = element_text(angle = 0, hjust = 0.5),
  plot.title = element_text(hjust = 0.5),
  ...
  )
}

# custom discrete color scheme
COULOUR_SCALE <- function(...) {
  scale_colour_manual(
    values = c("#e02424", "#222222d5", "#9c9c9c"),
    ...
  )
}

# default date-x line plot
GG_DATE_LINE <- function(data, x, y, colour, ...) {
  ggplot(data) +
  aes(x = {{x}}, y = {{y}}, colour = {{colour}}) +
  geom_line(...) +
  scale_x_datetime(date_breaks = "day",
                   date_labels = "%d", 
                   timezone = TIME_ZONE,
                   expand = c(0.02, 0, 0, 0))
}

