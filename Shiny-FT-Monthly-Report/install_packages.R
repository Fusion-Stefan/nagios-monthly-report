# Install packages if not available
lib_path <- normalizePath("../dist/App/R-Portable/library", mustWork = FALSE)
dir.create(lib_path, recursive = TRUE, showWarnings = FALSE)

list.of.packages <- c("shiny", "bslib", "tidyverse", "officer", "flextable", "jsonlite", "htmltools")

# Check for missing packages in the portable library
installed <- installed.packages(lib.loc = lib_path)[,"Package"]
new.packages <- setdiff(list.of.packages, installed)

# Install missing packages if any are found
if(length(new.packages)) {
  install.packages(
    new.packages,
    lib = "../dist/App/R-Portable/library",
    repos = "https://cran.stat.auckland.ac.nz/",
    type = "win.binary")
}
