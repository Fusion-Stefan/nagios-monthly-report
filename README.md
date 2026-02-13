# Nagios Monthly Report Generator

Use Nagios API and the Officer R package to easily generate a monthly report for clients!


## Installation

1. Download or clone this repository.
2. Download [R-Portable (4.2.0)](https://sourceforge.net/projects/rportable/files/R-Portable/4.2.0/R-Portable_4.2.0.paf.exe/download)
3. Install R-Portable. directly into the nagios-monthly-report\dist folder. Make sure that no "R-Portable" folder is created.
4. Double click the nagios-monthly-report\Shiny-FT-Monthly-Report\run_app.bat file to run the app.

   * Tip: Create a shortcut to point to this file to easily start the app.
   * The first time the app runs it will install all necessary R packages and might take a while.
   * To access the app browse "http://{ip-address}:3838".
5. Add new text file nagios-monthly-report\Scripts\nagios-api-key.txt with only the Nagios API key on the first line.