@echo off
echo Checking and installing packages...
REM Use Rscript for package installation (runs in clean session)
"..\dist\App\R-Portable\bin\Rscript.exe" --vanilla "install_packages.R"

echo Starting Shiny application...
REM Run the Shiny app
"..\dist\App\R-Portable\bin\R.exe" -e "shiny::runApp(appDir = '.', host = '0.0.0.0', port = 3838)"
pause