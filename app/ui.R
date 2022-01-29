# ui of A848
rm(list = ls())
packagesToLoad <- c('RPostgreSQL', 'shiny', 'shinythemes' ,'shinyWidgets', 'DT', 'dplyr', 'readxl', 'shinydashboard',
                    'shinydashboardPlus', 'data.table', 'fresh','shinyjs', 'shinyBS', 'openxlsx', 'rhandsontable',
                    'excelR', 'shinyalert')

# do the loading and print wether the package is installed
sapply(packagesToLoad, function(x) {require(x,character.only=TRUE)} )

addResourcePath('A848_logo', 'www/A848_logo.jpg')
addResourcePath('ProfilFoto', 'www/ProfilFoto.jpg')
addResourcePath('app.css', 'www/app.css')

source('app_helper_files/radioTooltips.R')

function(request) {
  source('app_helper_files/header.R')
source('app_helper_files/sidebar.R')
source('app_helper_files/body.R')
source('app_helper_files/controlbar.R')

  dashboardPage(skin='red-light',
    header,
    sidebar,
    body,
   controlbar)
}