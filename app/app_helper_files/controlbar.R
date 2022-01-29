controlbar <-
  dashboardControlbar(skin = "light", collapsed = TRUE, width = 400,
  conditionalPanel('output.user_group == `ADMIN` && input.tabs == `admin`',
  controlbarMenu(
  id = "menu",
  controlbarItem(
  NULL,
  br(),
  chooseSliderSkin("Flat"), #, color = "#e00007"),
 h4('Postgresql database'),
          tableOutput('available_dbs'),
   h4('Import and removal'),

          fileInput('file_db', 'Choose rds-file being loaded ', accept = c('.rds')),
          textInput('table_name', 'PostgreSQL table name', value = NULL),
        appButton(inputId = "save_replace", label = 'save (replace)', icon = icon("save")),
        appButton(inputId = "save_append", label = 'save appned', icon = icon("save")),
          appButton(inputId = "delete_table", label = 'remove table', icon = icon("remove")),
  br(),
  br(),
  br(),
   h4('Export'),
        textAreaInput("sql", "Enter a sql query", 'SELECT * FROM historic_data',
                      width = "300", height = '120px'),
        appButton(inputId = "execute", label = 'get_data', icon = icon("save")),
  br(),
  br(),
 h4('Download as rds file'),
  downloadButton("rds", NULL, block = F, style = "simple", size="lg")
  # br(),
  # br(),
  # h4('Shinyproxy username'),
  # verbatimTextOutput('sp_user'),
  # h4('Shinyproxy user groups'),
  # verbatimTextOutput('sp_groups')
  ))))