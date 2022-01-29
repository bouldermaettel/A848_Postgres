# server side of A848
function(input, output, session){

#   format_data <- function(data) {
#   data %>% datatable( options = list(searching = T, pageLength=20, lengthMenu = c(10, 20, 30, 50, 100, 200),
#                               columnDefs = list(list(className = 'dt-left', targets = '_all')), autoWidth = TRUE, scrollx=TRUE),
#              filter = list( position = 'top', clear = TRUE ), fillContainer = FALSE)
# }

source('src/data/data_wrangler.R')

variable <- reactiveValues()
data <- reactiveValues()
wb <- reactiveValues(upload = 0, edit = T)
delete_rows <- reactiveValues()
delete_table <- reactiveValues()

observeEvent(input$hide, {
js$hidehead('none')
variable$head <- 'hidden'
})

observeEvent(input$show, {
js$hidehead('')
    variable$head <- 'not hidden'
})

# adapt the max size of file-upload
options(shiny.maxRequestSize=50000*1024^2)

# # load data from PostgreSQL database
# observe({
#   if (wb$upload == 0) {
#     pgdrv <- dbDriver(drvName = "PostgreSQL")
#     con <-DBI::dbConnect(pgdrv,
#                          dbname ="postgres",
#                          # host ='172.18.0.3',
#                          host="db",
#                          #  port=5432,
#                          user = 'root',
#                          password = 'root')
#                          # user = 'shiny_user',
#                          # password = '7B*LQckf')
#   existing_tables <- DBI::dbListTables(con)
#   if ('historic_data' %in% existing_tables) {
#       res <- dbSendQuery(con, 'SELECT * FROM historic_data')
#       data$hist <- tibble::tibble(dbFetch(res))
#   } else {
#       data$hist <- NULL
#   }
# }
# # DBI::dbDisconnect(con)
#   wb$upload <- 1
# })

# load data from database (inside Docker)
observe({
  if (wb$upload == 0) {
    data$hist <- tibble::tibble(readRDS(file = "./data/data_test.rds")) %>% arrange(desc(ID_SMC))
    wb$upload <- 1
    }
})


# import new file (from user)
observeEvent(input$file_input, {
  req(input$file_input)
  inFile <- input$file_input
  ext <- substrRight(inFile$datapath, 4)
    data_temp <- tibble::tibble(get_data(inFile$datapath, 'Sendungen', range = cell_cols('A:P'), col_types = COLTYPES))
  # define new column names (without ID, that need to be defined first)
  colnames(data_temp) <- COLNAMES[2:length(COLNAMES)]
  # choose right formats of columns
  data_temp <- data_temp %>% mutate_at(vars('Datum_Eingang', 'Datum_Brief', 'Frist', 'Datum_Vernichtung', 'Stellungnahme'),  as.Date, format = "%d/%m/%Y")  %>% mutate_at(vars('Nr', 'PLZ'),  as.integer)
  # make an ID out of date as.numeric and three digit Nr
  data_temp <- tibble::add_column(data_temp, ID_SMC = as.integer(paste0(as.numeric(data_temp$Datum_Eingang), sprintf("%03d",data_temp$Nr))), .before = 'Datum_Eingang') %>% arrange(desc(ID_SMC))
  # new_data <- data_temp[data_temp$ID_SMC %in% list(18975020,18268001, 18268018 ,18268017),] %>% arrange(ID_SMC)
  data$new <- data_temp
  data$all <- bind_rows(data$new, data$hist)
})

  # choose output data for data tab
observe({
  if (input$data_source == 'historic'){
    data$show <- data$hist[,input$columns]
  } else if (input$data_source == 'new') {
    data$show <- data$new[,input$columns]
  } else if (input$data_source == 'all' | input$data_source == 'historic & new') {
    data$show <- data$all[,input$columns]
  }
})

# update column and grouping var inputs
observe({
updateSelectizeInput(session, "columns",
           choices= colnames(data$hist),
           selected = colnames(data$hist))

updateSelectizeInput(session, "grouping_vars",
           choices= colnames(data$hist),
           selected = c('Name', 'Vorname'))
})

# get unique records
observe({
  req(data$hist)
  if (input$tabs == 'unique') {
  if (input$data_source == 'historic') {
      data$unique <- tibble(get_uniques(first_df = data$hist, second_df = NULL, as.list(input$grouping_vars)))
  } else if (input$data_source == 'new')  {
      data$unique <- tibble(get_uniques(first_df = data$new, second_df = NULL, as.list(input$grouping_vars)))
  } else if (input$data_source == 'all')  {
      data$unique <- tibble(get_uniques(first_df = data$all, second_df = NULL, as.list(input$grouping_vars)))
  } else if (input$data_source == 'historic & new')  {
  data$unique <- tibble(get_uniques(first_df = data$new, second_df = data$hist, as.list(input$grouping_vars)))
    }
  }
})

# get exact duplicates
observe({
  req(data$hist)
  if (input$calc_mode == 'exact') {
  if (input$data_source == 'historic') {
      data$dupl <- tibble(get_duplicate_records(first_df = data$hist, group_vars = input$grouping_vars))
  } else if (input$data_source == 'new')  {
      data$dupl <- tibble(get_duplicate_records(first_df = data$new, group_vars = input$grouping_vars))
  } else if (input$data_source == 'all')  {
      data$dupl <- tibble(get_duplicate_records(first_df = data$all,group_vars = input$grouping_vars))
  } else if (input$data_source == 'historic & new')  {
  data$dupl <- tibble(get_duplicate_records(first_df = data$new, second_df = data$hist, group_vars = input$grouping_vars))
    }
  }
})

# get fuzzy duplicate records
observeEvent(input$calc, {
  req(data$hist)
  if (input$data_source == 'historic') {
      data$dupl_fuzzy <- tibble(get_fuzzy_duplicate_records(first_df = data$hist, group_vars = input$grouping_vars,
                                                             max_distance = input$fuzzy))
  } else if (input$data_source == 'new')  {
      data$dupl_fuzzy <- tibble(get_fuzzy_duplicate_records(first_df = data$new, group_vars = input$grouping_vars,
                                                             max_distance = input$fuzzy))
  } else if (input$data_source == 'all')  {
      data$dupl_fuzzy <- tibble(get_fuzzy_duplicate_records(first_df = data$all, group_vars = input$grouping_vars,
                                                             max_distance = input$fuzzy))
  } else if (input$data_source == 'historic & new')  {
  data$dupl_fuzzy <- tibble(get_fuzzy_duplicate_records(first_df = data$new, second_df = data$hist,
                                               group_vars = input$grouping_vars, max_distance = input$fuzzy))
      }
})

# choose output data for dupl tab
observe({
  if (input$calc_mode == 'exact'){
    data$show_dupl <- data$dupl[,input$columns]
  } else {
      req(data$dupl_fuzzy)
    data$show_dupl <- data$dupl_fuzzy[,input$columns]
  }
})

observe({
  req(data$unique)
      data$show_unique <- data$unique[,input$columns]
})

observe({
  req(data$hist)
  if (input$data_source == 'historic'){
    data$show <- data$hist[,input$columns]
  } else if (input$data_source == 'new') {
    data$show <- data$new[,input$columns]
  } else if (input$data_source == 'all' | input$data_source == 'historic & new') {
    data$show <- data$all[,input$columns]
  }
})

data$proxy_data <- dataTableProxy('data')
data$proxy_unique <- dataTableProxy('unique')
data$proxy_dupl <- dataTableProxy('dupl')
data$proxy_admin <- dataTableProxy('admin')

observeEvent(input$clear_filters, {
  clearSearch(data$proxy_data)
  clearSearch(data$proxy_unique)
  clearSearch(data$proxy_dupl)
  clearSearch(data$proxy_admin)
})

observeEvent(input$clear_selections, {
  data$proxy_data %>% selectRows(NULL)
  data$proxy_unique %>% selectRows(NULL)
  data$proxy_dupl %>% selectRows(NULL)
  data$proxy_admin %>% selectRows(NULL)
})

output$data <- renderDT({ format_datasets(data$show) })
output$dupl <- renderDT({ format_datasets(data$show_dupl) })
output$unique <- renderDT({ format_datasets(data$show_unique) })
output$admin <- renderDT({ format_datasets(data$db) })


observeEvent(input$excel, {
    if (input$excel==1) {
wb[['duplicates']] <- openxlsx::createWorkbook()
    }
  if (input$tabs == 'duplicates') {
    dupl_rows <- get_rows(input$dupl_rows_selected, input$dupl_rows_all)
  if (input$calc_mode == "fuzzy") {
    groups <- paste0(input$grouping_vars, collapse='_')
    sheet_name <- substr(paste(input$excel,'fuzzy', groups, input$fuzzy, sep='_'),1,31)
    make_sheet(sheet_name, data$show_dupl, dupl_rows, wb)
  } else {
    groups <- paste0(input$grouping_vars, collapse='_')
    sheet_name <- substr(paste(input$excel,'exact', groups, sep='_'),1,31)
    make_sheet(sheet_name, data$show_dupl, dupl_rows, wb)
    }
  } else if (input$tabs == 'unique') {
    unique_rows <- get_rows(input$unique_rows_selected, input$unique_rows_all)
    groups <- paste0(input$grouping_vars, collapse='_')
    sheet_name <- substr(paste(input$excel,input$tabs, groups, sep ='_'),1,31)
    make_sheet(sheet_name, data$show_unique, unique_rows, wb)
  } else if (input$tabs == 'data') {
    data_rows <- get_rows(input$data_rows_selected, input$data_rows_all)
    sheet_name <- substr(paste(input$excel,input$tabs, input$data_source, sep="_"),1,31)
    make_sheet(sheet_name, data$show, data_rows, wb)
  } else if (input$tabs == 'admin') {
    admin_rows <- get_rows(input$admin_rows_selected, input$admin_rows_all)
    sheet_name <- substr(paste(input$excel,input$tabs, input$data_source, sep="_"),1,31)
    make_sheet(sheet_name, data$db, admin_rows, wb)
  }
})

  output$xlsx <- downloadHandler(
  filename = function() {
    paste0('Duplicate_analysis', ".xlsx")
  },
  content = function(file) {
    openxlsx::saveWorkbook(wb[["duplicates"]], file = file, overwrite = TRUE)
  }
)

observeEvent(input$file_input,{
  shinyWidgets::updateAwesomeRadio(session, 'data_source', choices = c("historic", "new", 'all', "historic & new"),
                           selected = "new")
})

output$dirs <- renderText({
  paste(list.dirs('..', recursive=FALSE), collapse = ' | ')
})

############################## Excel to edit data!
observeEvent(input$edit_table, {
source('app_helper_files/modal_dialog.R')
    modal_dialog %>% shiny::showModal()
})

# Excel input$tabs == 'data':
observe({
  if (input$tabs == 'data') {
    data_rows <- get_rows(input$data_rows_selected, input$data_rows_all)
    data$orig <- assign_data(input$data_source, data$hist, data$new, data$all, data_rows, input$columns)
  } else if (input$tabs == 'unique') {
    unique_rows <- get_rows(input$unique_rows_selected, input$unique_rows_all)
    data$orig <- data$unique[unique_rows,input$columns]
  } else if  (input$tabs == 'duplicates') {
      dupl_rows <- get_rows(input$dupl_rows_selected, input$dupl_rows_all)
      if (input$calc_mode == 'exact'){
        data$orig <- data$dupl[dupl_rows,input$columns]
  } else {
        data$orig <- data$dupl_fuzzy[dupl_rows,input$columns]
      }
  }
})

#   # Excel input$tabs == 'unique':
# observe({
#   req(input$unique_rows_selected)
#   if ((input$tabs == 'unique') & (length(input$unique_rows_selected) > 0)) {
#       data$orig <- data$unique[input$unique_rows_selected,input$columns]
#       }
#   })

#   # Excel input$tabs == 'duplicates':
# observe({
#   req(input$dupl_rows_selected)
#   if ((input$tabs == 'duplicates') & (length(input$dupl_rows_selected) > 0)) {
#         if (input$calc_mode == 'exact'){
#         data$orig <- data$dupl[input$dupl_rows_selected,input$columns]
#                     print(data$orig)
#       } else {
#         req(data$dupl_fuzzy, input$dupl_rows_selected)
#         data$orig <- data$dupl_fuzzy[input$dupl_rows_selected,input$columns]
#         }
#     }
#   })

    observeEvent(input$table,{
  data$updated <- tibble::tibble(excel_to_R(input$table))
      print(data$updated)
  })

  # edited data
observeEvent(input$final_edit, {
  new_data <- data$updated %>% arrange(desc(ID_SMC)) #%>% mutate_at(vars('Datum_Eingang', 'Datum_Brief', 'Frist', 'Datum_Vernichtung', 'Stellungnahme'),  as.Date, format = "%d/%m/%Y")

  if (input$data_source == 'historic'){
      data$hist <- data$hist %>% arrange(desc(ID_SMC))
      data$hist[data$hist$ID_SMC %in% data$updated$ID_SMC, input$columns] <- new_data
  } else if (input$data_source == 'new') {
      data$new <- data$new %>% arrange(desc(ID_SMC))
      data$new[data$new$ID_SMC %in% data$updated$ID_SMC, input$columns] <- new_data
  } else if (input$data_source == 'all' | input$data_source == 'historic & new') {

      data$hist <- data$hist %>% arrange(desc(ID_SMC))
      data$hist[data$hist$ID_SMC %in% data$updated$ID_SMC, input$columns] <- new_data[data$updated$ID_SMC %in% data$hist$ID_SMC, ]

      data$new <- data$new %>% arrange(desc(ID_SMC))
      data$new[data$new$ID_SMC %in% data$updated$ID_SMC, input$columns] <- new_data[data$updated$ID_SMC %in% data$new$ID_SMC, ]

      data$all <- data$all %>% arrange(desc(ID_SMC))
      data$all[data$all$ID_SMC %in% data$updated$ID_SMC, input$columns] <- new_data
  }

})


# delete rows
observeEvent(input$delete_rows, {
  delete_rows$response <- FALSE
    shinyalert(title = "Are you sure you want to delete these rows?",
      callbackR = function(x) {
        delete_rows$response <- x
      },
      text = "This action can not be undone!",
      type = "warning",
      showCancelButton = TRUE,
      confirmButtonCol = '#DD6B55',
      confirmButtonText = 'Yes, delete them!'
    )
})


  # delete rows
observeEvent(input$delete_table, {
  delete_table$response <- FALSE
    shinyalert(title = "Are you sure you want to delete this table?",
      callbackR = function(x) {
        delete_table$response <- x
      },
      text = "This action can not be undone!",
      type = "warning",
      showCancelButton = TRUE,
      confirmButtonCol = '#DD6B55',
      confirmButtonText = 'Yes, delete it!'
    )
})

    observeEvent(delete_rows$response, {
      if (delete_rows$response) {
           if (input$tabs == 'data') {
         IDs <- data$show$ID_SMC[input$data_rows_selected] %>% sort(decreasing=FALSE)
       } else if (input$tabs == 'unique') {
         IDs <- data$show_unique$ID_SMC[input$unique_rows_selected] %>% sort(decreasing=FALSE)
       } else if (input$tabs == 'duplicates') {
         IDs <- data$show_dupl$ID_SMC[input$dupl_rows_selected] %>% sort(decreasing=FALSE)
       }


  if (input$data_source == 'historic'){
    data$hist %>% arrange(desc(ID_SMC))
    data$hist <- data$hist[!(data$hist$ID_SMC %in% IDs), ]

  } else if (input$data_source == 'new') {
    data$new %>% arrange(desc(ID_SMC))
    data$new <- data$new[!(data$hist$ID_SMC %in% IDs), ]

  } else if (input$data_source == 'all' | input$data_source == 'historic & new') {

    data$hist %>% arrange(desc(ID_SMC))
    data$hist <- data$hist[!(data$hist$ID_SMC %in% IDs), ]

    data$new %>% arrange(desc(ID_SMC))
    data$new <- data$new[!(data$new$ID_SMC %in% IDs), ]

    data$all %>% arrange(desc(ID_SMC))
    data$all <- data$all[!(data$all$ID_SMC %in% IDs), ]
    }
  }
})


# render excel
observe({
  columns <- data.frame(title = input$columns) # types = c('numeric', COLTYPES) )
  output$table <- renderExcel(excelTable(data = data$orig, showToolbar = T, search=TRUE, columns = columns,
                                         autoFill = T, rowResize = T, allowInsertRow = F, allowInsertColumn = F,
                                         allowDeleteRow = F, allowDeleteColumn = F))

})

# close <- reactive(list(input$dismiss_modal, input$cancel, input$final_edit, input$delete_confirm))
shiny::observeEvent(input$dismiss_modal, { shiny::removeModal() })
shiny::observeEvent(input$final_edit, {shiny::removeModal() })
shiny::observeEvent(input$cancel, {shiny::removeModal()})
shiny::observeEvent(input$delete_confirm, {shiny::removeModal()})

output$user_group <- renderText({
Sys.getenv('SHINYPROXY_USERGROUPS')
})

outputOptions(output, 'user_group', suspendWhenHidden = FALSE)

output$sp_user <- renderText({
    paste('Username:', Sys.getenv('SHINYPROXY_USERNAME'),'/ Usergroups:', Sys.getenv('SHINYPROXY_USERGROUPS'))
})

# output$sp_groups <- renderText({
#   paste('User groups:', Sys.getenv('SHINYPROXY_USERGROUPS'))
# })

  # load data from the database
    # load data from database

  observe({
    if (input$tabs == 'admin') {
pgdrv <- dbDriver(drvName = "PostgreSQL")
con <-DBI::dbConnect(pgdrv,
                    dbname ="postgres",
                     # host ='172.18.0.3',
                    host="db",
                     # port=5432,
                    user = 'root',
                    password = 'root')
data$tables_db <- DBI::dbListTables(con)
DBI::dbDisconnect(con)
      }
  })

observeEvent(input$execute, {
pgdrv <- dbDriver(drvName = "PostgreSQL")
con <-DBI::dbConnect(pgdrv,
                    dbname ="postgres",
                     # host ='172.18.0.3',
                    host="db",
                     # port=5432,
                    user = 'root',
                    password = 'root')
res <- dbSendQuery(con, input$sql)
data$db <- tibble::tibble(dbFetch(res))

})



    # save new file
observeEvent(input$transfer, {
      pgdrv <- dbDriver(drvName = "PostgreSQL")
con <-DBI::dbConnect(pgdrv,
                    dbname ="postgres",
                     # host ='172.18.0.3',
                    host="db",
                    #  port=5432,
                    user = 'root',
                    password = 'root')
    if (exists(data$all)) {
      DBI::dbWriteTable(con, 'historic_data', data$all, append=FALSE, overwrite=TRUE, row.names=FALSE)
    } else {
      DBI::dbWriteTable(con, 'historic_data', data$hist, append=FALSE, overwrite=TRUE, row.names=FALSE)
    }
      DBI::dbDisconnect(con)
})

output$available_dbs <- renderTable({
  df <- data.frame(Tables = data$tables_db)
  df
})

# load .rds file to load into SQL-database
  observeEvent(input$file_db, {
  req(input$file_db)
  data$db <- readRDS(input$file_db$datapath)
  })

# import file into database (save and replace if necessary)
observeEvent(input$save_replace, {
  pgdrv <- dbDriver(drvName = "PostgreSQL")
con <-DBI::dbConnect(pgdrv,
                dbname ="postgres",
                 # host ='172.18.0.3',
                host="db",
                #  port=5432,
                user = 'root',
                password = 'root')
   if (nchar(input$table_name) > 0 & !is.null(data$db)) {
  DBI::dbWriteTable(con, input$table_name, data$db, append=FALSE, overwrite=TRUE, row.names=FALSE)
   } else {
      shinyalert("Try again brudi!", "Either ou forgot to fill in a name for the table, or to load data.", type = "error")
   }
  DBI::dbDisconnect(con)
})

# import file into database (save and append)
observeEvent(input$save_append, {
  pgdrv <- dbDriver(drvName = "PostgreSQL")
con <-DBI::dbConnect(pgdrv,
              dbname ="postgres",
               # host ='172.18.0.3',
              host="db",
              #  port=5432,
              user = 'root',
              password = 'root')
existing_tables <- DBI::dbListTables(con)
  if (input$table_name %in% existing_tables & !is.null(data$db)) {
    DBI::dbWriteTable(con, input$table_name, data$db, append=TRUE, overwrite=FALSE, row.names=FALSE)
  } else {
     shinyalert("Try again brudi!", "Either no table in the PostgreSQL matched your entered, or you forgot to laod data", type = "error")
  }
DBI::dbDisconnect(con)
})

# import file into database (save and replace if necessary)
observeEvent(delete_table$response, {
  if (delete_table$response) {
  pgdrv <- dbDriver(drvName = "PostgreSQL")
con <-DBI::dbConnect(pgdrv,
                     dbname ="postgres",
                      # host ='172.18.0.3',
                      host="db",
                      #  port=5432,
                      user = 'root',
                      password = 'root')
  existing_tables <- DBI::dbListTables(con)
  if (input$table_name %in% existing_tables) {
  DBI::dbRemoveTable(con, input$table_name)
   } else {
      shinyalert("Try again brudi!", "The entered table does not exist!", type = "error")
   }
  DBI::dbDisconnect(con)
  }
})

output$rds <- downloadHandler(
filename = function() {
  paste0('PostgreSQL_export', ".rds")
},
content = function(file) {
  admin_rows <- get_rows(input$admin_rows_selected, input$admin_rows_all)
  saveRDS(data$db[admin_rows, ], file = file)
}
)

}
