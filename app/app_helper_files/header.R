header <-
  dashboardHeader(title = div(img(src = 'A848_logo', height = "55px"),
                           style = "position: relative; margin: -3px 0px 0px -25px; display:left-align;"),
                           titleWidth=300,
leftUi = tagList(
  appButton(inputId = "hide", label = NULL, icon = icon("eye-slash")),
  bsTooltip(id='hide', 'Click to hide the header', placement = "bottom", trigger = "hover", options = NULL),
  appButton(inputId = "clear_filters", label = 'X filters'),
    bsTooltip(id='clear_filters', 'Clear all filters', placement = "bottom", trigger = "hover", options = NULL),
  appButton(inputId = "clear_selections", label = 'X selection'),
    bsTooltip(id='clear_selections', 'Clear row selection', placement = "bottom", trigger = "hover", options = NULL),
  appButton(inputId = "edit_table", label = NULL, icon = icon("edit")),
    bsTooltip(id='edit_table', 'Edit selection/filtered data', placement = "bottom", trigger = "hover", options = NULL),
  appButton(inputId = "excel", label = NULL, icon = icon("cart-plus")),
    bsTooltip(id='excel', 'Add current analysis to excel tab', placement = "bottom", trigger = "hover", options = NULL),
  downloadButton("xlsx", NULL,block = F, style = "simple", size="lg"),
    bsTooltip(id='xlsx', 'Download xlsx', placement = "bottom", trigger = "hover", options = NULL),
  appButton(inputId = "delete_rows", label = NULL, icon = icon("remove")),
    bsTooltip(id='delete_rows', 'Delete selected rows', placement = "bottom", trigger = "hover", options = NULL),
  appButton(inputId = "transfer", label = NULL, icon = icon("save")),
    bsTooltip(id='transfer', 'Save data', placement = "bottom", trigger = "hover", options = NULL)),

 dropdownMenuOutput("taskMenu"),
 tags$li(verbatimTextOutput('sp_user'), class = "dropdown") #,userOutput("user")
)