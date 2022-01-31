modal_dialog <-
  shiny::modalDialog(
    title = "Edit the Table to your needs",
    div(
      class = "text-center",
      div(
     excelOutput("table"),
    )),
    size = "l",
    easyClose = TRUE,
    footer = div(
      class = "pull-right container",
      shiny::actionButton(
        inputId = "final_edit",
        label = 'Save',
        # style="color: #fff; background-color: #4CB00BBA; border-color: #2e6da4",
        style="color: #010101FF; background-color: #4CB00BBA; border-color: #2e6da4",
        icon = shiny::icon("save"),
        class = "btn-success"
      ),
      shiny::actionButton(
        inputId = "dismiss_modal",
        style="color: #fff; background-color: #C7C3C3FF; border-color: #2e6da4",
        icon = shiny::icon('close'),
        label = "Close",
        class = "btn-danger"
      )
    )
  )


