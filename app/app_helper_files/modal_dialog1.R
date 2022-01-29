modal_dialog1 <-
  shiny::modalDialog(
    title = "Do you really want to permanently delete the selected rows, this operation can not be undone!",

    size = "s",
    easyClose = TRUE,
    footer = div(
      class = "pull-right container",
      shiny::actionButton(
        inputId = "delete_confirm",
        label = 'Yes',
        icon = shiny::icon("check"),
        class = "btn-success"
      ),
      shiny::actionButton(
        inputId = "cancel",
        label = "Cancel",
        class = "btn-danger"
      )
    )
  )
