
format_datasets <- function(data) {
  return(data %>% datatable( options = list(searching = T, pageLength=20, lengthMenu = c(10, 20, 30, 50, 100, 200),
                              columnDefs = list(list(className = 'dt-left', targets = '_all')), autoWidth = TRUE, scrollx=TRUE),
             filter = list( position = 'top', clear = TRUE ), fillContainer = FALSE))
}


