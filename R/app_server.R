#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  bdello_data <- reactive({

    data <- data.frame(gosp = input$host_name_1,
                       n_gosp_start = input$host_start_1,
                       n_bdello_start = input$bdello_start_1,
                       n_potomnych = input$n_progeny_1,
                       czas_bdello = input$time_progeny_1)
    if(input$host_2){
      data2 <- data.frame(gosp = input$host_name_2,
                          n_gosp_start = input$host_start_2,
                          n_bdello_start = input$bdello_start_2,
                          n_potomnych = input$n_progeny_2,
                          czas_bdello = input$time_progeny_2)


      data <- rbind(data, data2)
    }

    if(input$host_3){
      data3 <- data.frame(gosp = input$host_name_3,
                          n_gosp_start = input$host_start_3,
                          n_bdello_start = input$bdello_start_3,
                          n_potomnych = input$n_progeny_3,
                          czas_bdello = input$time_progeny_3)


      data <- rbind(data, data3)
    }

    return(data)

  })

  bdello_plot <- reactive({

    data <- bdello_data()

    wynik <- oblicz_czas_bdello_wiele(data_bdello = data)

    colnames(wynik[[2]]) <- c('n host', 'n bdellovibrio', 'time', 'host')

    return(wynik)

  })

  output$plot_bdello <- renderPlot({bdello_plot()[[1]]})

  output$bdello_table <- renderTable(({bdello_plot()[[2]]}))
}
