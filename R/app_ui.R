#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import patchwork
#' @import magrittr
#' @noRd
app_ui <- fluidPage(
  theme = shinythemes::shinytheme("united"),

  titlePanel("BdelloSim"),
  fluidRow(
    column(8,
           h6('Binary or non-binary fission? Reproductive mode of predatory bacterium depends on a prey size.'),
           h6('Karolina Pląskowska, Łukasz Makowski, Agnieszka Strzałka, Jolanta Zakrzewska-Czerwińska')
    )
  ),
  #  sidebarLayout(
  #    sidebarPanel(
  fluidRow(
    column(2,
           p('Input starting values'),
           br(),
           textInput('host_name_1', 'Prey name', value = 'Salmonella enterica'),
           sliderInput('host_start_1', 'Input starting number of prey cells [mln]', value = 0.45, min = 0, max = 10, step = 0.05),
           sliderInput('bdello_start_1', 'Input starting number of Bdellovibrio cells', value = 1000, min = 1, max = 1000, step = 1),
           numericInput('n_progeny_1', 'Average number of progeny cells', value = 4, step = 1, min = 2),
           numericInput('time_progeny_1', 'Bdellovibrio life cycle length [min]', value = 290, step = 10),
           h4(strong('Time required for prey complete lysis:')),
           textOutput('res_host_1')

    ),
    column(2,
           checkboxInput('host_2', 'Add prey 2?', value = FALSE),
           conditionalPanel("input.host_2",
                            textInput('host_name_2', 'Prey name', value = 'Proteus mirabilis'),
                            sliderInput('host_start_2', 'Input starting number of prey cells [mln]', value = 0.7, min = 0, max = 10, step = 0.05),
                            sliderInput('bdello_start_2', 'Input starting number of Bdellovibrio cells', value = 1000, min = 1, max = 1000, step = 1),
                            numericInput('n_progeny_2', 'Average number of progeny cells', value = 2, step = 1, min = 2),
                            numericInput('time_progeny_2', 'Bdellovibrio life cycle length [min]', value = 250, step = 10),
                            h4(strong('Time required for prey complete lysis:')),
                            textOutput('res_host_2')
           )

    ),
    column(2,
           checkboxInput('host_3', 'Add prey 3?', value = FALSE),
           conditionalPanel("input.host_3",
                            textInput('host_name_3', 'Prey name', value = 'Shigella flexneri'),
                            sliderInput('host_start_3', 'Input starting number of prey cells [mln]', value = 0.15, min = 0, max = 10, step = 0.05),
                            sliderInput('bdello_start_3', 'Input starting number of Bdellovibrio cells', value = 1000, min = 1, max = 1000, step = 1),
                            numericInput('n_progeny_3', 'Average number of progeny cells', value = 6, step = 1, min = 2),
                            numericInput('time_progeny_3', 'Bdellovibrio life cycle length [min]', value = 380, step = 10),
                            h4(strong('Time required for prey complete lysis:')),
                            textOutput('res_host_3')
           )

    ),


    #mainPanel(
    column(6,
           plotOutput('plot_bdello'),
           tableOutput('bdello_table')

    )
  )

)


#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "BdelloSim"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
