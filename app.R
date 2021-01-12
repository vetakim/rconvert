library(shiny)

ui <- fluidPage (
                 titlePanel("Калькулятор"),
                 fluidRow(
                          column(3,
                                 numericInput(
                                              inputId="xIn",
                                              label="x",
                                              1000.0,
                                              min = 0,
                                              max = 1e6,
                                              step = 1
                                              ),
                                 numericInput(
                                              inputId="yIn",
                                              label="y",
                                              1000.0,
                                              min = 0,
                                              max = 1e6,
                                              step = 1
                                              ),
                                 numericInput(
                                              inputId="zIn",
                                              label="z",
                                              1000.0,
                                              min = 0,
                                              max = 1e6,
                                              step = 1
                                 )

                                 ),
                          column(3,
                                 textOutput("rOut"),
                                 hr(),
                                 textOutput("aOut"),
                                 hr(),
                                 textOutput("bOut")
                          )

                          ),
                 actionButton(inputId="calculate", label="Рассчитать")
)


server <- function (input, output) {
    coordinates <- eventReactive(input$calculate, {
                                out = data.frame("r" = input$xIn * 2,
                                                 "a" = input$yIn * 3,
                                                 "b" = input$zIn * 4)
                               return(out)
}
    )
    output$rOut <- renderText({
        coords = coordinates()$r
    })
    output$aOut <- renderText({
        coords = coordinates()$a
    })
    output$bOut <- renderText({
        coords = coordinates()$b
    })

}

shinyApp(ui=ui, server=server)
