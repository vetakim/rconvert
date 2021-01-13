library(shiny)
source(file='xyz2mssk.R')
source(file='tool.R')


ui <- fluidPage (
                 titlePanel("Калькулятор"),
                 fluidRow(
                          column(1, numericInput( inputId="bGrad", label="Широта, град", 0, min = 0, max = 360, step = 1, width="80px")),
                          column(1, numericInput( inputId="bMin", label="Широта, мин", 0, min = 0, max = 360, step = 1, width="80px")),
                          column(1, numericInput( inputId="bSec", label="Широта, сек", 0, min = 0, max = 360, step = 1, width="80px"))
                          ),
                 fluidRow(
                          column(1, numericInput( inputId="lGrad", label="Долгота, град", 0, min = 0, max = 360, step = 1, width="80px")),
                          column(1, numericInput( inputId="lMin", label="Долгота, мин", 0, min = 0, max = 360, step = 1, width="80px")),
                          column(1, numericInput( inputId="lSec", label="Долгота, сек", 0, min = 0, max = 360, step = 1, width="80px"))
                          ),
                 fluidRow(
                          column(1, numericInput( inputId="height", label="Высота, м", 0, min = 0, max = 1e6, step = 1, width="80px"))
                          ),

                 fluidRow(
                          column(3,
                                 numericInput( inputId="xIn", label="x", 1000.0, min = 0, max = 1e6, step = 1),
                                 numericInput( inputId="yIn", label="y", 1000.0, min = 0, max = 1e6, step = 1),
                                 numericInput( inputId="zIn", label="z", 1000.0, min = 0, max = 1e6, step = 1)

                                 ),
                          column(4,
                                 textOutput("rOut"),
                                 hr(),
                                 textOutput("aOut"),
                                 hr(),
                                 textOutput("bOut"),
                                 hr()
                          )

                          ),
                 actionButton(inputId="calculate", label="Рассчитать")
)


server <- function (input, output) {
    coordinates <- eventReactive(input$calculate, {
                                     latitude <- deg2rad(unionAngleGrad(input$bGrad, input$bMin, input$bSec))
                                     longitude <- deg2rad(unionAngleGrad(input$lGrad, input$lMin, input$lSec))
                                     SC = data.frame( B = latitude, L = longitude, H = input$height * 1e-3)
                                     xyz = matrix(c(input$xIn, input$yIn, input$zIn), nrow=3)
                                     rab <- xyz2mssk(xyz, 0, SC)
                                     out = data.frame(r=rab[1], a=rab[2], b=rab[3])
                                     return(out)
}
    )
    output$rOut <- renderText({
        coords = coordinates()$r
        paste("R = ", coords, " км")
    })
    output$aOut <- renderText({
        coords = rad2deg(coordinates()$a)
        paste("alpha = ", coords, "град.")
    })
    output$bOut <- renderText({
        coords = rad2deg(coordinates()$b)
        paste("beta = ", coords, "град.")
    })

}

shinyApp(ui=ui, server=server)
