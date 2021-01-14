library(shiny)
source(file='xyz2mssk.R')
source(file='tool.R')
source(file='mssk2xyz.R')


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
                                 selectInput(
                                             inputId = "inChoice",
                                             label="СК",
                                             choices= list("XYZ" = "xyz", "RAB" = "rab")
                                             ),
                                 numericInput( inputId="xIn", label="x", 1000.0, min = 0, max = 1e6, step = 1),
                                 numericInput( inputId="yIn", label="y", 1000.0, min = 0, max = 1e6, step = 1),
                                 numericInput( inputId="zIn", label="z", 1000.0, min = 0, max = 1e6, step = 1)

                                 ),
                          column(4,
                                 selectInput(
                                             inputId = "outChoice",
                                             label="СК",
                                             choices= list("RAB" = "rab", "XYZ" = "xyz")
                                             ),
                                 hr(),
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
                                     incoord = matrix(c(input$xIn, input$yIn, input$zIn), nrow=3)

                                     if ( input$inChoice == "xyz" ) {
                                         if ( input$outChoice == "rab" ) {
                                            outlist <- xyz2mssk(incoord, 0, SC)
                                         }
                                         if ( input$outChoice == "xyz" ) {
                                             outlist <- incoord
                                         }
                                     }

                                     if ( input$inChoice == "rab" ) {
                                         if ( input$outChoice == "xyz" ) {
                                           incoord[2] = deg2rad(incoord[2])
                                           incoord[3] = deg2rad(incoord[3])
                                             outlist <- mssk2xyz(incoord, 0, SC)
                                         }
                                         if ( input$outChoice == "rab" ) {
                                             outlist <- incoord
                                         }
                                     }
                                     out = data.frame(xOut=outlist[1], yOut=outlist[2], zOut=outlist[3])
                                     return(out)
}
    )
    name = ""
    unit = ""
    output$rOut <- renderText({
        coords = coordinates()$xOut
        if ( input$outChoice == "rab" ) {
            name = "R"
            unit = "км"
        }

        if ( input$outChoice == "xyz" ) {
            name = "x"
            unit = "км"
        }
        paste(name, " = ", coords, " ",  unit)
    })
    output$aOut <- renderText({
        coords = coordinates()$yOut

        if ( input$outChoice == "rab" ) {
            coords = rad2deg(coords)
            name = "alpha"
            unit = "град"
        }

        if ( input$outChoice == "xyz" ) {
            name = "y"
            unit = "км"
        }
        paste(name, " = ", coords, " ",  unit)
    })

    output$bOut <- renderText({

        coords = coordinates()$zOut

        if ( input$outChoice == "rab" ) {
            coords = rad2deg(coords)
            name = "beta"
            unit = "град"
        }

        if ( input$outChoice == "xyz" ) {
            name = "z"
            unit = "км"
        }
        paste(name, " = ", coords, " ",  unit)
    })

}

shinyApp(ui=ui, server=server)
