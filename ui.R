
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
  tags$head(tags$script(src = "message-handler.js")),
  
  # Application title
  titlePanel("Driver Behaviour Analysis Demo"),

  img(src='University_of_Luxembourg_SNT2.png', align = "center", width="600",height="200"),
  
  
  #fluidRow(wellPanel(
   # DT::dataTableOutput("table")
   # )
#  ),
  fluidRow(
      column(6,
          fluidRow(
              column(5,
                h1("Reference Driver"),
                h2(textOutput("timeTotal1st"))
              )
          )
      ),
      column(6,
             fluidRow(
               column(5,
                      numericInput("session_id_2nd", label = h1("You"), value = 390, step=1),
                      h2(textOutput("timeTotal2nd"))
               )
             )
      )
  ),

fluidRow(
  column(6,
           plotOutput("distPlot_1st", height = "400px")
         
  ),
  column(6,
           plotOutput("distPlot_2nd", height = "400px")
         
  )
),

fluidRow(
  mainPanel(
    h1("Sector 1"), img(src='sector1.png', align = "center", width="600",height="400"), width ="600",
    heigth = "400")
),

fluidRow(
  column(6,
         h2(textOutput("timeSector1_1st")),
         plotOutput("distPlot_sector1_1st", height = "400px")
         
  ),
  column(6,
         h2(textOutput("timeSector1_2nd")),
         plotOutput("distPlot_sector1_2nd", height = "400px")
         
  )
),



fluidRow(
  column(12,
         fluidRow(
           plotOutput("distPlot_together_sector1", height = "1600px")
         )
  )
),

fluidRow(
  mainPanel(
    h1("Sector 2"), img(src='sector2.png', align = "center", width="600",height="400"), width ="600",
    heigth = "400"),
  column(6,
         fluidRow(
           h2(textOutput("timeSector2_1st")),
           plotOutput("distPlot_sector2_1st", height = "400px")
         )
  ),
  column(6,
         fluidRow(
           h2(textOutput("timeSector2_2nd")),
           plotOutput("distPlot_sector2_2nd", height = "400px")
         )
  )
),

fluidRow(
  column(12,
         fluidRow(
           plotOutput("distPlot_together_sector2", height = "1600px")
         )
  )
),

fluidRow(
  mainPanel(
    h1("Sector 3"), img(src='sector3.png', align = "center", width="600",height="400"), width ="600",
    heigth = "400"),
  column(6,
         fluidRow(
           h2(textOutput("timeSector3_1st")),
           plotOutput("distPlot_sector3_1st", height = "400px")
         )
  ),
  column(6,
         fluidRow(
           h2(textOutput("timeSector3_2nd")),
           plotOutput("distPlot_sector3_2nd", height = "400px")
         )
  )
),

fluidRow(
  column(12,
         fluidRow(
           plotOutput("distPlot_together_sector3", height = "1600px")
         )
  )
),

fluidRow(
  mainPanel(
    h1("General Plots"), width ="600",
    heigth = "400")
),

fluidRow(
  column(6,
         fluidRow(
           plotOutput("generalPlot_1st", height = "1600px")
         )
  ),
  column(6,
         fluidRow(
           plotOutput("generalPlot_2nd", height = "1600px")
         )
  )
),

fluidRow(
  plotOutput("generalPlotTogether", height = "800px")
)

)
)