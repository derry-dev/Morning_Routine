fluidPage(
  fluidRow(
    column(2),
    column(8,
           wellPanel(
             textOutput("storytext"),
             br(),
             fluidRow(
               column(12, align="center", uiOutput("choicedialog")),
               column(12, align="center", uiOutput("button"))
             )
           )
    ),
    column(2)
  )
)
