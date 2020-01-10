library(shiny)

library(shiny)
library(shinythemes)
shinyUI(fluidPage(theme=shinytheme("slate"),
                  headerPanel(h1(em(
                      "RASTGELE SUREKLI OLASILIK DAGILIMLARI")))
                  ,
                  fluidRow(
                      column(4,
                             wellPanel(
                                 radioButtons("dist",em("SUREKLI DAGILIMLAR:"),
                                              list("Normal Dagilim"="norm","Duzgun Dagilim"="unif","t Dagilimi"="t","F Dagilimi"="F","Gamma Dagilimi"="gam","Ustel Dagilim"="exp","Ki-Kare Dagilimi"="chisq","Log-normal Dagilim"="lnorm","Beta Dagilimi"="beta","Cauchy Dagilimi"="cauchy","Weibull Dagilimi"="weibull")),
                                 sliderInput("n",em("Orneklem Buyuklugu:"),1,1000,30),
                                 uiOutput("dist1"),
                                 uiOutput("dist2"),
                                 checkboxInput("density","Frekans (Yogunluk) Egrisini Goster",FALSE),
                                 conditionalPanel(
                                     condition="input.density==true",
                                     numericInput("bw","Bant Genisligi:",1)
                                 ),
                                 downloadButton("dldat", em("Orneklemi Indir"), class="btn btn-warning")
                             )
                      ),
                      column(8,
                             tabsetPanel(
                                 tabPanel("Histogram",plotOutput("plot",height="600px")),
                                 tabPanel("BoxPlot",plotOutput("boxplot",height="600px")),
                                 tabPanel("ViolinPlot",plotOutput("vioplot",height="600px")),
                                 tabPanel("Summary",verbatimTextOutput("summary")),
                                 tabPanel("Table",tableOutput("table"))
                                 
                             )
                      )
                  )
))
