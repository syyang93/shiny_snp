library(shiny)
library(shinythemes)

fluidPage(theme = shinytheme("flatly"),
          tags$style(type='text/css', ".selectize-input { font-size: 14; line-height: 14px;} .selectize-dropdown { font-size: 14; line-height: 1.5; }"),
          
          # Application title
          titlePanel("Do hours of studying predict test scores?"),
          # Sidebar 
          sidebarLayout(
            sidebarPanel(
              sliderInput('N', 'Number of observations', min = 0, max = 100, value = 50),
              sliderInput('Beta', 'For every additional "T" allele, how many cm taller is an individual? (Effect estimate)', min = 0, max = 2, value = 1, step = 0.1), # for every hour that you study, how many extra points do you get?
              sliderInput('errorsd', 'On average, how many centimeters will observed heights deviate from expected heights? (Error term)', min = 0, max = 25, value = 10),
              checkboxInput('covariate', div(style = "font-size:15px", "Add teacher as a confounding variable"), value = FALSE),
              uiOutput("teacherSlider"),
              downloadButton("downloadData", "Download simulated data as a csv")
            ),
            # Show the data
            mainPanel(
              plotOutput("showPlot"),
              h5(textOutput("lm")),
              htmlOutput('text')
            )
          )
)
