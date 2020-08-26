library(shiny)
library(shinythemes)

Sys.setlocale("LC_CTYPE", "en_US.UTF-8") # must specify encoding!
Sys.setlocale("LC_ALL", "English")

fluidPage(theme = shinytheme("flatly"),
          tags$style(type='text/css', ".selectize-input { font-size: 14; line-height: 14px;} .selectize-dropdown { font-size: 14; line-height: 1.5; }"),
          
          # Application title
          titlePanel("Do hours of studying predict test scores?"),
          # Sidebar 
          sidebarLayout(
            sidebarPanel(
              sliderInput('N', 'Number of observations', min = 0, max = 100, value = 50),
              sliderInput('Beta', 'For every hour of studying, how many additional points do you expect to gain? (Effect estimate)', min = 0, max = 2, value = 1, step = 0.1), # for every hour that you study, how many extra points do you get?
              sliderInput('errorsd', 'On average, how many points will observed scores deviate from expected test scores? (Error term)', min = 0, max = 25, value = 10),
              checkboxInput('showlm', div(style = "font-size:15px", "Show estimates from a linear regression"), value = FALSE),
              checkboxInput('show_eq', div(style = "font-size:15px", "Show equation used to generate data"), value = FALSE),
              checkboxInput('covariate', div(style = "font-size:15px", "Add teacher as a confounding variable"), value = FALSE),
              uiOutput("teacherSlider"),
              downloadButton("downloadData", "Download simulated data as a csv")
            ),
            # Show the data
            mainPanel(
              plotOutput("showPlot"),
              htmlOutput('text'),
              htmlOutput('equation')
            )
          )
)
