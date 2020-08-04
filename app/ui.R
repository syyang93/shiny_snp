library(shiny)
library(magrittr)
library(shinythemes)
library(tidyverse)

fluidPage(theme = shinytheme("flatly"),
          tags$style(type='text/css', ".selectize-input { font-size: 14; line-height: 14px;} .selectize-dropdown { font-size: 14; line-height: 1.5; }"),
          
          # Application title
          titlePanel("Which two X-men would you like to look at?"),
          # Sidebar 
          sidebarLayout(
            sidebarPanel(
              selectInput("char1", div(style = "font-size:20px", "Character 1:"), 
                          choices = c("Angel = Warren Worthington", "Ariel/Sprite/Shadowcat = Kitty Pryde", 
                                      "Banshee = Sean Cassidy", "Binary/Ms Marvel = Carol Danvers*", 
                                      "Colossus = Peter (Piotr) Rasputin", "Cyclops = Scott Summers", 
                                      "Dazzler = Alison Blaire", "Forge = Name Unknown*", 
                                      "Gambit = Name Unknown", "Havok = Alex Summers", "Jubilee = Jubilation Lee", 
                                      "Longshot = (unknown real name)", "Magneto = Erik Magnus*", "Marvel Girl/Phoenix = Jean Grey", 
                                      "Moira MacTaggert (scientist helper)*", "Mystique = Name Unknown*", 
                                      "Nightcrawler = Kurt Wagner", "Omnipresent narration", "Phoenix(2) = Rachel Summers", 
                                      "Professor X = Charles Xavier (no costume)*", "Psylocke = Elizabeth (Betsy) Braddock", 
                                      "Rogue = Name Unknown", "Storm = Ororo Munroe", "Wolverine = Logan"
                          ), selected = c("Marvel Girl/Phoenix = Jean Grey")
              ),
              selectInput("char2", div(style = "font-size:20px", "Character 2:"), 
                          choices = c("Angel = Warren Worthington", "Ariel/Sprite/Shadowcat = Kitty Pryde", 
                                      "Banshee = Sean Cassidy", "Binary/Ms Marvel = Carol Danvers*", 
                                      "Colossus = Peter (Piotr) Rasputin", "Cyclops = Scott Summers", 
                                      "Dazzler = Alison Blaire", "Forge = Name Unknown*", 
                                      "Gambit = Name Unknown", "Havok = Alex Summers", "Jubilee = Jubilation Lee", 
                                      "Longshot = (unknown real name)", "Magneto = Erik Magnus*", "Marvel Girl/Phoenix = Jean Grey", 
                                      "Moira MacTaggert (scientist helper)*", "Mystique = Name Unknown*", 
                                      "Nightcrawler = Kurt Wagner", "Omnipresent narration", "Phoenix(2) = Rachel Summers", 
                                      "Professor X = Charles Xavier (no costume)*", "Psylocke = Elizabeth (Betsy) Braddock", 
                                      "Rogue = Name Unknown", "Storm = Ororo Munroe", "Wolverine = Logan"
                          ), selected = c("Cyclops = Scott Summers")
              ),
              sliderInput("appearances",
                          div(style = "font-size:20px", "Minimum number of appearances:"),
                          min = 1,
                          max = 100,
                          value = 30),
            checkboxInput('issuelabel', div(style = "font-size:15px", "Show shared issue labels?"), value = FALSE),
            downloadButton("downloadData", "Download shared issues as csv")
          ),
            # Show the drink and picture
            mainPanel(
              plotOutput("showPlot")
            )
          )
)
