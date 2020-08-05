library(shiny)
library(magrittr)
library(shinythemes)
library(rsconnect)
library(tidyverse)
library(rsconnect)
library(ggplot2)

Sys.setlocale("LC_CTYPE", "en_US.UTF-8") # must specify encoding!
Sys.setlocale("LC_ALL", "English")

# Making up data

a2 = 0.32
a1 = 1-a2

# generate N people with A1 and A2 assuming HWE
n = 1000
hom_a1 = floor(a1^2 * n)
hom_a2 = floor(a2^2 * n)
het = n - hom_a1 - hom_a2

# Height distribution 
avg.height = 900
sd.height = 30

# Sex/Age effects
avg.male = 100 # (males are 100 cm taller, on average)
avg.height = 5 # (for each additional year of age, people are on average 5cm taller)

# make dataframe
heights = rnorm(n, mean = avg.height, sd = sd.height)

data.frame(height = heights, age = age, sex = sex)
heights[1:nrow(hom_a1)]


# Get data: Palmer penguins
library(palmerpenguins)
penguins

ggplot(penguins, aes(body_mass_g, bill_length_mm, col = species)) + geom_point()

# colorblind palette
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

function(input, output) {
  output$downloadData <- downloadHandler(filename ="Issues_both_characters.csv",
    content = function(file){
      write.csv(print(new.df2 %>% dplyr::select(input$char1, input$char2, issue) %>% filter((!!as.name(input$char1)) >= input$appearances) %>% filter((!!as.name(input$char2)) >= input$appearances) ), file, row.names = F)})
  output$showPlot <- renderPlot({
    # subsets
    toplot = subset(new.df, character %in% c(input$char1, input$char2))
    shared.issues = new.df2 %>% dplyr::select(input$char1, input$char2, issue) %>% filter((!!as.name(input$char1)) >= input$appearances) %>% filter((!!as.name(input$char2)) >= input$appearances) # need to pass a string: https://stackoverflow.com/questions/48219732/pass-a-string-as-variable-name-in-dplyrfilter
    
    if(!input$issuelabel){
      ggplot(toplot, aes(issue, total_depictions, col = character)) + geom_line() + 
        geom_hline(yintercept = input$appearances, col = 'red') + 
        ggtitle(paste(nrow(shared.issues), 'issues with more than', input$appearances, 'appearances for both characters')) + 
        theme_classic() + theme(text = element_text(size = 15)) + xlab('Issue number') + ylab('Number of depictions') +
        labs(col  = "Characters") + scale_colour_manual(values=cbp1)
      
    } else{
      ggplot(toplot, aes(issue, total_depictions, col = character)) + geom_line() + 
        geom_hline(yintercept = input$appearances, col = 'red') + 
        ggtitle(paste(nrow(shared.issues), 'issues with more than', input$appearances, 'appearances for both characters')) + 
        theme_classic() + theme(text = element_text(size = 15)) + xlab('Issue number') + ylab('Number of depictions') + 
        geom_text(data = shared.issues, aes(issue, y = 60, label = paste('issue', issue), vjust = -0.5), angle = 90, col = 'black') + geom_vline(xintercept = shared.issues$issue, lty = 2, colour = "gray50")  +
        labs(col = "Characters") + scale_colour_manual(values=cbp1)
    }
  })
}
