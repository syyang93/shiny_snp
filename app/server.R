library(shiny)
library(magrittr)
library(shinythemes)
library(rsconnect)
library(tidyverse)
library(rsconnect)
library(ggplot2)

Sys.setlocale("LC_CTYPE", "en_US.UTF-8") # must specify encoding!
Sys.setlocale("LC_ALL", "English")

# tuesdata <- tidytuesdayR::tt_load('2020-06-30') # don't actually need all this

vis = readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-30/character_visualization.csv')
depict = dplyr::select(vis, issue, costume, character, depicted)

# make dataframe for ggplotting
depict %>% 
  group_by(issue, character) %>% 
  summarise(total_depictions = sum(depicted)) -> new.df
# list all characters: 
# new.df$character %>% unique %>% dput

# make dataframe for counting number of issues
new.df %>% reshape2::dcast(character ~ issue) %>% t() %>% as.data.frame() -> new.df2
colnames(new.df2) = unlist(new.df2[1,])
new.df2 = new.df2[-1,]
new.df2$issue = as.numeric(rownames(new.df2))
new.df2 = as.data.frame(apply(new.df2, 2, as.numeric))

# colorblind palette
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# setwd('~/Documents/Making Things/Tidy.Tuesday/Cocktails/')
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
