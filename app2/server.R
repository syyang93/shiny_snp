library(shiny)
library(shinythemes)
library(rsconnect)
library(ggplot2)
formatbeta = function(beta){
  formatC(round(beta, 3), 3, format = "f")
}

formatpval = function(pval){
  ifelse(pval > 0.001, format(round(pval, 3), nsmall = 3), formatC(pval, format = "e", digits = 2))
}

mysamp <- function(n, m, s, lwr, upr, nnorm) {
  samp <- rnorm(nnorm, m, s)
  samp <- samp[samp >= lwr & samp <= upr]
  if (length(samp) >= n) {
    return(sample(samp, n))
  }  
  stop(simpleError("Not enough values to sample from. Try increasing nnorm."))
}

Sys.setlocale("LC_CTYPE", "en_US.UTF-8") # must specify encoding!
Sys.setlocale("LC_ALL", "English")

# revenue ~ ad spending
# test score ~ time studying + class 
# salary ~ years of experience + degrees

# N = 1000 # 1000 people
# m = 100
# s = 2
# 
# predictor = rnorm(N, mean = m, sd = s)
# beta = 0.5
# se = 0.01
# intercept = 3
# outcome = intercept + rnorm(N, mean = beta, sd = se) * predictor
# 
# plot(predictor, outcome)
# lm(outcome ~ predictor) %>% summary

# add confounders

# monthly exam, hours = hours studied in one month
# colorblind palette
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

function(input, output) {
  
  # plot(predictor, outcome)
  # lm(outcome ~ predictor) %>% summary
  df <- reactive ({
    N = input$N # 1000 people
    beta = input$Beta
    error <- rnorm(N, 0, 1)
    
    m = 15
    s = 5
    
    # function to restrict hours of studying to be greater than 0
    # taken from here: https://stackoverflow.com/questions/19343133/setting-upper-and-lower-limits-in-rnorm
    predictor = mysamp(N, m, s, 0, 100, nnorm = 100)
    
    intercept = 25 # You'd get 25 points on the test if you did not study at all.
    outcome = intercept + beta * predictor + error * input$errorsd
    df = data.frame(predictor = predictor, outcome = outcome)
  })
  output$downloadData <- downloadHandler(filename ="simulated_data.csv",
                                         content = function(file){
                                           write.csv(print(df()), file, row.names = F)})
  output$lm <- renderText({
    paste0('Estimates from linear regression:')
  })
  output$text <- renderUI({
    data = df()
    lm.fit = coef(summary(lm(data$outcome ~ data$predictor)))
    str1 <- paste("Effect estimate (Points gained per hour studied):", formatbeta(lm.fit[2,1]))
    str2 <- paste("Standard error (Reliability of estimate):", formatbeta(lm.fit[2,2]))
    str3 <- paste("P-value (Significance of association):", formatpval(lm.fit[2,4]))
    HTML(paste(str1, str2, str3, sep = '<br/>'))
    
  })
  output$showPlot <- renderPlot({
    data = df()
    p = ggplot(data, aes(predictor, outcome)) + geom_point() + 
      xlab('Hours of studying per week') + ylab('Test score (points)') + ylim(0, 100) + 
      geom_smooth(method = 'lm') + theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14))
    print(p)
  })
}
