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

Sys.setlocale("LC_CTYPE", "en_US.UTF-8") # must specify encoding!
Sys.setlocale("LC_ALL", "English")


function(input, output) {
  
  # plot(predictor, outcome)
  # lm(outcome ~ predictor) %>% summary
  df <- reactive ({
    
    # Individuals with disease
    # dis.0 = input$disease_0
    # dis.1 = input$disease_1
    # dis.2 = input$disease_2
    dis.0 = 10
    dis.1 = 50
    dis.2 = 100
    disease = rbind(data.frame(alleles = rep(0, dis.0), disease = rep(1, dis.0)), data.frame(alleles = rep(1, dis.1), disease = rep(1, dis.1)), data.frame(alleles = rep(2, dis.2), disease = rep(1, dis.2)))
    
    # Individuals without disease
    # nodis.0 = input$nodisease_0
    # nodis.1 = input$nodisease_1
    # nodis.2 = input$nodisease_2
    
    nodis.0 = 100 
    nodis.1 = 50
    nodis.2 = 10
    nodisease = rbind(data.frame(alleles = rep(0, nodis.0), disease = rep(0, nodis.0)), data.frame(alleles = rep(1, nodis.1), disease = rep(0, nodis.1)), data.frame(alleles = rep(2, nodis.2), disease = rep(0, nodis.2)))
    
    df = rbind(disease, nodisease)
    df$ID = sample(1:nrow(df))
    df = dplyr::select(df, ID, alleles, disease)
    df = df[order(df$ID),]
  })
  output$downloadData <- downloadHandler(filename ="simulated_data.csv",
                                         content = function(file){
                                           write.csv(print(df()), file, row.names = F)})
  output$lm <- renderText({
    paste0('Estimates from logistic regression:')
  })
  output$text <- renderUI({
    data = df()
    lm.fit = coef(summary(glm(disease ~ alleles, data = data, family = 'binomial')))
    str1 <- paste("Effect estimate (Points gained per hour studied):", formatbeta(lm.fit[2,1]))
    str2 <- paste("Standard error (Reliability of estimate):", formatbeta(lm.fit[2,2]))
    str3 <- paste("P-value (Significance of association):", formatpval(lm.fit[2,4]))
    HTML(paste(str1, str2, str3, sep = '<br/>'))
    
  })
  output$showPlot <- renderPlot({
    data = df()
    p = ggplot(data, aes(alleles, disease)) + stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE) + 
      geom_jitter(width = 0.1, height = 0.1) + 
      xlab('Number of disease alleles') + 
      scale_x_continuous(breaks=c(0, 1, 2)) + 
      scale_y_continuous(breaks=c(0, 1)) + ylab('Disease status')
    
    print(p)
  })
}
