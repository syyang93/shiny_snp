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


# monthly exam, hours = hours studied in one month
# colorblind palette
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

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
    glm(disease ~ alleles, data = df, family = 'binomial') %>% summary
    
    # plot
    ggplot(df, aes(alleles, disease)) + stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE) + geom_jitter(width = 0.1, height = 0.1) + xlab('Number of disease alleles')
     
        
        beta = input$Beta
    error <- rnorm(N, 0, 1)
    
    # Values from here: https://tasks.illustrativemathematics.org/content-standards/HSS/ID/A/4/tasks/1020#:~:text=The%20heights%20of%20adult%20men,standard%20deviation%20of%202.5%20inches.
    m = 70 # What's the average height (for males) in inches
    s = 3 # What's the standard distribution for height
    
    # generate heights
    outcome = rnorm(N, m, s)
    
    intercept = 25 # You'd get 25 points on the test if you did not study at all.
    outcome = intercept + beta * predictor + error * input$errorsd
    df.first = data.frame(student_id = 1:length(predictor), predictor = predictor, outcome = outcome)
    if(input$covariate == TRUE){
      classA.sample = sample(nrow(df.first), nrow(df.first)/2)
      df.first$Teacher = 'Placeholder'
      df.first$Teacher[classA.sample] = 'Mr. A'
      df.first$Teacher[-classA.sample] = 'Mr. B'
      df.first$outcome[classA.sample] = df.first$outcome[classA.sample] + input$teacher_effect
    } 
    df = df.first
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
  output$teacherSlider = renderUI({
    if (input$covariate == FALSE) {
      return(NULL)
    }
    
    if (input$covariate == TRUE) {
      list(
        sliderInput(
          "teacher_effect",
          "Mr. A's class, on average, gets this many points more than Mr. B's class",
          min = 0,
          max = 40,
          value = 20
        ),
        checkboxInput('color_cov', div(style = "font-size:15px", "Color by teacher"), value = FALSE)
      )
    }
  })
  output$showPlot <- renderPlot({
    data = df()
    p = ggplot(data, aes(predictor, outcome)) + geom_point() + 
      xlab('Hours of studying per week') + ylab('Test score (points)') + ylim(0, 100) + 
      geom_smooth(method = 'lm', se = FALSE) + theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14))
    if(input$covariate == TRUE) { 
      if(input$color_cov == TRUE){
        p = ggplot(data, aes(predictor, outcome, col = Teacher)) + geom_point() + 
          xlab('Hours of studying per week') + ylab('Test score (points)') + ylim(0, 100) + 
          geom_smooth(method = 'lm', se = FALSE) + theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14), legend.position = 'none')
      }
    }
    print(p)
  })
}
