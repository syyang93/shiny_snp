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

# Deploy App
# rsconnect::setAccountInfo(name='syyang93', token='D5215863487CE5402209BD40C0749F16', secret=secret)
# setwd('~/Documents/Work/shiny_snp/linear_regression_example/')
# deployApp()

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
    predictor = mysamp(N, m, s, 0, 100, nnorm = 600)
    
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
  
  output$text <- renderUI({
    if (input$showlm == FALSE) {
      return(NULL)
    }
    if (input$showlm == TRUE) {
      data = df()
      lm.fit = coef(summary(lm(data$outcome ~ data$predictor)))
      str = paste0('Estimates from linear regression:')
      str0 <- paste("Y-intercept (b):", formatbeta(lm.fit[1,1]))
      str1 <- paste("Slope (m):", formatbeta(lm.fit[2,1]))
      str3 <- paste("P-value (Significance of association):", formatpval(lm.fit[2,4]))
      HTML(paste(str, str0, str1, str3, sep = '<br/>'))
    }
  })
  output$equation <- renderUI({
    if (input$show_eq == FALSE) {
      return(NULL)
    }
    if (input$show_eq == TRUE) {
    data = df()
    intercept = 25
    lm.fit = coef(summary(lm(data$outcome ~ data$predictor)))
    str1 <- paste("<br/>Actual equation used to generate data:")
    if(input$covariate == FALSE){
    str2 <- "y = (m * x) + b"
    str3 <- paste0("Test score = (", input$Beta, " * hours studied)  + ", 25)
    
    } else{
      str2 <- 'y = (m1 * x1) + (m2 + x2) + b'
      str3 <- paste0("Test score = (", input$Beta, " * hours studied)  + (", input$teacher_effect, " * teacher) + ", 25)
    }
    HTML(paste(str1, str2, str3, sep = '<br/>'))
    }
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
      geom_smooth(method = 'lm', se = FALSE) + theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14), legend.position = 'none') + scale_color_manual(values = cbp1)
      }
    }
    print(p)
  })
}
