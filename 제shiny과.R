library(shiny)
library(ggplot2)

mtcars
mtcars$mpg

ui <- fluidPage(
  titlePanel('regression'),
  sidebarLayout(
    sidebarPanel(
      helpText(),
      selectInput('x', 'Build a regression model of mpg against:',
                  choices = c("",names(mtcars)[-1])),
      actionButton("analysis"," Analysis "),
      hr()
    ),
    mainPanel(
      verbatimTextOutput("text"),
      plotOutput('regPlot')
    )
  )
)

server = function(input, output){
  
  regEquation=reactive({
    options(digits = 2)
    fit <- eval(parse(text=paste0("lm( mpg ~",input$x,",data = mtcars)")))
    b   <- coef(fit)
    equation=paste0("mpg = ",round(b[2],2),input$x," + ",round(b[1],2))
    equation
  })
  output$regPlot <- renderPlot({
    input$analysis
    
    isolate({
      
      ggplot(data=mtcars,aes_string(req(input$x),"mpg"))+
        geom_point()+
        geom_smooth(method="lm")+
        ggtitle(regEquation())
    })
  })
  
  output$text=renderPrint({
    
    input$analysis
    
    isolate({
      options(digits = 2)
      fit <- eval(parse(text=paste0("lm(mpg ~",req(input$x),",data = mtcars)")))
      b   <- coef(fit)
      summary(fit)
    })
  })
}

shinyApp(ui,server)
