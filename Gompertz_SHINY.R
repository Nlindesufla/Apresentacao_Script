## ajuste do modelo não linear de Gompertz no Shiny

library(shiny)
ui<- fluidPage(
  titlePanel(
    span("Exemplo do modelo não linear de Gompertz", style = "color:red")    ),
  helpText("Ajuste do modelo não linear de Gompertz na descrição 
           do crescimento do fruto do Pequi"),
  sidebarPanel(
    sliderInput("A","A",min=80,max=140, step=1,value = 95,
                animate=animationOptions(interval=1000, loop=TRUE)),
    sliderInput("B","B",min=15,max=55,  step=1,value = 30),
    sliderInput("k","k",min=0,max=0.1,      step=0.005,value = 0.02,
                animate=animationOptions(interval=1000, loop=TRUE))
  ),
  mainPanel( 
    plotOutput("grafico"), 
    h2("Resumo de Idade (meses)"),
    verbatimTextOutput("resumoIdade"),
    h2("Massa"),
    verbatimTextOutput("resumoMassa"))
  )

server<- function(input,output){ 
  Gompertz<- function(x,A,B,k){
    y<- A*exp(-exp(k*(B-x)))
    return(y)
  }
  x <- c(1,3,5,7,9,11)
  y <- c(16.08,33.83,65.80,97.20,191.55,326.20)
  
  output$resumoIdade<-  renderPrint({ summary(x) })
  output$resumoMassa<- renderPrint({ summary(y) })
  
  output$grafico<- renderPlot({
    plot(x,y, lwd=2,family="Bookman",cex.main=2,
         xlim = c(0,max(x)),pch=19,  #
         xlab="Idade (meses)",ylab="Massa",
         main="y~A*exp(-exp(k*(B-x)))",col="red")
    curve(Gompertz(x,A=input$A,B=input$B,k=input$k),
          0,max(x), col="blue", lwd=3, add = TRUE)
  })  
}

shinyApp(ui=ui, server=server)
