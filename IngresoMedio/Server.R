library(dygraphs)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(readr)
library(xts)
library(TTR)
library(Hmisc)
library(archivist)
library(devtools)
library(archivist.github)
library(archivist)
library(bit64)

devtools::install_github("pbiecek/archivist")


# Correr el server
server <- function(input, output) {
  library(archivist)
  library(dplyr)
  
  
  
  
  output$graph <- renderDygraph({
    if(input$variable == "IMUSD"){
      IngresoMedio <- IngresoMedioUSD[,c(input$Aglomerado)]
      TITLE = "Ingresos Medios en Dolares"}
    else if(input$variable == "IMN"){
      TITLE = "Ingresos Medios Normalizados"
      IngresoMedio <- IngresoMedioNorm[,c(input$Aglomerado)]
    } else {
      TITLE = "Ingresos Medios"
      IngresoMedio <- IngresoMedioARS[,c(input$Aglomerado)]
    } 
    
    withProgress(message = "Loading...", {
      dygraph(IngresoMedio, main = TITLE, ylab = "Valor") %>% 
        dyRangeSelector() %>%
        dyLegend(labelsDiv = "legendDivID")
    })
  })
  # output$graph1 <- renderDygraph({
  #   
  #   if(input$variable == "IMUSD"){
  #     TITLE = "Ingresos Medios en Dolares"
  #     IngresoMedio <- IngresoMedio/133}
  #   else if(input$variable == "IMN"){
  #     TITLE = "Ingresos Medios Normalizados"
  #     IngresoMedio <- 100 * cumprod(1 + ROC(IngresoMedio, type = "discrete")[-1, ])
  #   } else {
  #     TITLE = "Ingresos Medios"
  #     IngresoMedio <- IngresoMedio
  #     }
  #   
  #   withProgress(message = "Loading...", {
  #     
  #     dygraph(log(IngresoMedio), main = TITLE, ylab = "Valor") %>% 
  #       dyOptions(colors = RColorBrewer::brewer.pal(32, "Set2")) %>%
  #       dyRangeSelector() %>%
  #       dyLegend(labelsDiv = "legendDivID1")
  #   })
  # })
  output$table <- renderDataTable({(IngresoMedio)})
  
  output$downloadData <- downloadHandler(
    filename = function() {
      if(input$variable == "IMUSD"){
        TITLE = "Ingresos Medios en Dolares "}
      else if(input$variable == "IMN"){
        TITLE = "Ingresos Medios Normalizados "
      } else {
        TITLE = "Ingresos Medios "
      } 
      
      paste(TITLE, Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      if(input$variable == "IMUSD"){
        IngresoMedio <- IngresoMedioUSD}
      else if(input$variable == "IMN"){
        IngresoMedio <- IngresoMedioNorm
      } else {
        IngresoMedio <- IngresoMedioARS
      } 
      write.csv(as.data.frame(IngresoMedio), con)
    }
  )
}
