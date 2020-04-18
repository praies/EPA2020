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
library(markdown)

# Cargar la data ARS
IngresoMedioARS <- read_csv("Data/ingreso_medio_pesos.csv")
IngresoMedioARS <- as.xts(IngresoMedioARS[,-1], as.Date(IngresoMedioARS$X1, "%d/%m/%Y"))
IngresoMedioARS <- na.locf(IngresoMedioARS)
IngresoMedioARS <- na.locf(IngresoMedioARS, fromLast = T)

# Cargar la data USD
IngresoMedioUSD <- read_csv("Data/ingreso_medio_dolares.csv")
IngresoMedioUSD <- as.xts(IngresoMedioUSD[,-1], as.Date(IngresoMedioUSD$X1, "%d/%m/%Y"))
IngresoMedioUSD <- na.locf(IngresoMedioUSD)
IngresoMedioUSD <- na.locf(IngresoMedioUSD, fromLast = T)

# Cargar la data Norm
IngresoMedioNorm <- read_csv("Data/ingreso_medio_base_100_2006.csv")
IngresoMedioNorm <- as.xts(IngresoMedioNorm[,-1], as.Date(IngresoMedioNorm$Date, "%d/%m/%Y"))
IngresoMedioNorm <- na.locf(IngresoMedioNorm)
IngresoMedioNorm <- na.locf(IngresoMedioNorm, fromLast = T)

############################# API
# Correr el UI
ui <- dashboardPage(
  dashboardHeader(title = "EPH - Ocupados"),
    dashboardSidebar(sidebarMenu(
      menuItem("Ingresos Medios", tabName = "tab", icon = icon("chart-line")),
      # menuItem("Ingresos Medios - Escala Log", tabName = "tab1", icon = icon("chart-line")),
      menuItem("Nota Metodológica", tabName = "tab2", icon = icon("table")),
      menuItem("Descargar Tabla", tabName = "tab4", icon = icon("download"))),
      
      selectInput("variable", "Categoria:",
                c("Pesos Corrientes" = "IM",
                  "Dólares Base 100 = Q3 2006" = "IMN",
                  "Dólares Corrientes" = "IMUSD")),
      pickerInput(inputId = "Aglomerado",
                label = "Selecciona el Aglomerado",
                choices = c(colnames(IngresoMedioARS)),
                selected = "BAHIA BLANCA - CERRI",options = list(`actions-box` = TRUE),multiple = T)),
   
    
  dashboardBody(
    tabItems(
      tabItem(tabName = "tab",
              fluidRow(
                box(dygraphOutput("graph"), width=60),
                box(textOutput("legendDivID"),
                    title = "Legend", collapsible = TRUE, 
                    width=55)
              )
      ),
      # tabItem(tabName = "tab1",
      #         fluidRow(
      #           box(dygraphOutput("graph1"), width=55),
      #           box(textOutput("legendDivID1"),
      #               title = "Legend", collapsible = TRUE, width=55)
      #         )
      # ),
      tabItem(tabName = "tab2",
      fluidRow(
        column(12,
               includeMarkdown("Data/readme.rmd")
        )
      )
    ),
    tabItem(tabName = "tab4",
            fluidRow(
              column(12,
    downloadLink('downloadData', 'Presione Aqui')
    )
    )
   )
  )
 )
)

# Correr el server
server <- function(input, output) {
  library(archivist)
  library(dplyr)
  
  
  
  
  output$graph <- renderDygraph({
    if(input$variable == "IMUSD"){
      IngresoMedio <- IngresoMedioUSD[,c(input$Aglomerado)]
    TITLE = "Ingresos Medios en USD"}
    else if(input$variable == "IMN"){
      TITLE = "Ingresos Medios en USD Normalizados, Base 100 = Q3 2006"
      IngresoMedio <- IngresoMedioNorm[,c(input$Aglomerado)]
      } else {
      TITLE = "Ingresos Medios en Pesos Corrientes"
      IngresoMedio <- IngresoMedioARS[,c(input$Aglomerado)]
    } 
      
    withProgress(message = "Loading...", {
      dygraph(IngresoMedio, main = TITLE) %>% 
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

# Correr la API
shinyApp(ui, server)

# Compara para una fecha, las distintas provicias.
# Mean, std, Min, Max, Percentiles. 

