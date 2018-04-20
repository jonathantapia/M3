# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

load(url("https://github.com/jonathantapia/EstadisticaModulo3/raw/master/Train.Rdata"))

library(shinythemes)
library(shiny)
library(dplyr)
library(highcharter)
library(ggplot2)
library(DT)
library(Cairo)


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cosmo"),
                
                # Application title
                titlePanel("Modelamiento"),
                
                # Sidebar with a slider input for number of bins 
                sidebarLayout(
                  sidebarPanel(
                    #checkboxGroupInput("variable", "Seleccione las variables:",
                    #                   choices = (names(data))[-c(1:3,4,6,12)], selected = "Outlet_Type")
                    # ,
                    sliderInput("n",
                                "Numero de registros:",
                                min = 1,
                                max = 30,# nrow(data), numero de filas
                                value = 10),
                    selectInput("variable","Seleccione la variable:", choices = (names(data))[-c(1:3,4,6,12)],selected = "Outlet_Type"),
                    tableOutput("tablaResumen"),
                    downloadButton("descarga",label = "descargar")
                  ),
                  
                  # Show a plot of the generated distribution
                  mainPanel(
                    helpText("Modelamiento de ventas vs MRP"),
                    tabsetPanel(type="pills",
                                tabPanel("Tabla_Frecuencias",br(),
                                         dataTableOutput("tabla")
                                ),
                                tabPanel("Tabla_Campos",br(),
                                         #selectInput("variable2","Seleccione la variable:", choices = (names(data))[-c(1:3,4,6,12)],selected = "Outlet_Type"),
                                         checkboxGroupInput("variable2", "Seleccione las variables:",
                                                            choices = (names(data))[-c(1:3,4,6,12)], selected = "Outlet_Type"),
                                         dataTableOutput("tabla2")
                                         
                                ),
                                tabPanel("Grafico",
                                         
                                         highchartOutput ("graf")
                                         
                                ),
                                tabPanel("Grafico_Zoom",
                                         fluidRow(
                                           column(width = 8, class = "well",
                                                  h4("Seleccione una seccion del grafico de la izquierda"),
                                                  fluidRow(
                                                    column(width = 6,
                                                           plotOutput("plot2", height = 450,
                                                                      brush = brushOpts(
                                                                        id = "plot2_brush",
                                                                        resetOnNew = TRUE
                                                                      )
                                                           )
                                                    ),
                                                    column(width = 6,
                                                           plotOutput("plot3", height = 450)
                                                    )
                                                  )
                                           )
                                           
                                         )
                                         
                                )
                    )
                  )
                )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  output$descarga <- downloadHandler(filename = function(){"data.csv"},
                                     content = function(file){write.csv2(data,file)}
  )
  
  #output$tabla <- renderTable({
  # generate bins based on input$bins from ui.R
  #head(data,input$n)
  #head(data[,c("Item_MRP","Item_Outlet_Sales",input$variable)],input$n)
  #})
  output$tabla <- renderDataTable({
    
    datatable(head(data[,c("Item_MRP", "Item_Outlet_Sales",input$variable)],input$n),
              extensions = 'Buttons', options = list(dom = 'Bfrtip',buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
              )
              
    )
  })
  
  output$tabla2 <- renderDataTable({
    
    datatable(head(data[,c("Item_MRP", "Item_Outlet_Sales",input$variable2)],input$n),
              extensions = 'Buttons', options = list(dom = 'Bfrtip',buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
              )
              
    )
  })
  
  output$tablaResumen <- renderTable({
    # group_by(data,input$variable)%>% summarize(Numero=n())
    table(data[,c(input$variable)])
  })
  
  output$graf <- renderHighchart({
    # group_by(data,input$variable)%>% summarize(Numero=n())
    hchart(data[,c(input$variable)], colorByPoint=TRUE,name="variable")
    
  })
  
  
  ranges2 <- reactiveValues(x = NULL, y = NULL)
  
  output$plot2 <- renderPlot({
    ggplot(data, aes(Item_MRP, Item_Outlet_Sales)) +
      geom_point()
  })
  
  output$plot3 <- renderPlot({
    ggplot(data, aes(Item_MRP, Item_Outlet_Sales)) +
      geom_point() +
      coord_cartesian(xlim = ranges2$x, ylim = ranges2$y, expand = FALSE)
  })
  
  
  observe({
    brush <- input$plot2_brush
    if (!is.null(brush)) {
      ranges2$x <- c(brush$xmin, brush$xmax)
      ranges2$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges2$x <- NULL
      ranges2$y <- NULL
    }
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

