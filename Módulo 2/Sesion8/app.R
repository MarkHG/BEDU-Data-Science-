#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
d1011 <- read.csv("match.data.csv")
# Define UI for application that draws a histogram
ui <- shinyUI(
    pageWithSidebar(
        headerPanel("Aplicacion basica con Shiny"),
        sidebarPanel(
            p("Crear plots con el DF 'auto'"), 
            selectInput("x", "Seleccione el valor de X",
                        choices = names(d1011))
        ),
        
        mainPanel(
            
            #Agregando pestañas     # <-----------
            tabsetPanel(              # <-----------
                                      tabPanel("Plots",   #Pestaña de Plots  <-----------
                                               h3(textOutput("output_text")), 
                                               plotOutput("output_plot"),
                                      ),
                                      
                                      tabPanel("imágenes",  #Pestaña de imágenes <-----------
                                               img( src = "1.png", 
                                                    height = 450, width = 450),
                                               
                                               img( src = "2.png", 
                                                    height = 450, width = 450)
                                      ), 
                                      
                                      #Aprovehamos y agregamos las siguientes pestañas # <-----------
                                      tabPanel("Summary", verbatimTextOutput("summary")),    # salida del Summary <-----------
                                      tabPanel("Table", tableOutput("table")),               # salida de la tabla <-----------
                                      tabPanel("Data Table", dataTableOutput("datatable"))   # salida del data table <-----------
            )
        )
    )
    
)
# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$output_text <- renderText(paste("mpg~", input$x))   #Titulo del main Panel
    
    #Gráficas                       <----------
    output$output_plot <- renderPlot( plot( as.formula(paste("mpg ~", input$x)),
                                            data = d1011) )
    
    #imprimiendo el summary       <----------                                  
    output$summary <- renderPrint({
        summary(d1011)
    })
    
    # Agregando el dataframe       <----------
    output$table <- renderTable({ 
        data.frame(d1011)
    })
    
    #Agregando el data table       <----------
    output$data_table <- renderDataTable({d1011}, 
                                         options = list(aLengthMenu = c(5,25,50),
                                                        iDisplayLength = 5))
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
