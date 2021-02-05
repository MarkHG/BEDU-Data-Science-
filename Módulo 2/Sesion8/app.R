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
home.score <- d1011$home.score
#home.score <- na.omit(home.score)
away.score <- d1011$away.score
#away.score <- na.omit(away.score)
new.table <- cbind(home.score,away.score)
z <- setNames(new.table, c("home.score","away.score"))
#z <- na.omit(home.score, away.score)



# Define UI for application that draws a histogram

ui <- shinyUI(
    pageWithSidebar(
        headerPanel("Post Work 8 con Shiny"),
        sidebarPanel(
            p("Crear plots con el DF 'auto'"), 
            selectInput("x", "Seleccione el valor de X",
                        choices = names(z))
        ),
        
        mainPanel(
            
            #Agregando pestañas     # <-----------
            tabsetPanel(              # <-----------
                                      tabPanel("Plots",   #Pestaña de Plots  <-----------
                                               h3(textOutput("output_text")), 
                                               plotOutput("output_plot"),
                                      ),
                                      
                                      tabPanel("imágenes",  #Pestaña de imágenes <-----------
                                               
                                               img( src = "0.png", 
                                                    height = 450, width = 450),
                                               
                                               img( src = "01.png", 
                                                    height = 450, width = 450),
                                               
                                               img( src = "02.png", 
                                                    height = 450, width = 450),
                                               
                                               
                                               
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
    mtcars
    d1011
    home.score <- d1011$home.score
    away.score <- d1011$away.score
    new.table <- cbind(home.score,away.score)
    
    output$output_text <- renderText(paste("home.socore~", input$x))   #Titulo del main Panel
    
    #Gráficas                       <----------
    output$output_plot <- renderPlot( plot( as.formula(paste("home.score ~", input$x)),
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
