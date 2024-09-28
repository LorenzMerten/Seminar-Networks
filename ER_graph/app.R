library(shiny)
library(igraph)

#I used this Shiny App as part of a seminar presentation on random graphs.
#This App is meant to roughly replicate Figure 4.1 on page 116 of 
#"RANDOM GRAPHS AND COMPLEX NETWORKS" by Remco van der Hofstad 
#(see https://www.win.tue.nl/~rhofstad/NotesRGCN.pdf).
#The main point is to show the drastic increase in the size of the largest
#connected component in an Erdos-Renyi graph for n*p > 1





er_graph <- function(n,p){
  #Erstellt einen Erdös-Renyi Graphen und gibt die Groesse des groessten
  # Clusters an
  
  graph <- sample_gnp(n,p)
  clust <- clusters(graph)
  
  #Groesstes Cluster finden
  max_clust_id <- which.max(clust$csize)
  clust_id <- clust$membership == max_clust_id
  v_max <- V(graph)[clust_id]
  
  #Markiere die Knoten im groessten Cluster um sie spaeter blau zu markieren
  E(graph)$largest <- F
  E(graph)[.from(v_max)]$largest <- T
  
  return(list(g = graph,max = max(clust$csize)))
  
}


ui <- fluidPage(
  
  titlePanel("ER(n,p)"),
  
  sidebarLayout(
    sidebarPanel(
      textOutput("nxp"),
      numericInput("n","n", value = 10),
      numericInput("p","p", value = 0.05, min = 0, max = 1, step = 0.1),
      conditionalPanel(condition = "input.show_hist",
                       numericInput("hist_length","Anzahl Beobachtungen im Histogramm",
                                    value = 1000)
      ),
      textOutput("largest_cluster"),
      checkboxInput(inputId = "show_hist",
                    label = "Histogramm zeigen?",
                    value = FALSE),
      actionButton("action", "Graph neu generieren")
    ),
    
    #Plotte Graph und (falls gefordert Histogramm)
    mainPanel(
      plotOutput("er_plot"),
      conditionalPanel(condition = "input.show_hist",
                       plotOutput("hist")
      )
    )
  )
)


server <- function(input, output) {
  
  output$nxp <- renderText({paste0("n*p = ",input$n * input$p)})
  
  observeEvent(input$action,{
    er_graph_data <- reactive({
      er_graph(input$n, input$p)
    })
    
    #Graph mit verfuegbaren Daten zeichnen und groesstes Cluster blau markieren
    output$er_plot <- renderPlot({
      g_layout <- make_empty_graph(input$n, directed = F)
      layout <- layout_in_circle(g_layout)
      
      plot(er_graph_data()$g, layout = layout,
           vertex.label = NA,
           vertex.size = 7,
           edge.color = ifelse(E(er_graph_data()$g)$largest, "blue", "lightgrey"))
    })
    
    
    output$largest_cluster <- renderText({paste0("größte Komponente: ", er_graph_data()$max)})
  })
  
  #Erstelle Histogramm der groessten Clustergroessen
  output$hist <- renderPlot({
    hist_data <- numeric(input$hist_length)
    for(i in 1:input$hist_length){
      smpl <- sample_gnp(input$n,input$p)
      hist_data[i] <- max(clusters(smpl)$csize)
    }
    hist(hist_data, breaks = "Sturges", ylab = "Dichte", freq = FALSE,
         main = paste0("Histogramm für ",input$hist_length," Beobachtungen"))
  })
  
}


shinyApp(ui = ui, server = server)
