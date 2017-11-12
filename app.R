rm(list=ls())
base_to_tableau <- function(base){
  
  data.frame(temps = base) %>%
    mutate(A = temps - temps[1],
           B = lag(A),
           duree = A - B) %>%
    select(-A, -B) %>% 
    .[-1,] %>%
    mutate(etape = 
             rep(c("contraction","repos"),times=ceiling(nrow(.)/2))[seq_len(nrow(.))],
           etape= forcats::as_factor(etape),
           etape = forcats::lvls_expand(etape,c("contraction","repos")),
           etape = forcats::lvls_reorder(etape,c(2,1)),
           session = rep(seq_len(ceiling(nrow(.)/2)),each=2)[seq_len(nrow(.))],
           session = forcats::as_factor(as.character(session))
           
    )
}

genere_graph <- function(base){
  if (length(base) == 1) {return(
    
    ggplot()+annotate("text", x = 4, y = 25, label = "Lancer le compteur")+
      ggthemes::theme_solid(base_family = "white")
    
  )}
  base %>% base_to_tableau() %>% 
    ggplot(aes(x=session, y=duree, fill=etape)) +
    geom_bar(stat="identity") +
    scale_fill_manual(values = c("repos" = "lightblue", "contraction" = "red"
    ),name = " ") + theme_minimal() + expand_limits(y=30,x=3)
  
}

# genere_graph(base)+expand_limits(x=3)

library(shiny)
library(ggplot2)
library(dplyr)
library(tibble)
library(ggthemes)
library(forcats)

ui <- fluidPage(
  titlePanel("Suivi de contraction"),
  
  sidebarLayout(
    sidebarPanel(
      # actionButton("go","go")
      h2(textOutput("compteur"))
    ),
    mainPanel(
      div(id="test",
      plotOutput("dessin", click = "plot_click"),
      h4("suffit de cliquer sur le dessin a chaque debut ou fin de contraction")
    )
    )
  )
)

server <- function(input, output,session) {
  base<-c()

  observeEvent(input$plot_click,{
    base <<- c(base,Sys.time())
  })
  
  output$dessin <- renderPlot({
     invalidateLater(1000, session)
    genere_graph(c(base,Sys.time()))
    
  })
  
  output$compteur <- renderText({
     invalidateLater(1000, session)
    # browser()
    if(is.null(base)){return(NULL)}
  # duree <- round(Sys.time()-  as.POSIXct(base[length(base)],origin = "1970-01-01"))
  duree <- difftime(units = "secs",Sys.time(),  as.POSIXct(base[length(base)],origin = "1970-01-01")) %>% round()
  
  paste(ifelse(length(base)%%2 == 0,"repos : ","contraction : "),duree," s")
  
  })
}

shinyApp(ui = ui, server = server)
