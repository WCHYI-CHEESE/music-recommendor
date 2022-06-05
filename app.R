library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)

music <- read.csv("https://gist.githubusercontent.com/rioto9858/ff72b72b3bf5754d29dd1ebf898fc893/raw/1164a139a780b0826faef36c865da65f2d3573e0/top50MusicFrom2010-2019.csv")
music <- music[-c(5,6,7,8,9,10,11,12,13)] # delete unused column
colnames(music)[3] <- "genre"
colnames(music)[5] <- "rating"
music <- music %>% distinct(title, .keep_all = TRUE)
music <- arrange(music, desc(rating))
# View(music)


# features for bar chart
new_table <- music[-c(2,3,4)]
new_table <- as.data.frame(new_table)
colnames(new_table)[1:2] <- c("title","n")
new_table$n <- as.numeric(new_table$n)

ui <- fluidPage(theme = shinytheme("superhero"),
                
                headerPanel('Music Recommender'),
                
                tabsetPanel(
                  
                  tabPanel(
                    "Description",
                    div(includeMarkdown("readme.Rmd"), 
                        align="justify")
                  ),
                  
                  tabPanel("Table",
                           sidebarPanel(
                             tags$label(h3('Input parameters')),
                             selectInput("genre", label = "Genre:", 
                                          unique(music$genre)
                                          
                             ),
                             
                             sliderInput("year","Year:",2010,2019,2015),
                             
                             numericInput("obs","Number of observation:",15,min = 1,max = 20),
                             
                             actionButton("submitbutton", "Submit", 
                                          class = "btn btn-primary")
                             
                             
                           ),
                           mainPanel(
                             
                             tags$label(h3('Output')),
                             verbatimTextOutput('contents'),
                             tableOutput("table")
                           )
                           
                  ),
                  
                  tabPanel(
                    "Rating",
                    mainPanel(
                      h1("Top 50 Music Rating"),
                      plotOutput("plot")
                    )
                    
                  ),
                  
                ),
                
                
)

server<- function(input, output) {
  
  output$plot <- renderPlot({
    new_table %>%
      top_n(50) %>% 
      ggplot(aes(x = reorder(title, n), y = n, fill = title)) + 
      geom_col() + 
      coord_flip() + 
      xlab("") + 
      ylab("Rating") + 
      theme(legend.position = "none")
  }
  
  )
  
  
  
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Recommended songs:") 
    } else {
      return("Server is ready.")
    }
  })
  
  
  datasetInput <- reactive ({
    rating_table <- filter(music, genre==input$genre)
    
    rating_table <- filter(rating_table, year == input$year)
    
    rating_table <- head(rating_table, n=input$obs)
    
    print(rating_table)
    
  })
  
  
  output$table <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    }
  })
  
  
}


shinyApp(ui = ui, server = server)
