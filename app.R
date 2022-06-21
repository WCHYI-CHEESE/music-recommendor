#install.packages("shinythemes")
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

ui <- fluidPage(shinythemes::themeSelector(),
                
                headerPanel("Music Recommender"),
                
                tabsetPanel(
                  
                  
                  tabPanel("Recommendation",
                           sidebarPanel(
                             tags$label(h3('What songs do you like?')),
                             selectInput("genre", label = "Genre:",
                                         unique(music$genre)
                                         
                             ),
                             
                             sliderInput("year","Year:",2010,2019,2015),
                             
                             numericInput("obs","Number of songs:",15,min = 1,max = 20),
                             
                             actionButton("submitbutton", "Search",
                                          class = "btn btn-primary")
                             
                             
                           ),
                           mainPanel(
                             
                             tags$label(h3('We recommend')),
                             verbatimTextOutput('contents'),
                             tableOutput("table")
                           )
                           
                  ), #close tab panel of recommendation
                  
                  tabPanel(
                    "Top 50",
                    mainPanel(
                      h1("Top 50 Music Rating"),
                      plotOutput("plot")
                    ) 
                    
                  ), #close tab panel of Top 50
                  
                  tabPanel("Documentation", icon = icon("book-open"),
                           p("This Shiny application will recommend some of the popular songs 
                             to the users depending on their preferences.", align= "justify"),
                           br(),
                           p("Our data set is originally from Kaggle: Top Spotify songs from 2010-2019,
                             which is scraped from Spotify: Organise Your Music. This dataset includes 
                             the Top 50 Spotify music from 2010 to 2019. ", align="justify"),
                           br(),
                           p("After performing data cleaning, this dataset contains 14 columns and 603 
                             rows of data. The type of attribute of this datasets includes:", align = "justify"),
                           p(" - 'title' is categorical."),
                           p(" - 'artist' is categorical."),
                           p(" - 'genre of the track' is categorical. "),
                           p(" - 'year' is quantitative. "),
                           p(" - 'Beats per minute' is quantitative. "),
                           p(" - 'energy' is quantitative. "),
                           p(" - 'Danceability' is quantitative. "),
                           p(" - 'Loudness/dB' is quantitative. "),
                           p(" - 'Liveness' is quantitative."),
                           p(" - 'Valence' is quantitative. "),
                           p(" - 'Length' is quantitative."),
                           p(" - 'Acousticness' is quantitative. "),
                           p(" - 'Speechiness' is quantitative. "),
                           p(" - 'Popularity' is quantitative."),
                           br(),
                           p("The user will need to choose their preferred genre of songs, the year the song was published, 
                             and the total number of songs they wish to be recommended. After they made up their choice, 
                             and quick search we will recommend them songs based on our dataset.", align ="justify"),
                           br(),
                           p("The user can also view the Top 50 songs with the highest music rating on the “Top 50” page.", align ="justify")
                           )
                  
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
