library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)

polls <- read.csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/polls.csv')
polls <- polls[-c(6,7,8,9)] # delete unused column
polls <- polls[order(polls$title),] # sort the polls$year in ascending order
polls["gender"][polls["gender"] == "male"] <- "Male"  # change the male -> Male in gender column
polls["gender"][polls["gender"] == "female"] <- "Female"
polls["gender"][polls["gender"] == "mixed"] <- "Mixed"
# View(polls)

# new table
d1 <- aggregate(polls$rank,by = list(polls$title),mean)
sub_data <- polls[,c(2,3,4,5)]
d2 <- sub_data[!duplicated(sub_data$title), ]
new_df <- cbind(d2,d1)
new_df <- new_df[-c(5)]
colnames(new_df)[5] <- "rating"

# features for plotting
new_table <- cbind(new_df$title,new_df$rating)
new_table <- as.data.frame(new_table)
colnames(new_table)[1:2] <- c("title","n")
new_table$n <- as.numeric(new_table$n)

ui <- fluidPage(theme = shinytheme("superhero"),
  
  headerPanel('Music Recommender'),
  
  tabsetPanel(
    
    tabPanel(
      "Description",
      titlePanel("Description"), 
      div(includeMarkdown("readme.Rmd"), 
          align="justify")
    ),
    
    tabPanel("Table",
              sidebarPanel(
                tags$label(h3('Input parameters')),
                checkboxGroupInput("gender", label = "Gender:", 
                                   c("Male" = "Male",
                                     "Female" = "Female",
                                     "Mixed" = "Mixed"
                                     )
                ),
                
                sliderInput("year","Year:",1979,2019,2000),
                
                numericInput("obs","Number of observation:",5,min = 1,max = 10),
                
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
        h1("Top 60 Music Rating"),
        plotOutput("plot")
      )
      
    ),
  
  ),
    
  
)

server<- function(input, output) {

  output$plot <- renderPlot({
    new_table %>%
      top_n(60) %>% 
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

  # datasetInput and output$table is not complete
  datasetInput <- reactive({
    #df <- as.data.frame(new_df)
    
    #df <- filter(df, gender=="input$gender")
    #df <- top_n(5)
    #print(df)
    
    df <- new_df[, c("gender", input$gender), drop = FALSE]
    print(df)
  })
  
  
  output$table <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput) 
    }
  })
  
    
}

shinyApp(ui = ui, server = server)
