#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)

# Define UI for application 
ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("Movie Review and Recommendation"),
   
   # Sidebar with  text input and submit buttons 
   sidebarLayout(
      sidebarPanel(
        helpText("Show the rating of movie and 
            recommended movie for specific user"),
        
        numericInput('vec1', 'Enter Movie ID', 000),
      submitButton("Show rating"),
      
      numericInput('vec2', 'Enter User ID', 000),
      submitButton("Show Recommndations")
   ),
      
      # Show output of review, comments and recommendations
      mainPanel(
        h4("Rating"),
        verbatimTextOutput("Rating"),
        
       tableOutput("Comments"),
        
        tableOutput("Recommendations")
        
      )
   )
))

#read files to be used
rating<- read.csv('C:/Users/user/Desktop/college/Movie Project/data files/Final_Review.csv')
comments<- read.csv('C:/Users/user/Desktop/college/Movie Project/data files/f_comments.csv')
recommendation=read.csv('C:/Users/user/Desktop/college/Movie Project/ml-latest-small/recommendations.csv')

### define server logic
server <- shinyServer(function(input, output,session) {
   
    output$Rating <- renderPrint({
    x=rating[rating$Movie_Id==input$vec1,4]
    print(x)
  })
  
  output$Comments <- renderTable({
    y=subset(comments, comments$Movie_ID==input$vec1,select='Review')
    print.data.frame(y,row.names = F)
  })
  
  output$Recommendations <- renderTable({
    z=subset(recommendation, recommendation$user_Id==input$vec2)
    z=transpose(z[,-1])
    colnames(z)="Recommended Movies"
    print.data.frame(z,row.names = F)
  })
  
})

# Run the application 
shinyApp(ui = ui, server = server)

