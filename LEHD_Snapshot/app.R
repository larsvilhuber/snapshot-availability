#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(RCurl)


baseurl <- "https://raw.githubusercontent.com/larsvilhuber/snapshot-availability/master/"
data <- getURL(paste(baseurl,"qwi_industry_extract.csv",sep = ""))
dataset.full <- read.csv(text=data)

server <- shinyServer(function(input, output) {
  
#  industry <- reactive({
#    dataset.full
#  })
  industry <- dataset.full
  
  output$plot <- renderPlot({
    print(input$usevar)
    sumvar <- paste("sum",input$usevar,sep = "")
    grpsums <- aggregate(industry[,input$usevar], list(Option=industry$Option),FUN=sum, na.rm=TRUE)
    names(grpsums)[2] <- sumvar
    gindustry <- merge(industry,grpsums)
    gindustry$pctvar <- gindustry[,input$usevar] / gindustry[,sumvar]
    
    # now plot the whole thing
    gg <-ggplot(data=gindustry,aes(industry,weight=pctvar,fill=Option)) +geom_bar(position="dodge",alpha=.5) +
      ylab(paste("Distribution of ",input$usevar,sep="")) + 
      xlab("NAICS sector") + 
      scale_fill_brewer(type="qual", palette = "Dark2")
    gg <- gg +   theme(axis.line = element_line(colour = "black"),
                       panel.grid.major = element_blank(),
                       panel.grid.major.x = element_line(colour="black"),
                       panel.grid.minor = element_blank(),
                       panel.border = element_blank(),
                       panel.background = element_blank())
    
    print(gg)
    
  }, height=700)
  
}
)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
  
  titlePanel("LEHD Snapshot"),
  
  sidebarPanel(
    
    selectInput('usevar', 'Variable to use', names(industry)[17:48],selected = "Emp"),
    checkboxInput('jitter', 'Jitter'),
    checkboxInput('smooth', 'Smooth')
  ),
  
  mainPanel(
    plotOutput('plot')
  )
))

# Run the application 
shinyApp(ui = ui, server = server)

