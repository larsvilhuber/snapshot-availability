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


totalheight <- 500
baseurl <- "https://raw.githubusercontent.com/larsvilhuber/snapshot-availability/master/"
industry <- read.csv(text=getURL(paste(baseurl,"qwi_industry_extract.csv",sep = "")))
version <-  read.csv(text=getURL(paste(baseurl,"metadata.csv",sep = "")))

server <- shinyServer(function(input, output) {

#  industry <- reactive({
#    dataset.full
#  })


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
  }, height=400)

  output$view <- renderTable({
    print(input$usevar)
    sumvar <- paste("sum",input$usevar,sep = "")
    grpsums <- aggregate(industry[,input$usevar], list(Option=industry$Option),FUN=sum, na.rm=TRUE)
    names(grpsums)[2] <- sumvar
    gindustry <- merge(industry,grpsums)
    gindustry$pctvar <- gindustry[,input$usevar] / gindustry[,sumvar]

    grpsums2 <- aggregate(industry[,input$usevar], list(Option=industry$Option,industry=industry$industry),
                          FUN=sum, na.rm=TRUE)
    head(grpsums2)
    names(grpsums2)[3] <- sumvar
    gindustry2 <- merge(industry,grpsums2)
    gindustry2$pctvar <- gindustry2[,input$usevar] / gindustry2[,sumvar]
    gindustry2[,c("industry",sumvar,"pctvar","Option")]
  })

  output$downloadData <- downloadHandler(
    filename = function() {
      paste("qwi_industry_extract", '.csv', sep='')
    },
    content = function(file) {
      write.csv(industry, file)
    }
  )

  output$downloadMetadata <- downloadHandler(
    filename = function() {
      paste("qwi_industry_extract_metadata", '.csv', sep='')
    },
    content = function(file) {
      write.csv(version[,2:4], file)
    }
  )
})

  maintext <- "This is a test."

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(

  titlePanel("LEHD Snapshot Comparability"),

  includeMarkdown("header.md"),

  fluidRow(
    column(12,
           textOutput("maintext",container = span))
  ),

  fluidRow(
    column(4,
           selectInput('usevar', 'Variable to use', names(industry)[17:48],selected = "Emp"),
           #    checkboxInput('showtable', 'Show data', TRUE)
           #    checkboxInput('smooth', 'Smooth')
           downloadButton('downloadData', 'Download dataset'),
          downloadButton('downloadMetadata', 'Download metadata')
    ),
    column(8,
    plotOutput('plot')
    ),

  hr()

  # With the conditionalPanel, the condition is a JavaScript
  # expression. In these expressions, input values like
  # input$n are accessed with dots, as in input.n
#  fluidRow(
#    column(4,
#           downloadButton('downloadData', 'Download dataset')
#           ),
#    column(4,
#           downloadButton('downloadMetadata', 'Download metadata')
#           )
#    )
))
# Run the application
shinyApp(ui = ui, server = server)
