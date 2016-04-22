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
industry <- read.csv(text=getURL(paste(baseurl,"qwi_industry_extract.csv",sep = "")),stringsAsFactors = FALSE)
industry[is.na(industry$Option),c("Option")]<- "NA"
version <-  read.csv(text=getURL(paste(baseurl,"metadata.csv",sep = "")))

server <- shinyServer(function(input, output) {

  grpsums.r <- reactive({
    if ( input$mergeB == TRUE ) { industry[industry$Option == "NA",c("Option")]<- "B" }
    grpsums <- aggregate(industry[,input$usevar], list(Option=industry$Option),FUN=sum, na.rm=TRUE)
    names(grpsums)[2] <- "sumvar"
    grpsums
  })

  grpsums2.r <- reactive({
    if ( input$mergeB == TRUE ) { industry[industry$Option == "NA",c("Option")]<- "B" }
    grpsums2 <- aggregate(industry[,input$usevar], list(Option=industry$Option,industry=industry$industry),
                                                 FUN=sum, na.rm=TRUE)
    names(grpsums2)[3] <- input$usevar
    grpsums2 <- merge(grpsums2,grpsums.r())
    grpsums2$pctvar <- 100*grpsums2[,3]/grpsums2$sumvar
    grpsums2
  })
  
  # ggindustry2.r <- reactive({
  # grpsums2 <- aggregate(industry[,input$usevar], list(Option=industry$Option,industry=industry$industry),
  #                       FUN=sum, na.rm=TRUE)
  # head(grpsums2)
  # names(grpsums2)[3] <- sumvar
  # gindustry2 <- merge(industry,grpsums2)
  # gindustry2$pctvar <- gindustry2[,input$usevar] / gindustry2[,sumvar]
  # gindustry2
  # })
  
  ggindustry.r <- reactive({
    if ( input$mergeB == TRUE ) { industry[industry$Option == "NA",c("Option")]<- "B" }
#    sumvar <- paste("sum",input$usevar,sep = "")
#    grpsums <- aggregate(industry[,input$usevar], list(Option=industry$Option),FUN=sum, na.rm=TRUE)
#    names(grpsums)[2] <- sumvar
    gindustry <- merge(industry,grpsums.r())
    gindustry$pctvar <- gindustry[,input$usevar] / gindustry[,"sumvar"]
    gindustry
  })


  output$plot <- renderPlot({

    ggindustry <- ggindustry.r()
        # now plot the whole thing
    gg <-ggplot(data=ggindustry,aes(industry,weight=pctvar,fill=Option)) +geom_bar(position="dodge",alpha=.5) +
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

  output$view <- renderDataTable({
    #ggindustry2.r()[,c("state", "Option","industry", "pctvar")]
    grpsums2.r()
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
           selectInput('usevar', 'Variable to use', names(industry)[18:49],selected = "Emp"),
           checkboxInput('showtable', 'Show data', FALSE),
           checkboxInput('mergeB', 'Merge B and NA'),
           downloadButton('downloadData', 'Download dataset'),
          downloadButton('downloadMetadata', 'Download metadata')
    ),
    column(8,
    plotOutput('plot')
    )
   ),


#  hr(),
  
  fluidRow(conditionalPanel(
    condition = "input.showtable == true",
  dataTableOutput('view')))

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
