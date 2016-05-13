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
#library(pander)
library(stargazer)
library(nnet)


totalheight <- 500
baseurl <- "https://raw.githubusercontent.com/larsvilhuber/snapshot-availability/master/"
qwischemaurl <- "http://lehd.ces.census.gov/data/schema/"
#industry <- read.csv(text=getURL(paste(baseurl,"qwi_industry_extract.csv",sep = "")),stringsAsFactors = FALSE)
#
industry <- read.csv("qwi_industry_extract.csv",stringsAsFactors = FALSE)
industry$yq <- paste(industry$year,industry$quarter,sep="Q")
industry[is.na(industry$Option),c("Option")]<- "NA"
version <-  read.csv(text=getURL(paste(baseurl,"metadata.csv",sep = "")))
version$variables <- "variables_qwipu.csv"

# TEMPORARY: override schema version
version$schema <- "V4.1d-draft"
version$variables <- "variables_qwi.csv"
# read in labels
namesQWI <-  read.csv(text=getURL(paste(qwischemaurl,version$schema,version$variables,sep = "/")),stringsAsFactors = FALSE)
# this maps short names to verbose names, and subsets to only the right ones
namesData <- as.data.frame(names(industry))
names(namesData)[1] <- "Variable"
choices <- merge(namesData,namesQWI,by.x = "Variable", by.y = "Indicator.Variable",stringsAsFactors = FALSE)


server <- shinyServer(function(input, output) {

  usevar.r <- reactive({
    usevar <- as.character(choices[choices$Indicator.Name==input$usevarName,c("Variable")])
    #print(usevar)
    usevar
  })
  
  industry.r <- reactive({
    if ( input$mergeB == TRUE ) { industry[industry$Option == "NA",c("Option")]<- "B" }
    industry <- subset(industry,yq==input$yq)
    industry    
  })
  
  states.r <- reactive({
    states <- unique(industry.r()[,c("state","Option")])
  })
  
  grpsums.r <- reactive({
    industry <- industry.r()
    grpsums <- aggregate(industry[,usevar.r()], list(Option=industry$Option),
                                FUN=sum, na.rm=TRUE)
    names(grpsums)[2] <- "sumvar"
    grpsums
  })

  grpsums2.r <- reactive({
    industry <- industry.r()
    grpsums2 <- aggregate(industry[,usevar.r()], 
                          list(Option=industry$Option,industry=industry$industry),
                          FUN=sum, na.rm=TRUE)
    grpsums2$yq <- input$yq
    names(grpsums2)[3] <- usevar.r()
    grpsums2 <- merge(grpsums2,grpsums.r())
    grpsums2$pctvar <- 100*grpsums2[,3]/grpsums2$sumvar
    grpsums2[with(grpsums2, order(Option,industry)),c("yq","Option","industry","pctvar") ]
  })
  

  ggindustry.r <- reactive({
    industry <- industry.r()
    gindustry <- merge(industry,grpsums.r())
    gindustry$pctvar <- gindustry[,usevar.r()] / gindustry[,"sumvar"]
    gindustry
  })

  output$test <- renderText({
    ggindustry <- ggindustry.r()
    ggindustry$Option <- relevel(as.factor(ggindustry$Option), ref= "A")
    model <- as.formula(paste("Option ~ ",usevar.r(),sep=""))
    #pander(table(ggindustry$Option))
    #myprobit <- glm(formula = model ,family=binomial(link = "logit"), data=ggindustry)
    #pander(myprobit,caption = paste("Test of MCAR for `",usevar,"` (conditional on mapping `NA` into `B`)",sep=""))
    #pander(myprobit)
    mylogit <- multinom(formula = model ,data=ggindustry)
    stargazer(mylogit,type = "html",title = paste("Multinomial logit model for Option ~",usevar.r(),sep=""),
              intercept.bottom = FALSE,style = "qje")
  })
  
  output$plot <- renderPlot({
    #industry <- industry.r()
    states <- states.r()
    #print(table(states$Option))
    # kludgy way to do labels
    #print(table(industry$Option))
    ggindustry <- ggindustry.r()
    ggindustry[,c("Option")] <-  ifelse(ggindustry$Option == "A",
                                      paste("A (",table(states$Option)[1]," states)",sep = ""),
                                      ggindustry$Option)
    ggindustry[,c("Option")] <-  ifelse(ggindustry$Option == "B",
                                      paste("B (",table(states$Option)[2]," states)",sep = ""),
                                      ggindustry$Option)
    ggindustry[,c("Option")] <-  ifelse(ggindustry$Option == "NA",
                                      paste("NA (",table(states$Option)[3]," states)",sep = ""),
                                      ggindustry$Option)
    #print(table(ggindustry$Option))
        # now plot the whole thing
    gg <-ggplot(data=ggindustry,aes(industry,weight=pctvar,fill=Option)) +geom_bar(position="dodge",alpha=.5) +
      ylab(paste("Distribution of ",usevar.r(),sep="")) +
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


  output$states <- renderDataTable({
    #ggindustry2.r()[,c("state", "Option","industry", "pctvar")]
    states.r()[,c("state","Option")]
  },options = list(searching = FALSE))
  
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

  output$mlogittext <- renderText("Assessing the MCAR assumption:")
  })


# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(

  titlePanel("LEHD Snapshot Comparability"),

  includeMarkdown("header.md"),
#  includeMarkdown("mlogit.md"),
  
  
  fluidRow(
    column(12,
           textOutput("maintext",container = span))
  ),
#  fluidRow(
#    column(12,
#           #           selectInput('usevar', 'Choose a different indicator as appropriate:', names(industry)[18:49],selected = "Emp"),
#           selectInput('usevarName', 'Choose a different indicator as appropriate:', choices$Indicator.Name,
#                       selected = choices$Indicator.Name[1],width = "100%")
#    )
#    ),
  fluidRow(
    column(3,
#           selectInput('usevar', 'Choose a different indicator as appropriate:', names(industry)[18:49],selected = "Emp"),
#           selectInput('usevarName', 'Choose a different indicator as appropriate:', choices$Indicator.Name,
#                       selected = choices$Indicator.Name[1]),
#           selectInput('usevar', 'Choose a different indicator as appropriate:', choices$Variable),
          selectInput('yq', 'Choosing a different time period will also change the available states:', sort(unique(industry[,"yq"])), selected = "2014Q4", selectize=TRUE),
          checkboxInput('showlogit', 'Show MCAR test', FALSE),
          checkboxInput('showtable', 'Show data', FALSE),
           checkboxInput('showstates', 'Show states', FALSE),
           checkboxInput('mergeB', 'Merge B and NA'),
           downloadButton('downloadData', 'Download dataset'),
          downloadButton('downloadMetadata', 'Download metadata')
    ),
# when logit visible
#    conditionalPanel(condition = "input.showlogit == true",
                     column(6,
             selectInput('usevarName', 'Choose a different indicator as appropriate:', choices$Indicator.Name,
                          selected = as.character(choices[choices$Variable=="Emp",c("Indicator.Name")]),
                          width = "100%")
                 ,
             plotOutput('plot')
    ),
#),
# when logit visible
  conditionalPanel(condition = "input.showlogit == true",
                     column(3,
           textOutput("mlogittext",container = span),
           htmlOutput("test")
    ))
# # when logit not visible
  # conditionalPanel(
  # condition = "input.showlogit == false",
  # column(9,
  #        selectInput('usevarName', 'Choose a different indicator as appropriate:', choices$Indicator.Name,
  #                    selected = as.character(choices[choices$Variable=="Emp",c("Indicator.Name")]),
  #                    width = "100%")
  #        ,
  #        plotOutput('plot')
  # ))),
# 
  ),

  fluidRow(
    column(4,conditionalPanel(
      condition = "input.showstates == true",
      dataTableOutput('states'))),
    column(8,conditionalPanel(
    condition = "input.showtable == true",
  dataTableOutput('view')))
  )

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
