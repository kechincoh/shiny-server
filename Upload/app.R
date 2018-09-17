#note: the extension buttons only work on browser not on app in Rstudio

#load required libraries
library(shiny)                      #libray to use shiny app
library(DT)                         #library to create tables
library(ggplot2)                    #library for plotting
library(reshape2)                   #library for data manipulation
library(stringr)                    #library for string manipulation
library(dplyr)                      #library to rename columns
library(lubridate)                  #library to manipulate date 

#source helper functions
source('./R/get_functions.R')
get_functions()

#choices for protocol
protocol = c(13272,13277,13351,13384,13401,13447,14036,14108,15342)

# Define UI for application that creating table
ui <- fluidPage(
 
    fluidRow(
      column(2,
           selectInput("template", "Choose a template:",
                         choices = c("",unique(as.character(protocol)))),  
           fileInput('datafile', 'Choose CSV file',
                     multiple = TRUE,
                     accept=c('text/csv','text/comma-separated-values,text/plain',".csv")),
          
           # Horizontal line ----
           tags$hr()
            ),
      column(10,
           tabsetPanel(id="tabs2",
                          tabPanel("Monitoring Report", DT::dataTableOutput("moni2"),
                                   # Button
                                   downloadButton("downMoni2", "Download")),
                          tabPanel("Death within 30 days", DT::dataTableOutput("death2"),
                                   #Butoon
                                   downloadButton("downdeath2", "Download")),
                          tabPanel("Demographic Report", DT::dataTableOutput("demo2"),
                                   fluidRow(
                                     column(5,
                                            plotOutput("age_bar2") ),
                                     column(6,
                                            plotOutput("eth_bar2")),
                                     column(5,
                                            plotOutput("gender_bar2")),
                                     column(5,
                                            plotOutput("race_bar2"))
                          ))))
                          
           )
    )
    
# Define server logic required to create table
server <- function(input, output) {
  
  
  ########################### upload ##################################
  df <- reactive({
    req(input$datafile)
    
    #read all csv files uploaded
    files= lapply(input$datafile$datapath, read.csv,header=TRUE,stringsAsFactors=F,na.string="")
    
    #add names of element's list to respective supplied CSV files' names
    names(files)<-input$datafile$name
    
    #filter all elements of list by Active==1
    files_active = lapply(files,function(x) x[which(x$RecordActive==1),])

    files_active
  })
  
  #Reactive value for selected template for monitor report
  templateInput <- reactive({
    switch(input$template,
           "13272"= Report_13272nodb(df()),
           "13277"= Report_13277nodb(df()),
           "13351"= Report_13351nodb(df()),
           "13384"= Report_13384nodb(df()),
           "13401"= Report_13401nodb(df()),
           "13447"= Report_13447nodb(df()),
           "14036"= Report_14036nodb(df()),
           "14108"= Report_14108nodb(df()),
           "15342"= Report_15342nodb(df())
          )
    
  })
  
  #Reactive value for selected template for death report
  deathInputnodb <- reactive({
    switch(input$template,
           "13272"= Report_death_13272_nodb(df()),
           "13277"= Report_death_13277_nodb(df()),
           "13351"= Report_death_13351_nodb(df()),
           "13384"= Report_death_13384_nodb(df()),
           "13401"= Report_death_13401_nodb(df()),
           "13447"= Report_death_13447_nodb(df()),
           "14036"= Report_death_14036_nodb(df()),
           "14108"= Report_death_14108_nodb(df()),
           "15342"= Report_death_15342_nodb(df())
           )
    
    
  })
  
  #Reactive value for selected template for demo report
  demoInputnodb <- reactive({
    switch(input$template,
           "13272" = Report_demo_13272_nodb(df()),
           "13277" = Report_demo_13277_nodb(df()),
           "13351" = Report_demo_13351_nodb(df()),
           "13384" = Report_demo_13384_nodb(df()),
           "13401" = Report_demo_13401_nodb(df()),
           "13447" = Report_demo_13447_nodb(df()),
           "14036" = Report_demo_14036_nodb(df()),
           "14108" = Report_demo_14108_nodb(df()),
           "15342" = Report_demo_15342_nodb(df())
           )
    
  })
  
  ################# Barplots #############################
  # barplot for age for nodb #
  output$age_bar2 <- renderPlot({
    our_data <-demoInputnodb()
    if(dim(our_data)[1]<4)
    {
      plot1(our_data,1:3,"Age","Freq")
    }
    else if(dim(our_data)[1]>3 & dim(our_data)[1]<6)
    {
      plot2(our_data,1:3,"Age","Freq")
    }
    else if(dim(our_data)[1]>6 & dim(our_data)[1]<10)
    {
      plot4(our_data,1:3,"Age","Freq")
    }
    else
    {
      plot3(our_data,1:3,"Age","Freq")
    }
    
  })
  
  # barplot for ethnicity #
  output$eth_bar2 <- renderPlot({
    
    our_data <- demoInputnodb()
    if(dim(our_data)[1]<4)
    {
      plot1(our_data,12:14,"Ethnicity","Freq")
    }
    else if(dim(our_data)[1]>3 & dim(our_data)[1]<6)
    {
      plot2(our_data,12:14,"Ethnicity","Freq")
    }
    else if(dim(our_data)[1]>6 & dim(our_data)[1]<10)
    {
      plot4(our_data,12:14,"Ethnicity","Freq")
    }
    else
    {
      plot3(our_data,12:14,"Ethnicity","Freq")
    }
  })
  
  # barplot for Gender #
  output$gender_bar2 <- renderPlot({
    
    our_data <- demoInputnodb()
    
    if(dim(our_data)[1]<4)
    {
      plot1(our_data,4:5,"Gender","Freq")
    }
    else if(dim(our_data)[1]>3 & dim(our_data)[1]<6)
    {
      plot2(our_data,4:5,"Gender","Freq")
      
    }
    else if(dim(our_data)[1]>6 & dim(our_data)[1]<10)
    {
      plot4(our_data,4:5,"Gender","Freq")
    }
    else
    {
      plot3(our_data,4:5,"Gender","Freq")
    }
  })
  
  # barplot for Race #
  output$race_bar2 <- renderPlot({
    
    our_data <- demoInputnodb()
    
    if(dim(our_data)[1]<4)
    {
      plot1(our_data,6:11,"Race","Freq")
    }
    else if(dim(our_data)[1]>3 & dim(our_data)[1]<6)
    {
      plot2(our_data,6:11,"Race","Freq")
      
    }
    else if(dim(our_data)[1]>6 & dim(our_data)[1]<10)
    {
      plot4(our_data,6:11,"Race","Freq")
    }
    else
    {
      plot3(our_data,6:11,"Race","Freq")
    }
    
  })
  
  ######################## Rendering the table ###############################################
  output$moni2 <- DT::renderDataTable({templateInput()},extensions = 'Buttons',options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")) ,
                           initComplete = JS(
                             "function(settings, json) {",
                             "$(this.api().table().header()).css({'background-color': '#008080', 'color': '#fff'});",
                             "}"),escape=FALSE,dom = 'Bfrtip',buttons = list('copy',list(extend='csv',filename='file'),list(extend='pdf',orientation = 'landscape',pageSize = 'A2',filename='file'))
  ))
  output$death2 <- DT::renderDataTable({deathInputnodb()},options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")),
                          initComplete = JS(
                            "function(settings, json) {",
                            "$(this.api().table().header()).css({'background-color': '#008080', 'color': '#fff'});",
                            "}"),escape=FALSE,dom = 'Bfrtip',buttons = list('copy',list(extend='csv',filename='file'),list(extend='pdf',orientation = 'landscape',pageSize = 'A2',filename='file'))
  ))
  
  output$demo2 <- DT::renderDataTable({demoInputnodb()},options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")),
                          initComplete = JS(
                            "function(settings, json) {",
                            "$(this.api().table().header()).css({'background-color': '#008080', 'color': '#fff'});",
                            "}"),escape=FALSE,dom = 'Bfrtip',buttons = list('copy',list(extend='csv',filename='file'),list(extend='pdf',orientation = 'landscape',pageSize = 'A2',filename='file'))
  ))
  

}



# Run the application 
shinyApp(ui = ui, server = server )

