library(shiny)

shinyUI(
 fluidPage(style="padding-top: 0px;",
    #tab
   list(tags$head(HTML('<link rel="icon", href="Ishka_logo2.png", 
                                   type="image/png" />'))),
   div(style="padding: 0px 0px; width: '100%'",
       titlePanel(
         title="", windowTitle="Ishka Solutions"
       )
   ),
   
   #Gray #CCCCCC
   #Blue   #426BBE
   #Green  #51B749
   #Orange  #DF4A21
   
   #style for absolutePanels
   tags$head(tags$style(
     HTML('
          #predictor {
          background-color: #DF4A21; opacity: 0.95;
          }
          #predictand {
          background-color: #DF4A21; opacity: 0.95;
          }
          #calcFOR{
          background-color: #DF4A21; opacity: 0.95;
          }
          #Calcupdate {
          background-color: #DF4A21; opacity: 0.95;
          }
          #SaveModel {
          background-color: #DF4A21; opacity: 0.95;
          }
          #loadSaveMod{
          background-color: #DF4A21; opacity: 0.95;
          }
          #updateDATA {
          background-color: #51B749
          }
          #calfor{
          background-color: #51B749;
          }
          #downloadReport{
          background-color: #51B749;
          }
          #savenewmodel{
          background-color: #51B749;
          }
          #updateCALC{
          background-color: #51B749;
          }
          #loadModel{
          background-color: #51B749;
          }
          #ForcResults{
          background-color: #DF4A21; opacity: 0.95;
          }
          ')
  )),
  #Navigation Bar
  navbarPage(title=div(img(src="Ishka_logo2.png",height = 30, width = 30),"SEAFF"),
             id="nav",
      
  #Nav 1
  tabPanel("Recent Conditions",  
           fluidRow(
             absolutePanel(id = "ninoplot",        
                           fixed = FALSE,
                           draggable = TRUE, top = 90, height = 45, 
                           left = 100, width = 450,        
                           plotOutput('ninoPlot')
             ),
             absolutePanel(id = "dmiplot",        
                           fixed = FALSE,
                           draggable = TRUE, top = 90, height = 45, 
                           left = 550, width = 450,        
                           plotOutput('dmiPlot')
             ),
             #absolutePanel(id = "ipoplot",        
            #               fixed = FALSE,
             #              draggable = TRUE, top = 300, height = 90, 
            #               left = 100, width = 450,        
            #               plotOutput('ipoPlot')
            # ),
             absolutePanel(id = "pdoplot",        
                           fixed = FALSE,
                           draggable = TRUE, top = 480, height = 45, 
                           left = 100, width = 450,        
                           plotOutput('pdoPlot')
             ),
             absolutePanel(id = "Dataupdate",
                           div(style="padding: 0px; border: 0px solid #CCC;"),  
                           fixed = FALSE,
                           draggable = TRUE, top = 90, height = "auto", 
                           left = 10,width = 120,
                           actionButton("updateDATA", "Update Data")
             )
  )
  ),
  
  #Nav 2
  tabPanel("Forecast Results", 
    sidebarLayout(       
           sidebarPanel(id = "calcFOR", width = 3,
                         selectInput("model.id", "Select Model(s)",
                                     c("Default" = "default",
                                       "Custom" = "custom")
                         ),
                        uiOutput("models"),
                        actionButton("calfor", "Update Forecast"),
                        hr(),
                        selectInput("site.id", "Select Catchment",
                                    c("Total Snowy Scheme" = "TotalSnowyScheme",
                                      "Lake Hume DamWall" = "Lake_Hume_DamWall",
                                      "Hume Dam Natural" = "Hume_Dam_Natural",
                                      "Hume Unregulated" = "Hume_Unregulated",
                                      "Dartmouth DamWall" = "Dartmouth_DamWall",
                                      "Snowy Murray" = "SnowyMurray",
                                      "Upper Tumut" = "UpperTumut",
                                      "BL Natural" = "BL_Natural"),
                                    "TotalSnowyScheme"),
                         radioButtons('format', 'Report format', c('HTML', 'Word'),
                                            selected='Word',inline = TRUE),
                          downloadButton('downloadReport',label="Download Report")
                         
           ),
          mainPanel(id = "ForcResults",style="padding: 8px; border: 0px solid #CCC;", 
                    align = "center",
           
           fluidRow(
             column(8, align = "center",
                    plotOutput('cumForPlot')
                    )
           ),
           fluidRow(
             column(8,align = "center",
                    tableOutput('forTable')
                    )
           )
           )
          )
  
  ),
  
  #Nav 3
  tabPanel("Explore Models",
    tags$head(tags$style(
      HTML(".selectize-input {font-size: 75%;}, .selectize-dropdown {font-size: 75%;}"))
    ),
    sidebarLayout(
     sidebarPanel(id = "loadSaveMod", width = 2,
      style="padding: 8px; border: 0px solid #CCC;",
      #actionButton("loadModel", "Load Model"),
      #hr(),
      
      #Save current model loaded into temp
      actionButton("savenewmodel", "Save Custom Model"),
      textInput("model.filename", "File Name",width = 150,
                value = paste("Custom_Model_",
                              length(list.dirs(path = "./models/models.custom",recursive = FALSE))-1,sep=""),
                placeholder = paste("Custom_Model_",
                                    length(list.dirs(path = "./models/models.custom",recursive = FALSE))-1
                                    ,sep="")),
      textAreaInput("Model.description", "Notes",value="",width = 150,height = 200)
     ),
    
    mainPanel(id = "manipulateNewModel",  
              style="padding: 8px; border: 0px solid #CCC;",        
    absolutePanel(id = "Calcupdate",
      style="padding: 8px; border: 0px solid #CCC;",   
      fixed = FALSE,
    	draggable = TRUE, top = 200, height = "auto", 
      left = 530, width = 440,        
    	fluidRow(
    	  column(4,
    	    actionButton("updateCALC", "Calc EOFs/CCA")),
    	  column(5, 
    	    checkboxInput("optimizetxty", "Optimise tx & ty?"))
    	),
      fluidRow(
        column(3,
          numericInput("tx", "tx",2,min=1,max=30)),
        column(3,
               numericInput("ty", "ty",2,min=1,max=30)),
        column(5, p("Warning predicting rainfall could take hours."))
      )
      
    ),
    	           
    absolutePanel(id = "predictor",  fixed = FALSE,
      draggable = TRUE, top = 5,  height = "auto", 
    	left = 15, width = 500,
      style="padding: 8px; border: 0px solid #CCC; overflow-y:scroll; max-height: 500px",
    	fluidRow(
    	  column(6,
          selectInput("predor", "Left Field",
    	      c("SST" = "ssts",
    	        "SLP" = "slps",
    	        "Precip" = "precip"),"ssts")),
    	  column(3,
          numericInput("nr.predictor", "# Regions", 0, min = 0, max = 4))

    	 ),

      uiOutput("ui1")
    ),
    	     
    absolutePanel(id = "predictand", class = "panel panel-default", fixed = FALSE,
      draggable = TRUE, top = 5, height="auto",
      style="padding: 8px; border: 0px solid #CCC;",
      left = 530, width = 440,
      fluidRow(
        column(6,
          selectInput("predand", "Right Field",
            c("BoM + Scheme" = "flows",
    	       "Scheme" = "scheme",
    	       "Precip" = "precip"),"scheme")),
        column(3,
          numericInput("smoother", "Smoother", 36, min = 1, max = 60))
      ),
    	                   
      fluidRow(
        column(4,
          dateInput("anomaly.t1","Start",value="1960-01-01",startview="year")),
        column(4,
          dateInput("anomaly.t2","End",value="2010-01-01",startview="year")),
        column(3,
          checkboxInput("detrend","Detrend?"))
      )
    	                   
    )
    )
  )
  ),
  
  tabPanel("Left EOF",
    fluidRow(
      column(2,numericInput("n.eof2", "# EOF", 1, min = 1, max = 15)),
    	column(8,plotOutput('eof2.plot'))
    )
  ),
    	  
  tabPanel("Right EOF",
    fluidRow(
    	column(2,numericInput("n.eof1", "# EOF", 1, min = 1, max = 15)),
    	column(8,plotOutput('eof1.plot'))
    )
  ),
    	  
  tabPanel("CCA",  
    fluidRow(
      column(2,numericInput("ncca", "CCA #", 1, min = 1, max = 20)),
    	column(8,plotOutput('cca.plot'))
    )
  ),
  
  tabPanel("Optim tx ty",
    fluidRow(
      column(5,plotOutput('optimplot1')),
      column(5,plotOutput('optimplot2'))
    )
  )
    		
  
)
)
)

