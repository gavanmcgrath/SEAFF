#Source All Scripts
source("./scripts/SnowyLib.R")

model.temp.dir <- "./models/models.custom/temp/"


#########SERVER#########################
shinyServer(function(input, output, session) {
  

#Start Page  
  observeEvent(input$updateDATA, {
    withProgress(message = 'Updating data', min=0,max=1,value = 0, {
      
      #If newer data available download latest ssts and process
      incProgress(0.2, detail = "Checking SSTs")
      upsst <- update.ssts()
      ssts <- read_ssts()
      if (upsst) saveRDS(ssts,file="./data/processed/ssts.rds")
      
      #If newer data available download latest slps and process
      incProgress(0.4, detail = "Checking SLPs")
      upslp <- update.slp()
      slp <- read_slp()
      if (upslp) saveRDS(slp,file="./data/processed/slp.rds")

      #If newer data available download latest BoM rain and process
      incProgress(0.6, detail = "Checking BoM Precip")
      update_BOM_RainGrids()

      #Read flow data and process
      incProgress(0.8, detail = "Checking Flows")
      bom <- get_BOM_Flow(what="monthly")
      scheme <- get.scheme.flows()
      scheme <- fill.missing.flows(scheme)
      flows <- merge.bomscheme.flows(bom=bom,scheme=scheme,unit.diff=c(0.001,1))
      saveRDS(flows,file="./data/processed/flows.rds")
      saveRDS(scheme,file="./data/processed/scheme.rds")
      flow.quantiles.4.scheme()
      
      incProgress(0.9, detail = "Updating Teleconenction Indices")
      dmi.download()
      ipo.download()
      get.PDO()
    })
  })
  
  output$models <- renderUI({
    avail.models <- switch(input$model.id ,
                           "default" = list.dirs(path = "./models/models.default/", 
                                                 full.names = FALSE,recursive = FALSE),
                           "custom" = list.dirs(path = "./models/models.custom/", 
                                                full.names = FALSE,recursive = FALSE))
    lapply(avail.models, function(modl) {
      #read model text file
      #get.model.info
      mod.info <- modl  #for now, bt edit to include model notes saved at the time of creation in Model Explorer
      #if custom edit the true/false
      checkboxInput(modl, modl, ifelse(input$model.id == "default",TRUE,FALSE))
    })
  })
 
  
  observeEvent(input$loadModel,{
    #select model using folder search
    model.dir <- choose.dir("./models/")
    fid <- paste(model.dir,"cca.rds",sep="")
    if (file.exists(fid)) {
      r.cca <- readRDS(fid)
    
    #update input
    # Change the value
    n <- nrow(r.cca$predictor.windows$lat)
    updateNumericInput(session, "nr.predictor", value = n)  #but read from size of windows
    
    updateSelectInput(session, "predor",
                      choices = s_options,
                      selected = r.cca$predor.name
    )
    
    updateSelectInput(session, "predand",
                      choices = s_options,
                      selected = r.cca$predand.name
    )
    
    for (i in 1:n){
      updateNumericInput(session, paste("lata1",i,sep=""), value = 2)  #but read from size of windows
      updateNumericInput(session, paste("lata2",i,sep=""), value = 2)  #but read from size of windows
      updateNumericInput(session, paste("lona1",i,sep=""), value = 2)  #but read from size of windows
      updateNumericInput(session, paste("lona2",i,sep=""), value = 2)  #but read from size of windows
      updateNumericInput(session, paste("lag",i,sep=""), value = 2)  #but read from size of windows
    }
    updateNumericInput(session, "smoother", value = r.cca$smoother)  #smoother
    updateCheckboxInput(session, "detrend",value = r.cca$detrend)  #detrend
    
    updateDateInput(session, "anomaly.t1",
                    label = paste("Start"),
                    value = r.cca$date.start)
    updateDateInput(session, "anomaly.t1",
                    label = paste("End"),
                    value = r.cca$date.end)
    
    }
    
  })
  
  observeEvent(input$savenewmodel,{
    withProgress(message = 'Status:', min=0,max=1,value = 0, {
    
    incProgress(0.2)  
    #Copy files from model temp dir to a new folder
    new.fid <- paste("./models/models.custom/",input$model.filename,"/",sep="")
    dir.create(new.fid, showWarnings = TRUE, recursive = FALSE, mode = "0777")
    files2copy <- list.files(model.temp.dir)
    file.copy(from = paste(model.temp.dir,files2copy,sep=""), to =  paste(new.fid,files2copy,sep=""))
    #unlink(files2copy)
    
    write.table(input$Model.description,file=paste(new.fid,"Notes.txt"),row.names=FALSE,col.names = FALSE)
    incProgress(0.4)  
    
    #Save figs
    if (file.exists(paste(model.temp.dir,"rdf.rds",sep=""))){
      r.df <- readRDS(paste(model.temp.dir,"rdf.rds",sep=""))
      mx.r <- which(r.df[,"r_v"] == max(r.df[,"r_v"],na.rm=TRUE))[1]
      z.max.lower <- r.df[mx.r,"z_v_lo95"] 
      if (length(mx.r) == 0) mx.r=1 
      simp.mod <- which(r.df[1:mx.r,"z_v"] >= z.max.lower)[1]
      tx <-  r.df[simp.mod,"tx"]
      ty <-  r.df[simp.mod,"ty"]
      
      pdf(file = paste(new.fid,"/CCA_CrossVal_a.pdf",sep=""),width=6,height=6)
        plotSigVariates(model.temp.dir)
      dev.off()
      incProgress(0.5)  
      pdf(file = paste(new.fid,"/CCA_CrossVal_b.pdf",sep=""),width=6,height=6)
        plotCrossValCors(model.temp.dir)
      dev.off()
      incProgress(0.6)  
    }
      #Plots of the CCA patterns
      r.cca <- readRDS(file=paste(model.temp.dir,"cca.rds",sep=""))
      tx <- r.cca$tx
      ty <- r.cca$ty
      
      xlim.r <- switch(r.cca$predor.name,
                       "flows" = c(139,155),
                       "scheme"= c(145,155),
                       "precip"= c(139,155))
      ylim.r <- switch(r.cca$predand.name,
                       "flows" = c(-45,-27),
                       "scheme"= c(-40,-30),
                       "precip"= c(-45,-27))
      for (i in 1:min(tx,ty)) {
        incProgress(0.6+i*0.05)
        pdf(file = paste(new.fid,"/CCA",ifelse(i<10,"0",""),i,".pdf",sep=""),width=12,height=6)
        mca_pca_combo_plot(r.cca,
                           type=c(attr(r.cca$center.left,"type"),
                                  attr(r.cca$center.right,"type")),
                           num=i, xlim.r=xlim.r,ylim.r=ylim.r)
        dev.off()
      }
      incProgress(0.99)  
    })
  })
  
  
  output$ui1 <- renderUI({
    default.windows <- switch(input$predor,
                              "ssts"= list(lon = rbind(c(180,250),c(280,360),c(-1,-1),c(-1,-1)),
                                           lat = rbind(c(-60,-20),c(10,70),c(-1,-1),c(-1,-1)),
                                           lag = c(36,156,-1,-1)),
                              "slps" = list(lon = rbind(c(90,280),c(-1,-1),c(-1,-1),c(-1,-1)),
                                            lat = rbind(c(-65,20),c(-1,-1),c(-1,-1),c(-1,-1)),
                                            lag = c(36,-1,-1,-1)),
                              "precip" = list(lon = rbind(c(139,155),c(-1,-1),c(-1,-1),c(-1,-1)),
                                              lat = rbind(c(-45,-27),c(-1,-1),c(-1,-1),c(-1,-1)),
                                              lag = c(3,-1,-1,-1)))
    
    if (!(input$nr.predictor == 0)){
      lapply(1:input$nr.predictor, function(i) {
        fluidRow(
          column(3,numericInput(paste("lata1",i,sep=""),"Lat",default.windows$lat[i,1])),
          column(3,numericInput(paste("lona1",i,sep=""),"Long",default.windows$lon[i,1])),
          column(3,numericInput(paste("lata2",i,sep=""),"Lat",default.windows$lat[i,2])),
          column(3,numericInput(paste("lona2",i,sep=""),"Long",default.windows$lon[i,2])),
          column(3,numericInput(paste("laga",i,sep=""),"Lag",default.windows$lag[i]))
        )
      })
    } else {
     
      nrws <- which(default.windows$lag >=0 )
      
      lapply(nrws, function(i) {
      fluidRow(
        column(3,numericInput(paste("lata1",i,sep=""),"Lat",default.windows$lat[i,1])),
        column(3,numericInput(paste("lona1",i,sep=""),"Long",default.windows$lon[i,1])),
        column(3,numericInput(paste("lata2",i,sep=""),"Lat",default.windows$lat[i,2])),
        column(3,numericInput(paste("lona2",i,sep=""),"Long",default.windows$lon[i,2])),
        column(3,numericInput(paste("laga",i,sep=""),"Lag",default.windows$lag[i]))
      )
      })
      
    }
  }) 
  
  
  observeEvent(input$updateCALC, {
    withProgress(message = 'Status:', min=0,max=5,value = 0, {
      
      incProgress(0.1, detail = "Loading data")
      predictand.data <- switch(input$predand,
                                "flows" = readRDS("./data/processed/flows.rds"),
                                "scheme" = readRDS("./data/processed/scheme.rds"),
                                "precip" = readRDS("./data/processed/precip.rds"))
      
      predictor.data <- switch(input$predor,
                               "ssts" = readRDS("./data/processed/ssts.rds"),
                               "slps" = readRDS("./data/processed/slp.rds"),
                               "precip" = readRDS("./data/processed/precip.rds"))
      
      lags <- c(input$laga1,input$laga2,input$laga3,input$laga4)
      lons = rbind(c(input$lona11,input$lona21),
                  c(input$lona12,input$lona22),
                  c(input$lona13,input$lona23),
                  c(input$lona41,input$lona24))
      lats = rbind(c(input$lata11,input$lata21),
                  c(input$lata12,input$lata22),
                  c(input$lata13,input$lata23),
                  c(input$lata14,input$lata24))
      wh.lags <- which(lags >= 0)
      lags <- lags[wh.lags]
      lons <- lons[wh.lags,]
      lats <- lats[wh.lags,]
      
      predictor.windows <- list(
        lon = lons,
        lat = lats,
        lag = lags)
      
      predictand.windows <- switch(input$predand,
                                   "flows"=  list(lon = rbind(c(140,155),c(151,155)),
                                                  lat = rbind(c(-45,-30),c(-29,-25)),
                                                  lag = c(0,0)),
                                   "scheme" = list(lon = rbind(c(100,151),c(151,155)),
                                                   lat = rbind(c(-45,-30),c(-30,-25)),
                                                   lag = c(0,0)),
                                   "precip" = list(lon = rbind(c(139,155)),
                                                   lat = rbind(c(-45,-27)),
                                                   lag = c(0))
      )
      
      predictor.dates <- attributes(predictor.data)$date
      predictand.dates <- attributes(predictand.data)$date
      #x.match <- which(predictor.dates %in% predictand.dates)
      #y.match <- which(predictand.dates %in% predictor.dates)
      #start.date <- predictor.dates[x.match[1]]
      #start.date <- start.date + months(max(c(predictand.windows$lag,predictor.windows$lag)))
      #end.date <- predictor.dates[x.match[length(x.match)]]
      
      #start.date <- as.Date(input$anomaly.t1,format = "%Y-%m-%d")
      #start.date <- as.Date(input$anomaly.t1,format = "%Y-%m-%d")
      
      predictor <- field2dmat2(predictor.data, 
                               start.date = input$anomaly.t1, 
                               #predictor.dates[1],  #start.date, 
                               end.date = input$anomaly.t2, 
                               #predictor.dates[length(predictor.dates[1])], 
                               #end.date,
                               xlim = predictor.windows$lon,
                               ylim = predictor.windows$lat,
                               lag = predictor.windows$lag)
      
      saveRDS(predictor,file= paste(model.temp.dir,"predictor.field.rds",sep=""))
      
      if (input$predand %in% c("flows","scheme")) {
        predictand <- field1dmat2(predictand.data,
                                  start.date = input$anomaly.t1, #predictand.dates[1], #start.date, 
                                  end.date = input$anomaly.t2, #predictand.dates[length(predictand.dates[1])], 
                                  #end.date,
                                  xlim = predictand.windows$lon,
                                  ylim = predictand.windows$lat,
                                  lag = predictand.windows$lag)
      } else {
        predictand <- field2dmat2(predictand.data,
                                  start.date = input$anomaly.t1, 
                                  #predictand.dates[1], #start.date, 
                                  end.date = input$anomaly.t2, 
                                  #predictand.dates[length(predictand.dates[1])], 
                                  #end.date
                                  xlim = predictand.windows$lon,
                                  ylim = predictand.windows$lat,
                                  lag = predictand.windows$lag)
      }
      
      saveRDS(predictand,file=paste(model.temp.dir,"predictand.field.rds",sep=""))
      
      
      incProgress(0.1, detail = "Processing data")
      yr1 <- substr(input$anomaly.t1,1,4)
      yr2 <- substr(input$anomaly.t2,1,4)
      if (input$detrend) {
        incProgress(0.1, detail = "Detrending")
        predictor <- detrend_Field(predictor,
                        anomPeriod=as.numeric(c(yr1,yr2)))
      }
      
      yr1 <- substr(input$anomaly.t1,1,4)
      yr2 <- substr(input$anomaly.t2,1,4)
      if (input$detrend) {
        incProgress(0.1, detail = "Detrending")
        predictand <- detrend_Field(predictand,
                        anomPeriod=as.numeric(c(yr1,yr2)))
      }
      
      incProgress(0.05, detail = "Smoothing")
      predictor <- filter_Field(predictor,
          filt=rep(1,input$smoother)/input$smoother,method="ma")
      incProgress(0.05, detail = "Smoothing")
      predictand <- filter_Field(predictand,
          filt=rep(1,input$smoother)/input$smoother,method="ma")
      
      if (input$predand %in% c("flows","scheme")) {
        predictand <- scale_Flow(predictand)
      }
      
      incProgress(0.1, detail = "Calc Anomalies")
      predictor <- mnthly_Anom(predictor,
                                    anomPeriod = c(yr1, yr2),
                                    sdMethod = "mon")
      
      incProgress(0.1, detail = "Calc Anomalies")
     
      predictand <- mnthly_Anom(predictand,
                                     anomPeriod = c(yr1, yr2),
                                     sdMethod = "mon")

      
      #predictand <- match.fields.by.date(predictand,predictor,c(yr1, yr2))
      #predictor <- match.fields.by.date(predictor,predictand,c(yr1, yr2))
      #predictand <- match.fields.by.date(predictand,predictor,c(yr1, yr2))
      #predictor <- match.fields.by.date(predictor,predictand,c(yr1, yr2))
      
      incProgress(0.1, detail = "PCA")
      predictor.pca <- pca(predictor,neof=min(30,dim(predictor)[2]),weightbylat = TRUE, ceof=FALSE)
      
      incProgress(0.05, detail = "PCA")
      if (input$predand %in% c("flows","scheme")) {
        weightbylat <- FALSE
      } else {
        weightbylat <- TRUE
      }
      predictand.pca <- pca(predictand,neof=min(30,dim(predictand)[2]),
                            weightbylat = weightbylat, ceof=FALSE)
      
      if (input$optimizetxty) {
        incProgress(0.05, detail = "Evaluating tx, ty")
        results.list <- optim.txty4cca(predictor,predictand,0)
        tx <- results.list$tx
        ty <- results.list$ty
       
        incProgress(0.05, detail = paste("CCA with tx =",tx, "ty =",ty,sep=" "))
        
        r.cca <- cca2(predictor.pca,predictand.pca,predictor,predictand,tx=tx,ty=ty)
      } else {
        tx <- input$tx
        ty <- input$ty
        r.cca <- cca2(predictor.pca,predictand.pca,predictor,predictand,tx=tx,ty=ty)
      }
      r.cca$predand.name <- input$predand
      r.cca$predor.name <-  input$predor
      r.cca$predand.windows <- predictand.windows
      r.cca$predor.windows <- predictor.windows
      r.cca$date.start <- input$anomaly.t1 
      r.cca$date.end <- input$anomaly.t2 
      r.cca$tx <- tx
      r.cca$ty <- ty
      r.cca$smoother <- input$smoother
      r.cca$detrend <- input$detrend
      
      incProgress(0.05, detail = "Saving PCA and CCA")
      saveRDS(predictor.pca,file=paste(model.temp.dir,"predor.pca.rds",sep=""))
      saveRDS(predictand.pca,file=paste(model.temp.dir,"predand.pca.rds",sep=""))
      saveRDS(r.cca,file=paste(model.temp.dir,"cca.rds",sep=""))
    })
    
  })
  
  output$eof1.plot <- renderPlot({
    predictand.pca <- readRDS(file=paste(model.temp.dir,"predand.pca.rds",sep=""))
    xlim <- switch(input$predand,
                   "flows" = c(139,155),
                   "scheme"= c(145,155),
                   "precip"= c(139,155))
    ylim <- switch(input$predand,
                   "flows" = c(-45,-27),
                   "scheme"= c(-40,-33),
                   "precip"= c(-45,-27))
    eof_pca_combo_plot(predictand.pca,type=attr(predictand.pca,"type"),num=input$n.eof1,
                       xlim=xlim,ylim=ylim)
  })
  
  output$eof2.plot <- renderPlot({
    predictor.pca <- readRDS(file=paste(model.temp.dir,"predor.pca.rds",sep=""))
    eof_pca_combo_plot(predictor.pca,type=attr(predictor.pca,"type"),num=input$n.eof2)
  })
  
  output$cca.plot <- renderPlot({
    r.cca <- readRDS(file=paste(model.temp.dir,"cca.rds",sep=""))
    
    xlim.r <- switch(input$predand,
                   "flows" = c(139,155),
                   "scheme"= c(145,155),
                   "precip"= c(139,155))
    ylim.r <- switch(input$predand,
                   "flows" = c(-45,-27),
                   "scheme"= c(-40,-30),
                   "precip"= c(-45,-27))
    
    mca_pca_combo_plot(r.cca,
                       type=c(attr(r.cca$center.left,"type"),
                              attr(r.cca$center.right,"type")),
                       num=input$ncca, xlim.r=xlim.r,ylim.r=ylim.r)
  })
  
  output$optimplot1 <- renderPlot({
    plotSigVariates(model.temp.dir)
  })
  
  output$optimplot2 <- renderPlot({
    plotCrossValCors(model.temp.dir)
  })
  
  
  
  observeEvent(input$calfor, {
    
    #for (i in list.of.models){
    cca.obj <- file=paste(model.temp.dir,"cca.rds",sep="")
    
    newdat <- readnewdata(input$predor)
    
    x.eof <- readRDS()
    y.eof <- readRDS()
    
    data.transform.info <- c()
    
    dat <- prepdat4cca(newdat,data.transform.info)
    
    forcst <- pred.cca(dat,cca.obj,
                         x.eof,y.eof,
                         x.mn=NULL,y.mn = NULL,
                         pred.what="y")
    
    forecast.data <- rescale.forecast(forcst,input$predand)
    saveRDS(forecast.data,file=paste("forecastDATA",input$predand,".rds"))
  })
  
    
  output$ninoPlot <- renderPlot({
    ls.global <- ls( envir = .GlobalEnv)
    if (!("ssts" %in% ls.global)) {
      ssts <<- readRDS("./data/processed/ssts.rds")
    }
    ymd <- as.numeric(unlist(strsplit(as.character(Sys.Date()),split="-")))
    present <- ymd[1]+(ymd[2]+1)/12
    nino.index(ssts,what="nino3.4",clim.period = c(1971,2000),plt=TRUE,
               xlim=c(present-20,present))
  })
  
  output$dmiPlot <- renderPlot({
    #ls.global <- ls( envir = .GlobalEnv)
    #if (!("dmi" %in% ls.global)) {
      dmi <<- readRDS("./data/processed/dmi.rds")
    #}
    ymd <- as.numeric(unlist(strsplit(as.character(Sys.Date()),split="-")))
    present <- ymd[1]+(ymd[2]+1)/12
    dmi.index(dmi,xlim=c(present-20,present))
  })
  
  output$pdoPlot <- renderPlot({
    pdo <<- readRDS("./data/processed/pdo.rds")
    ymd <- as.numeric(unlist(strsplit(as.character(Sys.Date()),split="-")))
    present <- ymd[1]+(ymd[2]+1)/12
    dmi.index(pdo,xlim=c(present-80,present),index.name = "Pacific Decadal Oscillation")
  })
  
  #output$ipoPlot <- renderPlot({
  #  ipo <<- readRDS("./data/processed/ipo.rds")
  #  ymd <- as.numeric(unlist(strsplit(as.character(Sys.Date()),split="-")))
  #  present <- ymd[1]+(ymd[2]+1)/12
  #  dmi.index(ipo,xlim=c(present-80,present),index.name = "Interdecdal Pacific Oscillation")
  #})
  
  output$cumForPlot <- renderPlot({
    plotCumFlowStats(input$site.id)
  })
  
   output$ssttsPlot <- renderPlot({
    #source('./scripts/exFig2.r')
  })
  
   
   
  output$forTable <-  renderTable({
    forcastTable(input$site.id)
  	},digits = 0,spacing = 'm')
  
   output$downloadReport <- downloadHandler(
    filename = function() {
      paste('./reports/forecast_report',Sys.Date(), sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },

    content <- function(file) {
      src <- normalizePath('Report.Rmd')
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'Report.Rmd', overwrite = TRUE)
      out <- render('Report.Rmd', switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ))
      file.rename(out, file)
    }
  )

})
