library(openxlsx)
library(data.table)
library(snow)

opTimeCountsMile <- NULL
motornummer<-NULL
engType <- NULL
engSeries <- NULL
setwd(workDir)
driveTypeBreaks <- c(0, 850, Inf)  #c(-1,1,300) km/h
driveTypeBreaks1 <- c(0, 1e-2, 850, Inf)
engSpeedTypeBreaks <- c(0, 1e-2, Inf) # U/min
fuelConsTypeBreaks <- c(0, 1e-2, Inf)

# opTimeCountsMile <- NULL
failSlides <- data.frame(matrix(nrow=0,ncol=6))

#Path ----

Sys.setenv(R_ZIPCMD = "C:/Rtools/bin/zip.exe")
### preliminaries and path definitions
.libPaths(c(.libPaths(),"C:/EngineDL/SETUP_Files/R-3.3.3/library2"))
workDir <- "C:/EngineDL/WorkDir/Version_6_0_177-822/" # path of the R workspace (see row 13)
#projDir <- "C:/Users/KRSANTH/Documents/R/R-3.3.1/library/"
# packDir <- paste0(workDir,"Packages") # location of the installed R packages
projDir <- "C:/EngineDL/Vehicle_Data/Version_6_0_177-822/"
# 'projDir' is the project directory. In this directory there must be the configuration file 'config.xlsx'. There must
# also be one subdirectory for every vehicle (Dauerlauf), e.g. subdirectory '123-4567', subdirectory '444-1111', etc.
# And each of these vehicle subdirectories must have four subdirectories again: 'Daten', 'Daten10Hz', 'Drehzahlgradienten'
# and 'Grafiken'. The 1Hz data csv files must be copied to 'Daten'. The 10Hz data csv files must be copied to 'Daten10Hz'.
# This script saves all the plots to 'Grafiken' and some start engine speed plots to 'Drahzahlgradienten'.
ppTemplate <- "C:/EngineDL/PPT_Template/EngineDLTemp_1.pptx" # MS PowerPoint template for reporting (see funtion 'createReport')
pdflatex <- 'C:/Program Files/MiKTeX 2.9/miktex/bin/x64/pdflatex.exe' # PDFLaTeX distribution (see funtions 'createGenDatPng', 'getFailSlides')
#pdflatex <- "C:/Users/SHINAMI/AppData/Local/Programs/MiKTeX2.9/miktex/bin/x64/pdflatex.exe" # PDFLaTeX distribution (see funtions 'createGenDatPng', 'getFailSlides')

# ----


### function definitions
readwithTrack<- function(fNames){
  #signalVec<-c("EngOilMplSnsr_S","EngOilMplSnsrPwm_PercLvl","EngOilLvl","EngOilLvl_V2","Odo","Time_abs")
  print("i am in function")
  data<-data.table::fread(fNames,sep=";",dec = ",",header = TRUE)
  ##considering the name is Version_6_0_<vehicleNum>
  track <- as.numeric(substr(sapply(strsplit(fNames,"[_-]"),function(x){x[10]}),1,2)) 
  dateStamp <- as.numeric(substr(sapply(strsplit(fNames,"[_-]"),function(x){x[9]}),1,14))
  data[["track"]] <- track
  data[["dateStamp"]] <- dateStamp
  return(data)
}
# Function ----
CalcCount <- function(dir,mainConfig,channSigConfig,conditionalConfig,pwrTrqConfig,trackConfig,perfmDQ) {
  print("Calc Count function Start")
  caclCountStartTime <- Sys.time()
  print(caclCountStartTime)
  library(readxl)
  setwd(dir)
  targTrackDat <- data.frame(read_excel("genDat.xlsx", sheet = 2))
  targTrackDat<-data.frame(targTrackDat[,1:2])
  print(dir)
  ###added on 23-01-2017. 
  targTrackDat<-na.omit(targTrackDat)
  ##reading Data.
  ##checking if the fulldata is present or not.
  if(!dir.exists(paste0(dir,"fullData/"))){
    print("Full Data is not present ")
    dir.create(paste0(dir,"fullData/"))
    
    csvDir <- paste0(dir,"Daten/")
    print(csvDir)
    fNames<- list.files(csvDir, pattern = "*.csv")
    print(fNames[1])
    start_time <- Sys.time()
    c1<-makeCluster(4)
    system.time(Y <- parLapply(c1,x = paste0(csvDir,fNames),fun =readwithTrack))
    #system.time(Y <- lapply(X = paste0(csvDir,fNames),FUN =readwithTrack))
    DT<-rbindlist(Y,fill = TRUE)
    fulldata<-as.data.frame(DT)
    X<-fulldata
    saveRDS(fulldata,paste0(paste0(dir,"fullData/","fulldf.rds")))
    rm(fulldata)
    rm(DT)
    rm(Y)
    stopCluster(c1)
    print("finished creating data")
  }else{
    print("reading data from the Full Data folder")
    system.time(X <- readRDS(paste0(paste0(dir,"fullData/"), "fulldf.rds")))
    print("finished reading data")
  }
  ##
  allColNames <- NULL
  allColNames <- c(allColNames, names(X))
  ###This code will output which signal is used out of list in all the calculations
  LogDir <- paste0(dir,"/Logs/")
  if(!dir.exists(paste0(dir,"/Logs/"))){
    dir.create(paste0(dir,"/Logs/"))
    file.create(paste0(dir,"/Logs/Log.txt"))
  }
  LogFile <- paste0(dir,"/Logs/Log.txt")
  write(paste0("Log File @ run",Sys.time()),LogFile,append = T)
  signalLists <- channSigConfig[,8:25]
  for(signal_i in 1:nrow(channSigConfig))
  {
    signalName <- channSigConfig[signal_i,8:25]
    signalName <- signalName[which(!is.na(signalName))]
    signalPresent <- as.character(signalName[signalName %in% names(X)])
    signalPresent <- c(signalPresent,signalPresent)
    usedSignal <- signalPresent[which.max(colSums(!is.na(X[,signalPresent])))]
    write(paste0("For Parameter",as.character(paste(channSigConfig[signal_i,2],collapse = "")),"(",paste(signalPresent[!duplicated(signalPresent)],collapse=";"),")","-->",paste0(usedSignal)),LogFile,append = T)
    #write(paste0("For Channel ",as.character(paste(channSigConfig[signal_i,3],collapse = ""))," Follwing signals are present ",paste(signalName,collapse = ";")," and Used signal is ",paste0(usedSignal)),LogFile,append = T)
    #print(paste0("For Channel",as.character(channSigConfig[signal_i,3]),"Follwing signals are present ",paste(signalName,collapse = ";"),"and Used signal is ",paste0(usedSignal)))#,LogFile,append = T)
    
  }
  ###Signal Lists ends
  
  ##generating data quality rds
  dataQualityDF1 <- data.frame(signalName = character(),rawRowCount = numeric(),rawrowPec = numeric(),
                               filterRowCount = numeric(),filterRowPerc = numeric(),
                               redRowCount = numeric(),redRowPerc = numeric(),remainingRowCount = numeric(),
                               remainingRowPerc = numeric(),lessthn2Perc = numeric(),naBeforeCount =numeric(), stringsAsFactors = FALSE)
  
  #dqCheck <- readRDS(paste0(dir,"/Daten/dataQualityMM.rds"))
  
  dq_rds <- data.frame(columnName = character(),perce = numeric(),stringsAsFactors = FALSE)
  for(cidx in 1:length(allColNames))
  {
    dq_rds[cidx,1] <- as.character(allColNames[cidx])
    dq_rds[cidx,2] <- round(sum(length(which(!is.na(X[,allColNames[cidx]]))))/nrow(X)*100,2)
  }
  dq_rds<-subset(dq_rds, !(colnames(dq_rds)%in% c("track","datestamp")))
  #saveRDS(dq_rds,file=paste0(dir,"dataFreqtable.rds"))
  saveRDS(dq_rds,file=paste0(dir,"Daten/dataFreqtable.rds"))
  
  ###Signal Lists starts
  
  ###data quality RDS compeletion
  dqCheck <- NULL
  if(perfmDQ == "yes" & is.null(dqCheck)){
    print("Starting Data Quality Check")
    channSigConfig1 <- subset(channSigConfig,channSigConfig[,7]!="CG")
    signalVector <- channSigConfig1[,8:25]
    signalVector <- c(t(signalVector))
    signalVector <- signalVector[!is.na(signalVector)]
    colName <- signalVector[signalVector %in% names(X)]
    #availSignals <- signalVector[which.max(colSums(!is.na(X[,signalVector])))]
    
    ##Odo reading
    XOdo <- as.numeric(X$Odo)
    X$Odo <- as.numeric(X$Odo)
    OdoKm <- round(c(max(0,max(X$Odo,na.rm=TRUE)-min(X$Odo,na.rm=TRUE))))
    if(OdoKm > 80000){
      patchOdo <- OdoKm*0.12/100
    }else if (OdoKm >30000 & OdoKm < 80000){
      patchOdo <- OdoKm*0.3/100
    }else if (OdoKm < 30000){
      patchOdo <- OdoKm*0.5/100
    }
    
    ##camtronic signals in data quality.
    if("CylCutoff_Stat" %in% names(X)){
      if(min(X$CylCutoff_Stat,na.rm = T) == 0 & mean(X$CylCutoff_Stat,na.rm = T)==0 & max(X$CylCutoff_Stat,na.rm = T) == 0)
      {
        X$CylCutoff_Stat <- NA
      }
    }
    
    
    OdoBreaks <- min(XOdo,na.rm = T)
    BreakLength <- max(XOdo,na.rm = T)/patchOdo
    OdoBreaks1 <- OdoBreaks
    for(idx in 1:BreakLength)
    {
      OdoBreaks1 <- OdoBreaks[idx] + patchOdo
      OdoBreaks <- c(OdoBreaks,OdoBreaks1)
    }
    
    starttime <- Sys.time()
    for(i in 1:length(colName)){
      # for(i in 1:length(colName)){
      dataQualityDF1[i,1] <- as.character(colName[i])
      dataQualityDF1[i,2] <- sum(length(which(!is.na(as.numeric(X[,colName[i]])))))
      dataQualityDF1[i,3] <- round(sum(length(which(!is.na(as.numeric(X[,colName[i]])))))/nrow(X)*100,2)
      dataQualityDF1[i,11] <- sum(length(which(is.na(as.numeric(X[,colName[i]])))))
      #dataQualityDF1[i,2] <- sum(length(which(!is.na(X[,colName[i]]))))
      sigIdx <- which(channSigConfig[,8:25] == colName[i],arr.ind = TRUE)
      sigIdx <- sigIdx[1,1]
      sigMin <- as.numeric(channSigConfig[sigIdx,5])
      sigMax <- as.numeric(channSigConfig[sigIdx,6])
      X1 <- as.numeric(X[[colName[i]]])
      X1 <- X1[X1>=sigMin & X1<=sigMax]
      dataQualityDF1[i,4] <- sum(length(which(!is.na(X1))))
      dataQualityDF1[i,5] <- round(sum(length(which(!is.na(X1))))/nrow(X)*100,2)
      ## run patch Odo Km
      rm(X1)
      diffCount <- 0
      for(idx in 1:(length(OdoBreaks)-1)){
        X2 <- as.numeric(X[[colName[i]]])
        X2 <- X2[X2>=sigMin & X2<=sigMax]
        X22 <- as.numeric(subset(X2,X$Odo >= OdoBreaks[idx] & X$Odo <= OdoBreaks[idx+1]))
        print(colName[i])
        #print(sd(X22,na.rm =  TRUE))
        #print(summary(X22))
        #diffMaxMin <- (max(X22,na.rm = T) - min(X22,na.rm = T))/nrow(X)*100
        
        diffMaxMin <- abs(max(X22,na.rm = T) - min(X22,na.rm = T))/abs(mean(X22,na.rm = T))
        print(diffMaxMin)
        if(diffMaxMin < 0.002 & !is.nan(diffMaxMin))
        {
          print("icame here")
          if(sum(X22[1:length(X22)],na.rm = T) !=0){
            X22[1:length(X22)] <- NA
            X[1:length(X22),colName[i]] <- NA
          }
          print(length(which(is.na(X22))))
          diffCount <- diffCount + length(which(is.na(X22)))
          print(diffCount)
        }
        rm(X2)
        rm(X22)
      }
      dataQualityDF1[i,6] <- diffCount
      dataQualityDF1[i,7] <- round(diffCount/nrow(X)*100,2)
      
      #rm(X11)
      ##changes needs to be done here filter - removed
      dataQualityDF1[i,8] <- dataQualityDF1[i,2] - dataQualityDF1[i,6]
      dataQualityDF1[i,9] <- abs(round(dataQualityDF1[i,8]/nrow(X)*100,2))
      #
      print(as.character(colName[i]))
      ##
    }
    for(s in 1:nrow(dataQualityDF1)){
      if(dataQualityDF1[s,9] <2){
        dataQualityDF1[s,10] <- 0
      }else{
        dataQualityDF1[s,10] <- dataQualityDF1[s,9]
      }
    }
    endtime <- Sys.time()
    difftime(endtime, starttime, tz,units = c("auto"))
    saveRDS(dataQualityDF1,paste0(dir,"/Daten/dataQualityMM.rds"))
    saveRDS(X,paste0(dir,"/fullData/fulldf.rds"))
    write.csv(dataQualityDF1,"dataQuality_usingminmax.csv")
    print("Finished Data Quality Check")
  }
  
  #   ##creating track data
  allTrackID <- NULL
  allTrackNames <- NULL
  if (nrow(targTrackDat) < 8) {
    allTrackID <- as.numeric(unlist(trackConfig[, 1]))
    allTrackNames <- as.character(unlist(trackConfig[, 2]))
  } else{
    allTrackID <- as.numeric(unlist(trackConfig[, 1]))
    allTrackNames <- as.character(unlist(trackConfig[, 3]))
  }
  targTrackName <-
    sapply(
      targTrackDat[, 1],
      FUN = function(x) {
        paste("Strecke", x, "\n", allTrackNames[allTrackID == x])
      }
    )
  
  speed <- matrix(nrow = 0, ncol = 7)
  eng <- matrix(nrow = 0, ncol = 7)
  ssa <- matrix(nrow = 0, ncol = 7)
  eEng <- matrix(nrow = 0, ncol = 7)
  ped <- matrix(nrow = 0, ncol = 7)
  
  ##defining Breaks
  engSpeedPosBreaks <- c(1e-2, Inf) # U/min
  opTimeBreaks <- c(0, 1e-2, Inf)  #c(0,200,Inf) U/min
  driveTimeBreaks <- c(0, 1e-2, Inf)  #c(-1,1,300) km/h
  idleModeBreaks <- c(1e-2, 850, Inf)  #c(100,850) U/min
  pushModeBreaks1 <- c(0, 1e-2, Inf)  #c(-Inf,10) µl/250ms
  pushModeBreaks2 <- c(1e-2, 850, Inf)  #c(700,Inf) U/min
  pedHighBreaks1 <- c(80, Inf)  # %
  pedHighBreaks2 <- c(700, Inf)  # U/min
  startThresh <- 200  # U/min
  startMinTime <- 3  #
  standStillBreaks1 <-
    c(0, Inf)  # km/h Changed from 20 to Inf Based on clarification given by PM3 on 16/02/2017 (Limit on Speed removed)
  standStillBreaks2 <- c(0, 1e-2)  #c(-Inf,200) U/min
  highLoadStartTime <- 2 #
  highLoadStartThresh <- 80 # %
  dpfRegBreaks1 <- c(0, 1)  #
  dpfRegBreaks2 <- c(700, Inf)  # U/min
  
  driveTypeBreaks <- c(0, 850, Inf)  #c(-1,1,300) km/h
  driveTypeBreaks1 <- c(0, 1e-2, 850, Inf)
  engSpeedTypeBreaks <- c(0, 1e-2, Inf) # U/min
  fuelConsTypeBreaks <- c(0, 1e-2, Inf)
  track <- NULL
  distanceSpeed <- NULL
  oilLev <- NULL
  odoOilLev <- NULL
  motorModeCounts <- matrix(nrow = 0, ncol = 6)
  #opTimeCounts <- matrix(nrow = 0, ncol = length(opTimeBreaks) - 1)
  driveTimeCounts <- matrix(nrow = 0, ncol = length(driveTimeBreaks) - 1)
  idleModeCounts <- matrix(nrow = 0, ncol = length(idleModeBreaks) - 1)
  pushModeCounts <- matrix(nrow = 0, ncol = length(pushModeBreaks1) - 1)
  ymdata <- NULL
  ySeq <- data.frame(Odo = double())
  ymdata < matrix(nrow = 0, ncol = 3)
  medianXAirTemp <- NULL
  maxXAirTemp <- NULL
  minXAirTemp <- NULL
  
  numHighLoadStarts <- NULL
  vehSpeedStartCounts <- NULL
  
  ##RLE Tracking for Few functions
  library(accelerometry)
  rleTrack <- rle2(X$dateStamp, return.list = TRUE, indices = TRUE)
  
  ##Initializaing Good/Fail Slides Frame
  failSlides <- data.frame(matrix(nrow = 0, ncol = 6))
  GoodSlides <- data.frame(matrix(nrow = 0, ncol = 17))
  fail_index <- 1
  good_index <- 1
  #loop through config file.
  # getwd()
  # setwd('..')
  dir <- paste0(getwd(),"/Daten/")
  #dir<-paste0(dir)
  for (ri in 1:length(rleTrack$starts)) {
    track[ri] <- X$track[rleTrack$starts[ri]]
  }
  trackName <-
    sapply(
      track,
      FUN = function(x) {
        paste("Strecke", x, "\n", allTrackNames[allTrackID == x])
      }
    )
  ##Calculating Miles for every parameters.
  print("Calculating Miles for the Parameters")
  OdoMiles <- NULL
  for(mi in 1:length(rleTrack$starts))
  {
    X1 <- X[rleTrack$starts[mi]:rleTrack$stops[mi], ]
    XOdo <- getChann1("Odo", X1,0,1000000)
    XOdo <- as.numeric(XOdo)
    OdoMileperTrack <-  c(max(0,max(XOdo, na.rm = TRUE) - min(XOdo, na.rm = TRUE)))
    OdoMiles <- c(OdoMiles,OdoMileperTrack)
  }
  #saving the RDS
  xVarName <- mainConfig[!is.na(mainConfig[,6]),6]
  for(p_idx in 1:length(xVarName)){
    saveRDS(object = OdoMiles, file = paste0(dir,xVarName[p_idx],"Mile.rds"))
  }
  
  ##End of Miles calculation
  print("Staring All Parameter Calculations")
  gid <- as.numeric(unlist(mainConfig[, 1]))
  for (idx in 87:87) {
    if (gid[idx] == 201) {
      #2.1 Motorbetriebsart
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      starttime <- Sys.time()
      calc1Val <- as.character(mainConfig[idx, 12])
      calc2Val <- as.character(mainConfig[idx, 13])
      ##calc5Odo <- as.character(mainConfig[idx, 14])
      varName <- as.character(mainConfig[idx, 6])
      varNameMile <- as.character(paste0(varName, "Mile"))
      assign(varNameMile, NULL)
      #signal 1
      sig1idx <- which(channSigConfig[, 3] == calc1Val)
      sig1Val <- channSigConfig[sig1idx, 4]
      signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
      signal1Vec[signal1Vec == "NA"] <- NA
      signal1Vec <- signal1Vec[!is.na(signal1Vec)]
      assign(sig1Val, signal1Vec)
      #signal 2
      sig2idx <- which(channSigConfig[, 3] == calc2Val)
      sig2Val <- channSigConfig[sig2idx, 4]
      signal2Vec <- as.character(channSigConfig[sig2idx, 8:25])
      signal2Vec[signal2Vec == "NA"] <- NA
      signal2Vec <- signal2Vec[!is.na(signal2Vec)]
      assign(sig2Val, signal2Vec)
      # #Signal3 - Odo
      # sig3idx <- which(channSigConfig[,3] == calc5Odo)
      # sig3Val <- channSigConfig[sig3idx, 4]
      # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
      # signal3Vec[signal3Vec == "NA"] <- NA
      # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
      # assign(sig3Val, signal3Vec)
      #signal threshold
      sig1min <- as.numeric(channSigConfig[sig1idx,5])
      sig1max <- as.numeric(channSigConfig[sig1idx,6])
      sig2min <- as.numeric(channSigConfig[sig2idx,5])
      sig2max <- as.numeric(channSigConfig[sig2idx,6])
      # #sig3min <- as.numeric(channSigConfig[sig3idx,5])
      # ##sig3max <- as.numeric(channSigConfig[sig3idx,6])
      #get data
      calc1Val <- getChann1(get(sig1Val), X,sig1min,sig1max)
      calc2Val <- getChann1(get(sig2Val), X,sig2min,sig2max)
      #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      opTimeCounts <- matrix(nrow = 0, ncol = length(opTimeBreaks) - 1)
      for (ri in 1:length(rleTrack$starts)) {
        X1 <- X[rleTrack$starts[ri]:rleTrack$stops[ri], ]
        calc1Val <- getChann1(get(sig1Val), X,sig1min,sig1max)
        opTimeCounts <-
          rbind(
            opTimeCounts,
            countFreq1D(
              x = calc1Val,
              breaks = opTimeBreaks,
              unitFactor = 1 / 3600
            )$count
          )
        rm(X1)
      }
      
      opTimeCountsMile <- NULL
      opTimePerTrack <- tapply(opTimeCounts[, 2], trackName, FUN = sum)
      trackNameUse <-
        sort(unique(c(
          names(opTimePerTrack)[opTimePerTrack > 0], targTrackName
        )))
      
      opTimeCountsMile <-
        c(opTimeCountsMile, sum(calc1Val[!is.na(calc2Val)], na.rm = TRUE) / 3600)
      # assign(varNameMile[1], c(max(
      #   0,
      #   max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
      # )))
      if (is.null(opTimePerTrack)) {
        f_idx <- which(mainConfig[, 1] == 201)
        failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
        failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
        if(length(colName1) == 0){
          failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
        }else if(length(colName2) == 0){
          failSlides[fail_index,4] <-as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
        }
        fail_index <- fail_index + 1
      }else{
        g_idx <- which(mainConfig[, 1] == 201)
        GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
        GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
        #summary of Signal 1
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName1 <- c(colName1,colName1)
        col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
        count1 <- sum(length(which(!is.na(X[,col1]))))
        perc1 <- round(count1/nrow(X)*100)
        GoodSlides[good_index,3] <- paste0(col1)
        GoodSlides[good_index,4] <- paste0(count1)
        GoodSlides[good_index,5] <- paste0(perc1,"%")
        #summary of Signal 2 
        colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
        colName2 <- c(colName2,colName2)
        col2 <- colName2[which.max(colSums(!is.na(X[,colName2])))]
        count2 <- sum(length(which(!is.na(X[,col1]))))
        perc2 <- round(count2/nrow(X)*100)
        GoodSlides[good_index,6] <- paste0(col2)
        GoodSlides[good_index,7] <- paste0(count2)
        GoodSlides[good_index,8] <- paste0(perc2,"%")
        # #summary of Signal 3 
        # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
        # colName3 <- c(colName3,colName3)
        # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
        # count3 <- sum(length(which(!is.na(X[,col3]))))
        # perc3 <- round(count3/nrow(X)*100)
        # GoodSlides[good_index,9] <- paste0(col3)
        # GoodSlides[good_index,10] <- paste0(count3)
        # GoodSlides[good_index,11] <- paste0(perc3,"%")
        good_index <- good_index + 1
        ###save RDS for the parameter
        saveRDS(opTimePerTrack, paste0(dir, "opTimePerTrack", ".rds"))
        saveRDS(opTimeCountsMile, paste0(dir, "opTimeCountsMile", ".rds"))
        saveRDS(object=trackName,file=paste0(dir,"trackName.rds"))
        saveRDS(object=trackNameUse,file=paste0(dir,"trackNameUse.rds"))
        saveRDS(object=targTrackName,file=paste0(dir,"targTrackName.rds"))
        saveRDS(object=targTrackDat[,2],file=paste0(dir,"targDist.rds"))
        saveRDS(object=targTrackDat[,2],file=paste0(dir,"targDistMile.rds"))
        #saveRDS(get(varNameMile[1]), paste0(dir, varNameMile[1], ".rds"))
      }
      
      endtime <- Sys.time()
      print(paste0(
        "total time for calculating ",
        "OptimeCounts",
        " is ",
        endtime - starttime
      ))
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    }
    else if (gid[idx] == 202) {
      #2.2 Streckenprofil/2.2.1 Motorbetriebsart
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      starttime <- Sys.time()
      varName <- as.character(mainConfig[idx, 6])
      # varNameMile <- as.character(paste0(varName, "Mile"))
      assign(varName, NULL)
      # assign(varNameMile, NULL)
      calc1Val <- as.character(mainConfig[idx, 10])
      calc2Val <- as.character(mainConfig[idx, 11])
      calc3Val <- as.character(mainConfig[idx, 12])
      calc4Val <- as.character(mainConfig[idx, 13])
      # #calc5Odo <- as.character(mainConfig[idx, 14])
      #signal 1
      sig1idx <- which(channSigConfig[, 3] == calc1Val)
      sig1Val <- channSigConfig[sig1idx, 4]
      signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
      signal1Vec[signal1Vec == "NA"] <- NA
      signal1Vec <- signal1Vec[!is.na(signal1Vec)]
      assign(sig1Val, signal1Vec)
      #signal 2
      sig2idx <- which(channSigConfig[, 3] == calc2Val)
      sig2Val <- channSigConfig[sig2idx, 4]
      signal2Vec <- as.character(channSigConfig[sig2idx, 8:25])
      signal2Vec[signal2Vec == "NA"] <- NA
      signal2Vec <- signal2Vec[!is.na(signal2Vec)]
      assign(sig2Val, signal2Vec)
      #signal 3
      sig3idx <- which(channSigConfig[, 3] == calc3Val)
      sig3Val <- channSigConfig[sig3idx, 4]
      signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
      signal3Vec[signal3Vec == "NA"] <- NA
      signal3Vec <- signal3Vec[!is.na(signal3Vec)]
      assign(sig3Val, signal3Vec)
      #signal 4
      sig4idx <- which(channSigConfig[, 3] == calc4Val)
      sig4Val <- channSigConfig[sig4idx, 4]
      signal4Vec <- as.character(channSigConfig[sig4idx, 8:25])
      signal4Vec[signal4Vec == "NA"] <- NA
      signal4Vec <- signal4Vec[!is.na(signal4Vec)]
      assign(sig4Val, signal4Vec)
      # #Signal5 - Odo
      # sig5idx <- which(channSigConfig[, 3] == calc5Odo)
      # sig5Val <- channSigConfig[sig5idx, 4]
      # signal5Vec <- as.character(channSigConfig[sig5idx, 8:25])
      # signal5Vec[signal5Vec == "NA"] <- NA
      # signal5Vec <- signal5Vec[!is.na(signal5Vec)]
      # assign(sig5Val, signal5Vec)
      #signal threshold
      sig1min <- as.numeric(channSigConfig[sig1idx,5])
      sig1max <- as.numeric(channSigConfig[sig1idx,6])
      sig2min <- as.numeric(channSigConfig[sig2idx,5])
      sig2max <- as.numeric(channSigConfig[sig2idx,6])
      sig3min <- as.numeric(channSigConfig[sig3idx,5])
      sig3max <- as.numeric(channSigConfig[sig3idx,6])
      sig4min <- as.numeric(channSigConfig[sig4idx,5])
      sig4max <- as.numeric(channSigConfig[sig4idx,6])
      # sig5min <- as.numeric(channSigConfig[sig5idx,5])
      # sig5max <- as.numeric(channSigConfig[sig5idx,6])
      
      for (ri in 1:length(rleTrack$starts)) {
        X1 <- X[rleTrack$starts[ri]:rleTrack$stops[ri], ]
        driveTypeBreaks <- c(0, 850, Inf)  #c(-1,1,300) km/h
        driveTypeBreaks1 <- c(0, 1e-2, 850, Inf)
        engSpeedTypeBreaks <- c(0, 1e-2, Inf) # U/min
        fuelConsTypeBreaks <- c(0, 1e-2, Inf)
        calc1Val <- getChann1(get(sig1Val), X1,sig1min,sig1max)
        calc2Val <- getChann1(get(sig2Val), X1,sig2min,sig2max)
        calc3Val <- getChann1(get(sig3Val), X1,sig3min,sig3max)
        calc4Val <- getChann1(get(sig4Val), X1,sig4min,sig4max)
        # calc5Odo <- getChann1(get(sig5Val), X1,sig5min,sig5max)
        ausX1 <- na.omit(X1[, c("EngRPM", "VehSpd_Disp", "SSA_EngSp")])
        motorModeCounts <-
          rbind(
            motorModeCounts,
            countFreq2DMF(
              a = aus,
              b = phase3s,
              c = segein,
              d = neutral,
              e = schub,
              f = last,
              x1 = calc4Val,
              x2 = calc2Val,
              x3 = calc3Val,
              breaks1 =
                driveTypeBreaks,
              breaks2 = driveTypeBreaks1,
              breaks3 = engSpeedTypeBreaks,
              breaks4 = fuelConsTypeBreaks,
              unit = 1 /
                3600,
              X = X1
            )
          )
        rm(X1)
      }
      assign(varName[1],motorModeCounts)
      # calc5Odo <- getChann1(get(sig5Val), X,sig5min,sig5max)
      # assign(varNameMile[1], c(max(
      #   0,
      #   max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
      # )))
      if (is.null(get(varName))) {
        f_idx <- which(mainConfig[, 1] == 202)
        failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
        failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
        if(length(colName1) == 0){
          failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
        }else if(length(colName2) == 0){
          failSlides[fail_index,4] <-as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
        }
        fail_index <- fail_index + 1
      } else{
        ##saveRDS in the dir location
        g_idx <- which(mainConfig[, 1] == 202)
        GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
        GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
        #summary of Signal 1
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName1 <- c(colName1,colName1)
        col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
        count1 <- sum(length(which(!is.na(X[,col1]))))
        perc1 <- round(count1/nrow(X)*100)
        GoodSlides[good_index,3] <- paste0(col1)
        GoodSlides[good_index,4] <- paste0(count1)
        GoodSlides[good_index,5] <- paste0(perc1,"%")
        #summary of Signal 2 
        colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
        colName2 <- c(colName2,colName2)
        col2 <- colName2[which.max(colSums(!is.na(X[,colName2])))]
        count2 <- sum(length(which(!is.na(X[,col1]))))
        perc2 <- round(count2/nrow(X)*100)
        GoodSlides[good_index,6] <- paste0(col2)
        GoodSlides[good_index,7] <- paste0(count2)
        GoodSlides[good_index,8] <- paste0(perc2,"%")
        #summary of Signal 3 
        colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
        colName3 <- c(colName3,colName3)
        col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
        count3 <- sum(length(which(!is.na(X[,col3]))))
        perc3 <- round(count3/nrow(X)*100)
        GoodSlides[good_index,9] <- paste0(col3)
        GoodSlides[good_index,10] <- paste0(count3)
        GoodSlides[good_index,11] <- paste0(perc3,"%")
        #summary of Signal 4 
        colName4 <- get(sig4Val)[get(sig4Val) %in% names(X)]
        colName4 <- c(colName4,colName4)
        col4 <- colName4[which.max(colSums(!is.na(X[,colName4])))]
        count4 <- sum(length(which(!is.na(X[,col4]))))
        perc4 <- round(count4/nrow(X)*100)
        GoodSlides[good_index,12] <- paste0(col4)
        GoodSlides[good_index,13] <- paste0(count4)
        GoodSlides[good_index,14] <- paste0(perc4,"%")
        # #summary of Signal 5 
        # colName5 <- get(sig5Val)[get(sig5Val) %in% names(X)]
        # colName5 <- c(colName5,colName5)
        # col5 <- colName5[which.max(colSums(!is.na(X[,colName5])))]
        # count5 <- sum(length(which(!is.na(X[,col5]))))
        # perc5 <- round(count5/nrow(X)*100)
        # GoodSlides[good_index,15] <- paste0(col5)
        # GoodSlides[good_index,16] <- paste0(count5)
        # GoodSlides[good_index,17] <- paste0(perc5,"%")
        good_index <- good_index + 1
        saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
        #saveRDS(get(varNameMile[1]), paste0(dir, varNameMile[1], ".rds"))
      }
      endtime <- Sys.time()
      print(paste0(
        "total time for calculating ",
        varName[1],
        " is ",
        endtime - starttime
      ))
      rm(calc1Val)
      rm(calc2Val)
      rm(calc3Val)
      rm(calc4Val)
      rm(motorModeCounts)
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    }
    else if (gid[idx] == 203) {
      #2.2 Streckenprofil/2.2.2 Laufleistung
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      starttime <- Sys.time()
      varName <- as.character(mainConfig[idx, 6])
      #varNameMile <- as.character(paste0(varName, "Mile"))
      calc1Val <- as.character(mainConfig[idx, 10])
      sig1idx <- which(channSigConfig[, 3] == calc1Val)
      sig1Val <- channSigConfig[sig1idx, 4]
      signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
      signal1Vec[signal1Vec == "NA"] <- NA
      signal1Vec <- signal1Vec[!is.na(signal1Vec)]
      assign(sig1Val, signal1Vec)
      # #signal Odo
      # #calc5Odo <- as.character(mainConfig[idx, 14])
      # sig5idx <- which(channSigConfig[, 3] == calc5Odo)
      # sig5Val <- channSigConfig[sig5idx, 4]
      # signal5Vec <- as.character(channSigConfig[sig5idx, 8:25])
      # signal5Vec[signal5Vec == "NA"] <- NA
      # signal5Vec <- signal5Vec[!is.na(signal5Vec)]
      # assign(sig5Val, signal5Vec)
      sig1min <- as.numeric(channSigConfig[sig1idx,5])
      sig1max <- as.numeric(channSigConfig[sig1idx,6])
      # sig5min <- as.numeric(channSigConfig[sig5idx,5])
      # sig5max <- as.numeric(channSigConfig[sig5idx,6])
      #calc5Odo <- getChann1(get(sig5Val), X,sig5min,sig5max)
      di <- NULL
      for (i in 1:length(rleTrack$starts)) {
        X1 <- X[rleTrack$starts[i]:rleTrack$stops[i], ]
        calc1Val <- getChann1(get(sig1Val), X1,sig1min,sig1max)
        di<-c(di,max(
          0,
          max(calc1Val, na.rm = TRUE) - min(calc1Val, na.rm = TRUE)
        ))
      }
      assign(varName[1],di)
      # assign(varNameMile[1], c(max(
      #   0,
      #   max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
      # )))
      if (is.null(get(varName))) {
        f_idx <- which(mainConfig[, 1] == 203)
        failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
        failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
        if(length(colName1) == 0){
          failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
        }else if(length(colName2) == 0){
          failSlides[fail_index,4] <-as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
        }
        fail_index <- fail_index + 1
      } else{
        ##saveRDS in the dir location
        g_idx <- which(mainConfig[, 1] == 203)
        GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
        GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
        #summary of Signal 1
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName1 <- c(colName1,colName1)
        col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
        count1 <- sum(length(which(!is.na(X[,col1]))))
        perc1 <- round(count1/nrow(X)*100)
        GoodSlides[good_index,3] <- paste0(col1)
        GoodSlides[good_index,4] <- paste0(count1)
        GoodSlides[good_index,5] <- paste0(perc1,"%")
        
        #summary of Signal 5 
        # colName5 <- get(sig5Val)[get(sig5Val) %in% names(X)]
        # colName5 <- c(colName5,colName5)
        # col5 <- colName5[which.max(colSums(!is.na(X[,colName5])))]
        # count5 <- sum(length(which(!is.na(X[,col5]))))
        # perc5 <- round(count5/nrow(X)*100)
        # GoodSlides[good_index,15] <- paste0(col5)
        # GoodSlides[good_index,16] <- paste0(count5)
        # GoodSlides[good_index,17] <- paste0(perc5,"%")
        # good_index <- good_index + 1
        saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
        #saveRDS(get(varNameMile[1]), paste0(dir, varNameMile[1], ".rds"))
      }
      endtime <- Sys.time()
      print(paste0(
        "total time for calculating ",
        varName[1],
        " is ",
        endtime - starttime
      ))
      rm(calc1Val)
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    }
    else if (gid[idx] == 204) {
      #2.2 Streckenprofil	2.2.3 Kraftstoffverbrauch
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      starttime <- Sys.time()
      varName <- as.character(mainConfig[idx, 6])
      #varNameMile <- as.character(paste0(varName, "Mile"))
      assign(varName, NULL)
      #assign(varNameMile, NULL)
      calc1Val <- as.character(mainConfig[idx, 9])
      ##calc5Odo <- as.character(mainConfig[idx, 14])
      #signal 1
      sig1idx <- which(channSigConfig[, 3] == calc1Val)
      sig1Val <- channSigConfig[sig1idx, 4]
      signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
      signal1Vec[signal1Vec == "NA"] <- NA
      signal1Vec <- signal1Vec[!is.na(signal1Vec)]
      assign(sig1Val, signal1Vec)
      sig1min <- as.numeric(channSigConfig[sig1idx,5])
      sig1max <- as.numeric(channSigConfig[sig1idx,6])
      # #Signal3 - Odo
      # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
      # sig3Val <- channSigConfig[sig3idx, 4]
      # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
      # signal3Vec[signal3Vec == "NA"] <- NA
      # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
      # assign(sig3Val, signal3Vec)
      # #signal threshold
      sig1min <- as.numeric(channSigConfig[sig1idx,5])
      sig1max <- as.numeric(channSigConfig[sig1idx,6])
      # #sig3min <- as.numeric(channSigConfig[sig3idx,5])
      # #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      
      fuelCons <- NULL
      fuelConsMile <- NULL
      for (ri in 1:length(rleTrack$starts)) {
        X1 <- X[rleTrack$starts[ri]:rleTrack$stops[ri], ]
        #XFuelCons1 <- X1$FuelCons
        calc1Val <- getChann1(get(sig1Val),X1,sig1min,sig1max)
        #calc5Odo <- getChann1(get(sig3Val), X1,sig3min,sig3max)
        #print(paste0(idx," is running"))
        fuelCons <- c(fuelCons, sum(calc1Val, na.rm = TRUE) * 4 / 1e6)
        rm(X1)
        # assign(varNameMile[1], c(get(varNameMile[1]),max(
        #   0, max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
        # )))
      }
      
      if (is.null(get(varName))) {
        f_idx <- which(mainConfig[, 1] == 204)
        failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
        failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
        if(length(colName1) == 0){
          failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
        }else if(length(colName2) == 0){
          failSlides[fail_index,4] <-as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
        }
        fail_index <- fail_index + 1
      } else{
        ##saveRDS in the dir location
        g_idx <- which(mainConfig[, 1] == 204)
        GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
        GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
        #summary of Signal 1
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName1 <- c(colName1,colName1)
        col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
        count1 <- sum(length(which(!is.na(X[,col1]))))
        perc1 <- round(count1/nrow(X)*100)
        GoodSlides[good_index,3] <- paste0(col1)
        GoodSlides[good_index,4] <- paste0(count1)
        GoodSlides[good_index,5] <- paste0(perc1,"%")
        # #summary of Signal 3 
        # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
        # colName3 <- c(colName3,colName3)
        # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
        # count3 <- sum(length(which(!is.na(X[,col3]))))
        # perc3 <- round(count3/nrow(X)*100)
        # GoodSlides[good_index,9] <- paste0(col3)
        # GoodSlides[good_index,10] <- paste0(count3)
        # GoodSlides[good_index,11] <- paste0(perc3,"%")
        good_index <- good_index + 1
        saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
        #saveRDS(get(varNameMile[1]), paste0(dir, varNameMile[1], ".rds"))
      }
      endtime <- Sys.time()
      print(paste0(
        "total time for calculating ",
        varName[1],
        " is ",
        endtime - starttime
      ))
      rm(calc1Val)
      rm(calc5Odo)
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    }
    else if (gid[idx] == 2041){
      #2.2 Streckenprofil	2.2.4 Streckenchronik
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      starttime <- Sys.time()
      varName <- as.character(mainConfig[idx, 6])
      ##varNameMile <- as.character(paste0(varName,"Mile"))
      calc1Val <- as.character(mainConfig[idx, 9])
      #calc5Odo <- as.character(mainConfig[idx, 14])
      #signal 1
      sig1idx <- which(channSigConfig[, 3] == calc1Val)
      sig1Val <- channSigConfig[sig1idx, 4]
      signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
      signal1Vec[signal1Vec == "NA"] <- NA
      signal1Vec <- signal1Vec[!is.na(signal1Vec)]
      assign(sig1Val, signal1Vec)
      # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
      # sig3Val <- channSigConfig[sig3idx, 4]
      # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
      # signal3Vec[signal3Vec == "NA"] <- NA
      # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
      # assign(sig3Val, signal3Vec)
      #signal threshold
      sig1min <- as.numeric(channSigConfig[sig1idx,5])
      sig1max <- as.numeric(channSigConfig[sig1idx,6])
      #sig3min <- as.numeric(channSigConfig[sig3idx,5])
      #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      # calc5Odo <- getChann1(get(sig3Val), X,sig3min,sig3max)
      for (ri in 1:length(rleTrack$starts)) {
        X1 <- X[rleTrack$starts[ri]:rleTrack$stops[ri], ]
        calc1Val <- getChann1(get(sig1Val), X1,sig1min,sig1max)
        Xdata <- data.frame(X1$Odo)
        ySeq <- rbind(ySeq, Xdata)
        yMin <- min(Xdata, na.rm = TRUE)
        yMax <- max(Xdata, na.rm = TRUE)
        yDiff <- max(Xdata, na.rm = TRUE) - min(Xdata, na.rm = TRUE)
        yValue <- cbind(yMin, yMax, yDiff)
        ymdata <- rbind(ymdata, yValue)
        medianXAirTemp <-
          rbind(medianXAirTemp, median(calc1Val, na.rm = TRUE))
        maxXAirTemp <- rbind(maxXAirTemp, max(calc1Val, na.rm = TRUE))
        minXAirTemp <- rbind(minXAirTemp, min(calc1Val, na.rm = TRUE))
        #rm(XAirTemp)
      }
      # assign(varNameMile[1], c(max(
      #   0, max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
      # )))
      endtime <- Sys.time()
      print(paste0(
        "total time for calculating ",
        varName[1],
        " is ",
        endtime - starttime
      ))
      saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
      # saveRDS(get(varNameMile[1]),paste0(dir,varNameMile[1],".rds"))
      saveRDS(medianXAirTemp,paste0(dir,"medianXAirTemp",".rds"))
      saveRDS(maxXAirTemp,paste0(dir,"maxXAirTemp",".rds"))
      saveRDS(minXAirTemp,paste0(dir,"minXAirTemp",".rds"))
      rm(calc1Val)
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    } 
    else if (gid[idx] == 205) {
      #2.3 Drehzahl-Drehmoment-Kennfeld
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      starttime <- Sys.time()
      varName <- as.character(mainConfig[idx, 6])
      # varNameMile <- as.character(paste0(varName, "Mile"))
      assign(varName, NULL)
      # assign(varNameMile, NULL)
      calc1Val <- as.character(mainConfig[idx, 8])
      calc2Val <- as.character(mainConfig[idx, 9])
      #calc5Odo <- as.character(mainConfig[idx, 14])
      #signal 1
      sig1idx <- which(channSigConfig[, 3] == calc1Val)
      sig1Val <- channSigConfig[sig1idx, 4]
      signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
      signal1Vec[signal1Vec == "NA"] <- NA
      signal1Vec <- signal1Vec[!is.na(signal1Vec)]
      assign(sig1Val, signal1Vec)
      #signal 2
      sig2idx <- which(channSigConfig[, 3] == calc2Val)
      sig2Val <- channSigConfig[sig2idx, 4]
      signal2Vec <- as.character(channSigConfig[sig2idx, 8:25])
      signal2Vec[signal2Vec == "NA"] <- NA
      signal2Vec <- signal2Vec[!is.na(signal2Vec)]
      assign(sig2Val, signal2Vec)
      # #Signal3 - Odo
      # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
      # sig3Val <- channSigConfig[sig3idx, 4]
      # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
      # signal3Vec[signal3Vec == "NA"] <- NA
      # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
      # assign(sig3Val, signal3Vec)
      #signal threshold
      sig1min <- as.numeric(channSigConfig[sig1idx,5])
      sig1max <- as.numeric(channSigConfig[sig1idx,6])
      sig2min <- as.numeric(channSigConfig[sig2idx,5])
      sig2max <- as.numeric(channSigConfig[sig2idx,6])
      # #sig3min <- as.numeric(channSigConfig[sig3idx,5])
      # #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      # XEngTor <- getChann(sigEngTor,X,1000)
      calc1Val <- getChann1(get(sig1Val), X,sig1min,sig1max)
      calc2Val <- getChann1(get(sig2Val), X,sig2min,sig2max)
      # calc5Odo <- getChann1(get(sig3Val), X,sig3min,sig3max)
      assign(x = varName[1],
             value = do.call(
               "countTable2D",
               args = list(
                 x = calc1Val,
                 y = calc2Val,
                 mergeTab = get(varName)
               )
             ))
      # assign(varNameMile[1], c(max(
      #   0, max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
      # )))
      if (is.null(get(varName))) {
        f_idx <- which(mainConfig[, 1] == 205)
        failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
        failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
        if(length(colName1) == 0){
          failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
        }else if(length(colName2) == 0){
          failSlides[fail_index,4] <-as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
        }
        fail_index <- fail_index + 1
      } else{
        ##saveRDS in the dir location
        g_idx <- which(mainConfig[, 1] == 205)
        GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
        GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
        #summary of Signal 1
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName1 <- c(colName1,colName1)
        col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
        count1 <- sum(length(which(!is.na(X[,col1]))))
        perc1 <- round(count1/nrow(X)*100)
        GoodSlides[good_index,3] <- paste0(col1)
        GoodSlides[good_index,4] <- paste0(count1)
        GoodSlides[good_index,5] <- paste0(perc1,"%")
        #summary of Signal 2 
        colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
        colName2 <- c(colName2,colName2)
        col2 <- colName2[which.max(colSums(!is.na(X[,colName2])))]
        count2 <- sum(length(which(!is.na(X[,col1]))))
        perc2 <- round(count2/nrow(X)*100)
        GoodSlides[good_index,6] <- paste0(col2)
        GoodSlides[good_index,7] <- paste0(count2)
        GoodSlides[good_index,8] <- paste0(perc2,"%")
        # #summary of Signal 3 
        # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
        # colName3 <- c(colName3,colName3)
        # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
        # count3 <- sum(length(which(!is.na(X[,col3]))))
        # perc3 <- round(count3/nrow(X)*100)
        # GoodSlides[good_index,9] <- paste0(col3)
        # GoodSlides[good_index,10] <- paste0(count3)
        # GoodSlides[good_index,11] <- paste0(perc3,"%")
        
        good_index <- good_index + 1
        saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
        saveRDS(get(varNameMile[1]), paste0(dir, varNameMile[1], ".rds"))
      }
      ##remove the variables to free up the memory
      endtime <- Sys.time()
      print(paste0(
        "total time for calculating ",
        varName[1],
        " is ",
        endtime - starttime
      ))
      rm(calc1Val, calc2Val, calc5Odo)
      rm(list = paste0(varName[1]))
      # rm(list = paste0(varNameMile[1]))
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    }
    else if (gid[idx] == 206) {
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      #2.4 Fahrzeuggeschwindigkeit-Motorleistung-Kennfeld
      starttime <- Sys.time()
      varName <- as.character(mainConfig[idx, 6])
      # varNameMile <- as.character(paste0(varName, "Mile"))
      assign(varName, NULL)
      # assign(varNameMile, NULL)
      calc1Val <- as.character(mainConfig[idx, 8])
      calc2Val <- as.character(mainConfig[idx, 10])
      calc4Val <- as.character(mainConfig[idx, 13])
      #calc5Odo <- as.character(mainConfig[idx, 14])
      #signal 1
      sig1idx <- which(channSigConfig[, 3] == calc1Val)
      sig1Val <- channSigConfig[sig1idx, 4]
      signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
      signal1Vec[signal1Vec == "NA"] <- NA
      signal1Vec <- signal1Vec[!is.na(signal1Vec)]
      assign(sig1Val, signal1Vec)
      #signal 2
      sig2idx <- which(channSigConfig[, 3] == calc2Val)
      sig2Val <- channSigConfig[sig2idx, 4]
      signal2Vec <- as.character(channSigConfig[sig2idx, 8:25])
      signal2Vec[signal2Vec == "NA"] <- NA
      signal2Vec <- signal2Vec[!is.na(signal2Vec)]
      assign(sig2Val, signal2Vec)
      # #Signal3 - Odo
      # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
      # sig3Val <- channSigConfig[sig3idx, 4]
      # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
      # signal3Vec[signal3Vec == "NA"] <- NA
      # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
      # assign(sig3Val, signal3Vec)
      #Signal 4
      sig4idx <- which(channSigConfig[, 3] == calc4Val)
      sig4Val <- channSigConfig[sig4idx, 4]
      signal4Vec <- as.character(channSigConfig[sig4idx, 8:25])
      signal4Vec[signal4Vec == "NA"] <- NA
      signal4Vec <- signal4Vec[!is.na(signal4Vec)]
      assign(sig4Val, signal4Vec)
      #signal threshold
      sig1min <- as.numeric(channSigConfig[sig1idx,5])
      sig1max <- as.numeric(channSigConfig[sig1idx,6])
      sig2min <- as.numeric(channSigConfig[sig2idx,5])
      sig2max <- as.numeric(channSigConfig[sig2idx,6])
      # #sig3min <- as.numeric(channSigConfig[sig3idx,5])
      # #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      sig4min <- as.numeric(channSigConfig[sig4idx,5])
      sig4max <- as.numeric(channSigConfig[sig4idx,6])
      
      # XEngTor <- getChann(sigEngTor,X,1000)
      calc1Val <- getChann1(get(sig1Val), X,sig1min,sig1max)
      calc2Val <- getChann1(get(sig2Val), X,sig2min,sig2max)
      # calc5Odo <- getChann1(get(sig3Val), X,sig3min,sig3max)
      calc4Val <- getChann1(get(sig4Val), X,sig4min,sig4max)
      #Operating Time Calc
      #isOpTime <- (!is.na(calc4Val) & calc4Val>0)
      assign(x = varName[1],
             value = do.call(
               "countTable2D",
               args = list(
                 x = calc1Val[!is.na(calc4Val) &
                                calc4Val > 0],
                 y = pi / 3e4 * calc4Val[!is.na(calc4Val) &
                                           calc4Val > 0] * calc2Val[!is.na(calc4Val) &
                                                                      calc4Val > 0],
                 mergeTab = get(varName)
               )
             ))
      # assign(varNameMile[1], c(max(
      #   0, max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
      # )))
      if (is.null(get(varName))) {
        f_idx <- which(mainConfig[, 1] == 206)
        failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
        failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
        if(length(colName1) == 0){
          failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
        }else if(length(colName2) == 0){
          failSlides[fail_index,4] <-as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
        }
        fail_index <- fail_index + 1
      } else{
        ##saveRDS in the dir location
        g_idx <- which(mainConfig[, 1] == 206)
        GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
        GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
        #summary of Signal 1
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName1 <- c(colName1,colName1)
        col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
        count1 <- sum(length(which(!is.na(X[,col1]))))
        perc1 <- round(count1/nrow(X)*100)
        GoodSlides[good_index,3] <- paste0(col1)
        GoodSlides[good_index,4] <- paste0(count1)
        GoodSlides[good_index,5] <- paste0(perc1,"%")
        #summary of Signal 2 
        colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
        colName2 <- c(colName2,colName2)
        col2 <- colName2[which.max(colSums(!is.na(X[,colName2])))]
        count2 <- sum(length(which(!is.na(X[,col1]))))
        perc2 <- round(count2/nrow(X)*100)
        GoodSlides[good_index,6] <- paste0(col2)
        GoodSlides[good_index,7] <- paste0(count2)
        GoodSlides[good_index,8] <- paste0(perc2,"%")
        # #summary of Signal 3 
        # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
        # colName3 <- c(colName3,colName3)
        # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
        # count3 <- sum(length(which(!is.na(X[,col3]))))
        # perc3 <- round(count3/nrow(X)*100)
        # GoodSlides[good_index,9] <- paste0(col3)
        # GoodSlides[good_index,10] <- paste0(count3)
        # GoodSlides[good_index,11] <- paste0(perc3,"%")
        #summary of Signal 4 
        colName4 <- get(sig4Val)[get(sig4Val) %in% names(X)]
        colName4 <- c(colName4,colName4)
        col4 <- colName4[which.max(colSums(!is.na(X[,colName4])))]
        count4 <- sum(length(which(!is.na(X[,col4]))))
        perc4 <- round(count4/nrow(X)*100)
        GoodSlides[good_index,12] <- paste0(col4)
        GoodSlides[good_index,13] <- paste0(count4)
        GoodSlides[good_index,14] <- paste0(perc4,"%")
        
        good_index <- good_index + 1
        saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
        # saveRDS(get(varNameMile[1]), paste0(dir, varNameMile[1], ".rds"))
      }
      ##remove the variables to free up the memory
      endtime <- Sys.time()
      print(paste0(
        "total time for calculating ",
        varName[1],
        " is ",
        endtime - starttime
      ))
      rm(calc1Val, calc2Val, calc5Odo)
      rm(list = paste0(varName[1]))
      # rm(list = paste0(varNameMile[1]))
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    }
    else if (gid[idx] == 2061){
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      #2.4 Fahrzeuggeschwindigkeit	2.4.1 Verteilung der Fahrzeuggeschwindigkeit
      starttime <- Sys.time()
      varName <- as.character(mainConfig[idx, 6])
      # varNameMile <- as.character(paste0(varName, "Mile"))
      assign(varName, NULL)
      # assign(varNameMile, NULL)
      calc1Val <- as.character(mainConfig[idx, 8])
      #calc5Odo <- as.character(mainConfig[idx, 14])
      #signal 1
      sig1idx <- which(channSigConfig[, 3] == calc1Val)
      sig1Val <- channSigConfig[sig1idx, 4]
      signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
      signal1Vec[signal1Vec == "NA"] <- NA
      signal1Vec <- signal1Vec[!is.na(signal1Vec)]
      assign(sig1Val, signal1Vec)
      # #Signal3 - Odo
      # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
      # sig3Val <- channSigConfig[sig3idx, 4]
      # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
      # signal3Vec[signal3Vec == "NA"] <- NA
      # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
      # assign(sig3Val, signal3Vec)
      #Signal 4
      # XEngTor <- getChann(sigEngTor,X,1000)
      #signal threshold
      sig1min <- as.numeric(channSigConfig[sig1idx,5])
      sig1max <- as.numeric(channSigConfig[sig1idx,6])
      sig2min <- as.numeric(channSigConfig[sig2idx,5])
      sig2max <- as.numeric(channSigConfig[sig2idx,6])
      # #sig3min <- as.numeric(channSigConfig[sig3idx,5])
      # #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      calc1Val <- getChann1(get(sig1Val), X,sig1min,sig1max)
      # calc5Odo <- getChann1(get(sig3Val), X,sig3min,sig3max)
      assign(x = varName[1],
             value = do.call("countTable1D1", args = list(
               x = calc1Val, mergeTab = get(varName)
             )))
      # assign(varNameMile[1], c(max(
      #   0, max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
      # )))
      if (is.null(get(varName))) {
        f_idx <- which(mainConfig[, 1] == 2061)
        failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
        failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
        if(length(colName1) == 0){
          failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
        }else if(length(colName2) == 0){
          failSlides[fail_index,4] <-as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
        }
        fail_index <- fail_index + 1
      } else{
        ##saveRDS in the dir location
        g_idx <- which(mainConfig[, 1] == 2061)
        GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
        GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
        #summary of Signal 1
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName1 <- c(colName1,colName1)
        col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
        count1 <- sum(length(which(!is.na(X[,col1]))))
        perc1 <- round(count1/nrow(X)*100)
        GoodSlides[good_index,3] <- paste0(col1)
        GoodSlides[good_index,4] <- paste0(count1)
        GoodSlides[good_index,5] <- paste0(perc1,"%")
        #summary of Signal 2 
        colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
        colName2 <- c(colName2,colName2)
        col2 <- colName2[which.max(colSums(!is.na(X[,colName2])))]
        count2 <- sum(length(which(!is.na(X[,col1]))))
        perc2 <- round(count2/nrow(X)*100)
        GoodSlides[good_index,6] <- paste0(col2)
        GoodSlides[good_index,7] <- paste0(count2)
        GoodSlides[good_index,8] <- paste0(perc2,"%")
        # #summary of Signal 3 
        # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
        # colName3 <- c(colName3,colName3)
        # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
        # count3 <- sum(length(which(!is.na(X[,col3]))))
        # perc3 <- round(count3/nrow(X)*100)
        # GoodSlides[good_index,9] <- paste0(col3)
        # GoodSlides[good_index,10] <- paste0(count3)
        # GoodSlides[good_index,11] <- paste0(perc3,"%")
        
        good_index <- good_index + 1
        saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
        # saveRDS(get(varNameMile[1]), paste0(dir, varNameMile[1], ".rds"))
      }
      ##remove the variables to free up the memory
      endtime <- Sys.time()
      print(paste0(
        "total time for calculating ",
        varName[1],
        " is ",
        endtime - starttime
      ))
      rm(calc1Val, calc5Odo)
      rm(list = paste0(varName[1]))
      # rm(list = paste0(varNameMile[1]))
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    } 
    else if (gid[idx] == 207) {
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      #2.5 Leerlaufzeit
      starttime <- Sys.time()
      varName <- as.character(mainConfig[idx, 6])
      # varNameMile <- as.character(paste0(varName, "Mile"))
      assign(varName, NULL)
      # assign(varNameMile, NULL)
      calc2Val <- as.character(mainConfig[idx, 13])
      ##calc5Odo <- as.character(mainConfig[idx, 14])
      #signal 2
      sig2idx <- which(channSigConfig[, 3] == calc2Val)
      sig2Val <- channSigConfig[sig2idx, 4]
      signal2Vec <- as.character(channSigConfig[sig2idx, 8:25])
      signal2Vec[signal2Vec == "NA"] <- NA
      signal2Vec <- signal2Vec[!is.na(signal2Vec)]
      assign(sig2Val, signal2Vec)
      # #Signal3 - Odo
      # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
      # sig3Val <- channSigConfig[sig3idx, 4]
      # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
      # signal3Vec[signal3Vec == "NA"] <- NA
      # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
      # assign(sig3Val, signal3Vec)
      #signal threshold
      sig1min <- as.numeric(channSigConfig[sig1idx,5])
      sig1max <- as.numeric(channSigConfig[sig1idx,6])
      sig2min <- as.numeric(channSigConfig[sig2idx,5])
      sig2max <- as.numeric(channSigConfig[sig2idx,6])
      # #sig3min <- as.numeric(channSigConfig[sig3idx,5])
      # #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      
      # calc5Odo <- getChann1(get(sig3Val), X,sig3min,sig3max)
      for (ri in 1:length(rleTrack$starts)) {
        X1 <- X[rleTrack$starts[ri]:rleTrack$stops[ri], ]
        
        calc2Val <- getChann1(get(sig2Val), X1,sig2min,sig2max)
        idleModeCounts <-
          rbind(
            idleModeCounts,
            countFreq1D(
              x = calc2Val,
              breaks = idleModeBreaks,
              unitFactor = 1 / 3600
            )$count
          )
      }
      assign(varName[1], idleModeCounts)
      # assign(varNameMile[1], c(max(
      #   0, max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
      # )))
      if (is.null(get(varName))) {
        f_idx <- which(mainConfig[, 1] == 207)
        failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
        failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
        if(length(colName1) == 0){
          failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
        }else if(length(colName2) == 0){
          failSlides[fail_index,4] <-as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
        }
        fail_index <- fail_index + 1
      } else{
        ##saveRDS in the dir location
        g_idx <- which(mainConfig[, 1] == 207)
        GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
        GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
        
        #summary of Signal 2 
        colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
        colName2 <- c(colName2,colName2)
        col2 <- colName2[which.max(colSums(!is.na(X[,colName2])))]
        count2 <- sum(length(which(!is.na(X[,col1]))))
        perc2 <- round(count2/nrow(X)*100)
        GoodSlides[good_index,6] <- paste0(col2)
        GoodSlides[good_index,7] <- paste0(count2)
        GoodSlides[good_index,8] <- paste0(perc2,"%")
        # #summary of Signal 3 
        # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
        # colName3 <- c(colName3,colName3)
        # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
        # count3 <- sum(length(which(!is.na(X[,col3]))))
        # perc3 <- round(count3/nrow(X)*100)
        # GoodSlides[good_index,9] <- paste0(col3)
        # GoodSlides[good_index,10] <- paste0(count3)
        # GoodSlides[good_index,11] <- paste0(perc3,"%")
        
        good_index <- good_index + 1
        saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
        # saveRDS(get(varNameMile[1]), paste0(dir, varNameMile[1], ".rds"))
      }
      rm(calc2Val)
      # rm(calc5Odo)
      rm(idleModeCounts)
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    }
    else if (gid[idx] == 208) {
      #2.6 Volllastbeschleunigung
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      starttime <- Sys.time()
      varName <- as.character(mainConfig[idx, 6])
      # varNameMile <- as.character(paste0(varName, "Mile"))
      assign(varName, NULL)
      # assign(varNameMile, NULL)
      calc1Val <- as.character(mainConfig[idx, 10])
      calc2Val <- as.character(mainConfig[idx, 13])
      #calc5Odo <- as.character(mainConfig[idx, 14])
      #signal 1
      sig1idx <- which(channSigConfig[, 3] == calc1Val)
      sig1Val <- channSigConfig[sig1idx, 4]
      signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
      signal1Vec[signal1Vec == "NA"] <- NA
      signal1Vec <- signal1Vec[!is.na(signal1Vec)]
      assign(sig1Val, signal1Vec)
      #signal 2
      sig2idx <- which(channSigConfig[, 3] == calc2Val)
      sig2Val <- channSigConfig[sig2idx, 4]
      signal2Vec <- as.character(channSigConfig[sig2idx, 8:25])
      signal2Vec[signal2Vec == "NA"] <- NA
      signal2Vec <- signal2Vec[!is.na(signal2Vec)]
      assign(sig2Val, signal2Vec)
      # #Signal3 - Odo
      # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
      # sig3Val <- channSigConfig[sig3idx, 4]
      # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
      # signal3Vec[signal3Vec == "NA"] <- NA
      # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
      # assign(sig3Val, signal3Vec)
      #signal threshold
      sig1min <- as.numeric(channSigConfig[sig1idx,5])
      sig1max <- as.numeric(channSigConfig[sig1idx,6])
      sig2min <- as.numeric(channSigConfig[sig2idx,5])
      sig2max <- as.numeric(channSigConfig[sig2idx,6])
      # #sig3min <- as.numeric(channSigConfig[sig3idx,5])
      # #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      
      # XEngTor <- getChann(sigEngTor,X,1000)
      calc1Val <- getChann1(get(sig1Val), X,sig1min,sig1max)
      calc2Val <- getChann1(get(sig2Val), X,sig2min,sig2max)
      #calc5Odo <- getChann1(get(sig3Val), X,sig3min,sig3max)
      isOpTime <- (!is.na(calc2Val) & (calc2Val)>0)
      incalc <-
        durationTime(
          x = calc1Val[isOpTime],
          y = calc2Val[isOpTime],
          breaksX = pedHighBreaks1,
          breaksY = pedHighBreaks2
        )
      assign(x = varName[1],
             value = do.call("countTable1D", args = list(
               x = incalc, mergeTab = get(varName)
             )))
      # assign(varNameMile[1], c(max(
      #   0, max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
      # )))
      if (is.null(get(varName))) {
        f_idx <- which(mainConfig[, 1] == 208)
        failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
        failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
        if(length(colName1) == 0){
          failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
        }else if(length(colName2) == 0){
          failSlides[fail_index,4] <-as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
        }
        fail_index <- fail_index + 1
      } else{
        ##saveRDS in the dir location
        g_idx <- which(mainConfig[, 1] == 208)
        GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
        GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
        #summary of Signal 1
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName1 <- c(colName1,colName1)
        col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
        count1 <- sum(length(which(!is.na(X[,col1]))))
        perc1 <- round(count1/nrow(X)*100)
        GoodSlides[good_index,3] <- paste0(col1)
        GoodSlides[good_index,4] <- paste0(count1)
        GoodSlides[good_index,5] <- paste0(perc1,"%")
        #summary of Signal 2 
        colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
        colName2 <- c(colName2,colName2)
        col2 <- colName2[which.max(colSums(!is.na(X[,colName2])))]
        count2 <- sum(length(which(!is.na(X[,col1]))))
        perc2 <- round(count2/nrow(X)*100)
        GoodSlides[good_index,6] <- paste0(col2)
        GoodSlides[good_index,7] <- paste0(count2)
        GoodSlides[good_index,8] <- paste0(perc2,"%")
        # #summary of Signal 3 
        # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
        # colName3 <- c(colName3,colName3)
        # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
        # count3 <- sum(length(which(!is.na(X[,col3]))))
        # perc3 <- round(count3/nrow(X)*100)
        # GoodSlides[good_index,9] <- paste0(col3)
        # GoodSlides[good_index,10] <- paste0(count3)
        # GoodSlides[good_index,11] <- paste0(perc3,"%")
        
        good_index <- good_index + 1
        saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
        # saveRDS(get(varNameMile[1]), paste0(dir, varNameMile[1], ".rds"))
      }
      ##remove the variables to free up the memory
      endtime <- Sys.time()
      print(paste0(
        "total time for calculating ",
        varName[1],
        " is ",
        endtime - starttime
      ))
      rm(calc1Val, calc2Val, calc5Odo, incalc)
      rm(list = paste0(varName[1]))
      # rm(list = paste0(varNameMile[1]))
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    } 
    else if (gid[idx] == 209) {
      #2.7 LÃ¤ngs-/Querbeschleunigung-Kennfeld
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      starttime <- Sys.time()
      varName <- as.character(mainConfig[idx, 6])
      # varNameMile <- as.character(paste0(varName, "Mile"))
      assign(varName, NULL)
      # assign(varNameMile, NULL)
      calc1Val <- as.character(mainConfig[idx, 8])
      calc2Val <- as.character(mainConfig[idx, 9])
      calc4Val <- as.character(mainConfig[idx, 13])
      #calc5Odo <- as.character(mainConfig[idx, 14])
      #signal 1
      sig1idx <- which(channSigConfig[, 3] == calc1Val)
      sig1Val <- channSigConfig[sig1idx, 4]
      signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
      signal1Vec[signal1Vec == "NA"] <- NA
      signal1Vec <- signal1Vec[!is.na(signal1Vec)]
      assign(sig1Val, signal1Vec)
      #signal 2
      sig2idx <- which(channSigConfig[, 3] == calc2Val)
      sig2Val <- channSigConfig[sig2idx, 4]
      signal2Vec <- as.character(channSigConfig[sig2idx, 8:25])
      signal2Vec[signal2Vec == "NA"] <- NA
      signal2Vec <- signal2Vec[!is.na(signal2Vec)]
      assign(sig2Val, signal2Vec)
      # #Signal3 - Odo
      # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
      # sig3Val <- channSigConfig[sig3idx, 4]
      # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
      # signal3Vec[signal3Vec == "NA"] <- NA
      # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
      # assign(sig3Val, signal3Vec)
      #Signal 4
      sig4idx <- which(channSigConfig[, 3] == calc4Val)
      sig4Val <- channSigConfig[sig4idx, 4]
      signal4Vec <- as.character(channSigConfig[sig4idx, 8:25])
      signal4Vec[signal4Vec == "NA"] <- NA
      signal4Vec <- signal4Vec[!is.na(signal4Vec)]
      assign(sig4Val, signal4Vec)
      #signal threshold
      sig1min <- as.numeric(channSigConfig[sig1idx,5])
      sig1max <- as.numeric(channSigConfig[sig1idx,6])
      sig2min <- as.numeric(channSigConfig[sig2idx,5])
      sig2max <- as.numeric(channSigConfig[sig2idx,6])
      # #sig3min <- as.numeric(channSigConfig[sig3idx,5])
      # #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      sig4min <- as.numeric(channSigConfig[sig4idx,5])
      sig4max <- as.numeric(channSigConfig[sig4idx,6])
      
      # XEngTor <- getChann(sigEngTor,X,1000)
      calc1Val <- getChann1(get(sig1Val), X,sig1min,sig1max)
      calc2Val <- getChann1(get(sig2Val), X,sig2min,sig2max)
      # calc5Odo <- getChann1(get(sig3Val), X,sig3min,sig3max)
      calc4Val <- getChann1(get(sig4Val), X,sig4min,sig4max)
      #Operating Time Calc
      assign(x = varName[1],
             value = do.call(
               "countTable2D",
               args = list(
                 x = calc1Val[calc4Val > 0],
                 y = calc2Val[calc4Val > 0],
                 mergeTab = get(varName)
               )
             ))
      # assign(varNameMile[1], c(max(
      #   0, max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
      # )))
      if (is.null(get(varName))) {
        f_idx <- which(mainConfig[, 1] == 209)
        failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
        failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
        if(length(colName1) == 0){
          failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
        }else if(length(colName2) == 0){
          failSlides[fail_index,4] <-as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
        }
        fail_index <- fail_index + 1
      } else{
        ##saveRDS in the dir location
        g_idx <- which(mainConfig[, 1] == 209)
        GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
        GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
        #summary of Signal 1
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName1 <- c(colName1,colName1)
        col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
        count1 <- sum(length(which(!is.na(X[,col1]))))
        perc1 <- round(count1/nrow(X)*100)
        GoodSlides[good_index,3] <- paste0(col1)
        GoodSlides[good_index,4] <- paste0(count1)
        GoodSlides[good_index,5] <- paste0(perc1,"%")
        #summary of Signal 2 
        colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
        colName2 <- c(colName2,colName2)
        col2 <- colName2[which.max(colSums(!is.na(X[,colName2])))]
        count2 <- sum(length(which(!is.na(X[,col1]))))
        perc2 <- round(count2/nrow(X)*100)
        GoodSlides[good_index,6] <- paste0(col2)
        GoodSlides[good_index,7] <- paste0(count2)
        GoodSlides[good_index,8] <- paste0(perc2,"%")
        # #summary of Signal 3 
        # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
        # colName3 <- c(colName3,colName3)
        # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
        # count3 <- sum(length(which(!is.na(X[,col3]))))
        # perc3 <- round(count3/nrow(X)*100)
        # GoodSlides[good_index,9] <- paste0(col3)
        # GoodSlides[good_index,10] <- paste0(count3)
        # GoodSlides[good_index,11] <- paste0(perc3,"%")
        #summary of Signal 4 
        colName4 <- get(sig4Val)[get(sig4Val) %in% names(X)]
        colName4 <- c(colName4,colName4)
        col4 <- colName4[which.max(colSums(!is.na(X[,colName4])))]
        count4 <- sum(length(which(!is.na(X[,col4]))))
        perc4 <- round(count4/nrow(X)*100)
        GoodSlides[good_index,12] <- paste0(col4)
        GoodSlides[good_index,13] <- paste0(count4)
        GoodSlides[good_index,14] <- paste0(perc4,"%")
        #summary of Signal 5 
        
        good_index <- good_index + 1
        saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
        # saveRDS(get(varNameMile[1]), paste0(dir, varNameMile[1], ".rds"))
      }
      ##remove the variables to free up the memory
      endtime <- Sys.time()
      print(paste0(
        "total time for calculating ",
        varName[1],
        " is ",
        endtime - starttime
      ))
      rm(calc1Val, calc2Val, calc4Val, calc5Odo)
      rm(list = paste0(varName[1]))
      # rm(list = paste0(varNameMile[1]))
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    } 
    else if (gid[idx] == 210) {
      #2.8 Höhenprofile
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      starttime <- Sys.time()
      varName <- as.character(mainConfig[idx, 6])
      # varNameMile <- as.character(paste0(varName, "Mile"))
      assign(varName, NULL)
      # assign(varNameMile, NULL)
      calc1Val <- as.character(mainConfig[idx, 10])
      calc2Val <- as.character(mainConfig[idx, 11])
      calc4Val <- as.character(mainConfig[idx, 13])
      #calc5Odo <- as.character(mainConfig[idx, 14])
      #signal 1
      sig1idx <- which(channSigConfig[, 3] == calc1Val)
      sig1Val <- channSigConfig[sig1idx, 4]
      signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
      signal1Vec[signal1Vec == "NA"] <- NA
      signal1Vec <- signal1Vec[!is.na(signal1Vec)]
      assign(sig1Val, signal1Vec)
      #signal 2
      sig2idx <- which(channSigConfig[, 3] == calc2Val)
      sig2Val <- channSigConfig[sig2idx, 4]
      signal2Vec <- as.character(channSigConfig[sig2idx, 8:25])
      signal2Vec[signal2Vec == "NA"] <- NA
      signal2Vec <- signal2Vec[!is.na(signal2Vec)]
      assign(sig2Val, signal2Vec)
      # #Signal3 - Odo
      # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
      # sig3Val <- channSigConfig[sig3idx, 4]
      # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
      # signal3Vec[signal3Vec == "NA"] <- NA
      # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
      assign(sig3Val, signal3Vec)
      #Signal 4
      sig4idx <- which(channSigConfig[, 3] == calc4Val)
      sig4Val <- channSigConfig[sig4idx, 4]
      signal4Vec <- as.character(channSigConfig[sig4idx, 8:25])
      signal4Vec[signal4Vec == "NA"] <- NA
      signal4Vec <- signal4Vec[!is.na(signal4Vec)]
      assign(sig4Val, signal4Vec)
      #signal threshold
      sig1min <- as.numeric(channSigConfig[sig1idx,5])
      sig1max <- as.numeric(channSigConfig[sig1idx,6])
      sig2min <- as.numeric(channSigConfig[sig2idx,5])
      sig2max <- as.numeric(channSigConfig[sig2idx,6])
      # #sig3min <- as.numeric(channSigConfig[sig3idx,5])
      # #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      sig4min <- as.numeric(channSigConfig[sig4idx,5])
      sig4max <- as.numeric(channSigConfig[sig4idx,6])
      
      
      calc1Val <- getChann1(get(sig1Val), X,sig1min,sig1max)
      calc2Val <- getChann1(get(sig2Val), X,sig2min,sig2max)
      # calc5Odo <- getChann1(get(sig3Val), X,sig3min,sig3max)
      calc4Val <- getChann1(get(sig4Val), X,sig4min,sig4max)
      #Operating Time Calc
      #assign(gpsAltCorr,NULL)
      gpsAltCorr <-
        smoothGpsAlt(gpsAlt = calc2Val[calc4Val > 0], gpsPrec = calc1Val[calc4Val >
                                                                           0])
      #assign(x = gpsAltCorr,value = do.call("smoothGpsAlt",args = list(gpsAlt=calc1Val[calc4Val>0],gpsPrec=calc2Val[calc4Val>0])))
      assign(x = varName[1],
             value = do.call("countTable1D", args = list(
               x = gpsAltCorr, mergeTab = get(varName)
             )))
      # assign(varNameMile[1], c(max(
      #   0, max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
      # )))
      if (is.null(get(varName))) {
        f_idx <- which(mainConfig[, 1] == 210)
        failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
        failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
        if(length(colName1) == 0){
          failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
        }else if(length(colName2) == 0){
          failSlides[fail_index,4] <-as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
        }
        fail_index <- fail_index + 1
      } else{
        ##saveRDS in the dir location
        g_idx <- which(mainConfig[, 1] == 210)
        GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
        GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
        #summary of Signal 1
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName1 <- c(colName1,colName1)
        col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
        count1 <- sum(length(which(!is.na(X[,col1]))))
        perc1 <- round(count1/nrow(X)*100)
        GoodSlides[good_index,3] <- paste0(col1)
        GoodSlides[good_index,4] <- paste0(count1)
        GoodSlides[good_index,5] <- paste0(perc1,"%")
        #summary of Signal 2 
        colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
        colName2 <- c(colName2,colName2)
        col2 <- colName2[which.max(colSums(!is.na(X[,colName2])))]
        count2 <- sum(length(which(!is.na(X[,col1]))))
        perc2 <- round(count2/nrow(X)*100)
        GoodSlides[good_index,6] <- paste0(col2)
        GoodSlides[good_index,7] <- paste0(count2)
        GoodSlides[good_index,8] <- paste0(perc2,"%")
        # #summary of Signal 3 
        # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
        # colName3 <- c(colName3,colName3)
        # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
        # count3 <- sum(length(which(!is.na(X[,col3]))))
        # perc3 <- round(count3/nrow(X)*100)
        # GoodSlides[good_index,9] <- paste0(col3)
        # GoodSlides[good_index,10] <- paste0(count3)
        # GoodSlides[good_index,11] <- paste0(perc3,"%")
        #summary of Signal 4 
        colName4 <- get(sig4Val)[get(sig4Val) %in% names(X)]
        colName4 <- c(colName4,colName4)
        col4 <- colName4[which.max(colSums(!is.na(X[,colName4])))]
        count4 <- sum(length(which(!is.na(X[,col4]))))
        perc4 <- round(count4/nrow(X)*100)
        GoodSlides[good_index,12] <- paste0(col4)
        GoodSlides[good_index,13] <- paste0(count4)
        GoodSlides[good_index,14] <- paste0(perc4,"%")
        
        good_index <- good_index + 1
        saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
        # saveRDS(get(varNameMile[1]), paste0(dir, varNameMile[1], ".rds"))
      }
      ##remove the variables to free up the memory
      endtime <- Sys.time()
      print(paste0(
        "total time for calculating ",
        varName[1],
        " is ",
        endtime - starttime
      ))
      rm(calc1Val, calc2Val, calc4Val, calc5Odo, gpsAltCorr)
      rm(list = paste0(varName[1]))
      # rm(list = paste0(varNameMile[1]))
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    } 
    else if (gid[idx] == 211) {
      #2.9 Schubbetrieb	2.9.1 Schubbetriebszeit
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      starttime <- Sys.time()
      varName <- as.character(mainConfig[idx, 6])
      # varNameMile <- as.character(paste0(varName, "Mile"))
      assign(varName, NULL)
      # assign(varNameMile, NULL)
      calc1Val <- as.character(mainConfig[idx, 10])
      calc4Val <- as.character(mainConfig[idx, 13])
      #calc5Odo <- as.character(mainConfig[idx, 14])
      #signal 1
      sig1idx <- which(channSigConfig[, 3] == calc1Val)
      sig1Val <- channSigConfig[sig1idx, 4]
      signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
      signal1Vec[signal1Vec == "NA"] <- NA
      signal1Vec <- signal1Vec[!is.na(signal1Vec)]
      assign(sig1Val, signal1Vec)
      # #Signal3 - Odo
      # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
      # sig3Val <- channSigConfig[sig3idx, 4]
      # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
      # signal3Vec[signal3Vec == "NA"] <- NA
      # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
      # assign(sig3Val, signal3Vec)
      #Signal 4
      sig4idx <- which(channSigConfig[, 3] == calc4Val)
      sig4Val <- channSigConfig[sig4idx, 4]
      signal4Vec <- as.character(channSigConfig[sig4idx, 8:25])
      signal4Vec[signal4Vec == "NA"] <- NA
      signal4Vec <- signal4Vec[!is.na(signal4Vec)]
      assign(sig4Val, signal4Vec)
      #signal threshold
      sig1min <- as.numeric(channSigConfig[sig1idx,5])
      sig1max <- as.numeric(channSigConfig[sig1idx,6])
      
      # #sig3min <- as.numeric(channSigConfig[sig3idx,5])
      # #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      sig4min <- as.numeric(channSigConfig[sig4idx,5])
      sig4max <- as.numeric(channSigConfig[sig4idx,6])
      
      # calc5Odo <- getChann1(get(sig3Val), X,sig3min,sig3max)
      
      #result <- NULL
      pushModeCounts <- NULL
      for (ri in 1:length(rleTrack$starts)){
        X1 <- X[rleTrack$starts[ri]:rleTrack$stops[ri], ]
        calc1Val <- getChann1(get(sig1Val), X1,sig1min,sig1max)
        calc4Val <- getChann1(get(sig4Val), X1,sig4min,sig4max)
        pushModeCountsHelp1 <-
          countFreq2D(
            x = calc1Val,
            y = calc4Val,
            breaksX = pushModeBreaks1,
            breaksY = pushModeBreaks2,
            unitFactor = 1 / 3600
          )$count
        pushModeCountsHelp2 <- pushModeCountsHelp1[2, ]
        pushModeCountsHelp2[2] <-
          pushModeCountsHelp2[2] + sum(pushModeCountsHelp1[1, ])
        #print(pushModeCountsHelp2)
        pushModeCounts <- rbind(pushModeCounts, pushModeCountsHelp2)
        rm(X1)
        #print(result)
      }
      assign(x = varName[1], value = pushModeCounts)
      # assign(varNameMile[1], c(max(
      #   0, max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
      # )))
      
      if (is.null(get(varName))) {
        f_idx <- which(mainConfig[, 1] == 211)
        failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
        failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
        if(length(colName1) == 0){
          failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
        }else if(length(colName2) == 0){
          failSlides[fail_index,4] <-as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
        }
        fail_index <- fail_index + 1
      } else{
        ##saveRDS in the dir locationâ
        g_idx <- which(mainConfig[, 1] == 211)
        GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
        GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
        #summary of Signal 1
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName1 <- c(colName1,colName1)
        col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
        count1 <- sum(length(which(!is.na(X[,col1]))))
        perc1 <- round(count1/nrow(X)*100)
        GoodSlides[good_index,3] <- paste0(col1)
        GoodSlides[good_index,4] <- paste0(count1)
        GoodSlides[good_index,5] <- paste0(perc1,"%")
        # 
        # #summary of Signal 3 
        # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
        # colName3 <- c(colName3,colName3)
        # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
        # count3 <- sum(length(which(!is.na(X[,col3]))))
        # perc3 <- round(count3/nrow(X)*100)
        # GoodSlides[good_index,9] <- paste0(col3)
        # GoodSlides[good_index,10] <- paste0(count3)
        # GoodSlides[good_index,11] <- paste0(perc3,"%")
        #summary of Signal 4 
        colName4 <- get(sig4Val)[get(sig4Val) %in% names(X)]
        colName4 <- c(colName4,colName4)
        col4 <- colName4[which.max(colSums(!is.na(X[,colName4])))]
        count4 <- sum(length(which(!is.na(X[,col4]))))
        perc4 <- round(count4/nrow(X)*100)
        GoodSlides[good_index,12] <- paste0(col4)
        GoodSlides[good_index,13] <- paste0(count4)
        GoodSlides[good_index,14] <- paste0(perc4,"%")
        
        good_index <- good_index + 1
        saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
        # saveRDS(get(varNameMile[1]), paste0(dir, varNameMile[1], ".rds"))
      }
      ##remove the variables to free up the memory
      endtime <- Sys.time()
      print(paste0(
        "total time for calculating ",
        varName[1],
        " is ",
        endtime - starttime
      ))
      rm(calc1Val, calc2Val, calc4Val, calc5Odo)
      rm(list = paste0(varName[1]))
      # rm(list = paste0(varNameMile[1]))
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    }
    else if (gid[idx] == 212) {
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      #2.9 Schubbetrieb	2.9.2 Schubbetriebsphase
      starttime <- Sys.time()
      varName <- as.character(mainConfig[idx, 6])
      # varNameMile <- as.character(paste0(varName, "Mile"))
      assign(varName, NULL)
      # assign(varNameMile, NULL)
      calc1Val <- as.character(mainConfig[idx, 10])
      calc4Val <- as.character(mainConfig[idx, 13])
      #calc5Odo <- as.character(mainConfig[idx, 14])
      #signal 1
      sig1idx <- which(channSigConfig[, 3] == calc1Val)
      sig1Val <- channSigConfig[sig1idx, 4]
      signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
      signal1Vec[signal1Vec == "NA"] <- NA
      signal1Vec <- signal1Vec[!is.na(signal1Vec)]
      assign(sig1Val, signal1Vec)
      # #Signal3 - Odo
      # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
      # sig3Val <- channSigConfig[sig3idx, 4]
      # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
      # signal3Vec[signal3Vec == "NA"] <- NA
      # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
      # assign(sig3Val, signal3Vec)
      #Signal 4
      sig4idx <- which(channSigConfig[, 3] == calc4Val)
      sig4Val <- channSigConfig[sig4idx, 4]
      signal4Vec <- as.character(channSigConfig[sig4idx, 8:25])
      signal4Vec[signal4Vec == "NA"] <- NA
      signal4Vec <- signal4Vec[!is.na(signal4Vec)]
      assign(sig4Val, signal4Vec)
      #signal threshold
      sig1min <- as.numeric(channSigConfig[sig1idx,5])
      sig1max <- as.numeric(channSigConfig[sig1idx,6])
      # #sig3min <- as.numeric(channSigConfig[sig3idx,5])
      # #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      sig4min <- as.numeric(channSigConfig[sig4idx,5])
      sig4max <- as.numeric(channSigConfig[sig4idx,6])
      # XEngTor <- getChann(sigEngTor,X,1000)
      calc1Val <- getChann1(get(sig1Val), X,sig1min,sig1max)
      # calc5Odo <- getChann1(get(sig3Val), X,sig3min,sig3max)
      calc4Val <- getChann1(get(sig4Val), X,sig4min,sig4max)
      
      pushModeDur <-
        durationTime(
          x = calc1Val,
          y = calc4Val,
          breaksX = pushModeBreaks1[1:2],
          breaksY = pushModeBreaks2[2:3]
        )
      assign(x = varName[1],
             value = do.call("countTable1D", args = list(
               x = pushModeDur, mergeTab = get(varName)
             )))
      # assign(varNameMile[1], c(max(
      #   0,
      #   max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
      # )))
      #pushModeCounts <- rbind(pushModeCounts,pushModeCountsHelp2)
      if (is.null(get(varName))) {
        f_idx <- which(mainConfig[, 1] == 212)
        failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
        failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
        if(length(colName1) == 0){
          failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
        }else if(length(colName2) == 0){
          failSlides[fail_index,4] <-as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
        }
        fail_index <- fail_index + 1
      } else{
        ##saveRDS in the dir location
        g_idx <- which(mainConfig[, 1] == 212)
        GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
        GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
        #summary of Signal 1
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName1 <- c(colName1,colName1)
        col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
        count1 <- sum(length(which(!is.na(X[,col1]))))
        perc1 <- round(count1/nrow(X)*100)
        GoodSlides[good_index,3] <- paste0(col1)
        GoodSlides[good_index,4] <- paste0(count1)
        GoodSlides[good_index,5] <- paste0(perc1,"%")
        #summary of Signal 2 
        
        #summary of Signal 3 
        # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
        # colName3 <- c(colName3,colName3)
        # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
        # count3 <- sum(length(which(!is.na(X[,col3]))))
        # perc3 <- round(count3/nrow(X)*100)
        # GoodSlides[good_index,9] <- paste0(col3)
        # GoodSlides[good_index,10] <- paste0(count3)
        # GoodSlides[good_index,11] <- paste0(perc3,"%")
        #summary of Signal 4 
        colName4 <- get(sig4Val)[get(sig4Val) %in% names(X)]
        colName4 <- c(colName4,colName4)
        col4 <- colName4[which.max(colSums(!is.na(X[,colName4])))]
        count4 <- sum(length(which(!is.na(X[,col4]))))
        perc4 <- round(count4/nrow(X)*100)
        GoodSlides[good_index,12] <- paste0(col4)
        GoodSlides[good_index,13] <- paste0(count4)
        GoodSlides[good_index,14] <- paste0(perc4,"%")
        
        good_index <- good_index + 1
        saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
        # saveRDS(get(varNameMile[1]), paste0(dir, varNameMile[1], ".rds"))
      }
      ##remove the variables to free up the memory
      endtime <- Sys.time()
      print(paste0(
        "total time for calculating ",
        varName[1],
        " is ",
        endtime - starttime
      ))
      rm(calc1Val, calc4Val, calc5Odo)
      rm(list = paste0(varName[1]))
      ## rm(list = paste0(varNameMile[1]))
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    }
    else if (gid[idx] == 213) {
      #2.9 Schubbetrieb	2.9.3 Katalysatortemperaturen im Schubbetrieb
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      failSlides[fail_index, 1] <- mainConfig[idx, 2]
      failSlides[fail_index, 2] <- mainConfig[idx, 3]
      failSlides[fail_index,3] <- "No Parameter Calculation Present"
      fail_index <- fail_index + 1
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
      
    } 
    else if (gid[idx] == 214) {
      #2.10 Verbrennungsmodus 	2.10 Homogener und Schichtbetrieb
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      failSlides[fail_index, 1] <- mainConfig[idx, 2]
      failSlides[fail_index, 2] <- mainConfig[idx, 3]
      failSlides[fail_index,3] <- "No Parameter Calculation Present"
      fail_index <- fail_index + 1
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    } 
    else if (gid[idx] == 215) {
      #2.10 Verbrennungsmodus 	2.10 Betriebsartwechsel
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      failSlides[fail_index, 1] <- mainConfig[idx, 2]
      failSlides[fail_index, 2] <- mainConfig[idx, 3]
      failSlides[fail_index,3] <- "No Parameter Calculation Present"
      fail_index <- fail_index + 1
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    }
    else if (gid[idx] == 216) {
      #2.11 Vorentflammung 	2.11.1 Zyklen pro Motordrehzahl
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      failSlides[fail_index, 1] <- mainConfig[idx, 2]
      failSlides[fail_index, 2] <- mainConfig[idx, 3]
      failSlides[fail_index,3] <- "No Parameter Calculation Present"
      fail_index <- fail_index + 1
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    }
    else if (gid[idx] == 217) {
      #2.11 Vorentflammung 	2.11.2 Zyklen pro KÃ¼hlmitteltemperatur
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      failSlides[fail_index, 1] <- mainConfig[idx, 2]
      failSlides[fail_index, 2] <- mainConfig[idx, 3]
      failSlides[fail_index,3] <- "No Parameter Calculation Present"
      fail_index <- fail_index + 1
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
      
    }
    else if (gid[idx] == 218) {
      #2.11 Vorentflammung 	2.11.3 Zyklen pro relativer Krafstoffmasse
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      failSlides[fail_index, 1] <- mainConfig[idx, 2]
      failSlides[fail_index, 2] <- mainConfig[idx, 3]
      failSlides[fail_index,3] <- "No Parameter Calculation Present"
      fail_index <- fail_index + 1
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    } 
    else if (gid[idx] == 219) {
      #2.12 Heizen des Katalysators
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      failSlides[fail_index, 1] <- mainConfig[idx, 2]
      failSlides[fail_index, 2] <- mainConfig[idx, 3]
      failSlides[fail_index,3] <- "No Parameter Calculation Present"
      fail_index <- fail_index + 1
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
      
    } 
    else if (gid[idx] == 220) {
      #2.13 Regeneration des DieselruÃpartikelfilters
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      if (engType == "O") {
        starttime <- Sys.time()
        varName <- as.character(mainConfig[idx, 6])
        # varNameMile <- as.character(paste0(varName, "Mile"))
        assign(varName, NULL)
        # assign(varNameMile, NULL)
        calc1Val <- as.character(mainConfig[idx, 9])
        calc4Val <- as.character(mainConfig[idx, 13])
        #calc5Odo <- as.character(mainConfig[idx, 14])
        #signal 1
        sig1idx <- which(channSigConfig[, 3] == calc1Val)
        sig1Val <- channSigConfig[sig1idx, 4]
        signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
        signal1Vec[signal1Vec == "NA"] <- NA
        signal1Vec <- signal1Vec[!is.na(signal1Vec)]
        assign(sig1Val, signal1Vec)
        #signal 2
        sig2idx <- which(channSigConfig[, 3] == calc4Val)
        sig2Val <- channSigConfig[sig2idx, 4]
        signal2Vec <- as.character(channSigConfig[sig2idx, 8:25])
        signal2Vec[signal2Vec == "NA"] <- NA
        signal2Vec <- signal2Vec[!is.na(signal2Vec)]
        assign(sig2Val, signal2Vec)
        # #Signal3 - Odo
        # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
        # sig3Val <- channSigConfig[sig3idx, 4]
        # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
        # signal3Vec[signal3Vec == "NA"] <- NA
        # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
        # assign(sig3Val, signal3Vec)
        #signal threshold
        sig1min <- as.numeric(channSigConfig[sig1idx,5])
        sig1max <- as.numeric(channSigConfig[sig1idx,6])
        sig2min <- as.numeric(channSigConfig[sig2idx,5])
        sig2max <- as.numeric(channSigConfig[sig2idx,6])
        # #sig3min <- as.numeric(channSigConfig[sig3idx,5])
        # #sig3max <- as.numeric(channSigConfig[sig3idx,6])
        
        
        
        calc1Val <- getChann1(get(sig1Val), X,sig1min,sig1max)
        calc4Val <- getChann1(get(sig2Val), X,sig2min,sig2max)
        # calc5Odo <- getChann1(get(sig3Val), X,sig3min,sig3max)
        calc1Val[is.na(calc1Val)] <- 0
        calc1Val[calc1Val > 1] <- 0
        assign(x = varName[1],
               value = do.call(
                 "countDPFRegFunction",
                 args = list(
                   x = calc1Val,
                   breaksX = dpfRegBreaks1,
                   breaksY = dpfRegBreaks2,
                   roundDigit = 2
                 )
               ))
        append(get(varName), get(varName))
        # assign(varNameMile[1], c(max(
        #   0,
        #   max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
        # )))
        if (is.null(get(varName))) {
          idx <- which(mainConfig[, 1] == 220)
          failSlides[fail_index, 1] <- mainConfig[idx, 2]
          failSlides[fail_index, 2] <- mainConfig[idx, 3]
          if(length(colName1) == 0){
            failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
          }else if(length(colName2) == 0){
            failSlides[fail_index,4] <-as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
          }
          fail_index <- fail_index + 1
        } else{
          ##saveRDS in the dir location
          g_idx <- which(mainConfig[, 1] == 220)
          GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
          GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
          #summary of Signal 1
          colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
          colName1 <- c(colName1,colName1)
          col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
          count1 <- sum(length(which(!is.na(X[,col1]))))
          perc1 <- round(count1/nrow(X)*100)
          GoodSlides[good_index,3] <- paste0(col1)
          GoodSlides[good_index,4] <- paste0(count1)
          GoodSlides[good_index,5] <- paste0(perc1,"%")
          #summary of Signal 2 
          colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
          colName2 <- c(colName2,colName2)
          col2 <- colName2[which.max(colSums(!is.na(X[,colName2])))]
          count2 <- sum(length(which(!is.na(X[,col1]))))
          perc2 <- round(count2/nrow(X)*100)
          GoodSlides[good_index,6] <- paste0(col2)
          GoodSlides[good_index,7] <- paste0(count2)
          GoodSlides[good_index,8] <- paste0(perc2,"%")
          #summary of Signal 3 
          # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
          # colName3 <- c(colName3,colName3)
          # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
          # count3 <- sum(length(which(!is.na(X[,col3]))))
          # perc3 <- round(count3/nrow(X)*100)
          # GoodSlides[good_index,9] <- paste0(col3)
          # GoodSlides[good_index,10] <- paste0(count3)
          # GoodSlides[good_index,11] <- paste0(perc3,"%")
          
          good_index <- good_index + 1
          saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
          # saveRDS(get(varNameMile[1]),
          # paste0(dir, varNameMile[1], ".rds"))
        }
        ##remove the variables to free up the memory
        endtime <- Sys.time()
        print(paste0(
          "total time for calculating ",
          varName[1],
          " is ",
          endtime - starttime
        ))
        rm(calc1Val, calc4Val, calc5Odo)
        rm(list = paste0(varName[1]))
        ## rm(list = paste0(varNameMile[1]))
        print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
      }
    }
    else if (gid[idx] == 221) {
      #2.14 Fahrprogramm
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      starttime <- Sys.time()
      varName <- as.character(mainConfig[idx, 6])
      # varNameMile <- as.character(paste0(varName, "Mile"))
      assign(varName, NULL)
      # assign(varNameMile, NULL)
      calc1Val <- as.character(mainConfig[idx, 8])
      calc2Val <- as.character(mainConfig[idx, 13])
      #calc5Odo <- as.character(mainConfig[idx, 14])
      #signal 1
      sig1idx <- which(channSigConfig[, 3] == calc1Val)
      sig1Val <- channSigConfig[sig1idx, 4]
      signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
      signal1Vec[signal1Vec == "NA"] <- NA
      signal1Vec <- signal1Vec[!is.na(signal1Vec)]
      assign(sig1Val, signal1Vec)
      #signal 2
      sig2idx <- which(channSigConfig[, 3] == calc2Val)
      sig2Val <- channSigConfig[sig2idx, 4]
      signal2Vec <- as.character(channSigConfig[sig2idx, 8:25])
      signal2Vec[signal2Vec == "NA"] <- NA
      signal2Vec <- signal2Vec[!is.na(signal2Vec)]
      assign(sig2Val, signal2Vec)
      # #Signal3 - Odo
      # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
      # sig3Val <- channSigConfig[sig3idx, 4]
      # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
      # signal3Vec[signal3Vec == "NA"] <- NA
      # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
      # assign(sig3Val, signal3Vec)
      #signal threshold
      sig1min <- as.numeric(channSigConfig[sig1idx,5])
      sig1max <- as.numeric(channSigConfig[sig1idx,6])
      sig2min <- as.numeric(channSigConfig[sig2idx,5])
      sig2max <- as.numeric(channSigConfig[sig2idx,6])
      #sig3min <- as.numeric(channSigConfig[sig3idx,5])
      #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      
      calc1Val <- getChann1(get(sig1Val), X,sig1min,sig1max)
      calc2Val <- getChann1(get(sig2Val), X,sig2min,sig2max)
      # calc5Odo <- getChann1(get(sig3Val), X,sig3min,sig3max)
      classCodes<-c(0,1,2,3,4,5,6,7,15)
      d_idx <- which(calc1Val %in% classCodes)
      calc1Val <- calc1Val[d_idx]
      calc2Val <- calc2Val[d_idx]
      assign(x = varName[1],
             value = do.call("countTable1D1", args = list(
               x = calc1Val[calc2Val > 0], mergeTab = get(varName)
             )))
      # assign(varNameMile[1], c(max(
      #   0, max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
      # )))
      if (is.null(get(varName))) {
        idx <- which(mainConfig[, 1] == 221)
        failSlides[fail_index, 1] <- mainConfig[idx, 2]
        failSlides[fail_index, 2] <- mainConfig[idx, 3]
        if(length(colName1) == 0){
          failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
        }else if(length(colName2) == 0){
          failSlides[fail_index,4] <-as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
        }
        fail_index <- fail_index + 1
      } else{
        ##saveRDS in the dir location
        idx <- which(mainConfig[, 1] == 221)
        GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
        GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
        #summary of Signal 1
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName1 <- c(colName1,colName1)
        col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
        count1 <- sum(length(which(!is.na(X[,col1]))))
        perc1 <- round(count1/nrow(X)*100)
        GoodSlides[good_index,3] <- paste0(col1)
        GoodSlides[good_index,4] <- paste0(count1)
        GoodSlides[good_index,5] <- paste0(perc1,"%")
        #summary of Signal 2 
        colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
        colName2 <- c(colName2,colName2)
        col2 <- colName2[which.max(colSums(!is.na(X[,colName2])))]
        count2 <- sum(length(which(!is.na(X[,col1]))))
        perc2 <- round(count2/nrow(X)*100)
        GoodSlides[good_index,6] <- paste0(col2)
        GoodSlides[good_index,7] <- paste0(count2)
        GoodSlides[good_index,8] <- paste0(perc2,"%")
        #summary of Signal 3 
        # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
        # colName3 <- c(colName3,colName3)
        # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
        # count3 <- sum(length(which(!is.na(X[,col3]))))
        # perc3 <- round(count3/nrow(X)*100)
        # GoodSlides[good_index,9] <- paste0(col3)
        # GoodSlides[good_index,10] <- paste0(count3)
        # GoodSlides[good_index,11] <- paste0(perc3,"%")
        
        good_index <- good_index + 1
        saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
        # saveRDS(get(varNameMile[1]), paste0(dir, varNameMile[1], ".rds"))
      }
      ##remove the variables to free up the memory
      endtime <- Sys.time()
      print(paste0(
        "total time for calculating ",
        varName[1],
        " is ",
        endtime - starttime
      ))
      rm(calc1Val, calc2Val, calc5Odo)
      rm(list = paste0(varName[1]))
      ## rm(list = paste0(varNameMile[1]))
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    }
    else if (gid[idx] == 222) {
      # #2.15 Motorstart	2.15.1 Anzahl Starts
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      starttime <- Sys.time()
      varName <- as.character(mainConfig[idx, 6])
      # varNameMile <- as.character(paste0(varName, "Mile"))
      assign(varName, NULL)
      # assign(varNameMile, NULL)
      calc2Val <- as.character(mainConfig[idx, 13])
      #calc5Odo <- as.character(mainConfig[idx, 14])
      #signal 2
      sig2idx <- which(channSigConfig[, 3] == calc2Val)
      sig2Val <- channSigConfig[sig2idx, 4]
      signal2Vec <- as.character(channSigConfig[sig2idx, 8:25])
      signal2Vec[signal2Vec == "NA"] <- NA
      signal2Vec <- signal2Vec[!is.na(signal2Vec)]
      assign(sig2Val, signal2Vec)
      # #Signal3 - Odo
      # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
      # sig3Val <- channSigConfig[sig3idx, 4]
      # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
      # signal3Vec[signal3Vec == "NA"] <- NA
      # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
      # assign(sig3Val, signal3Vec)
      #signal threshold
      
      sig2min <- as.numeric(channSigConfig[sig2idx,5])
      sig2max <- as.numeric(channSigConfig[sig2idx,6])
      # #sig3min <- as.numeric(channSigConfig[sig3idx,5])
      # #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      numStarts <-NULL
      #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      for (ri in 1:length(rleTrack$starts)) {
        X1 <- X[rleTrack$starts[ri]:rleTrack$stops[ri], ]
        calc2Val <- getChann1(get(sig2Val), X1,sig2min,sig2max)
        # print(summary(calc2Val))
        idStart <- startIndices(calc2Val, startThresh, startMinTime)
        numStarts <- c(numStarts, length(idStart))
        
        rm(X1)
      }
      # print(numStarts)
      assign(varName[1], numStarts)
      # assign(varNameMile[1], c(max(
      #   0, max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
      # )))
      if (is.null(get(varName))) {
        f_idx <- which(mainConfig[, 1] == 222)
        failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
        failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
        if(length(colName1) == 0){
          failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
        }else if(length(colName2) == 0){
          failSlides[fail_index,4] <-as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
        }
        fail_index <- fail_index + 1
      } else{
        ##saveRDS in the dir location
        g_idx <- which(mainConfig[, 1] == 222)
        GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
        GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
        #summary of Signal 1
        
        #summary of Signal 2 
        colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
        colName2 <- c(colName2,colName2)
        col2 <- colName2[which.max(colSums(!is.na(X[,colName2])))]
        count2 <- sum(length(which(!is.na(X[,col2]))))
        perc2 <- round(count2/nrow(X)*100)
        GoodSlides[good_index,6] <- paste0(col2)
        GoodSlides[good_index,7] <- paste0(count2)
        GoodSlides[good_index,8] <- paste0(perc2,"%")
        #summary of Signal 3 
        # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
        # colName3 <- c(colName3,colName3)
        # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
        # count3 <- sum(length(which(!is.na(X[,col3]))))
        # perc3 <- round(count3/nrow(X)*100)
        # GoodSlides[good_index,9] <- paste0(col3)
        # GoodSlides[good_index,10] <- paste0(count3)
        # GoodSlides[good_index,11] <- paste0(perc3,"%")
        
        good_index <- good_index + 1
        saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
        # saveRDS(get(varNameMile[1]), paste0(dir, varNameMile[1], ".rds"))
      }
      rm(calc2Val)
      # rm(calc5Odo)
      rm(numStarts)
      rm(idStart)
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    }
    else if (gid[idx] == 223) {
      #2.15 Motorstart	2.15.2 MotorÃ¶ltemperatur beim Start
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      starttime <- Sys.time()
      varName <- as.character(mainConfig[idx, 6])
      # varNameMile <- as.character(paste0(varName, "Mile"))
      assign(varName, NULL)
      #assign(varNameMile, NULL)
      calc1Val <- as.character(mainConfig[idx, 8])
      calc2Val <- as.character(mainConfig[idx, 13])
      #calc5Odo <- as.character(mainConfig[idx, 14])
      #signal 1
      sig1idx <- which(channSigConfig[, 3] == calc1Val)
      sig1Val <- channSigConfig[sig1idx, 4]
      signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
      signal1Vec[signal1Vec == "NA"] <- NA
      signal1Vec <- signal1Vec[!is.na(signal1Vec)]
      assign(sig1Val, signal1Vec)
      #signal 2
      sig2idx <- which(channSigConfig[, 3] == calc2Val)
      sig2Val <- channSigConfig[sig2idx, 4]
      signal2Vec <- as.character(channSigConfig[sig2idx, 8:25])
      signal2Vec[signal2Vec == "NA"] <- NA
      signal2Vec <- signal2Vec[!is.na(signal2Vec)]
      assign(sig2Val, signal2Vec)
      # #Signal3 - Odo
      # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
      # sig3Val <- channSigConfig[sig3idx, 4]
      # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
      # signal3Vec[signal3Vec == "NA"] <- NA
      # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
      # assign(sig3Val, signal3Vec)
      #signal threshold
      sig1min <- as.numeric(channSigConfig[sig1idx,5])
      sig1max <- as.numeric(channSigConfig[sig1idx,6])
      sig2min <- as.numeric(channSigConfig[sig2idx,5])
      sig2max <- as.numeric(channSigConfig[sig2idx,6])
      # #sig3min <- as.numeric(channSigConfig[sig3idx,5])
      # #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      
      
      # calc1Val <- getChann1(get(sig1Val), X,sig1min,sig1max)
      # calc2Val <- getChann1(get(sig2Val), X,sig2min,sig2max)
      # # calc5Odo <- getChann1(get(sig3Val), X,sig3min,sig3max)
      # idStart <- startIndices(calc2Val, startThresh, startMinTime)
      # calc1Val <- calc1Val[idStart]
      # assign(x = varName[1],
      #        value = do.call("countTable1D", args = list(
      #          x = calc1Val, mergeTab = get(varName)
      #        )))
      # assign(varNameMile[1], c(max(
      #   0, max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
      # )))
      for (ri in 1:length(rleTrack$starts)) {
        X1 <- X[rleTrack$starts[ri]:rleTrack$stops[ri], ]
        calc1Val <- getChann1(get(sig1Val), X1,sig1min,sig1max)
        calc2Val <- getChann1(get(sig2Val), X1,sig2min,sig2max)
        # print(summary(calc2Val))
        idStart <- startIndices(calc2Val, startThresh, startMinTime)
        calc1Val <- calc1Val[idStart]
        assign(x = varName[1],
               value = do.call("countTable1D", args = list(
                 x = calc1Val, mergeTab = get(varName)
               )))
        
        rm(X1)
      }
      
      if (is.null(get(varName))) {
        f_idx <- which(mainConfig[, 1] == 223)
        failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
        failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
        if(length(colName1) == 0){
          failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
        }else if(length(colName2) == 0){
          failSlides[fail_index,4] <-as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
        }
        fail_index <- fail_index + 1
      } else{
        ##saveRDS in the dir location
        g_idx <- which(mainConfig[, 1] == 223)
        GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
        GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
        #summary of Signal 1
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName1 <- c(colName1,colName1)
        col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
        count1 <- sum(length(which(!is.na(X[,col1]))))
        perc1 <- round(count1/nrow(X)*100)
        GoodSlides[good_index,3] <- paste0(col1)
        GoodSlides[good_index,4] <- paste0(count1)
        GoodSlides[good_index,5] <- paste0(perc1,"%")
        #summary of Signal 2 
        colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
        colName2 <- c(colName2,colName2)
        col2 <- colName2[which.max(colSums(!is.na(X[,colName2])))]
        count2 <- sum(length(which(!is.na(X[,col1]))))
        perc2 <- round(count2/nrow(X)*100)
        GoodSlides[good_index,6] <- paste0(col2)
        GoodSlides[good_index,7] <- paste0(count2)
        GoodSlides[good_index,8] <- paste0(perc2,"%")
        #summary of Signal 3 
        # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
        # colName3 <- c(colName3,colName3)
        # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
        # count3 <- sum(length(which(!is.na(X[,col3]))))
        # perc3 <- round(count3/nrow(X)*100)
        # GoodSlides[good_index,9] <- paste0(col3)
        # GoodSlides[good_index,10] <- paste0(count3)
        # GoodSlides[good_index,11] <- paste0(perc3,"%")
        
        good_index <- good_index + 1
        saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
        # saveRDS(get(varNameMile[1]), paste0(dir, varNameMile[1], ".rds"))
      }
      ##remove the variables to free up the memory
      endtime <- Sys.time()
      print(paste0(
        "total time for calculating ",
        varName[1],
        " is ",
        endtime - starttime
      ))
      rm(calc1Val, calc2Val, calc5Odo)
      rm(list = paste0(varName[1]))
      ## rm(list = paste0(varNameMile[1]))
      rm(idStart)
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    } 
    else if (gid[idx] == 224) {
      #2.15 Motorstart	2.15.3 Motorabstelldauer
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      starttime <- Sys.time()
      varName <- as.character(mainConfig[idx, 6])
      # varNameMile <- as.character(paste0(varName, "Mile"))
      assign(varName, NULL)
      # assign(varNameMile, NULL)
      calc1Val <- as.character(mainConfig[idx, 8])
      calc4Val <- as.character(mainConfig[idx, 13])
      #calc5Odo <- as.character(mainConfig[idx, 14])
      #signal 1
      sig1idx <- which(channSigConfig[, 3] == calc1Val)
      sig1Val <- channSigConfig[sig1idx, 4]
      signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
      signal1Vec[signal1Vec == "NA"] <- NA
      signal1Vec <- signal1Vec[!is.na(signal1Vec)]
      assign(sig1Val, signal1Vec)
      # #Signal3 - Odo
      # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
      # sig3Val <- channSigConfig[sig3idx, 4]
      # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
      # signal3Vec[signal3Vec == "NA"] <- NA
      # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
      # assign(sig3Val, signal3Vec)
      #Signal 4
      sig4idx <- which(channSigConfig[, 3] == calc4Val)
      sig4Val <- channSigConfig[sig4idx, 4]
      signal4Vec <- as.character(channSigConfig[sig4idx, 8:25])
      signal4Vec[signal4Vec == "NA"] <- NA
      signal4Vec <- signal4Vec[!is.na(signal4Vec)]
      assign(sig4Val, signal4Vec)
      #signal threshold
      sig1min <- as.numeric(channSigConfig[sig1idx,5])
      sig1max <- as.numeric(channSigConfig[sig1idx,6])
      #sig3min <- as.numeric(channSigConfig[sig3idx,5])
      # #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      sig4min <- as.numeric(channSigConfig[sig4idx,5])
      sig4max <- as.numeric(channSigConfig[sig4idx,6])
      # calc1Val <- getChann1(get(sig1Val), X,sig1min,sig1max)
      # # calc5Odo <- getChann1(get(sig3Val), X,sig3min,sig3max)
      # calc4Val <- getChann1(get(sig4Val), X,sig4min,sig4max)
      # standStillDur <-
      #   durationTime(
      #     x = calc1Val,
      #     y = calc4Val,
      #     breaksX = standStillBreaks1,
      #     breaksY = standStillBreaks2
      #   )
      # assign(x = varName[1],
      #        value = do.call("countTable1D", args = list(
      #          x = standStillDur, mergeTab = get(varName)
      #        )))
      for (ri in 1:length(rleTrack$starts)) {
        X1 <- X[rleTrack$starts[ri]:rleTrack$stops[ri], ]
        calc1Val <- getChann1(get(sig1Val), X1,sig1min,sig1max)
        calc4Val <- getChann1(get(sig4Val), X1,sig4min,sig4max)
        
        standStillDur <-
          durationTime(
            x = calc1Val,
            y = calc4Val,
            breaksX = standStillBreaks1,
            breaksY = standStillBreaks2
          )
        assign(x = varName[1],
               value = do.call("countTable1D", args = list(
                 x = standStillDur, mergeTab = get(varName)
               )))
        rm(X1)
      }
      
      # assign(varNameMile[1], c(max(
      #   0, max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
      # )))
      
      if (is.null(get(varName))) {
        f_idx <- which(mainConfig[, 1] == 224)
        failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
        failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
        if(length(colName1) == 0){
          failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
        }else if(length(colName2) == 0){
          failSlides[fail_index,4] <-as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
        }
        fail_index <- fail_index + 1
      } else{
        ##saveRDS in the dir location
        g_idx <- which(mainConfig[, 1] == 224)
        GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
        GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
        #summary of Signal 1
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName1 <- c(colName1,colName1)
        col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
        count1 <- sum(length(which(!is.na(X[,col1]))))
        perc1 <- round(count1/nrow(X)*100)
        GoodSlides[good_index,3] <- paste0(col1)
        GoodSlides[good_index,4] <- paste0(count1)
        GoodSlides[good_index,5] <- paste0(perc1,"%")
        
        #summary of Signal 3 
        # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
        # colName3 <- c(colName3,colName3)
        # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
        # count3 <- sum(length(which(!is.na(X[,col3]))))
        # perc3 <- round(count3/nrow(X)*100)
        # GoodSlides[good_index,9] <- paste0(col3)
        # GoodSlides[good_index,10] <- paste0(count3)
        # GoodSlides[good_index,11] <- paste0(perc3,"%")
        #summary of Signal 4 
        colName4 <- get(sig4Val)[get(sig4Val) %in% names(X)]
        colName4 <- c(colName4,colName4)
        col4 <- colName4[which.max(colSums(!is.na(X[,colName4])))]
        count4 <- sum(length(which(!is.na(X[,col4]))))
        perc4 <- round(count4/nrow(X)*100)
        GoodSlides[good_index,12] <- paste0(col4)
        GoodSlides[good_index,13] <- paste0(count4)
        GoodSlides[good_index,14] <- paste0(perc4,"%")
        good_index <- good_index + 1
        saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
        # saveRDS(get(varNameMile[1]), paste0(dir, varNameMile[1], ".rds"))
      }
      ##remove the variables to free up the memory
      endtime <- Sys.time()
      print(paste0(
        "total time for calculating ",
        varName[1],
        " is ",
        endtime - starttime
      ))
      rm(calc1Val, calc4Val, calc5Odo)
      rm(list = paste0(varName[1]))
      ## rm(list = paste0(varNameMile[1]))
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    }
    else if (gid[idx] == 225) {
      #2.15 Motorstart	2.15.4 Anzahl Hochlaststarts
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      starttime <- Sys.time()
      varName <- as.character(mainConfig[idx, 6])
      # varNameMile <- as.character(paste0(varName, "Mile"))
      assign(varName, NULL)
      # assign(varNameMile, NULL)
      calc1Val <- as.character(mainConfig[idx, 10])
      calc4Val <- as.character(mainConfig[idx, 13])
      #calc5Odo <- as.character(mainConfig[idx, 14])
      #signal 1
      sig1idx <- which(channSigConfig[, 3] == calc1Val)
      sig1Val <- channSigConfig[sig1idx, 4]
      signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
      signal1Vec[signal1Vec == "NA"] <- NA
      signal1Vec <- signal1Vec[!is.na(signal1Vec)]
      assign(sig1Val, signal1Vec)
      #Signal3 - Odo
      # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
      # sig3Val <- channSigConfig[sig3idx, 4]
      # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
      # signal3Vec[signal3Vec == "NA"] <- NA
      # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
      # assign(sig3Val, signal3Vec)
      #Signal 4
      sig4idx <- which(channSigConfig[, 3] == calc4Val)
      sig4Val <- channSigConfig[sig4idx, 4]
      signal4Vec <- as.character(channSigConfig[sig4idx, 8:25])
      signal4Vec[signal4Vec == "NA"] <- NA
      signal4Vec <- signal4Vec[!is.na(signal4Vec)]
      assign(sig4Val, signal4Vec)
      #signal threshold
      sig1min <- as.numeric(channSigConfig[sig1idx,5])
      sig1max <- as.numeric(channSigConfig[sig1idx,6])
      sig4min <- as.numeric(channSigConfig[sig4idx,5])
      sig4max <- as.numeric(channSigConfig[sig4idx,6])
      # #sig3min <- as.numeric(channSigConfig[sig3idx,5])
      # #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      
      # calc5Odo <- getChann1(get(sig3Val), X,sig3min,sig3max)
      numHighLoadStarts <- NULL
      
      for (ri in 1:length(rleTrack$starts)) {
        X1 <- X[rleTrack$starts[ri]:rleTrack$stops[ri], ]
        calc1Val <- getChann1(get(sig1Val), X1,sig1min,sig1max)
        calc4Val <- getChann1(get(sig4Val), X1,sig4min,sig4max)
        idStart <- startIndices(calc4Val, startThresh, startMinTime)
        numHighLoadStarts <-
          c(numHighLoadStarts,
            sum(
              sapply(idStart, function(x) {
                max(calc1Val[max(x - highLoadStartTime, 1):(x + highLoadStartTime)], na.rm =
                      TRUE)
              }) >= highLoadStartThresh,
              na.rm = TRUE
            ))
        
      }
      assign(varName[1], numHighLoadStarts)
      
      # assign(varNameMile[1], c(max(
      #   0, max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
      # )))
      if (is.null(get(varName))) {
        f_idx <- which(mainConfig[, 1] == 225)
        failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
        failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
        if(length(colName1) == 0){
          failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
        }else if(length(colName2) == 0){
          failSlides[fail_index,4] <-as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
        }
        fail_index <- fail_index + 1
      } else{
        ##saveRDS in the dir location
        g_idx <- which(mainConfig[, 1] == 225)
        GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
        GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
        #summary of Signal 1
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName1 <- c(colName1,colName1)
        col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
        count1 <- sum(length(which(!is.na(X[,col1]))))
        perc1 <- round(count1/nrow(X)*100)
        GoodSlides[good_index,3] <- paste0(col1)
        GoodSlides[good_index,4] <- paste0(count1)
        GoodSlides[good_index,5] <- paste0(perc1,"%")
        #summary of Signal 2 
        # colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
        # colName2 <- c(colName2,colName2)
        # col2 <- colName2[which.max(colSums(!is.na(X[,colName2])))]
        # count2 <- sum(length(which(!is.na(X[,col1]))))
        # perc2 <- round(count2/nrow(X)*100)
        # GoodSlides[good_index,6] <- paste0(col2)
        # GoodSlides[good_index,7] <- paste0(count2)
        # GoodSlides[good_index,8] <- paste0(perc2,"%")
        # #summary of Signal 3 
        # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
        # colName3 <- c(colName3,colName3)
        # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
        # count3 <- sum(length(which(!is.na(X[,col3]))))
        # perc3 <- round(count3/nrow(X)*100)
        # GoodSlides[good_index,9] <- paste0(col3)
        # GoodSlides[good_index,10] <- paste0(count3)
        
        
        good_index <- good_index + 1
        saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
        # saveRDS(get(varNameMile[1]), paste0(dir, varNameMile[1], ".rds"))
      }
      rm(calc1Val)
      rm(calc2Val)
      rm(numHighLoadStarts)
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    }
    else if (gid[idx] == 226) {
      #2.15 Motorstart	2.15.5 Fahrzeuggeschwindigkeit beim 3S-Start
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      starttime <- Sys.time()
      varName <- as.character(mainConfig[idx, 6])
      # varNameMile <- as.character(paste0(varName, "Mile"))
      assign(varName, NULL)
      # assign(varNameMile, NULL)
      calc1Val <- as.character(mainConfig[idx, 8])
      calc2Val <- as.character(mainConfig[idx, 10])
      calc4Val <- as.character(mainConfig[idx, 13])
      #calc5Odo <- as.character(mainConfig[idx, 14])
      #signal 1
      sig1idx <- which(channSigConfig[, 3] == calc1Val)
      sig1Val <- channSigConfig[sig1idx, 4]
      signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
      signal1Vec[signal1Vec == "NA"] <- NA
      signal1Vec <- signal1Vec[!is.na(signal1Vec)]
      assign(sig1Val, signal1Vec)
      #signal 2
      sig2idx <- which(channSigConfig[, 3] == calc2Val)
      sig2Val <- channSigConfig[sig2idx, 4]
      signal2Vec <- as.character(channSigConfig[sig2idx, 8:25])
      signal2Vec[signal2Vec == "NA"] <- NA
      signal2Vec <- signal2Vec[!is.na(signal2Vec)]
      assign(sig2Val, signal2Vec)
      # #Signal3 - Odo
      # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
      # sig3Val <- channSigConfig[sig3idx, 4]
      # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
      # signal3Vec[signal3Vec == "NA"] <- NA
      # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
      # assign(sig3Val, signal3Vec)
      #Signal 4
      sig4idx <- which(channSigConfig[, 3] == calc4Val)
      sig4Val <- channSigConfig[sig4idx, 4]
      signal4Vec <- as.character(channSigConfig[sig4idx, 8:25])
      signal4Vec[signal4Vec == "NA"] <- NA
      signal4Vec <- signal4Vec[!is.na(signal4Vec)]
      assign(sig4Val, signal4Vec)
      #signal threshold
      sig1min <- as.numeric(channSigConfig[sig1idx,5])
      sig1max <- as.numeric(channSigConfig[sig1idx,6])
      sig2min <- as.numeric(channSigConfig[sig2idx,5])
      sig2max <- as.numeric(channSigConfig[sig2idx,6])
      # #sig3min <- as.numeric(channSigConfig[sig3idx,5])
      # #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      sig4min <- as.numeric(channSigConfig[sig4idx,5])
      sig4max <- as.numeric(channSigConfig[sig4idx,6])
      
      
      # calc1Val <- getChann1(get(sig1Val), X,sig1min,sig1max)
      # calc2Val <- getChann1(get(sig2Val), X,sig2min,sig2max)
      # calc4Val <- getChann1(get(sig4Val), X,sig4min,sig4max)
      # idStart3s <- startIndices1(calc4Val, calc2Val, 0)
      # assign(x = varName[1],
      #        value = do.call("countTable1D", args = list(
      #          x = calc1Val[idStart3s], mergeTab = get(varName)
      #        )))
      for (ri in 1:length(rleTrack$starts)) {
        X1 <- X[rleTrack$starts[ri]:rleTrack$stops[ri], ]
        calc1Val <- getChann1(get(sig1Val), X1,sig1min,sig1max)
        calc2Val <- getChann1(get(sig2Val), X1,sig2min,sig2max)
        calc4Val <- getChann1(get(sig4Val), X1,sig4min,sig4max)
        idStart3s <- startIndices1(calc4Val, calc2Val, 0)
        assign(x = varName[1],
               value = do.call("countTable1D", args = list(
                 x = calc1Val[idStart3s], mergeTab = get(varName)
               )))
        
        rm(X1)
      }
      
      if (is.null(get(varName))) {
        f_idx <- which(mainConfig[, 1] == 226)
        failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
        failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
        if(length(colName1) == 0){
          failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
        }else if(length(colName2) == 0){
          failSlides[fail_index,4] <-as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
        }
        fail_index <- fail_index + 1
      } else{
        ##saveRDS in the dir location
        g_idx <- which(mainConfig[, 1] == 226)
        GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
        GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
        #summary of Signal 1
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName1 <- c(colName1,colName1)
        col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
        count1 <- sum(length(which(!is.na(X[,col1]))))
        perc1 <- round(count1/nrow(X)*100)
        GoodSlides[good_index,3] <- paste0(col1)
        GoodSlides[good_index,4] <- paste0(count1)
        GoodSlides[good_index,5] <- paste0(perc1,"%")
        #summary of Signal 2 
        colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
        colName2 <- c(colName2,colName2)
        col2 <- colName2[which.max(colSums(!is.na(X[,colName2])))]
        count2 <- sum(length(which(!is.na(X[,col1]))))
        perc2 <- round(count2/nrow(X)*100)
        GoodSlides[good_index,6] <- paste0(col2)
        GoodSlides[good_index,7] <- paste0(count2)
        GoodSlides[good_index,8] <- paste0(perc2,"%")
        #summary of Signal 3 
        # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
        # colName3 <- c(colName3,colName3)
        # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
        # count3 <- sum(length(which(!is.na(X[,col3]))))
        # perc3 <- round(count3/nrow(X)*100)
        # GoodSlides[good_index,9] <- paste0(col3)
        # GoodSlides[good_index,10] <- paste0(count3)
        # GoodSlides[good_index,11] <- paste0(perc3,"%")
        #summary of Signal 4 
        colName4 <- get(sig4Val)[get(sig4Val) %in% names(X)]
        colName4 <- c(colName4,colName4)
        col4 <- colName4[which.max(colSums(!is.na(X[,colName4])))]
        count4 <- sum(length(which(!is.na(X[,col4]))))
        perc4 <- round(count4/nrow(X)*100)
        GoodSlides[good_index,12] <- paste0(col4)
        GoodSlides[good_index,13] <- paste0(count4)
        GoodSlides[good_index,14] <- paste0(perc4,"%")
        
        good_index <- good_index + 1
        saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
        # saveRDS(get(varNameMile[1]), paste0(dir, varNameMile[1], ".rds"))
      }
      ##remove the variables to free up the memory
      endtime <- Sys.time()
      print(paste0(
        "total time for calculating ",
        varName[1],
        " is ",
        endtime - starttime
      ))
      rm(calc1Val, calc4Val, calc5Odo)
      rm(list = paste0(varName[1]))
      ## rm(list = paste0(varNameMile[1]))
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    } 
    else if (gid[idx] == 22611) {
      #2.15 Motorstart	2.15.5 Fahrzeuggeschwindigkeit beim 3S-Start
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      starttime <- Sys.time()
      varName <- as.character(mainConfig[idx, 6])
      # varNameMile <- as.character(paste0(varName, "Mile"))
      assign(varName, NULL)
      # assign(varNameMile, NULL)
      calc1Val <- as.character(mainConfig[idx, 8])
      calc2Val <- as.character(mainConfig[idx, 10])
      calc4Val <- as.character(mainConfig[idx, 13])
      #calc5Odo <- as.character(mainConfig[idx, 14])
      #signal 1
      sig1idx <- which(channSigConfig[, 3] == calc1Val)
      sig1Val <- channSigConfig[sig1idx, 4]
      signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
      signal1Vec[signal1Vec == "NA"] <- NA
      signal1Vec <- signal1Vec[!is.na(signal1Vec)]
      assign(sig1Val, signal1Vec)
      #signal 2
      sig2idx <- which(channSigConfig[, 3] == calc2Val)
      sig2Val <- channSigConfig[sig2idx, 4]
      signal2Vec <- as.character(channSigConfig[sig2idx, 8:25])
      signal2Vec[signal2Vec == "NA"] <- NA
      signal2Vec <- signal2Vec[!is.na(signal2Vec)]
      assign(sig2Val, signal2Vec)
      # #Signal3 - Odo
      # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
      # sig3Val <- channSigConfig[sig3idx, 4]
      # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
      # signal3Vec[signal3Vec == "NA"] <- NA
      # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
      # assign(sig3Val, signal3Vec)
      #Signal 4
      sig4idx <- which(channSigConfig[, 3] == calc4Val)
      sig4Val <- channSigConfig[sig4idx, 4]
      signal4Vec <- as.character(channSigConfig[sig4idx, 8:25])
      signal4Vec[signal4Vec == "NA"] <- NA
      signal4Vec <- signal4Vec[!is.na(signal4Vec)]
      assign(sig4Val, signal4Vec)
      #signal threshold
      sig1min <- as.numeric(channSigConfig[sig1idx,5])
      sig1max <- as.numeric(channSigConfig[sig1idx,6])
      sig2min <- as.numeric(channSigConfig[sig2idx,5])
      sig2max <- as.numeric(channSigConfig[sig2idx,6])
      # #sig3min <- as.numeric(channSigConfig[sig3idx,5])
      # #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      sig4min <- as.numeric(channSigConfig[sig4idx,5])
      sig4max <- as.numeric(channSigConfig[sig4idx,6])
      
      
      calc1Val <- getChann1(get(sig1Val), X,sig1min,sig1max)
      calc2Val <- getChann1(get(sig2Val), X,sig2min,sig2max)
      # calc5Odo <- getChann1(get(sig3Val), X,sig3min,sig3max)
      calc4Val <- getChann1(get(sig4Val), X,sig4min,sig4max)
      idStart3s <- startIndices1(calc4Val, calc2Val, 0)
      c1 <- calc2Val[idStart3s] == 1
      c2 <- calc4Val[idStart3s]>0
      c3 <- c1 & c2
      assign(x = varName[1],
             value = do.call("countTable1D", args = list(
               x = calc1Val[c3], mergeTab = get(varName)
             )))
      # assign(varNameMile[1], c(max(
      #   0, max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
      # )))
      #pushModeCounts <- rbind(pushModeCounts,pushModeCountsHelp2)
      if (is.null(get(varName))) {
        f_idx <- which(mainConfig[, 1] == 22611)
        failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
        failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
        if(length(colName1) == 0){
          failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
        }else if(length(colName2) == 0){
          failSlides[fail_index,4] <-as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
        }
        fail_index <- fail_index + 1
      } else{
        ##saveRDS in the dir location
        g_idx <- which(mainConfig[, 1] == 22611)
        GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
        GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
        #summary of Signal 1
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName1 <- c(colName1,colName1)
        col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
        count1 <- sum(length(which(!is.na(X[,col1]))))
        perc1 <- round(count1/nrow(X)*100)
        GoodSlides[good_index,3] <- paste0(col1)
        GoodSlides[good_index,4] <- paste0(count1)
        GoodSlides[good_index,5] <- paste0(perc1,"%")
        #summary of Signal 2 
        colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
        colName2 <- c(colName2,colName2)
        col2 <- colName2[which.max(colSums(!is.na(X[,colName2])))]
        count2 <- sum(length(which(!is.na(X[,col1]))))
        perc2 <- round(count2/nrow(X)*100)
        GoodSlides[good_index,6] <- paste0(col2)
        GoodSlides[good_index,7] <- paste0(count2)
        GoodSlides[good_index,8] <- paste0(perc2,"%")
        #summary of Signal 3 
        # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
        # colName3 <- c(colName3,colName3)
        # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
        # count3 <- sum(length(which(!is.na(X[,col3]))))
        # perc3 <- round(count3/nrow(X)*100)
        # GoodSlides[good_index,9] <- paste0(col3)
        # GoodSlides[good_index,10] <- paste0(count3)
        # GoodSlides[good_index,11] <- paste0(perc3,"%")
        #summary of Signal 4 
        colName4 <- get(sig4Val)[get(sig4Val) %in% names(X)]
        colName4 <- c(colName4,colName4)
        col4 <- colName4[which.max(colSums(!is.na(X[,colName4])))]
        count4 <- sum(length(which(!is.na(X[,col4]))))
        perc4 <- round(count4/nrow(X)*100)
        GoodSlides[good_index,12] <- paste0(col4)
        GoodSlides[good_index,13] <- paste0(count4)
        GoodSlides[good_index,14] <- paste0(perc4,"%")
        
        good_index <- good_index + 1
        saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
        # saveRDS(get(varNameMile[1]), paste0(dir, varNameMile[1], ".rds"))
      }
      ##remove the variables to free up the memory
      endtime <- Sys.time()
      print(paste0(
        "total time for calculating ",
        varName[1],
        " is ",
        endtime - starttime
      ))
      rm(calc1Val, calc4Val, calc5Odo)
      rm(list = paste0(varName[1]))
      ## rm(list = paste0(varNameMile[1]))
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    } 
    else if (gid[idx] == 2261){
      #2.15 Motorstart	2.15.6 Fahrpedalstellung beim 3S-Start
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      starttime <- Sys.time()
      varName <- as.character(mainConfig[idx, 6])
      # varNameMile <- as.character(paste0(varName, "Mile"))
      assign(varName, NULL)
      # assign(varNameMile, NULL)
      calc1Val <- as.character(mainConfig[idx, 8])
      calc2Val <- as.character(mainConfig[idx, 10])
      calc4Val <- as.character(mainConfig[idx, 13])
      #calc5Odo <- as.character(mainConfig[idx, 14])
      #signal 1
      sig1idx <- which(channSigConfig[, 3] == calc1Val)
      sig1Val <- channSigConfig[sig1idx, 4]
      signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
      signal1Vec[signal1Vec == "NA"] <- NA
      signal1Vec <- signal1Vec[!is.na(signal1Vec)]
      assign(sig1Val, signal1Vec)
      #signal 2
      sig2idx <- which(channSigConfig[, 3] == calc2Val)
      sig2Val <- channSigConfig[sig2idx, 4]
      signal2Vec <- as.character(channSigConfig[sig2idx, 8:25])
      signal2Vec[signal2Vec == "NA"] <- NA
      signal2Vec <- signal2Vec[!is.na(signal2Vec)]
      assign(sig2Val, signal2Vec)
      # #Signal3 - Odo
      # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
      # sig3Val <- channSigConfig[sig3idx, 4]
      # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
      # signal3Vec[signal3Vec == "NA"] <- NA
      # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
      # assign(sig3Val, signal3Vec)
      #Signal 4
      sig4idx <- which(channSigConfig[, 3] == calc4Val)
      sig4Val <- channSigConfig[sig4idx, 4]
      signal4Vec <- as.character(channSigConfig[sig4idx, 8:25])
      signal4Vec[signal4Vec == "NA"] <- NA
      signal4Vec <- signal4Vec[!is.na(signal4Vec)]
      assign(sig4Val, signal4Vec)
      #signal threshold
      sig1min <- as.numeric(channSigConfig[sig1idx,5])
      sig1max <- as.numeric(channSigConfig[sig1idx,6])
      sig2min <- as.numeric(channSigConfig[sig2idx,5])
      sig2max <- as.numeric(channSigConfig[sig2idx,6])
      # #sig3min <- as.numeric(channSigConfig[sig3idx,5])
      # #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      sig4min <- as.numeric(channSigConfig[sig4idx,5])
      sig4max <- as.numeric(channSigConfig[sig4idx,6])
      
      
      # calc1Val <- getChann1(get(sig1Val), X,sig1min,sig1max)
      # calc2Val <- getChann1(get(sig2Val), X,sig2min,sig2max)
      # # calc5Odo <- getChann1(get(sig3Val), X,sig3min,sig3max)
      # calc4Val <- getChann1(get(sig4Val), X,sig4min,sig4max)
      # #Operating Time Calc
      # isOpTime <- (!is.na(calc4Val) & calc4Val > 0)
      # idStart3s <- startIndices1(calc4Val, calc2Val, 0)
      # assign(x = varName[1],
      #        value = do.call("countTable1D", args = list(
      #          x = calc1Val[idStart3s], mergeTab = get(varName)
      #        )))
      for (ri in 1:length(rleTrack$starts)) {
        X1 <- X[rleTrack$starts[ri]:rleTrack$stops[ri], ]
        calc1Val <- getChann1(get(sig1Val), X1,sig1min,sig1max)
        calc2Val <- getChann1(get(sig2Val), X1,sig2min,sig2max)
        calc4Val <- getChann1(get(sig4Val), X1,sig4min,sig4max)
        idStart3s <- startIndices1(calc4Val, calc2Val, 0)
        assign(x = varName[1],
               value = do.call("countTable1D", args = list(
                 x = calc1Val[idStart3s], mergeTab = get(varName)
               )))
        rm(X1)
      }
      
      # assign(varNameMile[1], c(max(
      #   0, max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
      # )))
      if (is.null(get(varName))) {
        f_idx <- which(mainConfig[, 1] == 2261)
        failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
        failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
        if(length(colName1) == 0){
          failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
        }else if(length(colName2) == 0){
          failSlides[fail_index,4] <-as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
        }
        fail_index <- fail_index + 1
      } else{
        ##saveRDS in the dir location
        g_idx <- which(mainConfig[, 1] == 2261)
        GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
        GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
        #summary of Signal 1
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName1 <- c(colName1,colName1)
        col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
        count1 <- sum(length(which(!is.na(X[,col1]))))
        perc1 <- round(count1/nrow(X)*100)
        GoodSlides[good_index,3] <- paste0(col1)
        GoodSlides[good_index,4] <- paste0(count1)
        GoodSlides[good_index,5] <- paste0(perc1,"%")
        #summary of Signal 2 
        colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
        colName2 <- c(colName2,colName2)
        col2 <- colName2[which.max(colSums(!is.na(X[,colName2])))]
        count2 <- sum(length(which(!is.na(X[,col1]))))
        perc2 <- round(count2/nrow(X)*100)
        GoodSlides[good_index,6] <- paste0(col2)
        GoodSlides[good_index,7] <- paste0(count2)
        GoodSlides[good_index,8] <- paste0(perc2,"%")
        #summary of Signal 3 
        # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
        # colName3 <- c(colName3,colName3)
        # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
        # count3 <- sum(length(which(!is.na(X[,col3]))))
        # perc3 <- round(count3/nrow(X)*100)
        # GoodSlides[good_index,9] <- paste0(col3)
        # GoodSlides[good_index,10] <- paste0(count3)
        # GoodSlides[good_index,11] <- paste0(perc3,"%")
        #summary of Signal 4 
        colName4 <- get(sig4Val)[get(sig4Val) %in% names(X)]
        colName4 <- c(colName4,colName4)
        col4 <- colName4[which.max(colSums(!is.na(X[,colName4])))]
        count4 <- sum(length(which(!is.na(X[,col4]))))
        perc4 <- round(count4/nrow(X)*100)
        GoodSlides[good_index,12] <- paste0(col4)
        GoodSlides[good_index,13] <- paste0(count4)
        GoodSlides[good_index,14] <- paste0(perc4,"%")
        
        
        good_index <- good_index + 1
        saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
        # saveRDS(get(varNameMile[1]), paste0(dir, varNameMile[1], ".rds"))
      }
      ##remove the variables to free up the memory
      endtime <- Sys.time()
      print(paste0(
        "total time for calculating ",
        varName[1],
        " is ",
        endtime - starttime
      ))
      rm(calc1Val, calc2Val, calc5Odo)
      rm(list = paste0(varName[1]))
      ## rm(list = paste0(varNameMile[1]))
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    }
    else if (gid[idx] == 2262){
      #2.15 Motorstart	2.15.7 Drehzahlverlauf des Verbrennungsmotors beim Start
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      ## for 10Hz Data
      print("This is for 10Hz Data")
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    } 
    else if (gid[idx] == 227) {
      #2.16 Temperatur	2.16.1 Auenlufttemperatur
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      starttime <- Sys.time()
      varName <- as.character(mainConfig[idx, 6])
      # varNameMile <- as.character(paste0(varName, "Mile"))
      assign(varName, NULL)
      # assign(varNameMile, NULL)
      calc1Val <- as.character(mainConfig[idx, 8])
      calc2Val <- as.character(mainConfig[idx, 13])
      #calc5Odo <- as.character(mainConfig[idx, 14])
      #signal 1
      sig1idx <- which(channSigConfig[, 3] == calc1Val)
      sig1Val <- channSigConfig[sig1idx, 4]
      signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
      signal1Vec[signal1Vec == "NA"] <- NA
      signal1Vec <- signal1Vec[!is.na(signal1Vec)]
      assign(sig1Val, signal1Vec)
      #signal 2
      sig2idx <- which(channSigConfig[, 3] == calc2Val)
      sig2Val <- channSigConfig[sig2idx, 4]
      signal2Vec <- as.character(channSigConfig[sig2idx, 8:25])
      signal2Vec[signal2Vec == "NA"] <- NA
      signal2Vec <- signal2Vec[!is.na(signal2Vec)]
      assign(sig2Val, signal2Vec)
      # #Signal3 - Odo
      # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
      # sig3Val <- channSigConfig[sig3idx, 4]
      # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
      # signal3Vec[signal3Vec == "NA"] <- NA
      # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
      # assign(sig3Val, signal3Vec)
      #signal threshold
      sig1min <- as.numeric(channSigConfig[sig1idx,5])
      sig1max <- as.numeric(channSigConfig[sig1idx,6])
      sig2min <- as.numeric(channSigConfig[sig2idx,5])
      sig2max <- as.numeric(channSigConfig[sig2idx,6])
      # #sig3min <- as.numeric(channSigConfig[sig3idx,5])
      # #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      
      
      calc1Val <- getChann1(get(sig1Val), X,sig1min,sig1max)
      calc2Val <- getChann1(get(sig2Val), X,sig2min,sig2max)
      # calc5Odo <- getChann1(get(sig3Val), X,sig3min,sig3max)
      assign(x = varName[1],
             value = do.call("countTable1D", args = list(
               x = calc1Val[calc2Val > 0], mergeTab = get(varName)
             )))
      # assign(varNameMile[1], c(max(
      #   0, max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
      # )))
      if (is.null(get(varName))) {
        f_idx <- which(mainConfig[, 1] == 227)
        failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
        failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
        if(length(colName1) == 0){
          failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
        }else if(length(colName2) == 0){
          failSlides[fail_index,4] <-as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
        }
        fail_index <- fail_index + 1
      } else{
        ##saveRDS in the dir location
        g_idx <- which(mainConfig[, 1] == 227)
        GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
        GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
        #summary of Signal 1
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName1 <- c(colName1,colName1)
        col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
        count1 <- sum(length(which(!is.na(X[,col1]))))
        perc1 <- round(count1/nrow(X)*100)
        GoodSlides[good_index,3] <- paste0(col1)
        GoodSlides[good_index,4] <- paste0(count1)
        GoodSlides[good_index,5] <- paste0(perc1,"%")
        #summary of Signal 2 
        colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
        colName2 <- c(colName2,colName2)
        col2 <- colName2[which.max(colSums(!is.na(X[,colName2])))]
        count2 <- sum(length(which(!is.na(X[,col1]))))
        perc2 <- round(count2/nrow(X)*100)
        GoodSlides[good_index,6] <- paste0(col2)
        GoodSlides[good_index,7] <- paste0(count2)
        GoodSlides[good_index,8] <- paste0(perc2,"%")
        #summary of Signal 3 
        # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
        # colName3 <- c(colName3,colName3)
        # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
        # count3 <- sum(length(which(!is.na(X[,col3]))))
        # perc3 <- round(count3/nrow(X)*100)
        # GoodSlides[good_index,9] <- paste0(col3)
        # GoodSlides[good_index,10] <- paste0(count3)
        # GoodSlides[good_index,11] <- paste0(perc3,"%")
        
        good_index <- good_index + 1
        saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
        # saveRDS(get(varNameMile[1]), paste0(dir, varNameMile[1], ".rds"))
      }
      ##remove the variables to free up the memory
      endtime <- Sys.time()
      print(paste0(
        "total time for calculating ",
        varName[1],
        " is ",
        endtime - starttime
      ))
      rm(calc1Val, calc2Val, calc5Odo)
      rm(list = paste0(varName[1]))
      ## rm(list = paste0(varNameMile[1]))
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    }
    else if (gid[idx] == 228) {
      #2.16 Temperatur	2.16.2 KÃ¼hlmitteltemperatur
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      starttime <- Sys.time()
      varName <- as.character(mainConfig[idx, 6])
      # varNameMile <- as.character(paste0(varName, "Mile"))
      assign(varName, NULL)
      # assign(varNameMile, NULL)
      calc1Val <- as.character(mainConfig[idx, 8])
      calc2Val <- as.character(mainConfig[idx, 13])
      #calc5Odo <- as.character(mainConfig[idx, 14])
      #signal 1
      sig1idx <- which(channSigConfig[, 3] == calc1Val)
      sig1Val <- channSigConfig[sig1idx, 4]
      signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
      signal1Vec[signal1Vec == "NA"] <- NA
      signal1Vec <- signal1Vec[!is.na(signal1Vec)]
      assign(sig1Val, signal1Vec)
      #signal 2
      sig2idx <- which(channSigConfig[, 3] == calc2Val)
      sig2Val <- channSigConfig[sig2idx, 4]
      signal2Vec <- as.character(channSigConfig[sig2idx, 8:25])
      signal2Vec[signal2Vec == "NA"] <- NA
      signal2Vec <- signal2Vec[!is.na(signal2Vec)]
      assign(sig2Val, signal2Vec)
      # #Signal3 - Odo
      # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
      # sig3Val <- channSigConfig[sig3idx, 4]
      # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
      # signal3Vec[signal3Vec == "NA"] <- NA
      # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
      # assign(sig3Val, signal3Vec)
      #signal threshold
      sig1min <- as.numeric(channSigConfig[sig1idx,5])
      sig1max <- as.numeric(channSigConfig[sig1idx,6])
      sig2min <- as.numeric(channSigConfig[sig2idx,5])
      sig2max <- as.numeric(channSigConfig[sig2idx,6])
      # #sig3min <- as.numeric(channSigConfig[sig3idx,5])
      # #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      
      calc1Val <- getChann1(get(sig1Val), X,sig1min,sig1max)
      calc2Val <- getChann1(get(sig2Val), X,sig2min,sig2max)
      # calc5Odo <- getChann1(get(sig3Val), X,sig3min,sig3max)
      assign(x = varName[1],
             value = do.call("countTable1D", args = list(
               x = calc1Val[calc2Val > 0], mergeTab = get(varName)
             )))
      # assign(varNameMile[1], c(max(
      #   0, max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
      # )))
      if (is.null(get(varName))) {
        f_idx <- which(mainConfig[, 1] == 228)
        failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
        failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
        if(length(colName1) == 0){
          failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
        }else if(length(colName2) == 0){
          failSlides[fail_index,4] <-as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
        }
        fail_index <- fail_index + 1
      } else{
        ##saveRDS in the dir location
        g_idx <- which(mainConfig[, 1] == 228)
        GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
        GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
        #summary of Signal 1
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName1 <- c(colName1,colName1)
        col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
        count1 <- sum(length(which(!is.na(X[,col1]))))
        perc1 <- round(count1/nrow(X)*100)
        GoodSlides[good_index,3] <- paste0(col1)
        GoodSlides[good_index,4] <- paste0(count1)
        GoodSlides[good_index,5] <- paste0(perc1,"%")
        #summary of Signal 2 
        colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
        colName2 <- c(colName2,colName2)
        col2 <- colName2[which.max(colSums(!is.na(X[,colName2])))]
        count2 <- sum(length(which(!is.na(X[,col1]))))
        perc2 <- round(count2/nrow(X)*100)
        GoodSlides[good_index,6] <- paste0(col2)
        GoodSlides[good_index,7] <- paste0(count2)
        GoodSlides[good_index,8] <- paste0(perc2,"%")
        #summary of Signal 3 
        # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
        # colName3 <- c(colName3,colName3)
        # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
        # count3 <- sum(length(which(!is.na(X[,col3]))))
        # perc3 <- round(count3/nrow(X)*100)
        # GoodSlides[good_index,9] <- paste0(col3)
        # GoodSlides[good_index,10] <- paste0(count3)
        # GoodSlides[good_index,11] <- paste0(perc3,"%")
        
        good_index <- good_index + 1
        saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
        # saveRDS(get(varNameMile[1]), paste0(dir, varNameMile[1], ".rds"))
      }
      ##remove the variables to free up the memory
      endtime <- Sys.time()
      print(paste0(
        "total time for calculating ",
        varName[1],
        " is ",
        endtime - starttime
      ))
      rm(calc1Val, calc2Val, calc5Odo)
      rm(list = paste0(varName[1]))
      ## rm(list = paste0(varNameMile[1]))
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
      
    }
    else if (gid[idx] == 229) {
      #2.16 Temperatur	2.16.3 Nidertemperturkreislauf im elektrichen Fahrbetrieb
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      failSlides[fail_index, 1] <- mainConfig[idx, 2]
      failSlides[fail_index, 2] <- mainConfig[idx, 3]
      failSlides[fail_index,3] <- "No Parameter Calculation Present"
      fail_index <- fail_index + 1
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
      
    } 
    else if (gid[idx] == 230) {
      #2.17 MotorÃ¶ltemperatur	2.17.1 Verweildauer pro Temperaturklasse
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      starttime <- Sys.time()
      varName <- as.character(mainConfig[idx, 6])
      # varNameMile <- as.character(paste0(varName, "Mile"))
      assign(varName, NULL)
      # assign(varNameMile, NULL)
      calc1Val <- as.character(mainConfig[idx, 8])
      calc2Val <- as.character(mainConfig[idx, 13])
      #calc5Odo <- as.character(mainConfig[idx, 14])
      #signal 1
      sig1idx <- which(channSigConfig[, 3] == calc1Val)
      sig1Val <- channSigConfig[sig1idx, 4]
      signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
      signal1Vec[signal1Vec == "NA"] <- NA
      signal1Vec <- signal1Vec[!is.na(signal1Vec)]
      assign(sig1Val, signal1Vec)
      #signal 2
      sig2idx <- which(channSigConfig[, 3] == calc2Val)
      sig2Val <- channSigConfig[sig2idx, 4]
      signal2Vec <- as.character(channSigConfig[sig2idx, 8:25])
      signal2Vec[signal2Vec == "NA"] <- NA
      signal2Vec <- signal2Vec[!is.na(signal2Vec)]
      assign(sig2Val, signal2Vec)
      # #Signal3 - Odo
      # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
      # sig3Val <- channSigConfig[sig3idx, 4]
      # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
      # signal3Vec[signal3Vec == "NA"] <- NA
      # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
      # assign(sig3Val, signal3Vec)
      #signal threshold
      sig1min <- as.numeric(channSigConfig[sig1idx,5])
      sig1max <- as.numeric(channSigConfig[sig1idx,6])
      sig2min <- as.numeric(channSigConfig[sig2idx,5])
      sig2max <- as.numeric(channSigConfig[sig2idx,6])
      #sig3min <- as.numeric(channSigConfig[sig3idx,5])
      #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      
      calc1Val <- getChann1(get(sig1Val), X,sig1min,sig1max)
      calc2Val <- getChann1(get(sig2Val), X,sig2min,sig2max)
      # calc5Odo <- getChann1(get(sig3Val), X,sig3min,sig3max)
      assign(x = varName[1],
             value = do.call("countTable1D", args = list(
               x = calc1Val[calc2Val > 0], mergeTab = get(varName)
             )))
      # assign(varNameMile[1], c(max(
      #   0, max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
      # )))
      if (is.null(get(varName))) {
        f_idx <- which(mainConfig[, 1] == 230)
        failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
        failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
        if(length(colName1) == 0){
          failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
        }else if(length(colName2) == 0){
          failSlides[fail_index,4] <-as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
        }
        fail_index <- fail_index + 1
      } else{
        ##saveRDS in the dir location
        g_idx <- which(mainConfig[, 1] == 230)
        GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
        GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
        #summary of Signal 1
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName1 <- c(colName1,colName1)
        col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
        count1 <- sum(length(which(!is.na(X[,col1]))))
        perc1 <- round(count1/nrow(X)*100)
        GoodSlides[good_index,3] <- paste0(col1)
        GoodSlides[good_index,4] <- paste0(count1)
        GoodSlides[good_index,5] <- paste0(perc1,"%")
        #summary of Signal 2 
        colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
        colName2 <- c(colName2,colName2)
        col2 <- colName2[which.max(colSums(!is.na(X[,colName2])))]
        count2 <- sum(length(which(!is.na(X[,col1]))))
        perc2 <- round(count2/nrow(X)*100)
        GoodSlides[good_index,6] <- paste0(col2)
        GoodSlides[good_index,7] <- paste0(count2)
        GoodSlides[good_index,8] <- paste0(perc2,"%")
        # #summary of Signal 3 
        # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
        # colName3 <- c(colName3,colName3)
        # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
        # count3 <- sum(length(which(!is.na(X[,col3]))))
        # perc3 <- round(count3/nrow(X)*100)
        # GoodSlides[good_index,9] <- paste0(col3)
        # GoodSlides[good_index,10] <- paste0(count3)
        # GoodSlides[good_index,11] <- paste0(perc3,"%")
        good_index <- good_index + 1
        saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
        # saveRDS(get(varNameMile[1]), paste0(dir, varNameMile[1], ".rds"))
      }
      ##remove the variables to free up the memory
      endtime <- Sys.time()
      print(paste0(
        "total time for calculating ",
        varName[1],
        " is ",
        endtime - starttime
      ))
      rm(calc1Val, calc2Val, calc5Odo)
      rm(list = paste0(varName[1]))
      ## rm(list = paste0(varNameMile[1]))
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    }
    else if (gid[idx] == 231) {
      #2.17 MotorÃ¶ltemperatur	2.17.2 Drehmoment-Ãltemperatur-Kennfeld
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      starttime <- Sys.time()
      varName <- as.character(mainConfig[idx, 6])
      varNameMile <- as.character(paste0(varName, "Mile"))
      assign(varName, NULL)
      # assign(varNameMile, NULL)
      calc1Val <- as.character(mainConfig[idx, 8])
      calc2Val <- as.character(mainConfig[idx, 9])
      calc4Val <- as.character(mainConfig[idx, 13])
      #calc5Odo <- as.character(mainConfig[idx, 14])
      #signal 1
      sig1idx <- which(channSigConfig[, 3] == calc1Val)
      sig1Val <- channSigConfig[sig1idx, 4]
      signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
      signal1Vec[signal1Vec == "NA"] <- NA
      signal1Vec <- signal1Vec[!is.na(signal1Vec)]
      assign(sig1Val, signal1Vec)
      #signal 2
      sig2idx <- which(channSigConfig[, 3] == calc2Val)
      sig2Val <- channSigConfig[sig2idx, 4]
      signal2Vec <- as.character(channSigConfig[sig2idx, 8:25])
      signal2Vec[signal2Vec == "NA"] <- NA
      signal2Vec <- signal2Vec[!is.na(signal2Vec)]
      assign(sig2Val, signal2Vec)
      #Signal3 - Odo
      # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
      # sig3Val <- channSigConfig[sig3idx, 4]
      # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
      # signal3Vec[signal3Vec == "NA"] <- NA
      # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
      # assign(sig3Val, signal3Vec)
      #Signal 4
      sig4idx <- which(channSigConfig[, 3] == calc4Val)
      sig4Val <- channSigConfig[sig4idx, 4]
      signal4Vec <- as.character(channSigConfig[sig4idx, 8:25])
      signal4Vec[signal4Vec == "NA"] <- NA
      signal4Vec <- signal4Vec[!is.na(signal4Vec)]
      assign(sig4Val, signal4Vec)
      #signal threshold
      sig1min <- as.numeric(channSigConfig[sig1idx,5])
      sig1max <- as.numeric(channSigConfig[sig1idx,6])
      sig2min <- as.numeric(channSigConfig[sig2idx,5])
      sig2max <- as.numeric(channSigConfig[sig2idx,6])
      # #sig3min <- as.numeric(channSigConfig[sig3idx,5])
      # #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      sig4min <- as.numeric(channSigConfig[sig4idx,5])
      sig4max <- as.numeric(channSigConfig[sig4idx,6])
      
      calc1Val <- getChann1(get(sig1Val), X,sig1min,sig1max)
      calc2Val <- getChann1(get(sig2Val), X,sig2min,sig2max)
      # calc5Odo <- getChann1(get(sig3Val), X,sig3min,sig3max)
      calc4Val <- getChann1(get(sig4Val), X,sig4min,sig4max)
      
      #Operating Time Calc
      isOpTime <- (!is.na(calc4Val) & calc4Val > 0)
      assign(x = varName[1],
             value = do.call(
               "countTable2D",
               args = list(
                 x = calc1Val[isOpTime],
                 y = calc2Val[isOpTime],
                 mergeTab = get(varName)
               )
             ))
      # assign(varNameMile[1], c(max(
      #   0, max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
      # )))
      if (is.null(get(varName))) {
        f_idx <- which(mainConfig[, 1] == 231)
        failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
        failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
        if(length(colName1) == 0){
          failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
        }else if(length(colName2) == 0){
          failSlides[fail_index,4] <-as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
        }
        fail_index <- fail_index + 1
      } else{
        ##saveRDS in the dir location
        g_idx <- which(mainConfig[, 1] == 231)
        GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
        GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
        #summary of Signal 1
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName1 <- c(colName1,colName1)
        col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
        count1 <- sum(length(which(!is.na(X[,col1]))))
        perc1 <- round(count1/nrow(X)*100)
        GoodSlides[good_index,3] <- paste0(col1)
        GoodSlides[good_index,4] <- paste0(count1)
        GoodSlides[good_index,5] <- paste0(perc1,"%")
        #summary of Signal 2 
        colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
        colName2 <- c(colName2,colName2)
        col2 <- colName2[which.max(colSums(!is.na(X[,colName2])))]
        count2 <- sum(length(which(!is.na(X[,col1]))))
        perc2 <- round(count2/nrow(X)*100)
        GoodSlides[good_index,6] <- paste0(col2)
        GoodSlides[good_index,7] <- paste0(count2)
        GoodSlides[good_index,8] <- paste0(perc2,"%")
        #summary of Signal 3 
        # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
        # colName3 <- c(colName3,colName3)
        # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
        # count3 <- sum(length(which(!is.na(X[,col3]))))
        # perc3 <- round(count3/nrow(X)*100)
        # GoodSlides[good_index,9] <- paste0(col3)
        # GoodSlides[good_index,10] <- paste0(count3)
        # GoodSlides[good_index,11] <- paste0(perc3,"%")
        #summary of Signal 4 
        colName4 <- get(sig4Val)[get(sig4Val) %in% names(X)]
        colName4 <- c(colName4,colName4)
        col4 <- colName4[which.max(colSums(!is.na(X[,colName4])))]
        count4 <- sum(length(which(!is.na(X[,col4]))))
        perc4 <- round(count4/nrow(X)*100)
        GoodSlides[good_index,12] <- paste0(col4)
        GoodSlides[good_index,13] <- paste0(count4)
        GoodSlides[good_index,14] <- paste0(perc4,"%")
        
        good_index <- good_index + 1
        saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
        # saveRDS(get(varNameMile[1]), paste0(dir, varNameMile[1], ".rds"))
      }
      ##remove the variables to free up the memory
      endtime <- Sys.time()
      print(paste0(
        "total time for calculating ",
        varName[1],
        " is ",
        endtime - starttime
      ))
      rm(calc1Val, calc2Val, calc5Odo)
      rm(list = paste0(varName[1]))
      # rm(list = paste0(varNameMile[1]))
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    }
    else if (gid[idx] == 232) {
      #2.17 MotorÃ¶ltemperatur	2.17.3 Drehzahl-Ãltemperatur-Kennfeld
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      starttime <- Sys.time()
      varName <- as.character(mainConfig[idx, 6])
      # varNameMile <- as.character(paste0(varName, "Mile"))
      assign(varName, NULL)
      # assign(varNameMile, NULL)
      calc1Val <- as.character(mainConfig[idx, 8])
      calc2Val <- as.character(mainConfig[idx, 9])
      #calc5Odo <- as.character(mainConfig[idx, 14])
      #signal 1
      sig1idx <- which(channSigConfig[, 3] == calc1Val)
      sig1Val <- channSigConfig[sig1idx, 4]
      signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
      signal1Vec[signal1Vec == "NA"] <- NA
      signal1Vec <- signal1Vec[!is.na(signal1Vec)]
      assign(sig1Val, signal1Vec)
      #signal 2
      sig2idx <- which(channSigConfig[, 3] == calc2Val)
      sig2Val <- channSigConfig[sig2idx, 4]
      signal2Vec <- as.character(channSigConfig[sig2idx, 8:25])
      signal2Vec[signal2Vec == "NA"] <- NA
      signal2Vec <- signal2Vec[!is.na(signal2Vec)]
      assign(sig2Val, signal2Vec)
      # #Signal3 - Odo
      # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
      # sig3Val <- channSigConfig[sig3idx, 4]
      # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
      # signal3Vec[signal3Vec == "NA"] <- NA
      # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
      # assign(sig3Val, signal3Vec)
      #signal threshold
      sig1min <- as.numeric(channSigConfig[sig1idx,5])
      sig1max <- as.numeric(channSigConfig[sig1idx,6])
      sig2min <- as.numeric(channSigConfig[sig2idx,5])
      sig2max <- as.numeric(channSigConfig[sig2idx,6])
      # #sig3min <- as.numeric(channSigConfig[sig3idx,5])
      # #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      
      calc1Val <- getChann1(get(sig1Val), X,sig1min,sig1max)
      calc2Val <- getChann1(get(sig2Val), X,sig2min,sig2max)
      # calc5Odo <- getChann1(get(sig3Val), X,sig3min,sig3max)
      assign(x = varName[1],
             value = do.call(
               "countTable2D",
               args = list(
                 x = calc1Val,
                 y = calc2Val,
                 mergeTab = get(varName)
               )
             ))
      # assign(varNameMile[1], c(max(
      #   0, max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
      # )))
      if (is.null(get(varName))) {
        f_idx <- which(mainConfig[, 1] == 232)
        failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
        failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
        if(length(colName1) == 0){
          failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
        }else if(length(colName2) == 0){
          failSlides[fail_index,4] <-as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
        }
        fail_index <- fail_index + 1
      } else{
        ##saveRDS in the dir location
        g_idx <- which(mainConfig[, 1] == 232)
        GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
        GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
        #summary of Signal 1
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName1 <- c(colName1,colName1)
        col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
        count1 <- sum(length(which(!is.na(X[,col1]))))
        perc1 <- round(count1/nrow(X)*100)
        GoodSlides[good_index,3] <- paste0(col1)
        GoodSlides[good_index,4] <- paste0(count1)
        GoodSlides[good_index,5] <- paste0(perc1,"%")
        #summary of Signal 2 
        colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
        colName2 <- c(colName2,colName2)
        col2 <- colName2[which.max(colSums(!is.na(X[,colName2])))]
        count2 <- sum(length(which(!is.na(X[,col1]))))
        perc2 <- round(count2/nrow(X)*100)
        GoodSlides[good_index,6] <- paste0(col2)
        GoodSlides[good_index,7] <- paste0(count2)
        GoodSlides[good_index,8] <- paste0(perc2,"%")
        #summary of Signal 3 
        # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
        # colName3 <- c(colName3,colName3)
        # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
        # count3 <- sum(length(which(!is.na(X[,col3]))))
        # perc3 <- round(count3/nrow(X)*100)
        # GoodSlides[good_index,9] <- paste0(col3)
        # GoodSlides[good_index,10] <- paste0(count3)
        # GoodSlides[good_index,11] <- paste0(perc3,"%")
        
        good_index <- good_index + 1
        saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
        # saveRDS(get(varNameMile[1]), paste0(dir, varNameMile[1], ".rds"))
      }
      ##remove the variables to free up the memory
      endtime <- Sys.time()
      print(paste0(
        "total time for calculating ",
        varName[1],
        " is ",
        endtime - starttime
      ))
      rm(calc1Val, calc2Val, calc5Odo)
      rm(list = paste0(varName[1]))
      ## rm(list = paste0(varNameMile[1]))
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    } 
    else if (gid[idx] == 233) {
      # 2.18 MotorÃ¶ldruck	2.18.1 Verweildauer pro Druckklasse
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      starttime <- Sys.time()
      varName <- as.character(mainConfig[idx, 6])
      #varNameMile <- as.character(paste0(varName, "Mile"))
      assign(varName, NULL)
      #assign(varNameMile, NULL)
      calc1Val <- as.character(mainConfig[idx, 8])
      calc2Val <- as.character(mainConfig[idx, 13])
      #calc5Odo <- as.character(mainConfig[idx, 14])
      #signal 1
      sig1idx <- which(channSigConfig[, 3] == calc1Val)
      sig1Val <- channSigConfig[sig1idx, 4]
      signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
      signal1Vec[signal1Vec == "NA"] <- NA
      signal1Vec <- signal1Vec[!is.na(signal1Vec)]
      assign(sig1Val, signal1Vec)
      #signal 2
      sig2idx <- which(channSigConfig[, 3] == calc2Val)
      sig2Val <- channSigConfig[sig2idx, 4]
      signal2Vec <- as.character(channSigConfig[sig2idx, 8:25])
      signal2Vec[signal2Vec == "NA"] <- NA
      signal2Vec <- signal2Vec[!is.na(signal2Vec)]
      assign(sig2Val, signal2Vec)
      #Signal3 - Odo
      # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
      # sig3Val <- channSigConfig[sig3idx, 4]
      # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
      # signal3Vec[signal3Vec == "NA"] <- NA
      # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
      # assign(sig3Val, signal3Vec)
      #signal threshold
      sig1min <- as.numeric(channSigConfig[sig1idx,5])
      sig1max <- as.numeric(channSigConfig[sig1idx,6])
      sig2min <- as.numeric(channSigConfig[sig2idx,5])
      sig2max <- as.numeric(channSigConfig[sig2idx,6])
      #sig3min <- as.numeric(channSigConfig[sig3idx,5])
      #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      
      calc1Val <- getChann1(get(sig1Val), X,sig1min,sig1max)
      if (length(calc1Val) > 0 &&
          sum(!is.na(calc1Val)) > 0 && mean(Mod(calc1Val), na.rm = TRUE) > 10) {
        calc1Val <- calc1Val / 1000
      }
      calc2Val <- getChann1(get(sig2Val), X,sig2min,sig2max)
      # calc5Odo <- getChann1(get(sig3Val), X,sig3min,sig3max)
      assign(x = varName[1],
             value = do.call("countTable1D", args = list(
               x = calc1Val[calc2Val > 0], mergeTab = get(varName)
             )))
      # assign(varNameMile[1], c(max(
      #   0, max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
      # )))
      if (is.null(get(varName))) {
        f_idx <- which(mainConfig[, 1] == 233)
        failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
        failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
        if(length(colName1) == 0){
          failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
        }else if(length(colName2) == 0){
          failSlides[fail_index,4] <-as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
        }
        fail_index <- fail_index + 1
      } else{
        ##saveRDS in the dir location
        g_idx <- which(mainConfig[, 1] == 233)
        GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
        GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
        #summary of Signal 1
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName1 <- c(colName1,colName1)
        col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
        count1 <- sum(length(which(!is.na(X[,col1]))))
        perc1 <- round(count1/nrow(X)*100)
        GoodSlides[good_index,3] <- paste0(col1)
        GoodSlides[good_index,4] <- paste0(count1)
        GoodSlides[good_index,5] <- paste0(perc1,"%")
        #summary of Signal 2 
        colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
        colName2 <- c(colName2,colName2)
        col2 <- colName2[which.max(colSums(!is.na(X[,colName2])))]
        count2 <- sum(length(which(!is.na(X[,col1]))))
        perc2 <- round(count2/nrow(X)*100)
        GoodSlides[good_index,6] <- paste0(col2)
        GoodSlides[good_index,7] <- paste0(count2)
        GoodSlides[good_index,8] <- paste0(perc2,"%")
        #summary of Signal 3 
        # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
        # colName3 <- c(colName3,colName3)
        # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
        # count3 <- sum(length(which(!is.na(X[,col3]))))
        # perc3 <- round(count3/nrow(X)*100)
        # GoodSlides[good_index,9] <- paste0(col3)
        # GoodSlides[good_index,10] <- paste0(count3)
        # GoodSlides[good_index,11] <- paste0(perc3,"%")
        
        good_index <- good_index + 1
        saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
        # saveRDS(get(varNameMile[1]), paste0(dir, varNameMile[1], ".rds"))
      }
      ##remove the variables to free up the memory
      endtime <- Sys.time()
      print(paste0(
        "total time for calculating ",
        varName[1],
        " is ",
        endtime - starttime
      ))
      rm(calc1Val, calc2Val, calc5Odo)
      rm(list = paste0(varName[1]))
      ## rm(list = paste0(varNameMile[1]))
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    }
    else if (gid[idx] == 234) {
      # 2.18 MotorÃ¶ldruck	2.18.2 Drehzahl-Ãldruck-Kennfeld
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      starttime <- Sys.time()
      varName <- as.character(mainConfig[idx, 6])
      # varNameMile <- as.character(paste0(varName, "Mile"))
      assign(varName, NULL)
      # assign(varNameMile, NULL)
      calc1Val <- as.character(mainConfig[idx, 8])
      calc2Val <- as.character(mainConfig[idx, 9])
      calc4Val <- as.character(mainConfig[idx, 13])
      #calc5Odo <- as.character(mainConfig[idx, 14])
      #signal 1
      sig1idx <- which(channSigConfig[, 3] == calc1Val)
      sig1Val <- channSigConfig[sig1idx, 4]
      signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
      signal1Vec[signal1Vec == "NA"] <- NA
      signal1Vec <- signal1Vec[!is.na(signal1Vec)]
      assign(sig1Val, signal1Vec)
      #signal 2
      sig2idx <- which(channSigConfig[, 3] == calc2Val)
      sig2Val <- channSigConfig[sig2idx, 4]
      signal2Vec <- as.character(channSigConfig[sig2idx, 8:25])
      signal2Vec[signal2Vec == "NA"] <- NA
      signal2Vec <- signal2Vec[!is.na(signal2Vec)]
      assign(sig2Val, signal2Vec)
      # #Signal3 - Odo
      # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
      # sig3Val <- channSigConfig[sig3idx, 4]
      # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
      # signal3Vec[signal3Vec == "NA"] <- NA
      # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
      # assign(sig3Val, signal3Vec)
      #Signal 4
      sig4idx <- which(channSigConfig[, 3] == calc4Val)
      sig4Val <- channSigConfig[sig4idx, 4]
      signal4Vec <- as.character(channSigConfig[sig4idx, 8:25])
      signal4Vec[signal4Vec == "NA"] <- NA
      signal4Vec <- signal4Vec[!is.na(signal4Vec)]
      assign(sig4Val, signal4Vec)
      #signal threshold
      sig1min <- as.numeric(channSigConfig[sig1idx,5])
      sig1max <- as.numeric(channSigConfig[sig1idx,6])
      sig2min <- as.numeric(channSigConfig[sig2idx,5])
      sig2max <- as.numeric(channSigConfig[sig2idx,6])
      # #sig3min <- as.numeric(channSigConfig[sig3idx,5])
      # #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      sig4min <- as.numeric(channSigConfig[sig4idx,5])
      sig4max <- as.numeric(channSigConfig[sig4idx,6])
      
      calc1Val <- getChann1(get(sig1Val), X,sig1min,sig1max)
      #calc2Val <- getChann1(get(sig2Val), X,sig2min,sig2max)
      calc2Val <- getChann(get(sig2Val),X)
      if(length(calc2Val)>0 && sum(!is.na(calc2Val))>0 && mean(Mod(calc2Val),na.rm=TRUE)>10){
        calc2Val <- calc2Val/1000
      }
      # calc5Odo <- getChann1(get(sig3Val), X,sig3min,sig3max)
      calc4Val <- getChann1(get(sig4Val), X,sig4min,sig4max)
      
      assign(x = varName[1],
             value = do.call(
               "countTable2D",
               args = list(
                 x = calc1Val,
                 y = calc2Val,
                 mergeTab = get(varName)
               )
             ))
      # assign(varNameMile[1], c(max(
      #   0, max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
      # )))
      if (is.null(get(varName))) {
        f_idx <- which(mainConfig[, 1] == 234)
        failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
        failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
        if(length(colName1) == 0){
          failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
        }else if(length(colName2) == 0){
          failSlides[fail_index,4] <-as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
        }
        fail_index <- fail_index + 1
      } else{
        ##saveRDS in the dir location
        g_idx <- which(mainConfig[, 1] == 234)
        GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
        GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
        #summary of Signal 1
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName1 <- c(colName1,colName1)
        col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
        count1 <- sum(length(which(!is.na(X[,col1]))))
        perc1 <- round(count1/nrow(X)*100)
        GoodSlides[good_index,3] <- paste0(col1)
        GoodSlides[good_index,4] <- paste0(count1)
        GoodSlides[good_index,5] <- paste0(perc1,"%")
        #summary of Signal 2 
        colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
        colName2 <- c(colName2,colName2)
        col2 <- colName2[which.max(colSums(!is.na(X[,colName2])))]
        count2 <- sum(length(which(!is.na(X[,col1]))))
        perc2 <- round(count2/nrow(X)*100)
        GoodSlides[good_index,6] <- paste0(col2)
        GoodSlides[good_index,7] <- paste0(count2)
        GoodSlides[good_index,8] <- paste0(perc2,"%")
        #summary of Signal 3 
        # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
        # colName3 <- c(colName3,colName3)
        # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
        # count3 <- sum(length(which(!is.na(X[,col3]))))
        # perc3 <- round(count3/nrow(X)*100)
        # GoodSlides[good_index,9] <- paste0(col3)
        # GoodSlides[good_index,10] <- paste0(count3)
        # GoodSlides[good_index,11] <- paste0(perc3,"%")
        #summary of Signal 4 
        colName4 <- get(sig4Val)[get(sig4Val) %in% names(X)]
        colName4 <- c(colName4,colName4)
        col4 <- colName4[which.max(colSums(!is.na(X[,colName4])))]
        count4 <- sum(length(which(!is.na(X[,col4]))))
        perc4 <- round(count4/nrow(X)*100)
        GoodSlides[good_index,12] <- paste0(col4)
        GoodSlides[good_index,13] <- paste0(count4)
        GoodSlides[good_index,14] <- paste0(perc4,"%")
        
        good_index <- good_index + 1
        saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
        # saveRDS(get(varNameMile[1]), paste0(dir, varNameMile[1], ".rds"))
      }
      ##remove the variables to free up the memory
      endtime <- Sys.time()
      print(paste0(
        "total time for calculating ",
        varName[1],
        " is ",
        endtime - starttime
      ))
      rm(calc1Val, calc2Val, calc5Odo)
      rm(list = paste0(varName[1]))
      ## rm(list = paste0(varNameMile[1]))
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    } 
    else if (gid[idx] == 235) {
      # 2.19 MotorÃ¶lstand
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      print("this is for motoroilstand")
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
      
    }
    else if (gid[idx] == 2351){
      # 2.19.1 Anzahl der ZAS-Schaltungen
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      starttime <- Sys.time()
      varName <- as.character(mainConfig[idx, 6])
      # varNameMile <- as.character(paste0(varName, "Mile"))
      assign(varName, NULL)
      # assign(varNameMile, NULL)
      calc1Val <- as.character(mainConfig[idx, 8])
      calc2Val <- as.character(mainConfig[idx, 9])
      calc4Val <- as.character(mainConfig[idx, 10])
      #calc5Odo <- as.character(mainConfig[idx, 14])
      #signal 1
      sig1idx <- which(channSigConfig[, 3] == calc1Val)
      sig1Val <- channSigConfig[sig1idx, 4]
      signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
      signal1Vec[signal1Vec == "NA"] <- NA
      signal1Vec <- signal1Vec[!is.na(signal1Vec)]
      assign(sig1Val, signal1Vec)
      #signal 2
      sig2idx <- which(channSigConfig[, 3] == calc2Val)
      sig2Val <- channSigConfig[sig2idx, 4]
      signal2Vec <- as.character(channSigConfig[sig2idx, 8:25])
      signal2Vec[signal2Vec == "NA"] <- NA
      signal2Vec <- signal2Vec[!is.na(signal2Vec)]
      assign(sig2Val, signal2Vec)
      # #Signal3 - Odo
      # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
      # sig3Val <- channSigConfig[sig3idx, 4]
      # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
      # signal3Vec[signal3Vec == "NA"] <- NA
      # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
      # assign(sig3Val, signal3Vec)
      #Signal 4
      sig4idx <- which(channSigConfig[, 3] == calc4Val)
      sig4Val <- channSigConfig[sig4idx, 4]
      signal4Vec <- as.character(channSigConfig[sig4idx, 8:25])
      signal4Vec[signal4Vec == "NA"] <- NA
      signal4Vec <- signal4Vec[!is.na(signal4Vec)]
      assign(sig4Val, signal4Vec)
      #signal threshold
      sig1min <- as.numeric(channSigConfig[sig1idx,5])
      sig1max <- as.numeric(channSigConfig[sig1idx,6])
      sig2min <- as.numeric(channSigConfig[sig2idx,5])
      sig2max <- as.numeric(channSigConfig[sig2idx,6])
      # #sig3min <- as.numeric(channSigConfig[sig3idx,5])
      # #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      sig4min <- as.numeric(channSigConfig[sig4idx,5])
      sig4max <- as.numeric(channSigConfig[sig4idx,6])
      
      calc1Val <- getChann1(get(sig1Val), X,sig1min,sig1max)
      calc2Val <- getChann1(get(sig2Val), X,sig2min,sig2max)
      # calc5Odo <- getChann1(get(sig3Val), X,sig3min,sig3max)
      calc4Val <- getChann1(get(sig4Val), X,sig4min,sig4max)
      
      c_idx <- which(diff(calc4Val) == 3)
      ##change on issue with shifting. changed on 16-1-2018 by Anish
      c_idx <- c_idx + 1
      calc1Val <- calc1Val[c_idx]
      calc2Val <- calc2Val[c_idx]
      calc4Val <- calc4Val[c_idx]
      if(!is.null(calc4Val)){
        assign(x = varName[1],
               value = do.call(
                 "countTable2D",
                 args = list(
                   x = calc1Val,
                   y = calc2Val,
                   mergeTab = get(varName)
                 )
               ))
      }
      else{
        assign(x=varName[1],NULL)
      }
      
      # assign(varNameMile[1], c(max(
      #   0, max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
      # )))
      if (is.null(get(varName))) {
        f_idx <- which(mainConfig[, 1] == 2351)
        failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
        failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
        if(length(colName1) == 0){
          failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
        }else if(length(colName2) == 0){
          failSlides[fail_index,4] <-as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
        }
        fail_index <- fail_index + 1
      } else{
        ##saveRDS in the dir location
        g_idx <- which(mainConfig[, 1] == 2351)
        GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
        GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
        #summary of Signal 1
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName1 <- c(colName1,colName1)
        col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
        count1 <- sum(length(which(!is.na(X[,col1]))))
        perc1 <- round(count1/nrow(X)*100)
        GoodSlides[good_index,3] <- paste0(col1)
        GoodSlides[good_index,4] <- paste0(count1)
        GoodSlides[good_index,5] <- paste0(perc1,"%")
        #summary of Signal 2 
        colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
        colName2 <- c(colName2,colName2)
        col2 <- colName2[which.max(colSums(!is.na(X[,colName2])))]
        count2 <- sum(length(which(!is.na(X[,col1]))))
        perc2 <- round(count2/nrow(X)*100)
        GoodSlides[good_index,6] <- paste0(col2)
        GoodSlides[good_index,7] <- paste0(count2)
        GoodSlides[good_index,8] <- paste0(perc2,"%")
        # #summary of Signal 3 
        # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
        # colName3 <- c(colName3,colName3)
        # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
        # count3 <- sum(length(which(!is.na(X[,col3]))))
        # perc3 <- round(count3/nrow(X)*100)
        # GoodSlides[good_index,9] <- paste0(col3)
        # GoodSlides[good_index,10] <- paste0(count3)
        # GoodSlides[good_index,11] <- paste0(perc3,"%")
        #summary of Signal 4 
        colName4 <- get(sig4Val)[get(sig4Val) %in% names(X)]
        colName4 <- c(colName4,colName4)
        print(colName4)
        col4 <- colName4[which.max(colSums(!is.na(X[,colName4])))]
        print(col4)
        count4 <- sum(length(which(!is.na(X[,col4]))))
        perc4 <- round(count4/nrow(X)*100)
        GoodSlides[good_index,12] <- paste0(col4)
        GoodSlides[good_index,13] <- paste0(count4)
        GoodSlides[good_index,14] <- paste0(perc4,"%")
        
        good_index <- good_index + 1
        saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
        # saveRDS(get(varNameMile[1]), paste0(dir, varNameMile[1], ".rds"))
        print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
      }
    }
    else if (gid[idx] == 2352){
      # 2.19.2 Zeitanteil ZAS-Betrieb
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      starttime <- Sys.time()
      varName <- as.character(mainConfig[idx, 6])
      #varNameMile <- as.character(paste0(varName, "Mile"))
      assign(varName, NULL)
      #assign(varNameMile, NULL)
      calc1Val <- as.character(mainConfig[idx, 8])
      calc2Val <- as.character(mainConfig[idx, 9])
      calc4Val <- as.character(mainConfig[idx, 10])
      #calc5Odo <- as.character(mainConfig[idx, 14])
      #signal 1
      sig1idx <- which(channSigConfig[, 3] == calc1Val)
      sig1Val <- channSigConfig[sig1idx, 4]
      signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
      signal1Vec[signal1Vec == "NA"] <- NA
      signal1Vec <- signal1Vec[!is.na(signal1Vec)]
      assign(sig1Val, signal1Vec)
      #signal 2
      sig2idx <- which(channSigConfig[, 3] == calc2Val)
      sig2Val <- channSigConfig[sig2idx, 4]
      signal2Vec <- as.character(channSigConfig[sig2idx, 8:25])
      signal2Vec[signal2Vec == "NA"] <- NA
      signal2Vec <- signal2Vec[!is.na(signal2Vec)]
      assign(sig2Val, signal2Vec)
      #Signal3 - Odo
      # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
      # sig3Val <- channSigConfig[sig3idx, 4]
      # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
      # signal3Vec[signal3Vec == "NA"] <- NA
      # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
      # assign(sig3Val, signal3Vec)
      #Signal 4
      sig4idx <- which(channSigConfig[, 3] == calc4Val)
      sig4Val <- channSigConfig[sig4idx, 4]
      signal4Vec <- as.character(channSigConfig[sig4idx, 8:25])
      signal4Vec[signal4Vec == "NA"] <- NA
      signal4Vec <- signal4Vec[!is.na(signal4Vec)]
      assign(sig4Val, signal4Vec)
      #signal threshold
      sig1min <- as.numeric(channSigConfig[sig1idx,5])
      sig1max <- as.numeric(channSigConfig[sig1idx,6])
      sig2min <- as.numeric(channSigConfig[sig2idx,5])
      sig2max <- as.numeric(channSigConfig[sig2idx,6])
      ##sig3min <- as.numeric(channSigConfig[sig3idx,5])
      #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      sig4min <- as.numeric(channSigConfig[sig4idx,5])
      sig4max <- as.numeric(channSigConfig[sig4idx,6])
      
      calc1Val <- getChann1(get(sig1Val), X,sig1min,sig1max)
      calc2Val <- getChann1(get(sig2Val), X,sig2min,sig2max)
      # calc5Odo <- getChann1(get(sig3Val), X,sig3min,sig3max)
      calc4Val <- getChann1(get(sig4Val), X,sig4min,sig4max)
      
      c_idx <- which(calc4Val == 3)
      calc1Val <- calc1Val[c_idx]
      calc2Val <- calc2Val[c_idx]
      calc4Val <- calc4Val[c_idx]
      
      if(!is.null(calc4Val)){
        assign(x = varName[1],
               value = do.call(
                 "countTable2D",
                 args = list(
                   x = calc1Val,
                   y = calc2Val,
                   mergeTab = get(varName)
                 )
               ))
      }
      else{
        assign(x=varName[1],NULL)
      }
      # # assign(varNameMile[1], c(max(
      #   0, max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
      # )))
      if (is.null(get(varName))) {
        f_idx <- which(mainConfig[, 1] == 2352)
        failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
        failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
        if(length(colName1) == 0){
          failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
        }else if(length(colName2) == 0){
          failSlides[fail_index,4] <-as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
        }
        fail_index <- fail_index + 1
      } else{
        ##saveRDS in the dir location
        g_idx <- which(mainConfig[, 1] == 2352)
        GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
        GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
        #summary of Signal 1
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName1 <- c(colName1,colName1)
        col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
        count1 <- sum(length(which(!is.na(X[,col1]))))
        perc1 <- round(count1/nrow(X)*100)
        GoodSlides[good_index,3] <- paste0(col1)
        GoodSlides[good_index,4] <- paste0(count1)
        GoodSlides[good_index,5] <- paste0(perc1,"%")
        #summary of Signal 2 
        colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
        colName2 <- c(colName2,colName2)
        col2 <- colName2[which.max(colSums(!is.na(X[,colName2])))]
        count2 <- sum(length(which(!is.na(X[,col1]))))
        perc2 <- round(count2/nrow(X)*100)
        GoodSlides[good_index,6] <- paste0(col2)
        GoodSlides[good_index,7] <- paste0(count2)
        GoodSlides[good_index,8] <- paste0(perc2,"%")
        # #summary of Signal 3 
        # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
        # colName3 <- c(colName3,colName3)
        # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
        # count3 <- sum(length(which(!is.na(X[,col3]))))
        # perc3 <- round(count3/nrow(X)*100)
        # GoodSlides[good_index,9] <- paste0(col3)
        # GoodSlides[good_index,10] <- paste0(count3)
        # GoodSlides[good_index,11] <- paste0(perc3,"%")
        #summary of Signal 4 
        colName4 <- get(sig4Val)[get(sig4Val) %in% names(X)]
        colName4 <- c(colName4,colName4)
        col4 <- colName4[which.max(colSums(!is.na(X[,colName4])))]
        count4 <- sum(length(which(!is.na(X[,col4]))))
        perc4 <- round(count4/nrow(X)*100)
        GoodSlides[good_index,12] <- paste0(col4)
        GoodSlides[good_index,13] <- paste0(count4)
        GoodSlides[good_index,14] <- paste0(perc4,"%")
        
        good_index <- good_index + 1
        saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
        # saveRDS(get(varNameMile[1]), paste0(dir, varNameMile[1], ".rds"))
        print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
      }
    }
    else if (gid[idx] == 2353){
      # 2.19.3 Laufstreckenverteilung
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      starttime <- Sys.time()
      varName <- as.character(mainConfig[idx, 6])
      # varNameMile <- as.character(paste0(varName, "Mile"))
      assign(varName, NULL)
      # assign(varNameMile, NULL)
      calc1Val <- as.character(mainConfig[idx, 8])
      calc2Val <- as.character(mainConfig[idx, 9])
      calc4Val <- as.character(mainConfig[idx, 10])
      calc5Odo <- as.character(mainConfig[idx, 14])
      #signal 1
      sig1idx <- which(channSigConfig[, 3] == calc1Val)
      sig1Val <- channSigConfig[sig1idx, 4]
      signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
      signal1Vec[signal1Vec == "NA"] <- NA
      signal1Vec <- signal1Vec[!is.na(signal1Vec)]
      assign(sig1Val, signal1Vec)
      #signal 2
      sig2idx <- which(channSigConfig[, 3] == calc2Val)
      sig2Val <- channSigConfig[sig2idx, 4]
      signal2Vec <- as.character(channSigConfig[sig2idx, 8:25])
      signal2Vec[signal2Vec == "NA"] <- NA
      signal2Vec <- signal2Vec[!is.na(signal2Vec)]
      assign(sig2Val, signal2Vec)
      #Signal3 - Odo
      sig3idx <- which(channSigConfig[, 3] == calc5Odo)
      sig3Val <- channSigConfig[sig3idx, 4]
      signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
      signal3Vec[signal3Vec == "NA"] <- NA
      signal3Vec <- signal3Vec[!is.na(signal3Vec)]
      assign(sig3Val, signal3Vec)
      #Signal 4
      sig4idx <- which(channSigConfig[, 3] == calc4Val)
      sig4Val <- channSigConfig[sig4idx, 4]
      signal4Vec <- as.character(channSigConfig[sig4idx, 8:25])
      signal4Vec[signal4Vec == "NA"] <- NA
      signal4Vec <- signal4Vec[!is.na(signal4Vec)]
      assign(sig4Val, signal4Vec)
      #signal threshold
      sig1min <- as.numeric(channSigConfig[sig1idx,5])
      sig1max <- as.numeric(channSigConfig[sig1idx,6])
      sig2min <- as.numeric(channSigConfig[sig2idx,5])
      sig2max <- as.numeric(channSigConfig[sig2idx,6])
      #sig3min <- as.numeric(channSigConfig[sig3idx,5])
      #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      sig4min <- as.numeric(channSigConfig[sig4idx,5])
      sig4max <- as.numeric(channSigConfig[sig4idx,6])
      
      calc1Val <- getChann1(get(sig1Val), X,sig1min,sig1max)
      calc2Val <- getChann1(get(sig2Val), X,sig2min,sig2max)
      #calc5Odo <- getChann1(get(sig3Val), X,sig3min,sig3max)
      calc4Val <- getChann1(get(sig4Val), X,sig4min,sig4max)
      
      if(!is.null(calc4Val)){
        testingDF <- as.data.frame(calc5Odo)
        diff_df <- testingDF[-1,] - testingDF[-nrow(testingDF),]
        diff_df <- as.data.frame(diff_df)
        library(data.table)
        diff_df <- setDF(shift(diff_df))[]
        diff_df[is.na(diff_df)] <- 0
        diff_df <- rbind(c(0.0),diff_df)
        
        #maindataDF <- as.data.frame(cbind(get(calc1Val),get(calc2Val),diff_df$V1))
        maindataDF <- as.data.frame(cbind(calc1Val,calc2Val,diff_df$V1))
        colnames(maindataDF) <- c("XEngSpd","XEngTor","XOdo")
        maindataDF <- subset(maindataDF,maindataDF$XOdo < 0.3)
        
        assign(x = varName[1],
               value = maindataDF )
      }
      else{
        assign(x=varName[1],NULL)
      }
      
      # assign(varNameMile[1], c(max(
      #   0, max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
      # )))
      if (is.null(get(varName))) {
        f_idx <- which(mainConfig[, 1] == 2353)
        failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
        failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
        if(length(colName1) == 0){
          failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
        }else if(length(colName2) == 0){
          failSlides[fail_index,4] <-as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
        }
        fail_index <- fail_index + 1
      } else{
        ##saveRDS in the dir location
        g_idx <- which(mainConfig[, 1] == 2353)
        GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
        GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
        #summary of Signal 1
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName1 <- c(colName1,colName1)
        col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
        count1 <- sum(length(which(!is.na(X[,col1]))))
        perc1 <- round(count1/nrow(X)*100)
        GoodSlides[good_index,3] <- paste0(col1)
        GoodSlides[good_index,4] <- paste0(count1)
        GoodSlides[good_index,5] <- paste0(perc1,"%")
        #summary of Signal 2 
        colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
        colName2 <- c(colName2,colName2)
        col2 <- colName2[which.max(colSums(!is.na(X[,colName2])))]
        count2 <- sum(length(which(!is.na(X[,col1]))))
        perc2 <- round(count2/nrow(X)*100)
        GoodSlides[good_index,6] <- paste0(col2)
        GoodSlides[good_index,7] <- paste0(count2)
        GoodSlides[good_index,8] <- paste0(perc2,"%")
        #summary of Signal 3 
        # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
        # colName3 <- c(colName3,colName3)
        # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
        # count3 <- sum(length(which(!is.na(X[,col3]))))
        # perc3 <- round(count3/nrow(X)*100)
        # GoodSlides[good_index,9] <- paste0(col3)
        # GoodSlides[good_index,10] <- paste0(count3)
        # GoodSlides[good_index,11] <- paste0(perc3,"%")
        #summary of Signal 4 
        colName4 <- get(sig4Val)[get(sig4Val) %in% names(X)]
        colName4 <- c(colName4,colName4)
        col4 <- colName4[which.max(colSums(!is.na(X[,colName4])))]
        count4 <- sum(length(which(!is.na(X[,col4]))))
        perc4 <- round(count4/nrow(X)*100)
        GoodSlides[good_index,12] <- paste0(col4)
        GoodSlides[good_index,13] <- paste0(count4)
        GoodSlides[good_index,14] <- paste0(perc4,"%")
        
        good_index <- good_index + 1
        saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
        # saveRDS(get(varNameMile[1]), paste0(dir, varNameMile[1], ".rds"))
        print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
      }    
    }
    else if (gid[idx] == 2354){
      # 2.19.4 Laufstrecke im ZAS-Betrieb
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      starttime <- Sys.time()
      varName <- as.character(mainConfig[idx, 6])
      # varNameMile <- as.character(paste0(varName, "Mile"))
      assign(varName, NULL)
      # assign(varNameMile, NULL)
      calc1Val <- as.character(mainConfig[idx, 8])
      calc2Val <- as.character(mainConfig[idx, 9])
      calc4Val <- as.character(mainConfig[idx, 10])
      #calc5Odo <- as.character(mainConfig[idx, 14])
      #signal 1
      sig1idx <- which(channSigConfig[, 3] == calc1Val)
      sig1Val <- channSigConfig[sig1idx, 4]
      signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
      signal1Vec[signal1Vec == "NA"] <- NA
      signal1Vec <- signal1Vec[!is.na(signal1Vec)]
      assign(sig1Val, signal1Vec)
      #signal 2
      sig2idx <- which(channSigConfig[, 3] == calc2Val)
      sig2Val <- channSigConfig[sig2idx, 4]
      signal2Vec <- as.character(channSigConfig[sig2idx, 8:25])
      signal2Vec[signal2Vec == "NA"] <- NA
      signal2Vec <- signal2Vec[!is.na(signal2Vec)]
      assign(sig2Val, signal2Vec)
      #Signal3 - Odo
      # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
      # sig3Val <- channSigConfig[sig3idx, 4]
      # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
      # signal3Vec[signal3Vec == "NA"] <- NA
      # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
      # assign(sig3Val, signal3Vec)
      #Signal 4
      sig4idx <- which(channSigConfig[, 3] == calc4Val)
      sig4Val <- channSigConfig[sig4idx, 4]
      signal4Vec <- as.character(channSigConfig[sig4idx, 8:25])
      signal4Vec[signal4Vec == "NA"] <- NA
      signal4Vec <- signal4Vec[!is.na(signal4Vec)]
      assign(sig4Val, signal4Vec)
      #signal threshold
      sig1min <- as.numeric(channSigConfig[sig1idx,5])
      sig1max <- as.numeric(channSigConfig[sig1idx,6])
      sig2min <- as.numeric(channSigConfig[sig2idx,5])
      sig2max <- as.numeric(channSigConfig[sig2idx,6])
      # sig3min <- as.numeric(channSigConfig[sig3idx,5])
      # sig3max <- as.numeric(channSigConfig[sig3idx,6])
      sig4min <- as.numeric(channSigConfig[sig4idx,5])
      sig4max <- as.numeric(channSigConfig[sig4idx,6])
      
      calc1Val <- getChann1(get(sig1Val), X,sig1min,sig1max)
      calc2Val <- getChann1(get(sig2Val), X,sig2min,sig2max)
      #calc5Odo <- getChann1(get(sig3Val), X,sig3min,sig3max)
      calc4Val <- getChann1(get(sig4Val), X,sig4min,sig4max)
      
      if(!is.null(calc4Val)){
        testingDF <- as.data.frame(calc5Odo)
        diff_df <- testingDF[-1,] - testingDF[-nrow(testingDF),]
        diff_df <- as.data.frame(diff_df)
        library(data.table)
        diff_df <- setDF(shift(diff_df))[]
        diff_df[is.na(diff_df)] <- 0
        diff_df <- rbind(c(0.0),diff_df)
        #maindataDF <- as.data.frame(cbind(get(calc1Val),get(calc2Val),get(calc4Val),diff_df$V1))
        maindataDF <- as.data.frame(cbind(calc1Val,calc2Val,calc4Val,diff_df$V1))
        colnames(maindataDF) <- c("XEngSpd","XEngTor","XCyclCutOff","XOdo")
        
        maindataDF <- subset(maindataDF,maindataDF$XCyclCutOff == 3)
        maindataDF <- subset(maindataDF,maindataDF$XOdo < 0.3)
        
        assign(x = varName[1],
               value = maindataDF )
      }else{
        assign(x=varName[1],NULL)
      }
      
      # assign(varNameMile[1], c(max(
      #   0, max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
      # )))
      if (is.null(get(varName))) {
        f_idx <- which(mainConfig[, 1] == 2354)
        failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
        failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
        if(length(colName1) == 0){
          failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
        }else if(length(colName2) == 0){
          failSlides[fail_index,4] <-as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
        }
        fail_index <- fail_index + 1
      } else{
        ##saveRDS in the dir location
        g_idx <- which(mainConfig[, 1] == 2354)
        GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
        GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
        #summary of Signal 1
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName1 <- c(colName1,colName1)
        col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
        count1 <- sum(length(which(!is.na(X[,col1]))))
        perc1 <- round(count1/nrow(X)*100)
        GoodSlides[good_index,3] <- paste0(col1)
        GoodSlides[good_index,4] <- paste0(count1)
        GoodSlides[good_index,5] <- paste0(perc1,"%")
        #summary of Signal 2 
        colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
        colName2 <- c(colName2,colName2)
        col2 <- colName2[which.max(colSums(!is.na(X[,colName2])))]
        count2 <- sum(length(which(!is.na(X[,col1]))))
        perc2 <- round(count2/nrow(X)*100)
        GoodSlides[good_index,6] <- paste0(col2)
        GoodSlides[good_index,7] <- paste0(count2)
        GoodSlides[good_index,8] <- paste0(perc2,"%")
        #summary of Signal 3 
        # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
        # colName3 <- c(colName3,colName3)
        # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
        # count3 <- sum(length(which(!is.na(X[,col3]))))
        # perc3 <- round(count3/nrow(X)*100)
        # GoodSlides[good_index,9] <- paste0(col3)
        # GoodSlides[good_index,10] <- paste0(count3)
        # GoodSlides[good_index,11] <- paste0(perc3,"%")
        #summary of Signal 4 
        colName4 <- get(sig4Val)[get(sig4Val) %in% names(X)]
        colName4 <- c(colName4,colName4)
        col4 <- colName4[which.max(colSums(!is.na(X[,colName4])))]
        count4 <- sum(length(which(!is.na(X[,col4]))))
        perc4 <- round(count4/nrow(X)*100)
        GoodSlides[good_index,12] <- paste0(col4)
        GoodSlides[good_index,13] <- paste0(count4)
        GoodSlides[good_index,14] <- paste0(perc4,"%")
        
        good_index <- good_index + 1
        saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
        # saveRDS(get(varNameMile[1]), paste0(dir, varNameMile[1], ".rds"))
        print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
      }
    }
    else if (gid[idx] == 2355){
      # 2.19.5 Durchschnittliche Verweildauer im ZAS-Betrieb
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      starttime <- Sys.time()
      varName <- as.character(mainConfig[idx, 6])
      # varNameMile <- as.character(paste0(varName, "Mile"))
      assign(varName, NULL)
      # assign(varNameMile, NULL)
      calc1Val <- as.character(mainConfig[idx, 8])
      calc2Val <- as.character(mainConfig[idx, 9])
      calc4Val <- as.character(mainConfig[idx, 10])
      #calc5Odo <- as.character(mainConfig[idx, 14])
      #signal 1
      sig1idx <- which(channSigConfig[, 3] == calc1Val)
      sig1Val <- channSigConfig[sig1idx, 4]
      signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
      signal1Vec[signal1Vec == "NA"] <- NA
      signal1Vec <- signal1Vec[!is.na(signal1Vec)]
      assign(sig1Val, signal1Vec)
      #signal 2
      sig2idx <- which(channSigConfig[, 3] == calc2Val)
      sig2Val <- channSigConfig[sig2idx, 4]
      signal2Vec <- as.character(channSigConfig[sig2idx, 8:25])
      signal2Vec[signal2Vec == "NA"] <- NA
      signal2Vec <- signal2Vec[!is.na(signal2Vec)]
      assign(sig2Val, signal2Vec)
      # #Signal3 - Odo
      # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
      # sig3Val <- channSigConfig[sig3idx, 4]
      # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
      # signal3Vec[signal3Vec == "NA"] <- NA
      # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
      # assign(sig3Val, signal3Vec)
      #Signal 4
      sig4idx <- which(channSigConfig[, 3] == calc4Val)
      sig4Val <- channSigConfig[sig4idx, 4]
      signal4Vec <- as.character(channSigConfig[sig4idx, 8:25])
      signal4Vec[signal4Vec == "NA"] <- NA
      signal4Vec <- signal4Vec[!is.na(signal4Vec)]
      assign(sig4Val, signal4Vec)
      #signal threshold
      sig1min <- as.numeric(channSigConfig[sig1idx,5])
      sig1max <- as.numeric(channSigConfig[sig1idx,6])
      sig2min <- as.numeric(channSigConfig[sig2idx,5])
      sig2max <- as.numeric(channSigConfig[sig2idx,6])
      #sig3min <- as.numeric(channSigConfig[sig3idx,5])
      #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      sig4min <- as.numeric(channSigConfig[sig4idx,5])
      sig4max <- as.numeric(channSigConfig[sig4idx,6])
      
      calc1Val <- getChann1(get(sig1Val), X,sig1min,sig1max)
      calc2Val <- getChann1(get(sig2Val), X,sig2min,sig2max)
      #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      calc4Val <- getChann1(get(sig4Val), X,sig4min,sig4max)
      
      if(!is.null(calc4Val))
      {
        maindatadf <- data.frame(calc1Val,calc2Val,calc4Val)
        #maindatadf <- maindatadf[!is.na(maindatadf$XCyclCutOff)]
        colnames(maindatadf) <- c("XEngSpd","XEngTor","XCyclCutOff")
        maindatadf <- subset(maindatadf,!is.na(maindatadf["XCyclCutOff"]))
        library(accelerometry)
        rleMain <- rle2(maindatadf$XCyclCutOff,indices = T,return.list = T)
        temp1DF <- data.frame(engspd = as.numeric(),engtrq = as.numeric(),cylcutoff = as.numeric())
        ##looping to find only CylCutoff = 3
        for (idx in 1:length(rleMain$values)){
          if(rleMain$values[idx] == 3)
          {
            #print("I am @ three")
            #print(idx)
            startIdx <- rleMain$starts[idx]
            stopIdx <- rleMain$stops[idx]
            v1 <- maindatadf[startIdx:stopIdx,1:2]
            v2 <- 1/length(maindatadf$XCyclCutOff[startIdx:stopIdx])
            v3 <- cbind(v1,v2)
            temp1DF<-rbind(temp1DF,v3)
          }
        }
        
        print(temp1DF)
        assign(x = varName[1],
               value = temp1DF)
        
      }else{
        assign(x=varName[1],NULL)
      }
      #       assign(varNameMile[1], c(max(
      #   0, max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
      # )))
      if (is.null(get(varName))) {
        f_idx <- which(mainConfig[, 1] == 2355)
        failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
        failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
        if(length(colName1) == 0){
          failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
        }else if(length(colName2) == 0){
          failSlides[fail_index,4] <-as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
        }
        fail_index <- fail_index + 1
      } else{
        ##saveRDS in the dir location
        g_idx <- which(mainConfig[, 1] == 2355)
        GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
        GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
        #summary of Signal 1
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName1 <- c(colName1,colName1)
        col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
        count1 <- sum(length(which(!is.na(X[,col1]))))
        perc1 <- round(count1/nrow(X)*100)
        GoodSlides[good_index,3] <- paste0(col1)
        GoodSlides[good_index,4] <- paste0(count1)
        GoodSlides[good_index,5] <- paste0(perc1,"%")
        #summary of Signal 2 
        colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
        colName2 <- c(colName2,colName2)
        col2 <- colName2[which.max(colSums(!is.na(X[,colName2])))]
        count2 <- sum(length(which(!is.na(X[,col1]))))
        perc2 <- round(count2/nrow(X)*100)
        GoodSlides[good_index,6] <- paste0(col2)
        GoodSlides[good_index,7] <- paste0(count2)
        GoodSlides[good_index,8] <- paste0(perc2,"%")
        # #summary of Signal 3 
        # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
        # colName3 <- c(colName3,colName3)
        # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
        # count3 <- sum(length(which(!is.na(X[,col3]))))
        # perc3 <- round(count3/nrow(X)*100)
        # GoodSlides[good_index,9] <- paste0(col3)
        # GoodSlides[good_index,10] <- paste0(count3)
        # GoodSlides[good_index,11] <- paste0(perc3,"%")
        #summary of Signal 4 
        colName4 <- get(sig4Val)[get(sig4Val) %in% names(X)]
        colName4 <- c(colName4,colName4)
        col4 <- colName4[which.max(colSums(!is.na(X[,colName4])))]
        count4 <- sum(length(which(!is.na(X[,col4]))))
        perc4 <- round(count4/nrow(X)*100)
        GoodSlides[good_index,12] <- paste0(col4)
        GoodSlides[good_index,13] <- paste0(count4)
        GoodSlides[good_index,14] <- paste0(perc4,"%")
        
        good_index <- good_index + 1
        saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
        # saveRDS(get(varNameMile[1]), paste0(dir, varNameMile[1], ".rds"))
        
      }
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    }
    else if (gid[idx] == 2356){
      # 2.19.6 ZAS-Schaltungensarten
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      starttime <- Sys.time()
      varName <- as.character(mainConfig[idx, 6])
      # varNameMile <- as.character(paste0(varName, "Mile"))
      assign(varName, NULL)
      # assign(varNameMile, NULL)
      calc1Val <- as.character(mainConfig[idx, 8])
      #calc5Odo <- as.character(mainConfig[idx, 14])
      #signal 1
      sig1idx <- which(channSigConfig[, 3] == calc1Val)
      sig1Val <- channSigConfig[sig1idx, 4]
      signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
      signal1Vec[signal1Vec == "NA"] <- NA
      signal1Vec <- signal1Vec[!is.na(signal1Vec)]
      assign(sig1Val, signal1Vec)
      #Signal3 - Odo
      # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
      # sig3Val <- channSigConfig[sig3idx, 4]
      # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
      # signal3Vec[signal3Vec == "NA"] <- NA
      # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
      # assign(sig3Val, signal3Vec)
      #signal threshold
      sig1min <- as.numeric(channSigConfig[sig1idx,5])
      sig1max <- as.numeric(channSigConfig[sig1idx,6])
      #sig3min <- as.numeric(channSigConfig[sig3idx,5])
      #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      
      calc1Val <- getChann1(get(sig1Val), X,sig1min,sig1max)
      #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      
      if(!is.null(calc1Val))
      {
        library(accelerometry)
        rle1 <- rle2(calc1Val, return.list = TRUE, indices = TRUE)
        
        #rle1$values
        
        x1 <-rle1$values
        pairs <- data.frame(first = head(x1,-1),second = tail(x1,-1))
        t<-table(pairs)
        
        assign(x = varName[1],
               value = t)
      }else{
        assign(x=varName[1],NULL)
      }
      
      # assign(varNameMile[1], c(max(
      #   0, max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
      # )))
      if (is.null(get(varName))) {
        f_idx <- which(mainConfig[, 1] == 2356)
        failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
        failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
        if(length(colName1) == 0){
          failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
        }else if(length(colName2) == 0){
          failSlides[fail_index,4] <-as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
        }
        fail_index <- fail_index + 1
      } else{
        ##saveRDS in the dir location
        g_idx <- which(mainConfig[, 1] == 2356)
        GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
        GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
        #summary of Signal 1
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName1 <- c(colName1,colName1)
        col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
        count1 <- sum(length(which(!is.na(X[,col1]))))
        perc1 <- round(count1/nrow(X)*100)
        GoodSlides[good_index,3] <- paste0(col1)
        GoodSlides[good_index,4] <- paste0(count1)
        GoodSlides[good_index,5] <- paste0(perc1,"%")
        #summary of Signal 3 
        # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
        # colName3 <- c(colName3,colName3)
        # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
        # count3 <- sum(length(which(!is.na(X[,col3]))))
        # perc3 <- round(count3/nrow(X)*100)
        # GoodSlides[good_index,9] <- paste0(col3)
        # GoodSlides[good_index,10] <- paste0(count3)
        # GoodSlides[good_index,11] <- paste0(perc3,"%")
        good_index <- good_index + 1
        saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
        # saveR#DS(get(varNameMile[1]), paste0(dir, varNameMile[1], ".rds"))
        print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
      }
    }
    else if (gid[idx] == 236) {
      # 2.20 Elektrische Wasserpumpe	2.20.1 Leistung
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      failSlides[fail_index, 1] <- mainConfig[idx, 2]
      failSlides[fail_index, 2] <- mainConfig[idx, 3]
      failSlides[fail_index,3] <- "No Parameter Calculation Present"
      fail_index <- fail_index + 1
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    } 
    else if (gid[idx] == 237) {
      # 2.20 Elektrische Wasserpumpe	2.20.2 An/Aus-Zyklen
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      failSlides[fail_index, 1] <- mainConfig[idx, 2]
      failSlides[fail_index, 2] <- mainConfig[idx, 3]
      failSlides[fail_index,3] <- "No Parameter Calculation Present"
      fail_index <- fail_index + 1
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    }
    else if (gid[idx] == 238) {
      # 2.21 Camtronic	2.21.1 Anzahl Schaltugen
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      failSlides[fail_index, 1] <- mainConfig[idx, 2]
      failSlides[fail_index, 2] <- mainConfig[idx, 3]
      failSlides[fail_index,3] <- "No Parameter Calculation Present"
      fail_index <- fail_index + 1
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    }
    else if (gid[idx] == 239) {
      # 2.21 Camtronic	2.21.2 Schaltstufen
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      failSlides[fail_index, 1] <- mainConfig[idx, 2]
      failSlides[fail_index, 2] <- mainConfig[idx, 3]
      failSlides[fail_index,3] <- "No Parameter Calculation Present"
      fail_index <- fail_index + 1
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    }
    else if (gid[idx] == 240) {
      # 2.22 Nockenwelle	2.22.1 Verstellwinkel
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      failSlides[fail_index, 1] <- mainConfig[idx, 2]
      failSlides[fail_index, 2] <- mainConfig[idx, 3]
      failSlides[fail_index,3] <- "No Parameter Calculation Present"
      fail_index <- fail_index + 1
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    }
    else if (gid[idx] == 241) {
      # 2.22 Nockenwelle	2.22.2 Anzahl Ver- und EntriegelungsvorgÃ¤nge
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      failSlides[fail_index, 1] <- mainConfig[idx, 2]
      failSlides[fail_index, 2] <- mainConfig[idx, 3]
      failSlides[fail_index,3] <- "No Parameter Calculation Present"
      fail_index <- fail_index + 1
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    } 
    else if (gid[idx] == 242) {
      # 2.23 Abgasturbolader	2.23.1 Temperaturwechsel
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      failSlides[fail_index, 1] <- mainConfig[idx, 2]
      failSlides[fail_index, 2] <- mainConfig[idx, 3]
      failSlides[fail_index,3] <- "No Parameter Calculation Present"
      fail_index <- fail_index + 1
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    } 
    else if (gid[idx] == 243) {
      # 2.23 Abgasturbolader	2.23.2 Turbinendrehzahlgradient
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      failSlides[fail_index, 1] <- mainConfig[idx, 2]
      failSlides[fail_index, 2] <- mainConfig[idx, 3]
      failSlides[fail_index,3] <- "No Parameter Calculation Present"
      fail_index <- fail_index + 1
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    } 
    else if (gid[idx] == 244) {
      # 2.23 Abgasturbolader	2.23.3 T3-Abgastemperatur
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      starttime <- Sys.time()
      varName <- as.character(mainConfig[idx, 6])
      # varNameMile <- as.character(paste0(varName, "Mile"))
      assign(varName, NULL)
      # assign(varNameMile, NULL)
      calc1Val <- as.character(mainConfig[idx, 8])
      calc2Val <- as.character(mainConfig[idx, 13])
      #calc5Odo <- as.character(mainConfig[idx, 14])
      #signal 1
      sig1idx <- which(channSigConfig[, 3] == calc1Val)
      sig1Val <- channSigConfig[sig1idx, 4]
      signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
      signal1Vec[signal1Vec == "NA"] <- NA
      signal1Vec <- signal1Vec[!is.na(signal1Vec)]
      assign(sig1Val, signal1Vec)
      #signal 2
      sig2idx <- which(channSigConfig[, 3] == calc2Val)
      sig2Val <- channSigConfig[sig2idx, 4]
      signal2Vec <- as.character(channSigConfig[sig2idx, 8:25])
      signal2Vec[signal2Vec == "NA"] <- NA
      signal2Vec <- signal2Vec[!is.na(signal2Vec)]
      assign(sig2Val, signal2Vec)
      # #Signal3 - Odo
      # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
      # sig3Val <- channSigConfig[sig3idx, 4]
      # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
      # signal3Vec[signal3Vec == "NA"] <- NA
      # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
      # assign(sig3Val, signal3Vec)
      #signal threshold
      sig1min <- as.numeric(channSigConfig[sig1idx,5])
      sig1max <- as.numeric(channSigConfig[sig1idx,6])
      sig2min <- as.numeric(channSigConfig[sig2idx,5])
      sig2max <- as.numeric(channSigConfig[sig2idx,6])
      #sig3min <- as.numeric(channSigConfig[sig3idx,5])
      #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      
      
      calc1Val <- getChann1(get(sig1Val), X,sig1min,sig1max)
      calc2Val <- getChann1(get(sig2Val), X,sig2min,sig2max)
      assign(x = varName[1],
             value = do.call("countTable1D", args = list(
               x = calc1Val[calc2Val > 0], mergeTab = get(varName)
             )))
      # assign(varNameMile[1], c(max(
      #   0, max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
      # )))
      if (is.null(get(varName))) {
        f_idx <- which(mainConfig[, 1] == 244)
        failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
        failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
        if(length(colName1) == 0){
          failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
        }else if(length(colName2) == 0){
          failSlides[fail_index,4] <-as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
        }
        fail_index <- fail_index + 1
      } else{
        ##saveRDS in the dir location
        g_idx <- which(mainConfig[, 1] == 244)
        GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
        GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
        if(length(colName1) == 0){
          failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
        }else if(length(colName2) == 0){
          failSlides[fail_index,4] <-as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
        }
        #summary of Signal 1
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName1 <- c(colName1,colName1)
        col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
        count1 <- sum(length(which(!is.na(X[,col1]))))
        perc1 <- round(count1/nrow(X)*100)
        GoodSlides[good_index,3] <- paste0(col1)
        GoodSlides[good_index,4] <- paste0(count1)
        GoodSlides[good_index,5] <- paste0(perc1,"%")
        #summary of Signal 2 
        colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
        colName2 <- c(colName2,colName2)
        col2 <- colName2[which.max(colSums(!is.na(X[,colName2])))]
        count2 <- sum(length(which(!is.na(X[,col1]))))
        perc2 <- round(count2/nrow(X)*100)
        GoodSlides[good_index,6] <- paste0(col2)
        GoodSlides[good_index,7] <- paste0(count2)
        GoodSlides[good_index,8] <- paste0(perc2,"%")
        # #summary of Signal 3 
        # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
        # colName3 <- c(colName3,colName3)
        # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
        # count3 <- sum(length(which(!is.na(X[,col3]))))
        # perc3 <- round(count3/nrow(X)*100)
        # GoodSlides[good_index,9] <- paste0(col3)
        # GoodSlides[good_index,10] <- paste0(count3)
        # GoodSlides[good_index,11] <- paste0(perc3,"%")
        # 
        #         GoodSlides[good_index,15] <- paste0(col5)
        #         GoodSlides[good_index,16] <- paste0(count5)
        #         GoodSlides[good_index,17] <- paste0(perc5,"%")
        good_index <- good_index + 1
        saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
        # saveRDS(get(varNameMile[1]), paste0(dir, varNameMile[1], ".rds"))
      }
      ##remove the variables to free up the memory
      endtime <- Sys.time()
      print(paste0(
        "total time for calculating ",
        varName[1],
        " is ",
        endtime - starttime
      ))
      rm(calc1Val, calc2Val, calc5Odo)
      rm(list = paste0(varName[1]))
      ## rm(list = paste0(varNameMile[1]))
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    } 
    else if (gid[idx] == 2441){
      # 2.23 Abgasturbolader	2.23.4 Drehzahl-T3-Abgastemperatur-Kennfeld
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      starttime <- Sys.time()
      varName <- as.character(mainConfig[idx, 6])
      # varNameMile <- as.character(paste0(varName, "Mile"))
      assign(varName, NULL)
      # assign(varNameMile, NULL)
      calc1Val <- as.character(mainConfig[idx, 8])
      calc2Val <- as.character(mainConfig[idx, 9])
      #calc4Val <- as.character(mainConfig[idx, 13])
      #calc5Odo <- as.character(mainConfig[idx, 14])
      #signal 1
      sig1idx <- which(channSigConfig[, 3] == calc1Val)
      sig1Val <- channSigConfig[sig1idx, 4]
      signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
      signal1Vec[signal1Vec == "NA"] <- NA
      signal1Vec <- signal1Vec[!is.na(signal1Vec)]
      assign(sig1Val, signal1Vec)
      #signal 2
      sig2idx <- which(channSigConfig[, 3] == calc2Val)
      sig2Val <- channSigConfig[sig2idx, 4]
      signal2Vec <- as.character(channSigConfig[sig2idx, 8:25])
      signal2Vec[signal2Vec == "NA"] <- NA
      signal2Vec <- signal2Vec[!is.na(signal2Vec)]
      assign(sig2Val, signal2Vec)
      #Signal3 - Odo
      # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
      # sig3Val <- channSigConfig[sig3idx, 4]
      # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
      # signal3Vec[signal3Vec == "NA"] <- NA
      # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
      # assign(sig3Val, signal3Vec)
      
      #signal threshold
      sig1min <- as.numeric(channSigConfig[sig1idx,5])
      sig1max <- as.numeric(channSigConfig[sig1idx,6])
      sig2min <- as.numeric(channSigConfig[sig2idx,5])
      sig2max <- as.numeric(channSigConfig[sig2idx,6])
      #sig3min <- as.numeric(channSigConfig[sig3idx,5])
      #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      
      for (ri in 1:length(rleTrack$starts)){
        X1 <- X[rleTrack$starts[ri]:rleTrack$stops[ri],]
        calc1Val <- getChann1(get(sig1Val), X1,sig1min,sig1max)
        calc2Val <- getChann1(get(sig2Val), X1,sig2min,sig2max)
        #Operating Time Calc
        assign(x = varName[1],
               value = do.call(
                 "countTable2D",
                 args = list(
                   x = calc1Val,
                   y = calc2Val,
                   mergeTab = get(varName)
                 )
               ))
      }
      # assign(varNameMile[1], c(max(
      #   0, max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
      # )))
      if (is.null(get(varName))) {
        f_idx <- which(mainConfig[, 1] == 2441)
        failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
        failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
        if(length(colName1) == 0){
          failSlides[fail_index,3] <- as.character(paste0("None of the Signals", paste(get(sig1Val),collapse = ";"),"Not present."))
        }else if(length(colName2) == 0){
          failSlides[fail_index,4] <- as.character(paste0("None of the Signals", paste(get(sig2Val),collapse = ";"),"Not present."))
        }
        fail_index <- fail_index + 1
      } else{
        ##saveRDS in the dir location
        g_idx <- which(mainConfig[, 1] == 2441)
        GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
        GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
        #summary of Signal 1
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName1 <- c(colName1,colName1)
        col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
        count1 <- sum(length(which(!is.na(X[,col1]))))
        perc1 <- round(count1/nrow(X)*100)
        GoodSlides[good_index,3] <- paste0(col1)
        GoodSlides[good_index,4] <- paste0(count1)
        GoodSlides[good_index,5] <- paste0(perc1,"%")
        #summary of Signal 2 
        colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
        colName2 <- c(colName2,colName2)
        col2 <- colName2[which.max(colSums(!is.na(X[,colName2])))]
        count2 <- sum(length(which(!is.na(X[,col1]))))
        perc2 <- round(count2/nrow(X)*100)
        GoodSlides[good_index,6] <- paste0(col2)
        GoodSlides[good_index,7] <- paste0(count2)
        GoodSlides[good_index,8] <- paste0(perc2,"%")
        #summary of Signal 3 
        # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
        # colName3 <- c(colName3,colName3)
        # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
        # count3 <- sum(length(which(!is.na(X[,col3]))))
        # perc3 <- round(count3/nrow(X)*100)
        # GoodSlides[good_index,9] <- paste0(col3)
        # GoodSlides[good_index,10] <- paste0(count3)
        # GoodSlides[good_index,11] <- paste0(perc3,"%")
        
        
        good_index <- good_index + 1
        saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
        # saveRDS(get(varNameMile[1]), paste0(dir, varNameMile[1], ".rds"))
      }
      ##remove the variables to free up the memory
      endtime <- Sys.time()
      print(paste0(
        "total time for calculating ",
        varName[1],
        " is ",
        endtime - starttime
      ))
      rm(calc1Val, calc2Val, calc5Odo)
      rm(list = paste0(varName[1]))
      # rm(list = paste0(varNameMile[1]))
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    } 
    else if (gid[idx] == 2442) {
      # 2.23 Abgasturbolader	2.23.5 Drehzahl-T4-Abgastemperatur-Kennfeld
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      failSlides[fail_index, 1] <- mainConfig[idx, 2]
      failSlides[fail_index, 2] <- mainConfig[idx, 3]
      failSlides[fail_index,3] <- "No Parameter Calculation Present"
      fail_index <- fail_index + 1
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    }
    else if (gid[idx] == 2443) {
      # 2.23 Abgasturbolader	2.23.6 Abgasturboladerdrehzahl
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      starttime <- Sys.time()
      varName <- as.character(mainConfig[idx, 6])
      # varNameMile <- as.character(paste0(varName, "Mile"))
      assign(varName, NULL)
      # assign(varNameMile, NULL)
      calc1Val <- as.character(mainConfig[idx, 8])
      calc2Val <- as.character(mainConfig[idx, 13])
      #calc5Odo <- as.character(mainConfig[idx, 14])
      #signal 1
      sig1idx <- which(channSigConfig[, 3] == calc1Val)
      sig1Val <- channSigConfig[sig1idx, 4]
      signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
      signal1Vec[signal1Vec == "NA"] <- NA
      signal1Vec <- signal1Vec[!is.na(signal1Vec)]
      assign(sig1Val, signal1Vec)
      #signal 2
      sig2idx <- which(channSigConfig[, 3] == calc2Val)
      sig2Val <- channSigConfig[sig2idx, 4]
      signal2Vec <- as.character(channSigConfig[sig2idx, 8:25])
      signal2Vec[signal2Vec == "NA"] <- NA
      signal2Vec <- signal2Vec[!is.na(signal2Vec)]
      assign(sig2Val, signal2Vec)
      #Signal3 - Odo
      # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
      # sig3Val <- channSigConfig[sig3idx, 4]
      # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
      # signal3Vec[signal3Vec == "NA"] <- NA
      # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
      # assign(sig3Val, signal3Vec)
      #signal threshold
      sig1min <- as.numeric(channSigConfig[sig1idx,5])
      sig1max <- as.numeric(channSigConfig[sig1idx,6])
      sig2min <- as.numeric(channSigConfig[sig2idx,5])
      sig2max <- as.numeric(channSigConfig[sig2idx,6])
      #sig3min <- as.numeric(channSigConfig[sig3idx,5])
      #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      
      calc1Val <- getChann1(get(sig1Val), X,sig1min,sig1max)
      calc2Val <- getChann1(get(sig2Val), X,sig2min,sig2max)
      #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      assign(x = varName[1],
             value = do.call("countTable1D", args = list(
               x = calc1Val[calc2Val > 0], mergeTab = get(varName)
             )))
      # assign(varNameMile[1], c(max(
      #   0, max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
      # )))
      if (is.null(get(varName))) {
        f_idx <- which(mainConfig[, 1] == 2443)
        failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
        failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
        if(length(colName1) == 0){
          failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
        }else if(length(colName2) == 0){
          failSlides[fail_index,4] <-as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
        }
        fail_index <- fail_index + 1
      } else{
        ##saveRDS in the dir location
        g_idx <- which(mainConfig[, 1] == 2443)
        GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
        GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
        #summary of Signal 1
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName1 <- c(colName1,colName1)
        col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
        count1 <- sum(length(which(!is.na(X[,col1]))))
        perc1 <- round(count1/nrow(X)*100)
        GoodSlides[good_index,3] <- paste0(col1)
        GoodSlides[good_index,4] <- paste0(count1)
        GoodSlides[good_index,5] <- paste0(perc1,"%")
        #summary of Signal 2 
        colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
        colName2 <- c(colName2,colName2)
        col2 <- colName2[which.max(colSums(!is.na(X[,colName2])))]
        count2 <- sum(length(which(!is.na(X[,col1]))))
        perc2 <- round(count2/nrow(X)*100)
        GoodSlides[good_index,6] <- paste0(col2)
        GoodSlides[good_index,7] <- paste0(count2)
        GoodSlides[good_index,8] <- paste0(perc2,"%")
        # #summary of Signal 3 
        # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
        # colName3 <- c(colName3,colName3)
        # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
        # count3 <- sum(length(which(!is.na(X[,col3]))))
        # perc3 <- round(count3/nrow(X)*100)
        # GoodSlides[good_index,9] <- paste0(col3)
        # GoodSlides[good_index,10] <- paste0(count3)
        # GoodSlides[good_index,11] <- paste0(perc3,"%")
        
        good_index <- good_index + 1
        saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
        # saveRDS(get(varNameMile[1]), paste0(dir, varNameMile[1], ".rds"))
      }
      ##remove the variables to free up the memory
      endtime <- Sys.time()
      print(paste0(
        "total time for calculating ",
        varName[1],
        " is ",
        endtime - starttime
      ))
      rm(calc1Val, calc2Val, calc5Odo)
      rm(list = paste0(varName[1]))
      ## rm(list = paste0(varNameMile[1]))
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    } 
    else if (gid[idx] == 2444) {
      # 2.23 Abgasturbolader	2.23.7 Motordrehzahl-Abgasturboladerdrehzahl-Kennfeld
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      starttime <- Sys.time()
      varName <- as.character(mainConfig[idx, 6])
      # varNameMile <- as.character(paste0(varName, "Mile"))
      assign(varName, NULL)
      # assign(varNameMile, NULL)
      calc1Val <- as.character(mainConfig[idx, 8])
      calc2Val <- as.character(mainConfig[idx, 9])
      calc4Val <- as.character(mainConfig[idx, 13])
      #calc5Odo <- as.character(mainConfig[idx, 14])
      #signal 1
      sig1idx <- which(channSigConfig[, 3] == calc1Val)
      sig1Val <- channSigConfig[sig1idx, 4]
      signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
      signal1Vec[signal1Vec == "NA"] <- NA
      signal1Vec <- signal1Vec[!is.na(signal1Vec)]
      assign(sig1Val, signal1Vec)
      #signal 2
      sig2idx <- which(channSigConfig[, 3] == calc2Val)
      sig2Val <- channSigConfig[sig2idx, 4]
      signal2Vec <- as.character(channSigConfig[sig2idx, 8:25])
      signal2Vec[signal2Vec == "NA"] <- NA
      signal2Vec <- signal2Vec[!is.na(signal2Vec)]
      assign(sig2Val, signal2Vec)
      #Signal3 - Odo
      # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
      # sig3Val <- channSigConfig[sig3idx, 4]
      # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
      # signal3Vec[signal3Vec == "NA"] <- NA
      # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
      # assign(sig3Val, signal3Vec)
      #Signal 4
      sig4idx <- which(channSigConfig[, 3] == calc4Val)
      sig4Val <- channSigConfig[sig4idx, 4]
      signal4Vec <- as.character(channSigConfig[sig4idx, 8:25])
      signal4Vec[signal4Vec == "NA"] <- NA
      signal4Vec <- signal4Vec[!is.na(signal4Vec)]
      assign(sig4Val, signal4Vec)
      #signal threshold
      sig1min <- as.numeric(channSigConfig[sig1idx,5])
      sig1max <- as.numeric(channSigConfig[sig1idx,6])
      sig2min <- as.numeric(channSigConfig[sig2idx,5])
      sig2max <- as.numeric(channSigConfig[sig2idx,6])
      #sig3min <- as.numeric(channSigConfig[sig3idx,5])
      #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      sig4min <- as.numeric(channSigConfig[sig4idx,5])
      sig4max <- as.numeric(channSigConfig[sig4idx,6])
      
      calc1Val <- getChann1(get(sig1Val), X,sig1min,sig1max)
      calc2Val <- getChann1(get(sig2Val), X,sig2min,sig2max)
      #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      calc4Val <- getChann1(get(sig4Val), X,sig4min,sig4max)
      #Operating Time Calc
      assign(x = varName[1],
             value = do.call(
               "countTable2D",
               args = list(
                 x = calc1Val,
                 y = calc2Val,
                 mergeTab = get(varName)
               )
             ))
      # assign(varNameMile[1], c(max(
      #   0, max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
      # )))
      if (is.null(get(varName))) {
        f_idx <- which(mainConfig[, 1] == 2444)
        failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
        failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
        if(length(colName1) == 0){
          failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
        }else if(length(colName2) == 0){
          failSlides[fail_index,4] <-as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
        }
        fail_index <- fail_index + 1
      } else{
        ##saveRDS in the dir location
        g_idx <- which(mainConfig[, 1] == 2444)
        GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
        GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
        #summary of Signal 1
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName1 <- c(colName1,colName1)
        col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
        count1 <- sum(length(which(!is.na(X[,col1]))))
        perc1 <- round(count1/nrow(X)*100)
        GoodSlides[good_index,3] <- paste0(col1)
        GoodSlides[good_index,4] <- paste0(count1)
        GoodSlides[good_index,5] <- paste0(perc1,"%")
        #summary of Signal 2 
        colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
        colName2 <- c(colName2,colName2)
        col2 <- colName2[which.max(colSums(!is.na(X[,colName2])))]
        count2 <- sum(length(which(!is.na(X[,col1]))))
        perc2 <- round(count2/nrow(X)*100)
        GoodSlides[good_index,6] <- paste0(col2)
        GoodSlides[good_index,7] <- paste0(count2)
        GoodSlides[good_index,8] <- paste0(perc2,"%")
        # #summary of Signal 3 
        # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
        # colName3 <- c(colName3,colName3)
        # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
        # count3 <- sum(length(which(!is.na(X[,col3]))))
        # perc3 <- round(count3/nrow(X)*100)
        # GoodSlides[good_index,9] <- paste0(col3)
        # GoodSlides[good_index,10] <- paste0(count3)
        # GoodSlides[good_index,11] <- paste0(perc3,"%")
        #summary of Signal 4 
        colName4 <- get(sig4Val)[get(sig4Val) %in% names(X)]
        colName4 <- c(colName4,colName4)
        col4 <- colName4[which.max(colSums(!is.na(X[,colName4])))]
        count4 <- sum(length(which(!is.na(X[,col4]))))
        perc4 <- round(count4/nrow(X)*100)
        GoodSlides[good_index,12] <- paste0(col4)
        GoodSlides[good_index,13] <- paste0(count4)
        GoodSlides[good_index,14] <- paste0(perc4,"%")
        
        good_index <- good_index + 1
        saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
        # saveRDS(get(varNameMile[1]), paste0(dir, varNameMile[1], ".rds"))
      }
      ##remove the variables to free up the memory
      endtime <- Sys.time()
      print(paste0(
        "total time for calculating ",
        varName[1],
        " is ",
        endtime - starttime
      ))
      rm(calc1Val, calc2Val, calc5Odo)
      rm(list = paste0(varName[1]))
      ## rm(list = paste0(varNameMile[1]))
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    } 
    else if (gid[idx] == 24441) {
      # 2.23 Abgasturbolader	2.23.8 Abgastemperatur T3 von Zylinder 1 - 4
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      starttime <- Sys.time()
      varName <- as.character(mainConfig[idx, 6])
      # varNameMile <- as.character(paste0(varName, "Mile"))
      assign(varName, NULL)
      # assign(varNameMile, NULL)
      calc1Val <- as.character(mainConfig[idx, 8])
      calc2Val <- as.character(mainConfig[idx, 13])
      #calc5Odo <- as.character(mainConfig[idx, 14])
      #signal 1
      sig1idx <- which(channSigConfig[, 3] == calc1Val)
      sig1Val <- channSigConfig[sig1idx, 4]
      signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
      signal1Vec[signal1Vec == "NA"] <- NA
      signal1Vec <- signal1Vec[!is.na(signal1Vec)]
      assign(sig1Val, signal1Vec)
      #signal 2
      sig2idx <- which(channSigConfig[, 3] == calc2Val)
      sig2Val <- channSigConfig[sig2idx, 4]
      signal2Vec <- as.character(channSigConfig[sig2idx, 8:25])
      signal2Vec[signal2Vec == "NA"] <- NA
      signal2Vec <- signal2Vec[!is.na(signal2Vec)]
      assign(sig2Val, signal2Vec)
      # #Signal3 - Odo
      # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
      # sig3Val <- channSigConfig[sig3idx, 4]
      # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
      # signal3Vec[signal3Vec == "NA"] <- NA
      # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
      # assign(sig3Val, signal3Vec)
      #signal threshold
      sig1min <- as.numeric(channSigConfig[sig1idx,5])
      sig1max <- as.numeric(channSigConfig[sig1idx,6])
      sig2min <- as.numeric(channSigConfig[sig2idx,5])
      sig2max <- as.numeric(channSigConfig[sig2idx,6])
      #sig3min <- as.numeric(channSigConfig[sig3idx,5])
      #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      
      calc1Val <- getChann1(get(sig1Val), X,sig1min,sig1max)
      calc2Val <- getChann1(get(sig2Val), X,sig2min,sig2max)
      #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      assign(x = varName[1],
             value = do.call("countTable1D", args = list(
               x = calc1Val[calc2Val > 0], mergeTab = get(varName)
             )))
      # assign(varNameMile[1], c(max(
      #   0, max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
      # )))
      print(get(varName))
      if (is.null(get(varName))) {
        f_idx <- which(mainConfig[, 1] == 24441)
        failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
        failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
        if(length(colName1) == 0){
          failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
        }else if(length(colName2) == 0){
          failSlides[fail_index,4] <-as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
        }
        
        fail_index <- fail_index + 1
      } else{
        ##saveRDS in the dir location
        g_idx <- which(mainConfig[, 1] == 24441)
        GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
        GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
        #summary of Signal 1
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName1 <- c(colName1,colName1)
        col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
        count1 <- sum(length(which(!is.na(X[,col1]))))
        perc1 <- round(count1/nrow(X)*100)
        GoodSlides[good_index,3] <- paste0(col1)
        GoodSlides[good_index,4] <- paste0(count1)
        GoodSlides[good_index,5] <- paste0(perc1,"%")
        #summary of Signal 2 
        colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
        colName2 <- c(colName2,colName2)
        col2 <- colName2[which.max(colSums(!is.na(X[,colName2])))]
        count2 <- sum(length(which(!is.na(X[,col1]))))
        perc2 <- round(count2/nrow(X)*100)
        GoodSlides[good_index,6] <- paste0(col2)
        GoodSlides[good_index,7] <- paste0(count2)
        GoodSlides[good_index,8] <- paste0(perc2,"%")
        #summary of Signal 3 
        # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
        # colName3 <- c(colName3,colName3)
        # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
        # count3 <- sum(length(which(!is.na(X[,col3]))))
        # perc3 <- round(count3/nrow(X)*100)
        # GoodSlides[good_index,9] <- paste0(col3)
        # GoodSlides[good_index,10] <- paste0(count3)
        # GoodSlides[good_index,11] <- paste0(perc3,"%")
        
        good_index <- good_index + 1
        saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
        # saveRDS(get(varNameMile[1]), paste0(dir, varNameMile[1], ".rds"))
      }
      ##remove the variables to free up the memory
      endtime <- Sys.time()
      print(paste0(
        "total time for calculating ",
        varName[1],
        " is ",
        endtime - starttime
      ))
      rm(calc1Val, calc2Val, calc5Odo)
      rm(list = paste0(varName[1]))
      ## rm(list = paste0(varNameMile[1]))
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    }
    else if (gid[idx] == 24442) {
      # 2.23 Abgasturbolader	2.23.9 Abgastemperatur T3 von Zylinder 1 - 4
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      starttime <- Sys.time()
      varName <- as.character(mainConfig[idx, 6])
      varNameMile <- as.character(paste0(varName, "Mile"))
      assign(varName, NULL)
      assign(varNameMile, NULL)
      calc1Val <- as.character(mainConfig[idx, 8])
      calc2Val <- as.character(mainConfig[idx, 13])
      #calc5Odo <- as.character(mainConfig[idx, 14])
      #signal 1
      sig1idx <- which(channSigConfig[, 3] == calc1Val)
      sig1Val <- channSigConfig[sig1idx, 4]
      signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
      signal1Vec[signal1Vec == "NA"] <- NA
      signal1Vec <- signal1Vec[!is.na(signal1Vec)]
      assign(sig1Val, signal1Vec)
      #signal 2
      sig2idx <- which(channSigConfig[, 3] == calc2Val)
      sig2Val <- channSigConfig[sig2idx, 4]
      signal2Vec <- as.character(channSigConfig[sig2idx, 8:25])
      signal2Vec[signal2Vec == "NA"] <- NA
      signal2Vec <- signal2Vec[!is.na(signal2Vec)]
      assign(sig2Val, signal2Vec)
      #Signal3 - Odo
      # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
      # sig3Val <- channSigConfig[sig3idx, 4]
      # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
      # signal3Vec[signal3Vec == "NA"] <- NA
      # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
      # assign(sig3Val, signal3Vec)
      #signal threshold
      sig1min <- as.numeric(channSigConfig[sig1idx,5])
      sig1max <- as.numeric(channSigConfig[sig1idx,6])
      sig2min <- as.numeric(channSigConfig[sig2idx,5])
      sig2max <- as.numeric(channSigConfig[sig2idx,6])
      #sig3min <- as.numeric(channSigConfig[sig3idx,5])
      #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      
      calc1Val <- getChann1(get(sig1Val), X,sig1min,sig1max)
      calc2Val <- getChann1(get(sig2Val), X,sig2min,sig2max)
      #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      assign(x = varName[1],
             value = do.call("countTable1D", args = list(
               x = calc1Val[calc2Val > 0], mergeTab = get(varName)
             )))
      # assign(varNameMile[1], c(max(
      #   0, max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
      # )))
      if (is.null(get(varName))) {
        f_idx <- which(mainConfig[, 1] == 24442)
        failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
        failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
        if(length(colName1) == 0){
          failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
        }else if(length(colName2) == 0){
          failSlides[fail_index,4] <-as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
        }
        fail_index <- fail_index + 1
      } else{
        ##saveRDS in the dir location
        g_idx <- which(mainConfig[, 1] == 24442)
        GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
        GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
        #summary of Signal 1
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName1 <- c(colName1,colName1)
        col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
        count1 <- sum(length(which(!is.na(X[,col1]))))
        perc1 <- round(count1/nrow(X)*100)
        GoodSlides[good_index,3] <- paste0(col1)
        GoodSlides[good_index,4] <- paste0(count1)
        GoodSlides[good_index,5] <- paste0(perc1,"%")
        #summary of Signal 2 
        colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
        colName2 <- c(colName2,colName2)
        col2 <- colName2[which.max(colSums(!is.na(X[,colName2])))]
        count2 <- sum(length(which(!is.na(X[,col1]))))
        perc2 <- round(count2/nrow(X)*100)
        GoodSlides[good_index,6] <- paste0(col2)
        GoodSlides[good_index,7] <- paste0(count2)
        GoodSlides[good_index,8] <- paste0(perc2,"%")
        #summary of Signal 3 
        # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
        # colName3 <- c(colName3,colName3)
        # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
        # count3 <- sum(length(which(!is.na(X[,col3]))))
        # perc3 <- round(count3/nrow(X)*100)
        # GoodSlides[good_index,9] <- paste0(col3)
        # GoodSlides[good_index,10] <- paste0(count3)
        # GoodSlides[good_index,11] <- paste0(perc3,"%")
        
        good_index <- good_index + 1
        saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
        # saveRDS(get(varNameMile[1]), paste0(dir, varNameMile[1], ".rds"))
      }
      ##remove the variables to free up the memory
      endtime <- Sys.time()
      print(paste0(
        "total time for calculating ",
        varName[1],
        " is ",
        endtime - starttime
      ))
      rm(calc1Val, calc2Val, calc5Odo)
      rm(list = paste0(varName[1]))
      # rm(list = paste0(varNameMile[1]))
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    } 
    else if (gid[idx] == 24443) {
      # 2.23 Abgasturbolader	2.23.10 Abgastemperatur T3 von Zylinder 2 - 3
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      starttime <- Sys.time()
      varName <- as.character(mainConfig[idx, 6])
      # varNameMile <- as.character(paste0(varName, "Mile"))
      assign(varName, NULL)
      # assign(varNameMile, NULL)
      calc1Val <- as.character(mainConfig[idx, 8])
      calc2Val <- as.character(mainConfig[idx, 13])
      #calc5Odo <- as.character(mainConfig[idx, 14])
      #signal 1
      sig1idx <- which(channSigConfig[, 3] == calc1Val)
      sig1Val <- channSigConfig[sig1idx, 4]
      signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
      signal1Vec[signal1Vec == "NA"] <- NA
      signal1Vec <- signal1Vec[!is.na(signal1Vec)]
      assign(sig1Val, signal1Vec)
      #signal 2
      sig2idx <- which(channSigConfig[, 3] == calc2Val)
      sig2Val <- channSigConfig[sig2idx, 4]
      signal2Vec <- as.character(channSigConfig[sig2idx, 8:25])
      signal2Vec[signal2Vec == "NA"] <- NA
      signal2Vec <- signal2Vec[!is.na(signal2Vec)]
      assign(sig2Val, signal2Vec)
      # #Signal3 - Odo
      # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
      # sig3Val <- channSigConfig[sig3idx, 4]
      # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
      # signal3Vec[signal3Vec == "NA"] <- NA
      # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
      # assign(sig3Val, signal3Vec)
      #signal threshold
      sig1min <- as.numeric(channSigConfig[sig1idx,5])
      sig1max <- as.numeric(channSigConfig[sig1idx,6])
      sig2min <- as.numeric(channSigConfig[sig2idx,5])
      sig2max <- as.numeric(channSigConfig[sig2idx,6])
      #sig3min <- as.numeric(channSigConfig[sig3idx,5])
      #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      
      calc1Val <- getChann1(get(sig1Val), X,sig1min,sig1max)
      calc2Val <- getChann1(get(sig2Val), X,sig2min,sig2max)
      #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      assign(x = varName[1],
             value = do.call("countTable1D", args = list(
               x = calc1Val[calc2Val > 0], mergeTab = get(varName)
             )))
      # assign(varNameMile[1], c(max(
      #   0, max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
      # )))
      if (is.null(get(varName))) {
        f_idx <- which(mainConfig[, 1] == 24443)
        failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
        failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
        if(length(colName1) == 0){
          failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
        }else if(length(colName2) == 0){
          failSlides[fail_index,4] <-as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
        }
        fail_index <- fail_index + 1
      } else{
        ##saveRDS in the dir location
        g_idx <- which(mainConfig[, 1] == 24443)
        GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
        GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
        #summary of Signal 1
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName1 <- c(colName1,colName1)
        col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
        count1 <- sum(length(which(!is.na(X[,col1]))))
        perc1 <- round(count1/nrow(X)*100)
        GoodSlides[good_index,3] <- paste0(col1)
        GoodSlides[good_index,4] <- paste0(count1)
        GoodSlides[good_index,5] <- paste0(perc1,"%")
        #summary of Signal 2 
        colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
        colName2 <- c(colName2,colName2)
        col2 <- colName2[which.max(colSums(!is.na(X[,colName2])))]
        count2 <- sum(length(which(!is.na(X[,col1]))))
        perc2 <- round(count2/nrow(X)*100)
        GoodSlides[good_index,6] <- paste0(col2)
        GoodSlides[good_index,7] <- paste0(count2)
        GoodSlides[good_index,8] <- paste0(perc2,"%")
        #summary of Signal 3 
        # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
        # colName3 <- c(colName3,colName3)
        # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
        # count3 <- sum(length(which(!is.na(X[,col3]))))
        # perc3 <- round(count3/nrow(X)*100)
        # GoodSlides[good_index,9] <- paste0(col3)
        # GoodSlides[good_index,10] <- paste0(count3)
        # GoodSlides[good_index,11] <- paste0(perc3,"%")
        
        good_index <- good_index + 1
        saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
        # saveRDS(get(varNameMile[1]), paste0(dir, varNameMile[1], ".rds"))
      }
      ##remove the variables to free up the memory
      endtime <- Sys.time()
      print(paste0(
        "total time for calculating ",
        varName[1],
        " is ",
        endtime - starttime
      ))
      rm(calc1Val, calc2Val, calc5Odo)
      rm(list = paste0(varName[1]))
      # rm(list = paste0(varNameMile[1]))
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    }
    else if (gid[idx] == 24444) {
      # 2.23 Abgasturbolader	2.23.11 Abgastemperatur T3 von Zylinder 2 - 3
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      starttime <- Sys.time()
      varName <- as.character(mainConfig[idx, 6])
      # varNameMile <- as.character(paste0(varName, "Mile"))
      assign(varName, NULL)
      # assign(varNameMile, NULL)
      calc1Val <- as.character(mainConfig[idx, 8])
      calc2Val <- as.character(mainConfig[idx, 13])
      #calc5Odo <- as.character(mainConfig[idx, 14])
      #signal 1
      sig1idx <- which(channSigConfig[, 3] == calc1Val)
      sig1Val <- channSigConfig[sig1idx, 4]
      signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
      signal1Vec[signal1Vec == "NA"] <- NA
      signal1Vec <- signal1Vec[!is.na(signal1Vec)]
      assign(sig1Val, signal1Vec)
      #signal 2
      sig2idx <- which(channSigConfig[, 3] == calc2Val)
      sig2Val <- channSigConfig[sig2idx, 4]
      signal2Vec <- as.character(channSigConfig[sig2idx, 8:25])
      signal2Vec[signal2Vec == "NA"] <- NA
      signal2Vec <- signal2Vec[!is.na(signal2Vec)]
      assign(sig2Val, signal2Vec)
      #Signal3 - Odo
      # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
      # sig3Val <- channSigConfig[sig3idx, 4]
      # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
      # signal3Vec[signal3Vec == "NA"] <- NA
      # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
      # assign(sig3Val, signal3Vec)
      #signal threshold
      sig1min <- as.numeric(channSigConfig[sig1idx,5])
      sig1max <- as.numeric(channSigConfig[sig1idx,6])
      sig2min <- as.numeric(channSigConfig[sig2idx,5])
      sig2max <- as.numeric(channSigConfig[sig2idx,6])
      #sig3min <- as.numeric(channSigConfig[sig3idx,5])
      #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      
      calc1Val <- getChann1(get(sig1Val), X,sig1min,sig1max)
      calc2Val <- getChann1(get(sig2Val), X,sig2min,sig2max)
      #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      assign(x = varName[1],
             value = do.call("countTable1D", args = list(
               x = calc1Val[calc2Val > 0], mergeTab = get(varName)
             )))
      # assign(varNameMile[1], c(max(
      #   0, max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
      # )))
      if (is.null(get(varName))) {
        f_idx <- which(mainConfig[, 1] == 2444)
        failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
        failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
        if(length(colName1) == 0){
          failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
        }else if(length(colName2) == 0){
          failSlides[fail_index,4] <-as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
        }
        fail_index <- fail_index + 1
      } else{
        ##saveRDS in the dir location
        g_idx <- which(mainConfig[, 1] == 2444)
        GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
        GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
        #summary of Signal 1
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName1 <- c(colName1,colName1)
        col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
        count1 <- sum(length(which(!is.na(X[,col1]))))
        perc1 <- round(count1/nrow(X)*100)
        GoodSlides[good_index,3] <- paste0(col1)
        GoodSlides[good_index,4] <- paste0(count1)
        GoodSlides[good_index,5] <- paste0(perc1,"%")
        #summary of Signal 2 
        colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
        colName2 <- c(colName2,colName2)
        col2 <- colName2[which.max(colSums(!is.na(X[,colName2])))]
        count2 <- sum(length(which(!is.na(X[,col1]))))
        perc2 <- round(count2/nrow(X)*100)
        GoodSlides[good_index,6] <- paste0(col2)
        GoodSlides[good_index,7] <- paste0(count2)
        GoodSlides[good_index,8] <- paste0(perc2,"%")
        # #summary of Signal 3 
        # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
        # colName3 <- c(colName3,colName3)
        # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
        # count3 <- sum(length(which(!is.na(X[,col3]))))
        # perc3 <- round(count3/nrow(X)*100)
        # GoodSlides[good_index,9] <- paste0(col3)
        # GoodSlides[good_index,10] <- paste0(count3)
        # GoodSlides[good_index,11] <- paste0(perc3,"%")
        
        good_index <- good_index + 1
        saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
        # saveRDS(get(varNameMile[1]), paste0(dir, varNameMile[1], ".rds"))
      }
      ##remove the variables to free up the memory
      endtime <- Sys.time()
      print(paste0(
        "total time for calculating ",
        varName[1],
        " is ",
        endtime - starttime
      ))
      rm(calc1Val, calc2Val, calc5Odo)
      rm(list = paste0(varName[1]))
      # rm(list = paste0(varNameMile[1]))
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
      
    } 
    else if (gid[idx] == 245) {
      # 2.24 Hydraulik	2.24.1 Raildruck
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      starttime <- Sys.time()
      varName <- as.character(mainConfig[idx, 6])
      # varNameMile <- as.character(paste0(varName, "Mile"))
      assign(varName, NULL)
      # assign(varNameMile, NULL)
      calc1Val <- as.character(mainConfig[idx, 8])
      calc2Val <- as.character(mainConfig[idx, 13])
      #calc5Odo <- as.character(mainConfig[idx, 14])
      #signal 1
      sig1idx <- which(channSigConfig[, 3] == calc1Val)
      sig1Val <- channSigConfig[sig1idx, 4]
      signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
      signal1Vec[signal1Vec == "NA"] <- NA
      signal1Vec <- signal1Vec[!is.na(signal1Vec)]
      assign(sig1Val, signal1Vec)
      #signal 2
      sig2idx <- which(channSigConfig[, 3] == calc2Val)
      sig2Val <- channSigConfig[sig2idx, 4]
      signal2Vec <- as.character(channSigConfig[sig2idx, 8:25])
      signal2Vec[signal2Vec == "NA"] <- NA
      signal2Vec <- signal2Vec[!is.na(signal2Vec)]
      assign(sig2Val, signal2Vec)
      # #Signal3 - Odo
      # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
      # sig3Val <- channSigConfig[sig3idx, 4]
      # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
      # signal3Vec[signal3Vec == "NA"] <- NA
      # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
      # assign(sig3Val, signal3Vec)
      #signal threshold
      sig1min <- as.numeric(channSigConfig[sig1idx,5])
      sig1max <- as.numeric(channSigConfig[sig1idx,6])
      sig2min <- as.numeric(channSigConfig[sig2idx,5])
      sig2max <- as.numeric(channSigConfig[sig2idx,6])
      #sig3min <- as.numeric(channSigConfig[sig3idx,5])
      #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      
      calc1Val <- getChann1(get(sig1Val), X,sig1min,sig1max)
      calc2Val <- getChann1(get(sig2Val), X,sig2min,sig2max)
      #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      assign(x = varName[1],
             value = do.call("countTable1D", args = list(
               x = calc1Val[calc2Val > 0], mergeTab = get(varName)
             )))
      # assign(varNameMile[1], c(max(
      #   0, max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
      # )))
      if (is.null(get(varName))) {
        f_idx <- which(mainConfig[, 1] == 245)
        failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
        failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
        if(length(colName1) == 0){
          failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
        }else if(length(colName2) == 0){
          failSlides[fail_index,4] <- as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
        }
        fail_index <- fail_index + 1
      } else{
        ##saveRDS in the dir location
        g_idx <- which(mainConfig[, 1] == 245)
        GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
        GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
        #summary of Signal 1
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName1 <- c(colName1,colName1)
        col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
        count1 <- sum(length(which(!is.na(X[,col1]))))
        perc1 <- round(count1/nrow(X)*100)
        GoodSlides[good_index,3] <- paste0(col1)
        GoodSlides[good_index,4] <- paste0(count1)
        GoodSlides[good_index,5] <- paste0(perc1,"%")
        #summary of Signal 2 
        colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
        colName2 <- c(colName2,colName2)
        col2 <- colName2[which.max(colSums(!is.na(X[,colName2])))]
        count2 <- sum(length(which(!is.na(X[,col1]))))
        perc2 <- round(count2/nrow(X)*100)
        GoodSlides[good_index,6] <- paste0(col2)
        GoodSlides[good_index,7] <- paste0(count2)
        GoodSlides[good_index,8] <- paste0(perc2,"%")
        #summary of Signal 3 
        # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
        # colName3 <- c(colName3,colName3)
        # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
        # count3 <- sum(length(which(!is.na(X[,col3]))))
        # perc3 <- round(count3/nrow(X)*100)
        # GoodSlides[good_index,9] <- paste0(col3)
        # GoodSlides[good_index,10] <- paste0(count3)
        # GoodSlides[good_index,11] <- paste0(perc3,"%")
        
        good_index <- good_index + 1
        saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
        # saveRDS(get(varNameMile[1]), paste0(dir, varNameMile[1], ".rds"))
      }
      ##remove the variables to free up the memory
      endtime <- Sys.time()
      print(paste0(
        "total time for calculating ",
        varName[1],
        " is ",
        endtime - starttime
      ))
      rm(calc1Val, calc2Val, calc5Odo)
      rm(list = paste0(varName[1]))
      ## rm(list = paste0(varNameMile[1]))
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    }
    else if (gid[idx] == 246) {
      # 2.24 Hydraulik	2.24.2 Anzahl Einspritzungen
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    }
    else if (gid[idx] == 247) {
      # 2.24 Hydraulik	2.24.3 Zeitanteil Kraftstofftemperatur
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      starttime <- Sys.time()
      varName <- as.character(mainConfig[idx, 6])
      # varNameMile <- as.character(paste0(varName, "Mile"))
      assign(varName, NULL)
      # assign(varNameMile, NULL)
      calc1Val <- as.character(mainConfig[idx, 8])
      calc2Val <- as.character(mainConfig[idx, 13])
      #calc5Odo <- as.character(mainConfig[idx, 14])
      #signal 1
      sig1idx <- which(channSigConfig[, 3] == calc1Val)
      sig1Val <- channSigConfig[sig1idx, 4]
      signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
      signal1Vec[signal1Vec == "NA"] <- NA
      signal1Vec <- signal1Vec[!is.na(signal1Vec)]
      assign(sig1Val, signal1Vec)
      #signal 2
      sig2idx <- which(channSigConfig[, 3] == calc2Val)
      sig2Val <- channSigConfig[sig2idx, 4]
      signal2Vec <- as.character(channSigConfig[sig2idx, 8:25])
      signal2Vec[signal2Vec == "NA"] <- NA
      signal2Vec <- signal2Vec[!is.na(signal2Vec)]
      assign(sig2Val, signal2Vec)
      #Signal3 - Odo
      # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
      # sig3Val <- channSigConfig[sig3idx, 4]
      # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
      # signal3Vec[signal3Vec == "NA"] <- NA
      # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
      # assign(sig3Val, signal3Vec)
      #signal threshold
      sig1min <- as.numeric(channSigConfig[sig1idx,5])
      sig1max <- as.numeric(channSigConfig[sig1idx,6])
      sig2min <- as.numeric(channSigConfig[sig2idx,5])
      sig2max <- as.numeric(channSigConfig[sig2idx,6])
      #sig3min <- as.numeric(channSigConfig[sig3idx,5])
      #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      # for (ri in 1:length(rleTrack$starts)){
      #   X1 <- X[rleTrack$starts[ri]:rleTrack$stops[ri],]
      # calc1Val <- getChann1(get(sig1Val), X1,sig1min,sig1max)
      # calc2Val <- getChann1(get(sig2Val), X1,sig2min,sig2max)
      # assign(x = varName[1],
      #        value = do.call("countTable1D", args = list(
      #          x = calc1Val[calc2Val > 0], mergeTab = get(varName)
      #        )))
      # }
      calc1Val <- getChann1(get(sig1Val), X1,sig1min,sig1max)
      calc2Val <- getChann1(get(sig2Val), X1,sig2min,sig2max)
      assign(x = varName[1],
             value = do.call("countTable1D", args = list(
               x = calc1Val[calc2Val > 0], mergeTab = get(varName)
             )))
      # assign(varNameMile[1], c(max(
      #   0, max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
      # )))
      if (is.null(get(varName))) {
        f_idx <- which(mainConfig[, 1] == 247)
        failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
        failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
        if(length(colName1) == 0){
          failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
        }else if(length(colName2) == 0){
          failSlides[fail_index,4] <-as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
        }
        fail_index <- fail_index + 1
      } else{
        ##saveRDS in the dir location
        g_idx <- which(mainConfig[, 1] == 247)
        GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
        GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
        #summary of Signal 1
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName1 <- c(colName1,colName1)
        col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
        count1 <- sum(length(which(!is.na(X[,col1]))))
        perc1 <- round(count1/nrow(X)*100)
        GoodSlides[good_index,3] <- paste0(col1)
        GoodSlides[good_index,4] <- paste0(count1)
        GoodSlides[good_index,5] <- paste0(perc1,"%")
        #summary of Signal 2 
        colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
        colName2 <- c(colName2,colName2)
        col2 <- colName2[which.max(colSums(!is.na(X[,colName2])))]
        count2 <- sum(length(which(!is.na(X[,col1]))))
        perc2 <- round(count2/nrow(X)*100)
        GoodSlides[good_index,6] <- paste0(col2)
        GoodSlides[good_index,7] <- paste0(count2)
        GoodSlides[good_index,8] <- paste0(perc2,"%")
        #summary of Signal 3 
        # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
        # colName3 <- c(colName3,colName3)
        # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
        # count3 <- sum(length(which(!is.na(X[,col3]))))
        # perc3 <- round(count3/nrow(X)*100)
        # GoodSlides[good_index,9] <- paste0(col3)
        # GoodSlides[good_index,10] <- paste0(count3)
        # GoodSlides[good_index,11] <- paste0(perc3,"%")
        
        good_index <- good_index + 1
        saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
        # saveRDS(get(varNameMile[1]), paste0(dir, varNameMile[1], ".rds"))
      }
      ##remove the variables to free up the memory
      endtime <- Sys.time()
      print(paste0(
        "total time for calculating ",
        varName[1],
        " is ",
        endtime - starttime
      ))
      rm(calc1Val, calc2Val, calc5Odo)
      rm(list = paste0(varName[1]))
      # rm(list = paste0(varNameMile[1]))
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    } 
    else if (gid[idx] == 248) {
      # 2.24 Hydraulik	2.24.4 Einspritzzeiten
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      failSlides[fail_index, 1] <- mainConfig[idx, 2]
      failSlides[fail_index, 2] <- mainConfig[idx, 3]
      failSlides[fail_index,3] <- "No Parameter Calculation Present"
      fail_index <- fail_index + 1
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
      
    } 
    else if (gid[idx] == 249) {
      # 2.24 Hydraulik	2.24.5 Raildruck beim Start
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      starttime <- Sys.time()
      varName <- as.character(mainConfig[idx, 6])
      # varNameMile <- as.character(paste0(varName, "Mile"))
      assign(varName, NULL)
      # assign(varNameMile, NULL)
      calc1Val <- as.character(mainConfig[idx, 8])
      calc2Val <- as.character(mainConfig[idx, 13])
      #calc5Odo <- as.character(mainConfig[idx, 14])
      #signal 1
      sig1idx <- which(channSigConfig[, 3] == calc1Val)
      sig1Val <- channSigConfig[sig1idx, 4]
      signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
      signal1Vec[signal1Vec == "NA"] <- NA
      signal1Vec <- signal1Vec[!is.na(signal1Vec)]
      assign(sig1Val, signal1Vec)
      #signal 2
      sig2idx <- which(channSigConfig[, 3] == calc2Val)
      sig2Val <- channSigConfig[sig2idx, 4]
      signal2Vec <- as.character(channSigConfig[sig2idx, 8:25])
      signal2Vec[signal2Vec == "NA"] <- NA
      signal2Vec <- signal2Vec[!is.na(signal2Vec)]
      assign(sig2Val, signal2Vec)
      # #Signal3 - Odo
      # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
      # sig3Val <- channSigConfig[sig3idx, 4]
      # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
      # signal3Vec[signal3Vec == "NA"] <- NA
      # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
      # assign(sig3Val, signal3Vec)
      #signal threshold
      sig1min <- as.numeric(channSigConfig[sig1idx,5])
      sig1max <- as.numeric(channSigConfig[sig1idx,6])
      sig2min <- as.numeric(channSigConfig[sig2idx,5])
      sig2max <- as.numeric(channSigConfig[sig2idx,6])
      #sig3min <- as.numeric(channSigConfig[sig3idx,5])
      #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      # for (ri in 1:length(rleTrack$starts)){
      # X1 <- X[rleTrack$starts[ri]:rleTrack$stops[ri],]
      # calc1Val <- getChann1(get(sig1Val), X1,sig1min,sig1max)
      # calc2Val <- getChann1(get(sig2Val), X1,sig2min,sig2max)
      # idStart <- startIndices(calc2Val, startThresh, startMinTime)
      # assign(x = varName[1],
      #        value = do.call("countTable1D", args = list(
      #          x = calc1Val[idStart], mergeTab = get(varName)
      #        )))
      # }
      calc1Val <- getChann1(get(sig1Val), X1,sig1min,sig1max)
      calc2Val <- getChann1(get(sig2Val), X1,sig2min,sig2max)
      idStart <- startIndices(calc2Val, startThresh, startMinTime)
      assign(x = varName[1],
             value = do.call("countTable1D", args = list(
               x = calc1Val[idStart], mergeTab = get(varName)
             )))
      # assign(varNameMile[1], c(max(
      #   0, max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
      # )))
      if (is.null(get(varName))) {
        f_idx <- which(mainConfig[, 1] == 249)
        failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
        failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
        if(length(colName1) == 0){
          failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
        }else if(length(colName2) == 0){
          failSlides[fail_index,4] <- as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
        }
        fail_index <- fail_index + 1
      } else{
        ##saveRDS in the dir location
        g_idx <- which(mainConfig[, 1] == 249)
        GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
        GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
        #summary of Signal 1
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName1 <- c(colName1,colName1)
        col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
        count1 <- sum(length(which(!is.na(X[,col1]))))
        perc1 <- round(count1/nrow(X)*100)
        GoodSlides[good_index,3] <- paste0(col1)
        GoodSlides[good_index,4] <- paste0(count1)
        GoodSlides[good_index,5] <- paste0(perc1,"%")
        #summary of Signal 2 
        colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
        colName2 <- c(colName2,colName2)
        col2 <- colName2[which.max(colSums(!is.na(X[,colName2])))]
        count2 <- sum(length(which(!is.na(X[,col1]))))
        perc2 <- round(count2/nrow(X)*100)
        GoodSlides[good_index,6] <- paste0(col2)
        GoodSlides[good_index,7] <- paste0(count2)
        GoodSlides[good_index,8] <- paste0(perc2,"%")
        # #summary of Signal 3 
        # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
        # colName3 <- c(colName3,colName3)
        # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
        # count3 <- sum(length(which(!is.na(X[,col3]))))
        # perc3 <- round(count3/nrow(X)*100)
        # GoodSlides[good_index,9] <- paste0(col3)
        # GoodSlides[good_index,10] <- paste0(count3)
        # GoodSlides[good_index,11] <- paste0(perc3,"%")
        
        good_index <- good_index + 1
        saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
        # saveRDS(get(varNameMile[1]), paste0(dir, varNameMile[1], ".rds"))
      }
      ##remove the variables to free up the memory
      endtime <- Sys.time()
      print(paste0(
        "total time for calculating ",
        varName[1],
        " is ",
        endtime - starttime
      ))
      rm(calc1Val, calc2Val, calc5Odo)
      rm(list = paste0(varName[1]))
      # rm(list = paste0(varNameMile[1]))
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
      
    }
    else if (gid[idx] == 250) {
      # 2.24 Hydraulik	2.24.6 Klassierung Kraftstofftemperatur beim Start
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      starttime <- Sys.time()
      varName <- as.character(mainConfig[idx, 6])
      # varNameMile <- as.character(paste0(varName, "Mile"))
      assign(varName, NULL)
      # assign(varNameMile, NULL)
      calc1Val <- as.character(mainConfig[idx, 8])
      calc2Val <- as.character(mainConfig[idx, 13])
      #calc5Odo <- as.character(mainConfig[idx, 14])
      #signal 1
      sig1idx <- which(channSigConfig[, 3] == calc1Val)
      sig1Val <- channSigConfig[sig1idx, 4]
      signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
      signal1Vec[signal1Vec == "NA"] <- NA
      signal1Vec <- signal1Vec[!is.na(signal1Vec)]
      assign(sig1Val, signal1Vec)
      #signal 2
      sig2idx <- which(channSigConfig[, 3] == calc2Val)
      sig2Val <- channSigConfig[sig2idx, 4]
      signal2Vec <- as.character(channSigConfig[sig2idx, 8:25])
      signal2Vec[signal2Vec == "NA"] <- NA
      signal2Vec <- signal2Vec[!is.na(signal2Vec)]
      assign(sig2Val, signal2Vec)
      #Signal3 - Odo
      # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
      # sig3Val <- channSigConfig[sig3idx, 4]
      # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
      # signal3Vec[signal3Vec == "NA"] <- NA
      # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
      # assign(sig3Val, signal3Vec)
      #signal threshold
      sig1min <- as.numeric(channSigConfig[sig1idx,5])
      sig1max <- as.numeric(channSigConfig[sig1idx,6])
      sig2min <- as.numeric(channSigConfig[sig2idx,5])
      sig2max <- as.numeric(channSigConfig[sig2idx,6])
      #sig3min <- as.numeric(channSigConfig[sig3idx,5])
      #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      for (ri in 1:length(rleTrack$starts)){
        X1 <- X[rleTrack$starts[ri]:rleTrack$stops[ri],]
        calc1Val <- getChann1(get(sig1Val), X1,sig1min,sig1max)
        calc2Val <- getChann1(get(sig2Val), X1,sig2min,sig2max)
        #sig3max <- as.numeric(channSigConfig[sig3idx,6])
        idStart <- startIndices(calc2Val, startThresh, startMinTime)
        assign(x = varName[1],
               value = do.call("countTable1D", args = list(
                 x = calc1Val[idStart], mergeTab = get(varName)
               )))
      }
      # assign(varNameMile[1], c(max(
      #   0, max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
      # )))
      if (is.null(get(varName))) {
        f_idx <- which(mainConfig[, 1] == 250)
        failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
        failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
        if(length(colName1) == 0){
          failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
        }else if(length(colName2) == 0){
          failSlides[fail_index,4] <-as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
        }
        fail_index <- fail_index + 1
      } else{
        ##saveRDS in the dir location
        g_idx <- which(mainConfig[, 1] == 250)
        GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
        GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
        #summary of Signal 1
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName1 <- c(colName1,colName1)
        col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
        count1 <- sum(length(which(!is.na(X[,col1]))))
        perc1 <- round(count1/nrow(X)*100)
        GoodSlides[good_index,3] <- paste0(col1)
        GoodSlides[good_index,4] <- paste0(count1)
        GoodSlides[good_index,5] <- paste0(perc1,"%")
        #summary of Signal 2 
        colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
        colName2 <- c(colName2,colName2)
        col2 <- colName2[which.max(colSums(!is.na(X[,colName2])))]
        count2 <- sum(length(which(!is.na(X[,col1]))))
        perc2 <- round(count2/nrow(X)*100)
        GoodSlides[good_index,6] <- paste0(col2)
        GoodSlides[good_index,7] <- paste0(count2)
        GoodSlides[good_index,8] <- paste0(perc2,"%")
        #summary of Signal 3 
        # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
        # colName3 <- c(colName3,colName3)
        # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
        # count3 <- sum(length(which(!is.na(X[,col3]))))
        # perc3 <- round(count3/nrow(X)*100)
        # GoodSlides[good_index,9] <- paste0(col3)
        # GoodSlides[good_index,10] <- paste0(count3)
        # GoodSlides[good_index,11] <- paste0(perc3,"%")
        
        good_index <- good_index + 1
        saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
        # saveRDS(get(varNameMile[1]), paste0(dir, varNameMile[1], ".rds"))
      }
      ##remove the variables to free up the memory
      endtime <- Sys.time()
      print(paste0(
        "total time for calculating ",
        varName[1],
        " is ",
        endtime - starttime
      ))
      rm(calc1Val, calc2Val, calc5Odo)
      rm(list = paste0(varName[1]))
      # rm(list = paste0(varNameMile[1]))
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    }
    else if (gid[idx] == 2501) {
      # 2.24 Hydraulik	2.24.7 Kraftstoffniederdruck
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      if (engType == "M") {
        starttime <- Sys.time()
        varName <- as.character(mainConfig[idx, 6])
        # varNameMile <- as.character(paste0(varName, "Mile"))
        assign(varName, NULL)
        # assign(varNameMile, NULL)
        calc1Val <- as.character(mainConfig[idx, 8])
        calc2Val <- as.character(mainConfig[idx, 13])
        #calc5Odo <- as.character(mainConfig[idx, 14])
        #signal 1
        sig1idx <- which(channSigConfig[, 3] == calc1Val)
        sig1Val <- channSigConfig[sig1idx, 4]
        signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
        signal1Vec[signal1Vec == "NA"] <- NA
        signal1Vec <- signal1Vec[!is.na(signal1Vec)]
        assign(sig1Val, signal1Vec)
        #signal 2
        sig2idx <- which(channSigConfig[, 3] == calc2Val)
        sig2Val <- channSigConfig[sig2idx, 4]
        signal2Vec <- as.character(channSigConfig[sig2idx, 8:25])
        signal2Vec[signal2Vec == "NA"] <- NA
        signal2Vec <- signal2Vec[!is.na(signal2Vec)]
        assign(sig2Val, signal2Vec)
        #Signal3 - Odo
        # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
        # sig3Val <- channSigConfig[sig3idx, 4]
        # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
        # signal3Vec[signal3Vec == "NA"] <- NA
        # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
        # assign(sig3Val, signal3Vec)
        #signal threshold
        sig1min <- as.numeric(channSigConfig[sig1idx,5])
        sig1max <- as.numeric(channSigConfig[sig1idx,6])
        sig2min <- as.numeric(channSigConfig[sig2idx,5])
        sig2max <- as.numeric(channSigConfig[sig2idx,6])
        #sig3min <- as.numeric(channSigConfig[sig3idx,5])
        #sig3max <- as.numeric(channSigConfig[sig3idx,6])
        
        #calc1Val <- getChann1(get(sig1Val), X,sig1min,sig1max)
        calc1Val <- getChann(get(sig1Val),X)
        if(length(calc1Val)>0 && sum(!is.na(calc1Val))>0 && mean(Mod(calc1Val),na.rm=TRUE)>10){
          calc1Val <- calc1Val/1000
        }
        calc2Val <- getChann1(get(sig2Val), X,sig2min,sig2max)
        #sig3max <- as.numeric(channSigConfig[sig3idx,6])
        assign(x = varName[1],
               value = do.call("countTable1D", args = list(
                 x = calc1Val[calc2Val > 0], mergeTab = get(varName)
               )))
        # assign(varNameMile[1], c(max(
        #   0,
        #   max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
        # )))
        if (is.null(get(varName))) {
          f_idx <- which(mainConfig[, 1] == 2501)
          failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
          failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
          if(length(colName1) == 0){
            failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
          }else if(length(colName2) == 0){
            failSlides[fail_index,4] <-as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
          }
          fail_index <- fail_index + 1
        } else{
          ##saveRDS in the dir location
          g_idx <- which(mainConfig[, 1] == 2501)
          GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
          GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
          #summary of Signal 1
          colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
          colName1 <- c(colName1,colName1)
          col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
          count1 <- sum(length(which(!is.na(X[,col1]))))
          perc1 <- round(count1/nrow(X)*100)
          GoodSlides[good_index,3] <- paste0(col1)
          GoodSlides[good_index,4] <- paste0(count1)
          GoodSlides[good_index,5] <- paste0(perc1,"%")
          #summary of Signal 2 
          colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
          colName2 <- c(colName2,colName2)
          col2 <- colName2[which.max(colSums(!is.na(X[,colName2])))]
          count2 <- sum(length(which(!is.na(X[,col1]))))
          perc2 <- round(count2/nrow(X)*100)
          GoodSlides[good_index,6] <- paste0(col2)
          GoodSlides[good_index,7] <- paste0(count2)
          GoodSlides[good_index,8] <- paste0(perc2,"%")
          #summary of Signal 3 
          # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
          # colName3 <- c(colName3,colName3)
          # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
          # count3 <- sum(length(which(!is.na(X[,col3]))))
          # perc3 <- round(count3/nrow(X)*100)
          # GoodSlides[good_index,9] <- paste0(col3)
          # GoodSlides[good_index,10] <- paste0(count3)
          # GoodSlides[good_index,11] <- paste0(perc3,"%")
          
          good_index <- good_index + 1
          saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
          # saveRDS(get(varNameMile[1]),
          # paste0(dir, varNameMile[1], ".rds"))
        }
        ##remove the variables to free up the memory
        endtime <- Sys.time()
        print(paste0(
          "total time for calculating ",
          varName[1],
          " is ",
          endtime - starttime
        ))
        rm(calc1Val, calc2Val, calc5Odo)
        rm(list = paste0(varName[1]))
        # rm(list = paste0(varNameMile[1]))
      }
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    } 
    else if (gid[idx] == 2502) {
      # 2.24 Hydraulik	2.24.8 Kraftstoffniederdruck beim Start
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      if (engType == "M") {
        starttime <- Sys.time()
        varName <- as.character(mainConfig[idx, 6])
        # varNameMile <- as.character(paste0(varName, "Mile"))
        assign(varName, NULL)
        # assign(varNameMile, NULL)
        calc1Val <- as.character(mainConfig[idx, 8])
        calc2Val <- as.character(mainConfig[idx, 13])
        #calc5Odo <- as.character(mainConfig[idx, 14])
        #signal 1
        sig1idx <- which(channSigConfig[, 3] == calc1Val)
        sig1Val <- channSigConfig[sig1idx, 4]
        signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
        signal1Vec[signal1Vec == "NA"] <- NA
        signal1Vec <- signal1Vec[!is.na(signal1Vec)]
        assign(sig1Val, signal1Vec)
        #signal 2
        sig2idx <- which(channSigConfig[, 3] == calc2Val)
        sig2Val <- channSigConfig[sig2idx, 4]
        signal2Vec <- as.character(channSigConfig[sig2idx, 8:25])
        signal2Vec[signal2Vec == "NA"] <- NA
        signal2Vec <- signal2Vec[!is.na(signal2Vec)]
        assign(sig2Val, signal2Vec)
        #Signal3 - Odo
        # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
        # sig3Val <- channSigConfig[sig3idx, 4]
        # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
        # signal3Vec[signal3Vec == "NA"] <- NA
        # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
        # assign(sig3Val, signal3Vec)
        #signal threshold
        sig1min <- as.numeric(channSigConfig[sig1idx,5])
        sig1max <- as.numeric(channSigConfig[sig1idx,6])
        sig2min <- as.numeric(channSigConfig[sig2idx,5])
        sig2max <- as.numeric(channSigConfig[sig2idx,6])
        #sig3min <- as.numeric(channSigConfig[sig3idx,5])
        #sig3max <- as.numeric(channSigConfig[sig3idx,6])
        
        # calc1Val <- getChann1(get(sig1Val), X,sig1min,sig1max)
        calc1Val <- getChann(get(sig1Val),X)
        if(length(calc1Val)>0 && sum(!is.na(calc1Val))>0 && mean(Mod(calc1Val),na.rm=TRUE)>10){
          calc1Val <- calc1Val/1000
        }
        calc2Val <- getChann1(get(sig2Val), X,sig2min,sig2max)
        #sig3max <- as.numeric(channSigConfig[sig3idx,6])
        idStart <- startIndices(calc2Val, startThresh, startMinTime)
        assign(x = varName[1],
               value = do.call("countTable1D", args = list(
                 x = calc1Val[idStart], mergeTab = get(varName)
               )))
        # assign(varNameMile[1], c(max(
        #   0,
        #   max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
        # )))
        if (is.null(get(varName))) {
          f_idx <- which(mainConfig[, 1] == 2502)
          failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
          failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
          if(length(colName1) == 0){
            failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
          }else if(length(colName2) == 0){
            failSlides[fail_index,4] <-as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
          }
          fail_index <- fail_index + 1
        } else{
          ##saveRDS in the dir location
          g_idx <- which(mainConfig[, 1] == 2502)
          GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
          GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
          #summary of Signal 1
          colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
          colName1 <- c(colName1,colName1)
          col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
          count1 <- sum(length(which(!is.na(X[,col1]))))
          perc1 <- round(count1/nrow(X)*100)
          GoodSlides[good_index,3] <- paste0(col1)
          GoodSlides[good_index,4] <- paste0(count1)
          GoodSlides[good_index,5] <- paste0(perc1,"%")
          #summary of Signal 2 
          colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
          colName2 <- c(colName2,colName2)
          col2 <- colName2[which.max(colSums(!is.na(X[,colName2])))]
          count2 <- sum(length(which(!is.na(X[,col1]))))
          perc2 <- round(count2/nrow(X)*100)
          GoodSlides[good_index,6] <- paste0(col2)
          GoodSlides[good_index,7] <- paste0(count2)
          GoodSlides[good_index,8] <- paste0(perc2,"%")
          #summary of Signal 3 
          # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
          # colName3 <- c(colName3,colName3)
          # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
          # count3 <- sum(length(which(!is.na(X[,col3]))))
          # perc3 <- round(count3/nrow(X)*100)
          # GoodSlides[good_index,9] <- paste0(col3)
          # GoodSlides[good_index,10] <- paste0(count3)
          # GoodSlides[good_index,11] <- paste0(perc3,"%")
          
          good_index <- good_index + 1
          saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
          # saveRDS(get(varNameMile[1]),
          #         paste0(dir, varNameMile[1], ".rds"))
        }
        ##remove the variables to free up the memory
        endtime <- Sys.time()
        print(paste0(
          "total time for calculating ",
          varName[1],
          " is ",
          endtime - starttime
        ))
        rm(calc1Val, calc2Val, calc5Odo)
        rm(list = paste0(varName[1]))
        ## rm(list = paste0(varNameMile[1]))
      }
      
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    }
    else if (gid[idx] == 2503) {
      # 2.24 Hydraulik	2.24.9 Kraftstofftemperatur Niederdruckkreislauf
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      if (engType == "M") {
        starttime <- Sys.time()
        varName <- as.character(mainConfig[idx, 6])
        # varNameMile <- as.character(paste0(varName, "Mile"))
        assign(varName, NULL)
        # assign(varNameMile, NULL)
        calc1Val <- as.character(mainConfig[idx, 8])
        calc2Val <- as.character(mainConfig[idx, 13])
        #calc5Odo <- as.character(mainConfig[idx, 14])
        #signal 1
        sig1idx <- which(channSigConfig[, 3] == calc1Val)
        sig1Val <- channSigConfig[sig1idx, 4]
        signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
        signal1Vec[signal1Vec == "NA"] <- NA
        signal1Vec <- signal1Vec[!is.na(signal1Vec)]
        assign(sig1Val, signal1Vec)
        #signal 2
        sig2idx <- which(channSigConfig[, 3] == calc2Val)
        sig2Val <- channSigConfig[sig2idx, 4]
        signal2Vec <- as.character(channSigConfig[sig2idx, 8:25])
        signal2Vec[signal2Vec == "NA"] <- NA
        signal2Vec <- signal2Vec[!is.na(signal2Vec)]
        assign(sig2Val, signal2Vec)
        #Signal3 - Odo
        # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
        # sig3Val <- channSigConfig[sig3idx, 4]
        # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
        # signal3Vec[signal3Vec == "NA"] <- NA
        # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
        # assign(sig3Val, signal3Vec)
        #signal threshold
        sig1min <- as.numeric(channSigConfig[sig1idx,5])
        sig1max <- as.numeric(channSigConfig[sig1idx,6])
        sig2min <- as.numeric(channSigConfig[sig2idx,5])
        sig2max <- as.numeric(channSigConfig[sig2idx,6])
        #sig3min <- as.numeric(channSigConfig[sig3idx,5])
        #sig3max <- as.numeric(channSigConfig[sig3idx,6])
        
        calc1Val <- getChann1(get(sig1Val), X,sig1min,sig1max)
        calc2Val <- getChann1(get(sig2Val), X,sig2min,sig2max)
        #sig3max <- as.numeric(channSigConfig[sig3idx,6])
        assign(x = varName[1],
               value = do.call("countTable1D", args = list(
                 x = calc1Val[calc2Val > 0], mergeTab = get(varName)
               )))
        # assign(varNameMile[1], c(max(
        #   0,
        #   max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
        # )))
        if (is.null(get(varName))) {
          f_idx <- which(mainConfig[, 1] == 2503)
          failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
          failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
          if(length(colName1) == 0){
            failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
          }else if(length(colName2) == 0){
            failSlides[fail_index,4] <-as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
          }
          fail_index <- fail_index + 1
        } else{
          ##saveRDS in the dir location
          g_idx <- which(mainConfig[, 1] == 2503)
          GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
          GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
          #summary of Signal 1
          colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
          colName1 <- c(colName1,colName1)
          col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
          count1 <- sum(length(which(!is.na(X[,col1]))))
          perc1 <- round(count1/nrow(X)*100)
          GoodSlides[good_index,3] <- paste0(col1)
          GoodSlides[good_index,4] <- paste0(count1)
          GoodSlides[good_index,5] <- paste0(perc1,"%")
          #summary of Signal 2 
          colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
          colName2 <- c(colName2,colName2)
          col2 <- colName2[which.max(colSums(!is.na(X[,colName2])))]
          count2 <- sum(length(which(!is.na(X[,col1]))))
          perc2 <- round(count2/nrow(X)*100)
          GoodSlides[good_index,6] <- paste0(col2)
          GoodSlides[good_index,7] <- paste0(count2)
          GoodSlides[good_index,8] <- paste0(perc2,"%")
          #summary of Signal 3 
          # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
          # colName3 <- c(colName3,colName3)
          # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
          # count3 <- sum(length(which(!is.na(X[,col3]))))
          # perc3 <- round(count3/nrow(X)*100)
          # GoodSlides[good_index,9] <- paste0(col3)
          # GoodSlides[good_index,10] <- paste0(count3)
          # GoodSlides[good_index,11] <- paste0(perc3,"%")
          
          good_index <- good_index + 1
          saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
          # saveRDS(get(varNameMile[1]),
          #         paste0(dir, varNameMile[1], ".rds"))
        }
        ##remove the variables to free up the memory
        endtime <- Sys.time()
        print(paste0(
          "total time for calculating ",
          varName[1],
          " is ",
          endtime - starttime
        ))
        rm(calc1Val, calc2Val, calc5Odo)
        rm(list = paste0(varName[1]))
        ## rm(list = paste0(varNameMile[1]))
      }
      
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    }
    else if (gid[idx] == 251) {
      # 2.25 Elektrischer Zusatzverdichter	2.25.1 Anzahl EZV-Zyklen pro Boostzeitklasse
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      failSlides[fail_index, 1] <- mainConfig[idx, 2]
      failSlides[fail_index, 2] <- mainConfig[idx, 3]
      failSlides[fail_index,3] <- "No Parameter Calculation Present"
      fail_index <- fail_index + 1
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    }
    else if (gid[idx] == 252) {
      # 2.25 Elektrischer Zusatzverdichter	2.25.2 Anzahl Bootzyklen
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      failSlides[fail_index, 1] <- mainConfig[idx, 2]
      failSlides[fail_index, 2] <- mainConfig[idx, 3]
      failSlides[fail_index,3] <- "No Parameter Calculation Present"
      fail_index <- fail_index + 1
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    }
    else if (gid[idx] == 253) {
      # 2.25 Elektrischer Zusatzverdichter	2.25.3 Anzahl Bootzyklen pro Platintemperaturklasse
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      failSlides[fail_index, 1] <- mainConfig[idx, 2]
      failSlides[fail_index, 2] <- mainConfig[idx, 3]
      failSlides[fail_index,3] <- "No Parameter Calculation Present"
      fail_index <- fail_index + 1
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    }
    else if (gid[idx] == 254) {
      # 2.25 Eletrischer Zusatzverdichter	2.25.4 Anzahl Bootzyklen pro Kondensatortemperaturklasse
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      failSlides[fail_index, 1] <- mainConfig[idx, 2]
      failSlides[fail_index, 2] <- mainConfig[idx, 3]
      failSlides[fail_index,3] <- "No Parameter Calculation Present"
      fail_index <- fail_index + 1
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    }
    else if (gid[idx] == 255) {
      # 2.26 Abgasanlage	2.26.1 T4-Abgastemperatur
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      starttime <- Sys.time()
      varName <- as.character(mainConfig[idx, 6])
      # varNameMile <- as.character(paste0(varName, "Mile"))
      assign(varName, NULL)
      # assign(varNameMile, NULL)
      calc1Val <- as.character(mainConfig[idx, 8])
      calc2Val <- as.character(mainConfig[idx, 13])
      #calc5Odo <- as.character(mainConfig[idx, 14])
      #signal 1
      sig1idx <- which(channSigConfig[, 3] == calc1Val)
      sig1Val <- channSigConfig[sig1idx, 4]
      signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
      signal1Vec[signal1Vec == "NA"] <- NA
      signal1Vec <- signal1Vec[!is.na(signal1Vec)]
      assign(sig1Val, signal1Vec)
      #signal 2
      sig2idx <- which(channSigConfig[, 3] == calc2Val)
      sig2Val <- channSigConfig[sig2idx, 4]
      signal2Vec <- as.character(channSigConfig[sig2idx, 8:25])
      signal2Vec[signal2Vec == "NA"] <- NA
      signal2Vec <- signal2Vec[!is.na(signal2Vec)]
      assign(sig2Val, signal2Vec)
      #Signal3 - Odo
      # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
      # sig3Val <- channSigConfig[sig3idx, 4]
      # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
      # signal3Vec[signal3Vec == "NA"] <- NA
      # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
      # assign(sig3Val, signal3Vec)
      #signal threshold
      sig1min <- as.numeric(channSigConfig[sig1idx,5])
      sig1max <- as.numeric(channSigConfig[sig1idx,6])
      sig2min <- as.numeric(channSigConfig[sig2idx,5])
      sig2max <- as.numeric(channSigConfig[sig2idx,6])
      #sig3min <- as.numeric(channSigConfig[sig3idx,5])
      #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      for (ri in 1:length(rleTrack$starts)){
        X1 <- X[rleTrack$starts[ri]:rleTrack$stops[ri],]
        calc1Val <- getChann1(get(sig1Val), X1,sig1min,sig1max)
        calc2Val <- getChann1(get(sig2Val), X1,sig2min,sig2max)
        #sig3max <- as.numeric(channSigConfig[sig3idx,6])
        assign(x = varName[1],
               value = do.call("countTable1D", args = list(
                 x = calc1Val[calc2Val > 0], mergeTab = get(varName)
               )))
      }
      # assign(varNameMile[1], c(max(
      #   0, max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
      # )))
      if (is.null(get(varName))) {
        f_idx <- which(mainConfig[, 1] == 255)
        failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
        failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
        if(length(colName1) == 0){
          failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
        }else if(length(colName2) == 0){
          failSlides[fail_index,4] <-as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
        }
        fail_index <- fail_index + 1
      } else{
        ##saveRDS in the dir location
        g_idx <- which(mainConfig[, 1] == 255)
        GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
        GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
        #summary of Signal 1
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName1 <- c(colName1,colName1)
        col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
        count1 <- sum(length(which(!is.na(X[,col1]))))
        perc1 <- round(count1/nrow(X)*100)
        GoodSlides[good_index,3] <- paste0(col1)
        GoodSlides[good_index,4] <- paste0(count1)
        GoodSlides[good_index,5] <- paste0(perc1,"%")
        #summary of Signal 2 
        colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
        colName2 <- c(colName2,colName2)
        col2 <- colName2[which.max(colSums(!is.na(X[,colName2])))]
        count2 <- sum(length(which(!is.na(X[,col1]))))
        perc2 <- round(count2/nrow(X)*100)
        GoodSlides[good_index,6] <- paste0(col2)
        GoodSlides[good_index,7] <- paste0(count2)
        GoodSlides[good_index,8] <- paste0(perc2,"%")
        #summary of Signal 3 
        # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
        # colName3 <- c(colName3,colName3)
        # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
        # count3 <- sum(length(which(!is.na(X[,col3]))))
        # perc3 <- round(count3/nrow(X)*100)
        # GoodSlides[good_index,9] <- paste0(col3)
        # GoodSlides[good_index,10] <- paste0(count3)
        # GoodSlides[good_index,11] <- paste0(perc3,"%")
        
        good_index <- good_index + 1
        saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
        # saveRDS(get(varNameMile[1]), paste0(dir, varNameMile[1], ".rds"))
      }
      ##remove the variables to free up the memory
      endtime <- Sys.time()
      print(paste0(
        "total time for calculating ",
        varName[1],
        " is ",
        endtime - starttime
      ))
      rm(calc1Val, calc2Val, calc5Odo)
      rm(list = paste0(varName[1]))
      ## rm(list = paste0(varNameMile[1]))
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    }
    else if (gid[idx] == 256) {
      # 2.26 Abgasanlage	2.26.2 Schaltungen Akustikklappe(Ottomotor) bzw.ND-AGR-Klappe(Dieselmotor)
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      failSlides[fail_index, 1] <- mainConfig[idx, 2]
      failSlides[fail_index, 2] <- mainConfig[idx, 3]
      failSlides[fail_index,3] <- "No Parameter Calculation Present"
      fail_index <- fail_index + 1
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
      
    } 
    else if (gid[idx] == 257) {
      # 2.27 AbgasrÃ¼ckfÃ¼hrung	2.27.1 Stellerposition ND-AGR
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      if (engType == "O") {
        starttime <- Sys.time()
        varName <- as.character(mainConfig[idx, 6])
        # varNameMile <- as.character(paste0(varName, "Mile"))
        assign(varName, NULL)
        # assign(varNameMile, NULL)
        calc1Val <- as.character(mainConfig[idx, 8])
        calc2Val <- as.character(mainConfig[idx, 13])
        #calc5Odo <- as.character(mainConfig[idx, 14])
        #signal 1
        sig1idx <- which(channSigConfig[, 3] == calc1Val)
        sig1Val <- channSigConfig[sig1idx, 4]
        signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
        signal1Vec[signal1Vec == "NA"] <- NA
        signal1Vec <- signal1Vec[!is.na(signal1Vec)]
        assign(sig1Val, signal1Vec)
        #signal 2
        sig2idx <- which(channSigConfig[, 3] == calc2Val)
        sig2Val <- channSigConfig[sig2idx, 4]
        signal2Vec <- as.character(channSigConfig[sig2idx, 8:25])
        signal2Vec[signal2Vec == "NA"] <- NA
        signal2Vec <- signal2Vec[!is.na(signal2Vec)]
        assign(sig2Val, signal2Vec)
        #Signal3 - Odo
        # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
        # sig3Val <- channSigConfig[sig3idx, 4]
        # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
        # signal3Vec[signal3Vec == "NA"] <- NA
        # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
        # assign(sig3Val, signal3Vec)
        #signal threshold
        sig1min <- as.numeric(channSigConfig[sig1idx,5])
        sig1max <- as.numeric(channSigConfig[sig1idx,6])
        sig2min <- as.numeric(channSigConfig[sig2idx,5])
        sig2max <- as.numeric(channSigConfig[sig2idx,6])
        #sig3min <- as.numeric(channSigConfig[sig3idx,5])
        #sig3max <- as.numeric(channSigConfig[sig3idx,6])
        
        calc1Val <- getChann1(get(sig1Val), X,sig1min,sig1max)
        calc2Val <- getChann1(get(sig2Val), X,sig2min,sig2max)
        #sig3max <- as.numeric(channSigConfig[sig3idx,6])
        assign(x = varName[1],
               value = do.call("countTable1D", args = list(
                 x = calc1Val[calc2Val > 0], mergeTab = get(varName)
               )))
        # assign(varNameMile[1], c(max(
        #   0,
        #   max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
        # )))
        if (is.null(get(varName))) {
          f_idx <- which(mainConfig[, 1] == 257)
          failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
          failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
          if(length(colName1) == 0){
            failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
          }else if(length(colName2) == 0){
            failSlides[fail_index,4] <-as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
          }
          fail_index <- fail_index + 1
        } else{
          ##saveRDS in the dir location
          g_idx <- which(mainConfig[, 1] == 257)
          GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
          GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
          #summary of Signal 1
          colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
          colName1 <- c(colName1,colName1)
          col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
          count1 <- sum(length(which(!is.na(X[,col1]))))
          perc1 <- round(count1/nrow(X)*100)
          GoodSlides[good_index,3] <- paste0(col1)
          GoodSlides[good_index,4] <- paste0(count1)
          GoodSlides[good_index,5] <- paste0(perc1,"%")
          #summary of Signal 2 
          colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
          colName2 <- c(colName2,colName2)
          col2 <- colName2[which.max(colSums(!is.na(X[,colName2])))]
          count2 <- sum(length(which(!is.na(X[,col1]))))
          perc2 <- round(count2/nrow(X)*100)
          GoodSlides[good_index,6] <- paste0(col2)
          GoodSlides[good_index,7] <- paste0(count2)
          GoodSlides[good_index,8] <- paste0(perc2,"%")
          #summary of Signal 3 
          # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
          # colName3 <- c(colName3,colName3)
          # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
          # count3 <- sum(length(which(!is.na(X[,col3]))))
          # perc3 <- round(count3/nrow(X)*100)
          # GoodSlides[good_index,9] <- paste0(col3)
          # GoodSlides[good_index,10] <- paste0(count3)
          # GoodSlides[good_index,11] <- paste0(perc3,"%")
          # 
          good_index <- good_index + 1
          saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
          # saveRDS(get(varNameMile[1]),
          #         paste0(dir, varNameMile[1], ".rds"))
        }
        ##remove the variables to free up the memory
        endtime <- Sys.time()
        print(paste0(
          "total time for calculating ",
          varName[1],
          " is ",
          endtime - starttime
        ))
        rm(calc1Val, calc2Val, calc5Odo)
        rm(list = paste0(varName[1]))
        ## rm(list = paste0(varNameMile[1]))
      }
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    }
    else if (gid[idx] == 258) {
      # 2.27 AbgasrÃ¼ckfÃ¼hrung	2.27.2 Stellerposition HD-AGR
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      if (engType == "O") {
        starttime <- Sys.time()
        varName <- as.character(mainConfig[idx, 6])
        # varNameMile <- as.character(paste0(varName, "Mile"))
        assign(varName, NULL)
        # assign(varNameMile, NULL)
        calc1Val <- as.character(mainConfig[idx, 8])
        calc2Val <- as.character(mainConfig[idx, 13])
        #calc5Odo <- as.character(mainConfig[idx, 14])
        #signal 1
        sig1idx <- which(channSigConfig[, 3] == calc1Val)
        sig1Val <- channSigConfig[sig1idx, 4]
        signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
        signal1Vec[signal1Vec == "NA"] <- NA
        signal1Vec <- signal1Vec[!is.na(signal1Vec)]
        assign(sig1Val, signal1Vec)
        #signal 2
        sig2idx <- which(channSigConfig[, 3] == calc2Val)
        sig2Val <- channSigConfig[sig2idx, 4]
        signal2Vec <- as.character(channSigConfig[sig2idx, 8:25])
        signal2Vec[signal2Vec == "NA"] <- NA
        signal2Vec <- signal2Vec[!is.na(signal2Vec)]
        assign(sig2Val, signal2Vec)
        # #Signal3 - Odo
        # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
        # sig3Val <- channSigConfig[sig3idx, 4]
        # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
        # signal3Vec[signal3Vec == "NA"] <- NA
        # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
        # assign(sig3Val, signal3Vec)
        #signal threshold
        sig1min <- as.numeric(channSigConfig[sig1idx,5])
        sig1max <- as.numeric(channSigConfig[sig1idx,6])
        sig2min <- as.numeric(channSigConfig[sig2idx,5])
        sig2max <- as.numeric(channSigConfig[sig2idx,6])
        #sig3min <- as.numeric(channSigConfig[sig3idx,5])
        #sig3max <- as.numeric(channSigConfig[sig3idx,6])
        
        calc1Val <- getChann1(get(sig1Val), X,sig1min,sig1max)
        calc2Val <- getChann1(get(sig2Val), X,sig2min,sig2max)
        #sig3max <- as.numeric(channSigConfig[sig3idx,6])
        assign(x = varName[1],
               value = do.call("countTable1D", args = list(
                 x = calc1Val[calc2Val > 0], mergeTab = get(varName)
               )))
        # assign(varNameMile[1], c(max(
        #   0,
        #   max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
        # )))
        if (is.null(get(varName))) {
          f_idx <- which(mainConfig[, 1] == 258)
          failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
          failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
          if(length(colName1) == 0){
            failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
          }else if(length(colName2) == 0){
            failSlides[fail_index,4] <-as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
          }
          fail_index <- fail_index + 1
        } else{
          ##saveRDS in the dir location
          g_idx <- which(mainConfig[, 1] == 258)
          GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
          GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
          #summary of Signal 1
          colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
          colName1 <- c(colName1,colName1)
          col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
          count1 <- sum(length(which(!is.na(X[,col1]))))
          perc1 <- round(count1/nrow(X)*100)
          GoodSlides[good_index,3] <- paste0(col1)
          GoodSlides[good_index,4] <- paste0(count1)
          GoodSlides[good_index,5] <- paste0(perc1,"%")
          #summary of Signal 2 
          colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
          colName2 <- c(colName2,colName2)
          col2 <- colName2[which.max(colSums(!is.na(X[,colName2])))]
          count2 <- sum(length(which(!is.na(X[,col1]))))
          perc2 <- round(count2/nrow(X)*100)
          GoodSlides[good_index,6] <- paste0(col2)
          GoodSlides[good_index,7] <- paste0(count2)
          GoodSlides[good_index,8] <- paste0(perc2,"%")
          #summary of Signal 3 
          # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
          # colName3 <- c(colName3,colName3)
          # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
          # count3 <- sum(length(which(!is.na(X[,col3]))))
          # perc3 <- round(count3/nrow(X)*100)
          # GoodSlides[good_index,9] <- paste0(col3)
          # GoodSlides[good_index,10] <- paste0(count3)
          # GoodSlides[good_index,11] <- paste0(perc3,"%")
          
          #GoodSlides[good_index,17] <- paste0(perc5,"%")
          good_index <- good_index + 1
          saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
          # saveRDS(get(varNameMile[1]),
          #         paste0(dir, varNameMile[1], ".rds"))
        }
        ##remove the variables to free up the memory
        endtime <- Sys.time()
        print(paste0(
          "total time for calculating ",
          varName[1],
          " is ",
          endtime - starttime
        ))
        rm(calc1Val, calc2Val, calc5Odo)
        rm(list = paste0(varName[1]))
        ## rm(list = paste0(varNameMile[1]))
      }
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    } 
    else if (gid[idx] == 2581) {
      # 2.27 AbgasrÃ¼ckfÃ¼hrung	2.27.3 Anzahl AGR-KÃ¼hler-Bypass-Klappen-Schaltungen
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      if (engType == "O") {
        starttime <- Sys.time()
        varName <- as.character(mainConfig[idx, 6])
        # varNameMile <- as.character(paste0(varName, "Mile"))
        assign(varName, NULL)
        # assign(varNameMile, NULL)
        calc1Val <- as.character(mainConfig[idx, 10])
        #calc5Odo <- as.character(mainConfig[idx, 14])
        #signal 1
        sig1idx <- which(channSigConfig[, 3] == calc1Val)
        sig1Val <- channSigConfig[sig1idx, 4]
        signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
        signal1Vec[signal1Vec == "NA"] <- NA
        signal1Vec <- signal1Vec[!is.na(signal1Vec)]
        assign(sig1Val, signal1Vec)
        #Signal3 - Odo
        # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
        # sig3Val <- channSigConfig[sig3idx, 4]
        # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
        # signal3Vec[signal3Vec == "NA"] <- NA
        # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
        # assign(sig3Val, signal3Vec)
        #signal threshold
        sig1min <- as.numeric(channSigConfig[sig1idx,5])
        sig1max <- as.numeric(channSigConfig[sig1idx,6])
        
        #sig3min <- as.numeric(channSigConfig[sig3idx,5])
        #sig3max <- as.numeric(channSigConfig[sig3idx,6])
        
        egrNumVal <- NULL
        for (ri in 1:length(rleTrack$starts)){
          X1 <- X[rleTrack$starts[ri]:rleTrack$stops[ri],]
          calc1Val <- getChann1(get(sig1Val), X1,sig1min,sig1max)
          #sig3max <- as.numeric(channSigConfig[sig3idx,6])
          #XEgr<-getChann(sigEGR,X) ## added on 06-09-2017 for 2.27.3 Anzahl Bypass-Klappen-Schaltungen
          minEGRVal <- 0
          threshEGRVal <- 100
          idEGRVal <-
            startIndicesCount(calc1Val, threshEGRVal, minEGRVal)
          egrNumVal <- c(egrNumVal, idEGRVal)
        }
        assign(varName[1], egrNumVal)
        # assign(varNameMile[1], c(max(
        #   0,
        #   max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
        # )))
        if (is.null(get(varName))) {
          f_idx <- which(mainConfig[, 1] == 2581)
          failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
          failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
          if(length(colName1) == 0){
            failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
          }else if(length(colName2) == 0){
            failSlides[fail_index,4] <-as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
          }
          fail_index <- fail_index + 1
        } else{
          ##saveRDS in the dir location
          g_idx <- which(mainConfig[, 1] == 2581)
          GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
          GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
          #summary of Signal 1
          colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
          colName1 <- c(colName1,colName1)
          col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
          count1 <- sum(length(which(!is.na(X[,col1]))))
          perc1 <- round(count1/nrow(X)*100)
          GoodSlides[good_index,3] <- paste0(col1)
          GoodSlides[good_index,4] <- paste0(count1)
          GoodSlides[good_index,5] <- paste0(perc1,"%")
          
          #summary of Signal 3 
          # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
          # colName3 <- c(colName3,colName3)
          # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
          # count3 <- sum(length(which(!is.na(X[,col3]))))
          # perc3 <- round(count3/nrow(X)*100)
          # GoodSlides[good_index,9] <- paste0(col3)
          # GoodSlides[good_index,10] <- paste0(count3)
          # GoodSlides[good_index,11] <- paste0(perc3,"%")
          
          good_index <- good_index + 1
          saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
          # saveRDS(get(varNameMile[1]),
          #         paste0(dir, varNameMile[1], ".rds"))
        }
        ##remove the variables to free up the memory
        endtime <- Sys.time()
        print(paste0(
          "total time for calculating ",
          varName[1],
          " is ",
          endtime - starttime
        ))
        rm(calc1Val, calc5Odo)
        rm(list = paste0(varName[1]))
        # rm(list = paste0(varNameMile[1]))
      }
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    }
    else if (gid[idx] == 259) {
      # 2.28 Elektromaschine	2.28.1 Generatorlast
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      starttime <- Sys.time()
      varName <- as.character(mainConfig[idx, 6])
      # varNameMile <- as.character(paste0(varName, "Mile"))
      assign(varName, NULL)
      # assign(varNameMile, NULL)
      calc1Val <- as.character(mainConfig[idx, 8])
      calc2Val <- as.character(mainConfig[idx, 13])
      #calc5Odo <- as.character(mainConfig[idx, 14])
      #signal 1
      sig1idx <- which(channSigConfig[, 3] == calc1Val)
      sig1Val <- channSigConfig[sig1idx, 4]
      signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
      signal1Vec[signal1Vec == "NA"] <- NA
      signal1Vec <- signal1Vec[!is.na(signal1Vec)]
      assign(sig1Val, signal1Vec)
      #signal 2
      sig2idx <- which(channSigConfig[, 3] == calc2Val)
      sig2Val <- channSigConfig[sig2idx, 4]
      signal2Vec <- as.character(channSigConfig[sig2idx, 8:25])
      signal2Vec[signal2Vec == "NA"] <- NA
      signal2Vec <- signal2Vec[!is.na(signal2Vec)]
      assign(sig2Val, signal2Vec)
      # #Signal3 - Odo
      # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
      # sig3Val <- channSigConfig[sig3idx, 4]
      # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
      # signal3Vec[signal3Vec == "NA"] <- NA
      # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
      # assign(sig3Val, signal3Vec)
      #signal threshold
      sig1min <- as.numeric(channSigConfig[sig1idx,5])
      sig1max <- as.numeric(channSigConfig[sig1idx,6])
      sig2min <- as.numeric(channSigConfig[sig2idx,5])
      sig2max <- as.numeric(channSigConfig[sig2idx,6])
      #sig3min <- as.numeric(channSigConfig[sig3idx,5])
      #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      
      calc1Val <- getChann1(get(sig1Val), X,sig1min,sig1max)
      calc2Val <- getChann1(get(sig2Val), X,sig2min,sig2max)
      #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      assign(x = varName[1],
             value = do.call("countTable1D", args = list(
               x = calc1Val[calc2Val > 0], mergeTab = get(varName)
             )))
      # assign(varNameMile[1], c(max(
      #   0, max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
      # )))
      if (is.null(get(varName))) {
        f_idx <- which(mainConfig[, 1] == 259)
        failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
        failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
        if(length(colName1) == 0){
          failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
        }else if(length(colName2) == 0){
          failSlides[fail_index,4] <-as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
        }
        fail_index <- fail_index + 1
      } else{
        ##saveRDS in the dir location
        g_idx <- which(mainConfig[, 1] == 259)
        GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
        GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
        #summary of Signal 1
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName1 <- c(colName1,colName1)
        col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
        count1 <- sum(length(which(!is.na(X[,col1]))))
        perc1 <- round(count1/nrow(X)*100)
        GoodSlides[good_index,3] <- paste0(col1)
        GoodSlides[good_index,4] <- paste0(count1)
        GoodSlides[good_index,5] <- paste0(perc1,"%")
        #summary of Signal 2 
        colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
        colName2 <- c(colName2,colName2)
        col2 <- colName2[which.max(colSums(!is.na(X[,colName2])))]
        count2 <- sum(length(which(!is.na(X[,col1]))))
        perc2 <- round(count2/nrow(X)*100)
        GoodSlides[good_index,6] <- paste0(col2)
        GoodSlides[good_index,7] <- paste0(count2)
        GoodSlides[good_index,8] <- paste0(perc2,"%")
        #summary of Signal 3 
        # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
        # colName3 <- c(colName3,colName3)
        # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
        # count3 <- sum(length(which(!is.na(X[,col3]))))
        # perc3 <- round(count3/nrow(X)*100)
        # GoodSlides[good_index,9] <- paste0(col3)
        # GoodSlides[good_index,10] <- paste0(count3)
        # GoodSlides[good_index,11] <- paste0(perc3,"%")
        
        good_index <- good_index + 1
        saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
        # saveRDS(get(varNameMile[1]), paste0(dir, varNameMile[1], ".rds"))
      }
      ##remove the variables to free up the memory
      endtime <- Sys.time()
      print(paste0(
        "total time for calculating ",
        varName[1],
        " is ",
        endtime - starttime
      ))
      rm(calc1Val, calc2Val, calc5Odo)
      rm(list = paste0(varName[1]))
      # rm(list = paste0(varNameMile[1]))
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    }
    else if (gid[idx] == 260) {
      # 2.28 Elektromaschine	2.28.2 Motordrehzahl-Generatorlast-Kennfeld
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      starttime <- Sys.time()
      varName <- as.character(mainConfig[idx, 6])
      # varNameMile <- as.character(paste0(varName, "Mile"))
      assign(varName, NULL)
      # assign(varNameMile, NULL)
      calc1Val <- as.character(mainConfig[idx, 8])
      calc2Val <- as.character(mainConfig[idx, 9])
      #calc5Odo <- as.character(mainConfig[idx, 14])
      #signal 1
      sig1idx <- which(channSigConfig[, 3] == calc1Val)
      sig1Val <- channSigConfig[sig1idx, 4]
      signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
      signal1Vec[signal1Vec == "NA"] <- NA
      signal1Vec <- signal1Vec[!is.na(signal1Vec)]
      assign(sig1Val, signal1Vec)
      #signal 2
      sig2idx <- which(channSigConfig[, 3] == calc2Val)
      sig2Val <- channSigConfig[sig2idx, 4]
      signal2Vec <- as.character(channSigConfig[sig2idx, 8:25])
      signal2Vec[signal2Vec == "NA"] <- NA
      signal2Vec <- signal2Vec[!is.na(signal2Vec)]
      assign(sig2Val, signal2Vec)
      #Signal3 - Odo
      # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
      # sig3Val <- channSigConfig[sig3idx, 4]
      # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
      # signal3Vec[signal3Vec == "NA"] <- NA
      # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
      # assign(sig3Val, signal3Vec)
      #signal threshold
      sig1min <- as.numeric(channSigConfig[sig1idx,5])
      sig1max <- as.numeric(channSigConfig[sig1idx,6])
      sig2min <- as.numeric(channSigConfig[sig2idx,5])
      sig2max <- as.numeric(channSigConfig[sig2idx,6])
      #sig3min <- as.numeric(channSigConfig[sig3idx,5])
      #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      
      calc1Val <- getChann1(get(sig1Val), X,sig1min,sig1max)
      calc2Val <- getChann1(get(sig2Val), X,sig2min,sig2max)
      #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      assign(x = varName[1],
             value = do.call(
               "countTable2D",
               args = list(
                 x = calc1Val,
                 y = calc2Val,
                 mergeTab = get(varName)
               )
             ))
      # assign(varNameMile[1], c(max(
      #   0, max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
      # )))
      if (is.null(get(varName))) {
        f_idx <- which(mainConfig[, 1] == 260)
        failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
        failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
        if(length(colName1) == 0){
          failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
        }else if(length(colName2) == 0){
          failSlides[fail_index,4] <-as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
        }
        fail_index <- fail_index + 1
      } else{
        ##saveRDS in the dir location
        g_idx <- which(mainConfig[, 1] == 260)
        GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
        GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
        #summary of Signal 1
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName1 <- c(colName1,colName1)
        col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
        count1 <- sum(length(which(!is.na(X[,col1]))))
        perc1 <- round(count1/nrow(X)*100)
        GoodSlides[good_index,3] <- paste0(col1)
        GoodSlides[good_index,4] <- paste0(count1)
        GoodSlides[good_index,5] <- paste0(perc1,"%")
        #summary of Signal 2 
        colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
        colName2 <- c(colName2,colName2)
        col2 <- colName2[which.max(colSums(!is.na(X[,colName2])))]
        count2 <- sum(length(which(!is.na(X[,col1]))))
        perc2 <- round(count2/nrow(X)*100)
        GoodSlides[good_index,6] <- paste0(col2)
        GoodSlides[good_index,7] <- paste0(count2)
        GoodSlides[good_index,8] <- paste0(perc2,"%")
        # #summary of Signal 3 
        # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
        # colName3 <- c(colName3,colName3)
        # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
        # count3 <- sum(length(which(!is.na(X[,col3]))))
        # perc3 <- round(count3/nrow(X)*100)
        # GoodSlides[good_index,9] <- paste0(col3)
        # GoodSlides[good_index,10] <- paste0(count3)
        # GoodSlides[good_index,11] <- paste0(perc3,"%")
        
        good_index <- good_index + 1
        saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
        # saveRDS(get(varNameMile[1]), paste0(dir, varNameMile[1], ".rds"))
      }
      ##remove the variables to free up the memory
      endtime <- Sys.time()
      print(paste0(
        "total time for calculating ",
        varName[1],
        " is ",
        endtime - starttime
      ))
      rm(calc1Val, calc2Val, calc5Odo)
      rm(list = paste0(varName[1]))
      # rm(list = paste0(varNameMile[1]))
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    }
    else if (gid[idx] == 261) {
      # 2.28 Elektromaschine	2.28.3 Motordrehzahl-Elektromaschinenleistung-Kennfeld
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      starttime <- Sys.time()
      varName <- as.character(mainConfig[idx, 6])
      # varNameMile <- as.character(paste0(varName, "Mile"))
      assign(varName, NULL)
      # assign(varNameMile, NULL)
      calc1Val <- as.character(mainConfig[idx, 8])
      calc2Val <- as.character(mainConfig[idx, 10])
      calc3Val <- as.character(mainConfig[idx, 11])
      calc4Val <- as.character(mainConfig[idx, 13])
      #calc5Odo <- as.character(mainConfig[idx, 14])
      #signal 1
      sig1idx <- which(channSigConfig[, 3] == calc1Val)
      sig1Val <- channSigConfig[sig1idx, 4]
      signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
      signal1Vec[signal1Vec == "NA"] <- NA
      signal1Vec <- signal1Vec[!is.na(signal1Vec)]
      assign(sig1Val, signal1Vec)
      #signal 2
      sig2idx <- which(channSigConfig[, 3] == calc2Val)
      sig2Val <- channSigConfig[sig2idx, 4]
      signal2Vec <- as.character(channSigConfig[sig2idx, 8:25])
      signal2Vec[signal2Vec == "NA"] <- NA
      signal2Vec <- signal2Vec[!is.na(signal2Vec)]
      assign(sig2Val, signal2Vec)
      #signal 3
      sig5idx <- which(channSigConfig[, 3] == calc3Val)
      sig5Val <- channSigConfig[sig5idx, 4]
      signal5Vec <- as.character(channSigConfig[sig5idx, 8:25])
      signal5Vec[signal5Vec == "NA"] <- NA
      signal5Vec <- signal5Vec[!is.na(signal5Vec)]
      assign(sig5Val, signal5Vec)
      #Signal5 - Odo
      # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
      # sig3Val <- channSigConfig[sig3idx, 4]
      # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
      # signal3Vec[signal3Vec == "NA"] <- NA
      # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
      # assign(sig3Val, signal3Vec)
      #Signal 4
      sig4idx <- which(channSigConfig[, 3] == calc4Val)
      sig4Val <- channSigConfig[sig4idx, 4]
      signal4Vec <- as.character(channSigConfig[sig4idx, 8:25])
      signal4Vec[signal4Vec == "NA"] <- NA
      signal4Vec <- signal4Vec[!is.na(signal4Vec)]
      assign(sig4Val, signal4Vec)
      #signal threshold
      sig1min <- as.numeric(channSigConfig[sig1idx,5])
      sig1max <- as.numeric(channSigConfig[sig1idx,6])
      sig2min <- as.numeric(channSigConfig[sig2idx,5])
      sig2max <- as.numeric(channSigConfig[sig2idx,6])
      #sig3min <- as.numeric(channSigConfig[sig3idx,5])
      #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      sig4min <- as.numeric(channSigConfig[sig4idx,5])
      sig4max <- as.numeric(channSigConfig[sig4idx,6])
      sig5min <- as.numeric(channSigConfig[sig5idx,5])
      sig5max <- as.numeric(channSigConfig[sig5idx,6])
      # XEngTor <- getChann(sigEngTor,X,1000)
      calc1Val <- getChann1(get(sig1Val), X,sig1min,sig1max)
      calc2Val <- getChann1(get(sig2Val), X,sig2min,sig2max)
      calc3Val <- getChann1(get(sig5Val), X,sig5min,sig5max)
      #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      calc4Val <- getChann1(get(sig4Val), X,sig4min,sig4max)
      assign(x = varName[1],
             value = do.call("countTable2D", args = list(
               x = calc1Val,
               y = (2*pi*calc2Val*calc3Val)/(1000*60),
               mergeTab = get(varName)
             )))
      # assign(varNameMile[1], c(max(
      #   0, max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
      # )))
      if (is.null(get(varName))) {
        f_idx <- which(mainConfig[, 1] == 261)
        failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
        failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
        colName3 <- get(sig5Val)[get(sig5Val) %in% names(X)]
        if(length(colName1) == 0){
          failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
        }else if(length(colName2) == 0){
          failSlides[fail_index,4] <- as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
        }
        else if(length(colName2) == 0){
          failSlides[fail_index,5] <- as.character(paste0("None of the Signals ", paste(get(sig5Val),collapse = ';')," not present."))
        }
        fail_index <- fail_index + 1
      } else{
        ##saveRDS in the dir location
        g_idx <- which(mainConfig[, 1] == 261)
        GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
        GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
        #summary of Signal 1
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName1 <- c(colName1,colName1)
        col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
        count1 <- sum(length(which(!is.na(X[,col1]))))
        perc1 <- round(count1/nrow(X)*100)
        GoodSlides[good_index,3] <- paste0(col1)
        GoodSlides[good_index,4] <- paste0(count1)
        GoodSlides[good_index,5] <- paste0(perc1,"%")
        #summary of Signal 2 
        colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
        colName2 <- c(colName2,colName2)
        col2 <- colName2[which.max(colSums(!is.na(X[,colName2])))]
        count2 <- sum(length(which(!is.na(X[,col1]))))
        perc2 <- round(count2/nrow(X)*100)
        GoodSlides[good_index,6] <- paste0(col2)
        GoodSlides[good_index,7] <- paste0(count2)
        GoodSlides[good_index,8] <- paste0(perc2,"%")
        #summary of Signal 3 
        # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
        # colName3 <- c(colName3,colName3)
        # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
        # count3 <- sum(length(which(!is.na(X[,col3]))))
        # perc3 <- round(count3/nrow(X)*100)
        # GoodSlides[good_index,9] <- paste0(col3)
        # GoodSlides[good_index,10] <- paste0(count3)
        # GoodSlides[good_index,11] <- paste0(perc3,"%")
        #summary of Signal 4 
        colName4 <- get(sig4Val)[get(sig4Val) %in% names(X)]
        colName4 <- c(colName4,colName4)
        col4 <- colName4[which.max(colSums(!is.na(X[,colName4])))]
        count4 <- sum(length(which(!is.na(X[,col4]))))
        perc4 <- round(count4/nrow(X)*100)
        GoodSlides[good_index,12] <- paste0(col4)
        GoodSlides[good_index,13] <- paste0(count4)
        GoodSlides[good_index,14] <- paste0(perc4,"%")
        #summary of Signal 5 
        colName5 <- get(sig5Val)[get(sig5Val) %in% names(X)]
        colName5 <- c(colName5,colName5)
        col5 <- colName5[which.max(colSums(!is.na(X[,colName5])))]
        count5 <- sum(length(which(!is.na(X[,col5]))))
        perc5 <- round(count5/nrow(X)*100)
        GoodSlides[good_index,15] <- paste0(col5)
        GoodSlides[good_index,16] <- paste0(count5)
        GoodSlides[good_index,17] <- paste0(perc5,"%")
        good_index <- good_index + 1
        saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
        # saveRDS(get(varNameMile[1]), paste0(dir, varNameMile[1], ".rds"))
      }
      ##remove the variables to free up the memory
      endtime <- Sys.time()
      print(paste0(
        "total time for calculating ",
        varName[1],
        " is ",
        endtime - starttime
      ))
      rm(calc1Val, calc2Val, calc5Odo)
      rm(list = paste0(varName[1]))
      # rm(list = paste0(varNameMile[1]))
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    }
    else if (gid[idx] == 262) {
      # 2.28 Elektromaschine	2.28.4 Motordrehzahl-Elektromaschinendrehmoment-Kennfeld
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      starttime <- Sys.time()
      varName <- as.character(mainConfig[idx, 6])
      # varNameMile <- as.character(paste0(varName, "Mile"))
      assign(varName, NULL)
      # assign(varNameMile, NULL)
      calc1Val <- as.character(mainConfig[idx, 8])
      calc2Val <- as.character(mainConfig[idx, 10])
      calc4Val <- as.character(mainConfig[idx, 13])
      #calc5Odo <- as.character(mainConfig[idx, 14])
      #signal 1
      sig1idx <- which(channSigConfig[, 3] == calc1Val)
      sig1Val <- channSigConfig[sig1idx, 4]
      signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
      signal1Vec[signal1Vec == "NA"] <- NA
      signal1Vec <- signal1Vec[!is.na(signal1Vec)]
      assign(sig1Val, signal1Vec)
      #signal 2
      sig2idx <- which(channSigConfig[, 3] == calc2Val)
      sig2Val <- channSigConfig[sig2idx, 4]
      signal2Vec <- as.character(channSigConfig[sig2idx, 8:25])
      signal2Vec[signal2Vec == "NA"] <- NA
      signal2Vec <- signal2Vec[!is.na(signal2Vec)]
      assign(sig2Val, signal2Vec)
      # #Signal5 - Odo
      # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
      # sig3Val <- channSigConfig[sig3idx, 4]
      # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
      # signal3Vec[signal3Vec == "NA"] <- NA
      # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
      # assign(sig3Val, signal3Vec)
      #Signal 4
      sig4idx <- which(channSigConfig[, 3] == calc4Val)
      sig4Val <- channSigConfig[sig4idx, 4]
      signal4Vec <- as.character(channSigConfig[sig4idx, 8:25])
      signal4Vec[signal4Vec == "NA"] <- NA
      signal4Vec <- signal4Vec[!is.na(signal4Vec)]
      assign(sig4Val, signal4Vec)
      #signal threshold
      sig1min <- as.numeric(channSigConfig[sig1idx,5])
      sig1max <- as.numeric(channSigConfig[sig1idx,6])
      sig2min <- as.numeric(channSigConfig[sig2idx,5])
      sig2max <- as.numeric(channSigConfig[sig2idx,6])
      #sig3min <- as.numeric(channSigConfig[sig3idx,5])
      #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      sig4min <- as.numeric(channSigConfig[sig4idx,5])
      sig4max <- as.numeric(channSigConfig[sig4idx,6])
      
      calc1Val <- getChann1(get(sig1Val), X,sig1min,sig1max)
      calc2Val <- getChann1(get(sig2Val), X,sig2min,sig2max)
      #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      calc4Val <- getChann1(get(sig4Val), X,sig4min,sig4max)
      #Operating Time Calc
      
      assign(x = varName[1],
             value = do.call("countTable2D", args = list(
               x = calc1Val,
               y = calc2Val,
               mergeTab = get(varName)
             )))
      # assign(varNameMile[1], c(max(
      #   0, max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
      # )))
      if (is.null(get(varName))) {
        f_idx <- which(mainConfig[, 1] == 262)
        failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
        failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
        colName3 <- get(sig4Val)[get(sig4Val) %in% names(X)]
        if(length(colName1) == 0){
          failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
        }else if(length(colName2) == 0){
          failSlides[fail_index,4] <- as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
        }
        else if(length(colName2) == 0){
          failSlides[fail_index,5] <- as.character(paste0("None of the Signals ", paste(get(sig4Val),collapse = ';')," not present."))
        }
        fail_index <- fail_index + 1
      } else{
        ##saveRDS in the dir location
        g_idx <- which(mainConfig[, 1] == 262)
        GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
        GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
        #summary of Signal 1
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName1 <- c(colName1,colName1)
        col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
        count1 <- sum(length(which(!is.na(X[,col1]))))
        perc1 <- round(count1/nrow(X)*100)
        GoodSlides[good_index,3] <- paste0(col1)
        GoodSlides[good_index,4] <- paste0(count1)
        GoodSlides[good_index,5] <- paste0(perc1,"%")
        #summary of Signal 2 
        colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
        colName2 <- c(colName2,colName2)
        col2 <- colName2[which.max(colSums(!is.na(X[,colName2])))]
        count2 <- sum(length(which(!is.na(X[,col1]))))
        perc2 <- round(count2/nrow(X)*100)
        GoodSlides[good_index,6] <- paste0(col2)
        GoodSlides[good_index,7] <- paste0(count2)
        GoodSlides[good_index,8] <- paste0(perc2,"%")
        #summary of Signal 3 
        # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
        # colName3 <- c(colName3,colName3)
        # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
        # count3 <- sum(length(which(!is.na(X[,col3]))))
        # perc3 <- round(count3/nrow(X)*100)
        # GoodSlides[good_index,9] <- paste0(col3)
        # GoodSlides[good_index,10] <- paste0(count3)
        # GoodSlides[good_index,11] <- paste0(perc3,"%")
        #summary of Signal 4 
        colName4 <- get(sig4Val)[get(sig4Val) %in% names(X)]
        colName4 <- c(colName4,colName4)
        col4 <- colName4[which.max(colSums(!is.na(X[,colName4])))]
        count4 <- sum(length(which(!is.na(X[,col4]))))
        perc4 <- round(count4/nrow(X)*100)
        GoodSlides[good_index,12] <- paste0(col4)
        GoodSlides[good_index,13] <- paste0(count4)
        GoodSlides[good_index,14] <- paste0(perc4,"%")
        good_index <- good_index + 1
        saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
        # saveRDS(get(varNameMile[1]), paste0(dir, varNameMile[1], ".rds"))
      }
      ##remove the variables to free up the memory
      endtime <- Sys.time()
      print(paste0(
        "total time for calculating ",
        varName[1],
        " is ",
        endtime - starttime
      ))
      rm(calc1Val, calc2Val, calc5Odo)
      rm(list = paste0(varName[1]))
      # rm(list = paste0(varNameMile[1]))
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    }
    else if (gid[idx] == 263) {
      # 2.28 Elektromaschine	2.28.5 Motorleistung-Elektromaschinenleistung-Kennfeld
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      starttime <- Sys.time()
      varName <- as.character(mainConfig[idx, 6])
      # varNameMile <- as.character(paste0(varName, "Mile"))
      assign(varName, NULL)
      # assign(varNameMile, NULL)
      calc1Val <- as.character(mainConfig[idx, 10])
      calc2Val <- as.character(mainConfig[idx, 11])
      calc3Val <- as.character(mainConfig[idx, 12])
      calc4Val <- as.character(mainConfig[idx, 13])
      #calc5Odo <- as.character(mainConfig[idx, 14])
      #signal 1
      sig1idx <- which(channSigConfig[, 3] == calc1Val)
      sig1Val <- channSigConfig[sig1idx, 4]
      signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
      signal1Vec[signal1Vec == "NA"] <- NA
      signal1Vec <- signal1Vec[!is.na(signal1Vec)]
      assign(sig1Val, signal1Vec)
      #signal 2
      sig2idx <- which(channSigConfig[, 3] == calc2Val)
      sig2Val <- channSigConfig[sig2idx, 4]
      signal2Vec <- as.character(channSigConfig[sig2idx, 8:25])
      signal2Vec[signal2Vec == "NA"] <- NA
      signal2Vec <- signal2Vec[!is.na(signal2Vec)]
      assign(sig2Val, signal2Vec)
      #signal 3
      sig5idx <- which(channSigConfig[, 3] == calc3Val)
      sig5Val <- channSigConfig[sig5idx, 4]
      signal5Vec <- as.character(channSigConfig[sig5idx, 8:25])
      signal5Vec[signal5Vec == "NA"] <- NA
      signal5Vec <- signal5Vec[!is.na(signal5Vec)]
      assign(sig5Val, signal5Vec)
      #Signal5 - Odo
      # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
      # sig3Val <- channSigConfig[sig3idx, 4]
      # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
      # signal3Vec[signal3Vec == "NA"] <- NA
      # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
      # assign(sig3Val, signal3Vec)
      #Signal 4
      sig4idx <- which(channSigConfig[, 3] == calc4Val)
      sig4Val <- channSigConfig[sig4idx, 4]
      signal4Vec <- as.character(channSigConfig[sig4idx, 8:25])
      signal4Vec[signal4Vec == "NA"] <- NA
      signal4Vec <- signal4Vec[!is.na(signal4Vec)]
      assign(sig4Val, signal4Vec)
      #signal threshold
      sig1min <- as.numeric(channSigConfig[sig1idx,5])
      sig1max <- as.numeric(channSigConfig[sig1idx,6])
      sig2min <- as.numeric(channSigConfig[sig2idx,5])
      sig2max <- as.numeric(channSigConfig[sig2idx,6])
      #sig3min <- as.numeric(channSigConfig[sig3idx,5])
      #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      sig4min <- as.numeric(channSigConfig[sig4idx,5])
      sig4max <- as.numeric(channSigConfig[sig4idx,6])
      sig5min <- as.numeric(channSigConfig[sig5idx,5])
      sig5max <- as.numeric(channSigConfig[sig5idx,6])
      # XEngTor <- getChann(sigEngTor,X,1000)
      calc1Val <- getChann1(get(sig1Val), X,sig1min,sig1max)
      calc2Val <- getChann1(get(sig2Val), X,sig2min,sig2max)
      calc3Val <- getChann1(get(sig5Val), X,sig5min,sig5max)
      #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      calc4Val <- getChann1(get(sig4Val), X,sig4min,sig4max)
      #Operating Time Calc
      
      assign(x = varName[1],
             value = do.call("countTable2D", args = list(
               x = (2*pi*calc4Val*calc1Val)/(1000*60),
               y = (2*pi*calc2Val*calc3Val)/(1000*60),
               mergeTab = get(varName)
             )))
      # assign(varNameMile[1], c(max(
      #   0, max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
      # )))
      if (is.null(get(varName))) {
        f_idx <- which(mainConfig[, 1] == 263)
        failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
        failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
        #colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
        if(length(colName1) == 0){
          failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
        }else if(length(colName2) == 0){
          failSlides[fail_index,4] <- as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
        }
        # else if(length(colName2) == 0){
        #   failSlides[fail_index,5] <- as.character(paste0("None of the Signals ", paste(get(sig3Val),collapse = ';')," not present."))
        # }
        fail_index <- fail_index + 1
      } else{
        ##saveRDS in the dir location
        g_idx <- which(mainConfig[, 1] == 263)
        GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
        GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
        #summary of Signal 1
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName1 <- c(colName1,colName1)
        col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
        count1 <- sum(length(which(!is.na(X[,col1]))))
        perc1 <- round(count1/nrow(X)*100)
        GoodSlides[good_index,3] <- paste0(col1)
        GoodSlides[good_index,4] <- paste0(count1)
        GoodSlides[good_index,5] <- paste0(perc1,"%")
        #summary of Signal 2 
        colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
        colName2 <- c(colName2,colName2)
        col2 <- colName2[which.max(colSums(!is.na(X[,colName2])))]
        count2 <- sum(length(which(!is.na(X[,col1]))))
        perc2 <- round(count2/nrow(X)*100)
        GoodSlides[good_index,6] <- paste0(col2)
        GoodSlides[good_index,7] <- paste0(count2)
        GoodSlides[good_index,8] <- paste0(perc2,"%")
        #summary of Signal 3 
        # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
        # colName3 <- c(colName3,colName3)
        # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
        # count3 <- sum(length(which(!is.na(X[,col3]))))
        # perc3 <- round(count3/nrow(X)*100)
        # GoodSlides[good_index,9] <- paste0(col3)
        # GoodSlides[good_index,10] <- paste0(count3)
        # GoodSlides[good_index,11] <- paste0(perc3,"%")
        #summary of Signal 4 
        colName4 <- get(sig4Val)[get(sig4Val) %in% names(X)]
        colName4 <- c(colName4,colName4)
        col4 <- colName4[which.max(colSums(!is.na(X[,colName4])))]
        count4 <- sum(length(which(!is.na(X[,col4]))))
        perc4 <- round(count4/nrow(X)*100)
        GoodSlides[good_index,12] <- paste0(col4)
        GoodSlides[good_index,13] <- paste0(count4)
        GoodSlides[good_index,14] <- paste0(perc4,"%")
        #summary of Signal 5 
        colName5 <- get(sig5Val)[get(sig5Val) %in% names(X)]
        colName5 <- c(colName5,colName5)
        col5 <- colName5[which.max(colSums(!is.na(X[,colName5])))]
        count5 <- sum(length(which(!is.na(X[,col5]))))
        perc5 <- round(count5/nrow(X)*100)
        GoodSlides[good_index,15] <- paste0(col5)
        GoodSlides[good_index,16] <- paste0(count5)
        GoodSlides[good_index,17] <- paste0(perc5,"%")
        good_index <- good_index + 1
        saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
        # saveRDS(get(varNameMile[1]), paste0(dir, varNameMile[1], ".rds"))
      }
      ##remove the variables to free up the memory
      endtime <- Sys.time()
      print(paste0(
        "total time for calculating ",
        varName[1],
        " is ",
        endtime - starttime
      ))
      rm(calc1Val, calc2Val, calc5Odo)
      rm(list = paste0(varName[1]))
      # rm(list = paste0(varNameMile[1]))
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    }
    else if (gid[idx] == 2631) {
      # 2.28 Elektromaschine	2.28.5 Motorleistung-Elektromaschinenleistung-Kennfeld
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      starttime <- Sys.time()
      varName <- as.character(mainConfig[idx, 6])
      # varNameMile <- as.character(paste0(varName, "Mile"))
      assign(varName, NULL)
      # assign(varNameMile, NULL)
      calc1Val <- as.character(mainConfig[idx, 8])
      calc2Val <- as.character(mainConfig[idx, 10])
      calc3Val <- as.character(mainConfig[idx, 11])
      #calc5Odo <- as.character(mainConfig[idx, 14])
      #signal 1
      sig1idx <- which(channSigConfig[, 3] == calc1Val)
      sig1Val <- channSigConfig[sig1idx, 4]
      signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
      signal1Vec[signal1Vec == "NA"] <- NA
      signal1Vec <- signal1Vec[!is.na(signal1Vec)]
      assign(sig1Val, signal1Vec)
      #signal 2
      sig2idx <- which(channSigConfig[, 3] == calc2Val)
      sig2Val <- channSigConfig[sig2idx, 4]
      signal2Vec <- as.character(channSigConfig[sig2idx, 8:25])
      signal2Vec[signal2Vec == "NA"] <- NA
      signal2Vec <- signal2Vec[!is.na(signal2Vec)]
      assign(sig2Val, signal2Vec)
      #signal 3
      sig5idx <- which(channSigConfig[, 3] == calc3Val)
      sig5Val <- channSigConfig[sig5idx, 4]
      signal5Vec <- as.character(channSigConfig[sig5idx, 8:25])
      signal5Vec[signal5Vec == "NA"] <- NA
      signal5Vec <- signal5Vec[!is.na(signal5Vec)]
      assign(sig5Val, signal5Vec)
      #Signal5 - Odo
      # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
      # sig3Val <- channSigConfig[sig3idx, 4]
      # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
      # signal3Vec[signal3Vec == "NA"] <- NA
      # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
      # assign(sig3Val, signal3Vec)
      #signal threshold
      sig1min <- as.numeric(channSigConfig[sig1idx,5])
      sig1max <- as.numeric(channSigConfig[sig1idx,6])
      sig2min <- as.numeric(channSigConfig[sig2idx,5])
      sig2max <- as.numeric(channSigConfig[sig2idx,6])
      #sig3min <- as.numeric(channSigConfig[sig3idx,5])
      #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      sig5min <- as.numeric(channSigConfig[sig5idx,5])
      sig5max <- as.numeric(channSigConfig[sig5idx,6])
      # XEngTor <- getChann(sigEngTor,X,1000)
      calc1Val <- getChann1(get(sig1Val), X,sig1min,sig1max)
      calc2Val <- getChann1(get(sig2Val), X,sig2min,sig2max)
      calc3Val <- getChann1(get(sig5Val), X,sig5min,sig5max)
      #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      #
      
      XRSGPwr=(2*pi*calc3Val*calc2Val)/(1000*60)
      XRSGPwr <- data.frame(XRSGPwr,calc2Val)
      colnames(XRSGPwr) <- c("XRSGPwr","XIsgRsgSpd")
      assign(x = varName[1],
             value = XRSGPwr)
      # assign(varNameMile[1], c(max(
      #   0, max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
      # )))
      if (is.null(get(varName))) {
        f_idx <- which(mainConfig[, 1] == 2631)
        failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
        failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
        #colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
        if(length(colName1) == 0){
          failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
        }else if(length(colName2) == 0){
          failSlides[fail_index,4] <- as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
        }
        # else if(length(colName2) == 0){
        #   failSlides[fail_index,5] <- as.character(paste0("None of the Signals ", paste(get(sig3Val),collapse = ';')," not present."))
        # }
        fail_index <- fail_index + 1
      } else{
        ##saveRDS in the dir location
        g_idx <- which(mainConfig[, 1] == 2631)
        GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
        GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
        #summary of Signal 1
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName1 <- c(colName1,colName1)
        col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
        count1 <- sum(length(which(!is.na(X[,col1]))))
        perc1 <- round(count1/nrow(X)*100)
        GoodSlides[good_index,3] <- paste0(col1)
        GoodSlides[good_index,4] <- paste0(count1)
        GoodSlides[good_index,5] <- paste0(perc1,"%")
        #summary of Signal 2 
        colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
        colName2 <- c(colName2,colName2)
        col2 <- colName2[which.max(colSums(!is.na(X[,colName2])))]
        count2 <- sum(length(which(!is.na(X[,col1]))))
        perc2 <- round(count2/nrow(X)*100)
        GoodSlides[good_index,6] <- paste0(col2)
        GoodSlides[good_index,7] <- paste0(count2)
        GoodSlides[good_index,8] <- paste0(perc2,"%")
        #summary of Signal 3 
        # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
        # colName3 <- c(colName3,colName3)
        # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
        # count3 <- sum(length(which(!is.na(X[,col3]))))
        # perc3 <- round(count3/nrow(X)*100)
        # GoodSlides[good_index,9] <- paste0(col3)
        # GoodSlides[good_index,10] <- paste0(count3)
        # GoodSlides[good_index,11] <- paste0(perc3,"%")
        #summary of Signal 4 
        #summary of Signal 5 
        colName5 <- get(sig5Val)[get(sig5Val) %in% names(X)]
        colName5 <- c(colName5,colName5)
        col5 <- colName5[which.max(colSums(!is.na(X[,colName5])))]
        count5 <- sum(length(which(!is.na(X[,col5]))))
        perc5 <- round(count5/nrow(X)*100)
        GoodSlides[good_index,15] <- paste0(col5)
        GoodSlides[good_index,16] <- paste0(count5)
        GoodSlides[good_index,17] <- paste0(perc5,"%")
        good_index <- good_index + 1
        saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
        # saveRDS(get(varNameMile[1]), paste0(dir, varNameMile[1], ".rds"))
      }
      ##remove the variables to free up the memory
      endtime <- Sys.time()
      print(paste0(
        "total time for calculating ",
        varName[1],
        " is ",
        endtime - starttime
      ))
      rm(calc1Val, calc2Val, calc5Odo)
      rm(list = paste0(varName[1]))
      # rm(list = paste0(varNameMile[1]))
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    }
    else if (gid[idx] == 264) {
      # 2.29 Riementrieb	2.29.1 Klimakompressorbetrieb im Leerlauf
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      starttime <- Sys.time()
      varName <- as.character(mainConfig[idx, 6])
      # varNameMile <- as.character(paste0(varName, "Mile"))
      assign(varName, NULL)
      # assign(varNameMile, NULL)
      calc1Val <- as.character(mainConfig[idx, 10])
      #calc2Val <- as.character(mainConfig[idx,11])
      calc4Val <- as.character(mainConfig[idx, 11])
      #calc5Odo <- as.character(mainConfig[idx, 14])
      #signal 1
      sig1idx <- which(channSigConfig[, 3] == calc1Val)
      sig1Val <- channSigConfig[sig1idx, 4]
      signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
      signal1Vec[signal1Vec == "NA"] <- NA
      signal1Vec <- signal1Vec[!is.na(signal1Vec)]
      assign(sig1Val, signal1Vec)
      # #Signal3 - Odo
      # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
      # sig3Val <- channSigConfig[sig3idx, 4]
      # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
      # signal3Vec[signal3Vec == "NA"] <- NA
      # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
      # assign(sig3Val, signal3Vec)
      #Signal 4
      sig4idx <- which(channSigConfig[, 3] == calc4Val)
      sig4Val <- channSigConfig[sig4idx, 4]
      signal4Vec <- as.character(channSigConfig[sig4idx, 8:25])
      signal4Vec[signal4Vec == "NA"] <- NA
      signal4Vec <- signal4Vec[!is.na(signal4Vec)]
      assign(sig4Val, signal4Vec)
      #signal threshold
      sig1min <- as.numeric(channSigConfig[sig1idx,5])
      sig1max <- as.numeric(channSigConfig[sig1idx,6])
      
      #sig3min <- as.numeric(channSigConfig[sig3idx,5])
      #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      sig4min <- as.numeric(channSigConfig[sig4idx,5])
      sig4max <- as.numeric(channSigConfig[sig4idx,6])
      
      calc1Val <- getChann1(get(sig1Val), X,sig1min,sig1max)
      #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      calc4Val <- getChann1(get(sig4Val), X,sig4min,sig4max)
      #Operating Time Calc
      isOpTime <- (!is.na(calc4Val) & calc4Val > 0)
      y <- (pi * calc4Val[isOpTime] * calc1Val[isOpTime]) / 3e4
      y[y > -Inf & y < 1] <- 0
      y[y > 1 & y < Inf] <- 1
      assign(x = varName[1],
             value = do.call("countTable2D", args = list(
               x = calc4Val[isOpTime],
               y = y,
               mergeTab = get(varName)
             )))
      # assign(varNameMile[1], c(max(
      #   0, max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
      # )))
      if (is.null(get(varName))) {
        f_idx <- which(mainConfig[, 1] == 264)
        failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
        failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
        if(length(colName1) == 0){
          failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
        }else if(length(colName2) == 0){
          failSlides[fail_index,4] <-as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
        }
        fail_index <- fail_index + 1
      } else{
        ##saveRDS in the dir location
        g_idx <- which(mainConfig[, 1] == 264)
        GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
        GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
        #summary of Signal 1
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName1 <- c(colName1,colName1)
        col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
        count1 <- sum(length(which(!is.na(X[,col1]))))
        perc1 <- round(count1/nrow(X)*100)
        GoodSlides[good_index,3] <- paste0(col1)
        GoodSlides[good_index,4] <- paste0(count1)
        GoodSlides[good_index,5] <- paste0(perc1,"%")
        
        #summary of Signal 3 
        # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
        # colName3 <- c(colName3,colName3)
        # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
        # count3 <- sum(length(which(!is.na(X[,col3]))))
        # perc3 <- round(count3/nrow(X)*100)
        # GoodSlides[good_index,9] <- paste0(col3)
        # GoodSlides[good_index,10] <- paste0(count3)
        # GoodSlides[good_index,11] <- paste0(perc3,"%")
        #summary of Signal 4 
        colName4 <- get(sig4Val)[get(sig4Val) %in% names(X)]
        colName4 <- c(colName4,colName4)
        col4 <- colName4[which.max(colSums(!is.na(X[,colName4])))]
        count4 <- sum(length(which(!is.na(X[,col4]))))
        perc4 <- round(count4/nrow(X)*100)
        GoodSlides[good_index,12] <- paste0(col4)
        GoodSlides[good_index,13] <- paste0(count4)
        GoodSlides[good_index,14] <- paste0(perc4,"%")
        
        good_index <- good_index + 1
        saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
        # saveRDS(get(varNameMile[1]), paste0(dir, varNameMile[1], ".rds"))
      }
      ##remove the variables to free up the memory
      endtime <- Sys.time()
      print(paste0(
        "total time for calculating ",
        varName[1],
        " is ",
        endtime - starttime
      ))
      rm(calc1Val, calc2Val, calc5Odo)
      rm(list = paste0(varName[1]))
      ## rm(list = paste0(varNameMile[1]))
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    }
    else if (gid[idx] == 265) {
      # 2.29 Riementrieb	2.29.2 Kennfeld Klimakompressorbetrieb im Leerlauf
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      starttime <- Sys.time()
      varName <- as.character(mainConfig[idx, 6])
      # varNameMile <- as.character(paste0(varName, "Mile"))
      assign(varName, NULL)
      # assign(varNameMile, NULL)
      calc1Val <- as.character(mainConfig[idx, 8])
      calc2Val <- as.character(mainConfig[idx, 10])
      
      #calc5Odo <- as.character(mainConfig[idx, 14])
      #signal 1
      sig1idx <- which(channSigConfig[, 3] == calc1Val)
      sig1Val <- channSigConfig[sig1idx, 4]
      signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
      signal1Vec[signal1Vec == "NA"] <- NA
      signal1Vec <- signal1Vec[!is.na(signal1Vec)]
      assign(sig1Val, signal1Vec)
      #signal 2
      sig2idx <- which(channSigConfig[, 3] == calc2Val)
      sig2Val <- channSigConfig[sig2idx, 4]
      signal2Vec <- as.character(channSigConfig[sig2idx, 8:25])
      signal2Vec[signal2Vec == "NA"] <- NA
      signal2Vec <- signal2Vec[!is.na(signal2Vec)]
      assign(sig2Val, signal2Vec)
      #Signal3 - Odo
      # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
      # sig3Val <- channSigConfig[sig3idx, 4]
      # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
      # signal3Vec[signal3Vec == "NA"] <- NA
      # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
      # assign(sig3Val, signal3Vec)
      #Signal 4
      
      #signal threshold
      sig1min <- as.numeric(channSigConfig[sig1idx,5])
      sig1max <- as.numeric(channSigConfig[sig1idx,6])
      sig2min <- as.numeric(channSigConfig[sig2idx,5])
      sig2max <- as.numeric(channSigConfig[sig2idx,6])
      #sig3min <- as.numeric(channSigConfig[sig3idx,5])
      #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      
      
      # XEngTor <- getChann(sigEngTor,X,1000)
      calc1Val <- getChann1(get(sig1Val), X,sig1min,sig1max)
      calc2Val <- getChann1(get(sig2Val), X,sig2min,sig2max)
      #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      
      #Operating Time Calc
      isOpTime <- (!is.na(calc1Val) & calc1Val > 0)
      y <- (pi * calc1Val * calc2Val) / 3e4
      x <- calc1Val[isOpTime & calc1Val > 450 & calc1Val < 950]
      y <- y[isOpTime & calc1Val > 450 & calc1Val < 950]
      x <- x[y > 1 & y < Inf]
      y <- y[y > 1 & y < Inf]
      assign(x = varName[1],
             value = do.call("countTable2D", args = list(
               x = x,
               y = y,
               mergeTab = get(varName)
             )))
      # assign(varNameMile[1], c(max(
      #   0, max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
      # )))
      if (is.null(get(varName))) {
        f_idx <- which(mainConfig[, 1] == 265)
        failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
        failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
        if(length(colName1) == 0){
          failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
        }else if(length(colName2) == 0){
          failSlides[fail_index,4] <-as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
        }
        fail_index <- fail_index + 1
      } else{
        ##saveRDS in the dir location
        g_idx <- which(mainConfig[, 1] == 265)
        GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
        GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
        #summary of Signal 1
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName1 <- c(colName1,colName1)
        col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
        count1 <- sum(length(which(!is.na(X[,col1]))))
        perc1 <- round(count1/nrow(X)*100)
        GoodSlides[good_index,3] <- paste0(col1)
        GoodSlides[good_index,4] <- paste0(count1)
        GoodSlides[good_index,5] <- paste0(perc1,"%")
        #summary of Signal 2 
        colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
        colName2 <- c(colName2,colName2)
        col2 <- colName2[which.max(colSums(!is.na(X[,colName2])))]
        count2 <- sum(length(which(!is.na(X[,col1]))))
        perc2 <- round(count2/nrow(X)*100)
        GoodSlides[good_index,6] <- paste0(col2)
        GoodSlides[good_index,7] <- paste0(count2)
        GoodSlides[good_index,8] <- paste0(perc2,"%")
        #summary of Signal 3 
        # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
        # colName3 <- c(colName3,colName3)
        # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
        # count3 <- sum(length(which(!is.na(X[,col3]))))
        # perc3 <- round(count3/nrow(X)*100)
        # GoodSlides[good_index,9] <- paste0(col3)
        # GoodSlides[good_index,10] <- paste0(count3)
        # GoodSlides[good_index,11] <- paste0(perc3,"%")
        
        GoodSlides[good_index,12] <- paste0(col4)
        GoodSlides[good_index,13] <- paste0(count4)
        GoodSlides[good_index,14] <- paste0(perc4,"%")
        
        good_index <- good_index + 1
        saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
        # saveRDS(get(varNameMile[1]), paste0(dir, varNameMile[1], ".rds"))
      }
      ##remove the variables to free up the memory
      endtime <- Sys.time()
      print(paste0(
        "total time for calculating ",
        varName[1],
        " is ",
        endtime - starttime
      ))
      rm(calc1Val, calc2Val, calc5Odo)
      rm(list = paste0(varName[1]))
      ## rm(list = paste0(varNameMile[1]))
      rm(x)
      rm(y)
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    }
    else if (gid[idx] == 266) {
      # 2.29 Riementrieb	2.29.3 Klimakompressorbetrieb
      starttime <- Sys.time()
      varName <- as.character(mainConfig[idx, 6])
      # varNameMile <- as.character(paste0(varName, "Mile"))
      assign(varName, NULL)
      # assign(varNameMile, NULL)
      calc1Val <- as.character(mainConfig[idx, 10])
      #calc2Val <- as.character(mainConfig[idx,11])
      calc4Val <- as.character(mainConfig[idx, 11])
      #calc5Odo <- as.character(mainConfig[idx, 14])
      #signal 1
      sig1idx <- which(channSigConfig[, 3] == calc1Val)
      sig1Val <- channSigConfig[sig1idx, 4]
      signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
      signal1Vec[signal1Vec == "NA"] <- NA
      signal1Vec <- signal1Vec[!is.na(signal1Vec)]
      assign(sig1Val, signal1Vec)
      #Signal3 - Odo
      # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
      # sig3Val <- channSigConfig[sig3idx, 4]
      # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
      # signal3Vec[signal3Vec == "NA"] <- NA
      # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
      # assign(sig3Val, signal3Vec)
      #Signal 4
      sig4idx <- which(channSigConfig[, 3] == calc4Val)
      sig4Val <- channSigConfig[sig4idx, 4]
      signal4Vec <- as.character(channSigConfig[sig4idx, 8:25])
      signal4Vec[signal4Vec == "NA"] <- NA
      signal4Vec <- signal4Vec[!is.na(signal4Vec)]
      assign(sig4Val, signal4Vec)
      #signal threshold
      sig1min <- as.numeric(channSigConfig[sig1idx,5])
      sig1max <- as.numeric(channSigConfig[sig1idx,6])
      
      #sig3min <- as.numeric(channSigConfig[sig3idx,5])
      #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      sig4min <- as.numeric(channSigConfig[sig4idx,5])
      sig4max <- as.numeric(channSigConfig[sig4idx,6])
      
      
      calc1Val <- getChann1(get(sig1Val), X,sig1min,sig1max)
      #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      calc4Val <- getChann1(get(sig4Val), X,sig4min,sig4max)
      #Operating Time Calc
      isOpTime <- (!is.na(calc4Val) & calc4Val > 0)
      y <- (pi * calc4Val[isOpTime] * calc1Val[isOpTime]) / 3e4
      y[y > -Inf & y < 1] <- 0
      y[y > 1 & y < Inf] <- 1
      assign(x = varName[1],
             value = do.call("countTable2D", args = list(
               x = calc4Val[isOpTime],
               y = y,
               mergeTab = get(varName)
             )))
      # assign(varNameMile[1], c(max(
      #   0, max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
      # )))
      if (is.null(get(varName))) {
        f_idx <- which(mainConfig[, 1] == 266)
        failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
        failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
        if(length(colName1) == 0){
          failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
        }else if(length(colName2) == 0){
          failSlides[fail_index,4] <-as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
        }
        fail_index <- fail_index + 1
      } else{
        ##saveRDS in the dir location
        g_idx <- which(mainConfig[, 1] == 266)
        GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
        GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
        #summary of Signal 1
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName1 <- c(colName1,colName1)
        col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
        count1 <- sum(length(which(!is.na(X[,col1]))))
        perc1 <- round(count1/nrow(X)*100)
        GoodSlides[good_index,3] <- paste0(col1)
        GoodSlides[good_index,4] <- paste0(count1)
        GoodSlides[good_index,5] <- paste0(perc1,"%")
        
        #summary of Signal 3 
        # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
        # colName3 <- c(colName3,colName3)
        # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
        # count3 <- sum(length(which(!is.na(X[,col3]))))
        # perc3 <- round(count3/nrow(X)*100)
        # GoodSlides[good_index,9] <- paste0(col3)
        # GoodSlides[good_index,10] <- paste0(count3)
        # GoodSlides[good_index,11] <- paste0(perc3,"%")
        #summary of Signal 4 
        colName4 <- get(sig4Val)[get(sig4Val) %in% names(X)]
        colName4 <- c(colName4,colName4)
        col4 <- colName4[which.max(colSums(!is.na(X[,colName4])))]
        count4 <- sum(length(which(!is.na(X[,col4]))))
        perc4 <- round(count4/nrow(X)*100)
        GoodSlides[good_index,12] <- paste0(col4)
        GoodSlides[good_index,13] <- paste0(count4)
        GoodSlides[good_index,14] <- paste0(perc4,"%")
        
        good_index <- good_index + 1
        saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
        # saveRDS(get(varNameMile[1]), paste0(dir, varNameMile[1], ".rds"))
      }
      ##remove the variables to free up the memory
      endtime <- Sys.time()
      print(paste0(
        "total time for calculating ",
        varName[1],
        " is ",
        endtime - starttime
      ))
      rm(calc1Val, calc2Val, calc5Odo)
      rm(list = paste0(varName[1]))
      # rm(list = paste0(varNameMile[1]))
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    }
    else if (gid[idx] == 267) {
      # 2.29 Riementrieb	2.29.4 Kennfeld Klimakompressorbetrieb
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      starttime <- Sys.time()
      varName <- as.character(mainConfig[idx, 6])
      # varNameMile <- as.character(paste0(varName, "Mile"))
      assign(varName, NULL)
      #assign(varNameMile, NULL)
      calc1Val <- as.character(mainConfig[idx, 8])
      calc2Val <- as.character(mainConfig[idx, 10])
      
      #calc5Odo <- as.character(mainConfig[idx, 14])
      #signal 1
      sig1idx <- which(channSigConfig[, 3] == calc1Val)
      sig1Val <- channSigConfig[sig1idx, 4]
      signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
      signal1Vec[signal1Vec == "NA"] <- NA
      signal1Vec <- signal1Vec[!is.na(signal1Vec)]
      assign(sig1Val, signal1Vec)
      #signal 2
      sig2idx <- which(channSigConfig[, 3] == calc2Val)
      sig2Val <- channSigConfig[sig2idx, 4]
      signal2Vec <- as.character(channSigConfig[sig2idx, 8:25])
      signal2Vec[signal2Vec == "NA"] <- NA
      signal2Vec <- signal2Vec[!is.na(signal2Vec)]
      assign(sig2Val, signal2Vec)
      #Signal3 - Odo
      # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
      # sig3Val <- channSigConfig[sig3idx, 4]
      # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
      # signal3Vec[signal3Vec == "NA"] <- NA
      # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
      # assign(sig3Val, signal3Vec)
      
      #signal threshold
      sig1min <- as.numeric(channSigConfig[sig1idx,5])
      sig1max <- as.numeric(channSigConfig[sig1idx,6])
      sig2min <- as.numeric(channSigConfig[sig2idx,5])
      sig2max <- as.numeric(channSigConfig[sig2idx,6])
      #sig3min <- as.numeric(channSigConfig[sig3idx,5])
      #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      
      
      calc1Val <- getChann1(get(sig1Val), X,sig1min,sig1max)
      calc2Val <- getChann1(get(sig2Val), X,sig2min,sig2max)
      #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      
      #Operating Time Calc
      isOpTime <- (!is.na(calc1Val) & calc1Val > 0)
      y <- (pi * calc1Val * calc2Val) / 3e4
      x <- calc1Val[isOpTime & calc1Val >= 0 & calc1Val <= 6000]
      y <- y[isOpTime & calc1Val >= 0 & calc1Val <= 6000]
      x <- x[y > 1 & y < Inf]
      y <- y[y > 1 & y < Inf]
      assign(x = varName[1],
             value = do.call("countTable2D", args = list(
               x = x,
               y = y,
               mergeTab = get(varName)
             )))
      # assign(varNameMile[1], c(max(
      #   0, max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
      # )))
      if (is.null(get(varName))) {
        f_idx <- which(mainConfig[, 1] == 267)
        failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
        failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
        if(length(colName1) == 0){
          failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
        }else if(length(colName2) == 0){
          failSlides[fail_index,4] <-as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
        }
        fail_index <- fail_index + 1
      } else{
        ##saveRDS in the dir location
        g_idx <- which(mainConfig[, 1] == 267)
        GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
        GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
        #summary of Signal 1
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName1 <- c(colName1,colName1)
        col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
        count1 <- sum(length(which(!is.na(X[,col1]))))
        perc1 <- round(count1/nrow(X)*100)
        GoodSlides[good_index,3] <- paste0(col1)
        GoodSlides[good_index,4] <- paste0(count1)
        GoodSlides[good_index,5] <- paste0(perc1,"%")
        #summary of Signal 2 
        colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
        colName2 <- c(colName2,colName2)
        col2 <- colName2[which.max(colSums(!is.na(X[,colName2])))]
        count2 <- sum(length(which(!is.na(X[,col1]))))
        perc2 <- round(count2/nrow(X)*100)
        GoodSlides[good_index,6] <- paste0(col2)
        GoodSlides[good_index,7] <- paste0(count2)
        GoodSlides[good_index,8] <- paste0(perc2,"%")
        #summary of Signal 3 
        # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
        # colName3 <- c(colName3,colName3)
        # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
        # count3 <- sum(length(which(!is.na(X[,col3]))))
        # perc3 <- round(count3/nrow(X)*100)
        # GoodSlides[good_index,9] <- paste0(col3)
        # GoodSlides[good_index,10] <- paste0(count3)
        # GoodSlides[good_index,11] <- paste0(perc3,"%")
        #summary of Signal 4 
        # colName4 <- get(sig4Val)[get(sig4Val) %in% names(X)]
        # colName4 <- c(colName4,colName4)
        # col4 <- colName4[which.max(colSums(!is.na(X[,colName4])))]
        # count4 <- sum(length(which(!is.na(X[,col4]))))
        # perc4 <- round(count4/nrow(X)*100)
        # GoodSlides[good_index,12] <- paste0(col4)
        # GoodSlides[good_index,13] <- paste0(count4)
        # GoodSlides[good_index,14] <- paste0(perc4,"%")
        
        good_index <- good_index + 1
        saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
        # saveRDS(get(varNameMile[1]), paste0(dir, varNameMile[1], ".rds"))
      }
      ##remove the variables to free up the memory
      endtime <- Sys.time()
      print(paste0(
        "total time for calculating ",
        varName[1],
        " is ",
        endtime - starttime
      ))
      rm(calc1Val, calc2Val, calc5Odo)
      rm(list = paste0(varName[1]))
      # rm(list = paste0(varNameMile[1]))
      rm(x)
      rm(y)
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    }
    else if (gid[idx] == 268) {
      print("this is not considered")
    }
    else if (gid[idx] == 269) {
      print("this is not considered")
    } 
    else if (gid[idx] == 270) {
      # 2.30 KurbelgehÃ¤useentlÃ¼ftung	2.30.1 Verweildauern KGH-EntlÃ¼ftungsdifferenzdruck
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      starttime <- Sys.time()
      varName <- as.character(mainConfig[idx, 6])
      # varNameMile <- as.character(paste0(varName, "Mile"))
      assign(varName, NULL)
      # assign(varNameMile, NULL)
      calc1Val <- as.character(mainConfig[idx, 10])
      calc2Val <- as.character(mainConfig[idx, 11])
      calc4Val <- as.character(mainConfig[idx, 13])
      #calc5Odo <- as.character(mainConfig[idx, 14])
      #signal 1
      sig1idx <- which(channSigConfig[, 3] == calc1Val)
      sig1Val <- channSigConfig[sig1idx, 4]
      signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
      signal1Vec[signal1Vec == "NA"] <- NA
      signal1Vec <- signal1Vec[!is.na(signal1Vec)]
      assign(sig1Val, signal1Vec)
      #signal 2
      sig2idx <- which(channSigConfig[, 3] == calc2Val)
      sig2Val <- channSigConfig[sig2idx, 4]
      signal2Vec <- as.character(channSigConfig[sig2idx, 8:25])
      signal2Vec[signal2Vec == "NA"] <- NA
      signal2Vec <- signal2Vec[!is.na(signal2Vec)]
      assign(sig2Val, signal2Vec)
      # #Signal3 - Odo
      # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
      # sig3Val <- channSigConfig[sig3idx, 4]
      # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
      # signal3Vec[signal3Vec == "NA"] <- NA
      # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
      # assign(sig3Val, signal3Vec)
      #Signal 4
      sig4idx <- which(channSigConfig[, 3] == calc4Val)
      sig4Val <- channSigConfig[sig4idx, 4]
      signal4Vec <- as.character(channSigConfig[sig4idx, 8:25])
      signal4Vec[signal4Vec == "NA"] <- NA
      signal4Vec <- signal4Vec[!is.na(signal4Vec)]
      assign(sig4Val, signal4Vec)
      #signal threshold
      sig1min <- as.numeric(channSigConfig[sig1idx,5])
      sig1max <- as.numeric(channSigConfig[sig1idx,6])
      sig2min <- as.numeric(channSigConfig[sig2idx,5])
      sig2max <- as.numeric(channSigConfig[sig2idx,6])
      #sig3min <- as.numeric(channSigConfig[sig3idx,5])
      #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      sig4min <- as.numeric(channSigConfig[sig4idx,5])
      sig4max <- as.numeric(channSigConfig[sig4idx,6])
      # XEngTor <- getChann(sigEngTor,X,1000)
      calc1Val <- getChann1(get(sig1Val), X,sig1min,sig1max)
      calc2Val <- getChann1(get(sig2Val), X,sig2min,sig2max)
      #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      calc4Val <- getChann1(get(sig4Val), X,sig4min,sig4max)
      #Operating Time Calc
      assign(x = varName[1],
             value = do.call(
               "countTable1D",
               args = list(x = (calc1Val[calc4Val > 0] - calc2Val[calc4Val > 0]), mergeTab =
                             get(varName))
             ))
      # assign(varNameMile[1], c(max(
      #   0, max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
      # )))
      if (is.null(get(varName))) {
        f_idx <- which(mainConfig[, 1] == 270)
        failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
        failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
        if(length(colName1) == 0){
          failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
        }else if(length(colName2) == 0){
          failSlides[fail_index,4] <-as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
        }
        fail_index <- fail_index + 1
      } else{
        ##saveRDS in the dir location
        g_idx <- which(mainConfig[, 1] == 270)
        GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
        GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
        #summary of Signal 1
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName1 <- c(colName1,colName1)
        col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
        count1 <- sum(length(which(!is.na(X[,col1]))))
        perc1 <- round(count1/nrow(X)*100)
        GoodSlides[good_index,3] <- paste0(col1)
        GoodSlides[good_index,4] <- paste0(count1)
        GoodSlides[good_index,5] <- paste0(perc1,"%")
        #summary of Signal 2 
        colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
        colName2 <- c(colName2,colName2)
        col2 <- colName2[which.max(colSums(!is.na(X[,colName2])))]
        count2 <- sum(length(which(!is.na(X[,col1]))))
        perc2 <- round(count2/nrow(X)*100)
        GoodSlides[good_index,6] <- paste0(col2)
        GoodSlides[good_index,7] <- paste0(count2)
        GoodSlides[good_index,8] <- paste0(perc2,"%")
        #summary of Signal 3 
        # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
        # colName3 <- c(colName3,colName3)
        # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
        # count3 <- sum(length(which(!is.na(X[,col3]))))
        # perc3 <- round(count3/nrow(X)*100)
        # GoodSlides[good_index,9] <- paste0(col3)
        # GoodSlides[good_index,10] <- paste0(count3)
        # GoodSlides[good_index,11] <- paste0(perc3,"%")
        #summary of Signal 4 
        colName4 <- get(sig4Val)[get(sig4Val) %in% names(X)]
        colName4 <- c(colName4,colName4)
        col4 <- colName4[which.max(colSums(!is.na(X[,colName4])))]
        count4 <- sum(length(which(!is.na(X[,col4]))))
        perc4 <- round(count4/nrow(X)*100)
        GoodSlides[good_index,12] <- paste0(col4)
        GoodSlides[good_index,13] <- paste0(count4)
        GoodSlides[good_index,14] <- paste0(perc4,"%")
        
        saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
        # saveRDS(get(varNameMile[1]), paste0(dir, varNameMile[1], ".rds"))
      }
      ##remove the variables to free up the memory
      endtime <- Sys.time()
      print(paste0(
        "total time for calculating ",
        varName[1],
        " is ",
        endtime - starttime
      ))
      rm(calc1Val, calc2Val, calc5Odo)
      rm(list = paste0(varName[1]))
      # rm(list = paste0(varNameMile[1]))
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    }
    else if (gid[idx] == 271) {
      # 2.30 KurbelgehÃ¤useentlÃ¼ftung	2.30.2 Drehmoment- KGH-EntlÃ¼ftungsdifferenzdruck -Kennfeld
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      starttime <- Sys.time()
      varName <- as.character(mainConfig[idx, 6])
      # varNameMile <- as.character(paste0(varName, "Mile"))
      assign(varName, NULL)
      #assign(varNameMile, NULL)
      calc1Val <- as.character(mainConfig[idx, 8])
      calc2Val <- as.character(mainConfig[idx, 10])
      calc3Val <- as.character(mainConfig[idx, 11])
      calc4Val <- as.character(mainConfig[idx, 13])
      #calc5Odo <- as.character(mainConfig[idx, 14])
      #signal 1
      sig1idx <- which(channSigConfig[, 3] == calc1Val)
      sig1Val <- channSigConfig[sig1idx, 4]
      signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
      signal1Vec[signal1Vec == "NA"] <- NA
      signal1Vec <- signal1Vec[!is.na(signal1Vec)]
      assign(sig1Val, signal1Vec)
      #signal 2
      sig2idx <- which(channSigConfig[, 3] == calc2Val)
      sig2Val <- channSigConfig[sig2idx, 4]
      signal2Vec <- as.character(channSigConfig[sig2idx, 8:25])
      signal2Vec[signal2Vec == "NA"] <- NA
      signal2Vec <- signal2Vec[!is.na(signal2Vec)]
      assign(sig2Val, signal2Vec)
      #signal 3
      sig5idx <- which(channSigConfig[, 3] == calc3Val)
      sig5Val <- channSigConfig[sig5idx, 4]
      signal5Vec <- as.character(channSigConfig[sig5idx, 8:25])
      signal5Vec[signal5Vec == "NA"] <- NA
      signal5Vec <- signal5Vec[!is.na(signal5Vec)]
      assign(sig5Val, signal5Vec)
      #Signal5 - Odo
      # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
      # sig3Val <- channSigConfig[sig3idx, 4]
      # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
      # signal3Vec[signal3Vec == "NA"] <- NA
      # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
      # assign(sig3Val, signal3Vec)
      #Signal 4
      sig4idx <- which(channSigConfig[, 3] == calc4Val)
      sig4Val <- channSigConfig[sig4idx, 4]
      signal4Vec <- as.character(channSigConfig[sig4idx, 8:25])
      signal4Vec[signal4Vec == "NA"] <- NA
      signal4Vec <- signal4Vec[!is.na(signal4Vec)]
      assign(sig4Val, signal4Vec)
      #signal threshold
      sig1min <- as.numeric(channSigConfig[sig1idx,5])
      sig1max <- as.numeric(channSigConfig[sig1idx,6])
      sig2min <- as.numeric(channSigConfig[sig2idx,5])
      sig2max <- as.numeric(channSigConfig[sig2idx,6])
      #sig3min <- as.numeric(channSigConfig[sig3idx,5])
      #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      sig4min <- as.numeric(channSigConfig[sig4idx,5])
      sig4max <- as.numeric(channSigConfig[sig4idx,6])
      sig5min <- as.numeric(channSigConfig[sig5idx,5])
      sig5max <- as.numeric(channSigConfig[sig5idx,6])
      # XEngTor <- getChann(sigEngTor,X,1000)
      calc1Val <- getChann1(get(sig1Val), X,sig1min,sig1max)
      calc2Val <- getChann1(get(sig2Val), X,sig2min,sig2max)
      calc3Val <- getChann1(get(sig5Val), X,sig5min,sig5max)
      #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      calc4Val <- getChann1(get(sig4Val), X,sig4min,sig4max)
      #Operating Time Calc
      isOpTime <- (!is.na(calc4Val) & calc4Val > 0)
      assign(x = varName[1],
             value = do.call("countTable2D", args = list(
               x = calc1Val[isOpTime],
               y = (calc2Val[isOpTime] - calc3Val[isOpTime]),
               mergeTab = get(varName)
             )))
      # assign(varNameMile[1], c(max(
      #   0, max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
      # )))
      if (is.null(get(varName))) {
        f_idx <- which(mainConfig[, 1] == 271)
        failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
        failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
        if(length(colName1) == 0){
          failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
        }else if(length(colName2) == 0){
          failSlides[fail_index,4] <-as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
        }
        fail_index <- fail_index + 1
      } else{
        ##saveRDS in the dir location
        g_idx <- which(mainConfig[, 1] == 271)
        GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
        GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
        #summary of Signal 1
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName1 <- c(colName1,colName1)
        col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
        count1 <- sum(length(which(!is.na(X[,col1]))))
        perc1 <- round(count1/nrow(X)*100)
        GoodSlides[good_index,3] <- paste0(col1)
        GoodSlides[good_index,4] <- paste0(count1)
        GoodSlides[good_index,5] <- paste0(perc1,"%")
        #summary of Signal 2 
        colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
        colName2 <- c(colName2,colName2)
        col2 <- colName2[which.max(colSums(!is.na(X[,colName2])))]
        count2 <- sum(length(which(!is.na(X[,col1]))))
        perc2 <- round(count2/nrow(X)*100)
        GoodSlides[good_index,6] <- paste0(col2)
        GoodSlides[good_index,7] <- paste0(count2)
        GoodSlides[good_index,8] <- paste0(perc2,"%")
        #summary of Signal 3 
        # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
        # colName3 <- c(colName3,colName3)
        # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
        # count3 <- sum(length(which(!is.na(X[,col3]))))
        # perc3 <- round(count3/nrow(X)*100)
        # GoodSlides[good_index,9] <- paste0(col3)
        # GoodSlides[good_index,10] <- paste0(count3)
        # GoodSlides[good_index,11] <- paste0(perc3,"%")
        #summary of Signal 4 
        colName4 <- get(sig4Val)[get(sig4Val) %in% names(X)]
        colName4 <- c(colName4,colName4)
        col4 <- colName4[which.max(colSums(!is.na(X[,colName4])))]
        count4 <- sum(length(which(!is.na(X[,col4]))))
        perc4 <- round(count4/nrow(X)*100)
        GoodSlides[good_index,12] <- paste0(col4)
        GoodSlides[good_index,13] <- paste0(count4)
        GoodSlides[good_index,14] <- paste0(perc4,"%")
        #summary of Signal 5 
        colName5 <- get(sig5Val)[get(sig5Val) %in% names(X)]
        colName5 <- c(colName5,colName5)
        col5 <- colName5[which.max(colSums(!is.na(X[,colName5])))]
        count5 <- sum(length(which(!is.na(X[,col5]))))
        perc5 <- round(count5/nrow(X)*100)
        GoodSlides[good_index,15] <- paste0(col5)
        GoodSlides[good_index,16] <- paste0(count5)
        GoodSlides[good_index,17] <- paste0(perc5,"%")
        good_index <- good_index + 1
        saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
        # saveRDS(get(varNameMile[1]), paste0(dir, varNameMile[1], ".rds"))
      }
      ##remove the variables to free up the memory
      endtime <- Sys.time()
      print(paste0(
        "total time for calculating ",
        varName[1],
        " is ",
        endtime - starttime
      ))
      rm(calc1Val, calc2Val, calc5Odo)
      rm(list = paste0(varName[1]))
      # rm(list = paste0(varNameMile[1]))
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    }
    else if (gid[idx] == 272) {
      # 2.30 KurbelgehÃ¤useentlÃ¼ftung	2.30.3 Drehzahl- KGH-EntlÃ¼ftungsdifferenzdruck -Kennfeld
      print(paste0("-------------",mainConfig[idx,1],"Started","-------------------"))
      starttime <- Sys.time()
      varName <- as.character(mainConfig[idx, 6])
      # varNameMile <- as.character(paste0(varName, "Mile"))
      assign(varName, NULL)
      # assign(varNameMile, NULL)
      calc1Val <- as.character(mainConfig[idx, 8])
      calc2Val <- as.character(mainConfig[idx, 10])
      calc3Val <- as.character(mainConfig[idx, 11])
      #calc5Odo <- as.character(mainConfig[idx, 14])
      #signal 1
      sig1idx <- which(channSigConfig[, 3] == calc1Val)
      sig1Val <- channSigConfig[sig1idx, 4]
      signal1Vec <- as.character(channSigConfig[sig1idx, 8:25])
      signal1Vec[signal1Vec == "NA"] <- NA
      signal1Vec <- signal1Vec[!is.na(signal1Vec)]
      assign(sig1Val, signal1Vec)
      #signal 2
      sig2idx <- which(channSigConfig[, 3] == calc2Val)
      sig2Val <- channSigConfig[sig2idx, 4]
      signal2Vec <- as.character(channSigConfig[sig2idx, 8:25])
      signal2Vec[signal2Vec == "NA"] <- NA
      signal2Vec <- signal2Vec[!is.na(signal2Vec)]
      assign(sig2Val, signal2Vec)
      #signal 3
      sig5idx <- which(channSigConfig[, 3] == calc3Val)
      sig5Val <- channSigConfig[sig5idx, 4]
      signal5Vec <- as.character(channSigConfig[sig5idx, 8:25])
      signal5Vec[signal5Vec == "NA"] <- NA
      signal5Vec <- signal5Vec[!is.na(signal5Vec)]
      assign(sig5Val, signal5Vec)
      #Signal5 - Odo
      # sig3idx <- which(channSigConfig[, 3] == calc5Odo)
      # sig3Val <- channSigConfig[sig3idx, 4]
      # signal3Vec <- as.character(channSigConfig[sig3idx, 8:25])
      # signal3Vec[signal3Vec == "NA"] <- NA
      # signal3Vec <- signal3Vec[!is.na(signal3Vec)]
      # assign(sig3Val, signal3Vec)
      #signal threshold
      sig1min <- as.numeric(channSigConfig[sig1idx,5])
      sig1max <- as.numeric(channSigConfig[sig1idx,6])
      sig2min <- as.numeric(channSigConfig[sig2idx,5])
      sig2max <- as.numeric(channSigConfig[sig2idx,6])
      #sig3min <- as.numeric(channSigConfig[sig3idx,5])
      #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      sig5min <- as.numeric(channSigConfig[sig5idx,5])
      sig5max <- as.numeric(channSigConfig[sig5idx,6])
      #Signal 4
      # XEngTor <- getChann(sigEngTor,X,1000)
      calc1Val <- getChann1(get(sig1Val), X,sig1min,sig1max)
      calc2Val <- getChann1(get(sig2Val), X,sig2min,sig2max)
      calc3Val <- getChann1(get(sig5Val), X,sig5min,sig5max)
      #sig3max <- as.numeric(channSigConfig[sig3idx,6])
      
      #Operating Time Calc
      isOpTime <- (!is.na(calc1Val) & calc1Val > 0)
      assign(x = varName[1],
             value = do.call("countTable2D", args = list(
               x = calc1Val[isOpTime],
               y = (calc2Val[isOpTime] - calc3Val[isOpTime]),
               mergeTab = get(varName)
             )))
      # assign(varNameMile[1], c(max(
      #   0, max(calc5Odo, na.rm = TRUE) - min(calc5Odo, na.rm = TRUE)
      # )))
      if (is.null(get(varName))) {
        f_idx <- which(mainConfig[, 1] == 271)
        failSlides[fail_index, 1] <- mainConfig[f_idx, 2]
        failSlides[fail_index, 2] <- mainConfig[f_idx, 3]
        if(length(colName1) == 0){
          failSlides[fail_index,3] <- as.character(paste0("None of the Signals ", paste(get(sig1Val),collapse = ';')," not present."))
        }else if(length(colName2) == 0){
          failSlides[fail_index,4] <-as.character(paste0("None of the Signals ", paste(get(sig2Val),collapse = ';')," not present."))
        }
        fail_index <- fail_index + 1
      } else{
        ##saveRDS in the dir location
        g_idx <- which(mainConfig[, 1] == 271)
        GoodSlides[good_index, 1] <- mainConfig[g_idx, 2]
        GoodSlides[good_index, 2] <- mainConfig[g_idx, 3]
        #summary of Signal 1
        colName1 <- get(sig1Val)[get(sig1Val) %in% names(X)]
        colName1 <- c(colName1,colName1)
        col1 <- colName1[which.max(colSums(!is.na(X[,colName1])))]
        count1 <- sum(length(which(!is.na(X[,col1]))))
        perc1 <- round(count1/nrow(X)*100)
        GoodSlides[good_index,3] <- paste0(col1)
        GoodSlides[good_index,4] <- paste0(count1)
        GoodSlides[good_index,5] <- paste0(perc1,"%")
        #summary of Signal 2 
        colName2 <- get(sig2Val)[get(sig2Val) %in% names(X)]
        colName2 <- c(colName2,colName2)
        col2 <- colName2[which.max(colSums(!is.na(X[,colName2])))]
        count2 <- sum(length(which(!is.na(X[,col1]))))
        perc2 <- round(count2/nrow(X)*100)
        GoodSlides[good_index,6] <- paste0(col2)
        GoodSlides[good_index,7] <- paste0(count2)
        GoodSlides[good_index,8] <- paste0(perc2,"%")
        #summary of Signal 3 
        # colName3 <- get(sig3Val)[get(sig3Val) %in% names(X)]
        # colName3 <- c(colName3,colName3)
        # col3 <- colName3[which.max(colSums(!is.na(X[,colName3])))]
        # count3 <- sum(length(which(!is.na(X[,col3]))))
        # perc3 <- round(count3/nrow(X)*100)
        # GoodSlides[good_index,9] <- paste0(col3)
        # GoodSlides[good_index,10] <- paste0(count3)
        # GoodSlides[good_index,11] <- paste0(perc3,"%")
        
        #summary of Signal 5 
        colName5 <- get(sig5Val)[get(sig5Val) %in% names(X)]
        colName5 <- c(colName5,colName5)
        col5 <- colName5[which.max(colSums(!is.na(X[,colName5])))]
        count5 <- sum(length(which(!is.na(X[,col5]))))
        perc5 <- round(count5/nrow(X)*100)
        GoodSlides[good_index,15] <- paste0(col5)
        GoodSlides[good_index,16] <- paste0(count5)
        GoodSlides[good_index,17] <- paste0(perc5,"%")
        good_index <- good_index + 1
        saveRDS(get(varName[1]), paste0(dir, varName[1], ".rds"))
        # saveRDS(get(varNameMile[1]), paste0(dir, varNameMile[1], ".rds"))
      }
      ##remove the variables to free up the memory
      endtime <- Sys.time()
      print(paste0(
        "total time for calculating ",
        varName[1],
        " is ",
        endtime - starttime
      ))
      rm(calc1Val, calc2Val, calc5Odo)
      rm(list = paste0(varName[1]))
      # rm(list = paste0(varNameMile[1]))
      print(paste0("-------------",mainConfig[idx,1],"Ended","-------------------"))
    }
  }
  ##re-calculating the fail slide reasons.
  #getting the required reasons variables.
  less2PercSignals <- dataQualityDF1$signalName[dataQualityDF1$lessthn2Perc == 0.0]
  filteredSignals <- dataQualityDF1$signalName[dataQualityDF1$remainingRowCount == 0.0]
  incorrectSignal <- dataQualityDF1$signalName[dataQualityDF1$filterRowCount == 0.0]
  for(fa_idx in 1:nrow(failSlides)){
    if(length(unique(grep(paste(less2PercSignals,collapse="|"),failSlides[fa_idx,3], value=TRUE))) !=0){
      failSlides[fa_idx,4] <- as.character(paste0("less than 2 perc data removed"))
    }else if(length(unique(grep(paste(filteredSignals,collapse="|"),failSlides[fa_idx,3], value=TRUE))) != 0){
      failSlides[fa_idx,4] <- as.character(paste0("removed from limits conditions"))
    }else if(length(unique(grep(paste(incorrectSignal,collapse="|"),failSlides[fa_idx,3], value=TRUE))) != 0){
      failSlides[fa_idx,4] <- as.character(paste0("signal data was incorrect"))
    }else{
      failSlides[fa_idx,4] <- as.character(paste0("No secondary reason available"))
    }
  }
  ##end
  ## failSlides column Names
  colnames(failSlides) <- c("Title","subTitle","Primary Reason","Secondary Reason","others1","Others2")
  ##
  saveRDS(failSlides, paste0(dir, "failslide.rds"))
  write.xlsx(failSlides,paste0(LogDir, "failslide.xlsx"))
  colnames(GoodSlides) <- c("Title","Subtitle","Param1","Param1_Count","Param1_Perc","Param2","Param2_Count","Param2_Perc","Param3","Param3_Count","Param3_Perc","Param4","Param4_Count","Param4_Perc","Param5","Param5_Count","Param5_Perc")
  saveRDS(GoodSlides, paste0(dir, "GoodSlideFrame.rds"))
  write.xlsx(GoodSlides,paste0(LogDir, "GoodSlideFrame.xslx"))
  saveRDS(object=mainConfig[,17:22],file=paste0(dir,"breaksDef.rds"))
  saveRDS(object=allColNames,file=paste0(dir,"colNames.rds"))
  
  rm(X)
  caclCountEndTime <- Sys.time()
  print(paste0("Total Time taken for Running Calc Count ",(caclCountEndTime - caclCountStartTime)))
}

createBreaks <- function(x,binWidth,low=NULL,up=NULL,zeroInBreaks=TRUE){
  if(is.null(low)){
    low <- min(0,x,na.rm=TRUE)
  }
  if(is.null(up)){
    up <- max(x,na.rm=TRUE)
  }
  breaksTemp <- seq(low,up,binWidth)
  if(zeroInBreaks){
    breaks <- breaksTemp-breaksTemp[which.min(abs(breaksTemp))]
  }else{
    breaks <- breaksTemp
  }
  if(is.null(up)){
    while(max(x,na.rm=TRUE)>=max(breaks)){
      breaks <- c(breaks,max(breaks)+binWidth)
    }
  }
  if(is.null(low)){
    while(any(min(x,na.rm=TRUE)<=min(breaks))){
      breaks <- c(min(breaks)-binWidth,breaks)
    }
  }
  return(breaks)
}

countTable1D <- function(x,mergeTab,roundDigit=2){
  if(!is.null(x) && length(x)>0 && any(!is.na(x))){
    y <- round(x,roundDigit)
    A <- table(y)
    allNames <- sort(unique(as.numeric(c(names(A),names(mergeTab)))))
    B <- rbind(table(allNames),table(allNames))-1
    B[1,names(mergeTab)] <- mergeTab
    B[2,names(A)] <- A
    rValue <- colSums(B)
  }else{
    rValue <- mergeTab
  }
  return(rValue)
}

countDPFRegFunction<-function(x,breaksX,breaksY,roundDigit=2){
  library(accelerometry)
  modeDur<-NULL
  isMode<-NULL
  y<- NULL
  if(is.null(breaksY))
  {
    breaksY<-breaksX
  }
  if(length(x)>0)
  {
    isMode<- (x>=breaksX[1] & x<=breaksX[2] & !is.na(x))
    modeSeq <- rle2(as.numeric(x),return.list=TRUE)
    if(any(modeSeq$values==1)){
      modeDur <- modeSeq$lengths[modeSeq$values==1]
    }
  }
  if(length(modeDur)>0)
  {
    y<-modeDur/60
    y<-round(y,roundDigit)
  }
  return(y)
}

countTable1D1 <- function(x,mergeTab,roundDigit=2){
  if(!is.null(x) && length(x)>0 && any(!is.na(x))){
    y <- round(x,roundDigit)
    A <- table(y)
    A  <-  c(A,structure(c(0,0,0,0,0), names=c(3,5,6,7,15)))
    allNames <- sort(unique(as.numeric(c(names(A),names(mergeTab)))))
    B <- rbind(table(allNames),table(allNames))-1
    B[1,names(mergeTab)] <- mergeTab
    B[2,names(A)] <- A
    rValue <- colSums(B)
  }else{
    rValue <- mergeTab
  }
  return(rValue)
}

countTable2D <- function(x,y,mergeTab,roundDigitX=2,roundDigitY=2){
  if(!is.null(x) && !is.null(y) && length(x)>0 && length(y)>0 && any(!is.na(x)) && any(!is.na(y))){
    A <- table(round(y,roundDigitY),round(x,roundDigitX))
    allNamesX <- sort(unique(as.numeric(c(colnames(A),colnames(mergeTab)))))
    allNamesY <- sort(unique(as.numeric(c(rownames(A),rownames(mergeTab)))))
    B <- matrix(0,nrow=length(allNamesY),ncol=length(allNamesX),dimnames=list(allNamesY,allNamesX))
    C <- B
    B[rownames(A),colnames(A)] <- A
    C[rownames(mergeTab),colnames(mergeTab)] <- mergeTab
    rValue <- B+C
  }else{
    rValue <- mergeTab
  }
  return(rValue)
}

countFreq1D <- function(x,unitFactor=1,breaks=NULL,low=NULL,up=NULL,binWidth=NULL,zeroInBreaks=TRUE){       ##doubts
  if(is.null(breaks)){
    breaks <- createBreaks(x,binWidth,low,up,zeroInBreaks)
  }
  if(length(x)>0){
    y <- x[x>=min(breaks) & x<=max(breaks) & !is.na(x)]
  }else{
    y <- NULL
  }
  if(length(y)>0){
    count <- hist(y,breaks=breaks,plot=FALSE,right=FALSE,include.lowest=TRUE)$counts*unitFactor
  }else{
    count <- vector("numeric",length(breaks)-1)
  }
  rValue <- list(count=count,breaks=breaks)
  return(rValue)
}

countFreq2D <- function(x,y,unitFactor=1,breaksX=NULL,breaksY=NULL,lowX=NULL,lowY=NULL,upX=NULL,upY=NULL,binWidthX=NULL,binWidthY=NULL,zeroInBreaks=TRUE){   ##problem
  if(is.null(breaksX)){
    breaksX <- createBreaks(x,binWidthX,lowX,upX,zeroInBreaks)
  }
  if(is.null(breaksY)){
    breaksY <- createBreaks(y,binWidthY,lowY,upY,zeroInBreaks)
  }
  breaksX2 <- breaksX
  breaksX2[which.max(breaksX2)] <- breaksX2[which.max(breaksX2)]+1e-6
  breaksY2 <- breaksY
  breaksY2[which.max(breaksY2)] <- breaksY2[which.max(breaksY2)]+1e-6
  
  if(is.null(x)){
    x <- NA*vector("numeric",length(y))
  }
  if(is.null(y)){
    y <- NA*vector("numeric",length(x))
  }
  
  numX <- length(breaksX2)-1
  numY <- length(breaksY2)-1
  numXYMax <- max(numX,numY)
  h <- table(c(findInterval(y,breaksY2),1:numXYMax),c(findInterval(x,breaksX2),1:numXYMax))
  k <- as.matrix(h[which(rownames(h)=="1"):which(rownames(h)==sprintf("%d",numXYMax)),which(colnames(h)=="1"):which(colnames(h)==sprintf("%d",numXYMax))])
  diag(k) <- diag(k)-1
  count <- k[1:numY,1:numX]*unitFactor
  rValue <- list(count=count,breaksX=breaksX,breaksY=breaksY)
  return(rValue)
}

createStackTableDF<-function(x,breaks=NULL){
  splitStr<-""
  #classNames<-NULL
  splitClassNames<-FALSE
  unit<-NULL
  y<-NULL
  absTxt<-FALSE
  txtForm<-rep("%.0f",length(breaks))
  
  txtForm[!is.infinite(breaks) & abs(breaks-round(breaks,1))>1e-5] <- "%.2f"
  classNames <- vector("character",length(breaks)-1)
  for(idx in 1:(length(breaks)-1)){
    if(!is.infinite(breaks[idx]) & !is.infinite(breaks[idx+1])){
      classNames[idx] <- paste0(sprintf(txtForm[idx],breaks[idx]),"-",splitStr,sprintf(txtForm[idx+1],breaks[idx+1]))
    }else{
      if(is.infinite(breaks[idx])){
        classNames[idx] <- paste0("<",splitStr,sprintf(txtForm[idx+1],breaks[idx+1]))
      }else{
        classNames[idx] <- paste0("\u2265",splitStr,sprintf(txtForm[idx],breaks[idx]))
      }
    }
  }
  a_rpm<-colnames(x)
  y_OFF<-x[1,]
  y_ON<-x[2,]
  y_ONCla<-vector("numeric",length(breaks)-1)
  y_OFFCla<-vector("numeric",length(breaks)-1)
  y_count<-vector("numeric",length(breaks)-1)
  y_ON_count<-NULL
  y_OFF_count<-NULL
  y_ClaTotal<-NULL
  unitFactor<-1/3600
  for(idx in 1:length(breaks)-1)
  {
    y_ONCla[idx]<-(sum(y_ON[a_rpm>=breaks[idx]&a_rpm<breaks[idx+1]])/sum(x[1,],x[2,]))*100
    y_ON_count[idx]<-sum(y_ON[a_rpm>=breaks[idx]&a_rpm<breaks[idx+1]])/3600
    y_OFFCla[idx]<-(sum(y_OFF[a_rpm>=breaks[idx]&a_rpm<breaks[idx+1]])/sum(x[1,],x[2,]))*100
    y_OFF_count[idx]<-sum(y_OFF[a_rpm>=breaks[idx]&a_rpm<breaks[idx+1]])/3600
    y_count[idx]<-sum(y_ON[a_rpm>=breaks[idx]&a_rpm<breaks[idx+1]],y_OFF[a_rpm>=breaks[idx]&a_rpm<breaks[idx+1]])*unitFactor
    y_ClaTotal[idx]<-sum(y_ON_count[idx],y_OFF_count[idx])
    
  }
  df_wc<-data.frame(classNames,y_count,y_ONCla,y_OFFCla,y_ON_count,y_OFF_count)
  df_wc1<-data.frame(classNames,y_ON_count,y_OFF_count,y_ClaTotal)
  colnames(df_wc1)<-c("Classes","ON","OFF","Total")
  return(df_wc1)
}

createTableDF<-function(x,breaks=NULL,classNames=NULL){
  splitStr<-""
  #classNames<-NULL
  splitClassNames<-FALSE
  unit<-NULL
  y<-NULL
  absTxt<-FALSE
  if(!is.null(breaks)){
    xCla <- vector("numeric",length(breaks)-1)
    for(idx in 1:(length(breaks)-1)){
      xCla[idx] <- sum(x[as.numeric(names(x))>=breaks[idx] & as.numeric(names(x))<breaks[idx+1]])
      print(xCla[idx])
    }
    if(!is.null(y)){
      yCla <- vector("numeric",length(breaks)-1)
      for(idx in 1:(length(breaks)-1)){
        yCla[idx] <- sum(y[as.numeric(names(y))>=breaks[idx] & as.numeric(names(y))<breaks[idx+1]])
      }
    }else{
      yCla <- NULL
    }
  }else{
    xCla <- x
    yCla <- y
  }
  
  z <- c(xCla,yCla)
  if(is.null(classNames)){
    if(splitClassNames){
      splitStr <- "\n"
    }else{
      splitStr <- ""
    }
    txtForm <- rep("%.0f",length(breaks))
    idxRatNum <- which(abs(breaks-round(breaks))>1e-5)
    if(length(idxRatNum)<=1){
      txtForm[idxRatNum] <- "%.1f"
    }else{
      txtForm <- rep("%.1f",length(breaks))
    }
    txtForm[!is.infinite(breaks) & abs(breaks-round(breaks,1))>1e-5] <- "%.2f"
    classNames <- vector("character",length(breaks)-1)
    for(idx in 1:(length(breaks)-1)){
      if(!is.infinite(breaks[idx]) & !is.infinite(breaks[idx+1])){
        classNames[idx] <- paste0(sprintf(txtForm[idx],breaks[idx]),"-",splitStr,sprintf(txtForm[idx+1],breaks[idx+1]))
      }else{
        if(is.infinite(breaks[idx])){
          classNames[idx] <- paste0("<",splitStr,sprintf(txtForm[idx+1],breaks[idx+1]))
        }else{
          classNames[idx] <- paste0("\u2265",splitStr,sprintf(txtForm[idx],breaks[idx]))
        }
      }
    }
  }
  
  xTxtForm <- paste0("%.",as.numeric(xCla>0),"f")
  yTxtForm <- paste0("%.",as.numeric(yCla>0),"f")
  
  if(!is.null(unit)){
    if(unit=="%"){
      unit <- unit
    }else{
      unit <- paste0(" ",unit)
    }
  }else{
    if(absTxt){
      unit <- ""
    }else{
      unit <- "%"
    }
  }
  
  fac <- 100/sum(z)
  if(absTxt){
    fac <- 1
  }
  
  df<-data.frame(factor(classNames,classNames),round(xCla,2),round(xCla/sum(xCla)*100,2))
  colnames(df)<-c("Classes","Hr","%")
  return(df)
}

createTableDFCounts<-function(x,breaks=NULL,classNames=NULL){
  splitStr<-""
  #classNames<-NULL
  splitClassNames<-FALSE
  unit<-NULL
  y<-NULL
  absTxt<-FALSE
  if(!is.null(breaks)){
    xCla <- vector("numeric",length(breaks)-1)
    for(idx in 1:(length(breaks)-1)){
      xCla[idx] <- sum(x[as.numeric(names(x))>=breaks[idx] & as.numeric(names(x))<breaks[idx+1]])
      print(xCla[idx])
    }
    if(!is.null(y)){
      yCla <- vector("numeric",length(breaks)-1)
      for(idx in 1:(length(breaks)-1)){
        yCla[idx] <- sum(y[as.numeric(names(y))>=breaks[idx] & as.numeric(names(y))<breaks[idx+1]])
      }
    }else{
      yCla <- NULL
    }
  }else{
    xCla <- x
    yCla <- y
  }
  
  z <- c(xCla,yCla)
  if(is.null(classNames)){
    if(splitClassNames){
      splitStr <- "\n"
    }else{
      splitStr <- ""
    }
    txtForm <- rep("%.0f",length(breaks))
    idxRatNum <- which(abs(breaks-round(breaks))>1e-5)
    if(length(idxRatNum)<=1){
      txtForm[idxRatNum] <- "%.1f"
    }else{
      txtForm <- rep("%.1f",length(breaks))
    }
    txtForm[!is.infinite(breaks) & abs(breaks-round(breaks,1))>1e-5] <- "%.2f"
    classNames <- vector("character",length(breaks)-1)
    for(idx in 1:(length(breaks)-1)){
      if(!is.infinite(breaks[idx]) & !is.infinite(breaks[idx+1])){
        classNames[idx] <- paste0(sprintf(txtForm[idx],breaks[idx]),"-",splitStr,sprintf(txtForm[idx+1],breaks[idx+1]))
      }else{
        if(is.infinite(breaks[idx])){
          classNames[idx] <- paste0("<",splitStr,sprintf(txtForm[idx+1],breaks[idx+1]))
        }else{
          classNames[idx] <- paste0("\u2265",splitStr,sprintf(txtForm[idx],breaks[idx]))
        }
      }
    }
  }
  
  xTxtForm <- paste0("%.",as.numeric(xCla>0),"f")
  yTxtForm <- paste0("%.",as.numeric(yCla>0),"f")
  
  if(!is.null(unit)){
    if(unit=="%"){
      unit <- unit
    }else{
      unit <- paste0(" ",unit)
    }
  }else{
    if(absTxt){
      unit <- ""
    }else{
      unit <- "%"
    }
  }
  
  fac <- 100/sum(z)
  if(absTxt){
    fac <- 1
  }
  
  df<-data.frame(factor(classNames,classNames),round(xCla,2),round(xCla/sum(xCla)*100,2))
  colnames(df)<-c("Classes","Counts","%")
  return(df)
}
countFreq2DMF <- function(a,b,c,d,e,f,x1,x2,x3,breaks1,breaks2,breaks3,breaks4,unit,X){
  a <- NULL
  b <- NULL
  c <- NULL
  d <- NULL
  e <- NULL
  f <- NULL
  
  numX1 <- length(driveTypeBreaks)-1
  numX2 <- length(driveTypeBreaks1)-1
  numX3 <- length(engSpeedTypeBreaks)-1
  numX4 <- length(fuelConsTypeBreaks)-1
  numX1X2Max <- max(numX1,numX2)
  numX2X3Max <- max(numX2,numX3)
  numX1X4Max <- max(numX1,numX4)
  
  ausX <- na.omit(X[,c("EngRPM","VehSpd_Disp","SSA_EngSp")])
  if(is.character(ausX$SSA_EngSp)){ausX$SSA_EngSp <- as.numeric(ausX$SSA_EngSp)}
  a <- rbind(a,nrow(ausX[ausX$EngRPM==0 & ausX$VehSpd_Disp==0 & ausX$SSA_EngSp==0,])*unit)
  b <- rbind(b,nrow(ausX[ausX$EngRPM==0 & ausX$VehSpd_Disp==0 & ausX$SSA_EngSp>0,])*unit)
  
  c <- rbind(c,table(c(findInterval(x2,breaks3),1:numX2X3Max),c(findInterval(x1,breaks2),1:numX2X3Max))[2,1]*unit)
  d <- rbind(d,table(c(findInterval(x1,breaks2),1:numX2))[2]*unit)
  
  comp <- table(c(findInterval(x3,breaks4),1:numX1X4Max),c(findInterval(x1,breaks1),1:numX1X4Max))
  diag(comp) <- diag(comp)-1
  count <- comp[1:numX4,1:numX1]*unit
  
  e <- rbind(e,count[1,2])
  f <- rbind(f,count[2,2])
  rValue <- cbind(a,b,c,d,e,f)
  return(rValue)
}
createHeatMapDF <- function(x,breaksX,breaksY){  ##Plotpath
  countMatrix=NULL
  brXExactVals=FALSE
  brYExactVals=FALSE
  unit="h"
  
  if(is.null(countMatrix)){
    colNam <- as.numeric(colnames(x))
    rowNam <- as.numeric(rownames(x))
    countMatrix <- matrix(0,length(breaksY)-1+brYExactVals,length(breaksX)-1+brXExactVals)
    if(!is.null(x) & length(x)>0){
      for(idx in 1:(length(breaksX)-1)){
        for(idy in 1:(length(breaksY)-1)){
          countMatrix[idy,idx] <- sum(x[rowNam>=breaksY[idy] & rowNam<breaksY[idy+1],colNam>=breaksX[idx] & colNam<breaksX[idx+1]])
        }
      }
      if(brXExactVals){
        for(idy in 1:(length(breaksY)-1)){
          countMatrix[idy,length(breaksX)] <- sum(x[rowNam>=breaksY[idy] & rowNam<breaksY[idy+1],colNam==breaksX[length(breaksX)]])
        }
      }
      if(brYExactVals){
        for(idx in 1:(length(breaksX)-1)){
          countMatrix[length(breaksY),idx] <- sum(x[rowNam==breaksY[length(breaksY)],colNam>=breaksX[idx] & colNam<breaksX[idx+1]])
        }
      }
      if(brXExactVals & brYExactVals){
        countMatrix[length(breaksY),length(breaksX)] <- x[rowNam==breaksY[length(breaksY)],colNam==breaksX[length(breaksX)]]
      }
    }
  }
  
  countMatrixRev <- countMatrix[seq(dim(countMatrix)[1],1,-1),]
  counts <- countMatrixRev
  countsPerc <- counts/sum(counts)*100
  counts[countMatrixRev==0] <- NA
  
  if(brXExactVals){
    classNamesX <- as.character(breaksX)
  }else{
    classNamesX <- vector("character",length(breaksX)-1)
    for(idx in 1:(length(breaksX)-1)){
      if(!is.infinite(breaksX[idx]) & !is.infinite(breaksX[idx+1])){
        classNamesX[idx] <- paste0(breaksX[idx],"-",breaksX[idx+1])
      }else{
        if(is.infinite(breaksX[idx])){
          classNamesX[idx] <- paste0("<",breaksX[idx+1])
        }else{
          classNamesX[idx] <- paste0("\u2265",breaksX[idx])
        }
      }
    }
  }
  if(brYExactVals){
    classNamesY <- rev(as.character(breaksY))
  }else{
    classNamesY <- vector("character",length(breaksY)-1)
    for(idy in 1:(length(breaksY)-1)){
      if(!is.infinite(breaksY[idy]) & !is.infinite(breaksY[idy+1])){
        classNamesY[idy] <- paste0(breaksY[idy],"-",breaksY[idy+1])
      }else{
        if(is.infinite(breaksY[idy])){
          classNamesY[idy] <- paste0("<",breaksY[idy+1])
        }else{
          classNamesY[idy] <- paste0("\u2265",breaksY[idy])
        }
      }
    }
    classNamesY <- rev(classNamesY)
  }
  
  if(unit==""){
    cellnote <- matrix(paste0(sprintf("%.0f",counts),"\n (",sprintf("%.1f",countsPerc),"%)"),length(breaksY)-1+brYExactVals,length(breaksX)-1+brXExactVals)
  }else{
    cellnote <- matrix(paste0(sprintf("%.1f",counts)," ",unit,"\n (",sprintf("%.1f",countsPerc),"%)"),length(breaksY)-1+brYExactVals,length(breaksX)-1+brXExactVals)
  }
  cellnote[is.na(counts)] <- ""
  counts2 <- counts
  counts2[sprintf("%.1f",countsPerc)=="0.0"] <- NA
  
  df<-data.frame(cellnote)
  colnames(df)<-classNamesX
  rownames(df)<-classNamesY
  return(df)
}

plotHeatmap <- function(plotPath,plotName,x,countMatrix=NULL,breaksX,breaksY,brXExactVals=FALSE,brYExactVals=FALSE,xLab,yLab,title="",subtitle="",subsubtitle="",unit="h",cexMain=1.8,cexLab=1.5,cexAx=2,cexNote=1.5,cexSub=1.1,save=TRUE){  ##Plotpath
  library("gplots")
  library("grid")
  
  if(!save){
    cexMain <- 1.5
    cexLab <- 1.3
    cexAx <- 1.5
    cexSub <- 1
    cexNote <- 1.2
  }
  if(save && length(breaksY)>14){
    cexNote <- 1.3
  }
  
  if(is.null(countMatrix)){
    colNam <- as.numeric(colnames(x))
    rowNam <- as.numeric(rownames(x))
    countMatrix <- matrix(0,length(breaksY)-1+brYExactVals,length(breaksX)-1+brXExactVals)
    if(!is.null(x) & length(x)>0){
      for(idx in 1:(length(breaksX)-1)){
        for(idy in 1:(length(breaksY)-1)){
          countMatrix[idy,idx] <- sum(x[rowNam>=breaksY[idy] & rowNam<breaksY[idy+1],colNam>=breaksX[idx] & colNam<breaksX[idx+1]])
        }
      }
      if(brXExactVals){
        for(idy in 1:(length(breaksY)-1)){
          countMatrix[idy,length(breaksX)] <- sum(x[rowNam>=breaksY[idy] & rowNam<breaksY[idy+1],colNam==breaksX[length(breaksX)]])
        }
      }
      if(brYExactVals){
        for(idx in 1:(length(breaksX)-1)){
          countMatrix[length(breaksY),idx] <- sum(x[rowNam==breaksY[length(breaksY)],colNam>=breaksX[idx] & colNam<breaksX[idx+1]])
        }
      }
      if(brXExactVals & brYExactVals){
        countMatrix[length(breaksY),length(breaksX)] <- x[rowNam==breaksY[length(breaksY)],colNam==breaksX[length(breaksX)]]
      }
    }
  }
  
  countMatrixRev <- countMatrix[seq(dim(countMatrix)[1],1,-1),]
  counts <- countMatrixRev
  countsPerc <- counts/sum(counts)*100
  counts[countMatrixRev==0] <- NA
  
  if(brXExactVals){
    classNamesX <- as.character(breaksX)
  }else{
    classNamesX <- vector("character",length(breaksX)-1)
    for(idx in 1:(length(breaksX)-1)){
      if(!is.infinite(breaksX[idx]) & !is.infinite(breaksX[idx+1])){
        classNamesX[idx] <- paste0(breaksX[idx],"-",breaksX[idx+1])
      }else{
        if(is.infinite(breaksX[idx])){
          classNamesX[idx] <- paste0("<",breaksX[idx+1])
        }else{
          classNamesX[idx] <- paste0("\u2265",breaksX[idx])
        }
      }
    }
  }
  if(brYExactVals){
    classNamesY <- rev(as.character(breaksY))
  }else{
    classNamesY <- vector("character",length(breaksY)-1)
    for(idy in 1:(length(breaksY)-1)){
      if(!is.infinite(breaksY[idy]) & !is.infinite(breaksY[idy+1])){
        classNamesY[idy] <- paste0(breaksY[idy],"-",breaksY[idy+1])
      }else{
        if(is.infinite(breaksY[idy])){
          classNamesY[idy] <- paste0("<",breaksY[idy+1])
        }else{
          classNamesY[idy] <- paste0("\u2265",breaksY[idy])
        }
      }
    }
    classNamesY <- rev(classNamesY)
  }
  
  if(unit==""){
    cellnote <- matrix(paste0(sprintf("%.0f",counts),"\n (",sprintf("%.1f",countsPerc),"%)"),length(breaksY)-1+brYExactVals,length(breaksX)-1+brXExactVals)
  }else{
    cellnote <- matrix(paste0(sprintf("%.1f",counts)," ",unit,"\n (",sprintf("%.1f",countsPerc),"%)"),length(breaksY)-1+brYExactVals,length(breaksX)-1+brXExactVals)
  }
  cellnote[is.na(counts)] <- ""
  counts2 <- counts
  counts2[sprintf("%.1f",countsPerc)=="0.0"] <- NA
  notecol <- rep("white",prod(dim(counts2)))
  notecol[is.na(sapply(t(counts2[seq(dim(counts2)[1],1,-1),]),c))] <- "grey60"
  
  coloPan <- colorpanel(n=1000,low="cornflowerblue",mid="chartreuse2",high="firebrick1")
  
  if(any(!is.na(counts))){
    if(save){
      png(paste0(plotPath,plotName,".png"),width=1200,height=600)
    }
    par(cex.main=cexMain)
    heatmap.2(counts2,cellnote=cellnote,Rowv=FALSE,Colv=FALSE,
              colsep=c(1:(length(breaksX)-1+brXExactVals)),rowsep=c(1:(length(breaksY)-1+brYExactVals)),
              sepwidth=c(0.01,0.01),dendrogram="none",notecex=cexNote,cexRow=cexAx,cexCol=cexAx,
              lmat=rbind(c(0,3,4), c(2,1,0),c(0,0,0)),lwid=c(0.1,10,1.6),lhei=c(0.04,0.25,0.035),
              key=TRUE, key.title = NA, key.ylab = NA, density.info = "none", key.xlab = NA,
              main=title,notecol=notecol,trace="none",labRow=classNamesY,labCol=classNamesX,col=coloPan,na.color="grey90")
    grid.text(xLab,x=unit(0.5,"npc"),y=unit(0.005,"npc"),just=c("center","bottom"),gp=gpar(cex=cexLab))
    grid.text(yLab,x=unit(0.92,"npc"),y=unit(0.5,"npc"),just=c("center","center"),rot = 90, gp=gpar(cex=cexLab))
    # mtext(4,text=yLab,line=-1,cex=cexLab)
    mtext(3,text=subtitle,line=-0.25,cex=cexLab)
    grid.text(subsubtitle,x=unit(0.00,"npc"),y=unit(0.005,"npc"),just=c("left","bottom"),gp=gpar(cex=cexSub))
    grid.text("Daten in Stunden",x=unit(0.94,"npc"),y=unit(0.86,"npc"),gp=gpar(cex=1.1,fontface=2))
    if(save){
      dev.off()
    }
  }
}

##camtronic1
plotHeatmapCam1 <- function(plotPath,plotName,x,countMatrix=NULL,breaksX,breaksY,brXExactVals=FALSE,brYExactVals=FALSE,xLab,yLab,title="",subtitle="",subsubtitle="",unit="",cexMain=1.8,cexLab=1.5,cexAx=2,cexNote=1.5,cexSub=1.1,save=TRUE){  ##Plotpath
  library("gplots")
  library("grid")
  
  if(!save){
    cexMain <- 1.5
    cexLab <- 1.3
    cexAx <- 1.5
    cexSub <- 1
    cexNote <- 1.2
  }
  if(save && length(breaksY)>14){
    cexNote <- 1.3
  }
  
  if(is.null(countMatrix)){
    colNam <- as.numeric(colnames(x))
    rowNam <- as.numeric(rownames(x))
    countMatrix <- matrix(0,length(breaksY)-1+brYExactVals,length(breaksX)-1+brXExactVals)
    if(!is.null(x) & length(x)>0){
      for(idx in 1:(length(breaksX)-1)){
        for(idy in 1:(length(breaksY)-1)){
          countMatrix[idy,idx] <- sum(x[rowNam>=breaksY[idy] & rowNam<breaksY[idy+1],colNam>=breaksX[idx] & colNam<breaksX[idx+1]])
        }
      }
      if(brXExactVals){
        for(idy in 1:(length(breaksY)-1)){
          countMatrix[idy,length(breaksX)] <- sum(x[rowNam>=breaksY[idy] & rowNam<breaksY[idy+1],colNam==breaksX[length(breaksX)]])
        }
      }
      if(brYExactVals){
        for(idx in 1:(length(breaksX)-1)){
          countMatrix[length(breaksY),idx] <- sum(x[rowNam==breaksY[length(breaksY)],colNam>=breaksX[idx] & colNam<breaksX[idx+1]])
        }
      }
      if(brXExactVals & brYExactVals){
        countMatrix[length(breaksY),length(breaksX)] <- x[rowNam==breaksY[length(breaksY)],colNam==breaksX[length(breaksX)]]
      }
    }
  }
  
  countMatrixRev <- countMatrix[seq(dim(countMatrix)[1],1,-1),]
  counts <- countMatrixRev
  countsPerc <- counts/sum(counts)*100
  counts[countMatrixRev==0] <- NA
  
  if(brXExactVals){
    classNamesX <- as.character(breaksX)
  }else{
    classNamesX <- vector("character",length(breaksX)-1)
    for(idx in 1:(length(breaksX)-1)){
      if(!is.infinite(breaksX[idx]) & !is.infinite(breaksX[idx+1])){
        classNamesX[idx] <- paste0(breaksX[idx],"-",breaksX[idx+1])
      }else{
        if(is.infinite(breaksX[idx])){
          classNamesX[idx] <- paste0("<",breaksX[idx+1])
        }else{
          classNamesX[idx] <- paste0("\u2265",breaksX[idx])
        }
      }
    }
  }
  if(brYExactVals){
    classNamesY <- rev(as.character(breaksY))
  }else{
    classNamesY <- vector("character",length(breaksY)-1)
    for(idy in 1:(length(breaksY)-1)){
      if(!is.infinite(breaksY[idy]) & !is.infinite(breaksY[idy+1])){
        classNamesY[idy] <- paste0(breaksY[idy],"-",breaksY[idy+1])
      }else{
        if(is.infinite(breaksY[idy])){
          classNamesY[idy] <- paste0("<",breaksY[idy+1])
        }else{
          classNamesY[idy] <- paste0("\u2265",breaksY[idy])
        }
      }
    }
    classNamesY <- rev(classNamesY)
  }
  
  if(unit==""){
    cellnote <- matrix(paste0(sprintf("%.0f",counts),"\n (",sprintf("%.1f",countsPerc),"%)"),length(breaksY)-1+brYExactVals,length(breaksX)-1+brXExactVals)
  }else{
    cellnote <- matrix(paste0(sprintf("%.1f",counts)," ",unit,"\n (",sprintf("%.1f",countsPerc),"%)"),length(breaksY)-1+brYExactVals,length(breaksX)-1+brXExactVals)
  }
  cellnote[is.na(counts)] <- ""
  counts2 <- counts
  counts2[sprintf("%.1f",countsPerc)=="0.0"] <- NA
  notecol <- rep("white",prod(dim(counts2)))
  notecol[is.na(sapply(t(counts2[seq(dim(counts2)[1],1,-1),]),c))] <- "grey60"
  
  coloPan <- colorpanel(n=1000,low="cornflowerblue",mid="chartreuse2",high="firebrick1")
  title=paste("Anzahl ZAS-Schanltungen pro Klasse: insgesamt",sprintf("%.0f",sum(x[,as.numeric(colnames(x))>0],na.rm=TRUE)),title)
  if(any(!is.na(counts))){
    if(save){
      png(paste0(plotPath,plotName,".png"),width=1200,height=600)
    }
    par(cex.main=cexMain)
    heatmap.2(counts2,cellnote=cellnote,Rowv=FALSE,Colv=FALSE,
              colsep=c(1:(length(breaksX)-1+brXExactVals)),rowsep=c(1:(length(breaksY)-1+brYExactVals)),
              sepwidth=c(0.01,0.01),dendrogram="none",notecex=cexNote,cexRow=cexAx,cexCol=cexAx,
              lmat=rbind(c(0,3,4), c(2,1,0),c(0,0,0)),lwid=c(0.1,10,1.6),lhei=c(0.04,0.25,0.035),
              key=TRUE, key.title = NA, key.ylab = NA, density.info = "none", key.xlab = NA,
              main=title,notecol=notecol,trace="none",labRow=classNamesY,labCol=classNamesX,col=coloPan,na.color="grey90")
    grid.text(xLab,x=unit(0.5,"npc"),y=unit(0.005,"npc"),just=c("center","bottom"),gp=gpar(cex=cexLab))
    grid.text(yLab,x=unit(0.92,"npc"),y=unit(0.5,"npc"),just=c("center","center"),rot = 90, gp=gpar(cex=cexLab))
    # mtext(4,text=yLab,line=-1,cex=cexLab)
    mtext(3,text=subtitle,line=-0.25,cex=cexLab)
    grid.text(subsubtitle,x=unit(0.00,"npc"),y=unit(0.005,"npc"),just=c("left","bottom"),gp=gpar(cex=cexSub))
    grid.text("Daten in Stunden",x=unit(0.94,"npc"),y=unit(0.86,"npc"),gp=gpar(cex=1.1,fontface=2))
    if(save){
      dev.off()
    }
  }
}

##camtronic2
plotHeatmapCam2 <- function(plotPath,plotName,x,countMatrix=NULL,breaksX,breaksY,brXExactVals=FALSE,brYExactVals=FALSE,xLab,yLab,title="",subtitle="",subsubtitle="",unit="h",cexMain=1.8,cexLab=1.5,cexAx=2,cexNote=1.5,cexSub=1.1,save=TRUE){  ##Plotpath
  library("gplots")
  library("grid")
  x<-x/3600
  if(!save){
    cexMain <- 1.5
    cexLab <- 1.3
    cexAx <- 1.5
    cexSub <- 1
    cexNote <- 1.2
  }
  if(save && length(breaksY)>14){
    cexNote <- 1.3
  }
  
  if(is.null(countMatrix)){
    colNam <- as.numeric(colnames(x))
    rowNam <- as.numeric(rownames(x))
    countMatrix <- matrix(0,length(breaksY)-1+brYExactVals,length(breaksX)-1+brXExactVals)
    if(!is.null(x) & length(x)>0){
      for(idx in 1:(length(breaksX)-1)){
        for(idy in 1:(length(breaksY)-1)){
          countMatrix[idy,idx] <- sum(x[rowNam>=breaksY[idy] & rowNam<breaksY[idy+1],colNam>=breaksX[idx] & colNam<breaksX[idx+1]])
        }
      }
      if(brXExactVals){
        for(idy in 1:(length(breaksY)-1)){
          countMatrix[idy,length(breaksX)] <- sum(x[rowNam>=breaksY[idy] & rowNam<breaksY[idy+1],colNam==breaksX[length(breaksX)]])
        }
      }
      if(brYExactVals){
        for(idx in 1:(length(breaksX)-1)){
          countMatrix[length(breaksY),idx] <- sum(x[rowNam==breaksY[length(breaksY)],colNam>=breaksX[idx] & colNam<breaksX[idx+1]])
        }
      }
      if(brXExactVals & brYExactVals){
        countMatrix[length(breaksY),length(breaksX)] <- x[rowNam==breaksY[length(breaksY)],colNam==breaksX[length(breaksX)]]
      }
    }
  }
  
  countMatrixRev <- countMatrix[seq(dim(countMatrix)[1],1,-1),]
  counts <- countMatrixRev
  countsPerc <- counts/sum(counts)*100
  counts[countMatrixRev==0] <- NA
  
  if(brXExactVals){
    classNamesX <- as.character(breaksX)
  }else{
    classNamesX <- vector("character",length(breaksX)-1)
    for(idx in 1:(length(breaksX)-1)){
      if(!is.infinite(breaksX[idx]) & !is.infinite(breaksX[idx+1])){
        classNamesX[idx] <- paste0(breaksX[idx],"-",breaksX[idx+1])
      }else{
        if(is.infinite(breaksX[idx])){
          classNamesX[idx] <- paste0("<",breaksX[idx+1])
        }else{
          classNamesX[idx] <- paste0("\u2265",breaksX[idx])
        }
      }
    }
  }
  if(brYExactVals){
    classNamesY <- rev(as.character(breaksY))
  }else{
    classNamesY <- vector("character",length(breaksY)-1)
    for(idy in 1:(length(breaksY)-1)){
      if(!is.infinite(breaksY[idy]) & !is.infinite(breaksY[idy+1])){
        classNamesY[idy] <- paste0(breaksY[idy],"-",breaksY[idy+1])
      }else{
        if(is.infinite(breaksY[idy])){
          classNamesY[idy] <- paste0("<",breaksY[idy+1])
        }else{
          classNamesY[idy] <- paste0("\u2265",breaksY[idy])
        }
      }
    }
    classNamesY <- rev(classNamesY)
  }
  
  if(unit==""){
    cellnote <- matrix(paste0(sprintf("%.0f",counts),"\n (",sprintf("%.1f",countsPerc),"%)"),length(breaksY)-1+brYExactVals,length(breaksX)-1+brXExactVals)
  }else{
    cellnote <- matrix(paste0(sprintf("%.1f",counts)," ",unit,"\n (",sprintf("%.1f",countsPerc),"%)"),length(breaksY)-1+brYExactVals,length(breaksX)-1+brXExactVals)
  }
  cellnote[is.na(counts)] <- ""
  counts2 <- counts
  counts2[sprintf("%.1f",countsPerc)=="0.0"] <- NA
  notecol <- rep("white",prod(dim(counts2)))
  notecol[is.na(sapply(t(counts2[seq(dim(counts2)[1],1,-1),]),c))] <- "grey60"
  
  coloPan <- colorpanel(n=1000,low="cornflowerblue",mid="chartreuse2",high="firebrick1")
  title=paste("(Relative) ZAS-Betriebszeit pro Klasse: insgesamt",sprintf("%.0f",sum(x[,as.numeric(colnames(x))>0],na.rm=TRUE)),"h",title)
  
  if(any(!is.na(counts))){
    if(save){
      png(paste0(plotPath,plotName,".png"),width=1200,height=600)
    }
    par(cex.main=cexMain)
    heatmap.2(counts2,cellnote=cellnote,Rowv=FALSE,Colv=FALSE,
              colsep=c(1:(length(breaksX)-1+brXExactVals)),rowsep=c(1:(length(breaksY)-1+brYExactVals)),
              sepwidth=c(0.01,0.01),dendrogram="none",notecex=cexNote,cexRow=cexAx,cexCol=cexAx,
              lmat=rbind(c(0,3,4), c(2,1,0),c(0,0,0)),lwid=c(0.1,10,1.6),lhei=c(0.04,0.25,0.035),
              key=TRUE, key.title = NA, key.ylab = NA, density.info = "none", key.xlab = NA,
              main=title,notecol=notecol,trace="none",labRow=classNamesY,labCol=classNamesX,col=coloPan,na.color="grey90")
    grid.text(xLab,x=unit(0.5,"npc"),y=unit(0.005,"npc"),just=c("center","bottom"),gp=gpar(cex=cexLab))
    grid.text(yLab,x=unit(0.92,"npc"),y=unit(0.5,"npc"),just=c("center","center"),rot = 90, gp=gpar(cex=cexLab))
    # mtext(4,text=yLab,line=-1,cex=cexLab)
    mtext(3,text=subtitle,line=-0.25,cex=cexLab)
    grid.text(subsubtitle,x=unit(0.00,"npc"),y=unit(0.005,"npc"),just=c("left","bottom"),gp=gpar(cex=cexSub))
    grid.text("Daten in Stunden",x=unit(0.94,"npc"),y=unit(0.86,"npc"),gp=gpar(cex=1.1,fontface=2))
    if(save){
      dev.off()
    }
  }
}

##camtronic3
plotHeatmapCam3 <- function(plotPath,plotName,x,countMatrix=NULL,breaksX,breaksY,brXExactVals=FALSE,brYExactVals=FALSE,xLab,yLab,title="",subtitle="",subsubtitle="",unit="h",cexMain=1.8,cexLab=1.5,cexAx=2,cexNote=1.5,cexSub=1.1,save=TRUE){  ##Plotpath
  library("gplots")
  library("grid")
  
  if(!save){
    cexMain <- 1.5
    cexLab <- 1.3
    cexAx <- 1.5
    cexSub <- 1
    cexNote <- 1.2
  }
  if(save && length(breaksY)>14){
    cexNote <- 1.3
  }
  options(stringsAsFactors = FALSE)
  cam1Df <- data.frame(engbreaks = as.character(),engTrq = as.character(),value = numeric())
  cam2Df <- data.frame(engbreaks = as.character(),engTrq = as.character(),value = numeric())
  cam3Df <- data.frame(engbreaks = as.character(),engTrq = as.character(),value = numeric())
  cam4Df <- data.frame(engbreaks = as.character(),engTrq = as.character(),value = numeric())
  cam5Df <- data.frame(engbreaks = as.character(),engTrq = as.character(),value = numeric())
  cam6Df <- data.frame(engbreaks = as.character(),engTrq = as.character(),value = numeric())
  cam7Df <- data.frame(engbreaks = as.character(),engTrq = as.character(),value = numeric())
  cam8Df <- data.frame(engbreaks = as.character(),engTrq = as.character(),value = numeric())
  cam9Df <- data.frame(engbreaks = as.character(),engTrq = as.character(),value = numeric())
  cam10Df <- data.frame(engbreaks = as.character(),engTrq = as.character(),value = numeric())
  cam11Df <- data.frame(engbreaks = as.character(),engTrq = as.character(),value = numeric())
  cam12Df <- data.frame(engbreaks = as.character(),engTrq = as.character(),value = numeric())
  cam13Df <- data.frame(engbreaks = as.character(),engTrq = as.character(),value = numeric())
  cam14Df <- data.frame(engbreaks = as.character(),engTrq = as.character(),value = numeric())
  cam15Df <- data.frame(engbreaks = as.character(),engTrq = as.character(),value = numeric())
  cam16Df <- data.frame(engbreaks = as.character(),engTrq = as.character(),value = numeric())
  cam17Df <- data.frame(engbreaks = as.character(),engTrq = as.character(),value = numeric())
  
  maindataDF <- as.data.frame(x)
  
  for(idx in 1:(length(breaksY)-1)){
    calc1Val <- subset(maindataDF,maindataDF$XEngSpd >= 0 & maindataDF$XEngSpd < 400 & maindataDF$XEngTor>= breaksY[idx] & maindataDF$XEngTor < breaksY[idx+1])
    if(length(calc1Val)!=0){
      cam1Df[idx,1] <- paste0("0","-","400")
      cam1Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam1Df[idx,3] <- sum(calc1Val$XOdo)
    }else {
      cam1Df[idx,1] <- paste0("0","-","400")
      cam1Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam1Df[idx,3] <- NA
    }
  }
  for(idx in 1:(length(breaksY)-1)){
    calc1Val <- subset(maindataDF,maindataDF$XEngSpd >= 400 & maindataDF$XEngSpd < 800 & maindataDF$XEngTor>= breaksY[idx] & maindataDF$XEngTor < breaksY[idx+1])
    if(length(calc1Val)!=0){
      cam2Df[idx,1] <- paste0("400","-","800")
      cam2Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam2Df[idx,3] <- sum(calc1Val$XOdo)
    }else {
      cam2Df[idx,1] <- paste0("400","-","800")
      cam2Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam2Df[idx,3] <- NA
    }
  }
  for(idx in 1:(length(breaksY)-1)){
    calc1Val <- subset(maindataDF,maindataDF$XEngSpd >= 800 & maindataDF$XEngSpd < 1200 & maindataDF$XEngTor>= breaksY[idx] & maindataDF$XEngTor < breaksY[idx+1])
    if(length(calc1Val)!=0){
      cam3Df[idx,1] <- paste0("800","-","1200")
      cam3Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam3Df[idx,3] <- sum(calc1Val$XOdo)
    }else {
      cam3Df[idx,1] <- paste0("800","-","1200")
      cam3Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam3Df[idx,3] <- NA
    }
  }
  for(idx in 1:(length(breaksY)-1)){
    calc1Val <- subset(maindataDF,maindataDF$XEngSpd >= 1200 & maindataDF$XEngSpd < 1600 & maindataDF$XEngTor>= breaksY[idx] & maindataDF$XEngTor < breaksY[idx+1])
    if(length(calc1Val)!=0){
      cam4Df[idx,1] <- paste0("1200","-","1600")
      cam4Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam4Df[idx,3] <- sum(calc1Val$XOdo)
    }else {
      cam4Df[idx,1] <- paste0("1200","-","1600")
      cam4Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam4Df[idx,3] <- NA
    }
  }
  for(idx in 1:(length(breaksY)-1)){
    calc1Val <- subset(maindataDF,maindataDF$XEngSpd >= 1600 & maindataDF$XEngSpd < 2000 & maindataDF$XEngTor>= breaksY[idx] & maindataDF$XEngTor < breaksY[idx+1])
    if(length(calc1Val)!=0){
      cam5Df[idx,1] <- paste0("1600","-","2000")
      cam5Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam5Df[idx,3] <- sum(calc1Val$XOdo)
    }else {
      cam5Df[idx,1] <- paste0("1600","-","2000")
      cam5Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam5Df[idx,3] <- NA
    }
  }
  for(idx in 1:(length(breaksY)-1)){
    calc1Val <- subset(maindataDF,maindataDF$XEngSpd >= 2000 & maindataDF$XEngSpd < 2400 & maindataDF$XEngTor>= breaksY[idx] & maindataDF$XEngTor < breaksY[idx+1])
    if(length(calc1Val)!=0){
      cam6Df[idx,1] <- paste0("2000","-","2400")
      cam6Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam6Df[idx,3] <- sum(calc1Val$XOdo)
    }else {
      cam6Df[idx,1] <- paste0("2000","-","2400")
      cam6Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam6Df[idx,3] <- NA
    }
  }
  for(idx in 1:(length(breaksY)-1)){
    calc1Val <- subset(maindataDF,maindataDF$XEngSpd >= 2400 & maindataDF$XEngSpd < 2800 & maindataDF$XEngTor>= breaksY[idx] & maindataDF$XEngTor < breaksY[idx+1])
    if(length(calc1Val)!=0){
      cam7Df[idx,1] <- paste0("2400","-","2800")
      cam7Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam7Df[idx,3] <- sum(calc1Val$XOdo)
    }else {
      cam7Df[idx,1] <- paste0("2400","-","2800")
      cam7Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam7Df[idx,3] <- NA
    }
  }
  for(idx in 1:(length(breaksY)-1)){
    calc1Val <- subset(maindataDF,maindataDF$XEngSpd >= 2800 & maindataDF$XEngSpd < 3200 & maindataDF$XEngTor>= breaksY[idx] & maindataDF$XEngTor < breaksY[idx+1])
    if(length(calc1Val)!=0){
      cam8Df[idx,1] <- paste0("2800","-","3200")
      cam8Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam8Df[idx,3] <- sum(calc1Val$XOdo)
    }else {
      cam8Df[idx,1] <- paste0("2800","-","3200")
      cam8Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam8Df[idx,3] <- NA
    }
  }
  for(idx in 1:(length(breaksY)-1)){
    calc1Val <- subset(maindataDF,maindataDF$XEngSpd >= 3200 & maindataDF$XEngSpd < 3600 & maindataDF$XEngTor>= breaksY[idx] & maindataDF$XEngTor < breaksY[idx+1])
    if(length(calc1Val)!=0){
      cam9Df[idx,1] <- paste0("3200","-","3600")
      cam9Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam9Df[idx,3] <- sum(calc1Val$XOdo)
    }else {
      cam9Df[idx,1] <- paste0("3200","-","3600")
      cam9Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam9Df[idx,3] <- NA
    }
  }
  for(idx in 1:(length(breaksY)-1)){
    calc1Val <- subset(maindataDF,maindataDF$XEngSpd >= 3600 & maindataDF$XEngSpd < 4000 & maindataDF$XEngTor>= breaksY[idx] & maindataDF$XEngTor < breaksY[idx+1])
    if(length(calc1Val)!=0){
      cam10Df[idx,1] <- paste0("3600","-","4000")
      cam10Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam10Df[idx,3] <- sum(calc1Val$XOdo)
    }else {
      cam10Df[idx,1] <-  paste0("3600","-","4000")
      cam10Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam10Df[idx,3] <- NA
    }
  }
  for(idx in 1:(length(breaksY)-1)){
    calc1Val <- subset(maindataDF,maindataDF$XEngSpd >= 4000 & maindataDF$XEngSpd < 4400 & maindataDF$XEngTor>= breaksY[idx] & maindataDF$XEngTor < breaksY[idx+1])
    if(length(calc1Val)!=0){
      cam11Df[idx,1] <- paste0("4000","-","4400")
      cam11Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam11Df[idx,3] <- sum(calc1Val$XOdo)
    }else {
      cam11Df[idx,1] <- paste0("4000","-","4400")
      cam11Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam11Df[idx,3] <- NA
    }
  }
  for(idx in 1:(length(breaksY)-1)){
    calc1Val <- subset(maindataDF,maindataDF$XEngSpd >= 4400 & maindataDF$XEngSpd < 4800 & maindataDF$XEngTor>= breaksY[idx] & maindataDF$XEngTor < breaksY[idx+1])
    if(length(calc1Val)!=0){
      cam12Df[idx,1] <- paste0("4400","-","4800")
      cam12Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam12Df[idx,3] <- sum(calc1Val$XOdo)
    }else {
      cam12Df[idx,1] <- paste0("0","-","7000")
      cam12Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam12Df[idx,3] <- NA
    }
  }
  for(idx in 1:(length(breaksY)-1)){
    calc1Val <- subset(maindataDF,maindataDF$XEngSpd >= 4800 & maindataDF$XEngSpd < 5200 & maindataDF$XEngTor>= breaksY[idx] & maindataDF$XEngTor < breaksY[idx+1])
    if(length(calc1Val)!=0){
      cam13Df[idx,1] <- paste0("4800","-","5200")
      cam13Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam13Df[idx,3] <- sum(calc1Val$XOdo)
    }else {
      cam13Df[idx,1] <- paste0("4800","-","5200")
      cam13Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam13Df[idx,3] <- NA
    }
  }
  for(idx in 1:(length(breaksY)-1)){
    calc1Val <- subset(maindataDF,maindataDF$XEngSpd >= 5200 & maindataDF$XEngSpd < 5600 & maindataDF$XEngTor>= breaksY[idx] & maindataDF$XEngTor < breaksY[idx+1])
    if(length(calc1Val)!=0){
      cam14Df[idx,1] <- paste0("5200","-","5600")
      cam14Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam14Df[idx,3] <- sum(calc1Val$XOdo)
    }else {
      cam14Df[idx,1] <- paste0("5200","-","5600")
      cam14Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam14Df[idx,3] <- NA
    }
  }
  for(idx in 1:(length(breaksY)-1)){
    calc1Val <- subset(maindataDF,maindataDF$XEngSpd >= 5600 & maindataDF$XEngSpd < 6000 & maindataDF$XEngTor>= breaksY[idx] & maindataDF$XEngTor < breaksY[idx+1])
    if(length(calc1Val)!=0){
      cam15Df[idx,1] <- paste0("5600","-","6000")
      cam15Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam15Df[idx,3] <- sum(calc1Val$XOdo)
    }else {
      cam15Df[idx,1] <- paste0("5600","-","6000")
      cam15Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam15Df[idx,3] <- NA
    }
  }
  for(idx in 1:(length(breaksY)-1)){
    calc1Val <- subset(maindataDF,maindataDF$XEngSpd >= 6000 & maindataDF$XEngSpd < Inf & maindataDF$XEngTor>= breaksY[idx] & maindataDF$XEngTor < breaksY[idx+1])
    if(length(calc1Val)!=0){
      cam16Df[idx,1] <- paste0("6000","-","Inf")
      cam16Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam16Df[idx,3] <- sum(calc1Val$XOdo)
    }else {
      cam16Df[idx,1] <- paste0("6000","-","Inf")
      cam16Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam16Df[idx,3] <- NA
    }
  }
  
  ##rbinding it
  plotdf1 <- rbind(cam1Df,cam2Df,cam3Df,cam4Df,cam5Df,cam6Df,cam7Df,cam8Df,cam9Df,cam10Df,cam11Df,cam12Df,cam13Df,cam14Df,cam15Df,cam16Df)
  
  
  
  ##reshaping data
  library(reshape2)
  plotdf2 <- reshape(plotdf1,idvar = "engTrq",timevar = "engbreaks",ids = "value",direction = "wide")
  
  rownames(plotdf2) <- plotdf2[,1]
  plotdf2<- plotdf2[,2:17]
  print(plotdf2)
  plotdf2[plotdf2 == "NaN"] <- NA
  plotdf2[is.na(plotdf2)] <- NA
  x2<- as.matrix(plotdf2)
  #counts <- x2
  countMatrix <- x2
  title=paste("Laufstrecke pro Klasse: insgesamt",sprintf("%.0f",sum(x2,na.rm=TRUE)),"km",title)
  
  countMatrixRev <- countMatrix[seq(dim(countMatrix)[1],1,-1),]
  countMatrixRev[is.na(countMatrixRev)] <- 0
  counts <- countMatrixRev
  countsPerc <- counts/sum(counts)*100
  counts[countMatrixRev==0] <- NA
  
  if(brXExactVals){
    classNamesX <- as.character(breaksX)
  }else{
    classNamesX <- vector("character",length(breaksX)-1)
    for(idx in 1:(length(breaksX)-1)){
      if(!is.infinite(breaksX[idx]) & !is.infinite(breaksX[idx+1])){
        classNamesX[idx] <- paste0(breaksX[idx],"-",breaksX[idx+1])
      }else{
        if(is.infinite(breaksX[idx])){
          classNamesX[idx] <- paste0("<",breaksX[idx+1])
        }else{
          classNamesX[idx] <- paste0("\u2265",breaksX[idx])
        }
      }
    }
  }
  if(brYExactVals){
    classNamesY <- rev(as.character(breaksY))
  }else{
    classNamesY <- vector("character",length(breaksY)-1)
    for(idy in 1:(length(breaksY)-1)){
      if(!is.infinite(breaksY[idy]) & !is.infinite(breaksY[idy+1])){
        classNamesY[idy] <- paste0(breaksY[idy],"-",breaksY[idy+1])
      }else{
        if(is.infinite(breaksY[idy])){
          classNamesY[idy] <- paste0("<",breaksY[idy+1])
        }else{
          classNamesY[idy] <- paste0("\u2265",breaksY[idy])
        }
      }
    }
    classNamesY <- rev(classNamesY)
  }
  
  if(unit==""){
    cellnote <- matrix(paste0(sprintf("%.0f",counts),"\n (",sprintf("%.1f",countsPerc),"%)"),length(breaksY)-1+brYExactVals,length(breaksX)-1+brXExactVals)
  }else{
    cellnote <- matrix(paste0(sprintf("%.1f",counts)," ",unit,"\n (",sprintf("%.1f",countsPerc),"%)"),length(breaksY)-1+brYExactVals,length(breaksX)-1+brXExactVals)
  }
  cellnote[is.na(counts)] <- ""
  counts2 <- counts
  counts2[sprintf("%.1f",countsPerc)=="0.0"] <- NA
  notecol <- rep("white",prod(dim(counts2)))
  notecol[is.na(sapply(t(counts2[seq(dim(counts2)[1],1,-1),]),c))] <- "grey60"
  
  coloPan <- colorpanel(n=1000,low="cornflowerblue",mid="chartreuse2",high="firebrick1")
  
  if(any(!is.na(counts))){
    if(save){
      png(paste0(plotPath,plotName,".png"),width=1200,height=600)
    }
    par(cex.main=cexMain)
    heatmap.2(counts2,cellnote=cellnote,Rowv=FALSE,Colv=FALSE,
              colsep=c(1:(length(breaksX)-1+brXExactVals)),rowsep=c(1:(length(breaksY)-1+brYExactVals)),
              sepwidth=c(0.01,0.01),dendrogram="none",notecex=cexNote,cexRow=cexAx,cexCol=cexAx,
              lmat=rbind(c(0,3,4), c(2,1,0),c(0,0,0)),lwid=c(0.1,10,1.6),lhei=c(0.04,0.25,0.035),
              key=TRUE, key.title = NA, key.ylab = NA, density.info = "none", key.xlab = NA,
              main=title,notecol=notecol,trace="none",labRow=classNamesY,labCol=classNamesX,col=coloPan,na.color="grey90")
    grid.text(xLab,x=unit(0.5,"npc"),y=unit(0.005,"npc"),just=c("center","bottom"),gp=gpar(cex=cexLab))
    grid.text(yLab,x=unit(0.92,"npc"),y=unit(0.5,"npc"),just=c("center","center"),rot = 90, gp=gpar(cex=cexLab))
    # mtext(4,text=yLab,line=-1,cex=cexLab)
    mtext(3,text=subtitle,line=-0.25,cex=cexLab)
    grid.text(subsubtitle,x=unit(0.00,"npc"),y=unit(0.005,"npc"),just=c("left","bottom"),gp=gpar(cex=cexSub))
    grid.text("Daten in Stunden",x=unit(0.94,"npc"),y=unit(0.86,"npc"),gp=gpar(cex=1.1,fontface=2))
    if(save){
      dev.off()
    }
  }
}

##camtronic4
plotHeatmapCam4 <- function(plotPath,plotName,x,countMatrix=NULL,breaksX,breaksY,brXExactVals=FALSE,brYExactVals=FALSE,xLab,yLab,title="",subtitle="",subsubtitle="",unit="h",cexMain=1.8,cexLab=1.5,cexAx=2,cexNote=1.5,cexSub=1.1,save=TRUE){  ##Plotpath
  library("gplots")
  library("grid")
  
  if(!save){
    cexMain <- 1.5
    cexLab <- 1.3
    cexAx <- 1.5
    cexSub <- 1
    cexNote <- 1.2
  }
  if(save && length(breaksY)>14){
    cexNote <- 1.3
  }
  
  options(stringsAsFactors = FALSE)
  cam1Df <- data.frame(engbreaks = as.character(),engTrq = as.character(),value = numeric())
  cam2Df <- data.frame(engbreaks = as.character(),engTrq = as.character(),value = numeric())
  cam3Df <- data.frame(engbreaks = as.character(),engTrq = as.character(),value = numeric())
  cam4Df <- data.frame(engbreaks = as.character(),engTrq = as.character(),value = numeric())
  cam5Df <- data.frame(engbreaks = as.character(),engTrq = as.character(),value = numeric())
  cam6Df <- data.frame(engbreaks = as.character(),engTrq = as.character(),value = numeric())
  cam7Df <- data.frame(engbreaks = as.character(),engTrq = as.character(),value = numeric())
  cam8Df <- data.frame(engbreaks = as.character(),engTrq = as.character(),value = numeric())
  cam9Df <- data.frame(engbreaks = as.character(),engTrq = as.character(),value = numeric())
  cam10Df <- data.frame(engbreaks = as.character(),engTrq = as.character(),value = numeric())
  cam11Df <- data.frame(engbreaks = as.character(),engTrq = as.character(),value = numeric())
  cam12Df <- data.frame(engbreaks = as.character(),engTrq = as.character(),value = numeric())
  cam13Df <- data.frame(engbreaks = as.character(),engTrq = as.character(),value = numeric())
  cam14Df <- data.frame(engbreaks = as.character(),engTrq = as.character(),value = numeric())
  
  maindataDF <- as.data.frame(x)
  
  for(idx in 1:(length(breaksY)-1)){
    calc1Val <- subset(maindataDF,maindataDF$XEngSpd >= 750 & maindataDF$XEngSpd < 1000 & maindataDF$XEngTor>= breaksY[idx] & maindataDF$XEngTor < breaksY[idx+1])
    if(length(calc1Val)!=0){
      cam1Df[idx,1] <- paste0("750","-","1000")
      cam1Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam1Df[idx,3] <- sum(calc1Val$XOdo)
    }else {
      cam1Df[idx,1] <- paste0("750","-","1000")
      cam1Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam1Df[idx,3] <- NA
    }
  }
  for(idx in 1:(length(breaksY)-1)){
    calc1Val <- subset(maindataDF,maindataDF$XEngSpd >= 1000 & maindataDF$XEngSpd < 1250 & maindataDF$XEngTor>= breaksY[idx] & maindataDF$XEngTor < breaksY[idx+1])
    if(length(calc1Val)!=0){
      cam2Df[idx,1] <- paste0("1000","-","1250")
      cam2Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam2Df[idx,3] <- sum(calc1Val$XOdo)
    }else {
      cam2Df[idx,1] <- paste0("1000","-","1250")
      cam2Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam2Df[idx,3] <- NA
    }
  }
  for(idx in 1:(length(breaksY)-1)){
    calc1Val <- subset(maindataDF,maindataDF$XEngSpd >= 1250 & maindataDF$XEngSpd < 1500 & maindataDF$XEngTor>= breaksY[idx] & maindataDF$XEngTor < breaksY[idx+1])
    if(length(calc1Val)!=0){
      cam3Df[idx,1] <- paste0("1250","-","1500")
      cam3Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam3Df[idx,3] <- sum(calc1Val$XOdo)
    }else {
      cam3Df[idx,1] <- paste0("1250","-","1500")
      cam3Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam3Df[idx,3] <- NA
    }
  }
  for(idx in 1:(length(breaksY)-1)){
    calc1Val <- subset(maindataDF,maindataDF$XEngSpd >= 1500 & maindataDF$XEngSpd < 1750 & maindataDF$XEngTor>= breaksY[idx] & maindataDF$XEngTor < breaksY[idx+1])
    if(length(calc1Val)!=0){
      cam4Df[idx,1] <- paste0("1500","-","1750")
      cam4Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam4Df[idx,3] <- sum(calc1Val$XOdo)
    }else {
      cam4Df[idx,1] <- paste0("1500","-","1750")
      cam4Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam4Df[idx,3] <- NA
    }
  }
  for(idx in 1:(length(breaksY)-1)){
    calc1Val <- subset(maindataDF,maindataDF$XEngSpd >= 1750 & maindataDF$XEngSpd < 2000 & maindataDF$XEngTor>= breaksY[idx] & maindataDF$XEngTor < breaksY[idx+1])
    if(length(calc1Val)!=0){
      cam5Df[idx,1] <- paste0("1750","-","2000")
      cam5Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam5Df[idx,3] <- sum(calc1Val$XOdo)
    }else {
      cam5Df[idx,1] <- paste0("1750","-","2000")
      cam5Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam5Df[idx,3] <- NA
    }
  }
  for(idx in 1:(length(breaksY)-1)){
    calc1Val <- subset(maindataDF,maindataDF$XEngSpd >= 2000 & maindataDF$XEngSpd < 2250 & maindataDF$XEngTor>= breaksY[idx] & maindataDF$XEngTor < breaksY[idx+1])
    if(length(calc1Val)!=0){
      cam6Df[idx,1] <- paste0("2000","-","2250")
      cam6Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam6Df[idx,3] <- sum(calc1Val$XOdo)
    }else {
      cam6Df[idx,1] <- paste0("2000","-","2250")
      cam6Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam6Df[idx,3] <- NA
    }
  }
  for(idx in 1:(length(breaksY)-1)){
    calc1Val <- subset(maindataDF,maindataDF$XEngSpd >= 2250 & maindataDF$XEngSpd < 2500 & maindataDF$XEngTor>= breaksY[idx] & maindataDF$XEngTor < breaksY[idx+1])
    if(length(calc1Val)!=0){
      cam7Df[idx,1] <- paste0("2250","-","2500")
      cam7Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam7Df[idx,3] <- sum(calc1Val$XOdo)
    }else {
      cam7Df[idx,1] <- paste0("2250","-","2500")
      cam7Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam7Df[idx,3] <- NA
    }
  }
  for(idx in 1:(length(breaksY)-1)){
    calc1Val <- subset(maindataDF,maindataDF$XEngSpd >= 2500 & maindataDF$XEngSpd < 2750 & maindataDF$XEngTor>= breaksY[idx] & maindataDF$XEngTor < breaksY[idx+1])
    if(length(calc1Val)!=0){
      cam8Df[idx,1] <- paste0("2500","-","2750")
      cam8Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam8Df[idx,3] <- sum(calc1Val$XOdo)
    }else {
      cam8Df[idx,1] <- paste0("2500","-","2750")
      cam8Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam8Df[idx,3] <- NA
    }
  }
  for(idx in 1:(length(breaksY)-1)){
    calc1Val <- subset(maindataDF,maindataDF$XEngSpd >= 2750 & maindataDF$XEngSpd < 3000 & maindataDF$XEngTor>= breaksY[idx] & maindataDF$XEngTor < breaksY[idx+1])
    if(length(calc1Val)!=0){
      cam9Df[idx,1] <- paste0("2750","-","3000")
      cam9Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam9Df[idx,3] <- sum(calc1Val$XOdo)
    }else {
      cam9Df[idx,1] <- paste0("2750","-","3000")
      cam9Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam9Df[idx,3] <- NA
    }
  }
  for(idx in 1:(length(breaksY)-1)){
    calc1Val <- subset(maindataDF,maindataDF$XEngSpd >= 3000 & maindataDF$XEngSpd < 3250 & maindataDF$XEngTor>= breaksY[idx] & maindataDF$XEngTor < breaksY[idx+1])
    if(length(calc1Val)!=0){
      cam10Df[idx,1] <- paste0("3000","-","3250")
      cam10Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam10Df[idx,3] <- sum(calc1Val$XOdo)
    }else {
      cam10Df[idx,1] <-  paste0("3000","-","3250")
      cam10Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam10Df[idx,3] <- NA
    }
  }
  for(idx in 1:(length(breaksY)-1)){
    calc1Val <- subset(maindataDF,maindataDF$XEngSpd >= 3250 & maindataDF$XEngSpd < 3500 & maindataDF$XEngTor>= breaksY[idx] & maindataDF$XEngTor < breaksY[idx+1])
    if(length(calc1Val)!=0){
      cam11Df[idx,1] <- paste0("3250","-","3500")
      cam11Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam11Df[idx,3] <- round((mean(calc1Val$XVehSpd)/3600)*nrow(calc1Val),2)
    }else {
      cam11Df[idx,1] <- paste0("3250","-","3500")
      cam11Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam11Df[idx,3] <- NA
    }
  }
  for(idx in 1:(length(breaksY)-1)){
    calc1Val <- subset(maindataDF,maindataDF$XEngSpd >= 3500 & maindataDF$XEngSpd < 3750 & maindataDF$XEngTor>= breaksY[idx] & maindataDF$XEngTor < breaksY[idx+1])
    if(length(calc1Val)!=0){
      cam12Df[idx,1] <- paste0("3500","-","3750")
      cam12Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam12Df[idx,3] <- round((mean(calc1Val$XVehSpd)/3600)*nrow(calc1Val),2)
    }else {
      cam12Df[idx,1] <- paste0("3500","-","3750")
      cam12Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam12Df[idx,3] <- NA
    }
  }
  for(idx in 1:(length(breaksY)-1)){
    calc1Val <- subset(maindataDF,maindataDF$XEngSpd >= 3750 & maindataDF$XEngSpd < 4000 & maindataDF$XEngTor>= breaksY[idx] & maindataDF$XEngTor < breaksY[idx+1])
    if(length(calc1Val)!=0){
      cam13Df[idx,1] <- paste0("3750","-","4000")
      cam13Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam13Df[idx,3] <- round((mean(calc1Val$XVehSpd)/3600)*nrow(calc1Val),2)
    }else {
      cam13Df[idx,1] <- paste0("3750","-","4000")
      cam13Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam13Df[idx,3] <- NA
    }
  }
  for(idx in 1:(length(breaksY)-1)){
    calc1Val <- subset(maindataDF,maindataDF$XEngSpd >= 4000 & maindataDF$XEngSpd < Inf & maindataDF$XEngTor>= breaksY[idx] & maindataDF$XEngTor < breaksY[idx+1])
    if(length(calc1Val)!=0){
      cam14Df[idx,1] <- paste0("4000","-","Inf")
      cam14Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam14Df[idx,3] <- round((mean(calc1Val$XVehSpd)/3600)*nrow(calc1Val),2)
    }else {
      cam14Df[idx,1] <- paste0("4000","-","Inf")
      cam14Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam14Df[idx,3] <- NA
    }
  }
  
  ##rbinding it
  plotdf1 <- rbind(cam1Df,cam2Df,cam3Df,cam4Df,cam5Df,cam6Df,cam7Df,cam8Df,cam9Df,cam10Df,cam11Df,cam12Df,cam13Df,cam14Df)
  
  
  
  ##reshaping data
  library(reshape2)
  plotdf2 <- reshape(plotdf1,idvar = "engTrq",timevar = "engbreaks",ids = "value",direction = "wide")
  rownames(plotdf2) <- plotdf2[,1]
  plotdf2<- plotdf2[,2:ncol(plotdf2)]
  plotdf2[plotdf2 == "NaN"] <- NA
  plotdf2[is.na(plotdf2)] <- NA
  
  x2<- as.matrix(plotdf2)
  #counts <- x2
  countMatrix <- x2
  
  countMatrixRev <- countMatrix[seq(dim(countMatrix)[1],1,-1),]
  countMatrixRev[is.na(countMatrixRev)] <- 0
  counts <- countMatrixRev
  countsPerc <- counts/sum(counts)*100
  counts[countMatrixRev==0] <- NA
  
  if(brXExactVals){
    classNamesX <- as.character(breaksX)
  }else{
    classNamesX <- vector("character",length(breaksX)-1)
    for(idx in 1:(length(breaksX)-1)){
      if(!is.infinite(breaksX[idx]) & !is.infinite(breaksX[idx+1])){
        classNamesX[idx] <- paste0(breaksX[idx],"-",breaksX[idx+1])
      }else{
        if(is.infinite(breaksX[idx])){
          classNamesX[idx] <- paste0("<",breaksX[idx+1])
        }else{
          classNamesX[idx] <- paste0("\u2265",breaksX[idx])
        }
      }
    }
  }
  if(brYExactVals){
    classNamesY <- rev(as.character(breaksY))
  }else{
    classNamesY <- vector("character",length(breaksY)-1)
    for(idy in 1:(length(breaksY)-1)){
      if(!is.infinite(breaksY[idy]) & !is.infinite(breaksY[idy+1])){
        classNamesY[idy] <- paste0(breaksY[idy],"-",breaksY[idy+1])
      }else{
        if(is.infinite(breaksY[idy])){
          classNamesY[idy] <- paste0("<",breaksY[idy+1])
        }else{
          classNamesY[idy] <- paste0("\u2265",breaksY[idy])
        }
      }
    }
    classNamesY <- rev(classNamesY)
  }
  
  if(unit==""){
    cellnote <- matrix(paste0(sprintf("%.0f",counts),"\n (",sprintf("%.1f",countsPerc),"%)"),length(breaksY)-1+brYExactVals,length(breaksX)-1+brXExactVals)
  }else{
    cellnote <- matrix(paste0(sprintf("%.1f",counts)," ",unit,"\n (",sprintf("%.1f",countsPerc),"%)"),length(breaksY)-1+brYExactVals,length(breaksX)-1+brXExactVals)
  }
  cellnote[is.na(counts)] <- ""
  counts2 <- counts
  counts2[sprintf("%.1f",countsPerc)=="0.0"] <- NA
  notecol <- rep("white",prod(dim(counts2)))
  notecol[is.na(sapply(t(counts2[seq(dim(counts2)[1],1,-1),]),c))] <- "grey60"
  title=paste("Laufstrecke im ZAS-Betrieb pro Klasse: insgesamt",sprintf("%.0f",sum(x2,na.rm=TRUE)),"km",title)
  
  coloPan <- colorpanel(n=1000,low="cornflowerblue",mid="chartreuse2",high="firebrick1")
  
  if(any(!is.na(counts))){
    if(save){
      png(paste0(plotPath,plotName,".png"),width=1200,height=600)
    }
    par(cex.main=cexMain)
    heatmap.2(counts2,cellnote=cellnote,Rowv=FALSE,Colv=FALSE,
              colsep=c(1:(length(breaksX)-1+brXExactVals)),rowsep=c(1:(length(breaksY)-1+brYExactVals)),
              sepwidth=c(0.01,0.01),dendrogram="none",notecex=cexNote,cexRow=cexAx,cexCol=cexAx,
              lmat=rbind(c(0,3,4), c(2,1,0),c(0,0,0)),lwid=c(0.1,10,1.6),lhei=c(0.04,0.25,0.035),
              key=TRUE, key.title = NA, key.ylab = NA, density.info = "none", key.xlab = NA,
              main=title,notecol=notecol,trace="none",labRow=classNamesY,labCol=classNamesX,col=coloPan,na.color="grey90")
    grid.text(xLab,x=unit(0.5,"npc"),y=unit(0.005,"npc"),just=c("center","bottom"),gp=gpar(cex=cexLab))
    grid.text(yLab,x=unit(0.92,"npc"),y=unit(0.5,"npc"),just=c("center","center"),rot = 90, gp=gpar(cex=cexLab))
    # mtext(4,text=yLab,line=-1,cex=cexLab)
    mtext(3,text=subtitle,line=-0.25,cex=cexLab)
    grid.text(subsubtitle,x=unit(0.00,"npc"),y=unit(0.005,"npc"),just=c("left","bottom"),gp=gpar(cex=cexSub))
    grid.text("Daten in Stunden",x=unit(0.94,"npc"),y=unit(0.86,"npc"),gp=gpar(cex=1.1,fontface=2))
    if(save){
      dev.off()
    }
  }
}

##camtronic5
plotHeatmapCam5 <- function(plotPath,plotName,x,countMatrix=NULL,breaksX,breaksY,brXExactVals=FALSE,brYExactVals=FALSE,xLab,yLab,title="",subtitle="",subsubtitle="",unit="h",cexMain=1.8,cexLab=1.5,cexAx=2,cexNote=1.5,cexSub=1.1,save=TRUE){  ##Plotpath
  library("gplots")
  library("grid")
  
  if(!save){
    cexMain <- 1.5
    cexLab <- 1.3
    cexAx <- 1.5
    cexSub <- 1
    cexNote <- 1.2
  }
  if(save && length(breaksY)>14){
    cexNote <- 1.3
  }
  options(stringsAsFactors = FALSE)
  cam1Df <- data.frame(engbreaks = as.character(),engTrq = as.character(),value = numeric())
  cam2Df <- data.frame(engbreaks = as.character(),engTrq = as.character(),value = numeric())
  cam3Df <- data.frame(engbreaks = as.character(),engTrq = as.character(),value = numeric())
  cam4Df <- data.frame(engbreaks = as.character(),engTrq = as.character(),value = numeric())
  cam5Df <- data.frame(engbreaks = as.character(),engTrq = as.character(),value = numeric())
  cam6Df <- data.frame(engbreaks = as.character(),engTrq = as.character(),value = numeric())
  cam7Df <- data.frame(engbreaks = as.character(),engTrq = as.character(),value = numeric())
  cam8Df <- data.frame(engbreaks = as.character(),engTrq = as.character(),value = numeric())
  cam9Df <- data.frame(engbreaks = as.character(),engTrq = as.character(),value = numeric())
  cam10Df <- data.frame(engbreaks = as.character(),engTrq = as.character(),value = numeric())
  cam11Df <- data.frame(engbreaks = as.character(),engTrq = as.character(),value = numeric())
  cam12Df <- data.frame(engbreaks = as.character(),engTrq = as.character(),value = numeric())
  cam13Df <- data.frame(engbreaks = as.character(),engTrq = as.character(),value = numeric())
  cam14Df <- data.frame(engbreaks = as.character(),engTrq = as.character(),value = numeric())
  
  maindataDF <- as.data.frame(x)
  
  for(idx in 1:(length(breaksY)-1)){
    calc1Val <- subset(maindataDF,maindataDF$XEngSpd >= 750 & maindataDF$XEngSpd < 1000 & maindataDF$XEngTor>= breaksY[idx] & maindataDF$XEngTor < breaksY[idx+1])
    if(length(calc1Val)!=0){
      cam1Df[idx,1] <- paste0("750","-","1000")
      cam1Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam1Df[idx,3] <- length(calc1Val$v2)/sum(calc1Val$v2)
    }else {
      cam1Df[idx,1] <- paste0("750","-","1000")
      cam1Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam1Df[idx,3] <- NA
    }
  }
  for(idx in 1:(length(breaksY)-1)){
    calc1Val <- subset(maindataDF,maindataDF$XEngSpd >= 1000 & maindataDF$XEngSpd < 1250 & maindataDF$XEngTor>= breaksY[idx] & maindataDF$XEngTor < breaksY[idx+1])
    if(length(calc1Val)!=0){
      cam2Df[idx,1] <- paste0("1000","-","1250")
      cam2Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam2Df[idx,3] <- length(calc1Val$v2)/sum(calc1Val$v2)
    }else {
      cam2Df[idx,1] <- paste0("1000","-","1250")
      cam2Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam2Df[idx,3] <- NA
    }
  }
  for(idx in 1:(length(breaksY)-1)){
    calc1Val <- subset(maindataDF,maindataDF$XEngSpd >= 1250 & maindataDF$XEngSpd < 1500 & maindataDF$XEngTor>= breaksY[idx] & maindataDF$XEngTor < breaksY[idx+1])
    if(length(calc1Val)!=0){
      cam3Df[idx,1] <- paste0("1250","-","1500")
      cam3Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam3Df[idx,3] <- length(calc1Val$v2)/sum(calc1Val$v2)
    }else {
      cam3Df[idx,1] <- paste0("1250","-","1500")
      cam3Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam3Df[idx,3] <- NA
    }
  }
  for(idx in 1:(length(breaksY)-1)){
    calc1Val <- subset(maindataDF,maindataDF$XEngSpd >= 1500 & maindataDF$XEngSpd < 1750 & maindataDF$XEngTor>= breaksY[idx] & maindataDF$XEngTor < breaksY[idx+1])
    if(length(calc1Val)!=0){
      cam4Df[idx,1] <- paste0("1500","-","1750")
      cam4Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam4Df[idx,3] <- length(calc1Val$v2)/sum(calc1Val$v2)
    }else {
      cam4Df[idx,1] <- paste0("1500","-","1750")
      cam4Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam4Df[idx,3] <- NA
    }
  }
  for(idx in 1:(length(breaksY)-1)){
    calc1Val <- subset(maindataDF,maindataDF$XEngSpd >= 1750 & maindataDF$XEngSpd < 2000 & maindataDF$XEngTor>= breaksY[idx] & maindataDF$XEngTor < breaksY[idx+1])
    if(length(calc1Val)!=0){
      cam5Df[idx,1] <- paste0("1750","-","2000")
      cam5Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam5Df[idx,3] <- length(calc1Val$v2)/sum(calc1Val$v2)
    }else {
      cam5Df[idx,1] <- paste0("1750","-","2000")
      cam5Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam5Df[idx,3] <- NA
    }
  }
  for(idx in 1:(length(breaksY)-1)){
    calc1Val <- subset(maindataDF,maindataDF$XEngSpd >= 2000 & maindataDF$XEngSpd < 2250 & maindataDF$XEngTor>= breaksY[idx] & maindataDF$XEngTor < breaksY[idx+1])
    if(length(calc1Val)!=0){
      cam6Df[idx,1] <- paste0("2000","-","2250")
      cam6Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam6Df[idx,3] <- length(calc1Val$v2)/sum(calc1Val$v2)
    }else {
      cam6Df[idx,1] <- paste0("2000","-","2250")
      cam6Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam6Df[idx,3] <- NA
    }
  }
  for(idx in 1:(length(breaksY)-1)){
    calc1Val <- subset(maindataDF,maindataDF$XEngSpd >= 2250 & maindataDF$XEngSpd < 2500 & maindataDF$XEngTor>= breaksY[idx] & maindataDF$XEngTor < breaksY[idx+1])
    if(length(calc1Val)!=0){
      cam7Df[idx,1] <- paste0("2250","-","2500")
      cam7Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam7Df[idx,3] <- length(calc1Val$v2)/sum(calc1Val$v2)
    }else {
      cam7Df[idx,1] <- paste0("2250","-","2500")
      cam7Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam7Df[idx,3] <- NA
    }
  }
  for(idx in 1:(length(breaksY)-1)){
    calc1Val <- subset(maindataDF,maindataDF$XEngSpd >= 2500 & maindataDF$XEngSpd < 2750 & maindataDF$XEngTor>= breaksY[idx] & maindataDF$XEngTor < breaksY[idx+1])
    if(length(calc1Val)!=0){
      cam8Df[idx,1] <- paste0("2500","-","2750")
      cam8Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam8Df[idx,3] <- length(calc1Val$v2)/sum(calc1Val$v2)
    }else {
      cam8Df[idx,1] <- paste0("2500","-","2750")
      cam8Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam8Df[idx,3] <- NA
    }
  }
  for(idx in 1:(length(breaksY)-1)){
    calc1Val <- subset(maindataDF,maindataDF$XEngSpd >= 2750 & maindataDF$XEngSpd < 3000 & maindataDF$XEngTor>= breaksY[idx] & maindataDF$XEngTor < breaksY[idx+1])
    if(length(calc1Val)!=0){
      cam9Df[idx,1] <- paste0("2750","-","3000")
      cam9Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam9Df[idx,3] <- length(calc1Val$v2)/sum(calc1Val$v2)
    }else {
      cam9Df[idx,1] <- paste0("2750","-","3000")
      cam9Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam9Df[idx,3] <- NA
    }
  }
  for(idx in 1:(length(breaksY)-1)){
    calc1Val <- subset(maindataDF,maindataDF$XEngSpd >= 3000 & maindataDF$XEngSpd < 3250 & maindataDF$XEngTor>= breaksY[idx] & maindataDF$XEngTor < breaksY[idx+1])
    if(length(calc1Val)!=0){
      cam10Df[idx,1] <- paste0("3000","-","3250")
      cam10Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam10Df[idx,3] <- round((mean(calc1Val$XVehSpd)/3600)*nrow(calc1Val),2)
    }else {
      cam10Df[idx,1] <- paste0("3000","-","3250")
      cam10Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam10Df[idx,3] <- NA
    }
  }
  for(idx in 1:(length(breaksY)-1)){
    calc1Val <- subset(maindataDF,maindataDF$XEngSpd >= 3250 & maindataDF$XEngSpd < 3500 & maindataDF$XEngTor>= breaksY[idx] & maindataDF$XEngTor < breaksY[idx+1])
    if(length(calc1Val)!=0){
      cam11Df[idx,1] <- paste0("3250","-","3500")
      cam11Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam11Df[idx,3] <- length(calc1Val$v2)/sum(calc1Val$v2)
    }else {
      cam11Df[idx,1] <- paste0("3250","-","3500")
      cam11Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam11Df[idx,3] <- NA
    }
  }
  for(idx in 1:(length(breaksY)-1)){
    calc1Val <- subset(maindataDF,maindataDF$XEngSpd >= 3500 & maindataDF$XEngSpd < 3750 & maindataDF$XEngTor>= breaksY[idx] & maindataDF$XEngTor < breaksY[idx+1])
    if(length(calc1Val)!=0){
      cam12Df[idx,1] <- paste0("3500","-","3750")
      cam12Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam12Df[idx,3] <- length(calc1Val$v2)/sum(calc1Val$v2)
    }else {
      cam12Df[idx,1] <- paste0("3500","-","3750")
      cam12Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam12Df[idx,3] <- NA
    }
  }
  for(idx in 1:(length(breaksY)-1)){
    calc1Val <- subset(maindataDF,maindataDF$XEngSpd >= 3750 & maindataDF$XEngSpd < 4000 & maindataDF$XEngTor>= breaksY[idx] & maindataDF$XEngTor < breaksY[idx+1])
    if(length(calc1Val)!=0){
      cam13Df[idx,1] <- paste0("3750","-","4000")
      cam13Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam13Df[idx,3] <- length(calc1Val$v2)/sum(calc1Val$v2)
    }else {
      cam13Df[idx,1] <- paste0("3750","-","4000")
      cam13Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam13Df[idx,3] <- NA
    }
  }
  for(idx in 1:(length(breaksY)-1)){
    calc1Val <- subset(maindataDF,maindataDF$XEngSpd >= 4000 & maindataDF$XEngSpd < Inf & maindataDF$XEngTor>= breaksY[idx] & maindataDF$XEngTor < breaksY[idx+1])
    if(length(calc1Val)!=0){
      cam14Df[idx,1] <- paste0("4000","-","Inf")
      cam14Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam14Df[idx,3] <- length(calc1Val$v2)/sum(calc1Val$v2)
    }else {
      cam14Df[idx,1] <- paste0("4000","-","Inf")
      cam14Df[idx,2] <- paste0(breaksY[idx],"-",breaksY[idx+1])
      cam14Df[idx,3] <- NA
    }
  }
  ##rbinding it
  #plotdf1 <- rbind(cam1Df,cam2Df,cam3Df,cam4Df,cam5Df,cam6Df,cam7Df,cam8Df,cam9Df,cam10Df,cam11Df)
  
  #counts <- x2
  plotdf1_5 <- rbind(cam1Df,cam2Df,cam3Df,cam4Df,cam5Df,cam6Df,cam7Df,cam8Df,cam9Df,cam10Df,cam11Df,cam12Df,cam13Df,cam14Df)
  
  #plotdf1_5$value <- plotdf1_5$value/3600
  
  ##reshaping data
  library(reshape2)
  plotdf2_5 <- reshape(plotdf1_5,idvar = "engTrq",timevar = "engbreaks",ids = "value",direction = "wide")
  #print(plotdf2_5)
  rownames(plotdf2_5) <- plotdf2_5[,1]
  plotdf2_5<- plotdf2_5[,2:ncol(plotdf2_5)]
  plotdf2_5[plotdf2_5 == "NaN"] <- NA
  plotdf2_5[is.na(plotdf2_5)] <- NA
  
  x2<- as.matrix(plotdf2_5)
  #x2<-x2/3600
  countMatrix <- x2
  
  countMatrixRev <- countMatrix[seq(dim(countMatrix)[1],1,-1),]
  countMatrixRev[is.na(countMatrixRev)] <- 0
  counts <- countMatrixRev
  countsPerc <- counts/sum(counts)*100
  counts[countMatrixRev==0] <- NA
  
  if(brXExactVals){
    classNamesX <- as.character(breaksX)
  }else{
    classNamesX <- vector("character",length(breaksX)-1)
    for(idx in 1:(length(breaksX)-1)){
      if(!is.infinite(breaksX[idx]) & !is.infinite(breaksX[idx+1])){
        classNamesX[idx] <- paste0(breaksX[idx],"-",breaksX[idx+1])
      }else{
        if(is.infinite(breaksX[idx])){
          classNamesX[idx] <- paste0("<",breaksX[idx+1])
        }else{
          classNamesX[idx] <- paste0("\u2265",breaksX[idx])
        }
      }
    }
  }
  if(brYExactVals){
    classNamesY <- rev(as.character(breaksY))
  }else{
    classNamesY <- vector("character",length(breaksY)-1)
    for(idy in 1:(length(breaksY)-1)){
      if(!is.infinite(breaksY[idy]) & !is.infinite(breaksY[idy+1])){
        classNamesY[idy] <- paste0(breaksY[idy],"-",breaksY[idy+1])
      }else{
        if(is.infinite(breaksY[idy])){
          classNamesY[idy] <- paste0("<",breaksY[idy+1])
        }else{
          classNamesY[idy] <- paste0("\u2265",breaksY[idy])
        }
      }
    }
    classNamesY <- rev(classNamesY)
  }
  
  if(unit==""){
    cellnote <- matrix(paste0(sprintf("%.0f",counts),"\n (",sprintf("%.1f",countsPerc),"%)"),length(breaksY)-1+brYExactVals,length(breaksX)-1+brXExactVals)
  }else{
    cellnote <- matrix(paste0(sprintf("%.1f",counts)," ",unit,"\n (",sprintf("%.1f",countsPerc),"%)"),length(breaksY)-1+brYExactVals,length(breaksX)-1+brXExactVals)
  }
  cellnote[is.na(counts)] <- ""
  counts2 <- counts
  counts2[sprintf("%.1f",countsPerc)=="0.0"] <- NA
  notecol <- rep("white",prod(dim(counts2)))
  notecol[is.na(sapply(t(counts2[seq(dim(counts2)[1],1,-1),]),c))] <- "grey60"
  
  coloPan <- colorpanel(n=1000,low="cornflowerblue",mid="chartreuse2",high="firebrick1")
  title=paste("Durchschnittliche Verweildauer im ZAS-Betrieb pro Klasse: insgesamt",sprintf("%.0f",mean(x2,na.rm=TRUE)),"s",title)
  
  if(any(!is.na(counts))){
    if(save){
      png(paste0(plotPath,plotName,".png"),width=1200,height=600)
    }
    par(cex.main=cexMain)
    heatmap.2(counts2,cellnote=cellnote,Rowv=FALSE,Colv=FALSE,
              colsep=c(1:(length(breaksX)-1+brXExactVals)),rowsep=c(1:(length(breaksY)-1+brYExactVals)),
              sepwidth=c(0.01,0.01),dendrogram="none",notecex=cexNote,cexRow=cexAx,cexCol=cexAx,
              lmat=rbind(c(0,3,4), c(2,1,0),c(0,0,0)),lwid=c(0.1,10,1.6),lhei=c(0.04,0.25,0.035),
              key=TRUE, key.title = NA, key.ylab = NA, density.info = "none", key.xlab = NA,
              main=title,notecol=notecol,trace="none",labRow=classNamesY,labCol=classNamesX,col=coloPan,na.color="grey90")
    grid.text(xLab,x=unit(0.5,"npc"),y=unit(0.005,"npc"),just=c("center","bottom"),gp=gpar(cex=cexLab))
    grid.text(yLab,x=unit(0.92,"npc"),y=unit(0.5,"npc"),just=c("center","center"),rot = 90, gp=gpar(cex=cexLab))
    # mtext(4,text=yLab,line=-1,cex=cexLab)
    mtext(3,text=subtitle,line=-0.25,cex=cexLab)
    grid.text(subsubtitle,x=unit(0.00,"npc"),y=unit(0.005,"npc"),just=c("left","bottom"),gp=gpar(cex=cexSub))
    grid.text("Daten in Stunden",x=unit(0.94,"npc"),y=unit(0.86,"npc"),gp=gpar(cex=1.1,fontface=2))
    if(save){
      dev.off()
    }
  }
}

##camtronic6
plotHeatmapCam6 <- function(plotPath,plotName,x,xLab,yLab,title="",subtitle="",subsubtitle="",save=TRUE){
  #x<-as.data.frame(x)
  library(ggplot2)
  print(x)
  nba.m <- melt(x)
  colnames(nba.m) <-c ("VON","NACH","value")
  nba.m[,3][nba.m[,3] == 0]<-NA
  black.bold.text <- element_text(face = "bold", color = "black",debug = FALSE,size=12)
  print(nba.m)
  if(save){
    png(paste0(plotPath,plotName,".png"),width=1200,height=650)
  }
  black.bold.text <- element_text(face = "bold", color = "black",debug = FALSE,size=15)
  
  p<-ggplot(data = nba.m, aes(y=VON, x=NACH)) + 
    geom_tile(aes(fill = value),colour = "white",size = 0.5)+
    geom_text(aes(label = ifelse(is.na(value),"none",paste0(round(value, 1),"\n","(",round((value/sum(value,na.rm = T))*100,2),"%",")"))),size = 7) +
    labs(x="NACH", caption = "0 ->'no Cylinder Cutoff \n 1 -> 'Cylinder cutoff request(ecm internal)' \n 2-> 'Cylinder cutoff shifting' \n 3-> 'Cylinder Cutoff active'",y =  "VON", title =paste0("ZAS-Schaltungsarten pro Klasse: insgesamt ",sum(nba.m$value,na.rm = T)))+
    scale_fill_gradient2(low = "cornflowerblue", high = "firebrick1", mid = "chartreuse2",na.value = "grey60")+
    # scale_x_continuous(sec.axis = dup_axis()) + scale_y_continuous(sec.axis = dup_axis())
    scale_x_continuous(position = "top")+
    scale_y_reverse()+
    theme(plot.margin = unit(c(0.1,0.3,0.1,0.3), "inch"),
          panel.background = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.title.x = black.bold.text, axis.title.y = black.bold.text , 
          plot.title = element_text(face = "bold", color = "black",size = 14, 
                                    vjust = 0.5,hjust = 0.5),plot.caption = element_text(face = "plain",color = "black", size = 12,vjust = 0,hjust = 0),
          axis.text.x = element_text(size = 12,face = "plain"),axis.text.y = element_text(size = 12,face = "plain"),
          axis.ticks = element_blank())#+
  
  print(p)
  if(save){
    dev.off()
    #ggsave(filename = paste0(plotPath,plotName),plot = p,width = 15,height = 7)
  }
  
}

plotEnergyBalance <- function(plotPath,plotName,x,breaksX,breaksY,xLabel,yLabel,pTitle,save){
  
  ##calculating the data
  
  breaks <- breaksX
  #####classNames<-NULL
  splitStr<-""
  splitClassNames<-FALSE
  unit<-NULL
  y<-NULL
  absTxt<-FALSE
  txtForm<-rep("%.0f",length(breaks))
  
  txtForm[!is.infinite(breaks) & abs(breaks-round(breaks,1))>1e-5] <- "%.2f"
  classNames <- vector("character",length(breaks)-1)
  for(idx in 1:(length(breaks)-1)){
    if(!is.infinite(breaks[idx]) & !is.infinite(breaks[idx+1])){
      classNames[idx] <- paste0(sprintf(txtForm[idx],breaks[idx]),"-",splitStr,sprintf(txtForm[idx+1],breaks[idx+1]))
    }else{
      if(is.infinite(breaks[idx])){
        classNames[idx] <- paste0("<",splitStr,sprintf(txtForm[idx+1],breaks[idx+1]))
      }else{
        classNames[idx] <- paste0("\u2265",splitStr,sprintf(txtForm[idx],breaks[idx]))
      }
    }
  }
  ### Boosten Calculation
  CalcDF <- x
  XBoosten <- subset(CalcDF$XRSGPwr,CalcDF$XRSGPwr>0)
  XEngSpdBoosten <- subset(CalcDF$XIsgRsgSpd,CalcDF$XRSGPwr>0)
  
  boostenDF <- data.frame(cbind(XEngSpdBoosten,XBoosten))
  y_mean <- vector("numeric",length = length(breaks)-1)
  y_count <- vector("numeric",length = length(breaks)-1)
  y_prod <- vector("numeric",length = length(breaks)-1)
  for(idx in 1:length(breaks)-1)
  {
    y_mean[idx]<-mean(boostenDF$XBoosten[which(boostenDF$XEngSpdBoosten>=breaks[idx] & boostenDF$XEngSpdBoosten<breaks[idx+1])])  
    y_count[idx]<-length(boostenDF$XBoosten[boostenDF$XEngSpdBoosten>=breaks[idx] & boostenDF$XEngSpdBoosten<breaks[idx+1]])/3600
    y_prod[idx] <- y_mean[idx] * y_count[idx]
  }
  
  plotboostDF <- data.frame(classNames,y_mean,y_count,y_prod)
  ##End of boosten
  
  ##Generator Calculation
  XRSGPwrG <- subset(CalcDF$XRSGPwr,CalcDF$XRSGPwr<=0 &  CalcDF$XRSGPwr> -3)
  XEngSpdGene <- subset(CalcDF$XIsgRsgSpd,CalcDF$XRSGPwr<=0 & CalcDF$XRSGPwr>-3)
  CalcGDF <- data.frame(cbind(XRSGPwrG,XEngSpdGene))
  
  g_mean <- vector("numeric",length = length(breaks)-1)
  g_count <- vector("numeric",length = length(breaks)-1)
  g_prod <- vector("numeric",length = length(breaks)-1)
  
  for(idx in 1:length(breaks)-1)
  {
    g_mean[idx]<-mean(CalcGDF$XRSGPwrG[which(CalcGDF$XEngSpdGene>=breaks[idx] & CalcGDF$XEngSpdGene<breaks[idx+1])])  
    g_count[idx]<-length(CalcGDF$XRSGPwrG[CalcGDF$XEngSpdGene>=breaks[idx] & CalcGDF$XEngSpdGene<breaks[idx+1]])/3600
    g_prod[idx] <- g_mean[idx] * g_count[idx]
  }
  
  genPlotDF1 <- data.frame(classNames,g_mean,g_count,g_prod)
  ##End of Generator Calculation
  ##Rekuperation_calculation###
  XRSGPwrR <- subset(CalcDF$XRSGPwr,CalcDF$XRSGPwr<=-3)
  XEngSpdRekup <- subset(CalcDF$XIsgRsgSpd,CalcDF$XRSGPwr<=-3)
  
  #CalcRDF <- data.frame(cbind(XEngPwrR,XRSGPwrR,XEngSpdRekup))
  CalcRDF <- data.frame(cbind(XRSGPwrR,XEngSpdRekup))
  
  r_mean <- vector("numeric",length = length(breaks)-1)
  r_count <- vector("numeric",length = length(breaks)-1)
  r_prod <- vector("numeric",length = length(breaks)-1)
  for(idx in 1:length(breaks)-1)
  {
    r_mean[idx]<- mean(CalcRDF$XRSGPwrR[which(CalcRDF$XEngSpdRekup>=breaks[idx] & CalcRDF$XEngSpdRekup<breaks[idx+1])])  
    r_count[idx]<-length(CalcRDF$XRSGPwrR[CalcRDF$XEngSpdRekup>=breaks[idx] & CalcRDF$XEngSpdRekup<breaks[idx+1]])/3600
    r_prod[idx] <- r_mean[idx]* r_count[idx]
  }
  
  RekupPlotDF <- data.frame(classNames,r_mean,r_count,r_prod)
  ##end of Rekuperation_calculation##
  ##End of data calculation
  mainPlotdf<- data.frame(classNames,plotboostDF$y_prod,genPlotDF1$g_prod,RekupPlotDF$r_prod)
  colnames(mainPlotdf) <-c("class","Boosten","Generator","Rekuperation")
  ##mainPlotting
  mainPlotdf[mainPlotdf == "NaN"] <- NA
  library(ggplot2)
  library(ggalt)
  black.bold.text <- element_text(face = "bold", color = "black",debug = FALSE,size=15)
  # if(save){
  #   png(paste0(plotPath,plotName,".png"),width=1200,height=650)
  # }
  pSubtitle <- "Conditions for RSG :    RSG power => 0kW -> Boost mode,        RSG power 0 to -3kW => Generator mode,         RSG power < -3kW => Recuperation mode"
  
  finalPlot <- ggplot(mainPlotdf,aes(x = mainPlotdf$class,group = 1))+
    geom_point(aes(y=Boosten,colour = "Boosten"),size = 2)+
    ggalt::geom_xspline(aes(y=Boosten,colour = "Boosten"),size = 1)+
    geom_point(aes(y=Generator,colour = "Generator"),size = 2)+
    ggalt::geom_xspline(aes(y=Generator,colour = "Generator"),size = 1)+
    geom_point(aes(y=Rekuperation,colour = "Rekuperation"),size = 2)+
    ggalt::geom_xspline(aes(y=Rekuperation,colour = "Rekuperation"),size = 1)+
    scale_x_discrete(limits = levels(factor(mainPlotdf$class,levels = unique(mainPlotdf$class))))+
    #ylim(-1200,500)+
    #scale_y_continuous(breaks =breaksY)+
    scale_y_continuous(breaks = c(seq(min(breaksY),max(breaksY),diff(breaksY)[1])),limits = c(min(breaksY),max(breaksY)))+ ## Use this scale for fixed y-axis
    #scale_y_continuous(breaks = c(seq(min(breaksY),max(breaksY),diff(breaksY)[1])))+ ## use this scale for dynamic y-axis
    #scale_y_continuous(breaks = waiver())+
    # scale_y_continuous(limits= c(-1200,500),breaks = c(seq(-1200,500,100)))+
    labs(x=xLabel, caption = pSubtitle,y =  yLabel, title =pTitle)+
    guides(fill=guide_legend(nrow=1,byrow=TRUE))+
    theme(plot.margin = unit(c(0.1,0.3,0.1,0.3), "inch"),panel.background = element_rect(fill = "#e7ebf5"),panel.border = element_rect(color = "black", fill = NA) ,
          axis.title.x = black.bold.text, axis.title.y = black.bold.text , plot.title = element_text(face = "bold", color = "black",size = 15, vjust = 0.5,hjust = 0.5),plot.caption = element_text(face = "plain",color = "black", size = 7,vjust = 0,hjust = 0),axis.text.x = element_text(size = 14,face = "plain",angle=90),axis.text.y = element_text(size = 14,face = "plain"),legend.position=c(0.939,0.894),
          legend.text = element_text(size=12),legend.title=element_text(size=12))+
    scale_colour_manual("Legend",values = c(Boosten = 'darkgreen',Generator = 'orange',Rekuperation = 'blue'))
  
  #print(finalPlot)
  if(save){
    ggsave(paste0(plotPath,plotName,".png"),plot = finalPlot,scale = 2,height = 3.5,width = 7)
    #dev.off()
  }
  else{
    print(finalPlot)
  }
  ##output table to image
  sumb <- round(sum(mainPlotdf$Boosten,na.rm = TRUE),2)
  sumG <- round(sum(mainPlotdf$Generator,na.rm = TRUE),2)
  sumR <- round(sum(mainPlotdf$Rekuperation,na.rm = TRUE),2)
  testplot1 <- data.frame(stringsAsFactors = FALSE)
  r1 <- c("Sum",sumb,sumG,sumR)
  #testplot1[16,1]<-"SUM"
  testplot1 <- NULL
  
  mainPlotdf1<- mainPlotdf
  
  mainPlotdf1$class <- as.character(mainPlotdf1$class)
  mainPlotdf1$Boosten <- round(mainPlotdf1$Boosten,2)
  mainPlotdf1$Generator <- round(mainPlotdf1$Generator,2)
  mainPlotdf1$Rekuperation <- round(mainPlotdf1$Rekuperation,2)
  testplot1 <- rbind(mainPlotdf1,r1)
  
  #testplot1[16,1] <- "SUM"
  
  
  colnames(testplot1) <- c("Class Names","Boosten","Generator","Rekuperation")
  SNO <- 1:nrow(testplot1)
  testplot1<-cbind(SNO,testplot1)
  if(save){
    
    png(paste0(plotPath,"EnergybalanceTable",".png"),width=1000,height=600)
    grid.table(testplot1,rows = NULL)
    #dev.off()
  }
  
  if(save){
    dev.off()
  }
}

preCalcBarsPerTrack <- function(x,track,useTrack=NULL,distance=NULL,unit=NULL,numFloat=0){
  if(!is.null(useTrack)){
    isUseTrack <- as.logical(sapply(track,function(x){x %in% useTrack}))
    track <- track[isUseTrack]
    x <- x[isUseTrack]
    distance <- distance[isUseTrack]
  }
  if(is.null(unit)){
    unit <- ""
  }else{
    if(unit!="%"){
      unit <- paste0(" ",unit)
    }
  }
  numTracks <- length(unique(track))
  xPerTrack <- tapply(x,track,FUN=sum)
  xPercPerTrack <- xPerTrack/sum(xPerTrack)*100
  xTxt <- paste0(sprintf(paste0("%.",numFloat,"f"),xPerTrack),unit)
  xTxt0 <- paste0(sprintf("%.0f",xPerTrack),unit)
  xTxt[xPerTrack==0] <- xTxt0[xPerTrack==0]
  if(sum(x,na.rm=TRUE)>0){
    xPercTxt <- paste0(sprintf(paste0("%.",as.numeric(xPerTrack>0),"f"),xPercPerTrack),"%")
  }else{
    xPercTxt <- rep("- %",length(xPerTrack))
  }
  rValue <- list(x=x,track=track,distance=distance,numTracks=numTracks,xPerTrack=xPerTrack,xPercPerTrack=xPercPerTrack,xTxt=xTxt,xPercTxt=xPercTxt)
  return(rValue)
}

plotBarsPerTrack1 <- function(plotPath,plotName,x,track,useTrack=NULL,distance=NULL,unit=NULL,unitPerDist="",yLab="",title="",subtitle="",percTxt=TRUE,numFloat=0,cexMain=1.4,cexLab=1.3,cexAx=1.0,cexSub=1.1,save=TRUE){
  library("plotrix")
  library("epade")
  library("RColorBrewer")
  library("grid")
  library("TeachingDemos")
  
  if(!save){
    cexMain=1
    cexLab=1
    cexAx=1
    cexSub=1
  }
  preCalc <- preCalcBarsPerTrack(x,track,useTrack,distance,unit,numFloat)
  
  ##changes done on 28/8/2017 for track aesthetics
  if(preCalc$numTracks < 8){
    
    if(save){
      png(paste0(plotPath,plotName,".png"),width=1000,height=600)
    }
    par(cex.main=cexMain,cex.lab=cexLab,cex.axis=cexAx)
    bar.plot.wtd(x=preCalc$track,w=preCalc$x,wall=4,b=0.7,form="c",b2=1,main=title,xlab="Streckenart",ylab=yLab,col=brewer.pal(8,"Dark2"))
    if(percTxt){
      cexTxt <- ifelse(save,1.2,1)
      text(1:preCalc$numTracks,preCalc$xPerTrack+0.06*max(preCalc$xPerTrack),paste0(preCalc$xTxt,"\n (",preCalc$xPercTxt,")"),cex=cexTxt,col="darkblue")
    }else{
      cexTxt <- ifelse(save,1.5-0.2*(preCalc$numTracks>7),1)
      text(1:preCalc$numTracks,preCalc$xPerTrack+0.06*max(preCalc$xPerTrack),preCalc$xTxt,cex=cexTxt,col="darkblue")
    }
    if(!is.null(distance)){
      distPerTrack <- tapply(preCalc$distance,preCalc$track,FUN=sum)
      shadTxt <- paste(sprintf("%.1f",preCalc$xPerTrack/distPerTrack),unitPerDist)
      shadTxt[preCalc$xPerTrack==0] <- ""
      cexShadTxt <- ifelse(save,1.25-0.25*(preCalc$numTracks>7),1)
      shadowtext((1:length(unique(preCalc$track)))-0.075,rep(0.0075*max(preCalc$xPerTrack),preCalc$numTracks),shadTxt,cex=cexShadTxt,col="white",r=0.05)
    }
    grid.text(subtitle,x=unit(0.00,"npc"),y=unit(0.005,"npc"),just=c("left","bottom"),gp=gpar(cex=cexSub)) 
    if(save){
      dev.off()
    }
  }
  else{
    if(!save){
      cexMain=1
      cexLab=1
      cexAx=0.7
      cexSub=1
    }
    preCalc <- preCalcBarsPerTrack(x,track,useTrack,distance,unit,numFloat)
    
    if(save){
      png(paste0(plotPath,plotName,".png"),width=1000,height=600)
    }
    par(cex.main=cexMain,cex.lab=cexLab,cex.axis=cexAx)
    bar.plot.wtd(x=preCalc$track,w=preCalc$x,wall=4,b=0.7,form="c",b2=1,main=title,xlab="Streckenart",ylab=yLab,col=brewer.pal(11,"Paired"))
    if(percTxt){
      cexTxt <- ifelse(save,1.0,1)
      text(1:preCalc$numTracks,preCalc$xPerTrack+0.06*max(preCalc$xPerTrack),paste0(preCalc$xTxt,"\n (",preCalc$xPercTxt,")"),cex=cexTxt,col="darkblue")
    }else{
      cexTxt <- ifelse(save,1.2-0.2*(preCalc$numTracks>7),1)
      text(1:preCalc$numTracks,preCalc$xPerTrack+0.06*max(preCalc$xPerTrack),preCalc$xTxt,cex=cexTxt,col="darkblue")
    }
    if(!is.null(distance)){
      distPerTrack <- tapply(preCalc$distance,preCalc$track,FUN=sum)
      shadTxt <- paste(sprintf("%.1f",preCalc$xPerTrack/distPerTrack),unitPerDist)
      shadTxt[preCalc$xPerTrack==0] <- ""
      cexShadTxt <- ifelse(save,1.225-0.25*(preCalc$numTracks>7),1)
      shadowtext((1:length(unique(preCalc$track)))-0.075,rep(0.0075*max(preCalc$xPerTrack),preCalc$numTracks),shadTxt,cex=cexShadTxt,col="black",bg="white",r=0.05)
    }
    grid.text(subtitle,x=unit(0.00,"npc"),y=unit(0.005,"npc"),just=c("left","bottom"),gp=gpar(cex=cexSub)) 
    if(save){
      dev.off()
    }
  }
}

plotBarsPerTrack2 <- function(plotPath,plotName,x,nx,nameX,nameNx="restliche Betriebszeit**",colX="#377EB8",colNx="#FFFFFF00",track,useTrack=NULL,unit=NULL,yLab="",title="",subtitle="",cexMain=1.4,cexLab=1.3,cexAx=1.0,cexSub=1.1,save=TRUE){
  library("plotrix")
  library("epade")
  library("grid")
  library("TeachingDemos")
  
  if(!save){
    cexMain=1
    cexLab=1
    cexAx=1
    cexSub=1
  }
  
  preCalc <- preCalcBarsPerTrack(x,track,useTrack,NULL,unit)
  preCalc$nx <- preCalcBarsPerTrack(nx,track,useTrack)$x
  preCalc$nxPerTrack <- preCalcBarsPerTrack(nx,track,useTrack)$xPerTrack
  categ <- factor(c(rep(nameX,length(preCalc$x)),rep(nameNx,length(preCalc$nx))),c(nameX,nameNx))
  nameX2 <- regmatches(nameX,regexpr("[A-Za-z]+",nameX))
  
  ##changes done on 28-8-2017 
  if(preCalc$numTracks < 8){
    if(save){
      png(paste0(plotPath,plotName,".png"),width=1000,height=600)
    }
    par(cex.main=cexMain,cex.lab=cexLab,cex.axis=cexAx)
    bar.plot.wtd(x=categ,y=rep(preCalc$track,2),w=c(preCalc$x,preCalc$nx),wall=4,b=0.7,form="c",b2=1,main=title,xlab="Streckenart",ylab=yLab,col=c(colX,colNx),beside=FALSE)#"#66A61E"
    text(1:preCalc$numTracks,preCalc$xPerTrack+0.0025*max(preCalc$xPerTrack+preCalc$nxPerTrack),paste0(sprintf("%.1f",100/(1+preCalc$nxPerTrack/preCalc$xPerTrack)),"%"),col="white")
    text(0.5,1.1*max(preCalc$xPerTrack+preCalc$nxPerTrack),paste0(nameX2," pro Strecke mit Anteil an Gesamt",tolower(nameX2),":"),col="black",cex=ifelse(save,1.3,1),pos=4)
    shadowtext(1:preCalc$numTracks,rep(1.05*max(preCalc$xPerTrack+preCalc$nxPerTrack),preCalc$numTracks),paste0(preCalc$xTxt,"   (",preCalc$xPercTxt,")"),col="#377EB8",cex=ifelse(save,1.2,0.8),bg="grey60",r=0.03)
    grid.text(subtitle,x=unit(0.00,"npc"),y=unit(0.005,"npc"),just=c("left","bottom"),gp=gpar(cex=cexSub))
    if(save){
      dev.off()
    }
  }else{
    if(!save){
      cexMain=1
      cexLab=1
      cexAx=0.7
      cexSub=1
    }
    
    if(save){
      png(paste0(plotPath,plotName,".png"),width=1000,height=600)
    }
    par(cex.main=cexMain,cex.lab=cexLab,cex.axis=cexAx)
    bar.plot.wtd(x=categ,y=rep(preCalc$track,2),w=c(preCalc$x,preCalc$nx),wall=4,b=0.7,form="c",b2=1,main=title,xlab="Streckenart",ylab=yLab,col=c(colX,colNx),beside=FALSE)#"#66A61E"
    text(1:preCalc$numTracks,preCalc$xPerTrack+0.0025*max(preCalc$xPerTrack+preCalc$nxPerTrack),paste0(sprintf("%.1f",100/(1+preCalc$nxPerTrack/preCalc$xPerTrack)),"%"),col="white")
    text(0.5,1.1*max(preCalc$xPerTrack+preCalc$nxPerTrack),paste0(nameX2," pro Strecke mit Anteil an Gesamt",tolower(nameX2),":"),col="black",cex=ifelse(save,1.3,1),pos=4)
    shadowtext(1:preCalc$numTracks,rep(1.05*max(preCalc$xPerTrack+preCalc$nxPerTrack),preCalc$numTracks),paste0(preCalc$xTxt,"   (",preCalc$xPercTxt,")"),col="#377EB8",cex=ifelse(save,0.95,0.8),bg="grey60",r=0.03)
    grid.text(subtitle,x=unit(0.00,"npc"),y=unit(0.005,"npc"),just=c("left","bottom"),gp=gpar(cex=cexSub))
    if(save){
      dev.off()
    }
  }
}

plotBarsPerTrack3 <- function(plotPath,plotName,x,y,nameX,nameY,colX="#377EB8",colY="#66A61E",track,useTrack=NULL,unit=NULL,yLab="",title="",subtitle="",cexMain=1.4,cexLab=1.3,cexAx=1.0,cexSub=1.1,save=TRUE){
  library("plotrix")
  library("epade")
  library("grid")
  library("TeachingDemos")
  
  if(!save){
    cexMain=1
    cexLab=1
    cexAx=1
    cexSub=1
  }
  
  preCalc <- preCalcBarsPerTrack(x,track,useTrack,NULL,unit)
  preCalc2 <- preCalcBarsPerTrack(y,track,useTrack,NULL,unit)
  preCalc$y <- preCalc2$x
  preCalc$yPerTrack <- preCalc2$xPerTrack
  preCalc$yTxt <- preCalc2$xTxt
  preCalc$yPercTxt <- preCalc2$xPercTxt
  categ <- factor(c(rep(nameX,length(preCalc$x)),rep(nameY,length(preCalc$y))),c(nameX,nameY))
  nameX2 <- regmatches(nameX,regexpr("[A-Za-z]+",nameX))
  nameY2 <- regmatches(nameY,regexpr("[A-Za-z]+",nameY))
  
  if(save){
    png(paste0(plotPath,plotName,".png"),width=1000,height=600)
  }
  par(cex.main=cexMain,cex.lab=cexLab,cex.axis=cexAx)
  bar.plot.wtd(x=categ,y=rep(preCalc$track,2),w=c(preCalc$x,preCalc$y),wall=4,b=0.7,form="c",b2=1,main=title,xlab="Streckenart",ylab=yLab,col=c(colX,colY),beside=FALSE)
  colTxt <- as.character(ifelse(preCalc$xPerTrack-0.032*max(preCalc$xPerTrack+preCalc$yPerTrack)>0,"white","black"))
  text((1:preCalc$numTracks)-0.07,preCalc$xPerTrack-0.032*max(preCalc$xPerTrack+preCalc$yPerTrack),paste0(sprintf("%.1f",100/(1+preCalc$yPerTrack/preCalc$xPerTrack)),"%"),col=colTxt)
  text((1:preCalc$numTracks)-0.07,preCalc$xPerTrack+0.003*max(preCalc$xPerTrack+preCalc$yPerTrack),paste0(sprintf("%.1f",100/(1+preCalc$xPerTrack/preCalc$yPerTrack)),"%"),col="white")
  text(0.5,1.1*max(preCalc$xPerTrack+preCalc$yPerTrack),paste0(nameX2," und ",nameY2," pro Strecke mit Anteil an Gesamt",tolower(nameX2)," bzw. Gesamt",tolower(nameY2),":"),col="black",cex=ifelse(save,1.3,1),pos=4)
  shadowtext(1:preCalc$numTracks,rep(1.03*max(preCalc$xPerTrack+preCalc$yPerTrack),preCalc$numTracks),paste0(preCalc$xTxt,"   (",preCalc$xPercTxt,")"),col=colX,cex=0.9,bg="grey60",r=0.03)
  shadowtext(1:preCalc$numTracks,rep(1.06*max(preCalc$xPerTrack+preCalc$yPerTrack),preCalc$numTracks),paste0(preCalc$yTxt,"   (",preCalc$yPercTxt,")"),col=colY,cex=0.9,bg="grey60",r=0.03)
  grid.text(subtitle,x=unit(0.00,"npc"),y=unit(0.005,"npc"),just=c("left","bottom"),gp=gpar(cex=cexSub))
  if(save){
    dev.off()
  }
}

plotBarsPerTrack4 <- function(plotPath,plotName,x,y,nameX,nameY,trackX,trackY,useTrack=NULL,unit="km",yLab="",title="",subtitle="",cexMain=1.4,cexLab=1.3,cexAx=1.0,cexSub=1.1,save=TRUE){
  library("plotrix")
  library("epade")
  library("grid")
  library("TeachingDemos")
  
  if(!save){
    cexMain=1
    cexLab=1
    cexAx=1
    cexSub=1
  }
  if(length(useTrack)>=8){
    cexAx <- 0.7
    useTrack[useTrack=="Strecke 70 \n Landstr./Ãberland"] <- "Strecke 70 \n Landstr./Ãberl."
    trackX[trackX=="Strecke 70 \n Landstr./Ãberland"] <- "Strecke 70 \n Landstr./Ãberl."
    trackY[trackY=="Strecke 70 \n Landstr./Ãberland"] <- "Strecke 70 \n Landstr./Ãberl."
    useTrack[useTrack=="Strecke 93 \n Papenburg (Oval)"] <- "Strecke 93 \n Papenburg (Oval)"
    trackX[trackX=="Strecke 93 \n Papenburg (Oval)"] <- "Strecke 93 \n Papenburg (Oval)"
    trackY[trackY=="Strecke 93 \n Papenburg (Oval)"] <- "Strecke 93 \n Papenburg (Oval)"
  }
  
  trackXUni <- unique(trackX)
  trackYUni <- unique(trackY)
  preCalcX <- preCalcBarsPerTrack(c(x,vector("numeric",length(trackYUni))),c(trackX,trackYUni),useTrack,NULL,unit)
  preCalcY <- preCalcBarsPerTrack(c(y,vector("numeric",length(trackXUni))),c(trackY,trackXUni),useTrack,NULL,unit)
  z <- c(preCalcX$x,preCalcY$x)
  tra <- c(preCalcX$track,preCalcY$track)
  categ <- factor(c(rep(nameX,length(preCalcX$x)),rep(nameY,length(preCalcY$x))),c(nameX,nameY))
  txtPos1 <- 1.1*max(preCalcX$xPerTrack,preCalcY$xPerTrack)
  txtPos2 <- 1.05*max(preCalcX$xPerTrack,preCalcY$xPerTrack)
  txt <- paste0("Ist: ",sprintf("%.1f",preCalcX$xPerTrack/preCalcY$xPerTrack*100),"% von Soll")
  txt[preCalcY$xPerTrack==0] <- ""
  
  if(save){
    png(paste0(plotPath,plotName,".png"),width=1000,height=600)
  }
  
  if(length(useTrack)>=8){
    par(cex.main=cexMain,cex.lab=cexLab,cex.axis=cexAx)
    bar.plot.wtd(x=categ,y=tra,w=z,wall=4,b=0.9,form="c",b2=1,main=title,xlab="Streckenart",ylab=yLab,col=c("cornflowerblue","brown1"))
    text(0.5,txtPos1,"Ist- und Soll-Laufleistung pro Strecke mit Anteil an Gesamt-Ist- bzw. -Soll-Laufleistung:",cex=ifelse(save,1.3,1),pos=4)
    shadowtext(1:preCalcX$numTracks,rep(1.06*max(preCalcX$xPerTrack,preCalcY$xPerTrack),preCalcX$numTracks),paste0(preCalcX$xTxt,"   (",preCalcX$xPercTxt,")"),col="cornflowerblue",cex=0.7,bg="grey60",r=0.03)
    shadowtext(1:preCalcX$numTracks,rep(1.03*max(preCalcX$xPerTrack,preCalcY$xPerTrack),preCalcX$numTracks),paste0(preCalcY$xTxt,"   (",preCalcY$xPercTxt,")"),col="brown1",cex=0.7,bg="grey60",r=0.03)
    shadowtext(1:preCalcX$numTracks,preCalcX$xPerTrack+0.0025*max(preCalcX$xPerTrack,preCalcY$xPerTrack),txt,cex=0.9,col="white",r=0.05)
    grid.text(subtitle,x=unit(0.00,"npc"),y=unit(0.005,"npc"),just=c("left","bottom"),gp=gpar(cex=cexSub)) 
    if(save){
      dev.off()
    }
  }else{
    par(cex.main=cexMain,cex.lab=cexLab,cex.axis=cexAx)
    bar.plot.wtd(x=categ,y=tra,w=z,wall=4,b=0.9,form="c",b2=1,main=title,xlab="Streckenart",ylab=yLab,col=c("cornflowerblue","brown1"))
    text(0.5,txtPos1,"Ist- und Soll-Laufleistung pro Strecke mit Anteil an Gesamt-Ist- bzw. -Soll-Laufleistung:",cex=ifelse(save,1.3,1),pos=4)
    shadowtext(1:preCalcX$numTracks,rep(1.06*max(preCalcX$xPerTrack,preCalcY$xPerTrack),preCalcX$numTracks),paste0(preCalcX$xTxt,"   (",preCalcX$xPercTxt,")"),col="cornflowerblue",cex=0.9,bg="grey60",r=0.03)
    shadowtext(1:preCalcX$numTracks,rep(1.03*max(preCalcX$xPerTrack,preCalcY$xPerTrack),preCalcX$numTracks),paste0(preCalcY$xTxt,"   (",preCalcY$xPercTxt,")"),col="brown1",cex=0.9,bg="grey60",r=0.03)
    shadowtext(1:preCalcX$numTracks,preCalcX$xPerTrack+0.0025*max(preCalcX$xPerTrack,preCalcY$xPerTrack),txt,cex=0.9,col="white",r=0.05)
    grid.text(subtitle,x=unit(0.00,"npc"),y=unit(0.005,"npc"),just=c("left","bottom"),gp=gpar(cex=cexSub)) 
    if(save){
      dev.off()
    }
  }
  
}

plotBars <- function(plotPath,plotName,x,y=NULL,breaks=NULL,nameX=NULL,nameY=NULL,xLab="",yLab="Zeit [h]",title="",subtitle="",cexMain=1.4,cexLab=1.3,cexAx=1.1,cexText=1.5,cexSub=1.1,splitClassNames=FALSE,b2=1,classNames=NULL,barCol=NULL,percTxt=TRUE,absTxt=FALSE,unit=NULL,save=TRUE){
  library("plotrix")
  library("epade")
  library("grid")
  
  if(!save){
    cexMain=1
    cexLab=1
    cexAx=1
    cexText=1
    cexSub=1
  }
  
  ##changes done on 28-8-2017
  if(length(breaks>0) && length(breaks <12)){cexText=1}
  else if(length(breaks>13) && length(breaks < 40)){
    cexText = 0.45 
    cexAx=0.65
  }
  
  if(!is.null(breaks)){
    xCla <- vector("numeric",length(breaks)-1)
    for(idx in 1:(length(breaks)-1)){
      xCla[idx] <- sum(x[as.numeric(names(x))>=breaks[idx] & as.numeric(names(x))<breaks[idx+1]])
      #print(xCla[idx])
    }
    if(!is.null(y)){
      yCla <- vector("numeric",length(breaks)-1)
      for(idx in 1:(length(breaks)-1)){
        yCla[idx] <- sum(y[as.numeric(names(y))>=breaks[idx] & as.numeric(names(y))<breaks[idx+1]])
      }
    }else{
      yCla <- NULL
    }
  }else{
    xCla <- x
    yCla <- y
  }
  
  z <- c(xCla,yCla)
  if(is.null(classNames)){
    if(splitClassNames){
      splitStr <- "\n"
    }else{
      splitStr <- ""
    }
    txtForm <- rep("%.0f",length(breaks))
    idxRatNum <- which(abs(breaks-round(breaks))>1e-5)
    if(length(idxRatNum)<=1){
      txtForm[idxRatNum] <- "%.1f"
    }else{
      txtForm <- rep("%.1f",length(breaks))
    }
    txtForm[!is.infinite(breaks) & abs(breaks-round(breaks,1))>1e-5] <- "%.2f"
    classNames <- vector("character",length(breaks)-1)
    for(idx in 1:(length(breaks)-1)){
      if(!is.infinite(breaks[idx]) & !is.infinite(breaks[idx+1])){
        classNames[idx] <- paste0(sprintf(txtForm[idx],breaks[idx]),"-",splitStr,sprintf(txtForm[idx+1],breaks[idx+1]))
      }else{
        if(is.infinite(breaks[idx])){
          classNames[idx] <- paste0("<",splitStr,sprintf(txtForm[idx+1],breaks[idx+1]))
        }else{
          classNames[idx] <- paste0("\u2265",splitStr,sprintf(txtForm[idx],breaks[idx]))
        }
      }
    }
  }
  
  xTxtForm <- paste0("%.",as.numeric(xCla>0),"f")
  yTxtForm <- paste0("%.",as.numeric(yCla>0),"f")
  
  if(!is.null(unit)){
    if(unit=="%"){
      unit <- unit
    }else{
      unit <- paste0(" ",unit)
    }
  }else{
    if(absTxt){
      unit <- ""
    }else{
      unit <- "%"
    }
  }
  
  fac <- 100/sum(z)
  if(absTxt){
    fac <- 1
  }
  
  if(save & sum(z)>0){
    png(paste0(plotPath,plotName,".png"),width=1000,height=600)
  }
  par(cex.main=cexMain,cex.lab=cexLab,cex.axis=cexAx)
  if(is.null(y)){
    if(is.null(barCol)){
      barCol <- "lemonchiffon2"
    }
    bar.plot.wtd(x=factor(classNames,classNames),w=xCla,wall=4,b=0.7,form="c",b2=b2,main=title,ylab=yLab,xlab=xLab,col=barCol)
    if((percTxt & sum(z)>0) | absTxt){
      text(1:length(classNames),z+0.05*max(z),paste0(sprintf(xTxtForm,z*fac),unit),cex=cexText,col="darkblue")
    }
  }else{
    categ <- c(rep(nameX,length(xCla)),rep(nameY,length(yCla)))
    bar.plot.wtd(x=categ,y=factor(c(classNames,classNames),classNames),w=z,wall=4,b=0.9,form="c",b2=b2/2,main=title,xlab=xLab,ylab=yLab,col=c("lemonchiffon2","lemonchiffon4"))
    text(1:length(classNames),pmax(xCla,yCla)+0.06*max(z),paste0(sprintf(xTxtForm,xCla/sum(xCla)*100),"%\n",sprintf(yTxtForm,yCla/sum(yCla)*100),"%"),cex=cexText,srt=90,col="darkblue")
  }
  grid.text(subtitle,x=unit(0.00,"npc"),y=unit(0.005,"npc"),just=c("left","bottom"),gp=gpar(cex=cexSub))
  if(save & sum(z)>0){
    dev.off()
  }
}

plotBars1 <- function(plotPath,plotName,x,y=NULL,breaks=NULL,nameX=NULL,nameY=NULL,xLab="",yLab="Zeit [h]",title="",subtitle="",cexMain=1.2,cexLab=1.3,cexAx=1.1,cexText=1.5,cexSub=1.1,splitClassNames=FALSE,b2=1,classNames=NULL,barCol=NULL,percTxt=TRUE,absTxt=FALSE,unit=NULL,save=TRUE){
  library("plotrix")
  library("epade")
  library("grid")
  
  if(!save){
    cexMain=1
    cexLab=1
    cexAx=1
    cexText=1
    cexSub=1
  }
  ##changes done on 28-8-2017
  if(length(breaks>0) && length(breaks <12)){cexText=1}
  else if(length(breaks>13) && length(breaks < 40)){
    cexText = 0.45 
    cexAx=0.65
  }
  
  if(!is.null(breaks)){
    xCla <- vector("numeric",length(breaks)-1)
    for(idx in 1:(length(breaks)-1)){
      xCla[idx] <- sum(x[as.numeric(names(x))>=breaks[idx] & as.numeric(names(x))<breaks[idx+1]])
    }
    if(!is.null(y)){
      yCla <- vector("numeric",length(breaks)-1)
      for(idx in 1:(length(breaks)-1)){
        yCla[idx] <- sum(y[as.numeric(names(y))>=breaks[idx] & as.numeric(names(y))<breaks[idx+1]])
      }
    }else{
      yCla <- NULL
    }
  }else{
    xCla <- x
    yCla <- y
  }
  
  z <- c(xCla,yCla)
  if(is.null(classNames)){
    if(splitClassNames){
      splitStr <- "\n"
    }else{
      splitStr <- ""
    }
    txtForm <- rep("%.0f",length(breaks))
    idxRatNum <- which(abs(breaks-round(breaks))>1e-5)
    if(length(idxRatNum)<=1){
      txtForm[idxRatNum] <- "%.1f"
    }else{
      txtForm <- rep("%.1f",length(breaks))
    }
    txtForm[!is.infinite(breaks) & abs(breaks-round(breaks,1))>1e-5] <- "%.2f"
    classNames <- vector("character",length(breaks)-1)
    for(idx in 1:(length(breaks)-1)){
      if(!is.infinite(breaks[idx]) & !is.infinite(breaks[idx+1])){
        classNames[idx] <- paste0(sprintf(txtForm[idx],breaks[idx]),"-",splitStr,sprintf(txtForm[idx+1],breaks[idx+1]))
      }else{
        if(is.infinite(breaks[idx])){
          classNames[idx] <- paste0("<",splitStr,sprintf(txtForm[idx+1],breaks[idx+1]))
        }else{
          classNames[idx] <- paste0("\u2265",splitStr,sprintf(txtForm[idx],breaks[idx]))
        }
      }
    }
  }
  
  xTxtForm <- paste0("%.",as.numeric(xCla>0),"f")
  yTxtForm <- paste0("%.",as.numeric(yCla>0),"f")
  
  if(!is.null(unit)){
    if(unit=="%"){
      unit <- unit
    }else{
      unit <- paste0(" ",unit)
    }
  }else{
    if(absTxt){
      unit <- ""
    }else{
      unit <- "%"
    }
  }
  
  fac <- 100/sum(z)
  if(absTxt){
    fac <- 1
  }
  
  if(save & sum(z)>0){
    png(paste0(plotPath,plotName,".png"),width=1000,height=600)
  }
  par(cex.main=cexMain,cex.lab=cexLab,cex.axis=cexAx)
  if(is.null(y)){
    if(is.null(barCol)){
      barCol <- "lemonchiffon2"
    }
    bar.plot.wtd(x=factor(classNames,classNames),w=xCla,wall=4,b=0.7,form="c",b2=b2,main=title,ylab=yLab,xlab=xLab,col=barCol)
    if((percTxt & sum(z)>0) | absTxt){
      text(1:length(classNames),z+0.05*max(z),paste0(sprintf(xTxtForm,z*fac),unit),cex=cexText,col="darkblue")
    }
  }else{
    categ <- c(rep(nameX,length(xCla)),rep(nameY,length(yCla)))
    bar.plot.wtd(x=categ,y=factor(c(classNames,classNames),classNames),w=z,wall=4,b=0.9,form="c",b2=b2/2,main=title,xlab=xLab,ylab=yLab,col=c("lemonchiffon2","lemonchiffon4"))
    text(1:length(classNames),pmax(xCla,yCla)+0.06*max(z),paste0(sprintf(xTxtForm,xCla/sum(xCla)*100),"%\n",sprintf(yTxtForm,yCla/sum(yCla)*100),"%"),cex=cexText,srt=90,col="darkblue")
  }
  grid.text(subtitle,x=unit(0.00,"npc"),y=unit(0.005,"npc"),just=c("left","bottom"),gp=gpar(cex=cexSub))
  if(save & sum(z)>0){
    dev.off()
  }
}

plotBars2 <- function(plotPath,plotName,x,y=NULL,breaks=NULL,nameX=NULL,nameY=NULL,xLab="",yLab="Zeit [h]",title="",subtitle="",cexMain=1.1,cexLab=1.3,cexAx=1.1,cexText=1.5,cexSub=1.1,splitClassNames=FALSE,b2=1,classNames=NULL,barCol=NULL,percTxt=TRUE,absTxt=FALSE,unit=NULL,save=TRUE){
  library("plotrix")
  library("epade")
  library("grid")
  
  if(!save){
    cexMain=1
    cexLab=1
    cexAx=1
    cexText=1
    cexSub=1
  }
  
  ##changes done on 28-8-2017
  if(length(breaks>0) && length(breaks <12)){cexText=1}
  else if(length(breaks>13) && length(breaks < 40)){
    cexText = 0.45 
    cexAx=0.65
  }
  
  if(!is.null(breaks)){
    xCla <- vector("numeric",length(breaks)-1)
    for(idx in 1:(length(breaks)-1)){
      xCla[idx] <- sum(x[as.numeric(names(x))>=breaks[idx] & as.numeric(names(x))<breaks[idx+1]])
      #print(xCla[idx])
    }
    if(!is.null(y)){
      yCla <- vector("numeric",length(breaks)-1)
      for(idx in 1:(length(breaks)-1)){
        yCla[idx] <- sum(y[as.numeric(names(y))>=breaks[idx] & as.numeric(names(y))<breaks[idx+1]])
      }
    }else{
      yCla <- NULL
    }
  }else{
    xCla <- x
    yCla <- y
  }
  
  z <- c(xCla,yCla)
  if(is.null(classNames)){
    if(splitClassNames){
      splitStr <- "\n"
    }else{
      splitStr <- ""
    }
    txtForm <- rep("%.0f",length(breaks))
    idxRatNum <- which(abs(breaks-round(breaks))>1e-5)
    if(length(idxRatNum)<=1){
      txtForm[idxRatNum] <- "%.1f"
    }else{
      txtForm <- rep("%.1f",length(breaks))
    }
    txtForm[!is.infinite(breaks) & abs(breaks-round(breaks,1))>1e-5] <- "%.2f"
    classNames <- vector("character",length(breaks)-1)
    for(idx in 1:(length(breaks)-1)){
      if(!is.infinite(breaks[idx]) & !is.infinite(breaks[idx+1])){
        classNames[idx] <- paste0(sprintf(txtForm[idx],breaks[idx]),"-",splitStr,sprintf(txtForm[idx+1],breaks[idx+1]))
      }else{
        if(is.infinite(breaks[idx])){
          classNames[idx] <- paste0("<",splitStr,sprintf(txtForm[idx+1],breaks[idx+1]))
        }else{
          classNames[idx] <- paste0("\u2265",splitStr,sprintf(txtForm[idx],breaks[idx]))
        }
      }
    }
  }
  
  xTxtForm <- paste0("%.",as.numeric(xCla>0),"f")
  yTxtForm <- paste0("%.",as.numeric(yCla>0),"f")
  
  if(!is.null(unit)){
    if(unit=="%"){
      unit <- unit
    }else{
      unit <- paste0(" ",unit)
    }
  }else{
    if(absTxt){
      unit <- ""
    }else{
      unit <- "%"
    }
  }
  
  fac <- 100/sum(z)
  if(absTxt){
    fac <- 1
  }
  
  if(save & sum(z)>0){
    png(paste0(plotPath,plotName,".png"),width=1000,height=600)
  }
  par(cex.main=cexMain,cex.lab=cexLab,cex.axis=cexAx)
  if(is.null(y)){
    if(is.null(barCol)){
      barCol <- "lemonchiffon2"
    }
    bar.plot.wtd(x=factor(classNames,classNames),w=xCla,wall=4,b=0.7,form="c",b2=b2,main=title,ylab=yLab,xlab=xLab,col=barCol)
    if((percTxt & sum(z)>0) | absTxt){
      text(1:length(classNames),z+0.05*max(z),paste0(sprintf(xTxtForm,z*fac),unit),cex=cexText,col="darkblue")
      #text(0.5,1.05*max(z),paste0(" Pdiff = Pamb - PKGH "),font=2,col="black",cex=ifelse(save,1.7,1),pos=4)
      ##changes done on 23-08-2017
      # legend(x = 0.5, y = 1.05*max(z),legend = c(as.expression(bquote(paste('P'['diff']," = ",'P'['amb'],' - ','P'['KGH'])))),fill = "white",col = "black",text.font = 5,pch = NA)
      myleg <- legend(x = 0.5, y = 1.05*max(z),legend = c(as.expression(bquote(paste('P'['diff']," = ",'P'['amb'],' - ','P'['KGH'])))),inset = 0.05,bty="n",pt.cex = 2,cex=1.3,text.col = "black")
      # get the user coordinates to adjust the gap between the text and the border
      coord<-par("usr")
      # add a border, closer to the text (here, gap between border and beginning of text is a hundredth of the plot width) :
      rect(myleg$text$x[1]-diff(coord[1:2])/100,myleg$rect$top-myleg$rect$h,myleg$rect$left+myleg$rect$w,myleg$rect$top)
      
    }
  }else{
    categ <- c(rep(nameX,length(xCla)),rep(nameY,length(yCla)))
    bar.plot.wtd(x=categ,y=factor(c(classNames,classNames),classNames),w=z,wall=4,b=0.9,form="c",b2=b2/2,main=title,xlab=xLab,ylab=yLab,col=c("lemonchiffon2","lemonchiffon4"))
    text(1:length(classNames),pmax(xCla,yCla)+0.06*max(z),paste0(sprintf(xTxtForm,xCla/sum(xCla)*100),"%\n",sprintf(yTxtForm,yCla/sum(yCla)*100),"%"),cex=cexText,srt=90,col="darkblue")
  }
  grid.text(subtitle,x=unit(0.00,"npc"),y=unit(0.005,"npc"),just=c("left","bottom"),gp=gpar(cex=cexSub))
  if(save & sum(z)>0){
    dev.off()
  }
}

#plotBars3 (Only for font changes in 2.23.8-2.29.11)
plotBars3 <- function(plotPath,plotName,x,y=NULL,breaks=NULL,nameX=NULL,nameY=NULL,xLab="",yLab="Zeit [h]",title="",subtitle="",cexMain=1.4,cexLab=1.3,cexAx=0.8,cexText=1.2,cexSub=1.1,splitClassNames=FALSE,b2=1,classNames=NULL,barCol=NULL,percTxt=TRUE,absTxt=FALSE,unit=NULL,save=TRUE){
  library("plotrix")
  library("epade")
  library("grid")
  
  if(!save){
    cexMain=1
    cexLab=1
    cexAx=0.8
    cexText=0.8
    cexSub=1
  }
  ##changes done on 28-8-2017
  if(length(breaks>0) && length(breaks <12)){cexText=1}
  else if(length(breaks>13) && length(breaks < 40)){
    cexText = 0.45
    cexAx=0.65
  }
  
  if(!is.null(breaks)){
    xCla <- vector("numeric",length(breaks)-1)
    for(idx in 1:(length(breaks)-1)){
      xCla[idx] <- sum(x[as.numeric(names(x))>=breaks[idx] & as.numeric(names(x))<breaks[idx+1]])
      #print(xCla[idx])
    }
    if(!is.null(y)){
      yCla <- vector("numeric",length(breaks)-1)
      for(idx in 1:(length(breaks)-1)){
        yCla[idx] <- sum(y[as.numeric(names(y))>=breaks[idx] & as.numeric(names(y))<breaks[idx+1]])
      }
    }else{
      yCla <- NULL
    }
  }else{
    xCla <- x
    yCla <- y
  }
  
  z <- c(xCla,yCla)
  if(is.null(classNames)){
    if(splitClassNames){
      splitStr <- "\n"
    }else{
      splitStr <- ""
    }
    txtForm <- rep("%.0f",length(breaks))
    idxRatNum <- which(abs(breaks-round(breaks))>1e-5)
    if(length(idxRatNum)<=1){
      txtForm[idxRatNum] <- "%.1f"
    }else{
      txtForm <- rep("%.1f",length(breaks))
    }
    txtForm[!is.infinite(breaks) & abs(breaks-round(breaks,1))>1e-5] <- "%.2f"
    classNames <- vector("character",length(breaks)-1)
    for(idx in 1:(length(breaks)-1)){
      if(!is.infinite(breaks[idx]) & !is.infinite(breaks[idx+1])){
        classNames[idx] <- paste0(sprintf(txtForm[idx],breaks[idx]),"-",splitStr,sprintf(txtForm[idx+1],breaks[idx+1]))
      }else{
        if(is.infinite(breaks[idx])){
          classNames[idx] <- paste0("<",splitStr,sprintf(txtForm[idx+1],breaks[idx+1]))
        }else{
          classNames[idx] <- paste0("\u2265",splitStr,sprintf(txtForm[idx],breaks[idx]))
        }
      }
    }
  }
  
  xTxtForm <- paste0("%.",as.numeric(xCla>0),"f")
  yTxtForm <- paste0("%.",as.numeric(yCla>0),"f")
  
  if(!is.null(unit)){
    if(unit=="%"){
      unit <- unit
    }else{
      unit <- paste0(" ",unit)
    }
  }else{
    if(absTxt){
      unit <- ""
    }else{
      unit <- "%"
    }
  }
  
  fac <- 100/sum(z)
  if(absTxt){
    fac <- 1
  }
  
  if(save & sum(z)>0){
    png(paste0(plotPath,plotName,".png"),width=1000,height=600)
  }
  par(cex.main=cexMain,cex.lab=cexLab,cex.axis=cexAx)
  if(is.null(y)){
    if(is.null(barCol)){
      barCol <- "lemonchiffon2"
    }
    bar.plot.wtd(x=factor(classNames,classNames),w=xCla,wall=4,b=0.7,form="c",b2=b2,main=title,ylab=yLab,xlab=xLab,col=barCol)
    if((percTxt & sum(z)>0) | absTxt){
      text(1:length(classNames),z+0.05*max(z),paste0(sprintf(xTxtForm,z*fac),unit),cex=cexText,col="darkblue")
    }
  }else{
    categ <- c(rep(nameX,length(xCla)),rep(nameY,length(yCla)))
    bar.plot.wtd(x=categ,y=factor(c(classNames,classNames),classNames),w=z,wall=4,b=0.9,form="c",b2=b2/2,main=title,xlab=xLab,ylab=yLab,col=c("lemonchiffon2","lemonchiffon4"))
    text(1:length(classNames),pmax(xCla,yCla)+0.06*max(z),paste0(sprintf(xTxtForm,xCla/sum(xCla)*100),"%\n",sprintf(yTxtForm,yCla/sum(yCla)*100),"%"),cex=cexText,srt=90,col="darkblue")
  }
  grid.text(subtitle,x=unit(0.00,"npc"),y=unit(0.005,"npc"),just=c("left","bottom"),gp=gpar(cex=cexSub))
  if(save & sum(z)>0){
    dev.off()
  }
}


plotBarsLog<-function(x = NULL,breaks = NULL,y = NULL,plotPath = NULL, plotName = NULL, pSubtitle = NULL, xLab = NULL, yLab = NULL, pTitle = NULL,classNames = NULL,splitClassNames = FALSE,absTxt = FALSE,unit="%" ,save = FALSE){
  #library(ggplot2,lib.loc = "C:/Users/SHINAMI/Documents/R/R-3.3.2/library/")
  #library(ggplot2,lib.loc = "C:/EngineDL/SETUP_Files/R-3.3.3/library/")
  #library(ggplot2,lib.loc = "C:/Users/anishar/Documents/R/R-3.3.2/library/")
  #library(ggplot2,lib.loc="C:/EngineDL/")
  library(ggplot2)
  #library("ggplot2", lib.loc="~/R/R-3.3.2/library/")
  library(gridExtra)
  if(!is.null(breaks)){
    xCla <- vector("numeric",length(breaks)-1)
    xCla1 <- vector("numeric",length(breaks)-1)
    for(idx in 1:(length(breaks)-1)){
      xCla[idx] <- sum(x[as.numeric(names(x))>=breaks[idx] & as.numeric(names(x))<breaks[idx+1]])
      xCla1[idx]<-log(xCla[idx])
      # print(xCla[idx])
    }
    if(!is.null(y)){
      yCla <- vector("numeric",length(breaks)-1)
      for(idx in 1:(length(breaks)-1)){
        yCla[idx] <- sum(y[as.numeric(names(y))>=breaks[idx] & as.numeric(names(y))<breaks[idx+1]])
      }
    }else{
      yCla <- NULL
    }
  }else{
    xCla <- x
    yCla <- y
  }
  
  z <- c(xCla,yCla)
  if(is.null(classNames)){
    if(splitClassNames){
      splitStr <- "\n"
    }else{
      splitStr <- ""
    }
    txtForm <- rep("%.0f",length(breaks))
    idxRatNum <- which(abs(breaks-round(breaks))>1e-5)
    if(length(idxRatNum)<=1){
      txtForm[idxRatNum] <- "%.1f"
    }else{
      txtForm <- rep("%.1f",length(breaks))
    }
    txtForm[!is.infinite(breaks) & abs(breaks-round(breaks,1))>1e-5] <- "%.2f"
    classNames <- vector("character",length(breaks)-1)
    for(idx in 1:(length(breaks)-1)){
      if(!is.infinite(breaks[idx]) & !is.infinite(breaks[idx+1])){
        classNames[idx] <- paste0(sprintf(txtForm[idx],breaks[idx]),"-",splitStr,sprintf(txtForm[idx+1],breaks[idx+1]))
      }else{
        if(is.infinite(breaks[idx])){
          classNames[idx] <- paste0("<",splitStr,sprintf(txtForm[idx+1],breaks[idx+1]))
        }else{
          classNames[idx] <- paste0("\u2265",splitStr,sprintf(txtForm[idx],breaks[idx]))
        }
      }
    }
  }
  
  xTxtForm <- paste0("%.",as.numeric(xCla>0),"f")
  yTxtForm <- paste0("%.",as.numeric(yCla>0),"f")
  
  if(!is.null(unit)){
    if(unit=="%"){
      unit <- unit
    }else{
      unit <- paste0(" ",unit)
    }
  }else{
    if(absTxt){
      unit <- ""
    }else{
      unit <- "%"
    }
  }
  
  fac <- 100/sum(z)
  if(absTxt){
    fac <- 1
  }
  
  ##plotting function
  
  df <- data.frame(classNames,xCla)
  perc<-NULL
  for(idx in 1 : length(classNames))
  {
    perc[idx]<- round(z[idx]/sum(z) * 100, 2)
    perc[idx]<-paste0(z[idx],"\n","(",perc[idx],"%",")")  
  }
  if(save & sum(z)>0){
    png(paste0(plotPath,plotName,".png"),width=1200,height=650)
  }
  black.bold.text <- element_text(face = "bold", color = "black",debug = FALSE,size=15)
  p1<-ggplot(df, aes(x = classNames, y = xCla, fill = as.factor(classNames),label = xCla),show.legend = TRUE)+
    labs(x=xLab, caption = pSubtitle,y =  yLab, title =pTitle)+
    geom_bar(stat = "identity",fill = "#E7E3BA",width = 0.7,colour = "black") +
    geom_text(aes(label = perc), size =5 , vjust = -0.4, color = "blue") +
    scale_y_log10(breaks = c(1,5,10,50,100,500,1000,5000,10000,20000,50000,120000),limits = c(1,120000)) + scale_x_discrete(limits = classNames) + 
    theme(plot.margin = unit(c(0.1,0.3,0.1,0.3), "inch"),panel.background = element_rect(fill = "#e7ebf5"),panel.border = element_rect(color = "black", fill = NA) ,
          axis.title.x = black.bold.text, axis.title.y = black.bold.text , plot.title = element_text(face = "bold", color = "black",size = 15, vjust = 0.5,hjust = 0.5),plot.caption = element_text(face = "plain",color = "black", size = 12,vjust = 0,hjust = 0),axis.text.x = element_text(size = 14,face = "plain"),axis.text.y = element_text(size = 14,face = "plain"))#+
  # coord_equal(ratio = 5/2)
  print(p1)
  if(save){
    dev.off()
  }
  #detach("package:ggplot2", unload=TRUE)
}

##new function (PlotEngRPMOverTime) for 10Hz data added on 03-07-2017 
plotEngRPMOverTime <- function(plotPath,plotName,x,save = FALSE){
  colnames(x)<-c("kdx","t","eng")
  num_grp<-as.numeric(unique(x$kdx))
  library(ggplot2)
  gp<-ggplot(data = x,aes(x=t,y=eng,group = kdx))+
    geom_line(colour = "#00cc99",size = 0.0625)+
    theme(plot.margin = unit(c(0.1,0.3,0.1,0.3), "inch"),
          plot.title = element_text(hjust = 0.5,size = 15, colour = "black",face = "bold"),
          axis.title.x = element_text(hjust = 0.5,size = 14, colour = "black",face = "plain"),
          axis.title.y = element_text(hjust = 0.5,size = 14, colour = "black",face = "plain"),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          panel.grid.minor = element_blank())+
    scale_x_continuous(breaks = c(seq(0,2,0.1)),limits = c(0.0,2.0))+
    scale_y_continuous(breaks = c(seq(0,4000,500)),limits=c(0,4000))+
    xlab("Ziet[s]")+
    ylab("Motordrehzhal[U/min]")+
    ggtitle(paste0("Motordrehzahlkurven von ",length(num_grp)," Starts (Startzeitpunkt t=0.0s)"))
  print(gp)
  if(save){
    ggsave(paste0(plotPath,plotName,".png"),plot = gp,scale = 2,height = 3.5,width = 7)
  }
  else
  {
    print(gp)
  }
}
##new function (plotStackBar) added 14-3-2017 
plotStackBar<-function(x,plotPath,plotName,breaks,xLabel, yLabel,mainTitle,splitStr="",splitClassNames=FALSE,save = TRUE){
  library("plotrix")
  library("epade")
  library("grid")
  library("TeachingDemos")
  
  if(nrow(x)<2)
  {
    if(rownames(x)==0){
      x<-rbind(x,0)
      rownames(x)<-c(0,1)
    }
    else if(rownames(x)==1){
      x<-rbind(x,0)
      rownames(x)<-c(1,0)
    }
  }
  
  if(!save){
    cexMain=1.5
    cexLab=1.5
    cexAx=1
    cexSub=1
    
  }
  else{
    cexMain=1.5
    cexLab=1.5
    cexAx=1
    cexSub=1
    
  }
  if(splitClassNames){
    splitStr <- "\n"
  }else{
    splitStr <- ""
  }
  txtForm<-rep("%.0f",length(breaks))
  
  txtForm[!is.infinite(breaks) & abs(breaks-round(breaks,1))>1e-5] <- "%.2f"
  classNames <- vector("character",length(breaks)-1)
  for(idx in 1:(length(breaks)-1)){
    if(!is.infinite(breaks[idx]) & !is.infinite(breaks[idx+1])){
      classNames[idx] <- paste0(sprintf(txtForm[idx],breaks[idx]),"-",splitStr,sprintf(txtForm[idx+1],breaks[idx+1]))
    }else{
      if(is.infinite(breaks[idx])){
        classNames[idx] <- paste0("<",splitStr,sprintf(txtForm[idx+1],breaks[idx+1]))
      }else{
        classNames[idx] <- paste0("\u2265",splitStr,sprintf(txtForm[idx],breaks[idx]))
      }
    }
  }
  a_rpm<-as.numeric(colnames(x))
  y_OFF<-x[1,]
  y_ON<-x[2,]
  y_ONCla<-vector("numeric",length(breaks)-1)
  y_OFFCla<-vector("numeric",length(breaks)-1)
  y_count<-vector("numeric",length(breaks)-1)
  y_ON_count<-NULL
  y_OFF_count<-NULL
  y_ClaTotal<-NULL
  unitFactor<-1/3600
  for(idx in 1:length(breaks)-1)
  {
    y_ONCla[idx]<-(sum(y_ON[a_rpm>=breaks[idx]&a_rpm<breaks[idx+1]])/sum(x[1,],x[2,]))*100
    y_ON_count[idx]<-sum(y_ON[a_rpm>=breaks[idx]&a_rpm<breaks[idx+1]])/3600
    y_OFFCla[idx]<-(sum(y_OFF[a_rpm>=breaks[idx]&a_rpm<breaks[idx+1]])/sum(x[1,],x[2,]))*100
    y_OFF_count[idx]<-sum(y_OFF[a_rpm>=breaks[idx]&a_rpm<breaks[idx+1]])/3600
    y_count[idx]<-sum(y_ON[a_rpm>=breaks[idx]&a_rpm<breaks[idx+1]],y_OFF[a_rpm>=breaks[idx]&a_rpm<breaks[idx+1]])*unitFactor
    y_ClaTotal[idx]<-sum(y_ON_count[idx],y_OFF_count[idx])
    
  }
  df_wc<-data.frame(classNames,y_count,y_ONCla,y_OFFCla,y_ON_count,y_OFF_count)
  df_wc1<-data.frame(classNames,y_ON_count,y_OFF_count,y_ClaTotal)
  #library(reshape2)
  if(save){
    png(paste0(plotPath,plotName,".png"),width=1200,height=650)
  }
  #df.mwc<-melt(df_wc1,id.vars="classNames")
  
  #library(ggplot2)
  #levels(df.mwc$variable)[levels(df.mwc$variable)=="y_ON_count"]<-"kimakompressor AN"
  #levels(df.mwc$variable)[levels(df.mwc$variable)=="y_OFF_count"]<-"kimakompressor AUS"
  # black.bold.text <- element_text(face = "bold", color = "black",debug = FALSE,size=14)
  # p1<-ggplot(df.mwc, aes(x = classNames, y = value,fill=variable),show.legend = TRUE) +
  #   geom_bar(stat='identity',width = 0.5,color="black")+
  #   geom_text(aes(label = round(value,2)),size = 3, position = position_stack(vjust = 0.5))+
  #   labs(x=xLabel,y = yLabel, title =mainTitle)+
  #   scale_y_continuous(expand = c(0,0),limits = c(0,120))+
  #   #theme(panel.background = element_rect(fill = "#ffffff"),axis.line.x = element_line(color = "black"),panel.grid.minor.y = element_line(color="black"),panel.border = element_rect(color = "black", fill = NA))+
  #   theme(plot.margin = unit(c(0.1,0.3,0.1,0.3), "inch"),panel.background = element_rect(fill = "#e7ebf5"),panel.border = element_rect(color = "black", fill = NA) ,
  #         axis.title.x = black.bold.text, axis.title.y = black.bold.text , plot.title = element_text(face = "bold", color = "black",size = 12, vjust = 0.5,hjust = 0.5),plot.caption = element_text(face = "plain",color = "black", size = 9,vjust = 0,hjust = 0),axis.text.x = element_text(size = 14,face = "plain"),axis.text.y = element_text(size = 14,face = "plain"))+
  #   scale_fill_brewer()
  #   
  # print(p1)
  nameX <- "kimakompressor AN"
  nameNx<- "kimakompressor AUS"
  categ <- factor(c(rep(nameX,length(df_wc1$y_ON_count)),rep(nameNx,length(df_wc1$y_OFF_count))),c(nameX,nameNx))
  colX<-"#377EB8"
  colNx<-"#FFFFFF00"
  print(mainTitle)
  mainTitle = paste0("Verweildauer Klimakompressor in Leerlaufklassen : insgesamt " ,sprintf("%.0f",sum(df_wc1$y_ClaTotal)),mainTitle)
  par(cex.main=cexMain,cex.lab=cexLab,cex.axis=cexAx)
  bar.plot.wtd(x=categ,y=factor(rep(df_wc1$classNames,2),levels = classNames),w=c(df_wc1$y_ON_count,df_wc1$y_OFF_count),wall=4,b=0.45,form="c",b2=1,main=mainTitle,xlab=xLabel,ylab=yLabel,col=c(colX,colNx),beside=FALSE)
  shadowtext(1:length(df_wc1$classNames),df_wc1$y_ON_count+0.0025*max(df_wc1$y_ON_count+df_wc1$y_OFF_count),labels = paste0(round(df_wc1$y_ON_count,2)," h"),col = "white",bg="black")
  text(1:length(df_wc1$classNames),df_wc1$y_ClaTotal+0.03*max(df_wc1$y_ON_count+df_wc1$y_OFF_count),labels = paste0(round(df_wc1$y_OFF_count,2)," h"),col = "black")
  text(0.5,1.1*max(df_wc1$y_ON_count+df_wc$y_OFF_count),labels = paste0(" "),col="black",cex=ifelse(save,1.3,1),pos=4)
  shadowtext(1:length(df_wc1$classNames),rep(1.065*max(df_wc1$y_ON_count+df_wc$y_OFF_count),length(df_wc1$classNames)),labels =paste0(round(df_wc1$y_ClaTotal,2)," h"),col="#377EB8",cex=ifelse(save,1,0.8),bg="grey60",r=0.03)
  grid.text("",x=unit(0.00,"npc"),y=unit(0.005,"npc"),just=c("left","bottom"),gp=gpar(cex=1))
  if(save){
    dev.off()
  }
  #detach("package:ggplot2", unload=TRUE)
}

durationTime <- function(x,y=NULL,breaksX,breaksY=NULL){
  library("accelerometry")
  modeDur <- NULL
  if(is.null(y)){
    y <- x
  }
  if(is.null(breaksY)){
    breaksY <- breaksX
  }
  if(length(x)>0 & length(y)>0){
    isMode <- (x>=breaksX[1] & x<=breaksX[2] & !is.na(x) & y>=breaksY[1] & y<=breaksY[2] & !is.na(y))
    modeSeq <- rle2(as.numeric(isMode),return.list=TRUE)
    if(any(modeSeq$values==1)){
      modeDur <- modeSeq$lengths[modeSeq$values==1]
    }
  }
  return(modeDur)
}

startIndices <- function(x,threshold,minTime){
  library("accelerometry")
  if(length(x)>0){
    y <- rle2(as.numeric(x>=threshold),indices=TRUE,return.list=TRUE)
    idFromUnderToOverThresh <- which(diff(y$values)==1)
    isMinTime <- y$lengths[idFromUnderToOverThresh+1]>=minTime
    startIdx <- y$stops[idFromUnderToOverThresh[isMinTime]]
  }else{
    startIdx <- NULL
  }
  return(startIdx)
}

startIndicesCount <- function(x,threshold,minTime){
  library("accelerometry")
  if(length(x)>0){
    y <- rle2(as.numeric(x>=threshold),indices=TRUE,return.list=TRUE)
    idFromUnderToOverThresh <- which(diff(y$values)==1)
    isMinTime <- y$lengths[idFromUnderToOverThresh+1]>=minTime
    startIdx <- y$stops[idFromUnderToOverThresh[isMinTime]]
  }else{
    startIdx <- NULL
  }
  return(length(startIdx))
}

startIndices1 <- function(x,x1,threshold){
  library("accelerometry")
  if(length(x)>0){
    isMode3s <- (x==threshold & x1==1)
    y <- rle2(as.numeric(isMode3s),indices=TRUE,return.list=TRUE)
    # idFromUnderToOverThresh <-y$values==0
    idFromUnderToOverThresh <- which(diff(y$values)==-1)
    # isMinTime <- y$lengths[idFromUnderToOverThresh+1]>1
    startIdx <- y$stops[idFromUnderToOverThresh]
    startIdx <- startIdx + 1
  }else{
    startIdx <- NULL
  }
  return(startIdx)
}

smoothGpsAlt <- function(gpsAlt,gpsPrec){
  if(length(gpsAlt)<1){
    gpsAlt <- NA
    gpsPrec <- NA
  }
  library("accelerometry")
  gpsPrec[is.na(gpsPrec)] <- 1000
  gpsPrec[gpsPrec==0] <- 1e-5
  invGpsPrec <- 1/gpsPrec
  notNaSeq <- rle2(as.numeric(!is.na(gpsAlt)),indices=TRUE,return.list=TRUE)
  idNotNaSeq <- which(notNaSeq$values==1 & notNaSeq$lengths>5)
  gpsAltCorr <- NULL
  if(length(idNotNaSeq)>0){
    for(idx in 1:length(idNotNaSeq)){
      index <- notNaSeq$starts[idNotNaSeq[idx]]:notNaSeq$stops[idNotNaSeq[idx]]
      gpsAltCorr <- c(gpsAltCorr,smooth.spline(x=gpsAlt[index],w=invGpsPrec[index])$y)
    }
  }
  return(gpsAltCorr)
}
getChann <- function(sigNames,data,naThresh=NULL){
  colName <- sigNames[sigNames %in% names(data)]
  if(length(colName)<1){
    X <- NULL
  }else if(length(colName)>1){
    X <- data[[colName[which.max(colSums(!is.na(data[,colName])))]]]
  }else{
    X <- data[[colName]]
  }
  if(!is.null(naThresh) & length(colName)>0){
    X[X>=naThresh] <- NA
  }
  return(X)
}
getChann1 <- function(sigNames,data,minThresh=NULL,maxThresh=NULL){
  colName <- sigNames[sigNames %in% names(data)]
  #print(paste0("Following Signals are available:",colName))
  if(length(colName)<1){
    X <- NULL
  }else if(length(colName)>1){
    col <- colName[which.max(colSums(!is.na(data[,colName])))]
    #print(paste0("Signal used:",col))
    X <- data[[colName[which.max(colSums(!is.na(data[,colName])))]]]
    X[X>maxThresh] <- NA
    X <- X[X>=minThresh & X<= maxThresh]
    
    #X <- subset(X,col>=minThresh & col <= maxThresh)
  }else{
    X <- data[[colName]]
    #X <- X[X$get(colName)>=minThresh & X$get(colName)<= maxThresh]
    X[X>maxThresh] <- NA
    X <- X[X>=minThresh & X<= maxThresh]
    #X <- subset(X,colName >=minThresh & colName <= maxThresh)
  }
  # if(!is.null(naThresh) & length(colName)>0){
  #   X[X>=naThresh] <- NA
  # }
  return(X)
}

getConditionIdx <- function(x,conType,conVal){
  if(conType=="geq"){
    idxConTru <- which(x>=conVal)
  }else if(conType=="leq"){
    idxConTru <- which(x<=conVal)
  }else{
    idxConTru <- NULL
  }
  idxGapLast <- c(which(diff(idxConTru)>1),length(idxConTru))
  if(length(idxGapLast)<2){
    idxGapFirst <- 1
  }else{
    idxGapFirst <- c(1,idxGapLast[1:(length(idxGapLast)-1)]+1)
  }
  idxConTruAtFirst <- idxConTru[idxGapFirst]
  idxConTruAtLast <- idxConTru[idxGapLast]
  numObs <- length(idxConTruAtFirst)
  
  if(is.null(idxConTru) || length(idxConTru)<=0){
    rValue <- list(NULL,NULL,0)
  }else{
    rValue <- list(idxConTruAtFirst,idxConTruAtLast,numObs)
  }
  return(rValue)
}

createReport <- function(dir,mainConfig,template,fzg,motornummer,rsgIsg,pdflatex){
  library("ReporteRsjars")
  library("ReporteRs")
  
  if(!("genDat.png" %in% list.files(dir))){
    createGenDatPng(dir=dir,pdflatex=pdflatex)
  }
  if(!("failSlides.png" %in% list.files(dir))){
    getFailSlides(dir=dir,mainConfig=mainConfig,rsgIsg=rsgIsg,pdflatex=pdflatex)
  }
  
  report <- pptx(template=template)
  report <- addSlide(report,slide.layout="Title Slide" )
  report <- addTitle(report,paste("Standardauswertung Fahrzeugdauerlauf",fzg,"(",motornummer,")"))
  
  # report <- addSlide(report,slide.layout="1_Title Slide" )
  # report <- addTitle(report,paste("1. Allgemeine Daten zum Dauerlauf"))
  
  report <- addSlide(report,slide.layout="GenDataTable")
  report <- addTitle(report,"1. Allgemeine Daten zum Dauerlauf")
  report <- addImage(report,file=paste0(dir,"genDat.png"))
  report <- addFooter(report,paste("Standardauswertung Fahrzeugdauerlauf",fzg,"(",motornummer,")","|","RD I/CDV","|",Sys.Date()))
  report <- addPageNumber(report)
  
  
  # report <- addSlide(report,slide.layout="1_Title Slide" )
  # report <- addTitle(report,paste("2. Auswertungen aus Fahrkollektiven"))
  
  # idFirstBlk <- which.max(substr(mainConfig[,1],1,1)=="4")
  for(idx in 1:dim(mainConfig)[1]){
    print(idx)
    # if(idx==idFirstBlk){
    #   report <- addSlide(report,slide.layout="1_Title Slide" )
    #   report <- addTitle(report,paste("4. Belastungskollektive"))
    # }
    if(paste0(mainConfig[idx,4],".png") %in% list.files(paste0(dir,"Grafiken/"))){
      if(is.na(mainConfig[idx,3])){
        report <- addSlide(report,slide.layout="OnePlot")
        report <- addTitle(report,as.character(mainConfig[idx,2]))
        report <- addImage(report,file=paste0(dir,"Grafiken/",mainConfig[idx,4],".png"))
        report <- addParagraph(report,set_of_paragraphs("2. Auswertungen aus Fahrkollektiven"))
        #report <- addSubtitle(report,paste("Refer excel data in appendix"))
        report <- addFooter(report,paste("Standardauswertung Fahrzeugdauerlauf",fzg,"(",motornummer,")","|","RD I/CDV","|",Sys.Date()))
        report <- addPageNumber(report)
      }else{
        report <- addSlide(report,slide.layout="OnePlot_TextTitle")
        text1 <- pot(as.character(mainConfig[idx,2]),textProperties(font.weight="bold",font.size=24,font.family="CorpoA"))
        text2 <- pot(as.character(mainConfig[idx,3]),textProperties(font.weight="normal",font.size=18,font.family="CorpoA"))
        report <- addParagraph(report,set_of_paragraphs(text1,text2))
        report <- addImage(report,file=paste0(dir,"Grafiken/",mainConfig[idx,4],".png"))
        report <- addParagraph(report,set_of_paragraphs("2. Auswertungen aus Fahrkollektiven"))
        #report <- addSubtitle(report,paste("Refer excel data in appendix"))
        report <- addFooter(report,paste("Standardauswertung Fahrzeugdauerlauf",fzg,"(",motornummer,")","|","RD I/CDV","|",Sys.Date()))
        report <- addPageNumber(report)
      }
    }
  }
  
  # report <- addSlide(report,slide.layout="1_Title Slide" )
  # report <- addTitle(report,paste("5. Anhang"))
  
  report <- addSlide(report,slide.layout="OnePlot")
  report <- addTitle(report,"5.1 Datengrundlage")
  report <- addImage(report,file=paste0(dir,"Grafiken/","missogram.png"))
  report <- addParagraph(report,set_of_paragraphs("5. Anhang"))
  report <- addFooter(report,paste("Standardauswertung Fahrzeugdauerlauf",fzg,"(",motornummer,")","|","RD I/CDV","|",Sys.Date()))
  report <- addPageNumber(report)
  
  report <- addSlide(report,slide.layout="ChannelTable")
  report <- addTitle(report,"5.2 Datengrundlage")
  report <- addImage(report,file=paste0(dir,"table1.png"))
  report <- addImage(report,file=paste0(dir,"table2.png"))
  report <- addImage(report,file=paste0(dir,"table3.png"))
  report <- addParagraph(report,set_of_paragraphs("5. Anhang"))
  report <- addFooter(report,paste("Standardauswertung Fahrzeugdauerlauf",fzg,"(",motornummer,")","|","RD I/CDV","|",Sys.Date()))
  report <- addPageNumber(report)
  
  report <- addSlide(report,slide.layout="ChannelTable")
  report <- addTitle(report,"5.2 Datengrundlage")
  report <- addImage(report,file=paste0(dir,"table4.png"))
  report <- addImage(report,file=paste0(dir,"table5.png"))
  report <- addImage(report,file=paste0(dir,"table6.png"))
  report <- addParagraph(report,set_of_paragraphs("5. Anhang"))
  report <- addFooter(report,paste("Standardauswertung Fahrzeugdauerlauf",fzg,"(",motornummer,")","|","RD I/CDV","|",Sys.Date()))
  report <- addPageNumber(report)
  
  report <- addSlide(report,slide.layout="ChannelTable")
  report <- addTitle(report,"5.2 Datengrundlage")
  report <- addImage(report,file=paste0(dir,"table7.png"))
  report <- addImage(report,file=paste0(dir,"table8.png"))
  report <- addImage(report,file=paste0(dir,"table9.png"))
  report <- addParagraph(report,set_of_paragraphs("5. Anhang"))
  report <- addFooter(report,paste("Standardauswertung Fahrzeugdauerlauf",fzg,"(",motornummer,")","|","RD I/CDV","|",Sys.Date()))
  report <- addPageNumber(report)
  
  report <- addSlide(report,slide.layout="ChannelTable")
  report <- addTitle(report,"5.2 Datengrundlage")
  report <- addImage(report,file=paste0(dir,"table10.png"))
  report <- addImage(report,file=paste0(dir,"table11.png"))
  report <- addImage(report,file=paste0(dir,"table12.png"))
  report <- addParagraph(report,set_of_paragraphs("5. Anhang"))
  report <- addFooter(report,paste("Standardauswertung Fahrzeugdauerlauf",fzg,"(",motornummer,")","|","RD I/CDV","|",Sys.Date()))
  report <- addPageNumber(report)
  
  numFailSlides <- list.files(dir,pattern = "failSlides.*\\.png")
  
  if(length(numFailSlides)<=1){
    report <- addSlide(report,slide.layout="FailSlidesTable2")
    report <- addTitle(report,"5.3 Fehlende Abschnitte")
    report <- addImage(report,file=paste0(dir,"failSlides1.png"))
    report <- addParagraph(report,set_of_paragraphs("5. Anhang"))
    report <- addFooter(report,paste("Standardauswertung Fahrzeugdauerlauf",fzg,"(",motornummer,")","|","RD I/CDV","|",Sys.Date()))
    report <- addPageNumber(report)
  }else{
    report <- addSlide(report,slide.layout="FailSlidesTable2")
    report <- addTitle(report,"5.3 Fehlende Abschnitte")
    report <- addImage(report,file=paste0(dir,"failSlides1.png"))
    report <- addParagraph(report,set_of_paragraphs("5. Anhang"))
    report <- addFooter(report,paste("Standardauswertung Fahrzeugdauerlauf",fzg,"(",motornummer,")","|","RD I/CDV","|",Sys.Date()))
    report <- addPageNumber(report)
    
    report <- addSlide(report,slide.layout="FailSlidesTable2")
    report <- addTitle(report,"5.3 Fehlende Abschnitte")
    report <- addImage(report,file=paste0(dir,"failSlides2.png"))
    report <- addParagraph(report,set_of_paragraphs("5. Anhang"))
    report <- addFooter(report,paste("Standardauswertung Fahrzeugdauerlauf",fzg,"(",motornummer,")","|","RD I/CDV","|",Sys.Date()))
    report <- addPageNumber(report)
  }
  
  report <- addSlide(report,slide.layout="OnePlot")
  report <- addTitle(report,"5.4 Datan Motorölstand")
  # report <- addImage(report,file=paste0(dir,"Grafiken/LineDataTeble",".png"))
  report <- addParagraph(report,set_of_paragraphs("5. Anhang"))
  report <- addFooter(report,paste("Standardauswertung Fahrzeugdauerlauf",fzg,"(",motornummer,")","|","RD I/CDV","|",Sys.Date()))
  report <- addPageNumber(report)
  
  ##added on 29-08-2017 change request
  # breaksDef <- readRDS(file=paste0(projDir,fzg,"/Daten/breaksDef.rds"))
  # xlsxWB<-createExcelExport(cbind(mainConfig[,1:8],breaksDef,mainConfig[,15:17]),paste0(projDir,fzg,"/Daten/"),paste0(fzg))
  # file <- paste0(projDir,fzg,"/ExcelReport.xlsx")
  # saveWorkbook(xlsxWB,file)
  
  report <- addSlide(report,slide.layout="OnePlot")
  report <- addTitle(report,"Appendix")
  # report <- addImage(report,file=paste0(dir,"Grafiken/LineDataTeble",".png"))
  report <- addParagraph(report,set_of_paragraphs("6. Appendix"))
  #  report <- addRScript(doc = report,file = file)
  report <- addFooter(report,paste("Standardauswertung Fahrzeugdauerlauf",fzg,"(",motornummer,")","|","RD I/CDV","|",Sys.Date()))
  report <- addPageNumber(report)
  
  print(report)
  View(report)
  return(report)
}

createExcelExport<-function(mainConfig,datenDir,vehicleNum){
  ##initiliaze common variable
  print(datenDir)
  #xlsxFile <- paste0(excelPath,"/",excelName)
  xlsxWB<-createWorkbook()
  ##creating style
  titleHeaderStyle <- openxlsx::createStyle(fontColour = "black",fontName = "CorpoA", fontSize = 12,textDecoration = "bold")
  tableHeaderStyle<-createStyle(fontColour = "black",fontName = "CorpoS",fontSize = 11,textDecoration = "bold",fgFill = "#95b3d7",halign = "center",valign = "center",wrapText = TRUE,border = c("top","left","bottom","right"))
  tableDataStyle<-createStyle(fontColour = "black",fontName = "CorpoS",fontSize = 11,halign ="center",valign = "center",textDecoration ="bold")
  xLabelStyle<-createStyle(fontColour = "black",fontName = "CorpoA",fontSize = 12,fgFill = "#95b3d7",textDecoration = "bold",halign ="center",valign = "center")
  yLabelStyle<-createStyle(fontColour = "black",fontName = "CorpoA",fontSize = 12,fgFill = "#95b3d7",textDecoration = "bold",halign ="center",valign = "center",textRotation = 90)
  
  titleVec<-mainConfig[,2]
  subtitleVec<-mainConfig[,3]
  titleidx<-trimws(substring(titleVec[mainConfig[,15]!="NA"],1,4),which = "right")
  ##changes done on 28-08-2017
  subtitleidx<-trimws(gsub(pattern = "[A-Z]",replacement = " ",x = substring(subtitleVec[mainConfig[,15]!="NA"],1,7)),which = "right")
  
  for(i in 1:length(subtitleidx))
  {
    if(is.na(subtitleidx[i]))
    {
      subtitleidx[i]<-titleidx[i]
    }
  }
  ######setting up sheet and title
  for(i in 1:length(subtitleidx))
  {
    print(subtitleidx[i])
    addWorksheet(xlsxWB,subtitleidx[i],gridLines = FALSE)
    writeData(xlsxWB,sheet = subtitleidx[i],x=titleVec[mainConfig[,15]!="NA"][i],startCol = 2,startRow = 2)
    writeData(xlsxWB,sheet = subtitleidx[i],x=subtitleVec[mainConfig[,15]!="NA"][i],startCol = 2,startRow = 3)
    addStyle(xlsxWB,style = titleHeaderStyle,sheet = subtitleidx[i],rows = 2:3,cols =2)
  }
  ##fail Slides Summary
  addWorksheet(xlsxWB,"FailSlides",gridLines = FALSE)
  #f_df<-readRDS(paste0(datenDir,"TestFailSlides.rds"))
  f_df <-readRDS(paste0(datenDir,"failslide.rds"))
  writeData(xlsxWB,sheet = "FailSlides",x=f_df,startCol = 2,startRow = 5)
  #addStyle(xlsxWB,style = tableDataStyle,sheet = "FailSlides",rows = 5:100,cols = 2:10)
  
  ##good Slides Summary
  addWorksheet(xlsxWB,"GoodSlides",gridLines = FALSE)
  isGoodSlides <- readRDS(paste0(datenDir,"GoodSlideFrame.rds"))
  writeData(xlsxWB,sheet = "GoodSlides",x=isGoodSlides,startCol = 2,startRow = 5)
  
  
  
  rdsNames<-mainConfig[,6]
  extractType<-mainConfig[,15]
  rdsNames<-rdsNames[mainConfig[,15]!="NA"]
  extractType<-extractType[mainConfig[,15]!="NA"]
  breaks<-mainConfig[,15:22]
  breaks<-subset(breaks,breaks$EXCEL_EXTRACT!="NA")
  breaks <- breaks[,3:8]
  #breaks<-subset(breaks,breaks$ExcelExtract!="NA")
  xLabel<-mainConfig[,23]
  yLabel<-mainConfig[,24]
  xLabel<-xLabel[mainConfig[,15]!="NA"]
  yLabel<-yLabel[mainConfig[,15]!="NA"]
  id<-mainConfig[,1]
  id<-id[mainConfig[,15]!="NA"]
  
  
  # rdsNames<-mainConfig1[,6]
  # extractType<-mainConfig1[,15]
  # rdsNames<-rdsNames[mainConfig1[,15]!="NA"]
  # extractType<-extractType[mainConfig1[,15]!="NA"]
  # breaks<-mainConfig1[,15:22]
  # breaks<-subset(breaks,breaks$EXCEL_EXTRACT!="NA")
  # breaks <- breaks[,3:8]
  # xLabel<-mainConfig1[,23]
  # yLabel<-mainConfig1[,24]
  # xLabel<-xLabel[mainConfig1[,15]!="NA"]
  # yLabel<-yLabel[mainConfig1[,15]!="NA"]
  # id<-mainConfig1[,1]
  # id<-id[mainConfig1[,15]!="NA"]
  
  isGoodSlide <- readRDS(file=paste0(datenDir,"isGoodSlide.rds"))
  isG<-isGoodSlide[mainConfig[,15]!="NA"]
  #loopdf<-loopdf[which(isGoodSlide) & mainConfig[,15]!="NA"]
  loopdf<-data.frame(rdsNames,extractType,subtitleidx,breaks,id,isG)
  for(idx in 1:length(loopdf$id))
    
  {
    if(loopdf$id[idx]==221 && loopdf$isG[idx])
    {
      x<-readRDS(paste0(datenDir,loopdf$rdsNames[idx],".rds"))
      classCodes<-c(0,1,2,3,4,5,6,7,15)
      classNames = c("Sport","Comfort","Sport Plus","Race","Economy","Comfort Plus","Offroad","Manual","SNA")
      df<-data.frame(classCodes,classNames)
      c<- df$classCodes %in% names(x)
      c1<-classNames[c]
      x_df<-createTableDF(x/3600,classNames = c1)
      #changes done on 23-08-2017 
      writeData(xlsxWB,sheet = subtitleidx[idx],x=vehicleNum,startCol = 1,startRow = 1,headerStyle = tableHeaderStyle,borders = "all")
      writeData(xlsxWB,sheet = subtitleidx[idx],x=x_df,startCol = 2,startRow = 5,headerStyle = tableHeaderStyle,borders = "all")
    }
    # else if(loopdf$id[idx]==220 && loopdf$isG[idx])
    # {
    #   print("_____I CAME HERE________")
    #   breaksX<-c(seq(breaks[idx,1],breaks[idx,2],breaks[idx,3]),Inf)
    #   x<-readRDS(paste0(datenDir,loopdf$rdsNames[idx],".rds"))
    #   x<-table(x)
    #   x_df<-createTableDFCounts(x,breaksX[breaksX>=0])
    #   #changes done on 23-08-2017 
    #   writeData(xlsxWB,sheet = subtitleidx[idx],x=vehicleNum,startCol = 1,startRow = 1,headerStyle = tableHeaderStyle,borders = "all")
    #   writeData(xlsxWB,sheet = subtitleidx[idx],x=x_df,startCol = 2,startRow = 5,headerStyle = tableHeaderStyle,borders = "all")
    #   print("I COMPLETED")
    #   }
    else if(loopdf$extractType[idx]=="Table" && !is.na(breaks[idx,1]) && loopdf$isG[idx])
    {
      
      if(paste0(loopdf$rdsNames[idx],".rds") %in% list.files(datenDir))
      {
        if(loopdf$id[idx]==2061)
        {
          breaksX<-c(-Inf,seq(breaks[idx,1],breaks[idx,2],breaks[idx,3]),Inf)
          breaksX<-breaksX[breaksX>0]
          breaksX <- c(0.1,breaksX)
          x<-readRDS(paste0(datenDir,loopdf$rdsNames[idx],".rds"))
          x_df<-createTableDF(x/3600,breaksX)
          #changes done on 23-08-2017 
          writeData(xlsxWB,sheet = subtitleidx[idx],x=vehicleNum,startCol = 1,startRow = 1,headerStyle = tableHeaderStyle,borders = "all")
          writeData(xlsxWB,sheet = subtitleidx[idx],x=x_df,startCol = 2,startRow = 5,headerStyle = tableHeaderStyle,borders = "all")
          
        }
        else if(loopdf$id[idx]==208)
        {
          breaksX<-c(-Inf,seq(breaks[idx,1],breaks[idx,2],breaks[idx,3]),Inf)
          breaksX<-breaksX[breaksX>=0]
          x<-readRDS(paste0(datenDir,loopdf$rdsNames[idx],".rds"))
          x_df<-createTableDFCounts(x,breaksX)
          #changes done on 23-08-2017 
          writeData(xlsxWB,sheet = subtitleidx[idx],x=vehicleNum,startCol = 1,startRow = 1,headerStyle = tableHeaderStyle,borders = "all")
          writeData(xlsxWB,sheet = subtitleidx[idx],x=x_df,startCol = 2,startRow = 5,headerStyle = tableHeaderStyle,borders = "all")
          
        }
        else if(loopdf$id[idx]==210)
        {
          breaksX<-c(-Inf,seq(breaks[idx,1],breaks[idx,2],breaks[idx,3]),Inf)
          breaksX<-breaksX[breaksX>=0]
          x<-readRDS(paste0(datenDir,loopdf$rdsNames[idx],".rds"))
          x_df<-createTableDF(x/3600,breaksX)
          #changes done on 23-08-2017 
          writeData(xlsxWB,sheet = subtitleidx[idx],x=vehicleNum,startCol = 1,startRow = 1,headerStyle = tableHeaderStyle,borders = "all")
          writeData(xlsxWB,sheet = subtitleidx[idx],x=x_df,startCol = 2,startRow = 5,headerStyle = tableHeaderStyle,borders = "all")
          
        }
        else if(loopdf$id[idx]==212)
        {
          breaksX<-c(-Inf,seq(breaks[idx,1],breaks[idx,2],breaks[idx,3]),Inf)
          breaksX<-breaksX[breaksX>=0]
          x<-readRDS(paste0(datenDir,loopdf$rdsNames[idx],".rds"))
          x_df<-createTableDFCounts(x,breaksX)
          #changes done on 23-08-2017 
          writeData(xlsxWB,sheet = subtitleidx[idx],x=vehicleNum,startCol = 1,startRow = 1,headerStyle = tableHeaderStyle,borders = "all")
          writeData(xlsxWB,sheet = subtitleidx[idx],x=x_df,startCol = 2,startRow = 5,headerStyle = tableHeaderStyle,borders = "all")
          
        }
        else if(loopdf$id[idx]==223)
        {
          breaksX<-c(-Inf,seq(breaks[idx,1],breaks[idx,2],breaks[idx,3]),Inf)
          x<-readRDS(paste0(datenDir,loopdf$rdsNames[idx],".rds"))
          x_df<-createTableDFCounts(x,breaksX)
          #changes done on 23-08-2017 
          writeData(xlsxWB,sheet = subtitleidx[idx],x=vehicleNum,startCol = 1,startRow = 1,headerStyle = tableHeaderStyle,borders = "all")
          writeData(xlsxWB,sheet = subtitleidx[idx],x=x_df,startCol = 2,startRow = 5,headerStyle = tableHeaderStyle,borders = "all")
          
        }
        else if(loopdf$id[idx]==224)
        {
          breaksX<-c(-Inf,seq(breaks[idx,1],breaks[idx,2],breaks[idx,3]),Inf)
          breaksX<-breaksX[breaksX>=0]
          x<-readRDS(paste0(datenDir,loopdf$rdsNames[idx],".rds"))
          x_df<-createTableDFCounts(x,breaksX)
          #changes done on 23-08-2017 
          writeData(xlsxWB,sheet = subtitleidx[idx],x=vehicleNum,startCol = 1,startRow = 1,headerStyle = tableHeaderStyle,borders = "all")
          writeData(xlsxWB,sheet = subtitleidx[idx],x=x_df,startCol = 2,startRow = 5,headerStyle = tableHeaderStyle,borders = "all")
          
        }
        else if(loopdf$id[idx]==226)
        {
          breaksX<-c(-Inf,seq(breaks[idx,1],breaks[idx,2],breaks[idx,3]),Inf)
          breaksX<-breaksX[breaksX>0]
          breaksX <- sort(unique(c(0,1,breaksX)))
          x<-readRDS(paste0(datenDir,loopdf$rdsNames[idx],".rds"))
          x_df<-createTableDFCounts(x,breaksX)
          #changes done on 23-08-2017 
          writeData(xlsxWB,sheet = subtitleidx[idx],x=vehicleNum,startCol = 1,startRow = 1,headerStyle = tableHeaderStyle,borders = "all")
          writeData(xlsxWB,sheet = subtitleidx[idx],x=x_df,startCol = 2,startRow = 5,headerStyle = tableHeaderStyle,borders = "all")
        }
        else if(loopdf$id[idx]==2261)
        {
          breaksX<-c(-Inf,seq(breaks[idx,1],breaks[idx,2],breaks[idx,3]),Inf)
          breaksX<-breaksX[breaksX>0]
          breaksX <- sort(unique(c(0,0.01,breaksX)))
          x<-readRDS(paste0(datenDir,loopdf$rdsNames[idx],".rds"))
          x_df<-createTableDFCounts(x,breaksX)
          #changes done on 23-08-2017 
          writeData(xlsxWB,sheet = subtitleidx[idx],x=vehicleNum,startCol = 1,startRow = 1,headerStyle = tableHeaderStyle,borders = "all")
          writeData(xlsxWB,sheet = subtitleidx[idx],x=x_df,startCol = 2,startRow = 5,headerStyle = tableHeaderStyle,borders = "all")
        }
        else if(loopdf$id[idx]==227)
        {
          breaksX<-c(-Inf,seq(breaks[idx,1],breaks[idx,2],breaks[idx,3]),Inf)
          x<-readRDS(paste0(datenDir,loopdf$rdsNames[idx],".rds"))
          x_df<-createTableDF(x/3600,breaksX)
          #changes done on 23-08-2017 
          writeData(xlsxWB,sheet = subtitleidx[idx],x=vehicleNum,startCol = 1,startRow = 1,headerStyle = tableHeaderStyle,borders = "all")
          writeData(xlsxWB,sheet = subtitleidx[idx],x=x_df,startCol = 2,startRow = 5,headerStyle = tableHeaderStyle,borders = "all")
          
        }
        else if(loopdf$id[idx]==228)
        {
          breaksX<-c(-Inf,seq(breaks[idx,1],breaks[idx,2],breaks[idx,3]),Inf)
          x<-readRDS(paste0(datenDir,loopdf$rdsNames[idx],".rds"))
          x_df<-createTableDF(x/3600,breaksX)
          #changes done on 23-08-2017 
          writeData(xlsxWB,sheet = subtitleidx[idx],x=vehicleNum,startCol = 1,startRow = 1,headerStyle = tableHeaderStyle,borders = "all")
          writeData(xlsxWB,sheet = subtitleidx[idx],x=x_df,startCol = 2,startRow = 5,headerStyle = tableHeaderStyle,borders = "all")
          
        }
        else if(loopdf$id[idx]==230)
        {
          breaksX<-c(-Inf,seq(breaks[idx,1],breaks[idx,2],breaks[idx,3]),Inf)
          x<-readRDS(paste0(datenDir,loopdf$rdsNames[idx],".rds"))
          x_df<-createTableDF(x/3600,breaksX)
          #changes done on 23-08-2017 
          writeData(xlsxWB,sheet = subtitleidx[idx],x=vehicleNum,startCol = 1,startRow = 1,headerStyle = tableHeaderStyle,borders = "all")
          writeData(xlsxWB,sheet = subtitleidx[idx],x=x_df,startCol = 2,startRow = 5,headerStyle = tableHeaderStyle,borders = "all")
          
        }
        else if(loopdf$id[idx]==233)
        {
          breaksX<-c(-Inf,seq(breaks[idx,1],breaks[idx,2],breaks[idx,3]),Inf)
          breaksX<-breaksX[breaksX>=0]
          x<-readRDS(paste0(datenDir,loopdf$rdsNames[idx],".rds"))
          x_df<-createTableDF(x/3600,breaksX)
          #changes done on 23-08-2017 
          writeData(xlsxWB,sheet = subtitleidx[idx],x=vehicleNum,startCol = 1,startRow = 1,headerStyle = tableHeaderStyle,borders = "all")
          writeData(xlsxWB,sheet = subtitleidx[idx],x=x_df,startCol = 2,startRow = 5,headerStyle = tableHeaderStyle,borders = "all")
          
        }
        else if(loopdf$id[idx]==244)
        {
          breaksX<-c(-Inf,seq(breaks[idx,1],breaks[idx,2],breaks[idx,3]),Inf)
          x<-readRDS(paste0(datenDir,loopdf$rdsNames[idx],".rds"))
          x_df<-createTableDF(x/3600,breaksX)
          #changes done on 23-08-2017 
          writeData(xlsxWB,sheet = subtitleidx[idx],x=vehicleNum,startCol = 1,startRow = 1,headerStyle = tableHeaderStyle,borders = "all")
          writeData(xlsxWB,sheet = subtitleidx[idx],x=x_df,startCol = 2,startRow = 5,headerStyle = tableHeaderStyle,borders = "all")
          
        }
        else if(loopdf$id[idx]==24441)
        {
          breaksX<-c(-Inf,seq(breaks[idx,1],breaks[idx,2],breaks[idx,3]),Inf)
          x<-readRDS(paste0(datenDir,loopdf$rdsNames[idx],".rds"))
          x_df<-createTableDFCounts(x,breaksX)
          #changes done on 23-08-2017 
          writeData(xlsxWB,sheet = subtitleidx[idx],x=vehicleNum,startCol = 1,startRow = 1,headerStyle = tableHeaderStyle,borders = "all")
          writeData(xlsxWB,sheet = subtitleidx[idx],x=x_df,startCol = 2,startRow = 5,headerStyle = tableHeaderStyle,borders = "all")
          
        }
        else if(loopdf$id[idx]==24442)
        {
          breaksX<-c(-Inf,seq(breaks[idx,1],breaks[idx,2],breaks[idx,3]),Inf)
          x<-readRDS(paste0(datenDir,loopdf$rdsNames[idx],".rds"))
          x_df<-createTableDFCounts(x,breaksX)
          #changes done on 23-08-2017 
          writeData(xlsxWB,sheet = subtitleidx[idx],x=vehicleNum,startCol = 1,startRow = 1,headerStyle = tableHeaderStyle,borders = "all")
          writeData(xlsxWB,sheet = subtitleidx[idx],x=x_df,startCol = 2,startRow = 5,headerStyle = tableHeaderStyle,borders = "all")
          
        }
        else if(loopdf$id[idx]==24443)
        {
          breaksX<-c(-Inf,seq(breaks[idx,1],breaks[idx,2],breaks[idx,3]),Inf)
          x<-readRDS(paste0(datenDir,loopdf$rdsNames[idx],".rds"))
          x_df<-createTableDFCounts(x,breaksX)
          #changes done on 23-08-2017 
          writeData(xlsxWB,sheet = subtitleidx[idx],x=vehicleNum,startCol = 1,startRow = 1,headerStyle = tableHeaderStyle,borders = "all")
          writeData(xlsxWB,sheet = subtitleidx[idx],x=x_df,startCol = 2,startRow = 5,headerStyle = tableHeaderStyle,borders = "all")
          
        }
        else if(loopdf$id[idx]==24444)
        {
          breaksX<-c(-Inf,seq(breaks[idx,1],breaks[idx,2],breaks[idx,3]),Inf)
          x<-readRDS(paste0(datenDir,loopdf$rdsNames[idx],".rds"))
          x_df<-createTableDFCounts(x,breaksX)
          #changes done on 23-08-2017 
          writeData(xlsxWB,sheet = subtitleidx[idx],x=vehicleNum,startCol = 1,startRow = 1,headerStyle = tableHeaderStyle,borders = "all")
          writeData(xlsxWB,sheet = subtitleidx[idx],x=x_df,startCol = 2,startRow = 5,headerStyle = tableHeaderStyle,borders = "all")
          
        }
        else if(loopdf$id[idx]==245)
        {
          breaksX<-c(-Inf,seq(breaks[idx,1],breaks[idx,2],breaks[idx,3]),Inf)
          breaksX<-breaksX[breaksX>=0]
          x<-readRDS(paste0(datenDir,loopdf$rdsNames[idx],".rds"))
          x_df<-createTableDF(x/3600,breaksX)
          #changes done on 23-08-2017 
          writeData(xlsxWB,sheet = subtitleidx[idx],x=vehicleNum,startCol = 1,startRow = 1,headerStyle = tableHeaderStyle,borders = "all")
          writeData(xlsxWB,sheet = subtitleidx[idx],x=x_df,startCol = 2,startRow = 5,headerStyle = tableHeaderStyle,borders = "all")
          
        }
        else if(loopdf$id[idx]==247)
        {
          breaksX<-c(-Inf,seq(breaks[idx,1],breaks[idx,2],breaks[idx,3]),Inf)
          x<-readRDS(paste0(datenDir,loopdf$rdsNames[idx],".rds"))
          x_df<-createTableDF(x/3600,breaksX)
          #changes done on 23-08-2017 
          writeData(xlsxWB,sheet = subtitleidx[idx],x=vehicleNum,startCol = 1,startRow = 1,headerStyle = tableHeaderStyle,borders = "all")
          writeData(xlsxWB,sheet = subtitleidx[idx],x=x_df,startCol = 2,startRow = 5,headerStyle = tableHeaderStyle,borders = "all")
          
        }
        else if(loopdf$id[idx]==249)
        {
          breaksX<-c(-Inf,seq(breaks[idx,1],breaks[idx,2],breaks[idx,3]),Inf)
          breaksX<-breaksX[breaksX>=0]
          x<-readRDS(paste0(datenDir,loopdf$rdsNames[idx],".rds"))
          x_df<-createTableDFCounts(x,breaksX)
          #changes done on 23-08-2017 
          writeData(xlsxWB,sheet = subtitleidx[idx],x=vehicleNum,startCol = 1,startRow = 1,headerStyle = tableHeaderStyle,borders = "all")
          writeData(xlsxWB,sheet = subtitleidx[idx],x=x_df,startCol = 2,startRow = 5,headerStyle = tableHeaderStyle,borders = "all")
          
        }
        else if(loopdf$id[idx]==250)
        {
          breaksX<-c(-Inf,seq(breaks[idx,1],breaks[idx,2],breaks[idx,3]),Inf)
          x<-readRDS(paste0(datenDir,loopdf$rdsNames[idx],".rds"))
          x_df<-createTableDFCounts(x,breaksX)
          #changes done on 23-08-2017 
          writeData(xlsxWB,sheet = subtitleidx[idx],x=vehicleNum,startCol = 1,startRow = 1,headerStyle = tableHeaderStyle,borders = "all")
          writeData(xlsxWB,sheet = subtitleidx[idx],x=x_df,startCol = 2,startRow = 5,headerStyle = tableHeaderStyle,borders = "all")
          
        }
        else if(loopdf$id[idx]==2501)
        {
          breaksX<-c(-Inf,seq(breaks[idx,1],breaks[idx,2],breaks[idx,3]),Inf)
          x<-readRDS(paste0(datenDir,loopdf$rdsNames[idx],".rds"))
          x_df<-createTableDFCounts(x,breaksX)
          #changes done on 23-08-2017 
          writeData(xlsxWB,sheet = subtitleidx[idx],x=vehicleNum,startCol = 1,startRow = 1,headerStyle = tableHeaderStyle,borders = "all")
          writeData(xlsxWB,sheet = subtitleidx[idx],x=x_df,startCol = 2,startRow = 5,headerStyle = tableHeaderStyle,borders = "all")
          
        }
        else if(loopdf$id[idx]==2502)
        {
          breaksX<-c(-Inf,seq(breaks[idx,1],breaks[idx,2],breaks[idx,3]),Inf)
          x<-readRDS(paste0(datenDir,loopdf$rdsNames[idx],".rds"))
          x_df<-createTableDFCounts(x,breaksX)
          #changes done on 23-08-2017 
          writeData(xlsxWB,sheet = subtitleidx[idx],x=vehicleNum,startCol = 1,startRow = 1,headerStyle = tableHeaderStyle,borders = "all")
          writeData(xlsxWB,sheet = subtitleidx[idx],x=x_df,startCol = 2,startRow = 5,headerStyle = tableHeaderStyle,borders = "all")
          
        }
        else if(loopdf$id[idx]==2503)
        {
          breaksX<-c(-Inf,seq(breaks[idx,1],breaks[idx,2],breaks[idx,3]),Inf)
          x<-readRDS(paste0(datenDir,loopdf$rdsNames[idx],".rds"))
          x_df<-createTableDFCounts(x,breaksX)
          #changes done on 23-08-2017 
          writeData(xlsxWB,sheet = subtitleidx[idx],x=vehicleNum,startCol = 1,startRow = 1,headerStyle = tableHeaderStyle,borders = "all")
          writeData(xlsxWB,sheet = subtitleidx[idx],x=x_df,startCol = 2,startRow = 5,headerStyle = tableHeaderStyle,borders = "all")
          
        }
        else if(loopdf$id[idx]==255)
        {
          breaksX<-c(-Inf,seq(breaks[idx,1],breaks[idx,2],breaks[idx,3]),Inf)
          x<-readRDS(paste0(datenDir,loopdf$rdsNames[idx],".rds"))
          x_df<-createTableDF(x/3600,breaksX)
          #changes done on 23-08-2017 
          writeData(xlsxWB,sheet = subtitleidx[idx],x=vehicleNum,startCol = 1,startRow = 1,headerStyle = tableHeaderStyle,borders = "all")
          writeData(xlsxWB,sheet = subtitleidx[idx],x=x_df,startCol = 2,startRow = 5,headerStyle = tableHeaderStyle,borders = "all")
          
        }
        else if(loopdf$id[idx]==257)
        {
          breaksX<-c(-Inf,seq(breaks[idx,1],breaks[idx,2],breaks[idx,3]),Inf)
          breaksX<-breaksX[breaksX>=0&breaksX<=100]
          x<-readRDS(paste0(datenDir,loopdf$rdsNames[idx],".rds"))
          x_df<-createTableDF(x/3600,breaksX)
          #changes done on 23-08-2017 
          writeData(xlsxWB,sheet = subtitleidx[idx],x=vehicleNum,startCol = 1,startRow = 1,headerStyle = tableHeaderStyle,borders = "all")
          writeData(xlsxWB,sheet = subtitleidx[idx],x=x_df,startCol = 2,startRow = 5,headerStyle = tableHeaderStyle,borders = "all")
          
        }
        else if(loopdf$id[idx]==258)
        {
          breaksX<-c(-Inf,seq(breaks[idx,1],breaks[idx,2],breaks[idx,3]),Inf)
          breaksX<-breaksX[breaksX>=0]
          x<-readRDS(paste0(datenDir,loopdf$rdsNames[idx],".rds"))
          x_df<-createTableDF(x/3600,breaksX)
          #changes done on 23-08-2017 
          writeData(xlsxWB,sheet = subtitleidx[idx],x=vehicleNum,startCol = 1,startRow = 1,headerStyle = tableHeaderStyle,borders = "all")
          writeData(xlsxWB,sheet = subtitleidx[idx],x=x_df,startCol = 2,startRow = 5,headerStyle = tableHeaderStyle,borders = "all")
          
        }
        else if(loopdf$id[idx]==259)
        {
          breaksX<-c(-Inf,seq(breaks[idx,1],breaks[idx,2],breaks[idx,3]),Inf)
          breaksX<-breaksX[breaksX>=0]
          x<-readRDS(paste0(datenDir,loopdf$rdsNames[idx],".rds"))
          x_df<-createTableDF(x/3600,breaksX)
          #changes done on 23-08-2017 
          writeData(xlsxWB,sheet = subtitleidx[idx],x=vehicleNum,startCol = 1,startRow = 1,headerStyle = tableHeaderStyle,borders = "all")
          writeData(xlsxWB,sheet = subtitleidx[idx],x=x_df,startCol = 2,startRow = 5,headerStyle = tableHeaderStyle,borders = "all")
          
        }
        else if(loopdf$id[idx]==264)
        {
          breaksX<-c(-Inf,seq(breaks[idx,1],breaks[idx,2],breaks[idx,3]),Inf)
          breaksX<-breaksX[breaksX>0 & breaksX<Inf]
          x<-readRDS(paste0(datenDir,loopdf$rdsNames[idx],".rds"))
          x_df<-createStackTableDF(x,breaksX)
          #changes done on 23-08-2017 
          writeData(xlsxWB,sheet = subtitleidx[idx],x=vehicleNum,startCol = 1,startRow = 1,headerStyle = tableHeaderStyle,borders = "all")
          writeData(xlsxWB,sheet = subtitleidx[idx],x=x_df,startCol = 2,startRow = 5,headerStyle = tableHeaderStyle,borders = "all")
          
        }
        else if(loopdf$id[idx]==266)
        {
          breaksX<-c(-Inf,seq(breaks[idx,1],breaks[idx,2],breaks[idx,3]),Inf)
          breaksX<-breaksX[breaksX>=0 & breaksX<Inf]
          x<-readRDS(paste0(datenDir,loopdf$rdsNames[idx],".rds"))
          x_df<-createStackTableDF(x,breaksX)
          #changes done on 23-08-2017 
          writeData(xlsxWB,sheet = subtitleidx[idx],x=vehicleNum,startCol = 1,startRow = 1,headerStyle = tableHeaderStyle,borders = "all")
          writeData(xlsxWB,sheet = subtitleidx[idx],x=x_df,startCol = 2,startRow = 5,headerStyle = tableHeaderStyle,borders = "all")
          
        }
        else if(loopdf$id[idx]==270)
        {
          breaksX<-c(-Inf,seq(breaks[idx,1],breaks[idx,2],breaks[idx,3]),Inf)
          x<-readRDS(paste0(datenDir,loopdf$rdsNames[idx],".rds"))
          x_df<-createTableDF(x/3600,breaksX)
          #changes done on 23-08-2017 
          writeData(xlsxWB,sheet = subtitleidx[idx],x=vehicleNum,startCol = 1,startRow = 1,headerStyle = tableHeaderStyle,borders = "all")
          writeData(xlsxWB,sheet = subtitleidx[idx],x=x_df,startCol = 2,startRow = 5,headerStyle = tableHeaderStyle,borders = "all")
          
        }
      }
    }
    else if(loopdf$extractType[idx]=="Map" && loopdf$isG[idx])
    {
      breaksX1<-c(-Inf,seq(breaks[idx,1],breaks[idx,2],breaks[idx,3]),Inf)    
      breaksY1<-c(-Inf,seq(breaks[idx,4],breaks[idx,5],breaks[idx,6]),Inf)  
      if(paste0(loopdf$rdsNames[idx],".rds") %in% list.files(datenDir))
      {
        #print(paste0(loopdf$rdsNames[idx],".rds"))
        x<-readRDS(paste0(datenDir,loopdf$rdsNames[idx],".rds"))
        if(loopdf$id[idx]==205)
        {
          breaksX1<-breaksX1[breaksX1>=0]
          breaksX1[breaksX1==0]<-1
          x_df<-createHeatMapDF(x/3600,breaksX1,breaksY1)
          #changes done on 23-08-2017 
          writeData(xlsxWB,sheet = subtitleidx[idx],x=vehicleNum,startCol = 1,startRow = 1,headerStyle = tableHeaderStyle,borders = "all")
          writeData(xlsxWB,sheet = subtitleidx[idx],x=x_df,startCol = 2,startRow = 5,headerStyle = tableHeaderStyle,borders = "all",colNames = TRUE,rowNames = TRUE)
          
        }
        else if(loopdf$id[idx]==206)
        {
          breaksX1 <- breaksX1[breaksX1>=0]
          if(any(breaksX1==0)){
            breaksX1 <- sort(unique(c(0.01,breaksX1)))
            breaksX1 <- breaksX1[breaksX1>0]
            x_df<-createHeatMapDF(x/3600,breaksX1,breaksY1)
            #changes done on 23-08-2017 
            writeData(xlsxWB,sheet = subtitleidx[idx],x=vehicleNum,startCol = 1,startRow = 1,headerStyle = tableHeaderStyle,borders = "all")
            writeData(xlsxWB,sheet = subtitleidx[idx],x=x_df,startCol = 2,startRow = 5,headerStyle = tableHeaderStyle,borders = "all",colNames = TRUE,rowNames = TRUE)
            
          }
        }
        else if(loopdf$id[idx]==209)
        {
          breaksX1=round(breaksX1,1)
          breaksY1=round(breaksY1,1)
          x_df<-createHeatMapDF(x/3600,breaksX1,breaksY1)
          #changes done on 23-08-2017 
          writeData(xlsxWB,sheet = subtitleidx[idx],x=vehicleNum,startCol = 1,startRow = 1,headerStyle = tableHeaderStyle,borders = "all")
          writeData(xlsxWB,sheet = subtitleidx[idx],x=x_df,startCol = 2,startRow = 5,headerStyle = tableHeaderStyle,borders = "all",colNames = TRUE,rowNames = TRUE)
          
        }
        else if(loopdf$id[idx]==231)
        {
          breaksX1 <- breaksX1[breaksX1>=0]
          breaksX1[breaksX1==0] <- 1
          x_df<-createHeatMapDF(x/3600,breaksX1,breaksY1)
          #changes done on 23-08-2017 
          writeData(xlsxWB,sheet = subtitleidx[idx],x=vehicleNum,startCol = 1,startRow = 1,headerStyle = tableHeaderStyle,borders = "all")
          writeData(xlsxWB,sheet = subtitleidx[idx],x=x_df,startCol = 2,startRow = 5,headerStyle = tableHeaderStyle,borders = "all",colNames = TRUE,rowNames = TRUE)
          
        }
        else if(loopdf$id[idx]==232)
        {
          breaksX1 <- breaksX1[breaksX1>=0]
          breaksX1[breaksX1==0] <- 1
          breaksY1<-breaksY1[breaksY1>=0]
          x_df<-createHeatMapDF(x/3600,breaksX1,breaksY1)
          #changes done on 23-08-2017 
          writeData(xlsxWB,sheet = subtitleidx[idx],x=vehicleNum,startCol = 1,startRow = 1,headerStyle = tableHeaderStyle,borders = "all")
          writeData(xlsxWB,sheet = subtitleidx[idx],x=x_df,startCol = 2,startRow = 5,headerStyle = tableHeaderStyle,borders = "all",colNames = TRUE,rowNames = TRUE)
          
        }
        else if(loopdf$id[idx]==2441)
        {
          breaksX1 <- breaksX1[breaksX1>=0]
          breaksX1[breaksX1==0] <- 1
          x_df<-createHeatMapDF(x/3600,breaksX1,breaksY1)
          #changes done on 23-08-2017 
          writeData(xlsxWB,sheet = subtitleidx[idx],x=vehicleNum,startCol = 1,startRow = 1,headerStyle = tableHeaderStyle,borders = "all")
          writeData(xlsxWB,sheet = subtitleidx[idx],x=x_df,startCol = 2,startRow = 5,headerStyle = tableHeaderStyle,borders = "all",colNames = TRUE,rowNames = TRUE)
          
        }
        else if(loopdf$id[idx]==260)
        {
          breaksX1 <- breaksX1[breaksX1>=0]
          breaksX1[breaksX1==0] <- 1
          x_df<-createHeatMapDF(x/3600,breaksX1,breaksY1)
          #changes done on 23-08-2017 
          writeData(xlsxWB,sheet = subtitleidx[idx],x=vehicleNum,startCol = 1,startRow = 1,headerStyle = tableHeaderStyle,borders = "all")
          writeData(xlsxWB,sheet = subtitleidx[idx],x=x_df,startCol = 2,startRow = 5,headerStyle = tableHeaderStyle,borders = "all",colNames = TRUE,rowNames = TRUE)
          
        }
        else if(loopdf$id[idx]==261)
        {
          breaksX1<-c(-Inf,seq(breaks[idx,1],breaks[idx,2],200),Inf)    
          breaksY1<-c(-Inf,seq(breaks[idx,4],breaks[idx,5],1.5),Inf)  
          breaksX1 <- breaksX1[breaksX1>=0]
          breaksX1[breaksX1==0] <- 1
          x_df<-createHeatMapDF(x/3600,breaksX1,breaksY1)
          #changes done on 23-08-2017 
          writeData(xlsxWB,sheet = subtitleidx[idx],x=vehicleNum,startCol = 1,startRow = 1,headerStyle = tableHeaderStyle,borders = "all")
          writeData(xlsxWB,sheet = subtitleidx[idx],x=x_df,startCol = 2,startRow = 5,headerStyle = tableHeaderStyle,borders = "all",colNames = TRUE,rowNames = TRUE)
          
        }
        else if(loopdf$id[idx]==262)
        {
          breaksX1<-c(-Inf,seq(breaks[idx,1],breaks[idx,2],200),Inf)    
          breaksY1<-c(-Inf,seq(breaks[idx,4],breaks[idx,5],10),Inf)  
          breaksX1 <- breaksX1[breaksX1>=0]
          breaksX1[breaksX1==0] <- 1
          x_df<-createHeatMapDF(x/3600,breaksX1,breaksY1)
          #changes done on 23-08-2017 
          writeData(xlsxWB,sheet = subtitleidx[idx],x=vehicleNum,startCol = 1,startRow = 1,headerStyle = tableHeaderStyle,borders = "all")
          writeData(xlsxWB,sheet = subtitleidx[idx],x=x_df,startCol = 2,startRow = 5,headerStyle = tableHeaderStyle,borders = "all",colNames = TRUE,rowNames = TRUE)
          
        }
        else if(loopdf$id[idx]==263)
        {
          breaksX1<-c(-Inf,seq(breaks[idx,1],breaks[idx,2],10),Inf)    
          breaksY1<-c(-Inf,seq(breaks[idx,4],breaks[idx,5],1.5),Inf)  
          # breaksX1 <- breaksX1[breaksX1>=0]
          breaksX1 <- breaksX1
          # breaksX1[breaksX1==0] <- 1
          x_df<-createHeatMapDF(x/3600,breaksX1,breaksY1)
          #changes done on 23-08-2017 
          writeData(xlsxWB,sheet = subtitleidx[idx],x=vehicleNum,startCol = 1,startRow = 1,headerStyle = tableHeaderStyle,borders = "all")
          writeData(xlsxWB,sheet = subtitleidx[idx],x=x_df,startCol = 2,startRow = 5,headerStyle = tableHeaderStyle,borders = "all",colNames = TRUE,rowNames = TRUE)
          
        }
        x_df<-createHeatMapDF(x/3600,breaksX1,breaksY1)
        writeData(xlsxWB,sheet = subtitleidx[idx],x=x_df,startCol = 2,startRow = 5,headerStyle = tableHeaderStyle,borders = "all",colNames = TRUE,rowNames = TRUE)
        c<-2+ncol(x_df)
        setColWidths(xlsxWB,sheet = subtitleidx[idx],cols = 2:c,widths = 15)
        #setRowHeights(xlsxWB,sheet = subtitleidx[idx],rows = 5:15,widths = 15)
        mergeCells(xlsxWB,sheet = subtitleidx[idx],cols = 2:c,rows = 4)
        r<-5+nrow(x_df)
        mergeCells(xlsxWB,sheet = subtitleidx[idx],cols = 1,rows = 5:r)
        writeData(xlsxWB,sheet = subtitleidx[idx],x=xLabel[idx],startCol = 2, startRow = 4)
        addStyle(xlsxWB,sheet = subtitleidx[idx],style = xLabelStyle,cols = 2,rows = 4)
        writeData(xlsxWB,sheet = subtitleidx[idx],x=yLabel[idx],startCol = 1, startRow = 5)
        addStyle(xlsxWB,sheet = subtitleidx[idx],style = yLabelStyle,cols = 1,rows = 5)
        
      }
    }
  }
  return(xlsxWB)
}

calcGrad <- function(dir,fzg){
  library("accelerometry")
  library("chron")
  fileId <- NULL
  startId <- NULL
  gradCounts <- NULL
  gradOilTempCounts <- NULL
  gradCountsMile <- NULL
  gradOilTempCountsMile <- NULL
  sigEngSpd <- c("EngRPM","EngSpd","Eng_Spd","NMOT","N_MOT","nmot_w","n_eng")
  sigVehSpd <- c("VehSpd_Disp","Veh_Spd","N","v_veh","vfzg_w","f-V_Rad")
  sigEngOilTemp <- c("EngOilTemp","TOELE","t_oil")
  dataFiles <- list.files(paste0(dir,"Daten10Hz/"))#Daten10Hz
  isCsv <- substr(dataFiles,nchar(dataFiles)-3,nchar(dataFiles))==".csv"
  csvFiles <- dataFiles[isCsv]
  readDate <- sapply(strsplit(csvFiles,"[_-]"),function(x){x[3]})
  csvFiles <- csvFiles[sort(as.numeric(readDate),index.return=TRUE)$ix]
  readDate <- readDate[sort(as.numeric(readDate),index.return=TRUE)$ix]
  csvFilesSplit=strsplit(csvFiles,"10Hz")#10Hz
  for(idx in seq_along(csvFiles)){
    print(paste(idx,"von",sum(isCsv)))
    X <- read.csv(paste0(paste0(dir,"Daten10Hz/"),csvFiles[idx]),sep=";",dec=",")
    Y <- read.csv(paste0(paste0(dir,"Daten/"),paste0(csvFilesSplit[[idx]][1],"1Hz",csvFilesSplit[[idx]][2])),sep=";",dec=",")
    timeX <- as.numeric(chron(substr(X[[1]],1,10),substr(X[[1]],12,nchar(as.character(X[[1]])))))
    timeY <- as.numeric(chron(substr(Y[[1]],1,10),substr(Y[[1]],12,nchar(as.character(Y[[1]])))))
    XEngSpd <- getChann(sigEngSpd,X,10000)
    YEngOilTemp <- getChann(sigEngOilTemp,Y,200)
    XEngOilTemp <- approx(x=timeY,y=YEngOilTemp,xout=timeX,method="linear")[[2]]
    n <- length(XEngSpd)
    CEng0 <- getConditionIdx(XEngSpd,"leq",0)
    grad <- NULL
    gradOilTemp <- NULL
    if(CEng0[[3]]>0 && CEng0[[2]][1]<n){
      maxIdx <- CEng0[[3]]-(max(CEng0[[2]])>=n)
      for(jdx in 1:maxIdx){
        idxEngOn <- (CEng0[[2]][jdx]:min(CEng0[[1]][jdx+1],n,na.rm=TRUE))
        if(any(!is.na(XEngSpd[idxEngOn[2:min(21,length(idxEngOn))]]))){
          eng <- c(vector("numeric",10),XEngSpd[idxEngOn[1:min(21,length(idxEngOn))]])
          t <- seq(from=-1,by=0.1,length.out=length(eng))
          spli <- spline(x=t,y=eng,n=1000)
          spliDiff <- list(x=spli$x,y=c(0,diff(spli$y,2),0))
          startIdx <- which.max(spli$x>0)
          maxIdx <- which.max(spliDiff$y[startIdx:which.max(spli$x>=0.5)])+startIdx-1
          spliDiffY <- spliDiff$y[maxIdx:length(spliDiff$y)]
          rleSpliDiffY <- rle2(as.numeric(spliDiffY/max(spliDiffY)<0.3),indices=TRUE,return.list=TRUE)
          firstIdxBelow <- rleSpliDiffY$starts[rleSpliDiffY$values>0 & rleSpliDiffY$lengths>5]
          endIdx <- maxIdx+firstIdxBelow[1]-1
          lmGrad <- lm(spli$y[startIdx:endIdx]~spli$x[startIdx:endIdx])
          
          if(all(!is.na(lmGrad$coefficients))){
            grad <- c(grad,as.numeric(lmGrad$coefficients[2]))
            gradOilTemp <- c(gradOilTemp,XEngOilTemp[CEng0[[2]][jdx]])
            fileId <- c(fileId,idx)
            startId <- c(startId,jdx)
            
            png(paste0(dir,"Drehzahlgradienten/",paste0(fzg,"_File",idx,"_Start",jdx),".png"),width=1500,height=600)
            plot(t,eng,xlab="Zeit [s]",ylab="Motordrehzahl [U/min]",main=paste0("Fahrzeugdauerlauf ",fzg,"   (Datei ",idx,", Start ",jdx,")"),cex.main=2,cex.lab=1.5,cex.axis=1.5)
            rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col="gray87")
            grid(col="white")
            abline(lmGrad,lwd=2,col="red")
            points(t,eng,col="navy",pch=19)
            dev.off()
          }
        }
      }
    }
    gradCounts <- countTable1D(x=grad,mergeTab=gradCounts,roundDigit=0)
    gradOilTempCounts <- countTable2D(x=grad,y=gradOilTemp,mergeTab=gradOilTempCounts,roundDigitX=0,roundDigitY=0)
    
    YEngSpd <- getChann(sigEngSpd,Y,10000)
    YVehSpd <- getChann(sigVehSpd,Y,400)
    gradCountsMile <- c(gradCountsMile,sum(YVehSpd[!is.na(YEngSpd)],na.rm=TRUE)/3600)
    gradOilTempCountsMile <- c(gradOilTempCountsMile,sum(YVehSpd[!is.na(YEngSpd) & !is.na(YEngOilTemp)],na.rm=TRUE)/3600)
  }
  saveRDS(object=fileId,file=paste0(dir,"Daten/fileId.rds"))
  saveRDS(object=startId,file=paste0(dir,"Daten/startId.rds"))
  saveRDS(object=gradCounts,file=paste0(dir,"Daten/gradCounts.rds"))
  saveRDS(object=gradCountsMile,file=paste0(dir,"Daten/gradCountsMile.rds"))
  saveRDS(object=gradOilTempCounts,file=paste0(dir,"Daten/gradOilTempCounts.rds"))
  saveRDS(object=gradOilTempCountsMile,file=paste0(dir,"Daten/gradOilTempCountsMile.rds"))
}

calc10HzCounts <- function(dir,fzg){
  library("accelerometry")
  library("chron")
  fileId <- NULL
  startId <- NULL
  gradCounts <- NULL
  sigEngSpd <- c("EngRPM","EngSpd","Eng_Spd","NMOT","N_MOT","nmot_w","n_eng")
  sigVehSpd <- c("VehSpd_Disp","Veh_Spd","N","v_veh","vfzg_w","f-V_Rad")
  print(dir)
  dataFiles <- list.files(paste0(dir,"Daten10Hz/"))#Daten10Hz
  isCsv <- substr(dataFiles,nchar(dataFiles)-3,nchar(dataFiles))==".csv"
  csvFiles <- dataFiles[isCsv]
  readDate <- sapply(strsplit(csvFiles,"[_-]"),function(x){x[3]})
  csvFiles <- csvFiles[sort(as.numeric(readDate),index.return=TRUE)$ix]
  readDate <- readDate[sort(as.numeric(readDate),index.return=TRUE)$ix]
  csvFilesSplit=strsplit(csvFiles,"10Hz")#10Hz
  ##changes done on 28-8-2017
  if(length(csvFiles) == 0){
    return()
  }
  fileId <- NULL
  startId <- NULL
  idx<-1
  datadf<-data.frame()
  for(idx in 1:length(csvFiles)){
    print(paste(idx,"von",sum(isCsv)))
    X <- read.csv(paste0(paste0(dir,"Daten10Hz/"),csvFiles[idx]),sep=";",dec=",")
    timeX <- as.numeric(chron(substr(X[[1]],1,10),substr(X[[1]],12,nchar(as.character(X[[1]])))))
    XEngSpd <- getChann(sigEngSpd,X,10000)
    n <- length(XEngSpd)
    CEng0 <- getConditionIdx(XEngSpd,"leq",0)
    grad <- NULL
    if(CEng0[[3]]>0 && CEng0[[2]][1]<n){
      maxIdx <- CEng0[[3]]-(max(CEng0[[2]])>=n)
      for(jdx in 1:maxIdx){
        idxEngOn <- (CEng0[[2]][jdx]:min(CEng0[[1]][jdx+1],n,na.rm=TRUE))
        if(any(!is.na(XEngSpd[idxEngOn[2:min(21,length(idxEngOn))]]))){
          # eng <- c(vector("numeric",0),XEngSpd[idxEngOn[1:min(21,length(idxEngOn))]])
          # t <- seq(from=0,by=0.1,length.out=length(eng))
          # spli <-spline(x=t, y = eng,n = 80)
          # spliDiff <- list(x=spli$x,y=c(0,diff(spli$y,2),0))
          # startIdx <- which.max(spli$x>0)
          # maxIdx <- which.max(spliDiff$y[startIdx:which.max(spli$x>=0.5)])+startIdx-1
          # spliDiffY <- spliDiff$y[maxIdx:length(spliDiff$y)]
          # rleSpliDiffY <- rle2(as.numeric(spliDiffY/max(spliDiffY)<0.3),indices=TRUE,return.list=TRUE)
          # firstIdxBelow <- rleSpliDiffY$starts[rleSpliDiffY$values>0 & rleSpliDiffY$lengths>5]
          # endIdx <- maxIdx+firstIdxBelow[1]-1
          # #print(splin)
          # kdx<-(paste0(idx,"_",jdx))
          # df_temp<-data.frame(kdx,spli$x,spli$y)
          # datadf<-rbind(datadf,df_temp)
          eng <- c(vector("numeric",0),XEngSpd[idxEngOn[1:min(21,length(idxEngOn))]])
          t <- seq(from=0,by=0.1,length.out=length(eng))
          kdx<-(paste0(idx,"_",jdx))
          df_temp<-data.frame(kdx,t,eng)
          datadf<-rbind(datadf,df_temp) 
          
        }
      }
    }
  }
  saveRDS(datadf,file=paste0(dir,"Daten/engspdovertimeCounts.rds"))
  
}
getFailSlides <- function(dir,mainConfig,rsgIsg,pdflatex){
  library("stargazer")
  print("I ma @ failslide")
  isGoodSlide <- paste0(mainConfig[,6],".rds") %in% list.files(paste0(dir,"Daten/"))
  isToLook <- mainConfig[,5]!="barPerTrack" & !is.na(mainConfig[,5]) & isGoodSlide
  # if(engType == "O"){
  #   # varsX <- varsX[varsX != "fuellowPressCounts"]
  #   # varsX <- varsX[varsX != "fuellowPressAnzCounts"]
  #   # varsX <- varsX[varsX != "fuellowPressTempCounts"]
  #   isGoodSlide[63]<-TRUE
  #   isGoodSlide[64]<-TRUE
  #   isGoodSlide[65]<-TRUE
  # }
  # else if (engType == "M"){
  #   # varsX <- varsX[varsX != "dpfRegCounts"]
  #   # varsX <- varsX[varsX != "StellerpositionNDAGRCounts"]
  #   # varsX <- varsX[varsX != "StellerpositionHDAGRCounts"]
  #   # varsX <- varsX[varsX != "egrNumVal"]
  #   isGoodSlide[22]<-TRUE
  #   isGoodSlide[72]<-TRUE
  #   isGoodSlide[73]<-TRUE
  #   isGoodSlide[74]<-TRUE
  # }
  if(any(isToLook)){
    xValNames <- mainConfig[isToLook,6]
    xVals <- sapply(paste0(dir,"Daten/",xValNames,".rds"),readRDS)
    isGoodXVal <- !sapply(xVals,is.null)
    isGoodSlide[isToLook] <- isGoodXVal
    print("f2")
    if(rsgIsg=="ISG"){
      isGoodSlide[mainConfig[,1]=="232.1"] <- FALSE
    }else{
      isGoodSlide[mainConfig[,1]=="231.1"] <- FALSE
    }
    
    failSlides <- matrix(nrow=0,ncol=2)
    for(idx in seq_along(mainConfig[,1])){
      if(!isGoodSlide[idx]){
        failSlides <- rbind(failSlides,ifelse(rep(!is.na(mainConfig[idx,3]),2)>0,c(paste(mainConfig[idx,2],"/",mainConfig[idx,3]),mainConfig[idx,7]),c(mainConfig[idx,2],mainConfig[idx,7])))
      }
    }
    colnames(failSlides) <- c("Fehlende Abschnitte","Begründung")
    
    saveRDS(object=isGoodSlide,file=paste0(dir,"Daten/isGoodSlide.rds"))
    saveRDS(object=failSlides,file=paste0(dir,"Daten/failSlides.rds"))
    # print("f3")
    if(nrow(failSlides)<40){
      failSlidesStar <- stargazer(failSlides,summary=FALSE,colnames=TRUE,rownames=FALSE,digit.separator="")
      failSlidesHeader <- c("\\documentclass[9pt]{extarticle}","\\usepackage{lscape}","\\usepackage{caption}","\\usepackage[ansinew]{inputenc}","\\begin{document}","\\pagestyle{empty}","\\begin{landscape}","\\begin{table}[!htbp] \\centering","\\caption*{\\textbf{Liste der in diesem Bericht fehlenden Abschnitte inklusive Begründung}}","\\footnotesize","\\begin{tabular}{ll}","\\\\[-5ex]\\hline\\hline\\\\[-1.8ex]")
      failSlidesTex <- c(failSlidesHeader,failSlidesStar[10:length(failSlidesStar)],"\\end{landscape}","\\end{document}")
      
      write.table(failSlidesTex,paste0(dir,"failSlides.tex"),row.names=FALSE,col.names=FALSE,quote=FALSE)
      shell(paste0(pdflatex," ",dir,"failSlides.tex"))
      shell(paste0("C:/EngineDL/Softwares/IM/convert -density 300 -rotate 90 -crop 2800x1670+450+550 -alpha remove -background #FFA50050 ",workDir,"failSlides.pdf ",dir,"failSlides1.png")) #Problematic oyo
    }
    else{
      print(nrow(failSlides))
      failSlides1<-failSlides[1:39,]
      failSlides2<-failSlides[40:nrow(failSlides),]
      
      failSlidesStar1 <- stargazer(failSlides1,summary=FALSE,colnames=TRUE,rownames=FALSE,digit.separator="")
      failSlidesHeader1 <- c("\\documentclass[9pt]{extarticle}","\\usepackage{lscape}","\\usepackage{caption}","\\usepackage[ansinew]{inputenc}","\\begin{document}","\\pagestyle{empty}","\\begin{landscape}","\\begin{table}[!htbp] \\centering","\\caption*{\\textbf{Liste der in diesem Bericht fehlenden Abschnitte inklusive Begründung}}","\\footnotesize","\\begin{tabular}{ll}","\\\\[-5ex]\\hline\\hline\\\\[-1.8ex]")
      failSlidesTex1 <- c(failSlidesHeader1,failSlidesStar1[10:length(failSlidesStar1)],"\\end{landscape}","\\end{document}")
      
      failSlidesStar2 <- stargazer(failSlides2,summary=FALSE,colnames=TRUE,rownames=FALSE,digit.separator="")
      failSlidesHeader2 <- c("\\documentclass[9pt]{extarticle}","\\usepackage{lscape}","\\usepackage{caption}","\\usepackage[ansinew]{inputenc}","\\begin{document}","\\pagestyle{empty}","\\begin{landscape}","\\begin{table}[!htbp] \\centering","\\caption*{\\textbf{Liste der in diesem Bericht fehlenden Abschnitte inklusive Begründung}}","\\footnotesize","\\begin{tabular}{ll}","\\\\[-5ex]\\hline\\hline\\\\[-1.8ex]")
      failSlidesTex2 <- c(failSlidesHeader2,failSlidesStar2[10:length(failSlidesStar2)],"\\end{landscape}","\\end{document}")
      
      
      write.table(failSlidesTex1,paste0(dir,"failSlides1.tex"),row.names=FALSE,col.names=FALSE,quote=FALSE)
      shell(paste0(pdflatex," ",dir,"failSlides1.tex"))
      shell(paste0("C:/EngineDL/Softwares/IM/convert -density 300 -rotate 90 -crop 2800x1670+450+550 -alpha remove -background #FFA50050 ",workDir,"failSlides1.pdf ",dir,"failSlides1.png")) #Problematic oyo
      
      write.table(failSlidesTex2,paste0(dir,"failSlides2.tex"),row.names=FALSE,col.names=FALSE,quote=FALSE)
      shell(paste0(pdflatex," ",dir,"failSlides2.tex"))
      shell(paste0("C:/EngineDL/Softwares/IM/convert -density 300 -rotate 90 -crop 2800x1670+450+550 -alpha remove -background #FFA50050 ",workDir,"failSlides2.pdf ",dir,"failSlides2.png")) #Problematic oyo
      
    }
    
    return(isGoodSlide)
  }
}

getDataQuality <- function(dir,pdflatex){
  library(stargazer)
  
  # dataFiles <- list.files(paste0(dir,"Daten/"))
  # isCsv <- substr(dataFiles,nchar(dataFiles)-3,nchar(dataFiles))==".csv"
  # csvFiles <- dataFiles[isCsv]
  # readDate <- sapply(strsplit(csvFiles,"[_-]"),function(x){x[3]})
  # csvFiles <- csvFiles[sort(as.numeric(readDate),index.return=TRUE)$ix]
  # 
  coltable <- readRDS(paste0(dir,"Daten/dataFreqtable.rds"))
  #write.csv(coltable,paste0(projDir,"Compiled_Signals.csv"))
  # coltable$allColNames <- substr(coltable$allColNames,1,33)   ####substring change upto 32 chars only - 03-01-2017
  # coltable$Existenz1 <- round((coltable$Freq / length(csvFiles))*100,1)
  # coltable$Existenz <- paste0(coltable$Existenz1,"%")
  # colnames(coltable)[1] <- "Kanal" 
  # coltable <- coltable[,c(-2,-3)]
  # 
  colnames(coltable) <- c("Kanal","Existenz")
  table1 <- coltable[1:50,]
  table2 <- coltable[51:100,]
  table3 <- coltable[101:150,]
  table4 <- coltable[151:200,]
  table5 <- coltable[201:250,]
  table6 <- coltable[251:300,]
  table7 <- coltable[301:350,]
  table8 <- coltable[351:400,]
  table9 <- coltable[401:450,]
  table10 <- coltable[451:500,]
  table11 <- coltable[501:550,]
  table12 <- coltable[551:nrow(coltable),]
  
  table1Star <- stargazer(table1,summary=FALSE,colnames=TRUE,rownames=FALSE,digit.separator="")
  table2Star <- stargazer(table2,summary=FALSE,colnames=TRUE,rownames=FALSE,digit.separator="")
  table3Star <- stargazer(table3,summary=FALSE,colnames=TRUE,rownames=FALSE,digit.separator="")
  table4Star <- stargazer(table4,summary=FALSE,colnames=TRUE,rownames=FALSE,digit.separator="")
  table5Star <- stargazer(table5,summary=FALSE,colnames=TRUE,rownames=FALSE,digit.separator="")
  table6Star <- stargazer(table6,summary=FALSE,colnames=TRUE,rownames=FALSE,digit.separator="")
  table7Star <- stargazer(table7,summary=FALSE,colnames=TRUE,rownames=FALSE,digit.separator="")
  table8Star <- stargazer(table8,summary=FALSE,colnames=TRUE,rownames=FALSE,digit.separator="")
  table9Star <- stargazer(table9,summary=FALSE,colnames=TRUE,rownames=FALSE,digit.separator="")
  table10Star <- stargazer(table10,summary=FALSE,colnames=TRUE,rownames=FALSE,digit.separator="")
  table11Star <- stargazer(table11,summary=FALSE,colnames=TRUE,rownames=FALSE,digit.separator="")
  table12Star <- stargazer(table12,summary=FALSE,colnames=TRUE,rownames=FALSE,digit.separator="")
  
  tableHeader <- c("\\documentclass[9pt]{extarticle}","\\usepackage{lscape}","\\usepackage{caption}","\\usepackage[ansinew]{inputenc}","\\usepackage[ngerman]{babel}","\\begin{document}","\\pagestyle{empty}","\\begin{landscape}","\\begin{table}[!htbp] \\centering","\\caption*{\\textbf{Liste der Kan\"ale aus den 316 Rohdateien}}","\\footnotesize","\\begin{tabular}{ll}","\\\\[-5ex]\\hline\\hline\\\\[-1.8ex]")
  
  table1Tex <- c(tableHeader,table1Star[10:length(table1Star)],"\\end{landscape}","\\end{document}")
  table2Tex <- c(tableHeader,table2Star[10:length(table2Star)],"\\end{landscape}","\\end{document}")
  table3Tex <- c(tableHeader,table3Star[10:length(table3Star)],"\\end{landscape}","\\end{document}")
  table4Tex <- c(tableHeader,table4Star[10:length(table4Star)],"\\end{landscape}","\\end{document}")
  table5Tex <- c(tableHeader,table5Star[10:length(table5Star)],"\\end{landscape}","\\end{document}")
  table6Tex <- c(tableHeader,table6Star[10:length(table6Star)],"\\end{landscape}","\\end{document}")
  table7Tex <- c(tableHeader,table7Star[10:length(table7Star)],"\\end{landscape}","\\end{document}")
  table8Tex <- c(tableHeader,table8Star[10:length(table8Star)],"\\end{landscape}","\\end{document}")
  table9Tex <- c(tableHeader,table9Star[10:length(table9Star)],"\\end{landscape}","\\end{document}")
  table10Tex <- c(tableHeader,table10Star[10:length(table10Star)],"\\end{landscape}","\\end{document}")
  table11Tex <- c(tableHeader,table11Star[10:length(table11Star)],"\\end{landscape}","\\end{document}")
  table12Tex <- c(tableHeader,table12Star[10:length(table12Star)],"\\end{landscape}","\\end{document}")
  
  write.table(table1Tex,paste0(dir, "table1.tex"),row.names=FALSE,col.names=FALSE,quote=FALSE)
  write.table(table2Tex,paste0(dir, "table2.tex"),row.names=FALSE,col.names=FALSE,quote=FALSE)
  write.table(table3Tex,paste0(dir, "table3.tex"),row.names=FALSE,col.names=FALSE,quote=FALSE)
  write.table(table4Tex,paste0(dir, "table4.tex"),row.names=FALSE,col.names=FALSE,quote=FALSE)
  write.table(table5Tex,paste0(dir, "table5.tex"),row.names=FALSE,col.names=FALSE,quote=FALSE)
  write.table(table6Tex,paste0(dir, "table6.tex"),row.names=FALSE,col.names=FALSE,quote=FALSE)
  write.table(table7Tex,paste0(dir, "table7.tex"),row.names=FALSE,col.names=FALSE,quote=FALSE)
  write.table(table8Tex,paste0(dir, "table8.tex"),row.names=FALSE,col.names=FALSE,quote=FALSE)
  write.table(table9Tex,paste0(dir, "table9.tex"),row.names=FALSE,col.names=FALSE,quote=FALSE)
  write.table(table10Tex,paste0(dir, "table10.tex"),row.names=FALSE,col.names=FALSE,quote=FALSE)
  write.table(table11Tex,paste0(dir, "table11.tex"),row.names=FALSE,col.names=FALSE,quote=FALSE)
  write.table(table12Tex,paste0(dir, "table12.tex"),row.names=FALSE,col.names=FALSE,quote=FALSE)
  
  shell(paste0(pdflatex," ",dir,"table1.tex"))
  shell(paste0(pdflatex," ",dir,"table2.tex"))
  shell(paste0(pdflatex," ",dir,"table3.tex"))
  shell(paste0(pdflatex," ",dir,"table4.tex"))
  shell(paste0(pdflatex," ",dir,"table5.tex"))
  shell(paste0(pdflatex," ",dir,"table6.tex"))
  shell(paste0(pdflatex," ",dir,"table7.tex"))
  shell(paste0(pdflatex," ",dir,"table8.tex"))
  shell(paste0(pdflatex," ",dir,"table9.tex"))
  shell(paste0(pdflatex," ",dir,"table10.tex"))
  shell(paste0(pdflatex," ",dir,"table11.tex"))
  shell(paste0(pdflatex," ",dir,"table12.tex"))
  
  shell(paste0("C:/EngineDL/Softwares/IM/convert -density 300 -rotate 90 -crop 900x1870+1200+550 -alpha remove -background #E5F2E5 ",workDir,"\\table1.pdf ",dir,"\\table1.png")) 
  shell(paste0("C:/EngineDL/Softwares/IM/convert -density 300 -rotate 90 -crop 900x1870+1200+550 -alpha remove -background #E5F2E5 ",workDir,"\\table2.pdf ",dir,"\\table2.png"))
  shell(paste0("C:/EngineDL/Softwares/IM/convert -density 300 -rotate 90 -crop 900x1870+1200+550 -alpha remove -background #E5F2E5 ",workDir,"\\table3.pdf ",dir,"\\table3.png"))
  shell(paste0("C:/EngineDL/Softwares/IM/convert -density 300 -rotate 90 -crop 900x1870+1200+550 -alpha remove -background #E5F2E5 ",workDir,"\\table4.pdf ",dir,"\\table4.png"))
  shell(paste0("C:/EngineDL/Softwares/IM/convert -density 300 -rotate 90 -crop 900x1870+1200+550 -alpha remove -background #E5F2E5 ",workDir,"\\table5.pdf ",dir,"\\table5.png"))
  shell(paste0("C:/EngineDL/Softwares/IM/convert -density 300 -rotate 90 -crop 900x1870+1200+550 -alpha remove -background #E5F2E5 ",workDir,"\\table6.pdf ",dir,"\\table6.png"))
  shell(paste0("C:/EngineDL/Softwares/IM/convert -density 300 -rotate 90 -crop 900x1870+1200+550 -alpha remove -background #E5F2E5 ",workDir,"\\table7.pdf ",dir,"\\table7.png"))
  shell(paste0("C:/EngineDL/Softwares/IM/convert -density 300 -rotate 90 -crop 900x1870+1200+550 -alpha remove -background #E5F2E5 ",workDir,"\\table8.pdf ",dir,"\\table8.png"))
  shell(paste0("C:/EngineDL/Softwares/IM/convert -density 300 -rotate 90 -crop 900x1870+1200+550 -alpha remove -background #E5F2E5 ",workDir,"\\table9.pdf ",dir,"\\table9.png"))
  shell(paste0("C:/EngineDL/Softwares/IM/convert -density 300 -rotate 90 -crop 900x1870+1200+550 -alpha remove -background #E5F2E5 ",workDir,"\\table10.pdf ",dir,"\\table10.png"))
  shell(paste0("C:/EngineDL/Softwares/IM/convert -density 300 -rotate 90 -crop 900x1870+1200+550 -alpha remove -background #E5F2E5 ",workDir,"\\table11.pdf ",dir,"\\table11.png"))
  shell(paste0("C:/EngineDL/Softwares/IM/convert -density 300 -rotate 90 -crop 900x1870+1200+550 -alpha remove -background #E5F2E5 ",workDir,"\\table12.pdf ",dir,"\\table12.png"))
}

getMissogram <- function(dir,save=TRUE){
  require(dplyr)
  require(accelerometry)
  #library(ggplot2,lib.loc="C:/Users/SHINAMI/Documents/R/win-library/3.3")
  library(ggplot2,lib.loc="C:/EngineDL/SETUP_Files/R-3.3.3/library2/")
  #library(ggplot2,lib.loc="C:/EngineDL/")
  ymdata <- readRDS(paste0(dir,"Daten/ymdata.rds"))
  y1 <- na.omit(ymdata)
  y1[,c("yMin","yMax","yDiff")] <- round(y1[,c("yMin","yMax","yDiff")])
  s2 <- data.frame(san=integer())
  for(idx in 1:nrow(y1)){ 
    s1 <- data.frame(san = seq(y1[idx,1],y1[idx,2]))
    s2 <- rbind(s2,s1)
  }
  s3 <- data.frame(san = unique(s2$san))
  s4 <- data.frame(san=seq(from = min(y1[,1]), to = max(y1[,2]), by =1))
  s5 <- anti_join(s4, s3)
  s3$mark <- "1YES"
  if(nrow(s5)!=0){s5$mark <- "2NO"}  ##changes doen 7/7/2017 for S5 being null
  s6 <- merge(s3,s5,all = T)
  modeSeq <- rle2(s6$mark,return.list=TRUE)
  #data <- data.frame(cbind(modeSeq$lengths,modeSeq$values),stringsAsFactors = FALSE)
  # data <- data.frame(X1 = as.numeric(),X2 = as.character(),id = as.numeric(),stri)
  data <- data.frame(cbind(modeSeq$lengths,modeSeq$values),stringsAsFactors = FALSE)
  data$X1 <- as.numeric(data$X1)
  data$id <- 1
  #data$X2 <- factor(data$X2, levels = data$X2)
  # data2 <- data
  # data2$id <- 2
  # datam <- as.data.frame(rbind(data,data2))
  # datam[c(22,24,26,28,67,69,71,73),2] <- "YES"
  # print(datam)
  if(save){
    png(paste0(dir,"Grafiken/missogram",".png"),width=1000,height=600)
  }
  p <- ggplot(data, aes(x = id, y = X1, fill = X2 ,width=0.5)) + 
    geom_bar(stat = "identity",position = "stack")+ coord_flip() + 
    scale_fill_manual(values=c("#198c19","#ff4c4c"),name="Datenverfügbarkeit",label=c("Daten erfasst","fehlende Daten")) + 
    ggtitle("Fahrzeugkilometerstand [km]") +
    theme(legend.position = c(.9,.7),legend.title= element_text(size = 16),legend.text=element_text(size = 13),axis.title.y=element_blank(),axis.title.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks.y=element_blank(),plot.title = element_text(size = 20, face = "bold")) +  ylim(-300,max(y1[,2])+15000) + 
    geom_hline(yintercept = c(0,max(y1[,2])-min(y1[,1]))) + 
    annotate("text",x=1.03,y=nrow(s4)+500,label=paste0(nrow(s3)," km"),colour="#198c19",fontface=2,size = 6,hjust=0)+
    annotate("text",x=0.97,y=nrow(s4)+500,label=paste0(nrow(s5)," km"),colour="#ff4c4c",fontface=2,size = 6,hjust=0)+
    annotate("text",x=0.7,y=100,label=paste0("Dauerlaufbegin: \n",sprintf("%.0f",min(s2$san))," km"),colour="black",hjust=0)+
    annotate("text",x=0.7,y=nrow(s4)+400,label=paste0("aktueller Stand: \n",sprintf("%.0f",max(y1[,2]))," km"),colour="black",hjust=0)+
    # annotate("text",x=0.7,y=40000+200,label=paste0("Dauerlauf-Soll-Ende: Stand \n",sprintf("%.0f",40092)," km"),colour="black",hjust=0)+
    # annotate("text",x=2.5,y=100,label=paste0("In Messdaten enthaltene (=grun) und nicht enthaltene (=rot) Fahrzeugkilometerstende"),colour="black",hjust=0)+
    annotate("text",x=1.3,y=100,label=paste0("Fahrzeugkilometerstände mit gültigem (=grün) und ungültigem/fehlendem(=rot) Motordrehzahlsignal gemäß Messdaten"),colour="black",hjust=0)+
    theme(panel.grid.minor = element_line(colour="white", size=0.5)) +
    # scale_y_continuous(minor_breaks = NULL,limits = c(0,45000)) +
    # scale_y_continuous(minor_breaks = NULL,limits = c(0,(nrow(s3)+8000))) +
    scale_y_continuous(minor_breaks = NULL,limits = c(0,nrow(s3)+(nrow(s3)/6))) +
    scale_x_continuous(limits=c(0.5,1.5))
  # scale_x_continuous()
  print(p)
  # ggsave(filename = paste0(dir,"Grafiken/missogram",".png"),plot = p,width=6,height=4)
  if(save){
    dev.off()
  }
  #detach("package:ggplot2", unload=TRUE)
}

getMissogramCombo <- function(dir,save=TRUE){
  
  library(dplyr)
  library(accelerometry)
  library(gridExtra)
  library(grid)
  library(reshape2)
  options(stringsAsFactors = FALSE)
  #library(ggplot2,lib.loc="C:/Users/SHINAMI/Documents/R/win-library/3.3")
  library(ggplot2,lib.loc="C:/EngineDL/SETUP_Files/R-3.3.3/library2/")
  #library(ggplot2,lib.loc="C:/EngineDL/")
  ymdata <- readRDS(paste0(dir,"Daten/ymdata.rds"))
  y1 <- na.omit(ymdata)
  y1[,c("yMin","yMax","yDiff")] <- round(y1[,c("yMin","yMax","yDiff")])
  s2 <- data.frame(san=integer())
  for(idx in 1:nrow(y1)){ 
    s1 <- data.frame(san = seq(y1[idx,1],y1[idx,2]))
    s2 <- rbind(s2,s1)
  }
  y1 <- data.frame(y1)
  t <- readRDS(paste0(dir,"Daten/trackName.rds"))
  t <- data.frame(t)
  y1 <- data.frame(cbind(y1,t))
  
  s2$track <- NA
  
  for (idx in 1:nrow(s2)){
    a <- as.numeric(s2[idx,1])
    d <-  head(subset(y1, as.numeric(y1$yMax)>=a),1)
    s2[idx,2] <- d[1,4]
  }
  
  modeSeq <- rle2(s2$track,return.list=TRUE)
  data <- data.frame(cbind(modeSeq$lengths,modeSeq$values),stringsAsFactors = FALSE)
  data$X1 <- as.numeric(data$X1)
  data$id <- 10
  # data$X2 <- factor(data$X2, levels=rev(levels(data$X2)))
  
  Odo=sum(data$X1)
  Interval=5000
  Calculated_Interval=0
  Actual_Interval=0
  Multiplying_factor=0
  
  if(Odo<50000){
    Calculated_Interval=5000
  }else{
    Actual_Interval=Odo/10
    Multiplying_factor=Actual_Interval/5000
    Calculated_Interval=round(Multiplying_factor)*5000
  }
  
  
  options(scipen=999)
  p1 <- ggplot(data, aes(x = id, y = X1, fill = X2, width=0.5)) + 
    geom_bar(stat = "identity") + coord_flip() + 
    theme(legend.position="bottom",legend.direction="horizontal",legend.text=element_text(size=12), 
          plot.margin=unit(c(0.1,1,0.25,1), "cm"), 
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())+
    # xlab("Track") + 
    ylab("Fahrzeugkilometerstand [km]") + 
    guides(fill=guide_legend(nrow=1,byrow=TRUE)) +
    scale_x_continuous(breaks=10)+
    # scale_y_continuous(breaks=c(seq(0,sum(data$X1),20000),sum(data$X1)),expand = c(0, 0)) +
    scale_y_continuous(breaks=c(seq(0,sum(data$X1),Calculated_Interval)),expand = c(0, 0)) +
    labs(fill="")
  
  Median <- readRDS(paste0(dir,"Daten/medianXAirTemp.rds"))
  Maximum <- readRDS(paste0(dir,"Daten/maxXAirTemp.rds"))
  Minimum <- readRDS(paste0(dir,"Daten/minXAirTemp.rds"))
  
  y1 <- data.frame(cbind(y1,Median,Maximum,Minimum))
  y1$id <- 1
  
  y2 <- y1[,c("yMin","Median","Maximum","Minimum")]
  y2m <- melt(y2, id.var = c("yMin"))
  names(y2m)[names(y2m) == 'variable'] <- 'Außenlufttemperatur'
  
  p2 <- ggplot(data=y2m,aes(x=yMin, y=value, colour=Außenlufttemperatur, linetype =Außenlufttemperatur, size=Außenlufttemperatur)) +
    geom_line() + 
    theme(legend.position=c(0.95,0.9),legend.title=element_text("Außenlufttemperatur", size = 14,face = "bold"),legend.text=element_text(size=14),axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
          plot.margin=unit(c(0.1,1,0.2,0), "cm"),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          plot.title = element_text(size = 15, colour = "black",face = "bold"))+
    scale_linetype_manual(values=c("solid", "solid","solid"))+
    scale_color_manual(values=c("black", "firebrick1","cornflowerblue"))+
    scale_size_manual(values=c(1,.1,.1)) +
    ylab("Außenlufttemperatur") +
    ggtitle(paste("MIN-Außentemperatur", sprintf("%.1f",min(Minimum))," °C / MAX-Außentemperatur", sprintf("%.1f",max(Maximum))," °C")) +
    scale_x_continuous(breaks=c(seq(0,sum(data$X1),20000),sum(data$X1)),expand = c(0, 0))
  
  if(save){
    png(paste0(dir,"Grafiken/missogramCombo",".png"),width=1000,height=600)
  }
  grid.arrange(p2, p1, ncol=1, nrow =2, heights=c(2,1))
  if (save){
    dev.off()
  }
  #detach("package:ggplot2", unload=TRUE)
}

plotSuperFun <- function(idx,dir,mainConfig,rsgIsg="RSG",save=TRUE){
  mainConfig<-as.data.frame(mainConfig)
  idleModeBreaks <- c(1e-2,850,Inf)  #c(100,850) U/min
  pushModeBreaks2 <- c(1e-2,850,Inf)  #c(700,Inf) U/min
  pedHighBreaks1 <- c(80,Inf)  # %
  pedHighBreaks2 <- c(700,Inf)  # U/min
  startThresh <- 200  # U/min
  startMinTime <- 3  # 
  standStillBreaks1 <- c(0,Inf)  # km/h Changed from 20 to Inf Based on clarification given by PM3 on 16/02/2017 (Limit on Speed removed)
  highLoadStartTime <- 2 #
  highLoadStartThresh <- 80 # %
  dpfRegBreaks1 <- c(0,1)  # 
  dpfRegBreaks2 <- c(700,Inf)  # U/min
  #print(mainConfig[idx,6])
  #print(mainConfig[idx,2]) 
  
  x <- readRDS(file=paste0(dir,"Daten/",mainConfig[idx,6],".rds"))
  mile <- readRDS(file=paste0(dir,"Daten/",mainConfig[idx,6],"Mile.rds"))
  
  
  # if(!is.na(mainConfig[idx,8])){
  #   y <- readRDS(file=paste0(dir,"Daten/",mainConfig[idx,8],".rds"))
  #   mile2 <- readRDS(file=paste0(dir,"Daten/",mainConfig[idx,8],"Mile.rds"))
  #   if(!is.null(mile2) && is.null(mile)){
  #     mile <- mile2
  #   }
  # }
  if(mainConfig[idx,5]=="barPerTrack"){
    track <- readRDS(file=paste0(dir,"Daten/trackName.rds"))
    useTrack <- readRDS(file=paste0(dir,"Daten/trackNameUse.rds"))
    targTrack <- readRDS(file=paste0(dir,"Daten/targTrackName.rds"))
    distance <- readRDS(file=paste0(dir,"Daten/distanceOdo.rds"))
  }
  plotId <- as.numeric(mainConfig[idx,1])
  plotPath <- paste0(dir,"Grafiken/")
  #print(plotPath)
  plotName <- mainConfig[idx,4]
  if(any(!is.na(mainConfig[idx,33:35]))){
    breaksX <- c(-Inf,seq(mainConfig[idx,33],mainConfig[idx,34],mainConfig[idx,35]),Inf)
  }else{
    breaksX <- NULL
  }
  if(any(!is.na(mainConfig[idx,36:38]))){
    breaksY <- c(-Inf,seq(mainConfig[idx,36],mainConfig[idx,37],mainConfig[idx,38]),Inf)
  }else{
    breaksY <- NULL
  }
  
  if(plotId==201){
    #   #2.1 Motorbetriebsart
    plotBarsPerTrack6 <-  function(plotPath,plotName,x1,x2,x3,x4,x5,x6,nameX1,nameX2,nameX3,nameX4,nameX5,nameX6,
                                   colX1="#66C2A5",colX2="#FC8D62",colX3="#8DA0CB",colX4="#E78AC3",colX5="#A6D854",colX6="#FFD92F",
                                   unit=NULL,yLab="",title="",subtitle="",
                                   cexMain=1.4,cexLab=1.3,cexAx=0.9,cexSub=0.78,save=TRUE){
      library("plotrix")
      library("epade")
      library("grid")
      library("TeachingDemos")
      
      AusPerTrack <- sum(x1);
      Phase3sPerTrack <- sum(x2);
      SegeinPerTrack <- sum(x3);
      LeerlaufPerTrack <- sum(x4);
      SchubPerTrack <- sum(x5);
      LastPerTrack <- sum(x6);
      
      categ <- factor(c(rep(nameX1,length(AusPerTrack)),
                        rep(nameX2,length(Phase3sPerTrack)),
                        rep(nameX3,length(SegeinPerTrack)),
                        rep(nameX4,length(LeerlaufPerTrack)),
                        rep(nameX5,length(SchubPerTrack)),
                        rep(nameX6,length(LastPerTrack)))
                      ,c(nameX1,nameX2,nameX3,nameX4,nameX5,nameX6))
      
      # lbltxt <- paste0(sprintf("%.0f",AusPerTrack),sprintf("%.0f",Phase3sPerTrack),sprintf("%.0f",SegeinPerTrack),sprintf("%.0f",LeerlaufPerTrack),sprintf("%.0f",SchubPerTrack),sprintf("%.0f",LastPerTrack))
      txtpos <- (AusPerTrack+Phase3sPerTrack+SegeinPerTrack+LeerlaufPerTrack+SchubPerTrack+LastPerTrack) +0.06*max(AusPerTrack+Phase3sPerTrack+SegeinPerTrack,LeerlaufPerTrack+SchubPerTrack+LastPerTrack)
      track <-  "Gesamt"
      numTracks <- 1
      
      if(save){
        png(paste0(plotPath,plotName,".png"),width=1000,height=600)
      }
      par(cex.main=cexMain,cex.lab=cexLab,cex.axis=cexAx)
      
      bar.plot.wtd(x=categ,y=rep(track,6),w=c(AusPerTrack,Phase3sPerTrack,SegeinPerTrack,LeerlaufPerTrack,SchubPerTrack,LastPerTrack), ####removal of ylim(0,2750) on 03-01-2017
                   wall=4,b=0.6,form="c",b2=1,main=title,xlab="Streckenart",ylab=yLab,
                   col=c(colX1,colX2,colX3,colX4,colX5,colX6),beside=FALSE)
      
      shadowtext((1:numTracks)-0.15,txtpos, sprintf("%.0f",AusPerTrack),col="#66C2A5",font = 1,cex=1.1,r=0.03)
      shadowtext((1:numTracks)-0.10,txtpos,sprintf("%.0f",Phase3sPerTrack),col="#FC8D62",font = 1,cex=1.1,r=0.03)
      shadowtext((1:numTracks)-0.05,txtpos,sprintf("%.0f",SegeinPerTrack),col="#8DA0CB",font = 1,cex=1.1,r=0.03)
      shadowtext((1:numTracks)+0.0,txtpos,sprintf("%.0f",LeerlaufPerTrack),col="#E78AC3",font = 1,cex=1.1,r=0.03)
      shadowtext((1:numTracks)+0.05,txtpos,sprintf("%.0f",SchubPerTrack),col="#A6D854",font = 1,cex=1.1,r=0.03)
      shadowtext((1:numTracks)+0.1,txtpos,sprintf("%.0f",LastPerTrack),col="#FFD92F",font = 1,cex=1.1,r=0.03)
      
      text((1:numTracks)+0.15,txtpos,paste0("[h]"),col="blue",font = 1,cex=1.2)
      
      grid.text(subtitle,x=unit(0.00,"npc"),y=unit(0.005,"npc"),just=c("left","bottom"),gp=gpar(cex=cexSub))
      if(save){
        dev.off()
      }
    }
    
    
    plotBarsPerTrack6(plotPath=plotPath,
                      plotName=plotName,
                      x1=x[,1],
                      x2=x[,2],
                      x3=x[,3],
                      x4=x[,4],
                      x5=x[,5],
                      x6=x[,6],
                      nameX1="Aus",
                      nameX2="3S- Phase",
                      nameX3="SegeIn",
                      nameX4="Leerlauf",
                      nameX5="Schub",
                      nameX6="Last",
                      yLab="Zeit [h]",
                      title=paste0("Zeiten pro Motorbetriebsart: insgesamt ",sprintf("%.0f",sum(x)),"h bezogen auf ",sprintf("%.0f",sum(mile)),"km"),
                      subtitle="Last: [EngSpd \u2265 850 U/min, FuelCons > 0 µl/250ms], Schub: [EngSpd \u2265 850 U/min, FuelCons = 0 µl/250ms], \nLeerlauf: [0 U/min < EngSpd < 850 U/min], Segein: [EngSpd = 0 U/min, VehSpd > 0 km/h], 3S-Phase: [EngSpd = 0 U/min, VehSpd = 0 km/h, IgnMode = 1], Aus: [EngSpd = 0 U/min, VehSpd = 0 km/h, IgnMode = 0]",
                      unit="h",
                      save=save)
  }else if(plotId==202){
    
    # # 2.2.1 Motorbetriebsart
    
    plotBarsPerTrack5 <-  function(plotPath,plotName,x1,x2,x3,x4,x5,x6,nameX1,nameX2,nameX3,nameX4,nameX5,nameX6,
                                   colX1="#66C2A5",colX2="#FC8D62",colX3="#8DA0CB",colX4="#E78AC3",colX5="#A6D854",colX6="#FFD92F",
                                   track,useTrack=NULL,unit=NULL,yLab="",title="",subtitle="",
                                   cexMain=1.4,cexLab=1.3,cexAx=0.9,cexSub=0.78,save=TRUE){
      library("plotrix")
      library("epade")
      library("grid")
      library("TeachingDemos")
      
      preCalc <- preCalcBarsPerTrack(x1,track,useTrack,NULL,unit)
      preCalc2 <- preCalcBarsPerTrack(x2,track,useTrack,NULL,unit)
      preCalc3 <- preCalcBarsPerTrack(x3,track,useTrack,NULL,unit)
      preCalc4 <- preCalcBarsPerTrack(x4,track,useTrack,NULL,unit)
      preCalc5 <- preCalcBarsPerTrack(x5,track,useTrack,NULL,unit)
      preCalc6 <- preCalcBarsPerTrack(x6,track,useTrack,NULL,unit)
      
      preCalc$AusPerTrack <- preCalc$xPerTrack;
      preCalc$x2 <- preCalc2$x; preCalc$Phase3sPerTrack <- preCalc2$xPerTrack;
      preCalc$x3 <- preCalc3$x; preCalc$SegeinPerTrack <- preCalc3$xPerTrack;
      preCalc$x4 <- preCalc4$x; preCalc$LeerlaufPerTrack <- preCalc4$xPerTrack;
      preCalc$x5 <- preCalc5$x; preCalc$SchubPerTrack <- preCalc5$xPerTrack;
      preCalc$x6 <- preCalc6$x; preCalc$LastPerTrack <- preCalc6$xPerTrack;
      
      categ <- factor(c(rep(nameX1,length(preCalc$x)),
                        rep(nameX2,length(preCalc$x2)),
                        rep(nameX3,length(preCalc$x3)),
                        rep(nameX4,length(preCalc$x4)),
                        rep(nameX5,length(preCalc$x5)),
                        rep(nameX6,length(preCalc$x6)))
                      ,c(nameX1,nameX2,nameX3,nameX4,nameX5,nameX6))
      
      # print(length(preCalc$x))
      print(table(categ))
      txtpos <- (preCalc$AusPerTrack+preCalc$Phase3sPerTrack+preCalc$SegeinPerTrack+preCalc$LeerlaufPerTrack+preCalc$SchubPerTrack+preCalc$LastPerTrack)+0.07*max(preCalc$AusPerTrack+preCalc$Phase3sPerTrack+preCalc$SegeinPerTrack,preCalc$LeerlaufPerTrack+preCalc$SchubPerTrack+preCalc$LastPerTrack)
      
      if(save){
        png(paste0(plotPath,plotName,".png"),width=1000,height=600)
      }
      par(cex.main=cexMain,cex.lab=cexLab,cex.axis=cexAx)
      
      bar.plot.wtd(x=categ,y=rep(preCalc$track,6),w=c(preCalc$x,preCalc$x2,preCalc$x3,preCalc$x4,preCalc$x5,preCalc$x6),  ### removal of ylim(0,625) on 03-01-2017
                   wall=4,b=0.7,form="c",b2=1,main=title,xlab="Streckenart",ylab=yLab,
                   col=c(colX1,colX2,colX3,colX4,colX5,colX6),beside=FALSE)
      
      shadowtext((1:preCalc$numTracks)-0.4,txtpos,sprintf("%.0f",preCalc$AusPerTrack),col="#66C2A5",font=1,cex=1.1,r=0.03)
      shadowtext((1:preCalc$numTracks)-0.25,txtpos,sprintf("%.0f",preCalc$Phase3sPerTrack),col="#FC8D62",font=1,cex=1.1,r=0.03)
      shadowtext((1:preCalc$numTracks)-0.12,txtpos,sprintf("%.0f",preCalc$SegeinPerTrack),col="#8DA0CB",font=1,cex=1.1,r=0.03)
      shadowtext((1:preCalc$numTracks)+0.0,txtpos,sprintf("%.0f",preCalc$LeerlaufPerTrack),col="#E78AC3",font=1,cex=1.1,r=0.03)
      shadowtext((1:preCalc$numTracks)+0.2,txtpos,sprintf("%.0f",preCalc$SchubPerTrack),col="#A6D854",font=1,cex=1.1,r=0.03) 
      shadowtext((1:preCalc$numTracks)+0.45,txtpos,sprintf("%.0f",preCalc$LastPerTrack),col="#FFD92F",font=1,cex=1.1,r=0.03) 
      
      grid.text(subtitle,x=unit(0.00,"npc"),y=unit(0.005,"npc"),just=c("left","bottom"),gp=gpar(cex=cexSub)) 
      if(save){
        dev.off()
      }
    }
    
    plotBarsPerTrack5(plotPath=plotPath,
                      plotName=plotName,
                      x1=x[,1],
                      x2=x[,2],
                      x3=x[,3],
                      x4=x[,4],
                      x5=x[,5],
                      x6=x[,6],
                      nameX1="Aus",
                      nameX2="3S-Phase",
                      nameX3="SegeIn",
                      nameX4="Leerlauf",
                      nameX5="Schub",
                      nameX6="Last",
                      track=track,
                      useTrack=useTrack,
                      yLab="Zeit [h]",
                      title=paste0("Zeiten pro Motorbetriebsart und Streckenprofil: insgesamt ",sprintf("%.0f",sum(x)),"h bezogen auf ",sprintf("%.0f",sum(mile)),"km"),
                      subtitle="Last: [EngSpd \u2265 850 U/min, FuelCons > 0 µl/250ms], Schub: [EngSpd \u2265 850 U/min, FuelCons = 0 µl/250ms], \nLeerlauf: [0 U/min < EngSpd < 850 U/min], Segein: [EngSpd = 0 U/min, VehSpd > 0 km/h], 3S-Phase: [EngSpd = 0 U/min, VehSpd = 0 km/h, IgnMode = 1], Aus: [EngSpd = 0 U/min, VehSpd = 0 km/h, IgnMode = 0]",
                      unit="h",
                      save=save)
  }else if(plotId==203){
    #2.2.2 Laufleistung
    y<-readRDS(file=paste0(dir,"Daten/targDist.rds"))
    plotBarsPerTrack4(plotPath=plotPath,
                      plotName=plotName,
                      x=x,
                      y=y,
                      trackX=track,
                      trackY=targTrack,
                      useTrack=useTrack,
                      nameX="Ist",
                      nameY="Soll",
                      yLab="Fahrstrecke [km]",
                      title=paste0("Fahrstrecke* pro Streckenprofil: insgesamt ",sprintf("%.0f",sum(x))," km (Ist) / ",sprintf("%.0f",sum(y))," km (Soll)"),
                      save=save)
    # }else if(plotId==264){
    #   #2.2.2 Laufleistung
    #   plotBarsPerTrack4(plotPath=plotPath,
    #                     plotName=plotName,
    #                     x=x,
    #                     y=y,
    #                     trackX=track,
    #                     trackY=targTrack,
    #                     useTrack=useTrack,
    #                     nameX="Ist",
    #                     nameY="Soll",
    #                     yLab="Fahrstrecke [km]",
    #                     title=paste0("Fahrstrecke* pro Streckenprofil: insgesamt ",sprintf("%.0f",sum(x))," km (Ist) / ",sprintf("%.0f",sum(y))," km (Soll)"),
    #                     subtitle="* Ist-Fahrstrecke: Summe der Strecken aus vorliegenden Messdateien; Soll-Fahrstrecke: aus ''PrÃ¼fvorschrift Fahrzeugerprobung Dauerlauf'' (Abteilung RD/OTD, 01.06.2012)",#(Abteilung RD/OTD, 06.02.2015)"
    #                     save=save)
  }else if(plotId==204){
    #2.2.3 Kraftstoffverbrauch
    xPerTrack <- sapply(sort(unique(track)),function(a){sum(x[track==a])})
    milePerTrack <- sapply(sort(unique(track)),function(a){sum(mile[track==a])})
    plotBarsPerTrack1(plotPath=plotPath,
                      plotName=plotName,
                      x=as.numeric(xPerTrack/milePerTrack)*100,
                      unit="l/100km",
                      track=names(xPerTrack),
                      useTrack=useTrack,
                      yLab="Kraftstoffverbrauch [l/100km]",
                      title=paste0("Kraftstoffverbrauch pro Streckenprofil: insgesamt ",sprintf("%.0f",sum(x))," l bezogen auf ",sprintf("%.0f",sum(mile))," km (",sprintf("%.1f",sum(x)*100/sum(mile))," l/100km)"),
                      distance=100/milePerTrack,
                      unitPerDist="l",
                      percTxt=FALSE,
                      numFloat=1,
                      save=save)
  }else if(plotId==205){
    #2.3 Drehzahl-Drehmoment-Kennfeld
    # breaksYData <- subset(pwrTrqConfig[,8:10],pwrTrqConfig[,2] == as.character(engSeries[1,1]))
    breaksYData <- subset(pwrTrqConfig[,8:10],pwrTrqConfig[,2] == as.character(engSeries))
    breaksY <- c(-Inf,seq(breaksYData[1,1],breaksYData[1,2],breaksYData[1,3]),Inf)
    breaksX2 <- breaksX[breaksX>=0]
    breaksX2[breaksX2==0] <- 1
    plotHeatmap(plotPath=plotPath,
                plotName=plotName,
                x=x/3600,
                breaksX=breaksX2,
                breaksY=breaksY,
                xLab="Motordrehzahl [U/min]",
                yLab="Motordrehmoment [Nm]",
                title="Drehzahl-Drehmoment-Kennfeld des Verbrennungsmotors",
                subtitle=paste("(Relative) Betriebszeit* pro Klasse: insgesamt",sprintf("%.0f",sum(x[,as.numeric(colnames(x))>0],na.rm=TRUE)/3600),"h bezogen auf",sprintf("%.0f",sum(mile)),"km"),
                subsubtitle="* Betrieb: Motordrehzahl > 0 U/min",
                save=save)
  }else if(plotId==206){
    #2.4 Fahrzeuggeschwindigkeit-Motorleistung-Kennfeld
    breaksX2 <- breaksX[breaksX>=0]
    if(any(breaksX2==0)){
      breaksX2 <- sort(unique(c(0.01,breaksX2)))
      breaksX2 <- breaksX2[breaksX2>0]
    }
    # breaksYData <- subset(pwrTrqConfig[,4:6],pwrTrqConfig[,2] == as.character(engSeries[1,1]))
    breaksYData <- subset(pwrTrqConfig[,4:6],pwrTrqConfig[,2] == as.character(engSeries))
    breaksY <- c(-Inf,seq(breaksYData[1,1],breaksYData[1,2],breaksYData[1,3]),Inf)
    plotHeatmap(plotPath=plotPath,
                plotName=plotName,
                x=x/3600,
                breaksX=breaksX2,
                breaksY=breaksY,
                xLab="Fahrzeuggeschwindigkeit [km/h]",
                yLab="Motorleistung [kW]",
                title="Fahrzeuggeschwindigkeit-Motorleistung-Kennfeld",
                subtitle=paste("(Relative) Betriebszeit* pro Klasse: insgesamt",sprintf("%.0f",sum(x,na.rm=TRUE)/3600),"h bezogen auf",sprintf("%.0f",sum(mile)),"km"),
                subsubtitle="* Betrieb: Motordrehzahl > 0 U/min",
                save=save)
  }else if(plotId==2061){
    # 2.14.1 2.4 Fahrzeuggeschwindigkeit / 2.4.1 Verteilung der Fahrzeuggeschwindigkeit
    breaksX2 <- breaksX[breaksX>0]
    # breaksX2 <- breaksX2[breaksX2<=100]
    breaksX2 <- c(0.1,breaksX2)
    plotBars1(plotPath=plotPath,
              plotName=plotName,
              x=x/3600,
              breaks=breaksX2,
              xLab="Fahrzeuggeschwindigkeit [km/h]",
              yLab="Zeit [h]",
              title=paste("Betriebszeit des Verbrennungsmotors pro Fahrzeuggeschwindigkeitsklasse: insgesamt ",sprintf("%.0f",sum(x)/3600),"h bezogen auf",sprintf("%.0f",sum(mile)),"km"),
              subtitle="* Betrieb: Motordrehzahl > 0 U/min",
              splitClassNames=TRUE,
              save=save)
  }else if(plotId==207){
    #2.5 Leerlaufzeit
    plotBarsPerTrack2(plotPath=plotPath,
                      plotName=plotName,
                      x=x[,1],
                      nx=x[,2],
                      nameX="Leerlaufzeit*",
                      track=track,
                      useTrack=useTrack,
                      yLab="Zeit [h]",
                      title=paste("Leerlaufzeit* pro Streckenprofil: insgesamt",sprintf("%.0f",sum(x[,1])),"h bezogen auf",sprintf("%.0f",sum(mile)),"km"),
                      subtitle=paste("* Leerlauf: Motordrehzahl > 0 U/min und Motordrehzahl <",idleModeBreaks[2],"U/min,     ** Betrieb: Motordrehzahl > 0 km/h"),
                      unit="h",
                      save=save)
  }else if(plotId==208){
    #2.6 Volllastbeschleunigung
    plotBars(plotPath=plotPath,
             plotName=plotName,
             x=x,
             breaks=breaksX[breaksX>=0],
             xLab="Volllastbeschleunigungsdauer [s]",
             yLab="Anzahl Volllastbeschleunigungsphasen",
             title=paste("Volllastbeschleunigungsphasen* pro Dauerklasse: insgesamt",sprintf("%.0f",sum(x)),"bezogen auf",sprintf("%.0f",sum(mile)),"km"),
             subtitle=paste0("* Starkbeschleunigungsphase: solange Motordrehzahl \u2265 ",pedHighBreaks2[1]," U/min und Gaspedalwert \u2265 ",pedHighBreaks1[1],"%"),
             save=save)
  }else if(plotId==209){
    
    #2.7 LÃ¤ngs-/Querbeschleunigung-Kennfeld
    # breaksX3 <- c(breaksX[breaksX<0],-0.01,0.01,breaksX[breaksX>0])
    # breaksY3 <- breaksX3
    plotHeatmap(plotPath=plotPath,
                plotName=plotName,
                x=x/3600,
                breaksX=round(breaksX,1),
                breaksY=round(breaksY,1),
                xLab="Fahrzeugquerbeschleunigung [m/s²]",
                yLab="Fahrzeuglängsbeschleunigung [m/s²]",
                title="Beschleunigungskennfeld des Fahrzeugs",
                subtitle=paste("(Relative) Betriebszeit* pro Klasse: insgesamt",sprintf("%.0f",sum(x)/3600),"h bezogen auf",sprintf("%.0f",sum(mile)),"km"),
                subsubtitle="* Betrieb: Motordrehzahl > 0 U/min",
                save=save)
  }else if(plotId==210){
    #2.8 HÃ¶henprofil
    plotBars(plotPath=plotPath,
             plotName=plotName,
             x=x/3600,
             breaks=breaksX[breaksX>=0],
             xLab="Höhe [m]",
             yLab="Betriebszeit [h]",
             title=paste("Betriebszeit* des Verbrennungsmotors pro GPS-Höhenklasse: insgesamt",sprintf("%.0f",sum(x)/3600),"h bezogen auf",sprintf("%.0f",sum(mile)),"km"),
             subtitle="* Betrieb: Motordrehzahl > 0 U/min",
             splitClassNames=TRUE,
             save=save)
  }else if(plotId==211){
    #2.9.1 Schubbetriebszeit
    plotBarsPerTrack2(plotPath=plotPath,
                      plotName=plotName,
                      x=x[,1],
                      nx=x[,2],
                      nameX="Schubbetriebszeit*",
                      track=track,
                      useTrack=useTrack,
                      yLab="Schubbetriebszeit [h]",
                      title=paste("Schubbetriebszeit* pro Streckenprofil: insgesamt",sprintf("%.0f",sum(x[,1])),"h bezogen auf",sprintf("%.0f",sum(mile)),"km"),
                      subtitle=paste("* Schubbetrieb: Motordrehzahl \u2265",pushModeBreaks2[2],"U/min und Verbrauch = 0 Âµl/250ms,     ** Betrieb: Motordrehzahl > 0 U/min"),
                      unit="h",
                      save=save)
  }else if(plotId==212){
    #2.9.2 Schubbetriebsphase
    options(scipen=999)
    plotBars(plotPath=plotPath,
             plotName=plotName,
             x=x,
             breaks=breaksX[breaksX>=0],
             xLab="Schubbetriebsdauer [s]",
             yLab="Anzahl Schubbetriebsphasen",
             title=paste("Schubbetriebsphasen* pro Dauerklasse: insgesamt",sprintf("%.0f",sum(x)),"bezogen auf",sprintf("%.0f",sum(mile)),"km"),
             subtitle=paste("* Schubbetriebsphase: solange Motordrehzahl \u2265",pushModeBreaks2[2],"U/min und Verbrauch = 0 µl/250ms"),
             save=save)
  }else if(plotId==220){
    #2.13 Regeneration des DieselruÃpartikelfilters
    x<-table(x)
    plotBars(plotPath=plotPath,
             plotName=plotName,
             x=(x),
             breaks=breaksX[breaksX>=0],
             xLab="Dauer DPF-Regeneration [min]",
             yLab="Anzahl DPF-Regenerationen",
             title=paste("DPF-Regenerationen* pro Dauerklasse: insgesamt",sprintf("%.0f",sum(x)),"bezogen auf",sprintf("%.0f",sum(mile)),"km"),
             # subtitle=paste0("* Starkbeschleunigungsphase: solange Motordrehzahl \u2265 ",dpfRegBreaks2[1]," U/min und PFltCond_stFlg \u2265 ",dpfRegBreaks1[2],"%"),
             save=save)
  }else if(plotId==221){
    #   2.14 Fahrprogram
    classCodes<-c(0,1,2,3,4,5,6,7,15)
    classNames = c("Sport","Comfort","Sport Plus","Race","Economy","Comfort Plus","Offroad","Manual","SNA")
    df<-data.frame(classCodes,classNames)
    c<- df$classCodes %in% names(x)
    c1<-classNames[c]
    
    plotBars(plotPath=plotPath,
             plotName=plotName,
             x=x/3600,
             # classNames = c("Sport","Comfort","Sport Plus","Race","Economy","Comfort Plus","Offroad","Manual","SNA"),
             classNames = c1,
             xLab="Fahrprogrammmodus",
             yLab="Betriebszeit [h]",
             title=paste("Betriebszeit* des Verbrennungsmotors pro Fahrprogrammmodus : insgesamt",sprintf("%.0f",sum(x)/3600),"h bezogen auf",sprintf("%.0f",sum(mile)),"km"),
             subtitle="* Betrieb: Motordrehzahl > 0 U/min",
             save=save)
  }else if(plotId==222){
    #2.15.1 Anzahl Starts
    plotBarsPerTrack1(plotPath=plotPath,
                      plotName=plotName,
                      x=x,
                      track=track,
                      useTrack=useTrack,
                      yLab="Anzahl Starts",
                      title=paste("Anzahl Starts* pro Streckenprofil: insgesamt",sprintf("%.0f",sum(x)),"bezogen auf",sprintf("%.0f",sum(mile)),"km"),
                      subtitle=paste0("* Start: Motordrehzahl übersteigt die Schwelle von ",startThresh," U/min und bleibt mindestens ",startMinTime," Zeitschritte (",startMinTime," Sekunden) darüber"),
                      distance=distance/100,
                      unitPerDist="/100km",
                      save=save)
  }else if(plotId==223){
    #2.15.2 Motoröltemperatur beim Start
    plotBars(plotPath=plotPath,
             plotName=plotName,
             x=x,
             breaks=breaksX,
             xLab="Motoröltemperatur zum Startzeitpunkt [C°]",
             yLab="Anzahl Starts",
             title=paste("Anzahl Starts* pro Motoröltemperaturklasse: insgesamt",sprintf("%.0f",sum(x)),"bezogen auf",sprintf("%.0f",sum(mile)),"km"),
             subtitle=paste0("* Start: Motordrehzahl übersteigt die Schwelle von ",startThresh," U/min und bleibt mindestens ",startMinTime," Zeitschritte (",startMinTime," Sekunden) darüber"),
             save=save)
  }else if(plotId==224){
    #2.15.3 Motorabstelldauer
    plotBars(plotPath=plotPath,
             plotName=plotName,
             x=x,
             breaks=breaksX[breaksX>=0],
             xLab="Motorabstelldauer [s]",
             yLab="Anzahl Starts",
             title=paste("Anzahl Starts pro Motorabstelldauerklasse*: insgesamt",sprintf("%.0f",sum(x)),"bezogen auf",sprintf("%.0f",sum(mile)),"km"),
             subtitle=paste("* Abstellphase: solange Motordrehzahl = 0 U/min"),
             save=save)
  }else if(plotId==225){
    #2.15.4 Anzahl Hochlaststarts
    plotBarsPerTrack1(plotPath=plotPath,
                      plotName=plotName,
                      x=x,
                      track=track,
                      useTrack=useTrack,
                      yLab="Anzahl Hochlaststarts",
                      title=paste("Anzahl Hochlaststarts* pro Streckenprofil: insgesamt",sprintf("%.0f",sum(x)),"bezogen auf",sprintf("%.0f",sum(mile)),"km"),
                      subtitle=paste0("* Hochlaststart: Start (s. 2.23.1), bei dem im Bereich von ",highLoadStartTime," Zeitschritte (",highLoadStartTime," Sekunden) vor bis ",highLoadStartTime," Zeitschritte nach Startzeitpunkt mindestens einmal Gaspedalwert \u2265 ",highLoadStartThresh,"% ist"),
                      distance=distance/10000,
                      unitPerDist="/100km",
                      save=save)
  }else if(plotId==226){
    #2.15.5 Fahrzeuggeschwindigkeit beim 3S-Start
    # This code xlab shows CÂ° but requirement is Km/h
    breaksX2 <- breaksX[breaksX>0]
    breaksX2 <- sort(unique(c(0,1,breaksX2)))
    plotBarsLog(plotPath=plotPath,
                plotName=plotName,
                x=x,
                breaks=breaksX2,
                xLab="Fahrzeuggeschwindigkeit zum 3S-Startzeitpunkt [km/h]",
                yLab="Anzahl 3S-Starts (In log Scale)",
                pTitle =paste("Anzahl 3-S Starts* pro Fahrzeuggeschwindigkeitsklasse: insgesamt",sprintf("%.0f",sum(x)),"bezogen auf",sprintf("%.0f",sum(mile)),"km"),
                pSubtitle=paste0("* 3S-Start:  Ende einer Sequenz mit Zündungsmodus(Ign_Mode = 1) und Motordrehzahl = 0 U/min"),
                save=save)
  }else if(plotId==22611){
    #2.15.5 Fahrzeuggeschwindigkeit beim 3S-Start
    # This code xlab shows C° but requirement is Km/h
    breaksX2 <- breaksX[breaksX>0]
    breaksX2 <- sort(unique(c(0,1,breaksX2)))
    plotBarsLog(plotPath=plotPath,
                plotName=plotName,
                x=x,
                breaks=breaksX2,
                xLab="Fahrzeuggeschwindigkeit zum 3S-Startzeitpunkt [km/h]",
                yLab="Anzahl 3S-Starts (In log Scale)",
                pTitle =paste("Anzahl 3-S Starts* pro Fahrzeuggeschwindigkeitsklasse: insgesamt",sprintf("%.0f",sum(x)),"bezogen auf",sprintf("%.0f",sum(mile)),"km"),
                pSubtitle=paste0("* 3S-Start:  Ende einer Sequenz mit Zündungsmodus(Ign_Mode = 1) und Motordrehzahl = 0 U/min"),
                save=save)
  }else if(plotId==2261){
    #2.15.6 Fahrpedalstellung beim 3S-Start
    breaksX2 <- breaksX[breaksX>0]
    breaksX2 <- breaksX2[breaksX2<=100]
    breaksX2 <- sort(unique(c(0,0.01,breaksX2)))
    plotBarsLog(plotPath=plotPath,
                plotName=plotName,
                x=x,
                breaks=breaksX2,
                xLab="Anzahl 3S-Starts in Fahrpedalklassen",
                yLab="Anzahl Starts (In log Scale)",
                pTitle=paste("Anzahl 3S-Starts pro Fahrpedalstellung: insgesamt ",sprintf("%.0f",sum(x)),"bezogen auf",sprintf("%.0f",sum(mile)),"km"),
                pSubtitle="* 3S-Start: Ende einer Sequenz mit Zündungsmodus(Ign_Mode = 1) und Motordrehzahl = 0 U/min",
                save=save)
  }else if (plotId == 2262){
    # 2.15.7 Drehzahlverlauf des Verbrennungsmotors beim Start
    #x<-readRDS(file=paste0(dir,"Daten",mainConfig[idx,7],".rds"))
    plotEngRPMOverTime(plotPath = plotPath,
                       plotName = plotName,
                       x=x,
                       save=TRUE)
  }else if(plotId==227){
    #2.16.1 Außenlufttemperatur
    plotBars(plotPath=plotPath,
             plotName=plotName,
             x=x/3600,
             breaks=breaksX,
             xLab="Außentemperatur [C°]",
             yLab="Betriebszeit [h]",
             title=paste("Betriebszeit* des Verbrennungsmotors pro Außentemperaturklasse: insgesamt",sprintf("%.0f",sum(x)/3600),"h bezogen auf",sprintf("%.0f",sum(mile)),"km"),
             subtitle="* Betrieb: Motordrehzahl > 0 U/min",
             save=save)
  }else if(plotId==228){
    #2.16.2 Kühlmitteltemperatur
    plotBars(plotPath=plotPath,
             plotName=plotName,
             x=x/3600,
             breaks=breaksX,
             xLab="Kühlmitteltemperatur [C°]",
             yLab="Betriebszeit [h]",
             title=paste("Betriebszeit* des Verbrennungsmotors pro Kühlmitteltemperaturklasse: insgesamt",sprintf("%.0f",sum(x)/3600),"h bezogen auf",sprintf("%.0f",sum(mile)),"km"),
             subtitle="* Betrieb: Motordrehzahl > 0 U/min",
             save=save)
  }else if(plotId==230){
    #2.17.1 Verweildauer pro Temperaturklasse
    plotBars(plotPath=plotPath,
             plotName=plotName,
             x=x/3600,
             breaks=breaksX,
             xLab="Motoröltemperatur [C°]",
             yLab="Betriebszeit [h]",
             title=paste("Betriebszeit* des Verbrennungsmotors pro Motoröltemperaturklasse: insgesamt",sprintf("%.0f",sum(x)/3600),"h bezogen auf",sprintf("%.0f",sum(mile)),"km"),
             subtitle="* Betrieb: Motordrehzahl > 0 U/min",
             save=save)
  }else if(plotId==231){
    #2.17.2 Drehmoment-Öltemperatur-Kennfeld
    # breaksXData <- subset(pwrTrqConfig[,8:10],pwrTrqConfig[,2] == as.character(engSeries[1,1]))
    breaksXData <- subset(pwrTrqConfig[,8:10],pwrTrqConfig[,2] == as.character(engSeries))
    breaksX <- c(-Inf,seq(breaksXData[1,1],breaksXData[1,2],breaksXData[1,3]),Inf)
    
    plotHeatmap(plotPath=plotPath,
                plotName=plotName,
                x=x/3600,
                breaksX=breaksX,
                breaksY=breaksY,
                xLab="Motordrehmoment [Nm]",
                yLab="Motoröltemperatur [C°]",
                title="Drehmoment-Öltemperatur-Kennfeld des Verbrennungsmotors",
                subtitle=paste("(Relative) Betriebszeit* pro Klasse: insgesamt",sprintf("%.0f",sum(x,na.rm=TRUE)/3600),"h bezogen auf",sprintf("%.0f",sum(mile)),"km"),
                subsubtitle="* Betrieb: Motordrehzahl > 0 U/min",
                save=save)
  }else if(plotId==232){
    #2.17.3 Drehzahl-Öltemperatur-Kennfeld
    breaksX2 <- breaksX[breaksX>=0]
    breaksX2[breaksX2==0] <- 1
    plotHeatmap(plotPath=plotPath,
                plotName=plotName,
                x=x/3600,
                breaksX=breaksX2,
                breaksY=breaksY,
                xLab="Motordrehzahl [U/min]",
                yLab="Motoröltemperatur [C°]",
                title="Drehzahl-Öltemperatur-Kennfeld des Verbrennungsmotors",
                subtitle=paste("(Relative) Betriebszeit* pro Klasse: insgesamt",sprintf("%.0f",sum(x[,as.numeric(colnames(x))>0],na.rm=TRUE)/3600),"h bezogen auf",sprintf("%.0f",sum(mile)),"km"),
                subsubtitle="* Betrieb: Motordrehzahl > 0 U/min",
                save=save)
  }else if(plotId==233){
    #2.18.1 Verweildauer pro Druckklasse
    plotBars(plotPath=plotPath,
             plotName=plotName,
             x=x/3600,
             breaks=breaksX[breaksX>=0],
             xLab="Motoröldruck [bar]",
             yLab="Betriebszeit [h]",
             title=paste("Betriebszeit* des Verbrennungsmotors pro Motoröldruckklasse: insgesamt",sprintf("%.0f",sum(x)/3600),"h bezogen auf",sprintf("%.0f",sum(mile)),"km"),
             subtitle="* Betrieb: Motordrehzahl > 0 U/min",
             save=save)
  }else if(plotId==234){
    #2.18.2 Drehzahl-Öldruck-Kennfeld
    breaksX2 <- breaksX[breaksX>=0]
    breaksX2[breaksX2==0] <- 1
    plotHeatmap(plotPath=plotPath,
                plotName=plotName,
                x=x/3600,
                breaksX=breaksX2,
                breaksY=breaksY[breaksY>=0],
                xLab="Motordrehzahl [U/min]",
                yLab="Motoröldruck [bar]",
                title="Drehzahl-Öldruck-Kennfeld des Verbrennungsmotors",
                subtitle=paste("(Relative) Betriebszeit* pro Klasse: insgesamt",sprintf("%.0f",sum(x[,as.numeric(colnames(x))>0],na.rm=TRUE)/3600),"h bezogen auf",sprintf("%.0f",sum(mile)),"km"),
                subsubtitle="* Betrieb: Motordrehzahl > 0 U/min",
                save=save)
    # }else if(plotId==235 && sum(x)>0 && sum(y)>0){
    #   # 2.19 MotorÃ¶lstand
    #   # Need to alter the breaks; the output is not the same
    #   if(save){
    #     png(paste0(plotPath,plotName,".png"),width=1000,height=600)
    #     cexLab <- 1.5
    #     cexAx <- 1.5
    #     cexMain <- 2
    #     cexSub <- 1.1
    #     cexLeg <- 1.3
    #     cexTxt <- 1.3
    #   }else{
    #     cexLab <- 1
    #     cexAx <- 1
    #     cexMain <- 1
    #     cexSub <- 1
    #     cexLeg <- 1
    #     cexTxt <- 1
    #   }
    #   legPos <- "topright"
    #   y[4118657:4122037] <- y[4118656]
    #   plot(x[seq(1,length(x),100)],y[seq(1,length(x),100)],xlab="Kilometerstand [km]",ylab="MotorÃ¶lstand [mm]",main="MotorÃ¶lstand Ã¼ber Kilometerstand",ylim=c(80,150),cex.lab=cexLab,cex.axis=cexAx,cex.main=cexMain)
    #   rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col="gray87")
    #   grid(col="white")
    #   
    #   
    #   # d <- c(0,721395,1317280,1537638,1919090,2076250,2827557,3211272,3823129,4122231,4812463,5015185,5857660,6536785,6897370,7363314)
    #   # d <- c(0,721395,1317280,1537638,2076250,2559624,2827557,2909902,3823129,4227673,4812463,5015185,5857660,6536785,6897370,7363314)
    #   d=c(0,279087,1341272,2105441,3257020,3574414,4268632,4969967,5514036,6426145,6810538)
    #   for(idx in 1:(length(d)-1)){
    #     x2 <- x[(1+d[idx]):d[idx+1]]
    #     y2 <- y[(1+d[idx]):d[idx+1]]
    #     lines(x2[seq(1,length(x2),100)],y2[seq(1,length(x2),100)],col="#00008090",lwd=2)
    #     lmXY <- lm(y2~x2)
    #     lines(x2[c(1,length(x2))],lmXY$coefficients[1]+lmXY$coefficients[2]*x2[c(1,length(x2))],col="#80000090",lwd=2,type="o")
    #     z <- x2[1]+diff(x2[c(1,length(x2))])/2
    #     lines(rep(z,2),c(0,lmXY$coefficients[1]+lmXY$coefficients[2]*z),col="#00800090",lty=2)
    #     text(z,min(y)+1,sprintf("%.2f",lmXY$coefficients[2]*1000),col="#008000",cex=cexTxt)
    #   }
    #   legend(legPos,legend=c("Gleitendes Mittel des Ölstands über 1 h","Lineare Approximation","Steigung der Approximation in mm/1000km"),lwd=c(2,2,NA),pch=c(NA,NA,"x"),col=c("#00008090","#80000090","#00800090"),cex=cexLeg)
    #   if(save){
    #     dev.off()
    #   }
  }else if(plotId==2351){
    # 2.23.3 T3-Abgastemperatur
    plotHeatmapCam1(plotPath=plotPath,
                    plotName=plotName,
                    x=x,
                    breaks=breaksX,
                    breaksY=breaksY,
                    xLab="Motordrehzahl [U/min]",
                    yLab="Motordrehmoment [Nm]",
                    #title=paste("Drehzahl-Drehmoment-kennfeld des Verbrennungsmotors"),
                    title=paste("Schaltungen bezogen auf",sprintf("%.0f",sum(mile)),"km"),
                    subsubtitle = paste0("* Betrieb: Motordrehzahl > 0 U/min \n Aufgrund der Abtastrate von 1Hz kann die Anzahl der Schaltungen größer sein !"),
                    #subsubtitle="* Betrieb: Motordrehzahl > 0 U/min",
                    #splitClassNames=TRUE,
                    save=save)
  }else if(plotId==2352){
    # 2.23.3 T3-Abgastemperatur
    plotHeatmapCam2(plotPath=plotPath,
                    plotName=plotName,
                    x=x,
                    breaks=breaksX,
                    breaksY=breaksY,
                    xLab="Motordrehzahl [U/min]",
                    yLab="Motordrehmoment [Nm]",
                    #title=paste("Drehzahl-Drehmoment-kennfeld des Verbrennungsmotors"),
                    title=paste("bezogen auf",sprintf("%.0f",sum(mile)),"km"),
                    subsubtitle="* Betrieb: Motordrehzahl > 0 U/min",
                    #splitClassNames=TRUE,
                    save=save)
  }else if(plotId==2353){
    # 2.23.3 T3-Abgastemperatur
    breaksX<-breaksX[breaksX>=0]
    plotHeatmapCam3(plotPath=plotPath,
                    plotName=plotName,
                    x=x,
                    breaks=breaksX,
                    breaksY=breaksY,
                    unit = "km",
                    xLab="Motordrehzahl [U/min]",
                    yLab="Motordrehmoment [Nm]",
                    #title=paste("Drehzahl-Drehmoment-kennfeld des Verbrennungsmotors"),
                    title=paste("bezogen auf",sprintf("%.0f",sum(mile)),"km"),
                    subsubtitle="* Betrieb: Motordrehzahl > 0 U/min",
                    #splitClassNames=TRUE,
                    save=save)
  }else if(plotId==2354){
    # 2.23.3 T3-Abgastemperatur
    plotHeatmapCam4(plotPath=plotPath,
                    plotName=plotName,
                    x=x,
                    breaks=breaksX,
                    breaksY=breaksY,
                    unit = "km",
                    xLab="Motordrehzahl [U/min]",
                    yLab="Motordrehmoment [Nm]",
                    #title=paste("Drehzahl-Drehmoment-kennfeld des Verbrennungsmotors"),
                    title=paste("bezogen auf",sprintf("%.0f",sum(mile)),"km"),
                    subsubtitle="* Betrieb: Motordrehzahl > 0 U/min",
                    #splitClassNames=TRUE,
                    save=save)
  }else if(plotId==2355){
    # 2.23.3 T3-Abgastemperatur
    plotHeatmapCam5(plotPath=plotPath,
                    plotName=plotName,
                    x=x,
                    breaks=breaksX,
                    breaksY=breaksY,
                    unit = "s",
                    xLab="Motordrehzahl [U/min]",
                    yLab="Motordrehmoment [Nm]",
                    #title=paste("Drehzahl-Drehmoment-kennfeld des Verbrennungsmotors"),
                    title=paste("bezogen auf",sprintf("%.0f",sum(mile)),"km"),
                    subsubtitle="* Betrieb: Motordrehzahl > 0 U/min",
                    #splitClassNames=TRUE,
                    save=save)
  }else if(plotId==2356){
    # 2.23.3 T3-Abgastemperatur
    plotHeatmapCam6(plotPath=plotPath,
                    plotName=plotName,
                    x=x,
                    xLab="Motordrehzahl [U/min]",
                    yLab="Motordrehmoment [Nm]",
                    #title=paste("Drehzahl-Drehmoment-kennfeld des Verbrennungsmotors"),
                    title=paste("bezogen auf",sprintf("%.0f",sum(mile)),"km"),
                    subsubtitle="* Betrieb: Motordrehzahl > 0 U/min",
                    #splitClassNames=TRUE,
                    save=save)
  }else if(plotId==244){
    # 2.23.3 T3-Abgastemperatur
    plotBars(plotPath=plotPath,
             plotName=plotName,
             x=x/3600,
             breaks=breaksX,
             xLab="T3-Abgastemperatur [C°]",
             yLab="Zeit [h]",
             title=paste("Betriebszeit* des Verbrennungsmotors pro T3-Abgastemperaturklasse: insgesamt",sprintf("%.0f",sum(x)/3600),"h bezogen auf",sprintf("%.0f",sum(mile)),"km"),
             subtitle="* Betrieb: Motordrehzahl > 0 U/min",
             splitClassNames=TRUE,
             save=save)
  }else if(plotId==2441){
    #2.23.4 Drehzahl-T3-Abgastemperatur-Kennfeld
    breaksX2 <- breaksX[breaksX>=0]
    breaksX2[breaksX2==0] <- 1
    plotHeatmap(plotPath=plotPath,
                plotName=plotName,
                x=x/3600,
                breaksX=breaksX2,
                breaksY=breaksY,
                xLab="Motordrehzahl [U/min]",
                yLab="T3-Abgastemperatur [C°]",
                title="Drehzahl-T3-Abgastemperatur-Kennfeld des Verbrennungsmotors",
                subtitle=paste("(Relative) Betriebszeit* pro Klasse: insgesamt",sprintf("%.0f",sum(x[,as.numeric(colnames(x))>0],na.rm=TRUE)/3600),"h bezogen auf",sprintf("%.0f",sum(mile)),"km"),
                subsubtitle="* Betrieb: Motordrehzahl > 0 U/min",
                save=save)
  }else if(plotId==2443){
    #2.23.6 Verweildauer pro Turbine-Drehzahlklasse
    plotBars(plotPath=plotPath,
             plotName=plotName,
             x=x/3600,
             breaks=breaksX,
             xLab="ATL-Drehzahl [1000 U/min]",
             yLab="Betriebszeit [h]",
             title=paste("Betriebszeit* des Verbrennungsmotors pro ATL-Drehzahlklasse: insgesamt",sprintf("%.0f",sum(x)/3600),"h bezogen auf",sprintf("%.0f",sum(mile)),"km"),
             subtitle="* Betrieb: Motordrehzahl > 0 U/min",
             save=save)
  }else if(plotId==2444){
    #2.23.7 Drehzahl-Turbine-Drehzahl-Kennfeld
    # breaksX2 <- breaksX[breaksX>=0]
    # breaksX2[breaksX2==0] <- 1
    plotHeatmap(plotPath=plotPath,
                plotName=plotName,
                x=x/3600,
                # breaksX=breaksX2,
                breaksX=breaksX,
                # breaksY=breaksY[breaksY>=0],
                breaksY=breaksY,
                xLab="Motordrehzahl [U/min]",
                yLab="ATL-Drehzahl [1000 U/min]",
                title="ATL-Drehzahl - Motordrehzahl-Kennfeld des Verbrennungsmotors",
                subtitle=paste("(Relative) Betriebszeit* pro Klasse: insgesamt",sprintf("%.0f",sum(x[,as.numeric(colnames(x))>0],na.rm=TRUE)/3600),"h bezogen auf",sprintf("%.0f",sum(mile)),"km"),
                subsubtitle="* Betrieb: Motordrehzahl > 0 U/min",
                save=save)
  }else if(plotId==24441){
    #2.23.8 Abgastemperatur T3 von Zylinder 1 & 4
    plotBars3(plotPath=plotPath,
              plotName=plotName,
              x=x/3600,
              breaks=breaksX,
              xLab="T3-Abgastemperatur [C°]",
              yLab="Betriebszeit [h]",
              title=paste("Betriebszeit* des Verbrennungsmotors pro T3-Abgastemperaturklasse: insgesamt",sprintf("%.0f",sum(x)/3600),"h bezogen auf",sprintf("%.0f",sum(mile)),"km"),
              subtitle="* Betrieb: Motordrehzahl > 0 U/min",
              splitClassNames=TRUE,
              save=save)
  }else if(plotId==24442){
    #2.23.9 Abgastemperatur T3 von Zylinder 1 & 4
    plotBars3(plotPath=plotPath,
              plotName=plotName,
              x=x/3600,
              breaks=breaksX,
              xLab="T3-Abgastemperatur [C°]",
              yLab="Betriebszeit [h]",
              title=paste("Betriebszeit* des Verbrennungsmotors pro T3-Abgastemperaturklasse: insgesamt",sprintf("%.0f",sum(x)/3600),"h bezogen auf",sprintf("%.0f",sum(mile)),"km"),
              subtitle="* Betrieb: Motordrehzahl > 0 U/min",
              splitClassNames=TRUE,
              save=save)
  }else if(plotId==24443){
    #2.23.10 Abgastemperatur T3 von Zylinder 2 & 3
    plotBars3(plotPath=plotPath,
              plotName=plotName,
              x=x/3600,
              breaks=breaksX,
              xLab="T3-Abgastemperatur [C°]",
              yLab="Betriebszeit [h]",
              title=paste("Betriebszeit* des Verbrennungsmotors pro T3-Abgastemperaturklasse: insgesamt",sprintf("%.0f",sum(x)/3600),"h bezogen auf",sprintf("%.0f",sum(mile)),"km"),
              subtitle="* Betrieb: Motordrehzahl > 0 U/min",
              splitClassNames=TRUE,
              save=save)
  }else if(plotId==24444){
    #2.23.11 Abgastemperatur T3 von Zylinder 2 & 3
    plotBars3(plotPath=plotPath,
              plotName=plotName,
              x=x/3600,
              breaks=breaksX,
              xLab="T3-Abgastemperatur [C°]",
              yLab="Betriebszeit [h]",
              title=paste("Betriebszeit* des Verbrennungsmotors pro T3-Abgastemperaturklasse: insgesamt",sprintf("%.0f",sum(x)/3600),"h bezogen auf",sprintf("%.0f",sum(mile)),"km"),
              subtitle="* Betrieb: Motordrehzahl > 0 U/min",
              splitClassNames=TRUE,
              save=save)
  }else if(plotId==245){
    #2.24.1 Raildruck
    plotBars(plotPath=plotPath,
             plotName=plotName,
             x=x/3600,
             breaks=breaksX[breaksX>=0],
             xLab="Raildruck [bar]",
             yLab="Betriebszeit [h]",
             title=paste("Betriebszeit* des Verbrennungsmotors pro Raildruckklasse: insgesamt",sprintf("%.0f",sum(x)/3600),"h bezogen auf",sprintf("%.0f",sum(mile)),"km"),
             subtitle="* Betrieb: Motordrehzahl > 0 U/min",
             splitClassNames=TRUE,
             save=save)
  }else if(plotId==247){
    #2.24.3 Zeitanteil Kraftstofftemperatur
    plotBars(plotPath=plotPath,
             plotName=plotName,
             x=x/3600,
             breaks=breaksX,
             xLab="Kraftstofftemperatur [C°]",
             yLab="Betriebszeit [h]",
             title=paste("Betriebszeit* des Verbrennungsmotors pro Kraftstofftemperaturklasse: insgesamt",sprintf("%.0f",sum(x)/3600),"h bezogen auf",sprintf("%.0f",sum(mile)),"km"),
             subtitle="* Betrieb: Motordrehzahl > 0 U/min",
             save=save)
  }else if(plotId==249){
    #2.24.5 Raildruck beim Start
    plotBars(plotPath=plotPath,
             plotName=plotName,
             x=x,
             breaks=breaksX[breaksX>=0],
             xLab="Raildruck zum Startzeitpunkt [bar]",
             yLab="Anzahl Starts",
             title=paste("Anzahl Starts* pro Raildruckklasse: insgesamt",sprintf("%.0f",sum(x)),"bezogen auf",sprintf("%.0f",sum(mile)),"km"),
             subtitle=paste0("* Start: Motordrehzahl übersteigt die Schwelle von ",startThresh," U/min und bleibt mindestens ",startMinTime," Zeitschritte (",startMinTime," Sekunden) darüber"),
             splitClassNames=TRUE,
             save=save)
  }else if(plotId==250){
    #2.24.6 Klassierung Kraftstofftemperatur beim Start
    plotBars(plotPath=plotPath,
             plotName=plotName,
             x=x,
             breaks=breaksX,
             xLab="Kraftstofftemperatur zum Startzeitpunkt [CÂ°]",
             yLab="Anzahl Starts",
             title=paste("Anzahl Starts* pro Kraftstofftemperaturklasse: insgesamt",sprintf("%.0f",sum(x)),"bezogen auf",sprintf("%.0f",sum(mile)),"km"),
             subtitle=paste0("* Start: Motordrehzahl übersteigt die Schwelle von ",startThresh," U/min und bleibt mindestens ",startMinTime," Zeitschritte (",startMinTime," Sekunden) darüber"),
             save=save)
  }else if(plotId==2501){
    # 2.24.7 Kraftstoffniederdruck
    plotBars(plotPath=plotPath,
             plotName=plotName,
             x=x/3600,
             breaks=breaksX,
             xLab="Kraftstoffniederdruck [bar]",
             yLab="Betriebszeit [h]",
             title=paste("Betriebszeit* des Verbrennungsmotors pro Kraftstoffniederdruckklasse: insgesamt",sprintf("%.0f",sum(x)/3600),"h bezogen auf",sprintf("%.0f",sum(mile)),"km"),
             subtitle="* Betrieb: Motordrehzahl > 0 U/min",
             splitClassNames=TRUE,
             save=save)
  }else if(plotId==2502){
    # 2.24.8 Kraftstoffniederdruck beim Start
    plotBars(plotPath=plotPath,
             plotName=plotName,
             x=x,
             breaks=breaksX,
             xLab="Kraftstoffniederdruck zum Startzeitpunkt [bar]",
             yLab="Anzahl Starts",
             title=paste("Anzahl Starts* pro Kraftstoffniederdruckklasse: insgesamt",sprintf("%.0f",sum(x)),"bezogen auf",sprintf("%.0f",sum(mile)),"km"),
             subtitle=paste0("* Start: Motordrehzahl übersteigt die Schwelle von ",startThresh," U/min und bleibt mindestens ",startMinTime," Zeitschritte (",startMinTime," Sekunden) darüber"),
             splitClassNames=TRUE,
             save=save)
  }else if(plotId==2503){
    # 2.24.9 Kraftstofftemperatur Niederdruckkreislauf
    plotBars(plotPath=plotPath,
             plotName=plotName,
             x=x/3600,
             breaks=breaksX,
             xLab="Kraftstofftemperatur [C°]",
             yLab="Betriebszeit [h]",
             title=paste("Betriebszeit* des Verbrennungsmotors pro Kraftstofftemperaturklasse: insgesamt",sprintf("%.0f",sum(x)/3600),"h bezogen auf",sprintf("%.0f",sum(mile)),"km"),
             subtitle="* Betrieb: Motordrehzahl > 0 U/min",
             save=save)
  }else if(plotId==255){
    # 2.26.1 T4-Abgastemperatur
    plotBars(plotPath=plotPath,
             plotName=plotName,
             x=x/3600,
             breaks=breaksX,
             xLab="T4-Abgastemperatur [C°]",
             yLab="Betriebszeit [h]",
             title=paste("Betriebszeit* des Verbrennungsmotors pro T4-Abgastemperaturklasse: insgesamt",sprintf("%.0f",sum(x)/3600),"h bezogen auf",sprintf("%.0f",sum(mile)),"km"),
             subtitle="* Betrieb: Motordrehzahl > 0 U/min",
             splitClassNames=TRUE,
             save=save)
  }else if(plotId==257){
    # 2.27.1 Stellerposition ND-AGR
    
    plotBars(plotPath=plotPath,
             plotName=plotName,
             x=x/3600,
             breaks=breaksX[breaksX>=0&breaksX<=100],
             xLab="Lagerückmeldung Niederdruckabgasrückführventil [%]",
             yLab="Betriebszeit [h]",
             title=paste("Betriebszeit* des Verbrennungsmotors pro Stellerposition ND-AGR klasse: insgesamt",sprintf("%.0f",sum(x)/3600),"h bezogen auf",sprintf("%.0f",sum(mile)),"km"),
             subtitle="* Betrieb: Motordrehzahl > 0 U/min",
             splitClassNames=TRUE,
             save=save)
  }else if(plotId==258){
    # 2.27.2 Stellerposition HD-AGR
    plotBars(plotPath=plotPath,
             plotName=plotName,
             x=x/3600,
             breaks=breaksX[breaksX>=0&breaksX<=100],
             xLab="Lagerückmeldung Hochdruckabgasrückführventil [%]",
             yLab="Betriebszeit [h]",
             title=paste("Betriebszeit* des Verbrennungsmotors pro Stellerposition HD-AGR klasse: insgesamt",sprintf("%.0f",sum(x)/3600),"h bezogen auf",sprintf("%.0f",sum(mile)),"km"),
             subtitle="* Betrieb: Motordrehzahl > 0 U/min",
             splitClassNames=TRUE,
             save=save)
  }else if(plotId==2581){
    # 2.27.3
    plotBarsPerTrack1(plotPath=plotPath,
                      plotName=plotName,
                      x=x,
                      track=track,
                      useTrack=useTrack,
                      yLab="Anzahl Schaltungen",
                      title=paste("Anzahl AGR-Kühler-Bypass-Klappen-Schaltungen pro Streckenprofil",sprintf("%.0f",sum(x)),"bezogen auf",sprintf("%.0f",sum(mile)),"km"),
                      subtitle=paste0(""),
                      distance=distance/100,
                      unitPerDist="/100km",
                      save=save)
  }else if(plotId==259){
    #2.28.1 Generatorbelastung
    plotBars(plotPath=plotPath,
             plotName=plotName,
             x=x/3600,
             breaks=breaksX[breaksX>=0&breaksX<=100],#genLoadBreaks,
             xLab="Generatorbelastung [%]",
             yLab="Betriebszeit [h]",
             title=paste("Betriebszeit* des Verbrennungsmotors pro Generatorbelastungsklasse: insgesamt",sprintf("%.0f",sum(x)/3600),"h bezogen auf",sprintf("%.0f",sum(mile)),"km"),
             subtitle="* Betrieb: Motordrehzahl > 0 U/min",
             save=save)
  }else if(plotId==260){
    #2.28.2 Motordrehzahl-Generatorbelastung-Kennfeld
    breaksX2 <- breaksX[breaksX>=0]
    breaksX2[breaksX2==0] <- 1
    plotHeatmap(plotPath=plotPath,
                plotName=plotName,
                x=x/3600,
                breaksX=breaksX2,
                breaksY=breaksY[breaksY>=0],
                xLab="Motordrehzahl [U/min]",
                yLab="Generatorbelastung [%]",
                title="Motordrehzahl-Generatorbelastung-Kennfeld",
                subtitle=paste("(Relative) Betriebszeit* pro Klasse: insgesamt",sprintf("%.0f",sum(x[,as.numeric(colnames(x))>0],na.rm=TRUE)/3600),"h bezogen auf",sprintf("%.0f",sum(mile)),"km"),
                subsubtitle="* Betrieb: Motordrehzahl > 0 U/min",
                save=save)
  }else if(plotId==261){
    #2.28.3 Motordrehzahl-Elektromaschinenleistung-Kennfeld
    # Refer to 2.31.1 & 2.32.1 in 166 Rscript.. input ISG and RSG used here.. 
    # below copied the code only not considering the input ISG or RSG
    # Requirement is heatmap
    breaksX2 <- breaksX[breaksX>=0]
    breaksX2[breaksX2==0] <- 1
    plotHeatmap(plotPath=plotPath,
                plotName=plotName,
                x=x/3600,
                breaksX=breaksX2,
                breaksY=breaksY,
                xLab="Motordrehzahl [U/min]",
                yLab="Elektromaschinenleistung [kW]",
                title="Motordrehzahl-Elektromaschinenleistung-Kennfeld",
                subtitle=paste("(Relative) Betriebszeit* pro Klasse: insgesamt",sprintf("%.0f",sum(x[,as.numeric(colnames(x))>0],na.rm=TRUE)/3600),"h bezogen auf",sprintf("%.0f",sum(mile)),"km"),
                subsubtitle="* Betrieb: Motordrehzahl > 0 U/min",
                save=save)
  }else if(plotId==262){
    #2.28.4 Motordrehzahl-Elektromaschinendrehmoment-Kennfeld
    # Requirement is heatmap
    breaksX2 <- breaksX[breaksX>=0]
    breaksX2[breaksX2==0] <- 1
    plotHeatmap(plotPath=plotPath,
                plotName=plotName,
                x=x/3600,
                breaksX=breaksX2,
                breaksY=breaksY,
                xLab="Motordrehzahl [U/min]",
                yLab="Elektromaschinendrehmoment [Nm]",
                title="Motordrehzahl-Elektromaschinendrehmoment-Kennfeld",
                subtitle=paste("(Relative) Betriebszeit* pro Klasse: insgesamt",sprintf("%.0f",sum(x[,as.numeric(colnames(x))>0],na.rm=TRUE)/3600),"h bezogen auf",sprintf("%.0f",sum(mile)),"km"),
                subsubtitle="* Betrieb: Motordrehzahl > 0 U/min",
                save=save)
  }else if(plotId==263){
    #   #2.28.5 Motorleistung-Elektromaschinenleistung-Kennfeld
    #   #  Requirement is heatmap
    # breaksX2 <- breaksX[breaksX>=0]
    # breaksX2[breaksX2==0] <- 1
    # breaksXData <- subset(pwrTrqConfig[,4:6],pwrTrqConfig[,2] == as.character(engSeries[1,1]))
    breaksXData <- subset(pwrTrqConfig[,4:6],pwrTrqConfig[,2] == as.character(engSeries))
    breaksX <- c(-Inf,seq(breaksXData[1,1],breaksXData[1,2],breaksXData[1,3]),Inf)
    plotHeatmap(plotPath=plotPath,
                plotName=plotName,
                x=x/3600,
                breaksX=breaksX,
                breaksY=breaksY,
                xLab="Motorleistung [kW]",
                yLab="Elektromaschinenleistung [kW]",
                title="Leistungskennfeld für Motor und Elektromaschine",
                subtitle=paste("(Relative) Betriebszeit* pro Klasse: insgesamt",sprintf("%.0f",sum(x,na.rm=TRUE)/3600),"h bezogen auf",sprintf("%.0f",sum(mile)),"km"),
                subsubtitle="* Betrieb: Motordrehzahl > 0 U/min",
                save=save)
  }else if(plotId==2631){
    #   #2.28.6 Elektromaschine Energibilanz
    breaksX <- breaksX[breaksX>=0 & breaksX < Inf]
    breaksY <- breaksY[breaksY > -Inf & breaksY < Inf]
    print(breaksY)
    plotEnergyBalance(plotPath=plotPath,
                      plotName=plotName,
                      x=x,
                      breaksX=breaksX,
                      breaksY=breaksY, 
                      xLabel = "Drehzahlbereich [1/min]",
                      yLabel = "Energie [kWh]",
                      pTitle=paste("Energiebilanz pro Motordrehzahlklasse: insgesamt",sprintf("%.0f",nrow(na.omit(x))/3600),"h bezogen auf",sprintf("%.0f",sum(mile)),"km"),
                      # subtitle=paste("(Relative) Betriebszeit* pro Klasse: insgesamt",sprintf("%.0f",sum(x,na.rm=TRUE)/3600),"h bezogen auf",sprintf("%.0f",sum(mile)),"km"),
                      # subsubtitle="* Betrieb: Motordrehzahl > 0 U/min",
                      save=save)
  }  else if(plotId==264){
    #2.29.1 Klimakompressorbetrieb im Leerlauf
    breaksX<-breaksX[breaksX>0 & breaksX<Inf]
    splitClassName<-FALSE
    if(max(breaksX)-min(breaksX)>400){splitClassName=TRUE}
    plotStackBar(plotPath = plotPath,
                 plotName = plotName,
                 x=x,
                 breaks = breaksX,
                 xLabel = "Drehzahl [U/min]",
                 yLabel = "Zeit [h]",
                 mainTitle = paste0(" h bezogen auf ", sprintf("%.0f",sum(mile)) ," km"),# First half of the Title is present in FunctioplotStackBar
                 splitStr = "",
                 splitClassNames = splitClassName,
                 save = save)
  }else if(plotId==265){
    #2.29.2 Motordrehzahl-Klimakompressorbetrieb-Kennfeld
    # Refer to 2.31.1 & 2.32.1 in 166 Rscript.. input ISG and RSG used here..
    # below copied the code only not considering the input ISG or RSG
    # Requirement is heatmap
    # breaksX2 <- breaksX[breaksX>=0]
    # breaksX2[breaksX2==0] <- 1
    breaksX2 <- breaksX[breaksX>=0]
    breaksX2[breaksX2==0] <- 1
    plotHeatmap(plotPath=plotPath,
                plotName=plotName,
                x=x/3600,
                breaksX=breaksX2,
                breaksY=breaksY,
                xLab="Motordrehzahl [U/min]",
                yLab="Klimakompressorleistung [kW]",
                title="Drehzahl-Klimakompressorleistung-Kennfeld des Verbrennungsmotors",
                subtitle=paste("(Relative) Betriebszeit* pro Klasse: insgesamt",sprintf("%.0f",sum(x[,as.numeric(colnames(x))>0],na.rm=TRUE)/3600),"h (kompressor AN) bezogen auf",sprintf("%.0f",sum(mile)),"km"),
                subsubtitle="* Betrieb: Motordrehzahl > 0 U/min \n klimakompressorleistung(AN) > 1 kW",
                save=save)
    
  }else if(plotId==266){
    #2.29.3 Klimakompressorbetrieb im Leerlauf (Für ganze reihe)
    breaksX<-breaksX[breaksX>=0 & breaksX<Inf]
    splitClassName<-TRUE
    plotStackBar(plotPath = plotPath,
                 plotName = plotName,
                 x=x,
                 breaks = breaksX,
                 xLabel = "Drehzahl [U/min]",
                 yLabel = "Zeit [h]",
                 mainTitle = paste0(" h bezogen auf ", sprintf("%.0f",sum(mile)) ," km"),# First half of the Title is present in FunctioplotStackBar
                 splitStr = "",
                 splitClassNames = splitClassName,
                 save = save)
  }else if(plotId==267){
    # 2.29.4 Motordrehzahl-Elektromaschinenleistung-Kennfeld (Für ganze reihe))
    breaksX2 <- breaksX[breaksX>=0 & breaksX<Inf]
    breaksX2[breaksX2==0] <- 0
    plotHeatmap(plotPath=plotPath,
                plotName=plotName,
                x=x/3600,
                breaksX=breaksX2,
                breaksY=breaksY,
                xLab="Motordrehzahl [U/min]",
                yLab="Klimakompressorleistung [kW]",
                title="Drehzahl-Klimakompressorleistung-Kennfeld des Verbrennungsmotors",
                subtitle=paste("(Relative) Betriebszeit* pro Klasse: insgesamt",sprintf("%.0f",sum(x[,as.numeric(colnames(x))>0],na.rm=TRUE)/3600),"h (kompressor AN) bezogen auf",sprintf("%.0f",sum(mile)),"km"),
                subsubtitle="* Betrieb: Motordrehzahl > 0 U/min \n klimakompressorleistung(AN) > 1 kW",
                save=save)
  }else if(plotId==270){
    # 2.30.1 Verweildauern KGH-Entlüftungsdifferenzdruck
    plotBars2(plotPath=plotPath,
              plotName=plotName,
              x=x/3600,
              breaks=breaksX,
              xLab="KGH-Entlüftungsdifferenzdruck [mbar]",
              yLab="Betriebszeit [h]",
              title=paste("Betriebszeit* des Verbrennungsmotors pro KGH-Entlüftungsdiff.druckKlasse: insgesamt",sprintf("%.0f",sum(x)/3600),"h bezogen auf",sprintf("%.0f",sum(mile)),"km"),
              subtitle="* Betrieb: Motordrehzahl > 0 U/min",
              save=save)
  }else if(plotId==271){
    # 2.30.2 Drehmoment- KGH-Entlüftungsdifferenzdruck -Kennfeld
    # breaksXData <- subset(pwrTrqConfig[,8:10],pwrTrqConfig[,2] == as.character(engSeries[1,1]))
    breaksXData <- subset(pwrTrqConfig[,8:10],pwrTrqConfig[,2] == as.character(engSeries))
    
    breaksX <- c(-Inf,seq(breaksXData[1,1],breaksXData[1,2],breaksXData[1,3]),Inf)
    plotHeatmap(plotPath=plotPath,
                plotName=plotName,
                x=x/3600,
                breaksX=breaksX,
                breaksY=breaksY,
                xLab="Motordrehmoment [Nm]",
                yLab="KGH-Entlüftungsdifferenzdruck [mbar]",
                title="Drehmoment-KGH-Entlüftungsdiff.druck-Kennfeld des Verbrennungsmotors",
                subtitle=paste("(Relative) Betriebszeit* pro Klasse: insgesamt",sprintf("%.0f",sum(x,na.rm=TRUE)/3600),"h bezogen auf",sprintf("%.0f",sum(mile)),"km"),
                subsubtitle="* Betrieb: Motordrehzahl > 0 U/min",
                save=save)
  }else if(plotId==272){
    # 2.30.3 Drehzahl- KGH-Entlüftungsdifferenzdruck -Kennfeld
    breaksX2 <- breaksX[breaksX>=0]
    breaksX2[breaksX2==0] <- 1
    plotHeatmap(plotPath=plotPath,
                plotName=plotName,
                x=x/3600,
                breaksX=breaksX2,
                breaksY=breaksY,
                xLab="Motordrehzahl [U/min]",
                yLab="KGH-Entlüftungsdifferenzdruck [mbar]",
                title="Drehzahl-KGH-Entlüftungsdiff.druck-Kennfeld des Verbrennungsmotors",
                subtitle=paste("(Relative) Betriebszeit* pro Klasse: insgesamt",sprintf("%.0f",sum(x[,as.numeric(colnames(x))>0],na.rm=TRUE)/3600),"h bezogen auf",sprintf("%.0f",sum(mile)),"km"),
                subsubtitle="* Betrieb: Motordrehzahl > 0 U/min",
                save=save)  
  }  
  # 2.29.4 Motordrehzahl-Elektromaschinenleistung-Kennfeld (Für ganze reihe)
  # }else if(plotId==130){
  #   # 4.1 3S-Start-SummenhÖufigkeiten
  #   # Lift Charts
  # 
  # }else if(plotId==131){
  #   # 4.2 Leerlauf-SummenhÖufigkeiten
  #   # Lift Charts
  #   
  # }else if(plotId==132){
  #   # 4.3 Hochleistungsbetrieb-SummenhÖufigkeiten
  #   # Lift Charts
  #   
  # }else if(plotId==133){
  #   # 4.4 Kraftstoffverbrauch-SummenhÖufigkeiten
  #   # Lift Charts
  #   
}

createGenDatPng <- function(dir,pdflatex){
  library("stargazer")
  genDat <- read_excel(paste0(dir,"/genDat.xlsx"),sheet=1,col_names=FALSE)
  genDatStar <- stargazer(genDat,summary=FALSE,colnames=FALSE,rownames=FALSE,digit.separator="")
  genDat <- na.omit(genDat)
  genDat <- genDat[1:14,1:2]
  ##added on 28-8-2017
  engType <<- substr(genDat[7,2],1,1)
  engSeries <<- genDat[7,2]
  
  stopReas <- strsplit(genDatStar[12]," ")
  stopReas <- stopReas[[1]][3:(length(stopReas[[1]])-1)]
  ncharStopReas <- sapply(stopReas,nchar)
  newStopReas <- NULL
  idx <- max(which(cumsum(ncharStopReas)<40))
  newStopReas <- c(newStopReas,paste(c(strsplit(genDatStar[12]," ")[[1]][1:2],stopReas[1:idx],"\\\\"),collapse=" "))
  while(idx<length(stopReas)){
    stopReas <- stopReas[(idx+1):length(stopReas)]
    ncharStopReas <- sapply(stopReas,nchar)
    idx <- max(which(cumsum(ncharStopReas)<40))
    newStopReas <- c(newStopReas,paste(c(strsplit(genDatStar[12]," ")[[1]][2],stopReas[1:idx],"\\\\"),collapse=" "))
  }
  
  genDatHeader <- c("\\documentclass[preview]{standalone}","\\usepackage{caption}","\\usepackage[ansinew]{inputenc}","\\begin{document}","\\begin{table}[!htbp] \\centering","\\caption*{\\textbf{Allgemeine Daten (Quelle FINAS)}}","\\begin{tabular}{rl}","\\\\[-5ex]\\hline\\hline\\\\[-1.8ex]")
  genDatTex <- c(genDatHeader,genDatStar[10:11],newStopReas,genDatStar[13:length(genDatStar)],"\\end{document}")
  
  write.table(genDatTex,paste0(dir,"genDat.tex"),row.names=FALSE,col.names=FALSE,quote=FALSE)
  shell(paste0(pdflatex," ",dir,"genDat.tex"))
  shell(paste0("C:/EngineDL/Softwares/IM/convert -density 300 -alpha remove -background #478DBA50 ",workDir,"genDat.pdf ",dir,"genDat.png"))
}

createUi <- function(){
  library("shiny")
  library("jsonlite")
  library("shinydashboard")
  library("shinyBS")
  mainConfig <- as.data.frame(mainConfig)
  ui <- dashboardPage(
    dashboardHeader(title="Standardauswertung"),
    dashboardSidebar(
      sidebarMenu(
        radioButtons(inputId="fzg",label="Nummer des Fahrzeugdauerlaufs",choices=list.files(projDir)[list.files(projDir)!="~$FinalConfig_Draft.xlsx"]),
        radioButtons(inputId = "dqCheck",label = "Perform Data Quality",choices = c("yes","no"),selected = c("no")),
        actionButton(inputId="calcButton",label="Starte Berechnung"),
        menuItem(text="Allgemeine Daten",tabName="genData",icon=icon("list",lib="glyphicon")),
        menuItem(text="Auswertungen",tabName="analyses",icon=icon("bar-chart",lib="font-awesome")),
        menuItem(text="Belastungskollektive",tabName="blk",icon=icon("stats",lib="glyphicon")),
        menuItem(text="Fehlende Abschnitte",tabName="missData",icon=icon("eye-close",lib="glyphicon")),
        downloadButton(outputId="reportButton",label="Erstelle Bericht"),
        downloadButton(outputId="excelButton",label="Create Excel")
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(
          tabName="genData",
          fluidRow(
            box(
              title="Allgemeine Daten zum Dauerlauf",
              solidHeader=TRUE,
              status="primary",
              imageOutput("imgGenData"),
              width=6,
              height=460
            )
          )
        ),
        tabItem(
          tabName="missData",
          fluidRow(
            box(
              title="Fehlende Abschnitte",
              solidHeader=TRUE,
              status="warning",
              imageOutput("imgMissData"),
              width=9,
              height=610
            )
          )
        ),
        tabItem(
          tabName="analyses",
          fluidRow(
            box(
              title="Art des Startergenerators",
              radioButtons(inputId="rsgIsg",label="Riemengetriebener oder integrierter Startergenerator",choices=list("RSG","ISG"),inline=TRUE),
              width=8
            )
          ),
          lapply(which(substr(mainConfig[,1],1,1)=="2"),function(idx){
            sec <- ifelse(!is.na(mainConfig[idx,3]),paste(mainConfig[idx,2],"/",mainConfig[idx,3]),mainConfig[idx,2])
            if(!is.na(mainConfig[idx,4])){
              if(is.na(mainConfig[idx,17])){
                fluidRow(box(title=sec,solidHeader=TRUE,status="primary",plotOutput(mainConfig[idx,4]),width=8,height=550))
              }else{
                if(is.na(mainConfig[idx,20])){
                  fluidRow(box(title=sec,solidHeader=TRUE,status="primary",plotOutput(mainConfig[idx,4]),width=8,height=550),
                           box(title="Klassendefinition",
                               solidHeader=TRUE,
                               status="primary",
                               uiOutput(paste0("uiLowX",idx)),
                               uiOutput(paste0("uiUpX",idx)),
                               uiOutput(paste0("uiBinWidthX",idx)),
                               width=4))
                }else{
                  fluidRow(box(title=sec,solidHeader=TRUE,status="primary",plotOutput(mainConfig[idx,4]),width=8,height=550),
                           box(title="Klassendefinition",
                               solidHeader=TRUE,
                               status="primary",
                               uiOutput(paste0("uiLowX",idx)),
                               uiOutput(paste0("uiUpX",idx)),
                               uiOutput(paste0("uiBinWidthX",idx)),
                               uiOutput(paste0("uiLowY",idx)),
                               uiOutput(paste0("uiUpY",idx)),
                               uiOutput(paste0("uiBinWidthY",idx)),
                               width=4))
                }
              }
            }else{
              fluidRow(box(title=sec,solidHeader=TRUE,status="warning",width=8,collapsible=TRUE,collapsed=TRUE))
            }
          })
        ),
        tabItem(
          tabName="blk",
          lapply(which(substr(mainConfig[,1],1,1)=="3"),function(idx){
            sec <- ifelse(!is.na(mainConfig[idx,3]),paste(mainConfig[idx,2],"/",mainConfig[idx,3]),mainConfig[idx,2])
            if(!is.na(mainConfig[idx,4])){
              fluidRow(box(title=sec,solidHeader=TRUE,status="primary",plotOutput(mainConfig[idx,4]),width=8,height=550))
            }else{
              fluidRow(box(title=sec,solidHeader=TRUE,status="warning",width=8,collapsible=TRUE,collapsed=TRUE))
            }
          })
        )
      )
    )
  )
  return(ui)
}
#targTrackDat<-NULL
server <- function(input,output){
  observeEvent(input$calcButton,{
    if(input$fzg %in% list.files(projDir)){
      targTrackDat <<- read_excel(paste0(projDir,input$fzg,"/genDat.xlsx"),sheet=2)
      ###added on 13-06-2017.
      #targTrackDat <<- read_excel(paste0(projDir,"/genDat.xlsx"),sheet=2)
      targTrackDat<<-data.frame(targTrackDat[,1:2])
      targTrackDat<<-data.frame(targTrackDat[,1:2])
      genDat <- read_excel(paste0(projDir,input$fzg,"/genDat.xlsx"),sheet=1,col_names=FALSE)
      genDat <- na.omit(genDat)
      engType <<- substr(genDat[7,2],1,1)
      engSeries <<- genDat[7,2]
      ###added on 23-01-2017. 
      targTrackDat<-na.omit(targTrackDat)
      #calcCounts(dir=paste0(projDir,input$fzg,"/"),mainConfig=mainConfig,targTrackDat=targTrackDat)
      #CalcCount(dir=paste0(projDir,input$fzg,"/fullData/"),mainConfig,channSigConfig,conditionalConfig,pwrTrqConfig,trackConfig)
      perfmDQ = as.character(input$dqCheck)
      print(perfmDQ)
      if(perfmDQ == "yes"){
        print("The analysis will perform Data Quality Check.")
      }else{
        print("The analysis will not perform Data Quality Check.")
      }
      CalcCount(dir=paste0(projDir,input$fzg,"/"),mainConfig,channSigConfig,conditionalConfig,pwrTrqConfig,trackConfig,perfmDQ)
      #setwd(workDir)
      #calcGrad(dir=paste0(projDir,input$fzg,"/"),fzg=input$fzg)
      #calc10HzCounts(dir=paste0(projDir,input$fzg,"/"),fzg=input$fzg)
      #getMissogram(dir=paste0(projDir,input$fzg,"/"),save=TRUE)
      #getMissogramCombo(dir=paste0(projDir,input$fzg,"/"),save=TRUE)
      getDataQuality(dir=paste0(projDir,input$fzg,"/"),pdflatex=pdflatex)
      #getLineData(dir=paste0(projDir,input$fzg,"/"),save=TRUE)
      getFailSlides(dir=paste0(projDir,input$fzg,"/"),mainConfig=mainConfig,rsgIsg=input$rsgIsg,pdflatex=pdflatex)
      
    }
  })
  
  output$imgGenData <- renderImage({
    if(input$fzg %in% list.files(projDir)){
      dir <- paste0(projDir,input$fzg,"\\")
      createGenDatPng(dir=dir,pdflatex=pdflatex)
      list(src=paste0(dir,"genDat.png"),height=400,width=600)
    }else{
      list(src="",height=400,width=600)
    }
  },deleteFile=FALSE)
  
  observeEvent(input$rsgIsg,{
    getFailSlides(dir=paste0(projDir,input$fzg,"/"),mainConfig=mainConfig,rsgIsg=input$rsgIsg,pdflatex=pdflatex)
    saveRDS(object=input$rsgIsg,file=paste0(projDir,input$fzg,"/Daten/rsgIsg.rds"))
  })
  # 
  output$imgMissData <- renderImage({
    if(input$fzg %in% list.files(projDir)){
      dir <- paste0(projDir,input$fzg,"\\")
      getFailSlides(dir=dir,mainConfig=mainConfig,rsgIsg=input$rsgIsg,pdflatex=pdflatex)
      list(src=paste0(dir,"failSlides.png"),height=550,width=925)
    }else{
      list(src="",height=550,width=925)
    }
  },deleteFile=FALSE)
  # 
  #print("Error starts here")
  lapply(which(!is.na(mainConfig[,4])),function(idx){
    if(!is.na(mainConfig[idx,17])){
      #print("It comes here")
      ax <- ifelse(!is.na(mainConfig[idx,17])," x-Achse","")
      output[[paste0("uiLowX",idx)]] <- renderUI({
        if(as.character(input$fzg) %in% list.files(projDir)){
          breaksDef <- readRDS(paste0(projDir,input$fzg,"/Daten/breaksDef.rds"))
          numericInput(inputId=paste0("lowX",idx),label=paste0("Untere Klassengrenze",ax),value=breaksDef[idx,1])
        }
      })
      output[[paste0("uiUpX",idx)]] <- renderUI({
        if(as.character(input$fzg) %in% list.files(projDir)){
          breaksDef <- readRDS(paste0(projDir,input$fzg,"/Daten/breaksDef.rds"))
          numericInput(inputId=paste0("upX",idx),label=paste0("Obere Klassengrenze",ax),value=breaksDef[idx,2])
        }
      })
      output[[paste0("uiBinWidthX",idx)]] <- renderUI({
        if(as.character(input$fzg) %in% list.files(projDir)){
          breaksDef <- readRDS(paste0(projDir,input$fzg,"/Daten/breaksDef.rds"))
          numericInput(inputId=paste0("binWidthX",idx),label=paste0("Klassenbreite",ax),value=breaksDef[idx,3])
        }
      })
    }
    if(!is.na(mainConfig[idx,20])){
      output[[paste0("uiLowY",idx)]] <- renderUI({
        if(as.character(input$fzg) %in% list.files(projDir)){
          breaksDef <- readRDS(paste0(projDir,input$fzg,"/Daten/breaksDef.rds"))
          numericInput(inputId=paste0("lowY",idx),label="Untere Klassengrenze y-Achse",value=breaksDef[idx,4])
        }
      })
      output[[paste0("uiUpY",idx)]] <- renderUI({
        if(as.character(input$fzg) %in% list.files(projDir)){
          breaksDef <- readRDS(paste0(projDir,input$fzg,"/Daten/breaksDef.rds"))
          numericInput(inputId=paste0("upY",idx),label="Obere Klassengrenze y-Achse",value=breaksDef[idx,5])
        }
      })
      output[[paste0("uiBinWidthY",idx)]] <- renderUI({
        if(as.character(input$fzg) %in% list.files(projDir)){
          breaksDef <- readRDS(paste0(projDir,input$fzg,"/Daten/breaksDef.rds"))
          numericInput(inputId=paste0("binWidthY",idx),label="Klassenbreite y-Achse",value=breaksDef[idx,6])
        }
      })
    }
  })
  
  lapply(which(!is.na(mainConfig[,4])),function(idx){
    output[[mainConfig[idx,4]]] <- renderPlot({
      if(input$fzg %in% list.files(projDir)){
        input$rsgIsg
        print("Render 1")
        print(mainConfig[idx,4])
        isGoodSlide <- readRDS(file=paste0(projDir,input$fzg,"/Daten/isGoodSlide.rds"))
        #goodSlide <- paste0(mainConfig[,6],".rds") %in% list.files(paste0(dir,"Daten/"))
        
        if(isGoodSlide[idx]){
          breaksDef <- readRDS(file=paste0(projDir,input$fzg,"/Daten/breaksDef.rds"))
          if(!is.na(mainConfig[idx,17]) && substr(mainConfig[idx,1],1,1)=="2"){
            breaksDef[idx,1] <- input[[paste0("lowX",idx)]]
            breaksDef[idx,2] <- input[[paste0("upX",idx)]]
            breaksDef[idx,3] <- input[[paste0("binWidthX",idx)]]
          }
          if(!is.na(mainConfig[idx,20]) && substr(mainConfig[idx,1],1,1)=="2"){
            breaksDef[idx,4] <- input[[paste0("lowY",idx)]]
            breaksDef[idx,5] <- input[[paste0("upY",idx)]]
            breaksDef[idx,6] <- input[[paste0("binWidthY",idx)]]
          }
          saveRDS(object=breaksDef,file=paste0(projDir,input$fzg,"/Daten/breaksDef.rds"))
          #plotSuperFun(idx,dir=paste0(projDir,input$fzg,"/"),mainConfig=cbind(mainConfig[,1:8],breaksDef),rsgIsg=input$rsgIsg,save=FALSE)
          print("render 2")
          plotSuperFun(idx,dir=paste0(projDir,input$fzg,"/"),mainConfig=cbind(mainConfig,breaksDef),rsgIsg=input$rsgIsg,save=FALSE)
          
        }else{
          plot(x=0,y=0,xaxt="n",yaxt="n",xlab="",ylab="",col="white")
          text(0,0,"Keine Daten vorhanden",col="red",cex=2)
        }
      }
    },
    height=490,
    width=815)
  })
  
  output$reportButton <- downloadHandler(
    filename=function(){
      paste0("Standardauswertung_FzgDL_",input$fzg,".pptx")
    },
    content=function(file){
      isGoodSlide <- readRDS(file=paste0(projDir,input$fzg,"/Daten/isGoodSlide.rds"))
      #goodSlide <- paste0(mainConfig[,6],".rds") %in% list.files(paste0(dir,"Daten/"))
      # if(engType == "O"){
      #   # varsX <- varsX[varsX != "fuellowPressCounts"]
      #   # varsX <- varsX[varsX != "fuellowPressAnzCounts"]
      #   # varsX <- varsX[varsX != "fuellowPressTempCounts"]
      #   goodSlide[63]<-FALSE
      #   goodSlide[64]<-FALSE
      #   goodSlide[65]<-FALSE
      # }
      # else if (engType == "M"){
      #   # varsX <- varsX[varsX != "dpfRegCounts"]
      #   # varsX <- varsX[varsX != "StellerpositionNDAGRCounts"]
      #   # varsX <- varsX[varsX != "StellerpositionHDAGRCounts"]
      #   # varsX <- varsX[varsX != "egrNumVal"]
      #   goodSlide[22]<-FALSE
      #   goodSlide[72]<-FALSE
      #   goodSlide[73]<-FALSE
      #   isGoodSlide[74]<-FALSE
      # }
      
      for(idx in which(isGoodSlide)){
        breaksDef <- readRDS(file=paste0(projDir,input$fzg,"/Daten/breaksDef.rds"))
        print(idx)
        #plotSuperFun(idx,dir=paste0(projDir,input$fzg,"/"),mainConfig=cbind(mainConfig[,1:8],breaksDef),rsgIsg=input$rsgIsg,save=TRUE)
        plotSuperFun(idx,dir=paste0(projDir,input$fzg,"/"),mainConfig=cbind(mainConfig,breaksDef),rsgIsg=input$rsgIsg,save=TRUE)
      }
      motornummerdf<-read_excel(paste0(projDir,input$fzg,"/genDat.xlsx"),sheet=1)
      motornummer<<-as.character(motornummerdf[7,2])
      report <- createReport(dir=paste0(projDir,input$fzg,"/"),mainConfig=mainConfig,template=ppTemplate,fzg=input$fzg,motornummer = motornummer,rsgIsg=input$rsgIsg,pdflatex=pdflatex)
      writeDoc(report,file)
    }
  )
  
  output$excelButton<-downloadHandler(
    filename = function(){
      paste0("Standardauswertung_FzgDL_",input$fzg,".xlsx")
    },
    content = function(file){
      ##added on 23-08-2017 for changes 
      breaksDef <- readRDS(file=paste0(projDir,input$fzg,"/Daten/breaksDef.rds"))
      xlsxWB<-createExcelExport(cbind(mainConfig[,1:8],breaksDef,mainConfig[,25:32],mainConfig[,15:16]),paste0(projDir,input$fzg,"/Daten/"),paste0(input$fzg))
      saveWorkbook(xlsxWB,file)
      
    }
  )
}

### start app
library("readxl")
library("shiny")
#mainConfig <- read_excel(paste0(projDir,"config.xlsx"),sheet=1)
# mainConfig <- data.frame(read_excel(paste0(projDir,"config.xlsx"),sheet=1))
# #mainConfig <- data.frame(unlist(mainConfig))
# pwrTrqConfig <- data.frame(read_excel(paste0(projDir,"config.xlsx"),sheet=2))
mainConfig <-
  data.frame(read_excel(paste0(projDir,"FinalConfig_Draft.xlsx"), sheet = 1))
channSigConfig <-
  data.frame(read_excel(paste0(projDir,"FinalConfig_Draft.xlsx"), sheet = 2))
conditionalConfig <-
  data.frame(read_excel(paste0(projDir,"FinalConfig_Draft.xlsx"), sheet = 3))
pwrTrqConfig <-
  data.frame(read_excel(paste0(projDir,"FinalConfig_Draft.xlsx"), sheet = 4))
trackConfig <-
  data.frame(read_excel(paste0(projDir,"FinalConfig_Draft.xlsx"), sheet = 5))
runApp(appDir=shinyApp(ui=createUi(),server=server)) #host="0.0.0.0",port=5050



### do report without shiny apps
#
# ### choose vehicle ID
#
# fzg <- "213-877"
#  rsgIsg <- "RSG" # ('RSG' or 'ISG')
#
#
#
# ### do calculation
#
# library("readxl")
# mainConfig <- read_excel(paste0(projDir,"config.xlsx"),sheet=1) # load configuration file
# targTrackDat <- read_excel(paste0(projDir,fzg,"/genDat.xlsx"),sheet=2) # load general data
# calcCounts(dir=paste0(projDir,fzg,"/"),mainConfig=mainConfig,targTrackDat=targTrackDat) # do main calculation
#calcGrad(dir=paste0(projDir,fzg,"/"),fzg=fzg) # do engine speed starting gradient calculation#
# getFailSlides(dir=paste0(projDir,fzg,"/"),mainConfig=mainConfig,rsgIsg=rsgIsg,pdflatex=pdflatex) # create table with missing plots
#
#
#
# ### do the plots
#

#
#
#
# ### create and save report
#
# report <- createReport(dir=paste0(projDir,fzg,"/"),mainConfig=mainConfig,template=ppTemplate,fzg=fzg,rsgIsg=rsgIsg,pdflatex=pdflatex)
# writeDoc(report,file=paste0(projDir,fzg,"/Standardauswertung_FzgDL_",fzg,".pptx"))

  