## ---------------------------------------------------------------
## CLIMATE CHANGE IMPACTS ON BUMBLEBEES CONVERGE ACROSS CONTINENTS
## ---------------------------------------------------------------
## R 3.1.0 script for reproducing analyses and figures
## ---------------------------------------------------------------
##
## Kerr JT, Pindar A, Galpern P, Packer L, Roberts SM, Rasmont P,
## Schweiger O, Colla SR, Richardson LL, Wagner DL, Gall LF,
## Sikes DS, Pantoja A. 2015. Climate change impacts on bumblebees
## converge across continents. Science.
##
## Questions regarding the paper should be addressed to the
## corresponding author, Jeremy Kerr, University of Ottawa, Ontario,
## Canada (jkerr@uottawa.ca).  Questions pertaining specifically to
## this code should be addressed to Paul Galpern, University of Calgary,
## Alberta, Canada (pgalpern@ucalgary.ca).
##
##
## ---------------------------------------------------------------
## INSTRUCTIONS FOR RUNNING THIS SCRIPT
## ---------------------------------------------------------------
##
## Please ensure that these files, including this script
## are in the working directory in R. To determine the working
## directory use getwd(), and change using setwd():
##
## 1. data_bombclimate.csv,
## 2. species_landuse.csv,
## 3. speciesUSA_pesticides.csv,
## 4. cameronTree_final.tre
## 5. analysisScript_KerrPindarGalpern_Science_2015.R
##
## This code may then be run in R as follows without modification:
## source("analysisScript_KerrPindarGalpern_Science_2015.R")
##
##
## ---------------------------------------------------------------
## USER ADJUSTABLE PARAMETERS
## ---------------------------------------------------------------
##
## PATH TO RAW DATA
## If files are in the R working directory
## there is no need to change this parameter.
rawDataPath <- "./"
##
## EXTREME OBSERVATIONS
## Number of extreme latitudinal or thermal observations
## to use.  (5 in the main paper; 10 and 20 in supplementary)
numExtremeObs <- 5
##
## REMOVE SPECIES
## For exploratory purposes, it may be interesting to remove
## one or several species from the analysis.  To do this
## give any number of species names separated by pipes, as in
## a regular expression:  e.g. "affinis|occidentalis".
## Set to FALSE to include all available species (as in the paper).
## Some sample exploratory analyses:
## 1.  Species with outlier extremes in the most recent time period
## removeSpecies <- "sporadicus|borealis|pomorum|norvegicus|frigidus|bohemicus|cryptarum|affinis|flavifrons"
## 2.  Species that have southern limits in Mexico
# removeSpecies <- "pensylvanicus|huntii|fervidus|nevadensis|rufocinctus"
## 3.  Species that have experienced declines possibly unrelated to climate change
## removeSpecies <- "affinis|occidentalis"
removeSpecies <- FALSE
##
## PLOTTING COLOURS
## Plotting colours for North America (NAM) and Europe (EUR)
## respectively (used for species-mean points, regression lines,
## and confidence interval bands)
plotCol <- c("#d01c8b", "#4dac26")
ciCol <- c("#f1b6da40", "#b8e18640")
##
## FILE FORMAT FOR FIGURE PLOTS
## Save plots as type: "PDF", "JPEG" 
## (if "", plots appear in an R graphics window)
plotType <- "JPEG"
##
##
## ---------------------------------------------------------------
## SCRIPT TO REPRODUCE FIGURES, TABLES AND SUPPORTING ANALYSES
## ---------------------------------------------------------------
##
## Note that each step presumes that the objects created
## in the previous steps are available in the R workspace
## Turning off early steps (i.e. doStep1 <- FALSE)
## in particular may produce errors.
##
## ---------------------------------------------------------------
## LOAD R PACKAGES AND INSTALL IF NECESSARY
## ---------------------------------------------------------------
## These are required for PGLS analyses
if (!(suppressWarnings(require("ape")))) {
    install.packages("ape")
    require(ape)
}
if (!(suppressWarnings(require("nlme")))) {
    install.packages("nlme")
    require(nlme)
}
## 
## ---------------------------------------------------------------
## STEP ONE
## ---------------------------------------------------------------
## Determine species-period means for latitudinal, thermal
## and elevational observations. 
doStep1 <- TRUE
if (doStep1) {
    
    
    ## Load and prepare the raw data
    if (!exists("rawData")) rawData <- read.csv(paste0(rawDataPath, "data_bombclimate.csv"), stringsAsFactors=FALSE)
    speciesTable <- unique(rawData[, c("species", "continent")])
    samplingPeriods <- unique(rawData[, c("periodFrom", "periodTo")])
    
    
    rawRangeShift <- list()
    speciesKmNorthEquator <- list()
    speciesElevation <- list()
    cnt <- 0
    
    ## Determine means for each species
    for (iSpecies in 1:nrow(speciesTable)) {

        cat(speciesTable[iSpecies, "species"], "\n")
        
        ## Extract data from raw dataset for this species in all time periods
        thisSpecies <- rawData[rawData$species==speciesTable[iSpecies, "species"] &
                                   rawData$continent==speciesTable[iSpecies, "continent"], ]
        
        ## Find the species mean latitude and elevation (over all time periods)
        speciesKmNorthEquator[[iSpecies]] <- mean(thisSpecies$kmNorthEquator)
        speciesElevation[[iSpecies]] <- mean(thisSpecies$elevation)
        
        ## Determine means for each species in each time period
        for (iPeriod in 1:nrow(samplingPeriods)) {

            ## Extract data from raw dataset for this species in this time period
            thisSpeciesPeriod <- rawData[rawData$species==speciesTable[iSpecies, "species"] &
                                   rawData$continent==speciesTable[iSpecies, "continent"] &
                                   rawData$periodFrom==samplingPeriods[iPeriod, "periodFrom"] &
                                   rawData$periodTo==samplingPeriods[iPeriod, "periodTo"], ]
            
            thisSpeciesPeriodN <- nrow(thisSpeciesPeriod)
            
            ## Find the unique locations for this species in this period
            ## using the unique identifier provided for this purpose
            uniqueLoc <- !duplicated(thisSpeciesPeriod$spPerLoc)
            
            ## Determine latitudinal poleward ("Cold") and equatorward ("Hot") means
            thisLat <- thisSpeciesPeriod$kmNorthEquator[uniqueLoc][order(thisSpeciesPeriod$kmNorthEquator[uniqueLoc], decreasing=TRUE)]
            thisLatCold <- mean(thisLat[1:numExtremeObs], na.rm=TRUE)
            thisLatHot <- mean(rev(thisLat)[1:numExtremeObs], na.rm=TRUE)
            
            ## Determine thermal poleward ("Cold") and equatorward ("Hot") means
            thisThermalMax <- thisSpeciesPeriod$maxPeriodAnnualMeanT[uniqueLoc]
            thisThermalMax <- thisThermalMax[order(thisThermalMax, decreasing=TRUE)]
            thisThermalMin <- thisSpeciesPeriod$minPeriodAnnualMeanT[uniqueLoc]
            thisThermalMin <- thisThermalMin[order(thisThermalMin)]
            thisThermalCold <- mean(thisThermalMin[1:numExtremeObs], na.rm=TRUE)
            thisThermalHot <- mean(thisThermalMax[1:numExtremeObs], na.rm=TRUE)
            
            ## Determine overall elevational mean
            thisMeanElevation <- mean(thisSpeciesPeriod$elevation, na.rm=TRUE)
            
            ## Determine approximate latitudinal midpoint of the species-period spatial distribution
            thisMeanKmNorthEquator <- mean(thisSpeciesPeriod$kmNorthEquator[uniqueLoc], na.rm=TRUE)
            
            ## Retain just the summary data
            cnt <- cnt + 1
            rawRangeShift[[cnt]] <-  data.frame(species=speciesTable[iSpecies, "species"],
                                        continent=speciesTable[iSpecies, "continent"],
                                        periodYears=paste(samplingPeriods[iPeriod, c("periodFrom", "periodTo")], collapse="to"),
                                        period=paste0("t", iPeriod),
                                        latitudeHot=thisLatHot, latitudeCold=thisLatCold,
                                        thermalHot=thisThermalHot, thermalCold=thisThermalCold,
                                        elevation=thisMeanElevation,
                                        latitude=thisMeanKmNorthEquator,
                                        N=thisSpeciesPeriodN)
            
        }
        
    }
    
    rawRangeShift <- do.call(rbind, rawRangeShift)
    speciesKmNorthEquator <- do.call(rbind, speciesKmNorthEquator)
    speciesElevation <- do.call(rbind, speciesElevation)
    
    
    
    ## Reshape species-period mean data table to simplify specification of models
    rangeShift <- reshape(rawRangeShift, idvar=c("species", "continent"), timevar="period", direction="wide", sep="_")
    
    ## Find species-period deltas (i.e. change from t2 to t1; t3 to t1; and t4 to t1)
    ## and add to table
    vars <- apply(expand.grid(c("latitudeHot", "latitudeCold", "thermalHot", "thermalCold", "elevation", "N"), c("t2", "t3", "t4")), 1, function(x) paste(x, collapse="_"))
    vars <- data.frame(tX=vars, t1=gsub("t2|t3|t4", "t1", vars), delta=paste0("delta_", vars), stringsAsFactors=FALSE)

    delta <- lapply(1:nrow(vars), function(x) {
                      y <- data.frame(rangeShift[, vars[x, "tX"]] - rangeShift[, vars[x, "t1"]])
                      names(y) <- vars[x, "delta"]
                      return(y)
    })
    rangeShift <- data.frame(rangeShift, do.call(cbind, delta))
 
        
    ## Add species mean latitude and elevation to table
    rangeShift <- data.frame(rangeShift, meanLatitude=speciesKmNorthEquator, meanElevation=speciesElevation)
    
    ## Add species mean pesticide exposure data in the US (for North American species only)
    pesticides <- read.csv(paste0(rawDataPath, "speciesUSA_pesticides.csv"))
    speciesIndex <- match(pesticides$species, rangeShift$species)

    rangeShift$usaTotalPest_t3 <- NA
    rangeShift$usaTotalPest_t3[speciesIndex] <- pesticides$totalPest_t3
    rangeShift$usaTotalPest_t4 <- NA
    rangeShift$usaTotalPest_t4[speciesIndex] <- pesticides$totalPest_t4
    rangeShift$usaNeonics_t3 <- NA
    rangeShift$usaNeonics_t3[speciesIndex] <- pesticides$neonics_t3
    rangeShift$usaNeonics_t4 <- NA
    rangeShift$usaNeonics_t4[speciesIndex] <- pesticides$neonics_t4
    
    ## Add species mean landuse change exposure for North American and European species
    landuse <- read.csv(paste0(rawDataPath, "species_landuse.csv"))
    rangeShift <- data.frame(rangeShift, landuse[, -1])
    
    ## If removeSpecies is set, remove those species from the dataset that match the
    ## regular expression given above
    if (!is.logical(removeSpecies)) {
        cat("Removing:", gsub("\\|", ", ", removeSpecies), "\n")
        rangeShift <- rangeShift[!grepl(removeSpecies, rangeShift$species), ]
    }
    
}

## ---------------------------------------------------------------
## STEP TWO
## ---------------------------------------------------------------
## Run ordinary least-squares (OLS) linear regressions
## for the delta latitudinal and thermal
## variables.  Also run phylogenetic generalized
## least-squares (PGLS) regressions on the
## selected OLS models (i.e. with lowest AIC)
doStep2 <- TRUE
if (doStep2) {
    ## VARIABLES    
    ## A series of OLS linear regression models are run using
    ## the following combination of dependent and independent variables:
    ## y: (delta_latitude, delta_thermal) x (hot, cold) x (t2, t3, t4) 
    ## x: (latitude, thermal) x (hot, cold) x (t1)
    ##    continent
    ##    species period sample sizes 
    ##    species period landuse changes 
    ##    species period pesticide exposures [NAM only, t3 and t4 only] 
    
    ## For each y combination, four related "sub-models" are computed are as follows:
    ## 
    ## (1) y ~ x
    ## (2) y ~ x + continent + continent:x
    ## (3) y ~ x + x2 + x3 + x4 + x5 + x6 + x7
    ## (4) y ~ x + continent*(x2 + x3 + x4 + x5 + x6 + x7)
    ##
    ## Note:  Only sub-models (1) and (3) are computed when pesticides are analyzed

    ## Create a master table for the models to be run
    models <- data.frame(
                y=apply(expand.grid(c("delta_latitude", "delta_thermal"), c("Hot", "Cold"), c("_t2", "_t3", "_t4")), 1, function(x) paste(x, collapse="")),
                x=apply(expand.grid(c("latitude", "thermal"), c("Hot", "Cold"), c("_t1")), 1, function(x) paste(x, collapse="")),
                continent="continent",
                namOnly=FALSE,
                x2="N_t1",
                x2=rep(c("N_t2", "N_t3", "N_t4"), each=4),
                x4=rep(c("delta_crop_t2", "delta_crop_t3", "delta_crop_t4"), each=4),
                x5=rep(c("delta_pasture_t2", "delta_pasture_t3", "delta_pasture_t4"), each=4),
                x6=NA,
                x7=NA,
                stringsAsFactors=FALSE)

    models <- models[order(models[,1]), ]
    modelsNAMOnly <- models[grep("latitudeHot_t3|latitudeHot_t4|thermalHot_t3|thermalHot_t4", models[,1]), ]
    modelsNAMOnly$x6 <- rep(c("usaNeonics_t3", "usaNeonics_t4"), each=2)
    modelsNAMOnly$x7 <- rep(c("usaTotalPest_t3", "usaTotalPest_t4"), each=2)
    modelsNAMOnly$namOnly <- TRUE
    models <- rbind(models, modelsNAMOnly)
    
    ## Prepare for PGLS analyses
    ## Load and prune the Cameron et al. tree to represent 65 of the 67
    ## species we use in the analysis (we lack phylogenetic data for 2 species)
    fullTree <- read.tree(paste0(rawDataPath, "cameronTree_final.tre"))    
    useTree <- drop.tip(fullTree, tip=fullTree$tip.label[!(fullTree$tip.label %in% rangeShift$species)])
        
    ## Remove B. bohemicus data for North America.  This species has been recorded on both continents,
    ## however European sample size is much larger. Also remove B. magnus as we do not have
    ## phylogenetic information for this species.
    pglsRangeShift <- rangeShift[!(rangeShift$species=="bohemicus" & rangeShift$continent=="NAM"), ]
    pglsRangeShift <- pglsRangeShift[pglsRangeShift$species!="magnus", ]
    rownames(pglsRangeShift) <- pglsRangeShift$species
    
    lmResults <- vector("list", nrow(models))
    pglsResults <- vector("list", nrow(models))
    
    ## Run all four sub-models unless it is a North America only (namOnly) model
    for (iModel in 1:nrow(models)) {
        lmResults[[iModel]] <- list()
        lmResults[[iModel]][[1]] <- list()
        lmResults[[iModel]][[2]] <- list()
        lmResults[[iModel]][[3]] <- list()
        lmResults[[iModel]][[4]] <- list()
        
        ## Not a North American only (namOnly) model
        if (!models[iModel, "namOnly"]) {
            
            useRangeShift <- rangeShift
            usePglsRangeShift <- pglsRangeShift
            
            
            ## MODEL (1)
            lmResults[[iModel]][[1]]$lm <- lm(as.formula(paste(models[iModel, "y"], " ~ ", models[iModel, "x"], sep="")), data=useRangeShift)
    
            ## MODEL (2)
            lmResults[[iModel]][[2]]$lm <- lm(as.formula(paste(models[iModel, "y"], " ~ ", models[iModel, "x"], "*continent", sep="")), data=useRangeShift)
            
            ## MODEL (3)
            lmResults[[iModel]][[3]]$lm <- lm(as.formula(paste(models[iModel, "y"], " ~ ", models[iModel, "x"], " + ", paste(na.omit(as.character(models[iModel, -c(1:4)])), collapse=" + "), sep="")), data=useRangeShift)
            
            ## MODEL (4)
            lmResults[[iModel]][[4]]$lm <- lm(as.formula(paste(models[iModel, "y"], " ~ continent*(", models[iModel, "x"], " + ", paste(na.omit(as.character(models[iModel, -c(1:4)])), collapse=" + "), ")", sep="")), data=useRangeShift)
    
            ## Produce AIC table
            lmResults[[iModel]]$AIC <- AIC(lmResults[[iModel]][[1]]$lm, lmResults[[iModel]][[2]]$lm, lmResults[[iModel]][[3]]$lm, lmResults[[iModel]][[4]]$lm)
            
            
        }
        else {
            useRangeShift <- rangeShift[rangeShift$continent=="NAM", ]
            usePglsRangeShift <- pglsRangeShift[pglsRangeShift$continent=="NAM", ]
            useTree <- drop.tip(useTree, tip=useTree$tip.label[!(useTree$tip.label %in% usePglsRangeShift$species)])
            
            ## MODEL (1)
            lmResults[[iModel]][[1]]$lm <- lm(as.formula(paste(models[iModel, "y"], " ~ ", models[iModel, "x"], sep="")), data=useRangeShift)
            
            
            ## MODEL (3)
            lmResults[[iModel]][[2]]$lm <- lm(as.formula(paste(models[iModel, "y"], " ~ ", models[iModel, "x"], " + ", paste(na.omit(as.character(models[iModel, -c(1:4)])), collapse=" + "), sep="")), data=useRangeShift)
            
            ## Produce AIC table
            lmResults[[iModel]]$AIC <- AIC(lmResults[[iModel]][[1]]$lm, lmResults[[iModel]][[2]]$lm)
        }
        
        ## Identify selected model as the model with the lowest AIC,
        ## but if the next simplest model differs by 2 or less choose it
        .selectModel <- function(aic, dAICTolerance=2) {
            minAIC <- which.min(aic)
            dAIC <- abs(aic - aic[minAIC])
            dAIC <- dAIC - dAICTolerance
            if (minAIC == 4) {
                if (dAIC[3] <= 0) {
                    minAIC <- 3
                }
            }
            if (minAIC == 3) {
                if (dAIC[2] <= 0) {
                    minAIC <- 2
                }
            }
            if (minAIC == 2) {
                if (dAIC[1] <= 0) {
                    minAIC <- 1
                }
            }
            return(minAIC)
        }
        lmResults[[iModel]]$selectedModel <- .selectModel(lmResults[[iModel]]$AIC[,2])
        
        ## Use phylogenetic generalized least squares (PGLS) approach to re-examine the
        ## selected models using several models of covariance structure
        cBrownian <- corBrownian(phy=useTree)
        cOU <- corMartins(1, phy=useTree)
        cPagel <- corPagel(1, phy=useTree)

        pglsCorrModels <- c("Brownian", "OrnUhl", "Pagel", "Independent") 
        pglsResults[[iModel]] <- vector("list", length(pglsCorrModels))
        thisForm <- paste(as.character(formula(lmResults[[iModel]][[lmResults[[iModel]]$selectedModel]]$lm))[c(2,1,3)], collapse=" ")
        pglsResults[[iModel]][[1]]$pgls <- try(gls(formula(lmResults[[iModel]][[lmResults[[iModel]]$selectedModel]]$lm), usePglsRangeShift, correlation=cBrownian), silent=TRUE)
        pglsResults[[iModel]][[2]]$pgls <- try(gls(formula(lmResults[[iModel]][[lmResults[[iModel]]$selectedModel]]$lm), usePglsRangeShift, correlation=cOU), silent=TRUE)
        pglsResults[[iModel]][[3]]$pgls <- try(gls(formula(lmResults[[iModel]][[lmResults[[iModel]]$selectedModel]]$lm), usePglsRangeShift, correlation=cPagel), silent=TRUE)
        pglsResults[[iModel]][[4]]$pgls <- try(gls(formula(lmResults[[iModel]][[lmResults[[iModel]]$selectedModel]]$lm), usePglsRangeShift, correlation=NULL), silent=TRUE)

        if (class(pglsResults[[iModel]][[2]]$pgls) != "try-error") {
            alphaOrnUhl <- attr(pglsResults[[iModel]][[2]]$pgls$apVar, "Pars")["corStruct"]
            if (!is.numeric(alphaOrnUhl)) alphaOrnUhl <- NA
        }
        else {
            alphaOrnUhl <- NA
        }
        if (class(pglsResults[[iModel]][[3]]$pgls) != "try-error") {
            lambdaPagel <- attr(pglsResults[[iModel]][[3]]$pgls$apVar, "Pars")["corStruct"]
        }
        else {
            lambdaPagel <- NA
        }
               
        aic <- lapply(pglsResults[[iModel]], function(x) if (class(x$pgls) != "try-error") AIC(x$pgls) else NA)
        aic <- data.frame(t(data.frame(do.call(c, aic))))
        if (abs(aic[which.min(aic)] - aic[4]) <= 2) {
            pglsResults[[iModel]]$selectedModel <- 4

        }
        else {
            pglsResults[[iModel]]$selectedModel <- as.integer(which.min(aic))
        }
        
        aic <- data.frame(aic[1, 1:2], alphaOrnUhl, aic[1, 3], lambdaPagel, aic[1, 4], pglsCorrModels[pglsResults[[iModel]]$selectedModel], stringsAsFactors=FALSE)
        names(aic) <- c(paste("aic", pglsCorrModels[1:2], sep=""), "alphaOrnUhl", paste("aic", pglsCorrModels[3], sep=""), "lambdaPagel", paste("aic", pglsCorrModels[4], sep=""), "aicBestModel")
        rownames(aic) <- iModel
        pglsResults[[iModel]]$AIC <- aic
    }
}

## ---------------------------------------------------------------
## STEP THREE
## ---------------------------------------------------------------
## Produce latitudinal and thermal figures
doStep3 <- TRUE
if (doStep3) {
    
    
    
    tX <- lapply(2:4, function(x) {
            
            y <- list()
            y$period <- paste(samplingPeriods[x, c("periodFrom", "periodTo")], collapse="to")
            y$shift <- rangeShift[, grepl(paste(paste(rep(c("delta_thermal", "delta_latitude"), times=2), rep(c("Cold_t", "Hot_t"), each=2), x, sep=""), collapse="|"), names(rangeShift))]
            names(y$shift) <- gsub(paste0("_t", x), "", names(y$shift))
            y$shift <- cbind(y$shift, rangeShift[, paste(rep(c("thermal", "latitude"), times=2), rep(c("Cold_t1", "Hot_t1"), each=2), sep="")], continent=rangeShift[, "continent"])
            return(y)
        })
    

    for (lsType in c("ols", "pgls")) {
        for (iT in 1:length(tX)) {
            if (plotType=="PDF") {
                ## Create a PDF for Science specifications (2 columns @ 2.3 in/column)
                pdf(paste0(lsType, "_LatTherm_", numExtremeObs, "extreme_t", iT+1, ".pdf"),  width=4.6, height=4.6,  pointsize=10)
            } else if (plotType=="JPEG") {
                jpeg(paste0(lsType, "_LatTherm_", numExtremeObs, "extreme_t", iT+1, ".jpg"),  width=4.6, height=4.6,  pointsize=10, quality=100, res=600, units="in")
                
            } else {
                dev.new(width=4.6, height=4.6, units="in", pointsize=10)
            }
    
            layout(structure(c(1,2,3,4), .Dim = c(2L, 2L)))
    
            figLett <- 0
            
            for (iBound in c("Cold", "Hot")) {
              
                for (iExtreme in c("latitude", "thermal")) {           
                        
                    plotX <- tX[[iT]]$shift[, c("continent", paste(iExtreme, iBound, "_t1", sep=""))]
                    plotY <- tX[[iT]]$shift[ , c("continent", paste("delta_", iExtreme, iBound, sep=""))]
    
    
                    par(mar=c(3.5,3.5,1,0.5))
                      
                    figLett <- figLett + 1
                                                                                                     
                    plot(plotX[, 2], plotY[, 2], type="n", xlab="", ylab="", axes=FALSE)
                    
                    if (iExtreme == "thermal") {
                        
                        axis(1, seq(-80, 50, by=5))    
                        axis(1, seq(-80, 50, by=2.5), rep("", length(seq(-80, 50, by=2.5))), tcl=-0.25)
                        mtext(substitute(a * (list(b-d, phantom(i)^o*C)), list(a=paste(ifelse(iBound=="Hot", "Warm", "Cool"), " limit ", sep=""), b=paste0(samplingPeriods[1, "periodFrom"]), d=paste0(samplingPeriods[1, "periodTo"]))), side=1, line=2.25, cex=0.75) 
                        axis(2)
                        axis(2, seq(-80, 50, by=1), rep("", length(seq(-80, 50, by=1))), tcl=-0.25)
    
                        mtext(substitute(Delta~a * (list(by~b-d, phantom(i)^o*C)), list(a=paste(ifelse(iBound=="Hot", "Warm", "Cool"), " limit ", sep=""), b=paste0(samplingPeriods[iT+1, "periodFrom"]), d=paste0(samplingPeriods[iT+1, "periodTo"]))), side=2, line=2, cex=0.75)
                        mtext(LETTERS[figLett], side=1, line=-1.2, at=par("usr")[2] - (par("usr")[2]-par("usr")[1])*0.92, cex=1.5)
    
                    }
                    else {
                        if (iBound=="Cold") {
                            axis(1, seq(0, 10000, by=1000), lab=c("", "", "", "", "", "5000", "", "7000", "", "", ""))
                        }
                        else {
                            axis(1, seq(0, 10000, by=1000), lab=c("", "", "", "", "4000", "", "6000", "", "", "", ""))                            
                        }
                        axis(1, seq(0, 10000, by=500), rep("", length(seq(0, 10000, by=500))), tcl=-0.25)
                        mtext(substitute(a * (list(b-d, km)), list(a=paste(ifelse(iBound=="Hot", "Southern", "Northern"), " limit ", sep=""), b=paste0(samplingPeriods[1, "periodFrom"]), d=paste0(samplingPeriods[1, "periodTo"]))), side=1, line=2.25, cex=0.75)
                        axis(2)
                        axis(2, seq(-2000, 10000, by=100), rep("", length(seq(-2000, 10000, by=100))), tcl=-0.25)
                        mtext(substitute(Delta~a * (list(by~b-d, km)), list(a=paste(ifelse(iBound=="Hot", "Southern", "Northern"), " limit ", sep=""), b=paste0(samplingPeriods[iT+1, "periodFrom"]), d=paste0(samplingPeriods[iT+1, "periodTo"]))), side=2, line=2, cex=0.75)
                        mtext(LETTERS[figLett], side=1, line=-1.2, at=par("usr")[2] - (par("usr")[2]-par("usr")[1])*0.92, cex=1.5)
                        
                    }
                    box()
                    abline(h=0, lty="dashed")
                      
                    ## Function to plot a truncated regression line (i.e. truncated by x limits)
                    ## and truncated confidence limits. 
                    .regShort <-  function(plotX, lmResult, ci=TRUE, reg.col=NULL, reg.lwd=NULL, ci.border=NULL, ci.fill=NULL, gls=NULL) {
                        if (is.null(gls)) {
                            modelTerms <- rownames(attr(terms(lmResult), "factors"))[-1]
                        }
                        else {
                            modelTerms <- names(coef(lmResult))[-1]
                            modelTerms <- modelTerms[!grepl(":", modelTerms)]
                            modelTerms <- gsub("continentEUR", "continent", modelTerms)
                        }
                        if (any(modelTerms == "continent")) {
                            for (iContinent in c("NAM", "EUR")) {
                                chkX <- plotX[plotX$continent==iContinent, ]
                                additionalTerms <- modelTerms[!grepl(paste(c("delta_l",  "delta_t","continent", names(chkX)[2]), collapse="|"), modelTerms)]
                                newDf <- t(t(seq(min(chkX[,2]), max(chkX[,2]), by=diff(range(chkX[,2]))/20)))
                                
                                ## Find CI bands using the mean of the other variables
                                ## or alternately at 0.
                                findCIsAtMean <- FALSE
                                if ((length(additionalTerms) != 0) && findCIsAtMean) {
                                    newDf <- suppressWarnings(data.frame(newDf, t(data.frame(sapply(additionalTerms, function(x) mean(rangeShift[,x]))))))
                                }
                                else {
                                    newDf <- data.frame(newDf, matrix(0, nrow(newDf), length(additionalTerms)))
                                }
                                names(newDf) <- c(names(chkX)[2], additionalTerms)
                                newDf$continent <- iContinent
                                newDf$continent <- factor(newDf$continent, levels=c("NAM", "EUR"))
                                conf <- predict(lmResult, newdata=newDf, interval="confidence")
                                if (is.null(gls)) {
                                    if (ci) {
                                        lines(newDf[,1], conf[,2], col=substr(ci.border[ifelse(iContinent=="NAM", 2, 3)], 1, 7))
                                        lines(newDf[,1], conf[,3], col=substr(ci.border[ifelse(iContinent=="NAM", 2, 3)], 1, 7))
                                        polygon(c(newDf[,1], rev(newDf[,1])), c(conf[,2], rev(conf[,3])), border=NA, col=ci.fill[ifelse(iContinent=="NAM", 2, 3)])
                                    }
                                    lines(newDf[,1], conf[,1], col=reg.col[ifelse(iContinent=="NAM", 2, 3)], lwd=reg.lwd[ifelse(iContinent=="NAM", 2, 3)])
                                }
                                else {
                                    lines(newDf[,1], conf, col=substr(ci.border[ifelse(iContinent=="NAM", 2, 3)], 1, 7))
                                    text(par("usr")[2] - (par("usr")[2]-par("usr")[1])*0.95, par("usr")[4] - (par("usr")[4]-par("usr")[3])*0.1, paste0("corr=", toupper(gls)), cex=0.75, font=3, adj=0)                                
                                }
                                    
                            }
                        }
                        else {
                            chkX <- plotX
                            additionalTerms <- modelTerms[!grepl(paste(c("delta_l",  "delta_t","continent", names(chkX)[2]), collapse="|"), modelTerms)]
                            newDf <- t(t(seq(min(chkX[,2]), max(chkX[,2]), by=diff(range(chkX[,2]))/20)))
                            
                            ## Find CI bands using the mean of the other variables
                            ## or alternately at 0.
                            if ((length(additionalTerms) != 0) && findCIsAtMean) {
                                newDf <- suppressWarnings(data.frame(newDf, t(data.frame(sapply(additionalTerms, function(x) mean(rangeShift[,x]))))))
                            }
                            else {
                                newDf <- data.frame(newDf, matrix(0, nrow(newDf), length(additionalTerms)))
                            }
                            
                            names(newDf) <- c(names(chkX)[2], additionalTerms)
                            conf <- predict(lmResult, newdata=newDf, interval="confidence")
                            if (is.null(gls)) {
                                if (ci) {
                                    lines(newDf[,1], conf[,2], col=substr(ci.border[1], 1, 7))
                                    lines(newDf[,1], conf[,3], col=substr(ci.border[1], 1, 7))
                                    polygon(c(newDf[,1], rev(newDf[,1])), c(conf[,2], rev(conf[,3])), border=NA, col=ci.fill[1])
                                }
                                lines(newDf[,1], conf[,1], col=reg.col[1], lwd=reg.lwd[1])
    
                            }
                            else {
                                lines(newDf[,1], conf, col=reg.col[1], lwd=reg.lwd[1])
                                text(par("usr")[2] - (par("usr")[2]-par("usr")[1])*0.95, par("usr")[4] - (par("usr")[4]-par("usr")[3])*0.1, paste0("corr=", toupper(gls)), cex=0.75, font=3, adj=0)
    
                            }
                        }
                    }
                    
                    points(plotX[plotX$continent=="NAM", 2], plotY[plotY$continent=="NAM", 2], pch=20, cex=1.5, col=plotCol[1])
                    points(plotX[plotX$continent=="EUR", 2], plotY[plotY$continent=="EUR", 2], pch=20, cex=1.5, col=plotCol[2])
                    
                    ## Regression line(s)
                    ## Calling the variable iModel here is a hack because gls() does not correctly store
                    ## the call that enables prediction from the model.
                    iModel <- which(models$y==paste("delta_", tolower(iExtreme), iBound, "_t", iT+1, sep="") & models$namOnly==FALSE)
                    
                    simpleModels <- lmResults[[iModel]]$AIC$AIC[1:2]
                    bestOfSimpleModels <- which.min(simpleModels)
                    if ((bestOfSimpleModels==2) & (abs(diff(simpleModels)) <= 2)) {
                        bestOfSimpleModels <- 1
                    }
                    
                    if (lsType=="ols") {
                        .regShort(plotX, lmResults[[iModel]][[bestOfSimpleModels]]$lm, reg.col=c("black", plotCol), reg.lwd=rep(1.5, 3), ci.border=c("#a8a8a840", ciCol), ci.fill=c("#a8a8a840", ciCol))
                    }
                    else {
                        .regShort(plotX, pglsResults[[iModel]][[pglsResults[[iModel]]$selectedModel]]$pgls, reg.col=c("black", plotCol), reg.lwd=rep(1.5, 3), ci.border=c("#a8a8a840", ciCol), ci.fill=c("#a8a8a840", ciCol), gls=pglsCorrModels[pglsResults[[iModel]]$selectedModel])
                    }
                    
                    ## Legend to appear in top left panel
                    if (iExtreme=="latitude" && iBound=="Hot") {
                        legend(par("usr")[2] - (par("usr")[2]-par("usr")[1])*0.5, par("usr")[4] - (par("usr")[4]-par("usr")[3])*0.1, legend=c("North America", "Europe"), pch=20, col=plotCol, pt.cex=1.5, cex=0.75)
                    }
                }
            }
        if (plotType=="PDF" || plotType=="JPEG") dev.off()
        }
    }
}


## ---------------------------------------------------------------
## STEP FOUR
### ---------------------------------------------------------------
# Prepare tables (in CSV) summarizing the results
## from regressions on latitudinal and thermal variables
doStep4 <- TRUE
if (doStep4) {
    
    ## Function to process a list of ols regressions and optionally pgls AICs
    ## and produce a table with comparable entries.
    ## Replaces delta x (latitude, thermal) x (Hot, Cold) with "extreme"
    ## to simplify tabulation
    .tabulateResults <- function(ols, pglsAIC=NULL, useCI=FALSE, includeOlsAIC=FALSE, includeDiagnostics=FALSE) {
        allVars <- unique(do.call(c,lapply(ols, function(x) names(coef(x$ols)))))
        allVars <- data.frame(varName=allVars, label=allVars, sortOrder=0, stringsAsFactors=FALSE)
        
        .convertVarNames <- function(x) {
            x <- gsub("latitudeHot_t1|latitudeCold_t1|thermalHot_t1|thermalCold_t1", "extreme_t1", x)
            x <- gsub("continentEUR", "continent", x)
            x[grepl("continent:", x)] <- paste0(substr(x[grepl("continent:", x)], 11, nchar(x[grepl("continent:", x)])), ":continent")
            return(x)
        }
        allVars[,2] <- .convertVarNames(allVars[,2])
        allVars[allVars[,2]=="(Intercept)", 3] <- 1
        allVars[allVars[,2]=="extreme_t1", 3] <- 2
        allVars[allVars[,2]=="continent", 3] <- 3
        allVars[grepl(":", allVars[,2]), 3] <- 6        
        allVars[allVars[,2]=="extreme_t1:continent", 3] <- 4
        allVars[allVars[,3]==0, 3] <- 5

        baseTable <- data.frame(matrix(NA, length(unique(allVars[,2])), 2), stringsAsFactors=FALSE)
        rownames(baseTable) <- unique(allVars[,2])
        ciTable <- baseTable
        for (iTable in 1:length(ols)) {
            rawCoef <- coef(ols[[iTable]]$ols)
            if (useCI) {
                ## Find confidence intervals
                rawCI <- confint(ols[[iTable]]$ols)
                nextTable <- data.frame(gsub(" ", "", format(rawCoef, digits=1, nsmall=2, scientific=FALSE)), apply(gsub(" ", "", format(rawCI, digits=1, nsmall=2, scientific=FALSE)), 1, function(x) paste0("[", x[1], ", ", x[2], "]")), stringsAsFactors=FALSE)
            }
            else {
                ## Find standard errors
                if (class(ols[[iTable]]$ols) == "lm") {
                    rawCI <- summary(ols[[iTable]]$ols)$coefficients[,2]
                }
                else {
                    rawCI <- summary(ols[[iTable]]$ols)$tTable[,2]
                }
                nextTable <- data.frame(gsub(" ", "", format(rawCoef, digits=1, nsmall=2, scientific=FALSE)), gsub(" ", "", format(rawCI, digits=1, nsmall=2, scientific=FALSE)), stringsAsFactors=FALSE)
            }
            rownames(nextTable) <- .convertVarNames(rownames(nextTable))
            blankTable <- baseTable
            blankTable[rownames(nextTable), 1] <- nextTable[, 1]
            blankTable[rownames(nextTable), 2] <- nextTable[, 2]
            names(blankTable) <- paste(ols[[iTable]]$varName, c("_COEF", ifelse(useCI, "_95CI", "_SE")), sep="")
            ciTable <- cbind(ciTable, blankTable)
        }
        ciTable <- ciTable[order(allVars$sortOrder[match(rownames(ciTable), allVars$label)], allVars$label[match(rownames(ciTable), allVars$label)]), ]
        ciTable <- ciTable[, -c(1,2)]
        
        if (includeDiagnostics) {
            diagTable <- lapply(ols, function(x) {
                lmSum <- summary(x$ols)
                lmF <- lmSum$fstatistic
                P <- pf(lmF[1], lmF[2], lmF[3], lower.tail=FALSE)
                R2 <- lmSum$adj.r.squared
                return(list(R2Adj=R2, P=P))
            })
            P <- data.frame(do.call(cbind, lapply(diagTable, function(x) cbind(x$P, NA))))
            R2 <- data.frame(do.call(cbind, lapply(diagTable, function(x) cbind(x$R2Adj, NA))))
            rownames(P) <- "P"
            rownames(R2) <- "R2Adj"
            names(P) <- names(ciTable)
            names(R2) <- names(ciTable)
            R2 <- round(R2, 2)
            P <- signif(P, 2)
            ciTable <- rbind(ciTable, R2, P)
        }
        
        if (!is.null(pglsAIC)) {
            aicTable <- round(data.frame(do.call(cbind, lapply(pglsAIC, function(x) cbind(x, NA)))), 2)
            names(aicTable) <- names(ciTable)
            ciTable <- rbind(ciTable, aicTable)
        }
        
        if (includeOlsAIC) {
            olsAICTable <- round(data.frame(do.call(cbind, lapply(ols, function(x) cbind(AIC(x$ols), NA)))), 2)
            names(olsAICTable) <- names(ciTable)
            rownames(olsAICTable) <- "AIC"
            ciTable <- rbind(ciTable, olsAICTable)
        }
        
        
        return(ciTable)
        
    }
    
    ## Produce tables of NAM + EUR ols and pgls results for each time period
    for (iT in 1:length(tX)) {
        
        olsModel <- vector("list",4)
        pglsModel <- vector("list",4)
        pglsModelAIC <- vector("list",4)
        cnt <- 0
        for (iExtreme in c("latitude", "thermal")) {
            for (iBound in c("Hot", "Cold")) {
                cnt <- cnt + 1
                iModel <- which(models$y==paste("delta_", tolower(iExtreme), iBound, "_t", iT+1, sep="") & models$namOnly==FALSE)
                olsModel[[cnt]]$ols <- lmResults[[iModel]][[lmResults[[iModel]]$selectedModel]]$lm
                pglsModel[[cnt]]$ols <- pglsResults[[iModel]][[pglsResults[[iModel]]$selectedModel]]$pgls
                olsModel[[cnt]]$varName <- paste("OLS|delta_", tolower(iExtreme), iBound, "_t", iT+1, sep="")
                pglsModel[[cnt]]$varName <- paste("PGLS|delta_", tolower(iExtreme), iBound, "_t", iT+1, sep="")
                pglsModelAIC[[cnt]] <- t(pglsResults[[iModel]]$AIC[c(6,1,2,4,5)])
            }
        }
        olsTable <- .tabulateResults(olsModel)
        pglsTable <- .tabulateResults(pglsModel, pglsModelAIC)
        
        write.csv(olsTable, file=paste0("ols_LatTherm_", numExtremeObs, "extreme_t", iT+1, ".csv"), row.names=TRUE, na="")
        write.csv(pglsTable, file=paste0("pgls_LatTherm_", numExtremeObs, "extreme_t", iT+1, ".csv"), row.names=TRUE, na="")
    }
    ## Produce tables of NAM ols and pgls results for t3 and t4, including pesticide
    ## variables
    for (iT in c(2,3)) {
        
        olsModel <- vector("list",6)
        pglsModel <- vector("list",6)
        pglsModelAIC <- vector("list",6)

        cnt <- -3
        for (iExtreme in c("latitude", "thermal")) {
            iBound <- "Hot"
            cnt <- cnt + 3
            iModel <- which(models$y==paste("delta_", tolower(iExtreme), iBound, "_t", iT+1, sep="") & models$namOnly==TRUE)
            olsModel[[cnt+1]]$ols <- lmResults[[iModel]][[1]]$lm
            olsModel[[cnt+2]]$ols <- lmResults[[iModel]][[2]]$lm
            olsModel[[cnt+1]]$varName <- paste("OLS|delta_", tolower(iExtreme), iBound, "_t", iT+1, sep="")
            olsModel[[cnt+2]]$varName <- paste("OLS|delta_", tolower(iExtreme), iBound, "_t", iT+1, sep="")
            
            olsModel[[cnt+3]]$ols <- pglsResults[[iModel]][[pglsResults[[iModel]]$selectedModel]]$pgls
            olsModel[[cnt+3]]$varName <- paste("PGLS|delta_", tolower(iExtreme), iBound, "_t", iT+1, sep="")

            pglsModelAIC[[cnt+1]] <- rep(NA, 5)
            pglsModelAIC[[cnt+2]] <- rep(NA, 5)
            pglsModelAIC[[cnt+3]] <- t(pglsResults[[iModel]]$AIC[c(6,1,2,4,5)])
        }
        olsAndpglsTable <- .tabulateResults(olsModel, pglsAIC=pglsModelAIC, includeOlsAIC=TRUE)
        write.csv(olsAndpglsTable, file=paste0("ols_pgls_NAMOnly_LatTherm_", numExtremeObs, "extreme_t", iT+1, ".csv"), row.names=TRUE, na="")

    }
}

## ---------------------------------------------------------------
## STEP FIVE
## ---------------------------------------------------------------
## Run ordinary least-squares (OLS) linear regressions
## for the delta elevational variables.
## Also run phylogenetic generalized
## least-squares (PGLS) regressions on the
## selected OLS models (i.e. with lowest AIC)
doStep5 <- TRUE
if (doStep5) {
    ## VARIABLES    
    ## A series of OLS linear regression models are run using
    ## the following combination of dependent and independent variables:
    ## y: (delta_elevation) x (t2, t3, t4) 
    ## x: meanLatitude
    ##    continent
    
    ## For each y variable combination, two related "sub-models" are computed are as follows:
    ## 
    ## (1) y ~ x
    ## (2) y ~ x + continent + continent:x

    ## Create a master table for the models to be run
    models <- data.frame(
                y=apply(expand.grid("delta_elevation", c("_t2", "_t3", "_t4")), 1, function(x) paste(x, collapse="")),
                x=apply(expand.grid("latitude", c("_t2", "_t3", "_t4")), 1, function(x) paste(x, collapse="")),
                #x="latitude_t1",
                continent="continent",
                stringsAsFactors=FALSE)
    
    ## Prepare for PGLS analyses
    ## Load and prune the Cameron et al. tree to represent 65 of the 67
    ## species we use in the analysis (we lack phylogenetic data for 2 species)
    fullTree <- read.tree(paste0(rawDataPath, "cameronTree_final.tre"))    
    useTree <- drop.tip(fullTree, tip=fullTree$tip.label[!(fullTree$tip.label %in% rangeShift$species)])
        
    ## Remove B. bohemicus data for North America.  This species has been recorded on both continents,
    ## however European sample size is much larger. Also remove B. magnus as we do not have
    ## phylogenetic information for this species.
    pglsRangeShift <- rangeShift[!(rangeShift$species=="bohemicus" & rangeShift$continent=="NAM"), ]
    pglsRangeShift <- pglsRangeShift[pglsRangeShift$species!="magnus", ]
    rownames(pglsRangeShift) <- pglsRangeShift$species
    
    lmResultsElev <- vector("list", nrow(models))
    pglsResultsElev <- vector("list", nrow(models))
    
    ## Run all four sub-models unless it is a North America only (namOnly) model
    for (iModel in 1:nrow(models)) {
        lmResultsElev[[iModel]] <- list()
        lmResultsElev[[iModel]][[1]] <- list()
        lmResultsElev[[iModel]][[2]] <- list()
            
        useRangeShift <- rangeShift
        usePglsRangeShift <- pglsRangeShift
        
        
        ## MODEL (1)
        lmResultsElev[[iModel]][[1]]$lm <- lm(as.formula(paste(models[iModel, "y"], " ~ ", models[iModel, "x"], sep="")), data=useRangeShift)

        ## MODEL (2)
        lmResultsElev[[iModel]][[2]]$lm <- lm(as.formula(paste(models[iModel, "y"], " ~ ", models[iModel, "x"], "*continent", sep="")), data=useRangeShift)
        
        ## Produce AIC table
        lmResultsElev[[iModel]]$AIC <- AIC(lmResultsElev[[iModel]][[1]]$lm, lmResultsElev[[iModel]][[2]]$lm)
        
      
        ## Identify selected model as the model with the lowest AIC,
        ## but if the next simplest model differs by 2 or less choose it
        .selectModel <- function(aic, dAICTolerance=2) {
            minAIC <- which.min(aic)
            dAIC <- abs(aic - aic[minAIC])
            dAIC <- dAIC - dAICTolerance
            if (minAIC == 4) {
                if (dAIC[3] <= 0) {
                    minAIC <- 3
                }
            }
            if (minAIC == 3) {
                if (dAIC[2] <= 0) {
                    minAIC <- 2
                }
            }
            if (minAIC == 2) {
                if (dAIC[1] <= 0) {
                    minAIC <- 1
                }
            }
            return(minAIC)
        }
        lmResultsElev[[iModel]]$selectedModel <- .selectModel(lmResultsElev[[iModel]]$AIC[,2])
        
        ## Use phylogenetic generalized least squares (PGLS) approach to re-examine the
        ## selected models using several models of covariance structure
        cBrownian <- corBrownian(phy=useTree)
        cOU <- corMartins(1, phy=useTree)
        cPagel <- corPagel(1, phy=useTree)

        pglsCorrModels <- c("Brownian", "OrnUhl", "Pagel", "Independent") 
        pglsResultsElev[[iModel]] <- vector("list", length(pglsCorrModels))
        thisForm <- paste(as.character(formula(lmResultsElev[[iModel]][[lmResultsElev[[iModel]]$selectedModel]]$lm))[c(2,1,3)], collapse=" ")
        pglsResultsElev[[iModel]][[1]]$pgls <- try(gls(formula(lmResultsElev[[iModel]][[lmResultsElev[[iModel]]$selectedModel]]$lm), usePglsRangeShift, correlation=cBrownian), silent=TRUE)
        pglsResultsElev[[iModel]][[2]]$pgls <- try(gls(formula(lmResultsElev[[iModel]][[lmResultsElev[[iModel]]$selectedModel]]$lm), usePglsRangeShift, correlation=cOU), silent=TRUE)
        pglsResultsElev[[iModel]][[3]]$pgls <- try(gls(formula(lmResultsElev[[iModel]][[lmResultsElev[[iModel]]$selectedModel]]$lm), usePglsRangeShift, correlation=cPagel), silent=TRUE)
        pglsResultsElev[[iModel]][[4]]$pgls <- try(gls(formula(lmResultsElev[[iModel]][[lmResultsElev[[iModel]]$selectedModel]]$lm), usePglsRangeShift, correlation=NULL), silent=TRUE)

        if (class(pglsResultsElev[[iModel]][[2]]$pgls) != "try-error") {
            alphaOrnUhl <- attr(pglsResultsElev[[iModel]][[2]]$pgls$apVar, "Pars")["corStruct"]
            if (!is.numeric(alphaOrnUhl)) alphaOrnUhl <- NA
        }
        else {
            alphaOrnUhl <- NA
        }
        if (class(pglsResultsElev[[iModel]][[3]]$pgls) != "try-error") {
            lambdaPagel <- attr(pglsResultsElev[[iModel]][[3]]$pgls$apVar, "Pars")["corStruct"]
        }
        else {
            lambdaPagel <- NA
        }
               
        aic <- lapply(pglsResultsElev[[iModel]], function(x) if (class(x$pgls) != "try-error") AIC(x$pgls) else NA)
        aic <- data.frame(t(data.frame(do.call(c, aic))))
        if (abs(aic[which.min(aic)] - aic[4]) <= 2) {
            pglsResultsElev[[iModel]]$selectedModel <- 4

        }
        else {
            pglsResultsElev[[iModel]]$selectedModel <- as.integer(which.min(aic))
        }
        
        aic <- data.frame(aic[1, 1:2], alphaOrnUhl, aic[1, 3], lambdaPagel, aic[1, 4], pglsCorrModels[pglsResultsElev[[iModel]]$selectedModel], stringsAsFactors=FALSE)
        names(aic) <- c(paste("aic", pglsCorrModels[1:2], sep=""), "alphaOrnUhl", paste("aic", pglsCorrModels[3], sep=""), "lambdaPagel", paste("aic", pglsCorrModels[4], sep=""), "aicBestModel")
        rownames(aic) <- iModel
        pglsResultsElev[[iModel]]$AIC <- aic
        
    }
}





## ---------------------------------------------------------------
## STEP SIX
## ---------------------------------------------------------------
## Produce elevational figures
doStep6 <- TRUE
if (doStep6) {
    
    tX <- lapply(2:4, function(x) {
        
        y <- list()
        y$period <- paste(samplingPeriods[x, c("periodFrom", "periodTo")], collapse="to")
        y$shift <- rangeShift[, paste0("delta_elevation_t", x)]
        y$shift <- data.frame(y$shift, rangeShift[, paste0("latitude_t", x)], rangeShift[, "continent"])
        names(y$shift) <- c(paste(c("delta_elevation_t", "latitude_t"), x, sep=""), "continent")
        #y$t1 <- rangeShift[, c("continent", "latitude_t1")]
        return(y)
    })
    
    for (lsType in c("ols", "pgls")) {
        
        for (iT in 1:length(tX)) {
        
            if (plotType=="PDF") {
                ## Create a PDF for Science specifications (1 columns @ 2.3 in/column)
                pdf(paste0(lsType, "_Elevation_t", iT+1, ".pdf"),  width=2.3, height=2.3,  pointsize=10)
            } else if (plotType=="JPEG") {
                jpeg(paste0(lsType, "_Elevation_t", iT+1, ".jpg"),  width=2.3, height=2.3,  pointsize=10, quality=100, res=600, units="in")
            } else {
                dev.new(width=2.3, height=2.3, units="in", pointsize=10)
            }
    
            plotX <- tX[[iT]]$shift[, c("continent", paste0("latitude_t", iT+1))]
            #plotX <- tX[[iT]]$t1
            plotY <- tX[[iT]]$shift[ , c("continent", paste0("delta_elevation_t", iT+1))]


            par(mar=c(3.5,3.5,1,0.5))
              
            plot(plotX[, 2], plotY[, 2], type="n", xlab="", ylab="", axes=FALSE)
        
            axis(1, seq(0, 10000, by=1000))    
            axis(1, seq(0, 10000, by=500), rep("", length(seq(0, 10000, by=500))), tcl=-0.25)    
            axis(2, seq(-1000, 1000, by=400))    
            axis(2, seq(-1000, 1000, by=200), rep("", length(seq(-1000, 1000, by=200))), tcl=-0.25)
            mtext(substitute(a * (list(b-d, km)), list(a="Mean latitude ", b=paste0(samplingPeriods[iT+1, "periodFrom"]), d=paste0(samplingPeriods[iT+1, "periodTo"]))), side=1, line=2.25, cex=0.75) 
            mtext(substitute(Delta~a * (by~list(b-d, m)), list(a="Elevation ", b=paste0(samplingPeriods[iT+1, "periodFrom"]), d=paste0(samplingPeriods[iT+1, "periodTo"]))), side=2, line=2, cex=0.75) 

            box()
            abline(h=0, lty="dashed")
              
            ## Function to plot a truncated regression line (i.e. truncated by x limits)
            ## and truncated confidence limits. 
            .regShort <-  function(plotX, lmResult, ci=TRUE, reg.col=NULL, reg.lwd=NULL, ci.border=NULL, ci.fill=NULL, gls=NULL) {

                ## Find CI bands using the mean of the other variables
                ## or alternately at 0.
                findCIsAtMean <- FALSE
                if (is.null(gls)) {
                    modelTerms <- rownames(attr(terms(lmResult), "factors"))[-1]
                }
                else {
                    modelTerms <- names(coef(lmResult))[-1]
                    modelTerms <- modelTerms[!grepl(":", modelTerms)]
                    modelTerms <- gsub("continentEUR", "continent", modelTerms)
                }
                if (any(modelTerms == "continent")) {
                    for (iContinent in c("NAM", "EUR")) {
                        chkX <- plotX[plotX$continent==iContinent, ]
                        additionalTerms <- modelTerms[!grepl(paste(c("delta_l",  "delta_t","continent", names(chkX)[2]), collapse="|"), modelTerms)]
                        newDf <- t(t(seq(min(chkX[,2]), max(chkX[,2]), by=diff(range(chkX[,2]))/20)))
                        
                        
                        if ((length(additionalTerms) != 0) && findCIsAtMean) {
                            newDf <- suppressWarnings(data.frame(newDf, t(data.frame(sapply(additionalTerms, function(x) mean(rangeShift[,x]))))))
                        }
                        else {
                            newDf <- data.frame(newDf, matrix(0, nrow(newDf), length(additionalTerms)))
                        }
                        names(newDf) <- c(names(chkX)[2], additionalTerms)
                        newDf$continent <- iContinent
                        newDf$continent <- factor(newDf$continent, levels=c("NAM", "EUR"))
                        conf <- predict(lmResult, newdata=newDf, interval="confidence")
                        if (is.null(gls)) {
                            if (ci) {
                                lines(newDf[,1], conf[,2], col=substr(ci.border[ifelse(iContinent=="NAM", 2, 3)], 1, 7))
                                lines(newDf[,1], conf[,3], col=substr(ci.border[ifelse(iContinent=="NAM", 2, 3)], 1, 7))
                                polygon(c(newDf[,1], rev(newDf[,1])), c(conf[,2], rev(conf[,3])), border=NA, col=ci.fill[ifelse(iContinent=="NAM", 2, 3)])
                            }
                            lines(newDf[,1], conf[,1], col=reg.col[ifelse(iContinent=="NAM", 2, 3)], lwd=reg.lwd[ifelse(iContinent=="NAM", 2, 3)])
                        }
                        else {
                            lines(newDf[,1], conf, col=substr(ci.border[ifelse(iContinent=="NAM", 2, 3)], 1, 7))
                            text(par("usr")[2] - (par("usr")[2]-par("usr")[1])*0.95, par("usr")[4] - (par("usr")[4]-par("usr")[3])*0.1, paste0("corr=", toupper(gls)), cex=0.75, font=3, adj=0)                                
                        }
                            
                    }
                }
                else {
                    chkX <- plotX
                    additionalTerms <- modelTerms[!grepl(paste(c("delta_l",  "delta_t","continent", names(chkX)[2]), collapse="|"), modelTerms)]
                    newDf <- t(t(seq(min(chkX[,2]), max(chkX[,2]), by=diff(range(chkX[,2]))/20)))
                    
                    ## Find CI bands using the mean of the other variables
                    ## or alternately at 0.
                    if ((length(additionalTerms) != 0) && findCIsAtMean) {
                        newDf <- suppressWarnings(data.frame(newDf, t(data.frame(sapply(additionalTerms, function(x) mean(rangeShift[,x]))))))
                    }
                    else {
                        newDf <- data.frame(newDf, matrix(0, nrow(newDf), length(additionalTerms)))
                    }
                    
                    names(newDf) <- c(names(chkX)[2], additionalTerms)
                    conf <- predict(lmResult, newdata=newDf, interval="confidence")
                    if (is.null(gls)) {
                        if (ci) {
                            lines(newDf[,1], conf[,2], col=substr(ci.border[1], 1, 7))
                            lines(newDf[,1], conf[,3], col=substr(ci.border[1], 1, 7))
                            polygon(c(newDf[,1], rev(newDf[,1])), c(conf[,2], rev(conf[,3])), border=NA, col=ci.fill[1])
                        }
                        lines(newDf[,1], conf[,1], col=reg.col[1], lwd=reg.lwd[1])

                    }
                    else {
                        lines(newDf[,1], conf, col=reg.col[1], lwd=reg.lwd[1])
                        text(par("usr")[2] - (par("usr")[2]-par("usr")[1])*0.95, par("usr")[4] - (par("usr")[4]-par("usr")[3])*0.1, paste0("corr=", toupper(gls)), cex=0.75, font=3, adj=0)

                    }
                }
            }
            
            points(plotX[plotX$continent=="NAM", 2], plotY[plotY$continent=="NAM", 2], pch=20, cex=1.5, col=plotCol[1])
            points(plotX[plotX$continent=="EUR", 2], plotY[plotY$continent=="EUR", 2], pch=20, cex=1.5, col=plotCol[2])
            
            ## Regression line(s)
            ## Calling the variable iModel here is a hack because gls() does not correctly store
            ## the call that enables prediction from the model.
            iModel <- which(models$y==paste("delta_elevation_t", iT+1, sep=""))
            
            if (lsType=="ols") {
                .regShort(plotX, lmResultsElev[[iModel]][[lmResultsElev[[iModel]]$selectedModel]]$lm, reg.col=c("black", plotCol), reg.lwd=rep(1.5, 3), ci.border=c("#a8a8a840", ciCol), ci.fill=c("#a8a8a840", ciCol))
            }
            else {
                .regShort(plotX, pglsResultsElev[[iModel]][[pglsResultsElev[[iModel]]$selectedModel]]$pgls, reg.col=c("black", plotCol), reg.lwd=rep(1.5, 3), ci.border=c("#a8a8a840", ciCol), ci.fill=c("#a8a8a840", ciCol), gls=pglsCorrModels[pglsResultsElev[[iModel]]$selectedModel])
            }
            par(xpd=NA)
            legend(par("usr")[2] - (par("usr")[2]-par("usr")[1])*0.372, par("usr")[4] - (par("usr")[4]-par("usr")[3])*0, legend=c("North America", "Europe"), pch=20, col=plotCol, pt.cex=0.75, cex=0.5)
            if (plotType=="PDF" || plotType=="JPEG") dev.off()
        }
    }
}


## ---------------------------------------------------------------
## STEP SEVEN
## ---------------------------------------------------------------
## Prepare tables (in CSV) summarizing the results
## from regressions on elevational variables
doStep7 <- TRUE
if (doStep7) {
    
    pglsBestModelNoInt <- vector("list", length(tX))
    
    ## Produce tables of NAM + EUR ols and pgls results for each time period
    for (iT in 1:length(tX)) {
        
        olsModel <- vector("list", 1)
        pglsModel <- vector("list", 1)
        pglsModelAIC <- vector("list", 1)
        iModel <- which(models$y==paste("delta_elevation_t", iT+1, sep=""))
        olsModel[[1]]$ols <- lmResultsElev[[iModel]][[lmResultsElev[[iModel]]$selectedModel]]$lm
        olsModel[[1]]$varName <- paste("OLS|delta_elevation_t", iT+1, sep="")
        pglsModel[[1]]$ols <- pglsResultsElev[[iModel]][[pglsResultsElev[[iModel]]$selectedModel]]$pgls
        pglsModel[[1]]$varName <- paste("PGLS|delta_elevation_t", iT+1, sep="")
        pglsModelAIC[[1]] <- t(pglsResultsElev[[iModel]]$AIC[c(6,1,2,4,5)])
        olsTable <- .tabulateResults(olsModel)
        pglsTable <- .tabulateResults(pglsModel, pglsModelAIC)
        write.csv(olsTable, file=paste0("ols_elevation_t", iT+1, ".csv"), row.names=TRUE, na="")
        write.csv(pglsTable, file=paste0("pgls_elevation_t", iT+1, ".csv"), row.names=TRUE, na="")
        
        ## Also rerun the best PGLS regression without an intercept
        ## to better estimate parameters (after Schielzeth, 2009; Methods. Ecol. Evol)
        newFormula <- as.character(formula(pglsModel[[1]]$ols))
        newFormula <- paste0(newFormula[2], " ~ ", paste(newFormula[3:length(newFormula)], collapse=" + "), " - 1")
        bestCorrStructure <- list(cBrownian, cOU, cPagel, NULL)[pglsResultsElev[[iModel]]$selectedModel]
        pglsBestModelNoInt[[iT]] <- try(gls(formula(newFormula), usePglsRangeShift, correlation=bestCorrStructure[[1]]), silent=TRUE)
    
        
    }
}

## ---------------------------------------------------------------
## STEP EIGHT
### ---------------------------------------------------------------
# Determine species-period sample sizes and
## other summary information
doStep8 <- TRUE
if (doStep8) {

    sampleSizes <- data.frame()
    for (iContinent in unique(rawData$continent)) {
        
        thisContinent <- rawData[rawData$continent==iContinent, ]
        thisContinent$periodYears <- paste(thisContinent$periodFrom, thisContinent$periodTo, sep="-")
        tablePart1 <- data.frame(continent=iContinent,
                        meanKmNorthEquator=aggregate(kmNorthEquator ~ species, FUN=mean, data=thisContinent),
                        meanElevation=aggregate(elevation ~ species, FUN=mean, data=thisContinent)[, 2],
                        stringsAsFactors=FALSE)
        tablePart1 <- tablePart1[, c(2,1,3,4)]
        names(tablePart1)[c(1,3)] <- c("species", "meanKmNorthEquator")
        tablePart2 <- table(thisContinent$species, thisContinent$periodYears)
        species <- rownames(tablePart2)
        rownames(tablePart2) <- NULL
        sampleSizeTable <- cbind(tablePart1,
                                 tablePart2[match(species, tablePart1$species), 1:4])
        sampleSizeTable <- sampleSizeTable[order(sampleSizeTable$meanKmNorthEquator), ]
        sampleSizes <- rbind(sampleSizes, sampleSizeTable)
    }

}
