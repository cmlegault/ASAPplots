#' Plot ASAP
#' 
#' This function calls all the other functions to create the desired outputs.
#' @param wd directory where ASAP run is located
#' @param asap.name Base name of original dat file (without the .dat extension)
#' @param nyrs.ave number of years to average for calculating Reference Points (defaults to 5)
#' @param correlation.limit will return parameter combos where |correlation| > value (defaults to 0.9)
#' @param scale.catch.bubble.resid larger values increase size of catch age comp bubbles (defaults to 2)
#' @param scale.index.bubble.resid larger values increase size of catch age comp bubbles (defaults to 2)
#' @param scale.catch.bubble.data larger values increase size of catch age comp bubbles (defaults to 6)
#' @param scale.index.bubble.data larger values increase size of catch age comp bubbles (defaults to 6)
#' @param first.age youngest age to use in catch curves, -999 finds peak age (defaults to -999)
#' @param mcmc.burn number of realizations to remove from start of MCMC (defaults to 0)
#' @param mcmc.thin thinning parameter for MCMC (defaults to 1)
#' @param save.plots saves indivdual plots (defaults to TRUE)
#' @param make.one.pdf diagnostics/results/ref pts/MCMC/data are all saved as one pdf (defaults to TRUE)
#' @param plotf format for individual plots (defaults to 'png')
#' @export
#' @examples PlotASAP("C:\\main.dir\\my.asap.results.dir","simple") 

PlotASAP <- function(wd, asap.name, nyrs.ave=5, correlation.limit=0.9, 
                     scale.catch.bubble.resid=2, scale.index.bubble.resid=2, 
                     scale.catch.bubble.data=6, scale.index.bubble.data=6,
                     mcmc.burn=0, mcmc.thin=1,
                     first.age=-999, save.plots=TRUE, plotf='png', make.one.pdf=TRUE){

  od <- paste0(wd,"\\plots\\")
  pdf.name <- asap.name
  
  retro.flag <- FALSE  
  if (file.exists(paste0(wd,"\\",asap.name, ".rts"))){
    retro.flag <- TRUE
    asap.name <- paste0(asap.name,"_000")  # change asap.name to use null retro peel
  }

  ss5<-shell(paste0("dir ",wd,"\\plots"), intern=T )
  ss6 <- which(ss5=="File Not Found")
  if (length(ss6)>0 )  shell(paste0("mkdir ",wd,"\\plots"), intern=T )
  # the line below is more elegant, but not supported by older versions of R
  #if (dir.exists(od)==F )  shell(paste0("mkdir ",  od), intern=T )
  
  rdat <- paste0(asap.name,".rdat")
  asap <- dget(paste0(wd,"\\",rdat))
  
  liz.palette = c( "black"  ,  "purple3" ,  "blue"  ,  "turquoise2"  ,
                   "red2" ,   "orange" ,  "#994411",   "#770000"    ,
                   "#335500"  ,  "springgreen3" , "green1" ,  "gold3",
                   "#333388" ,      "orchid"   ,     "mediumpurple1"      , "gray60"  , 
                   "deeppink4"  ,    "violetred2"  ,     "#9900AA"    , "#8888EE",
                   "yellow1"   ,     "yellowgreen"  ,  "#778800" ,      "#FFBB11"  ,  
                   "#CC5588"  ,"#11BB77"   , "#11AADD"   ,   "#335522"   ,  
                   "#BB1133"   ,     "#4400AA",  "#FF7755"   ,  "#77AACC"   , 
                   "#FF00BB" ,  "grey50"   ,  "#FF2233" , "#99BB77"  ,  
                   "grey35"   ,    "#CCDD00" ,    "#AA77FF"   ,  "#88BB88"    )
  liz.palette <- rep(liz.palette, 5)
  
  graphics.off()
  
  windows( height=12, width=10, record=T)

  #--- Model Diagnostics
  
  gn <- GrabNames(wd,asap.name,asap)
  fleet.names <- gn$fleet.names
  index.names <- gn$index.names
  
  a1 <- GrabAuxFiles(wd,asap.name,asap,fleet.names,index.names)  #TODO make sure a1 exists
  npar<-a1$npar     
  max.grad<-a1$max.grad
  MakeSelectivityDecoder(wd,asap,a1,index.names,od)
  PlotFrontPage(wd,asap.name,asap,a1,save.plots,od,plotf)
  PlotHighCorr(a1,correlation.limit,save.plots,od,plotf)
  PlotHighCVs(a1,save.plots,od,plotf)
  SummarizeASAP(asap,a1,od)
  
  PlotMainLikelihoods(asap.name,asap,a1,save.plots,od,plotf,liz.palette)
  PlotRMSEtable(asap.name,asap,save.plots,od,plotf)
  PlotRMSE95CI(asap,index.names,save.plots,od,plotf,liz.palette)
  PlotCatch4Panel(asap,fleet.names,save.plots,od,plotf,liz.palette)
  PlotDiscard4Panel(asap,fleet.names,save.plots,od,plotf,liz.palette)
  PlotCatchAgeComp(asap,fleet.names,save.plots,od,plotf,liz.palette)  # catch
  PlotCatchAgeCompResids(asap,fleet.names,save.plots,od,plotf,scale.catch.bubble.resid)  # bubble plots
  PlotCatchAgeComp(asap,fleet.names,save.plots,od,plotf,liz.palette,is.catch.flag=FALSE)  # discards
  PlotCatchAgeCompResids(asap,fleet.names,save.plots,od,plotf,scale.catch.bubble.resid,
                         is.catch.flag=FALSE)  # discard bubble plots
  PlotFleetNeff(asap,fleet.names,save.plots,od,plotf,liz.palette)
  PlotFrancisFleet(asap,a1,fleet.names,save.plots,od,plotf)
  PlotFleetNeff(asap,fleet.names,save.plots,od,plotf,liz.palette,is.catch.flag=FALSE)  # discards
  PlotFrancisFleet(asap,a1,fleet.names,save.plots,od,plotf,is.catch.flag=FALSE)  # discards
  PlotIndices4Panel(asap,index.names,save.plots,od,plotf,liz.palette)
  PlotIndexAgeCompResids(asap,index.names,save.plots,od,plotf,scale.index.bubble.resid)
  PlotIndexNeff(asap,index.names,save.plots,od,plotf,liz.palette)
  PlotFrancisIndex(asap,index.names,save.plots,od,plotf)  
  
  #--- Model Results
  
  PlotFleetSelBlocks(asap,fleet.names,save.plots,od,plotf,liz.palette)
  PlotFleetFmult(asap,fleet.names,save.plots,od,plotf,liz.palette)
  PlotIndexSelectivities(asap,index.names,save.plots,od,plotf,liz.palette)
  PlotCatchCurvesForCatch(asap,a1,save.plots,od,plotf,first.age)
  PlotCatchCurvesForIndices(asap,a1,save.plots,od,plotf,first.age)
  get_Sinclair_Z(asap,a1,index.names,save.plots,od,plotf)    
  PlotCatchAtAgeConsistency(asap,fleet.names,save.plots,od,plotf)
  PlotIndexAtAgeConsistency(asap,index.names,save.plots,od,plotf)
  PlotCatchability(asap,index.names,a1,save.plots,od,plotf,liz.palette)
  PlotSSBFtrend(asap,save.plots,od,plotf)
  PlotAllBiomassTypes(asap,save.plots,od,plotf)
  PlotSSBatAge(asap,save.plots,od,plotf,liz.palette)
  PlotNAA(asap,save.plots,od,plotf,liz.palette)
  PlotRecruitmentDevs(asap,save.plots,od,plotf)
  PlotRecrSSByr(asap,save.plots,od,plotf) 
  PlotSRpredLine(asap,save.plots,od,plotf)
  PlotSARCrecSSB(asap,save.plots,od,plotf)
  PlotCV(asap,a1,save.plots,od,plotf)
  if (retro.flag == TRUE) PlotRetroWrapper(wd,asap.name,asap,save.plots,od,plotf)
  
  #--- Reference Points
  
  PlotYieldCurves(asap,a1,nyrs.ave,save.plots,od,plotf)
  PlotSPRtable(asap,a1,nyrs.ave,save.plots,od,plotf)
  PlotExpSpawn(asap,a1,nyrs.ave,save.plots,od,plotf)
  PlotAnnualSPRtargets(asap,save.plots,od,plotf)
  PlotAnnualMSY(asap,a1,save.plots,od,plotf)
  
  #--- MCMC results (assume user only does 1 chain)
  
  if (asap$options$do.mcmc>0) PlotMCMC(wd,asap.name,asap,mcmc.burn,mcmc.thin,save.plots,od,plotf)
  
  #--- Input Data
  PlotCatchByFleet(asap,fleet.names,save.plots,od,plotf,liz.palette)
  PlotCatchAgeCompBubbles(asap,fleet.names,save.plots,od,plotf,scale.catch.bubble.data)
  PlotCatchAgeCompBubbles(asap,fleet.names,save.plots,od,plotf,scale.catch.bubble.data,
                          is.catch.flag=FALSE)  # discards
  PlotIndexInput(asap,save.plots,od,plotf,liz.palette)
  PlotIndexAgeCompBubbles(asap,index.names,save.plots,od,plotf,scale.index.bubble.data)
  PlotWAAmatrices(asap,save.plots,od,plotf,liz.palette)
  PlotM(asap,save.plots,od,plotf)
  PlotMaturity(asap,save.plots,od,plotf)

  
  #------------------------------------
  #------  PDF Files  ---------------

  save.plots=F
  graphics.off()
  
  if (make.one.pdf==T) {
    windows()
    pdf(file=paste0(od,pdf.name,".ALL.PLOTS.pdf"), onefile=T)
    PlotFrontPage(wd,asap.name,asap,a1,save.plots,od,plotf)
    PlotMainLikelihoods(asap.name,asap,a1,save.plots,od,plotf,liz.palette)
    PlotHighCorr(a1,correlation.limit,save.plots,od,plotf)
    PlotHighCVs(a1,save.plots,od,plotf)
    PlotRMSEtable(asap.name,asap,save.plots,od,plotf)
    PlotRMSE95CI(asap,index.names,save.plots,od,plotf,liz.palette)
    PlotCatch4Panel(asap,fleet.names,save.plots,od,plotf,liz.palette)
    PlotDiscard4Panel(asap,fleet.names,save.plots,od,plotf,liz.palette)
    PlotCatchAgeComp(asap,fleet.names,save.plots,od,plotf,liz.palette)  # catch
    PlotCatchAgeCompResids(asap,fleet.names,save.plots,od,plotf,scale.catch.bubble.resid)  # bubble plots
    PlotCatchAgeComp(asap,fleet.names,save.plots,od,plotf,liz.palette,is.catch.flag=FALSE)  # discards
    PlotCatchAgeCompResids(asap,fleet.names,save.plots,od,plotf,scale.catch.bubble.resid,
                           is.catch.flag=FALSE)  # discard bubble plots
    PlotFleetNeff(asap,fleet.names,save.plots,od,plotf,liz.palette)
    PlotFrancisFleet(asap,a1,fleet.names,save.plots,od,plotf)
    PlotFleetNeff(asap,fleet.names,save.plots,od,plotf,liz.palette,is.catch.flag=FALSE)  # discards
    PlotFrancisFleet(asap,a1,fleet.names,save.plots,od,plotf,is.catch.flag=FALSE)  # discards
    PlotIndices4Panel(asap,index.names,save.plots,od,plotf,liz.palette)
    PlotIndexAgeCompResids(asap,index.names,save.plots,od,plotf,scale.index.bubble.resid)
    PlotIndexNeff(asap,index.names,save.plots,od,plotf,liz.palette)
    PlotFrancisIndex(asap,index.names,save.plots,od,plotf)  
    PlotFleetSelBlocks(asap,fleet.names,save.plots,od,plotf,liz.palette)
    PlotFleetFmult(asap,fleet.names,save.plots,od,plotf,liz.palette)
    PlotIndexSelectivities(asap,index.names,save.plots,od,plotf,liz.palette)
    PlotCatchCurvesForCatch(asap,a1,save.plots,od,plotf,first.age)
    PlotCatchCurvesForIndices(asap,a1,save.plots,od,plotf,first.age)
    get_Sinclair_Z(asap,a1,index.names,save.plots,od,plotf)    
    PlotCatchAtAgeConsistency(asap,fleet.names,save.plots,od,plotf)
    PlotIndexAtAgeConsistency(asap,index.names,save.plots,od,plotf)
    PlotCatchability(asap,index.names,a1,save.plots,od,plotf,liz.palette)
    PlotSSBFtrend(asap,save.plots,od,plotf)
    PlotAllBiomassTypes(asap,save.plots,od,plotf)
    PlotSSBatAge(asap,save.plots,od,plotf,liz.palette)
    PlotNAA(asap,save.plots,od,plotf,liz.palette)
    PlotRecruitmentDevs(asap,save.plots,od,plotf)
    PlotRecrSSByr(asap,save.plots,od,plotf) 
    PlotSRpredLine(asap,save.plots,od,plotf)
    PlotSARCrecSSB(asap,save.plots,od,plotf)
    PlotCV(asap,a1,save.plots,od,plotf)
    if (retro.flag == TRUE) PlotRetroWrapper(wd,asap.name,asap,save.plots,od,plotf)
    PlotYieldCurves(asap,a1,nyrs.ave,save.plots,od,plotf)
    PlotSPRtable(asap,a1,nyrs.ave,save.plots,od,plotf)
    PlotExpSpawn(asap,a1,nyrs.ave,save.plots,od,plotf)
    PlotAnnualSPRtargets(asap,save.plots,od,plotf)
    PlotAnnualMSY(asap,a1,save.plots,od,plotf)
    if (asap$options$do.mcmc>0) PlotMCMC(wd,asap.name,asap,mcmc.burn,mcmc.thin,save.plots,od,plotf)
    PlotCatchByFleet(asap,fleet.names,save.plots,od,plotf,liz.palette)
    PlotCatchAgeCompBubbles(asap,fleet.names,save.plots,od,plotf,scale.catch.bubble.data)
    PlotCatchAgeCompBubbles(asap,fleet.names,save.plots,od,plotf,scale.catch.bubble.data,
                            is.catch.flag=FALSE)  # discards
    PlotIndexInput(asap,save.plots,od,plotf,liz.palette)
    PlotIndexAgeCompBubbles(asap,index.names,save.plots,od,plotf,scale.index.bubble.data)
    PlotWAAmatrices(asap,save.plots,od,plotf,liz.palette)
    PlotM(asap,save.plots,od,plotf)
    PlotMaturity(asap,save.plots,od,plotf)
    dev.off()      
    graphics.off()
  } # end make.one.pdf
  
  #--------------------------------------------------
  #--------Now make individual pdfs------------------
  #_________DIAGNOSTIC PLOTS___________________
  
  windows()
  pdf(file=paste0(od,pdf.name,".DIAGNOSTIC.PLOTS.pdf"), onefile=T)
  PlotFrontPage(wd,asap.name,asap,a1,save.plots,od,plotf)
  PlotMainLikelihoods(asap.name,asap,a1,save.plots,od,plotf,liz.palette)
  PlotHighCorr(a1,correlation.limit,save.plots,od,plotf)
  PlotHighCVs(a1,save.plots,od,plotf)
  PlotRMSEtable(asap.name,asap,save.plots,od,plotf)
  PlotRMSE95CI(asap,index.names,save.plots,od,plotf,liz.palette)
  PlotCatch4Panel(asap,fleet.names,save.plots,od,plotf,liz.palette)
  PlotDiscard4Panel(asap,fleet.names,save.plots,od,plotf,liz.palette)
  PlotCatchAgeComp(asap,fleet.names,save.plots,od,plotf,liz.palette)  # catch
  PlotCatchAgeCompResids(asap,fleet.names,save.plots,od,plotf,scale.catch.bubble.resid)  # bubble plots
  PlotCatchAgeComp(asap,fleet.names,save.plots,od,plotf,liz.palette,is.catch.flag=FALSE)  # discards
  PlotCatchAgeCompResids(asap,fleet.names,save.plots,od,plotf,scale.catch.bubble.resid,
                         is.catch.flag=FALSE)  # discard bubble plots
  PlotFleetNeff(asap,fleet.names,save.plots,od,plotf,liz.palette)
  PlotFrancisFleet(asap,a1,fleet.names,save.plots,od,plotf)
  PlotFleetNeff(asap,fleet.names,save.plots,od,plotf,liz.palette,is.catch.flag=FALSE)  # discards
  PlotFrancisFleet(asap,a1,fleet.names,save.plots,od,plotf,is.catch.flag=FALSE)  # discards
  PlotIndices4Panel(asap,index.names,save.plots,od,plotf,liz.palette)
  PlotIndexAgeCompResids(asap,index.names,save.plots,od,plotf,scale.index.bubble.resid)
  PlotIndexNeff(asap,index.names,save.plots,od,plotf,liz.palette)
  PlotFrancisIndex(asap,index.names,save.plots,od,plotf)  
  dev.off()
  graphics.off()
  
  #_________RESULTS___________________
  
  windows()
  pdf(file=paste0(od,pdf.name,".RESULTS.PLOTS.pdf"), onefile=T)
  PlotFleetSelBlocks(asap,fleet.names,save.plots,od,plotf,liz.palette)
  PlotFleetFmult(asap,fleet.names,save.plots,od,plotf,liz.palette)
  PlotIndexSelectivities(asap,index.names,save.plots,od,plotf,liz.palette)
  PlotCatchCurvesForCatch(asap,a1,save.plots,od,plotf,first.age)
  PlotCatchCurvesForIndices(asap,a1,save.plots,od,plotf,first.age)
  get_Sinclair_Z(asap,a1,index.names,save.plots,od,plotf)    
  PlotCatchAtAgeConsistency(asap,fleet.names,save.plots,od,plotf)
  PlotIndexAtAgeConsistency(asap,index.names,save.plots,od,plotf)
  PlotCatchability(asap,index.names,a1,save.plots,od,plotf,liz.palette)
  PlotSSBFtrend(asap,save.plots,od,plotf)
  PlotAllBiomassTypes(asap,save.plots,od,plotf)
  PlotSSBatAge(asap,save.plots,od,plotf,liz.palette)
  PlotNAA(asap,save.plots,od,plotf,liz.palette)
  PlotRecruitmentDevs(asap,save.plots,od,plotf)
  PlotRecrSSByr(asap,save.plots,od,plotf) 
  PlotSRpredLine(asap,save.plots,od,plotf)
  PlotSARCrecSSB(asap,save.plots,od,plotf)
  PlotCV(asap,a1,save.plots,od,plotf)
  if (retro.flag == TRUE) PlotRetroWrapper(wd,asap.name,asap,save.plots,od,plotf)
  dev.off()
  graphics.off()
  
  #_________RETRO___________________
  
  if (retro.flag == TRUE){
    windows()
    pdf(file=paste0(od,pdf.name,".RETRO.PLOTS.pdf"), onefile=TRUE)
    PlotRetroWrapper(wd,asap.name,asap,save.plots,od,plotf)
    dev.off()
    graphics.off()
  }

  #_________  MCMC   ___________________
  if (asap$options$do.mcmc>0) {
    windows()
    pdf(file=paste0(od,pdf.name,".MCMC.PLOTS.pdf"), onefile=T)
    PlotMCMC(wd,asap.name,asap,mcmc.burn,mcmc.thin,save.plots,od,plotf) 
    dev.off()      
    graphics.off()
  }

  #_________REFERENCE POINTS___________________
  
  windows()
  pdf(file=paste0(od,pdf.name,".REF.POINTS.PLOTS.pdf"), onefile=T )
  PlotYieldCurves(asap,a1,nyrs.ave,save.plots,od,plotf)
  PlotSPRtable(asap,a1,nyrs.ave,save.plots,od,plotf)
  PlotExpSpawn(asap,a1,nyrs.ave,save.plots,od,plotf)
  PlotAnnualSPRtargets(asap,save.plots,od,plotf)
  PlotAnnualMSY(asap,a1,save.plots,od,plotf)
  dev.off()      
  graphics.off()
  
  #_________INPUTS___________________
  
  windows()
  pdf(file=paste0(od,pdf.name,".DATA.PLOTS.pdf"), onefile=T)
  PlotCatchByFleet(asap,fleet.names,save.plots,od,plotf,liz.palette)
  PlotCatchAgeCompBubbles(asap,fleet.names,save.plots,od,plotf,scale.catch.bubble.data)
  PlotCatchAgeCompBubbles(asap,fleet.names,save.plots,od,plotf,scale.catch.bubble.data,
                          is.catch.flag=FALSE)  # discards
  PlotIndexInput(asap,save.plots,od,plotf,liz.palette)
  PlotIndexAgeCompBubbles(asap,index.names,save.plots,od,plotf,scale.index.bubble.data)
  PlotWAAmatrices(asap,save.plots,od,plotf,liz.palette)
  PlotM(asap,save.plots,od,plotf)
  PlotMaturity(asap,save.plots,od,plotf)
  dev.off()      
  graphics.off()
  
  return()
}
