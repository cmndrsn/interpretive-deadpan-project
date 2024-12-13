
# Functions ---------------------------------------------------------------

# for calculating percent differences and changes....

.getPercentDifference <- function(X, Y) (abs(X - Y)/((X+Y)/2))*100
.getCoefficientofVariation <- function(X) sd(X)/mean(X)
.getPercentChange <- function(X, Y) ((X - Y)/abs(Y))*100


nameKeys = function(dat, type = 'key') #internal function for assigning key signature based on piece ID
{
  if(type == 'key')
  {
    dat$key = as.factor(dat$pieceID)
    dat$key = plyr::revalue(dat$key, c(M0 = "C", m0 = "c", M1 = "C#", m1 = "c#", 
                                       M2 = "D", m2 = "d",M3 = "Eb", m3 = "eb", 
                                       M4 = "E", m4 = "e", M5 = "F", m5 = "f",
                                       M6 = "F#", m6 = "f#", M7 = "G", m7 = "g", 
                                       M8 = "Ab", m8 = "ab", M9 = "A", m9 = "a", 
                                       M10 = "Bb", m10 = "bb", M11 = "B", m11 = "b"))
  }
  else if(type == 'pieceID')
  {
    dat$pieceID = as.factor(dat$key)
    dat$pieceID = plyr::revalue(dat$pieceID, c(C = "M0", c = "m0", `C#` = "M1", `c#` = "m1", 
                                               `Db` = "M1", `db` = "m1", 
                                               D = "M2", d = "m2",
                                               Eb = "M3", eb = "m3", 
                                               E = "M4", e = "m4", `F` = "M5", f = "m5",
                                               `F#` = "M6", `f#` = "m6", G = "M7", g = "m7", 
                                               Ab = "M8", `G#` = "M8", `g#` = "m8",  ab = "m8", A = "M9", a = "m9", 
                                               Bb = "M10", bb = "m10", B = "M11", b = "m11"))
  }
  return(dat)
}
prettyKeySig = function(dat, keyCol = 'key')
{
  
  # prepare unicode for fancy sharp and flat signs:
  flatSym <- '\\u266d'
  sharpSym <- '\\u266f'
  # isolate letter name:
  keyChroma = substr(dat[,keyCol],1,1)
  # isolate sharp and flat signs
  keyAccidental = substr(dat[,keyCol],2,2)
  # replace "b" with proper flat symbol
  keyAccidental[keyAccidental == "b"] = eval(parse(text=paste0("'", flatSym, "'")))
  # replace "#" with proper sharp symbol
  keyAccidental[keyAccidental == "#"] = eval(parse(text=paste0("'", sharpSym, "'")))
  # create fancy key signature
  fancyKey = paste0(keyChroma, keyAccidental)
  # replace original key column with fancy key:
  dat[,keyCol] = fancyKey
  
  return(dat)
}

collapseMeasures <- function(dat,includePickups=T)
{
  retDat <- dat
  if(!includePickups)
  {
    retDat <- subset(dat,mm!=0)
  }   # simply remove these before aggregating if undesired.  This syntax works even if mm is a factor
  
  retDat <- retDat %>% 
    dplyr::group_by(composer,set,albumID, pieceID, participant)    %>% # select grouping variables
    dplyr::mutate(arScore = sum(arScore*mmWt)/sum(mmWt), #manipulates only select variables, leaving rest of data structure
           arPerf = sum(arPerf*mmWt)/sum(mmWt), #take mean attackrate of performer tempo
           pitchHeight = sum(pitchHeight*mmWt)/sum(mmWt),#Get weighted #'s across measures
           attacks=sum(attacks),
           durationSec = mean(durationSec))
  
  retDat=subset(retDat,select=-c(mm,mmDuration,mmWt)) #drop unnecessary variables
  retDat = retDat %>% dplyr::select(composer,set,albumID,pieceID,key,chroma,mode,pitchHeight,attacks,arScore,arPerf,rms,durationSec,
                             participant,valence,arousal,label,
                             pool,expLoc,expID,subj) #Reorder labels
  #remove duplicates
  retDat %>% dplyr::distinct() -> retDat 
}

prettyKeyCol <- function(dat)
{ 
  
  # isolate letter name:
  keyChroma <- substr(dat,1,1)
  # isolate sharp and flat signs
  keyAccidental <- substr(dat,2,2)
  # replace "b" with proper flat symbol
  keyAccidental[keyAccidental == "b"] = eval(parse(text=paste0("'", flatSym, "'")))
  # replace "#" with proper sharp symbol
  keyAccidental[keyAccidental == "#"] = eval(parse(text=paste0("'", sharpSym, "'")))
  # create fancy key signature
  fancyKey = paste0(keyChroma, keyAccidental)
  # replace original key column with fancy key:
  return(fancyKey)
}

nameKeysSimple <- function(pieceID, sortMode = T) #internal function for assigning key signature based on piece ID
{
  pieceID <- as.character(pieceID)
  key <- as.factor(pieceID)
  key <- plyr::revalue(key, c(M0 = "C", m0 = "c", M1 = "C#", m1 = "c#", 
                             M2 = "D", m2 = "d",M3 = "Eb", m3 = "eb", 
                             M4 = "E", m4 = "e", M5 = "F", m5 = "f",
                             M6 = "F#", m6 = "f#", M7 = "G", m7 = "g", 
                             M8 = "Ab", m8 = "g#", M9 = "A", m9 = "a", 
                             M10 = "Bb", m10 = "bb", M11 = "B", m11 = "b"))
  
  
  if(sortMode)
  {
    major <- c('C', 'C#', 'D', 'Eb', 'E', 'F', 
              'F#', 'G', 'Ab', 'A', 'Bb', 'B')
    minor <- c('c', 'c#', 'd', 'eb', 'e', 'f', 
              'f#', 'g', 'g#', 'a', 'bb', 'b')
    key <- factor(key, levels = c(major, minor))
  }
  
  return(key)
}

# Correlation Analysis ----------------------------------------------------

percentCorrelationPlot <- function(dat, x, y, 
                                  ylim = c(-1,55),
                                  xLab = ' ', yLab = '',
                                  ...) 
{
  ggplot2::ggplot(dat, aes(x = x, y = y, colour = mode))+
    ggplot2::ylim(ylim[1], ylim[2])+
    ggplot2::geom_smooth(method = 'lm',se = F)+
    ggplot2::geom_point(aes(color = mode, label = key, shape = composer))+
    ggplot2::theme_classic()+
    ggplot2::labs(x = xLab, y = '')+
    ggplot2::scale_colour_manual(values = c('firebrick2', 'dodgerblue'),
                        labels = c('Major', 'Minor'))+
    ggplot2::scale_shape_manual(values = c(16, 17), labels = c('Bach', 'Chopin'))+
    ggplot2::theme(...) -> p
  return(p)
}

# For beta analysis -------------------------------------------------------

betaTransform = function(y, prop = T)
{
  # first convert to proportion:
  if(prop) y <- y/100
  # get number of observations:
  n <- length(y)
  # return transformed value:
  yT <- (y*(n-1)+0.5)/n
  
  return(yT)
}

# Supplementary Analyses --------------------------------------------------

plotCorrelation <- function(x, y, xLab = 'x', yLab = 'y')
{
  x <- factor(x)
  levels(x) = c('Major', 'minor')
    ggplot2::ggplot()+
    ggplot2::geom_point(data = data.frame(x, y), aes(x, y, color = x))+
    ggplot2::geom_smooth(colour = 'grey50', data = data.frame(as.numeric(x), y), aes(as.numeric(x), y), method = 'lm', se = F)+
    ggplot2::theme_classic()+
    ggplot2::labs(
      subtitle = paste('R =', round(cor.test(as.numeric(x), y)$estimate, 2)))+
    ggplot2::theme_classic()+
    ggplot2::scale_colour_manual(values = c('firebrick1', 'dodgerblue'), aes(color = as.factor(x)))+
    ggplot2::labs(x = xLab, y = yLab)+
    ggplot2::theme(legend.position = 'none')
}


# Paired Circumplex (Fig 2) -----------------------------------------------

.nameKeys <- function(dat) #internal function for assigning key signature based on piece ID
{
  dat$key <- as.factor(dat$pieceID)
  dat$key <- plyr::revalue(dat$key, c(M0 = "C", m0 = "c", M1 = "C#", m1 = "c#", 
                                     M2 = "D", m2 = "d",M3 = "Eb", m3 = "eb", 
                                     M4 = "E", m4 = "e", M5 = "F", m5 = "f",
                                     M6 = "F#", m6 = "f#", M7 = "G", m7 = "g", 
                                     M8 = "Ab", m8 = "ab", M9 = "A", m9 = "a", 
                                     M10 = "Bb", m10 = "bb", M11 = "B", m11 = "b"))
  return(dat)
}

pairedCircumplex <- function(dat, groupCol = expID, 
                            chosenLvls, style = 'mp', sigTest = F, 
                            piecesToJitter, zoomDat = T)
{
  
  # I am making a copy of the groupCol argument because subsetting is difficult
  # with bracket notation
  dat$temp <- dat[,groupCol]
  names(dat)[ncol(dat)] = "groupCol"
  # check if user did not set "chosenLvls" argument
  if(missing(chosenLvls))
  {
    # warn if two levels aren't present
    print(length(unique(dat$groupCol)))
    if(length(unique(dat$groupCol)) != 2)
    {
      stop("chosenLvls argument is missing but there are more than two levels 
            in the argument you passed to groupCol.
            Please select which two levels to use 
            with the chosenLvls argument and try again. :)")
    }
    else
    {
      chosenLvls <- levels(factor(dat$groupCol))
    }
  }
  
  dat %>% 
    dplyr::group_by(groupCol, mode, key) %>% 
    dplyr::summarize(valence = mean(valence),
              arousal = mean(arousal)) -> dat
  if(zoomDat)
  {
    dat$valence <- dat$valence/7
    dat$arousal <- dat$arousal/100
  }
  
  
  message("Order of levels in chosenLvls argument matters.
          Only two levels accepted, first plotted as 'dot',
          second as text")
  
  
  if(!missing(piecesToJitter))
  {
    message("Jittering ratings for", piecesToJitter)
    for(thisPiece in piecesToJitter)
    {dat$valence[dat$key == thisPiece] <- jitter(dat$valence[dat$key == thisPiece])
    dat$arousal[dat$key == thisPiece] <- jitter(dat$arousal[dat$key == thisPiece])}
    
  }
  
  message("Using point for ", chosenLvls[2], "\n and text for ", chosenLvls[1])
  
  dat %>% 
    ggplot2::ggplot(aes(valence,arousal, color=mode, label = key))+ 
    ggplot2::geom_hline(yintercept = (50.5/100))+
    ggplot2::geom_vline(xintercept = (4/7))+
    ggplot2::theme_classic()+
    ggplot2::labs(x="Valence",y="Arousal")+
    ggplot2::theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()) -> circumPlot
  
    circumPlot <- circumPlot + 
      ggplot2::geom_line(aes(group = key, color=mode), size = 1, alpha = 0.4, linetype = 1) +
      ggplot2::geom_text(aes(x = valence, y = arousal),size=4,
                data = subset(dat, groupCol == chosenLvls[1]), colour = 'grey60', alpha = 0.7)#+
    # ylim(1,100)+xlim(1,7)
 
  if(style == 'mp') {
    circumPlot +
      geom_point(
        data = subset(dat, groupCol == chosenLvls[2]), alpha = 0.5) +
      ggplot2::scale_shape_manual(values = c(1, 10))+
      ggplot2::scale_colour_manual(values = c("firebrick", "dodgerblue1"))
  } else {
    circumPlot +
      geom_point(data = subset(dat, groupCol == chosenLvls[2]), alpha = 0.5)#+
  }
}

# Bootstrap Differences ---------------------------------------------------

# Step 1: For each condition, randomly sample 30 ratings (with replacement) for 
## a given piece. Calculate mean absolute difference for valence and arousal, 
## along with Euclidean distance 
# Step 2: After calculating summary statistics on individual pieces, ungroup
## data and calculate coefficient of variation across all sampled pieces.
## Steps 1 and 2 give us the results of 1 virtual experiment.
# Step 3: Repeat this process for 10,000 virtual experiments
# Step 4: Calculate bootstrap confidence intervals for all 10,000 replications.

.randomDifferenceRating <- function(pieceToSample, DF1, DF2, sampleSize = 30)
{
  # we need fixed index numbers to sample the rows corresponding to pieceID of interest
  DF1$index = 1:nrow(DF1)
  DF2$index = 1:nrow(DF2)
  
  # first, randomly sample one row number in first dataframe where pieceID is pieceID of interest
  thisRowDF1 = sample(as.numeric(DF1$index[DF1$pieceID == pieceToSample]), size = sampleSize, replace = T)
  # now do same for second dataframe
  thisRowDF2 = sample(as.numeric(DF2$index[DF2$pieceID == pieceToSample]), size = sampleSize, replace = T)
  # show this row in dataframe 1
  rowDF1 = DF1[thisRowDF1,]
  #print(rowDF1)
  rowDF2 = DF2[thisRowDF2,]
  #print(rowDF2)
  # now take squared difference of valence and arousal between df1 and df2
  
  valenceA <- mean(as.numeric(rowDF1$valence))
  valenceB <- mean(as.numeric(rowDF2$valence))
  arousalA <- mean(as.numeric(rowDF1$arousal))
  arousalB <- mean(as.numeric(rowDF2$arousal))
  valenceDiff <- (mean(as.numeric(rowDF1$valence)) - mean(as.numeric(rowDF2$valence)))
  arousalDiff <- (mean(as.numeric(rowDF1$arousal)) - mean(as.numeric(rowDF2$arousal)))

  # return values as dataframe
  
  returnValues <- data.frame(valenceDiff, arousalDiff, euclidDiff, pieceID = pieceToSample,
                            valenceA, valenceB, arousalA, arousalB)
  
  return(returnValues)
}

.collapseSampleDifferenceArray <- function(dat)
{
  dat <- dat %>% 
    dplyr::group_by(pieceID) %>% 
    dplyr::summarize(valenceDiff = mean(valenceDiff),
              arousalDiff = mean(arousalDiff),
              euclidDiff = mean(euclidDiff),
              valenceA = valenceA,
              valenceB = valenceB,
              arousalA = arousalA,
              arousalB = arousalB)
  dat <- dat %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(valenceA_cv = .getCoefficientofVariation(valenceA),
           valenceB_cv = .getCoefficientofVariation(valenceB),
           arousalA_cv = .getCoefficientofVariation(arousalA),
           arousalB_cv = .getCoefficientofVariation(arousalB))
  return(dat)
}

# applies .randomDifferenceRating across a vector of randomly sampled (with replacement) pieceIDs 
.sampleDifferenceArray <- function(DF1, DF2, collapseRatings = T, sampleSize = 30)
{
  pieceSample <- unique(as.character(DF1$pieceID))
  
  # apply function across a vector of randomly sampled piece IDs called "pieceSample"
  
  differenceSample <- sapply(pieceSample, function(x){#message("sampling ",x); 
    .randomDifferenceRating(pieceToSample = as.character(x), DF1 = DF1, DF2 = DF2, 
                            sampleSize = sampleSize)},
    simplify = F)
  
  differenceSample <- purrr::map_df(differenceSample, ~as.data.frame(.x), .id="pieceID")
  # transpose it for dataframe formatting
  
  if(collapseRatings) return(.collapseSampleDifferenceArray(differenceSample))
  
  # return formatted dataframe
  else return(differenceSample)
}

bootstrapCircumplexDifference <- function(DF1, DF2, replications = 1000,
                                         returnSummary = F, returnPlot = T,
                                         style = 'mp', scaleVars = T,
                                         sampleSize = 30,
                                         xlimsVal = c(-1.3,1.3),
                                         ylimsVal = c(-1.3,1.3),
                                         xlimsAro = c(-1.3,1.3),
                                         ylimsAro = c(-1.3,1.3),
                                         seed = 1) {
  # for reproducibility:
  set.seed(1)
  # first scale and center data
  if(scaleVars)
  {
    # this is to scale data across groups: add column so we can subset them later
    DF1$tempID = 'a'
    DF2$tempID = 'b'
    boundDat = rbind(DF1, DF2)
    boundDat$valence = as.numeric(scale(boundDat$valence))
    boundDat$arousal = as.numeric(scale(boundDat$arousal))
    DF1 = subset(boundDat, tempID == 'a'); DF1$tempID = NULL
    DF2 = subset(boundDat, tempID == 'b'); DF2$tempID = NULL
    
    message("Scaled valence and arousal values")
  }
  message("Performing bootstrap distance calculations with ", replications, " replications")
  # apply .sampleDifferenceArray function to pieceSample and repeat this process 1000 times
  bootFrame <- pbapply::pbreplicate(n = replications, 
                                   expr = .sampleDifferenceArray(DF1 = DF1, 
                                                                 DF2 = DF2, 
                                                                 collapseRatings = T,
                                                                 sampleSize = sampleSize), simplify = F)
  #format as dataframe
  bootFrame <- do.call('rbind', bootFrame)
  
  
  # summarize data:
  bootFrame %>% 
    dplyr::group_by(pieceID) %>% 
    dplyr::summarize(valenceDist = mean(valenceDiff),
              arousalDist = mean(arousalDiff),
              valenceLCI = quantile(valenceDiff, 0.025),
              valenceUCI = quantile(valenceDiff, 0.975),
              arousalLCI = quantile(arousalDiff, 0.025),
              arousalUCI = quantile(arousalDiff, 0.975),
              valenceSig = (sign(valenceLCI) == sign(valenceUCI)),
              arousalSig = (sign(arousalLCI) == sign(arousalUCI)),
              valenceA = mean(valenceA),
              valenceB = mean(valenceB),
              arousalA = mean(arousalA),
              arousalB = mean(arousalB)) -> bootSumm
  # create variable for mode
  bootSumm$mode <- substr(bootSumm$pieceID, 1, 1)
  # rename mode to "Major" and "minor"
  bootSumm$mode <- forcats::fct_recode(bootSumm$mode, Major = 'M', minor = 'm')
  
  bootFrame %>% 
    dplyr::mutate(#euclideanDist = sqrt((valenceDiff^2) + (arousalDiff^2)),
      valenceDiffAbs = abs(valenceDiff),
      arousalDiffAbs = abs(arousalDiff)) -> bootFrame
  if(returnSummary) return(bootSumm)
  else return(bootFrame)
  
}

summarizeBootCircumplexDiffs <- function(bootData) {
  bootData %>% 
    dplyr::group_by(pieceID) %>% 
    dplyr::summarize(valenceDist = mean(valenceDiff),
              arousalDist = mean(arousalDiff),
              valenceLCI = quantile(valenceDiff, 0.025),
              valenceUCI = quantile(valenceDiff, 0.975),
              arousalLCI = quantile(arousalDiff, 0.025),
              arousalUCI = quantile(arousalDiff, 0.975),
              valenceSig = (sign(valenceLCI) == sign(valenceUCI)),
              arousalSig = (sign(arousalLCI) == sign(arousalUCI))
    ) -> bootSumm  
  return(bootSumm)
}



plotBootCircumplexDiffs <- function(bootSumm, style = 'mp',
                                   xlimsVal = c(-1.3,1.3),
                                   ylimsVal = c(-1.3,1.3),
                                   xlimsAro = c(-1.3,1.3),
                                   ylimsAro = c(-1.3,1.3)) {
  
  # create variable for mode
  bootSumm$mode <- substr(bootSumm$pieceID, 1, 1)
  # rename mode to "Major" and "minor"
  bootSumm$mode <- forcats::fct_recode(bootSumm$mode, Major = 'M', minor = 'm')
  # prepare plot for valence significant differences
  subset(prettyKeySig(nameKeys(bootSumm)), bootSumm$valenceSig == T
  ) %>%
    mutate(mode = forcats::fct_rev(mode)) %>%
    ggplot2::ggplot(., aes(x = valenceDist, y = arousalDist, 
                  xmin = valenceLCI, xmax = valenceUCI,
                  ymin = arousalLCI, ymax = arousalUCI,
                  label = key, color = mode))+
    ggplot2::geom_errorbarh(alpha = 0.6, size = 1.1)+
    ggplot2::geom_point(alpha = 0.6)+
    ggplot2::geom_vline(xintercept = 0, linetype = 'dashed', colour = 'grey50')+
    ggplot2::ylim (ylimsVal[1], ylimsVal[2])+
    ggplot2::xlim(xlimsVal[1], xlimsVal[2])+
    ggplot2::labs(x = "Valence Difference", 
         y = "", 
         title = "Valence")+
    ggplot2::theme_classic() -> diffSummaryVal
  
  subset(prettyKeySig(nameKeys(bootSumm)), bootSumm$arousalSig == T) %>% 
    dplyr::mutate(mode = forcats::fct_rev(mode)) %>%
    ggplot2::ggplot(., aes(x = valenceDist, y = arousalDist, 
                  xmin = valenceLCI, xmax = valenceUCI,
                  ymin = arousalLCI, ymax = arousalUCI,
                  label = key, color = mode))+
    ggplot2::geom_point(alpha = 0.6)+
    ggplot2::geom_errorbar(alpha = 0.6, size = 1.1)+
    ggplot2::geom_hline(yintercept = 0, linetype = 'dashed', colour = 'grey50')+
    ggplot2::ylim(ylimsAro[1], ylimsAro[2])+
    ggplot2::xlim(xlimsAro[1], xlimsAro[2])+
    ggplot2::labs(x = "", 
         y = "Arousal Difference", 
         title = "Arousal")+
    ggplot2::theme_classic() -> diffSummaryAro
  
  if(style == 'mp')
  {
    diffSummaryVal <- diffSummaryVal +
      ggplot2::scale_colour_manual(values = c("firebrick", "dodgerblue1"))
    diffSummaryAro <- diffSummaryAro +
      ggplot2::scale_colour_manual(values = c("firebrick", "dodgerblue1"))
  }
  
  
  gridExtra::grid.arrange(diffSummaryAro, diffSummaryVal, ncol = 2)
  
  return(bootSumm)
}





