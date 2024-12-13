# Pre-processing ----------------------------------------------------------

# Set seed for simulations

set.seed(123)

# For writing pretty key signatures ---------------------------------------

flatSym <- '\\u266d'
sharpSym <- '\\u266f'


# Measure differences between conditions ----------------------------------

bootData$changeValence = .getPercentChange(bootData$valenceA, bootData$valenceB)
bootData$changeArousal = .getPercentChange(bootData$arousalA, bootData$arousalB)
bootData %>%
  dplyr::group_by(pieceID) %>%
  dplyr::summarize(changeValenceMean = mean(changeValence),
            changeValenceLCI = quantile(changeValence, 0.025),
            changeValenceUCI = quantile(changeValence, 0.975),
            changeArousalMean = mean(changeArousal),
            changeArousalLCI = quantile(changeArousal, 0.025),
            changeArousalUCI = quantile(changeArousal, 0.975)
  ) -> bootPlotDF


bootPlotDF$valenceChangeSig = sign(bootPlotDF$changeValenceUCI) == sign(bootPlotDF$changeValenceLCI)
bootPlotDF$arousalChangeSig = sign(bootPlotDF$changeArousalUCI) == sign(bootPlotDF$changeArousalLCI)
bootPlotDF$mode = substr(bootPlotDF$pieceID, 1, 1)
bootPlotDF$composer = substr(bootPlotDF$pieceID, nchar(bootPlotDF$pieceID), nchar(bootPlotDF$pieceID))
bootPlotDF$mode = as.factor(bootPlotDF$mode); levels(bootPlotDF$mode) = c('Minor', "Major")
bootPlotDF$composer = as.factor(bootPlotDF$composer); levels(bootPlotDF$composer) = c('Bach', "Chopin")



# Data assembly -----------------------------------------------------------
# Take expressive and deadpan experiments from full data set:

bach <- subset(emoData, expID %in% c(141, 142))
chop <- subset(emoData, expID %in% c(155, 137))

bach$key <- prettyKeyCol(bach$key)
chop$key <- prettyKeyCol(chop$key)

# remove this if errors produced (calls C# piece Db for consistency with score)

#chop$key[chop$key == eval(parse(text=paste0("'", 'C', sharpSym, "'")))] <- 
#  eval(parse(text=paste0("'", 'D', flatSym, "'")))
# divide into expressive and deadpan data sets and declare conditions

## Chopin:

chopE = subset(chop, expID == '155')
#chopE$condition = 'expressive'
#chopE = subset(chopE, participant %in% unique(chopE$participant)[1:30])


chopD <- subset(chop, expID == '137')
#chopD$condition = 'deadpan'
#chopD = subset(chopD, participant %in% unique(chopD$participant)[1:30])

## Bach:

bachE <- subset(bach, expID == '141')
#bachE$condition = 'expressive'
#bachE = subset(bachE, participant %in% unique(bachE$participant)[1:32])
# remove participants who didn't use full range of scale
#bachE = subset(bachE, !participant %in% c('18','21'))

bachD <- subset(bach, expID == '142')
#bachD$condition = 'deadpan'
#bachD = subset(bachD, participant %in% unique(bachD$participant)[1:30])

# collect deadpan ratings
deadpan <- rbind(chopD, bachD)
# collect expressive ratings
expressive <- rbind(chopE, bachE)
# collect chopin ratings
chopConditions <- rbind(chopD, chopE)
# collect bach ratings
bachConds <- rbind(bachD, bachE)
# collect all ratings
fullDat <- rbind(deadpan, expressive)
# replace participant IDs with unique values
paste0(fullDat$participant, 
       substr(fullDat$composer,1,1), 
       substr(fullDat$condition,1,1)) -> pptUnique

fullDat$participant = pptUnique

# Get legend for correlation by condition figure --------------------------

dummyDat <- data.frame(x=c(1,2),y=c(1,3), z = c('Major','minor'), w = c('Bach','Chopin'), v = c('Regression', 'Unity'))

ggplot2::ggplot(data = dummyDat, aes(x=x,y=y,color = z, shape = w, linetype = v))+
  ggplot2::geom_point()+
  ggplot2::theme(legend.position = 'top')+
  ggplot2::geom_abline(color = 'black')+
  ggplot2::geom_smooth(color = 'grey50')+
  ggplot2::scale_linetype_manual(name = 'Line',
                        breaks= c('Unity', 'Regression'),
                        values = c(9,1))+
  ggplot2::scale_color_manual(name='Mode',
                     breaks=c('Major', 'minor'),
                     values=c('Major'='firebrick1', 'minor'='dodgerblue2'))+
  ggplot2::scale_shape_manual(name='Composer',
                     breaks=c('Bach', 'Chopin'),
                     values=c(16,17)) -> correlationLegendManual

correlationLegend = cowplot::get_plot_component(correlationLegendManual, 
                                                'guide-box-top', 
                                                return_all = TRUE)

# Get legend for percent correlation figure -------------------------------

ggplot2::ggplot(data = dummyDat, aes(x=x,y=y,color = z, shape = w))+
  ggplot2::geom_point(aes(x=1,y=1))+
  ggplot2::theme(legend.position = 'top')+
  ggplot2::scale_color_manual(name='Mode',
                     breaks=c('Major', 'minor'),
                     values=c('Major'='firebrick1', 
                              'minor'='dodgerblue2'))+
  ggplot2::scale_shape_manual(name='Composer',
                     breaks=c('Bach', 'Chopin'),
                     values=c(16,17)) -> percentLegendManual


percentLegend = cowplot::get_plot_component(percentLegendManual, 'guide-box-top', return_all = TRUE)

# Layout matrices ---------------------------------------------------------

# fig1Layout <- rbind(
#              c(2,2,2),
#              c(2,2,2),              
#              c(2,2,2),
#              c(3,3,3),
#              c(3,3,3),
#              c(3,3,3),
#             c(1,1,1))


circLayout <- rbind(
  rep(1, each = 8),
  rep(c(2,3), each = 6),
  rep(c(2,3), each = 6),
  rep(c(2,3), each = 6),
  rep(c(2,3), each = 6),
  rep(c(2,3), each = 6),
  rep(c(2,3), each = 6)
)


# corrSigLayout <-  rbind(
#                         c(2,2,2,2,3,3,3,3),
#                         c(2,2,2,2,3,3,3,3),              
#                         c(2,2,2,2,3,3,3,3),
#                         c(2,2,2,2,3,3,3,3),
#                         c(2,2,2,2,3,3,3,3),
#                         c(2,2,2,2,3,3,3,3),
#                         c(1,1,1,1,1,1,1,1))


corrSigLayout <-  rbind(
                        rep(c(2,3,5,4), times = c(6,4,4,6)),
                        rep(c(2,3,5,4), times = c(6,4,4,6)),              
                        rep(c(2,3,5,4), times = c(6,4,4,6)),
                        rep(c(2,3,5,4), times = c(6,4,4,6)),
                        rep(c(2,3,5,4), times = c(6,4,4,6)),
                        rep(c(2,3,5,4), times = c(6,4,4,6)),
                        rep(1, times = 20)
                        )


pctCorLay = rbind(
  c(2,3,4),
  c(2,3,4),
  c(2,3,4),
  c(5,6,7),
  c(5,6,7),
  c(5,6,7),
  c(1,1,1))

# Bootstrap Significance Plots --------------------------------------------

# make duplicate for plot
panel2Plot = data.frame(bootPlotDF)
# name key signature
panel2Plot$key = nameKeysSimple(substr(panel2Plot$pieceID, 1, nchar(panel2Plot$pieceID)-1))

# replace c# with c#/db for panel2plot
panel2Plot$key = as.factor(prettyKeyCol(panel2Plot$key))


levels(panel2Plot$key)[which(levels(panel2Plot$key) == 
                               eval(parse(text=paste0("'", 'C', sharpSym, "'")
                                          )
                                    ))] <- eval(
                                      parse(text=paste0("'", 'C', 
                                                        sharpSym, '/D',  
                                                        flatSym, "'")))

# initialize column to colour mode
panel2Plot$modeColVal = 'n'
# now distinguish significant colours for valence
panel2Plot$modeColVal[panel2Plot$valenceChangeSig == T] <- panel2Plot[panel2Plot$valenceChangeSig == T,]$mode
panel2Plot$modeColAro = 'n'
# and arousal
panel2Plot$modeColAro[panel2Plot$arousalChangeSig == T] <- panel2Plot[panel2Plot$arousalChangeSig == T,]$mode

subset(panel2Plot) %>%
  ggplot(aes(#xmin = changeValenceLCI, xmax = changeValenceUCI, 
    #x = changeValenceMean,
    y = key, label = key,
    x = changeValenceMean,
    xmin = changeValenceLCI, xmax = changeValenceUCI,
    alpha = valenceChangeSig,
    color = modeColVal#, shape = composer
  ))+
  xlim(-40, 60)+
  #geom_col()#+
  geom_vline(xintercept = 0, colour = 'grey', linetype= 2)+
  geom_errorbarh(position = position_dodge(width = 0.5))+
  geom_point(position = position_dodge(width = 0.5), aes(shape = composer))+
  #geom_text(color = 'black', position = position_dodge(width = 2))+
  theme_classic()+
  labs(x = '% Difference', y = 'Piece')+
  scale_colour_manual(values = c('dodgerblue', 'firebrick2', 'grey'),
                      name = 'Mode', labels = c('Major', 'Minor', ""))+
  scale_alpha_manual(values = c(0.6, 1), name = 'Difference', labels = c('Nonsignificant', 'Significant'))+
  scale_shape_manual(values = c(16,17), name = 'Composer')+
  theme(legend.position = 'none')+
  facet_grid(cols = vars(composer))-> bs1

subset(panel2Plot) %>%
  ggplot(aes(#xmin = changeValenceLCI, xmax = changeValenceUCI, 
    #x = changeValenceMean,
    y = key, label = key,
    x = changeArousalMean,
    xmin = changeArousalLCI, xmax = changeArousalUCI,
    alpha = arousalChangeSig,
    color = modeColAro#, shape = composer
  ))+
  xlim(-65, 80)+
  #geom_col()#+
  geom_vline(xintercept = 0, colour = 'grey', linetype= 2)+
  geom_errorbarh(position = position_dodge(width = 0.5))+
  geom_point(position = position_dodge(width = 0.5), aes(shape = composer))+
  #geom_text(color = 'black', position = position_dodge(width = 2))+
  theme_classic()+
  labs(x = '% Difference', y = 'Piece')+
  scale_colour_manual(values = c('dodgerblue', 'firebrick2', 'grey'),
                      name = 'Mode', labels = c('Major', 'Minor', ""))+
  scale_alpha_manual(values = c(0.6, 1), name = 'Difference', labels = c('Nonsignificant', 'Significant'))+
  scale_shape_manual(values = c(16,17), name = 'Composer')+
  theme(legend.position = 'none')+
  scale_y_discrete(position = 'right')+
  facet_grid(cols = vars(composer))-> bs2





