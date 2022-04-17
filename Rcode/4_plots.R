########################################################################
######################## PLOT RESULTS ##################################
########################################################################
  
#needed: fin1 from script 3_analysis  

#####################
#EXTRAXT PREDICTIONS AND RESIDUALS FROM FINAL MODEL
#####################              

    #dataframe with residuals and predictions on all levels
    #adds and dv variable adding whether residual is from str data or scr data
    #computes number of observations - number of missing values and adds 1 for STR and 2 for SCR 
      #(note: this has nothing to do with delta, but with which dv-df is first in the multivariat df!)
        results = data.frame(res0 = resid(fin1, level=0),
                             res1 = resid(fin1, level=1),
                             fit0 = predict(fin1, level=0),
                             fit1 = predict(fin1, level=1),
                             dv = rep(1:2, times=c(N.obs.str-N.mis.str,N.obs.scr-N.mis.scr)))
      
    #add into new dataframes for each dv as new columns
      #str
          str.fit = mutate(na.omit(str.multi), 
                  res0 = filter(results, dv == 1)$res0,   #residuals level 0 for str
                  res1 = filter(results, dv == 1)$res1,   #residuals level 1 for str
                  fit0 = filter(results, dv == 1)$fit0,    #fitted level 0 values str
                  fit1 = filter(results, dv == 1)$fit1)    #fitted level 1 values str
      #scr
          scr.fit = mutate(na.omit(scr.multi), 
                   res0 = filter(results, dv == 2)$res0,    #residuals level 0 for scr
                   res1 = filter(results, dv == 2)$res1,    #residuals level 1 for scr
                   fit0 = filter(results, dv == 2)$fit0,    #fitted level 0 values scr
                   fit1 = filter(results, dv == 2)$fit1)    #fitted level 1 values scr
          
        
###############################################
#ASSUMPTIONS FINAL MODEL
###############################################
        
    #overall look at residuals (both dv):
        plot(results$fit1, results$res1); abline(0,0)
        #qqnorm(results$res1); qqline(results$res1)
  
    #Linearity and Homoscedasticity 
      #residual vs. fitted plot for each dv
          plot(str.fit$fit0, str.fit$res0, col = "#00305E", pch = 1, cex=0.8, bty="l", cex.lab = 1.2,cex.axis = 1.2,
               xlab="Angepasste Werte (Ebene 1)", ylab="Residuen (Ebene 1)", abline(0,0))  
          plot(str.fit$fit1, str.fit$res1, col = "#00305E", pch = 1, cex=0.8, bty="l", cex.lab = 1.2,cex.axis = 1.2,
               xlab="Angepasste Werte (Ebene 2)", ylab="Residuen (Ebene 2)", abline(0,0)) 
          plot(scr.fit$fit0, scr.fit$res0, col = "#00305E", pch = 1, cex=0.8, bty="l", cex.lab = 1.2,cex.axis = 1.2,
               xlab="Angepasste Werte (Ebene 1)", ylab="Residuen (Ebene 1)", abline(0,0)) 
          plot(scr.fit$fit1, scr.fit$res1, col = "#00305E", pch = 1, cex=0.8, bty="l", cex.lab = 1.2,cex.axis = 1.2,
               xlab="Angepasste Werte (Ebene 2)", ylab="Residuen (Ebene 2)", abline(0,0)) #wichtiger! default und vp Ebene
        
    #Normality of Residuals
      #QQplot for each dv and histogram of residuals
          qqnorm(str.fit$res0, col = "#00305E", pch = 1, cex=0.8, bty="l",cex.lab = 1.2,cex.axis = 1.2,main = NULL,
                 xlab="Theoretische Quantile", ylab="Beobachtete Quantile"); qqline(str.fit$res0)
          qqnorm(str.fit$res1, col = "#00305E", pch = 1, cex=0.8, bty="l",cex.lab = 1.2,cex.axis = 1.2,main = NULL,
                 xlab="Theoretische Quantile", ylab="Beobachtete Quantile"); qqline(str.fit$res1) 
          qqnorm(scr.fit$res0, col = "#00305E", pch = 1, cex=0.8, bty="l",cex.lab = 1.2,cex.axis = 1.2,main = NULL,
                 xlab="Theoretische Quantile", ylab="Beobachtete Quantile"); qqline(scr.fit$res0)
          qqnorm(scr.fit$res1, col = "#00305E", pch = 1, cex=0.8, bty="l",cex.lab = 1.2,cex.axis = 1.2,main = NULL,
                 xlab="Theoretische Quantile", ylab="Beobachtete Quantile"); qqline(scr.fit$res1)
          
          hist(str.fit$res0, breaks=30, col = "#00305E", bty="l", cex.lab = 1.2,cex.axis = 1.2,main = NULL,
               xlab="Residuen (Ebene 1)", ylab="Häufigkeit")
          hist(str.fit$res1, breaks=30, col = "#00305E", bty="l", cex.lab = 1.2,cex.axis = 1.2,main = NULL,
               xlab="Residuen (Ebene 2)", ylab="Häufigkeit")
          hist(scr.fit$res0, breaks=30, col = "#00305E", bty="l", cex.lab = 1.2,cex.axis = 1.2,main = NULL,
               xlab="Residuen (Ebene 1)", ylab="Häufigkeit")
          hist(scr.fit$res1, breaks=30, col = "#00305E", bty="l", cex.lab = 1.2,cex.axis = 1.2,main = NULL,
               xlab="Residuen (Ebene 2)", ylab="Häufigkeit")
        
      #Kernel Density Plots for residuals (each dv)
        plot(density(str.fit$res1), main='Residual KDE Plot',xlab='Residual value')
        plot(density(scr.fit$res1), main='Residual KDE Plot',xlab='Residual value')
      

###############################################
#PLOTTING THE RANDOM EFFECTS FINAL
###############################################
    #extract random effects
        re = ranef(fin1)
        names(re) = c('SCR0','STR0','SCR4','STR4')
        
    #deviations of random effects from Grand Intercept scaled as standard deviations
        
      #STR Intercept
       re %>% mutate(vp = rownames(re), 
                      sc = scale(re$STR0)) %>%
          ggplot(aes(x = fct_reorder(vp, STR0), y = sc)) + 
          coord_cartesian(ylim = c(-3.1, 3.1)) + scale_y_continuous(breaks=c(-3, -2, -1, 0, 1, 2, 3))+
          geom_point(stat="identity", size=1.5, colour = "grey30") +
          labs(x="Versuchspersonen", y="Abweichung (in SD) vom mittleren Intercept") +
          geom_hline(yintercept=0, size=0.8, alpha=0.5, color="#00305E") +
         scale_x_discrete(labels=c("1","","","","5","","","","","10","","","","","15","",
                                   "","","","20","","","","","25","","","","","30","",
                                   "","","","35","","", "38"))+
         theme(panel.background = element_blank(),
               axis.ticks = element_blank(), axis.text.x=element_text(size=14),
               axis.text.y=element_text(size=16), axis.ticks.y = element_line(size = 0.2),
               axis.line = element_line(colour = "grey30"),
               axis.title.x = element_text(size=18, colour = "black", family="CMUSansSerif"),
               axis.title.y = element_text(size=18, colour = "black", family="CMUSansSerif"))
        
      #SCR Intercept
         re %>% mutate(vp = rownames(re), 
                      sc = scale(re$SCR0)) %>%
          ggplot(aes(x = fct_reorder(vp, SCR0), y = sc)) + 
          coord_cartesian(ylim = c(-3.1, 3.1)) + scale_y_continuous(breaks=c(-3, -2, -1, 0, 1, 2, 3))+
          geom_point(stat="identity", size=1.5, colour = "grey30") +
          labs(x="Versuchspersonen", y="Abweichung (in SD) vom mittleren Intercept") +
          geom_hline(yintercept=0, size=0.8, alpha=0.5, color="#00305E") +
         scale_x_discrete(labels=c("1","","","","5","","","","","10","","","","","15","",
                                   "","","","20","","","","","25","","","","","30","",
                                   "","","","35","","", "38"))+
         theme(panel.background = element_blank(),
               axis.ticks = element_blank(), axis.text.x=element_text(size=14),
               axis.text.y=element_text(size=16), axis.ticks.y = element_line(size = 0.2),
               axis.line = element_line(colour = "grey30"),
               axis.title.x = element_text(size=18, colour = "black", family="CMUSansSerif"),
               axis.title.y = element_text(size=18, colour = "black", family="CMUSansSerif"))
        
      #STR Interaktion (CSxlin)
        re %>% mutate(vp = rownames(re), 
                      sc = scale(re$STR4)) %>%
          ggplot(aes(x = fct_reorder(vp, STR0), y = sc)) + 
          coord_cartesian(ylim = c(-3.1, 3.1)) + scale_y_continuous(breaks=c(-3, -2, -1, 0, 1, 2, 3))+
          geom_point(stat="identity", size=1.5, colour = "grey30") +
          labs(x="Versuchspersonen", y="Abweichung (in SD) von mittlerer Interaktion") +
          geom_hline(yintercept=0, size=0.8, alpha=0.5, color="#00305E") +
          scale_x_discrete(labels=c("1","","","","5","","","","","10","","","","","15","",
                                    "","","","20","","","","","25","","","","","30","",
                                    "","","","35","","", "38"))+
          theme(panel.background = element_blank(),
                axis.ticks = element_blank(), axis.text.x=element_text(size=14),
                axis.text.y=element_text(size=16), axis.ticks.y = element_line(size = 0.2),
                axis.line = element_line(colour = "grey30"),
                axis.title.x = element_text(size=18, colour = "black", family="CMUSansSerif"),
                axis.title.y = element_text(size=18, colour = "black", family="CMUSansSerif"))
        
      #SCR Interaktion (CSxlin)
        re %>% mutate(vp = rownames(re), 
                      sc = scale(re$SCR4)) %>%
          ggplot(aes(x = fct_reorder(vp, SCR0), y = sc)) + 
          coord_cartesian(ylim = c(-3.1, 3.1)) + scale_y_continuous(breaks=c(-3, -2, -1, 0, 1, 2, 3))+
          geom_point(stat="identity", size=1.5, colour = "grey30") +
          labs(x="Versuchspersonen", y="Abweichung (in SD) von mittlerer Interaktion") +
          geom_hline(yintercept=0, size=0.8, alpha=0.5, color="#00305E") +
          scale_x_discrete(labels=c("1","","","","5","","","","","10","","","","","15","",
                                    "","","","20","","","","","25","","","","","30","",
                                    "","","","35","","", "38"))+
          theme(panel.background = element_blank(),
                axis.ticks = element_blank(), axis.text.x=element_text(size=14),
                axis.text.y=element_text(size=16), axis.ticks.y = element_line(size = 0.2),
                axis.line = element_line(colour = "grey30"),
                axis.title.x = element_text(size=18, colour = "black", family="CMUSansSerif"),
                axis.title.y = element_text(size=18, colour = "black", family="CMUSansSerif"))
        
  #################
  #PLOT: each participant’s individual estimate with the grand estimate as coloured column
  #################
    #extract fixed effects
      fe = fixef(fin1)
    #create new df with new columns computing vp-estimate & restrucure
      re.raw = re %>% mutate(vp = rownames(re), 
                             c.int = fe[["c0"]]+re$SCR0, 
                             t.int = fe[["t0"]]+re$STR0,
                             c.cslin = fe[["c4"]]+re$SCR4)
                             #t.cslin = fe[["t4"]]+re$STR4     ## geht nicht weil kann fixed effect für t4 uff!!
        re.raw$vp = as.character(re.raw$vp)
        re.raw[nrow(re.raw) + 1,] = c(0,0,0,0,"FE", fe[["c0"]],fe[["t0"]], fe[["c4"]])
        re.raw$vp = as.factor(re.raw$vp); re.raw$c.int = as.numeric(re.raw$c.int)
        re.raw$t.int = as.numeric(re.raw$t.int); re.raw$c.cslin = as.numeric(re.raw$c.cslin)
        
      #plot for SCR Intercept
        ggplot(data=re.raw, aes(x=fct_reorder(vp, c.int), y=c.int, fill=fct_other(vp, keep="FE", other_level="Name"))) +
          geom_bar(stat="identity") +
          labs(x="Versuchspersonen (grau) & Mittelwert (blau)", y="Geschätzte Intercepts (SCR)") +
          scale_fill_manual(values = c(FE="#00305E", Name="grey68"), guide="none") +
          scale_x_discrete(labels=c("1","","","","5","","","","","10","","","","","15","",
                                    "","","","20","","","M","","","25","","","","","30","",
                                    "","","","35","","", "38"))+
          theme(panel.background = element_blank(),
                axis.ticks = element_blank(), axis.text.x=element_text(size=14),
                axis.text.y=element_text(size=16), axis.ticks.y = element_line(size = 0.2),
                axis.line = element_line(colour = "grey30"),
                axis.title.x = element_text(size=18, colour = "black", family="CMUSansSerif"),
                axis.title.y = element_text(size=18, colour = "black", family="CMUSansSerif"))
        
      #plot for STR Intercept
        ggplot(data=re.raw, aes(x=fct_reorder(vp, t.int), y=t.int, fill=fct_other(vp, keep="FE", other_level="Name"))) +
          geom_bar(stat="identity") +
          labs(x="Versuchspersonen (grau) & Mittelwert (blau)", y="Geschätzte Intercepts (STR)") +
          scale_fill_manual(values = c(FE="#00305E", Name="grey68"), guide="none") +
          scale_x_discrete(labels=c("1","","","","5","","","","","10","","","","","15","",
                                    "","","","20","","","","","25","","","M","","","30","",
                                    "","","","35","","", "38"))+
          theme(panel.background = element_blank(),
                axis.ticks = element_blank(), axis.text.x=element_text(size=14),
                axis.text.y=element_text(size=16), axis.ticks.y = element_line(size = 0.2),
                axis.line = element_line(colour = "grey30"),
                axis.title.x = element_text(size=18, colour = "black", family="CMUSansSerif"),
                axis.title.y = element_text(size=18, colour = "black", family="CMUSansSerif"))
   
      #plot for SCR Interaktion
        ggplot(data=re.raw, aes(x=fct_reorder(vp, c.cslin), y=c.cslin, fill=fct_other(vp, keep="FE", other_level="Name"))) +
            geom_bar(stat="identity") +
            labs(x="Versuchspersonen (grau) & Mittelwert (blau)", y="Geschätzte Interaktion CSxlin (SCR)") +
            scale_fill_manual(values = c(FE="#00305E", Name="grey68"), guide="none") +
            theme(panel.background = element_blank(),
                  axis.text.x = element_blank(), axis.ticks = element_blank(),
                  axis.text.y=element_text(size=16), axis.ticks.y = element_line(size = 0.2),
                  axis.line = element_line(colour = "grey30"),
                  axis.title.x = element_text(size=18, colour = "black", family="CMUSansSerif"),
                  axis.title.y = element_text(size=18, colour = "black", family="CMUSansSerif"))
  
    #pairwise Scatterplot of random effects 
        pairs(re, upper.panel = NULL, col = "#00305E", pch = 16, cex=1.2,
            labels = expression("Intercept SCR", "Intercept STR", "Interaktion SCR", "Interaktion STR"))
  
###############################################
#PLOTTING MODELFIT STR
###############################################       

 str.fit %>% dplyr::group_by(stimtype, trialshort) %>%
                               dplyr::summarize(.groups="keep", 
                                         response = mean(response),
                                         fit0= mean(fit0),
                                         fit1 = mean(fit1)) %>% 
                                ungroup() %>%
        ggplot(aes(x=trialshort, y=response, color=stimtype)) + 
          coord_cartesian(xlim=c(1,17), ylim = c(40, 115))+
          geom_vline(xintercept = 8.5, linetype="dashed", color = "grey48", size=0.2)+ 
          geom_line(aes(group=stimtype), alpha=0.2)+
          geom_point(aes(shape=stimtype), alpha=0.2, size=2)+
          geom_line(aes(y = fit0, group=stimtype))+
          geom_point(aes(y = fit0, shape=stimtype), size=2)+
          scale_color_manual(values = c("#00AFF4", "#E46B0C"))+
          scale_shape_manual(values = c(15, 16))+
          labs(x="Trial", y="Schreckreaktion in µV") + 
          annotate("text", x=16.75, y=52, label="CS+", family="CMUSansSerif", 
                   color = "#E46B0C", fontface=2, size = 16 / .pt)+
          annotate("text", x=16.7, y=46, label="CS–", family="CMUSansSerif", 
                   color = "#00AFF4", fontface=2, size = 16/ .pt)+
          annotate("text", x=8.2, y=100, label="Instruktion", family="CMUSansSerif", color = "grey48", size = 16 / .pt, angle = 90)+
          scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16))+
          theme(panel.background = element_blank(), legend.position="none",
                axis.text=element_text(size=14),axis.ticks = element_line(size = 0.2),
                axis.line = element_line(colour = "grey30"),
                axis.title.x = element_text(size=16, colour = "black", family="CMUSansSerif"),
                axis.title.y = element_text(size=16, colour = "black", family="CMUSansSerif"))
        
  #Fitting all Subjects individual Regression lines and the mean regression line
        ggplot(str.fit, aes(x=trialshort, y=fit1, group=stimtype, color=stimtype)) + 
          geom_vline(xintercept = 8.5, linetype="dashed", color = "grey48", size=0.2)+ 
          geom_line(size=1)+ geom_point(size=2)+
          scale_color_manual(values = c("#00AFF4", "#E46B0C"))+
          facet_wrap(~vp, scale="free", nrow=5)+
          theme(panel.background = element_blank(), legend.position="none",
                axis.ticks = element_line(size = 0.2),axis.line = element_line(colour = "grey30"),
                axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"))

###############################################
#PLOTTING MODELFIT SCR
###############################################       
        
    scr.fit %>% dplyr::group_by(stimtype, trialshort) %>%
              dplyr::summarize(.groups="keep", 
                               response = mean(response),
                               fit0 = mean(fit0),
                               fit1 = mean(fit1)) %>% 
              ungroup() %>%
          ggplot(aes(x=trialshort, y=response, color=stimtype)) + 
                coord_cartesian(xlim=c(1,17), ylim = c(0, 0.5))+
                geom_vline(xintercept = 8.5, linetype="dashed", color = "grey48", size=0.2)+ 
                geom_line(aes(group=stimtype), alpha=0.2)+
                geom_point(aes(shape=stimtype), alpha=0.2, size=2)+
                geom_line(aes(y = fit0, group=stimtype))+
                geom_point(aes(y = fit0, shape=stimtype), size = 2)+
                scale_color_manual(values = c("#00AFF4", "#E46B0C"))+
                scale_shape_manual(values = c(15, 16))+
                labs(x="Trial", y="log(SCR+1) µS") + 
                annotate("text", x=16.75, y=as.numeric(0.17), label="CS+", family="CMUSansSerif", 
                         color = "#E46B0C", fontface=2, size = 16 / .pt)+
                annotate("text", x=16.7, y=as.numeric(0.05), label="CS–", family="CMUSansSerif", 
                         color = "#00AFF4", fontface=2, size = 16/ .pt)+
                annotate("text", x=8.2, y=0.4, label="Instruktion", family="CMUSansSerif", color = "grey48", size = 16 / .pt, angle = 90)+
                scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16))+
          theme(panel.background = element_blank(), legend.position="none",
                axis.text=element_text(size=14),axis.ticks = element_line(size = 0.2),
                axis.line = element_line(colour = "grey30"),
                axis.title.x = element_text(size=16, colour = "black", family="CMUSansSerif"),
                axis.title.y = element_text(size=16, colour = "black", family="CMUSansSerif"))
  
        
    ggplot(scr.fit, aes(x=trialshort, y = fit1, group=stimtype, color=stimtype)) + 
          geom_vline(xintercept = 8.5, linetype="dashed", color = "grey48", size=0.2)+ 
          geom_line(size=1)+ geom_point(size=2)+
          scale_color_manual(values = c("#00AFF4", "#E46B0C"))+
          facet_wrap(~vp, scale="free", nrow=5)+
          theme(panel.background = element_blank(), legend.position="none",
                axis.ticks = element_line(size = 0.2), axis.line = element_line(colour = "grey30"),
                axis.title.x = element_text(colour = "black"), axis.title.y = element_text(colour = "black"))
  