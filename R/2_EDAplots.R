########################################################################
###################### PLOTS FOR EDA ###################################
########################################################################
  
#######needed: sumSECI function from script "EDA.R"
  library(extrafont)    #Computer Modern font in figures
 
  #Function: for correct names of facets in facet_wrap
    order_names = list('1'="Reihenfolge 1", '2'="Reihenfolge 2", '3'="Reihenfolge 3", '4'="Reihenfolge 4")
    order_labeller = function(variable,value){return(order_names[value])}

####################################################
#PLOTS FOR STR
####################################################
      
  #Plot 1: STR mean trajectories
    str.data.final %>% sumSECI(responsevar = "response", groupvars = c("triallong")) %>% 
        ggplot(aes(x= triallong, y=response))+
      #coord_cartesian(ylim = c(-1, 1.25), xlim=c(1,32))+
      geom_line(aes(colour="#00305E"))+
      geom_point(aes(colour="#00305E"))+
      geom_errorbar(aes(ymin=response-se, ymax=response+se, colour="#00305E"), width=.2, size=0.5)+
      scale_color_manual(values = "#00305E")+
      labs(x="Trial", y="Schreckreaktion (µV)")+
      scale_x_continuous(breaks=c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32))+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(),legend.position="none",
            axis.text=element_text(size=14), axis.ticks = element_line(size = 0.2),
            axis.line = element_line(colour = "grey30"),
            axis.title.x = element_text(size=16, colour = "black", family="CMUSansSerif"),
            axis.title.y = element_text(size=16, colour = "black", family="CMUSansSerif"))
        

  #Plot 2: STR mean trajectories per Stimtype
    sum.str = sumSECI(str.data.final, responsevar = "response", groupvars = c("stimtype", "trialshort"))
    ggplot(data=sum.str, aes(x= trialshort, y=response, group=factor(stimtype)))+
      coord_cartesian(xlim=c(1,17), ylim = c(40, 115))+
      geom_vline(xintercept = 8.5, linetype="dashed", color = "grey48", size=0.2)+    
      geom_line(aes(color=factor(stimtype)))+
      geom_errorbar(aes(ymin=response-se, ymax=response+se, color=factor(stimtype)), width=.2, size=0.5)+
      geom_point(aes(color=factor(stimtype), shape=factor(stimtype)), size=2)+ 
      scale_color_manual(values = c("#00AFF4", "#E46B0C"))+
      scale_shape_manual(values = c(15, 16))+
      labs(x="Trial", y="Schreckreaktion in µV") + 
      annotate("text", x=16.75, y=as.numeric(sum.str[32,4]+1), label="CS+", family="CMUSansSerif", 
               color = "#E46B0C", fontface=2, size = 16 / .pt)+
      annotate("text", x=16.7, y=as.numeric(sum.str[16,4]-1), label="CS–", family="CMUSansSerif", 
               color = "#00AFF4", fontface=2, size = 16/ .pt)+
      annotate("text", x=8.2, y=100, label="Instruktion", family="CMUSansSerif", color = "grey48", size = 16 / .pt, angle = 90)+
      scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16))+
      scale_y_continuous(breaks=c(40,60,80,100,115))+
      theme(panel.background = element_blank(), legend.position="none",
            axis.text=element_text(size=14), axis.ticks = element_line(size = 0.2),
            axis.line = element_line(colour = "grey30"),
            axis.title.x = element_text(size=16, colour = "black", family="CMUSansSerif"),
            axis.title.y = element_text(size=16, colour = "black", family="CMUSansSerif"))


  #Plot 3: STR trajectories per Order (and Stimtype)
    str.data.final %>% sumSECI(responsevar = "response", groupvars = c("stimtype", "trialshort", "order")) %>%
      ggplot(aes(x= trialshort, y=response, group=factor(stimtype)))+
      #coord_cartesian(ylim = c(-2, 2), xlim=c(1,17))+
      geom_vline(xintercept = 8.5, linetype="dashed", color = "grey48", size=0.2)+    
      geom_line(aes(color=factor(stimtype)))+
      geom_errorbar(aes(ymin=response-se, ymax=response+se, color=factor(stimtype)), width=.1, size=0.2)+
      geom_point(aes(color=factor(stimtype), shape=factor(stimtype)), size=2)+ 
      scale_color_manual(values = c("#00AFF4", "#E46B0C"), labels = c("CS–", "CS+"), name=" ")+
      scale_shape_manual(values = c(15, 16), labels = c("CS–", "CS+"), name=" ")+
      labs(x="Trial", y="Schreckreaktion (µV)") + 
      annotate("text", x=8.1, y=160, label="Instruktion", color = "grey48", size = 14 / .pt, angle = 90)+
      facet_wrap(~ order, labeller =order_labeller)+
      scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16))+
      theme(panel.background = element_blank(), legend.position=c(0.5,0.5),
            legend.key = element_rect(colour = NA, fill = NA), legend.text = element_text(size=12, family="CMUSansSerif"),
            axis.text=element_text(size=14), axis.ticks = element_line(size = 0.2),
            axis.line = element_line(colour = "grey30"),
            axis.title.x = element_text(size=16, colour = "black", family="CMUSansSerif"),
            axis.title.y = element_text(size=16, colour = "black", family="CMUSansSerif"),
            strip.background = element_rect(colour="white",fill="white"),
            strip.text = element_text(size = 16))

  #Plot 4: STR trajectories per VP
    ggplot(data=str.data.final, aes(x= trialshort, y=response, group=stimtype))+
      coord_cartesian(xlim=c(1,17))+
      geom_vline(xintercept = 8.5, linetype="dashed", color = "grey48", size=0.2)+  
      geom_line(aes(color=factor(stimtype)))+
      geom_point(aes(color=factor(stimtype), shape=factor(stimtype)), size=2, na.rm=TRUE)+
      scale_color_manual(values = c("#00AFF4", "#E46B0C"), labels = c("CS–", "CS+"), name = " ")+
      scale_shape_manual(values = c(15, 16), labels = c("CS–", "CS+"), name = " ")+
      labs(x="Trial", y="Schreckreaktion (µV)") + 
      facet_wrap(~ vp, nrow=9, scale="free")+
      scale_x_continuous(breaks=c(2,4,6,8,10,12,14,16))+
      theme(panel.background = element_blank(),
            axis.ticks = element_line(size = 0.2),
            axis.line = element_line(colour = "grey30"),
            axis.title.x = element_text(colour = "black"),
            axis.title.y = element_text(colour = "black"),
            strip.background = element_rect(colour="white",fill="white"),
            legend.key = element_rect(colour = NA, fill = NA),
            strip.text = element_text(size = 12))

 #Plot 5: response frequency for each stimtype: Histogramm, KDE or freqpoly - as you wish:
    ggplot(str.data.final, color=factor(stimtype), fill=factor(stimtype)) +
      #geom_freqpoly(aes(x=response, color= factor(stimtype)), size=1, binwidth=10, na.rm=TRUE, alpha=0.5, position="identity") +
      #geom_histogram(aes(x=response, fill= factor(stimtype)), binwidth=2, na.rm=TRUE, alpha=0.5, position="identity") +
      geom_density(aes(x=response, fill=factor(stimtype),color=factor(stimtype)), alpha=0.3, bw=8, na.rm=TRUE)+
      scale_color_manual(values = c("#00AFF4", "#E46B0C"), labels = c("CS–", "CS+"), name=" ")+
      scale_fill_manual(values = c("#00AFF4", "#E46B0C"), labels = c("CS–", "CS+"), name= " ")+
      labs(x="Schreckreaktion (µV)", y="Dichte") + 
      theme(panel.background = element_blank(), legend.position = c(0.6,0.7),
            axis.text=element_text(size=14), axis.ticks = element_line(size = 0.2),
            legend.text = element_text(size=12, family="CMUSansSerif"),
            axis.title.x = element_text(size=16, colour = "black", family="CMUSansSerif"),
            axis.title.y = element_text(size=16, colour = "black", family="CMUSansSerif"),
            strip.background = element_rect(colour="white",fill="white"),
            legend.key = element_rect(colour = NA, fill = NA))


 #Plot 6: Boxplot: grouped by trial AND stimtype
    ggplot(data=str.data.final, aes(x=as.factor(trialshort), y=response))+
      geom_boxplot(aes(fill=factor(stimtype)), na.rm=TRUE, alpha=0.8, outlier.color = "indianred3", 
                   outlier.alpha = 0.5, outlier.size = 1)+
      scale_fill_manual(values = c("#00AFF4", "#E46B0C"), labels = c("CS–", "CS+"), name = " ")+
      scale_color_manual(values = c("#00AFF4", "#E46B0C"), labels = c("CS–", "CS+"), name = " ")+
      labs(x="Trial", y="Schreckreaktion (µV)") + 
      scale_x_discrete(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16))+
      theme(panel.background = element_blank(), legend.position = c(0.7,0.8),
            axis.ticks = element_line(size = 0.2), axis.text=element_text(size=14),
            legend.text = element_text(size=16, family="CMUSansSerif"), 
            legend.title = element_text(size=16, family="CMUSansSerif"),
            axis.title.x = element_text(size=16, colour = "black", family="CMUSansSerif"),
            axis.title.y = element_text(size=16, colour = "black", family="CMUSansSerif"),
            strip.background = element_rect(colour="white",fill="white"),
            legend.key = element_rect(colour = NA, fill = NA))                  

 #Plot 7: Boxplot grouped by Stimtype
    #new bool columns, TRUE if data point is outlier, FALSE if not
      str.outlier = str.data.final %>% group_by(stimtype) %>%
        mutate( outlier.high = response > quantile(response, .75, na.rm=TRUE) + 1.50*IQR(response, na.rm=TRUE), 
                outlier.low = response < quantile(response, .25, na.rm=TRUE) - 1.50*IQR(response, na.rm=TRUE))
    ggplot(data=str.outlier, aes(x=as.factor(stimtype), y=response))+
      geom_jitter(data = filter(str.outlier, outlier.high != T & outlier.low != T),
                  aes(color=factor(stimtype)), na.rm=TRUE, position=position_jitter(0.3), alpha=0.3)+
      geom_boxplot(aes(fill=factor(stimtype)), na.rm=TRUE, alpha=0.8, outlier.shape =NA, 
                   width= 0.7)+
      geom_jitter(data = filter(str.outlier, outlier.high == T | outlier.low == T), 
                  position=position_jitter(0.3), color="#00305E", alpha=0.5)+
      scale_fill_manual(values = c("#00AFF4", "#E46B0C"), labels = c("CS–", "CS+"), name = " ")+
      scale_color_manual(values = c("#00AFF4", "#E46B0C"), labels = c("CS–", "CS+"), name = " ")+
      labs(x="Stimulustyp", y="Schreckreaktion (µV)") + 
      scale_x_discrete(breaks=c("0","1"), labels=c("CS–", "CS+"))+
      theme(panel.background = element_blank(), legend.position = "none",
            axis.ticks = element_line(size = 0), axis.text=element_text(size=14),
            axis.title.x = element_text(size=16, colour = "black", family="CMUSansSerif"),
            axis.title.y = element_text(size=16, colour = "black", family="CMUSansSerif"),
            strip.background = element_rect(colour="white",fill="white"),
            legend.key = element_rect(colour = NA, fill = NA)) 
 
  #clean up
    rm(str.outlier, sum.str)


####################################################
#PLOTS FOR SCR
####################################################    

 #Plot 1: SCR mean trajectories
    scr.data.final %>% sumSECI(responsevar = "response", groupvars = c("triallong")) %>% 
      ggplot(aes(x= triallong, y=response))+
      geom_line(aes(colour="#00305E"))+
      geom_point(aes(colour="#00305E"))+
      geom_errorbar(aes(ymin=response-se, ymax=response+se, colour="#00305E"), width=.1, size=0.2)+
      scale_color_manual(values = "#00305E")+
      labs(x="Trial", y="log(SCR+1) µS")+
      scale_x_continuous(breaks=c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32))+
      theme(panel.background = element_blank(),legend.position="none",
            axis.text=element_text(size=14), axis.ticks = element_line(size = 0.2),
            axis.line = element_line(colour = "grey30"),
            axis.title.x = element_text(size=16, colour = "black", family="CMUSansSerif"),
            axis.title.y = element_text(size=14, colour = "black", family="CMUSansSerif"))

 #Plot 2: SCR mean trajectories per Stimtype
    sum.scr = sumSECI(scr.data.final, responsevar = "response", groupvars = c("stimtype", "trialshort"))
    ggplot(data=sum.scr, aes(x= trialshort, y=response, group=factor(stimtype)))+
      coord_cartesian(ylim = c(0, 0.5))+
      geom_vline(xintercept = 8.5, linetype="dashed", color = "grey48", size=0.2)+    
      geom_line(aes(color=factor(stimtype)))+
      geom_errorbar(aes(ymin=response-se, ymax=response+se, color=factor(stimtype)), width=.2, size=0.5)+
      geom_point(aes(color=factor(stimtype), shape=factor(stimtype)), size=2)+ 
      scale_color_manual(values = c("#00AFF4", "#E46B0C"))+
      scale_shape_manual(values = c(15, 16))+
      labs(x="Trial", y="log(SCR+1) µS") + 
      annotate("text", x=16.75, y=as.numeric(sum.scr[32,4]), label="CS+", family="CMUSansSerif", 
               color = "#E46B0C", fontface=2, size = 16 / .pt)+
      annotate("text", x=16.7, y=as.numeric(sum.scr[16,4]), label="CS–", family="CMUSansSerif", 
               color = "#00AFF4", fontface=2, size = 16/ .pt)+
      annotate("text", x=8.2, y=0.4, label="Instruktion", family="CMUSansSerif", color = "grey48", size = 16 / .pt, angle = 90)+
      scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16))+
      theme(panel.background = element_blank(), legend.position="none",
            axis.text=element_text(size=14),axis.ticks = element_line(size = 0.2),
            axis.line = element_line(colour = "grey30"),
            axis.title.x = element_text(size=16, colour = "black", family="CMUSansSerif"),
            axis.title.y = element_text(size=16, colour = "black", family="CMUSansSerif"))

 #Plot 3: STR mean trajectories per Order (and Stimtype)  
    scr.data.final %>% sumSECI(responsevar = "response", groupvars = c("stimtype", "trialshort", "order")) %>%
      ggplot(aes(x= trialshort, y=response, group=factor(stimtype)))+
      coord_cartesian(xlim = c(1, 17))+
      geom_vline(xintercept = 8.5, linetype="dashed", color = "grey48", size=0.2)+    
      geom_line(aes(color=factor(stimtype)))+
      geom_errorbar(aes(ymin=response-se, ymax=response+se, color=factor(stimtype)), width=.1, size=0.2)+
      geom_point(aes(color=factor(stimtype), shape=factor(stimtype)), size=2)+ 
      scale_color_manual(values = c("#00AFF4", "#E46B0C"), labels = c("CS–", "CS+"), name=" ")+
      scale_shape_manual(values = c(15, 16), labels = c("CS–", "CS+"), name=" ")+
      labs(x="Trial", y="log(SCR+1) µS") + 
      annotate("text", x=8.1, y=0.72, label="Instruktion", color = "grey48", size = 14 / .pt, angle = 90)+
      facet_wrap(~ order, labeller =order_labeller)+
      scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16))+
      theme(panel.background = element_blank(), legend.position=c(0.5,0.5),
            legend.key = element_rect(colour = NA, fill = NA), legend.text = element_text(size=12, family="CMUSansSerif"),
            axis.text=element_text(size=14), axis.ticks = element_line(size = 0.2),
            axis.line = element_line(colour = "grey30"),
            axis.title.x = element_text(size=16, colour = "black", family="CMUSansSerif"),
            axis.title.y = element_text(size=16, colour = "black", family="CMUSansSerif"),
            strip.background = element_rect(colour="white",fill="white"),
            strip.text = element_text(size = 16))

 #Plot 4: SCR mean trajectories per VP
    ggplot(data=scr.data.final, aes(x= trialshort, y=response, group=stimtype))+
      coord_cartesian(xlim=c(1,17))+
      geom_vline(xintercept = 8.5, linetype="dashed", color = "grey48", size=0.2)+  
      geom_line(aes(color=factor(stimtype)))+
      geom_point(aes(color=factor(stimtype), shape=factor(stimtype)), size=2)+
      scale_color_manual(values = c("#00AFF4", "#E46B0C"), labels = c("CS-", "CS+"), name = " ")+
      scale_shape_manual(values = c(15, 16), labels = c("CS-", "CS+"), name = " ")+
      labs(x="Trial", y="log(SCR+1) µS") + 
      facet_wrap(~ vp, nrow=9, scale="free")+
      scale_x_continuous(breaks=c(2,4,6,8,10,12,14,16))+
      theme(panel.background = element_blank(),
            axis.ticks = element_line(size = 0.2),
            axis.line = element_line(colour = "grey30"),
            axis.title.x = element_text(colour = "black"),
            axis.title.y = element_text(colour = "black"),
            strip.background = element_rect(colour="white",fill="white"),
            legend.key = element_rect(colour = NA, fill = NA),
            strip.text = element_text(size = 12)) 

#Plot 5: response frequency for each stimtyp: Histogramm, KDE or freqpoly
    ggplot(scr.data.final, color=factor(stimtype), fill=factor(stimtype)) +
      #coord_cartesian(xlim=c(0,1))+
      #geom_freqpoly(aes(x=response, color= factor(stimtype)), size=1, binwidth=0.05, na.rm=TRUE, alpha=0.5, position="identity") +
      #geom_histogram(aes(x=response, fill= factor(stimtype)), binwidth=0.01, na.rm=TRUE, alpha=0.5, position="identity") +
      geom_density(aes(x=response, fill=factor(stimtype),color=factor(stimtype)), alpha=0.3, bw=0.05, na.rm=TRUE)+
      scale_color_manual(values = c("#00AFF4", "#E46B0C"), labels = c("CS–", "CS+"), name=" ")+
      scale_fill_manual(values = c("#00AFF4", "#E46B0C"), labels = c("CS–", "CS+"), name= " ")+
      labs(x="log(SCR+1) µS", y="Dichte") + 
      theme(panel.background = element_blank(), legend.position = c(0.6,0.7),
            axis.text=element_text(size=14), axis.ticks = element_line(size = 0.2),
            legend.text = element_text(size=12, family="CMUSansSerif"),
            axis.title.x = element_text(size=16, colour = "black", family="CMUSansSerif"),
            axis.title.y = element_text(size=16, colour = "black", family="CMUSansSerif"),
            strip.background = element_rect(colour="white",fill="white"),
            legend.key = element_rect(colour = NA, fill = NA))

#Plot 6: Boxplot: grouped by trial AND stimtype
    ggplot(data=scr.data.final, aes(x=as.factor(trialshort), y=response))+
      coord_cartesian(ylim=c(-0.2,2))+
      geom_boxplot(aes(fill=factor(stimtype)), na.rm=TRUE, alpha=0.8, outlier.color = "indianred3", 
                   outlier.alpha = 0.5, outlier.size = 1)+
      scale_fill_manual(values = c("#00AFF4", "#E46B0C"), labels = c("CS–", "CS+"), name = " ")+
      scale_color_manual(values = c("#00AFF4", "#E46B0C"), labels = c("CS–", "CS+"), name = " ")+
      labs(x="Trial pro Stimulustyp", y="log(SCR+1) µS") + 
      scale_x_discrete(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16))+
      theme(panel.background = element_blank(), legend.position = c(0.7,0.8),
            axis.ticks = element_line(size = 0.2), axis.text=element_text(size=14),
            #axis.line = element_line(colour = "grey30"),
            legend.text = element_text(size=16, family="CMUSansSerif"), 
            legend.title = element_text(size=16, family="CMUSansSerif"),
            axis.title.x = element_text(size=16, colour = "black", family="CMUSansSerif"),
            axis.title.y = element_text(size=16, colour = "black", family="CMUSansSerif"),
            strip.background = element_rect(colour="white",fill="white"),
            legend.key = element_rect(colour = NA, fill = NA))                 

#Plot 7: Boxplot grouped by Stimtype
    #new bool columns, TRUE if data point is outlier, FALSE if not
    scr.outlier = scr.data.final %>% group_by(stimtype) %>%
        mutate( outlier.high = response > quantile(response, .75, na.rm=TRUE) + 1.50*IQR(response, na.rm=TRUE), 
                outlier.low = response < quantile(response, .25, na.rm=TRUE) - 1.50*IQR(response, na.rm=TRUE))
    ggplot(data=scr.outlier, aes(x=as.factor(stimtype), y=response))+
      geom_jitter(data = filter(scr.outlier, outlier.high != T & outlier.low != T),
                  aes(color=factor(stimtype)), na.rm=TRUE, position=position_jitter(0.3), alpha=0.3)+
      geom_boxplot(aes(fill=factor(stimtype)), na.rm=TRUE, alpha=0.8, outlier.shape =NA, width= 0.7)+
      geom_jitter(data = filter(scr.outlier, outlier.high == T | outlier.low == T), 
                  position=position_jitter(0.3), color="indianred3", alpha=0.5)+
      scale_fill_manual(values = c("#00AFF4", "#E46B0C"), labels = c("CS–", "CS+"), name = " ")+
      scale_color_manual(values = c("#00AFF4", "#E46B0C"), labels = c("CS–", "CS+"), name = " ")+
      labs(x="Stimulustyp", y="log(SCR+1) µS") + 
      scale_x_discrete(breaks=c("0","1"), labels=c("CS–", "CS+"))+
      theme(panel.background = element_blank(), legend.position = "none",
            axis.ticks = element_line(size = 0), axis.text=element_text(size=14),
            axis.title.x = element_text(size=16, colour = "black", family="CMUSansSerif"),
            axis.title.y = element_text(size=16, colour = "black", family="CMUSansSerif"),
            strip.background = element_rect(colour="white",fill="white"),
            legend.key = element_rect(colour = NA, fill = NA)) 
    
#clean up
    rm(scr.outlier,sum.scr, col_names, order_labeller)

################################
#MULTIVARIATE PLOTS
################################
      ggplot(data=str.data.final, aes(x = response, y = scr.data.final$response))+
                  geom_point(alpha=0.4, size=1, colour = "#00305E", na.rm=TRUE)+
                  geom_smooth(formula=y~x,method = "lm", se=T, size=1, color="grey30", fill = "grey68", alpha=0.2, na.rm=TRUE)+
                  scale_x_continuous(breaks=c(0,100,200,300,400))+
                  labs(x="Schreckreaktion in µV", y="log(SCR+1) µS") +
                  theme(panel.background = element_blank(),
                        axis.ticks = element_line(size = 0.2), axis.text=element_text(size=14),
                        axis.line = element_line(colour = "grey30"),
                        axis.title.x = element_text(size=16, colour = "black", family="CMUSansSerif"),
                        axis.title.y = element_text(size=16, colour = "black", family="CMUSansSerif"))

#Scatterplot using just the means for each subject; 95% confidence interval
    a = str.data.final %>% sumSECI(responsevar = "response", groupvars = c("vp"))
    b = scr.data.final %>% sumSECI(responsevar = "response", groupvars = c("vp")) 
    ggplot(a, aes(x = response, y = b$response))+
          geom_point(alpha=0.5, colour = "grey30")+
          geom_smooth(formula=y~x,method = "lm", se=T, size=1, color="#00305E")+
          labs(x="Schreckreaktion in µV", y="log(SCR+1) µS") +
          scale_x_continuous(breaks=c(0,25,50,75,100,125))+
          theme(panel.background = element_blank(),
                axis.ticks = element_line(size = 0.2), axis.text=element_text(size=14),
                axis.line = element_line(colour = "grey30"),
                axis.title.x = element_text(size=16, colour = "black", family="CMUSansSerif"),
                axis.title.y = element_text(size=16, colour = "black", family="CMUSansSerif"))
    rm(a, b)
