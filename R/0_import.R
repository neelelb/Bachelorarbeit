########################################################################
####################### IMPORT DATA ######################################
########################################################################

# load packages
  library(tidyverse)

######################################
# STR DATA
######################################
  #necessary files for startle data read-in:
    #(1) txt.file (output from matlab) with startle data >> Startle_19Mar.txt
    #(2) txt.files (4) containing the 4 different trial-orders respectively >> order[x].txt
    #(3) txt.file containing the allocation of participant numbers 
                    #to the orders under which they got tested >> vp.numbers.txt
  #reading in, renaming and bringing into correct structure (1)
    str.data = read.table("STR/Startle_19Mar.txt", dec=".", sep=",", header=TRUE, na = "NA", stringsAsFactors=F)
    names(str.data) = c('vp','strtrial','trigger','quality','response', 'responselatency', 'peaklatency')
    str.data$quality = as.factor(str.data$quality)
    
  #converting response from millivolt (matlab default) to microvolt
    str.data$response = as.numeric(str.data$response)*1000
  
  #drop trigger, responselatency and peaklatency because where only interested in response-magnitude
    str.data =  select(str.data, -trigger, -(responselatency:peaklatency))
  
  #read in (2) and (3) >> cs- = 0, cs+ = 1, str-ITI = 2
    order1 = read.table("STR/order1.txt", header=TRUE)
    order2 = read.table("STR/order2.txt", header=TRUE)
    order3 = read.table("STR/order3.txt", header=TRUE)
    order4 = read.table("STR/order4.txt", header=TRUE)
    vpnumbers = read.table("STR/vpnumbers.txt", sep=",", header=TRUE); vpnumbers$vp = as.factor(vpnumbers$vp)
  
  #add (3) and (2) to str.data so that for each person there's their original lab-code & order
    str.data = dplyr::mutate(str.data, vpnumbers)
    str.data$order = as.factor(str.data$order)
  
  #new df integrating each of the 4 orders with trialnumbers and stimtypes
    trialorder = data.frame(order = rep(1:4, each=56),
                         strtrial = rep(1:56, times=4), 
                         stimtype = c(order1$order1,order2$order2,order3$order3,order4$order4))
    trialorder$stimtype = as.factor(trialorder$stimtype); trialorder$order = as.factor(trialorder$order)
  
  #combine str.data and trialorder, so that for each trial the stimtype gets displayed as a new row
    str.data.combi= left_join(str.data, trialorder, by = c("order", "strtrial"))
    
  #some cleaning
    col_order = c('vp', 'order', 'strtrial', 'stimtype','quality', 'response')
    str.data.combi = str.data.combi[, col_order]
    rm(order1,order2,order3,order4,trialorder,vpnumbers,col_order)

  #every response where quality==3 is a missing and gets replaced with a 'NA'
    str.data.combi$response[which(str.data.combi$quality == "3")] = NA
  #dropping all ITI-responses because where just interested in the cs
    str.data.combi = filter(str.data.combi, stimtype!=2)
    
  #Function: for-loop for trialnumbers short
    #this loop counts up trialnumbers for each stimtype separately within each vp
    #stimtype column MUST be named stimtype in order for it to work
       appendtrialnumbersshort = function(df){
          result = c()
          currentvp = df[1,"vp"]
          c0 = 0
          c1 = 0
          for (i in 1:nrow(df)) {
            if (currentvp != df[i,"vp"]) {
              c0 = 0
              c1 = 0
              currentvp = df[i,"vp"]  }
            if (df$stimtype[i]==0) {
              c0 = c0+1
              result = c(result, c0) }
            if (df$stimtype[i]==1) {
              c1 = c1+1
              result = c(result, c1) }  }
          return(result) }
        
  #Function: for-loop for trialnumbers long
    #counts up trialnumbers within each vp
    #stimtype column MUST be named stimtype in order for it to work
       appendtrialnumberslong = function(df){
         result = c()
         currentvp = df[1,"vp"]
         c1 = 0
         for (i in 1:nrow(df)) {
           if (currentvp != df[i,"vp"]) {
             c1 = 0
             currentvp = df[i,"vp"] }
           if (df$stimtype[i]!=2) {
             c1 = c1+1
             result = c(result, c1) } }
         return(result) }
       
  #append trialnumbers as integer-columns to original dataframe
       str.data.combi = dplyr::mutate(str.data.combi, 
                              trialshort = appendtrialnumbersshort(str.data.combi),
                              triallong = appendtrialnumberslong(str.data.combi))
       str.data.combi$trialshort = as.integer(str.data.combi$trialshort)
       str.data.combi$triallong = as.integer(str.data.combi$triallong)

  #drop 'strtrial'-column & clean up workspace
      str.data.final = select(str.data.combi, -strtrial)
      rm(str.data.combi, str.data)
  #check structure and summary
    str(str.data.final); summary(str.data.final)

    
######################################
#SCR DATA
######################################
  #necessary files for scr data read-in:
    #(1) txt.file (output from matlab) with scr data >> EDA_5s_001_19Mar.txt
    #(2) txt.files (4) containing the 4 different trial-orders respectively >> order[x].txt
        #!important: these are different than the ones for str because they don't contain any ITI
    #(3) txt.file containing the allocation of participant numbers 
                #to the orders under which they got tested >> vp.numbers.txt
    
  #reading in (1)
    scr.data = read.table("SCR/EDA_5s_001_19Mar.txt", dec=".", sep=",", header=TRUE, na = "NaN", stringsAsFactors=F)
  #drop unused response columns (we only want TTP_SCR)
    scr.data = select(scr.data, Vp, trial_order, trial_type, TTP_SCR)
  #bringing into correct structure, renaming and check 
    scr.data$trial_type = as.factor(scr.data$trial_type)
    names(scr.data) = c('vp','scrtrial','trigger','response')
    str(scr.data); summary(scr.data)

  #filter rows >> trigger nr. 42 reflects scr to 'cs' >> remove trigger column
    scr.data.combi =  scr.data %>% filter(trigger == 42) %>% select(-trigger)
  #check: every vp needs to have 32 responses
    table(scr.data.combi$vp)
    
  #read in (2) and (3) >> cs- = 0, cs+ = 1
    order1scr = read.table("SCR/order1.txt", header=TRUE)
    order2scr = read.table("SCR/order2.txt", header=TRUE)
    order3scr = read.table("SCR/order3.txt", header=TRUE)
    order4scr = read.table("SCR/order4.txt", header=TRUE)
    vpnumbersscr = read.table("SCR/vpnumbers.txt", sep=",", header=TRUE); vpnumbersscr$vp = as.factor(vpnumbersscr$vp)
    
  #add (3) and trialnumbers to scr.data so that for each person there is the original lab-code, order and trialnumbers
    N = length(as.list(levels(str.data.final$vp))) #total number of vp from str.data
    scr.data.combi = dplyr::mutate(scr.data.combi, 
                                   vpnumbersscr,
                                   triallong = rep(1:32, times=N))
    scr.data.combi$order = as.factor(scr.data.combi$order)  
  
  #new df integrating each of the 4 orders with trialnumbers and stimtypes
    trialorderscr = data.frame(order = rep(1:4, each=32),
                           triallong = rep(1:32, times=4), 
                            stimtype = c(order1scr$order1,order2scr$order2,order3scr$order3,order4scr$order4))
    trialorderscr$stimtype = as.factor(trialorderscr$stimtype); trialorderscr$order = as.factor(trialorderscr$order)
    
  #combine scr.data and trialorderscr, so that for each trial the stimtype gets displayed as a new row   
    scr.data.combi = left_join(scr.data.combi, trialorderscr, by = c("order", "triallong"))
    
  #counting up trialnumbers within each stimtype (with function defined in str-part) 
    scr.data.combi = dplyr::mutate(scr.data.combi,
                                    trialshort = appendtrialnumbersshort(scr.data.combi))
    scr.data.combi$trialshort = as.integer(scr.data.combi$trialshort)
      
  #Function: add quality column 
    #if non response ('response==0') set quality==2; else set quality==1 
    #(there are no quality==3 (missings) in scr data frame)
      qualityscr = function(df){
        quality = c()
        for (i in 1:nrow(df)) {
          if (df$response[i]!=0) {
            quality = c(quality, 1) }
          if (df$response[i]==0){
            quality = c(quality, 2) }
          if (df$response[i]<0){
            quality = c(quality, 3) } }
        return(quality) }
    
  #add quality as new factor column to match with str df
    scr.data.final = dplyr::mutate(scr.data.combi, quality = qualityscr(scr.data.combi))
    scr.data.final$quality = as.factor(scr.data.final$quality)
      
  #reorder the df rows in regard to some of the columns so it matches with str.data.final
    col_order = c('vp', 'order', 'stimtype', 'quality', 'response', 'trialshort', 'triallong')
    scr.data.final = scr.data.final[, col_order]
  
  #check structure & clean up workspace
    rm(scr.data, scr.data.combi, order1scr, order2scr, order3scr, order4scr,
         vpnumbersscr, trialorderscr, qualityscr, col_order, appendtrialnumberslong, appendtrialnumbersshort)
    str(scr.data.final); summary(scr.data.final)

      
###################
#TRANSFORMATION OF SCR
###################  
  library(MASS)       #needed for boxcox function
  #drop all non-responses because box-cox-function works just with responses>0
    #have a look at response distribution
      scr.data.final %>% filter(response != 0) %>%
       ggplot(aes(x=response))+
        geom_histogram(binwidth=0.05, alpha=0.8, fill="deepskyblue4", na.rm=TRUE) +
        theme(panel.background = element_blank(), axis.ticks = element_line(size = 0.2))
  
  #generate simple linear model with filtered scr-data and pipe into boxcox function
    lm(response ~ trialshort*stimtype, 
        data = filter(scr.data.final, response != 0)) %>%
    boxcox()  #>>if lambda == 0, Box & Cox recommend to log-transform data

  #log transformation of scr data
    scr.data.final$response = log(scr.data.final$response+1)
    
  #because there's no need for MASS anymore and it masks 'select' from dplyr package:
      detach(package:MASS, unload=TRUE)
