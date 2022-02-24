########################################################################
############ EXPLORATIVE ANALYSIS & DESCRIPTIVES #######################
########################################################################

###### needed: str.data.final and scr.data.final from script "ReadData.R" ######

###################
#EXCLUDE PARTICIPANTS
###################
  #vp115 and vp132 need to be excluded because of technical issues with the EDA
    scr.data.final = filter(scr.data.final, vp!='115' & vp!='132')
    str.data.final = filter(str.data.final, vp!='115' & vp!='132')

###################
#HOW MANY OF...?  
###################
  #drops unnecessary factor levels in stimtype & vp
      str.data.final$stimtype = factor(str.data.final$stimtype); str.data.final$vp = factor(str.data.final$vp)
      scr.data.final$vp = factor(scr.data.final$vp)
  #new total number of vp
      N.vp = length(as.list(levels(str.data.final$vp)))
      
  #count number of total observations, missings and nonresponses
      N.obs.str = nrow(str.data.final)
      N.obs.scr = nrow(scr.data.final)
      N.mis.str = nrow(filter(str.data.final, is.na(response == 1))) 
      N.mis.scr = nrow(filter(scr.data.final, is.na(response == 1))) 
      N.nr.str = nrow(filter(str.data.final, response == 0)) 
      N.nr.scr = nrow(filter(scr.data.final, response == 0))
      
 #frequency table of response-qualities for str and scr
      table(str.data.final$quality, str.data.final$vp)
      table(scr.data.final$quality, scr.data.final$vp)
    
###################
#NORMALITY WITH SHAPIRO WILK TEST
################### 
  
  #Function: shapiro wilk test for normality >> produces df with global & trialwise sw-tests
    shapirowilk = function(df){
        normality = data.frame(matrix(ncol = 3, nrow = 0))
        x = c("trial", "sw_statistik", "pvalue")
        colnames(normality) = x; rm(x)
        a = shapiro.test(df$response)$statistic %>% round(digits=3)
        b = shapiro.test(df$response)$p.value %>% round(digits=3)
          if (b < 0.001) {
            b= paste(as.character(b), "*") }
          normality[nrow(normality) + 1,] = c("global",a,b)
          for (i in 1:16) {
            a = shapiro.test(filter(df, trialshort == i)$response)$statistic %>% round(digits=3)
            b = shapiro.test(filter(df, trialshort == i)$response)$p.value %>% round(digits=3)
            if (b < 0.001) {
              b= paste(as.character(b), "*") }
            normality[nrow(normality) + 1,] = c(i, a,b) }
          return(normality) }
        
  #normality test table for STR
     shapirowilk(str.data.final)
  #normality test table for SCR 
     shapirowilk(scr.data.final)

################################
#DESCRIPTIVE ANALYSIS UNIVARIATE
################################ 
  #Function: calculates mean, sem and ci for given number of groups in df
      sumSECI = function(data=NULL, responsevar, groupvars=NULL, na.rm=TRUE, ci=.95) {
              datac = data %>% group_by(across(groupvars)) %>% 
                              summarise(.groups="keep", 
                                        N = sum(!is.na(.data[[responsevar]])),
                                     mean = mean(.data[[responsevar]], na.rm=na.rm),
                                     sd   = sd(.data[[responsevar]], na.rm=na.rm))
            #rename the "mean" column & calculate sem 
              names(datac)[names(datac) == 'mean'] = responsevar
              datac$se = datac$sd / sqrt(datac$N)
        #ci-multiplier (calculate t-statistic with df=N-1)
          ciMult = qt(ci/2 + .5, datac$N-1)
          datac$ci = datac$se * ciMult
        return(datac)}
    
  #Function: calculates t-tests for difference of CS+ and CS- trialwise
        ttest = function(df1, df2){     #df1: final df from above, df2: summarised df (sum.av)
          ttest = data.frame(matrix(ncol = 5, nrow = 0))
          x = c("trial", "tstatistik", "pvalue", "df", "method")
          colnames(ttest) = x; rm(x)
          for (i in 1:16) {
            test = t.test(filter(df1, stimtype==1 & trialshort==i)$response, 
                          filter(df1, stimtype==0 & trialshort==i)$response,  paired=TRUE)
            a = test$statistic %>% round(digits=3)
            b = test$p.value %>% round(digits=4)
            c = test$parameter %>% round(digits=2)
            d = test$method
            ttest[nrow(ttest) + 1,] = c(i,a,b,c,d) }
          return(mutate(df2, ttest)) }
      

  #STR: univariate descriptives grouped by trial AND stimtype
      #summarising str
        sum.str = sumSECI(str.data.final, responsevar = "response", groupvars = all_of(c("stimtype", "trialshort")))
      #binding stimtype==0 and stimtype==1 horizontal next to each other 
        sum.str = bind_cols(filter(sum.str, stimtype==0), filter(sum.str, stimtype==1))
        sum.str = mutate(sum.str, diff=sum.str$response...11-sum.str$response...4)
      #calculating trialwise t-tests
        sum.str = ttest(str.data.final, sum.str)
     
  #SCR: univariate descriptives grouped by trial AND stimtype
      #summarising scr
        sum.scr = sumSECI(scr.data.final, responsevar = "response", groupvars = all_of(c("stimtype", "trialshort")))
      #binding stimtype==0 and stimtype==1 horizontal next to each other 
        sum.scr = bind_cols(filter(sum.scr, stimtype==0), filter(sum.scr, stimtype==1))
        sum.scr = mutate(sum.scr, diff=sum.scr$response...11-sum.scr$response...4)
      #calculating trialwise t-tests
        sum.scr = ttest(scr.data.final, sum.scr)
    
  #descriptives grouped JUST by stimtype (for first row of descriptives table)
    #STR
        sumSECI(str.data.final, responsevar = "response", groupvars = c("stimtype"))
        t.test(str.data.final$response[str.data.final$stimtype==1], 
               str.data.final$response[str.data.final$stimtype==0], paired=TRUE)
        
    #SCR   
        sumSECI(scr.data.final, responsevar = "response", groupvars = c("stimtype"))
        t.test(scr.data.final$response[scr.data.final$stimtype==1], 
               scr.data.final$response[scr.data.final$stimtype==0], paired=TRUE)        

  #global descriptives
        sumSECI(scr.data.final, responsevar = "response")   #STR
        sumSECI(str.data.final, responsevar = "response")   #SCR

    
################################
#BIVARIATE CORRELATIONS 
################################
  #Function: calculates pearsons r & p-value of both dv for each trial
        multivarcor = function(df1, df2){   #final df from above needed
          correlations = data.frame(matrix(ncol = 3, nrow = 0))
          x = c("trial", "correlations", "pvalues")
          colnames(correlations) = x; rm(x)
          for (i in 1:16) {
            a = cor.test(df1$response[df1$trialshort==i], df2$response[df2$trialshort==i], method="pearson")$estimate
            b = cor.test(df1$response[df1$trialshort==i], df2$response[df2$trialshort==i], method="pearson")$p.value
            if (b < 0.05) {b= paste(as.character(b), "*")}
            correlations[nrow(correlations) + 1,] = c(i, a,b) }
          return(correlations)}
        
      #correlations trialwise between scr and str (disregarding stimtype)
        correlations = multivarcor(str.data.final, scr.data.final)
        #add global correlation of both DV as new row 
        correlations[nrow(correlations) + 1,] = c("global",
                              cor.test(scr.data.final$response, str.data.final$response, method="pearson")$estimate,
                              cor.test(scr.data.final$response, str.data.final$response, method="pearson")$p.value)
        
    #correlations trialwise for CS+/CS-difference between scr and str
      #helping df with scaled responses (mean =0, sd=1)  for each dv
          scaled1 = str.data.final; scaled2 = scr.data.final
          scaled1$response = scale(scaled1$response); scaled2$response = scale(scaled2$response)
      #create new bivariate df with cs-differences for scr and str
          biv = data.frame(trialshort = rep(1:16, times=N.vp),
                           cs0_str = scaled1$response[scaled1$stimtype==0],
                           cs1_str = scaled1$response[scaled1$stimtype==1],
                           cs0_scr = scaled2$response[scaled2$stimtype==0],
                           cs1_scr = scaled2$response[scaled2$stimtype==1])
          biv$diff_str = biv$cs1_str-biv$cs0_str
          biv$diff_scr = biv$cs1_scr-biv$cs0_scr

      #Function: calculate correlations and t-Tests for differences
        bi_tc = function(df){ #bivariate df needed with difference scores (raw and scaled)
          results = data.frame(matrix(ncol = 7, nrow = 0))
          x = c("trial", "correlation", "pvaluec", "tstatistic", "pvaluet", "df", "method")
          colnames(results) = x; rm(x)
          for (i in 1:16) {
            cortest = cor.test(df$diff_str[df$trialshort==i], df$diff_scr[df$trialshort==i], method="pearson")
            a = cortest$estimate %>% round(digits=2)
            b = cortest$p.value %>% round(digits=3)
            ttest = t.test(df$diff_str[df$trialshort==i], df$diff_scr[df$trialshort==i],  paired=TRUE)
            c = ttest$statistic %>% round(digits=2)
            d = ttest$p.value %>% round(digits=3)
            e = ttest$parameter %>% round(digits=2)
            f = ttest$method
            results[nrow(results) + 1,] = c(i,a,b,c,d,e,f) }
          return(results)}
        
      #results >> save
        bivariate = bi_tc(biv)
        t.test(biv$diff_str, biv$diff_scr,  paired=TRUE)
        cor.test(biv$diff_str, biv$diff_scr, method="pearson")
      
##### clean up work space:
    rm(shapirowilk, ttest, multivarcor, correlations, bi_tc, bivariate, biv, scaled1, scaled2)
       
    