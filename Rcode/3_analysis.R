########################################################################
################# MODELBUILD MULTIVARIATE ##############################
########################################################################
  library(nlme)
######
#this code needs to run AFTER the SCR and STR data has been reorganized in 0_import to 
#scr.data.final and str.data.final and after 1_EDA ran
######
 
########################  
#PREPARATION FOR MODELBUILD
########################
  #center trial and create quadratic time term 
    scr.multi = mutate (scr.data.final, lin=scr.data.final$trialshort-8.5, quad=(scr.data.final$trialshort-8.5)^2)
    str.multi = mutate (str.data.final, lin=str.data.final$trialshort-8.5, quad=(str.data.final$trialshort-8.5)^2)
  #creating the dummy variable DELTA_k for outcome variable
    #delta1=SCR, delta2=STR
    scr.multi = mutate(scr.multi, delta1 = 1, delta2 = 0, grp = "delta1")
    str.multi = mutate(str.multi, delta1 = 0, delta2 = 1, grp = "delta2") 
  #bind str and scr data together to create one multivariate dataframe
    multivariat = bind_rows(str.multi, scr.multi)
    multivariat$grp = as.factor(multivariat$grp)
  #check structure
    str(multivariat); summary(multivariat)
    
  #controlling Function from (Grimm et al., 2017) for max iterations in estimation
    lmeCtlList = lmeControl(maxIter = 500,     #max nr of iterations for optimization algorithm
                            msMaxIter = 200,   #max nr of iterations for optimization step inside algorithm
                            tolerance = 1e-4,  #tolerance for convergence criterion
                            niter = 100,       #nr of iterations for EM algorithm used to refine initial estimates of RE-matrix
                            msTol = 1e-5,      #tolerance for the convergence criterion on the first iteration 
                            nlmStepMax = 500, msVerbose = FALSE)

########################    
#MODELBUILD
########################      
  
        # The boundary singularity message means that the within-person variance estimate is problematic 
        # (and one or more values in the matrix have been set to 0).
        # we need to take nlme >> comparisons with anova() are anti-conservative for Fixed Effects but
        # with comparison of Random Effect Structure it's conservative 
        # for comparing Fixed Effect results the authors of nlme suggest using conditional t-test with summary(morecomplexmodel)
        
        #all 'c' model parts are for S'C'R; all 't' model parts are for S'T'R
        
        ################ UNCONDITIONAL NULLMODEL  ##################
        
            null = nlme(response ~ delta1*(c0) + delta2*(t0), 
                             data = multivariat,
                             fixed =  c0+t0 ~1,
                             random = c0+t0 ~1,
                             group = ~vp,
                             start = c(0, 0),
                             weights = varIdent(form = ~1|grp),
                        #weights describe the within-group heteroscedasticity structure
                        #>> specifies separate Level1-Variances 
                        #NULL: homoscedastic within-group errors
                             na.action = na.omit,
                             method = "REML",
                             control = lmeCtlList)
            
            #obtain results:
              summary(null)
              intervals(null, 0.95)
            #The residual standard deviation which is square root of the residual variance is for the delta1 variable. 
            #To get the value for the delta2 variable, multiple the residual by 0.13831 (parameter estimate for delta2)
  
#regarding the results of 'null' the starting values for intercept fixed effects will be changed to 
#0.1 for c0 and 50 for t0 to facilitate estimation
            
      ################ UNCONDITIONAL GROWTH >> RANDOM EFFECT STRUCTURE ##################    
            
          #maximal RE structure (12 RE) >> 'system is computationally singular'
            ran1 = nlme(response ~ 
                           delta1*(c0+c1*lin+c2*quad+c3*as.integer(stimtype)+
                                     c4*lin*as.integer(stimtype)+c5*quad*as.integer(stimtype))+ 
                           delta2*(t0+t1*lin+t2*quad+t3*as.integer(stimtype)+
                                     t4*lin*as.integer(stimtype)+t5*quad*as.integer(stimtype)), 
                           data = multivariat,
                           fixed = c0+t0 ~1,
                           random = c0+t0+c1+t1+c2+t2+c3+t3+c4+t4+c5+t5 ~1,
                           group = ~vp,
                           start = c(0.1, 50),
                           weights = varIdent(form = ~1|grp),
                           na.action = na.omit, 
                           method = "REML",
                           control = lmeCtlList)
            
          #reducing the structure >> dropping quad as random effect >> max nr of it. reached without convergence
            ran2 = nlme(response ~ 
                             delta1*(c0+c1*lin+c3*as.integer(stimtype)+
                                       c4*lin*as.integer(stimtype)+c5*quad*as.integer(stimtype))+ 
                             delta2*(t0+t1*lin+t3*as.integer(stimtype)+
                                       t4*lin*as.integer(stimtype)+t5*quad*as.integer(stimtype)), 
                           data = multivariat,
                           fixed = c0+t0 ~1,
                           random = c0+t0+c1+t1+c3+t3+c4+t4+c5+t5 ~1,
                           group = ~vp,
                           start = c(0.1, 50),
                           weights = varIdent(form = ~1|grp),
                           na.action = na.omit, 
                           method = "REML",
                           control = lmeCtlList)
            
          #reducing the structure >> dropping lin as random effect >> Singularity in backsolve at level 0
            ran3 = nlme(response ~ 
                             delta1*(c0+c3*as.integer(stimtype)+
                                       c4*lin*as.integer(stimtype)+c5*quad*as.integer(stimtype))+ 
                             delta2*(t0+t3*as.integer(stimtype)+
                                       t4*lin*as.integer(stimtype)+t5*quad*as.integer(stimtype)), 
                           data = multivariat,
                           fixed = c0+t0 ~1,
                           random = c0+t0+c3+t3+c4+t4+c5+t5 ~1,
                           group = ~vp,
                           start = c(0.1, 50),
                           weights = varIdent(form = ~1|grp),
                           na.action = na.omit, 
                           method = "REML",
                           control = lmeCtlList)
  
            
          #reducing the structure >> dropping cs as random effect >> max nr of it. reached without convergence
            ran4 = nlme(response ~ 
                             delta1*(c0+
                                       c4*lin*as.integer(stimtype)+c5*quad*as.integer(stimtype))+ 
                             delta2*(t0+
                                       t4*lin*as.integer(stimtype)+t5*quad*as.integer(stimtype)), 
                           data = multivariat,
                           fixed = c0+t0 ~1,
                           random = c0+t0+c4+t4+c5+t5 ~1,
                           group = ~vp,
                           start = c(0.1, 50),
                           weights = varIdent(form = ~1|grp),
                           na.action = na.omit, 
                           method = "REML",
                           control = lmeCtlList)
            
            
          #reducing the structure >> dropping quadxCS as random effect
            ran5 = nlme(response ~ 
                             delta1*(c0+
                                       c4*lin*as.integer(stimtype))+ 
                             delta2*(t0+
                                       t4*lin*as.integer(stimtype)), 
                           data = multivariat,
                           fixed = c0+t0 ~1,
                           random = c0+t0+c4+t4 ~1,
                           group = ~vp,
                           start = c(0.1, 50),
                           weights = varIdent(form = ~1|grp),
                           na.action = na.omit, 
                           method = "REML",
                           control = lmeCtlList)
          
            #obtain results:
              summary(ran5)
              intervals(ran5, 0.95)
            
            #test if ran5 fits better compared to null with LRT:
              test1 = anova(null, ran5)
            
  

        #####################  TIME VARIABLE ######################
                                          
                                          
          #no growth but estimated with ML for comparison                              
              time0 = nlme(response ~ 
                               delta1*(c0+
                                         c4*lin*as.integer(stimtype))+ 
                               delta2*(t0+
                                         t4*lin*as.integer(stimtype)), 
                             data = multivariat,
                             fixed = c0+t0 ~1,
                             random = c0+t0+c4+t4 ~1,
                             group = ~vp,
                             start = c(0.1, 50),
                             weights = varIdent(form = ~1|grp),
                             na.action = na.omit, 
                             method = "ML",
                             control = lmeCtlList)
                                          
          #linear for both dv
            time1 = nlme(response ~
                         delta1*(c0+c1*lin+
                                   c4*lin*as.integer(stimtype))+ 
                         delta2*(t0+t1*lin+
                                   t4*lin*as.integer(stimtype)), 
                         data = multivariat,
                         fixed = c0+t0+c1+t1 ~1,
                         random = c0+t0+c4+t4 ~1,
                         group = ~vp,
                         start = c(0.1, 50, 0, 0),
                         weights = varIdent(form = ~1|grp),
                         na.action = na.omit, 
                         method = "ML",
                         control = lmeCtlList)
               
            
            #obtain results:
              summary(time1)
              intervals(time1, 0.95)
            
            #test if time1 fits better compared to time0 (without lin effects) with LRT:
              test2 = anova(time0, time1) 
         
    
  
          #linear + quadratic for both dv
                #regarding the results of 'time1' the starting values for fixed 
                #effects will be adapted to facilitate estimation
            time2 = nlme(response ~
                          delta1*(c0+c1*lin+c2*quad+
                                   c4*lin*as.integer(stimtype))+ 
                          delta2*(t0+t1*lin+t2*quad+
                                   t4*lin*as.integer(stimtype)), 
                          data = multivariat,
                          fixed = c0+t0+c1+t1+c2+t2 ~1,
                          random = c0+t0+c4+t4 ~1,
                          group = ~vp,
                          start = c(0.1, 50, 0, -3, 0, 0),
                          weights = varIdent(form = ~1|grp),
                          na.action = na.omit,
                          method = "ML",
                          control = lmeCtlList)
            
            #obtain results:
              summary(time2)
              intervals(time2, 0.95)
            
            #test if time2 fits better compared to time1 (without quad effects) with LRT:
              test3 = anova(time1, time2)
            
            
            #quad effect for SCR was not significant >> gets removed
                   time2.1 = nlme(response ~
                                 delta1*(c0+c1*lin+
                                           c4*lin*as.integer(stimtype))+ 
                                 delta2*(t0+t1*lin+t2*quad+
                                           t4*lin*as.integer(stimtype)), 
                               data = multivariat,
                               fixed = c0+t0+c1+t1+t2 ~1,
                               random = c0+t0+c4+t4 ~1,
                               group = ~vp,
                               start = c(0.1, 50, 0, -3, 0),
                               weights = varIdent(form = ~1|grp),
                               na.action = na.omit,
                               method = "ML",
                               control = lmeCtlList)
                
            #obtain results:
              summary(time2.1)
              intervals(time2.1, 0.95)
                   
            #test if time2.1 (with quad effect for STR) fits better compared to time1 with LRT:
              test4 = anova(time1, time2.1)
            
                  
                  
        ################ CONDITIONAL GROWTH >> ADDING FIXED EFFECT STIMTYPE ##################
        
            #main effect of stimtype
                  #regarding the results of 'time2.1' the starting values for fixed 
                  #effects will be adapted to facilitate estimation
            cs1 = nlme(response ~
                            delta1*(c0+c1*lin+c3*as.integer(stimtype)+
                                     c4*lin*as.integer(stimtype))+ 
                            delta2*(t0+t1*lin+t2*quad+t3*as.integer(stimtype)+
                                     t4*lin*as.integer(stimtype)), 
                            data = multivariat,
                            fixed = c0+t0+c1+t1+t2+c3+t3 ~1,
                            random = c0+t0+c4+t4 ~1,
                            group = ~vp,
                            start = c(0.1, 50, 0, -3, 0.1, 0, 0),
                            weights = varIdent(form = ~1|grp),
                            na.action = na.omit, 
                            method = "ML",
                            control = lmeCtlList)
    
                  #obtain results:
                    summary(cs1)
                    intervals(cs1, 0.95)
                  
                  #test if cs1 fits better compared to time2.1 (no cs effect) with LRT:
                    test5 = anova(time2.1, cs1)
            
  
            #+ cs in interaction with linear term
                #regarding the results of 'cs1' the starting values for fixed 
                #effects will be adapted to facilitate estimation
              cs2 = nlme(response ~
                           delta1*(c0+c1*lin+c3*as.integer(stimtype)+
                                     c4*lin*as.integer(stimtype))+ 
                           delta2*(t0+t1*lin+t2*quad+t3*as.integer(stimtype)+
                                     t4*lin*as.integer(stimtype)), 
                         data = multivariat,
                         fixed = c0+t0+c1+t1+t2+c3+t3+c4+t4 ~1,
                         random = c0+t0+c4+t4 ~1,
                         group = ~vp,
                         start = c(0.1, 50, 0, -3, 0.1, 0, 5, 0, 0),
                         weights = varIdent(form = ~1|grp),
                         na.action = na.omit, 
                         method = "ML",
                         control = lmeCtlList)
              
                    #obtain results:
                      summary(cs2)
                      intervals(cs2, 0.95)
                    
                    #test if cs1 fits better compared to time2.1 (no cs effect) with LRT:
                      test6 = anova(cs1, cs2)
            
              
            #lin*cs effect for STR was not significant >> gets removed
              cs2.1 = nlme(response ~
                           delta1*(c0+c1*lin+c3*as.integer(stimtype)+
                                     c4*lin*as.integer(stimtype))+ 
                           delta2*(t0+t1*lin+t2*quad+t3*as.integer(stimtype)+
                                     t4*lin*as.integer(stimtype)), 
                         data = multivariat,
                         fixed = c0+t0+c1+t1+t2+c3+t3+c4 ~1,
                         random = c0+t0+c4+t4 ~1,
                         group = ~vp,
                         start = c(0.1, 50, 0, -3, 0.1, 0, 5, 0),
                         weights = varIdent(form = ~1|grp),
                         na.action = na.omit, 
                         method = "ML",
                         control = lmeCtlList)
              
                  #obtain results:
                    summary(cs2.1)
                    intervals(cs2.1, 0.95)
                  
                  #test if cs2.1 (lin*cs just for scr) fits better compared to cs2 with LRT:
                    test7 = anova(cs1, cs2.1, cs2)
              
              
            #+ cs in interaction with quadratic time term
            cs3 = nlme(response ~
                         delta1*(c0+c1*lin+c3*as.integer(stimtype)+
                                   c4*lin*as.integer(stimtype)+c5*quad*as.integer(stimtype))+ 
                         delta2*(t0+t1*lin+t2*quad+t3*as.integer(stimtype)+
                                   t4*lin*as.integer(stimtype)+t5*quad*as.integer(stimtype)), 
                       data = multivariat,
                       fixed = c0+t0+c1+t1+t2+c3+t3+c4+c5+t5 ~1,
                       random = c0+t0+c4+t4 ~1,
                       group = ~vp,
                       start = c(0.1, 50, 0, -3, 0.1, 0, 5, 0, 0, 0),
                       weights = varIdent(form = ~1|grp),
                       na.action = na.omit, 
                       method = "ML",
                       control = lmeCtlList)
            
            
                  #obtain results:
                    summary(cs3)
                    intervals(cs3, 0.95)
                  
                  #test if cs3 (quad*cs for both) fits better compared to cs2.1 with LRT:
                    test8 = anova(cs2.1, cs3)
            
            
            
        ################ FINAL MODEL >> NUMBER ONE ##################
                  
                  fin1 = nlme(response ~
                               delta1*(c0+c1*lin+c3*as.integer(stimtype)+
                                         c4*lin*as.integer(stimtype))+ 
                               delta2*(t0+t1*lin+t2*quad+t3*as.integer(stimtype)+
                                         t4*lin*as.integer(stimtype)), 
                             data = multivariat,
                             fixed = c0+t0+c1+t1+t2+c3+t3+c4 ~1,
                             random = c0+t0+c4+t4 ~1,
                             group = ~vp,
                             start = c(0.1, 50, 0, -3, 0.1, 0, 5, 0),
                             weights = varIdent(form = ~1|grp),
                             na.action = na.omit, 
                             method = "REML",
                             control = lmeCtlList)
                  
                  
                  #obtain results:
                      summary(fin1)
                      anova(fin1)
                      intervals(fin1, 0.95, which="all")
                      
                  
            
        ################ ADDITIONAL ANALYSIS ##################    
            #fixed effect csxlin for str too
              fin2 = nlme(response ~
                            delta1*(c0+c1*lin+c3*as.integer(stimtype)+
                                      c4*lin*as.integer(stimtype))+ 
                            delta2*(t0+t1*lin+t2*quad+t3*as.integer(stimtype)+
                                      t4*lin*as.integer(stimtype)), 
                          data = multivariat,
                          fixed = c0+t0+c1+t1+t2+c3+t3+c4+t4 ~1,
                          random = c0+t0+c4+t4 ~1,
                          group = ~vp,
                          start = c(0.1, 50, 0, -3, 0.1, 0, 5, 0, 0),
                          weights = varIdent(form = ~1|grp),
                          na.action = na.omit, 
                          method = "REML",
                          control = lmeCtlList)   
                  #obtain results:
                    summary(fin2)
                    intervals(fin2, 0.95, which="fixed")  
                    anova(fin1, fin2)
                      
            #c1 and t1 as random effects instead of c4 & t4
              fin3 = nlme(response ~
                            delta1*(c0+c1*lin+c3*as.integer(stimtype)+
                                   c4*lin*as.integer(stimtype))+ 
                            delta2*(t0+t1*lin+t2*quad+t3*as.integer(stimtype)), 
                               data = multivariat,
                               fixed = c0+t0+c1+t1+t2+c3+t3+c4 ~1,
                               random = c0+t0+c1+t1 ~1,
                               group = ~vp,
                               start = c(0.1, 50, 0, -3, 0.1, 0, 5, 0),
                               weights = varIdent(form = ~1|grp),
                               na.action = na.omit, 
                               method = "REML",
                               control = lmeCtlList)
                  #obtain results:
                      summary(fin3)
                      intervals(fin3, 0.95, which="all")
                      
            
    