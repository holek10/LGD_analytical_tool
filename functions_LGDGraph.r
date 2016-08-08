fce <- function(dta, ah=40, nfile, store=T) {
    cutp <- 0
    r <- iLGD(dta, cutp=cutp)

    if (store) {
       fn <- paste("Apics//",nfile,".png", sep="")
       png(filename=fn, width=800, height=600)
    }

    par(mfrow=c(2,2))
    r1 <- cLGD(r, cutp=cutp, app.horizon=ah, exclude.closed=T)
    ra <- aLGD(r1, cutp=cutp, app.horizon=ah, exclude.closed=T)
    #aGraph(ra, legend.position="bottom", plot.what="L")
    aGraph(ra, legend.position="top", plot.what="R")
    title(n_closed_excl)

    tabka <- NULL
    tc <- c(12,18,24,36,48,60) # months
    tc <- tc[tc <= ah]
    tabka <- rbind(tc, 100-ra$curve_a[tc]*100)

    r1 <- cLGD(r, cutp=cutp, app.horizon=ah, exclude.closed=F)
    ra <- aLGD(r1, cutp=cutp, app.horizon=ah, exclude.closed=F)
    #aGraph(ra, legend.position="bottom", plot.what="L")
    aGraph(ra, legend.position="top", plot.what="R")
    title(n_closed_incl)

    tabka <- rbind(tabka, 100-ra$curve_a[tc]*100)

    r1 <- cLGD(r, cutp=cutp, app.horizon=ah, exclude.closed=T)
    rw <- wLGD(r1, cutp=cutp, app.horizon=ah, exclude.closed=T, min.length=12)
    #wGraph(rw, legend.position="bottom", plot.what="L")
    wGraph(rw, legend.position="top", plot.what="R")
    title(n_closed_excl)

    tabka <- rbind(tabka, 100-rw$curve_w[tc]*100)

    r1 <- cLGD(r, cutp=cutp, app.horizon=ah, exclude.closed=F)
    rw <- wLGD(r1, cutp=cutp, app.horizon=ah, exclude.closed=F, min.length=12)
    #wGraph(rw, legend.position="bottom", plot.what="L")
    wGraph(rw, legend.position="top", plot.what="R")
    title(n_closed_incl)

    tabka <- rbind(tabka, 100-rw$curve_w[tc]*100)
    rownames(tabka) <- c(n_thm, # "Time horizon (months)",
                         n_aglgd_closedexcluded, #"aLGD closed excluded",
                         n_aglgd_closedincluded, #"aLGD closed included",
                         n_wlgd_closedexcluded, #"wLGD closed excluded",
                         n_wlgd_closedincluded) #"wLGD closed included")

    if (store) {
       dev.off()
       fn <- paste("Apics//",nfile,".csv", sep="")
       write.csv2(tabka, file=fn)
    }
}





# IN : weighted or aggregated or residual LGD object
lgdGraph <- function(dta) { # not yet operational
   if ( class(dta) %in% c("wLGD") ) {
      # aggregated LGD
      # nacteni nebo nastaveni defaults
      wGraph(dta)
   }

   if ( class(dta) %in% c("aLGD") ) {
      # weighted LGD
      # nacteni nebo nastaveni defaults
      aGraph(dta)
   }
   if ( class(dta) %in% c("rLGD") ) {
      # residual LGD (zatim neni hotovo)
      # nacteni nebo nastaveni defaults
      rGraph(dta)
   }
}



# IN : aggregated LGD object
aGraph <- function(dta, legend.position="topright", plot.what,plot.ag="l") {
   preds_a <- dta$preds_a
   curve_a <- dta$curve_a

   co <- unique(preds_a$cohort)                                   #cohorts
       cond <- !is.na(preds_a$rec_observed)

   lo <- tapply(preds_a$period[cond], preds_a$cohort[cond], max)  # lenghts of observed data in each cohort

   if (plot.what=="R") {                                          # final curve [recovery]
      fc <- dta$curve_a
      fw <- dta$curve_w
      yl <- n_agg_recp
   }
   if (plot.what=="L") {                                          # final curve [lgd]
      fc <- 1-dta$curve_a
      fw <- 1-dta$curve_w
      yl <- n_agg_lgdp
   }

   plot(0:(length(fc)-1), fc, type="l", ylim=c(0,1), lwd=1.5,
                              col='white',
                              xlab=n_collh,
                              ylab=yl)                            # initiate plot with axes settings

   for (i in co) {                                                # drawing cohorts
      ctd <- preds_a[preds_a$cohort == i,]                        # cohort to draw
      if (plot.what=="R") { lines(c(0:lo[paste(i)]),     ctd$rec_observed[c(0:lo[paste(i)])+1] , lwd=2, col=EYcolors[2]) }
      if (plot.what=="L") { lines(c(0:lo[paste(i)]), 1 - ctd$rec_observed[c(0:lo[paste(i)])+1] , lwd=2, col=EYcolors[2]) }
   }

   if (plot.ag=="l" | plot.ag=="b") { lines(0:(length(fc)-1), fc, lwd=6, col=EYcolors[7]) }          # final curve logistic
   if (plot.ag=="w" | plot.ag=="b") { lines(0:(length(fw)-1), fw, lwd=6, col=EYcolors[6]) }          # final curve weighted

   grid()

   if (plot.what=="L") { legend(legend.position, legend=c(n_agg_lgd, n_coh), lwd=c(4,4), col=EYcolors[c(7,2)], bg='white') }
   if (plot.what=="R") { legend(legend.position, legend=c(n_agg_rec, n_coh), lwd=c(4,4), col=EYcolors[c(7,2)], bg='white') }

   return("drawed")
}



# IN : weighted LGD object
wGraph <- function(dta, legend.position="topright", plot.what) {
   preds_w <- dta$preds_w
   curve_w <- dta$curve_w

   co <- unique(preds_w$cohort)                                   #cohorts
       cond <- !is.na(preds_w$rec_observed)

   lo <- tapply(preds_w$period[cond], preds_w$cohort[cond], max)  # lenghts of observed data in each cohort
   mlo <- tapply(preds_w$period, preds_w$cohort, max)             # lenghts of observed data in each cohort

   if (plot.what=="R") {                                          # final curve [recovery]
      fc <- dta$curve_w
      yl <- n_we_recp
      leg <- n_we_rec
   }
   if (plot.what=="L") {                                          # final curve [lgd]
      fc <- 1-dta$curve_w
      yl <- n_we_lgdp
      leg <- n_we_lgd
   }

   plot(0:(length(fc)-1), fc, type="l", ylim=c(0,1),
                              col=EYcolors[7],
                              xlab=n_collh,
                              ylab=yl)                            # initiate plot with axes settings

   for (i in co) {                                                # drawing cohorts
      ctd <- preds_w[preds_w$cohort == i,]                        # cohort to draw
      if (plot.what=="L") {
          lines(c(0:lo[paste(i)]), 1- ctd$rec_final[c(0:lo[paste(i)])+1] , lwd=2, col=EYcolors[2])
          lines(c(lo[paste(i)] : mlo[paste(i)]), 1- ctd$rec_final[c(lo[paste(i)] : mlo[paste(i)])+1], col=EYcolors[3])
      }
      if (plot.what=="R") {
          lines(c(0:lo[paste(i)]),    ctd$rec_final[c(0:lo[paste(i)])+1] , lwd=2, col=EYcolors[2])
          lines(c(lo[paste(i)] : mlo[paste(i)]),    ctd$rec_final[c(lo[paste(i)] : mlo[paste(i)])+1], col=EYcolors[3])
      }
   }

   lines(0:(length(fc)-1), fc, lwd=6, col=EYcolors[7])          # final LGD curve

   grid()

   legend(legend.position, legend=c(leg, n_coh_obs, n_coh_pred), lwd=c(4,4,2), col=EYcolors[c(7,2,3)], bg='white')

   return("drawed")
}






# IN : residual LGD object
rGraph <- function(dta, legend.position = "topright", plot.what = "R", cutperiod = 0) {
    
    # rLGD class object is required, if not, it is created
    if(!is.rLGD(dta)) {
      stop("Input dataset is not rLGD object!")
    }
    
    # extract cohorts data, final LGD curve data, and maximum observed period for each cohort
    res_cohorts <- dta$res_cohorts
    res_curve <- dta$res_curve[ as.numeric(row.names(dta$res_curve)) == cutperiod, ]
    max_period_observed <- dta$max_period_observed
        
    # preparation for final LGD curve graph creation
    if (plot.what == "R") {                                          # final curve [recovery]
      y_values_final_curve <- res_curve
      ylabel <- n_res_recp
      legend <- n_res_rec
    }
    if (plot.what == "L") {                                          # final curve [lgd]
      y_values_final_curve <- 1 - res_curve
      ylabel <- n_res_lgdp
      legend <- n_res_lgd
    }
    
    # initiate plot with axes settings - LGD/Recover final curve displayed
    plot(0:(length(y_values_final_curve)-1),
         y_values_final_curve, type="l", ylim=c(0,1),
         col=EYcolors[7],
         xlab="n_collh",
         ylab =" ylabel")                            
    
    # draw line for each particular cohort in the above created  LGD/Recover final curve graph
    for(i in 1:length(res_cohorts) ){
      pom_dta <- res_cohorts[[i]]
      
      # data selection - cutperiod applied
      yvalues_coh <- pom_dta[as.numeric(row.names(pom_dta)) == cutperiod, ]
      
      # maximum period observed for a particular cohort
      max_obs_period <- max_period_observed[as.numeric(names(max_period_observed)) == as.numeric(names(res_cohorts)[i])]
      
      
      # LGD version
      if (plot.what == "L") {
        lines(c(0:max_obs_period), 1 - yvalues_coh[c(0:max_obs_period)+1] , lwd=2, col=EYcolors[2])
        lines(c(max_obs_period : (length(yvalues_coh)-1)),
              1 - yvalues_coh[c(max_obs_period : (length(yvalues_coh)-1))+1], col=EYcolors[3])
      }
      # Recovery version
      if (plot.what == "R") {
        lines(c(0:max_obs_period),  yvalues_coh[c(0:max_obs_period)+1] , lwd=2, col=EYcolors[2])
        lines(c(max_obs_period : (length(yvalues_coh)-1)),
              yvalues_coh[c(max_obs_period : (length(yvalues_coh)-1))+1], col=EYcolors[3])
      }
      
    }
    
        
    lines(0:(length(y_values_final_curve)-1), y_values_final_curve, lwd=6, col=EYcolors[7])          # final LGD curve
    
    grid()
    
    legend(legend.position, legend=c(legend, n_coh_obs, n_coh_pred), lwd=c(4,4,2), col=EYcolors[c(7,2,3)], bg='white')
    
    return("drawed")
}


# namalovani obrazku
#   km <- length(z$lgdrez)-1
#   plot(c(0:km), z$lgdrez, ylim=c(0,1), xlim=c(0,km), type="l", col=EYcolors[1], lwd=4, xlab="Months since default",
#                  ylab="Residual LGD (%) / Relative exposure (%) / Loss (%)", main=tit)
#
#   lines(c(0:km), z$eadrez_rel, col=EYcolors[4], lwd=4)
#   lines(c(0:km), z$lgdrez*z$eadrez_rel, col=EYcolors[7], lwd=5)
#   grid()
#   legend(l, legend=c("Residual LGD", "Relative exposure", "Loss"),
#            lwd=c(3,3,3), col=EYcolors[c(1,4,7)],bg='white')

#}
#------------------------------------------
















