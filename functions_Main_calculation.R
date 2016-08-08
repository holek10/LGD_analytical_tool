



  
main_calculation <- function(dta, dta_sec, hist_length, horizon, default_length, include_closed, include_open, 
                             settings_secured, months_to_report) 
  
  {
      
    #---- calculation for unsecured part ---
    max_cohort <- max(dta$cohort, na.rm=T)
    min_cohort <- date_add(max_cohort, -1 * hist_length  )   
    dta <- dta[dta$cohort > min_cohort, ]     
    # apply LGD functions
    individual_LGD <- iLGD(dta)
    cohort_LGD <- cLGD(individual_LGD ,app.horizon= horizon, include.closed= include_closed,include.open= include_open)
    aggregated_LGD <- aLGD(cohort_LGD, cutp=0 ,app.horizon = horizon, include.closed=include_closed ,include.open=include_open)
    weighted_LGD <- wLGD( cohort_LGD ,cutp=0 ,app.horizon= horizon, min.length = default_length, include.closed=include_closed, include.open=include_open)
    residual_LGD <- rLGD(weighted_LGD)  
    #ead_whole_portfolio <- sum(weighted_LGD$preds_w$ead[weighted_LGD$preds_w$period==0])
    ead_whole_portfolio <- sum(dta$ex[dta$period==0 & dta$cohort <= date_add(max_cohort, -1 * default_length  ) ], na.rm=T)
    #ead_whole_portfolio <- sum(dta$ex[dta$period==0])
    # recovery rates
    #months_to_show <- isolate(months_to_report[months_to_report <= horizon ] ) 
    months_to_show <- months_to_report[months_to_report <= horizon ] 
    output_unsecured <- t(as.matrix(weighted_LGD$curve_w[months_to_show+1]))
    rownames(output_unsecured) <- NULL
    colnames(output_unsecured) <- paste(colnames(output_unsecured),"M",sep="")

    #--- calculation for secured part ---
    
    #--- skip it if no collaterals exist for the given portfolio OR closed loans only were chosen
    if(nrow(dta_sec)==0 | (include_closed ==T & include_open ==F ) ) {
      dta_sec <- dta_sec[numeric(0),]
      #dta_sec <- 0
      dta_sec_collateral_value_by_type <- 0
      } else {
             
      # add collateral realization time ( relative to default date) and haircut value
      dta_sec$realization_time <- settings_secured$realization_time[match(dta_sec$collateral_type_name, settings_secured$collateral_type)] - dta_sec$months_in_default 
      dta_sec$haircut_value <- settings_secured$haircut[match(dta_sec$collateral_type_name, settings_secured$collateral_type)] 
      
      # limit dataset to match unsecured data (take records with prescribed min. default length and within specified collateral realization time )
      dta_sec <- dta_sec[dta_sec$months_in_default >= default_length & dta_sec$realization_time >=0 ,]
     
      #         if ( dataset_upload()$type == "current") {
      #           dta_sec$realization_time[!is.na(dta_sec$collateral_type)] <- unlist(lapply(dta_sec$collateral_type[!is.na(dta_sec$collateral_type)],
      #                                                                   function(x) input[[paste("realization_time_",x,sep="")]] ))
      #           dta_sec$haircut_value[!is.na(dta_sec$collateral_type)] <- unlist(lapply(dta_sec$collateral_type[!is.na(dta_sec$collateral_type)],
      #                                                                   function(x) input[[paste("haircut_",x,sep="")]] ))    
      #         }
      #         if ( dataset_upload()$type == "historical") {
      #           dta_sec$realization_time[!is.na(dta_sec$collateral_type)] <- unlist(lapply(dta_sec$collateral_type[!is.na(dta_sec$collateral_type)],
      #                                                                     function(x)   settings_secured$realization_time[settings_secured$collateral_type == x]))
      #           dta_sec$haircut_value[!is.na(dta_sec$collateral_type)] <- unlist(lapply(dta_sec$collateral_type[!is.na(dta_sec$collateral_type)],
      #                                                                     function(x)   settings_secured$haircut[settings_secured$collateral_type == x]))  
      #         }
      
      #--- aggregate the collateral value on the accid level
      collateral_value_accid <- tapply(dta_sec$collateral_value, dta_sec$accid, sum, na.rm=T)
      dta_sec$collateral_value_accid <- as.numeric(collateral_value_accid[match(dta_sec$accid, names(collateral_value_accid))])
      
      #--- calculate the recovery from the collateral given the realization time and haircut value
      dta_sec$collateral_recovery <- (dta_sec$collateral_value * (1 - dta_sec$haircut_value)) / ((1 + dta_sec$eir)^ dta_sec$realization_time )
      
      #--- aggregate the collateral recoveries on the accid level
      collateral_recovery_accid <- tapply(dta_sec$collateral_recovery, dta_sec$accid, sum, na.rm=T)
      dta_sec$collateral_recovery_accid <- as.numeric(collateral_recovery_accid[match(dta_sec$accid, names(collateral_recovery_accid))])
      
      #--- limit the recovery to the ead of the accid, so that there aren't "spillover" to other account in case of bigger recovery
      dta_sec$collateral_recovery_accid <- ifelse(dta_sec$collateral_recovery_accid > dta_sec$exposure , dta_sec$exposure, dta_sec$collateral_recovery_accid)
      dta_sec$collateral_recovery_rate_accid <- dta_sec$collateral_recovery_accid / dta_sec$exposure
      
      dta_sec_collateral_value_by_type <- tapply(dta_sec$collateral_value, dta_sec$collateral_type_name, sum, na.rm=T)
    }

    #--- old code, keeping it temporarily just for testing purposes
    #       dta_sec$collateral_value_haircut <- dta_sec$collateral_value * (1 - dta_sec$haircut_value)
    #       dta_sec$LGD <- 1 - ( dta_sec$collateral_value * (1 - dta_sec$haircut_value) ) / (dta_sec$exposure * (1 + dta_sec$eir)^ dta_sec$realization_time )
    #       dta_sec$LGD <- ifelse(dta_sec$LGD < 0 , 0, dta_sec$LGD)
    #       collateral_hairuct_aggregate <- tapply(dta_sec$collateral_value_haircut, dta_sec$accid, sum, na.rm=T)
    #       dta_sec$collateral_value_haircut_accid <- as.numeric(collateral_hairuct_aggregate[match(dta_sec$accid, names(collateral_haircut_aggregate))])
    #       average_LGD_accid <- tapply(dta_sec$LGD, dta_sec$accid, mean, na.rm=T)
    #       dta_sec$average_LGD <- as.numeric(average_LGD_accid[match(dta_sec$accid, names(average_LGD_accid))])
    #       dta_sec$collateral_discnt_rec <- (dta_sec$collateral_value * (1 - dta_sec$haircut_value)) / ((1 + dta_sec$eir)^ dta_sec$realization_time )
    #       collateral_discnt_rec_accid <- tapply(dta_sec$collateral_discnt_rec, dta_sec$accid, sum, na.rm=T)
    #       dta_sec$collateral_value_discnt_rec_accid <- as.numeric(collateral_discnt_rec_accid[match(dta_sec$accid, names(collateral_discnt_rec_accid))])
    
    #--- create final datasets to use in other functions ---
    final_dta_unsec <-dta[dta$period==0 & dta$cohort <= date_add(max_cohort, -1 * default_length  ),]
    #final_dta_unsec <- weighted_LGD$preds_w[weighted_LGD$preds_w$period ==0,]
    final_dta_sec <- dta_sec[!duplicated(dta_sec$accid),]
    
    #--- EAD of the secured part of portfolio (i.e. only loans at the latest date and with existing collateral)
    ead_secured <- sum(final_dta_sec$exp)
    
    #-- old version, keeping it just for testing purposes
    #portfolio_recovery_rate <- sum(final_dta_sec$collateral_recovery_accid,na.rm=T)/sum(final_dta_sec$exposure)
    
    #--- FINAL RECOVERY RATE FROM THE COLLATERAL, WHICH IS REPORTED
    output_secured <- sum(final_dta_sec$collateral_recovery_accid,na.rm=T)/ead_whole_portfolio

    # MAIN OUTPUT TABLE
    summary_main <- c(
      format(round(nrow(final_dta_unsec),0), nsmall = 0, big.mark = " " ),
      format(round(ead_whole_portfolio,0),nsmall = 0, big.mark = " " ),
      format(round(output_unsecured[length(output_unsecured)]*100,2), nsmall = 2),
      format(round(output_secured*100,2),nsmall=2),
      #format(round(mean(1-dta$average_LGD, na.rm=T)*100,2), nsmall = 2),
      format(round(ifelse(output_unsecured[length(output_unsecured)]+output_secured>1,1,output_unsecured[length(output_unsecured)]+output_secured)*100,2), nsmall = 2),
      format(round((1 - ifelse(output_unsecured[length(output_unsecured)]+output_secured>1,1,output_unsecured[length(output_unsecured)]+output_secured))*100,2), nsmall = 2)
    )
    
    summary_main_names <- c(
      "Count of loan IDs", 
      "Exposure at default",
      paste("Recovery - cash flows after ",colnames(output_unsecured)[length(output_unsecured)]," (%)", sep=""),
      "Recovery - collateral (%)", 
      "Total recovery (%)", 
      "LGD (%)"
    )
    
    output_table_main <- t(data.frame(summary_main))
    colnames(output_table_main) <- summary_main_names
    
    #--- UNSECURED OUTPUT TABLE                       
    summary_unsecured <- c(
      format(round(nrow(final_dta_unsec),0), nsmall = 0, big.mark = " "),
      format(round(sum(final_dta_unsec$ex, na.rm=T),0), nsmall = 0, big.mark = " "),
      format(round(nrow(final_dta_unsec[final_dta_unsec$closed == 0,]),0), nsmall = 0, big.mark = " "),
      format(round(sum(final_dta_unsec$ex[final_dta_unsec$closed == 0], na.rm=T),0), nsmall = 0, big.mark = " "),
      format(round(nrow(final_dta_unsec[final_dta_unsec$closed == 1,]),0), nsmall = 0, big.mark = " "),
      format(round(sum(final_dta_unsec$ex[final_dta_unsec$closed == 1], na.rm=T),0), nsmall = 0, big.mark = " "),
      format(round(output_unsecured*100,2), nsmall = 2 )
    ) 
    
    summary_unsecured_names <- c(
      "Total count",
      "Total exposure",
      "- Open loans - count", 
      "- Open loans - exposure", 
      "- Closed loans - count",
      "- Closed loans - exposure",
      paste("Recovery after ",colnames(output_unsecured)," (%)",sep="")
    )
    
    output_table_unsecured <- data.frame("Description" = summary_unsecured_names, "Value" = summary_unsecured)
    rownames(output_table_unsecured) <- NULL
    
    #--- SECURED OUTPUT TABLE
    summary_secured <- c(
      format(round(nrow(final_dta_unsec),0), nsmall = 0, big.mark = " "),
      format(round(sum(final_dta_unsec$ex, na.rm=T),0), nsmall = 0, big.mark = " "),
      format(round(nrow(final_dta_sec),0), nsmall = 0, big.mark = " "),
      format(round(sum(final_dta_sec$exposure, na.rm=T),0), nsmall = 0, big.mark = " "),        
      format(round(sum(final_dta_sec$collateral_value_accid, na.rm=T),0), nsmall = 0, big.mark=" "),
      format(round(dta_sec_collateral_value_by_type,0), nsmall = 0, big.mark = " "),      
      format(round(sum(final_dta_sec$collateral_recovery_accid, na.rm=T),0), nsmall = 0, big.mark=" "),        
      format(round(output_secured*100,2), nsmall = 2 )
    )
    
    summary_secured_names <- c(
      "Count of loan IDs",
      "Exposure at default",
      "- Count of loan IDs with collateral",
      "- Exposure at default of collateralized loans",
      "Collateral value", 
      paste("- ",names(dta_sec_collateral_value_by_type),sep=""), 
      "Collateral recovery",
      "Collateral recovery (%)"
    )
    
    output_table_secured <- data.frame("Description" = summary_secured_names, "Value" = summary_secured)
    rownames(output_table_secured) <- NULL

    #-- define function outputs --
    output <- list( dataset_secured = final_dta_sec, 
                    dataset_unsecured = final_dta_unsec, 
                    ead_whole_portfolio = ead_whole_portfolio,
                    ead_secured = ead_secured,
                    recovery_unsecured = output_unsecured,
                    recovery_secured = output_secured,
                    collateral_value_by_type = dta_sec_collateral_value_by_type,
                    weighted_LGD = weighted_LGD,
                    residual_LGD = residual_LGD,
                    output_table_main = output_table_main,
                    output_table_unsecured = output_table_unsecured,
                    output_table_secured = output_table_secured)
    
    return(output)

}
