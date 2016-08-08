date_diff <- function(start_date, end_date) {
  
  months_start <- as.numeric(substr(start_date,1,4))*12 + as.numeric(substr(start_date,5,6))
  months_end <-  as.numeric(substr(end_date,1,4))*12 + as.numeric(substr(end_date,5,6))
  period_diff <- months_end - months_start
  
  return(period_diff)
}


date_add <- function(start_date, diff_month) {
  start_year <- as.numeric(substr(start_date,1,4))
  start_month <- as.numeric(substr(start_date,5,6))
  
  start_count_months <- start_year*12 + start_month
  end_count_months <- start_count_months + diff_month

  end_month <- ifelse(end_count_months%%12 == 0, 12, end_count_months%%12) 
  end_year <- (end_count_months - end_month) / 12
  
  
  if(end_month < 10) { end_month <- paste("0",end_month, sep="")
  } 
  
  end_date <- as.numeric(paste(end_year, end_month, sep=""))
      
  return(end_date)  
}
