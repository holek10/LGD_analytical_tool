source("functions_Colors.r")
is.iLGD <- function(obj) { if ("iLGD" %in% class(obj)) { return(T) } else { return(F) } }
is.cLGD <- function(obj) { if ("cLGD" %in% class(obj)) { return(T) } else { return(F) } }
is.wLGD <- function(obj) { if ("wLGD" %in% class(obj)) { return(T) } else { return(F) } }
is.rLGD <- function(obj) { if ("rLGD" %in% class(obj)) { return(T) } else { return(F) } }

log.growth.estim <- function(x.observed, y.observed, x.evaluate, estim.method = 1, weights = rep(1,length(x.observed)), printpar = F) {

  # This function performs estimation of logistic growth function (for given data) and its evaluation for the x-points "xeval".
  #
  # Args:
  #   x.observed          - VECTOR OF NUMERIC - x-values, for which the y-values "y.observed" are observed (e.g. time)
  #   y.observed          - VECTOR OF NUMERIC - observed y-values on which the estimation will be based (corresponding to "x.observed")
  #   x.evaluate          - VECTOR OF NUMERIC - x-points, for which the estimated logistic growth function is evaluated
  #   estim.method        - NUMERIC - method of estimation - if estim.method = 1 then (weighted) residual sum of squares is minimized;
  #                                                           otherwise (weighted) sum of absolute differences (y.observed - fit) is minimized
  #   weights             - VECTOR OF NUMERIC - weights to be used for the optimization function - e.g. weighted residual sum of squares
  #   printpar            - BOOLEAN - if TRUE then estimated parameters are printed out in the console
  #
  # Returns:
  #   estimation          - VECTOR OF NUMERIC - estimation of logistic growth function for x-points "x.evaluate"


    log.growth.evaluate <- function(a,c,d,e=0,x) {
      # This function evaluates logistic growth function (with given parameters) for x-points "x"
      # Args:
      #   a, c, d, e          - NUMERIC - parameters of the logistic growth function
      #   x                   - VECTOR OF NUMERIC - x-points, for which the logistic growth function is evaluated
      #
      # Returns:
      #   evaluation          - VECTOR OF NUMERIC - evaluation of the logistic growth function(with given parameters)
      #                                             for the x-points "x"

      evaluation <- a / (1+exp(-c*(x-d))) + e
      return(evaluation)
    }


    optimization.func <- function(par, x.observed, y.observed, estim.method = 1, weights = rep(1,length(x.observed))) {
      # This function is the optimization function for estimation purposes (estimation of the logistic growth function) - i.e.
      # it returns (weighted) residual sum of squares or (weighted) sum of absolute differences (y.observed - fit) for given
      # estimated parameters "par". Estimation procedure "optim" (shown below) finds parameters which minimize this value
      #
      # Args:
      #   par                 - VECTOR OF NUMERIC - estimated parameters of the logistic growth function -
      #                                             it has to be of length 4 -> "par" = c(a, c, d, e) - see function log.growth.evaluate()
      #   x.observed          - VECTOR OF NUMERIC - x-values, for which the y-values "y.observed" are observed (e.g. time)
      #   y.observed          - VECTOR OF NUMERIC - observed y-values on which the estimation will be based (corresponding to "x.observed")
      #   estim.method        - NUMERIC - method of estimation - if estim.method = 1 then (weighted) residual sum of squares is minimized;
      #                                                           otherwise               (weighted) sum of absolute differences (y.observed - fit) is minimized
      #   weights             - VECTOR OF NUMERIC - weights to be used for the optimization function - e.g. weighted residual sum of squares
      #
      # Returns:
      #   crit                - NUMERIC - if "estim.method" = 1 then (weighted) residual sum of squares;
      #                                   otherwise                   (weighted) sum of absolute differences (y.observed - fit)
      #                                   for given estimated parameters "par"

        fit <- log.growth.evaluate(par[1],par[2],par[3],par[4],x.observed) # fitted values of y, based on estimated parameters "par"
        if (estim.method==1) {crit <- sum(weights*(fit-y.observed)^2)}
        else {crit <- sum(weights*abs(fit-y.observed))}
        return(crit)
    }

# initial estimation of the parameters
    d0 <- 0
    c0 <- (y.observed[4]-y.observed[1]) / (x.observed[4]-x.observed[1])
    a0 <- min(c( (y.observed[4]-max(y.observed)) / (1/(1+exp(-c0*x.observed[4])) - 1),1))
    e0 <- max(y.observed) - a0

# MAIN PART OF THE FUNCTION - estimation of the logistic growth function is performed
# Estimation procedure is performed in iterations - it converges to the final estimation
# using  limited-memory modification of the quasi-Newton method.
# This estimation procedure finds the parameters a, c, d, e of the logistic growth function,
# so that:
# if "estim.method" = 1      (weighted) residual sum of squares is minimized;
# otherwise                  (weighted) sum of absolute differences (y.observed - fit) is minimized.

    coef <- optim( c(a0, c0, d0, e0),
                   optimization.func, gr = NULL,
                   x.observed, y.observed,
                   estim.method, weights,
                   method = "L-BFGS-B",
                   lower = c(0,0,-max(x.observed),-10),
                   upper = c(2,1,max(x.observed),max(y.observed))
                 )$par

     if (printpar) {
        print(rbind(c(a0,c0,d0,e0),coef[1:4]))
     }

     estimation <- log.growth.evaluate(coef[1],coef[2],coef[3],coef[4],x.evaluate)
     return(estimation)
}
#------------------------


iLGD <- function(dta, cutp = 0, trunc.method = "above1", allow.negative.cf = F) {
  # This function calculates individual LGD (observed only)
  #
  # Args:
  #   dta                 - DATASET - LGD-AdaptedSourceData, restriction=NA
  #   cutp                - NUMERIC - take into account only observations after "cutp" months from default ("cutp" month included)
  #   trunc.method        - STRING -
  #                         truncations of recovery for both sides (below(0) and upper(1) boundaries - "above1below0");
  #                         for upper(1) boundary only - "above1";
  #                         for below(0) boundary only - "below0"
  #   allow.negative.cf   - BOOLEAN - argument indicating whether negative cash flows are allowed
  #
  # Returns:
  #   individual.LGD      - iLGD object, list object - list of length 5:
  #                         rec               - DATASET -  individual recovery [id, cohort, period, closed, dcf, cdcf, exp, rec] -- see legend below
  #                         lgd               - DATASET -  individual lgd
  #                         cutp              - NUMERIC - cutperiod used
  #                         truncated         - VECTOR OF NUMERIC - vector of IDs with observations that were truncated
  #                         allow.negative.cf - BOOLEAN - argument indicating whether negative cash flows are allowed
  #
  # LEGEND:
  #   cf                  - cash-flows
  #   dcf                 - discounted cash-flows
  #   cdcf                - cummulative discounted cash-flows
  #   ead                 - exposure at default
  #   rec                 - recovery


  # transforming data acc. to required time-cut,
  # i.e. take into account only observations after "cutp" months from default
  dta <- dta[dta$period >= cutp,]
  dta$period <- dta$period - cutp

  # when there is non-zero cash-flow in the 0th period, set cf=0 for the 0th period
  cond <- dta$period == 0
  #dta$ex[cond] <- dta$ex[cond] - dta$cf[cond]
  dta$cf[cond] <- 0

  # discounting cash-flows acc. to ir
  if (sum(dta$ir > 1, na.rm = T) > 0) {stop( 'Data contain inconsistent interest rate values. IR > 100%.') }
  dta$dcf <- dta$cf/((1+dta$ir)^dta$period)

  # for each ID we create a matrix according to the "maxperiod" of the cohort - i.e. each ID should have
  # "maxperiod" observations.
  # The correct "maxperiod" for each ID is created in data preparation phase (observation maxperiod, CF equal to 0)

  # co <- dta$cohort[!duplicated(dta$id)]                          # changed - see below
  num_rows  <- tapply(dta$period, dta$cohort, max)                 # calculates number of rows for each cohort

  cohorts <- unique(dta$cohort)                                          # all unique cohorts
  num_rows <- num_rows[match(cohorts, as.numeric(names(num_rows)))] + 1  # matches cohorts to their calculated number of rows
  nrow.dataset <- cbind(cohorts, num_rows)                               # dataset containing cohorts and their corresponding number of rows
  colnames (nrow.dataset) <- c("cohort", "number_of_rows")

  # temporary supporting data.frame
  # to be filled-in with specified data
  rec <- dta[!duplicated(dta$id), c("id", "cohort")]
  # attach nrows for the corresponding cohort
  rec <- merge(rec, nrow.dataset, by = "cohort", all.x = T)

  # for each ID create "number_of_rows" rows
  rec <- data.frame(id     = rep(rec$id,rec$number_of_rows),
                    cohort = rep(rec$cohort,rec$number_of_rows),
                    period = sequence(rec$number_of_rows)-1)
  rec <- merge(dta, rec, all.y = T, by = c ("cohort", "id", "period")) # merge dta and rec

  # cumulative discounted cash-flows
  rec <- rec[order(rec$id, rec$period, decreasing = F), ]   # order by ID and Period increasing,
                                                            # so that cdcf values are added correctly to the rec dataset - see unlist below
  dcf <- rec$dcf
  dcf[is.na(dcf)] <- 0  # replace NA with 0 for proper cumsum calculation
  rec$cdcf <- unlist(by(dcf, rec$id, cumsum))               # unlist() works correctly thanks to rec dataset ordering (see above)

  # check that order of IDs in by function is correct
  check <- sum(rec$id[!duplicated(rec$id)] != as.numeric(names(by))) # this should equal 0

  # determination of exposures in all periods
  # help function - moving last known exposure forward - for the added periods and the periods missing in the original data
  fceH <- function(vec) {
    return(approx(vec, method="constant", xout=seq_along(vec), rule=2)$y) # NA values fills with the closest lower value
  }


  # exposures (last known moved forward)
  rec$exp  <- unlist(by(rec$ex, rec$id, fceH))                    # unlist() works correctly thanks to rec dataset ordering (see above)

  # imputation of the "closed" status to fill-in the whole vector
  rec$closed <- as.logical(unlist(by(rec$closed, rec$id, fceH)))  # unlist() works correctly thanks to rec dataset ordering (see above)
  rm(fceH)

  # EADs for each ID in a row
  ead <- rec[rec$period==0, c("id", "ex")]
  colnames (ead) <- c("id", "ead")
  # add EAD to data rec
  rec <- merge(rec, ead, by = "id", all.x = T)


  # recovery computation
  rec$rec <- ifelse(rec$ead == 0,0,rec$cdcf/rec$ead)

  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ##     recovery is defined as non-decreasing function, hence function cummax (see below) is used - ONLY IF NEGATIVE CASH FLOWS ARE NOT ALLOWED
  ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  if(! allow.negative.cf) {
    rec <- rec[ order(rec$id, rec$period), ]    # order by ID and Period increasing,
                                                # so that rec values are added correctly to the rec dataset - see unlist below
    non_decreasing_rec <- tapply(rec$rec, rec$id, cummax)
    rec$rec_orig <- rec$rec                     # keep the original rec - before adjustment to non-decreasing function
    rec$rec <- unlist(non_decreasing_rec)       # unlist() works correctly thanks to rec dataset ordering (see above)
  }

  # truncations of recovery for both sides (below and upper boundaries - "above1below0");
  #                         for upper boundary only - "above1";
  #                         for below boundary only - "below0"
  switch (trunc.method,
          ## above1below0 version
          above1below0 = {
            truncated0 <- unique(rec$id[rec$rec < 0])
            truncated1 <- unique(rec$id[rec$rec > 1])
            truncated <- c(truncated0, truncated1)

            rec$rec[rec$rec < 0] <- 0
            rec$rec[rec$rec > 1] <- 1
            if (length(truncated0) > 0) {
              txt <- paste(length(truncated0), " IDs with recoveries < 0 truncated")
              warning(txt)
            }
            if (length(truncated1) > 0) {
              txt <- paste(length(truncated1), " IDs with recoveries > 1 truncated")
              warning(txt)
            }
          },
          ## above1 version
          above1 = {
            truncated <- unique(rec$id[rec$rec > 1])

            rec$rec[rec$rec > 1] <- 1
            if (length(truncated) > 0) {
              txt <- paste(length(truncated), " IDs with recoveries > 1 truncated")
              warning(txt)
            }

          },
          ## below0 version
          below0 = {
            truncated <- unique(rec$id[rec$rec < 0])

            rec$rec[rec$rec < 0] <- 0
            if (length(truncated) > 0) {
              txt <- paste(length(truncated), " IDs with recoveries < 0 truncated")
              warning(txt)
            }
          },
          ## default version
          { truncated <- "none" })


  # individual LGDs
  # add max_period from nrow.dataset
  # lgd <- data.frame(id=na, lgd=1-rec$rec[cumsum(nr)], ead=ead, closed = closed )
  rec <- merge(rec, nrow.dataset, by = "cohort")
  rec$max_period <- rec$number_of_rows - 1               # add maximum period (since periods are counted from 0 ->  max period = number_of_periods - 1)
  rec <- rec [, colnames(rec) != "number_of_rows"]       # exclude number of rows
  rec <- rec[, c(2, 3, 1, 4:length(colnames(rec)))]      # change order of columns so that ID and period are the first two
  rec <- rec[ order(rec$id, rec$period), ]               # order by id and period increasing
  rec$lgd <- 1-rec$rec                                   # add LGD
  lgd <- rec[rec$period == rec$max_period, c("id","lgd","ead","closed")]

  # data reduction
  #rec <- rec[,c("id","cohort","period","closed","dcf","cdcf","exp","rec","ead")]

  # output

  individual.LGD <- list(rec = rec, lgd = lgd, cutp = cutp, truncated = truncated, allow.negative.cf = allow.negative.cf)
  class(individual.LGD) <- c("list","iLGD")
  return(individual.LGD)
}
#------------------------


cLGD <- function(dta, cutp = 0, app.horizon, include.closed, include.open, trunc.method = "above1", allow.negative.cf = F) {

  # This function calculates cohort LGD (observed only) - i.e. for each cohort it collects corresponding IDs and their cash flows
  # and Exposure at defaults. Using those data recovery for each cohort is computed.
  #
  # Args:
  #   dta                 - DATASET - LGD-AdaptedSourceData or iLGD object ( if not iLGD object, it is created using the function iLGD() and input data)
  #   cutp                - NUMERIC - take into account only observations after "cutp" months from default ("cutp" month included)
  #   app.horizon         - STRING - (0, app.horizon) is the period window of our interest;
  #                                   i.e. LGD is calculated (predicted) for periods 0, 1, ..., app.horizon
  #   include.closed      - BOOLEAN - include/exclude close accounts
  #   include.open        - BOOLEAN - include/exclude open accounts
  #   trunc.method        - STRING -
  #                         truncations of recovery for both sides (below(0) and upper(1) boundaries - "above1below0");
  #                         for upper(1) boundary only - "above1";
  #                         for below(0) boundary only - "below0"
  #   allow.negative.cf   - BOOLEAN - argument indicating whether negative cash flows are allowed
  #
  # Returns:
  #   cohort.LGD          - cLGD object, list object - list of length 4:
  #                         rec               - DATASET - source data, aggregated to cohorts, cumulative dcf
  #                         cutp              - NUMERIC - cutperiod used
  #                         app.horizon       - NUMERIC - (0, app.horizon) is the period window of our interest;
  #                                                     i.e. LGD is calculated (predicted) for periods 0, 1, ..., app.horizon
  #                         include.closed    - BOOLEAN - closed accounts included/excluded
  #                         include.open      - BOOLEAN - open accounts included/excluded
  #                         truncated         - VECTOR OF NUMERIC - vector of Cohort IDs with observations that were truncated
  #                         allow.negative.cf - BOOLEAN - argument indicating whether negative cash flows are allowed
  #
  # LEGEND:
  #   cf                  - cash-flows
  #   dcf                 - discounted cash-flows
  #   cdcf                - cummulative discounted cash-flows
  #   ead                 - exposure at default
  #   rec                 - recovery

   #------
   # input data check (best to be iLGD object, otherwise attempt to create one)
   if(is.iLGD(dta)) {
      if(dta$cutp != cutp) { stop("Cut Points Mismatch - iLGD object was computed with different settings than required.") }
      if(dta$allow.negative.cf != allow.negative.cf) { stop("Allow negative cash flows Mismatch - iLGD object was computed with different settings than required.") }
   } else  {
      dta <- iLGD(dta, cutp = cutp, trunc.method = trunc.method, allow.negative.cf = allow.negative.cf )
   }

   #------
   # before the aggregation of cohorts it is checked whether to exclude/include closed accounts
   if (include.open && !include.closed) {         # include only open accounts
      exclude <- unique(dta$rec$id[dta$rec$closed == 1])
      dta$rec <- dta$rec[!(dta$rec$id %in% exclude), ]
      dta$lgd <- dta$lgd[!(dta$lgd$id %in% exclude), ] # ead column present here
   }
   if (!include.open && !include.closed) {  stop("Cannot exclude everything")

   }
   if (!include.open && include.closed) {         # include closed accounts only
      exclude <- unique(dta$rec$id[dta$rec$closed==0])
      dta$rec <- dta$rec[!(dta$rec$id %in% exclude), ]
      dta$lgd <- dta$lgd[!(dta$lgd$id %in% exclude), ] # ead column present here
   }

   #------
   # aggregation of iLGD data to cohorts (some cohorts may not be present)
   cohorts <- unique(dta$rec$cohort)                                # all unique cohorts ID (numeric)
   max_period <- tapply(dta$rec$period, dta$rec$cohort, max)        # maximum period for each cohort

   num_rows <- max_period[match(cohorts, as.numeric(names(max_period)))] + 1    # matches cohorts to their calculated maximum period -
                                                                                # since periods start with 0 -> number of periods = maximum_period +1
   nrow.dataset <- cbind(cohorts, num_rows)                                     # dataset containing cohorts and their corresponding number of rows
   colnames (nrow.dataset) <- c("cohort", "number_of_rows")


   rec <- data.frame(cohort = rep(cohorts,num_rows),
                     period = sequence(num_rows)-1)


   # cumulative discounted cash-flow computation
   cdcf <- tapply(dta$rec$cdcf, list(dta$rec$cohort, dta$rec$period), sum)
   rec <- rec[order(rec$cohort,rec$period), ]            # rec dataset ordered, so that cdcf values are added correctly to the rec dataset


   rec$cdcf <- na.omit(as.vector(t(cdcf)))               # as.vector() works correctly thanks to rec dataset ordering (see above)

   # exposure computation
   exp <- tapply(dta$rec$exp, list(dta$rec$cohort, dta$rec$period), sum)
   rec$exp <- na.omit(as.vector(t(exp)))                 # as.vector() works correctly thanks to rec dataset ordering (see above)

   # EADs for each cohort
   ead <- rec[rec$period == 0, c("cohort", "exp")]          # exposure at default - i.e. at period = 0
   rec <- merge(rec, ead, by = "cohort", all.x = T)
   colnames(rec)[colnames (rec) == "exp.x"] <- "exp"        # change column names
   colnames(rec)[colnames (rec) == "exp.y"] <- "ead"

   # recovery computation
   rec$rec <- ifelse(rec$ead == 0,0,rec$cdcf/rec$ead)
   rec <- rec[order(rec$cohort, rec$period), ]

   ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ##     recovery is defined as non-decreasing function, hence function cummax (see below) is used - ONLY IF NEGATIVE CASH FLOWS ARE NOT ALLOWED
   ##!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   if(! allow.negative.cf) {
     rec <- rec[order(rec$cohort, rec$period), ]                # rec dataset ordered, so that rec values are added correctly to the rec dataset
     non_decreasing_rec <- tapply(rec$rec, rec$cohort, cummax)
     rec$rec <- unlist(non_decreasing_rec)                      # unlist() works correctly thanks to rec dataset ordering (see above)
   }



   # truncations of recovery for both sides (below and upper boundaries - "above1below0");
   #                         for upper boundary only - "above1";
   #                         for below boundary only - "below0"
   switch (trunc.method,
           ## above1below0 version
           above1below0 = {
             truncated0 <- unique(rec$cohort[rec$rec < 0])
             truncated1 <- unique(rec$cohort[rec$rec > 1])
             truncated <- c(truncated0, truncated1)

             rec$rec[rec$rec < 0] <- 0
             rec$rec[rec$rec > 1] <- 1
             if (length(truncated0) > 0) {
               txt <- paste(length(truncated0), " cohorts with recoveries < 0 truncated")
               warning(txt)
             }
             if (length(truncated1) > 0) {
               txt <- paste(length(truncated1), " cohorts with recoveries > 1 truncated")
               warning(txt)
             }
           },
           ## above1 version
           above1 = {
             truncated <- unique(rec$cohort[rec$rec > 1])

             rec$rec[rec$rec > 1] <- 1
             if (length(truncated) > 0) {
               txt <- paste(length(truncated), " cohorts with recoveries > 1 truncated")
               warning(txt)
             }

           },
           ## below0 version
           below0 = {
             truncated <- unique(rec$cohort[rec$rec < 0])

             rec$rec[rec$rec < 0] <- 0
             if (length(truncated) > 0) {
               txt <- paste(length(truncated), " cohorts with recoveries < 0 truncated")
               warning(txt)
             }
           },
           ## default version
{ truncated <- "none" })



   #-------
   # preparation of the prediction matrix with app.horizon dimension
   # transparently prepared for the weighted and aggregated lgd prediction
   cohorts.unique <- unique(dta$rec$cohort)       # unique cohorts
   period.window <- rep(app.horizon, length(cohorts.unique)) # app.horizon - same for each cohort

   # observations above the app.horizon are omitted
   exclude <- unique(rec$cohort[rec$period > app.horizon])
   if (length(exclude) > 0) {
       warning(paste(length(exclude), "of", length(unique(rec$cohort)), "cohorts were truncated for being longer than \"app.horizon\""))
   }
   rec <- rec[!rec$period > app.horizon,]   # exclude observations above the app. horizon


 #  if (include.open | !include.closed) {
   # data.frame to prepared for the weighted and aggregated LGD prediction
   predictions <- data.frame(cohort = rep(cohorts.unique,period.window + 1),
                             period = sequence(period.window + 1) - 1)
   predictions <- merge(predictions, rec, all=T)
   names(predictions)[names(predictions)=="rec"] <- "rec_observed"

   # filling-in missing EAD values (moving last forward)
   predictions$ead <- approx(predictions$ead, method="constant", xout=seq_along(predictions$ead), rule=2)$y
   class(predictions) <- c(class(predictions),"cLGD")

   # ordering preds matrix
   predictions <- predictions[order(predictions$period),]
   predictions <- predictions[order(predictions$cohort),]
 #     } else {preds<- rec}

    # output
   cohort.LGD <- list(rec = predictions, cutp = cutp, app.horizon = app.horizon, include.closed = include.closed, include.open = include.open,
                      truncated = truncated, trunc.method = trunc.method, allow.negative.cf = allow.negative.cf)
   class(cohort.LGD) <- c("list","cLGD")
   return(cohort.LGD)
}
#------------------------


aLGD <- function(dta, cutp, app.horizon, include.closed, include.open, trunc.method = "above1", option1 = F, minlen = 0, maxlen = 0,
                 allow.negative.cf = F) {

  # This function calculates aggregated LGD - it uses data from all cohorts together - grouped by periods. These period data are then
  # used for recovery estimation - logistic growth function estimation or simple estimation of recoveries for given app.horizon
  # based on recoveries grouped by periods.
  #
  # Args:
  #   dta                 - DATASET - LGD-AdaptedSourceData or cLGD object ( if not cLGD object, it is created using the function cLGD() and input data)
  #   cutp                - NUMERIC - take into account only observations after "cutp" months from default ("cutp" month included)
  #   app.horizon         - STRING - (0, app.horizon) is the period window of our interest;
  #                                   i.e. LGD is calculated (predicted) for periods 0, 1, ..., app.horizon
  #   include.closed      - BOOLEAN - include/exclude close accounts
  #   include.open        - BOOLEAN - include/exclude open accounts
  #   trunc.method        - STRING -
  #                         truncations of recovery for both sides (below(0) and upper(1) boundaries - "above1below0");
  #                         for upper(1) boundary only - "above1";
  #                         for below(0) boundary only - "below0"
  #   option1             - BOOLEAN - computation of aggregated LGD, (ALGD)
  #                                   FALSE - use only observed recoveries with different lenghts
  #                                   TRUE - use observed recoveries extended to app.horizon (last known moved forwad)
  #   minlen              - NUMERIC - select only cohorts with at least "minlen" periods of observed data
  #   maxlen              - NUMERIC - select only cohorts with maximum of "maxlen" periods of observed data
  #   allow.negative.cf   - BOOLEAN - argument indicating whether negative cash flows are allowed
  #
  # Returns:
  #   list                - list object - list of length 3:
  #                         preds_a               - DATASET - dataset used for prediction purposes
  #                         curve_a               - VECTOR OF NUMERIC - estimation of recovery based on logistic growth function estimation
  #                         curve_w               - VECTOR OF NUMERIC - simple estimation of recoveries for given app.horizon
  #                                                                     based on recoveries grouped by periods
  #
  # LEGEND:
  #   cf                  - cash-flows
  #   dcf                 - discounted cash-flows
  #   cdcf                - cummulative discounted cash-flows
  #   ead                 - exposure at default
  #   rec                 - recovery

   # cLGD class object is required
   if(is.cLGD(dta)) {
      if((dta$cutp != cutp) | (dta$app.horizon != app.horizon) | (dta$include.closed != include.closed) | (dta$include.open != include.open) | (dta$allow.negative.cf != allow.negative.cf) | (dta$trunc.method != trunc.method) ) {
        stop("Data Mismatch - cLGD object was computed with different settings than required.")
      }
   } else {
      dta <- cLGD(dta, cutp = cutp, app.horizon = app.horizon, include.closed = include.closed, include.open = include.open,
                  trunc.method = trunc.method, allow.negative.cf = allow.negative.cf)
   }

   # Selecting only required data
   dta <- dta$rec

   if (minlen > 0) {            # select only cohorts with at least minlen periods of observed data -
                                # periods with observations are identified as those with no NA -> hence na.omit() is used, so that
                                # periods that are yet unobserved (and hence contain NA in rec column) are ommitted
      incl <- unique(dta$cohort)[tapply(na.omit(dta)$period, na.omit(dta)$cohort, max) >= minlen]     # this works, because every cohort (from unique(dta$cohort)) has at least 0th period observed
                                                        # (and hence max(periods_observed) >= 0) and hence will appear in the tapply output.
                                                        #  Furthermore, tapply and unique use the same ordering, which secures correct match.
      dta <- dta[dta$cohort %in% incl, ]
   }
   if (maxlen > 0) {            # select only cohorts with maximum of maxlen periods of observed data -
                                # periods with observations are identified as those with no NA -> hence na.omit() is used, so that
                                # periods that are yet unobserved (and hence contain NA in rec column) are ommitted
      incl <- unique(dta$cohort)[tapply(na.omit(dta)$period, na.omit(dta)$cohort, max) <= maxlen]     # this works, because every cohort (from unique(dta$cohort)) has at least 0th period observed
                                                        # (and hence max(periods_observed) >= 0) and hence will appear in the tapply output.
                                                        #  Furthermore, tapply and unique use the same ordering, which secures correct match.
      dta <- dta[dta$cohort %in% incl, ]
   }

   # aggregated LGD
   if (option1 == F) {                                # option1 = F -> use only observed recoveries -> hence usage of na.omit() - it will exclude periods yet unobserved
      periods <- na.omit(dta)$period                  # periods - observed data
      recovs <- na.omit(dta)$rec_observed             # recoveries - observed data
      eads <- na.omit(dta)$ead                        # ead - observed data
      topred <- 1:max(periods)                        # maximum available horizon

      wead <- tapply(na.omit(dta)$ead, na.omit(dta)$period, sum)                # total ead grouped by periods - only observed data!
      wead <- wead/sum(wead)
      dta.temp <- dta
      dta.temp$weights <- wead[match(dta.temp$period, as.numeric(names(wead)))] # add weights info to dta.temp
      weights <- na.omit(dta.temp)$weights                                      # only weigths corresponding to observed periods - to pers

      } else {                      # option1 = T -> use observed recoveries extended to app.horizon (last known moved forwad)
      periods <- dta$period         # periods - all (not only observed)
      vec <- dta$rec_observed       # recoveries (last moved forward)
      recovs <- approx(vec, method="constant", xout=seq_along(vec), rule=2)$y   # recoveries (last moved forward)
      eads <- dta$ead
      topred <- 1:app.horizon       # periods to predict into

      wead <- tapply(dta$ead, dta$period, sum)                 # total period ead
      wead <- wead/sum(wead)
      wead <- rep(wead, max(table(periods)))                   # replication to all data
      weights <- wead
   }

   fit <- log.growth.estim(periods, recovs, topred, estim.method = 1, weights = weights) # estimation of recovery based on logistic growth function estimation

   fit2 <- tapply(eads * recovs , periods, sum) / tapply(eads, periods, sum) # simple estimation based on recoveries grouped by periods


   # truncations of recovery for both sides (below and upper boundaries - "above1below0");
   #                         for upper boundary only - "above1";
   #                         for below boundary only - "below0"
   if (trunc.method == "above1below0") {
      fit[fit>1] <- 1
      fit[fit<0] <- 0
   }

   if (trunc.method == "above1") {
      fit[fit>1] <- 1
   }

   if (trunc.method == "below0") {
      fit[fit<0] <- 0
   }

   curve_a <- fit
   #----------

   return(list(preds_a = dta, curve_a = curve_a, curve_w = fit2))
}
#------------------------



wLGD <- function(dta, cutp, app.horizon, min.length, include.closed, include.open,
                 trunc.method = "above1", pred.method = "useobserved",
                 allow.negative.cf = F) {

  # This function calculates weighted LGD curve - first for each cohort (with minimum of "min.lenght" observed periods) Recoveries
  # for future periods (up to "app.horizon") are predicted (using logistic growth function estimation and
  # based on cohort?s recoveries observed data).Then if pred.method == "useobserved" - then observed values are used if observed values exist;
  # otherwise predicted values are used. If pred.method != "useobserved" - then only predicted values are used (even if observed value exists).
  # These (predicted or observed) cohort recovery values are then used to calculate weighted(by EAD) average for each period to get final LGD curve.
  #
  # Args:
  #   dta                 - DATASET - LGD-AdaptedSourceData or cLGD object ( if not cLGD object, it is created using the function cLGD() and input data)
  #   cutp                - NUMERIC - take into account only observations after "cutp" months from default ("cutp" month included)
  #   app.horizon         - STRING - (0, app.horizon) is the period window of our interest;
  #                                   i.e. LGD is calculated (predicted) for periods 0, 1, ..., app.horizon
  #   min.length          - NUMERIC - recoveries will be predicted only for cohorts with minimum of "min.lenght" observed periods
  #   include.closed      - BOOLEAN - include/exclude close accounts
  #   include.open        - BOOLEAN - include/exclude open accounts
  #   trunc.method        - STRING -
  #                         truncations of recovery for both sides (below(0) and upper(1) boundaries - "above1below0");
  #                         for upper(1) boundary only - "above1";
  #                         for below(0) boundary only - "below0"
  #   pred.method         - STRING - prediction method used
  #                         if pred.method == "useobserved" - then use observed values if observed values exist;
  #                                                           otherwise use predicted values.
  #                                                         - recovery is a non-decreasing function,
  #                                                           so all predicted recoveries lower than last observed set to last observed
  #                         if pred.method != "useobserved" - then use only predicted values (even if observed value exists)
  #   allow.negative.cf   - BOOLEAN - argument indicating whether negative cash flows are allowed
  #
  # Returns:
  #   list                - list object - list of length 3:
  #                         preds_w               - DATASET - cohort predictions
  #                         curve_w               - VECTOR OF NUMERIC -  weighted LGD curve
  #
  # LEGEND:
  #   cf                  - cash-flows
  #   dcf                 - discounted cash-flows
  #   cdcf                - cummulative discounted cash-flows
  #   ead                 - exposure at default
  #   rec                 - recovery

   # cLGD class object is required, if not, it is created
   if(is.cLGD(dta)) {
      if( (dta$cutp != cutp) | (dta$app.horizon != app.horizon) | (dta$include.closed != include.closed) | (dta$include.open != include.open) | (dta$allow.negative.cf != allow.negative.cf) | (dta$trunc.method != trunc.method) ) {
        stop("Data Mismatch - cLGD object was computed with different settings than required.")
      }
   } else {
      dta <- cLGD(dta, cutp = cutp, app.horizon = app.horizon, include.closed = include.closed, include.open = include.open,
                  trunc.method = trunc.method, allow.negative.cf = allow.negative.cf)
   }

   # select cohort recoveries data
   dta <- dta$rec

   # working data.frame
   dta2 <- dta

   cohorts.not.predicted <- NULL # recoveries will be predicted only for cohorts with minimum of "min.lenght" observed periods,
                                 # this vector "cohorts.not.predicted" keeps cohorts that dont meet "min.lenght" requirement

   # for-cycling all cohorts - for each cohort predict future recoveries based on its observed values
   for (cohort.id in unique(dta$cohort)) {
      cohort.data <- dta[dta$cohort == cohort.id, ]                     # dataset for particular cohort

      periods.observed <- na.omit(cohort.data)$period         # observed periods
      recovs.observed <- na.omit(cohort.data)$rec_observed    # recoveries observed
      topred <- 0:app.horizon                                 # periods to predict into

      # recoveries will be predicted only for cohorts with minimum of "min.lenght" observed periods
      if (length(periods.observed) > min.length) {
         fit <- log.growth.estim(periods.observed, recovs.observed, topred, estim.method = 1)

         # truncations of recovery for both sides (below and upper boundaries - "above1below0");
         #                         for upper boundary only - "above1";
         #                         for below boundary only - "below0"
         if (trunc.method == "above1below0") {
           fit[fit>1] <- 1
           fit[fit<0] <- 0
         }

         if (trunc.method == "above1") {
           fit[fit>1] <- 1
         }

         if (trunc.method == "below0") {
           fit[fit<0] <- 0
         }


         dta2$rec_predicted[dta$cohort == cohort.id] <- fit # fill in the predicted and fitted recoveries for a particular cohort
      }
      if (length(periods.observed) <= min.length) {   # Vector "cohorts.not.predicted" keeps cohorts that dont meet "min.lenght" requirement
        cohorts.not.predicted <- c(cohorts.not.predicted, cohort.id)
        #dta2$rec_predicted[dta$cohort == cohort.id] <- 0  # for cohorts having 0 recovery per given period (represented only by 1 line in the dataset)
      }
   }

   print("Cohorts excluded from predictions")
   print(cohorts.not.predicted)

   # excluding too short cohorts (that dont meet "min.lenght" requirement) from data
   dta2 <- dta2[!(dta2$cohort %in% cohorts.not.predicted), ]

   # final prediction (rec_final)
   if (pred.method == "useobserved") { # if pred.method == "useobserved" then use observed values if observed value exists;
                                       # otherwise use predicted values.
      unobserved <- is.na(dta2$rec_observed)    # unobserved data have rec_observed = NA

      # when observed, then observed, else predicted
      dta2$rec_final[unobserved] <- dta2$rec_predicted[unobserved]
      dta2$rec_final[!unobserved] <- dta2$rec_observed[!unobserved]

      # predicted lower than last known set to last known
      lastknown <- approx(dta2$rec_observed, xout=seq_along(dta2$rec_observed), method="constant", rule=2)$y  # this function approx fills missing rec data with last known values
      cond <- dta2$rec_predicted < lastknown
      dta2$rec_final[cond] <- lastknown[cond]
   } else {                               # if pred.method != "useobserved" then use only predicted values (even if observed value exists)
      dta2$rec_final <- dta$rec_predicted # predicted only
   }

   # weighted (with EAD) average of all cohorts - final curve
   recovered.volumes <- with(dta2, ead*rec_final)                 # ead*recovery = recovered volume (all periods, all cohorts)
   recovered.volumes[is.na(recovered.volumes)] <- 0               # is this correct?!

   recovered.volumes.period <- tapply(recovered.volumes, dta2$period, sum)  # sum of recovered volumes per periods
   ead.period <- tapply(dta2$ead, dta2$period, sum)                         # sum of Exposure at Default per periods

   curve_w <- recovered.volumes.period/ead.period                           # this works, because tapply() uses the same ordering
                                                                            # and both the  "recovered.volumes.period" and "ead.period"
                                                                            # use the same dataset "dta2"
   #----------
   wLGD <- list(preds_w=dta2, curve_w=curve_w)
   class(wLGD) <- c("list","wLGD")
   return(wLGD)
}
#------------------------



#!!!!! NEKONTROLOVANO YET !!!!!
#------------------------
# Residualni LGD (posouvani po kohortach v case)
# IN: LGD-AdaptedSourceData
#     required time cuts grid (in terms of "period")
# OUT: 1) residual LGD - id, cohorta, nabehy
#      2) relative exposure - data frame with relative losses
#      3) Loss - product of 1) and 2)
#      columns in terms of months after default (aligned to the left)
#      filled with NAs
rLGD <- function(dta, cutm){

  # This function calculates residual LGD  -
  #
  # Args:
  #   dta                 - DATASET - LGD-AdaptedSourceData or cLGD object ( if not cLGD object, it is created using the function cLGD() and input data)
  #   cutm                - NUMERIC - take into account only observations after "cutp" months from default ("cutp" month included)
  #
  # Returns:
  #   list                - list object - list of length 4:
  #                         cutp               -
  #                         res_ead            -
  #                         res_wlgd           -
  #                         res_algd           -
  #
  # LEGEND:
  #   cf                  - cash-flows
  #   dcf                 - discounted cash-flows
  #   cdcf                - cummulative discounted cash-flows
  #   ead                 - exposure at default
  #   rec                 - recovery


   #trivial
   res_ead <- NULL
   res_algd <- NULL
   res_wlgd <- NULL

   for (i in 0:9) {
      cutp <- i
      ah <- 30
      a1 <- iLGD(b, cutp=cutp)
      a2 <- cLGD(a1, cutp=cutp, app.horizon=30, exclude.closed=F)

      res_ead <- c(res_ead, sum(a2$preds$ead[a2$preds$period==0]))

      aa <- aLGD(a2, cutp=cutp, app.horizon=40, exclude.closed=F)
      aw <- wLGD(a2, cutp=cutp, app.horizon=40, exclude.closed=F)

      res_wlgd <- c(res_wlgd, max(aw$curve_w))
      res_algd <- c(res_algd, max(aa$curve_a))
   }

   res_ead <- res_ead/max(res_ead)
   rLGD <- data.frame(cutp=c(0:9), res_ead=res_ead, res_wlgd=res_wlgd, res_algd=res_algd)
   return(rLGD)
   #return(list=c(resLGD=resLGD, relExp=relExp, Loss=Loss))
}
#------------------------

rLGD.simple <- function(dta, recovery, period){

  # This function calculates residual LGD  -  Recovery rate (vector) is calculated for each period
  # as being a new starting period -> these vectors are returned in the form of a data frame.
  #
  # Args:
  #   dta                 - DATASET - containing info about periods and their corresponding recoveries
  #   recovery            - STRING - name of the column in the "dta" dataset containing info about recoveries
  #   period              - STRING - name of the column in the "dta" dataset containing info about periods
  #
  # Returns:
  #   list                - DATA.FRAME - column names = periods used as a new starting period:
  #                                       each column of the data.frame is the Recovery rate (vector) calculated
  #                                       for a particular period as being a new starting period
  #


  dta <- dta[ order(dta[, period]), ] # order input dataset by period

  # auxiliary function - returns Recovery rate (vector) calculated for "new.start.period" as being a new starting period
  single <- function (new_start_period){

    recovery_at_start_period <- dta[dta[, period] == new_start_period, recovery]
    # return Recovery rate calculated for "new_start_period" as being a new starting period
    # if "recovery_at_start_period" = 1 then return vector of O
    out <- ifelse(rep(recovery_at_start_period, length(dta[, period])) == 1,
                   rep(0, length(dta[, period])),
                   (dta[, recovery] - recovery_at_start_period)/(1 - recovery_at_start_period) )
    out[1:new_start_period] <- NA # for the periods earlier than "new_start_period" set NA
    return (out)
  }

  # lapply() used - for each period being new starting period - e.g.
  # if new_star_period = 3, than the remaining exposure at period = 3 is taken as new EAD and recovery is
  # a percentage of this new EAD recovered. Lapply() function returns list, each element represents
  # Recovery rate calculated for particular period as being a new starting period.
  periods <- dta[, period]

  #periods <- sort(unique(dta[, period]))

  periods <- periods[ periods != 0]   # exclude 0 period
  out <- lapply(periods, FUN = single)

  out <- data.frame(out)                                    # change to data frame - recoveries are columns
  out <- data.frame(cbind(dta[, recovery], out))            # add recovery for 0th period
  out <- data.frame(t(out))                                             # transpose, so that recoveries are rows
  row.names(out) <- dta[, period]                            # add column names
  colnames(out) <- dta[, period]

  return(out)

  }


rLGD <- function(dta){

  # This function calculates residual LGD  -  for each cohort and for weighted LGD curve. wLGD object is required as the input dataset.
  #
  # Args:
  #   dta                 - DATASET - wLGD object
  #
  # Returns:
  #   list                - LIST - list of length 3:
  #                           res_cohorts - list of data.frames - each data.frame contains residual LGD for particular cohort
  #                           res_curve   - data.frame - residual LGD curve
  #                           max_period_observed - list - for each cohort maximum observed period is calculated

  # wLGD class object is required, if not, it is created
  if(!is.wLGD(dta)) {
      stop("Input dataset is not wLGD object!")
  }

  # select cohort recoveries data
  cohort_data <- dta[[1]]

  # calculate residual LGD for each cohort
  res_cohorts <- dlply(cohort_data, .(cohort), function(x) rLGD.simple(dta = x, recovery = "rec_final", period = "period"))

  # select LGD curve data
  curve_data <- dta[[2]]
  curve_data <- data.frame (period = 0:(length(curve_data)-1), recovery = curve_data)   # curve data as a data.frame

  # calculate residual LGD for LGD curve
  res_curve <- rLGD.simple(dta = curve_data, recovery = "recovery", period= "period")

  # for each cohort calculate maximum of periods observed
  max_period_observed <- tapply(na.omit(cohort_data)$period, na.omit(cohort_data)$cohort, max)

  rLGD <- list(res_cohorts = res_cohorts, res_curve = res_curve, max_period_observed = max_period_observed)
  class(rLGD) <- c("list","rLGD")
  return (rLGD)
}
