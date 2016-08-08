# if ( any(!require(RODBC) | !require(xtable) | !require(plyr) | !require(devtools) ) )  {
#          setInternet2()
#       #   chooseCRANmirror()
#          install.packages(c("RODBC","xtable","plyr","devtools"), lib =.libPaths(),  dependencies = TRUE)
#      }
# 
# if (!require(rCharts) ) { 
#     devtools::install_github('rCharts', 'ramnathv')
#     devtools::install_github("shiny", "rstudio")
#   }


library(shiny)
#library(RODBC)
#library(hwriter)
library(rCharts)
library(xtable)
library(plyr)
library(hwriter)
library(shinybootstrap2)



options(scipen=999)
options(shiny.maxRequestSize=40*1024^2)  # to handle 30 MB of upload

# upload initial files and functions 
#language <- "EN"
#source("inifile.r", local=TRUE)

#source("functions_LGDCore.R", local=TRUE)
source("functions_Colors.r", local = TRUE)
source("functions_Dates.r", local = TRUE)
source("functions_LGDGraph.r", local=TRUE)
source("functions_LGDCore.r", local=TRUE)
source("functions_Main_calculation.R", local=TRUE)
#source("functions_LGDGraph.r")
#source("functions_LGDCore.r")

# load dataset 
load("sample_data_2.RData")


### define fixed variables
in_cutp <- 0
months_to_report <- c(6,12,18,24,30,36,48,60)
  
#--- Login page ----
ui_login_page <- function() {
  fluidPage(title= "LGD estimation tool", theme= "bootstrap.css",            

          singleton(tags$head(tags$script(src = "progress/progress.js", type="text/javascript"))),  
    # Login window form            
      HTML("<form class='form-horizontal well' style='width:400px;height:300px; position:fixed;margin-left:-200px;margin-top:-150px;top:50%;left:50%;'>
                 <legend >LGD estimation tool</legend>   
                 <div class='control-group'>
           <label class='control-label' for='inputUsername'>Username</label>
           <div class='controls'>
           <input type='text' id='inputUsername' placeholder='Username' >
           </div>
           </div>
           <div class='control-group'>
           <label class='control-label' for='inputPassword'>Password</label>
           <div class='controls'>
           <input type='text' id='inputPassword' placeholder='Password' style='-webkit-text-security: disc;'>
           </div>
           </div>  
           <div class='control-group'>
           <div class='controls'> 
           <button type='button' class='btn action-button btn-primary' id='button_login'>Log in</button>
           </div> 
           </div>
            <hr>
            <div  style='font-size:small; text-align:center;'>
              Type <b> demo </b> as username and password  
            </div>
          </form>
      ")
  )
  
}         


#values <- reactiveValues(b=2)

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output, session) {


# login details  
LoginData <- reactiveValues()
LoginData$LoggedIn <- FALSE
  


#-- /----- APPLICATION SETTINGS AND LOGIC  -----/------

source("mainPage.R", local=T)
source("progress.R", local=T)

#-- Login ----  
  doLogin <- reactive({
    
#     login_go <- 0
#     user_input <- 0
    
    input$button_login      # trigger
    
    isolate({
      if (!is.null(input$button_login)) {
        if (input$button_login > 0) {
          
          user_input <- input$inputUsername
          user_password <- input$inputPassword
          
#           SQL_password_hash <- sqlQuery(odbcConnect("MSSQL"), paste("select passwd from BT.dbo.ctrl_users where username = '",user_input,"'",sep="" )  )
#           user_password_hash <- sqlQuery(odbcConnect("MSSQL"), paste("select HASHBYTES('SHA','",user_password,"')",sep="")  )
#           SQL_password_hash <- paste(unlist(SQL_password_hash),collapse="")
#           user_password_hash <- paste(unlist(user_password_hash),collapse="")
#           
#          login_go <- 0
         # if(SQL_password_hash == user_password_hash ) {
          if (user_input == "demo" & user_password == "demo") { 
 #           login_go <- 1
            LoginData$LoggedIn <- TRUE
            LoginData$Account <- input$inputUsername
            #LoginData$Session <<- "Session ID" # TODO
#            LoginData$LoginTime <- Sys.time() # TODO

          }
        }
      }
    })
    #output <- list(login_go = login_go, username = user_input)
    #return(list(login_go, user_input,user_password,SQL_password_hash,user_password_hash ))
    # return(list(login_go, user_input,user_password ))
   # return(output)
    
  })
  
  
  
#  
#
# -- Main page (launch app)----

  output$mainPage <- renderUI({
    doLogin()
    #if (doLogin()$login_go == 1) {
    if (LoginData$LoggedIn) {      
      doLogout()
      ui_main_page()
    
    } else {
     
      ui_login_page()
    }
  })
   

  
# -- Logout ----  
  doLogout <- reactive({
    
    input$button_logout
    isolate({
    if (!is.null(input$button_logout)) {
        if (input$button_logout > 0) {
     
          LoginData$LoggedIn <- FALSE
          #LoginData$Account <- NULL
          #LoginData$Session <<- "Session ID" # TODO
          #LoginData$LoginTime <- NULL
          #LoginData$LoggedIn <<- TRUE
          #  doLogin()$login_go <- 0
         
       }
      }
    })
    
    })
 
#-- /----- DATA UPLOAD -----/------

# -- Data upload - historical/submit date choice ----
  output$historical_dates <- renderUI({
    
   # submit_table <- sqlQuery(odbcConnect("BT"), "select * from ctrl_submit")
    choices <- submit_table$submit_id
    names(choices) <- paste(submit_table$submit_id, ". Submitted ",submit_table$submit_date,"by ",submit_table$username)
    selectInput("submit_id_choice","" ,
                 choices  = rev(choices) ,selected = "", multiple=FALSE,  selectize = FALSE )
    
  })


#-- Data upload - main function ----

  dataset_upload <- reactive({     
    
    userData <-NULL     
    # trigger
    input$SQL_upload_current 
    input$SQL_upload_historical

    isolate({
      if (input$SQL_upload_current > 0) {
        progress <- Progress_current$new(session)
        progress$set(message = 'Initiating...',detail = "Preparing SQL connection", value = 0.1)
        Sys.sleep(0.4)
        progress$set(message = 'Executing SQL procedures...', detail = "Create input dataset for unsecured loans", value = 0.2)
        Sys.sleep(0.4)
        #sqlQuery(odbcConnect("BT"), "exec transform_data_unsecured")
        progress$set(detail = 'Create input dataset for secured loans', value = 0.3)
        Sys.sleep(0.4)
        #sqlQuery(odbcConnect("BT"), "exec transform_data_secured")
        progress$set(message = 'Loading data...', detail="Data for unsecured loans", value = 0.4)
        Sys.sleep(0.4)
        #unsecured_data <- sqlQuery(odbcConnect("BT"), "select * from in_data_unsecured" ) 
        progress$set(detail = 'Data for secured loans', value = 0.5)
        Sys.sleep(0.4)
        #secured_data <- sqlQuery(odbcConnect("BT"), "select * from in_data_secured" ) 
        progress$set(message = 'Performing data transformation...',detail = "Adjusting data types for unsecured loans", value = 0.6)
        Sys.sleep(0.4)
        # unsecured_data$record_date <- as.Date(unsecured_data$record_date, tz="")
        # unsecured_data$open_date <- as.Date(unsecured_data$open_date, tz="")
        progress$set(detail = "Adjusting data types for secured loans", value = 0.7)
        # secured_data$record_date <- as.Date(secured_data$record_date, tz="")
        # secured_data$default_date <- as.Date(secured_data$default_date, tz="")
        # secured_data$collateral_type_name <- as.character(secured_data$collateral_type_name)
        progress$set(message = 'Finalizing...',detail ="Closing SQL connection", value = 1)
        Sys.sleep(0.4)
            
        userData <- list(unsecured = unsecured_data, 
                         secured = secured_data, 
                         type= "current")
      
        progress$close()
        
      } else {
        if (input$SQL_upload_historical > 0) {
          progress <- Progress_historical$new(session)
          progress$set(message = 'Initiating...',detail = "Preparing SQL connection", value = 0.1)
          Sys.sleep(0.4)
          progress$set(message = 'Loading data...',detail = "Data for unsecured loans", value = 0.2)
          #historical_unsecured <- sqlQuery(odbcConnect("BT"), paste("select * from arch_in_data_unsecured where submit_id =",input$submit_id_choice ))
          progress$set(message = 'Loading data...',detail = "Data for secured loans", value = 0.3)
          #historical_secured <- sqlQuery(odbcConnect("BT"), paste("select * from arch_in_data_secured where submit_id =",input$submit_id_choice )) 
          progress$set(message = 'Loading saved settings...',detail = "", value = 0.4)
          #settings_unsecured <- sqlQuery(odbcConnect("BT"), paste("select * from out_unsecured_settings where submit_id =",input$submit_id_choice ))
          #settings_secured <- sqlQuery(odbcConnect("BT"), paste("select * from out_secured_settings where submit_id =",input$submit_id_choice ))
          progress$set(message = 'Transforming...',detail = "", value = 0.5)
          
          portfolios <- sort(unique(historical_unsecured$portfolio))  
        
          final_output <- NULL
          
         # for (i in 1:length(portfolios) )      {
          for (i in 1:3 )    {
        #  lapply(1:3, function(i) {  
       
          #  progress$set(detail = paste("Retreiving results for portfolio...",i, "out of",length(portfolios),sep=" "), value = i*(1-0.5)/length(portfolios)+0.5)
          #  progress$set(message = paste("Calculating results for portfolio...",i, "out of 3",sep=" "), value = i/3)
   
          portfolio <- portfolios[i] 
          record_date <- "2013-12-31"   # tentatively 
          
          progress$set(detail = paste("Retreiving results for portfolio...",portfolio, "out of",max(portfolios),sep=" "), value = i*(1-0.5)/length(portfolios)+0.5)
          
          dta <- historical_unsecured[historical_unsecured$portfolio == portfolio,] 
          dta_sec <- historical_secured[historical_secured$portfolio == portfolio & historical_secured$record_date == record_date,] 
          
          hist_length <- settings_unsecured$history_length[settings_unsecured$portfolio == portfolio]
          include_closed <- switch(settings_unsecured$default_type[settings_unsecured$portfolio == portfolio], "1" = T, "2" = T, "3" = F)
          include_open <- switch(settings_unsecured$default_type[settings_unsecured$portfolio == portfolio], "1" = T, "2" = F, "3" = T) 
          horizon <- settings_unsecured$recovery_horizon[settings_unsecured$portfolio == portfolio] 
          default_length <- settings_unsecured$default_length[settings_unsecured$portfolio == portfolio]   
          
          output <- main_calculation(dta = dta, dta_sec = dta_sec, hist_length = hist_length , horizon = horizon, 
                                     default_length = default_length,  include_closed = include_closed, include_open = include_open, 
                                     settings_secured = settings_secured, months_to_report = months_to_report ) 
          
          output$output_table_main <- cbind("Portfolio" = portfolio, output$output_table_main) 
          final_output <- rbind(final_output, output$output_table_main)
          } 
        
        progress$set(message = 'Finalizing...',detail = "Preparing outputs", value = 1)
        colnames(final_output) <- c( "Portfolio" ,"Count of loan IDs" ,"Exposure at default ","Recovery - cash flows (%)",
                                     "Recovery - collateral (%)" ,"Total recovery (%)" ,"LGD (%)" )
        
          progress$close()  
          
          userData <- list(unsecured = historical_unsecured, 
                           secured = historical_secured, 
                           type= "historical",
                           settings_unsecured = settings_unsecured,
                           settings_secured = settings_secured,
                           final_output = final_output)
       
        } else {
       userData <- NULL
        
        }
         
      } 
    
    }) 
    
    return(userData)  
   
  })
  

#--  Data upload - data correctness check ----
  user_dataset <- reactive({        # return NULL if wrong dataset was uploaded
    
    userData <- NULL 
    columns<- c("id","cohort", "ir", "ex", "closed", "period", "cf")
    if (FALSE %in% (columns %in% colnames( dataset_upload()$unsecured ) ) ) {
      userData <- NULL
    } else { 
      userData <- dataset_upload()$unsecured
    }
    return(userData)
  })
  
  user_dataset_sec <- reactive( {        # return NULL if wrong dataset was uploaded
    
    userData <- NULL 
    columns<- c("accid","portfolio", "exposure", "collateral_value", "eir")
    if (FALSE %in% (columns %in% colnames( dataset_upload()$secured ) ) ) {
      userData <- NULL
    } else { 
      userData <- dataset_upload()$secured
    }
    return(userData)
  })
 
#-- Data upload - feedback about loading----
  output$dataset_loaded <- renderUI( {
    
    if(is.null(user_dataset()) & is.null(user_dataset_sec()) ) {  
      
      HTML("<input id='dataset_loaded' value='0'  type='text' style='display:none'> ") 
    } else { 
    
      HTML("<input id='dataset_loaded' value='1'  type='text' style='display:none'> ")
    }
  
  })

#-- Data upload - feedback about dataset type ----
  output$dataset_type <- renderUI({
    
    if(!is.null(user_dataset()) | !is.null(user_dataset_sec()) ) {
      if (dataset_upload()$type == "current") {
      HTML("<input id='dataset_type' value='current'  type='text' style='display:none'> ") 
    } else {
        HTML("<input id='dataset_type' value='historical'  type='text' style='display:none'> ") 
      }  
    } else {
      return(NULL)
    }
    
  })

#--- Data upload - main message ----
  output$dataset_message <- renderPrint( {
    columns <- c("id","cohort", "ir", "ex", "closed", "period", "cf")
      # colnames(user_dataset() )
      if (!is.null(dataset_upload()$unsecured ) ) 
      {
        if (FALSE %in% (columns %in% colnames(dataset_upload()$unsecured ) )  )  
        {    
          error <- tags$div (class="alert alert-error", 
                            HTML("<button type='button' class='close' data-dismiss='alert'>&times;</button>   "),
                            p(strong("Oh snap!"),"Data upload error. Please make sure SQL data structure is OK."))                    
          cat(as.character(error))   
        } else {
          success <- tags$div(class="alert alert-success",
                              HTML("<button type='button' class='close' data-dismiss='alert'>&times;</button>   "),
                              p(strong("Well done!"), "You have successfully loaded the dataset. Specify calculation parameters in 'Settings' 
                                and see results for each portfolio in 'Portfolio overview' or outline in 'Summary' tab."))   
          cat(as.character(success))            
        }
      } else {
        info <- tags$div(class="alert alert-info",
                         HTML("<button type='button' class='close' data-dismiss='alert'>&times;</button>   "),
                         p(strong("Let's start!"), "Select dataset to upload by pressing respective button below.
                         Once data is uploaded go to 'Settings' tab to adjust parameters."))   
        cat(as.character(info))   
      }  
    }) 
   

#-- Data upload - message about dataset type ----   
  output$message_user <- renderPrint({
    if (!is.null(dataset_upload() ) ) {
     info <- tags$div("Uploaded: ",strong(input$dataset_type),"data")
#        info <- tags$div(class="label label-info", 
#                        HTML("<button type='button' class='close' data-dismiss='alert'>&times;</button>"),
#                         "You are working with",strong(input$dataset_type),"data")                
      cat(as.character(info))   
    }
  }) 


#-- /----- SETTINGS  -----/------

#-- Settings - for secured loans ---- 
  output$secured_settings <- renderUI( {
    
   if  ( !is.null(user_dataset_sec() ) ) {   
     dta <- user_dataset_sec()
     collateral_types <- as.character(sort(unique(dta$collateral_type_name))) 
     lapply(1:length(collateral_types), function(i) {
        type_name <- collateral_types[i]
        output <- tags$div(
                  HTML('<hr><table border = 0 width=100% cellpadding=15><tr><td class="well" style="vertical-align:middle;text-align:center;" width=20%>'),
                  "Collateral type:", tags$div(strong(type_name)), HTML('</td><td width=40% class="well">'),
                  #"of value", format(type_value, big.mark=" "), "RON",
                  sliderInput(inputId = paste("realization_time_",type_name,sep=""), label="Realization time", min=0, max=60, value=24, step=3, format="# months"),
                  HTML('</td><td width=40% class="well">'),
                  sliderInput(inputId = paste("haircut_",type_name,sep=""), label="Haircut", format="#%", value = 0.0, min=0.0, max=1, step=0.01),
                  HTML('</td></tr></table>')
        )
       output
     })           
    } else { 
      return(NULL) 
    }  
  })

outputOptions(output, "secured_settings", suspendWhenHidden = FALSE)

#-- Settings - for unsecured loans ----
  output$unsecured_settings <- renderUI( {
    
    if  ( !is.null(user_dataset() ) ) {   
      dta <- user_dataset()
      portfolios <- sort(unique(dta$portfolio))            
      lapply(1:length(portfolios), function(i) {   
        portfolio <- portfolios[i]
        
        if(is.null(user_dataset_sec())) {
          if(nrow(user_dataset_sec()[user_dataset_sec()$portfolio == portfolio,])>0) {
        output <- tags$div(
          HTML('<hr><div class=""><table border = 0 width=100% cellpadding=15><tr><td class="well" style="vertical-align:middle;text-align:center;" width=10%>'),
          "Portfolio:",div(strong(portfolio)), 
          HTML('</td><td width=60% class="well">'),
          strong("Recovery curve settings:"),br(),
          HTML('<table border = 0 width = 100% ><tr><td width = 40%>'),"History length:",HTML('</td><td>'),
          sliderInput(inputId = paste("history_length_",portfolio,sep=""), label="", min=1, max=60, value=48, step=1, format="# months"),
          HTML('</td></tr><tr><td>'),"Minimum default length:",HTML('</td><td>'),
          #HTML('</td><td width=20%>'),
          sliderInput(inputId = paste("min_default_length_",portfolio,sep=""), label="", min=1, max=24, value=6, format="# months"),
          HTML('</td></tr><tr><td>'),"Recovery horizon:",HTML('</td><td>'),
          sliderInput(inputId = paste("recovery_horizon_",portfolio,sep=""), label="", min=1, max=60, value=36, step=1, format="# months"),
          HTML('</td></tr></table>'),
          HTML('</td><td width=30% class="well">'),
          
          #HTML('</td><td width=40%>'),
          strong("Type of default to be included:"),
          radioButtons(inputId = paste("default_type_included_",portfolio,sep=""), label="", 
                       choices = c("both open and closed" = 1, "closed only" = 2, "open only" = 3), selected = 1),
          
          HTML('</td></tr></table></div>')
        )}
        } else {
          output <- tags$div(
            HTML('<hr><div class=""><table border = 0 width=100% cellpadding=15><tr><td class="well" style="vertical-align:middle;text-align:center;" width=10%>'),
            "Portfolio:",div(strong(portfolio)), 
            HTML('</td><td width=60% class="well">'),
            strong("Recovery curve settings:"),br(),
            HTML('<table border = 0 width = 100% ><tr><td width = 40%>'),"History length:",HTML('</td><td>'),
            sliderInput(inputId = paste("history_length_",portfolio,sep=""), label="", min=1, max=60, value=48, step=1, format="# months"),
            HTML('</td></tr><tr><td>'),"Minimum default length:",HTML('</td><td>'),
            #HTML('</td><td width=20%>'),
            sliderInput(inputId = paste("min_default_length_",portfolio,sep=""), label="", min=1, max=24, value=6, format="# months"),
            HTML('</td></tr><tr><td>'),"Recovery horizon:",HTML('</td><td>'),
            sliderInput(inputId = paste("recovery_horizon_",portfolio,sep=""), label="", min=1, max=60, value=36, step=1, format="# months"),
            HTML('</td></tr></table>'),
            HTML('</td><td width=30% class="well">'),
            
            #HTML('</td><td width=40%>'),
            strong("Type of default to be included:"),
            radioButtons(inputId = paste("default_type_included_",portfolio,sep=""), label="", 
                         choices = c("both open and closed" = 1, "closed only" = 2, "open only" = 3), selected = 1),
            
            
            strong("Calculate only unsecured part?"),
            radioButtons(inputId = paste("only_unsecured_",portfolio,sep=""), label="", 
                         choices = c("Yes" = 1, "No" = 2), selected = 2),
            
            HTML('</td></tr></table></div>')  
          )
        }
        return(output)
      })           
    } else { 
      return(NULL) 
    }  
  })
  
outputOptions(output, "unsecured_settings", suspendWhenHidden = FALSE)  

#-- Settings - historical settings for unsecured loans ----
  output$historical_settings_unsec <- renderText({
    
    if( !is.null(dataset_upload()) ) {
      
      if (dataset_upload()$type == "historical" ) {
      settings_unsecured <- dataset_upload()$settings_unsecured[1:5]
      settings_unsecured$default_type[settings_unsecured$default_type == 1] <- "both open and closed"
      settings_unsecured$default_type[settings_unsecured$default_type == 2] <- "closed only"
      settings_unsecured$default_type[settings_unsecured$default_type == 3] <- "open only"
      colnames(settings_unsecured) <- c("Portfolio","History length", "Minimum default length", "Recovery horizon", "Default type")
      print(xtable(settings_unsecured),type="html",html.table.attributes = "border = '0', class='table table-striped', align = 'center'",
            include.rownames=FALSE, include.colnames=TRUE, print.results = F)   
      }
      } else {
      return(NULL)
    }
    
  })

outputOptions(output, "historical_settings_unsec", suspendWhenHidden = FALSE)  

#-- Settings - historical settings for secured loans ----
  output$historical_settings_sec <- renderText({
    
    if( !is.null(dataset_upload()) ) {
      if ( dataset_upload()$type == "historical" ) {
      settings_secured <- dataset_upload()$settings_secured[1:3]
      colnames(settings_secured) <- c("Collateral type","Realization time", "Haircut") 
      print(xtable(settings_secured),type="html",html.table.attributes = "border = '0', class='table table-striped', align = 'center' ",
            include.rownames=FALSE, include.colnames=TRUE, print.results = F)   
      } 
    } else {
      return(NULL)
    } 
  })

outputOptions(output, "historical_settings_sec", suspendWhenHidden = FALSE)  


#-- /----- PORTFOLIO OVERVIEW  -----/------


#-- Portfolio overview - portfolio choice ----
  output$choose_portfolio <- renderUI( {   
    if( !is.null(user_dataset() ) ) {    
        portfolio <- as.character(sort(unique(user_dataset()$portfolio))) 
        selectInput("filter_portfolio_default","" ,choices  = portfolio ,selected = "",multiple=FALSE,  selectize = FALSE)        
    } else { return(NULL) }   
  })



#-- #-- Portfolio overview - feedback message portfolio  ----
  output$out_portfolio <- renderUI ({   
    input$show_overview
    isolate(input$filter_portfolio_default) 
    })

#-- Portfolio overview - date choice (tentatively) ----  
  output$choose_date <- renderUI( {    
    if( !is.null(user_dataset_sec() ) ) {  
        dates <- sort(unique(user_dataset_sec()$record_date))
        selectInput("select_date","" ,choices  = as.character(dates) ,selected = "2012-12-31" , multiple=FALSE,  selectize = FALSE)        
    } else { return(NULL) }    
  })


 

#-- Portfolio overview - main portfolio calculation ----  
  portfolio_calculation <- reactive({
    
    input$show_overview   # trigger
    
    isolate({ 
      
    if(!is.null(user_dataset()) & !is.null(user_dataset_sec())) {
       
      #---  retreive relevant datasets---
      portfolio <- input$filter_portfolio_default
      record_date <- input$select_date   # tentatively
      dta <- user_dataset()[user_dataset()$portfolio == portfolio,] 
      dta_sec <- user_dataset_sec()[user_dataset_sec()$portfolio == portfolio & user_dataset_sec()$record_date == record_date,]   
      collateral_types <- sort(unique(dta_sec$collateral_type_name))
      
      
      #-- inputs ---
      if( dataset_upload()$type == "current") {
        hist_length <- input[[paste("history_length_",portfolio,sep="")]] 
        include_closed <- switch(input[[paste("default_type_included_",portfolio,sep="")]], "1" = T, "2" = T, "3" = F)
        include_open <- switch( input[[paste("default_type_included_",portfolio,sep="")]], "1" = T, "2" = F, "3" = T)  
        horizon <- input[[paste("recovery_horizon_",portfolio,sep="")]]
        default_length <- input[[paste("min_default_length_",portfolio,sep="")]]
        settings_secured <- data.frame("collateral_type" = collateral_types,
                                       "realization_time" = unlist(lapply(collateral_types, function(x) input[[paste("realization_time_",x,sep="")]] )),
                                       "haircut" = unlist(lapply(collateral_types, function(x) input[[paste("haircut_",x,sep="")]] ))
        )
        
        if (input[[paste("only_unsecured_",portfolio,sep="")]]=="1") {dta_sec <- data.frame()}
      }
      if ( dataset_upload()$type == "historical") {
        settings_unsecured <- dataset_upload()$settings_unsecured
        settings_secured <- dataset_upload()$settings_secured
        hist_length <- settings_unsecured$history_length[settings_unsecured$portfolio == portfolio]
        include_closed <- switch(settings_unsecured$default_type[settings_unsecured$portfolio == portfolio], "1" = T, "2" = T, "3" = F)
        include_open <- switch(settings_unsecured$default_type[settings_unsecured$portfolio == portfolio], "1" = T, "2" = F, "3" = T) 
        horizon <- settings_unsecured$recovery_horizon[settings_unsecured$portfolio == portfolio] 
        default_length <- settings_unsecured$default_length[settings_unsecured$portfolio == portfolio]         
      }
      
      
      output <- main_calculation(dta = dta, 
                                 dta_sec = dta_sec, 
                                 hist_length = hist_length , 
                                 horizon = horizon, 
                                 default_length = default_length, 
                                 include_closed = include_closed, 
                                 include_open = include_open, 
                                 settings_secured = settings_secured,
                                 months_to_report = months_to_report
      ) 
      
      return(output)
      
    } else {
       return(NULL)
    }
    })
  })


#-- Portfolio overview - summary (main) ----  
  output$summary_main <- renderText({
    
    input$show_overview
    
    isolate({
      #  if (!is.null(secured_calculation())) {
      if (!is.null(portfolio_calculation())) {
        output_table_main <- portfolio_calculation()$output_table_main
        hwrite(output_table_main, border= 0 ,table.class = "table table-striped",  row.names=FALSE , 
               row.style=list('text-align:center ;font-weight:bold','text-align:center' )
               ,col.width=c('15%','15%','20%','20%','15%','15%')
               #     13.385827 19.685039 27.559055 19.685039 14.173228  5.511811
        )
      } else {
        return(NULL)
      } 
    })   
  })


 #-- Portfolio overview - summary for unsecured loans ----  
  output$summary_unsecured <- renderText({
    
    input$show_overview
    
    isolate({
      
      if (!is.null(portfolio_calculation())) {   
        output_table_unsecured <- portfolio_calculation()$output_table_unsecured 
        hwrite(output_table_unsecured, border= 0 ,table.class = "table table-striped", col.width=c("70%", "30%"), row.names=FALSE , 
               col.style=list('text-align:left; ','text-align:right'), row.style=list('font-weight:bold'))
      } else {
        return(NULL)
      } 
    })
  })
  

#-- Portfolio overview - summary for secured loans ----  
  output$summary_secured <- renderText({
    
    input$show_overview
    
    isolate({
      
      if (!is.null(portfolio_calculation())) {
        output_table_secured <- portfolio_calculation()$output_table_secured
        hwrite(output_table_secured, border= 0 ,table.class = "table table-striped", col.width=c("70%", "30%"), row.names=FALSE , 
               col.style=list('text-align:left; ','text-align:right'), row.style=list('font-weight:bold'))
        
      } else {
        return(NULL)
      }
    })
  }) 


 #-- Portfolio overview - slider for residual plot ----  
  output$residual_slider <- renderUI({
  
      if( !is.null(portfolio_calculation() ) ) {    
     
        max_period <- length(portfolio_calculation()$residual_LGD$res_curve)-1
        #c(0:(length(res_curve)-1))
        sliderInput(inputId = "residual_period", label="", format="", value = 3 , min=0 , max=max_period, step=1)
      } else { 
        return(NULL) 
      }  
    
  })


 #-- Portfolio overview - recovery curve chart ----  
  output$RecoveryCurve <- renderChart({
    
    
    if (!is.null(portfolio_calculation())) {
      
      
      
      preds_w <- portfolio_calculation()$weighted_LGD$preds_w
      curve_w <- portfolio_calculation()$weighted_LGD$curve_w
      co <- unique(preds_w$cohort)                                   #cohorts
      cond <- !is.na(preds_w$rec_observed)
      
      lo <- tapply(preds_w$period[cond], preds_w$cohort[cond], max)  # lenghts of observed data in each cohort
      mlo <- tapply(preds_w$period, preds_w$cohort, max)             # lenghts of observed data in each cohort
    
      h1 <- Highcharts$new()
      #h1$chart(height = 500)
      h1$title(text = "Recovery" ,style = list(color = "#646464"))
      h1$subtitle(text = isolate(paste("for portfolio",input$filter_portfolio_default, sep=" ")), style = list(color = "#646464"))
      h1$yAxis(min = 0, max = 1, title = list(text = "Residual Recovery (%)", style= list(color = "#646464")), labels = list(format = "{value:.2f}", formatter =   "#! function() {return 100*this.value + '%' ;} !#"   ))
      
      h1$xAxis(categories = c(0:(length(curve_w)-1)), labels = list(step = 6 ), title = list(enabled = TRUE, text = "Collection horizon (months)", style= list(color = "#646464")))
      h1$tooltip(formatter = "#!  function() {return '<span style=\"font-size:10px\">Period: '+ this.x +'</span><br>Cohort: '+ this.series.name +'<br>Value: <b>'+ Highcharts.numberFormat(100*this.y, 2) +'%';}  !#")
      
      #h1$tooltip(hideDelay = 500, headerFormat = '<span style="font-size: 10px">Period: {point.key}</span><br/>',pointFormat = "Cohort: {series.name}; Value: <b>{point.y:.3f}</b><br/>")
      
      h1$exporting(enabled = TRUE)
      h1$plotOptions(series = list(stickyTracking = FALSE, marker = list(enabled = FALSE)))
      h1$legend(enabled = TRUE, layout = "horizontal", align="center", verticalAlign = "bottom")
      ctd <- preds_w[preds_w$cohort == co[1],]
      h1$data( x = ctd$rec_final[c(0:lo[1])+1],  color = "#646464", lineWidth = 1,  type="line", name="Cohorts (observed)")
      for (i in co) {
        ctd <- preds_w[preds_w$cohort == i,] 
        h1$data(x = ctd$rec_final[c(0:lo[paste(i)])+1], linkedTo = ":previous", color = "#646464", lineWidth = 1,  type="line", name=unique(ctd$cohort))
      }
      ctd <- preds_w[preds_w$cohort == co[1],]
      h1$data( x = c(rep(NA,lo[1]), ctd$rec_final[c(lo[1] : mlo[1])+1]),  color ="#d3d3d3", lineWidth =1, type="line", name="Cohorts (predicted)")
      for (i in co) {
        ctd <- preds_w[preds_w$cohort == i,]
        h1$data( x = c(rep(NA,lo[paste(i)]) ,  ctd$rec_final[c(lo[paste(i)] : mlo[paste(i)])+1]), linkedTo = ":previous", lineWidth=1 ,color ="#d3d3d3", type="line", name=unique(ctd$cohort))
      }
      h1$data(x = as.numeric(curve_w),  type="line", name="Weighted Recovery", lineWidth=5, color = "#2f7ed8")
    
      h1$addParams(dom = 'RecoveryCurve')
      #h1$print("chart9999")
      #rm(i)
      return(h1)  
    
    } else { return(NULL)}

   
  })
  

 #-- Portfolio overview - residual curve chart  ----  
  output$ResidualCurve <- renderChart({
  
    
    if (!is.null(portfolio_calculation())) {
      
      dta <- portfolio_calculation()$residual_LGD
      cutperiod <- input$residual_period
      
      res_cohorts <- dta$res_cohorts
      res_curve <- dta$res_curve[ as.numeric(row.names(dta$res_curve)) == cutperiod, ]
      max_period_observed <- dta$max_period_observed
   
      h1 <- Highcharts$new()
      #h1$chart(height = 500)
      h1$title(text = "Residual recovery", style = list(color = "#646464"))
      h1$subtitle(text = isolate(paste("for portfolio",input$filter_portfolio_default, sep=" ")),style = list(color = "#646464"))
      h1$yAxis(min = 0, max = 1, title = list(text = "Residual Recovery (%)", style= list(color = "#646464")), labels = list(format = "{value:.2f}", formatter =   "#! function() {return 100*this.value + '%' ;} !#"   ))
      h1$xAxis(categories = c(0:(length(res_curve)-1)), labels = list(step = 6 ), title = list(enabled = TRUE, text = "Collection horizon (months)", style= list(color = "#646464")))
      h1$exporting(enabled = TRUE)
      h1$tooltip(formatter = "#!  function() {return '<span style=\"font-size:10px\">Period: '+ this.x +'</span><br>Cohort: '+ this.series.name +'<br>Value: <b>'+ Highcharts.numberFormat(100*this.y, 2) +'%';}  !#")
      
      #h1$tooltip(hideDelay = 500, headerFormat = '<span style="font-size: 10px">Period: {point.key}</span><br/>',pointFormat = "Cohort: {series.name}; Value: <b>{point.y:.3f}</b><br/>")
      h1$plotOptions(series = list(stickyTracking = FALSE, marker = list(enabled = FALSE)))
      h1$legend(enabled = TRUE, layout = "horizontal", align="center", verticalAlign = "bottom")
      
      pom_dta <- res_cohorts[[1]]
      yvalues_coh <- pom_dta[as.numeric(row.names(pom_dta)) == cutperiod, ]
      max_obs_period <- max_period_observed[as.numeric(names(max_period_observed)) == as.numeric(names(res_cohorts)[1])]
      h1$data( x = as.numeric(yvalues_coh[c(0:max_obs_period)+1]),  color = "#646464", lineWidth = 1,  type="line", name="Cohorts (observed)")
      for(i in 1:length(res_cohorts) ){ 
        pom_dta <- res_cohorts[[i]]
        yvalues_coh <- pom_dta[as.numeric(row.names(pom_dta)) == cutperiod, ]
        max_obs_period <- max_period_observed[as.numeric(names(max_period_observed)) == as.numeric(names(res_cohorts)[i])]
        h1$data(x =  as.numeric(yvalues_coh[c(0:max_obs_period)+1]), linkedTo = ":previous", color = "#646464", lineWidth = 1,  type="line", name=names(max_obs_period))
      }
      
      pom_dta <- res_cohorts[[1]]
      yvalues_coh <- pom_dta[as.numeric(row.names(pom_dta)) == cutperiod, ]
      max_obs_period <- max_period_observed[as.numeric(names(max_period_observed)) == as.numeric(names(res_cohorts)[1])]
      h1$data( x =   c(rep(NA,max_obs_period), as.numeric(yvalues_coh[c(max_obs_period : (length(yvalues_coh)-1))+1])),  color = "#d3d3d3", lineWidth = 1,  type="line", name="Cohorts (predicted)")
      for(i in 1:length(res_cohorts) ){ 
        pom_dta <- res_cohorts[[i]]
        yvalues_coh <- pom_dta[as.numeric(row.names(pom_dta)) == cutperiod, ]
        max_obs_period <- max_period_observed[as.numeric(names(max_period_observed)) == as.numeric(names(res_cohorts)[i])]
        h1$data(x =    c(rep(NA,max_obs_period), as.numeric(yvalues_coh[c(max_obs_period : (length(yvalues_coh)-1))+1])), linkedTo = ":previous", color = "#d3d3d3", lineWidth = 1,  type="line", name=names(max_obs_period))
      }

      h1$data(x = as.numeric(res_curve),  type="line", name="Residual Recovery", lineWidth=5, color = "#2f7ed8")
      
      h1$addParams(dom = 'ResidualCurve')
      #h1$print("chart9999")
      #rm(i)
      return(h1)  
      
    } else { return(NULL)}
    
   
})
  

 #-- Portfolio overview - LGD histogram ----  
  output$LGD_histogram <- renderChart({
    
    input$show_overview
    
    isolate({
    if (!is.null(portfolio_calculation()))
    {
      dta <- portfolio_calculation()$dataset_secured
      #temp <- hist(dta$LGD)
      h1 <- isolate(Highcharts$new())
      h1$title(text = "Individual recoveries", style = list(color = "#646464"))
      h1$subtitle(text = isolate(paste("for portfolio",isolate(input$filter_portfolio_default), sep=" ")), style = list(color = "#646464"))
     # h1$xAxis(categories = temp$breaks[-1])
      #h1$tooltip(formatter = "#!  function() {return '<span style=\"font-size:10px\">Bucket: '+ this.x +'</span><br>Cohort: '+ this.series.name +'<br>Count: <b>'+ this.y +'%';}  !#")
      h1$tooltip(hideDelay = 500, headerFormat = '<span style="font-size: 10px">Bucket: {point.key}</span><br/>',
                 pointFormat = "Count: <b>{point.y}</b> <br/>")
      h1$yAxis(title = list(text = "Frequency", style= list(color = "#646464")))
     h1$data(x = hist(dta$collateral_recovery_rate_accid, plot=F)$mids, y = hist(dta$collateral_recovery_rate_accid, plot=F)$counts, type = "column", color = "#646464")
     h1$exporting(enabled = TRUE)
      h1$legend(enabled = FALSE)
#       h1$exporting(button = list(contextButton = list(enabled = "true" ), 
#                                  exportButton = list(text = "Download", menuItems = "Highcharts.getOptions().exporting.buttons.contextButton.menuItems.splice(2)"),
#                                  printButton = list(text = "Print" , onclick = "function () {this.print();}")) )
      h1$addParams(dom = 'LGD_histogram')
      #h1$print("chart9999")
      return(h1)  
#     } else {
#       return(NULL)
    }
    })
    
  })

#-- /----- SUMMARY  -----/------


#-- Summary - calculation for all portfolios ----

# lapply(1:3, function(x) {local({
#   # for (x in 1:10) {local({
#   i <- x
#   
#   
#   #progress <- Progress$new(session)
#   # progress$set(value = 0)
#  # progress$set(message = paste("Calculating results for portfolio...",i, "out of 3",sep=" "), value = i/5)
# 
#   
#   
#   output[[paste('result', i, sep='')]] <- renderText({
#  # output[[paste('result', observe(x = input$filter_portfolio_default, suspend=T), sep='')]] <- renderText({
# # if (input[[paste('run', i, sep='')]] == 0)
#     if (input$all_calculate_button == 0)  return(NULL)
#     
#   #  isolate({
#     
#     dta_unsecured <-  user_dataset()
#     dta_secured <- user_dataset_sec()   
#     portfolios <- sort(unique(dta_unsecured$portfolio))  
#     
#     final_output <- NULL
#     # progress <- Progress$new(session)
#     # progress$set(value = 0)
#     # return(isolate({
#     #progress$set(message = paste("Calculating results for portfolio...",i, "out of",length(portfolios),sep=" "), value = i/length(portfolios))
#    # progress$set(message = paste("Calculating results for portfolio...",i, "out of 3",sep=" "), value = i/5)
#     
#     
#     portfolio <- portfolios[i] 
#     
#     # calculation for unsecured part
#     dta <- dta_unsecured[dta_unsecured$portfolio == portfolio,] 
#     max_cohort <- max(dta$cohort, na.rm=T)
#     min_cohort <- isolate(date_add(max_cohort, -1 * input[[paste("history_length_",portfolio,sep="")]]  ))      
#     dta <- dta[dta$cohort > min_cohort, ]     
#     include_closed <- switch(input[[paste("default_type_included_",portfolio,sep="")]], "1" = T, "2" = T, "3" = F)
#     include_open <- switch( input[[paste("default_type_included_",portfolio,sep="")]], "1" = T, "2" = F, "3" = T)  
#     
#     individual_LGD <- iLGD(dta)
#     cohort_LGD <- cLGD(individual_LGD ,app.horizon= input[[paste("recovery_horizon_",portfolio,sep="")]],
#                        include.closed= include_closed,include.open= include_open)
#     aggregated_LGD <- aLGD(cohort_LGD, cutp=0 ,app.horizon = input[[paste("recovery_horizon_",portfolio,sep="")]],
#                            include.closed=include_closed ,include.open=include_open)
#     weighted_LGD <- wLGD( cohort_LGD ,cutp=0 ,app.horizon= input[[paste("recovery_horizon_",portfolio,sep="")]],
#                           min.length = input[[paste("min_default_length_",portfolio,sep="")]],
#                           include.closed=include_closed, include.open=include_open)
#     
#     months_to_show <- isolate(months_to_report[months_to_report <= input[[paste("recovery_horizon_",portfolio,sep="")]] ] ) 
#     output_unsecured <- t(as.matrix(weighted_LGD$curve_w[months_to_show+1]))
#     rownames(output_unsecured) <- NULL
#     colnames(output_unsecured) <- paste(colnames(output_unsecured),"M",sep="")
#     
#     # calculation for secured part
#     dta <- dta_secured[dta_secured$portfolio == portfolio & dta_secured$record_date == "2012-12-31",]          
#     collateral_types <- as.character(sort(unique(dta$collateral_type_name)))
#     for (i in 1:length(collateral_types)) {  
#       type_name <- collateral_types[i]
#       dta$realization_time[dta$collateral_type_name == type_name]  <- input[[paste("realization_time_",type_name,sep="")]]
#       dta$haircut_value[dta$collateral_type_name == type_name] <- input[[paste("haircut_",type_name,sep="")]]      
#     }
#     dta$collateral_value_haircut <- dta$collateral_value * (1 - dta$haircut_value)
#     dta$LGD <- 1 - ( dta$collateral_value * (1 - dta$haircut_value) ) / (dta$exposure * (1 + dta$eir)^ dta$realization_time )
#     dta$LGD <- ifelse(dta$LGD < 0 , 0, dta$LGD)
#     collateral_aggregate <- tapply(dta$collateral_value, dta$accid, sum, na.rm=T)
#     dta$collateral_value_accid <- as.numeric(collateral_aggregate[match(dta$accid, names(collateral_aggregate))])
#     collateral__hairuct_aggregate <- tapply(dta$collateral_value_haircut, dta$accid, sum, na.rm=T)
#     dta$collateral_value_haircut_accid <- as.numeric(collateral__hairuct_aggregate[match(dta$accid, names(collateral__hairuct_aggregate))])
#     average_LGD_accid <- tapply(dta$LGD, dta$accid, mean, na.rm=T)
#     dta$average_LGD <- as.numeric(average_LGD_accid[match(dta$accid, names(average_LGD_accid))])
#     sub_dta <- dta[!duplicated(dta$accid),]
#     output_secured <- c(
#       nrow(sub_dta),
#       sum(sub_dta$exposure, na.rm=T),
#       mean(1-sub_dta$average_LGD, na.rm=T)
#     )
#     
#     pom_output <- c(portfolio , output_secured, output_unsecured[length(output_unsecured)])
#     final_output <- rbind(final_output, pom_output)
#     
#     #progress$close()
#     
#     #return(list(final_output = final_output, output_secured = output_secured , output_unsecured = output_unsecured , i = i ))
#     final_output <- data.frame(final_output, check.rows=FALSE)
#     rownames(final_output) <- NULL
#     final_output$total_recovery <- final_output$X4 + final_output$X5
#     final_output$LGD <- 1- final_output$total_recovery
#     rownames(final_output) <- NULL
#     final_output[,1] <- format(final_output[,1], nsmall = 0)
#     final_output[,2] <- format(final_output[,2], nsmall = 0, big.mark=" ")
#     final_output[,3] <- format(final_output[,3], nsmall = 0, big.mark=" ")
#     final_output[,4] <- format(final_output[,4], nsmall = 2)
#     final_output[,5] <- format(final_output[,5], nsmall = 2)
#     final_output[,6] <- format(final_output[,6], nsmall = 2)
#     final_output[,7] <- format(final_output[,7], nsmall = 2)
#     colnames(final_output) <- c("Portfolio", "Count of loan IDs", "Exposure at default",
#                                 "Recovery (secured)", "Recovery (unsecured)", "Total recovery", "LGD" )
#     
#     print(xtable(final_output),type="html",html.table.attributes = "border = '0', class='table table-striped', align = 'center' 
#           style='table-layout:fixed;'", 
#           include.rownames=FALSE, include.colnames=TRUE, print.results = F)   
#    
#   })
#   
# 
#   
# })})

 

#-- Summary - historical summary output ----
  output$historical_summary <- renderText({
    
    if( dataset_upload()$type == "historical" ) {
      summary <- dataset_upload()$final_output
      print(xtable(summary),type="html",html.table.attributes = "border = '0', class='table table-striped', align = 'center' 
      style='table-layout:fixed;'" , include.rownames=FALSE, include.colnames=TRUE, print.results = F)   

    } else {
      return(NULL)
    }
    
  })

#-- Summary - calculation for all portfolios (version 2) ----

  output$all_calculate <- renderText({
    
    
    # trigger
    if (input$all_calculate_button ==0 )  return(NULL)
    
    isolate({   
    #  if (!is.null(user_dataset() ) & !is.null(user_dataset_sec() ) ) {
        
        dta_unsecured <-  user_dataset()
        dta_secured <- user_dataset_sec()
        portfolios <- sort(unique(dta_unsecured$portfolio))  
      
        final_output <- NULL
      
        progress <- Progress_summary$new(session)
        progress$set(value = 0)
        for (i in 1:length(portfolios) )      {
     #   for (i in 1:3 )    {
      #  lapply(1:3, function(i) {  
     
          progress$set(message = paste("Calculating results for portfolio...",i, "out of",length(portfolios),sep=" "), value = i/length(portfolios))
        #  progress$set(message = paste("Calculating results for portfolio...",i, "out of 3",sep=" "), value = i/3)
      
          
        portfolios <- sort(unique(user_dataset()$portfolio))  
        portfolio <- portfolios[i]   
        record_date <- "2012-12-31"   # input$select_date 
        dta <- user_dataset()[user_dataset()$portfolio == portfolio,] 
        dta_sec <- user_dataset_sec()[user_dataset_sec()$portfolio == portfolio & user_dataset_sec()$record_date == record_date,]   
        
        collateral_types <- sort(unique(dta_sec$collateral_type_name))
        
        #---  retreive relevant datasets---
        # dta <- user_dataset()[user_dataset()$portfolio == portfolio,] 
        #  dta_sec <- user_dataset_sec()[user_dataset_sec()$portfolio == portfolio & user_dataset_sec()$record_date == record_date,]   
        hist_length <- input[[paste("history_length_",portfolio,sep="")]] 
        include_closed <- switch(input[[paste("default_type_included_",portfolio,sep="")]], "1" = T, "2" = T, "3" = F)
        include_open <- switch( input[[paste("default_type_included_",portfolio,sep="")]], "1" = T, "2" = F, "3" = T)
        
        
        
        secured_only <- input[[paste("secured_only_",portfolio,sep="")]]
        
        
        
        
        horizon <- input[[paste("recovery_horizon_",portfolio,sep="")]]
        default_length <- input[[paste("min_default_length_",portfolio,sep="")]]
        settings_secured <- data.frame("collateral_type" = collateral_types,
                                       "realization_time" = unlist(lapply(collateral_types, function(x) input[[paste("realization_time_",x,sep="")]] )),
                                       "haircut" = unlist(lapply(collateral_types, function(x) input[[paste("haircut_",x,sep="")]] ))
        )
        
        output <- main_calculation(dta = dta, dta_sec = dta_sec, hist_length = hist_length , horizon = horizon, 
                                   default_length = default_length,  include_closed = include_closed, include_open = include_open, 
                                   settings_secured = settings_secured, months_to_report = months_to_report ) 
        
        pom_output <- cbind("Portfolio" = portfolio, output$output_table_main) 
        #colnames(pom_output) <- NULL
        final_output <- rbind(final_output, pom_output)
        
        }
       
        progress$close()
     
        hwrite(final_output, border= 0 ,table.class = "table table-striped table-condensed",  row.names=FALSE , col.names=TRUE, center=TRUE, 
                row.style=list('text-align:center ;font-weight:bold' ), 
               col.style=c('text-align:center','text-align:center','text-align:center','text-align:center',
                           'text-align:center','text-align:center','text-align:center' )
               ,col.width=c('6%','14%','20%','20%','20%','14%','6%')
               #    7.142857 13.492063 19.841270 19.841270 19.841270 14.285714  5.555556
        )
        })
#         
#       } else { 
#         return(NULL)
#       }
      
  
    
  })

# 
# 
#   output$downloadPDF <- downloadHandler(
#     filename = paste("Recovery_curve_",paste(as.character(format(Sys.Date(),format="%Y%m%d"))),"_",
#                      paste(as.character(format(Sys.time(),format="%H-%M-%S"))),".pdf",sep=""),
#     content = function(file) {
#       pdf(file = file,  width=11.7, height=8.3 )# width=8.5, height=11)
#       wGraph(lgd_calculation()$weighted_LGD, legend.position="top", plot.what="R")    
#       dev.off()
#     }
#   )  
#   
#   output$downloadPNG <- downloadHandler(
#     filename = paste("Recovery_curve_",paste(as.character(format(Sys.Date(),format="%Y%m%d"))),"_",
#                      paste(as.character(format(Sys.time(),format="%H-%M-%S"))),".jpg",sep=""),
#     content = function(file) {
#       jpeg(file = file, width=800 , height=600, quality=100)
#       wGraph(lgd_calculation()$weighted_LGD, legend.position="top", plot.what="R")    
#       dev.off()
#     }
#   )
#   
#   output$downloadData <- downloadHandler(
#     filename = 'dataset.csv',
#     content = function(file) {
#       write.csv2(lgd_calculation()$weighted_LGD$preds_w, file=file, row.names = F)
#     }
#   )
#   

#-- /----- ENGINE  -----/------

# 
# 
#   final_dataset <- reactive({
#     
#     # trigger
#     input$overview
#         
#     isolate({   
#     if (!is.null(user_dataset() ) & !is.null(input$filter_portfolio_default) ) {
#    #   if(TRUE %in% (c("portfolio") %in% colnames(user_dataset())) & !is.null(input$filter_portfolio_default) ) {
#         dta <- user_dataset()[user_dataset()$portfolio %in% input$filter_portfolio_default,]  
#    #   } else {
#     #    dta <- user_dataset()
#    #   }         
#       max_cohort <- max(dta$cohort, na.rm=T)
#       min_cohort <- isolate(date_add(max_cohort, -1 * input[[paste("history_length_",input$filter_portfolio_default,sep="")]]  ))      
#       dta <- dta[dta$cohort > min_cohort, ]      
#     } else { 
#       dta <- NULL
#     }  
#     
#     include_closed <- switch(input[[paste("default_type_included_",input$filter_portfolio_default,sep="")]], "1" = T, "2" = T, "3" = F)
#     include_open <- switch(input[[paste("default_type_included_",input$filter_portfolio_default,sep="_")]] , "1" = T, "2" = F, "3" = T) 
#     
#     output <- list(dataset = dta, history_length = input[[paste("history_length_",input$filter_portfolio_default,sep="")]] ,
#                    recovery_horizon = input[[paste("recovery_horizon_",input$filter_portfolio_default,sep="")]],
#                    min_default_length = input[[paste("min_default_length_",input$filter_portfolio_default,sep="")]],
#                    include_closed = include_closed, include_open = include_open)
#     
#     return(dta)
#    # return(output)
#   }) 
#     
#   })

  
#    
# 
# 
#   
#   include_default_type <- reactive(  {    
#        
# #     include_closed <- switch(input$default_type_included, "1" = T, "2" = T, "3" = F)
# #     include_open <- switch( input$default_type_included , "1" = T, "2" = F, "3" = T) 
#     
#     include_closed <- switch(input[[paste("default_type_included_",input$filter_portfolio_default,sep="")]], "1" = T, "2" = T, "3" = F)
#     include_open <- switch( input[[paste("default_type_included_",input$filter_portfolio_default,sep="")]], "1" = T, "2" = F, "3" = T) 
#     
#     return(list(include_closed = include_closed, 
#                 include_open = include_open))
#     
#   })
#   
# 
#     
#   # main calculation
#   lgd_calculation <- reactive( {
#      
#     input$overview   # trigger
#     isolate({     
#       if( !is.null(final_dataset()))  {
#          
#       individual_LGD <- iLGD(final_dataset()  )  
#                   
#       cohort_LGD <- cLGD(individual_LGD
#                          ,app.horizon = input[[paste("recovery_horizon_",input$filter_portfolio_default,sep="")]]
#                          ,include.closed=include_default_type()$include_closed
#                          ,include.open=include_default_type()$include_open)
#               
#       aggregated_LGD <- aLGD(cohort_LGD,cutp=0,app.horizon = input[[paste("recovery_horizon_",input$filter_portfolio_default,sep="")]]
#                              ,include.closed= include_default_type()$include_closed
#                              ,include.open= include_default_type()$include_open)
#       
#       weighted_LGD <- wLGD(cohort_LGD,cutp=0,app.horizon = input[[paste("recovery_horizon_",input$filter_portfolio_default,sep="")]]
#                            ,min.length = input[[paste("min_default_length_",input$filter_portfolio_default,sep="")]]
#                            ,include.closed=include_default_type()$include_closed
#                            ,include.open=include_default_type()$include_open  )  
#       
#       residual_LGD <- rLGD( weighted_LGD )
#       
#       output <- list(individual_LGD = individual_LGD, cohort_LGD = cohort_LGD, aggregated_LGD = aggregated_LGD,
#                      weighted_LGD = weighted_LGD,residual_LGD = residual_LGD)
#       return(output)
#       }
#     })   
#   })
#   

######### UNIFINSHED !!!!!-----



#  
#        
# output$summary_main <- renderText({
#     
#     input$calculate   
#     isolate({
#       if (!is.null(portfolio_calculation())) {   
#        x <- portfolio_calculation()$summary_main 
#       print(xtable(x),type="html",html.table.attributes = "border = '0', class='table table-striped', align = 'center'", 
#             include.rownames=FALSE, include.colnames=TRUE, print.results = F)         
#       } else {
#         return(NULL)
#       } 
#     })   
#   })
# 
# 
# output$summary_unsecured <- renderText({
#     
#     input$calculate   
#     isolate({
#       if (!is.null(portfolio_calculation())) {   
#        x <- portfolio_calculation()$summary_unsecured 
#       print(xtable(x),type="html",html.table.attributes = "border = '0', class='table table-striped', align = 'center'", 
#             include.rownames=FALSE, include.colnames=TRUE, print.results = F)         
#       } else {
#         return(NULL)
#       } 
#     })   
#   })
# 
# output$summary_secured <- renderText({
#     
#     input$calculate   
#     isolate({
#       if (!is.null(portfolio_calculation())) {   
#        x <- portfolio_calculation()$summary_secured 
#       print(xtable(x),type="html",html.table.attributes = "border = '0', class='table table-striped', align = 'center'", 
#             include.rownames=FALSE, include.colnames=TRUE, print.results = F)         
#       } else {
#         return(NULL)
#       } 
#     })   
#   })
# 

# 
#        
#   # main calculation
#   unsecured_calculation_historical <- reactive( {
#       
#      if (!is.null(user_dataset() ) & !is.null(input$filter_portfolio_historical) ) {
#    
#       dta <- user_dataset()[user_dataset()$portfolio %in% input$filter_portfolio_historical,]  
#       settings_unsecured <- dataset_upload()$settings_unsecured[1:5]   
#       max_cohort <- max(dta$cohort, na.rm=T)
#       min_cohort <- isolate(date_add(max_cohort, -1 * settings_unsecured$history_length[settings_unsecured$portfolio == portfolio]  ))      
#       dta <- dta[dta$cohort > min_cohort, ]     
#       include_closed <- switch(settings_unsecured$default_type[settings_unsecured$portfolio == portfolio], "1" = T, "2" = T, "3" = F)
#       include_open <- switch( settings_unsecured$default_type[settings_unsecured$portfolio == portfolio], "1" = T, "2" = F, "3" = T)  
#       individual_LGD <- iLGD(dta)
#       cohort_LGD <- cLGD(individual_LGD ,app.horizon= settings_unsecured$recovery_horizon[settings_unsecured$portfolio == portfolio],
#                          include.closed= include_closed,include.open= include_open)
#       aggregated_LGD <- aLGD(cohort_LGD, cutp=0 ,app.horizon = settings_unsecured$recovery_horizon[settings_unsecured$portfolio == portfolio],
#                              include.closed=include_closed ,include.open=include_open)
#       weighted_LGD <- wLGD( cohort_LGD ,cutp=0 ,app.horizon= settings_unsecured$recovery_horizon[settings_unsecured$portfolio == portfolio],
#                             min.length = settings_unsecured$default_length[settings_unsecured$portfolio == portfolio],
#                             include.closed=include_closed, include.open=include_open)
#       residual_LGD <- rLGD(weighted_LGD)
#       
#       output <- list(individual_LGD = individual_LGD, 
#                      cohort_LGD = cohort_LGD, 
#                      aggregated_LGD = aggregated_LGD,
#                      weighted_LGD = weighted_LGD,
#                      residual_LGD = residual_LGD,
#                      dataset = dta)
#       return(output)
#     } 
#   })
#   
#outputOptions(output, "unsecured_calculation_historical", suspendWhenHidden = FALSE)


#   secured_calculation <- reactive({
#     
#     # trigger
#     input$calculate
#     
#     isolate({
#       
#       if( !is.null(user_dataset_sec()) & !is.null(input$filter_portfolio_default) & !is.null(input$select_date)   ) {    
#         dta <- user_dataset_sec()[user_dataset_sec()$portfolio == input$filter_portfolio_default & user_dataset_sec()$record_date == input$select_date,]   
#         #coll_split <- with(dta, tapply(collateral_value, as.character(collateral_type_name), sum, na.rm=T))
#         collateral_types <- as.character(sort(unique(dta$collateral_type_name)))
#         #type_name <- names(coll_split)[i]
#         for (i in 1:length(collateral_types)) {
#           #type_name <- names(coll_split)[i]
#           type_name <- collateral_types[i]
#           dta$realization_time[dta$collateral_type_name == type_name]  <- input[[paste("realization_time_",type_name,sep="")]]
#           dta$haircut_value[dta$collateral_type_name == type_name] <- input[[paste("haircut_",type_name,sep="")]]      
#         }
#         dta$collateral_value_haircut <- dta$collateral_value * (1 - dta$haircut_value)
#         dta$LGD <- 1 - ( dta$collateral_value * (1 - dta$haircut_value) ) / (dta$exposure * (1 + dta$eir)^ dta$realization_time )
#         dta$LGD <- ifelse(dta$LGD < 0 , 0, dta$LGD)
#         collateral_aggregate <- tapply(dta$collateral_value, dta$accid, sum, na.rm=T)
#         dta$collateral_value_accid <- as.numeric(collateral_aggregate[match(dta$accid, names(collateral_aggregate))])
#         collateral__hairuct_aggregate <- tapply(dta$collateral_value_haircut, dta$accid, sum, na.rm=T)
#         dta$collateral_value_haircut_accid <- as.numeric(collateral__hairuct_aggregate[match(dta$accid, names(collateral__hairuct_aggregate))])
#         average_LGD_accid <- tapply(dta$LGD, dta$accid, mean, na.rm=T)
#         dta$average_LGD <- as.numeric(average_LGD_accid[match(dta$accid, names(average_LGD_accid))])
#         
#         # take subset with distinct loan ID only
#         sub_dta <- dta[!duplicated(dta$accid), c("accid","portfolio","exposure","eir","collateral_value_accid","collateral_value_haircut_accid","average_LGD", 
#                                                  "collateral_type_name")]
#         return(list(dataset = sub_dta, collateral_info = collateral_types))
#       }        
#     })  
#   })
#   

# 
#   secured_calculation_historical <- reactive({
#     
#       if( !is.null(user_dataset_sec()) & !is.null(input$filter_portfolio_historical) & !is.null(input$select_date_historical)   ) {    
#         dta <- user_dataset_sec()[user_dataset_sec()$portfolio == input$filter_portfolio_historical & user_dataset_sec()$record_date == input$select_date_historical,]   
#         #settings_unsecured <- dataset_upload()$settings_unsecured[1:5]
#         #settings_unsecured$default_type[settings_unsecured$default_type == 1] <- "both open and closed"
#         #settings_unsecured$default_type[settings_unsecured$default_type == 2] <- "closed only"
#         #settings_unsecured$default_type[settings_unsecured$default_type == 3] <- "open only"
#         settings_secured <- dataset_upload()$settings_secured[1:3]
#         #coll_split <- with(dta, tapply(collateral_value, as.character(collateral_type_name), sum, na.rm=T))
#         collateral_types <- as.character(sort(unique(dta$collateral_type_name)))
#         #type_name <- names(coll_split)[i]
#         for (i in 1:length(collateral_types)) {
#           #type_name <- names(coll_split)[i]
#           type_name <- collateral_types[i]
#           dta$realization_time[dta$collateral_type_name == type_name]  <- settings_secured$realization_time[settings_secured$collateral_type_name == type_name]
#           dta$haircut_value[dta$collateral_type_name == type_name] <- settings_secured$haircut[settings_secured$collateral_type_name == type_name]  
#         }
#         dta$collateral_value_haircut <- dta$collateral_value * (1 - dta$haircut_value)
#         dta$LGD <- 1 - ( dta$collateral_value * (1 - dta$haircut_value) ) / (dta$exposure * (1 + dta$eir)^ dta$realization_time )
#         dta$LGD <- ifelse(dta$LGD < 0 , 0, dta$LGD)
#         collateral_aggregate <- tapply(dta$collateral_value, dta$accid, sum, na.rm=T)
#         dta$collateral_value_accid <- as.numeric(collateral_aggregate[match(dta$accid, names(collateral_aggregate))])
#         collateral__hairuct_aggregate <- tapply(dta$collateral_value_haircut, dta$accid, sum, na.rm=T)
#         dta$collateral_value_haircut_accid <- as.numeric(collateral__hairuct_aggregate[match(dta$accid, names(collateral__hairuct_aggregate))])
#         average_LGD_accid <- tapply(dta$LGD, dta$accid, mean, na.rm=T)
#         dta$average_LGD <- as.numeric(average_LGD_accid[match(dta$accid, names(average_LGD_accid))])
#         
#         # take subset with distinct loan ID only
#         sub_dta <- dta[!duplicated(dta$accid), c("accid","portfolio","exposure","eir","collateral_value_accid","collateral_value_haircut_accid","average_LGD", 
#                                                  "collateral_type_name")]
#         return(list(dataset = sub_dta, collateral_info = collateral_types))
#       }        
#    # })  
#   })

#outputOptions(output, "secured_calculation_historical", suspendWhenHidden = FALSE)

#progress <- Progress$new(session)
#progress$set(value = 0)
#obs <- reactiveValues(A = length(sort(unique(user_dataset()$portfolio))) )
# values <- reactiveValues(a=2)
# observe( {
#   input$all_calculate_button
#   values$a <- length(sort(unique(user_dataset()$portfolio)))
# })
# 
# b <- observe( {
#   input$all_calculate_button
#   return(length(sort(unique(user_dataset()$portfolio))))
# })


#output$number <- reactive ({ length(sort(unique(user_dataset()$portfolio)))  })
#values <- reactiveValues(a = 2)
#sth <- 3 
 #observe({

#a <- reactive({ return(5) })
#a <- observe({ length(unique(user_dataset()$portfolio)) }, quoted=F)
#a <- observe({ if ( !is.null(user_dataset() )  )  { values$a <- 5 }  })
 #  { values$a <- length(sort(unique(user_dataset()$portfolio))) }
#   else { sth <- 0 }
#   return(sth)
 
   # })
# sth <- reactive({
#   
#   if ( !is.null(user_dataset() )  ) { length(sort(unique(user_dataset()$portfolio))) }
# })





#mycounter <- reactiveValues(counter =14) 
#reactiveValue$counter + 1
# 
# 
#   save_to_database <- reactive({
#     
#     input$sav
#     
#     isolate({
#       portfolios <- sort(unique(user_dataset()$portfolio))
#       collateral_types <- as.character(sort(unique(user_dataset_sec()$collateral_type_name)))
#       #username <- doLogin()$username
#       username <- "admin"
#       
#       # initiate output 
#       data_tab <- NULL
#       coll_tab <- NULL
#    
#       # create table with collateral parameteres saved by user
#       for (i in 1:length(collateral_types)) {
#         type_name <- collateral_types[i]
#         coll_tab <- rbind(coll_tab , c(type_name, input[[paste("realization_time_",type_name,sep="")]],
#                                        input[[paste("haircut_",type_name,sep="")]]
#                                        )
#                           )
#       } 
#       coll_tab <- data.frame(coll_tab)
#       
#       # create table with portfolio paramteres saved by user
#       for (i in 1:length(portfolios)) {
#         portfolio <- portfolios[i]
#         data_tab <- rbind(data_tab, c(portfolio, input[[paste("history_length_",portfolio,sep="")]] ,
#                                       input[[paste("min_default_length_",portfolio,sep="")]],
#                                       input[[paste("recovery_horizon_",portfolio,sep="")]],
#                                       input[[paste("default_type_included_",portfolio,sep="")]]
#                                       )
#                           )     
#       }
#       data_tab <- data.frame(data_tab)
#     
#       # add date and user information
#       data_tab$calc_date <- Sys.Date()   
#       #data_tab$record_date <- as.Date("2011-02-24")
#       data_tab$username <- username
#       coll_tab$calc_date <- Sys.Date()
#       #coll_tab$record_date <- as.Date("2010-01-23")
#       coll_tab$username <- username
#       
#       
#       
#       # adjust column names used in SQL output table 
#       colnames(data_tab) <- c("portfolio", "history_length", "default_length","recovery_horizon", "default_type", "calc_date", "username")
#       colnames(coll_tab) <- c("collateral_type_name", "realization_time", "haircut", "calc_date", "username")
#       
#       # create corresponding data types to be used in SQL table (e.g. 'datetime', 'nvarchar' or 'decimal')
#       data_tab_col_types <- c(rep("int", length(data_tab[-c(1:2)])), "date", "nvarchar(50)")
#       coll_tab_col_types <- c("nvarchar(50)", "int","numeric", "date", "nvarchar(50)")
#       names(data_tab_col_types) <-  colnames(data_tab)
#       names(coll_tab_col_types) <- colnames(coll_tab)
#      
#       if ( nrow( sqlTables(odbcConnect("BT"), schema= "dbo", tableName = "out_unsecured_settings") ) == 0 ) {
#         sqlSave(odbcConnect("BT"), data_tab, tablename = "out_unsecured_settings", rownames=F,  varTypes = data_tab_col_types )
#         sqlSave(odbcConnect("BT"), coll_tab, tablename = "out_secured_settings", rownames=F,  varTypes = coll_tab_col_types )
#       } else { 
#         if ( any(username == sqlQuery(odbcConnect("BT"), "select distinct username from out_unsecured_settings")$username )  ) {
#       #  if ( any(unique(data_tab$record_date) ==  as.Date(sqlQuery(channel , "select distinct record_date from out_unsecured_settings", as.is=T)$record_date)    ) ) {
#           sqlUpdate(odbcConnect("BT"), data_tab, tablename = "out_unsecured_settings", index = c( "portfolio", "username") ) 
#           sqlUpdate(odbcConnect("BT"), coll_tab, tablename = "out_secured_settings", index = c( "collateral_type_name", "username") ) 
#         
#         } else {
#         #sqlQuery(odbcConnect("BT") , "drop table unsecured_settings")
#         sqlSave(odbcConnect("BT"), data_tab, tablename = "out_unsecured_settings", rownames=F, append=T )  
#         sqlSave(odbcConnect("BT"), coll_tab, tablename = "out_secured_settings", rownames=F, append=T )  
#         #sqlSave(odbcConnect("BT"), data_tab, tablename = "unsecured_settings", rownames=F, varTypes = col_types )   
#         }
#       }
#       #
#       
# #       # store datasets in SQL tables
#        data_unsecured <- user_dataset()
#        data_secured <- user_dataset_sec()
#      
#       # very first run, when table is not present in SQL database 
#       if ( nrow( sqlTables(odbcConnect("BT"), schema= "dbo", tableName = "out_data_unsecured") ) == 0 ) {   
#         sqlQuery(odbcConnect("BT"), "select * into out_data_unsecured from in_data_unsecured" )
#         sqlQuery(odbcConnect("BT"), "select * into out_data_secured from in_data_secured" )
#       }
#       # update missing records in unsecured output table if applicable
#       out_record_date <- as.Date(sqlQuery(odbcConnect("BT"), "select max(record_date) as max_date from out_data_unsecured")$max_date)
#       if ( max(data_unsecured$record_date) > out_record_date  ) {
#         sqlQuery(odbcConnect("BT"), "exec update_out_data_unsecured")
#       }
#       # update missing records in secured output table if applicable
#       out_record_date <- as.Date(sqlQuery(odbcConnect("BT"), "select max(record_date) as max_date from out_data_secured")$max_date)
#       if ( max(data_secured$record_date) > out_record_date  ) {
#         sqlQuery(odbcConnect("BT"), "exec update_out_data_secured")
#       }
#        
#       
# 
#     })
#   })
# 


#   output$renderTable({
#     
#     source("functions_Secured.R")
#     
#     
#   })
  
 
#-- /----- TESTING AREA  -----/------
  
   output$test <- renderPrint( {      
     #dim(final_dataset())
     #input$filter_portfolio_default
     #head(user_dataset_sec())

     #values$a
     #lgd_calculation()
     #as.character(sort(unique(user_dataset_sec()$collateral_type_name)))[2]
     #save_to_database()
     #user_dataset_load()$type
     #names(dataset_upload())
     #paste(!is.null(user_dataset_sec()), !is.null(input$filter_portfolio_historical) , !is.null(input$select_date_historical))
     #names(secured_calculation_historical())
     #paste ("current: ", input$SQL_upload_current, "historical" , input$SQL_upload_historical)
     #names(portfolio_calculation())
     isolate(values$b)
   })
  
  # END OF TESTING AREA
   

#-- /----- NOT USED -----/------
# 
#   output$RecoveryCurve <- renderPlot({
#   
#     if (!is.null(lgd_calculation()))
#     {
#       wGraph(lgd_calculation()$weighted_LGD, legend.position="top", plot.what="R")   
#     } else { 
#       return(plot.new()) 
#     }      
#   })
# 
#   output$ResidualCurve <- renderPlot({
#     
#     if (!is.null(lgd_calculation()))
#     {
#       rGraph(lgd_calculation()$residual_LGD, legend.position="top", plot.what="R", cutperiod = input$residual_period)   
#     } else { 
#       return(plot.new()) 
#     }      
#   })

#   residual_LGD <- reactive({
#     
#     rLGD(lgd_calculation()$weighted_LGD)
#     
#   })

 
# 
#   output$RecoveryCurve_A <- renderPlot({
#     
#     if (!is.null(lgd_calculation()))
#     {
#       aGraph(lgd_calculation()$aggregated_LGD, legend.position="top", plot.what="R")    
#     } else { 
#       return(plot.new()) 
#     }  
#   })

 
  
    
#     output$hist_LGD <- renderPlot({
#       
#       if (!is.null(secured_calculation()))
#       {
#         dta <- secured_calculation()
#         hist(dta$LGD)
#        # ltv <- 
#       } else { 
#         return(plot.new()) 
#       }  
#     })
  
#     
#   output$DataTable_sec <- renderDataTable({
#       
#     if (!is.null(secured_calculation()))
#     {
#       secured_calculation()
#       } else { return(NULL) }
#       
#     }, options = list(bSortClasses = TRUE , aLengthMenu = c(5, 10, 25, 50, 100), iDisplayLength = 5))  
#     
#  
#   output$DataTable <- renderDataTable({
#     
#     if (!is.null(user_dataset())) {
#      user_dataset()
#     } else { return(NULL) }
#     
#   }, options = list(bSortClasses = TRUE , iDisplayLength = 10))
#   
#   
  

  
#   
#     
#   histogram_unsec <- reactive({
#     
#   #  isolate({
#     if (!is.null(final_dataset())) {
#       individual_LGD <- iLGD(final_dataset())
#       x <- hist(individual_LGD$lgd$lgd, plot=F, breaks=10)
#       output <- list(counts = x$counts, mids = x$mids)
#       return(output)
#     } else {
#       return(NULL)
#     }
#   #  })
#      
#    })
#    
#   output$LGD_histogram_unsec <- renderChart({
#     
#   #  isolate({
#     if (!is.null(final_dataset())) {
#       input <- histogram_unsec()
#       h1 <- Highcharts$new()
#       h1$title(text = "Individual LGD")
#       h1$subtitle(text = isolate(paste("Portfolio",input$filter_portfolio_default, sep=" ")))
#       # h1$xAxis(categories = temp$breaks[-1])
#       
#       h1$yAxis(title = list(text = "Frequency"))
#       h1$data(x = input$mids, y = input$counts, type = "column", name="")
#       h1$exporting(enabled = TRUE)
#       h1$legend(enabled = FALSE)
#       #       h1$exporting(button = list(contextButton = list(enabled = "true" ), 
#       #                                  exportButton = list(text = "Download", menuItems = "Highcharts.getOptions().exporting.buttons.contextButton.menuItems.splice(2)"),
#       #                                  printButton = list(text = "Print" , onclick = "function () {this.print();}")) )
#       h1$addParams(dom = 'LGD_histogram_unsec')
#       #h1$print("chart9999")
#       return(h1)  
#       
#     }
#       
#  # })
#   
#   })
#   



})
