library(shiny)
#library(RODBC)
#library(hwriter)
library(rCharts)
library(xtable)
library(plyr)


 shinyUI( uiOutput("mainPage") )


# shinyUI( 
# 
# 
# fluidPage( title = "LGD Analytical Tool v1.1", theme = "bootstrap.css",
# 
# #div(class="",     
#     #    fluidPage(
#     fluidRow(style="height:50px",
#              column(2,offset = 5, style="position:relative;top:10px;text-align:center",
#                     HTML("<font size=2>"),htmlOutput("message_user"),HTML("</font>")
#              ),
#              column(5, style="float:right;width:300px;position:relative;top:10px; text-align:right",
#                     HTML("<font size=2>You are logged as: </font>"),
#                     #strong(doLogin()$username),
#                    # strong(LoginData$Account),
#                     tags$button(class="btn action-button btn-small", id="button_logout", type="button", as.character("Log out")), 
#                    br()
#              ) 
#              #  )         
#     ),          
# #verbatimTextOutput("test"),
#   
#     #  tags$head(tags$script(src = "button.js")),
#     
#     tags$head(tags$script(src = "progress/progress.js", type="text/javascript")),
#     tags$head(tags$link( rel="stylesheet", type="text/css", href = "progress/progress.css")),
#     #tags$head(tags$link( href="bootstrap.css", rel="stylesheet", type="text/css")),
#     tags$head(tags$style(type="text/css", "select#submit_id_choice { width:100%; }")),
#     #tags$head(tags$script( src="shared/bootstrap/js/bootstrap.min.js")),
#     #tags$head(tags$link(href="shared/bootstrap/css/bootstrap-responsive.min.css", rel="stylesheet", type="text/css")),
#     #-- User Interface start ----
#     #navbarPage("LGD Analytical Tool v1.1",  header = HTML("<br><br>"), footer = "", theme="bootstrap.css", id="main_tabs",
#   
#   HTML('
#   <!-- main panel -->
#   <div class="navbar navbar-static-top">
#     <div class="navbar-inner">
#       <div class="container">
#         <span class="brand pull-left">LGD Analytical Tool v1.1</span>
#         <ul class="nav" id="main_tabs">
#           <li class="active">
#             <a data-toggle="tab" href="#data_upload_tab">Data upload</a>
#           </li>
#           <li >
#             <a data-toggle="tab" href="#settings_tab">Settings</a>
#           </li>
#           <li >
#             <a data-toggle="tab" href="#portfolio_overview_tab">Portfolio overview</a>
#           </li>
#           <li>
#             <a data-toggle="tab" href="#summary_tab">Summary</a>
#           </li>
#         </ul>
#         <div data-display-if="input.dataset_type ==  &#39;current&#39;  &amp;&amp; input.SQL_upload_current > 0"  >
#         <a class="btn btn-primary pull-right" id="submit_results">Submit results</a> </div>      
#       </div>
#     </div>
#   </div>
#   <!-- end of main panel --> '), 
#     
#   HTML('
#   <div class="row-fluid"><br><br></div>     <!-- extra space -->
# 
#   <!-- start of tab-content -->
#   <div class= "tab-content"> 
# 
#     <!-- start of Data upload panel -->
#     <div class="tab-pane active" data-value="" id="data_upload_tab">
# 
#   '),      
# #    tabPanel("Data loading", 
#         fluidRow(br(),br()),
#         fluidRow(
#           column(8, offset = 2, 
#             htmlOutput("dataset_message"), 
#            
#             htmlOutput("dataset_loaded") , 
#             htmlOutput("dataset_type")
#           )
#         ),
#         fluidRow(
#          column(4, offset = 2,
#            wellPanel(style="height:200px;",
#             strong("Select most recent dataset"),br(),
#             p("Make sure SQL input tables are updated before you start."),
# #             tags$button(class="btn action-button btn-block btn-large", id="SQL_upload_current", 
# #             type="button", as.character("Upload current data from SQL server"),
# #             onclick = "var curr_val = $('#SQL_upload_historical').val(0).change();" 
#             tags$input(id="SQL_upload_current", type="text", value="0" ,style="display:none;"),
#       tags$button( id="SQL_upload_current_button", type="button", class="btn btn-block btn-large", 
#      onclick="setTimeout(function() { var curr_val = $('#SQL_upload_current').val(); 
# $('#SQL_upload_current').val(parseInt(curr_val)+1).change(); } ,200 );
#                 $('#SQL_upload_historical').val('0').change(); " ,as.character("Upload current data from SQL server")),
#        #       '),          
#  
#            #               "$('#SQL_upload_historical').val('0').change()"
#             #              "$('#SQL_upload_historical').prop('disabled', true);$('#submit_id_choice').prop('disabled', true);"
#           #  ),
#             br(),   
#             div(class = "upload_current") 
#            )
#           ),
#          column(4, offset = 0,
#           wellPanel(style="height:200px;"  ,         
#            strong("Select historical dataset"),
#            htmlOutput("historical_dates"),
# #            tags$button(class="btn action-button btn-block btn-large", id="SQL_upload_historical", 
# #            type="button", as.character("Upload historical data from SQL server"),
# #            onclick = "$('#SQL_upload_current').val('0').change()"
#            #              $('#SQL_upload_current').prop('disabled', true);"
#          #  ),
#         tags$input(id="SQL_upload_historical", type="text", value="0" ,style="display:none;"),
#       tags$button( id="SQL_upload_historical_button", type="button", class="btn btn-block btn-large", 
#      onclick="setTimeout(function() { var curr_val = $('#SQL_upload_historical').val(); 
# $('#SQL_upload_historical').val(parseInt(curr_val)+1).change(); } ,200 );     
#    $('#SQL_upload_current').val('0').change(); " ,as.character("Upload historical data from SQL server")),
#            br(),
#            div(class = "upload_historical")
# #           <input class="input-xlarge" id="disabledInput" type="text" placeholder="Disabled input here..." disabled>
# #                       <span class="input-xlarge uneditable-input">Some value here</span>
#           )
#                )
#        ),
#   HTML('</div>
#   <!-- end of Data upload panel -->
# 
#   <!-- start of Settings panel -->
#   <div class="tab-pane" data-value="" id="settings_tab">
#       '),
#     conditionalPanel(condition = "input.dataset_loaded == 1 && input.dataset_type == 'historical'",
#       fluidRow(
#         column(5, offset = 1, 
#           h4("Setings for unsecured loans"),
#           htmlOutput("historical_settings_unsec")
#         ),
#         column(4, offset = 1,
#           h4("Settings for secured loans"),
#          htmlOutput("historical_settings_sec")
#         )
#                      
#       )               
#     ),
#     conditionalPanel(condition = "input.dataset_loaded == 1 && input.dataset_type == 'current'",
#       fluidRow(
#         column(3, class="well", 
#           HTML('
#           <ul class="nav nav-pills nav-stacked">
#             <li class="active">
#               <a data-toggle="tab" href="#tab_unsecured">Settings for unsecured</a>
#             </li>
#             <li>
#               <a data-toggle="tab" href="#tab_secured">Settings for secured</a>
#             </li> 
#           </ul> 
#           <hr>
#           <div style="position:relative;margin:auto;text-align:center;">
#           <button class="btn action-button btn-primary" id="save_settings" type="button">Save settings</button>
#           <span class="help-block" style="font-size:small">Press the button to continue with portfolio overview</span>
#           </div>
#                ') 
#         ),
#         column(9,                       
#           HTML(' 
#             <div class="tab-content"><div class="tab-pane active" id="tab_unsecured">'),
#              strong("Set parameters for portfolio(s):"),
#              br(),br(),
#              #HTML("<hr>"),
#              helpText("For each portfolio choose relevant recovery settings: length of history, 
#                       minimum periods in default, recovery horizon and type of loans to be included."),
#              uiOutput("unsecured_settings"),br(),
#              HTML('</div><div class="tab-pane" id="tab_secured">'),
#              strong("Set parameters for collateral type(s):"),
#              br(),br(),
#              #HTML("<hr>"),
#              helpText("For each collateral type choose relevant realization time (time-to-money) and appropraite haircut.
#                       These settings will be used for all portfolios in analysis for secured loans."),
#              
#              uiOutput("secured_settings"),br(),
#          HTML('</div> 
#             </div>')
#         )
#       ) 
#     ),
#   HTML('</div>
#   <!-- end of Settings panel -->
# 
#   <!-- start of Portfolio overview panel -->
#   <div class="tab-pane" data-value="" id="portfolio_overview_tab">
#       '),     
#     fluidRow(
#       column(3,
#       conditionalPanel(condition = "input.save_settings != 0 | input.dataset_type == 'historical'",
#         wellPanel(
#           strong("Select portfolio"),uiOutput("choose_portfolio"),                 
#           strong("Select date") ,  uiOutput("choose_date"),
#           hr(),
#           div(style="position:relative;margin:auto;text-align:center;",
#           tags$button(class="btn action-button btn-primary", id="show_overview",type="button", as.character("Show overview") ),
#           helpText("Allow some time for computation", style="font-size:small")),
#           conditionalPanel(condition ="$('html').hasClass('shiny-busy')",style="position:relative;margin:auto;text-align:center;",
#           #p("Calculation in progress.."),
#           img(src="ajaxloaderq.gif")
#            )
#         )
#       )                 
#       ),
#       column(9,
#         conditionalPanel(condition = "input.show_overview != 0  ",
#           div(style = "display:inline-block;",h4("Summary for portfolio")),
#           div(style = "display:inline-block", h4(htmlOutput("out_portfolio"))),
#           htmlOutput("summary_main")
#         )
#       )
#     ),
#     fluidRow(hr()),
#     fluidRow(
#       conditionalPanel(condition = "input.show_overview != 0 ",
#         column(4,
#           h4("Summary for unsecured part"),
#           htmlOutput("summary_unsecured")
#         ),
#         column(8,
#           showOutput("RecoveryCurve", "highcharts"),
#           #hr(),
#           helpText("Use slider below to see the recovery in selected horizon (in months)"),
#           htmlOutput("residual_slider"),
#           showOutput("ResidualCurve", "highcharts")
#         )
#       )                
#     ),
#     fluidRow(hr()),
#     fluidRow(
#       conditionalPanel(condition = "input.show_overview != 0 ",
#         column(4,
#           h4("Summary for secured part"),
#           htmlOutput("summary_secured")
#         ),
#         column(8,
#           showOutput("LGD_histogram", "highcharts")
#         )
#       ) 
#     ),       
#   HTML('</div>
#   <!-- end of Portfolio overview panel -->
# 
#   <!-- start of Summary panel -->
#   <div class="tab-pane" data-value="" id="summary_tab">
#       '),
#   conditionalPanel(condition = "input.dataset_loaded == 1 && input.dataset_type == 'historical'",
#     fluidRow(
#       column(8, offset = 2, 
#         h4("Summary"),
#         htmlOutput("historical_summary")
#       )            
#     )               
#   ),
#   conditionalPanel(condition = "input.dataset_loaded == 1 && input.dataset_type == 'current'",  
#     fluidRow(
#       column(4, offset = 4,
#         wellPanel(style="position:relative;margin:auto;text-align:center;",
#           tags$button(class="btn action-button btn-block btn-large btn-primary", id="all_calculate_button",
#           type="button", as.character("Show summary")),
#           helpText("Allow some time for computation", style="font-size:small"),
#           conditionalPanel(condition ="$('html').hasClass('shiny-busy')",
#            style="position:relative;margin:auto;text-align:center;",
#            #p("Calculation in progress.."),
#            img(src="ajaxloaderq.gif")
#            )
#         )
#       )
#     ),
#     fluidRow(
#       column(10, offset = 1, 
#         br(),
#           #div(class="summary_progress"),
#           lapply(1:22, function(x) {local({
#            #for (x in 1:10) {local({
#            i <- x
#            htmlOutput(paste('result', i, sep=''))
#            })}) 
#            #div(class="summary_progress",htmlOutput("all_calculate"))
#       )
#     )
#   ),
#   HTML('</div>
#   <!-- end of Portfolio overview panel -->
#   </div>
#   <!-- end of tab-content -- >
#        ')
#     
# #  )  
# 
#     
#     
#     
#     
#     
#     
#     
# #      ')
#     
# #           #              ,value="my_tab_1"),  
# #                              
# #                tabPanel("Settings",
# #                         
# #                         
# #                         ,value="tab_3"),
# #                tabPanel("Portfolio overview",
# 
# #                         ,value="portfolio_overview"),
# #                
# #                tabPanel("Summary",
# 
# # 
# #                 )  
#     
# #)  
# 
# )
# 
# 
# )   # closing bracket for shinyUI()
#   
# 
# 
# 



