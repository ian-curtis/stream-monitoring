# Define UI ####
ui <- fluidPage(
  useWaiter(),
  waiterShowOnLoad(),
  
  
  
  navbarPage(
    "Stream Monitoring: New Project Creation",
    tabPanel("Home",
             mainPanel(
               tags$p("Welcome to the interface used to create a new stream monitoring project!"),
               p("Details about why this exists, who might want to use it, and what it does")
             )
    ),
    
    # Primary page for creating the project ####
    tabPanel("Create the Project!",
             mainPanel(
               
               ## Authenticate ####
               tags$p("Hello! Before we begin, we need access to your Google account. This is necessary to create files and share the project with you. For information on how your data is used, see the PRIVACY POLICY."),
               actionButton("auth", "Authenticate With Google"),
               br(), br(),
               
               ## Explanation of process and Ready? Question ####
               conditionalPanel(
                 condition = "output.auth_complete==\"yes\"",
                 htmlOutput("greeting"),
                 br(),
                 tags$p("If you've authenticated and you are ready to start the project creation process, activate the Ready! button below."),
                 actionButton("ready", "Ready!"),
                 br(), br()
                 
               ),
               
               ## Waterbody question ####
               conditionalPanel(
                 condition = "input.ready",
                 textInput("wb", "What waterbody are you collecting data from?"),
                 actionButton("wb_entered", "Next"),
                 br(), br(),
               ),
               
               conditionalPanel(
                 condition = "input.wb_entered",
                 htmlOutput("confirm_wb")
               ),

               ## Number of Sites question ####
               conditionalPanel(
                 condition = "output.wb_ready==\"yes\"", # NEW CONDITION HERE
                 br(),
                 selectInput("n_sites", label = "Number of Sites Recorded",
                             choices = c(1, 2, 3, 4, 5, 6), selected = 1),
                 actionButton("n_entered", "Next"),
                 br(), br()
               ),
               
               htmlOutput("confirm_sites"),
               br(),
               
               ## Project location question ####
               conditionalPanel(
                 condition = "input.n_entered",
                 p("As of the current version of this application, your project will be built to your Google Drive. It will be place in your \"home\" page, also known as \"My Drive\". The project will appear as a folder with several files inside of it. Once the project creation process has finished, you are free to move the new folder anywhere in your Drive."),
                 actionButton("proj_loc_accepted", "Continue"),
                 # textInput("proj_search", "Where would you like your project to live?"),
                 # actionButton("parent_dir_entered", "Next"),
                 br(), br()
               ),
               
               
               # htmlOutput("confirm_parent_dir"),
               # br(), br(),
               
               ## Communicate that the next part is mostly automated but there will be some manual work ####
               
               conditionalPanel(
                 # condition = "output.good_to_go==\"yes\"",
                 condition = "input.proj_loc_accepted",
                 p("Ok! From here on out, the process is mostly automated. There will be a few times where we'll stop for some manual actions and checks, but we'll be sure to let you know when we get there."),
                 p("If you're ready (and you're sure the information you entered above is correct) hit the \"Ready\" Button!"),
                 actionButton("start_project", "Ready!"),
                 br(), br()
               ),
               
               ## Request manual edit for connecting the sheet to the form ####
               
               conditionalPanel(
                 condition = "output.edit_form==\"now\"",
                 h2("Manual Edit Requested: Google Form"),
                 htmlOutput("edit_form_msg"), br(),
                 actionButton("form_edited", "Continue!"),
                 br(), br()
               ),
               
               ## If we detect that the connected sheet was NOT created ####
               
               conditionalPanel(
                 condition = "output.redo_form==\"yes\"",
                 p("Oops! It couldn't be verified that a Google Sheet was connected to the newly-created Google Form. Please verify you followed the instructions above. As a reminder, you will need to access the link provided and use the \"Link to Sheets\" button on the Form. Then, make sure you create a new sheet (do not change the default name). Once you've done this, select the \"Continue!\" button again.")
               ),
               
               
               ## If we detect that the connected sheet WAS created #### 
               
               conditionalPanel(
                 condition = "output.redo_form==\"no\"",
                 p("Great! We just verified that you successfully linked that Sheet to the Form. Ready to move on?"),
                 actionButton("form_created", "Keep Going!"),
                 br(), br()
               ),
               
               
               ## Request manual edit to approve linkage between primary sheet and raw data ####
               conditionalPanel(
                 condition = "output.connect_primary==\"ready\"",
                 h2("Manual Edit Requested: Primary Datasheet"),
                 htmlOutput("primary_link_msg"), br(),
                 actionButton("primary_linked", "Continue!"),
                 br(), br()
               ),
               
               # MAYBE SOMETHING HERE TO CHECK THAT THE CONNECTION WAS DONE CORRECTLY
               
               ## Request manual edit to approve linkage between site sheet(s) and primary sheet ####
               conditionalPanel(
                 condition = "output.connect_site==\"ready\"",
                 h2("Manual Edit Requested: Site Specific Sheets"),
                 htmlOutput("site_link_msg"), br(),
                 actionButton("site_linked", "Continue!"),
                 br(), br()
               ),
               
               # MAYBE SOMETHING HERE TO CHECK THAT THE SITE CONNECTIONS WERE DONE CORRECTLY
               
               # Wrap up the app with a concluding paragraph ####
               conditionalPanel(
                 condition = "output.proj_status==\"complete\"",
                 p("Congrats! The creation process has been completed. You now have a fully functional stream monitoring project. At this point, enter your data into the Google Form. The spreadsheets will automatically update and populate with data. Necessary charts will appear in the site-specific sheets which you can then export as PNG images to share with the public. Have fun!!"),
                 br(), br()
               )
               
             )
    ),
    
    
    tabPanel("How Your Data Is Used",
             p("Information on what data is stored and used and how/why.")
    )
    
  )
)

ui