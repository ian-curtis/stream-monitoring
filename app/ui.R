# Define UI ####
ui <- fluidPage(
  theme = bs_theme(version = 4,
                   base_font = c("Georgia", "Garamond", "'Times New Roman'", "Times", "serif")
  ),
  # tags$head(
  #   tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  # ),
  useWaiter(),
  waiterShowOnLoad(),
  
  
  
  navbarPage(
    "Stream Monitoring: New Project Creation",
    tabPanel("Home",
             mainPanel(
               tags$p("Welcome to the interface used to create a new stream monitoring project!")
             )
    ),
    
    # Primary page for creating the project ####
    tabPanel("Create the Project!",
             mainPanel(
               
               ## Authenticate ####
               # A Privacy Policy Draft has been written (below) 
               # If this part of the project is not going to be made public, it probably isn't needed
               tags$p("Hello! Before we begin, we need access to your Google account. This is necessary to create files and share the project with you. For information on how your data is used, see How Your Data Is Used."),
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
                 p("Now, enter the name of the waterbody you collected/will collect data from. Be sure to spell this correctly (and with correct casing) as this will be used for the Google folder name and will be inserted into all of the dashboard plots. It is possible to edit mistakes later, but it will be time consuming."),
                 br(),
                 textInput("wb", "What waterbody are you collecting data from?"),
                 actionButton("wb_entered", "Next"),
                 br(), br(),
               ),
               
               conditionalPanel(
                 condition = "input.wb_entered",
                 htmlOutput("confirm_wb")
               ),
               
               br(),
               
               ## Project location question ####
               conditionalPanel(
                 condition = "output.wb_ready",
                 p("As of the current version of this application, your project will be built to your Google Drive. It will be placed in your \"home\" page, also known as \"My Drive\". The project will appear as a folder with several files inside of it. Once the project creation process has finished, you are free to move the new folder anywhere in your Drive."),
                 tags$p("Do note that the project will create enough files to support up to six unique sites. You do not have to use all six sites at once but they are there in case you wish to start measuring from a new site."),
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
    
    # Privacy Policy ####
    tabPanel("How Your Data Is Used",
             p("Your privacy is a primary concern. When designing this web app, care was taken to ensure that all data that is collected is used and that no unnecessary data is collected or stored. Be assured that your data is not sent to be stored on a server, nor will it be sold to anyone else."),
             p("In order to enable Google sign on, an app has to be verified through Google who ensures that its sign on capabilities are being used properly. Once you as a user sign in through Google, the app technically has access to all of your Google Drive files. It is able to read them, write to them, create new ones, delete some, and could even empty your trash for you. We request such big permissions here because we are in fact reading files, creating new files, and editing files. However, only the actions that need to be done are programmed in. In fact, there are no commands in the app for deleting files. If you accidentally start the program and need to start over, you will need to manually delete any files the program creates. This is in place to ensure that no accidental deletion of important files occurs."),
             p("The only personal data about you that is accessed is the name associated with your Google account. We do this not to get access to your data but to help you verify that you have logged in to the correct account. The app will greet you by name and that is all your name is used for."),
             p("Once you have logged in, an access token is created allowing the app access to your Google files. This token IS cached (i.e., is temporarily stored within the app). We do this to to make the sign on process as simple as possible and to ensure that the authentication process will work both when testing the app and when it is actually published online."),
             p("Once you close the app, the folder and file containing your account's access token is deleted off of the app. If you'd like to use the app again, you will have to sign in again to allow the app to create a new token for your account. We hope that this helps you be assured that your files and your data are only used to make the project file and that's it.")
    )
    
  )
)
ui