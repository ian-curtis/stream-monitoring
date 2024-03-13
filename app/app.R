#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Next Steps:
# Future: build in validation (shinyFeedback package or validate() function)

library(shiny)
library(googledrive)
library(googlesheets4)
library(dplyr)
library(waiter)
library(cli)

# onStop(function() {
#   drive_deauth()
# })

# google_client <- gargle::gargle_oauth_client_from_json(
#   path = "./secret/oauth_secret_water.json",
#   name = "sm-google-client"
# )

drive_auth_configure(path = "./secret/oauth_secret_water.json")

# gargle::secret_decrypt_json(
#   system.file("secret", "googledrive-testing.json", package = "googledrive"),
#   "GOOGLEDRIVE_KEY"
# )

form_template <- "https://docs.google.com/forms/d/1TUC63kNrlLhupNcBSbPoQX0tGdCLpx9rSfVX5mFBG1U/edit"
main_template <- "https://docs.google.com/spreadsheets/d/1AmUUw_6sXgMzVGPQrE7ziaeig8p63pdkQSnVggxMfJE/edit"
site_template <- "https://docs.google.com/spreadsheets/d/1k2wjsG1VgqpUxwC124WaWlGSqIlHIYkTzAUwVRh-Uf4/edit"

hide_sheet <- function(ssid, sheet_id, op = c("hide", "show")) {
  
  op <- match.arg(op)
  
  hide_req <- list(
    updateSheetProperties =
      list(properties = list(sheetId = sheet_id,
                             hidden = ifelse(op == "hide", "True", "False")),
           fields = "hidden")
  )
  
  req <- request_generate(
    "sheets.spreadsheets.batchUpdate",
    params = list(
      spreadsheetId = ssid,
      requests = hide_req
    )
  )
  
  # print(req)
  
  resp_raw <- request_make(req)
  response <- gargle::response_process(resp_raw)
  
}


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
                 condition = "input.auth",
                 htmlOutput("greeting"),
                 br(),
                 tags$p("If you've authenticated and you are ready to start the project creation process, activate the Ready! button below."),
                 actionButton("ready", "Ready!"),
                 br(), br()
                 
               ),
               
               ## Waterbody question ####
               conditionalPanel(
                 condition = "input.ready",
                 textInput("wb", "What waterbody did you / will you measure data from?"),
                 actionButton("wb_entered", "Next"),
                 br(), br(),
               ),
               
               
               htmlOutput("confirm_wb"),
               br(),
               
               ## Number of Sites question ####
               conditionalPanel(
                 condition = "input.wb_entered",
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
                 textInput("proj_search", "Where would you like your project to live?"),
                 actionButton("parent_dir_entered", "Next"),
                 br(), br()
               ),
               
          
               htmlOutput("confirm_parent_dir"),
               br(), br(),
               
               ## Communicate that the next part is mostly automated but there will be some manual work ####
               
               conditionalPanel(
                 condition = "output.good_to_go==\"yes\"",
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
                 actionButton("primary_linked", "Continue!")
               ),
               
               # MAYBE SOMETHING HERE TO CHECK THAT THE CONNECTION WAS DONE CORRECTLY
               
               ## Request manual edit to approve linkage between site sheet(s) and primary sheet ####
               conditionalPanel(
                 condition = "output.connect_site==\"ready\"",
                 h2("Manual Edit Requested: Site Specific Sheets"),
                 htmlOutput("site_link_msg"), br(),
                 actionButton("site_linked", "Continue!")
               ),
               
               # MAYBE SOMETHING HERE TO CHECK THAT THE SITE CONNECTIONS WERE DONE CORRECTLY
               
               # Wrap up the app with a concluding paragraph ####
               conditionalPanel(
                 condition = "output.proj_status==\"complete\"",
                 p("Congrats! The creation process has been completed. You now have a fully functional stream monitoring project. At this point, enter your data into the Google Form. The spreadsheets will automatically update and populate with data. Necessary charts will appear in the site-specific sheets which you can then export as PNG images to share with the public. Have fun!!")
               )
              
             )
             ),
    
    
    tabPanel("How Your Data Is Used",
             p("Information on what data is stored and used and how/why.")
             )
        
  )
)

# Define server logic ####
server <- function(input, output) {
  
  waiter_hide()

  # record the name of the user and send a greeting ####
  user <- eventReactive(input$auth, {
    drive_auth(email = FALSE)
    gs4_auth(token = drive_token())
    list(name = drive_user()$displayName)
  })
  
  output$greeting <- renderText({
    HTML(paste0("Hey ",
           "<span style=\"color: #099392\"><b>", user()$name, "</b></span>",
           "! Welcome to the app. Now that you're logged in, we can move forward. You first will be asked a couple questions about your project which will help us determine how many files to create. Once these questions are complete, the majority of the process will happen automatically. However, there are a few manual steps. We'll pause a couple times and ask you to perform an action or two."))
  })
  
  # record the waterbody data was recorded from and send a confirmation
  waterbody <- eventReactive(input$wb_entered, {
    as.character(input$wb)
  })
  output$confirm_wb <- renderText({
    HTML(paste0("Alright! Looks like you have recorded / will record data from ", 
                "<span style=\"color: #099392\"><b>", waterbody(), "</b></span>",
                "! If this is correct, please enter the number of sites you are measuring from."))
  })
  
  # record the number of sites and send a confirmation
  nsites <- eventReactive(input$n_entered, {
    as.integer(input$n_sites)
  })
  output$confirm_sites <- renderText({
    HTML(paste0("Cool! Looks like you want to have data for ", 
                               "<span style=\"color: #099392\"><b>", pluralize("{nsites()} site{?s}."), "</b></span> ",
                "If that's correct, use the box below to find a location for your project. Type in the name of an existing Google Drive folder you want your project to live in. If you'd like your project to live in your Drive's home page (\"My Drive\"), type nothing and just hit \"Next\". FYI: The project itself will appear as a folder with files within. Note: search terms are CASE SENSITIVE."
    ))
  })
  
  # record the location of the project and try to find it

  rv <- reactiveValues(results = NULL)

  observeEvent(input$parent_dir_entered, {
    
    if (input$proj_search == "") {
      
      rv$results <- NA
      
    } else {
      
      waiter <- waiter::Waiter$new(html = div(
        spin_loaders(10),
        "Searching Google Drive..."))
      waiter$show()
      on.exit(waiter$hide())
      
      term <- reactive(input$proj_search)
      
      search_results <- drive_find(term(), type = "folder")
      
      rv$results <- search_results
      
    }
    
    
  })
  
  
  confirm_dir_message <- eventReactive(rv$results, {
    
    if (length(rv$results) == 1) {
      
      message <- HTML(paste0(
        "Got it! You want your project to live inside of the folder called ",
        "<span style=\"color: #099392\"><b>My Drive</b></span>."
      ))
      continue <- "yes"
      
    } else if (nrow(rv$results) == 0) {

        message <- "Looks like no results were found. Did you make your search case sensitive (e.g., \"Data\" ≠ \"data\")? Go ahead and try to search again."
        continue <- "no"

    } else if (nrow(rv$results) == 1) {

      message <- HTML(paste0(
        "Got it! You want your project to live inside of the folder called ",
        "<span style=\"color: #099392\"><b>", rv$results$name, "</b></span>."
      ))
      continue <- "yes"

    } else {

      message <- "We found too many results for that search term. Try entering a more specific phrase."
      continue <- "no"
      
    }

    list(message, continue)
    
  })
  
  output$confirm_parent_dir <- renderText({confirm_dir_message()[[1]]})
  output$good_to_go <- renderText({confirm_dir_message()[[2]]})
  outputOptions(output, "good_to_go", suspendWhenHidden = FALSE)

  ## Main Project Creation Part 1 ####
  
  ### Set Up Google Form ####
  observeEvent(input$start_project, {
    
    waiter <- waiter::Waiter$new(html = div(
      spin_loaders(10),
      "Creating Google Form (for data input)..."))
    waiter$show()
    on.exit(waiter$hide())
    
    if(is.na(rv$results)) given_dir <- NULL else given_dir <- rv$results
    
    proj_dir <- drive_mkdir(
      name = paste0("Stream Monitoring - ", input$wb), 
      path = given_dir)
    
    form_copy <- drive_cp(form_template, name = "01 Data Entry Form") %>% 
      drive_mv(path = proj_dir)
    form_link <- drive_link(form_copy)
    form_resp_link <- paste0(drive_link(form_copy), "#responses")
    
    rv$wb <- input$wb
    rv$proj_dir <- proj_dir
    rv$form_copy <- form_copy
    rv$form_link <- form_link
    rv$form_resp_link <- form_resp_link
    rv$edit_form <- "now"
  })
  
  output$edit_form <- renderText({rv$edit_form})
  outputOptions(output, "edit_form", suspendWhenHidden = FALSE)
  
  output$edit_form_msg <- renderText({
    
    HTML(paste(
      "You'll need to create a Sheets file connected to the Google form that was just created.",
      "Just go to the link below and click on the \"Link to Sheets\" button in your browser.",
      "You'll want to \"Create a New Sheet\" (and not use an existing sheet). Do not edit the default title.",
      a(href = rv$form_resp_link, "Link to your Google Form"),
      "Have you created the new Sheet? If so, press the \"Continue\" button.",
      sep = "<br><br>"
    ))
    
  })
  
  ### Confirm Sheet was connected to the form ####
  
  # If found, get its link and rename it
  observeEvent(input$form_edited, {
    
    files_in_dir <- drive_ls(rv$proj_dir) %>% nrow()
    
    if (files_in_dir == 1) rv$redo_form <- "yes" else rv$redo_form <- "no"
    
  })
  
  output$redo_form <- renderText({rv$redo_form})
  outputOptions(output, "redo_form", suspendWhenHidden = FALSE)

  ### Rename Responses Sheet and Set Up Primary Data Sheet ####

  observeEvent(input$form_created, {
    
    waiter <- waiter::Waiter$new(html = div(
      spin_loaders(10),
      "Renaming Form Responses Sheet..."))
    waiter$show()
    
    #### move and rename form responses sheet ####
    form_responses <- drive_ls(rv$proj_dir) %>% 
      filter(name == "01 Data Entry Form (Responses)")
    form_r_link <- drive_link(form_responses)
    
    # rv$form_r_link <- form_r_link
    sheet_rename(form_r_link, new_name = "Data")
    drive_rename(form_r_link, name = "02 Raw Data")
    
    waiter$hide()
    
    waiter <- waiter::Waiter$new(html = div(
      spin_loaders(10),
      "Creating Primary Datasheet..."))
    waiter$show()
    
    #### copy the primary sheet and move it to the correct place ####
    main_copy <- drive_cp(main_template, name = "03 Primary Datasheet") %>%
      drive_mv(path = rv$proj_dir)
    main_link <- drive_link(main_copy)
    rv$main_link <- main_link
    
    waiter$hide()
    
    waiter <- waiter::Waiter$new(html = div(
      spin_loaders(10),
      "Setting up connections between raw data and primary sheet..."))
    waiter$show()
    
    #### write in the waterbody name to the sub data sheet ####
    range_write(main_copy,
                data.frame(x = rv$wb),
                sheet = "Sub Data",
                range = "L2",
                col_names = FALSE)
    
    #### set up connections between the raw data and the primary sheet ####
    
    
    import_ecoli <- paste0("=QUERY(IMPORTRANGE(\"", form_r_link,
                           "\", \"'Data'!L2:P\"), ",
                           "\"select * where Col1 is not null order by Col2\")")
    
    range_write(main_copy,
                data.frame(x = gs4_formula(import_ecoli)),
                sheet = "All E. Coli Data",
                range = "A2",
                col_names = FALSE)
    
    import_macro <- paste0("=QUERY(IMPORTRANGE(\"", form_r_link,
                           "\", \"'Data'!Q2:T\"), ",
                           "\"select * where Col1 is not null order by Col2\")")
    
    range_write(main_copy,
                data.frame(x = gs4_formula(import_macro)),
                sheet = "All Macro Data",
                range = "A2",
                col_names = FALSE)
    
    import_chem <- paste0("=QUERY(IMPORTRANGE(\"", form_r_link,
                          "\", \"'Data'!D2:K\"), ",
                          "\"select * where Col1 is not null order by Col2\")")
    
    range_write(main_copy,
                data.frame(x = gs4_formula(import_chem)),
                sheet = "All Stream Chem Data",
                range = "A2",
                col_names = FALSE)
    
    #### hide the last two sheets (sub data and MPN table) ####
    sheet_ids <- sheet_properties(main_copy) %>% 
      filter(index >= 3) %>% 
      select(id)
    
    hide_sheet(main_copy$id, sheet_ids[[1]][[1]], "hide")
    hide_sheet(main_copy$id, sheet_ids[[1]][[2]], "hide")
    
    rv$connect_primary <- "ready"
    
    waiter$hide()
    
  })
  
  output$connect_primary <- renderText({rv$connect_primary})
  outputOptions(output, "connect_primary", suspendWhenHidden = FALSE)
  
  output$primary_link_msg <- renderText({
    
    HTML(paste(
      "You'll need to give Google permission to import data from another spreadsheet. In this case, we are looking to import data from the raw data file (that you created earlier) into the primary datasheet (which houses all of the data together in one place and applies some calculations).",
      "To do this, head to the link below and click on the cell that says \"#REF!\".",
      "A dialog box should appear. Use the \"Allow Access\" button to give Google permission to read from the other file.",
      "You should see the \"#REF!\" change to a \"#N/A\". This is expected and indicates that it worked.",
      "Here's that file:",
      a(href = rv$main_link, "Link to the Primary Datasheet"),
      "Have you approved the connection? If so, press the \"Continue\" button.",
      sep = "<br><br>"
    ))
    
  })
  
  ### Create the site sheets (and ask for manual input) ####
  
  import_site_data <- function(primary_link, source_str, range, site_num) {
    
    string <- paste0("=QUERY(IMPORTRANGE(\"", primary_link, 
                     "\", \"'", source_str, "'!", range, "\"), \"select * where Col2 = 'Site ", site_num, "'\", 1)")
    
    return(string)
    
  }
  
  observeEvent(input$primary_linked, {
    
    site_sheets <- list()
    
    for (site in seq(input$n_sites)) {
      
      waiter <- waiter::Waiter$new(html = div(
        spin_loaders(10),
        paste0("Setting up Site Sheet ", site, " of ", input$n_sites, "...")))
      waiter$show()
      
      site_copy <- drive_cp(site_template, name = paste0("04-", site, " Site ", site, " Sheet")) %>%
        drive_mv(path = rv$proj_dir)
      site_link <- drive_link(site_copy)
      site_sheets[[site]] <- site_link
      
      # site e. coli data
      range_write(site_copy,
                  data.frame(x = gs4_formula(import_site_data(rv$main_link, "All E. Coli Data", "A:F", site))),
                  sheet = "Data",
                  range = "A1",
                  col_names = FALSE)
      
      # site macro data
      range_write(site_copy,
                  data.frame(x = gs4_formula(import_site_data(rv$main_link, "All Macro Data", "A:F", site))),
                  sheet = "Data",
                  range = "K1",
                  col_names = FALSE)
      
      # site stream chem data
      range_write(site_copy,
                  data.frame(x = gs4_formula(import_site_data(rv$main_link, "All Stream Chem Data", "A:H", site))),
                  sheet = "Data",
                  range = "S1",
                  col_names = FALSE)
      
      # site macro data
      range_write(site_copy,
                  data.frame(x = gs4_formula(paste0("=IMPORTRANGE(\"", rv$main_link, "\", \"'Sub Data'!D:J\")"))),
                  sheet = "Data",
                  range = "AB1",
                  col_names = FALSE)
    
      waiter$hide()
    }
    
  rv$site_sheets <- site_sheets
  rv$connect_site <- "ready"
    
  })
  
  output$connect_site <- renderText({rv$connect_site})
  outputOptions(output, "connect_site", suspendWhenHidden = FALSE)
  

  make_site_links <- function(site_sheets) {
    
    site_links <- ""
    
    for (i in length(site_sheets)) {
      print(i)
      link <- site_sheets[[i]]
      print(link)
      site_links <- paste0(a(href = link, paste("Link to Site", i, "Sheet")), "<br>")
      print(site_links)
      
    }
    gsub('.{4}$', '', site_links)
    site_links
    
  }
  
  output$site_link_msg <- renderText({
    HTML(paste(
      "You'll again need to give Google permission to import data from another spreadsheet. In this case, we are looking to import data from the primary datasheet (that you created earlier) into the site-specific datasheet(s) (which house all of the data for a specific site and generate plots).",
      "You will need to do this for each of the site files that were created. Head to each of the links below and click on the cell that says \"#REF!\".",
      "A dialog box should appear. Use the \"Allow Access\" button to give Google permission to read from the other file.",
      "You should see the \"#REF!\" change to a \"#N/A\". This is expected and indicates that it worked.",
      "The file(s) to approve connections for:",
      make_site_links(rv$site_sheets),
      "Have you approved the connection? If so, press the \"Continue\" button.",
      sep = "<br><br>"
    ))
  })
  
  observeEvent(input$site_linked, {
    
    ## Hide the "data" sheet in the spreadsheet ####
    for (sheet in rv$site_sheets) {
      
      sheet_ids <- sheet_properties(sheet) %>% 
        filter(index == 0) %>% 
        select(id)
      hide_sheet(sheet, sheet_ids[[1]][[1]], "hide")
      
    }
    
    rv$proj_status <- "complete"
    
    
  })
  
  output$proj_status <- renderText({rv$proj_status})
  outputOptions(output, "proj_status", suspendWhenHidden = FALSE)
  
}


# Run the application 
shinyApp(ui, server)
