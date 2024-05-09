# Define server logic ####
server <- function(input, output, session) {
  waiter_hide()
  
  session$onSessionEnded(function() { 
    
    unlink(".secrets/.user", recursive = TRUE)
    
    })
  
  rv <- reactiveValues(results = NULL)
  
  # record the name of the user and send a greeting ####
  observeEvent(input$auth, {
    
    # drive_auth(email = FALSE, cache = here::here("app/.cache"))
    drive_auth(email = TRUE, cache = ".secrets/.user")
    gs4_auth(token = drive_token())
    
    rv$auth_complete <- "yes"
    rv$user <- drive_user()$displayName
    
  })
  
  output$greeting <- renderText({
    
    HTML(paste0("Hey ",
                "<span style=\"color: #099392\"><b>", rv$user, "</b></span>",
                "! Welcome to the app. Now that you're logged in, we can move forward. You first will be asked a couple questions about your project which will help us determine how many files to create. Once these questions are complete, the majority of the process will happen automatically. However, there are a few manual steps. We'll pause a couple times and ask you to perform an action or two."))
  })
  
  output$auth_complete <- renderText({rv$auth_complete})
  outputOptions(output, "auth_complete", suspendWhenHidden = FALSE)
  
  # record the waterbody data was recorded from and send a confirmation ####
  observeEvent(input$wb_entered, {
    
    rv$wb <- as.character(input$wb)
    
    if (any(str_detect(rv$wb, no_no)) | rv$wb == "") rv$wb_ready <- "no" else rv$wb_ready <- "yes"
    
  })
  
  output$confirm_wb <- renderText({
    validate(
      need(!any(str_detect(rv$wb, no_no)), message = "Please keep your input family friendly. Try again."),
      need(rv$wb != "", message = "Waterbody name cannot be empty.")
    )
    HTML(paste0("Alright! Looks like you are recording data from ", 
                "<span style=\"color: #099392\"><b>", rv$wb, "</b></span>",
                "! If this is not correct, please type a new name and try again."))
  })
  
  output$wb_ready <- renderText({rv$wb_ready})
  outputOptions(output, "wb_ready", suspendWhenHidden = FALSE)
  
  # record the number of sites and send a confirmation ####
  # nsites <- eventReactive(input$n_entered, {
  #   as.integer(input$n_sites)
  # })
  # output$confirm_sites <- renderText({
  #   HTML(paste0("Cool! Looks like you want to have data for ", 
  #               "<span style=\"color: #099392\"><b>", pluralize("{nsites()} site{?s}."), "</b></span> ",
  #               "If that's correct, use the box below to find a location for your project. Type in the name of an existing Google Drive folder you want your project to live in. If you'd like your project to live in your Drive's home page (\"My Drive\"), type nothing and just hit \"Next\". FYI: The project itself will appear as a folder with files within. Note: search terms are CASE SENSITIVE."
  #   ))
  # })
  
  # output$confirm_sites <- renderText({
  #   HTML(paste0("Cool! Looks like you want to have data for ", 
  #                             "<span style=\"color: #099392\"><b>", 
  #               pluralize("{nsites()} site{?s}."), 
  #               "</b></span> "))
  # })
  
  # record the location of the project and try to find it ####
  
  # observeEvent(input$parent_dir_entered, {
  #   
  #   if (input$proj_search == "") {
  #     
  #     rv$results <- NA
  #     
  #   } else {
  #     
  #     waiter <- waiter::Waiter$new(html = div(
  #       spin_loaders(10),
  #       "Searching Google Drive..."))
  #     waiter$show()
  #     on.exit(waiter$hide())
  #     
  #     term <- reactive(input$proj_search)
  #     
  #     search_results <- drive_find(term(), type = "folder")
  #     
  #     rv$results <- search_results
  #     
  #   }
  #   
  #   
  # })
  
  
  # confirm_dir_message <- eventReactive(rv$results, {
  #   
  #   if (length(rv$results) == 1) {
  #     
  #     message <- HTML(paste0(
  #       "Got it! You want your project to live inside of the folder called ",
  #       "<span style=\"color: #099392\"><b>My Drive</b></span>."
  #     ))
  #     continue <- "yes"
  #     
  #   } else if (nrow(rv$results) == 0) {
  #     
  #     message <- "Looks like no results were found. Did you make your search case sensitive (e.g., \"Data\" ≠ \"data\")? Go ahead and try to search again."
  #     continue <- "no"
  #     
  #   } else if (nrow(rv$results) == 1) {
  #     
  #     message <- HTML(paste0(
  #       "Got it! You want your project to live inside of the folder called ",
  #       "<span style=\"color: #099392\"><b>", rv$results$name, "</b></span>."
  #     ))
  #     continue <- "yes"
  #     
  #   } else {
  #     
  #     message <- "We found too many results for that search term. Try entering a more specific phrase."
  #     continue <- "no"
  #     
  #   }
  #   
  #   list(message, continue)
  #   
  # })
  
  # output$confirm_parent_dir <- renderText({confirm_dir_message()[[1]]})
  # output$good_to_go <- renderText({confirm_dir_message()[[2]]})
  # outputOptions(output, "good_to_go", suspendWhenHidden = FALSE)
  
  ## Main Project Creation Part 1 ####
  
  ### Set Up Google Form ####
  observeEvent(input$start_project, {
    
    removeUI(selector='#start_project', immediate=TRUE)
    
    waiter <- waiter::Waiter$new(html = div(
      spin_loaders(10),
      "Creating a new folder and a Google Form (for data input)..."))
    waiter$show()
    on.exit(waiter$hide())
    
    # if(is.na(rv$results)) given_dir <- NULL else given_dir <- rv$results
    # 
    # proj_dir <- drive_mkdir(
    #   name = paste0("Stream Monitoring - ", input$wb), 
    #   path = given_dir)
    
    proj_dir <- drive_mkdir(
      name = paste0("Stream Monitoring - ", input$wb)
    )
    
    checklist_copy <- drive_cp(proj_checklist, name = "00 Project Checklist and Notes") %>% 
      drive_mv(path = proj_dir)
    
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
      a(href = rv$form_resp_link, "Link to your Google Form (opens in new window)", target = "_blank"),
      "Have you created the new Sheet? If so, press the \"Continue\" button.",
      sep = "<br><br>"
    ))
    
  })
  
  ### Confirm Sheet was connected to the form ####
  
  # If found, get its link and rename it
  observeEvent(input$form_edited, {
    
    files_in_dir <- drive_ls(rv$proj_dir) %>% nrow()
    
    if (files_in_dir == 2) rv$redo_form <- "yes" else rv$redo_form <- "no"
    
  })
  
  output$redo_form <- renderText({rv$redo_form})
  outputOptions(output, "redo_form", suspendWhenHidden = FALSE)
  
  ### Rename Responses Sheet and Set Up Primary Data Sheet ####
  
  observeEvent(input$form_created, {
    
    removeUI(selector='#form_created', immediate=TRUE)
    
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
    drive_rename(form_r_link, name = "02 Raw Data (only edit if necessary")
    
    waiter$hide()
    
    waiter <- waiter::Waiter$new(html = div(
      spin_loaders(10),
      "Creating Primary Datasheet..."))
    waiter$show()
    
    #### copy the primary sheet and move it to the correct place ####
    main_copy <- drive_cp(main_template, name = "03 Primary Datasheet (do not edit)") %>%
      drive_mv(path = rv$proj_dir)
    main_link <- drive_link(main_copy)
    rv$main_link <- main_link
    
    waiter$hide()
    
    waiter <- waiter::Waiter$new(html = div(
      spin_loaders(10),
      "Setting up connections between raw data and primary sheet..."))
    waiter$show()
    
    #### set up connections between the raw data and the primary sheet ####
    
    
    import_ecoli <- paste0("=QUERY(IMPORTRANGE(\"", form_r_link,
                           "\", \"'Data'!K2:O\"), ",
                           "\"select * where Col1 is not null order by Col2\", 0)")
    
    range_write(main_copy,
                data.frame(x = gs4_formula(import_ecoli)),
                sheet = "All E. Coli Data",
                range = "A2",
                col_names = FALSE)
    
    import_macro <- paste0("=QUERY(IMPORTRANGE(\"", form_r_link,
                           "\", \"'Data'!P2:T\"), ",
                           "\"select * where Col3 is not null order by Col1, Col2\", 0)")
    
    range_write(main_copy,
                data.frame(x = gs4_formula(import_macro)),
                sheet = "All Macro Data",
                range = "A2",
                col_names = FALSE)
    
    import_chem <- paste0("=QUERY(IMPORTRANGE(\"", form_r_link,
                          "\", \"'Data'!C2:J\"), ",
                          "\"select * where Col1 is not null order by Col2\", 0)")
    
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
      a(href = rv$main_link, "Link to the Primary Datasheet (opens in new window)", target = "_blank"),
      "Have you approved the connection? If so, press the \"Continue\" button.",
      sep = "<br><br>"
    ))
    
  })
  
  ### tk VERIFY THE CONNECTIONS WERE DONE CORRECTLY
  
  ### Create the site sheets (and ask for manual input) ####
  
  observeEvent(input$primary_linked, {
    
    site_sheets <- list()
    
    for (site in seq(6)) {
      
      waiter <- waiter::Waiter$new(html = div(
        spin_loaders(10),
        paste0("Setting up Site Sheet ", site, " of 6...")))
      waiter$show()
      
      site_copy <- drive_cp(site_template, name = paste0("04-", site, " Site ", site, " Sheet (do not edit)")) %>%
        drive_mv(path = rv$proj_dir)
      site_link <- drive_link(site_copy)
      site_sheets[[site]] <- site_link
      
      # site e. coli data
      ecoli_str <- paste0("=QUERY(IMPORTRANGE(\"", rv$main_link, 
                          "\", \"'", "All E. Coli Data", "'!A:F\"), 
                       \"select * where Col2 = 'Site ", site, "' order by Col1, Col3\", 1)")
      range_write(site_copy,
                  data.frame(x = gs4_formula(ecoli_str)),
                  sheet = "Raw Data",
                  range = "A1",
                  col_names = FALSE)
      
      # site macro data
      macro_str <- paste0("=QUERY(IMPORTRANGE(\"", rv$main_link, 
                          "\", \"'", "All Macro Data", "'!A:E\"), 
                       \"select * where Col3 = 'Site ", site, "' order by Col2\", 1)")
      range_write(site_copy,
                  data.frame(x = gs4_formula(macro_str)),
                  sheet = "Raw Data",
                  range = "K1",
                  col_names = FALSE)
      
      # site stream chem data
      chem_str <- paste0("=QUERY(IMPORTRANGE(\"", rv$main_link, 
                         "\", \"'", "All Stream Chem Data", "'!A:H\"), 
                       \"select * where Col2 = 'Site ", site, "' order by Col1\", 1)")
      range_write(site_copy,
                  data.frame(x = gs4_formula(chem_str)),
                  sheet = "Raw Data",
                  range = "T1",
                  col_names = FALSE)
      
      # site sub data
      range_write(site_copy,
                  data.frame(x = gs4_formula(paste0("=IMPORTRANGE(\"", rv$main_link, "\", \"'Sub Data'!A:I\")"))),
                  sheet = "Raw Data",
                  range = "AC1",
                  col_names = FALSE)
      
      # Change these numbers later
      # for (chart_num in seq(1, 8)) {
      #   
      #   update_title(site_copy$id, chart_num, site, rv$wb)
      #   
      # }
      
      # for (chart_num in c(1, 2, 5, 6, 7, 8)) {
      #   
      #   update_title(site_copy$id, chart_num, site, rv$wb)
      #   
      # }
      
      
      waiter$hide()
    }
    
    rv$site_sheets <- site_sheets
    rv$connect_site <- "ready"
    
  })
  
  output$connect_site <- renderText({rv$connect_site})
  outputOptions(output, "connect_site", suspendWhenHidden = FALSE)
  
  
  make_site_links <- function(site_sheets) {
    
    site_links <- ""
    
    for (i in seq(rv$n_sites)) {
      
      link <- site_sheets[[i]]
      site_links <- paste0(site_links, a(href = link, paste("Link to Site", i, "Sheet (opens in new window)"), target = "_blank"), "<br>")
      
    }
    gsub("<br>$", "", site_links)
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
  
  # ADD IN VERIFICATION THAT THE SHEETS WERE LINKED PROPERLY
  
  observeEvent(input$site_linked, {
    
    waiter <- waiter::Waiter$new(html = div(
      spin_loaders(10),
      paste0("Finalizing site sheets...")))
    waiter$show()
    
    ## Hide the "data" sheet  and "macro data" in the spreadsheet ####
    for (url in rv$site_sheets) {
      ssid <- as_dribble(url)
      
      sheet_ids <- sheet_properties(ssid) %>%
        filter(index <= 1) %>%
        select(id)
      
      hide_sheet(ssid$id, sheet_ids[[1]][[1]], "hide")
      hide_sheet(ssid$id, sheet_ids[[1]][[2]], "hide")
      
    }
    
    rv$proj_status <- "complete"
    
    drive_deauth()
    gs4_deauth()
    
    waiter$hide()
    
  })
  
  output$proj_status <- renderText({rv$proj_status})
  outputOptions(output, "proj_status", suspendWhenHidden = FALSE)
  
}

server