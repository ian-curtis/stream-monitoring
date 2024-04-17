# Define server logic ####
server <- function(input, output, session) {
  waiter_hide()
  rv <- reactiveValues(results = NULL)
  
  # record the name of the user and send a greeting ####
  observeEvent(input$auth, {
    
    # drive_auth(email = FALSE, cache = here::here("app/.cache"))
    drive_auth(email = FALSE, cache = FALSE)
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
                "! If this is correct, please enter the number of sites you are measuring from."))
  })
  
  output$wb_ready <- renderText({rv$wb_ready})
  outputOptions(output, "wb_ready", suspendWhenHidden = FALSE)

  # record the number of sites and send a confirmation ####
  nsites <- eventReactive(input$n_entered, {
    as.integer(input$n_sites)
  })
  # output$confirm_sites <- renderText({
  #   HTML(paste0("Cool! Looks like you want to have data for ", 
  #               "<span style=\"color: #099392\"><b>", pluralize("{nsites()} site{?s}."), "</b></span> ",
  #               "If that's correct, use the box below to find a location for your project. Type in the name of an existing Google Drive folder you want your project to live in. If you'd like your project to live in your Drive's home page (\"My Drive\"), type nothing and just hit \"Next\". FYI: The project itself will appear as a folder with files within. Note: search terms are CASE SENSITIVE."
  #   ))
  # })
  
  output$confirm_sites <- renderText({
    HTML(paste0("Cool! Looks like you want to have data for ", 
                              "<span style=\"color: #099392\"><b>", 
                pluralize("{nsites()} site{?s}."), 
                "</b></span> "))
  })
  
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
    
    checklist_copy <- drive_cp(proj_checklist, name = "00 Project Checklist") %>% 
      drive_mv(path = proj_dir)
    
    form_copy <- drive_cp(form_template, name = "01 Data Entry Form") %>% 
      drive_mv(path = proj_dir)
    form_link <- drive_link(form_copy)
    form_resp_link <- paste0(drive_link(form_copy), "#responses")
    
    form_data <- get_form_data(form_copy$id)
    
    #### Edit questions that have you choose a site number ####
    # need to edit [[5]], [[14]], [[20]]
    questions <- data$items
    
    for (q_num in c(5, 14, 20)) {
      
      question <- questions[[q_num]]
      
      item_id <- question$itemId
      qid <- question$questionItem$question$questionId
      
      new_options <- list()
      
      for (i in seq(nsites())) {
        
        new_options[[i]] <- list(value = paste("Site", i))
        
      }
      
      question$questionItem$question$choiceQuestion$options <- new_options
      
      item_update_req <- form_req_gen(
        "forms.forms.batchUpdate",
        params = list(
          formId = form_copy$id,
          includeFormInResponse = "False",
          requests = list(updateItem = list(item = question,
                                            location = list(index = q_num - 1),
                                            updateMask = "questionItem.question.choiceQuestion.options")),
          writeControl = list(targetRevisionId = data$revisionId)
          
        )
      )
      
      item_update_raw <- request_make(item_update_req)
      response_process(item_update_raw)
      
    }
    
    rv$wb <- input$wb
    rv$n_sites <- as.integer(input$n_sites)
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
      a(href = rv$main_link, "Link to the Primary Datasheet (opens in new window)", target = "_blank"),
      "Have you approved the connection? If so, press the \"Continue\" button.",
      sep = "<br><br>"
    ))
    
  })
  
  ### VERIFY THE CONNECTIONS WERE DONE CORRECTLY
  
  ### Create the site sheets (and ask for manual input) ####
  
  import_site_data <- function(primary_link, source_str, range, site_num) {
    
    string <- paste0("=QUERY(IMPORTRANGE(\"", primary_link, 
                     "\", \"'", source_str, "'!", range, "\"), \"select * where Col2 = 'Site ", site_num, "'\", 1)")
    
    return(string)
    
  }
  
  observeEvent(input$primary_linked, {
    
    site_sheets <- list()
    
    for (site in seq(rv$n_sites)) {
      
      waiter <- waiter::Waiter$new(html = div(
        spin_loaders(10),
        paste0("Setting up Site Sheet ", site, " of ", rv$n_sites, "...")))
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

    ## Hide the "data" sheet in the spreadsheet ####
    for (url in rv$site_sheets) {
      ssid <- as_dribble(url)
      
      sheet_ids <- sheet_properties(ssid) %>%
        filter(index == 0) %>%
        select(id)
  
      hide_sheet(ssid$id, sheet_ids[[1]][[1]], "hide")

    }
    
    rv$proj_status <- "complete"
    
    waiter$hide()
    
  })
  
  output$proj_status <- renderText({rv$proj_status})
  outputOptions(output, "proj_status", suspendWhenHidden = FALSE)
  
}

server