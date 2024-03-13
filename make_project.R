# Setup ####

library(cli)
library(optparse)
library(googledrive)
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(googlesheets4))

options(googledrive_quiet = TRUE)

form_temp <- "https://docs.google.com/forms/d/1mzmhcEinAQ0RZ7q-taqwGG-AOKTFxAxT4WJsfGvH5LQ"
main_temp <- "https://docs.google.com/spreadsheets/d/1AmUUw_6sXgMzVGPQrE7ziaeig8p63pdkQSnVggxMfJE/edit?usp=drive_link"
site_temp <- "https://docs.google.com/spreadsheets/d/1k2wjsG1VgqpUxwC124WaWlGSqIlHIYkTzAUwVRh-Uf4/edit?usp=drive_link"

cli_h1("Set Up")

cli_alert_info("Collecting arguments.")

# create the allowed flags for the command line
option_list = list(
  make_option(c("--home"), type = "character", default = NULL, 
              help = "drive folder name you want project folder to live in; defaults to 'My Drive'", 
              metavar = "character"),
  make_option(c("-w", "--waterbody"), type = "character", default = "Waterbody Name",
              help = "the water body from which the data was collected", metavar = "character"),
  make_option(c("-n", "--nsites"), type = "integer", default = 1, 
              help="number of sites you are taking data from (default 1)", metavar="character")
)

# make an R object for the values given in the flags on the command line
opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

home <- ifelse(is.null(opt$home), "My Drive", opt$home)

cli_div(theme = list(span.strong = list(color = "orange")))
cli_par()
cli_text("Let's make sure you have the correct settings.")
cli_end()

cli_par()
cli_text("Based on the input, you are creating a stream monitoring project for {.strong {opt$waterbody}} with {.strong {opt$nsites}} site{?s}.")
cli_end()

cli_par()
cli_text("This project will be created on Google Drive, inside of the folder titled {.strong {home}}.")
cli_end()

cli_par()
cli_text("Is this correct? If so, press {.kbd ENTER}. If not, press {.kbd CTRL} + {.kbd C} and then {.kbd ENTER} and try again.")
cli_end()

invisible(scan("stdin", character(), nlines = 1, quiet = TRUE))


cli_alert_info("Connecting to Google Account.")

# connect to Google account (must be done once interactively, then it's cached)
# use `drive_user()` to force (re)authorization
drive_user()
gs4_auth(token = drive_token())

cli_alert_success("Found your Google account!")

cli_alert_info("Searching for your project's home.")

# search for ID for project's parent folder
if (is.null(opt$home)) parent_dir <- NULL else parent_dir <- drive_find(opt$home, type = "folder")

# if (!is.null(parent_dir) & nrow(parent_dir) == 0) {
#   
#   cli_abort("Could not find the folder specified to be the parent directory. Please check your Drive and try the program again.")
#   
# }

cli_alert_success("Found project home!")

#### Project Creation ####
cli_h1("Project Creation")

cli_alert_info("Creating project folder.")

# create folder for the project files
proj_dir <- drive_mkdir("test", path = parent_dir)

cli_alert_info("Creating data entry form. Will request manual edit soon.")

# copy my template files and put into the new folder

# make a copy of the data entry form and create a link to the responses page
form_copy <- drive_cp(form_temp, name = "01 Data Entry Form") %>% 
  drive_mv(path = proj_dir)
form_link <- drive_link(form_copy)
form_resp_link <- paste0(drive_link(form_copy), "#responses")

manual_edit_form <- function() {
  
  cli_div(theme = list(h2 = list(color = "red")))
  cli_h2("Manual Edit Requested")
  cli_alert_warning("Hey...you'll need to create a Sheets file connected to that form I just created.")
  
  cli_par()
  cli_text("Just go to the link below and click on the \"Link to Sheets\" button.")
  cli_end()
  
  cli_par()
  cli_text("You'll want to \"Create a New Sheet\" (and not use an existing sheet).")
  cli_text("{.url {form_resp_link}}")
  cli_end()
  
  cli_par()
  cli_text("Have you created the new Sheet? If so, press {.kbd ENTER}.")
  cli_end()
  
  invisible(scan("stdin", character(), nlines = 1, quiet = TRUE))
  return(invisible(NULL))
}

manual_edit_form()

# check to see if they did it right

while (nrow(drive_ls(proj_dir)) == 1) {
  cli_alert_danger("Could not find the new connected Google Sheet. Please try again.")
  manual_edit_form()
}

cli_alert_success("Found new responses file.")

form_responses <- drive_ls(proj_dir) %>% 
  filter(name == "01 Data Entry Form (Responses)")
form_r_link <- drive_link(form_responses)

sheet_rename(form_r_link, new_name = "Data")
drive_rename(form_r_link, name = "02 Raw Data")

#### Copy Primary Sheet ####

cli_h1("Creating the Primary Datasheet")
cli_alert_info("Copying the primary sheet to your project. Manual edit coming soon!")

main_copy <- drive_cp(main_temp, name = "03 Primary Datasheet") %>%
  drive_mv(path = proj_dir)
main_link <- drive_link(main_copy)

# E. Coli

cli_alert_info("Setting up E. Coli import")

import_ecoli <- paste0("=QUERY(IMPORTRANGE(\"", form_r_link,
                       "\", \"'Data'!L2:P\"), ",
                       "\"select * where Col1 is not null order by Col2\")")

range_write(main_copy,
            data.frame(x = gs4_formula(import_ecoli)),
            sheet = "All E. Coli Data",
            range = "A2",
            col_names = FALSE)

# Macros

cli_alert_info("Setting up Macro import")

import_macro <- paste0("=QUERY(IMPORTRANGE(\"", form_r_link,
                       "\", \"'Data'!Q2:T\"), ",
                       "\"select * where Col1 is not null order by Col2\")")

range_write(main_copy,
            data.frame(x = gs4_formula(import_macro)),
            sheet = "All Macro Data",
            range = "A2",
            col_names = FALSE)

# Stream Chem

cli_alert_info("Setting up Stream Chemistry import")

import_chem <- paste0("=QUERY(IMPORTRANGE(\"", form_r_link,
                       "\", \"'Data'!D2:K\"), ",
                       "\"select * where Col1 is not null order by Col2\")")

range_write(main_copy,
            data.frame(x = gs4_formula(import_chem)),
            sheet = "All Stream Chem Data",
            range = "A2",
            col_names = FALSE)

range_write(main_copy,
            data.frame(x = opt$waterbody),
            sheet = "Sub Data",
            range = "L2",
            col_names = FALSE)


manual_edit_import <- function(link) {
  
  cli_div(theme = list(h2 = list(color = "red")))
  cli_h2("Manual Edit Requested")
  cli_alert_warning("Hey...you'll need to give permission to draw data between Google files.")
  
  cli_par()
  cli_text("Just go to the link below click on the cell that says \"#REF!\".")
  cli_end()
  
  cli_par()
  cli_text("A dialog box should appear. Use the \"Allow Access\" button to give Google permission to read from another file.")
  cli_end()
  
  cli_par()
  cli_text("You should see the \"#REF!\" change to a \"#N/A\".")
  cli_end()
  
  cli_par()
  cli_text("FYI: This is necessary because the sheet linked below is automatically drawing and sorting data from other files created earlier.")
  cli_end()
  
  cli_par()
  cli_text("Here's that file.")
  cli_text("{.url {link}}\n")
  cli_end()
  
  cli_par()
  cli_alert_info("Note: You may not see much in this file and you might see lots of \"#N/A\" at first. That's ok!!")
  cli_end()
  
  cli_par()
  cli_text("Have you approved the connection? If so, press {.kbd ENTER}.")
  cli_end()
  
  invisible(scan("stdin", character(), nlines = 1, quiet = TRUE))
  return(invisible(NULL))
}

manual_edit_import(main_link)

#### Copy Site Sheet(s) ####

cli_h1("Creating the Site-Specific Sheets")

cli_alert("Manual edits will be needed for each site created. Stay tuned.")

# build formula string for IMPORTRANGE to import site data from the primary sheet
import_site_data <- function(primary_link, source_str, range, site_num) {
  
  string <- paste0("=QUERY(IMPORTRANGE(\"", primary_link, 
                   "\", \"'", source_str, "'!", range, "\"), \"select * where Col2 = 'Site ", site_num, "'\", 1)")
  
  return(string)
  
}

for (site in seq(opt$nsites)) {
  
  cli_h2(paste("Setting up Site", site))
  
  site_copy <- drive_cp(site_temp, name = paste0("04-", site, " Site ", site, " Sheet")) %>%
    drive_mv(path = proj_dir)
  site_link <- drive_link(site_copy)
  
  # site e. coli data
  range_write(site_copy,
              data.frame(x = gs4_formula(import_site_data(main_link, "All E. Coli Data", "A:F", site))),
              sheet = "Data",
              range = "A1",
              col_names = FALSE)
  
  # site macro data
  range_write(site_copy,
              data.frame(x = gs4_formula(import_site_data(main_link, "All Macro Data", "A:F", site))),
              sheet = "Data",
              range = "K1",
              col_names = FALSE)
  
  # site stream chem data
  range_write(site_copy,
              data.frame(x = gs4_formula(import_site_data(main_link, "All Stream Chem Data", "A:H", site))),
              sheet = "Data",
              range = "S1",
              col_names = FALSE)
  
  # site macro data
  range_write(site_copy,
              data.frame(x = gs4_formula(paste0("=IMPORTRANGE(\"", main_link, "\", \"'Sub Data'!D:J\")"))),
              sheet = "Data",
              range = "AB1",
              col_names = FALSE)
  
  manual_edit_import(site_link)
  
}


cli_alert_success("All site sheets created.")
cli_alert_success("The new project has been created.")


