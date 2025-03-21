# import packages
library(shiny)
library(googledrive)
library(googlesheets4)
library(dplyr)
library(stringr)
library(waiter)
library(cli)
library(gargle)
library(bslib)
library(fresh)

# when app is closed, verify user is deauthorized
# I don't know if this actually works...
onStop(function() {
  drive_deauth()
})

# set up OAuth client (EDIT FOR DEPLOYMENT?)
drive_auth_configure(path = "./.secrets/client.json")

# set up links for the template files and the banned words
# proj_checklist <- "https://docs.google.com/document/d/1jWs3EtLgnGE_M_aPtOMon9yW6zmc8ZmoPXRSmvyqbeU"
form_template <- "https://docs.google.com/forms/d/1Pd09xnHNOX6Q9JXFQ-L8hUClaeBaTAGU8xkYgTylqwY/edit"
main_template <- "https://docs.google.com/spreadsheets/d/1rlhVZuQGNeazfnfzOLMBozkUBcti78dJmnqxuGXDy4k"
site_template <- "https://docs.google.com/spreadsheets/d/1W5xNg5NlKTC43BantaZmQk-apgA1F9hmXq0zVElK2mg/edit"
no_no <- readRDS("data/illegal_words.rds")


#' Hide a sheet in a Google Spreadsheet
#'
#' @param ssid The Spreadsheet ID (string). Links and dribbles are NOT allowed.
#' @param sheet_id The sheet ID to change. Can be obtained from [googlesheets4::sheet_properties()].
#' @param op The operation to perform, either "hide" (the default) or "show".
#'
#' @return Nothing, actions are performed invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#'   library(googlesheets4)
#'   
#'   url <- "link_to_google_file"
#'   ssid <- as_dribble(url)
#'
#'   sheet_ids <- sheet_properties(ssid) %>%
#'     filter(index == 0) %>%
#'     select(id)
#'
#'   hide_sheet(ssid$id, sheet_ids[[1]][[1]], "hide")
#' }
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

#' Find the data for all charts in a Google Spreadsheet file
#'
#' @param ssid The Spreadsheet ID (string). Links and dribbles are NOT allowed.
#'
#' @return A nested list of chart data as returned from the API.
#' @export
#'
#' @examples
#' \dontrun{
#'   library(googlesheets4)
#'   
#'   url <- "link_to_google_file"
#'   ssid <- as_dribble(url)
#'
#'   find_chart_data(ssid$id)
#' }
find_chart_data <- function(ssid) {
  
  chart_data_req <- request_generate(
    "sheets.spreadsheets.get",
    params = list(
      spreadsheetId = ssid,
      fields = "sheets"
    )
  )
  
  chart_data_raw <- request_make(chart_data_req)
  chart_data_resp <- gargle::response_process(chart_data_raw)
  
  # find the full chart data from the response from the API
  
  chart_data <- list()
  idx <- 1
  
  for (sheet in chart_data_resp$sheets) {
    
    for (chart in sheet$charts) {
      
      info <- chart
      chart_data[[idx]] <- info
      idx <- idx + 1
      
    }
    
  }
  
  return(chart_data)
  
  
}

#' Update the title of a chart in a Google Spreadsheet (internal purposes)
#'
#' @param ssid The Spreadsheet ID (string). Links and dribbles are NOT allowed.
#' @param chart_num The chart number to edit (integer index).
#' @param site_num The site number the data was recorded from.
#'
#' @return Nothing, all actions are performed invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#'   library(googlesheets4)
#'   
#'   url <- "link_to_google_file"
#'   ssid <- as_dribble(url)
#'
#'   update_title(ssid$id, 1, 1)
#' }
update_title <- function(ssid, chart_num, site_num) {
  
  chart_data <- find_chart_data(ssid)
  
  # pull out original spec and update the title
  chart_spec <- chart_data[[chart_num]]$spec
  og_title <- chart_spec$title
  
  new_title <- gsub("Site #", paste("Site", site_num), og_title)
  chart_spec$title <- new_title
  
  # insert updated spec
  chart_data[[chart_num]]$spec <- chart_spec
  
  # build request
  title_req <- list(
    updateChartSpec =
      list(chartId = chart_data[[chart_num]]$chartId,
           spec = chart_data[[chart_num]]$spec)
  )
  
  req <- request_generate(
    "sheets.spreadsheets.batchUpdate",
    params = list(
      spreadsheetId = ssid,
      requests = title_req
    )
  )
  
  # print(req)
  
  resp_raw <- request_make(req)
  response <- response_process(resp_raw)
  
}

#' Update the subtitle of a chart in a Google Spreadsheet (internal purposes)
#'
#' @param ssid The Spreadsheet ID (string). Links and dribbles are NOT allowed.
#' @param chart_num The chart number to edit (integer index).
#' @param wb The name of the waterbody the data was recorded from.
#'
#' @return Nothing, all actions are performed invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#'   library(googlesheets4)
#'   
#'   url <- "link_to_google_file"
#'   ssid <- as_dribble(url)
#'
#'   update_subtitle(ssid$id, 1, "River")
#' }
update_subtitle <- function(ssid, chart_num, wb) {
  
  chart_data <- find_chart_data(ssid)
  
  # pull out original spec and update the title
  chart_spec <- chart_data[[chart_num]]$spec
  og_subtitle <- chart_spec$subtitle
  
  new_subtitle <- gsub("WATERBODY", wb, og_subtitle)
  chart_spec$subtitle <- new_subtitle
  
  # insert updated spec
  chart_data[[chart_num]]$spec <- chart_spec
  
  # build request
  title_req <- list(
    updateChartSpec =
      list(chartId = chart_data[[chart_num]]$chartId,
           spec = chart_data[[chart_num]]$spec)
  )
  
  req <- request_generate(
    "sheets.spreadsheets.batchUpdate",
    params = list(
      spreadsheetId = ssid,
      requests = title_req
    )
  )
  
  # print(req)
  
  resp_raw <- request_make(req)
  response <- response_process(resp_raw)
  
}

form_req_gen <- function(endpoint = character(),
                         params = list(),
                         key = NULL,
                         token = drive_token()) {
  
  # endpoints derived from Discovery Document which was grabbed from ingest-functions.R within gargle
  # a few modifications had to be made to the ingesting file as the Forms API is a little different?
  .endpoints <- readRDS(here::here("form_endpoints.RData"))
  ept <- .endpoints[[endpoint]]
  if (is.null(ept)) {
    print("Could not find your endpoint!")
  }
  
  req <- gargle::request_develop(endpoint = ept, params = params)
  
  gar_req <- gargle::request_build(
    path = req$path,
    method = req$method,
    params = req$params,
    body = req$body,
    token = token
  )
  
  gar_req$url <- gsub("www.", "forms.", gar_req$url)
  gar_req
}

get_form_data <- function(form_id) {
  
  form_data_req <- form_req_gen(
    "forms.forms.get",
    params = list(
      formId = form_id
    )
  )
  
  form_data_raw <- request_make(form_data_req)
  response_process(form_data_raw)
  
}