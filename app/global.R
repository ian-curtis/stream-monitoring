library(shiny)
library(googledrive)
library(googlesheets4)
library(dplyr)
library(stringr)
library(waiter)
library(cli)
library(gargle)

# onStop(function() {
#   drive_deauth()
# })

drive_auth_configure(path = "./secret/oauth_secret_water.json")

form_template <- "https://docs.google.com/forms/d/1TUC63kNrlLhupNcBSbPoQX0tGdCLpx9rSfVX5mFBG1U/edit"
main_template <- "https://docs.google.com/spreadsheets/d/1AmUUw_6sXgMzVGPQrE7ziaeig8p63pdkQSnVggxMfJE/edit"
site_template <- "https://docs.google.com/spreadsheets/d/1k2wjsG1VgqpUxwC124WaWlGSqIlHIYkTzAUwVRh-Uf4/edit"
no_no <- readRDS("illegal_words.RData")

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