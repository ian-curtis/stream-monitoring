library(googlesheets4)
source("chart_ops.R")

ssid <- "19j1tq8gGNSYKLDqL1b1sRhFpOuUT1wBNcIZnVpN74Sk"

# chart_ids <- find_chart_ids(ssid)
# chart_data <- find_chart_data(ssid)



# next: extract the title and update it based on the site #
# then post it to the sheet
# future: explore other customizations

update_title <- function(ssid, chart_num, site_num, new_title) {
  
  chart_ids <- find_chart_ids(ssid)
  chart_data <- find_chart_data(ssid)
  
  # pull out original spec
  chart_spec <- chart_data[[chart_num]]$spec
  print(chart_spec$title)
  
  # # build new title
  # title <- chart_spec$title
  # new_title <- gsub("#", site_num, title)
  # new_title <- gsub("Waterbody", wb, new_title)
  
  chart_spec$title <- new_title
  chart_spec$basicChart$domains[[1]]$domain$sourceRange$sources[[1]]$sheetId <- 0
  chart_spec$basicChart$series[[1]]$series$sourceRange$sources[[1]]$sheetId <- 0
  print(chart_spec$title)
  
  # insert updated spec
  # chart_data[[chart_num]]$spec <- chart_spec
  
  
  title_req <- list(
    updateChartSpec =
      list(chartId = chart_ids[chart_num],
           spec = chart_spec)
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
  print(resp_raw)
  response <- gargle::response_process(resp_raw)

}