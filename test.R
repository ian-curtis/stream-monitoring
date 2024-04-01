library(googlesheets4)
source("chart_ops.R")

ssid <- "1H23wxbY95w_AgB47J2S8jrC6c1LFc3a6qyqCBhxqbTw"

update_title <- function(ssid, chart_num, new_title) {

  chart_data <- find_chart_data(ssid)
  
  # pull out original spec and update the title
  chart_spec <- chart_data[[chart_num]]$spec
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
  response <- gargle::response_process(resp_raw)

}


for (chart_num in seq(1, 8)) {
  
  update_title(ssid, chart_num, "3")
  
}

update_title(ssid, 4, "3")

