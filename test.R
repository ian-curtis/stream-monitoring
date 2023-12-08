library(googlesheets4)

# edp <- gs4_endpoints()

endpoint <- "sheets.spreadsheets.batchUpdate"

title_req <- list(
  updateChartSpec = list(spec = list(title = "TEST TEST"))
)

req <- request_generate(
  endpoint,
  params = list(
    spreadsheetId = "1prsIe09ABeURDynrTEEbgXnWM7ksdRYY3iN_CdKm3c4",
    requests = title_req
  )
)

resp_raw <- request_make(req)
gargle::response_process(resp_raw)