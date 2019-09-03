#'
#'
#' @export
gt_profile <- function() {
  if (!dir.exists("data")) {
    dir.create("data")
  }


  file_path <-  paste0(getwd(), "/data") %>% str_replace_all("/", "\\\\\\\\")


  fprof  <- makeFirefoxProfile(list(
    browser.download.dir = file_path,
    browser.download.folderList = 2L,
    browser.download.manager.showWhenStarting = FALSE,
    browser.helperApps.alwaysAsk.force = FALSE,
    browser.helperApps.neverAsk.saveToDisk = "text/comma-separated-values, text/csv, application/csv, application/excel, application/vnd.ms-excel, application/vnd.msexcel, text/anytext"))

  return(fprof)
}

#'
#'
#' @export
gt_browser <- function(firefox_profile) {

  port <- sample(4000L:5000L, 1)

  rD <- rsDriver(verbose = FALSE, port = port, browser = "firefox",extraCapabilities = firefox_profile)

  remDr <- rD$client

  return(remDr)
}







#'
#'
#' @export
gt_login <- function(remDr, login, password) {

  url <- "https://www.guidedtrack.com/users/sign_in"
  remDr$navigate(url)

  login_name <- remDr$findElement(using = "css selector", "#user_email")

  login_name$clickElement()

  login_name$sendKeysToElement(list(login))

  login_pw <- remDr$findElement(using = "css selector", "#user_password")

  login_pw$clickElement()

  login_pw$sendKeysToElement(list(password))


  login_button <- remDr$findElement(using = "css selector", "input.btn")

  login_button$clickElement()
}



#'
#'
#' @export
gt_data <- function(remDr, program_id){

  url <- stringr::str_glue("https://www.guidedtrack.com/programs/{program_id}/runs")
  remDr$navigate(url)

  generate_csv_button <- remDr$findElement(using = "css selector", "a.btn-primary:nth-child(1)")

  generate_csv_button$clickElement()

  Sys.sleep(2)

  while_spinning(remDr)

  Sys.sleep(2)

  download_button <- remDr$findElement(using = "css selector", "a.btn-success:nth-child(2)")

  download_button$clickElement()

}

#'
#'
#' @export
while_spinning <- function(remDr) {

  still_spinning <- TRUE

  while(still_spinning) {

    Sys.sleep(2)

    gt_page <- remDr$getPageSource()  %>%
      magrittr::extract2(1) %>%
      xml2::read_html()

    is_it_spinning <- gt_page %>%
      html_nodes("#download-csv") %>%
      html_attr("style")

    still_spinning <- is.na(is_it_spinning)


  }
}


#'
#'
#' @export
gt_get_data <- function(program_id, ...) {
  firefox_profile <- gt_profile()

  remDr <- gt_browser(firefox_profile)

  Sys.sleep(1)

  gt_login(remDr, ...)

  Sys.sleep(1)

  # debugonce(gt_data)

  gt_data(remDr, program_id = program_id)

  cat("Data pull successfull. Closing Browser..")

  remDr$close()

}
