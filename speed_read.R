library(tidyverse)
library(rvest)

#' speed_read
#'
#' @param url character
#'
#' @return
#' @export
#'
#' @examples
#' read_Rticle(url = "http://www.thespec.com/whatson-story/7317283-john-oliver-on-kidney-dialysis-taco-bell-and-death/")

read_Rticle <- function(url = "") {
  speed_read <- function(text = "") {
    if (text == "") {
      warning("there is no text")
    } else {
      tokens <- stringr::str_split(text , "[[:space:]]")
      print(paste("read time =",
                  round(length(tokens[[1]]) * 0.20 / 60, 2),
                  "minutes"))
      Sys.sleep(2)
      for (token in tokens[[1]]) {
        has_punctuation = grepl("[[:punct:]]", token)
        wait_time = if (has_punctuation)
          0.25
        else
          0.14
        cat(token)
        Sys.sleep(wait_time)
        cat("\014")
      }
    }
  }
  
  url %>% read_html(.) %>%
    html_nodes("p") %>%
    html_text %>%
    map_chr(., gsub, pattern = "\\s+", replacement = " ") %>%
    map_chr(., gsub, pattern = "\n+|\t+", replacement = " ") %>%
    .[map(., nchar) > 1] %>%
    magrittr::extract(2:length(.)) %>%
    paste(collapse = " ") %>%
    speed_read()
}






