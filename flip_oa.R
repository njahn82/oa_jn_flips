#' How many WoS-indexed journals flipped to OA?
library(tidyverse)
#' checking doaj
doaj <- readr::read_csv("https://doaj.org/csv")
doaj %>%
  filter(grepl("Nature Communications", `Journal title`)) %>% View()
#' doaj does contain infos about when the journal was funded, 
#' wos indexing status is also missing, so let's use the Bielefeld Gold OA list instead.
u <- "https://pub.uni-bielefeld.de/download/2934907/2934908/ISSN_Gold-OA_3.0.csv"
isos_jns <- readr::read_csv(u)
wos_doaj_jns <- isos_jns %>%
  filter(ISSN_IN_DOAJ == 1, ISSN_IN_WOS == 1) %>% 
  select(ISSN, ISSN_L, TITLE, TITLE_SOURCE)
#' Let's prepare a doaj look-up table
doaj_lookup <- doaj %>% 
  select(issn_print = `Journal ISSN (print version)`,
         issn_e = `Journal EISSN (online version)`,
         year_flipped = `First calendar year journal provided online Open Access content`,
         title = `Journal title`) %>%
  # gathering issns into one column
  tidyr::gather(issn_print, issn_e, key = "issn_type", value = "issn") %>%
  # remove missing values
  filter(!is.na(issn))
wos_oa_df <- wos_doaj_jns %>% 
  inner_join(doaj_lookup, by = c("ISSN" = "issn")) 
#' next we obtain the start date of these journals using the ZDB. We first define a parsing function:
#' zdb check if journal is still active
#' marc field 362a
library(httr)
library(xml2)
zdb_check <- function(issn = NULL) {
  req <- httr::GET(paste0("http://services.dnb.de/sru/zdb?version=1.1&operation=searchRetrieve&query=iss%3D", issn, "&recordSchema=MARC21plus-1-xml")) %>%
    httr::content()
  tt <- xml2::xml_find_all(req, "//d:records//d:record", ns = c(d = "http://www.loc.gov/zing/srw/"))
  
  period <- xml2::xml_find_first(tt, ".//d1:datafield[@tag='362']//d1:subfield[@code='a']", 
                           ns = c(d1 = "http://www.loc.gov/MARC21/slim")) %>%
    xml_text()
  as_tibble(cbind(period, issn))
}
#' fetch issns
zdb_period <- purrr::map(wos_oa_df$ISSN, purrr::safely(zdb_check))
zdb_period_df <- map_df(zdb_period, "result") %>%
  filter(!is.na(period)) %>%
  mutate(year_active_since_str = stringi::stri_extract_last_regex(period, "\\d{4} -")) %>%
  mutate(year_active_since = str_replace(year_active_since_str, " -", ""))
# merge with wos_oa_df
zdb_oa_df <- wos_oa_df %>%
  left_join(zdb_period_df, by = c("ISSN" = "issn")) 
write_csv(zdb_oa_df, "zdb_doaj_isos_gold_match.csv")
