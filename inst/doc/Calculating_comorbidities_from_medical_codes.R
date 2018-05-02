## ----setup, echo = FALSE, cache = FALSE----------------------------------
suppressWarnings({
  suppressPackageStartupMessages({
    loadNamespace("knitr") # for opts_chunk only
    library(icd)
    library(magrittr)
    library(utils)
    })
  })

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "README-"
)

patients_icd9 <- data.frame(
  visit_id = c(1000, 1000, 1000, 1000, 1001, 1001, 1002),
  icd9 = as.icd9(c("40201", "2258", "7208", "25001", "34400", "4011", "4011")),
  poa = c("Y", NA, "N", "Y", "X", "Y", "E"),
  stringsAsFactors = FALSE
  )

## ----ahrq----------------------------------------------------------------
#icd9_map_ahrq <- icd:::sas_parse_ahrq() # user doesn't need to do this
names(icd9_map_ahrq)
icd9_map_ahrq$CHF[1:5]
icd10_map_ahrq$CHF[1:5]

## ----quan elix-----------------------------------------------------------
names(icd10_map_quan_deyo)
names(icd10_map_quan_elix)

