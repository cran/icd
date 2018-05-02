## ----setup, include=FALSE------------------------------------------------

######## DEBUGGING ONLY ############
# see https://stackoverflow.com/questions/49308254/how-do-i-debug-a-segfault-which-only-occurs-during-vignette-building?noredirect=1#comment85618697_49308254
# writeLines(.libPaths(), "/tmp/libpaths.txt")

suppressWarnings({
  suppressPackageStartupMessages({
    loadNamespace("knitr") # for opts_chunk only
    library(icd)
    library(magrittr)
    })
  })

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "README-"
)

## ----vermont-charlson----------------------------------------------------
# typical hospital format data, with many columns for diagnoses
head(vermont_dx)
# convert to long format (could use other tools, but the icd version accounts better for known structure of the data.
head(vermont_dx %>% wide_to_long)
# calculate charlson scores and summarize
vermont_dx %>% wide_to_long %>% charlson %>% summary
# show the first few actual scores: the names are the patient IDs:
vermont_dx %>% wide_to_long %>% charlson %>% head(25) -> vermont_charlson
vermont_charlson
names(vermont_charlson)
unname(vermont_charlson)

