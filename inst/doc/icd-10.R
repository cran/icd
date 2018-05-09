## ----setup, include=FALSE------------------------------------------------

suppressWarnings({
  suppressPackageStartupMessages({
    loadNamespace("knitr") # for opts_chunk only
    library(icd)
    library(magrittr)
    })
  })

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


## ----uranium-long--------------------------------------------------------
uranium_pathology[1:10, ]

## ----uranium-wide--------------------------------------------------------
uranium_pathology %>% long_to_wide %>% head

## ----comorbidities-------------------------------------------------------
quan_comorbidities <- comorbid(uranium_pathology, icd10_map_quan_elix)
# see the first few rows and columns:
quan_comorbidities[1:6, 3:10]

## ----cholera-------------------------------------------------------------
# create trivial comorbidity map:
cholera_typhoid_map <- list(cholera = "A00", typhoid = "A01")
patients <- data.frame(patient = c("0001", "0001", "0002"), code = c("A001234567", "A01", "A019"))
comorbid(patients , map = cholera_typhoid_map)

## ----htncx---------------------------------------------------------------
icd10_map_quan_elix$HTNcx

