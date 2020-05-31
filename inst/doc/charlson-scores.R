## ----setup, include=FALSE-----------------------------------------------------
suppressWarnings({
  suppressPackageStartupMessages({
    requireNamespace("knitr")
    library("icd")
    library("magrittr")
    })
  })
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----vermont-charlson---------------------------------------------------------
head(vermont_dx[1:10])
vch <- charlson(vermont_dx)
summary(vch)
head(vch)
head(names(vch))

## ----charlsondf---------------------------------------------------------------
head(charlson(vermont_dx, return_df = TRUE))

## ----vermontvanwalraven-------------------------------------------------------
`Vermont Van Walraven Scores` <- van_walraven(vermont_dx)
hist(`Vermont Van Walraven Scores`)

## ----icd9and10----------------------------------------------------------------
icd9 <- data.frame(pts = c("A", "A"), c("041.04", "244.9"))
icd10 <- data.frame(pts = c("A", "A"), c("C82.28", "M08.979"))
both <- comorbid_elix(icd9) | comorbid_elix(icd10)
van_walraven_from_comorbid(both)

## ----icd9then10---------------------------------------------------------------
icd9 <- data.frame(pts = c("A", "A"), c("041.04", "244.9"))
icd10 <- data.frame(pts = c("B", "B"), c("C82.28", "M08.979"))
both <- rbind(comorbid_elix(icd9), comorbid_elix(icd10))
van_walraven_from_comorbid(both)

