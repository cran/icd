## ----setup, include=FALSE-----------------------------------------------------
suppressWarnings({
  suppressPackageStartupMessages({
    loadNamespace("knitr") # for opts_chunk only
    library("icd")
    library("magrittr")
    })
  })
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----uranium-long-------------------------------------------------------------
uranium_pathology[1:10, ]

## ----explain10----------------------------------------------------------------
explain_code("R55")

## ----uranium-wide-------------------------------------------------------------
head(uranium_pathology)

## ----comorbidities------------------------------------------------------------
quan_comorbidities <- comorbid(uranium_pathology, icd10_map_quan_elix)
# see the first few rows and columns:
quan_comorbidities[1:6, c(1, 3:10)]

## ----tidy---------------------------------------------------------------------
comorbid_charlson(uranium_pathology, return_df = TRUE)[1:5, 1:5]

## ----big----------------------------------------------------------------------
# shuffle the rows:
set.seed(1441)
u <- uranium_pathology[sample(seq_len(nrow(uranium_pathology))), ]
head(u)
quan_comorbidities <- comorbid(u,
                               icd10_map_quan_elix,
                               return_df = TRUE,
                               return_binary = TRUE,
                               restore_id_order = FALSE)
# see the first few rows and columns:
quan_comorbidities[1:6, c(1, 3:9)]

## ----cholera------------------------------------------------------------------
# create trivial comorbidity map:
cholera_typhoid_map <- list(cholera = "A00", typhoid = "A01")
patients <- data.frame(patient = c("0001", "0001", "0002"),
                       code = c("A001234567", "A01", "A019"))
comorbid(patients , map = cholera_typhoid_map)

## ----htncx--------------------------------------------------------------------
icd10_map_quan_elix$HTNcx

## ----setuppcs, echo = FALSE---------------------------------------------------
n <- 10
nm <- "code"
set.seed(1441)
pcs_sample <- sample(unname(unlist(icd10_map_ahrq_pcs)), n)
pts <- data.frame(id = sample(LETTERS, n),
                  pc = pcs_sample)
res <- icd10_comorbid(pts,
                      map = icd10_map_ahrq_pcs,
                      icd_name = "pc",
                      return_binary = TRUE)
print(res)
colSums(res)

