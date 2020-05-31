## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  out.width = "100%",
  fig.width = 14,
  fig.height = 10.5
  # global.par = TRUE
)
library("icd")
n <- 10000
set.seed(1441)
# load the dice with more PCCC diagnostic codes
h <- c(
  icd10cm2019$code,
  rep(unlist(unname(icd10_map_pccc_dx)), 10)
)
dat <- data.frame(
  id = n + seq(n),
  icd_dx1 = sample(h, n, replace = TRUE),
  icd_dx2 = sample(h, n, replace = TRUE)
)
# for builds without the ability to download or use cached data, ignore pcs
if (icd:::.exists_in_cache("icd10cm2019_pc")) {
  i <- get_icd10cm2019_pc()$code
  dat <- cbind(dat,
               icd_pcs1 = sample(i, n, replace = TRUE),
               icd_pcs2 = sample(i, n, replace = TRUE)
  )
}

## ----headdat------------------------------------------------------------------
head(dat)

## ----calcpre, eval = TRUE, echo = FALSE---------------------------------------
pccc_dx <- comorbid_pccc_dx(dat)
if (icd:::.exists_in_cache("icd10cm2019_pc")) {
  pccc_pcs <- icd10_comorbid_pccc_pcs(dat,
                                      icd_name = c("icd_pcs1", "icd_pcs2")
  )
  res <- pccc_dx | pccc_pcs
} else {
  res <- pccc_dx
}

## ----calc, eval = FALSE-------------------------------------------------------
#  pccc_dx <- comorbid_pccc_dx(dat)
#  pccc_pcs <- icd10_comorbid_pccc_pcs(dat,
#    icd_name = c("icd_pcs1", "icd_pcs2")
#  )
#  res <- pccc_dx | pccc_pcs

## -----------------------------------------------------------------------------
res[295:300, ]

## ----colsumpccc---------------------------------------------------------------
colSums(res)

## ----par, include = FALSE-----------------------------------------------------
#graphics::par(mar = c(12, 4, 4, 2) + 0.1) # bottom, left, top, right

## ----pcccbarplot, echo=FALSE--------------------------------------------------
graphics::barplot(sort(colSums(res), decreasing = TRUE),
  names.arg = names_pccc,
  ylab = "count",
  las = 2,
  cex.names = 0.75
)

