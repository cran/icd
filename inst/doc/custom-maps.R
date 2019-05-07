## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----charlsonexample-----------------------------------------------------
print(icd::icd10_map_quan_deyo, n_comorbidities = 3, n_codes = 8)

## ----obesity-------------------------------------------------------------
library(icd)
obesity_map <- list(
  "1" = "E663",
  "2" = c("E669", "E668", "E6609"),
  "3" = "E661",
  "4" = "E662")

obesity <- data.frame(
  ICD.10.Code = c("E6601", "E663", "E663", "E6609"),
  Encounter.ID = c("408773", "542207", "358741", "342534")
)

custom_map_result <- icd::icd10_comorbid(
  obesity,
  map = obesity_map)

custom_map_result

# finally, format as requested by the user
apply(custom_map_result, 1, function(x) {
  if (!any(x)) 4 else which(x)[1]
  })

# see also:
icd::icd10_map_ahrq$Obesity
icd::icd10_map_quan_elix$Obesity
icd::icd10_comorbid_quan_elix(
  obesity,
  return_df = TRUE)["Obesity"]

## ----sotwo---------------------------------------------------------------
library(icd)
diagnoses <- c("C349", "A219", "B003", "C509", "B700", "A090")
one_pt <- data.frame(id = rep("patient1", length(diagnoses)),
                     diagnoses)
dif_pt <- data.frame(id = paste0("patient", seq_along(diagnoses)),
                     diagnoses)
my_map <- list(c01to17 = expand_range("C01", "C17"),
               a74to75 = expand_range("A748", "A759"),
               b00to33 = expand_range("B001", "B331"),
               b69to72 = expand_range("B69", "B72"),
               c00to94 = expand_range("C000", "C942"))
# optionally use as.comorbidity_map which ensures it is valid, and let's it
# print more pleasantly
my_map <- as.comorbidity_map(my_map)
print(my_map)
icd::comorbid(one_pt, map = my_map)
(six_pts_cmb <- icd::comorbid(dif_pt, map = my_map))

## ----matriximage, fig.width=6, echo = FALSE------------------------------
{
  image(t(six_pts_cmb),
        col = c("light blue", "blue"),
        xlab = "Custom disease ranges",
        axes = FALSE
  )
  axis(1,
       at = seq(0, 1, length.out = length(my_map)),
       labels = names(my_map),
       lwd = 0
  )
  axis(2,
       at = seq(0, 1, length.out = nrow(dif_pt)),
       labels = dif_pt$id,
       lwd = 0,
       las = 2
  )
}

