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

## ----pkgdesc, results='asis', echo = FALSE-------------------------------
cat(packageDescription("icd")$Description)

## ----show data formats, echo=TRUE----------------------------------------
# long format ICD-9-CM codes, with present-on-arrival flags
patients_icd9

# long format ICD-10 codes, real mortality data
uranium_pathology[1:5, ]

# wide format, real ICD-9 discharge diagnoses
vermont_dx[1:5, c(1, 6:15)]

## ----getcomorbidities----------------------------------------------------
# use AHRQ revision of Elixhauser comorbidities, show only first eight columns
icd9_comorbid_ahrq(patients_icd9)[, 1:8]

## ----getcomorbidities2---------------------------------------------------
# find Elixhauser comorbidities present-on-arrival
patients_icd9 %>% filter_poa %>% icd9_comorbid_elix %>% head

# same as above, then summarize first five:
patients_icd9 %>% 
  filter_poa %>% 
  icd9_comorbid_elix %>% 
  extract(, 1:5) %>% 
  apply(2, as.integer) %>% 
  summary

# convert vermont discharge data to wide format, 
# find comorbidities, convert TRUE to 1 and show first few
vermont_dx %>% wide_to_long  %>% icd9_comorbid_quan_deyo  %>% apply(2, as.integer) %>%  head

## ----lots of brackets, eval = FALSE--------------------------------------
#  head(apply(icd9_comorbid_quan_deyo(wide_to_long(vermont_dx)), 2, as.integer))

## ----type guessing-------------------------------------------------------
is_valid("100") # valid ICD-9 code
is_valid("A1001") # valid ICD-10 code
is_valid(c("100", "A1001")) # they can't both be valid

## ----set type------------------------------------------------------------
# decimal format ICD-10 codes
codes <- c("A10.01", "L40.50", "Z77.098")
# set class to be icd10cm (and implicitly icd10)
as.icd10cm(codes)
# or set class to indicate decimal code and icd10 (not necessarily icd10cm)
codes %>% as.decimal_diag %>% as.icd10

## ----mixed ICD-9 and ICD-10 data-----------------------------------------
df <- data.frame(i9 = as.icd9(c("100", "001")), 
                 i10 = as.icd10(c("Z771", "Z87820")))

# it is an error to try to convert ICD-9 class codes to ICD-10
# df %>% as.icd9 %>% as.icd10

## ----simple conversion---------------------------------------------------
decimal_to_short(c("1", "10.20", "100", "123.45"))
short_to_decimal(c("1", "22", "2244", "1005"))

# similar operations with magrittr, also showing invalid codes
codes <- as.icd9(c("87.65", "9999", "Aesop", -100, "", NA))
decimal_to_short(codes)

## ----validation----------------------------------------------------------
# guess both ICD version (9, but could be 10?), and decimal vs short form
is_valid("V10.2")

# state we are using short or decimal codes:
is_valid(c("099.17", "-1"), short_code = TRUE)
is_valid(c("099.17", "-1.1"), short_code = FALSE)
is_valid(c("1", "001", "100", "123456", "003.21"), short_code = TRUE)

## ----ranges--------------------------------------------------------------
# get all possible codes
#"003" %i9sa% "0033" %>% head(9) # show first 9 of 111 values
# just get the ones which correspond to diagnoses (keeping the 3-digit chapters)
#"494" %i9s% "4941"

#"10099" %i9sa% "10101"
#"V10" %i9da% "V10.02"
"E987" %i9da% "E988.1"

# can't range between different types:
# "V10" %i9s% "E800" # throws an error

## ----rangeanomaly--------------------------------------------------------
expand_range("4820", "4823") # default, equivalent to %i9s%
expand_range("4820", "4823", defined = FALSE)
# see the first few differences (which are by definition not 'real' codes):
setdiff(expand_range("4820", "4823", defined = FALSE),
        expand_range("4820", "4823")) %>% head

## ----"childrenReal"------------------------------------------------------
children(as.icd9("391"))
# mid-level code
icd:::children.icd9("0032")
# leaf node has no children
# be explicit about the type of code:
test_code <- as.icd9(as.icd_short_diag("00321"))
children(test_code)
# or the same, but guessing the characteristics
children("00321")
# pneumococcal pneumonia is a three-digit ICD-9 code with no descendants
children("481")

## ----all children--------------------------------------------------------
# first ten possible ICD-9 child codes from 391
children("391", defined = FALSE)[1:10]

## ----explain simple------------------------------------------------------
explain("1.0") # 'decimal' format code inferred
explain("0019") # 'short' format code inferred

## ----explain complex-----------------------------------------------------
# we can be explicit about short vs decimal
explain("434.00", short_code = FALSE)
explain(c("43410", "43491"), short_code = TRUE)
#explain top level code with children
"391" %>% explain # single three-digit code
"391" %>% children # let's see the child codes
"391" %>% children %>% explain # children condensed to parent code
"391" %>% children %>% explain(condense = FALSE) # prevent condense

## ----explain arbitrary---------------------------------------------------
explain(list(somecodes = as.icd9(c("001", "391")),
                 morecodes = as.icd9cm(c("001.1", "001.9"))))

## ----cholera-------------------------------------------------------------
explain(list(cholera = "001", rheumatic_heart = "390"))

## ----noexplain, eval = FALSE---------------------------------------------
#  s <- explain("001.5") # gives warning

## ----Example Dementia----------------------------------------------------
length(icd9_map_quan_deyo[["Dementia"]]) # 133 possible ICD-9 codes
length(icd10_map_quan_deyo[["Dementia"]]) # the ICD-10 map is different
# explain summarizes these to just two groups:
icd9_map_quan_deyo[["Dementia"]] %>% explain(warn = FALSE)
# contrast with:
icd9_map_quan_deyo[["Dementia"]] %>% explain(condense = TRUE, warn = FALSE)

## ----Show Range Operator-------------------------------------------------
length("390" %i9da% "392.1")
"390" %i9da% "392.1" %>% explain(warn = FALSE)

## ----Show POA Choices, echo=FALSE----------------------------------------
poa_choices

## ----simplepoa-----------------------------------------------------------
patients_icd9 %>% filter_poa_yes

## ----notnopoa------------------------------------------------------------
patients_icd9 %>% filter_poa_not_no

## ----ahrq----------------------------------------------------------------
#icd9_map_ahrq <- icd:::sas_parse_ahrq() # user doesn't need to do this
names(icd9_map_ahrq)
icd9_map_ahrq$CHF[1:5]
icd10_map_ahrq$CHF[1:5]

## ----elix----------------------------------------------------------------
# the names of the comorbidities in each map are available as named lists:
names_elix[1:5]
unlist(unname(names_elix))
# The map contents have ICD codes with the class set
icd9_map_elix$HTNcx
icd10_map_elix$HTNcx

## ----quan elix-----------------------------------------------------------
names(icd10_map_quan_deyo)
names(icd10_map_quan_elix)

## ----chainpoatocomorbid--------------------------------------------------
patients_icd9 %>%
  filter_poa_not_no %>%
  icd9_comorbid_ahrq %>%
  extract(1:9)

## ----elixvsquanelix------------------------------------------------------
difference <- diff_comorbid(icd9_map_elix, icd9_map_quan_elix,
                            all_names = c("CHF", "PHTN", "HTN", "Valvular"))
# reuslts also returned as data
str(difference)

## ----quanonlyphtn--------------------------------------------------------
difference$PHTN$only.y %>% get_defined %>% explain

## ----cardiacgrep---------------------------------------------------------
icd9cm_hierarchy[
  grepl(pattern = "(heart)|(cardiac)",
        x = c(icd9cm_hierarchy$long_desc, icd9cm_hierarchy$short_desc),
        ignore.case = TRUE),
  "code"] %>% unique -> cardiac

## ----cardiac Chain Explain Example---------------------------------------
as.icd9(cardiac) %>% explain(warn = FALSE) %>% head(10)

## ----speed, eval = FALSE-------------------------------------------------
#  # codes selected from AHRQ mapping
#  many_patients <- icd:::generate_random_pts(1e7)
#  system.time(
#    icd999999999_comorbid_ahrq(many_patients)
#    )[["elapsed"]]

## ----arbitrary Mapping---------------------------------------------------
names(icd9_chapters)[c(1:5, 14)]
my_map <- icd:::icd9_chapters_to_map(icd9_chapters[c(2, 5, 14)])
icd9_comorbid(patients_icd9, my_map) # no positive 

## ----realmapping---------------------------------------------------------
ahrq_strict <- lapply(icd9_map_ahrq, get_defined)
str(icd9_map_ahrq[1:5]) # first five of the original:
str(icd9_map_ahrq[1:5]) # and first five of the result:

## ----"find three digit billable"-----------------------------------------
icd9cm_hierarchy$code %>% get_defined -> all_real
# select the non-V and non-E codes
three_digit_real <- all_real[icd9_is_n(all_real)]
# display
three_digit_df <- data.frame(code = three_digit_real, description = explain(three_digit_real, condense = FALSE))
print(three_digit_df[1:10, ], row.names = FALSE)

## ----"compare ICD-9 versions"--------------------------------------------
new_since_27 <- setdiff(icd9cm_billable[["32"]][["code"]],
                         icd9cm_billable[["27"]][["code"]]) %>% head
lost_since_27 <- setdiff(icd9cm_billable[["27"]][["code"]],
                         icd9cm_billable[["32"]][["code"]]) %>% tail
# we know this is an ICD-9-CM code, so declare this using nice magrittr motif:
lost_since_27 %<>% as.icd9cm
lost_since_27 %<>% as.icd9cm

# these are a few which were gained since v27
data.frame(code = new_since_27, desc = new_since_27 %>% explain)
# these are a few which were lost since v27
data.frame(code = lost_since_27, desc = lost_since_27 %>% explain)

