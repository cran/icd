## ----setup, echo=FALSE, warning=FALSE, cache=FALSE-----------------------
# https://www.jstatsoft.org/pages/view/authors
# https://www.jstatsoft.org/pages/view/style
suppressPackageStartupMessages({
  requireNamespace("knitr")
  library(icd.data, warn.conflicts = FALSE)
  library(icd, warn.conflicts = FALSE)
})
fig.width = 6.5
knitr::opts_chunk$set(fig.width = fig.width)
knitr::opts_chunk$set(fig.height = fig.width / ((1 + sqrt(5)) / 2))
knitr::opts_knit$set(concordance = TRUE) # rstudio setting broken

kable_caption_bottom <- function(x) {
  y <- unlist(strsplit(as.character(x), '\\n'))
  cap_line <- grep("caption", y)
  cap <- y[cap_line]
  last <- y[length(y)]
  y <- y[-c(cap_line, length(y))]
  y <- c(y, cap, last)
  y <- paste(y, sep = "\\n")
  class(y) <- "knitr_kable"
  attr(y, "format") <- "latex"
  y
}

my_kable <- function(x, col.names, caption, ...)
  kable_caption_bottom(
    knitr::kable(x, 
                 col.names = sprintf("\\textbf{%s}", col.names),
                 escape = FALSE,
                 format = "latex",
                 longtable = TRUE,
                 caption = caption, 
                 ...))

## ----longtail, eval=TRUE, echo=FALSE, fig.cap="Frequency distribution of ICD codes in 5,000 pediatric hospital inpatients showing nearly 4,000 unique ICD codes; 1,401 appear only once, making them and the many other low-frequency codes of low or negative value in inter-group comparisons."----
h <- inverse.rle(structure(list(
  lengths = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 2, 1, 1, 1, 1, 1, 3, 1, 2, 1, 1, 1, 4, 2, 1, 1, 2, 2, 4, 4, 2, 1, 1, 2, 1, 1, 1, 3, 1, 2, 1, 4, 1, 2, 4, 4, 3, 2, 3, 7, 5, 2, 10, 4, 4, 4, 5, 6, 6, 1, 8, 13, 9, 11, 10, 13, 13, 14, 10, 16, 23, 18, 24, 27, 32, 39, 45, 33, 51, 87, 86, 76, 126, 166, 240, 365, 632, 1401),
  values = c(525, 405, 296, 283, 281, 268, 227, 209, 193, 180, 168, 166, 156, 146, 144, 135, 134, 131, 120, 116, 115, 113, 110, 106, 103, 90, 88, 86, 85, 82, 81, 80, 78, 77, 76, 75, 73, 72, 71, 70, 69, 68, 67, 64, 63, 62, 61, 56, 55, 54, 53, 51, 50, 49, 48, 46, 45, 44, 43, 41, 40, 39, 38, 37, 36, 35, 34, 33, 32, 31, 30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
), class = "rle"))
  plot(h, type = "h", 
       xlab = "ICD code, by descending frequency", 
       ylab = "count", 
       xlim = c(0, 4000),
       log = "y")

## ----terminology, echo=FALSE, eval=TRUE, results='asis'------------------
termdf <- as.data.frame(matrix(byrow = TRUE, ncol = 2,
                      data = c(
                        "\\textit{comorbidity}", "broad category of disease, \\textit{e.g.}, Cancer",
                        "\\textit{comorbidity map}", "set of comorbidities, each defined by diagnostic codes",
                        "\\textit{patient-visit}", "record identifier, unique for each encounter with a patient, but could represent a patient at one moment, or a summation of all conditions a patient has ever had"
                      )))
my_kable(termdf, col.names = c("Term", "Description"),
         align = c("l", "p{10 cm}"),
         caption = "Terminology")

## ----suckedintoengine----------------------------------------------------
library(icd)
explain_code("V97.33XD")

## ----exampleicdcodewho---------------------------------------------------
explain_code(c("S62", "S62.6"))

## ----exampleicdcodecm----------------------------------------------------
explain_code(c("S62.60", "S62.607", "S62.607S"))

## ----examplecodechap-----------------------------------------------------
icd10_sub_chapters$`Injuries To The Wrist, Hand And Fingers`
icd10_chapters$`Injury, poisoning and certain other consequences of external causes`

## ----charlsonnames-------------------------------------------------------
names(icd10_map_charlson)

## ----firstfewcharlson----------------------------------------------------
icd10_map_charlson[1:2]

## ----comorbidmatrix, echo=FALSE, fig.cap="These are visualizations of some complete maps, black representing the appearance of a particular ICD code in a comorbidity column"----
cmbimage <- function(map, xlab, ...) {
  code <- unique(unlist(map))
  x <- comorbid(map = map, x = data.frame(ptid = seq_along(code), code))
  mode(x) <- "integer"
  image(t(x[, ]), ylim = c(1, 0), 
        col = c("#FFFFFF", "#000000"), 
        xaxt = 'n', yaxt = 'n', 
        useRaster = TRUE, ...)
  mtext(text = xlab, side = 1, line = 1.5, outer = FALSE)
}
{
  par(mfcol = c(1, 3), cex = 1, mar = c(2, 2, 2, 1) + 0.1, oma = rep(0.5, 4))
  # defaults: c(5, 4, 4, 2) + 0.1, oma = c(0, 0, 0, 0))
  cmbimage(icd9_map_ahrq, xlab = "ICD-9 AHRQ\ncomorbidities")
  mtext(text = "ICD codes", side = 2, line = 0.5, outer = FALSE)
  cmbimage(icd10_map_ahrq, xlab = "ICD-10 AHRQ\ncomorbidities")
  cmbimage(icd9_map_multi_ccs[["lvl1"]], xlab = "ICD-9 CCS\ncategories")
}

## ----simplemap, eval = FALSE---------------------------------------------
#  list(
#    "Rheumatic Heart Disease" = "I098",
#    "Hypertension" = c("I10", "I11"),
#    "Heart failure" = c("I50", "I110")
#  )

## ----workedexinput, echo=FALSE, results="asis"---------------------------
workedexinput <- matrix(ncol = 4,
                        data = c(
                          paste("Encounter", c("one", "two", "three", "four")),
                          "K401", "I0981", "M352", "I110",
                          "", "C450", "I10", "H40001",
                          "", "", "", "I10"))
my_kable(workedexinput, 
         col.names = c("Patient-Visit", sprintf("Code %i", 1:3)),
         #align = c("l", "p{10 cm}"),
         caption = "Four patient-visits with some ICD-10 codes in `wide' format for worked example")

## ----cmbresult, echo=FALSE, results="asis"-------------------------------
workedexinput <- matrix(ncol = 4,
                        data = c(
                          paste("Encounter", c("one", "two", "three", "four")),
                          "", "yes", "", "",
                          "", "", "yes", "yes",
                          "", "", "", "yes"
                          ))
my_kable(workedexinput, 
         col.names = c("Patient-Visit", "Rheum", "HTN", "CHF"),
         caption = "Output of the worked example using ICD-10 codes. `Rheum' is Rheumatic Disease, `HTN' is hypertension, `CHF' is Congestive Heart Failure.")

## ----vermonthead---------------------------------------------------------
head(vermont_dx[1:10])

## ----vermontlong---------------------------------------------------------
v <- wide_to_long(vermont_dx)[c("visit_id", "icd_code")]
head(v)

## ----vermontcompute------------------------------------------------------
v_cmb <- comorbid_charlson(v, return_df = TRUE)

## ----vermontresult, echo=FALSE-------------------------------------------
print(head(v_cmb), row.names = FALSE)

## ----vermontimage, echo=FALSE, fig.cap="This visualization of the result of the comorbidity calculation shows a black cell for each positive comorbidity in one thousand patients from Vermont, USA."----
comorbid_matrix <- comorbid_ahrq(v)
{
#  par(mar = par("mar") + 0)
  image(comorbid_matrix, 
        col = c("#FFFFFF", "#000000"),
        xaxt = 'n', yaxt = 'n')
        #useRaster = TRUE)
  mtext(text = "Comorbidity", side = 2, line = 1)
  mtext(text = "Patient-visit", side = 1, line = 1)
}

## ----bench, echo=FALSE---------------------------------------------------
fac <- 1e-3
res <- data.frame(
  datarows = 10^(0:7),
  icd = fac * c(
    3.152695, 4.477749, 4.648492, 6.255048,
    17.4577, 111.1337, 1035.634, 10862.5),
  comorbidity = fac * c(
    22.528121, 22.638468, 32.976028, 127.175669,
    1002.1865, 10354.9292, 35392.493, 402769.8),
  medicalrisk = fac * c(
    3.965962, 3.752622, 11.69087, 77.603945,
    734.2927, 8355.8409, 83382.279, 1935343))
# fit a line to the last four of 'icd' column and predict duration for one
# hundred million patient-visits with twenty comorbidities per patient
fit_res <- log10(res[5:8, ])
icd_model <- lm(icd ~ datarows, data = fit_res)
cmb_model <- lm(comorbidity ~ datarows, data = fit_res)
mdr_model <- lm(medicalrisk ~ datarows, data = fit_res)
pred_hours <- function(model)
  10 ^ predict(model, data.frame(datarows = log10(20 * 10e8))) / 3600
preds <- vapply(list(icd_model, cmb_model, mdr_model),
                FUN = pred_hours, FUN.VALUE = numeric(1))
names(preds) <- c("icd", "comorbidity", "medicalrisk")
xseq = seq(0, 7)
yseq = seq(-3, 3, 3)
logxaxis <- sapply(paste("expression(10^", xseq, ")", sep = ""),
                   function(x) eval(parse(text = x)))
logyaxis <- sapply(paste("expression(10^", yseq, ")", sep = ""),
                   function(x) eval(parse(text = x)))
colours <- c(comorbidity = 'darkred', icd = 'black', medicalrisk = 'darkblue')
# https://stackoverflow.com/questions/40938561/plot-new-has-not-been-called-yet-error-in-rmarkdown-rstudio-1-0-44#41947860
resratio <- data.frame(datarows = res$datarows,
                       icd = 1,
                       comorbidity = res$comorbidity / res$icd,
                       medicalrisk = res$medicalrisk / res$icd)

## ----versus, echo=FALSE, fig.cap="Performance comparison of comorbidity packages up to 10,000,000 rows, with 500,000 patient-visits and 20 comorbidities per visit. Models are fitted where the log-log relationship becomes linear, where rows > 1,000. Using an eight core 3.40GHz CPU, R 3.4.4 using Linux, kernel 4.15. \\pkg{comorbidity} was run with and without parallel option, and the best result was chosen for each number of iterations."----
{
  plot(NA, NA, log = "xy",
       type = "l", col = 'darkred',
       xlab = "rows of data",
       ylab = "seconds",
       xlim = c(1, max(res$datarows)),
       ylim = c(fac, max(c(res$medicalrisk, res$comorbidity))),
       xaxt = "n", yaxt = "n"
  )  
  axis(1, 10^xseq, logxaxis)
  axis(2, 10^yseq, logyaxis)
  lines(x = res$datarows, y = res$comorbidity, col = colours["comorbidity"])
  lines(x = res$datarows, y = res$icd, col = colours["icd"])
  lines(x = res$datarows, y = res$medicalrisk, col = colours["medicalrisk"])
  abline(icd_model, col = colours["icd"], lty = 3)
  abline(cmb_model, col = colours["comorbidity"], lty = 3)
  abline(mdr_model, col = colours["medicalrisk"], lty = 3)
  legend(1, 1000, legend = names(res[-1]), fill = colours[names(res[-1])])
}

## ----speedup, echo=FALSE, fig.width=6, fig.height=3.7, fig.cap="Relative speed-up using icd compared to the alternatives, using the same numbers of patient-visits and comorbidities in Figure \\ref{fig:versus}."----
{
  plot(NA, NA, log = "x",
       type = "l", col = 'darkred',
       xlab = "rows of data",
       ylab = "time / time using 'icd'",
       xlim = c(1, max(resratio$datarows)),
       ylim = c(1, max(c(resratio$medicalrisk, resratio$comorbidity))),
       xaxt = "n"
  )
  axis(1, 10^xseq, logxaxis)
  lines(x = resratio$datarows, y = resratio$icd, col = colours["icd"])
  lines(x = resratio$datarows, y = resratio$comorbidity, col = colours["comorbidity"])
  lines(x = resratio$datarows, y = resratio$medicalrisk, col = colours["medicalrisk"])
  legend(1, 170, legend = names(resratio[-1]), fill = colours[names(resratio[-1])])
}

## ----bigdata, echo=FALSE, fig.cap="Predicted duration of computation for one hundred-million patient-visits, with twenty patients per patient"----
{
  barplot(preds,
          col = colours[c("icd", "comorbidity", "medicalrisk")],
          ylab = "hours", log = "y", ylim = c(1, 5000))
  legend(x = 0.2, y = 2500, legend = names(preds), fill = colours[names(preds)])
}


## ----pulmonaryproblem----------------------------------------------------
sc <- c("Chronic Obstructive Pulmonary Disease And Allied Conditions",
        "Pneumoconioses And Other Lung Diseases Due To External Agents")
icd9_sub_chapters[sc]

## ----permute497----------------------------------------------------------
explain_code(c("947", "749", "794", "479"), warn = FALSE)

## ----charlson497---------------------------------------------------------
"497" %in% icd9_map_charlson

## ----neighbor497---------------------------------------------------------
"49699" %in% icd9_map_quan_deyo[["Pulmonary"]]
"496999" %in% icd9_map_charlson

## ----moregenerous--------------------------------------------------------
alice <- data.frame(id = "alice", icd9 = "49699")
comorbid_charlson(alice, return_df = TRUE)[["Pulmonary"]]

## ----elixrange-----------------------------------------------------------
head("243" %i9da% "244.2")

"244" %in% ("243" %i9da% "244.2")

## ----validity------------------------------------------------------------
is_valid(c("441", "441.0", "441.01", "XXX"))
is_billable(c("441", "441.0", "441.01", "XXX"))
head(
  data.frame(code = children("441"),
             billable = is_billable(children("441"))))

## ----hierarchy441--------------------------------------------------------
children("441")

## ----explain4410---------------------------------------------------------
explain_code(children("4410"))

## ----explaineach4410-----------------------------------------------------
explain_code(children("4410"), condense = FALSE)

