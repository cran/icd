% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datadocs.R
\docType{data}
\name{icd9_chapters}
\alias{icd9_chapters}
\alias{icd9_majors}
\title{ICD-9 chapters}
\source{
\url{http://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/codes.html}
}
\description{
\code{icd9_chapters}, \code{icd9_chapters_sub} and \code{icd9_majors} contain
mappings from the higher level descriptions of ICD-9 codes to the ranges of
ICD-9 codes they describe. Helpful in summarizing codes or grouping for
human-readable output. These can easily be converted to a co-morbidity
mapping, as shown in the vignette.
}
\details{
\itemize{ \item 001-139 Infectious And Parasitic Diseases \item 140-239
Neoplasms \item 240-279 Endocrine, Nutritional And Metabolic Diseases, And
Immunity Disorders \item 280-289 Diseases Of The Blood And Blood-Forming
Organs \item 290-319 Mental Disorders \item 320-389 Diseases Of The Nervous
System And Sense Organs \item 390-459 Diseases Of The Circulatory System
\item 460-519 Diseases Of The Respiratory System \item 520-579 Diseases Of
The Digestive System \item 580-629 Diseases Of The Genitourinary System \item
630-679 Complications Of Pregnancy, Childbirth, And The Puerperium \item
680-709 Diseases Of The Skin And Subcutaneous Tissue \item 710-739 Diseases
Of The Musculoskeletal System And Connective Tissue \item 740-759 Congenital
Anomalies \item 760-779 Certain Conditions Originating In The Perinatal
Period \item 780-799 Symptoms, Signs, And Ill-Defined Conditions \item
800-999 Injury And Poisoning \item V01-V91 Supplementary Classification Of
Factors Influencing Health Status And Contact With Health Services \item
E000-E999 Supplementary Classification Of External Causes Of Injury And
Poisoning }

Format: list with chapter/sub-chapter or major names stored in list
  names, each with two element named character vector with start and end
  codes.
}
\keyword{category}
\keyword{datasets}
\keyword{list}
