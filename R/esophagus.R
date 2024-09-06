#' SAKK Esophagus Cancer Data
#'
#' @description
#' This dataset contains survival data from patients with resectable esophageal carcinoma
#' to compare the outcomes of two treatment regimens: chemoradiation followed by surgery
#' with or without neoadjuvant treatment.
#'
#' @format A data frame with 297 patients and 8 variables:
#'
#' \describe{
#'   \item{\strong{ID}}{Patient Identification.}
#'   \item{\strong{sex}}{Biological sex of the patient.}
#'   \item{\strong{arm}}{Represents the group of patients being followed over time in the study.}
#'   \item{\strong{OS.time}}{Overall survival time in years.}
#'   \item{\strong{OS.event}}{Overall survival status (0 = censored, 1 = event).}
#'   \item{\strong{hist}}{Histological type of the tumor.}
#'   \item{\strong{PFS.time}}{Progression-free survival time in years.}
#'   \item{PFS.event}{Progression-free survival status (0 = censored, 1 = event).}
#' }
#'
#' @source SAKK Competence Center, Switzerland
#'
#' @references
#' Ruhstaller, T., Thuss-Patience, P., Hayoz, S., Schacher, S., Knorrenschild, J. R., Schnider, A., ... & Stahl, M. (2018). Neoadjuvant chemotherapy followed by chemoradiation and surgery with and without cetuximab in patients with resectable esophageal cancer: a randomized, open-label, phase III trial (SAKK 75/08). Annals of oncology, 29(6), 1386-1393.


"esophagus"

