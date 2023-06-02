#' BSBI Demo Dataset
#'
#' This DataFrame is based on a subset of the Botanical Society of the British Isles
#' taxon list for March 2022. It only contains rows flagged as native and species,
#' subspecies or variety rank - a total of 2954 rows.
#'
#' THIS IS UNRELIABLE DATA! It may have been changed for illustrative purposes and
#' should not be used in real analyses. Please visit the source for a clean version.
#'
#' @format ## `BSBI_UK_native_sp_ssp_var_2022`
#' A data frame with 2,954 rows and 16 columns:
#' \describe{
#'   \item{taxon name}{The name of the taxon without the authors}
#'   \item{authority}{The authors of the taxon name}
#' }
#' @source <https://bsbi.org/taxon-lists>
"BSBI_UK_native_sp_ssp_var_2022"

#' Belgian Demo Dataset
#'
#' This DataFrame is based on a subset of the Belgian flora published in 2011 and
#' downloaded from GBIF. It only contains rows flagged as Magnoliopsida and species,
#' subspecies or variety rank - a total of 1535 rows.
#'
#' THIS IS UNRELIABLE DATA! It may have been changed for illustrative purposes and
#' should not be used in real analyses. Please visit the source for a clean version.
#'
#' @format ## `Belgian_Magnoliopsida_sp_ssp_var_2011`
#' A data frame with 1,535 rows and 23 columns:
#' \describe{
#'   \item{scientific_name}{The name of the taxon without the authors}
#'   \item{authorship}{The authors of the taxon name}
#' }
#' @source <https://www.gbif.org/dataset/39653f3e-8d6b-4a94-a202-859359c164c5>
"Belgian_Magnoliopsida_sp_ssp_var_2011"
