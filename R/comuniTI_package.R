
#' Crea la lista dei Comuni ticinesi alla data desiderata
#'
#' @description Partendo dalla lista storicizzata dei Comuni del Ticino, messa a disposizione dall'Ust, questo pacchetto permette di creare la lista dei Comuni ticinesi a una determinata data e anche di collegare due liste che hanno una diversa data di riferimento (questo serve ad esempio a armonizzare dei dati comunali a una lista di Comuni uguale - per creare quindi una serie storica, ad esempio) 
#' 
#' @docType package
#' @name comuniTI
NULL




#' Lista storicizzata dei distretti ticinesi
#' 
#' Data frame con 8 righe e 4 colonne. 
#' 
#' 
#' @details Ogni riga rappresenta un distretto.
#' Per ogni distretto sono disponibili le colonne seguenti:
#' 
#' \itemize{
#'   \item districtHistId numero storicizzato del distretto
#'   \item districtId codice federale del distretto
#'   \item districtLongName nome lungo del distretto
#'   \item districtShortName nome corto del distretto
#' }
#' 
#' @docType data
#' @keywords datasets
#' @name distrettiTI
#' @usage data(distrettiTI)
#' @format Un data frame con 8 righe e 4 colonne
NULL

