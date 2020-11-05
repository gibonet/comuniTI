# #' @importFrom magrittr %>%
# #' @export
# magrittr::`%>%`

#' @import data.table


###############################################################################
# Funzione mutazioni_
# mutazioni_ <- function(listastoricizzata, data1, data2){
#   mutazioni <- listastoricizzata %>%
#     filter(municipalityEntryMode == 11, 
#            (municipalityDateOfChange <= data2 & municipalityDateOfChange >= data1) | 
#              (municipalityAdmissionDate <= data2 & municipalityAdmissionDate >= data1))
#   mutazioni
# }
mutazioni_ <- function(listastoricizzata, data1, data2){
  municipalityEntryMode <- NULL
  municipalityDateOfChange <- NULL
  
  listastoricizzata <- as.data.table(listastoricizzata)
  
  mutazioni <- listastoricizzata[
    municipalityEntryMode == 11 &
      municipalityDateOfChange <= data2 &
      municipalityDateOfChange >= data1
  ]
  
  mutazioni
}

# mutazioni_(comuniTI, data1, data2)
###############################################################################

###############################################################################
mutazioni_assorbenti_ <- function(mutazioni){
  municipalityAbolitionDate <- NULL
  mutazioni <- as.data.table(mutazioni)
  
  mutazioni[is.na(municipalityAbolitionDate)]
}

mutazioni_assorbiti_ <- function(mutazioni){
  municipalityAbolitionDate <- NULL
  mutazioni <- as.data.table(mutazioni)
  
  mutazioni[!is.na(municipalityAbolitionDate)]
}
###############################################################################


###############################################################################
# Provo a correggere aggiungi_codici_post_mutazioni() con alcune funzioni

#' @rdname aggiungi_codici_post_mutazioni
#' @export
#' 
#' @description N.B.: \code{armonizza} è una nuova versione di 
#' \code{aggiungi_codici_post_mutazioni}. È quindi meglio utilizzare
#'  \code{armonizza} (anche se è da considerare ancora sperimentale). 
#'  Dovrebbe funzionare con i comuni ticinesi a partire dal 31.12.2004.
#' @examples 
#' 
#' # Carica la lista storicizzata dei Comuni del Ticino
#' data(comuniTI)
#' 
#' c12_13 <- armonizza(comuniTI,
#'                      data1 = "2012-12-31", 
#'                      data2 = "2013-04-14")
#' 
#' dim(c12_13)
#' str(c12_13)
armonizza <- function(listastoricizzata, data1, data2){
  # 0. Creazione della lista dei comuni alla data1
  comuni1 <- comuni_stato(listastoricizzata, data.rif = data1)
  
  # 1. Estrazione della 'lista' di tutte le mutazioni avvenute tra data1 e data2
  m <- mutazioni_(listastoricizzata, data1, data2)
  
  # 2. Separazione di m in 2 (mutazioni_assorbenti_ e mutazioni_assorbiti_)
  m1 <- mutazioni_assorbenti_(m)
  m2 <- mutazioni_assorbiti_(m)
  
  # 3. 
  tmp1 <- merge(m2, m1[ , -c("municipalityAbolitionNumber")], 
                by.x = "municipalityAbolitionNumber",
                by.y = "municipalityAdmissionNumber",
                sort = FALSE
                )

  # municipalityId.x (codice "vecchio")
  # municipalityId.y (codice "nuovo", armonizzato)
  # municipalityLongName.x ("vecchio" nome)
  # municipalityLongName.y ("nuovo" nome)
  # sel_vars <- c("municipalityId.x", "municipalityId.y", "municipalityLongName.x", "municipalityLongName.y")
  municipalityId.x <- NULL
  municipalityId.y <- NULL
  bfs_new <- NULL
  
  tmp1 <- tmp1[ , c("municipalityId.x", "municipalityId.y")]
  setnames(tmp1, old = c("municipalityId.x", "municipalityId.y"),
           new = c("municipalityId", "bfs_new"))

  # 4. Valuto se ci sono "mutazioni mancanti"
  mancanti <- setdiff(
    sort(unique(m2$municipalityAbolitionNumber)),
    sort(unique(m1$municipalityAdmissionNumber))
  ) #
  
  l <- length(mancanti)
  if(l == 0){
    print("Tutto ok...")
    mancanti_ <- NULL
  }else{
    municipalityAbolitionNumber <- NULL
    municipalityAbolitionMode <- NULL
    municipalityId <- NULL
    
    mancanti_ <- m[municipalityAbolitionNumber %in% mancanti]
    nuovi_ <- mancanti_[
      , c("municipalityId", "municipalityAbolitionNumber", "municipalityAbolitionMode")][
        municipalityAbolitionMode == 26]
    setnames(nuovi_, old = "municipalityId", new = "bfs_new")
    mancanti_ <- merge(mancanti_, nuovi_[ , -c("municipalityAbolitionMode")],
                       by = "municipalityAbolitionNumber", all.x = TRUE, sort = FALSE)
    mancanti_ <- mancanti_[ , c("municipalityId", "bfs_new")]
  }
  
  if(is.null(mancanti_)){
    res <- tmp1
  }else{
    res <- rbindlist(list(tmp1, mancanti_))
  }
  
  res <- unique(res)
  # res <- res %>% dplyr::distinct()
  
  # Codici dei comuni che non cambiano
  non_cambiano <- setdiff(comuni1$municipalityId, res$municipalityId)
  non_cambiano_df <- data.table(municipalityId = non_cambiano, 
                                bfs_new = non_cambiano)
  
  # Metto assieme i comuni che cambiano con quelli che non cambiano
  res2 <- rbindlist(list(res, non_cambiano_df))
  # res2 <- dplyr::bind_rows(res, non_cambiano_df)
  
  # E faccio il join con la lista dei comuni alla data1 (comuni1)
  # In questo modo questa lista avrà la colonna aggiuntiva bfs_new, con i 
  # codici che i comuni avranno alla data2
  comuni1 <- merge(comuni1, res2, by = "municipalityId", all.x = TRUE)
  # comuni1 <- comuni1 %>% dplyr::left_join(res2, by = "municipalityId")
  # comuni1 %>% dplyr::arrange(municipalityId)
  # comuni1[order(municipalityId)]
  setorderv(comuni1, cols = "municipalityId")
  comuni1
}

# data1 <- "2004-12-31"
# # data1 <- "2015-12-31"
# data2 <- "2016-04-10"
# prova <- armonizza(comuniTI, data1, data2)
# # View(prova)
# prova
# length(prova$municipalityId)
# length(unique(prova$municipalityId))
