#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`



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
  
  mutazioni <- listastoricizzata %>%
    dplyr::filter(municipalityEntryMode == 11, 
           municipalityDateOfChange <= data2, 
           municipalityDateOfChange >= data1)
  mutazioni
}

# mutazioni_(comuniTI, data1, data2)
###############################################################################

###############################################################################
mutazioni_assorbenti_ <- function(mutazioni){
  municipalityAbolitionDate <- NULL
  
  mutazioni %>%
    dplyr::filter(is.na(municipalityAbolitionDate)) #%>%
  #     select(municipalityId, municipalityShortName, municipalityAdmissionNumber,
  #            municipalityAdmissionMode, municipalityAbolitionMode)
}

mutazioni_assorbiti_ <- function(mutazioni){
  municipalityAbolitionDate <- NULL
  
  mutazioni %>%
    dplyr::filter(!is.na(municipalityAbolitionDate)) #%>%
  #     select(municipalityId, municipalityShortName, municipalityAbolitionNumber,
  #            municipalityAdmissionMode, municipalityAbolitionMode, municipalityAdmissionDate)
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
  tmp1 <- m2 %>% merge(m1 %>% dplyr::select(-municipalityAbolitionNumber), 
                       by.x = "municipalityAbolitionNumber",
                       by.y = "municipalityAdmissionNumber")
  # municipalityId.x (codice "vecchio")
  # municipalityId.y (codice "nuovo", armonizzato)
  # municipalityLongName.x ("vecchio" nome)
  # municipalityLongName.y ("nuovo" nome)
  # sel_vars <- c("municipalityId.x", "municipalityId.y", "municipalityLongName.x", "municipalityLongName.y")
  sel_vars <- c("municipalityId.x", "municipalityId.y")
  municipalityId.x <- NULL
  municipalityId.y <- NULL
  bfs_new <- NULL
  
  tmp1 <- tmp1 %>% dplyr::select_(.dots = sel_vars) %>%
    dplyr::rename(municipalityId = municipalityId.x, bfs_new = municipalityId.y)
  
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
    
    mancanti_ <- m %>% dplyr::filter(municipalityAbolitionNumber %in% mancanti)
    colonne <- c("municipalityId", "municipalityAbolitionNumber", "municipalityAbolitionMode")
    nuovi_ <- mancanti_ %>% dplyr::select_(.dots = colonne) %>% 
      dplyr::filter(municipalityAbolitionMode == 26) %>%
      dplyr::rename(bfs_new = municipalityId)
    mancanti_ <- mancanti_ %>% 
      dplyr::left_join(nuovi_ %>% dplyr::select(-municipalityAbolitionMode), 
                by = "municipalityAbolitionNumber") %>%
      dplyr::select(municipalityId, bfs_new)
  }
  
  if(is.null(mancanti_)){
    res <- tmp1
  }else{
    res <- dplyr::bind_rows(tmp1, mancanti_)
  }
  
  res <- res %>% dplyr::distinct()
  
  # Codici dei comuni che non cambiano
  non_cambiano <- comuni1$municipalityId %>% 
    setdiff(res$municipalityId)
  non_cambiano_df <- dplyr::data_frame(municipalityId = non_cambiano, 
                                bfs_new = non_cambiano)
  
  # Metto assieme i comuni che cambiano con quelli che non cambiano
  res2 <- dplyr::bind_rows(res, non_cambiano_df)
  
  # E faccio il join con la lista dei comuni alla data1 (comuni1)
  # In questo modo questa lista avrà la colonna aggiuntiva bfs_new, con i 
  # codici che i comuni avranno alla data2
  comuni1 <- comuni1 %>% dplyr::left_join(res2, by = "municipalityId")
  comuni1 %>% dplyr::arrange(municipalityId)
}

# data1 <- "2004-12-31"
# # data1 <- "2015-12-31"
# data2 <- "2016-04-10"
# prova <- armonizza(comuniTI, data1, data2)
# # View(prova)
# prova
# length(prova$municipalityId)
# length(unique(prova$municipalityId))
