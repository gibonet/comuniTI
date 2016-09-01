## Funzioni in questo script:
## comuni_stato: partendo dalla lista storicizzata dei Comuni svizzeri, estrae la lista dei Comuni a una determinata data (stato)
## aggiungi_codici_post_mutazioni: partendo dalla lista storicizzata dei Comuni svizzeri, da una data 'iniziale' (data1, data prima delle mutazioni) e da una data 'finale' (data2, data dopo mutazioni), questa funzione crea una lista dei Comuni alla data iniziale, aggiungendo una colonna di codici UST che i Comuni avranno alla data finale (bfs_new).


#' Estrae la lista dei Comuni a una determinata data,
#' partendo dalla lista storicizzata dei Comuni ticinesi.
#' 
#' Estrae la lista dei Comuni a una determinata data, partendo dalla lista storicizzata dei Comuni ticinesi (e da una data di riferimento).
#' 
#' @usage comuni_stato(listastoricizzata, data.rif = "2012-12-31")
#' 
#' @param listastoricizzata Un data frame con la lista storicizzata dei Comuni ticinesi
#' @param data.rif di riferimento dello stato dei Comuni desiderato
#' 
#' @return Un data frame con la lista (univoca) dei Comuni a una determinata data
#' 
#' @references \url{http://www.bfs.admin.ch/bfs/portal/it/index/infothek/nomenklaturen/blank/blank/gem_liste/02.html}
#' 
#' @author Gibo
#' @seealso \code{\link{aggiungi_codici_post_mutazioni}}
#' 
#' @examples 
#' # Carica la lista storicizzata dei Comuni del Ticino
#' data(comuniTI)
#' # Elenco dei Comuni del Ticino al 31 dicembre 2012
#' c12 <- comuni_stato(comuniTI, data.rif="2012-12-31")
#' str(c12)
#' @export
comuni_stato <- function(listastoricizzata, data.rif="2012-12-31"){
  ## Questa funzione estrae dalla lista storicizzata dei Comuni svizzeri la lista dei Comuni svizzeri a una certa data (stato) (per il Ticino basta prendere la lista storicizzata solo del Ticino)
  indici <- with(listastoricizzata, municipalityAdmissionDate <= data.rif & (municipalityAbolitionDate >= data.rif | is.na(municipalityAbolitionDate)) & municipalityEntryMode==11)
  res <- listastoricizzata[indici, ]
  res
}

## Esempio:
## comuni_stato(comuniTI, "1980-12-31")


#' Crea la lista dei Comuni ticinesi a una determinata data, aggiungendo i codici dei Comuni di una data successiva (sperimentale, in alcuni casi non funziona).
#' 
#' @description Partendo dalla lista storicizzata dei Comuni ticinesi, estrae l'elenco (univoco) dei Comuni alla data1, aggiungendo una colonna con i codici che i Comuni avranno alla data2 (dopo fusioni, ad esempio)
#' 
#' @usage aggiungi_codici_post_mutazioni(listastoricizzata, 
#'                                       data1 = "2012-12-31", 
#'                                       data2 = "2013-04-14")
#' 
#' @param listastoricizzata Un data frame con la lista storicizzata dei Comuni ticinesi
#' @param data1 Data di riferimento per cui si vuole estrarre la lista dei Comuni ticinesi
#' @param data2 Data successiva a data1, per aggiungere la colonna bfs_new con i codici dei Comuni che avranno alla data2
#' 
#' @return Un data frame con la lista (univoca) dei Comuni ticinesi alla data data1, con l'aggiunta della colonna data2 che indica i codici che i Comuni avranno a quella data (ad esempio dopo fusioni)
#' 
#' @references \url{http://www.bfs.admin.ch/bfs/portal/it/index/infothek/nomenklaturen/blank/blank/gem_liste/02.html}
#' 
#' @author Gibo
#' 
#' @seealso \code{\link{comuni_stato}}
#' 
#' @examples 
#' # Carica la lista storicizzata dei Comuni del Ticino
#' data(comuniTI)
#' 
#' c12_13 <- aggiungi_codici_post_mutazioni(comuniTI, 
#'                                          data1 = "2012-12-31", 
#'                                          data2 = "2013-04-14")
#' 
#' dim(c12_13)
#' str(c12_13)
#' @export
aggiungi_codici_post_mutazioni <- function(listastoricizzata, data1="2012-12-31", data2="2013-04-14"){
  ## Partendo dalla lista storicizzata dei Comuni svizzeri, da una data 'iniziale' (data1, data prima delle mutazioni) e da una data 'finale' (data2, data dopo mutazioni), questa funzione crea una lista dei Comuni alla data iniziale, aggiungendo una colonna di codici UST che i Comuni avranno alla data finale (bfs_new).
  listastoricizzata <- listastoricizzata[listastoricizzata$municipalityEntryMode==11,]
  comuni1 <- comuni_stato(listastoricizzata, data1)
    
  mutazioni_indici <- with(listastoricizzata, municipalityDateOfChange <= data2 & municipalityDateOfChange >= data1)
  mutazioni <- listastoricizzata[mutazioni_indici,]
  
  mutazioni_assorbenti <- mutazioni[is.na(mutazioni$municipalityAbolitionDate),]
  mutazioni_assorbiti <- mutazioni[!is.na(mutazioni$municipalityAbolitionDate),]
  
  colonne1 <- c("municipalityId", "municipalityAdmissionNumber")
  mutazioni_assorbenti <- mutazioni_assorbenti[colonne1]
  
  colonne2 <- c("municipalityId", "municipalityAbolitionNumber")
  mutazioni_assorbiti <- mutazioni_assorbiti[colonne2]
  
  rm(colonne1, colonne2)
  
  mutazioni_risultato <- merge(mutazioni_assorbiti, mutazioni_assorbenti, by.x="municipalityAbolitionNumber", by.y="municipalityAdmissionNumber")  
  ## municipalityId.x: codice UST del Comune alla data1 (data prima delle fusioni/aggregazioni)
  ## municipalityId.y: codice UST che il Comune avrà alla data2, dopo le mutazioni (fusioni, ecc...)
  
  ## Adesso, bisogna assegnare a comuniTI_2012 i codici UST che i Comuni 'avranno' in comuniTI_last
  colonne <- c("municipalityId", "municipalityShortName")
  temp <- merge(comuni1[colonne], mutazioni_risultato, by.x="municipalityId", by.y="municipalityId.x", all.x=TRUE)
#   temp$municipalityId - sort(comuni1$municipalityId)
  
  ## Ordino in base al codice UST del Comune, in modo crescente (municipalityId)
  comuni1 <- comuni1[order(comuni1$municipalityId),]
  temp$municipalityId.y[is.na(temp$municipalityId.y)] <- temp$municipalityId[is.na(temp$municipalityId.y)]
  
  comuni1$bfs_new <- temp$municipalityId.y
#   length(comuni1$bfs_new)
#   length(unique(comuni1$bfs_new))
  comuni1  
}

## Esempio:
## temp <- aggiungi_codici_post_mutazioni(listastoricizzata=comuniTI)
## str(temp)
## length(temp$bfs_new)
## length(unique(temp$bfs_new))
  
  
  
  