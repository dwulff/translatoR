#' Translate text from source to target language using GoogleTranslate
#' 
#' @param text a string containing the to be translated text.
#' @param source a string specifying the source language, e.g., "en" for English.
#' @param target a string specifying the target language, e.g., "de" for German.
#' 
#' @examples
#' translatoR('Hello world!','en','de')
#' 
#' @export
#' 
#' @author Dirk U. Wulff

translatoR = function(text, source, target){
  characters = c(as.character(0:9),letters,LETTERS)
  collapse   = paste0(' ',paste(sample(characters,100,replace = T),collapse=''),' ')
  if(length(text)>1) text = paste(text,collapse=collapse)
  
  txt = utils:::URLencode(text)
  url = paste0("https://translate.googleapis.com/translate_a/single?client=gtx&sl=",
               source,"&tl=",target,"&dt=t&q=",txt)
  res = RCurl:::getURL(url)
  res = strsplit(res,'\\],\\[')[[1]]
  tra = strsplit(res,'",')
  tra = unlist(lapply(tra,function(x) x[1]))
  tra = gsub('\\[\\[\\[\\"','',tra)
  tra = gsub('\"','',tra)
  tra = paste(tra,collapse='')
  tra = strsplit(tra,collapse)[[1]]
  return(tra)
  }


