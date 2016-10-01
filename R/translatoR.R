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
  txt = utils:::URLencode(text)
  url = paste0("https://translate.googleapis.com/translate_a/single?client=gtx&sl=",
               source,"&tl=",target,"&dt=t&q=",txt)
  res = RCurl:::getURL(url)
  tra = strsplit(res,',')[[1]][1]
  tra = substr(tra,5,nchar(tra)-1)
  return(tra)
  }



