require(utils)
require(RCurl)

translatoR = function(text, source, target){
  txt = utils::URLencode(text)
  url = paste0("https://translate.googleapis.com/translate_a/single?client=gtx&sl=",
               source,"&tl=",target,"&dt=t&q=",txt)
  res = RCurl::getURL(url)
  tra = strsplit(res,',')[[1]][1]
  tra = substr(tra,5,nchar(tra)-1)
  return(tra)
  }



