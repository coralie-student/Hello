#' Hello World
#'
#' `hello` says _"Hello, world!"_ in chosen language
#' @param who specifies the name of the person to whom the “hello”
#'  message is addressed. This must be a character vector of length 1.
#' @param lang character vector, length 1,
#'  specifies the user preferred language. This can be “EN” (the default value)
#'  for English, “FR” for French, “IT” for Italian, “ES” for Spanish,
#'  or “DE” for German. Upper and lower cases should be interpreted as identical.
#' @param langData an optional data.frame with two columns.
#'  The first column gives the language codes and the second column gives
#'  the corresponding “hello” word.
#'  The default value of LangData is set to the dataset language.
#'  see `?language`
#'
#'
#'
#' @examples
#' hello("james")
#' hello("amelia","Es")
#'
#' @return a `character` vector with a personalized "hello" message
#'
#' @export
#' @importFrom stringr str_c

hello <- function(who, lang="en", langData=Hello::language) {
  if(!is.character(who)){
    str_c("Error in hello(", who,"): Please enter a valid name; see ?hello", sep="")
  }else if(! lang %in% langData[[1]]){
    str_c("Sorry, ",who,", your language ('",lang,"') is not available!")
  }
  else{
    lang<-tolower(lang)
    index<-which(tolower(langData[[1]])==lang)
    paste0(index)
    cat(langData[[2]][index], ", ", who, "!", sep="")
  }

}

