#' TH word maker
#' brings other TH functions to construct final word
#' @param nsyl integer number of syllables
#' @examples
#' x <- for(i in 1:50){ print(thannoun(nsyl=2)) }
#' @returns character "word" of chosen syllabic length
#' @export

thannoun <- function(nsyl=1){
  nsyl <- as.numeric(nsyl)
  fir <- sample(th_initials, 1)
  
  if(nsyl==1){
    int <- syl(fir, nsyl, pos="first")
    wrd <- c(fir, int)
    finwrd <- paste(wrd, collapse='')
    return(finwrd)
  } else if (nsyl >= 2) {
    int <- syl(fir, nsyl, pos="first")
    
    y <- vector("list", length=as.numeric(nsyl))
    y[[1]] <- int 
    
    for(i in 2:(as.numeric(nsyl))){
      mid_n <- syl(y[[i-1]], nsyl, pos="subseq")
      y[[i]] <- mid_n
    }
    
    vec <- unlist(y)
    wrd <- c(fir, vec)
    finwrd <- paste(wrd, collapse='')
    return(finwrd)
  }
}