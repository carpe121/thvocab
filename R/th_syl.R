#' TH Syllables
#' builds syllables around vowels and consonant clusters
#' @param prevlet character of previous letter
#' @param nsyl positive integer number of syllables
#' @param pos string "first" or "subseq" position of letters
#' @examples
#' syl("b", nsyl=1, pos="first")
#' @returns character "syllable" of consonants and vowels
#' @export

syl <- function(prevlet, nsyl=1, pos="first"){
  fir <- prevlet[length(prevlet)]
  
  if(fir %in% th_vowels){                  
    nxlt <- th_vows(fir)          #a(e)-
    
    if(nsyl==1){
      nxlt2 <- sample(th_consonents, 1)         #a(e)d
      nxlt3 <- th_clusts(nxlt2, pos="subseq")   #a(e)d(r)
      int <- c(nxlt, nxlt2, nxlt3)
    } else if(nsyl >= 2){
      vowfollow <- sample(1:3, 1)
      
      if(vowfollow==1){
        int <- nxlt   #-a-
      } else {
        nxlt2 <- sample(th_consonents, 1)    #-a[consonant]
        nxlt3 <- th_clusts(fir, pos="subseq")    #-ad(r)
        int <- c(nxlt, nxlt2, nxlt3)
      }
    }
  } else if (fir %in% th_consonents){
    if(pos=="first"){
      nxlt <- th_clusts(fir, pos="first")
      nxlt2 <- sample(th_vowels, 1)
      nxlt3 <- th_vows(nxlt2)
      int <- c(nxlt, nxlt2, nxlt3)
    } else if(pos=="subseq"){
      nxlt <- th_clusts(fir, pos="subseq")
      nxlt2 <- sample(th_vowels, 1)
      nxlt3 <- th_vows(nxlt2)
      int <- c(nxlt, nxlt2, nxlt3)
    }
  } else if(fir==""){
    nxlt <- sample(th_consonents, 1)         #""d
    nxlt2 <- th_clusts(nxlt, pos="subseq")   #""d(r)
    int <- c(nxlt, nxlt2)
  }
  
  if(any(is.na(int))==TRUE && length(int)==1){
    int <- ""
  } else {
    int <- as.character(na.omit(int))
  }
  
  return(int)
}