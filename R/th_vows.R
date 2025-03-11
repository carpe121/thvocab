#' TH vowels
#' if a vowel is chosen, decide if standalone vowel, diphthong, or triphthong
#' @param prevlet character of previous letter
#' @examples
#' th_vows("a")
#' @returns character single or cluster of vowels
#' @export

th_vows <- function(prevlet){
  fir <- prevlet[length(prevlet)]
  
  if(fir %in% diphths){
    vowflip <- sample(1:5, 1)
    if(fir=="a"){
      if(vowflip==1){
        nxlt <- "u"					#au
      } else if (vowflip==2){
        nxlt <- "i"				#ai
      } else if(vowflip >= 3){
        nxlt <- NA				#a
      }
    } else if(fir=="i"){
      if(vowflip==1){
        nxlt <- "e"					#ie
      } else if(vowflip==2 | vowflip==3){
        nxlt <- "a"					#ia
      } else if(vowflip >= 4) {
        nxlt <- NA			#i
      }
    } else if(fir=="e"){
      if(vowflip==1 | vowflip==2){
        nxlt <- "i"					#ei
      } else if (vowflip >= 3){
        nxlt <- NA				#e
      }
    } else if(fir=="o"){
      if(vowflip==1){
        nxlt <- "u"				#ou
      } else if (vowflip >= 2){
        nxlt <- NA			#o
      }
    }
  } else {
    nxlt <- NA				#"y" or "u"
  }
  return(nxlt)
}