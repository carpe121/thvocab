#' TH Consonant maker
#' constructs permitted consonant clusters
#' @param prevlet character of previous letter
#' @param pos string "first" or "subseq" position of letters
#' @examples
#' th_clusts("b", pos="first")
#' @returns single or cluster of letter characters
#' @export

th_clusts <- function(prevlet, pos="first"){
  conroller <- sample(1:5, 1)
  
  fir <- prevlet[length(prevlet)]
  
  if(fir=="b"){
    if(conroller >= 3){
      nxlt2 <- "r"			#(-)br(-)
      if(pos=="first"){
        broller <- sample(1:3,1)
        
        if(broller==1){
          clust <- c(nxlt2, "s")		#brs-
        } else {
          clust <- c(nxlt2)
        }
        
      } else {
        clust <- c(nxlt2)
      }
    } else{
      clust <- NA			#(-)b
    }
  } else if (fir=="d"){
    if(conroller >= 3){
      nxlt2 <- sample(d_nexts, 1)			#dh-, dr-
      if(nxlt2=="r" && pos=="first"){
        droller <- sample(1:3,1)
        
        if(droller==1){
          clust <- c(nxlt2, "s")				#drs-
        } else {
          clust <- c(nxlt2)			#dr
        }
        
      } else {					#dh-, dr-
        clust <- c(nxlt2)
      }
    } else {
      clust <- NA						#d-
    }
    
  } else if (fir=="g"){
    if(conroller >= 4){
      clust <- sample(g_nexts, 1)		#gl-, gh-
    } else {
      clust <- NA						#g-
    }
    
  } else if (fir=="l"){
    if(pos=="subseq"){
      if(conroller >= 4){
        clust <- sample(l_nexts, 1)		#-lk(-), -gh-
      } else {
        clust <- NA						#l-
      }	
    } else if(pos=="first"){
      clust <- NA
    }
    
  } else if (fir=="n"){
    if(pos=="subseq"){
      if(conroller >= 4){
        clust <- sample(n_nexts, 1)		#-nd(-), -ng-
      } else {
        clust <- NA						#-n-
      }	
    } else if(pos=="first"){
      clust <- NA								#-n-
    }
    
  } else if (fir=="r"){
    if(pos=="subseq"){
      if(conroller >= 3){
        clust <- sample(r_nexts, 1)		#-rd-, -rk-
      } else {
        clust <- NA						#r-
      }	
    } else if(pos=="first"){
      clust <- NA							#r
    }
    
  } else if (fir=="s"){
    if(conroller >= 2){
      clust <- sample(s_nexts, 1)		#sm-, -sh-, etc.
    } else {
      clust <- NA						#s-
    }
  } else if (fir=="t"){
    if(conroller >= 4){
      clust <- sample(t_nexts, 1)		#ts-, -th-, etc.
    } else {
      clust <- NA						#t-
    }
  } else {
    clust <- NA
  }
  return(clust)
}