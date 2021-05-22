#require(dplyr)

f_mergevars <- function(datX,datY){
mergevars <- colnames(datX)[colnames(datX) %in% colnames(datY)]
return(mergevars)
}


f_addVar <- function(datX,datY, allX=TRUE){

  nrowB <- dim(datX)[1]
  mergevars <- f_mergevars(datX, datY)

  out <- dplyr::left_join(datX, datY, by = mergevars, all.x = TRUE)

  if(dim(out)[1] !=  nrowB)warning("Number of rows do not match")
  message(paste0("Message: x nrow= ", dim(out)[1]," and y nrow=", dim(datX)[1], "\n Number of variables added: ",dim(out)[2]- dim(datX)[2] ))
  
  
return(out)

}
