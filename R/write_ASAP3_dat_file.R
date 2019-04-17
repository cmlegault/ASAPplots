#' WriteASAP3DatFile
#' 
#' Function to write ASAP 3 dat file (modified from Tim Miller's text_datwrite.R file).
#' @param fname full directory and file name to be created (including .dat suffix)
#' @param dat.object R object containing all the necessary information
#' @param header.text text put run description line in input file
#' @export

WriteASAP3DatFile <- function(fname,dat.object,header.text){
  # fname <- 'bsb.dat'; dat.object <- asap.dat; header.text <- c('Base ASAP run')
  
  # Create full file name
  n = nchar(fname)
  
  # Create file with one comment
  cat('# ASAP VERSION 3.0\n#', header.text, '\n', file=fname, append=FALSE)
  
  # File data and comments
  dat <- dat.object$dat
  comments <- dat.object$comments
  
  # Counter for comments
  comment.ct <- 0    
  
  for (i in 1:length(dat)){
    x <- dat[[i]]
    
    if(data.class(x)=='numeric' | data.class(x)=='integer'){
      comment.ct <- comment.ct + 1
      cat(comments[comment.ct],'\n', sep='', file=fname, append=T)
      cat(x, '\n', file=fname, append=T) 
    }  # end of numeric/integer if statement
    
    if(data.class(x)=='matrix'){
      comment.ct <- comment.ct + 1
      cat(comments[comment.ct],'\n',file=fname,append=T)
      write.table(x,col=F,row=F,quote=T, file=fname,append=T)
    }  # end of matrix if statement
    
    if(data.class(x)=='list'){
      for (j in 1:length(x)){
        comment.ct <- comment.ct + 1
        cat(comments[comment.ct],'\n',file=fname,append=T)
        write.table(x[[j]],col=F,row=F,quote=T, file=fname,append=T)
      }  # end of 'j' for loop
    }  # end of list if statement
    
  }  # End of "i" for loop
  
  # Add fleet and survey names
  cat('######\n###### FINIS ######\n# Fleet Names\n',file=fname,append=T)
  write.table(as.matrix(paste('#$',dat.object$fleet.names,sep='')),col=F,row=F,quote=F, file=fname,append=T)
  cat('# Survey Names\n',file=fname,append=T)
  write.table(as.matrix(paste('#$',dat.object$survey.names,sep='')),col=F,row=F,quote=F, file=fname,append=T)
  cat('#\n',file=fname,append=T)
  
  return()
} # End of function      


