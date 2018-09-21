#' docxASAP
#' 
#' Create a Word document of the figures from ASAPplots with predefined captions.
#' @param wd directory where ASAP run is located
#' @param asap.name Base name of original dat file (without the .dat extension)
#' @param od output directory for plots and csv files (default = NULL means wd\\plots\\)
#' @param docx.name name of Word document to create (default = docxASAP.docx)
#' @param control.file csv file of parameters defining plots to add to docx (default = docxASAP.csv)
#' @param first.figure.number starting value for figures, succesive figures based on Order
#' @param figure.prefix text to add before figure number, e.g., "B" becomes Figure B1. (default = "")
#' @param plotf type of plot to save (default = 'png')
#' @param use.group only add figures from this Group to docx (default = NULL means all groups added)
#' @param append.asap.name.caption flag to add (TRUE = default) asap.name to end of figure caption text
#' @export

docxASAP <- function(wd,asap.name,od=NULL,docx.name="docxASAP.docx",control.file="docxASAP.csv",first.figure.number=1,figure.prefix="",plotf='png',use.group=NULL,append.asap.name.caption=TRUE){
  
  # uses package "officer" by name, uses package "dplyr" without naming (need to use library(dplyr))
  
  # if od not input, set equal to default used in ASAPplots
  if (is.null(od)) od <- paste0(wd, "\\plots\\")
  
  # read the control file
  # a csv file containing the following named columns
  # Include "Yes" means figures from that row will be included in docx (if Group selected)
  #  note: can also use "YES", "yes", "Y", "y", or "TRUE" to include figure, anything else for not 
  # Order integer defining the sequence of plots, docx will number included figures accordingly
  # Number overrides succesive numbering to apply this value to figures for this row
  # Group classifier that allows easy subsetting of plots, e.g., use.group=Retro only adds retro figs
  # ASAPplot name of saved plot file in dir od from ASAPplots
  #   note: if multiple fleets or indices will add all in order with "Figure #. continued" as legend
  # FigureText text for the figure caption, period will be added at end of text
  con.file.ini <- read.csv(paste0(wd, "\\", control.file), header = TRUE, stringsAsFactors = FALSE)
  
  # check for correct file
  if (!all(names(con.file.ini) %in% c("Include", "Order", "Number", "Group", "ASAPplot", "FigureText"))){
    print("Problem with control.file, required column names not all present")
    return()
  }
  
  # only figures in use.group 
  if (is.null(use.group)){
    con.file <- con.file.ini  # use all rows in csv file
  }else{
    con.file <- con.file.ini %>%
      filter(Group %in% use.group)
  }
  
  # filter for Included figures in selected Group
  con.file <- con.file %>%
    filter(Include %in% c("Yes", "YES", "yes", "Y", "y", "TRUE"))
  if (length(con.file$Include) == 0){
    print("No figures to be included in Word doc")
    print("Check Include column of control.file for use.group Group")
    return()
  }
  
  # create thisfig for figure caption using first.figure.number and Number column overwrite
  # note: Number overwrite does not fill holes in Order, so be careful using only some Number values
  # best to either define Number for all Included figures or else leave whole Number column blank 
  con.file <- con.file %>%
    arrange(Order) %>%
    mutate(ifig = first.figure.number + as.numeric(row.names(.)) - 1) %>%
    mutate(thisfig = ifelse(is.na(Number), ifig, Number)) %>%
    arrange(thisfig)
  
  # how many figures to add to Word doc
  nplots <- length(con.file$thisfig)
  
  # start the docx file
  my_doc <- officer::read_docx()
  
  # loop through all the figures
  for (iplot in 1:nplots){
    
    myfile <- paste0(od, con.file$ASAPplot[iplot], ".", plotf)
    thisfig <- con.file$thisfig[iplot]
    
    # form figure caption
    mycaption <- paste0("Figure ", figure.prefix, thisfig, ". ", con.file$FigureText[iplot])
    if (append.asap.name.caption == TRUE){
      mycaption <- paste0(mycaption, " ", asap.name, ".")  
    }else{
      mycaption <- paste0(mycaption, ".")
    }
    
    # add myfile if only single file
    if(file.exists(myfile)){
      my_doc <- my_doc %>%
        officer::body_add_img(src=myfile, width = 6.5, height = 6.5, style = "centered") %>%
        officer::body_add_par(mycaption, style = "Normal") %>%
        officer::body_add_par("", style = "Normal") %>% # blank line
        officer::body_add_break(pos = "after") # page break
    }else{
      # check for multiple files that start with myfile - be careful of extra files with diff extensions
      gg1 <- list.files(od, pattern = con.file$ASAPplot[iplot])
      ngg1 <- length(gg1)
      if (ngg1 >= 1){
        for (igg in 1:ngg1){
          myfile <- paste0(od, gg1[igg])
          if (igg > 1) mycaption <- paste0("Figure ", figure.prefix, thisfig, ". continued")
          my_doc <- my_doc %>%
            officer::body_add_img(src=myfile, width = 6.5, height = 6.5, style = "centered") %>%
            officer::body_add_par(mycaption, style = "Normal") %>%
            officer::body_add_par("", style = "Normal") %>% # blank line
            officer::body_add_break(pos = "after") # page break
        }
      }
    }
    
  }
  
  # make the docx file
  print(my_doc, target = paste0(od, docx.name))
  return()
}
