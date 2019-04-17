#' ReadASAP3DatFile
#' 
#' Read ASAP 3 dat file into R (thanks to Tim Miller for providing original version).
#' @param datf full path and name of dat file (including .dat suffix)
#' @export

ReadASAP3DatFile <- function(datf){
  
  ### Read in file
  char.lines <- readLines(datf)
    # Each line of text is a different element in char.lines.  
    # So length(char.lines) == number of rows in datf
    # Each line of text is a different element in the resulting object

    
  ### Extract the headers for all data elements
  com.ind <- which(substring(char.lines,1,1) == "#")
    # com.ind = line numbers in char.lines that represent comments
  dat.start <- com.ind[c(which(diff(com.ind)>1), length(com.ind))]
    # dat.start represents the first line number (i.e. the header line) for each data element (M, Fecundity, etc); So the line number corresponds to the heading for the data element, not the beginning of the actual data
    # the last element in dat.start is the line number of the last line in the dat file
    # diff takes the difference (in numbers) between two adjacent data elements; So diff(com.ind) shows the number of rows between comments; When diff > 1, there are data between two comment lines
  comments <- char.lines[dat.start]
    # vector of the headers for all data elements; last element in comments is the last line of the data file (which comprises '#')

   
  ### Create a list of data elements
  dat <- list()
  ind <- 0
  
  dat$n_years <- scan(datf, what = integer(), skip = dat.start[ind <- ind + 1], n = 1)
    # Within the skip fx, n represents the maximum number of data values to be read
    # So for the above scan call, we are skipping 6 lines (dat.start[1] = 6) and reading in 1 integer
  dat$year1 <- scan(datf, what = integer(), skip = dat.start[ind <- ind + 1], n = 1)
  dat$n_ages <- scan(datf, what = integer(), skip = dat.start[ind <- ind + 1], n = 1)
  dat$n_fleets <- scan(datf, what = integer(), skip = dat.start[ind <- ind + 1], n = 1)
  dat$n_fleet_sel_blocks <- scan(datf, what = integer(), skip = dat.start[ind <- ind + 1], n = 1)
    # Number of selectivity blocks (across all fleets)
  dat$n_indices <- scan(datf, what = integer(), skip = dat.start[ind <- ind + 1], n = 1)
  
  dat$M <- matrix(scan(datf, what = double(), skip = dat.start[ind <- ind + 1], n = dat$n_years*dat$n_ages), dat$n_years, dat$n_ages, byrow = TRUE)
  dat$fec_opt <- scan(datf, what = integer(), skip = dat.start[ind <- ind + 1], n = 1)
  dat$fracyr_spawn <- scan(datf, what = double(), skip = dat.start[ind <- ind + 1], n = 1)
  dat$maturity <- matrix(scan(datf, what = double(), skip = dat.start[ind <- ind + 1], n = dat$n_years*dat$n_ages), dat$n_years, dat$n_ages, byrow = TRUE)
  dat$n_WAA_mats <- scan(datf, what = integer(), skip = dat.start[ind <- ind + 1], n = 1)
    # at this point, nind = 11
  dat$WAA_mats <- lapply(1:dat$n_WAA_mats, function(x) matrix(scan(datf, what = double(), skip = dat.start[ind+x], n = dat$n_years*dat$n_ages), dat$n_years, dat$n_ages, byrow = TRUE))
    ind <- ind+dat$n_WAA_mats
    # Updated ind after reading in the WAA matrices;  if there are 3 WAA matrices, nind now = 14

      
  ### Number of WAA Pointers
  npt <- dat$n_fleets * 2 + 2 + 2
  dat$WAA_pointers <- as.matrix(sapply(1:npt, function(x) scan(datf, what = integer(), skip = dat.start[ind+1]+x-1, n = 1)))
    ind <- ind + 1
    # Regardless of # of WAA pointers, nind only increases by one because all WAA pointers are within a single vector

    
  ### Selectivity block assignment and parameters
  dat$sel_block_assign <- lapply(1:dat$n_fleets, function(x) scan(datf, what = integer(), skip = dat.start[ind+x], n = dat$n_years))
    ind <- ind+dat$n_fleets
  ### Selectivity options for each block
  dat$sel_block_option <- scan(datf, what = integer(), skip = dat.start[ind <- ind + 1], n = dat$n_fleet_sel_blocks)
    # ind check
        # print(ind);  print(dat.start[ind]);
  dat$sel_ini <- lapply(1:dat$n_fleet_sel_blocks, function(x) matrix(scan(datf, what = double(), skip = dat.start[ind+x], n = 4*(dat$n_ages+6)), dat$n_ages+6, 4, byrow = TRUE))
    ind <- ind + dat$n_fleet_sel_blocks
  dat$fleet_sel_start_age <- scan(datf, what = integer(), skip = dat.start[ind <- ind + 1], n = dat$n_fleets)
  dat$fleet_sel_end_age <- scan(datf, what = integer(), skip = dat.start[ind <- ind + 1], n = dat$n_fleets)

  dat$Frep_ages <- scan(datf, what = integer(), skip = dat.start[ind <- ind + 1], n = 2)
  dat$Frep_type <- scan(datf, what = integer(), skip = dat.start[ind <- ind + 1], n = 1)
  dat$use_like_const <- scan(datf, what = integer(), skip = dat.start[ind <- ind + 1], n = 1)
  dat$release_mort <- scan(datf, what = double(), skip = dat.start[ind <- ind + 1], n = dat$n_fleets)

    
  ### Catch and discards
  dat$CAA_mats <- lapply(1:dat$n_fleets, function(x) matrix(scan(datf, what = double(), skip = dat.start[ind+x], n = dat$n_years*(dat$n_ages+1)), dat$n_years, dat$n_ages+1, byrow = TRUE))
    ind <- ind + dat$n_fleets
  dat$DAA_mats <- lapply(1:dat$n_fleets, function(x) matrix(scan(datf, what = double(), skip = dat.start[ind+x], n = dat$n_years*(dat$n_ages+1)), dat$n_years, dat$n_ages+1, byrow = TRUE))
    ind <- ind + dat$n_fleets
  dat$prop_rel_mats <- lapply(1:dat$n_fleets, function(x) matrix(scan(datf, what = double(), skip = dat.start[ind+x], n = dat$n_years*(dat$n_ages)), dat$n_years, dat$n_ages, byrow = TRUE))
    ind <- ind + dat$n_fleets

    
  ### Survey index specifications    
  dat$index_units <- scan(datf, what = integer(), skip = dat.start[ind <- ind + 1], n = dat$n_indices)
  dat$index_acomp_units <- scan(datf, what = integer(), skip = dat.start[ind <- ind + 1], n = dat$n_indices)
  dat$index_WAA_pointers <- scan(datf, what = integer(), skip = dat.start[ind <- ind + 1], n = dat$n_indices)
  dat$index_month <- scan(datf, what = double(), skip = dat.start[ind <- ind + 1], n = dat$n_indices)
  dat$index_sel_choice <- scan(datf, what = integer(), skip = dat.start[ind <- ind + 1], n = dat$n_indices)
  dat$index_sel_option <- scan(datf, what = integer(), skip = dat.start[ind <- ind + 1], n = dat$n_indices)
  dat$index_sel_start_age <- scan(datf, what = integer(), skip = dat.start[ind <- ind + 1], n = dat$n_indices)
  dat$index_sel_end_age <- scan(datf, what = integer(), skip = dat.start[ind <- ind + 1], n = dat$n_indices)
  dat$use_index_acomp <- scan(datf, what = integer(), skip = dat.start[ind <- ind + 1], n = dat$n_indices)
  dat$use_index <- scan(datf, what = integer(), skip = dat.start[ind <- ind + 1], n = dat$n_indices)
  
  
  ### Survey index selectivity
  dat$index_sel_ini <- lapply(1:dat$n_indices, function(x) matrix(scan(datf, what = double(), skip = dat.start[ind+x], n = 4*(dat$n_ages+6)), dat$n_ages+6, 4, byrow = TRUE))
    ind <- ind + dat$n_indices

    
  ### Survey index data matrices
    # Columns are: [Year, Value, CV, 1:nage, Sample Size]
  dat$IAA_mats <- lapply(1:dat$n_indices, function(x) matrix(scan(datf, what = double(), skip = dat.start[ind+x], n = dat$n_years*(dat$n_ages+4)), dat$n_years, dat$n_ages+4, byrow = TRUE))
    ind <- ind + dat$n_indices

    
  ### Phases  
  dat$phase_F1 <- scan(datf, what = integer(), skip = dat.start[ind <- ind + 1], n = 1)
  dat$phase_F_devs <- scan(datf, what = integer(), skip = dat.start[ind <- ind + 1], n = 1)
  dat$phase_rec_devs <- scan(datf, what = integer(), skip = dat.start[ind <- ind + 1], n = 1)
  dat$phase_N1_devs <- scan(datf, what = integer(), skip = dat.start[ind <- ind + 1], n = 1)
  dat$phase_q <- scan(datf, what = integer(), skip = dat.start[ind <- ind + 1], n = 1)
  dat$phase_q_devs <- scan(datf, what = integer(), skip = dat.start[ind <- ind + 1], n = 1)
  dat$phase_SR_scalar <- scan(datf, what = integer(), skip = dat.start[ind <- ind + 1], n = 1)
  dat$phase_steepness <- scan(datf, what = integer(), skip = dat.start[ind <- ind + 1], n = 1)

    
  ### CVs and Lambdas
  # .... related to recruitment
  dat$recruit_cv <- as.matrix(scan(datf, what = double(), skip = dat.start[ind <- ind + 1], n = dat$n_years))
  # .... related to catch, discrads and indices
  dat$lambda_index <- scan(datf, what = double(), skip = dat.start[ind <- ind + 1], n = dat$n_indices)
  dat$lambda_catch <- scan(datf, what = double(), skip = dat.start[ind <- ind + 1], n = dat$n_fleets)
  dat$lambda_discard <- scan(datf, what = double(), skip = dat.start[ind <- ind + 1], n = dat$n_fleets)
  dat$catch_cv <- matrix(scan(datf, what = double(), skip = dat.start[ind <- ind + 1], n = dat$n_years*dat$n_fleets), dat$n_years, dat$n_fleets, byrow = TRUE)
  dat$discard_cv <- matrix(scan(datf, what = double(), skip = dat.start[ind <- ind + 1], n = dat$n_years*dat$n_fleets), dat$n_years, dat$n_fleets, byrow = TRUE)
  dat$catch_Neff <- matrix(scan(datf, what = double(), skip = dat.start[ind <- ind + 1], n = dat$n_years*dat$n_fleets), dat$n_years, dat$n_fleets, byrow = TRUE)
  dat$discard_Neff <- matrix(scan(datf, what = double(), skip = dat.start[ind <- ind + 1], n = dat$n_years*dat$n_fleets), dat$n_years, dat$n_fleets, byrow = TRUE)
  # .... related to fishing mortality
  dat$lambda_F1 <- scan(datf, what = double(), skip = dat.start[ind <- ind + 1], n = dat$n_fleets)
  dat$cv_F1 <- scan(datf, what = double(), skip = dat.start[ind <- ind + 1], n = dat$n_fleets)
  dat$lambda_F_devs <- scan(datf, what = double(), skip = dat.start[ind <- ind + 1], n = dat$n_fleets)
  dat$cv_F_devs <- scan(datf, what = double(), skip = dat.start[ind <- ind + 1], n = dat$n_fleets)
  # .... related to abundance (and recruitment again)
  dat$lambda_N1_devs <- scan(datf, what = double(), skip = dat.start[ind <- ind + 1], n = 1)
  dat$cv_N1_devs <- scan(datf, what = double(), skip = dat.start[ind <- ind + 1], n = 1)
  dat$lambda_rec_devs <- scan(datf, what = double(), skip = dat.start[ind <- ind + 1], n = 1)
  # .... related to catchability
  dat$lambda_q <- scan(datf, what = double(), skip = dat.start[ind <- ind + 1], n = dat$n_indices)
  dat$cv_q <- scan(datf, what = double(), skip = dat.start[ind <- ind + 1], n = dat$n_indices)
  dat$lambda_q_devs <- scan(datf, what = double(), skip = dat.start[ind <- ind + 1], n = dat$n_indices)
  dat$cv_q_devs <- scan(datf, what = double(), skip = dat.start[ind <- ind + 1], n = dat$n_indices)
  # .... related to S-R relationship
  dat$lambda_steepness <- scan(datf, what = double(), skip = dat.start[ind <- ind + 1], n = 1)
  dat$cv_steepness <- scan(datf, what = double(), skip = dat.start[ind <- ind + 1], n = 1)
  dat$lambda_SR_scalar <- scan(datf, what = double(), skip = dat.start[ind <- ind + 1], n = 1)
  dat$cv_SR_scalar <- scan(datf, what = double(), skip = dat.start[ind <- ind + 1], n = 1)  

  
  ### Initial guesses
  dat$N1_flag <- scan(datf, what = integer(), skip = dat.start[ind <- ind + 1], n = 1)
    # 1 for devs from exponential decline, 2 for devs from initial guesses
  dat$N1_ini <- scan(datf, what = double(), skip = dat.start[ind <- ind + 1], n = dat$n_ages)
  dat$F1_ini <- scan(datf, what = double(), skip = dat.start[ind <- ind + 1], n = dat$n_fleets)
  dat$q_ini <- scan(datf, what = double(), skip = dat.start[ind <- ind + 1], n = dat$n_indices)
  dat$SR_scalar_type <- scan(datf, what = integer(), skip = dat.start[ind <- ind + 1], n = 1)
    # 1 for R0, 0 for SSB0
  dat$SR_scalar_ini <- scan(datf, what = double(), skip = dat.start[ind <- ind + 1], n = 1)
  dat$steepness_ini <- scan(datf, what = double(), skip = dat.start[ind <- ind + 1], n = 1)
  dat$Fmax <- scan(datf, what = double(), skip = dat.start[ind <- ind + 1], n = 1)
  dat$ignore_guesses <- scan(datf, what = integer(), skip = dat.start[ind <- ind + 1], n = 1)

   
  ### Projections
  dat$do_proj <- scan(datf, what = integer(), skip = dat.start[ind <- ind + 1], n = 1)
  dat$dir_fleet <- scan(datf, what = integer(), skip = dat.start[ind <- ind + 1], n = dat$n_fleets)
  dat$nfinalyear <- scan(datf, what = integer(), skip = dat.start[ind <- ind + 1], n = 1)
  # .... number of years in projections
  n <- (dat$nfinalyear-dat$year1)-dat$n_years+1
  # .... projection data by year (if n>0)  
  if(n>0) { dat$proj_ini <- matrix(scan(datf, what = double(), skip = dat.start[ind <- ind + 1], n = n*5), n, 5, byrow = TRUE) }  else 
      dat$proj_ini <- matrix(nrow = 0, ncol = 5)
    # be careful here, may cause problems writing because no lines when final year of projection=last year in assessment

    
  ### MCMC
  dat$doMCMC <- scan(datf, what = integer(), skip = dat.start[ind <- ind + 1], n = 1)
  dat$MCMC_nyear_opt <- scan(datf, what = integer(), skip = dat.start[ind <- ind + 1], n = 1)
    # 0=output nyear NAA, 1=output nyear+1 NAA
  dat$MCMC_nboot <- scan(datf, what = integer(), skip = dat.start[ind <- ind + 1], n = 1)
  dat$MCMC_nthin <- scan(datf, what = integer(), skip = dat.start[ind <- ind + 1], n = 1)
  dat$MCMC_nseed <- scan(datf, what = integer(), skip = dat.start[ind <- ind + 1], n = 1)
  # .... agepro  
  dat$fill_R_opt <- scan(datf, what = integer(), skip = dat.start[ind <- ind + 1], n = 1)
    # 1=SR, 2=geomean
  dat$R_avg_start <- scan(datf, what = integer(), skip = dat.start[ind <- ind + 1], n = 1)
  dat$R_avg_end <- scan(datf, what = integer(), skip = dat.start[ind <- ind + 1], n = 1)

    
  ### Final loose ends
  dat$make_R_file <- scan(datf, what = integer(), skip = dat.start[ind <- ind + 1], n = 1)
  dat$testval <- scan(datf, what = integer(), skip = dat.start[ind <- ind + 1], n = 1)

  if(dat$testval != -23456)  {stop("dat file not read in correctly because testval does not equal -23456")}
  if(dat$testval == -23456)  {print("Hooray!  Test value at end of file is correct")}

    
  ### Survey and fleet names
  # Beginning lines for fleet and survey names
  fleet.beg.line  <- (1:length(char.lines))[char.lines=='# Fleet Names']
  survey.beg.line <- (1:length(char.lines))[char.lines=='# Survey Names'] 
  # Comments corresponding to fleet and survey names
  fleet.com  <- char.lines[(fleet.beg.line+1) :(fleet.beg.line+dat$n_fleets)]
  survey.com <- char.lines[(survey.beg.line+1):(survey.beg.line+dat$n_indices)]
  # Fleet and survey names
  fleet.names  <- do.call(rbind,strsplit(fleet.com, '$',fixed=TRUE))[,2]
  survey.names <- do.call(rbind,strsplit(survey.com,'$',fixed=TRUE))[,2]
  
    
  ### Final output
  return(list(dat = dat,  comments = comments,  fleet.names = fleet.names,  survey.names = survey.names))

  }

# datf <- c("P:/Mackerel/AssessmentModels/ASAP/ASAP_V3_Check_WAA.DAT");   asap.dat <- ReadASAP3DatFile(datf)


