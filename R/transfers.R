# Project: R function to create sequence variables to group episodes of care with
#          transfers into a single admission in NSW Admitted Patient Data
#          Collection (APDC)
# Adapted from: 'transfers.R' created by Timothy Dobbins, available here:
#               https://github.com/timothydobbins/hospital-transfers-multipackage
# Changes: converting code to tidyR
#          creating an alternative function that mimics the SAS method
#          (which produces slightly different results to the R method)
# Created by: James Hedley
# Date created: 19th July 2021
# Last updated: 26th July 2021


# Create a function to add group transfers into the same admission ----
transfers <- function (data=data,
                       id=id,
                       admdate=admdate,
                       sepdate=sepdate,
                       mode=mode,
                       transfer_modes=c('5','9'),
                       transfer=transfer,
                       episode=episode,
                       fileseq=fileseq,
                       stayseq=stayseq,
                       transseq=transseq,
                       admdate_first=admdate_first,
                       sepdate_last=sepdate_last,
                       totlos=totlos) {

  # Inputs:
  # id - an exisitng variable containing patient ID number
  # admdate - an existing variable containing admission date
  # sepdate - an existing variable containing separation date
  # mode - an existing variable indicating the mode of separation for each episode
  # transfer_modes - which modes of separation should be counted as transfers?
  # transfer - a new indicator variable for whether an episode is a transfer (0=not a transfer, 1=transfer)
  # episode - a new variable that will count episodes for each patient ID, counting from 1,2,3, etc.
  # fileseq - a new variable that will count rows in the entire dataset, counting from 1,2,3 etc.
  # stayseq - a new variable that will group episodes into larger admissions, counting from 1,2,3, etc.
  # transseq - a new variable that will count episodes within a larger admission, counting from 0,1,2 etc.
  # admdate_first - a new variable that will contain the earliest admission date for the larger admission
  # sepdate_last - a new variable that will contain the latest separation date for the larger admission
  # totlos - a new variable that will contain the total length of stay for the larger admission (less than 1 day = 1 day)

  # Load required libraries
  library('tidyr')
  library('dplyr')
  library('lubridate')

  # Enquote input variable names
  id <- enquo(id)
  admdate <- enquo(admdate)
  sepdate <- enquo(sepdate)
  mode <- enquo(mode)
  transfer <- enquo(transfer)
  episode <- enquo(episode)
  fileseq <- enquo(fileseq)
  stayseq <- enquo(stayseq)
  transseq <- enquo(transseq)
  admdate_first <- enquo(admdate_first)
  sepdate_last <- enquo(sepdate_last)
  totlos <- enquo(totlos)


  # Create a temporary dataset to apply changes to
  temp <- data %>%
    select({{id}},{{admdate}},{{sepdate}},{{mode}})
  
  # Rename each variable using a default name if a different name is used in the dataset
  colnames(temp) <- c('id','admdate','sepdate','mode')

  # Sort data by ID, start date, and end date
  temp <- temp %>%
    arrange(id)
  
  # Create a fileseq variable
  temp <- temp %>%
    mutate(fileseq = row_number())


  # Create in indicator variable for whether an episode is a transfer
  temp <- temp %>%
    mutate(transfer=1*(mode %in% transfer_modes))


  # Create a variable for episode number for each patient
  temp <- temp %>%
    group_by(id) %>%
    mutate(episode = row_number()) %>%
    ungroup()


  # Create temporary variables for admission and separation dates
  temp <- temp %>%
    mutate(adm_tmp = admdate,
           sep_tmp = sepdate)


  # Duplicate each observation into two observations, one for admission and one for separation
  temp <- temp %>%
    pivot_longer(cols=c('adm_tmp','sep_tmp'), # Reshape to long format, with one row for admission, and one for separation
                 names_to='date_type', # Create a variable 'date_type' to indicate if observation is an admission or a separation
                 values_to='date') # Create a variable 'date' with the admission or separation date


  # Create a variable for adjusted date, to sort observations by the order of events
  # If an admission and separation occur on the same date, the admission occurs first
  temp <- temp %>%
    mutate(date_adj = as.numeric(date)
           +0.1*(date_type=="sep_tmp" & transfer==1)) # Add 0.1 to date_adj if observation is a separation
                                                      # from a transfer, to ensure it occurs after admissions


  # Create a numeric variable to indicate whether an observation is an admission or separation
  # This variable 'inout' will be +1 for admissions, and -1 for separations
  temp <- temp %>%
    mutate(inout=+1                   # inout = +1 if date_type is an admission,
           -2*(date_type=="sep_tmp")) # otherwise +1-2= -1 if date_type is a separation


  # Sort the data by ID, adjusted date, episode, and date type (admissions before separations)
  temp <- temp %>% arrange(id, date_adj, episode, -inout)


  # Create a variable to indicate whether an observation (episode) is part of a larger admission
  # For each patient, if the cumulative sum of 'inout' drops below 1, the admission has ended
  temp <- temp %>%
    group_by(id) %>%
    mutate(cumsum=cumsum(inout)) %>%
    ungroup()


  # Create a variable to indicate whether an observation (episode) is the first in the admission
  # An episode is the first in the admission if the cumulative sum of 'inout' is 1, and the
  # episode is the first for the patient or the cumulative sum of 'inout' for the previous episode
  # was 0 (i.e. the previous admission has ended)
  temp <- temp %>%
    group_by(id) %>%
    mutate(newstay = 1*(cumsum==1 & (row_number()==1 | lag(cumsum)==0))) %>%
    ungroup()


  # Create a variable to indicate which larger admission each observation (episode) belongs to
  # An observation (episode) belongs to the same admission as the previous episode if the cumulative sum
  # of 'newstay' is unchange. When 'newstay' increases, the observation is part of a new admission
  temp <- temp %>%
    group_by(id) %>%
    mutate(stayseq=cumsum(newstay)) %>%
    ungroup()


  # Remove duplicate episodes, reshape back to one row per episode (admissions and separations on the same row)
  temp <- temp %>%
    pivot_wider(id_cols=c(id,episode,mode,transfer,stayseq,fileseq),
                names_from='date_type',
                values_from='date')


  # Sort data by ID and stayseq
  temp <- temp %>% arrange(id,episode)


  # Create a variable 'transseq' to count episodes within larger admissions for each ID
  temp <- temp %>%
    group_by(id,stayseq) %>%
    mutate(transseq = row_number()-1) %>% # count starting at 0
    ungroup()


  # Create a variable for the first admission date of each larger admission
  temp <- temp %>%
    group_by(id,stayseq) %>%
    mutate(admdate_first = min(adm_tmp)) %>%
    ungroup()


  # Create a variable for the final separation date of each larger admission
  temp <- temp %>%
    group_by(id,stayseq) %>%
    mutate(sepdate_last = max(sep_tmp)) %>%
    ungroup()


  # Rename temporary admission and separation date variables
  temp <- temp %>%
    rename(admdate = adm_tmp,
           sepdate = sep_tmp)


  # Create a variable for total length of stay
  # If separation is on same day as admission, then length of stay is 1 day
  temp <- temp %>%
    mutate(totlos = sepdate_last - admdate_first) %>%
    mutate(totlos = totlos + 1*(totlos==0)) # Update length of stay to 1 day if length of stay is 0


  # Only keep the relevant variables, removing existing variable
  temp <- temp %>%
    select(episode, transfer, fileseq, stayseq,
           transseq, admdate_first, sepdate_last, totlos)


  # Sort dataset in original order
  temp <- temp %>% arrange(fileseq)


  # Rename variables using names provided
  colnames(temp) <- c(quo_name(episode),
                      quo_name(transfer),
                      quo_name(fileseq),
                      quo_name(stayseq),
                      quo_name(transseq),
                      quo_name(admdate_first),
                      quo_name(sepdate_last),
                      quo_name(totlos))


  # Add existing dataset to transfers data (merge by row number)
  temp <- bind_cols(data,temp)


  # Return dataset
  return(temp)

}





# Create a function that duplicates the SAS transfers code ----
transfers_sas <- function (data=data,
                       id=id,
                       admdate=admdate,
                       sepdate=sepdate,
                       mode=mode,
                       transfer_modes=c('5','9'),
                       transfer=transfer,
                       episode=episode,
                       fileseq=fileseq,
                       stayseq=stayseq,
                       transseq=transseq,
                       admdate_first=admdate_first,
                       sepdate_last=sepdate_last,
                       totlos=totlos) {

  # Inputs:
  # id - an exisitng variable containing patient ID number
  # admdate - an existing variable containing admission date
  # sepdate - an existing variable containing separation date
  # mode - an existing variable indicating the mode of separation for each episode
  # transfer_modes - which modes of separation should be counted as transfers?
  # transfer - a new indicator variable for whether an episode is a transfer (0=not a transfer, 1=transfer)
  # episode - a new variable that will count episodes for each patient ID, counting from 1,2,3, etc.
  # fileseq - a new variable that will count rows in the entire dataset, counting from 1,2,3 etc.
  # stayseq - a new variable that will group episodes into larger admissions, counting from 1,2,3, etc.
  # transseq - a new variable that will count episodes within a larger admission, counting from 0,1,2 etc.
  # admdate_first - a new variable that will contain the earliest admission date for the larger admission
  # sepdate_last - a new variable that will contain the latest separation date for the larger admission
  # totlos - a new variable that will contain the total length of stay for the larger admission (less than 1 day = 1 day)

  # Load required libraries
  library('tidyr')
  library('dplyr')


  # Enquote input variable names
  id <- enquo(id)
  admdate <- enquo(admdate)
  sepdate <- enquo(sepdate)
  mode <- enquo(mode)
  transfer <- enquo(transfer)
  episode <- enquo(episode)
  fileseq <- enquo(fileseq)
  stayseq <- enquo(stayseq)
  transseq <- enquo(transseq)
  admdate_first <- enquo(admdate_first)
  sepdate_last <- enquo(sepdate_last)
  totlos <- enquo(totlos)


  # Create a temporary dataset to apply changes to
  temp <- data %>%
    select({{id}},{{admdate}},{{sepdate}},{{mode}})


  # Rename each variable using a default name if a different name is used in the dataset
  colnames(temp) <- c('id','admdate','sepdate','mode')

    # Sort data by ID, start date, and end date
  temp <- temp %>%
    arrange(id)

  # Create a fileseq variable
  temp <- temp %>%
    mutate(fileseq = row_number())

  
  # Create in indicator variable for whether an episode is a transfer
  temp <- temp %>%
    mutate(transfer=1*(mode %in% transfer_modes))
  
  
  # Create a variable 'episode' for episode number for each patient
  temp <- temp %>%
    group_by(id) %>%
    mutate(episode = row_number()) %>%
    ungroup()


  # Create variables to identify nested transfers
  maxepisode <- max(temp$episode)
  for (i in 1:maxepisode) { # Loop through each value of episode to update data one row at a time

    # Set starting values for new variables (only where 'episode'==1)
    if (i==1) {
      temp <- temp %>%
        mutate(nest_start=ifelse(episode==i,admdate,NA_Date_) %>% as_date(),
               nest_end=ifelse(episode==i,sepdate,NA_Date_) %>% as_date,
               nest_mode=ifelse(episode==i,mode,NA_character_),
               nested=ifelse(episode==i,0,NA),
               transseq=ifelse(episode==i,0,NA)
        )
    } # Close the if (i==1) statement


    if (i>1) {
      # Update all variables with their previous value (equivalent to 'RETAIN' in SAS)
      temp <- temp %>%
        mutate(nest_start=ifelse(episode==i,lag(nest_start),nest_start),
               nest_end=ifelse(episode==i,lag(nest_end),nest_end),
               nest_mode=ifelse(episode==i,lag(nest_mode),nest_mode),
               nested=ifelse(episode==i,lag(nested),nested)
        )

      # Update nested with it's previous value + 1 if episode is nested
      # (i.e. if admission and separation dates are before the nest end date)
      temp <- temp %>%
        mutate(nested=ifelse(
          (episode==i & (admdate<=nest_end & sepdate<=nest_end)),
          nested+1,
          nested
        ))

      # Update other nesting variables if episode is not nested
      temp <- temp %>%
        mutate(
          nest_start=ifelse(
            (episode==i & !(admdate<=nest_end & sepdate<=nest_end)),
            admdate,
            nest_start),
          nest_mode=ifelse(
            (episode==i & !(admdate<=nest_end & sepdate<=nest_end)),
            mode,
            nest_mode),
          nested=ifelse(
            (episode==i & !(admdate<=nest_end & sepdate<=nest_end)),
            0,
            nested)
        )

      # Update 'nest_end' if epsidoe is not nested
      # Need to do this step separately, after updating the previous variables to ensure 'nest_end'
      # isn't updated first, then other nesting variables updated based on the new value for 'nest_end'
      temp <- temp %>%
        mutate(
          nest_end=ifelse(
            (episode==i & !(admdate<=nest_end & sepdate<=nest_end)),
            sepdate,
            nest_end)
        )


      # Don't need to create lag variables since we can use dplyr::lag()


      # Update transseq if episode is:
      #  - an overlapping transfer (admission date before previous separaton date)
      #  - a nested transfer (admission date before initial record's separation date)
      #  - a transfer (mode of separation is one of those specified in 'transfer_modes'
      #     and admission date is equal to previous episode's separation date)
      temp <- temp %>%
        mutate(
          transseq=ifelse(
            (episode==i &
               ((nested>0) | (admdate<lag(nest_end)) |
                  ((lag(nest_mode) %in% transfer_modes) & admdate==lag(nest_end)))),
            lag(transseq)+1,
            replace_na(transseq,0))
          )

    } # Close the if statement
  } # Close the for loop

  # Update 'stayseq' based on value of transseq, within each patient
  temp <- temp %>%
    group_by(id) %>%
    mutate(stayseq=cumsum(transseq==0)) %>%
    ungroup()

  # Convert nest_start and nest_end to dates
  temp <- temp %>%
    mutate(nest_start=as_date(nest_start),
           nest_end=as_date(nest_end))


  # Create a variable for the first admission date of each larger admission
  temp <- temp %>%
    group_by(id,stayseq) %>%
    mutate(admdate_first = min(admdate)) %>%
    ungroup()


  # Create a variable for the final separation date of each larger admission
  temp <- temp %>%
    group_by(id,stayseq) %>%
    mutate(sepdate_last = max(sepdate)) %>%
    ungroup()


  # Create a variable for total length of stay
  # If separation is on same day as admission, then length of stay is 1 day
  temp <- temp %>%
    mutate(totlos = sepdate_last - admdate_first) %>%
    mutate(totlos = totlos + 1*(totlos==0)) # Update length of stay to 1 day if length of stay is 0


  # Only keep the relevant variables, removing existing variable
  temp <- temp %>%
    select(episode, transfer, fileseq, stayseq,
           transseq, admdate_first, sepdate_last, totlos)


  # Sort dataset in original order
  temp <- temp %>% arrange(fileseq)


  # Rename variables using names provided
  colnames(temp) <- c(quo_name(episode),
                      quo_name(transfer),
                      quo_name(fileseq),
                      quo_name(stayseq),
                      quo_name(transseq),
                      quo_name(admdate_first),
                      quo_name(sepdate_last),
                      quo_name(totlos))


  # Add existing dataset to transfers data (merge by row number)
  temp <- bind_cols(data,temp)


  # Return dataset
  return(temp)

}
