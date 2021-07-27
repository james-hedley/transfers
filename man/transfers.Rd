\name{transfers}
\alias{transfers}

\title{transfers}

\description{
For use with NSW Admitted Patient Data Collection (APDC) data. transfers() will create new variables to group separate
episodes of care together if they are part of a larger hospital admission.

This has been adapted from 'transfers.R' created by Timothy Dobbins, available here:
  https://github.com/timothydobbins/hospital-transfers-multipackage

transfers() is an adaptation of transfers.R from Timothy Dobbins.

transfers_sas() is an adaptation of transfers.SAS, implemented in R

These two methods produce slightly different results, hence both functions are available depending on user preference

}
\usage{
transfers(data = data, id = id, admdate = admdate, sepdate = sepdate,
          mode = mode, transfer_modes = c('5','9'), transfer = transfer,
          episode = episode, fileseq = fileseq, stayseq = stayseq,
          transseq = transseq, admdate_first = admdate_first,
          sepdate_last = sepdate_last, totlos = totlos)

transfers_sas(data = data, id = id, admdate = admdate, sepdate = sepdate,
              mode = mode, transfer_modes = c('5','9'), transfer = transfer,
              episode = episode, fileseq = fileseq, stayseq = stayseq,
              transseq = transseq, admdate_first = admdate_first,
              sepdate_last = sepdate_last, totlos = totlos)

}
\arguments{
  \item{data}{
A tidy dataset in long format (i.e. one row per person per episode of care)
}
  \item{id}{
The variable that identifies individual people in the dataset
}
  \item{admdate}{
The variable that contains the date of the start of the episode of care (admission start date)
}
  \item{sepdate}{
The variable that contains the date of the end of the episode of care (separation date)
}
  \item{mode}{
The variable that contains the mode of separation
}
  \item{transfer_modes}{
A vector with the modes of separation that should be considered as transfers
}
  \item{transfer}{
The name of a new variable that will contain a binary (0 or 1) indicator for whether an episode of care was a transfer
}
  \item{episode}{
The name of a new variable that will contain an episode sequence (i.e. counting episodes of care 1, 2, 3 etc. for each person)
}
  \item{fileseq}{
The name of a new variable that will contain a file sequence (i.e. counting rows in the dataset 1, 2, 3 etc.)
}
  \item{stayseq}{
The name of a new variable that will contain a sequence of larger hospital admissions comprised of multiple episodes of care (i.e. counting 1, 2, 3 for each person)
}
  \item{transseq}{
The name of a new variable that will contain a sequence of episodes of care within each larger hospital admission (i.e. counting 0, 1, 2 etc. for each person and each larger hospital admission)
}
  \item{admdate_first}{
The name of a new variable that will contain the earliest episode start date for the larger hospital admission
}
  \item{sepdate_last}{
The name of a new variable that will contain the latest episode end date for the larger hospital admission
}
  \item{totlos}{
The name of a new variable that will contain the total length of stay in daysfor the larger hospital admission. Calculated as sepdate_last-admdate_first. Minimum length of stay is 1 day
}
}

\examples{
example_apdc_data

example_apdc_data \%>\% transfers::transfers()
}


\references{
https://github.com/timothydobbins/hospital-transfers-multipackage
}

\author{
James Hedley

Adpated from code written by Timothy Dobbins
}

