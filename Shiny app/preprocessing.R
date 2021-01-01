library(dplyr)

data <- read.csv('oasis_longitudinal.csv', sep = ',')

patient_data <- function() {
  patient <- data.frame('Patient.Id' = data$Subject.ID, 'Gender' = data$M.F, 'Hand' = data$Hand, 
                        'EDUC' = data$EDUC, 'SES' = data$SES, 
                        'Status' = data$Group)
  patient <- patient[!duplicated(patient$Patient.Id),]
  return(patient)
}

mri_data <- function() {
  mri <- select(data, -M.F , -Hand, - EDUC, -SES, - Group)
  names(mri)[names(mri) == 'Subject.ID'] <- 'Patient.Id'
  names(mri)[names(mri) == 'MRI.ID'] <- 'MRI.Id'
  return(mri)
}