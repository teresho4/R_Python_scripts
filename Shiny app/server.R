library(shiny)
library(ggplot2)
library(dplyr)
source('preprocessing.R')

server <- function(input, output) {
  patient <- patient_data()
  mri <- mri_data()
  
  
  # DISTRIBUTIONS
  output$educ <- renderPlot({
    ggplot(patient, aes(x = EDUC, fill = Status)) + geom_histogram()
  })
  output$ses <- renderPlot({
    ggplot(patient, aes(SES, fill = Status)) + geom_histogram(binwidth = 0.4)
  })
  output$gender <- renderPlot({
    ggplot(patient) + geom_bar(aes(x = Gender, fill = Status), width = 0.4)
  })
  join <- inner_join(select(patient, Patient.Id, Status), 
                               select(mri, Patient.Id, CDR, MMSE), 
                               by = 'Patient.Id')
  output$cdr <- renderPlot({
    ggplot(join, aes(CDR, fill = Status)) + geom_histogram(binwidth = 0.2)
  })
  output$mmse <- renderPlot({
    ggplot(join, aes(MMSE, fill = Status)) + geom_histogram()
  })
  
  
  # CORRELATIONS
  join_correlations <- inner_join(select(patient, Patient.Id, EDUC, SES, Gender), 
                                  select(mri, Patient.Id, CDR, MMSE), 
                                  by = 'Patient.Id')
  join_correlations %>%
    group_by(Patient.Id) %>%
    mutate(CDR = mean(CDR)) %>%
    mutate(MMSE = mean(MMSE)) %>%
    unique() ->
    join_correlations
  
  # Years of Education & CDR
  output$educ_cdr <- renderPlot({
    ggplot(join_correlations, aes(x = EDUC, y = CDR)) +
      labs(y = 'mean CDR') + 
      geom_point() + 
      stat_smooth(method = lm, se = FALSE)
  })
  output$educ_cdr_chisq <- renderPrint({
    EDUC <- join_correlations$EDUC
    CDR <- join_correlations$CDR
    test <- chisq.test(EDUC, CDR)
    test
  })
  
  # Years of Education & MMSE
  output$educ_mmse <- renderPlot({
    ggplot(join_correlations, aes(x = EDUC, y = MMSE)) +
      labs(y = 'mean MMSE') + 
      geom_point() + 
      stat_smooth(method = lm, se = FALSE)
  })
  output$educ_mmse_chisq <- renderPrint({
    EDUC <- join_correlations$EDUC
    MMSE <- join_correlations$MMSE
    test <- chisq.test(EDUC, MMSE)
    test
  })
  
  # Gender & CDR | MMSE
  output$gender_cdr <- renderPrint({
    CDR <- join_correlations$CDR
    Gender <- join_correlations$Gender
    t.test(CDR ~ Gender)
  })
  
  output$gender_mmse <- renderPrint({
    MMSE <- join_correlations$MMSE
    Gender <- join_correlations$Gender
    t.test(MMSE ~ Gender)
  })
  
  # CONVERTED
  join_converted <- inner_join(select(patient, Patient.Id, Status), 
                               select(mri, Patient.Id, Visit, Age, CDR, MMSE, nWBV), 
                               by = 'Patient.Id')
  join_converted <- filter(join_converted, Status == "Converted")
  
  output$cdr_dynamics <- renderPlot({
    ggplot(join_converted, aes(Visit, CDR, size = pop, color = Patient.Id)) + 
      geom_line(size = 1) + 
      geom_point(shape = 21, size = 2, fill = 'white') +
      facet_wrap(~ Patient.Id)
  })
  output$age <- renderTable({
    join_converted %>%
      filter(CDR != 0.0) %>%
      distinct(Patient.Id, .keep_all = TRUE) %>%
      select(Patient.Id, Age)
  })
  
  output$average_cdr <- renderPlot({
    join_converted %>% 
      group_by(Visit) %>%
      summarise(AvCDR = mean(CDR)) %>%
      ggplot(aes(Visit, AvCDR)) + 
        geom_line(size = 1, linetype = "dashed") +
        geom_point() +
        labs(y = 'Average CDR')
  })
  
  output$mmse_dynamics <- renderPlot({
    ggplot(join_converted, aes(Visit, MMSE, size = pop, color = Patient.Id)) +
      geom_line(size = 1) + 
      geom_point(shape = 21, size = 2, fill = 'white') +
      facet_wrap(~ Patient.Id)
  })
  
  output$volume_dynamics <- renderPlot({
    ggplot(join_converted, aes(Visit, nWBV, size = pop, color = Patient.Id)) +
      geom_line(size = 1) + 
      geom_point(shape = 21, size = 2, fill = 'white') +
      facet_wrap(~ Patient.Id)
  })
  
  output$average_nwbv <- renderPlot({
    join_converted %>% 
      group_by(Visit) %>%
      summarise(AvnWBV = mean(nWBV)) %>%
      ggplot(aes(Visit, AvnWBV)) +
        geom_line(size = 1, linetype = "dashed") +
        geom_point() +
        stat_smooth(method = lm, se = FALSE) +
        labs(y = 'Average nWBV')
  })
  
  
  # MRI INDICATORS
  join_mri <- inner_join(select(patient, Patient.Id, Status, Gender), 
                                select(mri, Patient.Id, Age, eTIV, nWBV, ASF), 
                                by = 'Patient.Id')
  
  output$age_etiv <- renderPlot({
    join_mri %>% 
      group_by(Patient.Id, Age) %>% 
      summarise(eTIV = mean(eTIV)) %>% 
      ggplot(aes(Age, eTIV)) +
        geom_point() +
        stat_smooth(method = lm, colour="mediumpurple4", fill="mediumpurple") +
        labs(y = 'mean eTIV')
  })
  
  output$gender_etiv <- renderPlot({
    join_mri %>% 
      group_by(Patient.Id, Gender) %>% 
      summarise(eTIV = mean(eTIV)) %>%
      group_by(Gender) %>%
      summarise(meanTIV = mean(eTIV), sdTIV = sd(eTIV)) %>% 
      mutate(lower = meanTIV - 2*sdTIV, upper = meanTIV + 2*sdTIV)  %>% 
        ggplot(aes(x = Gender, y = meanTIV, ymin = lower, ymax = upper, color = Gender)) +
          geom_bar(stat = 'identity', fill = 'white', width = 0.5) +
          geom_errorbar(width = 0.05, size = 1) +
          scale_x_discrete(labels = c("Female", "Male")) +
          labs(y = 'mean eTIV')
  })
  
  join_mri_binary_status <- join_mri
  join_mri_binary_status$Status[join_mri$Status == 'Converted']  <- 'Demented'
  
  output$asf <- renderPlot({
    join_mri_binary_status %>% 
      group_by(Patient.Id, Status) %>% 
      summarise(mASF = mean(ASF)) %>%
      group_by(Status) %>%
      summarise(mean_ASF = mean(mASF), 
                sd_ASF = sd(mASF)) %>% 
      mutate(lower = mean_ASF - 2*sd_ASF, 
             upper = mean_ASF + 2*sd_ASF) %>%
      ggplot(aes(x = Status, y = mean_ASF, ymin = lower, ymax = upper, color = Status)) +
        geom_bar(stat = 'identity', fill = 'white', width = 0.5) +
        geom_errorbar(width = 0.05, size = 1)  +
        labs(y = 'mean ASF')
  })
  
  output$etiv <- renderPlot({
    join_mri_binary_status %>% 
      group_by(Patient.Id, Status) %>% 
      summarise(mTIV = mean(eTIV)) %>%
      group_by(Status) %>%
      summarise(meanTIV = mean(mTIV), 
                sdTIV = sd(mTIV)) %>% 
      mutate(lower = meanTIV - 2*sdTIV, 
             upper = meanTIV + 2*sdTIV) %>%
      ggplot(aes(x = Status, y = meanTIV, ymin = lower, ymax = upper, color = Status)) +
        geom_bar(stat = 'identity', fill = 'white', width = 0.5) +
        geom_errorbar(width = 0.05, size = 1)  +
        labs(y = 'mean eTIV')
  })
  
  output$nwbv <- renderPlot({
    join_mri_binary_status %>% 
      group_by(Patient.Id, Status) %>% 
      summarise(mWBV = mean(nWBV)) %>%
      group_by(Status) %>%
      summarise(meanWBV = mean(mWBV), 
                sdWBV = sd(mWBV)) %>% 
      mutate(lower = meanWBV - 2*sdWBV, 
             upper = meanWBV + 2*sdWBV) %>%
      ggplot(aes(x = Status, y = meanWBV, ymin = lower, ymax = upper, color = Status)) +
        geom_bar(stat = 'identity', fill = 'white', width = 0.5) +
        geom_errorbar(width = 0.05, size = 1) +
        labs(y = 'mean nWBV')
  })
}