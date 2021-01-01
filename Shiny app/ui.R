library(shiny)

ui <- navbarPage(
  "MRI and Alzheimers",
  
  tabPanel("Distributions",
           verticalLayout(
             tabsetPanel(
               tabPanel("Years of Education", plotOutput('educ')),
               tabPanel("Socioeconomic Status", plotOutput('ses')),
               tabPanel("Gender", plotOutput('gender')),
               tabPanel("Clinical Dementia Rating", plotOutput('cdr')),
               tabPanel("Mini Mental State Examination", plotOutput('mmse'))))),
  
  tabPanel("Correlations",
           verticalLayout(
             tabsetPanel(
               tabPanel("Years of Education & CDR",
                        plotOutput('educ_cdr'),
                        verbatimTextOutput('educ_cdr_chisq')),
               tabPanel("Years of Education & MMSE",
                        plotOutput('educ_mmse'),
                        verbatimTextOutput('educ_mmse_chisq')),
               tabPanel("Gender", 
                        verbatimTextOutput('gender_cdr'),
                        verbatimTextOutput('gender_mmse'))))),
 
  tabPanel("Converted",
           verticalLayout(
             tabsetPanel(
               tabPanel("CDR dynamics & Age of detection", 
                        splitLayout(cellWidths = c("75%", "25%"), 
                          plotOutput('cdr_dynamics'),
                          tableOutput('age'))),
               tabPanel("Average CDR", plotOutput('average_cdr')),
               tabPanel("MMSE dynamics", plotOutput('mmse_dynamics')),
               tabPanel("Brain volume dynamics", plotOutput('volume_dynamics')),
               tabPanel("Average nWBV", plotOutput('average_nwbv'))))),

  tabPanel("MRI indicators",
           verticalLayout(
             tabsetPanel(
               tabPanel("Age and eTIV", plotOutput('age_etiv')),
               tabPanel("Gender and eTIV", plotOutput('gender_etiv')),
               tabPanel("ASF", plotOutput('asf')),
               tabPanel("Average Brain Volume (eTIV)", plotOutput('etiv')),
               tabPanel("Average Brain Volume (nWBV)", plotOutput('nwbv')))))
)