library(rofi)
library(dplyr)
library(ggplot2)
library(pROC)
library(gtsummary)
library(gt)
library(boot)
library(magrittr)
library(Gmisc, quietly = TRUE)
library(glue)
library(htmlTable)
library(grid)
library(gridExtra)
library(DiagrammeR)
##NA auditfilter = FALSE

listOfAuditFilters <- c("AF_sap_less90","AF_death_30d","AF_iss_15_ej_TE",
                        "AF_mass_transf", "AF_gcs_less9_ej_intubTE", 
                        "AF_iss_15_ej_iva", "AF_mer_60_min_interv",
                        "AF_mer_30min_DT", "AF_hlr_thorak", 
                        "AF_lever_och_mjaltskada", "AF_ej_trombrof_TBI_72h", "AF_all")
listOfAuditFiltersClean <- c("SBP < 90", "Dead at 30 days", "ISS > 15 and no team activation", 
                             "Massive transfusion", "GCS < 9 and not intubated", "ISS > 15 and not in ICU", 
                             "> 60 min until first intervention", "> 30 min until first CT",
                             "CPR and thoracotomy", "Liver or spleen injury", 
                             "No anticoagulants within 72 hours after TBI", "All")

#1,2,3,5,6,7,8 manual
## viktigt att det ska vara samma ordning ^
selectedAuditFilter <- listOfAuditFilters[1:12]

tableKappa <- data.frame(Auditfilter = character(0),
                         Sensitivity = numeric(0),
                         Specificity = numeric(0),
                         Kappa = numeric(0))
tableAuditFilter <- data.frame(Auditfilter = character(0),
                               Number = numeric(0),
                               Missing = numeric(0),
                               Manual = character(0))

#tableOfCalculatedData1 <- data.frame(Auditfilter = character(0),
 #                                    Specificity = numeric(0),
 #                                    Sensitivity = numeric(0))

#A table of sensitivity and specificity
#tableOfCalculatedData2 <- data.frame(Auditfilter = character(0),
#                                     AUC = numeric(0),
#                                     PValue = character(0),
#                                     PValue0.8 = character(0))
#A table of AUC
noacsr::source_all_functions()
importDataOfi <- import_data_ofi(data) #
cleanData <- clean_data(importDataOfi)
dataWithAuditFilters <- create_audit_filters(cleanData)
selectedData <- select_data(dataWithAuditFilters)

createFlowchart <- create_flowchart(importDataOfi)

tableOneData <- select(selectedData, ofi, 
                       Gender,pt_age_yrs,ISS,  
                       ed_sbp_value, ed_gcs_sum,
                       dt_ed_first_ct, ed_intubated, 
                       res_survival,dt_ed_emerg_proc,
                       host_care_level,Tr_Nivå
                       )

#data inclusion for my table one
counter <- 1 #set counter for for loop
manualOrOriginal <- character(0)
for(auditFilter in selectedAuditFilter){
  
  twoVariableData <- select(selectedData, auditFilter, ofi)
  totalValues <- nrow(twoVariableData)
  missingValue <- sum(is.na(twoVariableData[1]))
  #print(missingValue)
  #print(twoVariableData)
  twoVariableDataNaOmit <- na.omit(twoVariableData)
  #twoVariableData[1][is.na(twoVariableData[1])] <- FALSE #turning Na values false.
  numberOfTrue <- sum(twoVariableDataNaOmit[[1]])
  #print(sum(is.na(twoVariableDataNaOmit[1])))
  #print(missingValue)
  if (counter %in% c(1,2,3,5,6,7,8)){
    manualOrOriginal <- "manual"
  } else {
    manualOrOriginal <- "original"
  }
  confidenceIntervalKappa <- confidence_interval_kappa(twoVariableDataNaOmit)
  confidenceIntervalSensitivitySpecificity <- confidence_interval_sens_spec(twoVariableDataNaOmit)
  paste(c(round(calculateAUC, digits = 2)," (",confidenceInterval[5],"-",confidenceInterval[6],") "), collapse = "")
  print(confidenceIntervalKappa)
  
  tableAuditFilterResult <- data.frame(Auditfilter = listOfAuditFiltersClean[counter],
                                       Number = paste(c(numberOfTrue," (", round(numberOfTrue/totalValues*100, digits = 1), "%) "), collapse = ""),
                                       Missing = paste(c(missingValue," (", round(missingValue/totalValues*100, digits = 1), "%) "), collapse = ""),
                                       Manual = manualOrOriginal)
  tableAuditFilter <- rbind(tableAuditFilter,tableAuditFilterResult)
  
  tableKappaResult <- data.frame(Auditfilter = listOfAuditFiltersClean[counter],
                                 Sensitivity = paste(c(confidenceIntervalSensitivitySpecificity[1], " (", confidenceIntervalSensitivitySpecificity[2], "-", confidenceIntervalSensitivitySpecificity[3], ") "), collapse = ""),
                                 Specificity = paste(c(confidenceIntervalSensitivitySpecificity[4], " (", confidenceIntervalSensitivitySpecificity[5], "-", confidenceIntervalSensitivitySpecificity[6], ") "), collapse = ""),
                                 Kappa = paste(c(confidenceIntervalKappa[1]," (", confidenceIntervalKappa[2], "-", confidenceIntervalKappa[3], ") "), collapse = ""))
  tableKappa <- rbind(tableKappa,tableKappaResult)
  
  createAuditFilterTable <- create_audit_filter_table(listOfAuditFiltersClean, missingValues)
  counter <- counter + 1
}

tableK <- gt(tableKappa)
tableK %>% gtsave(filename = "tabk.html")
tableAF <- gt(tableAuditFilter)
tableAF %>% gtsave(filename = "tabAF.html")

tableOne <- tableOneData %>%
  mutate(host_care_level = factor(host_care_level, levels = c("Emergency department", 
                                                              "General ward", 
                                                              "Surgical ward", 
                                                              "Specialist ward/Intermediate ward", 
                                                              "Intensive care unit")))  %>%
  tbl_summary(by = ofi,
                        missing = "ifany",
                        type = all_dichotomous() ~ "categorical",
                        label = list(pt_age_yrs ~ "Age", 
                                     ed_sbp_value ~ "ED Systolic Blood Pressure", 
                                     ed_gcs_sum ~ "ED GCS",
                                     dt_ed_first_ct ~ "Time to first CT",
                                     ed_intubated ~ "Intubated at ED",
                                     res_survival ~ "Dead at 30 days",
                                     dt_ed_emerg_proc ~ "Time to definitive treatment",
                                     host_care_level ~ "Highest level of care",
                                     Tr_Nivå ~ "Trauma team activation ")) %>% 
  add_overall(last = TRUE, col_label = "**Overall** (N = {N})")%>%
  modify_table_styling(footnote = "Definition of abbreviations:
                         OFI = Opportunity for Improvement;
                         ISS = Injury Severity Score;
                         ED = Emergency Department;
                         SBP = Systolic Blood Pressure;
                         GCS = Glascow Coma Scale;
                        CT = Computer Tomography;") %>%
  modify_header(label = "",
                stat_1 = "**No**, (N = {n})",
                stat_2 = "**Yes**, (N = {n})") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**OFI**") %>%
  bold_labels() %>%
  add_p(test = list(all_continuous() ~ "wilcox.test")) %>%
  bold_p() 
############TABLE ONE#######################

'
tableThree <- tableOfCalculatedData1 %>%
  gt() %>% 
  cols_label(Auditfilter = "Audit filter",
             Number = "(N)",
             Specificity = "Specificity (%)",
             Sensitivity = "Sensitivity (%)") %>%
  cols_align(align = "left") %>%
  tab_source_note("Definition of abbreviations:
  OFI = Opportunity for Improvement;
  SBP = Systolic Blood Pressure;
  ISS = Injury Severity Score;
  GCS = Glascow Coma Scale;
  ICU = Intensive Care Unit;
  CT = Computer Tomopgraphy;
  ED = Emergency Department; 
  CPR = Cardiopulmonary Resuscitation;
  TBI = Traumatic Brain Injury
                  ")
############TABLE TWO#######################
tableFour <- gt(tableOfCalculatedData2) %>% 
  cols_label(PValue = "p-value (AUC = 0.5)",
             PValue0.8 = "p-value (AUC = 0.8)",
             Auditfilter = "Audit filter") %>%
  cols_align(align = "left") %>%
  tab_source_note("Definition of abbreviations: 
  OFI = Opportunity for Improvement; 
  AUC = Area under the receiver operating characteristic curve;
  SBP = Systolic Blood Pressure;
  ISS = Injury Severity Score;
  GCS = Glascow Coma Scale;
  ICU = Intensive Care Unit;
  CT = Computer Tomography;
  ED = Emergency Department; 
  CPR = Cardiopulmonary Resuscitation;
  TBI = Traumatic Brain Injury
  ")
############TABLE THREE#######################
tableTwo <- gt(tableFourData) %>%
  cols_label(listOfAuditFiltersClean = "Audit filters",
             missingValues = "Missing values n (%)") %>%
  cols_align(align = "left") %>%
  tab_source_note("Definition of abbreviations: 
  OFI = Opportunity for Improvement; 
  SBP = Systolic Blood Pressure;
  ISS = Injury Severity Score;
  GCS = Glascow Coma Scale;
  ICU = Intensive Care Unit;
  CT = Computer Tomography;
  ED = Emergency Department; 
  CPR = Cardiopulmonary Resuscitation;
  TBI = Traumatic Brain Injury
  ")
############TABLE FOUR#######################
tableFive <- gt(tableFiveData) %>%
  cols_label(Auditfilter = "Audit filter",
             Design = "Manually created/original") %>%
  cols_align(align = "left") %>%
  tab_source_note("Definition of abbreviations: 
  OFI = Opportunity for Improvement; 
  SBP = Systolic Blood Pressure;
  ISS = Injury Severity Score;
  GCS = Glascow Coma Scale;
  ICU = Intensive Care Unit;
  CT = Computer Tomography;
  ED = Emergency Department; 
  CPR = Cardiopulmonary Resuscitation;
  TBI = Traumatic Brain Injury
  ")


  tableTwo %>% gtsave(filename = "tab_2.html")
  tableThree %>% gtsave(filename = "tab_3.html")
  tableFour %>% gtsave(filename = "tab_4.html")
  tableFive %>% gtsave(filename = "tab_5.html")
   tableOne %>% gtsave(filename = "tab_1.html")
   
'
  