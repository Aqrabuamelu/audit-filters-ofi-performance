create_audit_filters <- function(cleanData){

  cleanData$AF_sap_less90 <- ifelse(
    is.na(cleanData$ed_sbp_value) & is.na(cleanData$ed_sbp_rtscat), NA, 
    ifelse(is.na(cleanData$ed_sbp_value), cleanData$ed_sbp_rtscat <= 3, 
           ifelse(cleanData$ed_sbp_value < 90 , TRUE,FALSE)))
  #cleanData$AF_sap_less90 <- cleanData$ed_sbp_value < 90 | cleanData$ed_sbp_rtscat <= 3
  #cleanData$AF_sap_less90[is.na(cleanData$AF_sap_less90)] <- FALSE
  #VK_sap_less90
  cleanData$AF_gcs_less9_ej_intubTE <- ifelse(
    is.na(cleanData$ed_gcs_sum) | is.na(cleanData$ed_intubated) | cleanData$ed_gcs_sum >= 9, NA, 
    ifelse(cleanData$ed_gcs_sum < 9 & cleanData$ed_intubated != "Yes", TRUE,FALSE))
  #cleanData$AF_gcs_less9_ej_intubTE <- cleanData$ed_gcs_sum < 9 & cleanData$ed_intubated != "Yes"
  #cleanData$AF_gcs_less9_ej_intubTE[is.na(cleanData$AF_gcs_less9_ej_intubTE)] <- FALSE
  #VK_gcs_less9_ej_intubTE
  cleanData$AF_mer_30min_DT <- cleanData$dt_ed_first_ct > 30
  #cleanData$AF_mer_30min_DT[is.na(cleanData$AF_mer_30min_DT)] <- FALSE
  #VK_mer_30min_DT
  cleanData$AF_mer_60_min_interv <- cleanData$dt_ed_emerg_proc > 60
  #cleanData$AF_mer_60_min_interv[is.na(cleanData$AF_mer_60_min_interv)] <- FALSE
  #VK_mer_60_min_interv
  #cleanData$AF_iss_15_ej_iva <- cleanData$ISS >= 15 & cleanData$host_care_level != "Intensive care unit"
  cleanData$AF_iss_15_ej_iva <- ifelse(
    is.na(cleanData$ISS) | is.na(cleanData$host_care_level) | cleanData$ISS < 15, NA, 
    ifelse(cleanData$ISS >= 15 & cleanData$host_care_level != "Intensive care unit", TRUE,FALSE))
  #cleanData$AF_iss_15_ej_iva[is.na(cleanData$AF_iss_15_ej_iva)] <- FALSE
  #VK_iss_15_ej_iva
  cleanData$AF_death_30d <- cleanData$res_survival == "Yes"
  #cleanData$AF_death_30d[is.na(cleanData$AF_death_30d)] <- FALSE
  #death after 30d
  #cleanData$AF_iss_15_ej_TE <- cleanData$ISS >= 15 & cleanData$Tr_Nivå == "No"
  cleanData$AF_iss_15_ej_TE <- ifelse(
    is.na(cleanData$ISS) | is.na(cleanData$Tr_Nivå) | cleanData$ISS < 15, NA, 
    ifelse(cleanData$ISS >= 15 & cleanData$Tr_Nivå == "No", TRUE,FALSE))
  #cleanData$AF_iss_15_ej_TE[is.na(cleanData$AF_iss_15_ej_TE)] <- FALSE
  #ISS 15 och ingen trauma larm
  cleanData$AF_ej_trombrof_TBI_72h <- cleanData$VK_ej_trombrof_TBI_72h == "ja"
  #turning values into boolean
  cleanData$AF_hlr_thorak <- cleanData$VK_hlr_thorak == "ja"
  #turning values into boolean
  cleanData$AF_mass_transf <- cleanData$VK_mass_transf == "ja"
  #turning values into boolean
  print(unique(cleanData$VK_leverskada))
  print(unique(cleanData$VK_mjaltskada))
  
  cleanData$AF_lever_och_mjaltskada <- cleanData$VK_leverskada == "ja" | cleanData$VK_mjaltskada == "ja"
  #combining VK_mjaltskada and VK_leverskada into one auditfilter and turning the values to boolean
  #turning values into boolean
  print(sum(cleanData$AF_lever_och_mjaltskada == TRUE, rm.na = TRUE))
  cleanData$ofi <- cleanData$ofi == "yes"
  
  afCols <- grep("^AF_", names(cleanData))
  #cleanData[afCols] <- lapply(cleanData[afCols], function(x) replace(x, is.na(x), FALSE))

  return(cleanData)
}