library(dplyr)
library(readxl)
# Set wd to project folder destination
setwd("C:/Users/alec.blase/OneDrive - HHS Office of the Secretary/Desktop/hospital-index-app")


#Breadth data to impute

summary_columns<-c("Find", "Send", "Receive", "Integrate", 
                   "Available", "Used",
                   "HospitalSend", "HospitalReceive", "AmbSend", "AmbReceive", 
                   "LTPACSend", "LTPACReceive", "BHSend", "BHReceive", 
                   "PE_Download", "PE_Import", "PE_Send", "PE_API", "PE_FHIR", "PE_PGHDFHIR", 
                   "PH_SS", "PH_IRR", "PH_ECR", "PH_PHR", "PH_CDR", "PH_ERL", "PH_HCR", 
                   "SDOH_Received", "SDOH_Used_Ind", "SDOH_Used_Pop",
                   "SBarrier_Tech", "Barrier_Send", "Barrier_Receive", "Barrier_Other", 
                   "MFind", "MSend", "MReceive", 
                   "IB_Extent_EHR", "IB_Extent_HIE", "IB_Extent_Prov", 
                   "API_Integrate_clin", "API_SendEHR_clin", "API_Integrate_admin", "API_SendEHR_admin")

# Define Functions
CreateCategories<-function(df) {
  #Create size categories
  df$hospSize[df$smallbed==1]<-0
  df$hospSize[df$medbed==1]<-1
  df$hospSize[df$largebed==1]<-2

  #Create system member
  df$sysMemb[df$mhsmemb==1]<-1
  df$sysMemb[is.na(df$mhsmemb)]<-0

  #Create ehr
  if (is.character(df$piemr)) {
    df$ehr<-df$piemr
    df$ehr[((df$piemr!="Epic") & (df$piemr!="Cerner") & (df$piemr!="CPSI/Evident") & (df$piemr!="Meditech")) | is.na(df$piemr)]<-"Other"
  } else {
    df$ehr[df$piemr==2]<-"Cerner"
    df$ehr[df$piemr==5]<-"Epic"
    df$ehr[df$piemr==9]<-"Meditech"
    df$ehr[df$piemr==17]<-"CPSI/Evident"
    df$ehr[is.na(df$ehr)]<-"Other"
  }
  return(df)
}

CreateDataSet<-function(df) {
  # Use tidy evaluation to allow programming with dplyr

  summary_columns<-c("Find", "Send", "Receive", "Integrate", 
                     "Available", "Used",
                     "HospitalSend", "HospitalReceive", "AmbSend", "AmbReceive", 
                     "LTPACSend", "LTPACReceive", "BHSend", "BHReceive", 
                     "PE_Download", "PE_Import", "PE_Send", "PE_API", "PE_FHIR", "PE_PGHDFHIR", 
                     "PH_SS", "PH_IRR", "PH_ECR", "PH_PHR", "PH_CDR", "PH_ERL", "PH_HCR", 
                     "SDOH_Received", "SDOH_Used_Ind", "SDOH_Used_Pop",
                     "SBarrier_Tech", "Barrier_Send", "Barrier_Receive", "Barrier_Other", 
                     "MFind", "MSend", "MReceive", 
                     "IB_Extent_EHR", "IB_Extent_HIE", "IB_Extent_Prov", 
                     "API_Integrate_clin", "API_SendEHR_clin", "API_Integrate_admin", "API_SendEHR_admin")
  
  # Keep only columns that exist in the dataframe
  existing_columns <- intersect(summary_columns, colnames(df))
  
  # Weighted mean function
  weighted_mean <- function(x, w) {
    if (all(is.na(x))) return(NA_real_) #If everything is NA, return NA
    sum(x * w, na.rm = TRUE) / sum(w[!is.na(x)], na.rm = TRUE) #Aply weight, divide by # not NA.
  }
  
  # Apply weighted mean with across
  summary <- df %>%
    summarise(
      across(
        all_of(existing_columns),
        ~weighted_mean(.x, weight),
        .names = "{.col}"
      )
    )
  summary$characteristic<-"Overall"
  summary$char_value<-"1"
  return(summary)
}

 

CreateDataSetCharacteristic<-function(df, charInput) {
  # Use tidy evaluation to allow programming with dplyr
  charInput_sym <- sym(charInput)
  
  summary_columns<-c("Find", "Send", "Receive", "Integrate", 
                     "Available", "Used",
                     "HospitalSend", "HospitalReceive", "AmbSend", "AmbReceive", 
                     "LTPACSend", "LTPACReceive", "BHSend", "BHReceive", 
                     "PE_Download", "PE_Import", "PE_Send", "PE_API", "PE_FHIR", "PE_PGHDFHIR", 
                     "PH_SS", "PH_IRR", "PH_ECR", "PH_PHR", "PH_CDR", "PH_ERL", "PH_HCR", 
                     "SDOH_Received", "SDOH_Used_Ind", "SDOH_Used_Pop",
                     "SBarrier_Tech", "Barrier_Send", "Barrier_Receive", "Barrier_Other", 
                     "MFind", "MSend", "MReceive", 
                     "IB_Extent_EHR", "IB_Extent_HIE", "IB_Extent_Prov", 
                     "API_Integrate_clin", "API_SendEHR_clin", "API_Integrate_admin", "API_SendEHR_admin")
  
  # Keep only columns that exist in the dataframe
  existing_columns <- intersect(summary_columns, colnames(df))
  
  # Weighted mean function
  weighted_mean <- function(x, w) {
    if (all(is.na(x))) return(NA_real_) #If everything is NA, return NA
    sum(x * w, na.rm = TRUE) / sum(w[!is.na(x)], na.rm = TRUE) #Aply weight, divide by # not NA.
  }
  
  # Apply weighted mean with across
  summary <- df %>%
    group_by(!!charInput_sym) %>%
    summarise(
      across(
        all_of(existing_columns),
        ~weighted_mean(.x, weight),
        .names = "{.col}"
      ),
      .groups = "drop"
    ) %>%
    mutate(characteristic = charInput,
           char_value = as.character(!!charInput_sym)) %>%
    select(characteristic, char_value, all_of(existing_columns))
  
  return(summary)
}

## 2021
df2021 <- read_excel("data/Interop Index Export 2021 Index Items.xls")
df<-CreateCategories(df2021)

overall<-CreateDataSet(df)
cah<-CreateDataSetCharacteristic(df, "cah")
size<-CreateDataSetCharacteristic(df, "hospSize")
location<-CreateDataSetCharacteristic(df, "cbsatype")
systemMembership<-CreateDataSetCharacteristic(df, "sysMemb")
ehr<-CreateDataSetCharacteristic(df, "ehr")

collapseddf2021<-rbind(overall, cah, size, location, systemMembership, ehr)
collapseddf2021$year<-2021


## 2022
df2022 <- read_excel("data/Interop Index Export 2022 Index Items.xls")
df<-CreateCategories(df2022)

overall<-CreateDataSet(df)
cah<-CreateDataSetCharacteristic(df, "cah")
size<-CreateDataSetCharacteristic(df, "hospSize")
location<-CreateDataSetCharacteristic(df, "cbsatype")
systemMembership<-CreateDataSetCharacteristic(df, "sysMemb")
ehr<-CreateDataSetCharacteristic(df, "ehr")

collapseddf2022<-rbind(overall, cah, size, location, systemMembership, ehr)
collapseddf2022$year<-2022

## 2023
df2023 <- read_excel("data/Interop Index Export 2023 Index Items.xls")
df<-CreateCategories(df2023)

overall<-CreateDataSet(df)
cah<-CreateDataSetCharacteristic(df, "cah")
size<-CreateDataSetCharacteristic(df, "hospSize")
location<-CreateDataSetCharacteristic(df, "cbsatype")
systemMembership<-CreateDataSetCharacteristic(df, "sysMemb")
ehr<-CreateDataSetCharacteristic(df, "ehr")

collapseddf2023<-rbind(overall, cah, size, location, systemMembership, ehr)
collapseddf2023$year<-2023

## 2024
df2024 <- read_excel("data/Interop Index Export 2024 Index Items.xls")
df<-CreateCategories(df2024)

overall<-CreateDataSet(df)
cah<-CreateDataSetCharacteristic(df, "cah")
size<-CreateDataSetCharacteristic(df, "hospSize")
location<-CreateDataSetCharacteristic(df, "cbsatype")
systemMembership<-CreateDataSetCharacteristic(df, "sysMemb")
ehr<-CreateDataSetCharacteristic(df, "ehr")

collapseddf2024<-rbind(overall, cah, size, location, systemMembership, ehr)
collapseddf2024$year<-2024

## 2025
df2025 <- read_excel("data/Interop Index Export 2025 Index Items.xls")
df2025 <- df2025 %>% 
  mutate(mhsmemb = case_when(mhsmemb == 1 ~ 1,
                             mhsmemb == "NA" ~ NA))
df<-CreateCategories(df2025)

overall<-CreateDataSet(df)
cah<-CreateDataSetCharacteristic(df, "cah")
size<-CreateDataSetCharacteristic(df, "hospSize")
location<-CreateDataSetCharacteristic(df, "cbsatype")
systemMembership<-CreateDataSetCharacteristic(df, "sysMemb")
ehr<-CreateDataSetCharacteristic(df, "ehr")

collapseddf2025<-rbind(overall, cah, size, location, systemMembership, ehr)
collapseddf2025$year<-2025

## Replace uncalculated values with NA to facilitate appending data frames.
### 2021
missing_columns <- setdiff(summary_columns, colnames(collapseddf2021))
collapseddf2021[missing_columns] <- NA

missing_columns <- setdiff(summary_columns, colnames(collapseddf2022))
collapseddf2022[missing_columns] <- NA

missing_columns <- setdiff(summary_columns, colnames(collapseddf2023))
collapseddf2023[missing_columns] <- NA

missing_columns <- setdiff(summary_columns, colnames(collapseddf2024))
collapseddf2024[missing_columns] <- NA

missing_columns <- setdiff(summary_columns, colnames(collapseddf2025))
collapseddf2025[missing_columns] <- NA

combineddf<-rbind(collapseddf2021, collapseddf2022, collapseddf2023, collapseddf2024, collapseddf2025)
combineddf$year<-as.character(combineddf$year)


dfItem<-combineddf    
#dfItem$characteristic[dfItem$characteristic == "mhsmemb"] <- "sysMemb"
#dfItem$char_value[dfItem$characteristic == "sysMemb" & is.na(dfItem$char_value)] <- 0

dfItem<-subset(dfItem, select=(-SBarrier_Tech))

dfItem <- dfItem %>%
  rename("Year" = year,
         "Find Data Electronically" = Find,
         "Send Data Electronically" = Send,
         "Receive Data Electronically" = Receive,
         "Integrate Data without Manual Effort" = Integrate,
         "Information Available to Clinicians" = Available,
         "Information Used by Clinicians" = Used,
         "Syndromic Surveillance Reporting by EHR / HIE" = PH_SS,
         "Immunization Data Reporting by EHR / HIE" = PH_IRR,
         "Electronic Case Reporting by EHR / HIE" = PH_ECR,
         "Public Health Registry Reporting by EHR / HIE" = PH_PHR,
         "Clinical Registry Reporting by EHR / HIE" = PH_CDR,
         "Electronic Lab Result Reporting by EHR / HIE" = PH_ERL,
         "Hospital Capacity Reporting by EHR / HIE" = PH_HCR,
         "Enable Patient Information Downloading" = PE_Download,
         "Enable Patient Information Importing" = PE_Import,
         "Enable Sending Patient Information" = PE_Send,
         "Enable Patient Access to Apps via APIs" = PE_API,
         "Enable Patient Access to Apps via FHIR" = PE_FHIR,
         "Enable Submission of Patient Generated Data via FHIR" = PE_PGHDFHIR,
         "Send Hospitals Information" = HospitalSend,
         "Receive Information from Hospitals" = HospitalReceive,
         "Send Ambulatory Providers Information" = AmbSend,
         "Receive Information from Ambulatory Providers" = AmbReceive,
         "Send Long-term Care Patient Information" = LTPACSend,
         "Receive Information from Long-term Care" = LTPACReceive,
         "Send Behavioral Health Patient Information" = BHSend,
         "Receive Information from Behavioral Health" = BHReceive,
         "Barriers to Receiving Information" = Barrier_Receive,
         "Barriers to Sending Information" = Barrier_Send,
         "Other Barriers to Exchange" = Barrier_Other,
         "Numerous Methods to Find Information" = MFind,
         "Numerous Methods to Send Information" = MSend,
         "Numerours Methods to Receive Information" = MReceive,
         "Receive SDoH Information from Other Organizations" = SDOH_Received,
         "Use SDoH Information from Other Organizations for Individual Care"=SDOH_Used_Ind,
         "Use SDoH Information from Other Organizations for Population Health"=SDOH_Used_Pop,
         "Provide EHR Data to Third-Party Apps - Clinical" = API_SendEHR_clin,
         "Integrate Third-Party Data into EHR - Clinical" = API_Integrate_clin,
         "Provide EHR Data to Third-Party Apps - Administrative" = API_SendEHR_admin,
         "Integrate Third-Party Data into EHR - Administrative" = API_Integrate_admin,
         "Experience Information Blocking by EHR Developers"=IB_Extent_EHR,
         "Experience Information Blocking by Healthcare Providers"=IB_Extent_Prov,
         "Experience Information Blocking by Health Information Networks"=IB_Extent_HIE)

save(dfItem, file="C:/Users/alec.blase/OneDrive - HHS Office of the Secretary/Desktop/hospital-index-app/data/index_items_df.RData")
