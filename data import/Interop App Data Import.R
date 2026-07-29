library(dplyr)
library(readxl)
# Set wd to project folder destination
setwd("C:/Users/alec.blase/OneDrive - HHS Office of the Secretary/Desktop/hospital-index-app")


#Breadth data to impute



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

  summary_columns <- c("Core", "Friction", "ClinInteropFx", "DataUse",
                       "breadth_of_exchange", "Barriers", "Methods", "InfoBlocking", "CorePlus", "PublicHealth", "PatientEngagement", "SDOH", "API", 
                       "PE_Download", "PE_Import", "PE_Send", "PE_API", "PE_FHIR", "PE_PGHDFHIR")

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
  
  summary_columns <- c("Core", "Friction", "ClinInteropFx", "DataUse",
                       "breadth_of_exchange", "Barriers", "Methods", "InfoBlocking", "CorePlus", "PublicHealth", "PatientEngagement", "SDOH", "API",
                       "PE_Download", "PE_Import", "PE_Send", "PE_API", "PE_FHIR", "PE_PGHDFHIR")
  
  # Keep only columns that exist in the dataframe
  existing_columns <- intersect(summary_columns, colnames(df))
  
  # Weighted mean function
  weighted_mean <- function(x, w) {
    if (all(is.na(x))) return(NA_real_) #If everything is NA, return NA
    sum(x * w, na.rm = TRUE) / sum(w[!is.na(x)], na.rm = TRUE) #Apply weight, divide by # not NA.
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
df2021 <- read_excel("data/Interop Index Export App 2021.xls")
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
df2022 <- read_excel("data/Interop Index Export App 2022.xls")
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
df2023 <- read_excel("data/Interop Index Export App 2023.xls")
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
df2024 <- read_excel("data/Interop Index Export App 2024.xls")
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
df2025 <- read_excel("data/Interop Index Export App 2025.xls")
df2025$mhsmemb <- na_if(df2025$mhsmemb, "NA") # Manually replacing string NA's from xls file with NULL
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
collapseddf2021$API<-NA
collapseddf2021$SDOH<-NA

### 2023
collapseddf2023$API<-NA

### 2024
collapseddf2024$Core<-NA
collapseddf2024$Friction<-NA
collapseddf2024$ClinInteropFx<-NA
collapseddf2024$DataUse<-NA
collapseddf2024$breadth_of_exchange<-NA
collapseddf2024$Friction<-NA
collapseddf2024$Barriers<-NA
collapseddf2024$Methods<-NA
collapseddf2024$InfoBlocking<-NA

### 2025
collapseddf2025$API <- NA
collapseddf2025$SDOH <- NA
collapseddf2025$CorePlus <- NA
collapseddf2025$PublicHealth <- NA
collapseddf2025$PatientEngagement <- NA
collapseddf2025$PE_Download <- NA
collapseddf2025$PE_Import <- NA
collapseddf2025$PE_Send <- NA
collapseddf2025$PE_API <- NA
collapseddf2025$PE_FHIR <- NA
collapseddf2025$PE_PGHDFHIR <- NA

## Smooth Core for 2022 and 2021 reflecting approach in paper. 
## Treats breadth as static from 2021-2023 since it is missing in 2022 and 2021.
breadth_smooth<-subset(collapseddf2023, select=c(breadth_of_exchange,characteristic,char_value)) #Extract breadth from 2023
collapseddf2022_smoothed<-merge(collapseddf2022, breadth_smooth, by=c("characteristic", "char_value")) # Merge breadth back in by hospital characteristic
collapseddf2022_smoothed$Core=(collapseddf2022_smoothed$breadth_of_exchange + collapseddf2022_smoothed$ClinInteropFx + collapseddf2022_smoothed$DataUse)/3*100 # Recalculate core by hospital characteristic

collapseddf2021_smoothBreadth<-merge(collapseddf2021, breadth_smooth, by=c("characteristic", "char_value"))
collapseddf2021_smoothBreadth$Core=(collapseddf2021_smoothBreadth$breadth_of_exchange + collapseddf2021_smoothBreadth$ClinInteropFx + collapseddf2021_smoothBreadth$DataUse)/3*100

##Smooth Patient Engagement
PGHD_smooth<-subset(collapseddf2022, select=c(PE_PGHDFHIR, characteristic,char_value)) #Extract breadth from 2022

collapseddf2021_smoothed<-merge(collapseddf2021_smoothBreadth, PGHD_smooth, by=c("characteristic", "char_value"))
collapseddf2021_smoothed$PatientEngagement=(collapseddf2021_smoothed$PE_Download + collapseddf2021_smoothed$PE_Import + collapseddf2021_smoothed$PE_Send + 
                                            collapseddf2021_smoothed$PE_API + collapseddf2021_smoothed$PE_FHIR + collapseddf2021_smoothed$PE_PGHDFHIR)/6

collapseddf2021_smoothed<-subset(collapseddf2021_smoothed, select= -c(PE_API, PE_Import, PE_Download, PE_FHIR, PE_PGHDFHIR, PE_Send))
collapseddf2022_smoothed<-subset(collapseddf2022_smoothed, select= -c(PE_API, PE_Import, PE_Download, PE_FHIR, PE_PGHDFHIR, PE_Send))
collapseddf2023<-subset(collapseddf2023, select= -c(PE_API, PE_Import, PE_Download, PE_FHIR, PE_PGHDFHIR, PE_Send))
collapseddf2024<-subset(collapseddf2024, select= -c(PE_API, PE_Import, PE_Download, PE_FHIR, PE_PGHDFHIR, PE_Send))
collapseddf2025<-subset(collapseddf2025, select= -c(PE_API, PE_Import, PE_Download, PE_FHIR, PE_PGHDFHIR, PE_Send))

combineddf<-rbind(collapseddf2021_smoothed, collapseddf2022_smoothed, collapseddf2023, collapseddf2024, collapseddf2025)

# Blank out missing from paper
## CorePlus
combineddf$CorePlus[combineddf$year==2021]<-NA
combineddf$CorePlus[combineddf$year==2023]<-NA

## Breadth
combineddf$breadth_of_exchange[combineddf$year==2021]<-NA
combineddf$breadth_of_exchange[combineddf$year==2022]<-NA

## Retain friction even though missing in paper.

## Convert to 0 to 100
combineddf$ClinInteropFx<-combineddf$ClinInteropFx*100
combineddf$DataUse<-combineddf$DataUse*100
combineddf$Barriers<-combineddf$Barriers*100
combineddf$Methods<-combineddf$Methods*100
combineddf$InfoBlocking<-combineddf$InfoBlocking*100
combineddf$PublicHealth<-combineddf$PublicHealth*100
combineddf$PatientEngagement<-combineddf$PatientEngagement*100
combineddf$API<-combineddf$API*100
combineddf$SDOH<-combineddf$SDOH*100
combineddf$breadth_of_exchange<-combineddf$breadth_of_exchange*100
combineddf$year<-as.character(combineddf$year)
save(combineddf, file="C:/Users/alec.blase/OneDrive - HHS Office of the Secretary/Desktop/hospital-index-app/data/trend_df.RData")
