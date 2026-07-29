library(tidyr)
library(dplyr)
library(readxl)

#Set wd to project folder destination
setwd("C:/Users/alec.blase/OneDrive - HHS Office of the Secretary/Desktop/hospital-index-app")

CreateDataSetDecile<-function(df, column) {
  # Use tidy evaluation to allow programming with dplyr
  column_sym <- enquo(column)
  column_name <- quo_name(column_sym)
  df <- df %>%
    mutate(decile = ntile(!!column_sym, 10))
  
  # Weighted mean function
  weighted_mean <- function(x, w) {
    if (all(is.na(x))) return(NA_real_) #If everything is NA, return NA
    sum(x * w, na.rm = TRUE) / sum(w[!is.na(x)], na.rm = TRUE) #Aply weight, divide by # not NA.
  }
  
  # Apply weighted mean with across
  summary <- df %>%
    group_by(decile) %>%
    summarise(
      !!column_name := weighted_mean(!!column_sym, weight),
      .groups = "drop"
    ) %>%
    mutate(decile = as.character(decile))
  
  summary$characteristic<-"Overall"
  summary$char_value<-"1"
  return(summary)
}

CreateDataSetDecileCharacteristic<-function(dfInput, column, charInput) {
  # Use tidy evaluation to allow programming with dplyr
  column_sym <- enquo(column)
  column_name <- quo_name(column_sym)
  charInput_sym <- sym(charInput)
  
  df<-dfInput%>%
    select(id, year, weight, !!column_sym, !!charInput_sym)

  #Assign decile value  
  df <- df %>%
    group_by(!!charInput_sym) %>%
    mutate(decile = ntile(!!column_sym, 10))

  # Weighted mean function
  weighted_mean <- function(x, w) {
    if (all(is.na(x))) return(NA_real_) #If everything is NA, return NA
    sum(x * w, na.rm = TRUE) / sum(w[!is.na(x)], na.rm = TRUE) #Aply weight, divide by # not NA.
  }
  # Apply weighted mean
  summary <- df %>%
    group_by(decile, !!charInput_sym) %>%
    summarise(
      !!column_name := weighted_mean(!!column_sym, weight),
      .groups = "drop"
    ) %>%
    mutate(decile = as.character(decile),
           characteristic = charInput,
           char_value = as.character(!!charInput_sym))
  
  return(summary)
}

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

# For 2023
df2023 <- read_excel("data/Interop Index Export App 2023.xls")

df<-CreateCategories(df2023)

results <- list()
measures<-c("Core", "ClinInteropFx", "DataUse", "breadth_of_exchange", "Friction", "Barriers", "Methods", "InfoBlocking")
for (x in measures) {
  assign(paste0(x, "Size"), CreateDataSetDecileCharacteristic(df,!!sym(x),"hospSize"))
  assign(paste0(x, "CAH"), CreateDataSetDecileCharacteristic(df, !!sym(x), "cah"))
  assign(paste0(x, "EHR"), CreateDataSetDecileCharacteristic(df, !!sym(x), "ehr"))
  assign(paste0(x, "Location"), CreateDataSetDecileCharacteristic(df, !!sym(x), "cbsatype"))
  assign(paste0(x, "SysMemb"), CreateDataSetDecileCharacteristic(df, !!sym(x), "sysMemb"))
}
CAH_list <- lapply(measures, function(m) get(paste0(m, "CAH")))
CAH_merged <- Reduce(function(x, y) merge(x, y, by = c("characteristic", "char_value", "decile", "cah")), CAH_list)

Size_list <- lapply(measures, function(m) get(paste0(m, "Size")))
Size_merged <- Reduce(function(x, y) merge(x, y, by = c("characteristic", "char_value", "decile", "hospSize")), Size_list)

EHR_list <- lapply(measures, function(m) get(paste0(m, "EHR")))
EHR_merged <- Reduce(function(x, y) merge(x, y, by = c("characteristic", "char_value", "decile", "ehr")), EHR_list)

Location_list <- lapply(measures, function(m) get(paste0(m, "Location")))
Location_merged <- Reduce(function(x, y) merge(x, y, by = c("characteristic", "char_value", "decile", "cbsatype")), Location_list)

SysMemb_list <- lapply(measures, function(m) get(paste0(m, "SysMemb")))
SysMemb_merged <- Reduce(function(x, y) merge(x, y, by = c("characteristic", "char_value", "decile", "sysMemb")), SysMemb_list)

CAH_merged<-subset(CAH_merged, select = -cah)
SysMemb_merged<-subset(SysMemb_merged, select = -sysMemb)
Location_merged<-subset(Location_merged, select = -cbsatype)
EHR_merged<-subset(EHR_merged, select = -ehr)
Size_merged<-subset(Size_merged, select = -hospSize)

summary2023<-rbind(CAH_merged, Location_merged, EHR_merged, SysMemb_merged, Size_merged)
summary2023$year<-"2023"

# For 2024
df2024 <- read_excel("Data/Interop Index Export App 2024.xls")

df<-CreateCategories(df2024)

results <- list()
measures<-c("CorePlus", "PublicHealth", "PatientEngagement", "SDOH", "API")
for (x in measures) {
  assign(paste0(x, "Size"), CreateDataSetDecileCharacteristic(df,!!sym(x),"hospSize"))
  assign(paste0(x, "CAH"), CreateDataSetDecileCharacteristic(df, !!sym(x), "cah"))
  assign(paste0(x, "EHR"), CreateDataSetDecileCharacteristic(df, !!sym(x), "ehr"))
  assign(paste0(x, "Location"), CreateDataSetDecileCharacteristic(df, !!sym(x), "cbsatype"))
  assign(paste0(x, "SysMemb"), CreateDataSetDecileCharacteristic(df, !!sym(x), "sysMemb"))
}
CAH_list <- lapply(measures, function(m) get(paste0(m, "CAH")))
CAH_merged <- Reduce(function(x, y) merge(x, y, by = c("characteristic", "char_value", "decile", "cah")), CAH_list)

Size_list <- lapply(measures, function(m) get(paste0(m, "Size")))
Size_merged <- Reduce(function(x, y) merge(x, y, by = c("characteristic", "char_value", "decile", "hospSize")), Size_list)

EHR_list <- lapply(measures, function(m) get(paste0(m, "EHR")))
EHR_merged <- Reduce(function(x, y) merge(x, y, by = c("characteristic", "char_value", "decile", "ehr")), EHR_list)

Location_list <- lapply(measures, function(m) get(paste0(m, "Location")))
Location_merged <- Reduce(function(x, y) merge(x, y, by = c("characteristic", "char_value", "decile", "cbsatype")), Location_list)

SysMemb_list <- lapply(measures, function(m) get(paste0(m, "SysMemb")))
SysMemb_merged <- Reduce(function(x, y) merge(x, y, by = c("characteristic", "char_value", "decile", "sysMemb")), SysMemb_list)

CAH_merged<-subset(CAH_merged, select = -cah)
SysMemb_merged<-subset(SysMemb_merged, select = -sysMemb)
Location_merged<-subset(Location_merged, select = -cbsatype)
EHR_merged<-subset(EHR_merged, select = -ehr)
Size_merged<-subset(Size_merged, select = -hospSize)

summary2024<-rbind(CAH_merged, Location_merged, EHR_merged, SysMemb_merged, Size_merged)
summary2024$year<-"2024"

# For 2025, use same measures as 2023
df2025 <- read_excel("Data/Interop Index Export App 2025.xls")
df2025$mhsmemb <- na_if(df2025$mhsmemb, "NA") # Manually replacing string NA's from xls file with NULL
df2025 <- df2025 %>% rename(id = aha_id)
df<-CreateCategories(df2025)

results <- list()
measures<-c("Core", "ClinInteropFx", "DataUse", "breadth_of_exchange", "Friction", "Barriers", "Methods", "InfoBlocking")
for (x in measures) {
  assign(paste0(x, "Size"), CreateDataSetDecileCharacteristic(df,!!sym(x),"hospSize"))
  assign(paste0(x, "CAH"), CreateDataSetDecileCharacteristic(df, !!sym(x), "cah"))
  assign(paste0(x, "EHR"), CreateDataSetDecileCharacteristic(df, !!sym(x), "ehr"))
  assign(paste0(x, "Location"), CreateDataSetDecileCharacteristic(df, !!sym(x), "cbsatype"))
  assign(paste0(x, "SysMemb"), CreateDataSetDecileCharacteristic(df, !!sym(x), "sysMemb"))
}
CAH_list <- lapply(measures, function(m) get(paste0(m, "CAH")))
CAH_merged <- Reduce(function(x, y) merge(x, y, by = c("characteristic", "char_value", "decile", "cah")), CAH_list)

Size_list <- lapply(measures, function(m) get(paste0(m, "Size")))
Size_merged <- Reduce(function(x, y) merge(x, y, by = c("characteristic", "char_value", "decile", "hospSize")), Size_list)

EHR_list <- lapply(measures, function(m) get(paste0(m, "EHR")))
EHR_merged <- Reduce(function(x, y) merge(x, y, by = c("characteristic", "char_value", "decile", "ehr")), EHR_list)

Location_list <- lapply(measures, function(m) get(paste0(m, "Location")))
Location_merged <- Reduce(function(x, y) merge(x, y, by = c("characteristic", "char_value", "decile", "cbsatype")), Location_list)

SysMemb_list <- lapply(measures, function(m) get(paste0(m, "SysMemb")))
SysMemb_merged <- Reduce(function(x, y) merge(x, y, by = c("characteristic", "char_value", "decile", "sysMemb")), SysMemb_list)

CAH_merged<-subset(CAH_merged, select = -cah)
SysMemb_merged<-subset(SysMemb_merged, select = -sysMemb)
Location_merged<-subset(Location_merged, select = -cbsatype)
EHR_merged<-subset(EHR_merged, select = -ehr)
Size_merged<-subset(Size_merged, select = -hospSize)

summary2025<-rbind(CAH_merged, Location_merged, EHR_merged, SysMemb_merged, Size_merged)
summary2025$year<-"2025"

# Combine all
merged_df <- merge(summary2023, summary2025, all = T)
distributionSummary<-merge(merged_df, summary2024, c("characteristic", "char_value", "decile","year"), all=T)

distributionSummary$ClinInteropFx<-distributionSummary$ClinInteropFx*100
distributionSummary$DataUse<-distributionSummary$DataUse*100
distributionSummary$Barriers<-distributionSummary$Barriers*100
distributionSummary$Methods<-distributionSummary$Methods*100
distributionSummary$InfoBlocking<-distributionSummary$InfoBlocking*100
distributionSummary$PublicHealth<-distributionSummary$PublicHealth*100
distributionSummary$PatientEngagement<-distributionSummary$PatientEngagement*100
distributionSummary$API<-distributionSummary$API*100
distributionSummary$SDOH<-distributionSummary$SDOH*100
distributionSummary$breadth_of_exchange<-distributionSummary$breadth_of_exchange*100

# Load dataset from RData generated from 'Interop App Data Import.R'
load("C:/Users/alec.blase/OneDrive - HHS Office of the Secretary/Desktop/hospital-index-app/data/trend_df.RData")
combineddfForDistribution<-combineddf
combineddfForDistribution$decile<-"Mean"
combineddfForDistribution<-subset(combineddfForDistribution, year=="2023" | year=="2024" | year=="2025")

# Impute NA for years that CorePlus items aren't calculated
combineddfForDistribution$CorePlus[combineddfForDistribution$year=="2023"]<-NA
combineddfForDistribution$API[combineddfForDistribution$year=="2023"]<-NA
combineddfForDistribution$SDOH[combineddfForDistribution$year=="2023"]<-NA
combineddfForDistribution$PatientEngagement[combineddfForDistribution$year=="2023"]<-NA
combineddfForDistribution$PublicHealth[combineddfForDistribution$year=="2023"]<-NA

combineddfForDistribution$CorePlus[combineddfForDistribution$year=="2025"]<-NA
combineddfForDistribution$API[combineddfForDistribution$year=="2025"]<-NA
combineddfForDistribution$SDOH[combineddfForDistribution$year=="2025"]<-NA
combineddfForDistribution$PatientEngagement[combineddfForDistribution$year=="2025"]<-NA
combineddfForDistribution$PublicHealth[combineddfForDistribution$year=="2025"]<-NA

distributionSummaryFinal<-rbind(distributionSummary,combineddfForDistribution)
new_order <- c("1", "2", "3", "4", "5", "6","7","8","9","10","Mean")
distributionSummaryFinal$decile<- forcats::fct_relevel(distributionSummaryFinal$decile, new_order)
save(distributionSummaryFinal, file="C:/Users/alec.blase/OneDrive - HHS Office of the Secretary/Desktop/hospital-index-app/data/distribution_df.RData")
