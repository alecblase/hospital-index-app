library(tidyr)
library(dplyr)
library(readxl)
library(purrr)
library(rlang)

# Set wd to project folder destination
setwd("C:/Users/alec.blase/OneDrive - HHS Office of the Secretary/Desktop/hospital-index-app")

CreateHistogramDataByCharacteristic <- function(dfInput, column, charInput) {
  library(dplyr)
  library(tidyr)
  library(rlang)
  
  # Tidy evaluation setup
  column_sym <- enquo(column)
  charInput_sym <- sym(charInput)
  
  # Define fixed breaks for bins: 0, 10, ..., 100
  breaks <- seq(0, 100, by = 10)
  bin_labels <- paste0(breaks[-length(breaks)], "-", breaks[-1])
  
  # Prepare data
  df <- dfInput %>%
    select(id, year, weight, !!column_sym, !!charInput_sym) %>%
    mutate(
      bin = cut(!!column_sym, breaks = breaks, include.lowest = TRUE, right = FALSE,
                labels = bin_labels)
    )
  
  # Weighted mean function
  weighted_mean <- function(x, w) {
    if (all(is.na(x))) return(NA_real_)
    sum(x * w, na.rm = TRUE) / sum(w[!is.na(x)], na.rm = TRUE)
  }
  
  # Summarize
  summary <- df %>%
    group_by(bin, !!charInput_sym) %>%
    summarise(
      weighted_mean = weighted_mean(!!column_sym, weight),
      hospital_count = n(),
      .groups = "drop"
    )
  
  # Ensure all combinations of bins and char_value exist
  all_combos <- expand_grid(
    bin = factor(bin_labels, levels = bin_labels),
    char_value = unique(as.character(df[[as_string(charInput_sym)]]))
  )
  
  summary_full <- all_combos %>%
    left_join(summary %>%
                mutate(char_value = as.character(!!charInput_sym)),
              by = c("bin", "char_value")) %>%
    mutate(
      hospital_count = replace_na(hospital_count, 0),
      characteristic = charInput
    )
  
  return(summary_full)
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

measures <- c("Core", "ClinInteropFx", "DataUse", "breadth_of_exchange", "Friction", "Barriers", "Methods", "InfoBlocking")
characteristics <- c("hospSize", "cah", "ehr", "cbsatype", "sysMemb")
scale_measures <- c("ClinInteropFx", "DataUse", "breadth_of_exchange","Barriers", "Methods", "InfoBlocking")

df <- df %>%
  mutate(across(all_of(measures), 
                ~ if (cur_column() %in% scale_measures) .x * 100 else .x))

summary2023 <- cross2(measures, characteristics) %>%
  map_df(~ {
    measure <- .x[[1]]
    char <- .x[[2]]
    
    # Run your existing function for each measure-characteristic pair
    df_res <- CreateHistogramDataByCharacteristic(df, !!sym(measure), char)
    
    # Add columns to identify measure and characteristic type
    df_res %>%
      mutate(
        measure = measure,
        characteristic_type = char
      )
  }) %>%
  mutate(year = "2023")%>%
  mutate(
    weighted_mean = if_else(measure %in% scale_measures, weighted_mean * 100, weighted_mean)
  )
summary2023<-subset(summary2023, select=c(-cah,-ehr,-cbsatype,-sysMemb,-hospSize, -characteristic_type))

hospTotals2023 <- summary2023 %>%
  group_by(measure, characteristic, char_value) %>%
  summarise(total_hospitals = sum(hospital_count, na.rm = TRUE), .groups = "drop")

summary2023<-merge(summary2023,hospTotals2023, by=c("measure","characteristic","char_value"))
summary2023<-subset(summary2023, summary2023$hospital_count!=summary2023$total_hospitals)
summary2023$hospital_percent=summary2023$hospital_count/summary2023$total_hospitals

# For 2024
df2024 <- read_excel("data/Interop Index Export App 2024.xls")

df<-CreateCategories(df2024)

measures <- c("CorePlus", "PublicHealth", "PatientEngagement", "SDOH", "API")
characteristics <- c("hospSize", "cah", "ehr", "cbsatype", "sysMemb")
scale_measures <- c("PublicHealth", "PatientEngagement", "SDOH","API")

df <- df %>%
  mutate(across(all_of(measures),
                ~ if (cur_column() %in% scale_measures) .x * 100 else .x))

summary2024 <- cross2(measures, characteristics) %>%
  map_df(~ {
    measure <- .x[[1]]
    char <- .x[[2]]
    
    # Run your existing function for each measure-characteristic pair
    df_res <- CreateHistogramDataByCharacteristic(df, !!sym(measure), char)
    
    # Add columns to identify measure and characteristic type
    df_res %>%
      mutate(
        measure = measure,
        characteristic_type = char
      )
  }) %>%
  mutate(year = "2024") %>%
  mutate(
    weighted_mean = if_else(measure %in% scale_measures, weighted_mean * 100, weighted_mean)
  )
summary2024<-subset(summary2024, select=c(-cah,-ehr,-cbsatype,-sysMemb,-hospSize, -characteristic_type))

hospTotals2024 <- summary2024 %>%
  group_by(measure, characteristic, char_value) %>%
  summarise(total_hospitals = sum(hospital_count, na.rm = TRUE), .groups = "drop")

summary2024<-merge(summary2024,hospTotals2024, by=c("measure","characteristic","char_value"))
summary2024<-subset(summary2024, summary2024$hospital_count!=summary2024$total_hospitals)
summary2024$hospital_percent=summary2024$hospital_count/summary2024$total_hospitals

# For 2025
df2025 <- read_excel("data/Interop Index Export App 2025.xls")
df2025$mhsmemb <- na_if(df2025$mhsmemb, "NA") # Manually replacing string NA's from xls file with NULL
df2025 <- df2025 %>% rename(id = aha_id)
df<-CreateCategories(df2025)

measures <- c("Core", "ClinInteropFx", "DataUse", "breadth_of_exchange", "Friction", "Barriers", "Methods", "InfoBlocking")
characteristics <- c("hospSize", "cah", "ehr", "cbsatype", "sysMemb")
scale_measures <- c("ClinInteropFx", "DataUse", "breadth_of_exchange","Barriers", "Methods", "InfoBlocking")

df <- df %>%
  mutate(across(all_of(measures), 
                ~ if (cur_column() %in% scale_measures) .x * 100 else .x))

summary2025 <- cross2(measures, characteristics) %>%
  map_df(~ {
    measure <- .x[[1]]
    char <- .x[[2]]
    
    # Run your existing function for each measure-characteristic pair
    df_res <- CreateHistogramDataByCharacteristic(df, !!sym(measure), char)
    
    # Add columns to identify measure and characteristic type
    df_res %>%
      mutate(
        measure = measure,
        characteristic_type = char
      )
  }) %>%
  mutate(year = "2025")%>%
  mutate(
    weighted_mean = if_else(measure %in% scale_measures, weighted_mean * 100, weighted_mean)
  )
summary2025<-subset(summary2025, select=c(-cah,-ehr,-cbsatype,-sysMemb,-hospSize, -characteristic_type))

hospTotals2025 <- summary2025 %>%
  group_by(measure, characteristic, char_value) %>%
  summarise(total_hospitals = sum(hospital_count, na.rm = TRUE), .groups = "drop")

summary2025<-merge(summary2025,hospTotals2025, by=c("measure","characteristic","char_value"))
summary2025<-subset(summary2025, summary2025$hospital_count!=summary2025$total_hospitals)
summary2025$hospital_percent=summary2025$hospital_count/summary2025$total_hospitals

# Combine all years
distributionSummary<-rbind(summary2023, summary2024, summary2025)

# Load dataset from RData generated from 'Interop App Data Import.R'
load("C:/Users/alec.blase/OneDrive - HHS Office of the Secretary/Desktop/hospital-index-app/data/trend_df.RData")
combineddfForDistribution<-combineddf
combineddfForDistribution$bin<-"Mean"
combineddfForDistribution<-subset(combineddfForDistribution, year=="2023" | year=="2024" | year=="2025")

df_long <- combineddfForDistribution %>%
  pivot_longer(
    cols = c(Core, Friction, ClinInteropFx, DataUse,Barriers,Methods,InfoBlocking,CorePlus,PublicHealth,PatientEngagement,API,SDOH,breadth_of_exchange),  # columns to reshape
    names_to = "measure",                     # new column for names
    values_to = "weighted_mean"                       # new column for values
  )



df_long<-subset(df_long, !is.na(weighted_mean))
df_long$hospital_count<-1
df_long$total_hospitals<-1
df_long$hospital_percent<-1

df_long<-subset(df_long, year=="2023" | year=="2024" | year=="2025")
df_long<-subset(df_long, !(year=="2023" & (measure=="PublicHealth" | measure=="SDOH" | measure=="PatientEngagement")))

distributionSummaryFinal<-rbind(distributionSummary,df_long)

#new_order <- c("1", "2", "3", "4", "5", "6","7","8","9","10","Mean")
#distributionSummaryFinal$bin<- forcats::fct_relevel(distributionSummaryFinal$bin, new_order)
save(distributionSummaryFinal, file="C:/Users/alec.blase/OneDrive - HHS Office of the Secretary/Desktop/hospital-index-app/data/histogram_df.RData")
#Overall<-subset(distributionSummaryFinal, distributionSummaryFinal$measure=="Overall")
