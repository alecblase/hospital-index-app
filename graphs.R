library(tidyr)
library(ggrepel)

hello_world <- function() {
  print("App running!")
}
x_limits <- c("2021", "2022", "2023", "2024", "2025")

trendline <-function(index, dataI, IndexComponent) {
  
  df<-dataI
  if(index=="Overall") {
    df<-subset(df, characteristic=="Overall", select=c(Core,CorePlus,Friction, year))
    df_long <- pivot_longer(df, cols = c(Core,CorePlus,Friction), names_to = "variable", values_to = "value")
    legendLabels <- IndexComponent$Index[IndexComponent$Index %in% c("Core", "Pathfinder", "Friction")]
    legendLabels<-unique(legendLabels)
  }
  else if (index=="Core") {
    df<-subset(df, characteristic=="Overall", select=c(ClinInteropFx,DataUse,breadth_of_exchange, year))
    df<-df%>%
      rename("Clinical Interoperability"=ClinInteropFx,
             "Data Availability & Use"=DataUse,
             "Breadth of Exchange"=breadth_of_exchange)
    
    df_long <- pivot_longer(df, cols = c("Clinical Interoperability","Data Availability & Use","Breadth of Exchange")
                            , names_to = "variable", values_to = "value")
      }
  
  else if (index=="Pathfinder") {
    df<-subset(df, characteristic=="Overall", select=c(PublicHealth,SDOH,API,PatientEngagement, year))
    
    df<-df%>%
      rename("Public Health"=PublicHealth,
             "Social Determinants of Health"=SDOH,
             "Clinician/Health System API"=API,
             "Patient Engagement"=PatientEngagement)
    df_long <- pivot_longer(df, cols = c("Public Health","Social Determinants of Health","Clinician/Health System API","Patient Engagement"),
                            names_to = "variable", values_to = "value")
      }
  
  else if (index=="Friction") {
    df<-subset(df, characteristic=="Overall", select=c(Barriers,Methods,InfoBlocking,year))
    
    df<-df%>%
      rename("Barriers to Exchange"=Barriers,
             "Multiple Methods to Exchange"=Methods,
             "Information Blocking"=InfoBlocking)

    df_long <- pivot_longer(df, cols = c("Barriers to Exchange","Multiple Methods to Exchange","Information Blocking"), names_to = "variable", values_to = "value")
      }

df_long<-df_long[!is.na(df_long$value),]
  ggplot(df_long, aes(x = year, y = value, color = variable, group = variable)) +
    geom_line(linewidth = 1.2) +
    geom_point(shape = 15, size = 3) +  # shape 15 = square
    geom_text_repel(aes(label = round(value)), vjust = -0.8, size = 6, show.legend = FALSE, color="black") +  # Add data labels
    labs(title = paste(index, "Index Trend Over Time"),
         x = "Year",
         y = "Index Score",
         color = NULL) +
    scale_x_discrete(limits = x_limits) +
    scale_y_continuous(limits = c(0,100), expand=c(0,0)) +
    scale_color_manual(values = c("red", "blue", "green", "yellow")) +
    theme_minimal(base_size=16) +
    theme(legend.position = "bottom")
}


trendline_characteristic <-function(indexInput, compInput, charInput, dataInput, IndexComponent) {
  df<-dataInput
  legendTitle<-LegendTitle(charInput)

  #Replace component with index value if it is composite for graphing.  
    if (compInput=="Composite") {
    compInput = indexInput
    }
  
  #Rename component if overall graph. 
  #The Overall input is only set to look across indices, not hospital characteristics. 
  #Input gets reset to Core.
  if(indexInput=="Overall") {
    compInput="Core"
  }
  
  
  graphTitle<-indexInput
  print(compInput)
  if(compInput!=indexInput) {
    graphTitle <- IndexComponent$Label[IndexComponent$Component==compInput]
  } 
  
  if (compInput=="CorePlus") {
    graphTitle<-"Pathfinder"
  }
  
  df<-df[!is.na(df[[compInput]]),]
  
p<-ggplot(subset(df, characteristic==charInput), aes(x = year, y = .data[[compInput]], color = char_value, group = char_value)) +
    geom_line(linewidth = 1.2) +
    geom_point(shape = 15, size = 3) +  # shape 15 = square
    geom_text_repel(aes(label = round(.data[[compInput]])), vjust = -0.8, size = 6, show.legend = FALSE, color="black") +  # Add data labels
    labs(title = paste(graphTitle, "Index by", legendTitle),
         x = "Year",
         y = "Index Score",
         color = legendTitle) +
    scale_y_continuous(limits = c(0,100), expand=c(0,0)) +
    theme_minimal(base_size=16) +
    theme(legend.position = "bottom")

# Adjust legend labels based on charInput
if (charInput %in% c("sysMemb", "cah")) {
  p <- p + scale_color_manual(values = c("0" = "#1f77b4", "1" = "#ff7f0e"),
                              labels = c("0" = "No", "1" = "Yes"))
} else if (charInput == "hospSize") {
  p <- p + scale_color_manual(values = c("0" = "#1f77b4", "1" = "#ff7f0e", "2" = "#2ca02c"),
                              labels = c("0" = "Small", "1" = "Medium", "2" = "Large"))
}

return(p)
}

#trendline("friction", "Overall")

distribution<-function(value, charInput, dataI, IndexComponent) {
# Subset for the bar plot (excluding "Mean" bin)
legendTitle<-LegendTitle(charInput)
df_bars <- subset(dataI, characteristic == charInput & measure == value & bin != "Mean")

# Subset for the mean line and label (bin == "Mean")
df_mean <- subset(dataI, characteristic == charInput & measure == value & bin == "Mean")


#Create Graph Title
graphTitle<-value
print(value)

if (value %in% IndexComponent$Component) {
  graphTitle <- IndexComponent$Label[IndexComponent$Component==value]
} else if(value=="CorePlus") {
  graphTitle<-"Pathfinder"
} else {
  graphTitle<-value
}


# Create a named vector for facet labels
if (charInput %in% c("sysMemb", "cah")) {
  facet_labels <- c("0" = "No", "1" = "Yes")
} else if (charInput == "hospSize") {
  facet_labels <- c("0" = "Small", "1" = "Medium", "2" = "Large")
} else {
  facet_labels <- NULL  # Default to showing raw values if no mapping provided
}

# Custom labeller for facets
facet_labeller <- labeller(char_value = facet_labels)


p<-ggplot(df_bars, aes(y = hospital_percent, x = bin)) +
  geom_bar(stat = "identity") +
  geom_vline(data = df_mean, aes(xintercept = weighted_mean / 10), 
             color = "red", linetype = "dashed", linewidth = 1) +
  geom_text(data = subset(df_bars, hospital_percent > 0),
            aes(label = paste0(round(hospital_percent * 100), "%")), 
            vjust = -0.8, size = 3, show.legend = FALSE) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand=c(0,0)) +

  facet_grid(rows = vars(char_value), labeller = facet_labeller) +
  # Add mean label near the vertical line
  geom_text(
    data = df_mean,
    aes(
      x = weighted_mean / 10, 
      y = 0, 
      label = paste0("Mean: ", round(weighted_mean), "%")
    ),
    color = "red",
    angle = 90,
    vjust = -0.5,
    hjust = -2
  ) + 
  labs(title = paste(graphTitle, "by", legendTitle),
       x = "Score Range",
       y = "Percent of Hospitals",
       color = legendTitle) +
  theme_minimal(base_size=16)

return(p)

}



LegendTitle<-function(charInput) {
  charInputOptions<-c("hospSize", "cah","sysMemb","cbsatype","ehr")
  charLabel<-c("Hospital Size","Critical Access Status", "Hospital System Membership","Location","EHR System")
  labeldf<-data.frame(charLabel, charInputOptions)
  legendTitle<-labeldf$charLabel[labeldf$charInputOptions==charInput]
  return(legendTitle)
}


#ggplot(subset(distributionSummaryFinal, characteristic=="hospSize" & decile!="Mean"), aes(x=factor(decile), y=Core)) +
#  geom_bar(stat = "identity") +
#  geom_vline(data = subset(distributionSummaryFinal, characteristic=="hospSize" & decile=="Mean"), aes(xintercept = Core/10), 
#             color = "red", linetype = "dashed", linewidth = 1) +
#  geom_text(aes(label = round(Core)), vjust = -0.8, size = 3, show.legend = FALSE) + # Add data labels
#  facet_grid(rows = vars(char_value))

