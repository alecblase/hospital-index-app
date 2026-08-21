#install.packages("shinyjs")
#install.packages("bslib", dependencies=TRUE)
#install.packages("fastmap")
library(bslib)
library(shiny)
library(ggplot2)
library(shinyjs)
library(DT)
library(dplyr)


source("graphs.R")
hello_world()

##Load Data
load("data/trend_df.RData")
load("data/histogram_df.RData")
load("data/index_items_df.RData")
load("data/IndexComponentCrosswalk.RData")
load("data/ItemComponentCrosswalk.RData")

# subset distribution dataset for only most current year
distributionSummaryFinal <- subset(distributionSummaryFinal, year=="2025")

ui <- page_sidebar(
  shinyjs::useShinyjs(), # newly added
  tags$head(
    tags$style(HTML("
      .shiny-options-group label {
        font-size: 14px;  /* Adjust this size as needed */
      }
    "))
  ),
  # App title ----
  title = "Hospital Interoperability Index",
  # Sidebar panel for inputs ----
  sidebar = sidebar(
    selectInput(
      "index",
      "Select Index",
      choices = list("Overall" = "Overall", "Core" = "Core", "Pathfinder" = "Pathfinder", "Friction" = "Friction"),
      selected = 1
    ),

  radioButtons(
    "comparison",
    "Comparison",
    choices = list("Across Index Components" = 1, "By Hospital Characteristic" = 2, "Individual Index Items" = 3)
  ),
  radioButtons(
    "graphType",
    "Graph Type",
    choices = list("Trend over time" = 1, "Current distribution" = 2)
  ),
  selectInput(
    "component",
    "Component",
    choices = list("Clinical Interop" = "ClinInteropFx", "Information Blocking" = "InfoBlocking", "SDOH" = "SDOH", "Choice 3" = 3),
    selected = 1
  ),
  selectInput(
    "characteristic",
    "Hospital Characteristic",
    choices = list("Overall" = "Overall", "Size" = "hospSize", "Critical Access Hospital" = "cah", "Hospital System Member" = "sysMemb", "Location" = "cbsatype", "EHR" = "ehr"),
    selected = 1
  ),
  tags$script(HTML("
    Shiny.addCustomMessageHandler('disableSelect', function(value) {
      $('#characteristic').prop('disabled', value);
    });
  "))
  ),
  uiOutput("mainOutput", height = "600px", width = "100%")
)

server <- function(input, output, session) {
  
  session$sendCustomMessage("disableSelect", TRUE)

    limitComponent <- reactive({
    subsetComponent <- subset(IndexComponent, IndexComponent$Index==input$index)
    return(c("Choose One" = "", unique(as.character(subsetComponent$Label))))
  })

  observeEvent(input$comparison, {
    if (input$comparison == "1") {
      shinyjs::disable('characteristic') 
      shinyjs::disable('component') 
      indexChoices<-list("Overall" = "Overall", "Core" = "Core", "Pathfinder" = "Pathfinder", "Friction" = "Friction")
      current_selection <- input$index
      updateSelectInput(session,"index", choices = indexChoices,
                        selected = if (current_selection %in% indexChoices) current_selection else "Core")
      
      characteristicChoices<-list("Overall"="Overall", "Size" = "hospSize", "Critical Access Hospital" = "cah", "Hospital System Member" = "sysMemb", "Location" = "cbsatype", "EHR" = "ehr")
      current_selection <- input$characteristic
      valid_choices <- characteristicChoices[characteristicChoices != ""]
      updateSelectInput(session,"characteristic", choices = characteristicChoices,
                        selected = if (current_selection %in% characteristicChoices) current_selection else "hospSize")
      
      
    } else if (input$comparison == "2") {
      if(input$index=="Overall") {
        updateSelectInput(session, "index", 
                        selected = "Core")
      } 
      
      #Update Index based on comparison type - exclude "Overall" for comparison across hospital characteristics.
      
      indexChoices<-list("Core" = "Core", "Pathfinder" = "Pathfinder", "Friction" = "Friction")
      current_selection <- input$index
      valid_choices <- indexChoices[indexChoices != ""]
      updateSelectInput(session,"index", choices = indexChoices,
                        selected = if (current_selection %in% indexChoices) current_selection else "Core")
      
      characteristicChoices<-list("Size" = "hospSize", "Critical Access Hospital" = "cah", "Hospital System Member" = "sysMemb", "Location" = "cbsatype", "EHR" = "ehr")
      current_selection <- input$characteristic
      valid_choices <- characteristicChoices[characteristicChoices != ""]
      updateSelectInput(session,"characteristic", choices = characteristicChoices,
                        selected = if (current_selection %in% characteristicChoices) current_selection else "hospSize")
      
      shinyjs::enable('characteristic') 
      shinyjs::enable('component')
    } 
    else if(input$comparison=="3") {
      shinyjs::enable('characteristic') 
      shinyjs::enable('component')    
      
      indexChoices<-list("Overall" = "Overall", "Core" = "Core", "Pathfinder" = "Pathfinder", "Friction" = "Friction")
      current_selection <- input$index
      valid_choices <- indexChoices[indexChoices != ""]
      
      updateSelectInput(session,"index", choices = indexChoices,
                        selected = if (current_selection %in% indexChoices) current_selection else "Core")
      
      characteristicChoices<-list("Overall"="Overall", "Size" = "hospSize", "Critical Access Hospital" = "cah", "Hospital System Member" = "sysMemb", "Location" = "cbsatype", "EHR" = "ehr")
      current_selection <- input$characteristic
      valid_choices <- characteristicChoices[characteristicChoices != ""]
      updateSelectInput(session,"characteristic", choices = characteristicChoices,
                        selected = if (current_selection %in% characteristicChoices) current_selection else "hospSize")
      
    } 
  })

  observe({

    # 1. When comparison is changed to 1, update graphType to 1
    observeEvent(input$comparison, {
      if (input$comparison == 1 && input$graphType != 1) {
        updateRadioButtons(session, "graphType", selected = 1)
      }
    })
    
    # 2. When graphType is changed to 2, update comparison to 2
    observeEvent(input$graphType, {
      if (input$graphType == 2 && input$comparison != 2) {
        updateRadioButtons(session, "comparison", selected = 2)
      }
    })
    
    # Update Component choices based on Index
    choices <- limitComponent()
    # Remove the empty choice (if present)
    valid_choices <- choices[choices != ""]
    # Automatically select the first valid option
    first_choice <- if (length(valid_choices) > 0) valid_choices[1] else ""
    
    updateSelectInput(session, "component", choices = choices,
                      selected = first_choice)
    

  })
  
  output$mainOutput <- renderUI({
    if (input$comparison == 3) {
      DT::dataTableOutput("summaryTable")
    } else {
      plotOutput("distPlot", height="600px")
    }
  })
  
  output$distPlot <- renderPlot({
    
    getComponent <- function(index, label) {
      if (label == "Composite" || label == "") {
        return(ifelse(index == "Pathfinder", "CorePlus", index))
      }
      
      matched <- IndexComponent$Component[IndexComponent$Label == label]
      if (length(matched) == 0 || is.na(matched)) return("Core")
      
      return(matched)
    }
    
    if (input$graphType == 1) {
      # Index Comparison
      if (input$comparison == 1) {
        trendline(input$index, combineddf, IndexComponent)
        
        # Characteristic Comparison
      } else if (input$comparison == 2) {
        component <- getComponent(input$index, input$component)
        trendline_characteristic(input$index, component, input$characteristic, combineddf, IndexComponent)
      }
      
    } else if (input$graphType == 2) {
      component <- getComponent(input$index, input$component)
      distribution(component, input$characteristic, distributionSummaryFinal, IndexComponent)
    }
    
  })
  
  output$summaryTable <- renderDT({
    req(input$comparison == 3)
    
    df <- dfItem
    
    #Round numeric cells to 2 digits.
    num_cols <- sapply(df, is.numeric)
    df[num_cols] <- lapply(df[num_cols], function(x) round(x, 2))
    
    # Filter rows by hospital characteristic
    df <- df[df$characteristic==input$characteristic, ]
    
    if(input$component=="Composite") {
      cols_to_select <- itemComponentCrossWalk$item[itemComponentCrossWalk$index == input$index]
    }else if(input$index=="Overall") {
      cols_to_select<-itemComponentCrossWalk$item
    }
      else {
      cols_to_select <- itemComponentCrossWalk$item[itemComponentCrossWalk$component == input$component]
    }
    

    
    # Add additional columns to keep
    cols_to_keep <- c("Year", "characteristic", "char_value", cols_to_select)
    
    # Select those columns from dfItem
    df <- select(df, all_of(cols_to_keep))
    
    #Reorder columns
    df <- df %>%
      arrange(characteristic, char_value, Year)
    
    # Define the desired order of first columns
    first_cols <- c("char_value","Year")
    
    # Keep only columns that exist in df to avoid errors
    first_cols <- first_cols[first_cols %in% colnames(df)]

    # Put those first, then all other columns
    df <- df[, c(first_cols, setdiff(colnames(df), first_cols))]
    df <- select(df, -characteristic)
    
    if(input$characteristic=="hospSize")
    {
      df$char_value[df$char_value=="0"]<-"Small"
      df$char_value[df$char_value=="1"]<-"Medium"
      df$char_value[df$char_value=="2"]<-"Large"
      
      df<- df %>% rename("Hospital Size" = char_value)
    } else if (input$characteristic=="cah") {
      df$char_value[df$char_value=="0"]<-"No"
      df$char_value[df$char_value=="1"]<-"Yes"
      df<- df %>% rename("Critical Access Status" = char_value)
    } else if (input$characteristic=="sysMemb") {
      df<- df %>% rename("Health System Member" = char_value)
      df$char_value[df$char_value=="0"]<-"No"
      df$char_value[df$char_value=="1"]<-"Yes"
    } else if (input$characteristic=="cbsatype") {
      df<- df %>% rename("Location" = char_value)
    } else if (input$characteristic=="ehr") {
      df<- df %>% rename("EHR" = char_value)
    }
    
    
    
    datatable(df, options = list(pageLength = 25, scrollX = TRUE,columnDefs = list(list(className = 'dt-center', targets = "_all"))))
  })
  
}

shinyApp(ui = ui, server = server)
