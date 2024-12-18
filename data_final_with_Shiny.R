library(readxl)
library(dplyr)



file_paths <- list.files(path = "C:/Users/ahsanm8/Desktop/VCU fall 2023/Fall 2024/Database/De-ID TMAs 2/De-ID TMAs 2", 
                         pattern = "\\.xlsx$", full.names = TRUE)


# Step 2: Read and process each file
datasets <- lapply(file_paths, function(file) {
  # Read the dataset
  data <- read_excel(file)
  
  # Add a new column to identify the source dataset
  data$Source <- basename(file)
  
  return(data)
})

# Step 3: Combine all datasets
# Bind the rows, taking the union of all columns
datasets <- lapply(datasets, function(data) {
  data <- mutate_all(data, as.character) # Convert all columns to character
  return(data)
})
final_dataset <- bind_rows(datasets)
final_dataset <- type.convert(final_dataset, as.is = TRUE) # Convert back to appropriate types

## Race

final_dataset <- final_dataset %>%
  mutate(Race = ifelse(is.na(Race), racep, Race)) %>% 
  select(-racep) 

final_dataset <- final_dataset %>%
  mutate(Race = ifelse(is.na(Race), race, Race)) %>% 
  select(-race) 


## Sex 
final_dataset <- final_dataset %>%
  mutate(sex = ifelse(is.na(sex), Gender, sex)) %>% 
  select(-Gender) 

final_dataset <- final_dataset %>%
  mutate(sex = ifelse(is.na(sex), sexp, sex)) %>% 
  select(-sexp)

final_dataset <- final_dataset %>%
  mutate(sex = ifelse(is.na(sex), Sex, sex)) %>% 
  select(-Sex)

final_dataset <- final_dataset %>%
  relocate(sex, .after = 5) 

final_dataset <- final_dataset %>%
  relocate(Source, .after = 1) 

## Age 


final_dataset$agedx <- ifelse(is.na(final_dataset$agedx), final_dataset$`Age at Dx`, final_dataset$agedx)
final_dataset$`Age at Dx` <- NULL

final_dataset <- final_dataset %>%
  relocate(agedx, .after = 6) 

final_dataset <- final_dataset %>%
  filter(!if_all(-Source, is.na))

final_dataset <- final_dataset %>%
  mutate(er = ifelse(is.na(er), ER, er)) %>% 
  select(-ER)
final_dataset <- final_dataset %>%
  mutate(pr = ifelse(is.na(pr), PR, pr)) %>% 
  select(-PR)

final_dataset$Source = ifelse(final_dataset$Source=="Colon Adeno TMA 01 Jinze Liu De-ID.xlsx","Colon1",final_dataset$Source)

final_dataset$Source = ifelse(final_dataset$Source=="Colon Adeno TMA 02 Jinze Liu De-ID.xlsx","Colon2",final_dataset$Source)

final_dataset$Source = ifelse(final_dataset$Source=="HCC TMA 01 Jinze Liu De-ID.xlsx","HCC1",final_dataset$Source)

final_dataset$Source = ifelse(final_dataset$Source=="HCC TMA 02 Jinze Liu De-ID.xlsx","HCC2",final_dataset$Source)

final_dataset$Source = ifelse(final_dataset$Source=="HCC TMA 03 Jinze Liu De-ID.xlsx","HCC3",final_dataset$Source)

final_dataset$Source = ifelse(final_dataset$Source=="HGSOC TMA 01 Jinze Liu De-ID.xlsx","HGSOC1",final_dataset$Source)

final_dataset$Source = ifelse(final_dataset$Source=="Lung Adeno TMA 01 RB1 Jinze Liu De-ID.xlsx","Lung1",final_dataset$Source)

final_dataset$Source = ifelse(final_dataset$Source=="Lung Adeno TMA 02 RB2 Jinze Liu De-ID.xlsx","Lung2",final_dataset$Source)

final_dataset$Source = ifelse(final_dataset$Source=="Lung Adeno TMA 03-01 Jinze Liu De-ID.xlsx","Lung3",final_dataset$Source)

final_dataset$Source = ifelse(final_dataset$Source=="TNBC TMA 01 RB1 Jinze Liu De-ID.xlsx","TNBC1",final_dataset$Source)

final_dataset$Source = ifelse(final_dataset$Source=="TNBC TMA 02 RB1 Jinze Liu De-ID.xlsx","TNBC2",final_dataset$Source)

final_dataset$Source = ifelse(final_dataset$Source=="TNBC TMA 03 Jinze Liu De-ID.xlsx","TNBC3",final_dataset$Source)
final_dataset$disease_name <- sub(".{1}$", "", final_dataset$Source)


final_dataset$deceased <- ifelse(is.na(final_dataset$deceased),final_dataset$`Deceased (yes/no)`, final_dataset$deceased)
final_dataset$`Deceased (yes/no)` <- NULL

final_dataset$deceased <- ifelse(is.na(final_dataset$deceased),final_dataset$`DECEASED? (1=yes, 0=0/unkown)`, final_dataset$deceased)
final_dataset$`DECEASED? (1=yes, 0=0/unkown)` <- NULL


final_dataset$Grade <- ifelse(is.na(final_dataset$Grade),final_dataset$grade2, final_dataset$Grade)
final_dataset$grade2 <- NULL

final_dataset$Grade <- ifelse(is.na(final_dataset$Grade),final_dataset$`Grade of Tumor`, final_dataset$Grade)
final_dataset$`Grade of Tumor` <- NULL
colnames(final_dataset)[1] <- "CoreID"

# glimpse(final_dataset)
# View(final_dataset)
## Patient Table
# patient_dataset = final_dataset[,-c(1,3,4,5)]
# 
# patient_dataset <- patient_dataset %>% distinct()
# patient_dataset$patid = seq(1:nrow(patient_dataset)) 
# patient_dataset <- patient_dataset %>%
#   relocate(patid, .after = 1) 
# # View(patient_dataset)
# # glimpse(patient_dataset)
# 
# patient_dataset <- patient_dataset[,-c(13,21)]
# 
# ## Cores table
# core_dataset <- final_dataset[,c(1,2,3,4,16,24)]
# 
# # View(core_dataset)


  
  grid_1 = read_excel("C:/Users/ahsanm8/Desktop/VCU fall 2023/Fall 2024/Database/De-ID TMAs 2/De-ID TMAs 2/Colon Adeno TMA 01 Jinze Liu De-ID.xlsx",sheet=2)
  grid_1 <- as.data.frame(grid_1[,-1])

# Get all unique values and their row and column locations
locations <- which(!is.na(grid_1), arr.ind = TRUE)

# Create a data frame with unique entries and their positions
result_table_grid1 <- data.frame(
  Entry = grid_1[locations],
  Row = locations[, "row"],
  Column = locations[, "col"]
)

# View the result
# print(result_table_grid1)

grid_2 = read_excel("C:/Users/ahsanm8/Desktop/VCU fall 2023/Fall 2024/Database/De-ID TMAs 2/De-ID TMAs 2/Colon Adeno TMA 02 Jinze Liu De-ID.xlsx",sheet=2)
grid_2 <- as.data.frame(grid_2[,-1])

# Get all unique values and their row and column locations
locations <- which(!is.na(grid_2), arr.ind = TRUE)

# Create a data frame with unique entries and their positions
result_table_grid2 <- data.frame(
  Entry = grid_2[locations],
  Row = locations[, "row"],
  Column = locations[, "col"]
)

# View the result
# print(result_table_grid2)

grid_3 = read_excel("C:/Users/ahsanm8/Desktop/VCU fall 2023/Fall 2024/Database/De-ID TMAs 2/De-ID TMAs 2/HCC TMA 01 Jinze Liu De-ID.xlsx",sheet=2)
grid_3 <- as.data.frame(grid_3[,-1])

# Get all unique values and their row and column locations
locations <- which(!is.na(grid_3), arr.ind = TRUE)

# Create a data frame with unique entries and their positions
result_table_grid3<- data.frame(
  Entry = grid_3[locations],
  Row = locations[, "row"],
  Column = locations[, "col"]
)

# View the result
# print(result_table_grid3)


grid_4 = read_excel("C:/Users/ahsanm8/Desktop/VCU fall 2023/Fall 2024/Database/De-ID TMAs 2/De-ID TMAs 2/HCC TMA 02 Jinze Liu De-ID.xlsx",sheet=2)
grid_4 <- as.data.frame(grid_4[,-1])

# Get all unique values and their row and column locations
locations <- which(!is.na(grid_4), arr.ind = TRUE)

# Create a data frame with unique entries and their positions
result_table_grid4<- data.frame(
  Entry = grid_4[locations],
  Row = locations[, "row"],
  Column = locations[, "col"]
)

# View the result
# print(result_table_grid4)

grid_5 = read_excel("C:/Users/ahsanm8/Desktop/VCU fall 2023/Fall 2024/Database/De-ID TMAs 2/De-ID TMAs 2/HCC TMA 03 Jinze Liu De-ID.xlsx",sheet=2)
grid_5 <- as.data.frame(grid_5[,-1])

# Get all unique values and their row and column locations
locations <- which(!is.na(grid_5), arr.ind = TRUE)

# Create a data frame with unique entries and their positions
result_table_grid5<- data.frame(
  Entry = grid_5[locations],
  Row = locations[, "row"],
  Column = locations[, "col"]
)

# View the result
# print(result_table_grid5)


grid_6 = read_excel("C:/Users/ahsanm8/Desktop/VCU fall 2023/Fall 2024/Database/De-ID TMAs 2/De-ID TMAs 2/HGSOC TMA 01 Jinze Liu De-ID.xlsx",sheet=2)
grid_6 <- as.data.frame(grid_6[,-1])

# Get all unique values and their row and column locations
locations <- which(!is.na(grid_6), arr.ind = TRUE)

# Create a data frame with unique entries and their positions
result_table_grid6<- data.frame(
  Entry = grid_6[locations],
  Row = locations[, "row"],
  Column = locations[, "col"]
)

# View the result
# print(result_table_grid6)

grid_7 = read_excel("C:/Users/ahsanm8/Desktop/VCU fall 2023/Fall 2024/Database/De-ID TMAs 2/De-ID TMAs 2/Lung Adeno TMA 01 RB1 Jinze Liu De-ID.xlsx",sheet=2)
grid_7 <- as.data.frame(grid_7[,-1])

# Get all unique values and their row and column locations
locations <- which(!is.na(grid_7), arr.ind = TRUE)

# Create a data frame with unique entries and their positions
result_table_grid7<- data.frame(
  Entry = grid_7[locations],
  Row = locations[, "row"],
  Column = locations[, "col"]
)

# View the result
# print(result_table_grid7)

grid_8 = read_excel("C:/Users/ahsanm8/Desktop/VCU fall 2023/Fall 2024/Database/De-ID TMAs 2/De-ID TMAs 2/Lung Adeno TMA 02 RB2 Jinze Liu De-ID.xlsx",sheet=2)
grid_8 <- as.data.frame(grid_8[,-1])

# Get all unique values and their row and column locations
locations <- which(!is.na(grid_8), arr.ind = TRUE)

# Create a data frame with unique entries and their positions
result_table_grid8<- data.frame(
  Entry = grid_8[locations],
  Row = locations[, "row"],
  Column = locations[, "col"]
)

# View the result
# print(result_table_grid8)

grid_9 = read_excel("C:/Users/ahsanm8/Desktop/VCU fall 2023/Fall 2024/Database/De-ID TMAs 2/De-ID TMAs 2/Lung Adeno TMA 03-01 Jinze Liu De-ID.xlsx",sheet=2)
grid_9 <- as.data.frame(grid_9[,-1])

# Get all unique values and their row and column locations
locations <- which(!is.na(grid_9), arr.ind = TRUE)

# Create a data frame with unique entries and their positions
result_table_grid9<- data.frame(
  Entry = grid_9[locations],
  Row = locations[, "row"],
  Column = locations[, "col"]
)

# View the result
# print(result_table_grid9)

grid_10 = read_excel("C:/Users/ahsanm8/Desktop/VCU fall 2023/Fall 2024/Database/De-ID TMAs 2/De-ID TMAs 2/TNBC TMA 01 RB1 Jinze Liu De-ID.xlsx",sheet=2)
grid_10 <- as.data.frame(grid_10[,-1])

# Get all unique values and their row and column locations
locations <- which(!is.na(grid_10), arr.ind = TRUE)

# Create a data frame with unique entries and their positions
result_table_grid10<- data.frame(
  Entry = grid_10[locations],
  Row = locations[, "row"],
  Column = locations[, "col"]
)

# View the result
# print(result_table_grid10)

grid_11 = read_excel("C:/Users/ahsanm8/Desktop/VCU fall 2023/Fall 2024/Database/De-ID TMAs 2/De-ID TMAs 2/TNBC TMA 02 RB1 Jinze Liu De-ID.xlsx",sheet=2)
grid_11 <- as.data.frame(grid_11[,-1])

# Get all unique values and their row and column locations
locations <- which(!is.na(grid_11), arr.ind = TRUE)

# Create a data frame with unique entries and their positions
result_table_grid11<- data.frame(
  Entry = grid_11[locations],
  Row = locations[, "row"],
  Column = locations[, "col"]
)

# View the result
# print(result_table_grid11)

grid_12 = read_excel("C:/Users/ahsanm8/Desktop/VCU fall 2023/Fall 2024/Database/De-ID TMAs 2/De-ID TMAs 2/TNBC TMA 03 Jinze Liu De-ID.xlsx",sheet=2)
grid_12 <- as.data.frame(grid_12[,-1])

# Get all unique values and their row and column locations
locations <- which(!is.na(grid_12), arr.ind = TRUE)

# Create a data frame with unique entries and their positions
result_table_grid12<- data.frame(
  Entry = grid_12[locations],
  Row = locations[, "row"],
  Column = locations[, "col"]
)

# View the result
# print(result_table_grid12)

grids_locations = rbind(result_table_grid1,result_table_grid2,result_table_grid3,result_table_grid4,result_table_grid5,
                        result_table_grid6,result_table_grid7,result_table_grid8,result_table_grid9,
                        result_table_grid10,result_table_grid11,result_table_grid12)
colnames(grids_locations)[1] <- "CoreID"

# merged_dataset <- grids_locations %>%
#   left_join(core_dataset, by = "CoreID")
# 
# # View the result
# print(merged_dataset)






# View(final_dataset)
################################# Patient table
# final_dataset<-final_dataset[,-3]
p_id<-c()
p_id[1]<-1
id<-1
for(i in 2:nrow(final_dataset)){

  if(identical(final_dataset[i,-1],final_dataset[(i-1),-1])){
    p_id[i]<-id
  }else{ p_id[i]<-id+1}
  
  id<-p_id[i]
}

final_dataset$pID<-p_id


final_dataset <- final_dataset %>%
  relocate(pID, .after = 1) 


final_dataset <- final_dataset %>%
  relocate(disease_name, .after = 3) 
final_dataset$tobaccouse = as.factor(final_dataset$tobaccouse)
final_dataset$tobacco_alpharesponse = as.factor(final_dataset$tobacco_alpharesponse)
final_dataset$tobaccouse <- factor(final_dataset$tobaccouse, 
                                   levels = c("10 or more cigarettes (1/2 pack or more)/day in last 30 days", 
                                              "5-9 cigarettes (between 1/4 to 1/2 pack)/day in last 30 days", 
                                              "4 or less cigarettes (less than 1/4 pack)/day in last 30 days", 
                                              "Current every day smoker", 
                                              "Current some day smoker", 
                                              "Former smoker", 
                                              "Former smoker, quit more than 30 days ago", 
                                              "Former user, smokeless tobacco quit more than 30 days ago", 
                                              "Hx smoker", 
                                              "Never (less than 100 in lifetime)", 
                                              "Never smoker", 
                                              "non-smoker", 
                                              "Smoker", 
                                              "Smoker, current status unknown", 
                                              "Unknown"),
                                   labels = c("Current Heavy Smoker", 
                                              "Current Light Smoker", 
                                              "Current Very Light Smoker", 
                                              "Current Smoker", 
                                              "Current Smoker", 
                                              "Former Smoker", 
                                              "Former Smoker", 
                                              "Former Smokeless Tobacco User", 
                                              "Former Smoker", 
                                              "Never Smoker", 
                                              "Never Smoker", 
                                              "Never Smoker", 
                                              "Smoker", 
                                              "Smoker", 
                                              "Unknown"))
final_dataset$tobacco_alpharesponse <- factor(final_dataset$tobacco_alpharesponse, 
                                              levels = c("4 or less cigarettes(less than 1/4 pack)/day in last 30 days", 
                                                         "Current every day smoker", 
                                                         "Current some day smoker", 
                                                         "Former smoker", 
                                                         "Former smoker, quit more than 30 days ago", 
                                                         "Former user, smokeless tobacco quit more than 30 days ago", 
                                                         "Heavy tobacco smoker", 
                                                         "Never (less than 100 in lifetime)", 
                                                         "Never smoker", 
                                                         "Smoker, current status unknown"), 
                                              labels = c("Current Very Light Smoker", 
                                                         "Current Everyday Smoker", 
                                                         "Current Occasional Smoker", 
                                                         "Former Smoker", 
                                                         "Former Smoker", 
                                                         "Former Smokeless Tobacco User", 
                                                         "Heavy Smoker", 
                                                         "Never Smoker", 
                                                         "Never Smoker", 
                                                         "Unknown"))

final_dataset$Race <- as.factor(final_dataset$Race)
final_dataset$Race[final_dataset$Race == "ASIAN"] = "Asian"
final_dataset$Race[final_dataset$Race == "BLACK OR AFRICAN AMERICAN"] = "Black"
final_dataset$Race[final_dataset$Race == "H"] = "Hispanic"
final_dataset$Race[final_dataset$Race == "W"] = "White"
final_dataset$Race[final_dataset$Race == "WHITE"] = "White"
final_dataset$Race = droplevels(final_dataset$Race)
final_dataset$sex[final_dataset$sex=="Female"] = "F"
patient_dataset = final_dataset[,-c(1,5,6)]

patient_dataset <- patient_dataset %>% distinct()

# View(patient_dataset)
# glimpse(patient_dataset)

patient_dataset <- patient_dataset[,-c(15,23)] ##### ARE THESE CORRECT?  YES because these are for core table , GRADE and Donor Core Height"

dim(patient_dataset)

# patient_dataset$pID






## Cores table
core_dataset <- final_dataset[,c(1,2,3,4,5,18,26)] ####### The column numbers need to be updated 

# core_dataset<-core_dataset[,-2] #remove observation id
# View(core_dataset)



grids_locations = rbind(result_table_grid1,result_table_grid2,result_table_grid3,result_table_grid4,result_table_grid5,
                        result_table_grid6,result_table_grid7,result_table_grid8,result_table_grid9,
                        result_table_grid10,result_table_grid11,result_table_grid12)
colnames(grids_locations)[1] <- "CoreID"

merged_dataset <- grids_locations %>%
  left_join(core_dataset, by = "CoreID")



View(merged_dataset)
View(final_dataset)
dim(final_dataset)

##########################################

 # MySQL/MariaDB connector

library(RMySQL)
library(readxl)
library(DBI)
library(RMariaDB)  # Use the appropriate driver package for your database

# Connect to a MySQL or MariaDB database
con <- dbConnect(RMySQL::MySQL(),
                 dbname = "TMA",
                 host = "localhost",
                 user = "root",
                 password = "1234",
                 port=3306)


(table<-dbListTables(con))

# Push the dataset to SQL
dbWriteTable(con, "final_dataset", final_dataset, overwrite = TRUE, row.names = FALSE)
dbWriteTable(con, "patient_table", patient_dataset, overwrite = TRUE, row.names = FALSE)
dbWriteTable(con, "core_table", merged_dataset, overwrite = TRUE, row.names = FALSE)


dbListTables(con)           # Lists all tables
dbReadTable(con, "final_dataset") # Reads back the data
dbReadTable(con, "patient_table") # Reads back the data
dbReadTable(con, "Core_table") # Reads back the data



# List all columns in the 'final_dataset' table
dbListFields(con, "final_dataset")
dbListFields(con, "patient_table") # Reads back the data
dbListFields(con, "Core_table") # Reads back the data


dbListTables(con) # Check if 'final_dataset' exists
dbGetQuery(con, "SELECT * FROM final_dataset LIMIT 5;") # Preview the data



##############################
# Shiny APP
###############################



library(shiny)
library(DBI)
library(RMySQL)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel("SQL Data Viewer and Analysis"),
  sidebarLayout(
    sidebarPanel(
      textInput("host", "Database Host", value = "localhost"),
      numericInput("port", "Database Port", value = 3306),
      textInput("dbname", "Database Name", value = "TMA"),
      textInput("user", "Username", value = "root"),
      passwordInput("password", "Password", value = "9547cf8I."),
      actionButton("connect", "Connect to Database"),
      
      hr(),
      
      uiOutput("table_selector"),
      uiOutput("join_column_selector"),
      
      actionButton("load_data", "Load Data"),
      
      hr(),
      
      uiOutput("variable_selector"),
      selectInput("plot_type", "Select Plot Type:", 
                  choices = c("Bar Plot", "Histogram", "Density Plot", "Scatter Plot"),
                  selected = "Scatter Plot"),
      actionButton("plot_button", "Generate Plot"),
      
      # Dropdown for Core Table and Source selection
      uiOutput("core_table_selector"),
      uiOutput("source_selector"),
      
      # Group By option for Summary Statistics
      uiOutput("group_by_selector")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("stat_plot")),
        tabPanel("Summary Statistics", verbatimTextOutput("summary_stats")),
        tabPanel("Contingency Table", tableOutput("contingency_table")),
        tabPanel("Table Preview", tableOutput("preview_table")),
        tabPanel("Core Grid", tableOutput("core_grid_table"))
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  # Reactive value for database connection
  con <- reactiveVal(NULL)
  
  # Connect to database
  observeEvent(input$connect, {
    tryCatch({
      db_con <- dbConnect(
        RMySQL::MySQL(),
        dbname = input$dbname,
        host = input$host,
        user = input$user,
        password = input$password,
        port = input$port
      )
      con(db_con)
      showNotification("Connected to database successfully!", type = "message")
    }, error = function(e) {
      showNotification("Failed to connect to database: Check details!", type = "error")
    })
  })
  
  # Define the core_gid function
  core_gid <- function(Core_table, Source){
    filtered_data <- Core_table[which(Core_table$Source == Source),]
    maxr <- max(na.omit(filtered_data$Row))
    maxc <- max(na.omit(filtered_data$Column))
    S <- matrix(NA, maxr, maxc)

    for(i in 1:nrow(filtered_data)){
      S[filtered_data$Row[i], filtered_data$Column[i]] <- filtered_data$pID[i]
    }
    S
  }
  # Table selection dropdown
  output$table_selector <- renderUI({
    req(con())
    tables <- dbListTables(con())
    selectInput("selected_tables", "Select Table(s):", choices = tables, multiple = TRUE)
  })
  
  # Join column selection
  output$join_column_selector <- renderUI({
    req(con())
    req(input$selected_tables)
    if (length(input$selected_tables) > 1) {
      common_columns <- Reduce(intersect, lapply(input$selected_tables, function(table) {
        dbListFields(con(), table)
      }))
      selectInput("join_column", "Select Join Column:", choices = common_columns, selected = common_columns[1])
    }
  })
  
  # Data fetching and optional join
  get_data <- reactive({
    req(input$selected_tables)
    req(con())
    tables <- input$selected_tables
    join_column <- input$join_column
    
    if (length(tables) == 1) {
      query <- sprintf("SELECT * FROM `%s`", tables[1])
      data <- dbGetQuery(con(), query)
    } else if (length(tables) > 1 && !is.null(join_column)) {
      join_query <- sprintf(
        "SELECT * FROM `%s` %s",
        tables[1],
        paste(
          lapply(2:length(tables), function(i) {
            sprintf("INNER JOIN `%s` ON `%s`.`%s` = `%s`.`%s`",
                    tables[i], tables[1], join_column, tables[i], join_column)
          }),
          collapse = " "
        )
      )
      data <- tryCatch({
        dbGetQuery(con(), join_query)
      }, error = function(e) {
        showNotification("SQL Query Error: Check join column or table selection.", type = "error")
        NULL
      })
    } else {
      showNotification("Please select a join column for multiple tables.", type = "error")
      return(NULL)
    }
    return(data)
  })
  
  # Display table preview
  output$preview_table <- renderTable({
    req(input$load_data)
    get_data()
  })
  
  # Variable selection dropdown
  output$variable_selector <- renderUI({
    req(get_data())
    variables <- names(get_data())
    selectInput("selected_variables", "Select Variables:", choices = variables, multiple = TRUE)
  })
  
  # Core Table and Source Selection
  output$core_table_selector <- renderUI({
    req(con())
    tables <- dbListTables(con())
    selectInput("core_table", "Select Core Table:", choices = tables)
  })
  
  output$source_selector <- renderUI({
    req(input$core_table)
    # Assuming 'Source' is a column in the Core table
    data <- dbGetQuery(con(), sprintf("SELECT DISTINCT Source FROM `%s`", input$core_table))
    selectInput("source", "Select Source:", choices = data$Source)
  })
  
  # Display the Core Grid table based on selected Source
  output$core_grid_table <- renderTable({
    req(input$core_table)
    req(input$source)
    data <- dbGetQuery(con(), sprintf("SELECT * FROM `%s` WHERE Source = '%s'", input$core_table, input$source))
    core_gid(data, input$source)
  })
  
  
  # Contingency Table
  output$contingency_table <- renderTable({
    req(input$selected_tables)
    req(input$selected_variables)
    req(length(input$selected_variables) == 2)
    
    data <- get_data()
    table(data[[input$selected_variables[1]]], data[[input$selected_variables[2]]])
  })
  
  
  # Generate statistical plots dynamically for selected variables
  output$stat_plot <- renderPlot({
    req(get_data())
    req(input$selected_variables)
    req(input$plot_type)
    
    data <- get_data()
    selected_variables <- input$selected_variables
    plot_type <- input$plot_type
    group_by_var <- input$group_by_variable
    
    # Check for grouping variable
    if (!is.null(group_by_var) && group_by_var != "None") {
      group_aes <- aes_string(color = group_by_var, fill = group_by_var)
    } else {
      group_aes <- NULL
    }
    
    # Create plots for each selected variable
    plots <- lapply(selected_variables, function(variable) {
      if (plot_type == "Bar Plot") {
        ggplot(data, aes_string(x = variable)) +
          geom_bar(group_aes) +
          labs(title = paste("Bar Plot:", variable), x = variable, y = "Count") +
          theme_minimal()
      } else if (plot_type == "Histogram") {
        ggplot(data, aes_string(x = variable)) +
          geom_histogram(binwidth = 10, fill = "blue", color = "white") +
          labs(title = paste("Histogram:", variable), x = variable, y = "Frequency") +
          theme_minimal()
      } else if (plot_type == "Density Plot") {
        ggplot(data, aes_string(x = variable)) +
          geom_density(group_aes, alpha = 0.5) +
          labs(title = paste("Density Plot:", variable), x = variable, y = "Density") +
          theme_minimal()
      } else if (plot_type == "Scatter Plot" && length(selected_variables) == 2) {
        ggplot(data, aes_string(x = selected_variables[1], y = selected_variables[2])) +
          geom_point(group_aes) +
          labs(title = "Scatter Plot", x = selected_variables[1], y = selected_variables[2]) +
          theme_minimal()
      } else {
        NULL
      }
    })
    
    # Arrange multiple plots if needed
    if (length(plots) > 1) {
      gridExtra::grid.arrange(grobs = plots)
    } else {
      plots[[1]]
    }
  })
  
  
  output$group_by_selector <- renderUI({
    req(get_data())
    variables <- names(get_data())
    selectInput("group_by_variable", "Group By Variable:", choices = c("None", variables), selected = "None")
  })
  
  # Summary statistics with optional grouping
  output$summary_stats <- renderPrint({
    req(get_data())
    req(input$selected_variables)
    data <- get_data()
    
    if (!is.null(input$group_by_variable) && input$group_by_variable != "None") {
      grouped_data <- split(data, data[[input$group_by_variable]])
      lapply(grouped_data, function(group) {
        summary(group[, input$selected_variables, drop = FALSE])
      })
    } else {
      summary(data[, input$selected_variables, drop = FALSE])
    }
  })
  
  # Close database connection on app close
  session$onSessionEnded(function() {
    if (!is.null(con())) dbDisconnect(con())
  })
}

# Run the application
shinyApp(ui = ui, server = server)



































