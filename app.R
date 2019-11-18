## app.R ##
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinydashboardPlus)
library(markdown)
library(readr)
library(ggplot2)
library(stringr)
library(dplyr)
library(DT)
library(tools)
library(shinyjs)
library(glue)
library(sodium)
library(data.table)
library(lubridate)
library(shinyalert)
library(rdrop2)
library(assertive)

#token <- drop_auth()
#saveRDS(token, "droptoken.rds")
token <- readRDS("droptoken.rds")
drop_auth(rdstoken = "droptoken.rds")
# Then pass the token to each drop_ function
#drop_acc(dtoken = token)

#Read in main data table from your local directory
#master1 <- read.csv("https://www.dropbox.com/s/fgty42qwpkzudwz/master1.txt?dl=1", stringsAsFactors = F)
################## new way to read in comma delineated file on locate machine.
master1 <- read.csv("Master1.csv", stringsAsFactors = FALSE)
scenario_temp <- master1[FALSE,]
#Read cip data table and order alphabetically
cip2 <- read_tsv("cip_code.txt")
cip1 <- cip2[order(cip2$CIP_Category),]
#Read soc data table and order alphabetically
soc2 <- read_tsv("soc_code.txt")
soc1 <- soc2[order(soc2$SOC_Cat_Name),]
#Credentials
#credentials = data.frame(
#  username_id = "Epic",
#  passod   = sapply("pass1",password_store),
#  permission  = c("advanced"), 
#  stringsAsFactors = F)
#saveRDS(credentials, "cred.rds")
#drop_upload("cred.rds", path = "responses")
drop_download("responses/cred.rds", overwrite = TRUE)
credentials <- readRDS("cred.rds")

# Data frame to convert degree code to number of years for degree
deg.code <- c(1,2,3,4,5,6,7,8,13,14,17,18,19,"N/A")
#years <- c(.8,1.5,2,3,4,5,6,7,3,5,10,7,8,0)
years <- c(1,2,2,3,4,5,6,7,3,5,10,7,8,0)
num.years <- data.frame(deg.code, years, stringsAsFactors = FALSE)

pc_index1 <- 0
pc_index2 <- 0
pc_index3 <- 0
roi_table <- data.frame(total_cost = numeric(), total_wages = numeric())
cummulative_table <- data.frame(total_1 = numeric())
cummulative_graph <- data.frame(years = numeric(), dfnomatch=NULL)
raw_table <- data.frame(option_1 = numeric())
running_total <- 0
career_years <- 0
graph_parameters <- NULL
cg_years <- data.frame(years = numeric())
cg_card1 <- data.frame(card1 = numeric())
cg_card2 <- data.frame(card2 = numeric())
cg_card3 <- data.frame(card3 = numeric())

#place_card <- function(index){
#  pcresult <- box(width = 4,
#                  strong("Occupation :"), 
#                  scenario_temp$occ.name[index], br(),
#                  strong("School :"), 
#                  scenario_temp$school.name[index], br(),
#                  strong("Curriculum :"), 
#                  scenario_temp$cip.name[index], br(),
#                  strong("Degree :"), 
#                  scenario_temp$degree.name[index], br(),
#                  strong("Salary :"), 
#                  scenario_temp$X17p[index], br(),
#                  strong("Tot Annual Cost :"), 
#                  scenario_temp$InStOff[index])
#  return(pcresult)
#}
place_card <- function(index){
  dc <- scenario_temp$degree.code[index]
  nyear <- filter(num.years, deg.code %in% dc) %>% select(years)
  pcresult <- box(width = 4,
                  strong("School :"), 
                  scenario_temp$school.name[index], br(),
                  strong("Curriculum :"), 
                  scenario_temp$cip.name[index], br(),
                  strong("Degree :"), 
                  scenario_temp$degree.name[index], br(),
                  strong("Number of years: "),(nyear),br(),
                  strong("Annual Cost :"),
                  scenario_temp$InStOff[index], br(), br(),
                  strong("Occupation :"), 
                  scenario_temp$occ.name[index], br(),
                  strong("Salary :"), 
                  scenario_temp$X17p[index])
  return(pcresult)
}
init_variables <- function(){
  cummulative_table <<- cummulative_table[FALSE,]
#  cummulative_graph <<- data.frame(col = numeric(), nomatch=NULL )
  raw_table <<- raw_table[FALSE,]
  cg_years <<- cg_years[FALSE]
  cg_card1 <<- data.frame(card1 = numeric())
  cg_card2 <<- data.frame(card2 = numeric())
  cg_card3 <<- data.frame(card3 = numeric())
#  cummulative_graph <- data.frame(years = numeric(), dfnomatch=NULL)
}
school_years <- function(index) {
  dc <- scenario_temp$degree.code[index]
  nyear <- as.numeric(filter(num.years, deg.code %in% dc) %>% select(years))
  return(nyear)
}
school_cost <- function(index, nyear){
  year_change <- 0
  running_total <<- 0
  annual_cost <- scenario_temp$InStOff[index]
  for(i in (1:nyear)){
    year_change <- round(annual_cost, 0)
    raw_table <<- rbind(raw_table, -(year_change))
    running_total <<- round((running_total - annual_cost),0)
    cummulative_table <<- rbind(cummulative_table, running_total)
  }
  return()
}
career_income <- function(index, year_change, occf){
  raw_table <<- rbind(raw_table, year_change)
  running_total <<- running_total + year_change
  cummulative_table <<- rbind(cummulative_table, running_total)
  for(i in (1:career_years)){
    tf <- (1 + ((0.00002 * ((i) ^ 2)) - 0.0023 * (i) + 0.0603))
    year_change <- year_change * tf * occf
    year_change <- round(year_change, 0)
    raw_table <<- rbind(raw_table, year_change)
    running_total <<- running_total + year_change
    cummulative_table <<- rbind(cummulative_table, running_total)
  }
  return()
}
create_years <- function(num_years2) {
  
  for(i in(1:num_years2)){
    cg_years <<- rbind(cg_years, as.numeric(i))
  }
  cummulative_graph <<- cg_years
  colnames(cummulative_graph) <<- c("years")
  graph_parameters <<- ggplot() + 
    xlab('Years') +
    ylab('Total Earnings in Thousands') +
    labs(title = 'Cummulative Cash Flow') +
    scale_colour_manual(name="Occupation", values = c("First" = "blue", "Second" = "green", "Third" = "red"),
                        labels = c(scenario_temp$occ.name[pc_index1],scenario_temp$occ.name[pc_index2],scenario_temp$occ.name[pc_index3])) +
    theme(plot.title = element_text(hjust = 0.5))
  return()
}

create_data <- function(index, num_years2) {
  init_variables()
  nyears <- school_years(index)
  school_cost(index, nyears)
  year_change <- scenario_temp$X17p[index]
  occf <- scenario_temp$MedOccF[index]
  career_years <<- as.numeric(num_years2 - nyears - 1)
  career_income(index, year_change, occf)
  add_column(index)
  return()
}
add_column <- function(index) {
  if(index == pc_index1) {
    cg_card1 <<- cummulative_table
    cummulative_graph <<- cbind(cummulative_graph, "card1" = cg_card1)
    graph_parameters <<- graph_parameters + geom_line(data = cummulative_graph, aes(x = years, y = card1/1000, colour = "First"), show.legend = TRUE) 
  }
  if(index == pc_index2) {
    cg_card2 <<- cummulative_table
    cummulative_graph <<- cbind(cummulative_graph, "card2" = cg_card2)
    graph_parameters <<- graph_parameters + geom_line(data = cummulative_graph, aes(x = years, y = card2/1000, colour = "Second"), show.legend = TRUE)
  }
  if(index == pc_index3) {
    cg_card3 <<- cummulative_table
    cummulative_graph <<- cbind(cummulative_graph, "card3" = cg_card3)
    graph_parameters <<- graph_parameters + geom_line(data = cummulative_graph, aes(x = years, y = card3/1000, colour = "Third"), show.legend = TRUE)
  }
  return()
}

header <- dashboardHeader( title = "E.P.I.C. Planning", titleWidth = 230, uiOutput("logoutbtn"))
sidebar <- dashboardSidebar(uiOutput("sidebarpanel")) 
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "home",
            h2("Welcome to EPIC, the Education-Profession Investment Calculator"),
            h2("Hello and Welcome to your EPIC experience"),
            h2("Test drive the world's first fully integrated"),
            h2("Education-Profession Investment Calculator"),
            useShinyalert()
    ),
    tabItem(tabName = "about",
            h2("Hello and Welcome to your EPIC experience"),
            h2("Test drive the world's first fully integrated"),
            h2("Education-Profession Investment Calculator"),
            h2("Text or call Lynn Chapman at 703-348-4086 for help or questions")
    ),
    tabItem(tabName = "tools",
            h2("This is where tools go."),
            fluidPage(
              fluidRow(
                box(width = 4,
                    radioButtons(inputId = "RecordsNum",
                                 label = "Number of records per page",
                                 choices = c(10, 20, 50, 100),
                                 selected = 10)
                ),
                box(width = 4,
                    h4("Explore career occupations:", a("https://www.onetonline.org/", href = "https://www.onetonline.org/", target = "_blank")),
                    h4("Explore schools:", a("https://collegescorecard.ed.gov/", href = "https://collegescorecard.ed.gov/", target = "_blank")),
                    h4("Skills assessment:", a("https://www.careeronestop.org/", href = "https://www.careeronestop.org/", target = "_blank")),
                    h4("Advanced Skills assessment:", a("https://www.wowi.com/", href = "https://www.wowi.com/", target = "_blank")),
                    h4("What do you want to do for a living?", a("https://www.mynextmove.org/", href = "https://www.mynextmove.org/", target = "_blank")),
                    h4("Occupational outlook handbook?", a("https://www.bls.gov/ooh/", href = "https://www.bls.gov/ooh/", target = "_blank")),
                    h4("School comparison and Research Tool", a("https://nces.ed.gov/collegenavigator/", href = "https://nces.ed.gov/collegenavigator/", target = "_blank"))
                )),
              box(width = 4,
                  checkboxGroupInput(inputId = "basic.column.names", label = "Pick the scenario columns you would like",
                                     selected = c("school.name", "X17p", "degree.name", "occ.name", "cip.name", "entry.degree",
                                                  "Experience", "InStOff"), choices = names(master1))),
              box(width = 4,
                  checkboxGroupInput(inputId = "jobs.column.names", label = "Pick the jobs columns you would like",
                                     selected = c("X17p", "X50p", "X82p", "occ.name", "entry.degree",
                                                  "Experience"), choices = names(master1))),
              box(width = 4,
                  checkboxGroupInput(inputId = "schools.column.names", label = "Pick the school columns you would like",
                                     selected = c("school.name", "State", "InStOff", "Ouston", "OuStOff",
                                                  "IGrant.Avg", "IGrant..age"), choices = names(master1)))
            )),
    tabItem(tabName = "instructions",
            h2("Click on your school and job preferences. If you really don't know, leave it blank"),
            h2("Text or call Lynn Chapman at 703-348-4086 for help or questions")
    ),
    tabItem(tabName = "jobsearch",
            h2("Go to ", a("www.onetonline.org", href ="https://www.onetonline.org/", target = "_blank"), " to look for jobs")
    ),
    
    tabItem(tabName = "exploreschools",
            fluidRow(
              box(width = 2,
                  selectInput(inputId = "epic.school.name",
                              label= "School Name:",
                              choices =  unique(master1$school.name),
                              multiple = TRUE)),
              box(width = 2,
                  sliderInput(inputId = "epic.tuition",
                              label = "Desired Tuition Level",
                              value = max(sort(unique(master1$InStOff))),
                              min = min(sort(unique(master1$InStOff))),
                              step = 1000,
                              max = max(sort(unique(master1$InStOff))))),
              box(width = 2,
                  selectInput(inputId = "epic.cip.cat",
                              label = "Curriculum Category:",
                              choices = cip1$CIP_Category,
                              multiple = TRUE)),
              box(width = 2,
                  selectInput(inputId = "epic.cip.name",
                              label = "Curriculum Name:",
                              choices = unique(master1$cip.name),
                              multiple = TRUE)),
              box(width = 2,
                  selectInput(inputId = "epic.state",
                              label = "State:",
                              choices = unique(master1$State),
                              multiple = TRUE)),
              box(width = 2,
                  selectInput(inputId = "epic.degree.name",
                              label = "Degree Name:",
                              choices =  unique(master1$degree.name),
                              multiple = TRUE))
            ),
            box(
              width = 12,
              div(style = 'overflow-x: scroll',DT::dataTableOutput(outputId = "epic.schools.table"))
            )
    ),
    tabItem(tabName = "explorejobs",
            fluidRow(
              box(width = 2,
                  sliderInput(inputId = "epic.income",
                              label = "Desired Income Level:",
                              value = min(sort(unique(master1$X17p))),
                              min = min(sort(unique(master1$X17p))),
                              step = 1000,
                              max = max(sort(unique(master1$X17p))))),
              box(width = 3,
                  selectInput(inputId = "epic.occ.cat",
                              label = "Occupation Category:",
                              choices = soc1$SOC_Cat_Name,
                              multiple = TRUE)),
              box(width = 3,
                  selectInput(inputId = "epic.occ.name",
                              label = "Occupation Name:",
                              choices = unique(master1$occ.name),
                              multiple = TRUE)),
              box(width = 2,
                  selectInput(inputId = "epic.experience",
                              label = "Experience:",
                              choices =  unique(master1$Experience),
                              multiple = TRUE)),
              box(width = 2,
                  selectInput(inputId = "epic.entry.degree",
                              label = "Rqd Entry Degree:",
                              choices =  unique(master1$entry.degree),
                              multiple = TRUE))
            ),
            box(
              width = 12,
              div(style = 'overflow-x: scroll',DT::dataTableOutput(outputId = "epic.jobs.table"))
            )
    ),
    tabItem(tabName = "buildbasic",
            fluidRow(
              box(width = 2,
                  actionButton(inputId = "add_scenarios", label = "Add", width = '100%'))
            ),
            fluidRow(
              box(
                width = 12,
                div(style = 'overflow-x: scroll',DT::dataTableOutput(outputId = "epic.choice.table"))
              )
            )
    ),
    tabItem(tabName = "compare",
            fluidRow(
              box(width = 2,
                  actionButton(inputId = "delete_scenario", label = "Delete", width = '100%')),
              box(width = 2,
                  actionButton(inputId = "load_scenario", label = "Load", width = '100%')),
              box(width = 2,
                  actionButton(inputId = "save_scenario", label = "Save", width = '100%'))
            ),
            fluidRow(
              box(width = 12,
                  div(style = 'overflow-x: scroll',DT::dataTableOutput(outputId = "epic.scenarios.table"))
              )
              
            ),
            fluidRow(
              box(width = 4,
                  actionButton(inputId = "clear_all", label = "Clear all Scenarios", width = '100%')),
              
              box(width = 2,
                  numericInput(inputId = "num_years", label = "Number of years to chart", value = 25, min = 1, max = 52 )),
              box(width = 2,
                  strong("Press to create Graph"),br(),
                  actionButton(inputId = "create_data", label = "Go", width = '100%'))
            ),
            fluidRow(
              box(width = 4,
                  actionButton(inputId = "add_one_compare", label = "Add First School to Compare", width = '100%')),
              box(width = 4,
                  actionButton(inputId = "add_two_compare", label = "Add Second School to Compare", width = '100%')),
              box(width = 4,
                  actionButton(inputId = "add_three_compare", label = "Add Third School to Compare", width = '100%'))
            ),
            fluidRow(
              uiOutput(outputId = "row.choice.table1"),
              uiOutput(outputId = "row.choice.table2"),
              uiOutput(outputId = "row.choice.table3")
            ),
            hr(),
            fluidRow(
              box(width = 6,
                  plotOutput("cummulative.plot")
              ),
              box(width = 6,
                  div(style = 'overflow-x: scroll',DT::dataTableOutput(outputId = "temp.table"))
                  ))
    ),
    tabItem(tabName = "login",
            div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                wellPanel(
                  tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                  textInput("userName", placeholder="Username", label = tagList(icon("user"), "Username")),
                  passwordInput("passwd", placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
                  br(),
                  div(
                    style = "text-align: center;",
                    actionButton("login", "SIGN IN", style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
                    br(),
                    br(),
                    br()
                  ),
                  div(
                    style = "text-align: center;",
                    actionButton("add_user", "Create Account", style = "color: white; background-color:#808080;
                                 padding: 10px 15px; width: 200px; cursor: pointer;
                                 font-size: 16px; font-weight: 600;"),
                    br()
                  )
                  )
            ))
  )
)
ui <- dashboardPagePlus(header, sidebar, body, skin = "blue")

server <- function(input, output, session) {
  
  login <- FALSE
  USER <- reactiveValues(login = login)
  
  observeEvent(input$login,{
    if (USER$login == FALSE) {
      Username <- isolate(input$userName)
      Password <- isolate(input$passwd)
      if(length(which(credentials$username_id==Username))==1) { 
        pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
        pasverify <- password_verify(pasmatch, Password)
        if(pasverify) {
          USER$login <- TRUE
        } else {
          shinyalert(title = "Username or Password incorrect", type = "error")
        }
      } else {
        shinyalert(title = "Username or Password incorrect", type = "error")
      }
    } 
  })
  output$logoutbtn <- renderUI({
    req(USER$login)
    tags$li(a(icon("fa fa-sign-out"), "Logout", 
              href="javascript:window.location.reload(true)"),
            class = "dropdown", 
            style = "background-color: #eee !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;")
  })
  output$sidebarpanel <- renderUI({
    if (USER$login == TRUE ){ 
      if (credentials[,"permission"][which(credentials$username_id==input$userName)]=="advanced") {
        sidebarMenu(id = "tabs",
                    menuItem("Home Page",
                             menuSubItem("Instructions", tabName = "instructions")),
                    menuItem("Explore Jobs", tabName = "explorejobs", icon = icon("user")),
                    menuItem("Explore Schools", tabName = "exploreschools", icon = icon("user")),
                    menuItem("Select scenarios", tabName = "buildbasic", icon = icon("tasks")),
                    menuItem("Compare Scenarios", tabName = "compare", icon = icon("tasks")),
                    menuItem("Look for Jobs", tabName = "jobsearch", icon = icon("tasks")),
                    menuItem("Tools", tabName = "tools", icon = icon("toolbox")),
                    menuItem("About", tabName = "about", icon = icon("info"))
        )
      }
    } else {
      sidebarMenu(id = "log_tabs",
                  menuItem("Login", tabName = "login")
      )
    }
  })
  observe({
    if (USER$login == TRUE ){
      updateTabItems(session, "tabs", "instructions") } else {
        updateTabItems(session, "log_tabs", "login")
      }
  })
  
  #Reactive variable that uses selected choices or full column if empty
  school.name_var <- reactive({
    if(is.null(input$epic.school.name )) {
      unique(master1$school.name)} else {
        input$epic.school.name
      }
  })
  #Reactive variable that uses selected choices or full column if empty 
  degree.name_var <- reactive({
    if(is.null(input$epic.degree.name )) {
      unique(master1$degree.name)} else {
        input$epic.degree.name
      }
  })
  #Reactive variable that uses selected choices or full column if empty 
  state_var <- reactive({
    if(is.null(input$epic.state)) {
      sort(unique(master1$State))} else {
        input$epic.state
      }
  })
  #Reactive variable that uses selected choices or full column if empty 
  experience_var <- reactive({
    if(is.null(input$epic.experience)) {
      unique(master1$Experience)} else {
        input$epic.experience
      }
  })
  #Reactive variable that uses selected choices or full column if empty
  occ.name_var <- reactive({
    if(is.null(input$epic.occ.name)) {
      unique(master1$occ.name)} else {
        input$epic.occ.name
      }
  })  
  #Reactive variable that uses selected choices or full column if empty
  entry.degree_var <- reactive({
    if(is.null(input$epic.entry.degree)) {
      unique(master1$entry.degree)} else {
        input$epic.entry.degree
      }
  })  
  #Reactive variable that uses selected choices or full column if empty
  cip.name_var <- reactive({
    if(is.null(input$epic.cip.name)) {
      unique(master1$cip.name)} else {
        input$epic.cip.name
      }
  })
  cip.cat_var <- reactive ({
    if(is.null(input$epic.cip.cat)){
      unique(master1$cip.cat)} else {
        cip1$CIP_Code[cip1$CIP_Category %in% input$epic.cip.cat]
      }
  })
  occ.cat_var <- reactive ({
    if(is.null(input$epic.occ.cat)){
      unique(master1$soc.cat)} else {
        soc1$SOC_Code[soc1$SOC_Cat_Name %in% input$epic.occ.cat]
      }
  })
 
  #Add button 
  observeEvent(input$add_scenarios, {
    scenario_to_add <- table_var()[input$epic.choice.table_rows_selected,]
    scenario_temp <<- rbind(scenario_temp, scenario_to_add)
    row.names(scenario_temp) <<- 1:nrow(scenario_temp)
  })
  #Delete Button  
  observeEvent(input$delete_scenario, {
    if(length(input$epic.scenarios.table_rows_selected)>= 1){
      scenario_temp <<- scenario_temp[-input$epic.scenarios.table_rows_selected,]
      if(nrow(scenario_temp)>= 1){
        row.names(scenario_temp) <<- 1:nrow(scenario_temp)}
    }
  })
  #Filter for First Table
  table_var <- reactive({
    filter(master1, school.name %in% school.name_var(), degree.name %in% degree.name_var(),
           cip.cat %in% cip.cat_var(), cip.name %in% cip.name_var(), State %in% state_var(), occ.name %in% occ.name_var(),
           soc.cat %in% occ.cat_var(), Experience %in% experience_var(), InStOff <= input$epic.tuition, X17p >= input$epic.income, entry.degree %in% entry.degree_var())
  })
  
  observe({
    if(is.null(input$epic.school.name)) {
      updateSelectInput(session, "epic.school.name", "School Name:", choices = sort(unique(table_var()$school.name)))  
    }
    if(is.null(input$epic.degree.name)) {
      updateSelectInput(session, "epic.degree.name", "Degree Name:", choices = unique(table_var()$degree.name))
    }
    if(is.null(input$epic.state)) {
      updateSelectInput(session, "epic.state", "State:", choices = sort(unique(table_var()$State)))
    }
    if(is.null(input$epic.occ.name)) {
      updateSelectInput(session, "epic.occ.name", "Occupation Name:", choices = sort(unique(table_var()$occ.name)))
    }
    if(is.null(input$epic.cip.name)) {
      updateSelectInput(session, "epic.cip.name", "Curriculum Name:", choices = sort(unique(table_var()$cip.name)))
    }
    if(is.null(input$epic.cip.cat)) {
      updateSelectInput(session, "epic.cip.cat", "Curriculum Category:",
                        choices = cip1$CIP_Category[cip1$CIP_Code %in% table_var()$cip.cat])
    }
    if(is.null(input$epic.occ.cat)){
      updateSelectInput(session, "epic.occ.cat", "Occupation Category:", 
                        choices = soc1$SOC_Cat_Name[soc1$SOC_Code %in% table_var()$soc.cat])
    }
  })
  
  #Explore schools table
  observe ( {  
    output$epic.schools.table <- renderDataTable({
      DT::datatable(data = table_var() %>% distinct(table_var()$school.name, .keep_all = TRUE)  %>% select(input$schools.column.names), 
                    options = list(pageLength = input$RecordsNum, filter = FALSE, order = list(list(3, 'sc'))) ,selection = list(mode = "multiple"))
    })
  })
  
  #Explore jobs table
  observe ( {  
    output$epic.jobs.table <- renderDataTable({
      DT::datatable(data = table_var() %>% distinct(table_var()$occ.name, .keep_all = TRUE)  %>% select(input$jobs.column.names), 
                    options = list(pageLength = input$RecordsNum, filter = FALSE, order = list(list(4, 'desc'))) ,selection = list(mode = "multiple"))
    })
  })
  
  #Choice Table
  observe ( {  
    output$epic.choice.table <- renderDataTable({
      DT::datatable(data = table_var()  %>% select(input$basic.column.names), 
                    options = list(pageLength = input$RecordsNum, filter = FALSE),selection = list(mode = "single"))
    })
  })
  
  observe ( {
    req(input$add_scenarios | input$delete_scenario | input$load_scenario)
      output$epic.scenarios.table <- renderDataTable({
        DT::datatable(data = scenario_temp %>% select(input$basic.column.names), rownames = TRUE,
                      options = list(pageLength = input$RecordsNum, filter = FALSE),selection = list(mode = "single"))
      })
  })
  
  #Table prep with filters and Column choices for second table

  observeEvent(input$add_one_compare, {
    pc_index1 <<- input$epic.scenarios.table_rows_selected
    if(!is.null(pc_index1)) {
      output$row.choice.table1 <- renderUI({
        place_card(pc_index1)
      })
    }
  })
  observeEvent(input$add_two_compare, {
    pc_index2 <<- input$epic.scenarios.table_rows_selected
    if(!is.null(pc_index2)) {
      output$row.choice.table2 <- renderUI({
        place_card(pc_index2)
      })
    }
  })
  observeEvent(input$add_three_compare, {
    pc_index3 <<- input$epic.scenarios.table_rows_selected
    if(!is.null(pc_index3)) {
      output$row.choice.table3 <- renderUI({
        place_card(pc_index3)
      })
    }
  })
  #Clear All choice boxes
  observeEvent(input$clear_all, {
    output$row.choice.table1 <- renderUI(NULL)
    output$row.choice.table2 <- renderUI(NULL)
    output$row.choice.table3 <- renderUI(NULL)
    pc_index1 <<- 0
    pc_index2 <<- 0
    pc_index3 <<- 0
    output$cummulative.plot <- renderPlot({
      NULL
    })
  })
  observeEvent(input$create_data,{
    cummulative_graph <<- cummulative_graph[FALSE,]
    num_years2 <- as.numeric(input$num_years)
    create_years(num_years2)
    if(pc_index1 > 0) {
      index <- pc_index1
      create_data(index, num_years2)
    }
    if(pc_index2 > 0) {
      index <- pc_index2
      create_data(index, num_years2)
    }
    if(pc_index3 > 0) {
      index <- pc_index3
      create_data(index, num_years2)
    }   

    output$cummulative.plot <- renderPlot({
      graph_parameters
    })
    output$temp.table <- renderDataTable({
      ({
        DT::datatable(data = cummulative_graph , rownames = TRUE,
                      options = list(pageLength = input$RecordsNum, filter = FALSE),selection = list(mode = "single"))
      })
    })
  })
  #Save scenario
  observeEvent(input$save_scenario,{
    filename <- paste0(input$userName, ".rds")
    saveRDS(scenario_temp, filename)
    drop_upload(filename, path = "responses")
    shinyalert(title = "Saved!", type = "success")
  })
  #Load scenario
  observeEvent(input$load_scenario, {
    filename2 <- paste0("responses/",input$userName, ".rds")
    if(drop_exists(filename2) == FALSE) {
      shinyalert(title = "File Not Found", type = "error")
    } else {
      #      filename2 <- paste0("responses", filename)
      drop_download(filename2, overwrite = TRUE)
      filename <- paste0(input$userName, ".rds")
      scenario_temp <<- readRDS(filename)
      shinyalert(title = "Loaded", type = "success")
    } 
  })
  observeEvent(input$add_user, {
    ### This is the pop up board for input a new row
    showModal(modalDialog(title = "Add a new account",
                          textInput(paste0("Names_add", input$Add_row_head), "Name"),
                          textInput(paste0("Password_add", input$Add_row_head), "Password"), 
                          actionButton("go", "Add item"),
                          easyClose = TRUE, footer = NULL ))
  })
  ### Add a new row to DT  
  observeEvent(input$go, {
    new_row=data.frame(
      username_id = input[[paste0("Names_add", input$Add_row_head)]],
      passod = sapply(input[[paste0("Password_add", input$Add_row_head)]],password_store),
      permission = "advanced",
      stringsAsFactors = FALSE
    )
    credentials <<- rbind(credentials, new_row)
    saveRDS(credentials, "cred.rds")
    drop_upload("cred.rds", path = "responses")
    removeModal()
  })
}

shinyApp(ui, server)