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
selectedrowindex = 0
#Read in main data table from your local directory
#master1 <- read.csv("https://www.dropbox.com/s/fgty42qwpkzudwz/master1.txt?dl=1", stringsAsFactors = F)
################## new way to read in comma delineated file on locate machine.
#master1 <- read.csv("Master1.csv")
#Read cip data table and order alphabetically
#cip2 <- read_tsv("cip_code.txt")
cip1 <- cip2[order(cip2$CIP_Category),]
#Read soc data table and order alphabetically
#soc2 <- read_tsv("soc_code.txt")
soc1 <- soc2[order(soc2$SOC_Cat_Name),]
#split soc into two groups
soc_group1 <- (soc1$SOC_Cat_Name[1:12])
soc_group2 <- (soc1$SOC_Cat_Name[13:24])
#spit cip into 4 groups
cip_group1 <- (cip1$CIP_Category[1:10])
cip_group2 <- (cip1$CIP_Category[11:19])
cip_group3 <- (cip1$CIP_Category[20:28])
cip_group4 <- (cip1$CIP_Category[29:37])
salary1 <- data.frame(age1 = double(), age_factor1 = double(), xsalary1 = double(), run_total1 = double())
salary2 <- data.frame(age1 = double(), age_factor1 = double(), xsalary1 = double(), run_total1 = double())
salary3 <- data.frame(age1 = double(), age_factor1 = double(), xsalary1 = double(), run_total1 = double())
# Data frame for roi graph
roi.data <- data.frame(school.n = character(), roi.n = factor())
# Data frame to convert degree code to number of years for degree
deg.code <- c(1,2,3,4,5,6,7,8,13,14,17,18,19,"")
years <- c(1,2,2,3,4,1,2,1,3,1,2,1,1,1)
num.years <- data.frame(deg.code, years)

ui <- dashboardPagePlus(
  header = dashboardHeaderPlus(
    title = "E.P.I.C. Planning", titleWidth = 230,
    left_menu = tagList(
      dropdownBlock(
        id = "scenerio",
        title = "Scenerios",
        icon = "list",
        radioButtons(
          inputId = "scenerio_level",
          label = "Level of Scenerio",
          choices = c("Basic" = "basic",
                      "Advanced" = "advanced"))),
      
      dropdownBlock(
        id = "competency",
        title = "Competency",
        icon = "list",
        radioButtons(
          inputId = "competency_level",
          label = "Competency",
          choices = c("Low" = "low_comp", "Average" = "ave_comp", "High" = "hi_comp"),
          selected = "ave_comp"
        )))),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home Page", tabName = "home",
               menuSubItem("Instructions", tabName = "instructions")),
      #      menuItem("My Lifestyle Stuff", tabName = "life profile", icon = icon("user"),
      #               menuSubItem("Household Structure", tabName = "household"),
      #               menuSubItem("One or more incomes?", tabName = "incomes"),
      #               menuSubItem("Kids or no kids?", tabName = "kids"),
      #               menuSubItem("Rich (new), Average (used), Poor (free)?", tabName = "class")),
      menuItem("My School Stuff", tabName = "ed profile", icon = icon("user"),
               menuSubItem("Explore Schools", tabName = "exploreschools"),
               menuSubItem("School Select", tabName = "school"),
               menuSubItem("Curriculum Select", tabName = "curriculum"),
               menuSubItem("Degree Select", tabName = "degree"),
               menuSubItem("Annual Total Cost", tabName = "tuition"),
               menuSubItem("State", tabName = "state")),
      menuItem("My Job Stuff", tabName = "occ profile", icon = icon("user"),
               menuSubItem("Explore Jobs", tabName = "explorejobs"),
               menuSubItem("Occupation Select", tabName = "occupation"),
               menuSubItem("Entry level degree", tabName = "entrydegree"),
               menuSubItem("Experience Required", tabName = "experience"),
               menuSubItem("Desired Income", tabName = "salary")),
      #      menuItem("My Peers", tabName = "peers", icon = icon("user"),
      #               menuSubItem("Similar lifestyle", tabName = "peer life"),
      #               menuSubItem("Similar schooling", tabName = "peer school"),
      #               menuSubItem("Similar job", tabName = "peer job"),
      #               menuSubItem("Best match", tabName = "peer match")),
      menuItem("Build Scenerios", tabName = "buildbasic", icon = icon("tasks")),
      menuItem("Compare Scenarios:", tabName = "compare", icon = icon("tasks")),
      menuItem("Tools", tabName = "tools", icon = icon("toolbox")),
      menuItem("About", tabName = "about", icon = icon("info"))
    )
  ),
# This section defines how the menu tabs function when selected. If the tabs are not defined or are not
# selected, no action takes place and the menus are not displayed.

    dashboardBody(
    tabItems(
      tabItem(tabName = "home",
              h2("Welcome to EPIC, the Education-Profession Investment Calculator")
      ),
      tabItem(tabName = "profile",
              h2("Thanks")
      ),
      tabItem(tabName = "about",
              h2("Hello and Welcome to your EPIC experience"),
              h2("Test drive the world's first fully 
                 integrated"),
              h2("Education-Profession Investment Calculator")
      ),
      tabItem(tabName = "tools",
              h2("This is where tools go."),
              box(width = 4,
                  radioButtons(inputId = "RecordsNum",
                               label = "Number of records per page",
                               choices = c(10, 20, 50, 100),
                               selected = 10)),
              box(width = 4,
                  radioButtons(inputId = "unique.search",
                               label = "Level of uniqueness",
                               choices = c("Normal", "Curriculum", "Occupation", "School"),
                               selected = "Normal")),
              box(width = 4,
                  h4("Explore career occupations: https://www.onetonline.org/"),
                  h4("Explore schools: https://collegescorecard.ed.gov/"),
                  h4("Skills assessment: https://www.careeronestop.org/"),
                  h4("Advanced Skills assessment: https://www.wowi.com/"),
                  h4("What do you want to do for a living? https://www.mynextmove.org/")),
              box(width = 4,
                  checkboxGroupInput(inputId = "column.names", label = "Pick the scenario columns you would like",
                                     selected = c("school.name", "X17p", "degree.name", "occ.name", "cip.name", "entry.degree",
                                                  "Experience", "InStOff"), choices = names(master1))),
              box(width = 4,
                  checkboxGroupInput(inputId = "jobs.column.names", label = "Pick the jobs columns you would like",
                                     selected = c("X17p", "X50p", "X82p", "occ.name", "entry.degree",
                                                  "Experience"), choices = names(master1))),
              box(width = 4,
                  checkboxGroupInput(inputId = "schools.column.names", label = "Pick the schoool columns you would like",
                                     selected = c("school.name", "InStOff", "Ouston", "OuStOff",
                                                   "IGrant.Avg", "IGrant..age"), choices = names(master1)))
      ),
      tabItem(tabName = "instructions",
              h2("Click on your school and job preferences. If you really don't know, leave it blank")
      ),
      tabItem(tabName = "school",
              h3("I would consider these schools:"),
              box(
                width = 5,
                selectInput(inputId = "pre.school.name",
                            label = NULL,
                            choices = levels(master1$school.name),
                            multiple = TRUE)
                )
              ),
      tabItem(tabName = "degree",
              h3("I want to get this degree and then get a job"),
              box(
                width = 5,
                selectInput(inputId = "pre.degree.name",
                            label = NULL,
                            choices = levels(master1$degree.name),
                            multiple = TRUE)
                )
              ),
      tabItem(tabName = "entrydegree",
              h3("My occupation requires at least this degree level"),
              box(
                width = 5,
                selectInput(inputId = "pre.entry.degree",
                            label = NULL,
                            choices = levels(master1$entry.degree),
                            multiple = TRUE)
                )
              ),
      tabItem(tabName = "experience",
              h3("I need this much work experience to get this job"),
              box(
                width = 5,
                selectInput(inputId = "pre.experience",
                            label = NULL,
                            choices = levels(master1$Experience),
                            multiple = TRUE)
                )
              ),
      tabItem(tabName = "state",
              h3("I would consider schools in these states:"),
              box(
                width = 5,
                selectInput(inputId = "pre.state",
                            label = NULL,
                            choices = levels(master1$State),
                            multiple = TRUE)
                )
              ),
      tabItem(tabName = "occupation",
              h3("I would consider jobs in these areas"),
              fluidRow(
                box(
                  width = 4,
                  checkboxGroupInput(inputId = "pre.occupation1",
                                     label = NULL,
                                     choices = soc_group1)),
                box(
                  width = 4,
                  checkboxGroupInput(inputId = "pre.occupation2",
                                     label = NULL,
                                     choices = soc_group2))
                )
              ),
      tabItem(tabName = "curriculum",
              h3("I would consider studying these areas"),
              fluidRow(
                box(width = 3,
                    checkboxGroupInput(inputId = "survey.Cip_Category1",
                                       label = NULL,
                                       choices = cip_group1)
                ),
                box(width = 3,
                    checkboxGroupInput(inputId = "survey.Cip_Category2",
                                       label = NULL,
                                       choices = cip_group2)
                ),
                box(width = 3,
                    checkboxGroupInput(inputId = "survey.Cip_Category3",
                                       label = NULL,
                                       choices = cip_group3)
                ),
                box(width = 3,
                    checkboxGroupInput(inputId = "survey.Cip_Category4",
                                       label = NULL,
                                       choices = cip_group4)
                    )
                )
              ),
      tabItem(tabName = "salary",
              h3("I would like to make at least this amount when I get out of school"),
              box(
                sliderInput(inputId = "pre.income",
                            label = NULL,
                            value = min(sort(unique(master1$X17p))),
                            min = min(sort(unique(master1$X17p))),
                            max = max(sort(unique(master1$X17p))))
                )
              ),
      tabItem(tabName = "tuition",
              h3("I don't want to spend more than this amount per year for school"),
              box(
                sliderInput(inputId = "pre.tuition",
                            label = NULL,
                            value = max(sort(unique(master1$InStOff))),
                            min = min(sort(unique(master1$InStOff))),
                            step = 1000,
                            max = max(sort(unique(master1$InStOff))))
                )
              ),
      tabItem(tabName = "exploreschools",
              fluidRow(
                box(width = 2,
                    selectInput(inputId = "nvs.school.name",
                                label= "School Name:",
                                choices =  levels(master1$school.name),
                                multiple = TRUE)),
                box(width = 2,
                    sliderInput(inputId = "nvs.tuition",
                                label = "Desired Tuition Level",
                                value = max(sort(unique(master1$InStOff))),
                                min = min(sort(unique(master1$InStOff))),
                                step = 1000,
                                max = max(sort(unique(master1$InStOff))))),
                box(width = 2,
                    
                    selectInput(inputId = "nvs.cip.cat",
                                label = "Curriculum Category:",
                                choices = cip1$CIP_Category,
                                multiple = TRUE)),
                box(width = 2,
                    
                    selectInput(inputId = "nvs.cip.name",
                                label = "Curriculum Name:",
                                choices = levels(master1$cip.name),
                                multiple = TRUE)),
                box(width = 2,
                    
                    selectInput(inputId = "nvs.state",
                                label = "State:",
                                choices = levels(master1$State),
                                multiple = TRUE)),
                box(width = 2,
                    selectInput(inputId = "nvs.degree.name",
                                label = "Degree Name:",
                                choices =  levels(master1$degree.name),
                                multiple = TRUE))
                ),
                box(
                  width = 12,
                  div(style = 'overflow-x: scroll',DT::dataTableOutput(outputId = "nvs.schools.table"))
                )
              ),
      tabItem(tabName = "explorejobs",
              fluidRow(
                box(width = 2,
                    sliderInput(inputId = "nvs.income",
                                label = "Desired Income Level:",
                                value = min(sort(unique(master1$X17p))),
                                min = min(sort(unique(master1$X17p))),
                                max = max(sort(unique(master1$X17p))))),
                box(width = 3,
                    selectInput(inputId = "nvs.occ.cat",
                                label = "Occupation Category:",
                                choices = soc1$SOC_Cat_Name,
                                multiple = TRUE)),
                box(width = 3,
                    selectInput(inputId = "nvs.occ.name",
                                label = "Occupation Name:",
                                choices = levels(master1$occ.name),
                                multiple = TRUE)),
                box(width = 2,
                    selectInput(inputId = "nvs.experience",
                                label = "Experience:",
                                choices =  levels(master1$Experience),
                                multiple = TRUE)),
                box(width = 2,
                    selectInput(inputId = "nvs.entry.degree",
                                label = "Rqd Entry Degree:",
                                choices =  levels(master1$entry.degree),
                                multiple = TRUE))
                ),
                box(
                  width = 12,
                  div(style = 'overflow-x: scroll',DT::dataTableOutput(outputId = "nvs.jobs.table"))
                )
              ),
      tabItem(tabName = "buildbasic",
              fluidRow(
                box(width = 2,
#                    h3("School"),
                    selectInput(inputId = "nvs.school.name",
                                label= "School Name:",
                                choices =  levels(master1$school.name),
                                multiple = TRUE),

                    sliderInput(inputId = "nvs.tuition",
                                label = "Desired Tuition Level",
                                value = max(sort(unique(master1$InStOff))),
                                min = min(sort(unique(master1$InStOff))),
                                step = 1000,
                                max = max(sort(unique(master1$InStOff))))),
                box(width = 2,

                    selectInput(inputId = "nvs.cip.cat",
                                label = "Curriculum Category:",
                                choices = cip1$CIP_Category,
                                multiple = TRUE),

                    selectInput(inputId = "nvs.cip.name",
                                label = "Curriculum Name:",
                                choices = levels(master1$cip.name),
                                multiple = TRUE)),
                box(width = 2,

                    selectInput(inputId = "nvs.state",
                                label = "State:",
                                choices = levels(master1$State),
                                multiple = TRUE),
                    selectInput(inputId = "nvs.degree.name",
                                label = "Degree Name:",
                                choices =  levels(master1$degree.name),
                                multiple = TRUE)),
#                h3("Job"),
                box(width = 2,
                    selectInput(inputId = "nvs.occ.cat",
                                label = "Occupation Category:",
                                choices = soc1$SOC_Cat_Name,
                                multiple = TRUE),
                    selectInput(inputId = "nvs.occ.name",
                                label = "Occupation Name:",
                                choices = levels(master1$occ.name),
                                multiple = TRUE)),
                box(width = 2,
                    selectInput(inputId = "nvs.experience",
                                label = "Experience:",
                                choices =  levels(master1$Experience),
                                multiple = TRUE),
                    selectInput(inputId = "nvs.entry.degree",
                                label = "Rqd Entry Degree:",
                                choices =  levels(master1$entry.degree),
                                multiple = TRUE)),
                box(width = 2,
                    sliderInput(inputId = "nvs.income",
                                label = "Desired Income Level:",
                                value = min(sort(unique(master1$X17p))),
                                min = min(sort(unique(master1$X17p))),
                                max = max(sort(unique(master1$X17p))))
                    ),
                box(
                  width = 12,
                  div(style = 'overflow-x: scroll',DT::dataTableOutput(outputId = "nvs.choice.table"))
                  )
                )
              ),
      tabItem(tabName = "compare",
              fluidRow(
                uiOutput(outputId = "tenure.select")
                ),
              fluidRow(
                uiOutput(outputId = "row.choice.table1"),
                uiOutput(outputId = "row.choice.table2"),
                uiOutput(outputId = "row.choice.table3")
                ),
              hr(),
              fluidRow(
                box(width = 4,
                    DT::dataTableOutput(outputId = "row.choice.wage1")
                    ),
                box(width = 4,
                    DT::dataTableOutput(outputId = "row.choice.wage2")
                    ),
                box(width = 4,
                    DT::dataTableOutput(outputId = "row.choice.wage3")
                    )
                ),
              hr(),
              fluidRow(
                box(width = 6,
                    plotOutput("cummulative.plot")
                    ),
                box(width = 6,
                    plotOutput("roi.plot"))
                )
              )
      )
    )
)

server <- function(input, output, session) {
  #Reactive variable that uses selected choices or full column if empty
  school.name_var <- reactive({
    if(is.null(input$nvs.school.name )) {
      unique(master1$school.name)} else {
        input$nvs.school.name
      }
  })
  #Reactive variable that uses selected choices or full column if empty 
  degree.name_var <- reactive({
    if(is.null(input$nvs.degree.name )) {
      unique(master1$degree.name)} else {
        input$nvs.degree.name
      }
  })
  #Reactive variable that uses selected choices or full column if empty 
  state_var <- reactive({
    if(is.null(input$nvs.state)) {
      unique(master1$State)} else {
        input$nvs.state
      }
  })
  #Reactive variable that uses selected choices or full column if empty 
  experience_var <- reactive({
    if(is.null(input$nvs.experience)) {
      unique(master1$Experience)} else {
        input$nvs.experience
      }
  })
  #Reactive variable that uses selected choices or full column if empty
  occ.name_var <- reactive({
    if(is.null(input$nvs.occ.name)) {
      unique(master1$occ.name)} else {
        input$nvs.occ.name
      }
  })  
  #Reactive variable that uses selected choices or full column if empty
  entry.degree_var <- reactive({
    if(is.null(input$nvs.entry.degree)) {
      unique(master1$entry.degree)} else {
        input$nvs.entry.degree
      }
  })  
  #Reactive variable that uses selected choices or full column if empty
  cip.name_var <- reactive({
    if(is.null(input$nvs.cip.name)) {
      unique(master1$cip.name)} else {
        input$nvs.cip.name
      }
  })
  cip.cat_var <- reactive ({
    if(is.null(input$nvs.cip.cat)){
      unique(master1$cip.cat)} else {
        cip1$CIP_Code[cip1$CIP_Category %in% input$nvs.cip.cat]
      }
  })
  occ.cat_var <- reactive ({
    if(is.null(input$nvs.occ.cat)){
      unique(master1$soc.cat)} else {
        soc1$SOC_Code[soc1$SOC_Cat_Name %in% input$nvs.occ.cat]
      }
  })
  occ_var <- reactive ({
    soc1$SOC_Cat_Name[soc1$SOC_Cat_Name %in% input$pre.occupation1 | soc1$SOC_Cat_Name %in% input$pre.occupation2]
  })
  cip_var <- reactive ({
    cip1$CIP_Category[cip1$CIP_Category %in% input$survey.Cip_Category1 |cip1$CIP_Category %in% input$survey.Cip_Category2 |
                        cip1$CIP_Category %in% input$survey.Cip_Category3 | cip1$CIP_Category %in% input$survey.Cip_Category4]
  })
  #Filter for First Table
  table_var1 <- reactive({
    filter(master1, school.name %in% school.name_var(), degree.name %in% degree.name_var(),
           cip.cat %in% cip.cat_var(), cip.name %in% cip.name_var(), State %in% state_var(), occ.name %in% occ.name_var(),
           soc.cat %in% occ.cat_var(), Experience %in% experience_var(), InStOff <= input$nvs.tuition, X17p >= input$nvs.income, entry.degree %in% entry.degree_var())
  })
  
  table_var <- reactive ({
    if((input$unique.search) == 'School') {
      table_var1() %>% distinct(table_var1()$school.name, .keep_all = TRUE)
    } else if((input$unique.search) == 'Curriculum') { 
      table_var1() %>% distinct(table_var1()$cip.name, .keep_all = TRUE)
    } else if((input$unique.search) == 'Occupation') {
      table_var1() %>% distinct(table_var1()$occ.name, .keep_all = TRUE)
    } else {
      table_var1()
    }
  })
  observe ({
    req(cip_var())
    updateSelectInput(session, "nvs.cip.cat", "Curriculum Category:", selected = cip_var())
  })
  observe({
    req(occ_var())
    updateSelectInput(session, "nvs.occ.cat", "Occupation Category:", selected = occ_var())
  }) 
  observe({
    if(is.null(input$nvs.school.name)) {
      updateSelectInput(session, "nvs.school.name", "School Name:", choices = unique(table_var()$school.name))  
    }
    if(is.null(input$nvs.degree.name)) {
      updateSelectInput(session, "nvs.degree.name", "Degree Name:", choices = unique(table_var()$degree.name))
    }
    if(is.null(input$nvs.state)) {
      updateSelectInput(session, "nvs.state", "State:", choices = unique(table_var()$State))
    }
    if(is.null(input$nvs.occ.name)) {
      updateSelectInput(session, "nvs.occ.name", "Occupation Name:", choices = unique(table_var()$occ.name))
    }
    if(is.null(input$nvs.cip.name)) {
      updateSelectInput(session, "nvs.cip.name", "Curriculum Name:", choices = unique(table_var()$cip.name))
    }
    if(is.null(input$nvs.cip.cat)) {
      updateSelectInput(session, "nvs.cip.cat", "Curriculum Category:",
                        choices = cip1$CIP_Category[cip1$CIP_Code %in% table_var()$cip.cat])
    }
    if(is.null(input$nvs.occ.cat)){
      updateSelectInput(session, "nvs.occ.cat", "Occupation Category:", 
                        choices = soc1$SOC_Cat_Name[soc1$SOC_Code %in% table_var()$soc.cat])
    }
  })

  #Explore schools table
  observe ( {  
    output$nvs.schools.table <- renderDataTable({
      DT::datatable(data = table_var()  %>% select(input$schools.column.names), 
                    options = list(pageLength = input$RecordsNum, filter = FALSE),selection = list(mode = "multiple"))
    })
  })
  
  #Explore jobs table
  observe ( {  
    output$nvs.jobs.table <- renderDataTable({
      DT::datatable(data = table_var()  %>% select(input$jobs.column.names), 
                    options = list(pageLength = input$RecordsNum, filter = FALSE),selection = list(mode = "multiple"))
    })
  })
  
  #First Table
  observe ( {  
    output$nvs.choice.table <- renderDataTable({
      DT::datatable(data = table_var()  %>% select(input$column.names), 
                    options = list(pageLength = input$RecordsNum, filter = FALSE),selection = list(mode = "multiple"))
    })
  })
  #ObserveEvents go back here  
  # This is where the first preferences get loaded intot he Basic Search menus
  # from school choice on preference page  
  observeEvent(input$pre.school.name, {
    updateSelectInput(session, "nvs.school.name", "School Name:", selected = input$pre.school.name)
  })
  #Import degree choice to scenerio from degree choice on preference page  
  observeEvent(input$pre.degree.name, {
    updateSelectInput(session, "nvs.degree.name", "Degree Name:", selected = input$pre.degree.name)
  })
  #Import degree choice to scenerio from degree choice on preference page  
  observeEvent(input$pre.state, {
    updateSelectInput(session, "nvs.state", "State:", selected = input$pre.state)
  })
  # from income level choice on preference page  
  observeEvent(input$pre.income, {
    updateSliderInput(session, "nvs.income", "Desired Income Level:", value = input$pre.income)
  })
  # from income level choice on preference page  
  observeEvent(input$pre.experience, {
    updateSliderInput(session, "nvs.experience", "Required experience:", value = input$pre.experience)
  })
  # from income level choice on preference page  
  observeEvent(input$pre.entry.degree, {
    updateSliderInput(session, "nvs.entry.degree", "Required entry degree:", value = input$pre.entry.degree)
  })
  # from tuition cost level choice on preference page  
  observeEvent(input$pre.tuition, {
    updateSliderInput(session, "nvs.tuition", "Maximum annual education cost:", value = input$pre.tuition)
  })
  #Table prep with filters and Column choices for second table
  new_var <- reactive({
    table_var()[input$nvs.choice.table_rows_selected,]
    # %>% select(occ.name, school.name, degree.name, X17p, InStOff, degree.code)
    #    table_var() %>% select(input$column.names)
  })
  observe({
    req(input$scenerio_level == "basic")
    output$tenure.select <- renderUI({
      fluidRow(
        box(width = 2,
            numericInput(inputId = "age.begin", label = "Age to begin", min = 17, max = 69, value =  17)
        ),
        box(width = 2,
            numericInput(inputId = "career.length", label = "Length of career", min = 1, max = 53, value = 20)
        ))  
    })
  })
  observe({
    req(input$scenerio_level == "advanced")
    output$tenure.select <- renderUI({
      fluidRow(
        box(width = 2,
            numericInput(inputId = "tenure.length", label = "Length of Tenure", min = 0, max = 51, value = 20))
      )
    })
  })
  #choice Cards after selecting scenarios from table 1  
  observe ({
    req(input$age.begin, input$career.length)
    if(nrow(new_var()) > 0) {
      output$row.choice.table1 <- renderUI({
        box(width = 4,
            strong("Occupation :"), 
            new_var()$occ.name[1], br(),
            strong("School :"), 
            new_var()$school.name[1], br(),
            strong("Curriculum :"), 
            new_var()$cip.name[1], br(),
            strong("Degree :"), 
            new_var()$degree.name[1], br(),
            strong("Salary :"), 
            new_var()$X17p[1], br(),
            strong("Tuition :"), 
            new_var()$InStOff[1])
      })
      dc <- new_var()$degree.code[1]
      years <- as.numeric (filter(num.years, deg.code %in% dc) %>% select(years))
      y <- 0
      a <- input$age.begin - 1
      for(i in (1:years)) {
        af <- (1 + ((0.00002 * ((i + a) ^ 2)) - 0.0034 * (i + a) + 0.127 ))
        x <- as.double(new_var()[1,] %>% select(InStOff))
        y <- y - x
        s1 <- list(age1 = (i + a), age_factor1 = af, xsalary1 = x, run_total1 = y)
        salary1 <- rbind(salary1, s1)
      }
      x <- as.double(new_var()[1,] %>% select(X17p))
      a <- input$age.begin + years - 1
      af <- (1 + ((0.00002 * (a ^ 2)) - 0.0034 * a + 0.127 ))
      x <- x / af
      for(i in (1:input$career.length)) {
        af <- (1 + ((0.00002 * ((i + a)^ 2)) - 0.0034 * (i + a) + 0.127 ))
        x <- x * af
        x <- round(x, 0)
        y <- x + y
        s1 <- list(age1 = (i + a), age_factor1 = af, xsalary1 = x, run_total1 = y)
        salary1 <- rbind(salary1, s1)
      }
      totalcost1 <- new_var()$InStOff[1] * years
      runtot1 <- (salary1$run_total[nrow(salary1)])
      roi1 <- (runtot1 + totalcost1) / totalcost1 * 100
      r1 <- list(school.n = paste("1",new_var()$school.name[1], "\n", new_var()$occ.name[1]), roi.n = roi1)
      roi.data <- rbind(roi.data, r1)
      
      #      output$row.choice.wage1 <- renderDataTable({
      #        DT::datatable(data = salary1, options = list(pageLength = 10, searching = FALSE, ordering = FALSE),
      #                      selection = "none")
      #      })
      output$cummulative.plot <- renderPlot({
        ggplot() + geom_line(data = salary1, aes(x = age1 ,y = run_total1/1000, colour="First"),
                             show.legend = TRUE) +
          scale_colour_manual(name="Occupation", values = c("First" = "blue", "Second" = "green", "Third" = "red"),
                              labels = new_var()$school.name) +
          xlab('AGE') +
          ylab('Total Earnings') +
          labs(title = "Cummulative Cash Flow") +
          theme(plot.title = element_text(hjust = 0.5))
      })
      output$roi.plot <- renderPlot({
        ggplot(roi.data, aes(x=school.n, y = roi.n)) + geom_bar(stat = "identity", width = 0.4) +
          xlab('School') + ylab('Percent') + labs(title = "ROI") +
          theme(plot.title = element_text(hjust = 0.5))
      })
    }
    if(nrow(new_var()) > 1) {
      output$row.choice.table2 <- renderUI({
        box(width = 4,
            strong("Occupation :"), 
            new_var()$occ.name[2], br(),
            strong("School :"), 
            new_var()$school.name[2], br(),
            strong("Curriculum :"), 
            new_var()$cip.name[2], br(),
            strong("Degree :"), 
            new_var()$degree.name[2], br(),
            strong("Salary :"), 
            new_var()$X17p[2], br(),
            strong("Tuition :"), 
            new_var()$InStOff[2])
      })
      dc <- new_var()$degree.code[2]
      years <- as.numeric (filter(num.years, deg.code %in% dc) %>% select(years))
      y <- 0
      a <- input$age.begin - 1
      for(i in (1:years)) {
        af <- (1 + ((0.00002 * ((i + a) ^ 2)) - 0.0034 * (i + a) + 0.127 ))
        x <- as.double(new_var()[2,] %>% select(InStOff))
        y <- y - x
        s2 <- list(age1 = (i + a), age_factor1 = af, xsalary1 = x, run_total1 = y)
        salary2 <- rbind(salary2, s2)
      }
      x <- as.double(new_var()[2,] %>% select(X17p))
      a <- input$age.begin + years - 1
      af <- (1 + ((0.00002 * (a ^ 2)) - 0.0034 * a + 0.127 ))
      x <- x / af
      for(i in (1:input$career.length)) {
        af <- (1 + ((0.00002 * ((i + a)^ 2)) - 0.0034 * (i + a) + 0.127 ))
        x <- x * af
        x <- round(x, 0)
        y <- x + y
        s2 <- list(age1 = (i + a), age_factor1 = af, xsalary1 = x, run_total1 = y)
        salary2 <- rbind(salary2, s2)
      }
      totalcost1 <- new_var()$InStOff[2] * years
      runtot1 <- (salary2$run_total[nrow(salary2)])
      roi2 <- (runtot1 + totalcost1) / totalcost1 * 100
      r2 <- list(school.n = paste("2", new_var()$school.name[2], "\n", new_var()$occ.name[2]), roi.n = roi2)
      roi.data <- rbind(roi.data, data.frame(as.list(r2)))
      
      #      output$row.choice.wage2 <- renderDataTable({
      #        DT::datatable(data = salary2, options = list(pageLength = 10, searching = FALSE, ordering = FALSE),
      #                      selection = "none")
      #      })
      
      output$cummulative.plot <- renderPlot({
        ggplot() + geom_line(data = salary1, aes(x = age1 ,y = run_total1/1000, colour = "First"),
                             show.legend = TRUE) +
          geom_line(data = salary2, aes(x = age1, y = run_total1/1000, colour = "Second"), 
                    show.legend = TRUE) +
          scale_colour_manual(name="Occupation", values = c("First" = "blue", "Second" = "green", "Third" = "red"),
                              labels = new_var()$school.name)  +
          xlab('AGE') +
          ylab('Total Earnings') +
          labs(title = "Cummulative Cash Flow") +
          theme(plot.title = element_text(hjust = 0.5))
      })
      output$roi.plot <- renderPlot({
        ggplot(roi.data, aes(x = school.n, y = roi.n)) + geom_bar(stat = "identity", width = 0.3) +
          xlab('School') +
          ylab('Percent') +
          labs(title = "ROI") +
          theme(plot.title = element_text(hjust = 0.5))
      })
    }
    if(nrow(new_var()) > 2) {
      output$row.choice.table3 <- renderUI({
        box(width = 4,
            strong("Occupation :"), 
            new_var()$occ.name[3], br(),
            strong("School :"), 
            new_var()$school.name[3], br(),
            strong("Curriculum :"), 
            new_var()$cip.name[3], br(),
            strong("Degree :"), 
            new_var()$degree.name[3], br(),
            strong("Salary :"), 
            new_var()$X17p[3], br(),
            strong("Tuition :"), 
            new_var()$InStOff[3])
      })
      dc <- new_var()$degree.code[3]
      years <- as.numeric (filter(num.years, deg.code %in% dc) %>% select(years))
      y <- 0
      a <- input$age.begin - 1
      for(i in (1:years)) {
        af <- (1 + ((0.00002 * ((i + a) ^ 2)) - 0.0034 * (i + a) + 0.127 ))
        x <- as.double(new_var()[3,] %>% select(InStOff))
        y <- y - x
        s3 <- list(age1 = (i + a), age_factor1 = af, xsalary1 = x, run_total1 = y)
        salary3 <- rbind(salary3, s3)
      }
      x <- as.double(new_var()[3,] %>% select(X17p))
      a <- input$age.begin + years - 1
      af <- (1 + ((0.00002 * (a ^ 2)) - 0.0034 * a + 0.127 ))
      x <- x / af
      for(i in (1:input$career.length)) {
        af <- (1 + ((0.00002 * ((i + a)^ 2)) - 0.0034 * (i + a) + 0.127 ))
        x <- x * af
        x <- round(x, 0)
        y <- x + y
        s3 <- list(age1 = (i + a), age_factor1 = af, xsalary1 = x, run_total1 = y)
        salary3 <- rbind(salary3, s3)
      }
      totalcost1 <- new_var()$InStOff[3] * years
      runtot1 <- (salary3$run_total[nrow(salary3)])
      roi3 <- (runtot1 + totalcost1) / totalcost1 * 100
      r3 <- list(school.n = paste("3", new_var()$school.name[3], "\n", new_var()$occ.name[3]), roi.n = roi3)
      roi.data <- rbind(roi.data, data.frame(as.list(r3)))
      
      #      output$row.choice.wage3 <- renderDataTable({
      #        DT::datatable(data = salary3, options = list(pageLength = 10, searching = FALSE, ordering = FALSE),
      #                      selection = "none")
      #      })
      output$cummulative.plot <- renderPlot({
        ggplot() + geom_line(data = salary1, aes(x = age1 ,y = run_total1/1000, colour = "First"),
                             show.legend = TRUE) +
          geom_line(data = salary2, aes(x = age1, y = run_total1/1000, colour = "Second"),
                    show.legend = TRUE) +
          geom_line(data = salary3, aes(x = age1, y = run_total1/1000, colour = "Third"),
                    show.legend = TRUE) +
          scale_colour_manual(name="Occupation", values = c("First" = "blue", "Second" = "green", "Third" = "red"),
                              labels = new_var()$school.name)  +
          xlab('AGE') +
          ylab('Total Earnings') +
          labs(title = "Cummulative Cash Flow") +
          theme(plot.title = element_text(hjust = 0.5))
      })
      output$roi.plot <- renderPlot({
        ggplot(roi.data, aes(x = school.n, y = roi.n)) + geom_bar(stat = "identity", width = 0.3) +
          xlab('School') +
          ylab('Percent') +
          labs(title = "ROI") +
          theme(plot.title = element_text(hjust = 0.5))
      })
    }
  })
  observe ({
    req(input$tenure.length, input$competency_level == "ave_comp")
    if(nrow(new_var()) > 0) {
      output$row.choice.table1 <- renderUI({
        box(width = 4,
            strong("Occupation :"), 
            new_var()$occ.name[1], br(),
            strong("School :"), 
            new_var()$school.name[1], br(),
            strong("Degree :"), 
            new_var()$degree.name[1], br(),
            strong("Salary :"), 
            new_var()$X17p[1], br(),
            strong("Tuition :"), 
            new_var()$InStOff[1])
      })
      dc <- new_var()$degree.code[1]
      years <- as.numeric (filter(num.years, deg.code %in% dc) %>% select(years))
      y <- 0
      a <- 0
      age <- 0
      occf <- as.numeric(new_var()[1,] %>% select(MedOccF))
      for(i in (1:years)) {
        tf <- (1 + ((0.00002 * ((a) ^ 2)) - 0.0023 * (a) + 0.0603 ))
        x <- as.double(new_var()[1,] %>% select(InStOff))
        y <- y - x
        age <- age + 1
        s1 <- list(age1 = age, ten_factor1 = tf, occ_factor = new_var()$MedOccF[1], xsalary1 = x, run_total1 = y)
        salary1 <- rbind(salary1, s1)
      }
      x <- as.double(new_var()[1,] %>% select(X17p))
      a <- 0
      
      for(i in (0:input$tenure.length)) {
        tf <- (1 + ((0.00002 * ((i + a)^ 2)) - 0.0023 * (i + a) + 0.0603 ))
        x <- x * tf * occf
        x <- round(x, 0)
        y <- x + y
        age <- age + 1
        s1 <- list(age1 = age, ten_factor1 = tf, occ_factor = new_var()$MedOccF[1], xsalary1 = x, run_total1 = y)
        salary1 <- rbind(salary1, s1)
      }
      totalcost1 <- new_var()$InStOff[1] * years
      runtot1 <- (salary1$run_total[nrow(salary1)])
      roi1 <- (runtot1 + totalcost1) / totalcost1 * 100
      r1 <- list(school.n = paste("1",new_var()$school.name[1], "\n", new_var()$occ.name[1]), roi.n = roi1)
      roi.data <- rbind(roi.data, r1)
      
      output$row.choice.wage1 <- renderDataTable({
        DT::datatable(data = salary1, options = list(pageLength = 10, searching = FALSE, ordering = FALSE),
                      selection = "none")
      })
      output$cummulative.plot <- renderPlot({
        ggplot() + geom_line(data = salary1, aes(x = age1 ,y = run_total1/1000, colour="First"),
                             show.legend = TRUE) +
          scale_colour_manual(name="Occupation", values = c("First" = "blue", "Second" = "green", "Third" = "red"),
                              labels = new_var()$school.name) +
          xlab('Tenure') +
          ylab('Total Earnings') +
          labs(title = "Cummulative Cash Flow") +
          theme(plot.title = element_text(hjust = 0.5))
      })
      output$roi.plot <- renderPlot({
        ggplot(roi.data, aes(x=school.n, y = roi.n)) + geom_bar(stat = "identity", width = 0.4) +
          xlab('School') + ylab('Percent') + labs(title = "ROI") +
          theme(plot.title = element_text(hjust = 0.5))
      })
    }
    if(nrow(new_var()) > 1) {
      output$row.choice.table2 <- renderUI({
        box(width = 4,
            strong("Occupation :"), 
            new_var()$occ.name[2], br(),
            strong("School :"), 
            new_var()$school.name[2], br(),
            strong("Degree :"), 
            new_var()$degree.name[2], br(),
            strong("Salary :"), 
            new_var()$X17p[2], br(),
            strong("Tuition :"), 
            new_var()$InStOff[2])
      })
      dc <- new_var()$degree.code[2]
      years <- as.numeric (filter(num.years, deg.code %in% dc) %>% select(years))
      y <- 0
      a <- 0
      occf <- as.numeric(new_var()[2,] %>% select(MedOccF))
      age <- 0
      for(i in (1:years)) {
        tf <- (1 + ((0.00002 * ((a) ^ 2)) - 0.0023 * (a) + 0.0603 ))
        x <- as.double(new_var()[2,] %>% select(InStOff))
        y <- y - x
        age <- age + 1
        s2 <- list(age1 = age, ten_factor1 = tf, occ_factor = new_var()$MedOccF[2], xsalary1 = x, run_total1 = y)
        salary2 <- rbind(salary2, s2)
      }
      x <- as.double(new_var()[2,] %>% select(X17p))
      a <- 0
      for(i in (0:input$tenure.length)) {
        tf <- (1 + ((0.00002 * ((i + a)^ 2)) - 0.0023 * (i + a) + 0.0603 ))
        x <- x * tf * occf
        x <- round(x, 0)
        y <- x + y
        age <- age + 1
        s2 <- list(age1 = age, ten_factor1 = tf, occ_factor = new_var()$MedOccF[2], xsalary1 = x, run_total1 = y)
        salary2 <- rbind(salary2, s2)
      }
      totalcost1 <- new_var()$InStOff[2] * years
      runtot1 <- (salary2$run_total[nrow(salary2)])
      roi2 <- (runtot1 + totalcost1) / totalcost1 * 100
      r2 <- list(school.n = paste("2", new_var()$school.name[2], "\n", new_var()$occ.name[2]), roi.n = roi2)
      roi.data <- rbind(roi.data, data.frame(as.list(r2)))
      
      output$row.choice.wage2 <- renderDataTable({
        DT::datatable(data = salary2, options = list(pageLength = 10, searching = FALSE, ordering = FALSE),
                      selection = "none")
      })
      
      output$cummulative.plot <- renderPlot({
        ggplot() + geom_line(data = salary1, aes(x = age1 ,y = run_total1/1000, colour = "First"),
                             show.legend = TRUE) +
          geom_line(data = salary2, aes(x = age1, y = run_total1/1000, colour = "Second"), 
                    show.legend = TRUE) +
          scale_colour_manual(name="Occupation", values = c("First" = "blue", "Second" = "green", "Third" = "red"),
                              labels = new_var()$school.name)  +
          xlab('Tenure') +
          ylab('Total Earnings') +
          labs(title = "Cummulative Cash Flow") +
          theme(plot.title = element_text(hjust = 0.5))
      })
      output$roi.plot <- renderPlot({
        ggplot(roi.data, aes(x = school.n, y = roi.n)) + geom_bar(stat = "identity", width = 0.3) +
          xlab('School') +
          ylab('Percent') +
          labs(title = "ROI") +
          theme(plot.title = element_text(hjust = 0.5))
      })
    }
    if(nrow(new_var()) > 2) {
      output$row.choice.table3 <- renderUI({
        box(width = 4,
            strong("Occupation :"), 
            new_var()$occ.name[3], br(),
            strong("School :"), 
            new_var()$school.name[3], br(),
            strong("Degree :"), 
            new_var()$degree.name[3], br(),
            strong("Salary :"), 
            new_var()$X17p[3], br(),
            strong("Tuition :"), 
            new_var()$InStOff[3])
      })
      dc <- new_var()$degree.code[3]
      years <- as.numeric (filter(num.years, deg.code %in% dc) %>% select(years))
      y <- 0
      a <- 0
      occf <- as.numeric(new_var()[3,] %>% select(MedOccF))
      age <- 0
      for(i in (1:years)) {
        tf <- (1 + ((0.00002 * ((a) ^ 2)) - 0.0023 * (a) + 0.0603 ))
        x <- as.double(new_var()[3,] %>% select(InStOff))
        y <- y - x
        age <- age + 1
        s3 <- list(age1 = age, ten_factor1 = tf, occ_factor = new_var()$MedOccF[3], xsalary1 = x, run_total1 = y)
        salary3 <- rbind(salary3, s3)
      }
      x <- as.double(new_var()[3,] %>% select(X17p))
      a <- 0
      for(i in (0:input$tenure.length)) {
        tf <- (1 + ((0.00002 * ((i + a)^ 2)) - 0.0023 * (i + a) + 0.0603 ))
        x <- x * tf * occf
        x <- round(x, 0)
        y <- x + y
        age <- age + 1
        s3 <- list(age1 = age, ten_factor1 = tf, occ_factor = new_var()$MedOccF[3], xsalary1 = x, run_total1 = y)
        salary3 <- rbind(salary3, s3)
      }
      totalcost1 <- new_var()$InStOff[3] * years
      runtot1 <- (salary3$run_total[nrow(salary3)])
      roi3 <- (runtot1 + totalcost1) / totalcost1 * 100
      r3 <- list(school.n = paste("3", new_var()$school.name[3], "\n", new_var()$occ.name[3]), roi.n = roi3)
      roi.data <- rbind(roi.data, data.frame(as.list(r3)))
      
      output$row.choice.wage3 <- renderDataTable({
        DT::datatable(data = salary3, options = list(pageLength = 10, searching = FALSE, ordering = FALSE),
                      selection = "none")
      })
      output$cummulative.plot <- renderPlot({
        ggplot() + geom_line(data = salary1, aes(x = age1 ,y = run_total1/1000, colour = "First"),
                             show.legend = TRUE) +
          geom_line(data = salary2, aes(x = age1, y = run_total1/1000, colour = "Second"),
                    show.legend = TRUE) +
          geom_line(data = salary3, aes(x = age1, y = run_total1/1000, colour = "Third"),
                    show.legend = TRUE) +
          scale_colour_manual(name="Occupation", values = c("First" = "blue", "Second" = "green", "Third" = "red"),
                              labels = new_var()$school.name)  +
          xlab('Tenure') +
          ylab('Total Earnings') +
          labs(title = "Cummulative Cash Flow") +
          theme(plot.title = element_text(hjust = 0.5))
      })
      output$roi.plot <- renderPlot({
        ggplot(roi.data, aes(x = school.n, y = roi.n)) + geom_bar(stat = "identity", width = 0.3) +
          xlab('School') +
          ylab('Percent') +
          labs(title = "ROI") +
          theme(plot.title = element_text(hjust = 0.5))
      })
    }
  })
  observe({
    if(nrow(new_var()) < 3) {
      output$row.choice.wage3 <- renderDataTable({ NULL })
      output$row.choice.table3 <- renderUI( NULL )
    }
    if(nrow(new_var()) < 2) {
      output$row.choice.wage2 <- renderDataTable({ NULL })
      output$row.choice.table2 <- renderUI( NULL )
    }
    if(nrow(new_var()) < 1) {
      output$row.choice.wage1 <- renderDataTable({ NULL })
      output$row.choice.table1 <- renderUI( NULL )
      output$cummulative.plot <- renderPlot( {NULL})
      output$roi.plot <- renderPlot ({NULL})
    }
  })
}
shinyApp(ui = ui, server = server)