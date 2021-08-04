packages <- c("shinydashboard", "shiny", "ggplot2", "plotly", "tidyverse", "lubridate", "dplyr", "readxl", "data.table")
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
invisible(lapply(packages, library, character.only = TRUE))

playerrank <- read_excel("PlayerRank.xlsx")
MLAXdata <- read.csv("MLAX21.csv")
newMLAXdata <- merge(MLAXdata, playerrank, by = "Name")
colnames(newMLAXdata)[16] <- "Player.Status"
newMLAXdata <- newMLAXdata %>% relocate(Player.Status, .after = Position.Name)
write.csv(newMLAXdata, "MLAXappdata.csv", row.names = FALSE)
FDdata <- read_excel("MLAXFPCMJAllVariables.xlsx", skip = 7)
FDdatafilter <- FDdata[, c(1, 78, 79, 20, 16, 56, 52, 81, 45, 14, 60, 48, 119, 116, 109, 133, 49, 53, 54, 17, 18)]
FDPlayerRank <- read_excel("FDPlayerRank.xlsx")
colnames(FDPlayerRank)[1] <- "Athlete"
newFDdatafilter <- merge(FDdatafilter, FDPlayerRank, by = "Athlete")
newFDdatafilter <- newFDdatafilter %>% relocate(Position.Name, .after = Athlete) %>% relocate(Player.Status, .after = Position.Name)
newFDdatafilter <- newFDdatafilter[, -1]

ui <- dashboardPage(
  header <- dashboardHeader(title = "Linear Regresion Tools"),
  sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem("GPS - Batch Simple", tabName = "gps", icon = icon("globe")),
      menuItem("GPS - Multiple", tabName = "multiple", icon = icon("tasks")),
      menuItem("GPS - One Simple", tabName = "simple", icon = icon("check")),
      menuItem("Force Plate - Batch Simple", tabName = "forceplate", icon = icon("weight"))
    )
  ),
  body <- dashboardBody(
    tabItems(
      # INPUT FOR BATCH LINEAR REGRESSION
      tabItem(tabName = "gps",
              h2("GPS Data - Batch of Simple Linear Regression"),
              fluidRow(
                box(
                  selectInput('sportgps', 'Select Sport', c('Womens Lacrosse' = 'WLAX1819.csv',
                                                            'Mens Lacrosse' = 'MLAXappdata.csv')),
                  selectInput('xvargps', 'Select Predictor (X) Variable', choices = 'No Choices Yet: Select Sport'),
                  checkboxGroupInput('yvarsgps', 'Select Response (Y) Variables', choices = 'No Choices Yet: Select Sport'),
                  checkboxGroupInput('positiongps', 'Select Position', choices = 'No Choices Yet: Select Sport'),
                  checkboxGroupInput('rolegps', 'Select Player Role', choices = 'No Choices Yet: Select Sport'),
                  submitButton('Update/Run Batch Regression')
                ),
                box(
                  downloadButton('downloadgps', 'Download Table to CSV'),
                  tableOutput('datatablegps')
                ),
                box(
                  uiOutput('note1')
                )
              )
      ),
      
      # INPUT FOR MULTIPLE REGRESSION
      tabItem(tabName = "multiple",
              h2("GPS Data - Multiple Linear Regression"),
              fluidPage(
                sidebarLayout(
                  sidebarPanel(
                    selectInput('sportmultiple', 'Select Sport', c('Womens Lacrosse' = 'WLAX1819.csv',
                                                                   'Mens Lacrosse' = 'MLAXappdata.csv')),
                    checkboxGroupInput('xvarsmultiple', 'Select Predictor (X) Variables', choices = 'No Choices Yet: Select Sport'),
                    selectInput('yvarmultiple', 'Select Response (Y) Variable', choices = 'No Choices Yet: Select Sport'),
                    checkboxGroupInput('positionmultiple', 'Select Position', choices = 'No Choices Yet: Select Sport'),
                    checkboxGroupInput('rolemultiple', 'Select Player Role', choices = 'No Choices Yet: Select Sport'),
                    submitButton('Update/Run Multiple Regression')
                  ),
                  mainPanel(
                    tabsetPanel(
                      tabPanel("Equation",
                               fluidRow(
                                 box(uiOutput('equationm'), uiOutput('numm')),
                                 box(uiOutput('notemultiple'))
                               ),
                               fluidRow(
                                 box(tableOutput('resultsm'))
                               )
                      ),
                      tabPanel("Summary", verbatimTextOutput('summarym')),
                      tabPanel("Residual Plot", plotOutput('residualsm')),
                      tabPanel("Normal Q-Q Plot", plotOutput('qqplotm'))
                    )
                  )
                )
              )
      ),
      
      # INPUT FOR SIMPLE LINEAR REGRESSION
      tabItem(tabName = "simple",
              h2("Simple Linear Regression of One X Variable"),
              fluidPage(
                sidebarLayout(
                  sidebarPanel(
                    selectInput('sportsimple', 'Select Sport', c('Womens Lacrosse' = 'WLAX1819.csv',
                                                                 'Mens Lacrosse' = 'MLAXappdata.csv')),
                    selectInput('xcolsimple', 'Select Predictor (X) Variable', choices = 'No Choices Yet: Select Sport'),
                    selectInput('ycolsimple', 'Select Response (Y) Variable', choices = 'No Choices Yet: Select Sport'),
                    checkboxGroupInput('positionsimple', 'Select Position', choices = 'No Choices Yet: Select Sport'),
                    checkboxGroupInput('rolesimple', 'Select Player Role', choices = 'No Choices Yet: Select Sport'),
                    submitButton('Update/Run Regression'),
                    uiOutput('notesimple')
                  ),
                  mainPanel(
                    tabsetPanel(
                      tabPanel("Scatter Plot", plotOutput('scattersimple'), uiOutput('equationsimple'), uiOutput('resultsimple'), uiOutput('numsimple')),
                      tabPanel("Summary", verbatimTextOutput('summarysimple')),
                      tabPanel("Residual Plot", plotOutput('residualsimple')),
                      tabPanel("Normal Q-Q Plot", plotOutput('qqplotsimple'))
                    )
                  )
                )
              )
      ),
      
      # INPUT FOR FORCE PLATE REGRESSION
      tabItem(tabName = "forceplate",
              h2("Force Plate Regression"),
              fluidRow(
                box(selectInput('xvarplate', 'Select Predictor (X) Variable', names(newFDdatafilter)),
                    checkboxGroupInput('yvarsplate', 'Select Response (Y) Variables', names(newFDdatafilter)),
                    checkboxGroupInput('positionplate', 'Select Position', choiceNames = unique(newFDdatafilter$Position.Name), choiceValues = unique(newFDdatafilter$Position.Name), selected = unique(newFDdatafilter$Position.Name)),
                    checkboxGroupInput('roleplate', 'Select Player Role', choiceNames = unique(newFDdatafilter$Player.Status), choiceValues = unique(newFDdatafilter$Player.Status), selected = unique(newFDdatafilter$Player.Status)),
                    submitButton('Run Batch Regression')),
                box(
                  downloadButton('downloadplate', 'Download Table to CSV'),
                  tableOutput('tableplate')
                ),
                box(
                  uiOutput('note2')
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  # OUTPUT FOR BATCH LINEAR REGRESSION
  observeEvent(input$sportgps, {
    datatablegps <- read.csv(input$sportgps)
    updateSelectInput(session, "xvargps", label = "Select Predictor (X) Variables", choices = colnames(datatablegps))
    updateCheckboxGroupInput(session, "yvarsgps", label = "Select Response (Y) Variable", choices = colnames(datatablegps))
    updateCheckboxGroupInput(session, "positiongps", label = "Select Position", choiceNames = unique(datatablegps$Position.Name), choiceValues = unique(datatablegps$Position.Name), selected = unique(datatablegps$Position.Name))
    updateCheckboxGroupInput(session, "rolegps", label = "Select Player Role", choiceNames = unique(datatablegps$Player.Status), choiceValues = unique(datatablegps$Player.Status), selected = unique(datatablegps$Player.Status))
    datatablegpsfilter <- reactive({
      resgps <- datatablegps %>% filter(Position.Name %in% input$positiongps) %>% filter(Player.Status %in% input$rolegps)
      resgps$Player.Status <- as.factor(resgps$Player.Status)
      resgps$Player.Status <- relevel(resgps$Player.Status, ref = "Starter")
      res1 <- resgps
    })
    dtagps <- shiny::reactive({
      datatablegps <- data.table(YVariable = paste(), RSquared = numeric(), AdjRSquared = numeric(), PValue = numeric(), Equation = paste())
      selectionsgps <- input$yvarsgps
      for (elemgps in selectionsgps) {
        regressgps <- lm(as.formula(paste(paste(elemgps), " ~ ", input$xvargps)), data = datatablegpsfilter())
        coeffs1 <- regressgps$coefficients
        infogps <- data.frame(paste(elemgps), round(summary(regressgps)$r.squared, 3), round(summary(regressgps)$adj.r.squared, 3), signif(summary(regressgps)$coef[2,4], 3), paste(paste(elemgps), "=", paste(round(coeffs1[1], 2), paste(round(coeffs1[-1], 2), names(coeffs1[-1]), sep = "*", collapse = " + "), sep = " + ")))
        names(infogps) <- c("YVariable", "RSquared", "AdjRSquared", "PValue", "Equation")
        datatablegps <- rbind(datatablegps, infogps)
      }
      dtagps <- datatablegps
    })
    output$datatablegps <- renderTable(dtagps())
    output$downloadgps <- downloadHandler(
      filename = function() {
        paste("batchregression.csv")
      },
      content = function(file) {
        write.csv(dtagps(), file, row.names = FALSE)
      }
    )
  })
  output$note1 <- renderUI({
    withMathJax(
      paste("Note: After changing the `Select Sport` input, push the `Update/Run Batch Regression` button to update checkbox options."),
      br(),
      br(),
      paste("Note: If using Player Status or Position Name as an X variable, ensure all of that variable's checkbox options are selected in its filter.")
    )
  })
  
  # _____________________________________________
  # OUTPUT FOR MULTIPLE REGRESSION
  observeEvent(input$sportmultiple, {
    datatablemultiple <- read.csv(input$sportmultiple)
    updateCheckboxGroupInput(session, "xvarsmultiple", label = "Select Predictor (X) Variables", choices = colnames(datatablemultiple))
    updateSelectInput(session, "yvarmultiple", label = "Select Response (Y) Variable", choices = colnames(datatablemultiple))
    updateCheckboxGroupInput(session, "positionmultiple", label = "Select Position", choiceNames = unique(datatablemultiple$Position.Name), choiceValues = unique(datatablemultiple$Position.Name), selected = unique(datatablemultiple$Position.Name))
    updateCheckboxGroupInput(session, "rolemultiple", label = "Select Player Role", choiceNames = unique(datatablemultiple$Player.Status), choiceValues = unique(datatablemultiple$Player.Status), selected = unique(datatablemultiple$Player.Status))
    datatablemultiplefilter <- reactive({
      resmultiple <- datatablemultiple %>% filter(Position.Name %in% input$positionmultiple) %>% filter(Player.Status %in% input$rolemultiple)
      resmultiple$Player.Status <- as.factor(resmultiple$Player.Status)
      resmultiple$Player.Status <- relevel(resmultiple$Player.Status, ref = "Starter")
      res2 <- resmultiple
    })
    prediction2 <- shiny::reactive({
      regress <- lm(as.formula(paste(input$yvarmultiple, " ~ ", paste(input$xvarsmultiple, collapse = "+"))), data = datatablemultiplefilter())
    })
    coeffsm <- shiny::reactive({
      regress2 <- lm(as.formula(paste(input$yvarmultiple, " ~ ", paste(input$xvarsmultiple, collapse = "+"))), data = datatablemultiplefilter())
      coeffsm <- regress2$coefficients
    })
    output$equationm <- renderUI({
      withMathJax(
        br(),
        paste(input$yvarmultiple, "=", paste(round(coeffsm()[1], 2), paste(round(coeffsm()[-1], 2), names(coeffsm()[-1]), sep = "*", collapse = " + "), sep = " + ")),
        br(),
        br()
      )
    })
    output$numm <- renderUI({
      paste0("The number of data points used for linear regression is: ", nrow(datatablemultiple))
    })
    output$notemultiple <- renderUI({
      withMathJax(
        paste("Note: After changing the `Select Sport` input, push the `Update/Run Batch Regression` button to update checkbox options."),
        br(),
        br(),
        paste("Note: If using Player Status or Position Name as an X variable, ensure all of that variable's checkbox options are selected in its filter.")
      )
    })
    
    dtamultiple <- shiny::reactive({
      datatablemultiple <- data.table(PredictorName = paste(), Slope = numeric(), PValue = numeric())
      selectionsmultiple <- input$xvarsmultiple
      for (count in 1:length(coeffsm())) {
        infomultiple <- data.frame(paste(names(coeffsm()[count])), round(coeffsm()[count], 3), signif(summary(prediction2())$coef[count,4], 3))
        names(infomultiple) <- c("PredictorName", "Slope", "PValue")
        datatablemultiple <- rbind(datatablemultiple, infomultiple)
      }
      dtamultiple <- datatablemultiple
    })
    
    output$resultsm <- renderTable(dtamultiple())
    output$summarym <- renderPrint({summary(prediction2())})
    output$residualsm <- renderPlot({
      ggplot(data = datatablemultiple, aes(x = prediction2()$fitted, y = prediction2()$residuals)) +
        geom_point() +
        labs(y = "Residuals", x = "Fitted Values") +
        ggtitle("Fitted vs Residual Values") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    })
    output$qqplotm <- renderPlot({
      qqnorm(prediction2()$residuals)
      qqline(prediction2()$residuals)
    })
  })
  
  # _____________________________________________
  # OUTPUT FOR SIMPLE LINEAR REGRESSION
  observeEvent(input$sportsimple, {
    datatablesimple <- read.csv(input$sportsimple)
    updateSelectInput(session, "xcolsimple", label = "Select Predictor (X) Variable", choices = colnames(datatablesimple))
    updateSelectInput(session, "ycolsimple", label = "Select Response (Y) Variable", choices = colnames(datatablesimple))
    updateCheckboxGroupInput(session, "positionsimple", label = "Select Position", choiceNames = unique(datatablesimple$Position.Name), choiceValues = unique(datatablesimple$Position.Name), selected = unique(datatablesimple$Position.Name))
    updateCheckboxGroupInput(session, "rolesimple", label = "Select Player Role", choiceNames = unique(datatablesimple$Player.Status), choiceValues = unique(datatablesimple$Player.Status), selected = unique(datatablesimple$Player.Status))
    datatablesimplefilter <- reactive({
      res <- datatablesimple %>% filter(Position.Name %in% input$positionsimple) %>% filter(Player.Status %in% input$rolesimple)
      res$Player.Status <- as.factor(res$Player.Status)
      res$Player.Status <- relevel(res$Player.Status, ref = "Starter")
      res1 <- res
    })
    xdatasimple <- reactive({
      datatablesimplefilter()[, input$xcolsimple]
    })
    ydatasimple <- reactive({
      datatablesimplefilter()[, input$ycolsimple]
    })
    predictionsimple <- shiny::reactive({
      regresssimple <- lm(as.formula(paste(input$ycolsimple, " ~ ", paste(input$xcolsimple))), data = datatablesimplefilter())
    })
    coeffs3 <- shiny::reactive({
      reg <- lm(as.formula(paste(input$ycolsimple, " ~ ", paste(input$xcolsimple))), data = datatablesimplefilter())
      cos1 <- reg$coefficients
    })
    output$notesimple <- renderUI({
      withMathJax(
        br(),
        br(),
        paste("Note: After changing the `Select Sport` input, push the `Update/Run Batch Regression` button to update checkbox options."),
        br(),
        br(),
        paste("Note: If using Player Status or Position Name as an X variable, ensure all of that variable's checkbox options are selected in its filter.")
      )
    })
    output$equationsimple <- renderUI({
      withMathJax(
        br(),
        paste(input$ycolsimple, "=", paste(round(coeffs3()[1], 2), paste(round(coeffs3()[-1], 2), names(coeffs3()[-1]), sep = "*", collapse = " + "), sep = " + "))
      )
    })
    output$resultsimple <- renderUI({
      withMathJax(
        br(),
        paste0(
          "R-squared = ", round(summary(predictionsimple())$r.squared, 3),
          ", Adj. R-squared = ", round(summary(predictionsimple())$adj.r.squared, 3),
          ", P-value ", " = ", signif(summary(predictionsimple())$coef[2, 4], 3)
        )
      )
    })
    output$numsimple <- renderUI({
      withMathJax(
        br(),
        paste0("The number of data points used for linear regression is: ", nrow(datatablesimplefilter()))
      )
    })
    output$summarysimple <- renderPrint({
      summary(predictionsimple())
    })
    output$scattersimple <- renderPlot({
      ggplot(data = datatablesimplefilter(), aes(x = xdatasimple(), y = ydatasimple())) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE) +
        labs(y = as.name(input$ycolsimple), x = as.name(input$xcolsimple)) +
        ggtitle("Linear Model") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    })
    output$residualsimple <- renderPlot({
      ggplot(data = datatablesimplefilter(), aes(x = predictionsimple()$fitted, y = predictionsimple()$residuals)) +
        geom_point() +
        labs(y = "Residuals", x = "Fitted Values") +
        ggtitle("Fitted vs Residual Values") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    })
    output$qqplotsimple <- renderPlot({
      qqnorm(predictionsimple()$residuals)
      qqline(predictionsimple()$residuals)
    })
  })
  
  # _____________________________________________
  # OUTPUT FOR FORCE PLATE BATCH REGRESSION
  output$note2 <- renderUI({
    withMathJax(
      paste("Note: If using Player Status or Position Name as an X variable, ensure all of that variable's checkbox options are selected in its filter.")
    )
  })
  newFDdatafilter1 <- reactive({
    resplate <- newFDdatafilter %>% filter(Position.Name %in% input$positionplate) %>% filter(Player.Status %in% input$roleplate)
    resplate$Player.Status <- as.factor(resplate$Player.Status)
    resplate$Player.Status <- relevel(resplate$Player.Status, ref = "Starter")
    res3 <- resplate
  })
  dtaplate <- shiny::reactive({
    datatableplate <- data.table(PredictorName = paste(), RSquared = numeric(), AdjRSquared = numeric(), PValue = numeric(), Equation = paste())
    selectionsplate <- input$yvarsplate
    for (elemplate in selectionsplate) {
      regressplate <- lm(as.formula(paste0("`", paste(elemplate), "`", " ~ ", "`", input$xvarplate, "`")), data = newFDdatafilter1())
      coeffs2 <- regressplate$coefficients
      infoplate <- data.frame(paste(elemplate), round(summary(regressplate)$r.squared, 3), round(summary(regressplate)$adj.r.squared, 3), signif(summary(regressplate)$coef[2,4], 3), paste(paste(elemplate), "=", paste(round(coeffs2[1], 2), paste(round(coeffs2[-1], 2), names(coeffs2[-1]), sep = "*", collapse = " + "), sep = " + ")))
      names(infoplate) <- c("PredictorName", "RSquared", "AdjRSquared", "PValue", "Equation")
      datatableplate <- rbind(datatableplate, infoplate)
    }
    dtaplate <- datatableplate
  })
  output$tableplate <- renderTable(dtaplate())
  output$downloadplate <- downloadHandler(
    filename = function() {
      paste("ForceDeckBatchRegression.csv")
    },
    content = function(file) {
      write.csv(dtaplate(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
