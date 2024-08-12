# Load packages used by the app
library(tidyverse)
library(arules)
library(arulesViz)


library(shiny)
library(bslib)


library(gitlink)





##############
G5SahelruleExplorer = function (x, sidebarWidth = 2, graphHeight = "600px") 
{
  rlang::check_installed(c("shiny", "shinythemes"))
  #shinyTheme <- shinythemes::shinytheme("yeti")
  # Set the default theme for ggplot2 plots
  ggplot2::theme_set(ggplot2::theme_minimal())
  
  # Apply the CSS used by the Shiny app to the ggplot2 plots
  thematic_shiny()
  
  # Set CSS theme
  theme = bs_theme(bootswatch = "darkly",
                   bg = "#222222",
                   fg = "#86C7ED",
                   success ="#86C7ED")
  message("ruleExplorer started.")
  o <- options(warn = 1)
  on.exit(options(o))
  roundUp <- function(x, digits = 3) {
    round(x + 0.5 * 10^-digits, digits)
  }
  roundDown <- function(x, digits = 3) {
    round(x - 0.5 * 10^-digits, digits)
  }
  if (!is(x, "rules") && !is(x, "transactions")) {
    message("Converting dataset into transactions.")
    x <- transactions(x)
  }
  xIndexCached <- "support"
  yIndexCached <- "confidence"
  zIndexCached <- "lift"
  itemLabels <- itemLabels(x)
  if (length(itemLabels) > 10000) {
    itemLabels <- list(`Disabled because of excessive number of items (>10,000)` = c(""))
  }
  if (is(x, "rules")) {
    if (length(x) < 1) {
      stop("Zero rules provided!")
    }
    minSupp <- roundDown(min(quality(x)$support), 5)
    maxSupp <- roundUp(max(quality(x)$support), 5)
    minConf <- roundDown(min(quality(x)$confidence), 3)
    maxConf <- roundUp(max(quality(x)$confidence), 3)
    minLift <- floor(min(quality(x)$lift))
    maxLift <- ceiling(max(quality(x)$lift))
    supp <- minSupp
    conf <- minConf
    lift <- minLift
  }
  else {
    minSupp <- 0
    maxSupp <- 1
    minConf <- 0
    maxConf <- 1
    minLift <- 0
    maxLift <- 25
    lift <- 0
    defaultParam <- new("APparameter")
    supp <- defaultParam@support
    conf <- defaultParam@confidence
  }
  shiny::shinyApp(ui = shiny::shinyUI(shiny::fluidPage(theme = theme, 
                                                       shiny::titlePanel("Food Security and Household Coping Strategies in the G5 Sahel Countries (2018-2023): Association Rule Explorer"), shiny::sidebarLayout(shiny::sidebarPanel(shiny::htmlOutput("numRulesOutput"), 
                                                                                                                                                shiny::br(), shiny::sliderInput("supp", "Minimum Support:", 
                                                                                                                                                                                min = minSupp, max = maxSupp, value = supp, step = (maxSupp - 
                                                                                                                                                                                                                                      minSupp)/10000, sep = ""), shiny::sliderInput("conf", 
                                                                                                                                                                                                                                                                                    "Minimum Confidence:", min = minConf, max = maxConf, 
                                                                                                                                                                                                                                                                                    value = conf, step = (maxConf - minConf)/1000, 
                                                                                                                                                                                                                                                                                    sep = ""), shiny::sliderInput("lift", "Minimum Lift:", 
                                                                                                                                                                                                                                                                                                                  min = minLift, max = maxLift, value = lift, step = (maxLift - 
                                                                                                                                                                                                                                                                                                                                                                        minLift)/1000, sep = ""), shiny::sliderInput("length", 
                                                                                                                                                                                                                                                                                                                                                                                                                     "Rule length (from-to):", min = 2, max = 20, 
                                                                                                                                                                                                                                                                                                                                                                                                                     value = c(2, 10), step = 1, sep = ""), shiny::em(shiny::HTML("Filter rules by items:")), 
                                                                                                                                                shiny::selectInput("colsType", NULL, c(`Exclude items:` = "rem", 
                                                                                                                                                                                       `Require items:` = "req")), shiny::uiOutput("choose_columns"), 
                                                                                                                                                shiny::selectInput("colsLHSType", NULL, c(`Exclude items from LHS:` = "rem", 
                                                                                                                                                                                          `Require items in LHS:` = "req")), shiny::uiOutput("choose_lhs"), 
                                                                                                                                                shiny::selectInput("colsRHSType", NULL, c(`Exclude items from RHS:` = "rem", 
                                                                                                                                                                                          `Require items in RHS:` = "req")), shiny::uiOutput("choose_rhs"), 
                                                                                                                                                width = sidebarWidth), shiny::mainPanel(shiny::tabsetPanel(id = "tabs", 
                                                                                                                                                                                                           shiny::tabPanel("Data Table", value = "datatable", 
                                                                                                                                                                                                                           shiny::br(), DT::dataTableOutput("rulesDataTable")), 
                                                                                                                                                                                                           shiny::tabPanel("Scatter", value = "scatter", shiny::wellPanel(shiny::fluidRow(shiny::column(2, 
                                                                                                                                                                                                                                                                                                        shiny::uiOutput("xAxisSelectInput")), shiny::column(2, 
                                                                                                                                                                                                                                                                                                                                                            shiny::uiOutput("yAxisSelectInput")), shiny::column(2, 
                                                                                                                                                                                                                                                                                                                                                                                                                shiny::uiOutput("cAxisSelectInput")), shiny::column(3, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                    shiny::sliderInput("jitter_scatter", "Jitter", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       min = 0, max = 1, value = 0, step = 1/100, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       sep = "")), shiny::column(3, shiny::uiOutput("topRules_scatter")))), 
                                                                                                                                                                                                                           plotly::plotlyOutput("scatterPlot", width = "100%", 
                                                                                                                                                                                                                                                height = graphHeight)), shiny::tabPanel("Matrix", 
                                                                                                                                                                                                                                                                                        value = "matrix", shiny::wellPanel(shiny::fluidRow(shiny::column(6, 
                                                                                                                                                                                                                                                                                                                                                         shiny::uiOutput("cAxisSelectInput_matrix")), 
                                                                                                                                                                                                                                                                                                                                           shiny::column(6, shiny::uiOutput("topRules_matrix")))), 
                                                                                                                                                                                                                                                                                        plotly::plotlyOutput("matrixPlot", width = "100%", 
                                                                                                                                                                                                                                                                                                             height = graphHeight)), shiny::tabPanel("Grouped Matrix", 
                                                                                                                                                                                                                                                                                                                                                     value = "grouped", shiny::wellPanel(shiny::fluidRow(shiny::column(6, 
                                                                                                                                                                                                                                                                                                                                                                                                                       shiny::uiOutput("cAxisSelectInput_grouped")), 
                                                                                                                                                                                                                                                                                                                                                                                                         shiny::column(6, shiny::uiOutput("kSelectInput")))), 
                                                                                                                                                                                                                                                                                                                                                     shiny::plotOutput("groupedPlot", width = "100%", 
                                                                                                                                                                                                                                                                                                                                                                       height = graphHeight)), shiny::tabPanel("Graph", 
                                                                                                                                                                                                                                                                                                                                                                                                               value = "graph", shiny::wellPanel(shiny::fluidRow(shiny::column(6, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                               shiny::uiOutput("cAxisSelectInput_graph")), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                 shiny::column(6, shiny::uiOutput("topRules_graph")))), 
                                                                                                                                                                                                                                                                                                                                                                                                               visNetwork::visNetworkOutput("graphPlot", width = "100%", 
                                                                                                                                                                                                                                                                                                                                                                                                                                            height = graphHeight)), shiny::tabPanel("Export", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    value = "export", shiny::br(), shiny::downloadButton("rules.csv", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         "Export rules (CSV)"))), width = 12 - sidebarWidth)))), 
                  server = function(input, output, session) {
                    output$numRulesOutput <- shiny::renderUI({
                      if (is(x, "rules")) {
                        shiny::em(shiny::HTML(paste("Selected rules: ", 
                                                    length(rules()), " of ", length(x))))
                      }
                      else {
                        shiny::em(shiny::HTML(paste("Rules: ", length(rules()))))
                      }
                    })
                    output$kSelectInput <- shiny::renderUI({
                      shiny::sliderInput("k", label = "Choose # of rule clusters", 
                                         min = 1, max = 50, step = 1, value = 15)
                    })
                    output$xAxisSelectInput <- shiny::renderUI({
                      shiny::selectInput("xAxis", "X Axis:", colnames(quality(rules())), 
                                         selected = xIndexCached)
                    })
                    output$yAxisSelectInput <- shiny::renderUI({
                      shiny::selectInput("yAxis", "Y Axis:", colnames(quality(rules())), 
                                         selected = yIndexCached)
                    })
                    output$cAxisSelectInput <- shiny::renderUI({
                      shiny::selectInput("cAxis", "Shading:", colnames(quality(rules())), 
                                         selected = zIndexCached)
                    })
                    output$cAxisSelectInput_matrix <- shiny::renderUI({
                      shiny::selectInput("cAxis_matrix", "Shading:", 
                                         colnames(quality(rules())), selected = zIndexCached)
                    })
                    output$cAxisSelectInput_grouped <- shiny::renderUI({
                      shiny::selectInput("cAxis_grouped", "Shading:", 
                                         colnames(quality(rules())), selected = zIndexCached)
                    })
                    output$cAxisSelectInput_graph <- shiny::renderUI({
                      shiny::selectInput("cAxis_graph", "Shading:", 
                                         colnames(quality(rules())), selected = zIndexCached)
                    })
                    output$topRules_matrix <- shiny::renderUI({
                      shiny::sliderInput("topRules_matrix", "Top rules shown (keep below 500):", 
                                         min = 1, max = length(rules()), value = min(100, 
                                                                                     length(rules())), step = 1, sep = "")
                    })
                    output$topRules_scatter <- shiny::renderUI({
                      shiny::sliderInput("topRules_scatter", "Top rules shown (keep below 500):", 
                                         min = 1, max = length(rules()), value = min(100, 
                                                                                     length(rules())), step = 1, sep = "")
                    })
                    output$topRules_graph <- shiny::renderUI({
                      shiny::sliderInput("topRules_graph", "Top rules shown (keep below 500):", 
                                         min = 1, max = length(rules()), value = min(100, 
                                                                                     length(rules())), step = 1, sep = "")
                    })
                    output$choose_columns <- shiny::renderUI({
                      shiny::selectizeInput("cols", NULL, itemLabels, 
                                            multiple = TRUE)
                    })
                    output$choose_lhs <- shiny::renderUI({
                      shiny::selectizeInput("colsLHS", NULL, itemLabels, 
                                            multiple = TRUE)
                    })
                    output$choose_rhs <- shiny::renderUI({
                      shiny::selectizeInput("colsRHS", NULL, itemLabels, 
                                            multiple = TRUE)
                    })
                    cachedRules <- NULL
                    cachedSupp <- supp
                    cachedConf <- conf
                    cachedLift <- lift
                    cachedMinL <- minLift
                    cachedMaxL <- maxLift
                    if (is(x, "rules")) {
                      cachedRules <- x
                      cachedSupp <<- info(x)$support
                      cachedConf <<- info(x)$confidence
                      cachedLift <<- min(quality(x)$lift)
                      cachedMinL <<- min(size(x))
                      cachedMaxL <<- max(size(x))
                    }
                    remineRules <- shiny::reactive({
                      supp <- input$supp
                      if (supp == 0) {
                        supp <- 1/length(x)
                      }
                      message("Remining rules...")
                      rules <- apriori(x, parameter = list(support = as.numeric(supp), 
                                                           confidence = as.numeric(input$conf), minlen = input$length[1], 
                                                           maxlen = input$length[2]), control = list(verbose = FALSE))
                      quality(rules) <- interestMeasure(rules, transactions = x)
                      message("Remined ", length(rules), " rules.")
                      cachedRules <<- rules
                      cachedSupp <<- input$supp
                      cachedConf <<- input$conf
                      cachedLift <<- input$lift
                      cachedMinL <<- input$length[1]
                      cachedMaxL <<- input$length[2]
                    })
                    override <- shiny::reactiveVal(FALSE)
                    shiny::observeEvent(input$cancel, {
                      shiny::removeModal()
                      shiny::updateSliderInput(session, "supp", value = cachedSupp)
                    })
                    shiny::observeEvent(input$continue, {
                      shiny::removeModal()
                      override(TRUE)
                    })
                    rules <- shiny::reactive({
                      if (is(x, "transactions")) {
                        if (input$supp * length(x) > 10 || override()) {
                          if (is.null(cachedRules)) {
                            remineRules()
                          }
                          if ((input$supp < cachedSupp) || input$conf < 
                              cachedConf) {
                            remineRules()
                          }
                          if (input$length[1] < cachedMinL || input$length[1] > 
                              cachedMaxL) {
                            remineRules()
                          }
                        }
                        else {
                          shiny::showModal(shiny::modalDialog(title = "Warning", 
                                                              "Very low minimum support! Too low values can result in long wait times and memory issues.", 
                                                              footer = shiny::tagList(shiny::actionButton("cancel", 
                                                                                                          "cancel"), shiny::actionButton("continue", 
                                                                                                                                         "proceed"))))
                        }
                      }
                      ar <- cachedRules
                      if (input$supp > cachedSupp) {
                        ar <- subset(ar, subset = support > input$supp)
                      }
                      if (input$conf > cachedConf) {
                        ar <- subset(ar, subset = quality(ar)$confidence > 
                                       input$conf)
                      }
                      if (input$lift > cachedLift) {
                        ar <- subset(ar, subset = lift > input$lift)
                      }
                      if (input$length[1] > cachedMinL) {
                        ar <- ar[size(ar) >= input$length[1]]
                      }
                      if (input$length[2] < cachedMaxL) {
                        ar <- ar[size(ar) <= input$length[2]]
                      }
                      if (input$colsType == "rem" && length(input$cols) > 
                          0) {
                        ar <- subset(ar, subset = !(items %in% input$cols))
                      }
                      if (input$colsType == "req" && length(input$cols) > 
                          0) {
                        ar <- subset(ar, subset = items %in% input$cols)
                      }
                      if (input$colsLHSType == "rem" && length(input$colsLHS) > 
                          0) {
                        ar <- subset(ar, subset = !(lhs %in% input$colsLHS))
                      }
                      if (input$colsLHSType == "req" && length(input$colsLHS) > 
                          0) {
                        ar <- subset(ar, subset = lhs %in% input$colsLHS)
                      }
                      if (input$colsRHSType == "rem" && length(input$colsRHS) > 
                          0) {
                        ar <- subset(ar, subset = !(rhs %in% input$colsRHS))
                      }
                      if (input$colsRHSType == "req" && length(input$colsRHS) > 
                          0) {
                        ar <- subset(ar, subset = rhs %in% input$colsRHS)
                      }
                      shiny::validate()
                      ar
                    })
                    shiny::observe({
                      shiny::req(input$xAxis)
                      xIndexCached <<- input$xAxis
                    })
                    shiny::observe({
                      shiny::req(input$yAxis)
                      yIndexCached <<- input$yAxis
                    })
                    shiny::observe({
                      shiny::req(input$cAxis)
                      zIndexCached <<- input$cAxis
                    })
                    shiny::observe({
                      shiny::req(input$cAxis_matrix)
                      zIndexCached <<- input$cAxis_matrix
                    })
                    shiny::observe({
                      shiny::req(input$cAxis_grouped)
                      zIndexCached <<- input$cAxis_grouped
                    })
                    shiny::observe({
                      shiny::req(input$cAxis_graph)
                      zIndexCached <<- input$cAxis_graph
                    })
                    handleErrors <- shiny::reactive({
                      shiny::validate(shiny::need(length(rules()) > 
                                                    0, "No rules to visualize! Decrease support, confidence or lift."))
                    })
                    output$rulesDataTable <- DT::renderDT({
                      handleErrors()
                      inspectDT(rules())
                    })
                    output$scatterPlot <- plotly::renderPlotly({
                      shiny::req(input$xAxis, input$yAxis, input$cAxis, 
                                 input$topRules_scatter, input$jitter_scatter)
                      handleErrors()
                      suppressWarnings(plot(rules(), method = "scatterplot", 
                                            measure = c(input$xAxis, input$yAxis), shading = input$cAxis, 
                                            engine = "htmlwidget", control = list(max = input$topRules_scatter, 
                                                                                  jitter = input$jitter_scatter)))
                    })
                    output$matrixPlot <- plotly::renderPlotly({
                      shiny::req(input$cAxis_matrix, input$topRules_matrix)
                      handleErrors()
                      suppressWarnings(plot(rules(), method = "matrix", 
                                            shading = input$cAxis_matrix, engine = "htmlwidget", 
                                            control = list(max = input$topRules_matrix)))
                    })
                    output$groupedPlot <- shiny::renderPlot({
                      shiny::req(input$cAxis_grouped, input$k)
                      handleErrors()
                      plot(rules(), method = "grouped", shading = input$cAxis_grouped, 
                           engine = "ggplot2", control = list(k = input$k)) + 
                        ggplot2::theme(text = element_text(size = 14))
                    })
                    output$graphPlot <- visNetwork::renderVisNetwork({
                      shiny::req(input$cAxis_graph, input$topRules_graph)
                      handleErrors()
                      suppressWarnings(plt <- plot(rules(), method = "graph", 
                                                   shading = input$cAxis_graph, engine = "htmlwidget", 
                                                   control = list(max = input$topRules_graph)))
                      plt
                    })
                    output$rules.csv <- shiny::downloadHandler(filename = "rules.csv", 
                                                               content = function(file) {
                                                                 utils::write.csv(as(rules(), "data.frame"), 
                                                                                  file)
                                                               })
                  })
}

##########
G5SahelruleExplorer(rules)
