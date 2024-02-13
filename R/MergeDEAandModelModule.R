#' @title Perform DE analysis ui side
#'
#' @description
#'
#' @param id Module's id.
#' @param label Button's label.
#' @param icon Button's icon.
#' @param ... Arguments passed to \code{\link{actionButton}}
#'
#' @return a \code{\link[shiny]{reactiveValues}} containing the data selected under slot \code{data}
#' and the name of the selected \code{data.frame} under slot \code{name}.
#' @export
#'
#'
#' @importFrom htmltools tagList tags singleton
#' @importFrom shinydashboard infoBoxOutput valueBoxOutput renderValueBox valueBox infoBox
#' @importFrom shiny NS actionButton icon uiOutput
#' @importFrom shinyWidgets updatePickerInput pickerInput


DeaPdxUI <- function(id)  {
  ns <- NS(id)
    fluidPage(
      tags$head(
        tags$style(type='text/css', ".span161 { width: 850px; }")#,
      ),
      tags$head(tags$style(type = "#boxPopUp1 .modal-body{ min-height:550px}")),
      br(),
      fluidPage(
      fluidRow(
          tabsetPanel(type = "pills",id = "DEGtabs",
      tabPanel("RUN DEA",
      br(),
      column(width=12,
      box(#title = "Creates DEG model",
          id = ns("CreatesDEGmodel"),
          title = p('Creates DEG model ',actionButton(ns("startCicerone"),label=NULL,icon = icon("info-circle"))),
          collapsible = TRUE, collapsed = FALSE,solidHeader = TRUE,
          status = "primary",width= 12,
          column(width = 12,
          uiOutput(ns("vars_selUI"))))
      ), # end of box
      column(width = 12,
      box(title = "Comparison",id = ns("Comparison"),collapsible = TRUE, collapsed = FALSE,solidHeader = TRUE,
          status = "primary",width= 12,
          #fluidRow(
          column(width=6,
                 uiOutput(ns("Group1")),
                 #textOutput(ns("group1table")),
                 htmlOutput(ns("group1table")),
          pickerInput(
            ns("move1"),
            label = "Move samples to Group 2",
            choices = NULL,
            selected = NULL,
            multiple = TRUE,
            choicesOpt = NULL,
            inline = FALSE,
            options = pickerOptions(
              actionsBox = TRUE,
              title = "Select samples to move",
              liveSearch = TRUE,
              liveSearchStyle = "contains",
            )
          ),
          pickerInput(
            ns("remove1"),
            label = "remove samples from Group1",
            choices = NULL,
            options = pickerOptions(
              actionsBox = TRUE,
              title = "Select samples to remove",
              liveSearch = TRUE,
              liveSearchStyle = "contains",
            ),
            selected = NULL,
            multiple = TRUE,
            choicesOpt = NULL,
            inline = FALSE
          )),
          column(width = 6,uiOutput(ns("Group2")),
                 htmlOutput(ns("group2table")),
                 pickerInput(
                   ns("move2"),
                   label = "Move samples to Group 1",
                   choices = NULL,
                   selected = NULL,
                   multiple = TRUE,
                   choicesOpt = NULL,
                   inline = FALSE,
                   options = pickerOptions(
                     actionsBox = TRUE,
                     title = "Select samples to move",
                     liveSearch = TRUE,
                     liveSearchStyle = "contains"
                   )
                 ),
                 pickerInput(
                   ns("remove2"),
                   label = "remove samples from Group 2",
                   choices = NULL,
                   options = pickerOptions(
                     actionsBox = TRUE,
                     title = "Select samples to remove",
                     liveSearch = TRUE,
                     liveSearchStyle = "contains",
                   ),
                   selected = NULL,
                   multiple = TRUE,
                   choicesOpt = NULL,
                   inline = FALSE
                 ))),
      br(),
      br(),
      actionButton(ns("Build"),"Build Model"))
    ), # end of first tabs

tabPanel("Figures",
    br(),
    tagList(
      box(#title = span(icon("cogs"), "Parameters"),
          title = p('Parameters ',actionButton(ns("startCicerone2"),label=NULL,icon = icon("info-circle"))),
          id = ns("Parameters"),
          collapsible = TRUE, collapsed = FALSE,solidHeader = TRUE,
          status = "success",width= 12,
      fluidRow(
      ),
      fluidRow(
        column(width = 12,numericInput(ns("PvalsT"),"adjusted P values threshold", min = 0, max = 1 , value = 0.05, step = 0.01))),
      fluidRow(
        column(width = 12,numericInput(ns("FCT"),"LogFC threshold", min = 0, max = 10 , value = 1, step = 0.05))#,
        ),
      fluidRow(uiOutput(ns('features_value_box')))
      ), # end of box
          box(title = span(icon("chart-bar"),"DEA figures"),collapsible = TRUE, collapsed = FALSE,solidHeader = TRUE,
          status = "success",width = 12,
          fluidRow(column(width =12,girafeOutput(ns("Pvals_distrib")))),
          br(),
          br(),
          fluidRow(column(width = 12,
                          div(id = ns("GeneVolcanodiv"),pickerInput(ns("GeneVolcano"),"Select Genes to annotate on volcano",
                                      selected = NULL,
                                      multiple = TRUE,
                                      choicesOpt = NULL,
                                      width = '100%',
                                      inline = FALSE,
                                      choices = NULL,
                                      options = pickerOptions(
                                        title = "Select genes to annotate",
                                        liveSearch = TRUE,
                                        liveSearchStyle = "contains",
                                        actionsBox = TRUE
                                      )))
                  )),
          fluidRow(column(width = 12,
                          plotOutput(ns("Volcano"))),
                   column(width = 12,
                          conditionalPanel("input.GeneVolcano != undifined && input.GeneVolcano.length <= 1", ns = ns,
                                           textOutput(ns('boxplots_error'))),
                          conditionalPanel("input.GeneVolcano != undifined && input.GeneVolcano.length >=2", ns = ns,
                          plotOutput(ns("boxplots"))))
                   ),
          fluidRow(column(width = 12,downloadButton(ns("dlfigures"), label = "Download figures")))
      )
    ) # end of Taglist
), # end of second tab
tabPanel("Tables",
         br(),
         box(title = span(icon("arrow-circle-down"),"Tables"),collapsible = TRUE, collapsed = FALSE,solidHeader = TRUE,
             status = "success",width= 12,
             fluidRow(
               tags$head(tags$style(".butt{background-color:#2E8B57;}")),
               column(width = 6,
                      h4("Up regulated genes :",style="padding-left:20px"),
                      br(),
                      DT::dataTableOutput(ns('up_table')),
                      downloadButton(ns("updl"),"Up-regulated",class = "butt")),
               column(width = 6,
                      h4("Down regulated genes :",style="padding-left:20px"),
                      br(),
                      DT::dataTableOutput(ns('down_table')),
                      downloadButton(ns("downdl"),"Down-regulated",class = "butt"))
             )#,
         ) # end of box
) # end of 3 tab
)
#) # end of div
)
) # end of fluidRow
) # end of FLuidPage
}

#' @param input,output,session standards \code{shiny} server arguments.
#' @param header Does the file have a Header
#' @param sep What is the file separator
#'
#' @export
#'
#' @title Perform DE analysis server side
#'
#' @importFrom shiny showModal modalDialog observeEvent reactiveValues callModule observe icon
#' @importFrom htmltools tags HTML
#' @importFrom DESeq2 DESeqDataSetFromMatrix estimateSizeFactors sizeFactors
#' @importFrom edgeR DGEList
#' @import ggiraph
#' @import ggplot2
#' @importFrom shinydashboard renderInfoBox infoBox
#' @importFrom shiny renderUI modalDialog observeEvent reactiveValues callModule observe icon
#' @import limma
#' @importFrom ggrepel geom_text_repel
#' @importFrom tidyr gather
#' @importFrom shinyWidgets updatePickerInput pickerInput pickerOptions
#' @import dplyr
#' @importFrom tictoc tic toc

DeaPdxServer <- function(input, output, session, matrix = NULL,sampleplan = NULL, var = NULL) {

  ### Define reactives #############
req(sampleplan)
req(matrix)
print("ENTERING DEA MODULE")
ns <- session$ns


######### Cicerone #################"""

#Note that a skewing of the null distribution towards 1 indicated the test is too conservative so results in more Type II Errors 
#And skewing towards 0 gives too many false positives (Type I error)


# title ="Here you can select/create groups to compare with the differential expression analysis",
# HTML(
#   "</br></br> Select a variable of interest and covariables (optionnal)</br></br>")
# )

############# Cicerone ###########""
guide <- Cicerone$
  new(id = ns("ciceroneGuide"))$
  step(el = ns("CreatesDEGmodel"),
       title ="Here we start to build the model for our differential expression analysis",
       HTML(
         "</br></br> Select a variable of interest and covariables (optionnal)</br></br> You can also choose to 'Create your own custom groups'")
  )$
   step(
     el = ns("table"),
     title = "This table resume the features keeped after the filtering step"
  )$
  step(
     el = ns("Comparison"),
     title = "Automaticaly you get an overview of the samples selected in each group",
     HTML("There you can add/move/remove samples from defined groups</br></br> <i>Note that at this moment you cannot use covariables with your custom groups'</i>")
  )$
  step(el =ns("Build"),
       title = "Now it is time to build our model !",
       HTML("<i> When you are satisfied with your groups </i>")
  )$
  step(el = "[data-value='Figures']",
       title = "Then you will find different figures and results generated with your data here",
       is_id = FALSE
  )$
  step(el = "[data-value='Tables']",
       title = "Per genes DEA P values are downloadable here",
       is_id = FALSE)
     
guide2 <- Cicerone$
  new(id = ns("ciceroneGuide2"))$
  step(el = ns("Parameters"),
       title ="Set up thresholds",
       HTML(
         "Use the log Fold change and the adjusted P value to select significative hits</br></br>")
  )$
  step(
    el = ns("features_value_box"),
    title = "Number of hits",
    HTML("In these boxes you can see the number of significatively up/down regulated features obtained after the filtering step")
  )$
  step(
    el = ns("Pvals_distrib"),
    title = "P-values distribution",
    HTML("This distribution gives you insights about the quality of your analysis, its shape can be helpuf to diagnose potential problems on data </br></br> Note that a skewing of the null distribution towards 1 indicated the test is too conservative so results in more Type II Errors 
#And skewing towards 0 gives too many false positives (Type I error). </br></br><a href='http://varianceexplained.org/statistics/interpreting-pvalue-histogram/'> How to interpret a p-value histogram </a>")
  )$
  step(el = ns("Volcano"),
       title = "Volcano plot",
       HTML("Here is the volcano plot on your DEA results. Green dots correspond to significant hits regarding your filters' thresholds")
 )$
 step(el = ns("GeneVolcanodiv"),
      title = "Annotate genes on volcano",
      HTML("Select gene names here, they'll be direcltly annotated on the Volcano. If at least two genes are selected, boxplots of expression will be drawn under the volcano plot figure")

  )$
  step(el = ns("dlfigures"),
      title = "All figures are downloadable here")

observeEvent(input$startCicerone, {
  guide$init()$start()
})

observeEvent(input$startCicerone2, {
  guide2$init()$start()
})

############# END of Cicerone ###########""

reactives <- reactiveValues(design = NULL, formula = NULL, contrast = NULL)
groups <- reactiveValues(Group1 = NULL, Group2 = NULL)
sampleplanmodel <- reactiveValues(table = NULL)

results <- reactiveValues(res = NULL, up = NULL, down = NULL,nsignfc = NULL,v = NULL,boxplots = NULL,Pvals_distrib = NULL)

observe({
  sampleplanmodel$table <- sampleplan$table
})

observeEvent(input$remove1,{
      sampleplanmodel$table[input$remove1,input$var] <- "removed"
})
    observeEvent(input$move1,{
      sampleplanmodel$table[input$move1,input$var] <- input$Group2sel
    })
    observeEvent(input$remove2,{
      sampleplanmodel$table[input$remove2,input$var] <- "removed"
    })
    observeEvent(input$move2,{
      sampleplanmodel$table[input$move2,input$var] <- input$Group1sel
    })

    observeEvent(c(input$var,
                   input$covar,
                   input$ok
    ),{
      if(!is.null(sampleplanmodel$table)){
        if(!is.null(input$var)){
          if(!is.null(matrix$table)){
            sampleplan <- sampleplanmodel$table
            print("sampleplanformula")
            print(nrow(sampleplan))
            design.idx <- colnames(sampleplan)
            if(input$covar != ""){
              print("with covar")
              vector <- c(input$var,input$covar)
              if(input$var != "Create your own groups"){
              formula <- as.formula(
                paste0('~0+',input$var,"+",paste0(input$covar,collapse = "+"),"+",
                       paste0(combn(vector,2,FUN = paste,collapse =":"),collapse = "+"))
              )} else if (input$var == "Create your own groups"){
                formula <- as.formula(
                  paste0('~0+',"personalisedGroup","+",paste0(input$covar,collapse = "+"),"+",
                         paste0(combn(vector,2,FUN = paste,collapse =":"),collapse = "+"))
              )} } else {

              if(input$var != "Create your own groups"){
                print("without covar")
              formula <- as.formula(
                paste0('~0+',as.character(input$var))
              )} else if (input$var == "Create your own groups"){
                print("without covar")
                formula <- as.formula(
                  paste0('~0+',"personalisedGroup"))
              }
            }
            reactives$formula <- formula
            print(reactives$formula)
          }}}

    })

    observeEvent(input$Build,{
      withProgress(
        message = "Building model",{
      if(!is.null(sampleplanmodel$table)){
        if(!is.null(input$var)){
          if(!is.null(input$Group2sel)){
            if(!is.null(input$Group1sel)){
              if(!is.null(matrix$table)){
                if(!is.null(reactives$formula)){
                  print("building model from this matrix table")
                  data <- sampleplanmodel$table
                  if (input$var == "Create your own groups"){
                  data[groups$Group2,"personalisedGroup"] <- "Group2"
                  data[groups$Group1,"personalisedGroup"] <- "Group1"
                  completeVec <- complete.cases(data[,"personalisedGroup"])
                  data <- data[completeVec,]
                  }
                  mat <- matrix$table[,rownames(data)]
                  design <- model.matrix(reactives$formula, data=data)
                  design <- design[which(rownames(design) %in% colnames(mat)), ]
                  colnames(design) <- make.names(colnames(design))
                  if((length(input$Group2sel) >=2) | (length(input$Group1sel) >=2)){
                    design2 <- data.frame(a = design[,1])
                    colname1 <- paste0(input$var,paste0(input$Group1sel,collapse = "_"))
                    if (length(input$Group1sel) >= 2){
                      design2[,colname1] <- rowSums(design[,paste0(input$var,input$Group1sel)])
                    } else {
                      design2[,colname1] <- design[,paste0(input$var,input$Group1sel)]
                    }
                    colname2 <- paste0(input$var,paste0(input$Group2sel,collapse = "_"))
                    if (length(input$Group2sel) >= 2){
                      design2[,colname2] <- rowSums(design[,paste0(input$var,input$Group2sel)])
                    } else {
                      design2[,colname2] <- design[,paste0(input$var,input$Group2sel)]
                    }
                    design2$a <- NULL
                    design2 <- design2[which((design2[,colname1] + design2[,colname2]) != 0),]
                    design <- design2
                    colnames(design) <- make.names(colnames(design))
                  }
                  if (input$var == "Create your own groups"){
                  contrast <-makeContrasts(contrasts = paste0(paste0("personalisedGroup","Group1"),"-",(paste0("personalisedGroup","Group2"))) ,
                                           levels=design)
                  } else {
                  #contrast <-makeContrasts(contrasts = paste0(paste0(input$var,input$Group1sel),"-",(paste0(input$var,input$Group2sel))) ,
                  contrast <-makeContrasts(contrasts = paste0(paste0(input$var,paste0(input$Group1sel,collapse = "_")),"-",(paste0(input$var,paste0(input$Group2sel,collapse = "_")))) ,
                                           levels=design)
                  }
                  reactives$contrast <- contrast
                  reactives$design <- design
                }
              }
            }}}
      }
    }) # end of progress
  })

    output$formula <- renderUI({
      if(!is.null(sampleplan$table)){
        print(Reduce(paste,deparse(reactives$formula)))
      } else {
        print("Provides a sampleplan first ")
      }
    })

    observeEvent(input$help1,{
      showModal(modalDialog(HTML(
        "<b>Variable of interest :</b></br>
    The variable that you want to use to create sample groups for the DE analysis </br></br></br>

    <b>Co-variables :</b></br>

    Select co-variables if you want that app to take into account their respective effects on genes' expression.

    "),
        title = "Variables infos",
        footer = tagList(
          modalButton("Got it"),
        )))
    })

    output$vars_selUI <- renderUI({
      tagList(
        fluidRow(
          column(width = 10,
                 pickerInput(ns("var"),"Variable of interest :",choices = c(var,"Create your own groups"),
                             multiple = FALSE, selected = var[1],width = '100%',
                             inline = FALSE,
                             options = pickerOptions(
                               actionsBox = TRUE,
                               title = "Select guides you want to remove",
                               liveSearch = TRUE,
                               liveSearchStyle = "contains"
                             ))),
          column(width = 10,
                 pickerInput(ns("covar"),"Covariables :",choices = c("None" = "",var),
                             multiple = FALSE, selected = "",width ='100%',
                             inline = FALSE,
                             options = pickerOptions(
                               actionsBox = TRUE,
                               title = "Select guides you want to remove",
                               liveSearch = TRUE,
                               liveSearchStyle = "contains"
          ))),
          column(width = 2,
                 actionButton(ns("help1"),"",icon = icon("info")))
        )
      )
    })

    output$Group1 <- renderUI({
      tagList(
        if(!is.null(input$var)) {
        if(input$var != "Create your own groups"){
        pickerInput(ns("Group1sel"),"Group 1", choices = na.omit(levels(sampleplan$table[,input$var])),
                    selected= na.omit(levels(sampleplanmodel$table[,input$var])[1]),
                    multiple = TRUE,
                    options = pickerOptions(
                      actionsBox = TRUE,
                      title = "Group 1",
                      liveSearch = TRUE,
                      liveSearchStyle = "contains"
                    ))

        }
        }
        )
    })

    observeEvent(c(input$Group1sel,input$var),{
      if(!is.null(input$var)) {
      if(input$var != "Create your own groups"){
      if(!is.null(input$Group1sel)){
        print("yo")
        
      groups$Group1 <- rownames(sampleplanmodel$table[which(sampleplanmodel$table[,input$var] %in% input$Group1sel),])
      }
      } else if (input$var == "Create your own groups"){
      showModal(
      fluidPage(
      modalDialog(
      title = "Create two groups for differential analysis",
      fluidRow(
      column(width = 6,
      pickerInput(ns("createGroup1"), "select samples to add in group 1",
                                      choices = rownames(sampleplanmodel$table),
                                      selected = NULL,
                  multiple = TRUE,
                  choicesOpt = NULL,
                  inline = FALSE,
                  options = pickerOptions(
                    actionsBox = TRUE,
                    title = "Select samples to add",
                    liveSearch = TRUE,
                    liveSearchStyle = "contains"
                  ))
      ),
      column(width = 6,
             pickerInput(ns("createGroup2"), "select samples to add in group 2",
                         choices = rownames(sampleplanmodel$table),
                         selected = NULL,
                         multiple = TRUE,
                         choicesOpt = NULL,
                         inline = FALSE,
                         options = pickerOptions(
                           actionsBox = TRUE,
                           title = "Select samples to add",
                           liveSearch = TRUE,
                           liveSearchStyle = "contains"
                         ))
      )),
      easyClose = TRUE,
      footer = tagList(
        modalButton(ns("Cancel")),
        actionButton(ns("ok"),"OK")
      )
      ) # end of fluidRow
      )
      )
      #) # end of div
      }
      }
    })

    observeEvent(input$ok,{
      groups$Group1 <- rownames(sampleplanmodel$table)[which(rownames(sampleplanmodel$table) %in% input$createGroup1)]
      groups$Group2 <- rownames(sampleplanmodel$table)[which(rownames(sampleplanmodel$table) %in% input$createGroup2)]
      removeModal()
    })

    observe({
      if(!is.null(groups$Group1)){
        updatePickerInput(session = session,
                          "remove1","remove samples from Group 1",selected = NULL,choices = groups$Group1,
                          pickerOptions(
                            actionsBox = TRUE,
                            title = "Select samples to remove",
                            header = "This is a title"
                          ))
        updatePickerInput(session = session,
                          "move1","Move samples to Group 2",selected = NULL,choices = groups$Group1)

      }
    })

    observe({
      if(!is.null(groups$Group2)){
        updatePickerInput(session = session,
                          "remove2","remove samples from Group 2",selected = NULL,choices = groups$Group2,
                          pickerOptions(
                            actionsBox = TRUE,
                            title = "Select samples to remove"
                          ))
        updatePickerInput(session = session,
                          "move2","Move samples to Group 1",selected = NULL,choices = groups$Group2)

      }
    })

    output$group1table <- renderText({
      if(length(groups$Group1) == 0){
        print("<font color=\"#FF0000\">There is 0 samples in group 1</br>If you want to use this group in DEA, go to the 'Samples selection' tab and look for missing sample</font>")
      } else {
      groups$Group1
      }
    })

    output$Group2 <- renderUI({
      tagList(
        if(input$var != "Create your own groups"){
        pickerInput(ns("Group2sel"),"Group 2", choices  = na.omit(levels(sampleplan$table[,input$var])),
                    selected = na.omit(levels(sampleplanmodel$table[,input$var])[2]),
                    inline = FALSE,
                    multiple = TRUE,
                    options = pickerOptions(
                      actionsBox = TRUE,
                      title = "Select guides you want to remove",
                      liveSearch = TRUE,
                      liveSearchStyle = "contains"
                    ))
        }
      )
    })

    observeEvent(input$Group2sel,{
      if(!is.null(input$var)) {
        #if(length(input$var) != 0) {
      if(input$var != "Create your own groups"){
      if(!is.null(input$Group2sel)){
      groups$Group2 <- rownames(sampleplanmodel$table[which(sampleplanmodel$table[,input$var] %in% input$Group2sel),])
      }}}
    })

    output$group2table <- renderText({
      print("length(groups$Group2)")
      print(length(groups$Group2))
      if(length(groups$Group2) ==0){
        print("<font color=\"#FF0000\">There is 0 samples in group 2</br>If you want to use this group in DEA, go to the 'Samples selection' tab and look for missing sample</font>")
      } else {
        groups$Group2
      }
    })

    observeEvent(c(input$Group1sel,input$Group2sel),ignoreInit = TRUE,{
      req(input$Group1sel)
      req(input$Group2sel)
      if(input$Group1sel != "" & input$Group2sel != ""){
        if(TRUE %in% (input$Group1sel %in% input$Group2sel)){
          showModal(modalDialog(p("Some groups were selected in both comparison sides"),
                                title = "Identical groups",
                                footer = tagList(
                                  modalButton("Got it"),
                                )))
        }
      }
    })

    observeEvent({input$var
      input$covar},{
        if(input$var %in% input$covar){
          validationModalModel(
            msg = "You can not use the same sample Plan column for variable and covariable ",
          )
          return(-1)
        }
      })

    ## Function def
    validationModalModel <- function(msg = "", title = "Model Error") {
      showModal(modalDialog(p(msg),
                            title = title,
                            footer = tagList(
                              modalButton("Dismiss"),
                            )))
    }

  observeEvent(c(reactives$design,
                 reactives$contrast),priority = 10,{
                   if (!is.null(matrix$table) && !is.null(reactives$design)){
                   tictoc::tic("Voom transormation and fitting linear model..")
                     counts <- matrix$table[,colnames(matrix$table) %in% rownames(reactives$design)]
                       for (col in 1:ncol(counts)){
                         counts[,col] <- as.numeric(counts[,col])
                       }
                       y <- DGEList(counts=counts, genes=rownames(counts))
                       
                       dgelist <- y
                       #Voom transforms count data to log2-counts per million (logCPM), estimate the mean-variance relationship and use this to compute appropriate observation-level weights
                       results$v <- voom(y, reactives$design, plot=FALSE, save.plot = FALSE)
                       fit <- lmFit(results$v, reactives$design)
                       fit2 <- contrasts.fit(fit,reactives$contrast)
                       fit2 <- eBayes(fit2)
                       res <- topTable(fit2, number=nrow(counts), adjust.method="BH")
                       res <- res[order(res$adj.P.Val),]
                       res$genes <- rownames(res)
                       results$res <- res
                   } # end of if NULL
                   toc(log = TRUE)
                 }) # end of observer

  createLink <- function(val) {
    sprintf('<a href="https://www.ensembl.org/Homo_sapiens/Gene/Summary?g=%s" target="_blank" class="btn btn-primary">Info</a>',val)
  }

  observeEvent(c(results$res,
                 input$FCT,
                 input$PvalsT),ignoreInit = TRUE,{

                   res <- results$res
                   nsign <- length(which(res$adj.P.Val < input$PvalsT))
                   results$nsignfc <- length(which(res$adj.P.Val < input$PvalsT & abs(res$logFC) > input$FCT))
                   up <- which(res$adj.P.Val < input$PvalsT & res$logFC > input$FCT)
                   down <- which(res$adj.P.Val < input$PvalsT & res$logFC < -input$FCT)
                   res$t <- NULL
                   #res$P.Value <- NULL
                   res$B <- NULL
                   res$label <- NULL
                   #res$featureID <- rownames(res)
                   res$ENSEMBL <- createLink(rownames(res))
                   print('end of DEG')
                   results$up <- res[up,]
                   results$down <- res[down,]
                   results$restable <- res
                 }) # end of observer

  output$Pvals_distrib <- renderGirafe({
    req(results$res)
    plot <- ggplot(data = results$res) + aes(x = `P.Value`) +
      geom_histogram_interactive(fill = "steelblue",breaks = seq(0, 1, length.out = 20))
    build  <- ggplot_build(plot)
    plot <- plot +  labs(title = "P values distribution", x = "P values", y = "Occurences")# +
      #geom_vline_interactive(xintercept=input$PvalsT, linetype="dashed", color = "red") # +
      # annotate("text",x=input$PvalsT + 0.02, label="\nP vals Threshold", y = max(build[["data"]][[1]][["count"]])/2,
      #          colour="red", angle=90, text=element_text(size=15), vjust = 0.5)
    # geom_text(aes(x=input$PvalsT + 0.02, label="\nP vals Threshold", y = max(build[["data"]][[1]][["count"]])/2),
    #           colour="red", angle=90, text=element_text(size=15), vjust = 0.5)
    # geom text is time consuming
    results$Pvals_distrib <- plot
    ggiraph::girafe(code = {print(plot)})
  })

#output$Pvals_distrib <- renderGirafe({

observe({
updatePickerInput("GeneVolcano", session = session, choices = rownames(matrix$table))
})

Volcano <- reactiveValues(plot = NULL,ggplot = NULL)
observe({
  req(results$res)
  tic("Ploting Volcano")
  #ggplot <- ggplot(results$res, aes(x = logFC, y = -log10(adj.P.Val))) +
  ggplot <- ggplot(results$res, aes(x = logFC, y = -log10(P.Value))) +
    ggtitle(colnames(reactives$contrast)) +
    scale_fill_gradient(low = "lightgray", high = "navy") +
    scale_color_gradient(low = "lightgray", high = "navy") +
    expand_limits(y = c(min(-log10(results$res$P.Value)), 1)) +
#### stat_density rend maintenant l'erreur In max(x, na.rm = na.rm) : aucun argument pour max ; -Inf est renvoyé WHY ????
    # stat_density_2d(aes(fill = ..level..), geom = "polygon",
    #                 show.legend = FALSE) +
    geom_point(data = results$res,
               color = "grey", alpha = 0.5) +
    geom_point(data = subset(results$res, logFC > input$FCT),
               color = "red", alpha = 0.5) +
    geom_point(data = subset(results$res, logFC < -input$FCT),
               color = "blue", alpha = 0.5) +
    geom_point(data = subset(results$res, adj.P.Val < input$PvalsT),
               color = "green", alpha = 0.5) +
    #geom_vline(xintercept = min(-log10(results$res$P.Value))) +
    #geom_hline(yintercept = min(-log10(results$res$P.Value))) +
    geom_hline(yintercept = -log10(max(subset(results$res, adj.P.Val < input$PvalsT)$P.Value)), linetype = "dashed") +
    geom_vline(xintercept = c(-input$FCT, input$FCT), linetype = "dashed") +
    theme_linedraw() +
    theme(panel.grid = element_blank()) +
    xlab("Fold change (log2)") +
    ylab("-log10(P-Value)")

  Volcano$plot <- ggplot
  toc(log = TRUE)
})

output$Volcano <- renderPlot({

  req(Volcano$plot)
  tic("Rendering Volcano...")
    ggplot <- Volcano$plot +
      geom_point(data = subset(results$res,genes %in% input$GeneVolcano),
                 color = "purple", alpha = 0.6) +
      ggrepel::geom_text_repel(
      #data = subset(results$res, adj.P.Val < input$PvalsT),
      #data = results$res[which(rownames(results$res) %in% input$GeneVolcano),],
      data = subset(results$res,genes %in% input$GeneVolcano),
      #aes(label = results$res$label),
      aes(label = genes),
      size = 5,
      force = 2,
      box.padding = unit(0.35, "lines"),
      point.padding = unit(0.3, "lines")
    )
    Volcano$ggplot <- ggplot
  return(ggplot)
    # ggplot_labeled <- gglabeller(ggplot, aes(label = rownames(results$res)))
    # return(ggplot_labeled)
    toc(log = TRUE)

  #girafe(code = {print(ggplot)})
})

observeEvent(input$GeneVolcano,{
  if(length(input$GeneVolcano) >1){
  req(matrix$table)
  req(sampleplanmodel$table)
  groups_table <- sampleplanmodel$table
  groups_table$Samples <- rownames(groups_table)
  if(input$var != "Create your own groups"){
  groups_table <- groups_table[,c(input$var,"Samples")]
  } else {
    groups_table[groups$Group2,"personalisedGroup"] <- "Group2"
    groups_table[groups$Group1,"personalisedGroup"] <- "Group1"
    completeVec <- complete.cases(groups_table[,"personalisedGroup"])
    groups_table <- groups_table[completeVec,]
    groups_table <- groups_table[,c("personalisedGroup","Samples")]
  }
  boxplotdata <- results$v$E[which(rownames(results$v$E) %in% input$GeneVolcano),]
  boxplotdata <- rbind(boxplotdata,colnames(boxplotdata))
  rownames(boxplotdata)[nrow(boxplotdata)] <- "Samples"
  boxplotdata <- as.data.frame(t(boxplotdata)) %>%  gather(key = "GENE",value = "COUNTS", -Samples)
  boxplotdata$Samples <- as.character(boxplotdata$Samples)
  boxplotdata <- inner_join(boxplotdata,groups_table, by = "Samples")
  boxplotdata$COUNTS <- as.numeric(boxplotdata$COUNTS)
  if(input$var != "Create your own groups"){
    boxplotdata[,input$var] <- as.character(boxplotdata[,input$var])
    boxplotdata <- boxplotdata[which(boxplotdata[,input$var] %in% c(input$Group1sel,input$Group2sel)),]
    
    results$boxplots <- ggplot(boxplotdata, aes_string(x=input$var, y="COUNTS", fill = input$var)) +
      geom_boxplot(outlier.alpha = FALSE)  + facet_wrap(~GENE) +
      geom_point(position=position_jitterdodge(jitter.width=0.5, dodge.width = 0.2,
                                               seed = 1234),
                 pch=21,
                 show.legend = T)
  } else {
    boxplotdata[,"personalisedGroup"] <- as.character(boxplotdata[,"personalisedGroup"])
    
    results$boxplots <- ggplot(boxplotdata, aes_string(x="personalisedGroup", y="COUNTS", fill = "personalisedGroup")) +
        geom_boxplot() + facet_wrap(~GENE) +
      geom_point(position=position_jitterdodge(jitter.width=0.5, dodge.width = 0.2,
                                               seed = 1234),
                 pch=21,
                 show.legend = T)
  }
  }
})

  output$boxplots <- renderPlot(results$boxplots)
  output$boxplots_error <- renderText({
    validate(
    need(length(input$GeneVolcano) >= 2, "Select at least two genes to draw boxplots...")
  )
  })

  output$results_table <- DT::renderDataTable({
    a <- results$restable
    a$genes <- NULL
    datatable(
    a,escape = FALSE,options = list(scrollX=TRUE, scrollCollapse=TRUE))})


  output$resdl <- downloadHandler(
    filename = function() {
      paste("DEA-PDX-Results", Sys.Date(), ".csv", sep=",")
    },
    content = function(file) {
      write.csv(results$res, file)
    }
  )
  
  output$dlfigures <- downloadHandler(
    filename = function() {
      paste("DEA-Figures", Sys.Date(), ".pdf", sep=",")
    },
    content = function(file) {
      pdf(file = file)
      plot(results$Pvals_distrib)
      plot(Volcano$ggplot)
      plot(results$boxplots)
      dev.off()
    }
  )

  output$up_table <- DT::renderDataTable({
    a <- results$up
    a[,c("logFC","AveExpr","P.Value","adj.P.Val")] <- signif(a[,c("logFC","AveExpr","P.Value","adj.P.Val")],digits = 2)
    a$genes <- NULL
    DT::datatable(
    extensions = "FixedColumns",
    a,escape = FALSE,options = list(scrollX=TRUE, scrollCollapse=TRUE,fixedColumns = list(leftColumns = 1)))
    })

  output$updl <- downloadHandler(
    filename = function() {
      paste("DEA-UPS-PDX-Results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      a <- results$up
      write.csv(results$up, file, sep = ",")
    }
  )


  output$down_table <- DT::renderDataTable({
    a <- results$down
    a[,c("logFC","AveExpr","P.Value","adj.P.Val")] <- signif(a[,c("logFC","AveExpr","P.Value","adj.P.Val")],digits = 2)
    a$genes <- NULL
    datatable(
    extensions = "FixedColumns",
    a,escape = FALSE,options = list(scrollX=TRUE, scrollCollapse=TRUE,fixedColumns = list(leftColumns = 1)))
    })

  output$downdl <- downloadHandler(
    filename = function() {
      paste("DEA-DOWN-PDX-Results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(results$down, file, sep = ",")
    }
  )

  output$featuress <-
    renderInfoBox({
      req(results$nsignfc)
      infoBox(
        "Number of features passing FC and Pval Filters",
        paste(results$nsignfc,"Among the ",nrow(results$res),"features pass the filters"),
        icon = icon("dna")
      )
    })

  output$upp_numbers <-
    #renderInfoBox({
    renderValueBox({
      req(results$up)
     # column(width = 12,
      #infoBox(
      valueBox(
        as.character(nrow(results$up)),
        "Upp regulated features",
        icon = icon("dna"),color = "red"
        # "Number of features passing FC and Pval Filters",
        # paste(nrow(results$up),"Upp uppregulated features"),
      )
     # )
    })

output$features_value_box <- renderUI({
    fluidRow(
      column(width = 6,
           #infoBoxOutput(ns("down_numbers")),
           valueBoxOutput(ns('down_numbers'),width =  12)),
    column(width = 6,
           valueBoxOutput(ns("upp_numbers"),width =  12)
    ))
  })

  output$down_numbers <-
    #renderInfoBox({
    renderValueBox({

      req(results$down)
      #column(width = 12,
      valueBox(
        as.character(nrow(results$down)),
        "Down regulated features",
        icon = icon("dna"),color = "blue"
      )
      #)
    })

  return(results)

}
