#' Server function
#'
#' Function containing all the server side of the Shiny App, must be called inside the run_app() function
#'
#' @param input input
#' @param output output
#' @param session session
#'
#' @return None
#'
#' @import data.table
#' @import ggiraph
#' @importFrom readxl read_excel
#' @import ggplot2
#' @importFrom ggthemes theme_gdocs
#' @importFrom cowplot plot_grid get_legend panel_border draw_grob
#' @importFrom rlist list.append
#' @importFrom dplyr full_join select %>% filter distinct group_by mutate select row_number
#' @importFrom shinyAce updateAceEditor aceEditor aceAutocomplete
#' @import knitr
#' @importFrom xfun embed_files
#' @importFrom plyr join
#' @import kableExtra
#' @import ComplexHeatmap
#' @importFrom circlize colorRamp2
#' @importFrom grid gpar grid.rect
#' @importFrom shinyWidgets updatePickerInput pickerInput pickerOptions
#' @import tictoc

server_PDX_app <- function(input, output, session) {

######### Global options and Values ##############
setwd("/tmp")
Sys.setenv(VROOM_THREADS = 8) # Nombre de procs utilise par la fonction vroom pour vite ouvrir les data
session$onSessionEnded(stopApp)
options(shiny.sanitize.errors = TRUE)
options(shiny.maxRequestSize=200*1024^2)

inFile <- system.file("extdata", "CnGap_CNL_serialized.RDA",package = "PdxAppPackage")
qs::qload(inFile,nthreads=1)

############## Cicerone ######################

guide <- Cicerone$
  new()$
  step(el = "Filters",
    title ="Here you can use filters to select a group of samples"
  )$
  step(
    el = "SamplesInfo",
    title = "This table resume the samples selected after the filtering step"
  )#$
  # step(
  #   el = "sidebar",
  #   title = "Some medatada distributions among selected samples"#,
  #   # is_id = FALSE,
  #   # tab = "Metadata",
  #   # tab_id ="sidebartabs"
  #   #toggle(condition = input$foo, selector = "#sidebartabs li a[data-value=haha]")
  # )

guide2 <- Cicerone$
  new()$
  step(el = "customlist",
       title ="This input expect one gene per line",
       HTML("</br><b> Warning :</b> If you launch a clustering on less than 3 genes, the app will crash</br></br>")
       
  )


genelist <- data.frame(genename = c("EIF2B1","EXOSC7","GAPDH","GFM1","GUK1","IARS2","IMP4","IPO13","LAS1L",
                                    "MCM2","MRPS34","NDC80","NSF","NUF2","NVL","PCNA","PES1","POLR3H","PSMB3","RFC5","RUVBL2"))
output$genelistexample <- downloadHandler(
  filename = function() {
    paste0("PDX_app_example_gene_list_",as.character(Sys.time()),".txt")
  },
  content = function(file) {
    req(genelist)
    print("genelist")
    print(genelist)
    write.table(data.frame(genename = c("EIF2B1","EXOSC7","GAPDH","GFM1","GUK1","IARS2","IMP4","IPO13","LAS1L",
                                        "MCM2","MRPS34","NDC80","NSF","NUF2","NVL","PCNA","PES1","POLR3H","PSMB3","RFC5","RUVBL2")),file = file, sep = ",",quote = FALSE,row.names = FALSE,col.names = FALSE)
  })


observeEvent(input$startCicerone, {
  guide$init()$start()
})

observeEvent(input$startCicerone2, {
  guide2$init()$start()
})

##############  END OF CICERONE ##############################

############## SamplesSelection Tab #####################
# Import sample Info
tic("Load Metadata V2 and operates on Drug Response NAs")
SamplesInfo   <- as.data.frame(read_excel(path = system.file("extdata", "MetadataV2.xlsx", package = "PdxAppPackage"),
                                          sheet = "metadata"))
SamplesInfo$CNV <- tidyr::replace_na(SamplesInfo$CNV,"NA")
SamplesInfo$RNA <- tidyr::replace_na(SamplesInfo$RNA,"NA")
SamplesInfo$ChIPseq <- tidyr::replace_na(SamplesInfo$ChIPseq,"NA")
SamplesInfo$Capecitabine[is.na(SamplesInfo$Capecitabine)] <- "ND"
SamplesInfo$AC_response[is.na(SamplesInfo$AC_response)] <- "ND"
#SamplesInfo$Brcaness <- as.character(SamplesInfo$Brcaness)
toc(log = TRUE)

tic("Correct Drug response format in MetadataV2")
for (row in 1:nrow(SamplesInfo)){
  if(SamplesInfo$Capecitabine[row] == "PD"){
    SamplesInfo$labelsCapecitabine[row] <- "PD  = progressive disease"
  } else if (SamplesInfo$Capecitabine[row] == "R") {
    SamplesInfo$labelsCapecitabine[row] <- "R  = Regression"
  } else if (SamplesInfo$Capecitabine[row] == "SD") {
    SamplesInfo$labelsCapecitabine[row] <- "SD = stable disease"
  } else if (SamplesInfo$Capecitabine[row] == "ND") {
    SamplesInfo$labelsCapecitabine[row] <- "ND = not done"
  } else if (SamplesInfo$Capecitabine[row] == "CR") {
    SamplesInfo$labelsCapecitabine[row] <- "CR = complete response"
  }
  if(SamplesInfo$AC_response[row] == "PD"){
    SamplesInfo$labelsAC_response[row] <- "PD  = progressive disease"
  } else if (SamplesInfo$AC_response[row] == "R") {
    SamplesInfo$labelsAC_response[row] <- "R  = Regression"
  } else if (SamplesInfo$AC_response[row] == "SD") {
    SamplesInfo$labelsAC_response[row] <- "SD = stable disease"
  } else if (SamplesInfo$AC_response[row] == "ND") {
    SamplesInfo$labelsAC_response[row] <- "ND = not done"
  } else if (SamplesInfo$AC_response[row] == "CR") {
    SamplesInfo$labelsAC_response[row] <- "CR = complete response"
  }

}
toc(log = TRUE)
SamplesInfo$labelsAC_response <- as.factor(SamplesInfo$labelsAC_response)
SamplesInfo$labelsCapecitabine <- as.factor(SamplesInfo$labelsCapecitabine)
SamplesInfo$Brcaness <- as.character(SamplesInfo$Brcaness)
SamplesInfoR <- reactiveValues(metadata = SamplesInfo)
# Import CNV Info
tic("Load SamplesInfo SampleSheet_CNV data")
CNVInfo   <- as.data.frame(read_excel(path = system.file("extdata", "SamplesInfo.xlsx", package = "PdxAppPackage"),
                                      sheet = "SampleSheet_CNV"))
CNVInfoR <- reactiveValues(metadata = CNVInfo)
toc(log = TRUE)
# Import Drug response Info
`%notin%` <- Negate(`%in%`)
# Creates Filters
output$Technofilters <- renderUI({
box(title = p('Filters',actionButton("startCicerone",label=NULL,icon = icon("info-circle"))), id = "Filters",
    width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE, status = "success",
       column(width =12,
                   pickerInput("SelSplName",
                                   label = "Enter sample name that you want to Add for further analysis",
                                   choices = NULL,
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
  column(width =4,checkboxGroupInput("TechnoCNV", label = "Copy Number",
                    choices = unique(SamplesInfo$CNV), selected = unique(SamplesInfo$CNV))),
  column(width =4,checkboxGroupInput("TechnoRNA", label = "Gene Expression",
                    choices = unique(SamplesInfo$RNA), selected = unique(SamplesInfo$RNA))),
  column(width =4,checkboxGroupInput("ChiP", label = "ChIP seq results",
                    choices = unique(SamplesInfo$ChIPseq), selected= unique(SamplesInfo$ChIPseq)))
 ) #end of box
})

observe({
  updatePickerInput(session = session, inputId = "SelSplName",
                    choices = unique(SamplesInfo[,"PDX_model"])[which(unique(SamplesInfo[,"PDX_model"]) %notin% SamplesInfoR$metadata$PDX_model)])
  updatePickerInput(session = session, "AnnotOnco", choices = colnames(SamplesInfoR$metadata)[colnames(SamplesInfoR$metadata) %in% cols_annots])

})
#Observers
observeEvent(c(input$TechnoCNV,input$TechnoRNA,input$ChiP),ignoreInit = TRUE,
             ignoreNULL = TRUE,{
  
  SamplesInfoR$metadata <- SamplesInfo[which(SamplesInfo$CNV %in% input$TechnoCNV),]
  SamplesInfoR$metadata <- SamplesInfoR$metadata[which(SamplesInfoR$metadata$RNA %in% input$TechnoRNA),]
  SamplesInfoR$metadata <- SamplesInfoR$metadata[which(SamplesInfoR$metadata$ChIPseq %in% input$ChiP),]
  observeEvent(input$SelSplName,ignoreInit = TRUE,
               ignoreNULL = TRUE,{
                 #if(!is.null(input$SelSplName)){
                 if(length(input$SelSplName) != 0){
                   lapply(input$SelSplName, function(x){
                     if (!(x %in% SamplesInfoR$metadata$PDX_model)){
                       SamplesInfoR$metadata <- rbind(SamplesInfoR$metadata,SamplesInfo[which(SamplesInfo$PDX_model ==  x),])
                     }
                   })
                 }
               })
  CNVInfoR$metadata <- CNVInfo[which(CNVInfo$CNV_SampleID %in% SamplesInfoR$metadata$CNV_SampleID),]
  })

############## Metadata Tab #####################

# CNV TEchno
output$TechnoPlot <- renderGirafe({
  TechnoPlot <- ggplot(CNVInfoR$metadata) +
    aes(x = `Technology`,tooltip = ..count..) +
    geom_bar_interactive() +
    labs(x = "Technology") +
    coord_flip() +
    theme_gdocs() +
    theme(legend.title = element_blank())

  girafe(code = {print(TechnoPlot)})

})
# Tum Type
output$TsubPlot <- renderGirafe({
  Tsub_interactive <- ggplot(SamplesInfoR$metadata) +
    aes(x = `Tumor_subtype`, fill =`Tumor_subtype`,tooltip = ..count..) +
    geom_bar_interactive() +
    labs(x = "Tumor subtype") +
    coord_flip() +
    theme_gdocs() +
    theme(legend.title = element_blank())
  girafe(code = {print(Tsub_interactive)})
})

# Lehman classification
output$LehmanPlot <- renderGirafe({
  tic("render Lehman Plot")
  Tsub_interactive <- ggplot(SamplesInfoR$metadata) +
    aes(x = `Lehman_classification`, fill =`Lehman_classification`,tooltip = ..count..) +
    geom_bar_interactive() +
    labs(x = "Lehman Classification") +
    coord_flip() +
    theme_gdocs() +
    theme(legend.title = element_blank())

  girafe(code = {print(Tsub_interactive)})
  toc(log = TRUE)
  })

# Brcaness
output$Brcaness <- renderGirafe({

  tic("render Brcaness")
  Tsub_interactive <- ggplot(SamplesInfoR$metadata) +
    aes(x = `Brcaness`, fill =`Brcaness`,tooltip = ..count..) +
    geom_bar_interactive() +
    scale_fill_manual(values= c("No" = "#FF0000" ,"Na"= "#808080","BRCAness" = "#00CC00")) +
    labs(x = "Brcaness") +
    coord_flip() +
    theme_gdocs() +
    theme(legend.title = element_blank())

  girafe(code = {print(Tsub_interactive)})
  toc(log = TRUE)
})

# Histology
output$histology <- renderGirafe({
  Tsub_interactive <- ggplot(SamplesInfoR$metadata) +
    aes(x = `histology`, fill =`histology`,tooltip = ..count..) +
    geom_bar_interactive() +
    labs(x = "histology") +
    coord_flip() +
    theme_gdocs() +
    theme(legend.title = element_blank())

  girafe(code = {print(Tsub_interactive)})
})

#Patient_NAC
output$Patient_NAC <- renderGirafe({
  Tsub_interactive <- ggplot(SamplesInfoR$metadata) +
    aes(x = `Patient_NAC`, fill =`Patient_NAC`,tooltip = ..count..) +
    geom_bar_interactive() +
    labs(x = "Patient_NAC") +
    theme_gdocs() +
    theme(legend.position = "bottom",legend.title = element_blank(),
          axis.text.x = element_text(angle = 35, size = 6),
          legend.direction="horizontal", legend.text = element_text(size = 7))

  girafe(code = {print(Tsub_interactive)})
})

output$Distributions <- renderUI({
  box(title = 'Distributions', width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE, status = "success",
      fluidPage(
      fluidRow(
        column(width = 6,
                      p("Tumor substypes",align = "center"),
                      girafeOutput("TsubPlot",height = "100%",width = "95%")),
        column(width = 6,
               p("Histology",align = "center"),
               girafeOutput("histology",height = "100%",width = "95%"))),
      fluidRow(
        column(width = 6,
               p("Lehman Classification",align = "center"),
               girafeOutput("LehmanPlot",height = "100%",width = "95%")),
        column(width = 6,
               p("Brcaness",align = "center"),
               girafeOutput("Brcaness",height = "100%",width = "95%"))),
      fluidRow(
        column(width = 12,
               p("Patient NAC",align = "center"),
               girafeOutput("Patient_NAC",height = "100%",width = "95%")))
      ) # end of fluidPage
  )# end of box
})

############### Drug response tab ###########################
cols <- c("SD = stable disease"= "#0000FF", "PD  = progressive disease" = "#FF0000" ,"ND = not done"= "#808080","R  = Regression" = "#009900",
          "CR = complete response" = "#00CC00")

output$Capecitabine <- renderGirafe({
  if(!is.null(SamplesInfoR$metadata)){
  Capecitabine <- ggplot(SamplesInfoR$metadata) +
    aes(x = `Capecitabine`, fill = `labelsCapecitabine`,tooltip = ..count..) +
    geom_bar_interactive() +
    labs(x = "Capecitabine Response") +
    coord_flip() +
    theme_gdocs() +
    theme(legend.title = element_blank()) +
    scale_fill_manual(values= cols)
girafe(code = {print(Capecitabine)})
}
})

reacAC <- reactive({
  AC <- ggplot(SamplesInfoR$metadata) +
    aes(x = `AC_response`, fill = `labelsAC_response`,tooltip = ..count..) +
    geom_bar_interactive() +
    labs(x = "AC response") +
    coord_flip() +
    theme_gdocs() +
    theme(legend.title = element_blank()) +
    scale_fill_manual(values= cols)

  legend_AC <- get_legend(
    AC +
      guides(color = guide_legend(nrow = 1, keywidth = 5)) +
      theme(legend.position = "top", legend.key.size = unit(2, "cm"),
            legend.box.just ="left",
            legend.title = element_blank(),
            legend.direction = "horizontal",
            legend.spacing.x = unit(1.0, 'cm') )

  )
  return(list(AC,legend_AC))
})

output$AC <- renderGirafe({
  if(!is.null(SamplesInfoR$metadata)){
  girafe(code = {print(reacAC()[1])})
  }
})

output$ACsplit <- renderGirafe({
    SplitBy <- input$splitDistrib
    if(!is.null(SamplesInfoR$metadata)){
    plot <- ggplot(SamplesInfoR$metadata[,c("AC_response","labelsAC_response",SplitBy)]) +
        ggtitle(paste0("AC response splitted by : ",SplitBy)) +
        aes(x = `AC_response`, fill = `labelsAC_response`,tooltip = ..count..) +
        geom_bar_interactive() +
      facet_grid(as.name(SplitBy)) +
        labs(x = "AC response") +
        coord_flip() +
        theme_gdocs() +
        panel_border() +
        theme(legend.position = "bottom",plot.title = element_text(size = 12, face = "bold")) +
    scale_fill_manual(values= cols)
    return(girafe(ggobj = plot))
    }
})

output$Capecitabinesplit <- renderGirafe({
  SplitBy <- input$splitDistrib
  plot <- ggplot(SamplesInfoR$metadata[,c("Capecitabine","labelsCapecitabine",SplitBy)]) +
    ggtitle(paste0("Capecitabine response splitted by : ",SplitBy)) +
    aes(x = `Capecitabine`, fill = `labelsCapecitabine`,tooltip = ..count..) +
    geom_bar_interactive() +
    facet_grid(as.name(SplitBy)) +
    labs(x = "Capecitabine response") +
    coord_flip() +
    theme_gdocs() +
    panel_border() +
    theme(legend.position = "bottom",plot.title = element_text(size = 12, face = "bold")) +
    scale_fill_manual(values= cols)
  return(girafe(ggobj = plot))
})

output$TechnoGenBox <- renderUI({
box(title = 'Technology', width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, status = "success",
        girafeOutput("TechnoPlot")

)# end of box
})

output$DrugResponseGenBox <- renderUI({
box(title = 'Drug response distributions', width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE, status = "success",
fluidRow(
    column(width = 6,p("Capecitabine"),
    fluidRow(girafeOutput("Capecitabine",height = "100%",width = "95%"))),
    column(width = 6,p("AC"),
    fluidRow(girafeOutput("AC",height = "100%",width = "95%")))
    )
 )# end of box
})

output$DrugResponseBox <- renderUI({
box(title = 'Variables effects on drug response', width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE, status = "success",
    fluidPage(
    fluidRow(column(width=6,
                    selectInput("Drug", "Select a drug",
                                choices = c("AC" = "AC_response","Capecitabine"),selected = "AC")
                    ),
             column(width = 6,
                    selectInput("splitDistrib","Split Drug response distributions by",
                                choices = c("Brcaness","Acquired_resistance","histology","AC_response","Capecitabine","Patient_NAC"),selected = "Brcaness")
                    )
             ),
    fluidRow(
      conditionalPanel(
        condition = "input.Drug == 'AC_response'",
        girafeOutput("ACsplit")
        ),
      conditionalPanel(
        condition = "input.Drug == 'Capecitabine'",
        girafeOutput("Capecitabinesplit")
      )
          )
    ) # end of FluidPage
  )#end of box
})

output$Mutations <- renderGirafe({

  tic("render girage split by mutation plot")
    SplitBy <- input$genesplit
    plot <- ggplot(SamplesInfoR$metadata[,c(input$Drug,paste0('labels',input$Drug),SplitBy)]) +
      ggtitle(paste0("Effect of mutation : ",SplitBy," on ",gsub('_response','',input$Drug)," response")) +
      aes_string(x = as.character(SplitBy), fill = as.character(SplitBy)) +
      geom_bar_interactive() +
      facet_grid(reformulate(input$Drug)) +
      labs(x = paste0(gsub('_response','',input$Drug)," response"),fill = as.name(SplitBy)) +
      panel_border() +
      theme(legend.position = "bottom",plot.title = element_text(size = 12, face = "bold"),
            legend.title = element_text(size = 3),
            axis.text.x = element_blank()) #+

    return(girafe(ggobj = plot))

    toc(log = TRUE)
})

output$MutationsBox <- renderUI({
  box(title = "Mutations effects on drug response", width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE, status = "success",
      fluidPage(
        fluidRow(
          column(width=6,selectInput("genesplit","Select a gene to split distribution",choices = c("AKT1","PIK3CA","BRCA1","BRCA2","TP53",
                                                                                         "EGFR","HRAS","GATA3","ERBB4","PI3KCA","STK11","NRAS","MAP3K1","PIK3R1","ROS1",
                                                                                         "NOTCH4","PTEN","NOTCH2","NF1",
                                                                                         "BRAF","TBX3"),
                           selected = "PIK3CA")
                 ) # end of column
          ), # end of row
      fluidRow(girafeOutput("Mutations"))
  )# end of fluidPage
  ) # end of box
})

############# RNAseq Tab #####################
tic(" Load all counts types from rda file")
qs::qload(file = system.file("extdata", "Pdx_counts_all_types_serialized.rda", package = "PdxAppPackage"),nthreads=1)
toc(log = TRUE)

tic("load lincRNA_protCod_Symbols_Raw data")
Rawdata <- reactiveValues(table = Raw)
toc(log = TRUE)
Metadata <- reactiveValues(table = NULL)
observe({
a <- SamplesInfoR$metadata
rownames(a) <- a$PDX_model
a$PDX_model <- NULL
Metadata$table <- a
})

tic("load lincRNA_protCod_Symbols_VST_&_TPS data")
TPMdata <- reactiveValues(table = NULL)
VSTdata <- reactiveValues(vars = Vst)
toc(log = TRUE)

observeEvent(Metadata$table,{
  req(Metadata$table)
  TPMdata$table <- Tpm[,colnames(Tpm) %in% rownames(Metadata$table)]
})

filtered_counts <- callModule(FilterRNAServer, id = "filtRnaSeq", session = session,
                              data = TPMdata)

# Creates the contrast Matrix
MetadataModel <- reactiveValues(table= NULL)
RawdataModel <- reactiveValues(table =  NULL)

observe({
  tic("addClusters to metadata")
 a <-Metadata$table[colnames(Rawdata$table),c("Tumor_subtype","Lehman_classification","Patient_NAC",
#                       # "AKT1","PIK3CA","NOTCH2","BRCA1",
#                       # "BRCA2","TP53","NOTCH4","PTEN",
#                       # "NF1","EGFR","HRAS","GATA3",
#                       # "ERBB4","PI3KCA","STK11","NRAS",
#                       # "MAP3K1","PIK3R1","ROS1","BRAF",
#                       #"TBX3",
                      "AC_response","Capecitabine",
                      "Acquired_resistance","histology","Brcaness","Clusters")]

if(!is.null(heatmap$plot$dendCol)){
  Clusters <- as.data.frame(heatmap$plot$dendCol)
  colnames(Clusters) <- "Clusters"
  a$Clusters <- Clusters$Clusters[match(rownames(a),rownames(Clusters))]
}

a <- a[which(!(rownames(a) %in% c("NA","NA.1","NA.2","NA.3"))),]
for (col in 1:ncol(a)){
 a[,col] <- as.factor(a[,col])
}

if(!is.null(Rawdata$table)){
RawdataModel$table <- Rawdata$table[which(rownames(Rawdata$table) %in% rownames(filtered_counts$DataFiltered)),which(colnames(Rawdata$table) %in% rownames(a))]
MetadataModel$table <- a[which(rownames(a) %in% colnames(Rawdata$table)),]
meta <- MetadataModel$table
}
 toc(log = TRUE)
})

output$modelUI <- renderUI({
  tagList(fluidPage(rv$all_ui[["Design"]]))
})

RawdataDEA <- reactiveValues(table =  NULL)
DEA <- reactiveValues(res= NULL, upp = NULL, down = NULL)
observeEvent(input$sidebartabs,{
  if(input$sidebartabs=="DEG"){
    if(ncol(RawdataModel$table) == 0){
    showModal(modalDialog(
      title = "No samples available for DEA analysis",
      HTML("Please check that all samples have not been filtered out in the Metadata outlet"),
      easyClose = FALSE,
      footer = tagList(
        actionButton("gotometadata","Ok"))))
    }
  }
})

observeEvent(input$gotometadata,{
newtab <- switch(input$sidebartabs, "DEG" = "SamplesSelection","SamplesSelection" = "DEG")
updateTabItems(session, "sidebartabs", newtab)
removeModal()
})

DEA <- callModule(DeaPdxServer, "DEA", session = session,
                  matrix = RawdataModel,
                  sampleplan = MetadataModel,
                  var = colnames(MetadataModel$table))

# Pca Module
MetadataPCA <- reactiveValues(table = NULL)
dataPCA <- reactiveValues(table = NULL)
observeEvent(c(DEA$up,input$dataPCA,SamplesInfoR$metadata,input$sidebartabs),{
  if(input$sidebartabs=="PCA"){
  tic("PCA observer")
  if (input$dataPCA == "Whole TPM table") {
    dataPCA <- TPMdata$table
  } else if (input$dataPCA == "Whole VST table"){
    dataPCA <- VSTdata$vars
  } else if (input$dataPCA == "Filtered TPM table"){
    dataPCA <- TPMdata$table[rownames(filtered_counts$DataFiltered),]
  } else if (input$dataPCA == "Filtered VST table"){
    dataPCA <- VSTdata$vars[rownames(filtered_counts$DataFiltered),]
  } else if (is.null(DEA$res)){
    print("PCANULLRES")
    showModal(modalDialog(
      title = "Missing differentially expressed genes' list",
      HTML("Please perform DE analysis first"),
      actionButton("gobackdeg", strong("Go to differential analysis tab"),width = "100%"),
      easyClose = FALSE
    ))
    dataPCA <- data.frame()
    return()
  } else if (input$dataPCA == "DE genes (VST data)"){
  print("DE genes (VST data)")
  a <- VSTdata$vars[rownames(DEA$up),]
  b <- VSTdata$vars[rownames(DEA$down),]
  dataPCA <- rbind(a,b)
  } else if (input$dataPCA == "DE UP genes (VST data)"){
  dataPCA <- VSTdata$vars[rownames(DEA$up),]
  } else if (input$dataPCA == "DE DOWN genes (VST data)"){
  dataPCA <- VSTdata$vars[rownames(DEA$down),]
}
  MetadataPCA <- MetadataModel$table[which(rownames(MetadataModel$table) %in% SamplesInfoR$metadata[,"PDX_model"]),]
  dataPCA$table <- dataPCA[,which(colnames(dataPCA) %in% SamplesInfoR$metadata$PDX_model)]
  MetadataPCA$table  <- MetadataPCA[rownames(MetadataPCA) %in% colnames(dataPCA),]
  if(ncol(dataPCA$table) == 0){
  showModal(modalDialog(
    title = "No RNAseq data available in your selected data, please go back to sample selection",
    actionButton("gobacksplsel4", strong("Go to sample selection tab"),width = "100%"),
    easyClose = FALSE,
    footer = tagList(
      modalButton("Cancel"))
  ))
  return()
  } else if(nrow(dataPCA$table) < 10){
    showModal(modalDialog(
      title = "Not enough genes selected to perform PCA",
      "If you want to perform PCA on differential expressed genes only. Please check there are at list 10 of them.",
      easyClose = FALSE,
      footer = tagList(
        modalButton("Cancel"))
    )) 
    return()
  } else {
  PCA <- callModule(DrawPCAServer2,"PCA1",
                    matrix = dataPCA,
                    metadata = MetadataPCA, session = session)
  }

  toc(log = TRUE)
}
})

cols_annots <- c("AC_response",'Capecitabine',"Acquired_resistance","Brca1.2.mutation",
          "Patient_NAC","Lehman_classification","Brcaness","Tumor_subtype",
          "histology","BRCA1","AKT1","PIK3CA","BRCA1","BRCA2","TP53",
          "EGFR","HRAS","GATA3","ERBB4","PI3KCA","STK11","MAP3K1","PIK3R1",
          "NOTCH4","PTEN","NOTCH2","NF1",  "BRAF","TBX3","ROS1")

Clusterdata <- reactiveValues(table = NULL)
ClusterMetadata <- reactiveValues(table = NULL)
observeEvent(c(input$ClusterData,Metadata$table,DEA$res,input$nvariantgenes,input$sidebartabs,input$customlist),{
  req(input$ClusterData)
  req(Metadata$table)
  withProgress(
    message = "Preparing data for heatmap",{
  tic("observe data table")
  if(input$sidebartabs=="RNAClustering"){
  if(input$ClusterData == "whole VSTs"){
    # Je ne garde que 100 gène car limité par la RAM de mon PC perso
    #Clusterdata$table <- VSTdata$vars[1:50,]
    #if(!is.null(input$nvariantgenes)){
      Clusterdata$table <- VSTdata$vars
    #}
  } else if (input$ClusterData == "Most variant genes") {
    if(length(filtered_counts$DataFiltered) == 0){
      showModal(modalDialog(
        title = "Filtering not done",
        "Please perform data transformation and filtering on the data outlet",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Ok")))) 
    } else {
      if(!is.null(input$nvariantgenes) & !is.na(input$nvariantgenes)){
      Clusterdata$table <- VSTdata$vars[rownames(filtered_counts$DataFiltered),][1:input$nvariantgenes,]
      }
    }
    } else if (input$ClusterData == "DEA genes" && is.null(DEA$res)) {
        showModal(modalDialog(
          title = "Missing differentially expressed genes' list",
          "Please perform DE analysis first",
          easyClose = TRUE
        ))
      updateSelectInput(session = session,"ClusterData",selected = "Filtered VSTs")
    } else if (input$ClusterData == "DEA genes" && !(is.null(DEA$res))) {
      a <- VSTdata$vars[rownames(DEA$up),]
      b <- VSTdata$vars[rownames(DEA$down),]
      Clusterdata$table <- rbind(a,b)
    } else if (input$ClusterData == "Custom list of genes"){
      if(!is.null(input$customlist) & !is.na(input$customlist) & (input$customlist != "")){
      customlist <- strsplit(input$customlist, "\n")
      #customlist <- as.character(customlist[[1]])
      customlist <- unlist(customlist[[1]])
      Clusterdata$table <- VSTdata$vars[rownames(VSTdata$vars) %in% customlist,]
      }
    }
  ClusterMetadata$table<- Metadata$table[which(rownames(Metadata$table) %in% colnames(Clusterdata$table)),cols_annots, drop =F]
  Clusterdata$table <- log10(Clusterdata$table[,which(colnames(Clusterdata$table) %in% rownames(ClusterMetadata$table)), drop =F])
  print(head(Clusterdata$table))
  print(head(ClusterMetadata$table))
  toc(log = TRUE)
  if (nrow(ClusterMetadata$table) == 0){
    showModal(modalDialog(
      title = "No RNAseq data available in your selected data, please go back to sample selection",
      actionButton("gobacksplsel2", strong("Go to sample selection tab"),width = "100%"),
      easyClose = FALSE,
      footer = tagList(
        modalButton("Cancel"))
    ))
  }
  }

})
  ClusterMeta <- ClusterMetadata$table
})

heatmap <- callModule(ClusteringServer, id = "heatmapID", session = session,
                                                 data = Clusterdata ,
                                                 metadata =  ClusterMetadata,
                                                 printRows = FALSE)

#### Output Tables #############
output$Metadata <- DT::renderDataTable(Metadata$table, options = list(scrollX=TRUE))
output$Rawdata <- DT::renderDataTable(Rawdata$table, options = list(scrollX=TRUE))
output$SamplesInfo <- DT::renderDataTable(DT::datatable(SamplesInfoR$metadata,extensions = "FixedColumns",
                                                        options = list(scrollX=TRUE,autoWidth = TRUE,fixedColumns = list(leftColumns = 1)), 
                                                        rownames= FALSE))

####### Output Values Box #############
features <- reactiveValues(number = NULL)
observeEvent(c(Rawdata$table,
               filtered_counts$DataFiltered),{
    if(!is.null(filtered_counts$DataFiltered)){
      features$number <- nrow(filtered_counts$DataFiltered)
    }else{
      features$number <- nrow(Rawdata$table)
   }
})

output$features <-
  renderInfoBox({
    infoBox(
      "Number of selected features",
      features$number,
      icon = icon("dna")
    )
})

############# CNV Tab #####################
#### Oncoplot
CNVtabsub <- reactiveValues(matrix = NULL, plot = NULL)
CNVtabsubOnco <- reactiveValues(matrix = NULL, nagenes = NULL)

#observe({
old_list <- reactiveValues(init = "old_list_init")
observeEvent(input$sidebartabs,{
  if(input$sidebartabs=="CNV"){
      SampleList_in_meta <- SampleList[SampleList %in% SamplesInfoR$metadata$PDX_model]
      if(length(SampleList_in_meta) > 0){
      if(SamplesInfoR$metadata$PDX_model != old_list$init){
      SampleList_in_meta <- SampleList[SampleList %in% SamplesInfoR$metadata$PDX_model]
    withProgress(
    message = "Updating OncoPrint inputs",{
      updatePickerInput(session = session, inputId = 'queries_genes', choices = GeneList,selected  = c("OR1D5","TP53","CDKN2A","AGRN","APOBR","YAP1","XRCC4","ZAR1","ZZZ3","ZYX","ZSWIM7","SPCS1","TBCC"))
      updatePickerInput(session = session, inputId = 'queries_samples', choices = SampleList_in_meta,
                  selected = as.character(SamplesInfoR$metadata$PDX_model))
     old_list$init <- SamplesInfoR$metadata$PDX_model
     })
    }} else {
      showModal(modalDialog(
        title = "No CNV data available in your selected data, please go back to sample selection",
        actionButton("gobacksplsel3", strong("Go to sample selection tab"),width = "100%"),
        easyClose = FALSE,
        footer = tagList(
          modalButton("Cancel"))
      ))
  }}
})

observe({
  if(length(input$queries_samples) >= 2 && length(input$queries_genes) >= 2){
    DataOnco <- GNL %>%
      filter(row.names(GNL) %in% input$queries_genes)
    DataOnco <- DataOnco[,input$queries_samples]
    CNVtabsubOnco$matrix <- DataOnco
  }
})

alter_fun <- list(background = function(x, y, w, h) {
  grid.rect(x, y, w-unit(0.5, "mm"), h-unit(0.5, "mm"),
            gp = gpar(fill = "#CCCCCC", col = NA))},
  L = function(x, y, w, h) {
    grid.rect(x, y, w-unit(0.3, "mm"), h-unit(0.3, "mm"),
              gp = gpar(fill = "deeppink4", col = NA))},
  N = function(x, y, w, h) {
    grid.rect(x, y, w-unit(0.3, "mm"), h-unit(0.3, "mm"),
              #gp = gpar(fill = "grey45", col = NA))},
              gp = gpar(fill = "SlateGray1", col = NA))},
  D = function(x, y, w, h) {
    grid.rect(x, y, w-unit(0.3, "mm"), h-unit(0.3, "mm"),
              gp = gpar(fill = "red", col = NA))},
  G = function(x, y, w, h) {
    grid.rect(x, y, w-unit(0.3, "mm"), h-unit(0.3, "mm"),
              gp = gpar(fill = "green", col = NA))},
  A = function(x, y, w, h) {
    grid.rect(x, y, w-unit(0.3, "mm"), h-unit(0.3, "mm"),
              gp = gpar(fill = "blue", col = NA))}
)

col.oncoprint <- c("A" = "blue",'G' = "green",
                   #"N" = "grey45",
                   "N" = "SlateGray1",
                   "L" = "deeppink4", "D" = "red")

metaOncoCNV <- reactiveValues(table = NULL)
observeEvent(c(CNVtabsubOnco$matrix,
               input$AnnotOnco),{
                 withProgress(
                   message = "Plotting OncoPrint",{
                 tic("Calculates Freq")
                 Freq <- apply(CNVtabsubOnco$matrix,1,table)
                 Denom <- lapply(Freq,function(x) {
                   b <- as.data.frame(x)
                   colnames(b) <- c("Type","Occurences")
                   b <- b[which(b$Type != "N"),]
                   b <- as.numeric(sum(b$Occurences))

                   return(b)
                 })
                 Sums <- lapply(Freq,function(x) {
                   return(as.numeric(sum(x)))
                 })
                 Freq <- list()
                 for (i in 1:length(Sums)){
                   Freq[[i]] <- round((Denom[[i]]/Sums[[i]])*100, digits = 1)
                 }
                 names(Freq) <- names(Denom)
                 Freq <-  as.data.frame(t(as.data.frame(Freq)))
                 colnames(Freq) <- "Alt_frequencies"
                 toc(log = TRUE)

                 left_ann <- HeatmapAnnotation(
                   Alt_Freq = anno_text(paste0(Freq$Alt_frequencies," %"),
                                        location  = unit(-0.2, "npc"),rot = 30,
                                        gp = gpar(fontsize = 8)),
                   which = "row",
                   show_legend = TRUE
                 )
                 if(length(input$AnnotOnco) > 0){
                   print("metaOncoCNV")
                   SamplesInfoRmetadata <- SamplesInfoR$metadata
                   metaOncoCNV <- SamplesInfoR$metadata[,c(input$AnnotOnco,"PDX_model")]
                   rownames(metaOncoCNV) <- metaOncoCNV$PDX_model
                   metaOncoCNV <- metaOncoCNV[,input$AnnotOnco, drop = F]
                   metaOncoCNV <- metaOncoCNV[colnames(CNVtabsubOnco$matrix),]

                   if (length(input$AnnotOnco) >=2 ){
                     bot_ann <- HeatmapAnnotation(
                       df = metaOncoCNV,
                       which = "col",
                       show_legend = TRUE
                     )#,
                   } else {
                     print("length =1")
                     bot_ann <- HeatmapAnnotation(
                       Annot_name = metaOncoCNV,
                       which = "col",
                       show_legend = TRUE,
                       annotation_legend_param = list(Annot_name = list(title = input$AnnotOnco)))#,
                     names(bot_ann) <- input$AnnotOnco
                   }
                   CNVtabsubOncomatrix <- CNVtabsubOnco$matrix
                   CNVtabsub$plot <- oncoPrint(CNVtabsubOnco$matrix,
                                               alter_fun = alter_fun,
                                               col = col.oncoprint,
                                               remove_empty_columns = TRUE,
                                               remove_empty_rows = TRUE,
                                               show_row_names = TRUE,
                                               #show_column_names = FALSE,
                                               show_column_names = TRUE,
                                               show_pct = FALSE,
                                               bottom_annotation = bot_ann,
                                               left_annotation = left_ann,
                                               top_annotation = HeatmapAnnotation(cbar = anno_oncoprint_barplot(bar_width = 0.98,
                                                                                                                which = "column"))
                                               )
                 } else {
                   CNVtabsub$plot <- oncoPrint(CNVtabsubOnco$matrix,
                                               alter_fun = alter_fun,
                                               col = col.oncoprint,
                                               remove_empty_columns = TRUE,
                                               remove_empty_rows = TRUE,
                                               show_row_names = TRUE,
                                               #show_column_names = FALSE,
                                               show_column_names = TRUE,
                                               show_pct = FALSE,
                                               left_annotation = left_ann,
                                               top_annotation = HeatmapAnnotation(cbar = anno_oncoprint_barplot(bar_width = 0.98,
                                                                                                                which = "column"))
                                               )
                 }
                 return(CNVtabsub$plot)
                   })
               }) # end of ObserveEvent

output$CNVOnco <- renderPlot({
  tic("Draw oncoPrint ...")
  if (!is.null(CNVtabsub$plot)){
    draw(CNVtabsub$plot)
  }
  toc(log = TRUE)
})

output$CNVsel <- DT::renderDataTable(
  datatable(CNVtabsubOnco$matrix,
            extensions = "FixedColumns",
            options = list(scrollX=TRUE, scrollCollapse=TRUE,fixedColumns = list(leftColumns = 1)))
)

output$CNVseldl <- downloadHandler(
  filename = function() {
    paste0("CNV_data_",as.character(Sys.time()),".csv")
  },
  content = function(file) {
    write.table(CNVtabsubOnco$matrix,file = file, sep = ",",quote = FALSE)
  })

output$nagenes <-
  renderInfoBox({
    infoBox(
      "The following genes have been removed from the Oncoplot because they do not show CNV values for the selected samples",
      CNVtabsubOnco$nagenes,
      icon = icon("dna")
    )
  })

#### Clustering

tic("load CNV Ploidy.txt file")
CNVploidy <- as.data.frame(read.table(system.file("extdata", "PloidyCNV.txt", package = "PdxAppPackage"), header = TRUE))
toc(log = TRUE)

metaCNV <- data.frame(Sample = unique(CNVploidy$Sample))
rownames(metaCNV) <- metaCNV$Sample
metaCNV$Group1 <- 'a'
metaCNV$Group2 <- 'b'
metaCNV$Sample <- NULL
metadataClustCNV <- reactiveValues(table = metaCNV )

CNVmatrixspread <- reactiveValues(table = NULL)
metadataClustCNV <- reactiveValues(table = NULL )
CnGap <- na.omit(CnGap[1:150,])
#CnGap <- na.omit(CnGap)
#observe({
observeEvent(c(input$sidebartabs,Metadata$table),{
#observeEvent(SamplesInfoR$metadata,{
  #if(input$sidebartabs=="ClusteringCNV"){
  tic("cnv clustering metadata & data")
  metaCNV <-  Metadata$table[!duplicated(rownames(Metadata$table)),cols_annots, drop = F]
  metaCNV$CNV <-  NULL
  metaCNV$CNV_SampleID <- NULL
  metaCNV <- metaCNV[rownames(metaCNV)[rownames(metaCNV) %in% colnames(CnGap)],]
  metaCNV <- metaCNV[-1,] # Cause one line of NAs was introduced
  CNVmatrixspread$table <- CnGap[,colnames(CnGap) %in% rownames(metaCNV), drop = F]
  metadataClustCNV$table <- metaCNV
  toc(log = TRUE)
  if(input$sidebartabs=="ClusteringCNV"){
    print('isnullcnvmatrix')
  if(!is.null(CNVmatrixspread$table)){
  if (nrow(metadataClustCNV$table) > 0 && ncol(CNVmatrixspread$table) >0){
    tic("Calling module cnv")
    CNVclust <- callModule(ClusteringServer,"ClusterCNV",
                           data = CNVmatrixspread, metadata = metadataClustCNV,printRows = FALSE,session = session)
 } else {
showModal(modalDialog(
title = "No CNV data available in your selected data, please go back to sample selection",
actionButton("gobacksplsel1", strong("Go to sample selection tab"),width = "100%"),
easyClose = FALSE,
footer = tagList(
  modalButton("Cancel"))
))
  }
  toc(log = TRUE)
    }
  }
})

observeEvent(input$gobackdeg,{
  newtab <- switch(input$sidebartabs, "PCA" = "DEG","DEG" = "PCA")
  updateTabItems(session, "sidebartabs", newtab)
  removeModal()
})
observeEvent(input$gobacksplsel2,{
  newtab <- switch(input$sidebartabs, "RNAClustering" = "SamplesSelection","RNAClustering" = "DEG")
  updateTabItems(session, "sidebartabs", newtab)
  removeModal()
})
observeEvent(input$gobacksplsel3,{
  newtab <- switch(input$sidebartabs, "CNV" = "SamplesSelection","SamplesSelection" = "CNV")
  updateTabItems(session, "sidebartabs", newtab)
  removeModal()
})
observeEvent(input$gobacksplsel4,{
  newtab <- switch(input$sidebartabs, "PCA" = "SamplesSelection","SamplesSelection" = "PCA")
  updateTabItems(session, "sidebartabs", newtab)
  removeModal()
})
#########" Report section ###########"
# server report editor ---------------------------------------------------------
### yaml generation
# rmd_yaml <- reactive({
#   paste0("---",
#          "\ntitle: '", input$report_title,
#          "'\nauthor: '", input$report_author,
#          "'\ndate: '", Sys.Date(),
#          "'\noutput:\n  html_document:\n    toc: ", input$report_toc, "\n    number_sections: ", input$report_ns, "\n    theme: ", input$report_theme, "\n---\n\n",collapse = "\n")
# })
# 
# 
# ### loading report template
# # update aceEditor module
# observe({
#   # loading rmd report from disk
#   inFile <- system.file("extdata", "reportTemplate.Rmd",package = "PdxAppPackage")
#   isolate({
#     if(!is.null(inFile) && !is.na(inFile)) {
#       rmdfilecontent <- paste0(readLines(inFile),collapse="\n")
#       shinyAce::updateAceEditor(session, "acereport_rmd", value = rmdfilecontent)
#     }
#   })
# })
# 
# ### ace editor options
# observe({
#   autoComplete <- if(input$enableAutocomplete) {
#     if(input$enableLiveCompletion) "live" else "enabled"
#   } else {
#     "disabled"
#   }
#   updateAceEditor(session, "acereport_rmd", autoComplete = autoComplete,theme=input$theme, mode=input$mode)
# })
# 
# #Enable/Disable R code completion
# rmdOb <- aceAutocomplete("acereport_rmd")
# observe({
#   if(input$enableRCompletion) {
#     rmdOb$resume()
#   } else {
#     rmdOb$suspend()
#   }
# })
# 
# ## currently not working as I want with rmarkdown::render, but can leave it like this - the yaml will be taken in the final version only
# output$knitDoc <- renderUI({
#   input$updatepreview_button
#   return(
#     withProgress(
#       message = "Updating the report in the app body",
#       detail = "This can take some time",
#       {
#         # temporarily switch to the temp dir, in case you do not have write
#         # permission to the current working directory
#         owd <- setwd(tempdir())
#         on.exit(setwd(owd))
#         tmp_content <- paste0(rmd_yaml(),input$acereport_rmd,collapse = "\n")
#         incProgress(0.5, detail = "Rendering report...")
#         htmlpreview <- knit2html(text = tmp_content, fragment.only = TRUE, quiet = TRUE)
#         incProgress(1)
#         isolate(HTML(htmlpreview))
#       })
#   )
# })
# 
# ## Download Report
# 
# output$saveRmd <- downloadHandler(
#   filename = function() {
#     if(input$rmd_dl_format == "rmd") {
#       "report.Rmd"
#     } else {
#       "report.html"
#     }
#   },
#   content = function(file) {
#     # knit2html(text = input$rmd, fragment.only = TRUE, quiet = TRUE))
#     tmp_content <-
#       paste0(rmd_yaml(),
#              input$acereport_rmd,collapse = "\n")
#     # input$acereport_rmd
#     if(input$rmd_dl_format == "rmd") {
#       cat(tmp_content,file=file,sep="\n")
#     } else {
#       if(input$rmd_dl_format == "html") {
#         # temporarily switch to the temp dir, in case you do not have write
#         # permission to the current working directory
#         owd <- setwd(tempdir())
#         on.exit(setwd(owd))
#         cat(tmp_content,file="tempreport.Rmd",sep="\n")
#         rmarkdown::render(input = "tempreport.Rmd",
#                           output_file = file,
#                           # fragment.only = TRUE,
#                           quiet = TRUE)
#       }
#     }
# })

###### Save State ############

observeEvent(c(input$init2,
               input$init)
             ,priority =10,ignoreInit = TRUE,{

               cat("save data \n")
               saveState(filename = "/tmp/WorkingEnvironment.rda",
                         Rawdata = Rawdata,
                         Metadata = Metadata,
                         SamplesInfoR = SamplesInfoR,
                         dds = dds ,
                         Model= Model,
                         PCA = PCA,
                         Transformed = Transformed,
                         input = input)

             })


output$exit_and_save <- downloadHandler(
  filename = function() {
    paste0("PDX_rState_",gsub(" ","_",gsub("-","",gsub(":","-",as.character(Sys.time())))),".rda")
  },
  content = function(file) {
    #saveState(filename)
    file.copy(from = "/tmp/WorkingEnvironment.rda", to = file)
    file.remove("/tmp/WorkingEnvironment.rda")
    stopApp("PDX APP closed closed")
  })

observeEvent(c(input$init2),
             ignoreInit = TRUE, {
               shinyjs::runjs("$('#exit_and_save')[0].click();")
             })

observeEvent(c(input$init),
             ignoreInit = TRUE, {
               shinyjs::runjs("$('#state_save_sc')[0].click();")
             })

output$state_save_sc <- downloadHandler(
  filename = function() {
    paste0("PDX_rState_",gsub(" ","_",gsub("-","",gsub(":","-",as.character(Sys.time())))),".rda")
  },
  content = function(file) {
    file.copy(from = "/tmp/WorkingEnvironment.rda", to = file)
    file.remove("/tmp/WorkingEnvironment.rda")
  }
)

## Restore state

observeEvent(input$restore,priority = 10,{

  withProgress(message = 'Loading analysis state', value = 0.5, {

    inFile <- input$restore
    if(!is.null(inFile)) {

      isolate({

        tmpEnv <- new.env()
        load(inFile$datapath, envir=tmpEnv)

        if (exists("SamplesInfoR_r", envir=tmpEnv, inherits=FALSE)) {#
          print("load SamplesInfoR_r")
          SamplesInfoR$metadata <- tmpEnv$SamplesInfoR_r$metadata
        }
        if (exists("Rawdata_r", envir=tmpEnv, inherits=FALSE)) {#
          print("load Rawdata_r")
          Rawdata$table <- tmpEnv$Rawdata_r$sampleplan
        }
        incProgress(0.1)
        if (exists("Metadata_r", envir=tmpEnv, inherits=FALSE)){
          print("load Metadata_r")
          Metadata$table <- tmpEnv$Metadata_r$table
        }
        incProgress(0.1)
        if (exists("r_inputs", envir=tmpEnv, inherits=FALSE)){
          print("load inputs")
          input <- tmpEnv$r_inputs
          lapply(names(input),
                 function(x) session$sendInputMessage(x, list(value = input[[x]]))
          )
          print(input)
        }
        incProgress(0.1)
        if (exists("Model_r", envir=tmpEnv, inherits=FALSE)){
          print("load Model_r")
          Model$contrast <- tmpEnv$Model_r$contrast
        }
        incProgress(0.1)
        if (exists("dds_r", envir=tmpEnv, inherits=FALSE)){
          print("load dds_r")
          dds <- tmpEnv$dds_r
        }
        incProgress(0.1)
        if (exists("PCA_r", envir=tmpEnv, inherits=FALSE)){
          print("load PCA_r")
          PCA <- tmpEnv$PCA_r
        }
        if (exists("Transformed_r", envir=tmpEnv, inherits=FALSE)){
          print("load Transformed_r")
          Transformed$vst <- tmpEnv$Transformed_r*vst
          Transformed$rlog <- tmpEnv$Transformed_r*rlog
          Transformed$tpm <- tmpEnv$Transformed_r*tpm

        }
        incProgress(0.1)
        rm(tmpEnv)
      }) #end of isolate
    }
    setProgress(1)
  })
}) # end of observer

}