#' @export
#' @importFrom shinyAce getAceModes getAceThemes
#' @importFrom shinyAce aceEditor
#' @importFrom shinyWidgets updatePickerInput pickerInput pickerOptions
#' @import shinydashboardPlus
ui_PDX_app <- function(){

  library(shinydashboardPlus)
  # get modes and themes for the ace editor
  modes <- shinyAce::getAceModes()
  themes <- shinyAce::getAceThemes()
  #if (interactive()){
  dashboardPage(skin = "green",
                #shinydashboardPlus::dashboardHeaderPlus(title = "PDX App",
                dashboardHeader(title = "PDX App"#,
                        # dropdownMenu(type = "notifications", badgeStatus = "success",
                        #              messageItem("Task completion",
                        #                          "This is the content of a message.",
                        #                        time = "5 mins" )
                        #             ),
                        # dropdownMenu(
                        #   type = "tasks", icon = icon("cog"), badgeStatus = NULL,
                        #   menuItem(
                        #     text = fileInput(inputId = "restore", accept = ".rda", label = "Restore Previous analysis State",
                        #                                       buttonLabel=list(icon("angle-double-up")))),#),
                        #   notificationItem(
                        #     text = actionButton("init2","Exit App & save",
                        #                         class = "btn_no_border",
                        #                         onclick = "setTimeout(function(){window.close();}, 100); "),
                        #     icon = icon("sign-out"),status = "primary"),
                        #   menuItem(
                        #     text = actionButton("init", "Save State as .rda", icon = icon("download"))
                        #   )
                        # )
                    ),#End of Header
    dashboardSidebar(
      sidebarUserPanel(
             "Institut Curie",
             #image = "https://upload.wikimedia.org/wikipedia/commons/7/77/Logo_Institut_Curie.jpg"
             image = "data:image/jpeg;base64,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"
        ),
      sidebarMenu(id = "sidebartabs",
        menuItem("Samples selection",tabName = "SamplesSelection"),
        menuItem("About selected samples",value="haha",
        menuSubItem("Metadata",tabName ="Metadata"),
        menuSubItem("Drug responses","DrugRes")),
        menuItem("RNAseq", tabName = "RNAseq",
                 menuSubItem("Filtering Counts","DataRNA"),
                 menuSubItem(text = "Clustering",tabName = "RNAClustering"),
                 menuSubItem("PCA","PCA"),
                 menuSubItem("DE analysis",tabName = "DEG")
                 ),
        menuItem("ChiPseq", tabName = "ChiPseq"),
        menuItem("CNV", tabName = "CNV"),
                 #menuSubItem("OncoPrint",tabName = "DataCNV")),
                 #menuSubItem("Clustering",tabName = "ClusteringCNV")),
        menuItem("Data Integration", tabName = "DataIntegration")#,
        #menuItem("Report", tabName = "Report")

      )#end of Menu
    ),#end of Sidebar
    dashboardBody(
      #fluidPage(
      #tags$style(type='text/css', css),
      tags$head(
        tags$style(type='text/css', ".span12 { width: 510px; }"),
        tags$style(type='text/css', ".span17 { width: 510px; height = 2000px }"),
          tags$style(type='text/css', ".span16 { width: 850px; }"),
      ),
      # Removes spin wheels from inputs
      tags$style(HTML("
        input[type=number] {
              -moz-appearance:textfield;
        }
        input[type=number]::{
              -moz-appearance:textfield;
        }
        input[type=number]::-webkit-outer-spin-button,
        input[type=number]::-webkit-inner-spin-button {
              -webkit-appearance: none;
              margin: 0;
        }
    ")),
      fluidPage(
        use_cicerone(),
        fluidRow(
        tabItems(
        tabItem(tabName = "SamplesSelection",
                #tags$style(css),
                useShinyjs(),
                # fluidRow(downloadButton("state_save_sc","Save State as .rda",style = "visibility: hidden;"),
                #          downloadButton("exit_and_save","Save State as .rda",style = "visibility: hidden;")),
                fluidRow(uiOutput("Technofilters")),
                fluidRow(box(title = 'Selected Samples', width = 12, solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE, status = "success",
                    DT::dataTableOutput("SamplesInfo")))
                ),
        tabItem(tabName = "Metadata",
                #fluidRow(uiOutput("TechnoGenBox ")),
                div(id = "Distributions",fluidRow(uiOutput("Distributions")))
                ),
        tabItem(tabName = "DrugRes",
                fluidRow(uiOutput("DrugResponseGenBox")),
                fluidRow(uiOutput("DrugResponseBox"),
                         uiOutput("MutationsBox"))
                ),
        tabItem(tabName = "DataRNA",
              mainPanel(width = 12,
               fluidRow(FilterRNAUI(id = "filtRnaSeq"))
              )
        ),
        tabItem("RNAClustering",
                fluidPage(
                fluidRow(column(width = 12,
                column(width = 6,
                       column(width = 12,pickerInput(inputId = "ClusterData",width = "100%", label = "Select Data for clustering" , 
                            choices = c("Most variant genes","DEA genes","Custom list of genes"),
                            selected = "Most variant genes"))),#),
                column(width = 6,
                           conditionalPanel(
                           condition = "input.ClusterData == 'Most variant genes'",
                           column(width = 10,
                           numericInput("nvariantgenes","Select this number of highest variant genes for clustering",
                                        min = 20,max = 1500,value = 50,width = "100%")),
                           # column(width = 2,actionButton("startCicerone2",label = NULL, icon =icon("info-circle")))
                           ),
                           conditionalPanel(
                             condition = "input.ClusterData == 'Custom list of genes'",
                             column(width = 10,
                             textAreaInput(inputId = "customlist",label = "Enter the gene list",
                                           width = "100%")),
                             column(width = 2,br(),actionButton("startCicerone2",label = NULL, icon =icon("info-circle"))),
                             br(),
                             downloadButton("genelistexample",
                                            width = "100%",
                                            "Download an example gene list")
                            )
                           ),
                )),
                br(),
                # br(),
                # br(),
                ClusteringUI(id = "heatmapID")
                )
        ),
          tabItem("PCA",
                #div(class = "span17",
                fluidPage(
                fluidRow(
                    column(width = 12, pickerInput("dataPCA","Select Data to use for PCA",
                                choices = c(#"Whole TPM table",
                                            "Whole VST table",
                                            #"Filtered TPM table",
                                            "Filtered VST table",
                                            "DE genes (VST data)",
                                            "DE UP genes (VST data)",
                                            "DE DOWN genes (VST data)"), 
                                selected = "Filtered VST table",multiple = FALSE,width = "100%")),
                )
                ), # end of fluidRow
                br(),
                br(),
                fluidRow(
                  DrawPCAUI2("PCA1")
                )
        ),# end if Tabitem RNA SEQ
        tabItem("DEG",
                        br(),
                          DeaPdxUI(id = "DEA")
        ),
        tabItem(tabName = "ChIPseq",
        ),
        #tabItem(tabName = "DataCNV",
        tabItem(tabName = "CNV",
                  br(),
                  fluidRow(column(width = 6,
                                            pickerInput(
                                              "queries_genes",
                                              label = "Select genes",
                                              choices = NULL,
                                              selected = NULL,
                                              multiple = TRUE,
                                              width = "100%",
                                              choicesOpt = NULL,
                                              inline = FALSE,
                                              options = pickerOptions(
                                                actionsBox = TRUE,
                                                title = "Select genes to add",
                                                liveSearch = TRUE,
                                                liveSearchStyle = "contains",
                                              )
                                            )
                                            ),
                                  column(width = 6,
                                         pickerInput(
                                           "queries_samples",
                                           label = "Select Samples",
                                           choices = NULL,
                                           selected = NULL,
                                           multiple = TRUE,
                                           choicesOpt = NULL,
                                           width = "100%",
                                           inline = FALSE,
                                           options = pickerOptions(
                                             actionsBox = TRUE,
                                             title = "Select samples to add",
                                             liveSearch = TRUE,
                                             liveSearchStyle = "contains",
                                           )
                                         )

                                        )
                                  ),
                               # fluidRow(
                                box(title = "OncoPlot",width = 12,
                                    collapsible = TRUE,
                                     collapsed = FALSE,
                                    solidHeader = TRUE,
                                     status = "success",
                                    column(width = 12,
                                           pickerInput("AnnotOnco", label = "Select variable to annotate Oncoprint",
                                                       selected = NULL, multiple = TRUE,
                                                       choices = NULL,
                                                       choicesOpt = NULL,
                                                       inline = FALSE,
                                                       width = "100%",
                                                       options = pickerOptions(
                                                         actionsBox = TRUE,
                                                         title = "Select variables for annotation",
                                                         liveSearch = TRUE,
                                                         liveSearchStyle = "contains",
                                                       ))
                                           ),
                                    fluidRow(
                                      column(width =12,
                                           plotOutput("CNVOnco",width = "100%")
                                           ))
                ),
                                  box(title = "OncoPlot data",width = 12,
                                    collapsible = TRUE,
                                    collapsed = TRUE,
                                    status = "success",
                                    DT::dataTableOutput("CNVsel"),
                                    fluidRow(downloadButton('CNVseldl', label = "Download CNV data")))
                                #)
        ),
      # tabItem(tabName = "ClusteringCNV",
      #         fluidRow(ClusteringUI(id = "ClusterCNV"))
      #         ),
      tabItem(tabName = "DataIntegration",
        )#,
      # tabItem(tabName = "Report",
      #           tabPanel(
      #             "Report Editor",
      #             icon = icon("pencil"),
      #             h1("Report Editor"),
      #             fluidRow(
      #               column(
      #                 width = 6,
      #                 box(
      #                   title = "markdown options", status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = 9,
      #                   radioButtons("rmd_dl_format", label = "Choose Format:", c("HTML" = "html", "R Markdown" = "rmd"), inline = TRUE),
      #                   textInput("report_title", "Title: "),
      #                   textInput("report_author", "Author: "),
      #                   radioButtons("report_toc", "Table of Contents", choices = list("Yes" = "true", "No" = "false")),
      #                   radioButtons("report_ns", "Number sections", choices = list("Yes" = "true", "No" = "false")),
      #                   selectInput("report_theme", "Theme",
      #                               choices = list("Default" = "default", "Cerulean" = "cerulean",
      #                                              "Journal" = "journal", "Flatly" = "flatly",
      #                                              "Readable" = "readable", "Spacelab" = "spacelab",
      #                                              "United" = "united", "Cosmo" = "cosmo")),
      #                   radioButtons("report_echo", "Echo the commands in the output", choices = list("Yes" = "TRUE", "No" = "FALSE")))),
      #               column(
      #                 width = 6,
      #                 box(
      #                   title = "editor options", status = "primary", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, width = 9,
      #                   checkboxInput("enableAutocomplete", "Enable AutoComplete", TRUE),
      #                   conditionalPanel(
      #                     "input.enableAutocomplete",
      #                     wellPanel(
      #                       checkboxInput("enableLiveCompletion", "Live auto completion", TRUE),
      #                       checkboxInput("enableRCompletion", "R code completion", TRUE)
      #                     )
      #                   ),
      # 
      #                   selectInput("mode", "Mode: ", choices=modes, selected="markdown"),
      #                   selectInput("theme", "Theme: ", choices=themes, selected="solarized_light"))
      #               )
      #               # ,
      #               # column( # kept for debugging purposes!
      #               #   width = 6,
      #               #   verbatimTextOutput("loadedRmd")
      #               # )
      #             ),
      #             fluidRow(
      #               column(3,
      #                      actionButton("updatepreview_button", "Update report",class = "btn btn-primary"),p()
      #               ),
      #               column(3, downloadButton("saveRmd", "Generate & Save",class = "btn btn-success"))
      #             ),
      # 
      #             tabBox(
      #               width = NULL,
      #               id="report_tabbox",
      #               tabPanel("Report preview",
      #                        icon = icon("file-text"),
      #                        htmlOutput("knitDoc")
      #               ),
      # 
      #               tabPanel("Edit report",
      #                        icon = icon("pencil-square-o"),
      #                        aceEditor("acereport_rmd", mode="markdown",theme = "solarized_light",autoComplete = "live",
      #                                  value="_Initialization of the_ `PdxAppPackage` _report generation..._",
      #                                  placeholder = "You can enter some code and text in R Markdown format",
      #                                  height="800px"))
      #             )
      #           ),
      #           # ui panel about -------------------------------------------------------
      #           tabPanel(
      #             "About", icon = icon("institution"),
      #             includeMarkdown(system.file("extdata", "about.md",package = "PdxAppPackage")),
      #             hr(),
      #             h4("Session Info"),
      #             verbatimTextOutput("sessioninfo")
      #           )
      #   ) # end of tabBox
        )) # end of fluidp
      )#end of tabitems
  )#end of dashboardbody
)#end of dashboardPage
#} # end of if interactive
} # end of ui
