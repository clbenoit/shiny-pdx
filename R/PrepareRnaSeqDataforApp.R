# library(readxl)
# library(dplyr)
#
# ### Metadata Excel Sheets
#
# metadata  <- read_excel(path = system.file("extdata", "MetadataV2.xlsx", package = "PdxAppPackage"),
#                                           sheet = "metadata")
#
# MatchRnaSeqIds <- read_excel(path = system.file("extdata", "MetadataV2.xlsx", package = "PdxAppPackage"),
#                              sheet = "SampleSheet_RNA")
# #MatchRnaSeqIds <- MatchRnaSeqIds[,c("RNA_SampleID","Sample_ID")]
# MatchRnaSeqIds$RNA_SampleID <- gsub(" ", "_", MatchRnaSeqIds$RNA_SampleID)
# MatchRnaSeqIds$RNA_SampleID <- gsub("_NA", "", MatchRnaSeqIds$RNA_SampleID)
# MatchRnaSeqIds$RNA_SampleID <- gsub("_p[0-9].*", "", MatchRnaSeqIds$RNA_SampleID)
# MatchRnaSeqIds$RNA_SampleID <- gsub("_$", "", MatchRnaSeqIds$RNA_SampleID)
#
#
# #### Counts matrix ##########
#
# RawRnacountstable <- read.table("/home/clement/Documents/Bio-informatique/Curie/dataPDX/total_pdx_raw_counts_table.csv", sep = ",", header = TRUE,
#                                 row.names = 1, check.names = FALSE)
#
# TPMdata <- read.table("/home/clement/Documents/Bio-informatique/Curie/dataPDX/total_pdx_tpm_counts_table.csv", sep = ",", header = TRUE,
#                       row.names = 1, check.names = FALSE)
#
# sampleDescription <- read.table(file = "/home/clement/Documents/Bio-informatique/Curie/dataPDX/Pdx_total_sampleDescription.txt",
#                               header = FALSE, sep = ",") %>%
#   tidyr::separate(V1,sep = "\\|", into = c("tablename","tomatch"))
#
#
# for (line in 1:nrow(sampleDescription)){
#
#   tomatch  <- sampleDescription[line,"tomatch"]
#   print(tomatch)
#   print(MatchRnaSeqIds[which(MatchRnaSeqIds$Sample_Name == tomatch ),"RNA_SampleID"])
#   if(length(which(MatchRnaSeqIds$Sample_Name == tomatch)) != 0){
#   sampleDescription[line,"newtableId"] <- as.character(MatchRnaSeqIds[which(MatchRnaSeqIds$Sample_Name == tomatch ),"RNA_SampleID"])
#   }
#
# }
# sampleDescription <- na.omit(sampleDescription)
#
#
#
#
# for (col in 1:length(colnames(RawRnacountstable))){
#
#   if(colnames(RawRnacountstable)[col] %in% sampleDescription$tablename){
#   colnames(RawRnacountstable)[col] <- sampleDescription[which(sampleDescription$tablename == as.character(colnames(RawRnacountstable)[col])),"newtableId"]
#   colnames(TPMdata)[col] <- sampleDescription[which(sampleDescription$tablename == as.character(colnames(TPMdata)[col])),"newtableId"]
#   }
#
# }
# RawRnacountstable <- RawRnacountstable[,sampleDescription$newtableId]
# TPMdata <- TPMdata[,sampleDescription$newtableId]
#
#
# head(RawRnacountstable)
# head(TPMdata)
#
# #write.table(RawRnacountstable,"/home/clement/Documents/Bio-informatique/Curie/Gitlab/pdxs/inst/extdata/total_pdx_raw_counts_table.csv", sep = ",", quote = FALSE)
# #write.table(TPMdata,"/home/clement/Documents/Bio-informatique/Curie/Gitlab/pdxs/inst/extdata/total_pdx_tpm_counts_table.csv", sep = ",", quote = FALSE)
#
#
#
# ##########################" Modif Metadata V2 Mutation column ################################""
#
#
# metadata  <- read_excel(path = system.file("extdata", "MetadataV2.xlsx", package = "PdxAppPackage"),
#                         sheet = "metadata")
#
#
# metadata <- metadata %>%
#   tidyr::separate(mutation,sep = ",", into = c("a","b","c","d"))
# metadata$a <- gsub(" ","",metadata$a)
# metadata$b <- gsub(" ","",metadata$b)
# metadata$c <- gsub(" ","",metadata$c)
# metadata$d <- gsub(" ","",metadata$d)
#
#
# genesList <- na.omit(unique(c(unique(metadata$a),unique(metadata$b),unique(metadata$c),unique(metadata$d))))
# genesList <- gsub(" ","",genesList)
#
# for (gene in genesList){
# print(gene)
#
# for (row in 1:nrow(metadata)){
#   if(gene %in% metadata[row,c("a","b","c","d")]){
#     metadata[row,gene] <- "Y"
#   } else {
#     metadata[row,gene] <- 'N'
#   }
# }
# }
#
# metadata[,c("a","b","c","d")] <- NULL
# metadata <- apply(metadata,2,function(x){tidyr::replace_na(x,"")})
#
# View(head(metadata))
#
# #xlsx::write.xlsx(metadata, file = "/home/clement/Documents/Bio-informatique/Curie/Gitlab/pdxs/inst/extdata/Geneseparated.xlsx",
# #           sheetName="metadata", append=FALSE, row.names = FALSE)
#



