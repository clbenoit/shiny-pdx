rm(list=ls())
library(dplyr)
DIR <- "/home/clement/Downloads/extdata/XENO_CNV_data"

CNV_info_table <- data.frame(
  segCN = list.files(path = DIR, recursive = TRUE, pattern = "*segCN.csv",
                     full.names = TRUE),
  genes = list.files(path = DIR, recursive = TRUE, pattern = "*genes.csv",
                     full.names = TRUE)
  )


CNV_info_table <- CNV_info_table %>%
mutate(Techno = case_when(grepl('CytoSHD',dirname(as.character(CNV_info_table$genes)), fixed = TRUE, ignore.case = TRUE) ~ "Cyto",
                          grepl('hSnp6',dirname(as.character(CNV_info_table$genes)), fixed = TRUE, ignore.case = TRUE) ~ "SNP6")
)
#
#
CNV_info_table$names <- gsub("_s1h1_CytoSHD","",gsub(".*/","",dirname(as.character(CNV_info_table$genes))))
CNV_info_table$names <- gsub("_s1h1_hSnp6","",CNV_info_table$names)

# # print("duplicated in projetct CNV")
# ### On a remoove 1006771 et 2000924 car créent duplicats avec 2000933
# # CNV_info_table$names[duplicated(CNV_info_table$names)]#
duplicated <- CNV_info_table[which(CNV_info_table$names %in% CNV_info_table$names[duplicated(CNV_info_table$names)]),]
duplicated <- duplicated[order(duplicated$names),]
# # write.table(duplicated,file = "/home/clement/Documents/Bio-informatique/Curie/Gitlab/pdxs/inst/extdata/duplicated_CNV_per_genes", quote = FALSE, sep = ",",row.names = FALSE)
# # # #
joined <- data.frame()
for (sample in 1:nrow(CNV_info_table)){
#
   segCN <- read.table(as.character(CNV_info_table$segCN[sample])
                       ,sep = ",", header = TRUE) %>%
     mutate(segName = name)

   head(segCN)
#
   genes <- read.table(as.character(CNV_info_table$genes[sample])
                       ,sep = ",", header = TRUE) %>%
     mutate(segName = segCN,sample = CNV_info_table$names[sample])

   head(genes)

   final <- left_join(genes,segCN, by = "segName") %>%
     select (c("SYMBOL","segCN","GNL","CnGap",'sample'))

   colnames(final) <- c("GENE","segCN","GNL","CnGap",'sample')
#
   head(final)

   joined <- rbind(joined,final)

 }
# #
# # #
# # #
# # # print(head(joined))
# write.table(joined,file = "/home/clement/Documents/Bio-informatique/Curie/Gitlab/pdxs/inst/extdata/CNV_per_genes_table", quote = FALSE, sep = ",")
# CNVtabmatrix <- joined
# GeneList <- as.character(unique(CNVtabmatrix$GENE))
# SampleList <- as.character(unique(CNVtabmatrix$sample))
#
# save(list = c("CNVtabmatrix","GeneList","SampleList"),file = "/home/clement/Documents/Bio-informatique/Curie/Gitlab/pdxs/inst/extdata/CNV_per_genes_table.RDA")
# #
# # library(ggplot2)
# # library(ggiraph)
# # library(cowplot)
# # inFile <- system.file("extdata", "CNV_per_genes_table",package = "PdxAppPackage")
# # #CNVtab <- reactiveValues(matrix = read.table(inFile, sep = ",", header = TRUE))
# # CNVtabmatrix <- read.table(inFile, sep = ",", header = TRUE)
# #
# # # example Pierre matrix
# # d <- data.frame(x = rep(letters[1:10], 15), y = rep(letters[1:15], each
# #                                                     = 10), alt = sample(c("N", "G", "L"), size = 150, replace = TRUE))
# # d <- filter(d, alt != "N")
# #
# # # queries_genes <- c("OR4F5","OR4F16","OR4F29","OR4F3")
# # # queries_samples <- c("BC41_ADN0036", "BC61_ADN0037","BC61_ADN0051","BC61_ADN0042")
# # #
# # CNVtabmatrix_sub <- CNVtabmatrix %>%
# #   select(c("GENE","sample","GNL","CnGap")) %>%
# #   filter(GENE %in% queries_genes) %>%
# #   filter(sample %in% queries_samples)
# #
# # nrow(CNVtabmatrix_sub)
# #
#
# ################### Check data #####################
library(readxl)
## Metadata
SamplesInfo   <- as.data.frame(read_excel(path = system.file("extdata", "MetadataV2.xlsx", package = "PdxAppPackage"),
                                          sheet = "metadata"))
MetaSamples <- unique(SamplesInfo$PDX_model)
print("Metalength")
print(length(MetaSamples))
## CNV
CNV <- read.table("/home/clement/Documents/Bio-informatique/Curie/Gitlab/pdxs/inst/extdata/CNV_per_genes_table",header = TRUE, sep = ",")
CNVsamples <- unique(CNV$sample)
print("CNVlength")
print(length(CNVsamples))
# inFile <- system.file("extdata", "CNV_per_genes_table.RDA",package = "PdxAppPackage")
# load(inFile)
# head(CNVtabmatrix)

## RNA

Raw <- read.table("/home/clement/Documents/Bio-informatique/Curie/Gitlab/pdxs/inst/extdata/total_pdx_raw_counts_table.csv", sep = ",", header = TRUE)
TPM <- read.table("/home/clement/Documents/Bio-informatique/Curie/Gitlab/pdxs/inst/extdata/total_pdx_tpm_counts_table.csv", sep = ",", header = TRUE)
RNAsamples <- unique(colnames(Raw))
print("RNAlength")
print(length(RNAsamples))


CNVintoMeta <- CNVsamples[CNVsamples %in% SamplesInfo$CNV_SampleID]
print('CNVintoMeta')
print(length(CNVintoMeta))
CNVnotinMeta <- CNVsamples[!(CNVsamples %in% SamplesInfo$CNV_SampleID)]
print("CNVnotinMeta")
print(length(CNVnotinMeta))
CNVnotindataset <- SamplesInfo$CNV_SampleID[!(SamplesInfo$CNV_SampleID %in% CNVsamples)]
print("CNVnotindatatset")
print(length(unique(na.omit(CNVnotindataset))))

RNAintoMeta <- colnames(Raw)[colnames(Raw) %in% SamplesInfo$PDX_model]
print("RNAintoMEta")
print(length(RNAintoMeta))
#Mutualgroups



##### Faire cyto SHD vs SNP6 pour les doublons.


