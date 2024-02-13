validationModal <- function(msg = "", title = "Validation failed") {
  showModal(modalDialog(p(msg),
                        title = title,
                        footer = tagList(
                          modalButton("Dismiss"),
                          actionButton("returnToInput", "Return To Input Tab")
                        )))

}


###save App state to binary data
saveState <- function(filename,input,Rawdata,Metadata,SamplesInfoR,dds,Model,PCA,
                      Transformed) {
  #saveState <- function(session,filename) {
  isolate({

    names <- c()
    print("save inputs")
    #r_inputs <<- lapply(reactiveValuesToList(input), unclass)
    #r_inputs <- reactiveValuesToList(input)
    r_inputs <<- input
    print("save reactives")
    # #r_reactives <- reactiveValuesToList(reactives)
    if(exists("Rawdata")){Rawdata_r <<- Rawdata;names <- append(names,"Rawdata_r")}
    if(exists("Metadata")){Metadata_r <<- Metadata;names <- append(names,"Metadata_r")}
    if(exists("SamplesInfoR")){SamplesInfoR_r <<- SamplesInfoR;names <- append(names,"SamplesInfoR_r")}
    if(exists("Model")){Model_r <<- Model;names <- append(names,"Model_r")}
    if(exists("dds")){dds_r <<- dds;names <- append(names,"dds_r")}
    if(exists("Transformed")){Transformed_r <<- Transformed;names <- append(names,"Transformed_r")}
    #if(exists("PCA")){PCA_r <<- PCA;names <- append(names,"PCA_r")}
    # save into rda
    print(names)
    print(class(names))
    save(list = names,  file = filename)

  })
}
