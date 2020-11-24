
importModuleServer <- function(input, output, session, vals) {
  
  ns = session$ns
  
  #########
  # TABLE #
  #########
  
  
  # helper to add to the sample table
  addToGeneralSampleTable <- function(inputID, id, col2, col3) {
    col1 <- ""
    if (inputID == "files") {
      col1 <- "Files"
    } else if (inputID == "example") {
      col1 <- "Example"
    } else if (inputID == "rds") {
      col1 <- "RDS"
    } else if (inputID == "cellRanger2") {
      col1 <- "Cell Ranger 2"
    } else if (inputID == "cellRanger3") {
      col1 <- "Cell Ranger 3"
    } else if (inputID == "starSolo") {
      col1 <- "STARsolo"
    } else if (inputID == "busTools") {
      col1 <- "BUStools"
    } else if (inputID == "seqc") {
      col1 <- "SEQC"
    } else if (inputID == "optimus") {
      col1 <- "Optimus"
    }
    
    fluidRowStyle <- paste0(paste0("#", ns(id)), "{border-bottom: 1px solid #bababa; padding-top: .9%; padding-bottom: .5%}")
    removeBtnStyle <- paste0(paste0("#", ns(paste0("remove", id))), "{padding-top: 0; padding-bottom: 0;}")
    insertUI(
      selector = paste0("#", ns("newSampleImport")),
      ui = fluidRow(
        id = ns(id),
        tags$style(HTML(paste0(fluidRowStyle, removeBtnStyle))),
        column(3, col1),
        column(3, col2),
        column(3, col3),
        column(3, actionButton(ns(paste0("remove", id)), "X"))
      )
    )
  }
  
  ##################
  # MODAL CREATION #
  ##################
  # modal to import all preprocessed data except for CellRanger data
  importModal <- function(failed=FALSE, needsDir=FALSE) {
    modalDialog(
      h3("Sample Name"),
      textInput(ns("sampleName"), "*This is the name you would like to give your sample."),
      # only some functions need this input
      if (needsDir)
        h3("Sample ID"),
      if (needsDir)
        textInput(ns("sampleID"), "*This name must match your sample's directory name."),
      
      
      h3("Base Directory"),
      directoryInput(ns('directory'), label = 'Choose Directory', value = '~'),
      
      if (failed)
        div(tags$b("Please fill out all the required fields", style = "color: red;")),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton(ns("modalOk"), "OK")
      )
    )
  }
  
  
  # modal to import CellRanger data
  importCRModal <- function() {
    modalDialog(
      h3("Add a Cell Ranger Sample"),
      tags$br(),
      h4("Option 1 - Select a directory containing multiple sample directories (and no other directories)."),
      actionButton(ns("crOpt1"), "Add"),
      tags$br(),
      h4("Option 2 - Select a single sample directory."),
      actionButton(ns("crOpt2"), "Add"),
      tags$br(),
      h4("Option 3 - Select a directory containing your data files (barcodes.tsv, features.tsv, matrix.mtx)."),
      actionButton(ns("crOpt3"), "Add"),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton(ns("crOK"), "OK")
      )
    )
  }
  
  
  # Upload a sample directory (CR) (parent of 'outs' directory)
  importCRSDir <- function(failed = FALSE) {
    modalDialog(
      h3("Sample Directory"),
      directoryInput(ns('sDirectory'), label = 'Choose Directory', value = '~'),
      h3("Sample Name"),
      h5("If you do not provide an alternate sample name, the sample name will be set to the sample directory name."),
      textInput(ns("sSampleID"), ""),
      
      if (failed)
        div(tags$b("Please fill out all the required fields", style = "color: red;")),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton(ns("SDirOK"), "OK")
      )
    )
  }
  
  
  # Upload a data directory (CR) (parent of 'data files')
  importCRDDir <- function(failed = FALSE) {
    modalDialog(
      h3("Data Directory"),
      directoryInput(ns('directory'), label = 'Choose Directory', value = '~'),
      h3("Sample Name"),
      
      textInput(ns("dSampleID"), "*This field is mandatory when uploading a data directory"),
      
      if (failed)
        div(tags$b("Please fill out all the required fields", style = "color: red;")),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton(ns("DDirOK"), "OK")
      )
    )
  }
  
  
  # Upload a base directory (CR) (parent of possibly multiple sample directories)
  importCRBDir <- function(failed = FALSE) {
    modalDialog(
      h3("Base Directory"),
      directoryInput(ns('bDirectory'), label = 'Choose Directory', value = '~'),
      wellPanel(h5("*For any sample names that you do not provide, the sample name will be set to the sample directory name.")),
      
      tags$div(id = ns("bDirTable")),
      
      if (failed)
        div(tags$b("Please fill out all the required fields", style = "color: red;")),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton(ns("BDirOK"), "OK")
      )
    )
  }

  # Components for uploading directories if user is importing from a preprocessing step
  volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), shinyFiles::getVolumes()())
  
  sample <- reactive(input$sample)
  output$sample <- renderText({
    shinyFiles::parseDirPath(volumes, sample())
  })
  sampleFile <- reactive(input$sampleFile)
  output$sampleFile <- renderText({
    shinyFiles::parseFilePaths(volumes, sampleFile())$datapath
  })
  output$base = renderText({
    readDirectoryInput(session, 'directory')
  })
  
  allImportEntries <- reactiveValues(samples=list(), id_count=0)
  
  
  # see https://github.com/wleepang/shiny-directory-input
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$directory
    },
    handlerExpr = {
      if (input$directory > 0) {
        # condition prevents handler execution on initial app launch
        path = choose.dir(default = readDirectoryInput(session, 'directory'),
                          caption="Choose a directory")
        updateDirectoryInput(session, 'directory', value = path)
      }
    }
  )
  
  # see https://github.com/wleepang/shiny-directory-input
  # for sample directory modal
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$sDirectory
    },
    handlerExpr = {
      if (input$sDirectory > 0) {
        # condition prevents handler execution on initial app launch
        path = choose.dir(default = readDirectoryInput(session, 'sDirectory'),
                          caption="Choose a directory")
        updateDirectoryInput(session, 'sDirectory', value = path)
        if (!is.na(path)) {
          updateTextInput(session, "sSampleID", value = basename(path))
        }
      }
    }
  )
  
  # event listener for the base directory modal (need to populate table for sample names)
  # see https://github.com/wleepang/shiny-directory-input
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$bDirectory
    },
    handlerExpr = {
      if (input$bDirectory > 0) {
        # condition prevents handler execution on initial app launch
        path = choose.dir(default = readDirectoryInput(session, 'bDirectory'),
                          caption="Choose a directory")
        updateDirectoryInput(session, 'bDirectory', value = path)
        # clear the previous table of sample names
        prevPath <- readDirectoryInput(session, 'bDirectory')
        count <- 0
        for (prev in list.dirs(prevPath, recursive = FALSE)) {
          count <- count+1
          removeUI(
            selector = paste0(c("#", ns("sampleRow"), count))
          )
        }
        # create a new table for the selected directory
        count <- 0
        if (!is.na(path)) {
          counts <- vector()
          for (sample in list.dirs(path, recursive = FALSE)) {
            count <- count+1
            counts <- c(counts, count)
            insertUI(
              # selector = "#bDirTable",
              selector = paste0("#", ns("bDirTable")),
              ui = fluidRow(
                id = paste0(ns("sampleRow"), count),
                column(6, basename(sample)),
                column(6, textAreaInput(paste0(ns("sampleName"), count), "Sample Name", resize = "none", value = basename(sample)))
              )
            )
          }
        }
      }
    }
  )
  
  ##############
  # ADD SAMPLE #
  ##############
  observeEvent(input$addCR2Sample, {
    showModal(importCRModal())
  })
  observeEvent(input$crOpt1, {
    removeModal()
    showModal(importCRBDir())
  })
  observeEvent(input$crOpt2, {
    removeModal()
    showModal(importCRSDir())
  })
  observeEvent(input$crOpt3, {
    removeModal()
    showModal(importCRDDir())
  })
  observeEvent(input$addCR3Sample, {
    showModal(importCRModal())
  })
  observeEvent(input$addSSSample, {
    showModal(importModal())
  })
  observeEvent(input$addBUSSample, {
    showModal(importModal())
  })
  observeEvent(input$addSEQSample, {
    showModal(importModal(needsDir = TRUE))
  })
  observeEvent(input$addOptSample, {
    showModal(importModal())
  })
  
  
  # event listener for "Clear All Samples" button
  observeEvent(input$clearAllImport, {
    for (entry in allImportEntries$samples) {
      removeUI(selector = paste0("#", ns(entry$id)))
    }
    allImportEntries$samples <- list()
  })
  
  #####################
  # CR MODAL HANDLERS #
  #####################
  # sample directory
  observeEvent(input$SDirOK, {
    samplePath <- readDirectoryInput(session, 'sDirectory')
    # make sure a directory is selected
    if (identical(samplePath, character(0))) {
      showModal(importCRSDir(failed = TRUE))
    } else {
      # add the files to the appropriate reactiveValues
      if (input$algoChoice == "cellRanger2") {
        id <- paste0("snewSampleCR2", allImportEntries$id_count)
        entry <- list(type="cellRanger2", id=id, params=list(cellRangerDirs = dirname(samplePath), sampleDirs = basename(samplePath), sampleNames = input$sSampleID))
        allImportEntries$samples <- c(allImportEntries$samples, list(entry))
        allImportEntries$id_count <- allImportEntries$id_count + 1
      } else {
        id <- paste0("snewSampleCR3", allImportEntries$id_count)
        entry <- list(type="cellRanger3", id=id, params=list(cellRangerDirs = paste0(dirname(samplePath), "/"), sampleDirs = basename(samplePath), sampleNames = input$sSampleID))
        allImportEntries$samples <- c(allImportEntries$samples, list(entry))
        allImportEntries$id_count <- allImportEntries$id_count + 1
      }
      # add new row to table
      addToGeneralSampleTable(input$algoChoice, id, samplePath, input$sSampleID)
      # handler to remove the sample that was just added
      observeEvent(input[[paste0("remove", id)]],{
        removeUI(
          selector = paste0("#", ns(id))
        )
        toRemove <- vector()
        for (entry in allImportEntries$samples) {
          if (entry$id == id) {
            toRemove <- c(toRemove, FALSE)
          } else {
            toRemove <- c(toRemove, TRUE)
          }
        }
        allImportEntries$samples <- allImportEntries$samples[toRemove]
      })
      removeModal()
    }
  })
  
  # data directory
  observeEvent(input$DDirOK, {
    dataPath <- readDirectoryInput(session, 'directory')
    if ((!nzchar(input$dSampleID)) || (identical(dataPath, character(0)))) {
      showModal(importCRDDir(failed = TRUE))
    } else {
      if (input$algoChoice == "cellRanger2") {
        id <- paste0("dnewSampleCR2", allImportEntries$id_count)
        entry <- list(type="cellRanger2", id=id, params=list(dataDir = dataPath, sampleName = input$dSampleID))
        allImportEntries$samples <- c(allImportEntries$samples, list(entry))
        allImportEntries$id_count <- allImportEntries$id_count + 1
      } else {
        id <- paste0("dnewSampleCR3", allImportEntries$id_count)
        entry <- list(type="cellRanger3", id=id, params=list(dataDir = dataPath, sampleName = input$dSampleID))
        allImportEntries$samples <- c(allImportEntries$samples, list(entry))
        allImportEntries$id_count <- allImportEntries$id_count + 1
      }
      # add new row to table
      addToGeneralSampleTable(input$algoChoice, id, dataPath, input$dSampleID)
      observeEvent(input[[paste0("remove", id)]],{
        removeUI(
          selector = paste0("#", ns(id))
        )
        toRemove <- vector()
        for (entry in allImportEntries$samples) {
          if (entry$id == id) {
            toRemove <- c(toRemove, FALSE)
          } else {
            toRemove <- c(toRemove, TRUE)
          }
        }
        allImportEntries$samples <- allImportEntries$samples[toRemove]
      })
      removeModal()
    }
  })
  
  # base directory
  observeEvent(input$BDirOK, {
    basePath <- readDirectoryInput(session, 'bDirectory')
    # if the user doesn't specify a base directory, show the modal again with the warning message
    if (identical(basePath, character(0))) {
      showModal(importCRBDir(failed = TRUE))
    } else {
      allDirs <- list.dirs(basePath, recursive = FALSE)
      # if we are adding a new CellRangerV2 sample
      if (input$algoChoice == "cellRanger2") {
        allUI <- vector()
        allIDs <- vector()
        count <- 0
        for (sample in allDirs) {
          count <- count + 1
          name <- input[[paste0("sampleName", count)]]
          if (!nzchar(name)) {
            name <- basename(sample)
          }
          id <- paste0("bnewSampleCR2", allImportEntries$id_count)
          entry <- list(type="cellRanger2", id=id, params=list(cellRangerDirs = substr(basePath, 1, nchar(basePath)-1), sampleDirs = basename(sample), sampleNames = name))
          allImportEntries$samples <- c(allImportEntries$samples, list(entry))
          fluidRowStyle <- paste0(paste0("#", id), "{border-bottom: 1px solid #bababa; padding-top: .9%; padding-bottom: .5%}")
          removeBtnStyle <- paste0(paste0("#remove", id), "{padding-top: 0; padding-bottom: 0;}")
          ui_i <- list(id=id, inputId=input$algoChoice, col2=basePath, col3=name)
          allImportEntries$id_count <- allImportEntries$id_count + 1
          allUI <- c(allUI, list(ui_i))
          allIDs <- c(allIDs, id)
        }
      } else { # if we are adding a new CellRangerV3 sample
        allUI <- vector()
        allIDs <- vector()
        count <- 0
        for (sample in allDirs) {
          count <- count + 1
          name <- input[[paste0("sampleName", count)]]
          if (!nzchar(name)) {
            name <- basename(sample)
          }
          id <- paste0("bnewSampleCR3", allImportEntries$id_count)
          entry <- list(type="cellRanger3", id=id, params=list(cellRangerDirs = substr(basePath, 1, nchar(basePath)-1), sampleDirs = basename(sample), sampleNames = name))
          allImportEntries$samples <- c(allImportEntries$samples, list(entry))
          fluidRowStyle <- paste0(paste0("#", id), "{border-bottom: 1px solid #bababa; padding-top: .9%; padding-bottom: .5%}")
          removeBtnStyle <- paste0(paste0("#remove", id), "{padding-top: 0; padding-bottom: 0;}")
          ui_i <- list(id=id, inputId=input$algoChoice, col2=basePath, col3=name)
          allImportEntries$id_count <- allImportEntries$id_count + 1
          allUI <- c(allUI, list(ui_i))
          allIDs <- c(allIDs, id)
        }
      }
      # insert all the new sample rows
      for (ui in allUI) {
        addToGeneralSampleTable(ui$inputId, ui$id, ui$col2, ui$col3)
      }
      
      # create event handlers for all the remove buttons
      # from: https://stackoverflow.com/questions/40038749/r-shiny-how-to-write-loop-for-observeevent
      lapply(
        X = allIDs,
        FUN = function(id_i){
          observeEvent(input[[paste0("remove", id_i)]], {
            removeUI(
              selector = paste0("#", ns(id_i))
            )
            toRemove <- vector()
            for (entry in allImportEntries$samples) {
              if (entry$id == id_i) {
                toRemove <- c(toRemove, FALSE)
              } else {
                toRemove <- c(toRemove, TRUE)
              }
            }
            allImportEntries$samples <- allImportEntries$samples[toRemove]
          })
        }
      )
      removeModal()
    }
  })
  
  #########################
  # PERPROC MODAL HANDLER #
  #########################
  # event handler for pressing OK on the import modal
  observeEvent(input$modalOk, {
    samplePath <- shinyFiles::parseDirPath(volumes, input$sample)
    basePath <- readDirectoryInput(session, 'directory')
    curFiles <- list()
    if ((!nzchar(input$sampleName)) || (identical(basePath, character(0)))) {
      showModal(importModal(failed = TRUE))
    } else {
      entry <- list()
      if (input$algoChoice == "starSolo") {
        id <- paste0("newSampleSS", allImportEntries$id_count)
        entry <- list(type="starSolo", id = id, params=list(STARsoloDirs = basePath, samples = input$sampleName))
        allImportEntries$samples <- c(allImportEntries$samples, list(entry))
        allImportEntries$id_count <- allImportEntries$id_count+1
      } else if (input$algoChoice == "busTools") {
        id <- paste0("newSampleBUS", allImportEntries$id_count)
        entry <- list(type="busTools", id = id, params=list(BUStoolsDirs = substr(basePath, 1, nchar(basePath)-1), samples = input$sampleName))
        allImportEntries$samples <- c(allImportEntries$samples, list(entry))
        allImportEntries$id_count <- allImportEntries$id_count+1
      } else if (input$algoChoice == "seqc") {
        id <- paste0("newSampleSEQ", allImportEntries$id_count)
        entry <- list(type="seqc", id = id, params=list(seqcDirs = basePath, prefix = input$sampleID, samples = input$sampleName))
        updateTextInput(session, "sampleID", value = "")
        allImportEntries$samples <- c(allImportEntries$samples, list(entry))
        allImportEntries$id_count <- allImportEntries$id_count+1
      } else if (input$algoChoice == "optimus") {
        id <- paste0("newSampleOpt", allImportEntries$id_count)
        entry <- list(type="optimus", id = id, params=list(OptimusDirs = basePath, samples = input$sampleName))
        allImportEntries$samples <- c(allImportEntries$samples, list(entry))
        allImportEntries$id_count <- allImportEntries$id_count+1
      }
      addToGeneralSampleTable(input$algoChoice, id, basePath, input$sampleName)
      observeEvent(input[[paste0("remove", id)]],{
        removeUI(
          selector = paste0("#", ns(id))
        )
        toRemove <- vector()
        for (entry in allImportEntries$samples) {
          if (entry$id == id) {
            toRemove <- c(toRemove, FALSE)
          } else {
            toRemove <- c(toRemove, TRUE)
          }
        }
        allImportEntries$samples <- allImportEntries$samples[toRemove]
      })
      removeModal()
    }
  })
  
  ########################
  # OTHER IMPORT OPTIONS #
  ########################
  # Event handler to import a file input
  observeEvent(input$addFilesImport, {
    id <- paste0("newSampleFiles", allImportEntries$id_count)
    entry <- list(type="files", id = id, params=list(assayFile = input$countsfile$datapath, annotFile = input$annotFile$datapath,
                                                     featureFile = input$featureFile$datapath, assayName = input$inputAssayType))
    allImportEntries$samples <- c(allImportEntries$samples, list(entry))
    allImportEntries$id_count <- allImportEntries$id_count+1
    assayFileCol <- ""
    annotFileCol <- ""
    featureFileCol <- ""
    if (!is.null(input$countsfile$datapath)) {
      assayFileCol <- paste0("Assay: ", input$countsfile$datapath)
    }
    if (!is.null(input$annotFile$datapath)) {
      annotFileCol <- paste0("Annotation: ", input$annotFile$datapath)
    }
    if (!is.null(input$featureFile$datapath)) {
      featureFileCol <- paste0("Features: ", input$featureFile$datapath)
    }
    
    locCol <- paste(c(assayFileCol, annotFileCol, featureFileCol), collapse = "\n")
    
    addToGeneralSampleTable("files", id, locCol, input$inputAssayType)
    
    observeEvent(input[[paste0("remove", id)]],{
      removeUI(
        selector = paste0("#", ns(id))
      )
      toRemove <- vector()
      for (entry in allImportEntries$samples) {
        if (entry$id == id) {
          toRemove <- c(toRemove, FALSE)
        } else {
          toRemove <- c(toRemove, TRUE)
        }
      }
      allImportEntries$samples <- allImportEntries$samples[toRemove]
    })
  })
  
  # Event handler to import an example input
  observeEvent(input$addExampleImport, {
    id <- paste0("newSampleExample", allImportEntries$id_count)
    entry <- list(type="example", id = id, params=list(dataset = input$selectExampleData))
    allImportEntries$samples <- c(allImportEntries$samples, list(entry))
    allImportEntries$id_count <- allImportEntries$id_count+1
    
    scRNAseqDatasets <- c("fluidigm_pollen", "allen_tasic")
    tenxPbmcDatasets <- c("pbmc3k", "pbmc4k", "pbmc6k", "pbmc8k", "pbmc33k", "pbmc68k")
    locCol <- ""
    if (input$selectExampleData %in% scRNAseqDatasets) {
      locCol <- "scRNA"
    } else {
      locCol <- "TENx"
    }
    
    addToGeneralSampleTable("example", id, locCol, input$selectExampleData)
    
    observeEvent(input[[paste0("remove", id)]], {
      removeUI(
        selector = paste0("#", ns(id))
      )
      toRemove <- vector()
      for (entry in allImportEntries$samples) {
        if (entry$id == id) {
          toRemove <- c(toRemove, FALSE)
        } else {
          toRemove <- c(toRemove, TRUE)
        }
      }
      allImportEntries$samples <- allImportEntries$samples[toRemove]
    })
  })
  
  # Event handler to import an RDS input
  observeEvent(input$addRDSImport, {
    id <- paste0("newSampleRDS", allImportEntries$id_count)
    entry <- list(type="rds", id = id, params=list(rdsFile=input$rdsFile$datapath))
    allImportEntries$samples <- c(allImportEntries$samples, list(entry))
    allImportEntries$id_count <- allImportEntries$id_count+1
    
    addToGeneralSampleTable("rds", id, input$rdsFile$datapath, "")
    
    observeEvent(input[[paste0("remove", id)]],{
      removeUI(
        selector = paste0("#", ns(id))
      )
      toRemove <- vector()
      for (entry in allImportEntries$samples) {
        if (entry$id == id) {
          toRemove <- c(toRemove, FALSE)
        } else {
          toRemove <- c(toRemove, TRUE)
        }
      }
      allImportEntries$samples <- allImportEntries$samples[toRemove]
    })
  })
  
  ##########
  # UPLOAD #
  ##########
  # Event handler for "Upload" button on import page
  observeEvent(input$uploadData, {
    withBusyIndicatorServer(ns("uploadData"), {
      if (length(allImportEntries$samples) == 0) {
        stop("You have not selected any samples to import.")
      }
      sceObj <- importMultipleSources(allImportEntries)
      if (input$combineSCEChoice == "addToExistingSCE") {
        if(!is.null(vals$original)) {
          sceList <- list(vals$original, sceObj)
          vals$original <- combineSCE(sceList = sceList,
                                      by.r = NULL,
                                      by.c = Reduce(intersect, lapply(sceList, function(x) { colnames(colData(x))})),
                                      combined = TRUE)
        } else {
          vals$original <- sceObj
        }
      } else {
        vals$original <- sceObj
      }
      
      # clear table and empty reactive
      for (entry in allImportEntries$samples) {
        removeUI(selector = paste0("#", ns(entry$id)))
      }
      allImportEntries$samples <- list()
    })
  })
  
  #################
  # SUMMARY TABLE #
  #################
  output$summarycontents <- renderTable({
    req(vals$counts)
    
    # Setting 'useAssay=NULL' assumes that the first assay is the one to count
    singleCellTK::summarizeSCE(inSCE = vals$counts,
                               useAssay = NULL,
                               sampleVariableName = "sample")
  }, striped = TRUE, border = TRUE, align = "c", spacing = "l")
  
  return(reactive({vals}))
  
}

