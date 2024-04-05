
library(BiocManager)
options(repos = BiocManager::repositories())
options(repos = getOption("repos"))
library(shiny)
library("shinydashboard")
library("shinyWidgets")
library("shinyjs")
library("shinyBS")
library(DT)
library(ComplexHeatmap)
library(periscope)
library(readxl)

server <- function(input, output, session) {
  
  # note this is both in ui and server - remember to change both
  type.ns <- reactive(input$NonSynonymousSelection)
  type.s <- reactive(input$SynonymousSelection)
  type.int <- reactive(input$IntronicSelection)
  
  
  output$pdfview <- renderUI({
    tags$iframe(style="height:700px; width:100%", src="ShinyHeatmap_Instructions.pdf")
  })
  
  rv <- reactiveValues(dataset=data.frame("startingvalues"))
  
  observeEvent(input$loadData, {
    shinyjs::hide("downloadAll")
  })
  observeEvent(input$plotButton, {
    shinyjs::show("downloadAll")
  })
  observeEvent(input$plotButton, {
    shinyjs::show("downloadPlot")
  })
  observeEvent(input$plotButton, {
    shinyjs::show("downloadPlotByType")
  })
  
  # Function to read the uploaded Excel file
  readExcelData <- function() {
    
    req(input$file)  # Ensure file is uploaded
    
    resetInputs()

    # Read the Excel file and extract column names
    file <- input$file$datapath
    df <- as.data.frame(read_excel(file))
    col_names <- names(df)
    
    rv$dataset <- df

    return(df)
    
    
  }
  
  
  
  resetInputs <- function() {
    updateSelectInput(session, "column_sample", choices = NULL, selected = NULL)
    updateSelectInput(session, "column_genes", choices = NULL, selected = NULL)
    updateSelectInput(session, "GenePercentage", choices = NULL, selected = NULL)
  }
  
  
  
  updateColumns <- function() {
    req(input$file)  # Ensure file is uploaded
    req(input$loadData)
    
    # Read the Excel file and extract column names
    file <- readExcelData()
    col_names <- names(file)
    
    # Update the choices in selectInput
    updateSelectInput(session, "column_sample", choices = col_names)
    updateSelectInput(session, "column_gene", choices = col_names)
    updateSelectInput(session, "GenePercentage")
    
  }
  
  
  # Load data button action
  observeEvent(input$loadData, {
    resetInputs()
    readExcelData()
    updateColumns()
    output$PlotDataTable <- NULL
  })
  
  
  reactiveData<-reactiveValues() 
  
  
  
  #--------------------------------------- ######## Part 1 - Tables ######### ------------------------------------------------------
  
  
  
  CreateDataset <- function() {
    file <- rv$dataset
    file[, "samplename"] <- as.factor(file[, "samplename"])
    file[, "Gene.refGene"] <- as.factor(file[, "Gene.refGene"])
    heat <- as.data.frame.matrix(table( file[, "samplename"], file[, "Gene.refGene"]))
    heat[heat > 1] <- 1
    reactiveData$RV_All <- heat
    return(heat)
  }
  CreateDatasetSynonymous <- function() {
    file <- rv$dataset
    file[, "samplename"] <- as.factor(file[, "samplename"])
    file[, "Gene.refGene"] <- as.factor(file[, "Gene.refGene"])
    heat <- as.data.frame.matrix(table( file[file$ExonicFunc.refGene %in% input$SynonymousSelection, "samplename"], 
          file[file$ExonicFunc.refGene %in% input$SynonymousSelection, "Gene.refGene"]))
    heat[heat > 1] <- 1
    colnames(heat) <- paste0(colnames(heat), "_S")
    reactiveData$RV_Synonymous <- heat
    return(heat)
  }
  CreateDatasetNonSynonymous <- function() {
    file <- rv$dataset
    file[, "samplename"] <- as.factor(file[, "samplename"])
    file[, "Gene.refGene"] <- as.factor(file[, "Gene.refGene"])
    heat <- as.data.frame.matrix(table( file[file$ExonicFunc.refGene %in% input$NonSynonymousSelection, "samplename"], 
          file[file$ExonicFunc.refGene %in% input$NonSynonymousSelection, "Gene.refGene"]))
    heat[heat > 1] <- 1
    colnames(heat) <- paste0(colnames(heat), "_NS")
    reactiveData$RV_NonSynonymous <- heat
    return(heat)
  }
  CreateDatasetIntronic <- function() {
    file <- rv$dataset
    file[, "samplename"] <- as.factor(file[, "samplename"])
    file[, "Gene.refGene"] <- as.factor(file[, "Gene.refGene"])
    heat <- as.data.frame.matrix(table( file[file$ExonicFunc.refGene %in% input$IntronicSelection, "samplename"], 
          file[file$ExonicFunc.refGene %in% input$IntronicSelection, "Gene.refGene"]))
    heat[heat > 1] <- 1
    colnames(heat) <- paste0(colnames(heat), "_int")
    reactiveData$RV_Intronic <- heat
    return(heat)
  }
  
  
  # Render table - ALL
  observe({
    input$plotButton
    output$PlotDataTable <- renderDT({
      
      req("samplename")  # Ensure column is selected
      req("Gene.refGene")  # Ensure column is selected
      req(input$plotButton) # Ensure button is selected
      
      # Read the Excel file and extract the selected column
      file <- CreateDataset()
      file

    }, 
    selection = 'none', filter = 'none',
    extensions = "FixedColumns",
    options = list(
      autoWidth = TRUE, pageLength = 15, paging = TRUE, searching = TRUE, info = FALSE,
      sort = TRUE, scrollX = TRUE, fixedColumns = list(leftColumns = 1),
      columnDefs = list(list(targets = 0, width = '100px'))
    ))
  })
  
  # Render table - SYNONYMOUS
  observe({
    input$plotButton
    output$PlotDataTableSynonymous <- renderDT({
      
      req("samplename")  # Ensure column is selected
      req("Gene.refGene")  # Ensure column is selected
      req(input$plotButton) # Ensure button is selected
      
      # Read the Excel file and extract the selected column
      file <- CreateDatasetSynonymous()
      file
      
    }, 
    selection = 'none', filter = 'none',
    extensions = "FixedColumns",
    options = list(
      autoWidth = TRUE, pageLength = 15, paging = TRUE, searching = TRUE, info = FALSE,
      sort = TRUE, scrollX = TRUE, fixedColumns = list(leftColumns = 1),
      columnDefs = list(list(targets = 0, width = '100px'))
    ))
  })
  
  # Render table - NON SYNONYMOUS
  observe({
    input$plotButton
    output$PlotDataTableNonSynonymous <- renderDT({
      
      req("samplename")  # Ensure column is selected
      req("Gene.refGene")  # Ensure column is selected
      req(input$plotButton) # Ensure button is selected
      
      # Read the Excel file and extract the selected column
      file <- CreateDatasetNonSynonymous()
      file
      
    }, 
    selection = 'none', filter = 'none',
    extensions = "FixedColumns",
    options = list(
      autoWidth = TRUE, pageLength = 15, paging = TRUE, searching = TRUE, info = FALSE,
      sort = TRUE, scrollX = TRUE, fixedColumns = list(leftColumns = 1),
      columnDefs = list(list(targets = 0, width = '100px'))
    ))
  })
  
  # Render table - INTRONIC
  observe({
    input$plotButton
    output$PlotDataTableIntronic <- renderDT({
      
      req("samplename")  # Ensure column is selected
      req("Gene.refGene")  # Ensure column is selected
      req(input$plotButton) # Ensure button is selected
      
      # Read the Excel file and extract the selected column
      file <- CreateDatasetIntronic()
      file
      
    }, 
    selection = 'none', filter = 'none',
    extensions = "FixedColumns",
    options = list(
      autoWidth = TRUE, pageLength = 15, paging = TRUE, searching = TRUE, info = FALSE,
      sort = TRUE, scrollX = TRUE, fixedColumns = list(leftColumns = 1),
      columnDefs = list(list(targets = 0, width = '100px'))
    ))
  })
  
  # Render table - COMPLETE
  observe({
    input$plotButton
    output$PlotDataTableComplete <- renderDT({
      
      req("samplename")  # Ensure column is selected
      req("Gene.refGene")  # Ensure column is selected
      req(input$plotButton) # Ensure button is selected
      
      # Read the Excel file and extract the selected column
      file.all <- CreateDataset()
      file.s <- CreateDatasetSynonymous()
      file.ns <- CreateDatasetNonSynonymous()
      file.int <- CreateDatasetIntronic()
      
      heat.final <- merge(file.all, file.ns, by="row.names") %>% 
        merge(file.s, by.x="Row.names", by.y="row.names") %>% 
        merge(file.int, by.x="Row.names", by.y="row.names") %>%
        tibble::column_to_rownames("Row.names")
      
      
      # add unmutated patients
      file <- rv$dataset
      samples.missing <- unique(file[, "samplename"][!( file[, "samplename"] %in% row.names(heat.final))])
      heat.missing <- data.frame(matrix(0, nrow=length(samples.missing), ncol=ncol(heat.final)))
      row.names(heat.missing) <- samples.missing
      colnames(heat.missing) <- colnames(heat.final)
      heat.final <- rbind.data.frame(heat.final, heat.missing)
      heat.final <- heat.final[ , order(names(heat.final))]
      reactiveData$RV_Complete <- heat.final
      heat.final
    }, 
    selection = 'none', filter = 'none',
    extensions = "FixedColumns",
    options = list(
      autoWidth = TRUE, pageLength = 15, paging = TRUE, searching = TRUE, info = FALSE,
      sort = TRUE, scrollX = TRUE, fixedColumns = list(leftColumns = 1),
      columnDefs = list(list(targets = 0, width = '100px'))
    ))
  })
  
  
  #--------------------------------------- ######## Part 2 - Plots ######### ------------------------------------------------------
  
  #--------------------------------------- ######## Part 2 part 1 - simple heatmap ######### ------------------------------------------------------
  
  # Plot HEATMAP boring
    PlotHeatmap_reactive <- reactive({
      
      req("samplename")  # Ensure column is selected
      req("Gene.refGene")  # Ensure column is selected
      req(input$plotButton) # Ensure button is selected
      
      # Read the Excel file and extract the selected column
      file <- rv$dataset
      
      dat.old <- file
      
      file <- file[!file$Gene.refGene %in% c("UNMUTATED", "unmutated"),]
      
      if (input$OnlyNonSynonymous == T){file <- file[file$ExonicFunc.refGene %in% input$NonSynonymousSelection,]}
      
      heat <- as.data.frame.matrix(table( file[, "samplename"], file[, "Gene.refGene"]))
      heat[heat > 1] <- 1
      
      heat <- t(heat)
      
      genes.sel <- ((rowSums(heat)/ncol(heat))*100 > as.numeric(as.character(input$GenePercentage)))
      heat <- heat[genes.sel,]
      
      memoSort <- function(M) {
        geneOrder <- sort(rowSums(M), decreasing=TRUE, index.return=TRUE)$ix;
        scoreCol <- function(x) {
          score <- 0;
          for(i in 1:length(x)) {
            if(x[i]) {
              score <- score + 2^(length(x)-i);
            }
          }
          return(score);
        }
        scores <- apply(M[geneOrder, ], 2, scoreCol);
        sampleOrder <- sort(scores, decreasing=TRUE, index.return=TRUE)$ix;
        return(M[geneOrder, sampleOrder]);
      }
      
      heat <- memoSort(heat)
      
      if (input$MutatedPatientsYesNo == T){
        missing <- unique(dat.old$samplename)[! unique(dat.old$samplename) %in% colnames(heat)]
        mat.missing <- data.frame(matrix(0, nrow=nrow(heat), ncol=length(missing)))
        colnames(mat.missing) <- missing
        mat.final <- cbind.data.frame(as.data.frame.matrix(heat), mat.missing)
      }
      else if (input$MutatedPatientsYesNo == F){
        mat.final <- heat
      }
      
      row_ha = rowAnnotation(GenePerc = anno_barplot(rowSums(mat.final, na.rm=T)/nrow(mat.final)), gp = gpar(fill="grey", border=NA, lty = "blank"))
      #column_ha = columnAnnotation("Number of mutations" = anno_barplot(colSums(mat.final, na.rm=T)/ncol(mat.final)))
      
      # barplot number of mutations per patient - top side
      num.muts <- table(file$ExonicFunc.refGene, file$samplename)
      num.muts <- num.muts[, match(colnames(mat.final), colnames(num.muts))]
      missing <- unique(dat.old$samplename)[! unique(dat.old$samplename) %in% colnames(heat)]
      colnames(num.muts)[is.na(colnames(num.muts))] <- missing
      num.muts[is.na(num.muts)] <- 0
      num.muts.input <- num.muts
      num.muts.input[1,] <- colSums(num.muts)
      
      top_ha <- columnAnnotation("Number of \n mutations" = anno_barplot(num.muts.input[1,], border = FALSE, 
  axis_param=list(gp=gpar(fontsize=6)), gp = gpar(fill="grey35", border="grey35", lty = "blank")),
   annotation_name_gp = gpar(fontsize=6), annotation_name_side="left")
    
      Heatmap(mat.final, cluster_rows = F, cluster_columns = F, col = c("grey90", "red4"), 
              border = T, show_heatmap_legend = F, show_column_names = F, show_row_names = T,
              row_names_gp = grid::gpar(fontsize = input$LabelSize),
              right_annotation = row_ha, top_annotation = top_ha)
      
    })
  
  
  output$PlotHeatmap <- renderPlot({
    PlotHeatmap_reactive()
  })
  
  
  
  #--------------------------------------- ######## Part 2 part 2 - heatmap by type ######### ------------------------------------------------------
  
  # Plot HEATMAP by type
  PlotHeatmap_byType_reactive <- reactive({
      
      req("samplename")  # Ensure column is selected
      req("Gene.refGene")  # Ensure column is selected
      req(input$plotButton) # Ensure button is selected
      
      # Read the Excel file and extract the selected column
      file <- rv$dataset
      dat.old <- file
      
      file <- file[!file$Gene.refGene %in% c("UNMUTATED", "unmutated"),]
      
      if (input$OnlyNonSynonymous == T){file <- file[file$ExonicFunc.refGene %in% input$NonSynonymousSelection,]}
      
      dat <- file
      
      mat <- as.data.frame.matrix(table( file[, "samplename"], file[, "Gene.refGene"]))
      mat <- t(mat)
      
      mat.binary <- mat
      mat.binary[mat.binary > 1] <- 1
      
      genes.sel <- ((rowSums(mat.binary)/ncol(mat.binary))*100 > as.numeric(as.character(input$GenePercentage)))
      mat.binary <- mat.binary[genes.sel,]
      
      memoSort <- function(M) {
        geneOrder <- sort(rowSums(M), decreasing=TRUE, index.return=TRUE)$ix;
        scoreCol <- function(x) {
          score <- 0;
          for(i in 1:length(x)) {
            if(x[i]) {
              score <- score + 2^(length(x)-i);
            }
          }
          return(score);
        }
        scores <- apply(M[geneOrder, ], 2, scoreCol);
        sampleOrder <- sort(scores, decreasing=TRUE, index.return=TRUE)$ix;
        return(M[geneOrder, sampleOrder]);
      }
      
      mat.binary.ordered <- memoSort(mat.binary)
      
      mat <- mat[match(row.names(mat.binary.ordered), row.names(mat)),]
      mat <- mat[, match(colnames(mat.binary.ordered), colnames(mat))]

      
      for (i in 1:nrow(mat)){
        for (j in 1:ncol(mat)){
          value <- mat[i,j]
          if (value == 0) {mat[i,j] <- "null"}
          types <- unique(dat$ExonicFunc.refGene[dat$samplename == colnames(mat)[j] & dat$Gene.refGene == row.names(mat)[i]])
          if (length(types) == 1) {mat[i,j] <- types}
          else if (length(types) > 1) {mat[i,j] <- "multiple"}
        }
      }
      
      mat[mat == "null"] <- NA
      
      if (input$MutatedPatientsYesNo == T){
        missing <- unique(dat.old$samplename)[! unique(dat.old$samplename) %in% colnames(mat)]
        mat.missing <- data.frame(matrix(NA, nrow=nrow(mat), ncol=length(missing)))
        colnames(mat.missing) <- missing
        mat.final <- cbind.data.frame(as.data.frame.matrix(mat), mat.missing)
      }
      else if (input$MutatedPatientsYesNo == F){
        mat.final <- mat
      }

      colss <- as.character(pals::polychrome(15))
      colss[1] <- "white"
      names(colss) <- c(NA, c(type.ns, type.s, type.int))
      colss["splicing"] <- "plum"
      colss["frameshift deletion"] <- "firebrick3"
      colss["frameshift insertion"] <- "firebrick4"
      colss["nonframeshift deletion"] <- "lightgoldenrod1"
      colss["nonframeshift insertion"] <- "lightgoldenrod2"
      colss["nonsynonymous SNV"] <- "grey50"
      colss["stopgain"] <- "palegreen1"
      colss["stoploss"] <- "palegreen2"
      colss["startloss"] <- "palegreen3"
      colss["synonymous SNV"] <- "darkgreen"
      colss["intronic"] <- "lightsalmon1"
      colss["UTR3"] <- "lightskyblue1"
      colss["UTR5"] <- "lightskyblue2"
      colss["UTR"] <- "lightskyblue3"
      colss["multiple"] <- "grey20"

                        
      # barplot percentage of patients mutated per gene - right side
      perc.pat <- rowSums(mat.binary.ordered)/ncol(mat.binary.ordered)*100
      row_ha <- rowAnnotation("Percentage(%)" = anno_barplot(perc.pat, border = FALSE,
                              axis_param=list(gp=gpar(fontsize=6), at=c(0,20,40,60), labels=c(0,20,40,60), labels_rot=0)), 
                              annotation_label = NULL, annotation_name_gp = gpar(fontsize=6))
      
      print(input$MutatedPatientsYesNo)
      

                        
      # barplot number of mutations per patient - top side
      num.muts <- table(dat$ExonicFunc.refGene, dat$samplename)
      num.muts <- num.muts[, match(colnames(mat.final), colnames(num.muts))]
      missing <- unique(dat.old$samplename)[! unique(dat.old$samplename) %in% colnames(mat)]
      colnames(num.muts)[is.na(colnames(num.muts))] <- missing
      num.muts[is.na(num.muts)] <- 0
      top_ha <- columnAnnotation("Number of \n mutations" = anno_barplot(t(num.muts), border = FALSE, 
   axis_param=list(gp=gpar(fontsize=6)), gp = gpar(fill=colss[row.names(num.muts)], border=NA, lty = "blank")),
                   annotation_name_gp = gpar(fontsize=6), annotation_name_side="left")
      
      
      # barplot number of mutations per patient - left side
      num.muts.gene <- t(table(dat$ExonicFunc.refGene, dat$Gene.refGene))
      num.muts.gene <- num.muts.gene/rowSums(num.muts.gene)
      num.muts.gene <- num.muts.gene[match(row.names(mat.final), row.names(num.muts.gene)),]
      left_ha <- HeatmapAnnotation("Number of \n mutations" = anno_barplot(num.muts.gene, border = FALSE, 
               axis_param=list(gp=gpar(fontsize=6)), 
               gp = gpar(fill=colss[colnames(num.muts.gene)], border=NA, lty = "blank")),
     annotation_name_gp = gpar(fontsize=6), which = "row")
      
      
                      
      Heatmap(mat.final, cluster_columns = F, cluster_rows = F, col=colss, na_col="white",
              row_names_gp = grid::gpar(fontsize = input$LabelSize),
              show_column_names = FALSE,
              name = "Mutation type", 
              row_names_side = "right",
              right_annotation = row_ha,
              top_annotation = top_ha,
              left_annotation=left_ha)
                      
    })
    
    output$PlotHeatmap_byType <- renderPlot({
      PlotHeatmap_byType_reactive()
    })
    
    
  
    #--------------------------------------- ######## Part 2 part 3 - heatmap by type 3 ways ######### ------------------------------------------------------
    PlotHeatmap_byType3types_reactive <- reactive({
      
      req("samplename")  # Ensure column is selected
      req("Gene.refGene")  # Eninput$IntronicSelectionsure column is selected
      req(input$plotButton) # Ensure button is selected
      observe(input$NonSynonymousSelection)
      observe(input$SynonymousSelection)
      observe(input$IntronicSelection)
      
      # Read the Excel file and extract the selected column
      file <- rv$dataset
      dat.old <- file
      
      file <- file[!file$Gene.refGene %in% c("UNMUTATED", "unmutated"),]
      
      file$ExonicFunc.refGene2 <- file$ExonicFunc.refGene
      file$ExonicFunc.refGene2[file$ExonicFunc.refGene %in% input$NonSynonymousSelection] <- "Non-synonymous"
      file$ExonicFunc.refGene2[file$ExonicFunc.refGene %in% input$SynonymousSelection] <- "Synonymous"
      file$ExonicFunc.refGene2[file$ExonicFunc.refGene %in% input$IntronicSelection] <- "Intronic"
      file$ExonicFunc.refGene <- file$ExonicFunc.refGene2
      
      if (input$OnlyNonSynonymous == T){file <- file[file$ExonicFunc.refGene %in% input$NonSynonymousSelection,]}
      
      dat <- file
      
      mat <- as.data.frame.matrix(table( file[, "samplename"], file[, "Gene.refGene"]))
      mat <- t(mat)
      
      mat.binary <- mat
      mat.binary[mat.binary > 1] <- 1
      
      genes.sel <- ((rowSums(mat.binary)/ncol(mat.binary))*100 > as.numeric(as.character(input$GenePercentage)))
      mat.binary <- mat.binary[genes.sel,]
      
      memoSort <- function(M) {
        geneOrder <- sort(rowSums(M), decreasing=TRUE, index.return=TRUE)$ix;
        scoreCol <- function(x) {
          score <- 0;
          for(i in 1:length(x)) {
            if(x[i]) {
              score <- score + 2^(length(x)-i);
            }
          }
          return(score);
        }
        scores <- apply(M[geneOrder, ], 2, scoreCol);
        sampleOrder <- sort(scores, decreasing=TRUE, index.return=TRUE)$ix;
        return(M[geneOrder, sampleOrder]);
      }
      
      mat.binary.ordered <- memoSort(mat.binary)
      
      mat <- mat[match(row.names(mat.binary.ordered), row.names(mat)),]
      mat <- mat[, match(colnames(mat.binary.ordered), colnames(mat))]
      
      
      for (i in 1:nrow(mat)){
        for (j in 1:ncol(mat)){
          value <- mat[i,j]
          if (value == 0) {mat[i,j] <- "null"}
          types <- unique(dat$ExonicFunc.refGene[dat$samplename == colnames(mat)[j] & dat$Gene.refGene == row.names(mat)[i]])
          if (length(types) == 1) {mat[i,j] <- types}
          else if (length(types) > 1) {mat[i,j] <- "multiple"}
        }
      }
      
      mat[mat == "null"] <- NA
      
      if (input$MutatedPatientsYesNo == T){
        missing <- unique(dat.old$samplename)[! unique(dat.old$samplename) %in% colnames(mat)]
        mat.missing <- data.frame(matrix(NA, nrow=nrow(mat), ncol=length(missing)))
        colnames(mat.missing) <- missing
        mat.final <- cbind.data.frame(as.data.frame.matrix(mat), mat.missing)
      }
      else if (input$MutatedPatientsYesNo == F){
        mat.final <- mat
      }
      
      colss <- as.character(pals::polychrome(5))
      colss[1] <- "white"
      names(colss) <- c(NA, c("Non-synonymous", "Synonymous", "Intronic", "multiple"))
      colss["Non-synonymous"] <- "firebrick4"
      colss["Synonymous"] <- "darkgreen"
      colss["Intronic"] <- "lightsalmon1"
      colss["multiple"] <- "black"
        
      
      # barplot percentage of patients mutated per gene - right side
      perc.pat <- rowSums(mat.binary.ordered)/ncol(mat.binary.ordered)*100
      row_ha <- rowAnnotation("Percentage(%)" = anno_barplot(perc.pat, border = FALSE,
      axis_param=list(gp=gpar(fontsize=6), at=c(0,20,40,60), labels=c(0,20,40,60), labels_rot=0)), 
                  annotation_label = NULL, annotation_name_gp = gpar(fontsize=6))
      
      
      # barplot number of mutations per patient - top side
      num.muts <- table(dat$ExonicFunc.refGene, dat$samplename)
      num.muts <- num.muts[, match(colnames(mat.final), colnames(num.muts))]
      missing <- unique(dat.old$samplename)[! unique(dat.old$samplename) %in% colnames(mat)]
      colnames(num.muts)[is.na(colnames(num.muts))] <- missing
      num.muts[is.na(num.muts)] <- 0
      top_ha <- columnAnnotation("Number of \n mutations" = anno_barplot(t(num.muts), border = FALSE, 
                                axis_param=list(gp=gpar(fontsize=6)), gp = gpar(fill=colss[row.names(num.muts)], border=NA, lty = "blank")),
                                annotation_name_gp = gpar(fontsize=6), annotation_name_side="left")
      
      
      # barplot number of mutations per patient - left side
      num.muts.gene <- t(table(dat$ExonicFunc.refGene, dat$Gene.refGene))
      num.muts.gene <- num.muts.gene/rowSums(num.muts.gene)
      num.muts.gene <- num.muts.gene[match(row.names(mat.final), row.names(num.muts.gene)),]
      left_ha <- HeatmapAnnotation("Number of \n mutations" = anno_barplot(num.muts.gene, border = FALSE, 
               axis_param=list(gp=gpar(fontsize=6)), 
               gp = gpar(fill=colss[colnames(num.muts.gene)], border=NA, lty = "blank")),
                annotation_name_gp = gpar(fontsize=6), which = "row")
      
      
      
      Heatmap(mat.final, cluster_columns = F, cluster_rows = F, col=colss, na_col="white",
              row_names_gp = grid::gpar(fontsize = input$LabelSize),
              show_column_names = FALSE,
              name = "Mutation type", 
              row_names_side = "right",
              right_annotation = row_ha,
              top_annotation = top_ha,
              left_annotation=left_ha)
      
    })
  
    output$PlotHeatmap_byType3types <- renderPlot({
      PlotHeatmap_byType3types_reactive()
    })
  

  
  
  observe({
    input$FilenameDownload
  
  output$downloadAll <- downloadHandler(
    filename = paste0(input$FilenameDownload, "_tables.zip"),
    content = function(fname) {
      fs <- c("PlotDataTableComplete.csv", "PlotDataTableAll.csv", "PlotDataTableSynonymous.csv", "PlotDataTableNonSynonymous.csv", "PlotDataTableIntronic.csv")
      write.csv(reactiveData$RV_Complete, file = "PlotDataTableComplete.csv", sep =",")
      write.csv(reactiveData$RV_All, file = "PlotDataTableAll.csv", sep =",")
      write.csv(reactiveData$RV_Synonymous, file = "PlotDataTableSynonymous.csv", sep =",")
      write.csv(reactiveData$RV_NonSynonymous, file = "PlotDataTableNonSynonymous.csv", sep =",")
      write.csv(reactiveData$RV_Intronic, file = "PlotDataTableIntronic.csv", sep =",")
      zip(zipfile=fname, files=fs)
    },
    contentType = "application/zip")
  })
  
  
  observe({
    input$FilenameDownload
    input$HeatmapWidth
    input$HeatmapHeight
    output$downloadPlot <- downloadHandler(
      filename = paste0(input$FilenameDownload, "_HeatmapStandard.svg"),
      content = function(file) {
        svg(file, width=input$HeatmapWidth, height=input$HeatmapHeight)
        plot(PlotHeatmap_reactive())
        dev.off()
     }
    )
  })
  
  observe({
    input$FilenameDownload
    input$HeatmapWidth
    input$HeatmapHeight
    output$downloadPlotByType <- downloadHandler(
      filename = paste0(input$FilenameDownload, "_HeatmapByType.svg"),
      content = function(file) {
        svg(file, width=input$HeatmapWidth, height=input$HeatmapHeight)
        plot(PlotHeatmap_byType_reactive())
        dev.off()
      }
    )
  })
  
  

}