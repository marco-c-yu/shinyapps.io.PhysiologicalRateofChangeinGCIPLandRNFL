###########
# Package: Physiological Rate of Change in GCIPL and RNFL
################################################################################
# Copyright (C) 2023  Marco Chak-Yan YU
# 
# This program is under the terms of the GNU General Public License
# as published by the Free Software Foundation, version 2 of the License.
# 
# Redistribution, add, delete or modify are NOT ALLOWED
# WITHOUT AUTHOR'S NOTIFICATION AND PERMISSION.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
################################################################################

library(shiny)
library(DT)
load('rate_models_sces12.RData')
ds.template <- data.template
cns <- colnames(ds.template)
rate.models <- rate.models.sces12
cen <- xs.cen.sces12
ds.template[1,] <- NA
ds.in <- ds.template

ds.in[1:2,colnames(ds.in)!='Female'] <- rbind(cen,cen)
ds.in[1:2,'Female'] <- 0:1
ds.reset <- ds.in

ds <- ds.in[,cns]
ds.out <- ds.in
opt <- c(rownames(rate.models),'band')
cns.ex <- rownames(rate.models)[!rownames(rate.models) %in% opt[opt!='band']]
cns <- c(cns[!cns %in% rownames(rate.models)],cns[cns %in% opt[opt!='band']])
var.band.T <- ('band' %in% opt)
var.band <- 0.95
nrow <- 1

shinyServer(function(input, output) {
  # init
  {
    addResourcePath("www", tempdir())
    
    opt <- c(rownames(rate.models),'band')
    cns.ex <- rownames(rate.models)[!rownames(rate.models) %in% opt[opt!='band']]
    cns <- c(cns[!cns %in% rownames(rate.models)],cns[cns %in% opt[opt!='band']])
    var.band.T <- ('band' %in% opt)
    var.band <- 0.95
    values <- reactiveValues()
    values$ds.template <- ds.template
    values$ds.in <- ds.in
    values$ds <- ds.in[,cns]
    values$ds.out <- ds.out
    output$input_table <- DT::renderDataTable(
      {DT::datatable(values$ds,
                     editable=list(target="all",disable=list(columns=0)),
                     class='cell-border stripe',
                     options=list(searching = FALSE,pageLength=20,lengthMenu=list(c(20,100,-1), c("20",'100',"All")),ordering=FALSE))})
    output$output_table <- DT::renderDataTable(
      {DT::datatable(values$ds.out,
                     editable=FALSE,
                     class='cell-border stripe',
                     options=list(searching = FALSE,pageLength=20,lengthMenu=list(c(20,100,-1), c("20",'100',"All")),ordering=FALSE))})
  }
  
  ####################
  # sidebarPanel
  output$download_template <- {downloadHandler(
    filename = function() {
      paste('input_data_template', '.csv', sep='')
    },
    content=function(file) {
      write.table(values$ds.template,file=file,append=FALSE,row.names=FALSE,na='',sep=',')
    }
  )}
  output$download_output <- {downloadHandler(
    filename = function() {
      paste('estimation_output_', Sys.Date(), '.csv', sep='')
    },
    content=function(file) {
      write.table(values$ds.out,file=file,append=FALSE,row.names=FALSE,na='',sep=',')
    }
  )}
  observeEvent(input$infile,{
    try({
      opt <- input$opt
      y.cns <- cns[cns %in% opt[opt!='band']]
      cns <- c(cns[!cns %in% rownames(rate.models)],cns[cns %in% opt[opt!='band']])
      ds.in <- read.csv(input$infile$datapath,stringsAsFactors=FALSE)
      for (cn in cns) {
        if (!(cn %in% colnames(ds.in))) eval(parse(text=paste('ds.in$',cn,' <- NA',sep='')))
        if (is.factor(ds.in[,cn])) ds.in[,cn] <- as.numeric(as.character(ds.in[,cn]))
      }
      ds <- ds.in[,cns]
      values$ds.in <- ds.in
      values$ds <- values$ds.in[,cns]
    },silent=TRUE)
  })
  observe({
    opt <- input$opt
    cns.ex <- rownames(rate.models)[!rownames(rate.models) %in% opt[opt!='band']]
    cns <- c(cns[!cns %in% rownames(rate.models)],cns[cns %in% opt[opt!='band']])
    values$ds.template <- ds.template[,cns]
    # create empty columns if not currently exist
    for (cn in cns) if (!(cn %in% colnames(values$ds.in))) values$ds.in[,cn] <- NA
    ds.in <- values$ds.in
    values$ds <- ds.in[,cns]
    ds.out <- ds.in
    values$ds.out <- ds.out[,!colnames(ds.out) %in% cns.ex]
  })
  observeEvent(input$run_rate, {
    opt <- input$opt
    cns.ex <- rownames(rate.models)[!rownames(rate.models) %in% opt[opt!='band']]
    cns <- c(cns[!cns %in% rownames(rate.models)],cns[cns %in% opt[opt!='band']])
    var.band.T <- ('band' %in% opt)
    var.band <- input$var.band
    ds.in <- values$ds.in
    ds <- ds.in[,cns]
    
    xs <- (cbind(1,ds))
    colnames(xs)[1] <- 'Constant'
    xs_cen <- xs
    for (cn in names(cen)[names(cen) %in% colnames(xs)]) xs_cen[,cn] <- xs[,cn]-as.numeric(cen[cn])
    xs_cen <- as.matrix(xs_cen)
    beta <- as.matrix(rate.models[,1:(ncol(rate.models)-1)])
    
    y <- rownames(beta)[1]
    rate <- as.data.frame(NULL)[1:nrow(ds),]
    rownames(rate) <- 1:nrow(ds)
    for (y in rownames(beta)[rownames(beta) %in% opt[opt!='band']]) {
      b <- na.omit(beta[y,])
      x <- xs_cen[,colnames(xs_cen) %in% names(b)]
      rate[,paste(y,'_rate',sep='')] <- x %*% b
      if (var.band.T) {
        rate[,paste(y,'_rate_lb',sep='')] <- rate[,paste(y,'_rate',sep='')]+qnorm((1-var.band)/2)*rate.models$sd.dage[rownames(rate.models)==y]
        rate[,paste(y,'_rate_ub',sep='')] <- rate[,paste(y,'_rate',sep='')]+qnorm(1-(1-var.band)/2)*rate.models$sd.dage[rownames(rate.models)==y]
      }
    }
    
    ds.out <- cbind(ds.in,rate)
    # values$ds.out <- ds.out
    values$ds.out <- ds.out[,!colnames(ds.out) %in% cns.ex]
  })
  
  ####################
  # mainPanel
  observeEvent(input$input_table_cell_edit, {
    info = input$input_table_cell_edit
    i = info$row
    j = info$col
    v = info$val

    values$ds <- DT::editData(values$ds,info)
    for (c in 1:ncol(values$ds)) values$ds[,c] <- as.numeric(as.character(values$ds[,c]))
    values$ds.in[,match(colnames(values$ds),colnames(values$ds.in))] <- values$ds
    # values$ds[i,j] = DT::coerceValue(as.numeric(v),values$ds[i,j])
    # values$ds.in[i,colnames(values$ds)[j]] = DT::coerceValue(as.numeric(v),values$ds.in[i,colnames(values$ds)[j]])
  })
  observeEvent(input$data_empty,{
    opt <- input$opt
    cns.ex <- rownames(rate.models)[!rownames(rate.models) %in% opt[opt!='band']]
    cns <- c(cns[!cns %in% rownames(rate.models)],cns[cns %in% opt[opt!='band']])
    ds <- ds.template[,cns]
    ds.in <- ds.template
    values$ds.in <- ds.in
    values$ds <- ds.in[,cns]
  })
  observeEvent(input$data_reset,{
    opt <- input$opt
    cns.ex <- rownames(rate.models)[!rownames(rate.models) %in% opt[opt!='band']]
    cns <- c(cns[!cns %in% rownames(rate.models)],cns[cns %in% opt[opt!='band']])
    ds <- ds.reset[,cns]
    ds.in <- ds.reset
    values$ds.in <- ds.in
    values$ds <- ds.in[,cns]
  })
  observeEvent(input$add_row, {
    opt <- input$opt
    cns.ex <- rownames(rate.models)[!rownames(rate.models) %in% opt[opt!='band']]
    cns <- c(cns[!cns %in% rownames(rate.models)],cns[cns %in% opt[opt!='band']])
    # create empty columns if not currently exist
    for (cn in cns) if (!(cn %in% colnames(values$ds.in))) values$ds.in[,cn] <- NA
    # nrow <- input$nrow
    values$ds.in[(nrow(values$ds.in)+1):(nrow(values$ds.in)+nrow),] <- NA
    values$ds <- values$ds.in[,cns]
    ds.in <- values$ds.in
  })
  observeEvent(input$duplicate_row, {
    opt <- input$opt
    cns.ex <- rownames(rate.models)[!rownames(rate.models) %in% opt[opt!='band']]
    cns <- c(cns[!cns %in% rownames(rate.models)],cns[cns %in% opt[opt!='band']])
    # create empty columns if not currently exist
    for (cn in cns) if (!(cn %in% colnames(values$ds.in))) values$ds.in[,cn] <- NA
    # nrow <- input$nrow
    values$ds.in[(nrow(values$ds.in)+1):(nrow(values$ds.in)+nrow),
                 colnames(values$ds.in) %in% colnames(ds.template)] <- 
      values$ds.in[rep(nrow(values$ds.in),nrow),
                   colnames(values$ds.in) %in% colnames(ds.template)]
    values$ds <- values$ds.in[,cns]
    ds.in <- values$ds.in
  })
  observeEvent(input$delete_row, {
    opt <- input$opt
    cns.ex <- rownames(rate.models)[!rownames(rate.models) %in% opt[opt!='band']]
    cns <- c(cns[!cns %in% rownames(rate.models)],cns[cns %in% opt[opt!='band']])
    # nrow <- input$nrow
    values$ds.in <- values$ds.in[1:(max(1,(nrow(values$ds.in)-nrow))),]
    values$ds <- values$ds.in[,cns]
    ds.in <- values$ds.in
  })
  
})
