#! /usr/bin/env Rscript

# Wrapper CLI to apply a modality classification model to a collection of 
# imaging series

# Input arguments:
# acc_dir - path to directory that contains studyID directory
# model_path - path to model.Rdata
 
# Parse command line arguments    
args=(commandArgs(TRUE))   
for(i in 1:length(args)){
    eval(parse(text=args[i]))
}

pkgs = c('oro.dicom',
         'tidyverse',
         'tidytext',
         'tm',
         'caret')
tmp = lapply(pkgs, function(x) suppressWarnings(suppressMessages(library(x, quietly=TRUE, character.only=TRUE))))

load(model_path)

predict_headers(acc_dir, models, tb_preproc)