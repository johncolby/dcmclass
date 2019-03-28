library(oro.dicom)
library(tidyverse)
library(tidytext)
library(tm)
library(caret)

# Function to extract a single DICOM header
get_hdr <- function(path){
  feature_names = c('SeriesNumber', 'CodeValue', 'StationName', 'SeriesDescription', 'ScanningSequence', 'ScanOptions', 'MRAcquisitionType', 'SliceThickness', 'RepetitionTime', 'EchoTime', 'MagneticFieldStrength', 'SpacingBetweenSlices', 'FlipAngle', 'VariableFlipAngleFlag', 'ImagesInAcquisition', 'Rows', 'Columns')

  as_tibble(readDICOMFile(list.files(path, full.names = TRUE)[1], skipSequence=TRUE, pixelData = FALSE)$hdr) %>%
    select(name, value) %>%
    filter(name %in% feature_names) %>%
    distinct(name, .keep_all = TRUE)
}

# Function to load all headers from a char vector of multiple studies
load_study_headers <- function(acc_dirs) {
  tibble(AccessionNumber = acc_dirs) %>%
    mutate(series = map(AccessionNumber, ~list.files(list.dirs(., recursive=FALSE), full.names=TRUE)),
           AccessionNumber = as.character(basename(AccessionNumber))) %>%
    unnest %>%
    # Load DICOM header data
    mutate(hdr = map(series, get_hdr),
           series = basename(series)) %>%
    unnest %>%
    spread(key='name', value='value', convert=TRUE) %>%
    # Format variables
    mutate_if(is_character, replace_na, replace='EMPTY') %>%
    mutate_if(~is_integer(.) || is_double(.), list(na=is.na)) %>%
    mutate_all(replace_na, replace=0) %>% 
    # Arrange table
    select(AccessionNumber, SeriesNumber, everything()) %>%
    arrange(AccessionNumber, SeriesNumber)
}

# Function to preprocess header data
preprocess_headers <- function(tb) {
  # Convert CHARACTER variables into document term matrix numeric variables
  splitter_re = '[[:punct:][:space:]]+'
  desc_tokens = tb %>%
    select(series, SeriesDescription) %>%
    unnest_tokens(word, SeriesDescription, token=str_split, pattern=splitter_re)
  options_tokens = tb %>%
    select(series, ScanOptions) %>%
    unnest_tokens(word, ScanOptions)
  tb_dtm = bind_rows(desc_tokens, options_tokens) %>%
    filter(!str_detect(word, '^[:digit:]+$')) %>% # remove number tokens
    count(series, word) %>%
    cast_dtm(document=series, term=word, value=n) %>%
    removeSparseTerms(sparse=0.99)
  tb_dtm = as_tibble(data.frame(as.matrix(tb_dtm[tb$series, ])))

  # Convert FACTOR variables into dummy encoded numeric variables
  fct_names = c('CodeValue', 'MRAcquisitionType', 'ScanningSequence', 'StationName', 'VariableFlipAngleFlag')
  tb_fct = tb %>%
    select(fct_names) %>%
    mutate_all(as.factor) %>%
    {. ->> tb_fct_tmp} %>%
    predict(dummyVars(formula(~ .) , data=.), newdata=.) %>%
    as_tibble

  # Select NUMERIC training variables
  tb_num = tb %>%
    select_if(~is_integer(.) || is_double(.))

  # Compile character, factor, and numeric training variables
  tb_preproc =  bind_cols(tb_dtm, tb_fct, tb_num) %>%
    select(-findLinearCombos(.)$remove) %>%
    as_tibble

  return(list(tb_preproc = tb_preproc, 
              tb_dtm     = tb_dtm, 
              tb_fct_tmp = tb_fct_tmp))
}

# Function to preprocess header data from new test cases
# (while respecting variables from original training set)
preprocess_headers_new <- function(tb_new, tb_preproc) {
  # Convert CHARACTER variables into document term matrix numeric variables
  splitter_re = '[[:punct:][:space:]]+'
  desc_tokens = tb_new %>%
    select(series, SeriesDescription) %>%
    unnest_tokens(word, SeriesDescription, token=str_split, pattern=splitter_re) 
  options_tokens = tb_new %>%
    select(series, ScanOptions) %>%
    unnest_tokens(word, ScanOptions)
  tb_dtm_new = bind_rows(desc_tokens, options_tokens) %>%
    filter(word %in% colnames(tb_preproc$tb_dtm)) %>%
    count(series, word) %>%
    cast_dtm(document=series, term=word, value=n)
  tb_dtm_new = as_tibble(data.frame(as.matrix(tb_dtm_new[tb_new$series, ]))) %>%
    left_join(tb_preproc$tb_dtm[0,], by=colnames(.)) %>%
    select(names(tb_preproc$tb_dtm)) %>%
    mutate_all(replace_na, replace=0)
  
  # Convert FACTOR variables into dummy encoded numeric variables
  fct_names = c('CodeValue', 'MRAcquisitionType', 'ScanningSequence', 'StationName', 'VariableFlipAngleFlag')
  tb_fct_new = tb_new %>%
    select(fct_names) %>%
    map2(tb_preproc$tb_fct_tmp, ~factor(.x, levels=levels(.y))) %>%
    as_tibble %>%
    predict(dummyVars(formula(~ .) , data=.), newdata=.) %>%
    as_tibble

  # Select NUMERIC training variables
  tb_num_new = tb_new %>%
    select_if(~is_integer(.) || is_double(.))

  # Compile character, factor, and numeric training variables
  bind_cols(tb_dtm_new, tb_fct_new, tb_num_new) %>%
    select(colnames(tb_preproc$tb_preproc)) %>%
    as_tibble
}

# Function to predict series classification of a bunch of headers
predict_headers <-function(acc_dir, models, tb_preproc) {
  tb_new = load_study_headers(acc_dir)
  tb_preproc_new = preprocess_headers_new(tb_new, tb_preproc)

  extractProb(models, unkX = data.frame(tb_preproc_new)) %>%
    select(flair, t1, t1ce, t2, object) %>%
    group_by(object) %>%
    nest %>%
    mutate(data=map(data, rowid_to_column)) %>%
    unnest %>%
    group_by(rowid) %>%
    summarize_if(is.numeric, mean) %>%
    gather(select=-rowid, key='class', value='prob') %>%
    group_by(class) %>%
    top_n(n=1, wt=prob) %>%
    mutate(SeriesNumber = tb_new$SeriesNumber[rowid],
           rowid = NULL) %>%
    distinct(class, .keep_all=TRUE) %>% 
    left_join(select(tb_new, SeriesNumber, series), by='SeriesNumber')
}
