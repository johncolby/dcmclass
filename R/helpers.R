#' Get DICOM header
#'
#' Extract a single representative DICOM header from a given series.
#'
#' @param path String. Directory path to a study series (which should contain DICOM files).
#' @param field_names Char vector. Set of DICOM header fields to extract.
#' @keywords internal
get_hdr <- function(path, field_names){
  as_tibble(oro.dicom::readDICOMFile(list.files(path, full.names = TRUE)[1], skipSequence=TRUE, pixelData = FALSE)$hdr) %>%
    select(name, value) %>%
    filter(name %in% field_names) %>%
    distinct(name, .keep_all = TRUE)
}

#' Load study headers
#'
#' Load representative headers from each series in a set of multiple studies
#'
#' \code{NA} are dummy coded as additional variables, as their presence/absence
#' may provide useful information for classification
#'
#' @param acc_dirs Char vector. Directory paths, whose basenames should be
#' accession numbers, and whose contents should be study directories.
#' @inheritParams get_hdr
#' @export
load_study_headers <- function(acc_dirs, field_names) {
  tibble(AccessionNumber = acc_dirs) %>%
    mutate(path = map(AccessionNumber, ~list.files(list.dirs(., recursive=FALSE), full.names=TRUE)),
           AccessionNumber = as.character(basename(AccessionNumber))) %>%
    unnest(path) %>%
    # Load DICOM header data
    mutate(hdr = map(path, get_hdr, field_names=field_names),
           series = basename(path)) %>%
    unnest(hdr) %>%
    spread(key='name', value='value', convert=TRUE) %>%
    # Handle missing values
    mutate_if(is_character, replace_na, replace='EMPTY') %>%
    mutate_if(~is_integer(.) || is_double(.), list(na=is.na)) %>%
    mutate_all(replace_na, replace=0) %>%
    # Arrange table
    select(AccessionNumber, SeriesNumber, everything()) %>%
    arrange(AccessionNumber, SeriesNumber)
}

#' Generate Document Term Matrix
#'
#' Preprocess character fields with text mining tools to generate a Document
#' Term Matrix
#'
#' @param field String. Character/string field to process.
#' @param pattern String. Regular expression splitting pattern.
#' @inheritParams preprocess_headers
#' @keywords internal
get_dtm <- function(tb, field, pattern) {
  tb_dtm = tb %>%
    select(series, field) %>%
    tidytext::unnest_tokens(word, !!field, token=stringr::str_split, pattern=pattern) %>%
    filter(!stringr::str_detect(word, '^[:digit:]+$')) %>% # remove number tokens
    count(series, word) %>%
    tidytext::cast_dtm(document=series, term=word, value=n) %>%
    tm::removeSparseTerms(sparse=0.99)
  return(as_tibble(data.frame(as.matrix(tb_dtm[tb$series, ]))))
}

#' Preprocess training DICOM headers
#'
#' Preprocess header data from training set.
#'
#' @param tb Dataframe with training set header data.
#' @param num_fields Char vector. Names of numeric fields. Should at least contain \code{SeriesNumber}.
#' @param fct_fields Char vector or \code{FALSE}. (Optional) Names of factor/categorical fields. These will be preprocessed into multiple one-hot i.e. dummy encoded variables.
#' @param char_fields Char vector or \code{FALSE}. (Optional) Names of character/string fields. These will be preprocessed with basic text mining methods into a set of variables representing the document term matrix (i.e. word frequency histograms).
#' @param char_splitters Char vector. Regular expression splitting patterns to use for \code{char_fields}.
#' @param ref List. (Optional) Reference training dataset as output by \code{preprocess_headers}.
#' @importFrom rlang is_false
#' @keywords internal
preprocess_headers <- function(tb, num_fields, fct_fields=FALSE, char_fields=FALSE, char_splitters=NULL, ref=NULL) {
  if(!is_null(ref)) {
    # Use same fields as training set
    num_fields  = ref$fields$num_fields
    fct_fields  = ref$fields$fct_fields
    char_fields = ref$fields$char_fields
    char_splitters = ref$char_splitters
  }

  # Convert CHARACTER variables into document term matrix numeric variables
  tb_dtm = select(tb)
  if(!is_false(char_fields)) {
    tb_dtm = map2(char_fields, char_splitters, ~get_dtm(tb, .x, .y)) %>%
      bind_cols
    if(!is_null(ref)) {
      # Add terms to match training set
      join_by = names(tb_dtm)[names(tb_dtm) %in% names(ref$tb_dtm)]
      tb_dtm = tb_dtm %>%
        left_join(ref$tb_dtm[0,], by=join_by) %>%
        select(names(ref$tb_dtm)) %>%
        mutate_all(replace_na, replace=0)
    }
  }

  # Convert FACTOR variables into dummy encoded numeric variables
  tb_fct     = select(tb)
  tb_fct_tmp = select(tb)
  if(!is_false(fct_fields)) {
    tb_fct_tmp = tb %>%
      select(fct_fields) %>%
      mutate_all(as.factor)
    if(!is_null(ref)) {
      # Add levels to match training set
      tb_fct_tmp = tb_fct_tmp %>%
        map2(ref$tb_fct_tmp, ~factor(.x, levels=levels(.y))) %>%
        as_tibble
    }
    # Generate dummy variables
    tb_fct = tb_fct_tmp %>%
      select_if(~ length(levels(.)) > 1) %>%
      stats::predict(caret::dummyVars(stats::formula(~ .) , data=.), newdata=.) %>%
      as_tibble
  }

  # Select NUMERIC training variables
  tb_num = tb %>%
    select(num_fields)

  # Compile character, factor, and numeric training variables
  tb_preproc = bind_cols(tb_dtm, tb_fct, tb_num)
  if(is_null(ref)) {
    # If training mode, return extra info need to match preprocessing at testing
    tb_preproc = tb_preproc %>%
      select(-caret::findLinearCombos(.)$remove) %>%
      as_tibble
    return(list(tb_preproc = tb_preproc,
                tb_dtm     = tb_dtm,
                tb_fct_tmp = tb_fct_tmp,
                fields     = list(num_fields=num_fields, fct_fields=fct_fields, char_fields=char_fields),
                char_splitters = char_splitters))
  } else {
    # If testing mode, just return the preprocessed data
    tb_preproc %>%
      select(colnames(ref$tb_preproc)) %>%
      as_tibble
  }
}

#' Predict DICOM headers
#'
#' Function to predict series classification of a bunch of headers
#'
#' @param acc_dir String. Directory path to unknown study to be classified.
#' @param models List. Pretrained model(s).
#' @param ref Reference preprocessed training data.
#' @export
#' @family main functions
predict_headers <-function(acc_dir, models, ref) {
  tb = load_study_headers(acc_dir, unlist(ref$fields))
  tb_preproc = preprocess_headers(tb, ref=ref)

  caret::extractProb(models, unkX = data.frame(tb_preproc)) %>%
    select(flair, t1, t1ce, t2, object) %>%
    group_by(object) %>%
    nest %>%
    mutate(data=map(data, tibble::rowid_to_column)) %>%
    unnest(data) %>%
    group_by(rowid) %>%
    summarize_if(is.numeric, mean) %>%
    gather(-rowid, key='class', value='prob') %>%
    group_by(class) %>%
    top_n(n=1, wt=prob) %>%
    mutate(SeriesNumber = tb$SeriesNumber[rowid],
           rowid = NULL) %>%
    distinct(class, .keep_all=TRUE) %>%
    left_join(select(tb, SeriesNumber, series), by='SeriesNumber')
}

#' @importFrom caret contr.ltfr
#' @export
caret::contr.ltfr
