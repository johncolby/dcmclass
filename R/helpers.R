#' Get DICOM header
#'
#' Extract a single representative DICOM header from a given series.
#'
#' @param path String. Directory path to a study series (which should contain DICOM files).
#' @param field_names Char vector. Set of DICOM header fields to extract.
#' @keywords internal
get_hdr <- function(path, field_names){
  tibble::as_tibble(
    oro.dicom::readDICOMFile(list.files(path, full.names = TRUE)[1],
                             skipSequence=TRUE, pixelData = FALSE)$hdr) %>%
    dplyr::select(name, value) %>%
    dplyr::filter(name %in% field_names) %>%
    dplyr::distinct(name, .keep_all = TRUE)
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
  tibble::tibble(AccessionNumber = acc_dirs) %>%
    dplyr::mutate(
      path = purrr::map(AccessionNumber,
                        ~list.files(list.dirs(., recursive=FALSE), full.names=TRUE)),
      AccessionNumber = as.character(basename(AccessionNumber))) %>%
    tidyr::unnest(path) %>%
    # Load DICOM header data
    dplyr::mutate(hdr = map(path, get_hdr, field_names=field_names),
                  series = basename(path)) %>%
    tidyr::unnest(hdr) %>%
    tidyr::spread(key='name', value='value', convert=TRUE) %>%
    # Handle missing values
    dplyr::mutate_if(is_character, replace_na, replace='EMPTY') %>%
    dplyr::mutate_if(~is_integer(.) || is_double(.), list(na=is.na)) %>%
    dplyr::mutate_all(replace_na, replace=0) %>%
    # Arrange table
    dplyr::select(AccessionNumber, SeriesNumber, everything()) %>%
    dplyr::arrange(AccessionNumber, SeriesNumber)
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
  n = series = field = word = NULL
  rm(list=c("series", "field", "word", "n"))
  tb_dtm = tb %>%
    dplyr::select(series, field) %>%
    tidytext::unnest_tokens(word, !!field, token=stringr::str_split, pattern=pattern) %>%
    dplyr::filter(!stringr::str_detect(word, '^[:digit:]+$')) %>% # remove number tokens
    dplyr::count(series, word) %>%
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
  if(!rlang::is_null(ref)) {
    # Use same fields as training set
    num_fields  = ref$fields$num_fields
    fct_fields  = ref$fields$fct_fields
    char_fields = ref$fields$char_fields
    char_splitters = ref$char_splitters
  }

  # Convert CHARACTER variables into document term matrix numeric variables
  tb_dtm = dplyr::select(tb)
  if(!is_false(char_fields)) {
    tb_dtm = purrr::map2(char_fields, char_splitters, ~get_dtm(tb, .x, .y)) %>%
      dplyr::bind_cols
    if(!rlang::is_null(ref)) {
      # Add terms to match training set
      join_by = names(tb_dtm)[names(tb_dtm) %in% names(ref$tb_dtm)]
      tb_dtm = tb_dtm %>%
        dplyr::left_join(ref$tb_dtm[0,], by=join_by) %>%
        dplyr::select(names(ref$tb_dtm)) %>%
        dplyr::mutate_all(tidyr::replace_na, replace=0)
    }
  }

  # Convert FACTOR variables into dummy encoded numeric variables
  tb_fct     = dplyr::select(tb)
  tb_fct_tmp = dplyr::select(tb)
  if(!is_false(fct_fields)) {
    tb_fct_tmp = tb %>%
      dplyr::select(fct_fields) %>%
      dplyr::mutate_all(as.factor)
    if(!rlang::is_null(ref)) {
      # Add levels to match training set
      tb_fct_tmp = tb_fct_tmp %>%
        purrr::map2(ref$tb_fct_tmp, ~factor(.x, levels=levels(.y))) %>%
        tibble::as_tibble
    }
    # Generate dummy variables
    tb_fct = tb_fct_tmp %>%
      dplyr::select_if(~ length(levels(.)) > 1) %>%
      stats::predict(caret::dummyVars(stats::formula(~ .) , data=.), newdata=.) %>%
      tibble::as_tibble
  }

  # Select NUMERIC training variables
  tb_num = tb %>%
    dplyr::select(num_fields)

  # Compile character, factor, and numeric training variables
  tb_preproc = dplyr::bind_cols(tb_dtm, tb_fct, tb_num)
  if(rlang::is_null(ref)) {
    # If training mode, return extra info need to match preprocessing at testing
    tb_preproc = tb_preproc %>%
      dplyr::select(-caret::findLinearCombos(.)$remove) %>%
      tibble::as_tibble
    return(list(tb_preproc = tb_preproc,
                tb_dtm     = tb_dtm,
                tb_fct_tmp = tb_fct_tmp,
                fields     = list(num_fields=num_fields, fct_fields=fct_fields, char_fields=char_fields),
                char_splitters = char_splitters))
  } else {
    # If testing mode, just return the preprocessed data
    tb_preproc %>%
      dplyr::select(colnames(ref$tb_preproc)) %>%
      tibble::as_tibble
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
    dplyr::select(flair, t1, t1ce, t2, object) %>%
    dplyr::group_by(object) %>%
    tidyr::nest %>%
    dplyr::mutate(data=map(data, tibble::rowid_to_column)) %>%
    tidyr::unnest(data) %>%
    dplyr::group_by(rowid) %>%
    dplyr::summarize_if(is.numeric, mean) %>%
    tidyr::gather(-rowid, key='class', value='prob') %>%
    dplyr::group_by(class) %>%
    dplyr::top_n(n=1, wt=prob) %>%
    dplyr::mutate(SeriesNumber = tb$SeriesNumber[rowid],
                  rowid = NULL) %>%
    dplyr::distinct(class, .keep_all=TRUE) %>%
    dplyr::left_join(
      dplyr::select(tb, SeriesNumber, series), by='SeriesNumber')
}

#' @importFrom caret contr.ltfr
#' @export
caret::contr.ltfr
