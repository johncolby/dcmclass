#' Train model
#' 
#' Train a model to identify modality (e.g axial FLAIR, T1, T1CE) based on 
#' DICOM header data.
#' 
#' @param dcm_dir String. Directory path to training set, organized like:
#' studies/series/dicoms.
#' @param gt_labels_path String. File path to ground truth labels file \code{gt_labels.csv}.
#' @param save_path String or NULL. (Optional) File path to save trained model as a \code{.Rdata} file.
#' @param n_cv Number of cross validation folds for model training.
#' @param repeats Number of repeats for repeated cross validation.
#' @export

train_model <- function(dcm_dir, gt_labels_path, save_path = 'model.Rdata', 
  n_cv = 5, repeats = 5) {

  # Load hand-labeled series classification
  gt_labels = readr::read_csv(gt_labels_path, col_types = 'ciiii') %>%
    gather(select=-AccessionNumber, key='class', value='SeriesNumber')

  # Construct training dataset
  tb_tmp = file.path(dcm_dir, unique(gt_labels$AccessionNumber)) %>%
    load_study_headers
  tb = tb_tmp %>%
    left_join(gt_labels, by=c('AccessionNumber', 'SeriesNumber')) %>%
    mutate(class = replace_na(class, replace='other'),
           class = as.factor(class))
  tb_preproc = preprocess_headers(tb_tmp)
  train_data = data.frame(tb_preproc$tb_preproc)

  # Train models
  pre_process = c('nzv', 'center', 'scale')
  tr_control = caret::trainControl(method     = 'repeatedcv',
                                   number     = n_cv,
                                   repeats    = repeats,
                                   classProbs = TRUE,
                                   sampling   = 'down')

  train_wrapper <- function(method){
    caret::train(train_data, tb$class, method     = method, 
                                       preProcess = pre_process,
                                       trControl  = tr_control)
  }

  models = map(c('xgbTree'), train_wrapper)

  if(!is_null(save_path)) {
    save(tb, 
         tb_preproc, 
         models, 
         file=save_path)
  }

  return(list(tb = tb, tb_preproc = tb_preproc, models = models))
}