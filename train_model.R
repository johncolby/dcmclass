# Script to train a model to identify modality (e.g FLAIR, T1, T1CE) based on 
# DICOM header data

source('functions.R')

# Load hand-labeled series classification
gt_labels = read_csv('gt_labels.csv', col_types = 'ciiii') %>%
  gather(select=-AccessionNumber, key='class', value='SeriesNumber')

# Construct training dataset
tb = file.path('dcm', unique(gt_labels$AccessionNumber)) %>%
  load_study_headers %>%
  {. ->> tb_tmp} %>%
  left_join(gt_labels, by=c('AccessionNumber', 'SeriesNumber')) %>%
  mutate(class = replace_na(class, replace='other'),
         class = as.factor(class))
tb_preproc = preprocess_headers(tb_tmp)
train_data = data.frame(tb_preproc$tb_preproc)

# Train models
pre_process = c('nzv', 'center', 'scale')
tr_control = trainControl(method     = 'repeatedcv',
                          number     = 5,
                          repeats    = 5,
                          classProbs = TRUE,
                          sampling   = 'down')

train_wrapper <- function(method){
  train(train_data, tb$class, method     = method, 
                              preProcess = pre_process,
                              trControl  = tr_control)
}

models = map(c('xgbTree'), train_wrapper)

save(tb, 
     tb_preproc, 
     models, 
     get_hdr, 
     load_study_headers, 
     preprocess_headers_new, 
     predict_headers, 
     file='model.Rdata')