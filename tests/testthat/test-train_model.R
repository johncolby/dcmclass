test_that("training", {
  Sys.setlocale('LC_ALL', 'C')
  on.exit(Sys.setlocale("LC_CTYPE", ""))
  output = expect_warning(train_model(dcm_dir='../../../dcm_test', gt_labels_path='../../../gt_labels.csv', n_cv = 2, repeats = 2, save_path = FALSE))
  expect_type(output, 'list')
  expect_s3_class(output$models[[1]], 'train')
  expect_known_hash(output$tb, '45b3cf6da0')
  expect_known_hash(output$tb_preproc, '0b4cc4ad32')
  })
