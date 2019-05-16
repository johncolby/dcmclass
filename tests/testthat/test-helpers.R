dcm_dir = '../../../dcm_test/'
num_fields = c('SeriesNumber', 'SliceThickness', 'RepetitionTime', 'EchoTime',
               'MagneticFieldStrength', 'SpacingBetweenSlices', 'FlipAngle',
               'ImagesInAcquisition', 'Rows', 'Columns')
fct_fields = c('CodeValue', 'MRAcquisitionType', 'ScanningSequence',
               'StationName', 'VariableFlipAngleFlag')
char_fields = c('SeriesDescription', 'ScanOptions')
char_splitters = c('[[:punct:][:space:]]+', '[:space:]+')
field_names = c(num_fields, fct_fields, char_fields)

test_that("load single series/DICOM", {
  series_dir = list.files(list.files(list.dirs(dcm_dir, recursive = FALSE), full.names = TRUE)[1], full.names = TRUE)[1]
  hdr = expect_warning(get_hdr(series_dir, field_names = num_fields))
  expect_s3_class(hdr, 'tbl_df')
  expect_identical(nrow(hdr), length(num_fields))
  expect_identical(sort(hdr$name), sort(num_fields))
})

acc_dirs = list.files(dcm_dir, full.names = TRUE)
tb  = expect_warning(load_study_headers(acc_dirs[1], field_names = field_names))
tb2 = expect_warning(load_study_headers(acc_dirs[2], field_names = field_names))

test_that("load study", {
  Sys.setlocale('LC_ALL', 'C')
  on.exit(Sys.setlocale("LC_CTYPE", ""))
  expect_s3_class(tb, 'tbl_df')
  expect_known_hash(tb, '9462959fdb')
})

test_that("generate TDM", {
  tb_dtm = expect_s3_class(get_dtm(tb, 'SeriesDescription', '[[:punct:][:space:]]+'), 'tbl_df')
  expect_known_hash(tb_dtm, 'e3fb49cec0')
})

test_that("preprocessing", {
  expect_known_hash(preprocess_headers(tb, num_fields=num_fields), '39f8e09d77')
  expect_known_hash(preprocess_headers(tb, num_fields=num_fields, char_fields = char_fields, char_splitters = char_splitters), '24be0e927c')
  expect_known_hash(preprocess_headers(tb, num_fields=num_fields, fct_fields=fct_fields), '2468b29d58')
  ref = expect_known_hash(preprocess_headers(tb, num_fields=num_fields, char_fields = char_fields, char_splitters = char_splitters, fct_fields=fct_fields), '62ad097172')
  expect_known_hash(preprocess_headers(tb2, num_fields=num_fields, char_fields = char_fields, char_splitters = char_splitters, fct_fields=fct_fields, ref=ref), '3439e2ebfd')
})
