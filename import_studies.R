# Script to download a representative DICOM file from each series in a set of studies

library(tidyverse)
library(glue)

# Input arguments:
# accs - char vector of accession numbers to download
# cred_path - path to air_login.txt for air_download.py
# air_url - URL for AIR API
# dcm_dir - output dir

pb = txtProgressBar(max=length(accs), style=3)
for (i in 1:length(accs)) {
    # Check if this study has already been retrieved
    acc = accs[i]
    study_dir = file.path(dcm_dir, acc)
    if(!dir.exists(study_dir)) {
        tryCatch({
            dir.create(study_dir)

            # Download study via AIR API
            out_path = file.path(study_dir, glue('{acc}.zip'))
            args = c('-c', cred_path, '-o', out_path, air_url, acc)
            system2('air_download', args)

            # Extract study
            unzip(out_path, exdir = study_dir)
            unlink(out_path)

            # Only keep 1 representative DICOM per series
            series = list.files(list.files(study_dir, full.names=TRUE), full.names=TRUE)
            dcms_to_del = unlist(map(series, ~list.files(., full.names=TRUE)[-1]))
            unlink(dcms_to_del)
        }, error = function(e) {
            print(e)
            unlink(study_dir)})
    } else {
        warning(glue('{acc} already exists, skipping.'))
    }
    setTxtProgressBar(pb, i)
}
close(pb)