#' Import DICOM studies
#' 
#' Download a representative DICOM file from each series in a set of studies.
#' 
#' @param accs Character vector. Accession numbers to download.
#' @param cred_path String. File path to air_login.txt (for \code{air_download}).
#' @param air_url String. URL for AIR API.
#' @param dcm_dir String. Directory path for output.
#' @export

import_studies <- function(accs, dcm_dir, cred_path, air_url) {

    pb = utils::txtProgressBar(max=length(accs), style=3)
    for (i in 1:length(accs)) {
        # Check if this study has already been retrieved
        acc = accs[i]
        study_dir = file.path(dcm_dir, acc)
        if(!dir.exists(study_dir)) {
            tryCatch({
                dir.create(study_dir)

                # Download study via AIR API
                out_path = file.path(study_dir, glue::glue('{acc}.zip'))
                args = c('-c', cred_path, '-o', out_path, air_url, acc)
                system2('air_download', args)

                # Extract study
                utils::unzip(out_path, exdir = study_dir)
                unlink(out_path)

                # Only keep 1 representative DICOM per series
                series = list.files(list.files(study_dir, full.names=TRUE), full.names=TRUE)
                dcms_to_del = unlist(map(series, ~list.files(., full.names=TRUE)[-1]))
                unlink(dcms_to_del)
            }, error = function(e) {
                print(e)
                unlink(study_dir)})
        } else {
            warning(glue::glue('{acc} already exists, skipping.'))
        }
        utils::setTxtProgressBar(pb, i)
    }
    close(pb)
}