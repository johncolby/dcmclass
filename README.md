# `dcmclass`

This is an R package for training and applying a machine learning classifier for the identification of series type (e.g. axial 3D T1, FLAIR, etc.), based on DICOM header data.

## Installation

```R
install.packages('devtools')
devtools::install_git('<git_repo_URL>')
```

## Usage

Load R package

```R
library(dcmclass)
```

### Import studies

`import_studies()`

This function will take in a list of accession numbers, and then use the AIR API to download a single representative DICOM file from each series in each study.

### Manually label training cases

Generate a `gt_labels.csv` file with ground truth manual labels for the training set. For each accession, it should contain the *true* series number corresponding to FLAIR, T1, T1CE, and T2. For example, it may look like:

```
AccessionNumber,flair,t1,t1ce,t2
11111111,700,5,11,600
11111112,400,5,9,8
11111113,600,7,9,8
11111114,400,5,10,8
11111115,400,5,10,8
```

### Train model

`train_model()`

This function will take in a training set (consisting of DICOM directories, generated above), and their ground truth labels (`gt_labels.csv`, generated above), and train a classifier for their identification. The trained model can then be saved as a `.Rdata` file for future use.

Because the `gt_labels.csv` and `model.Rdata` files contain patient derived and thus potentially identifiable information, we can't freely distribute these with this package. However, with these tools, you should be able to generate similar data for your needs.

### Predict new cases

`classify_series()`

To predict new/unknown cases, we can just load up and apply our pre-trained model. This file provides an example command line interface.
