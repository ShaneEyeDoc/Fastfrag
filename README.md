# Fastfrag

Fastfrag is an R package for fragment analysis, designed to process and visualize data from .fsa files. It is a modified version of the excellent Fragman package by Giovanny Covarrubias-Pazaran et al. (2016, BMC Genetics 17: 62). Fastfrag has been tailored for specific needs, offering an interactive plot that replicates the experience of Genemapper but in a more lightweight and user-friendly manner.

## Motivation

The Genemapper app can be restrictive and sluggish, especially when accessing it on shared resources. Fastfrag aims to provide a more efficient and accessible alternative for analyzing TCF4 CTG18.1 genotyping data, among other applications.

## Prerequisites

Make sure you have the following R packages installed:

```r
install.packages("tidyverse")
install.packages("plotly")
```

## Installation

To install the Fastfrag package:

1. Download the package from the GitHub repository or directly from the server:
   
2. Load the package in your R session:

   ```r
   library(Fastfrag)
   ```

## Analyzing Samples

To analyze .fsa files using Fastfrag:

1. Place your .fsa files in a folder on your system.
2. Set your `folder_path` to the directory containing your .fsa files.
3. Define your ladder. The default ladder used in our lab is ROX500, which can be specified as shown below.

4. Run the analysis using the `process_fsa_files` function:

   ```r
   folder_path <- "/your_folder_containing_fsa_files"
   ladder <- c(35, 50, 75, 100, 139, 150, 160, 200, 250, 300, 340, 350, 400, 450, 490, 500)

   process_fsa_files(folder_path, ladder)
   ```

5. The output will be individual plots that allows you to check if the ladder is properly calibrated along with an interactive electropherogram chart.

## Contributing

Feel free to contribute to Fastfrag by forking the repository and submitting pull requests.


---
V1
