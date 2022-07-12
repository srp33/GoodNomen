![GoodNomen logo](www/Logo.png)

# Introduction

*Good Nomen* enables researchers to standardize tabular data against ontologies from [BioPortal](https://bioportal.bioontology.org/) via a user-friendly interface. Data values and column names can be standardized.

# Demo Version

You can try Good Nomen [here](https://bioapps.byu.edu/GoodNomen). The demo server has a file-upload limit of 1 megabyte.

If you would like, you can use [this example data file](https://raw.githubusercontent.com/srp33/GoodNomen/master/example_data/nationwidechildrens.org_clinical_drug_brca.tsv). After uploading it to Good Nomen, indicate that the file has three header lines. When standardizing this file, we have focused primarily on the `pharmaceutical_therapy_drug_name` column.

# Installing and running Good Nomen on a Unix/Linux based operating system (including Mac OS)

This repository provides scripts for executing Good Nomen within a Docker container. To so, please complete the following steps:

1. Install the Docker engine (see [https://docs.docker.com/install](https://docs.docker.com/install/)).
2. Clone this GitHub repository. To so at the command line, you can use this command: `git clone https://github.com/srp33/GoodNomen.git`.
3. Obtain an API key from [BioPortal](https://www.bioontology.org/wiki/BioPortal_Help#Getting_an_API_key). In the GoodNomen directory of the GitHub repository that you just cloned, create a a file called `BioPortalApiKey.txt` and store the API key within that file.
4. Enter `bash build_docker` at the command line. (If you are using an interpreter other than bash, modify the command accordingly.)
5. Enter `bash run_docker` at the command line. This should start a container with Good Nomen running inside it. You should then be able to access the app at [http://localhost:80/GoodNomen](http://localhost:80/GoodNomen). You can change the port it uses by modifying `run_docker`. If you would like to modify the file-size limit, you can modify this near the top of the `UI.R` script and then rebuild the app.

# Contact Us

Please leave questions, comments, or bug reports for us [here](https://github.com/srp33/GoodNomen/issues).

This application was developed by the [Piccolo Lab](https://piccolo.byu.edu). Special thanks to [Alyssa Parker](https://medschool.vanderbilt.edu/igp/person/alyssa-parker) for conceiving this app and working so hard on it. [Elizabeth Anderson](https://www.linkedin.com/in/elizabeth-cook13) also made many contributions.
