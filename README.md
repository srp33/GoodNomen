![GoodNomen logo](www/Logo.png)

# Introduction

*Good Nomen* enables researchers to standardize tabular data against ontologies from [BioPortal](https://bioportal.bioontology.org/) via a user-friendly interface. Data values and column names can be standardized.

# Demo Version

You can try Good Nomen [here](https://bioapps.byu.edu/GoodNomen). The demo server has a file-upload limit of 10 megabytes.

# Installing and running Good Nomen

This repository provides scripts for executing Good Nomen within a Docker container. First, you will need to install the Docker engine (see [https://docs.docker.com/install](https://docs.docker.com/install/)).

Second, you will need to obtain an API key from [BioPortal](https://bioportal.bioontology.org/help#Getting_an_API_key). Store this in the root directory of this repository in a file called `BioPortalApiKey.txt`.

Then execute the `build_docker` script at the command line (this may need to be modified slightly on Windows). After the image has been built, use the `run_docker` script to start a container with Good Nomen running inside it. You should then be able to access the app at [http://localhost:8080/GoodNomen](http://localhost:8080/GoodNomen).

# Contact Us

Please leave questions or comments for us [here](https://github.com/srp33/GoodNomen/issues).
