# Abstract

*Summary*: Biomedical ontologies define terms and related synonyms for observations represented in research data. When mapped to such terms, data become more interoperable and reusable. However, associating data with ontology terms is often a manual, time-intensive process. To address this problem, we developed Good Nomen, a user-friendly Web application that enables researchers to map variables and data values against ontologies, in an interactive manner, with full provenance.

*Availability and Implementation*: Good Nomen is available from http://bioapps.byu.edu/GoodNomen. Source code is available at https://github.com/srp33/GoodNomen.
<!--%TODO: Make sure the URLs are valid and working.-->
<!--%TODO: Explain on the GitHub site how to run it locally via Docker.-->

*Contact*: stephen_piccolo@byu.edu

*Supplementary Information*: Supplementary material is available at Bioinformatics online.

\newpage

# Introduction

<!--%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%TODO: Add reference to https://www.nature.com/articles/sdata201921 (the variable quality of metadata)
@gonccalves2019variable
%TODO: Describe support for bioportal.bioontology.org
%TODO: Be more clear that this is designed for tabular data, not necessarily EHR data for which industrialized tools may exist.
%TODO: Mention FAIR principle of being "interoperable". "it is critical to use (1) commonly used controlled vocabularies, ontologies, thesauri (having resolvable globally unique and persistent identifiers" https://www.go-fair.org/fair-principles/i1-metadata-use-formal-accessible-shared-broadly-applicable-language-knowledge-representation/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%-->

Historically, biomedical researchers often worked independently of each other, and findings were communicated in relatively small circles, which sometimes led to disparate naming conventions for research variables and non-standardized data values. Improved communication technologies and digitization of health records have enabled more data sharing among research groups[@adler-milsteinSharingClinicalData2012;@kayeDataSharingGenomics2009a]. In addition, biomedical data are now shared frequently in the public domain, with the hope that broader access to such data will lead to potential cures[@murphyServingEnterpriseInformatics2010;@pollardEICUCollaborativeResearch2018;@internationalcancergenomeconsortiumInternationalNetworkCancer2010a;@thecancergenomeatlasresearchnetworkComprehensiveGenomicCharacterization2008]. However, differences in naming conventions and terms used to describe data values have persisted, making it difficult to infer semantic meanings from data or to integrate disparate datasets[@bensonUsingSNOMEDHL72010;@kaufmanDataStandardizationPharmaceutical2016;@meyerDataCancerComparative2012;@zhangOntologyguidedSemanticData2018;@gonccalves2019variable]. Individual researchers and standards bodies have recognized this issue and are developing biomedical ontologies, which specify standardized terms and synonyms for biomedical concepts and states[@hartelModelingDescriptionLogic2005]. Unfortunately, these standards are not being used to annotate biomedical research data as universally as hoped[@bensonWhyInteroperabilityHard2010]. Although it is possible to annotate data manually or to write custom computer scripts to facilitate the mapping, these processes may be inefficient and lack reproducibility.

To facilitate standardization of We have developed Good Nomen, an interactive Web application that allows users to standardize clinical and molecular data using reference terminologies through a user-friendly interface. Users have the option of picking from any ontology that can be found on https://bioportal.bioontology.org to standardize their data.
The specific use case we target is...
<!--%Sample-level annotations are notorious for lack of uniformity across publicly available datasets, even within the same repository. -->

Although Good Nomen supports using terminologies to standardize molecular data (such as HUGO for gene names), our descriptions in this paper focus on standardizing clinical data. We emphasize the scenario in which clinical data have been posted in public repositories, such as The Cancer Genome Atlas (TCGA). Many researchers access these data, and data descriptors have sometimes been mapped to semantic identifiers. But...redundant columns, non-standardized data values...move some wording from the Data Example section up here?

# Methods

Good Nomen was built using the R-based Shiny framework[@rcoreteamrfoundationforstatisticalcomputingLanguageEnvironmentStatistical;@chang2018shiny]. To map a clinical data file to a standardized terminology, users walk through a series of steps described below.

*Load Data*: Good Nomen accepts clinical data files in comma-separated value (CSV), tab-separated value (TSV), and Excel formats. Each row in an uploaded file should represent a given patient; each column should represent a given clinical variable. After uploading a data file, users have an option to specify the number of header lines in the file; some files contain no header lines, while others contain one or more. Next the user select a terminology to use in the standardization process. Next, the user picks from the list of ontologies found on BioPortal. Given the data uploaded, the top three ontologies that most closely match the data will be recommended. If the user has a specific ontology in mind rather than using the recommended ontologies, they can type it in the dropdown list and select it.

*Edit Data*: Data values can be standardized in two ways. The first method is an auto-matching process. After the user selects a column to standardize, Good Nomen sends all the data in the column to the BioPortal Annotator function. The annotator matches all the data to their standardized terms. A table containing potential matches is displayed, and the user is asked to accept or reject each of the matches (Figure \ref{fig:AutoMatch}). When the user accepts a match, Good Nomen replaces each occurrence of the original value in the user's data with the preferred term from the terminology. Alternatively, users may standardize data values via manual matching. With this approach, Good Nomen displays a list of unique data values from the selected column. The user may select one or more of these terms to map to a preferred term. Using the ontology that the user selected under the Load Data tab, all of the standardized terms will be availabe for search and selection. Users may select any term they wish or a enter a new term to use as the standardized term. Good Nomen then replaces all instances of the data value with the preferred term. In addition, via manual matching, users can standardize missing values; the user indicates which data values represent missingness, and Good Nomen replaces each of these values with the characters "NA".

*Update Column Names*: Users may standardize the names of columns in their datasets based on the selected terminology. After users select a column to rename, Good Nomen uses the BioPortal recommender to find matches for the column name. The top three matches are displayed to the user for selection, or if the user wishes, they can enter a new name entirely for the column.

*Save Data*: Users can export a standardized version of their data in CSV, TSV, or Excel formats. Good Nomen asks the user to enter a name for the output file. In addition to producing a standardized data file, Good Nomen generates an R script that contains the code necessary to reproduce the standardization process based on the user's selections while processing the data [@piccoloToolsTechniquesComputational2016a]. Users can download this script to save for future reference.

# Data Example

To test the effectiveness of our auto-matching approach, we analyzed treatment data from TCGA for 781 breast-cancer patients. This dataset contained 169 unique terms describing cancer therapies (Figure \ref{fig:BarChart1}). In many cases, multiple terms referred to the same drug treatment but were different variations on the generic drug name, referred to the drug's trade name, were misspelled, etc. For example, 13 different terms were used to describe *cyclophosphamide*, an alkylating agent commonly prescribed to breast-cancer patients. Using the NCI Thesaurus as a reference, Good Nomen's auto-matching feature identified matches for 95 of the 169 unique terms (Figure \ref{fig:BarChart2}). For cyclophosphamide, Good Nomen mapped 7 terms to the preferred term. The remaining terms were easily mappable through the manual-matching process. 

# Conclusions

Harnessing the power of reference terminologies into a user-friendly Web application will extend the reach and usefulness of these terminologies. Good Nomen enables researchers to standardize clinical and molecular terms more efficiently. Such standardization could provide greater consistency within a given dataset but also make it easier to combine data across studies.
%We hope that researchers who post datasets in the public domain to ensure greater consistency in how clinical and molecular terms are used.

# Acknowledgements

We thank Moom Roosan for providing insight on pharmacological mapping. Brigham Young University provided internal funds to support this project.
