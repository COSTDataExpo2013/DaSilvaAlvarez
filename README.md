DaSilvaAlvarez
==============

This repository contains supplementary material for the paper "Clicks and Cliques. Exploring the Soul of the Community" to enable the reader to reproduce the analysis.

* **papersoul.Rnw** is the Knitr source file to produce the paper. It includes the R code for all figures.

* **data** folder contains the SOTC data (pre-process with Xiaoque Cheng programs), the data tables from Census Bureau and the results from cluster (**res_cluster.Rdata**) and random forest (**res_rf.Rdata**) analysis. 
* **code** folder contains the R scripts to reproduce the analysis. There are 3 files within this folder
    + **data_chk_tidy.R** contains the data pre-processing code for SOTC data.
    + **census_chk_tidy.R** contains the data pre-processing code for the Census Bureau data.
    + **statmod_tidy.R** contains the cluster and Random Forest analysis for SOTC data. 
* The files  **papersoul.bib**, **bibstyle.bst**	,**svglov3.clo**	,**svjour3.cls**	are needed to produce the bibliography and document latex style    
