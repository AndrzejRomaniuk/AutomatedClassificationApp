# Automated Testing, Data Clasiffication

## Table of contents
* [General info](#general-info)
* [Setup](#setup)
* [Author and contact](#author-and-contact)

## General info
This Shiny application was written in RStudio (ver. 1.3.1073; R ver. 4.02) as an example of how to automate R analysis by dedicated micro-applications.
The author used classification testing as an example mainly due to working with multiple classification algorithms during his PhD; however it shows only
a simplified approach to the issue. 

The application enables user to firstly input a dataset (so far only in a CSV format) and define columns to be used [Stage 1]. Once set, users can define
selected details, like classification type to be utilized, input values to trainControl() function or whether to apply data normalisation before algorithm training [Stage 2]. Due to the app being only an exaple, only a selection of methods applicable to train() method has been implemented. Finally, users can download a train() result printout as well as two visualisations of gathered accuracy and kappa values, as box or violin plots [Stage 3].

## Setup
To be able to run or modify the app R and RStudio have to be installed on your computer:
* R package can be downloaded from https://cran.r-project.org/bin/windows/base/release.htm
* RStudio can be downloaded from: https://rstudio.com/products/rstudio/download/#download
* Install Shiny library: install.packages("shiny")

If you have R and RStudio already installed:
* Download the app.R file from the repository
* Open R file in RStudio and run the app
* Required libraries will be installed authomatically

Alternatively, to run the app via a link:
* In RStudio, run code: shiny::runGitHub("AutomatedClassificationApp","AndrzejRomaniuk", ref = "main")
* Required libraries will be installed authomatically

## Author and contact

Andrzej A. Romaniuk


PhD (Archaeology), MSc (Osteoarchaeology)

University of Edinburgh, Research/Teaching assistance

National Museums of Scotland, Research associate

Higher Education Academy, Associate fellow

https://www.ed.ac.uk/profile/andrzej-romaniuk

https://www.linkedin.com/in/andrzej-aleksander-romaniuk-74145292/

https://github.com/AndrzejRomaniuk

https://orcid.org/0000-0002-4977-9241

