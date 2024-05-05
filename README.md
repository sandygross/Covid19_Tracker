# Covid19-Tracker <img src="img/Covid19-Tracker.png" align="right" width=175 height=175 alt="" />

Explore and visualize global trends over time for COVID-19, and compare summary statistics across different countries.




## Motivation 

Target audience: Epidemiologists and Public Health Organizations

The COVID-19 pandemic caused a global health crisis, leading to numerous fatalities and widespread societal and economic damage. To better prepare for potential future outbreaks, it is crucial to conduct a retrospective analysis of the virus's spread. This entails a detailed review of its temporal and geographical progression to pinpoint regions that may require enhanced support during similar crises. This dashboard provides an comprehensive global overview, focusing on the critical period from 2020 to 2022, and offers specific summary statistics via a bar chart, presenting both absolute and relative numbers of cases, deaths, and tests conducted.The latter is particularly valuable, as it can can guide future resource allocation, ensuring timely and adequate support to countries in need. By leveraging this data, health professionals can discern patterns, assess intervention efficacy, and develop robust, evidence-based strategies for managing public health emergencies.

## Data

The two datasets used for this project are available [here](https://www.kaggle.com/datasets/josephassaker/covid19-global-dataset) and were originally scraped from worldometer.info. They include data from 225 countries, with records spanning from February 15, 2020, to May 14, 2022.

## Usage instructions

### Prerequisites 

For running this dashboard, please ensure that both R and RStudio are installed on your system.

### Installation Instructions

**1. Install `renv`in RStudio**
1. Launch RStudio
2. Install the `renv` package by running the following command in the R console:
```shell
install.packages("renv")
```

**2. Clone the repository and open the project**
1. Clone the GitHub repository to your local machine using the following command in the terminal:
```shell
git clone git@github.ubc.ca:MDS-2023-24/DSCI_532_individual-assignment_sandy02.git
```
2. Navigate to the directory where you have cloned the repository.
3. Double-click on the DSCI_532_individual-assignment_sandy02.Rproj file to open the project in RStudio.

**3. Restore the Project Environment**
 1. In the RStudio console, execute the following command to synchronize your environment with the project's lockfile:
```
renv::restore()
```
2. If prompted about activating the project, select **1: Activate the project** and use the project library.
3. If **renv::restore()** indicates that some packages are not installed or if issues occur, run the command again.

**4. Runy the Shiny App**
1. Launch the app running the following command in the R console:
```shell
shiny::runApp('src/')
```
