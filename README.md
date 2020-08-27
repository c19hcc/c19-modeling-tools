# COVID-19 Dashboard and Tools

The **COVID-19 Dashboard and Tools** is a set of tools, analyses, and dashboards designed to provide insight into the dynamics of COVID-19 regarding hospital capacity, PPE usage, and non-pharmaceutical interventions (NPIs). This collection of tools developed by the [COVID-19 Healthcare Coalition](https://www.c19hcc.org) provides access to data and models to help users understand the impact of key decisions, such as increasing hospital capacity or lifting interventions, and how the fatality rate may change in response to these decisions. 

This tool was developed in [R Shiny](https://shiny.rstudio.com/) and uses many popular R packages for data transformation, analysis, and visualization. Additionally, the [HCC C-19 Short-term forecast model](https://dashboards.c19hcc.org/ppe/) was developed in Python and is being imported into the dashboard using the `reticulate` package. The **COVID-19 Dashboard and Tools** is built with publicly available data and models and [can be used and viewed without installation online](https://dashboards.c19hcc.org/).

## Requirements

To view or update the **COVID-19 Dashboard and Tools** locally, the following pieces of computer software are required:

- A computer with any operating system (Windows, Linux, or MacOS)
- [R](https://www.r-project.org/)
- [RStudio](https://rstudio.com/)   
- [Python 3.0+](https://docs.conda.io/en/latest/miniconda.html)
- Web broswer (Preferred: Google Chrome, Firefox, Edge, Safari)

## Installation

Begin by cloning this repository which will download all required scripts needed to set up your local backend database and run the application locally.

```
git clone https://github.com/c19hcc/c19-modeling-tools.git
```

Next, ensure the following Python packages are installed either using `pip` or `conda`.

```
pip install -r requirements.txt
```

or...

```
conda install --file requirements.txt
```

Finally, open and run `setup.R` in RStudio or your IDE of choice. This script checks your local R packages, determines which, if any, new packages need to be installed, and then installs them from your default CRAN mirror. After doing so, `setup.R` sources another R script, `code/build_database.R` to build and update the backend data files that are used in the application. 

## Usage

After downloading the required R and Python libraries and building the backend database, open `app.R` in RStudio and click the _"Run App"_ button to run the application locally. After loading the required libraries, the R Shiny application should open automatically in a new window or in your browser.

Once the application is running, it should open to the **Home** screen which contains information on each of the individual dashboard views and analytics tools available in the application. The **Home** page also links to other [COVID-19 Healthcare Coalition](https://www.c19hcc.org) tools and resources which may be relevent for some users. 

### Updating the data

#### Manually
The data driving the application's analytics, metrics, and visualization is not automatically updated each time the app is run. 

To pull new county-level reported case counts and death counts, and testing statistics, users must run `code/pull_covid_datasets.py`.

After pulling this data, users must rebuild the database which will automatically update the other open-source dataset driving the application by re-running `code/build_database.R`. 

#### Automatically (UNIX only)
If using a UNIX system, you can set up a cron job to update the database on a daily basis by running `code/add_cronjob.sh`. This will invoke a shell script called `code/update_all_datasets_and_db.sh` which runs the two scripts mentioned above.

## License

Copyright 2020, The MITRE Corporation
Approved for Public Release; Distribution Unlimited. Case Number 20-1521.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
