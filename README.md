# eLTER SO Costs

![](./www/eLTER-SO-costs_application-logo.jpg)

## Authors :busts_in_silhouette:
- <b>Name:</b> Allan T. Souza
  - <b>Affiliation:</b> Institute for Atmospheric and Earth System Research (INAR), Forest Sciences, Faculty of Agriculture and Forestry, University of Helsinki, P.O. Box 27, 00014 Helsinki, Finland :finland:
  - <b> Institution ROR:</b> [https://ror.org/040af2s02](https://ror.org/040af2s02)
  - <b>E-mail:</b> allan.souza@helsinki.fi / allantsouza@gmail.com
  - <b>ORCID:</b> [0000-0002-1851-681X](https://orcid.org/0000-0002-1851-681X)

- <b>Name:</b> Syed Ashraful Alam
  - <b>Affiliation:</b> Institute for Atmospheric and Earth System Research (INAR), Forest Sciences, Faculty of Agriculture and Forestry, University of Helsinki, P.O. Box 27, 00014 Helsinki, Finland :finland:
  - <b> Institution ROR:</b> [https://ror.org/040af2s02](https://ror.org/040af2s02)
  - <b>E-mail:</b> ashraful.alam@helsinki.fi
  - <b>ORCID:</b> [0000-0001-9870-1494](https://orcid.org/0000-0001-9870-1494)

- <b>Name:</b> Terhi Rasilo
  - <b>Affiliation:</b> Institute for Atmospheric and Earth System Research (INAR), Forest Sciences, Faculty of Agriculture and Forestry, University of Helsinki, P.O. Box 27, 00014 Helsinki, Finland :finland:
  - <b> Institution ROR:</b> [https://ror.org/040af2s02](https://ror.org/040af2s02)
  - <b>E-mail:</b> terhi.rasilo@helsinki.fi
  - <b>ORCID:</b> [0000-0002-3502-4040](https://orcid.org/0000-0002-3502-4040)
  
- <b>Name:</b> Steffen Zacharias
  - <b>Affiliation:</b> Department for Monitoring and Exploration Technologies, UFZ—Helmholtz Centre for Environmental Research GmbH, Leipzig, Germany :germany:
  - <b> Institution ROR:</b> [https://ror.org/000h6jb29](https://ror.org/000h6jb29)
  - <b>E-mail:</b> steffen.zacharias@ufz.de
  - <b>ORCID:</b> [0000-0002-7825-0072](https://orcid.org/0000-0002-7825-0072)

- <b>Name:</b> Jaana Bäck
  - <b>Affiliation:</b> Institute for Atmospheric and Earth System Research (INAR), Forest Sciences, Faculty of Agriculture and Forestry, University of Helsinki, P.O. Box 27, 00014 Helsinki, Finland :finland:
  - <b> Institution ROR:</b> [https://ror.org/040af2s02](https://ror.org/040af2s02)
  - <b>E-mail:</b> jaana.back@helsinki.fi
  - <b>ORCID:</b> [0000-0002-6107-667X](https://orcid.org/0000-0002-6107-667X)

## Use license :orange_book:
- [CC0 1.0](https://creativecommons.org/publicdomain/zero/1.0/)

## Aims :dart:

This repository hosts the information used to build the [eLTER](https://elter-ri.eu/) SO Costs web application. This tool is a specialized resource designed to assist the eLTER (European Long-Term Ecosystem Research) community in estimating the costs associated with upgrading and operating [standard observations](https://elter-ri.eu/storage/app/uploads/public/62c/ea2/a00/62cea2a002845239798196.pdf) across various eLTER sites.

The essence of this tool lies in its ability to adapt calculations based on unique site-specific criteria, ensuring tailored cost estimations. Key factors taken into consideration include the site category, the habitats, the focus spheres, and the potential co-location with other Research Infrastructures, which cover the costs of specific standard observations.

The goal of this tool is to provide a user-friendly, efficient, and reliable means for the eLTER community to plan and allocate resources effectively.

## File structure :open_file_folder:
eLTER-SO-costs-App/
├── data # Folder for input data used by the app
├── renv # R environment setup with reproducibility in mind
├── www # Folder containing web assets like stylesheets and JS scripts
├── .Rprofile # R project-specific configuration file
├── .gitignore # Lists files to be ignored by version control system
├── LICENSE # The full text of the license for the project
├── README.html # Compiled HTML from README.md for detailed project information
├── README.md # Markdown text providing an overview of the project
├── app.R # The main application script for the Shiny web app
├── eLTER-SO-costs-App.Rproj # RStudio project file
└── renv.lock # Lock file to capture the state of the R environment

## Naming conventions for files :page_with_curl:

## Development in R and [`Shiny`](https://shiny.posit.co/)
This repository is dedicated to the development of a tool coded entirely in the R programming language, with a user interface developed using R [`Shiny`](https://shiny.posit.co/). R [`Shiny`](https://shiny.posit.co/) is a powerful framework for building interactive web applications directly from R scripts, making it an ideal choice for creating accessible and user-friendly tools.

About the Tool
- R-Based: The core logic and functionality of the tool are implemented in R, ensuring efficient data processing and analysis.
- R [`Shiny`](https://shiny.posit.co/) Interface: The tool features an interactive web interface built with R [`Shiny`](https://shiny.posit.co/), enhancing user experience and accessibility.
- Accessing the Shiny App
You can access the current version of the [`Shiny`](https://shiny.posit.co/) app [here](https://allantsouza.shinyapps.io/eLTER-SO-costs/).

#### Future Hosting on [eLTER](https://elter-ri.eu/) Services
Upon completion and thorough testing, this tool will be hosted as part of the [eLTER](https://elter-ri.eu/) services. This integration aims to provide seamless access and utility for the [eLTER](https://elter-ri.eu/) community. Until then, this repository serves as the primary hub for development, where updates, enhancements, and bug fixes will be continuously pushed.

### Using [`renv`](https://rstudio.github.io/renv/) for Reproducible Environments
This repository utilizes the [`renv`](https://rstudio.github.io/renv/) package to manage R dependencies, ensuring that everyone working on the project has access to the same package versions. This approach guarantees that the project is reproducible, avoiding the "it works on my machine" problem. By locking the project to specific versions of R packages, [`renv`](https://rstudio.github.io/renv/) ensures that the project can be run consistently across different setups and over time.

#### Why Use [`renv`](https://rstudio.github.io/renv/)?
- Reproducibility: Ensures that the R environment is consistent across different machines and over time, making the results reliable and reproducible.
- Collaboration: Simplifies collaboration by ensuring all contributors are using the same package versions.
- Isolation: Keeps project dependencies separate from your global R environment, minimizing conflicts between projects.

#### Getting Started with [`renv`](https://rstudio.github.io/renv/)
When you clone this repository for the first time, follow these steps to set up [`renv`](https://rstudio.github.io/renv/):

- Install [`renv`](https://rstudio.github.io/renv/) (if you haven't already):
You can install [`renv`](https://rstudio.github.io/renv/) globally in your R setup using:

```
install.packages("renv")
```

- Clone the Repository:
Clone this repository to your local machine as you normally would.

- Launch R:
Open an R session and set the working directory to the root of the cloned repository.

- Restore the Environment:
Run the following command in your R console:

```
renv::restore()
```

This command will install all the necessary packages, as specified in the `renv.lock` file, into a project-local library.

- Start Working: After the restoration process is complete, you can start working on the project. The [`renv`](https://rstudio.github.io/renv/) environment will automatically be activated when you open the project in the future.

By following these steps, you can ensure that the project dependencies are correctly managed, aiding in maintaining the project's integrity and reproducibility.


### Developer
#### [Allan T. Souza](https://allantsouza.netlify.app/), University of Helsinki, Finland
