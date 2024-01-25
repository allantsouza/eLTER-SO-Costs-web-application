# [eLTER](https://elter-ri.eu/) Cost Calculation Tool

![](./www/elter_logo.jpg)

## Introduction

Welcome to the repository for the [eLTER](https://elter-ri.eu/) Cost Calculation Tool, a specialized resource designed for the [eLTER (European Long-Term Ecosystem Research)](https://elter-ri.eu/) network. This tool is designed to assist in estimating the costs associated with implementing standard observations across various [eLTER](https://elter-ri.eu/) sites.

The essence of this tool lies in its ability to adapt calculations based on unique site-specific criteria, ensuring accurate and tailored cost estimations. Key factors taken into consideration include the site category, the range of habitats present, spheres of specialization, and the potential co-location with other Research Infrastructures, which cover the costs of specific standard observations.

Our goal is to provide a user-friendly, efficient, and reliable means for [eLTER](https://elter-ri.eu/) researchers and administrators to plan and allocate resources effectively. Through this tool, we aim to support the [eLTER](https://elter-ri.eu/) community in advancing ecological and environmental research across Europe.

Feel free to explore, contribute, and utilize this tool for your [eLTER](https://elter-ri.eu/) site planning and management needs.


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
