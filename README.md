# R8WD


## What R8WD does

This R package provides tools for working with Water Quality Portal (WQP) data to summarize quality control measures. The goal of this effort is to reduce the barriers between water quality data and people seeking to gain insight from it. This is useful for organizations seeking to assess whether their quality control data meet the needs of their project.


## How users can get started with the project

Pull requests, bug reports, and all other forms of contribution are welcomed and highly encouraged. If you are interested in contributing to development of R8WD, please see our [Contributing Policy](https://github.com/USEPA/R8WD/blob/main/CONTRIBUTING.md).


## Where users can get help with the project

For help using project code, consult R documentation files accessible through the command line: 

```R
?create_qc_visuals
```



## Disclaimer

The United States Environmental Protection Agency (EPA) GitHub project code is provided on an "as is" basis and the user assumes responsibility for its use. EPA has relinquished control of the information and no longer has responsibility to protect the integrity, confidentiality, or availability of the information. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity by EPA or the United States Government. 



## Installation

The R package can be installed from GitHub using the `remotes` package:

```R
if(!'remotes' %in% installed.packages()) install.packages('remotes')

remotes::install_github("USEPA/R8WD")
```


## A brief example

After installing `R8WD`, it can be loaded and a quarto report generated. A list of organization names available in the Water Quality Portal is available in this csv: https://cdx.epa.gov/wqx/download/DomainValues/Organization.CSV. Note that a given organization may not have data available for the time period or parameters selected.

```R
# load the package
library(R8WD)

# generate a report
org_name <- 'BLCKFEET'
params   <- c("Total Phosphorus, mixed forms","Total Nitrogen, mixed forms","Escherichia coli","Dissolved oxygen (DO)")

create_report(org = org_name, parameters = params)
```

This will create and open a quarto document in RStudio. Render the quarto document to html output by pressing `Ctrl+Shift+K`.

