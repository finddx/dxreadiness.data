# Pathogen Diagnostics Readiness Index (PDxRI)

The [Pathogen Diagnostics Readiness Index (PDxRI)](https://www.finddx.org/data-and-impact/dashboards/diagnostic-readiness-index/#/) is a tool developed by FIND and designed to assess the availability and readiness of diagnostics for 20 high-risk pathogens globally. With gaps in diagnostic capabilities posing significant challenges to pandemic preparedness, the PDxRI provides an essential framework for identifying areas requiring urgent attention.

### Objective

The PDxRI serves as a resource for policymakers, global health organizations, donors, and diagnostic developers to prioritize and allocate resources effectively, ensuring that all countries are equipped to detect and respond to emerging pathogens promptly.

### Data source

FIND conducts thorough technology landscapes for multiple pathogens and diseases and publishes the data in a publicly available resource known as the [Test Directory](https://finddx.shinyapps.io/testdirexplorer_beta/). Using the data available from technology landscape for 20 pathogens and for 7 indicators, we constructed PDxRI framework and composite indicators as explained below in the methodology section.

### Methodology

### How to update data

Data source 1: The [Outbreaks Readiness score report](https://find.lightning.force.com/lightning/r/Report/00OVj0000003Xc9MAE/view?queryScope=userFolders) is updated by the Business Intelligence team. The most up to date report downloaded from Salesforce is stored in [this](https://github.com/finddx/dxreadiness.data/tree/main/outbreaks/data/salesforce) folder.

Data source 2: Information on availability of TPPs per disease and transmission mode is found in [this](https://github.com/finddx/dxreadiness.data/blob/main/outbreaks/data/TPP_outbreak_diseases.xlsx) file. This information is maintained by the Outbreaks team. Frequency of landscape updates (updated by BI team) is also found in the same file.

[Script 1](https://github.com/finddx/dxreadiness.data/blob/main/outbreaks/R/dx_readiness_data.R): This script joins the Salesforce report data with the information on TPPs, transmission mode and frequency of updates. It also generates 2 files containing:

1.  the iData table with all available data based on the indicators.

2.  the iMeta table with all the metadata based on the indicators.

In case one needs to add/remove/modify information on the pathogens or diseases included in the index, please take a look at both Data source 1 and Data source 2, run the Script 1 and see if data agrees across the 2 sources before creating the iData and iMeta tables.

In case one needs to add/remove/modify information on the indicators, they should take a look at Data source 1 and modify Script 1 at the sections relevant to the indicators.

Two different files are generated, one including COVID-19 and one excluding COVID-19.

[Script 2](https://github.com/finddx/dxreadiness.data/blob/main/outbreaks/R/get_indexes_COINr.R): This script generates 2 indexes: DX2 which includes COVID-19 and DX5 which excludes COVID-19. Detailed information on the choice of the above indexes and types of normalization, outlier treatment is given in [this](https://github.com/finddx/dxreadiness.data/blob/main/outbreaks/dx_readiness_index_COINr.html) report.
