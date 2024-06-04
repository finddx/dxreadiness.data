library(COINr)
library(readxl)
library(openxlsx)
library(purrr)


iData <- readxl::read_excel("./data/dx_readiness_comp_data_20240514.xlsx", sheet = "iData")
iMeta <- readxl::read_excel("./data/dx_readiness_comp_data_20240514.xlsx", sheet = "iMeta")



# basic checks on these tables
check_iData(iData)
check_iMeta(iMeta)

DX <- new_coin(iData, iMeta)

# Get index with winsorisation for COVID-19 and min-max normalisation --------


DX2 <- DX


DX2 <- qTreat(DX2, dset = "Raw")

# check what happened
DX2$Analysis$Treated$Dets_Table |>
  round_df(2)


DX2 <- Normalise(DX2, dset = "Treated")
DX2 <- Aggregate(DX2, dset = "Normalised")

get_results(DX2, dset = "Aggregated", also_get = "uName") |>
  knitr::kable()

plot_bar(DX2, dset = "Aggregated", iCode = "dx_readiness_index",
         uLabel = "uName", flip_coords = TRUE, axes_label = "iName")

dx2 <- append(DX2$Data, DX2$Meta)
dx2[['maxlev']] <- NULL

dx2 <- list(Raw = dx2$Raw, Treated = dx2$Treated, Normalised = dx2$Normalised,
Aggregated = dx2$Aggregated, Ind = dx2$Ind, Lineage = dx2$Lineage,
Unit = dx2$Unit)

writexl::write_xlsx(dx2, path = "results/dxri_20240603.xlsx")



# Get index removing COVID-19 and min-max normalisation - !!!! there is no treatment !!!! --------

# remove COVID row.
iData_ <- iData[iData$uCode != "u_3", ]

# rebuild coin
DX5 <-  new_coin(iData_, iMeta)

# normalise and aggregate
DX5 <- Normalise(DX5, dset = "Raw")
DX5 <- Aggregate(DX5, dset = "Normalised")
get_results(DX5, dset = "Aggregated", also_get = "uName") |>
  knitr::kable()

plot_bar(DX5, dset = "Aggregated", iCode = "dx_readiness_index",
         uLabel = "uName", flip_coords = TRUE, axes_label = "iName")


dx5 <- append(DX5$Data, DX5$Meta)
dx5[['maxlev']] <- NULL

dx5 <- list(Raw = dx5$Raw, Normalised = dx5$Normalised,
            Aggregated = dx5$Aggregated, Ind = dx5$Ind, Lineage = dx5$Lineage,
            Unit = dx5$Unit)

writexl::write_xlsx(dx5, path = "results/dxri_wo_COVID_20240603.xlsx")

