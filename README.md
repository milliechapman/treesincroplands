[![Build Status](https://travis-ci.com/milliechapman/treesincroplands.svg?token=DT1gCAtvxqVkbs12U7nX&branch=master)](https://travis-ci.com/milliechapman/treesincroplands)

## Authors:

- Millie Chapman, @milliechapman
- Wayne Walker
- Susan Cook-Patton
- Mary Farina
- Bronson Griscom
- Peter Ellis
- Alessandro Baccini

## Description:
While agroforestry is promoted as a promising climate mitigation opportunity, available estimates of the carbon sequestration potential are based on coarse assessments of both cropland extent and associated aboveground carbon density. Here we combine 30-meter resolution global maps of aboveground woody biomass, tree cover, and cropland extent, as well as a 1 km map of global pasture land, to estimate the potential contribution of agroforestry to climate change mitigation. We also update existing estimates of standing above ground biomass in croplands. 

## Analyses and Data
All spatial analysis was done in Google Earth Engine and described in `manuscript\trees_in_croplands.Rmd`. 

Code available: https://code.earthengine.google.com/0c9dbb9d921e1c219874ffceb912b796 (run time ~21hrs but depends on GEE computational allocation to task; access to layers may need to be requested)

`data\cropbio_density_hist.csv` and `data\pasturebio_density_hist.csv` used in `scripts\data_clean.R` are direct outputs from GEE.

### Common files

- `README.md` this file, a general overview of the repository in markdown format.  

### Infrastructure for Testing

- `.travis.yml`: A configuration file for automatically running [continuous integration](https://travis-ci.com) checks to verify reproducibility of all `.Rmd` notebooks in the repo.  If all `.Rmd` notebooks can render successfully, the "Build Status" badge above will be green (`build success`), otherwise it will be red (`build failure`).  
- `DESCRIPTION` a metadata file for the repository, based on the R package standard. It's main purpose here is as a place to list any additional R packages/libraries needed for any of the `.Rmd` files to run.
- `tests/render_rmds.R` an R script that is run to execute the above described tests, rendering all `.Rmd` notebooks. 
