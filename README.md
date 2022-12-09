# GardenHub
### A repository for the common garden experiment data analyses
#### Last update: 07/12/2022 by Madi 

******

### Repository structure:
The repository will be subdivided into the following folders: 

- #### [data](https://github.com/ShrubHub/GardenHub/tree/main/data):
  - common_garden: one sub-folder per year 2015-2021.
      - For all years common garden measurements: [all_merged_data_2022.csv](https://github.com/ShrubHub/GardenHub/blob/main/data/common_garden_data_2022/all_cg_data_2022csv)
  - [phenology](https://github.com/ShrubHub/GardenHub/tree/main/data/phenology): including ground-truthing sheets (2022), add phenocam spreadsheets when done (only missing CG ones)
  - [source_pops](https://github.com/ShrubHub/GardenHub/tree/main/data/source_pops): all source population data (2017-2022)
      - For all years source population SLA, LA, LDMC: [all_source_area_traits.csv](https://github.com/ShrubHub/GardenHub/blob/main/data/source_pops/all_source_area_traits.csv)
            - For all maternal population height, width, stem elongation, leaf length, stem diameter (note: no species IDs, can be found by matching Standard IDs):  [mother_data.csv](https://github.com/ShrubHub/GardenHub/blob/main/data/source_pops/mother_data.csv)
      - For all years source (and maternal) population height, width, stem elongation, leaf length, stem diameter:  [unique_source_mother.csv](https://github.com/ShrubHub/GardenHub/blob/main/data/source_pops/unique_source_mother.csv)


  - [tomst](https://github.com/ShrubHub/GardenHub/tree/main/data/tomst): TOMST for common garden and source populations 2021-2022
  - [hobo](https://github.com/ShrubHub/GardenHub/tree/main/data/hobo): HOBO data in the common garden 2013-2021
  
  
- #### [scripts](https://github.com/ShrubHub/GardenHub/tree/main/scripts):
  - [common_garden](https://github.com/ShrubHub/GardenHub/tree/main/scripts/common_garden)
  - [source_populations] organization and merging of data from source population trait measurements (Madi collected 2021-22 and previous TS data): (https://github.com/ShrubHub/GardenHub/tree/main/scripts/source_populations)
  - [methods_models]  (https://github.com/ShrubHub/GardenHub/tree/main/scripts/methods_models)
  - phenology: 
  - [hobo](https://github.com/ShrubHub/GardenHub/tree/main/scripts/hobo)
  - [tomst](https://github.com/ShrubHub/GardenHub/tree/main/scripts/tomst)
  
- #### [figures]

- #### [documents](https://github.com/ShrubHub/GardenHub/tree/main/documents)

*****

#### Requirements
- `RStudio` version 1.2.5001 or greater
- packages `ggplot2`...

#### Feedback Etiquette

- Please use either an issue or a pull request.
- Please use "###" and your initial before your feedback comments.

 
