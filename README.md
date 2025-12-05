# Parma: analisi di dati di contesto

## Purpose of the project

Studying mission-relevant socio-economic data on Fondazione Cariparma's area of reach: Parma and it province.

## Organizzazione del progetto
[core content files]

- `analysis/`: Compute-intensive Quarto documents with source data ingestions and intermediate artifact preparation (NOT rendered in website)
  - `00_carica_shp_situas.qmd`: Load ISTAT shapefiles and SITUAS data
  - `01_carica_cens.qmd`: Load census data
  - `02_base_maps.qmd`: Create base maps 
    - qui si potrebbe fare più mappe usando i dati per COM / PROV + censimento AGE (classi decennali), CITIZENSHIP, GENDER

- `dashboard/`: Presentation or Dashboard Quarto documents (rendered in website)
  - Organized by topic in subdirectories (e.g., `demographic_trends/`, `disability/`, `non_autosufficienza/`, `servsoc`, etc.)
  - Each topic folder contains:
    - `data_load.R`: Data loading scripts
    - `visualizations.R`: Visualization scripts  
    - `index.qmd`: Main dashboard document to present findings

- `data/`:
  - `data_in/`: Raw input data with metadata
    - `istat_shp_ITA/`: ISTAT shapefiles for ALL Italian administrative boundaries
    - `istat_ehis_2019/`: European Health Interview Survey (2019 data -- pubb in 2022)
    - `ISTAT_DISAB_CIFRE/`: Disability statistics from ISTAT
    - `ISTAT_SERVSOC`: Social services (SUPPLY SIDE) cube statistics from ISTAT (2002-2022 -- pubb in 2025) 
    - `ER_stats/`: Emilia-Romagna regional statistics
    - Excel files: demographic indicators, municipal classifications, foreign residents
  - `data_out/`: Processed `.rds` files from targets pipeline
    - `ER_shp/`: Emilia-Romagna shapefiles and municipal/provincial code vectors
    - `ITA_shp/`: Italy-level processed shapefiles
    - `LB_shp/`: Local/municipal level shapefiles
    - `istat_demo_2002_2024/`: Time series demographic indicators (2002-2024)
    - `istat_cens_2023/`: Processed 2023 population census data (AGE, GENDER, CITIZENSHIP) for ER/PR regions
    - `istat_EHIS_2019/`: Processed health survey data (ADL/IADL indicators) 
    - `istat_GALI_2023/`: GALI disability indicators (2023)
    - Processed utilities tables (`comuni_ita_info_redux_sf.rds`, `comuni_ita_info_sf_VARDESC.rds`, `istat_metadati_20251105.rds`)
  - `maps/`: Generated map outputs to feed into `dashboard/*/index.qmd` files
  - `plots/`: Generated plot outputs  

- `R/`: Function definitions (1 file = 1 function pattern)
  - `istat_*.R`: Functions for ISTAT data APIs and shapefile processing
  - `f_*.R`: Helper functions (formatting, plotting, mapping)
  - `utilities.R`: General utilities (save dataframes, etc.)

- `bib/`: Bibliography files for citations
  - `CRP_dash.bib`: Reference bibliography linked to Zotero

- `source/`: Documentation on data sources and methodologies
  - `demog-disab-data.qmd`: Documentation on demographic and disability data sources
  - `istat-data.qmd`: Documentation on ISTAT data sources and APIs

- `posts/`: Blog posts (rendered in website)
  - Quarto blog format for project updates and insights

## TODO

### Continuità info sui 10 assi dello strategico 24-27

  > ...quelli andranno tenuti sotto controll 

+ 🔴 servizi sociali-disabilità
+ 🟠 servizi sociali-anziani
+ 🔴 ricerca innovazione
+ ...


### Obiettivo di fondo per Piano Strategico del 2028 (da fare nel 2027)

  > D.A.: Quali sono delle criticità che emergono oggi che il PS 2024-27 non aveva? 


### Temi

#### DISABILITA'
> OIS: L’Assegno Unico Universale (AUU) è un sostegno economico per le famiglie con figli a carico, garantito a tutti i nuclei indipendentemente dalla condizione: (...) **per i figli con disabilità, il beneficio è senza limiti di età**  
▪ L’importo dell’assegno associato alla presenza di figli con disabilità risulta più contenuto rispetto al dato nazionale**i nuclei con figli con disabilità, percepiscono un importo medio mensile inferiore a quello nazionale e regionale**


+ è vero che qui non c'è la presa in carico? (Elena Saccenti)
  + [ReportER] ADI (x distretto)
  + [ReportER] SMAC Disabili ≠ SMAC Anziani 
  + [Inps] AUU - spaccato per "figli disabili"  
  + [Istat Esplora Dati] https://esploradati.istat.it/databrowser/#/it/dw/categories/IT1,Z0800SSW,1.0/SSW_SOCSE/DCIS_SPESESERSOC1

#### TREND DEMOGRAFICI 
+ **piramide età** `data/data_out/istat_pop_com_ER_2023_AGE.rds`(FACETED x comune di ER) [https://rfortherestofus.com/2024/07/population-pyramid-part-1](https://rfortherestofus.com/2024/07/population-pyramid-part-1)

+ Territori marginalizzati // Mappe di ....
  + 🟦 aggiungo layer `aree interne`
  + 🟦 aggiungo layer `comunità montane`
  + aggiungo layer `Distretti`

> Andrea: però OKKIO perchè se vuoi mostrare la corrispondenza tra bisogni e territori, devi tener presente che molto di quelli che diamo a Parma (e.g. Ospedale, Università) poi serve in realtà tutta la prov. quindi non ci sarà una corrispondenza... 

#### SANITA'
+ mobilità sanitaria
  + fetta di stranieri
  + in realta prima venivano di più di adesso... 
+ qualità 
+ liste d'attesa?
+ medici e infermieri? 
+ badanti che mancano dopo questa generazione non si troverannno più neanche quelle

#### POVERTÀ ABITATIVA
- case dlel'ospedale adesso vuote?
- è un problema sia di quantità che di qualità

#### LAVORO POVERO


