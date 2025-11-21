# Parma analisi di contesto

## Organizazione del progetto

+ Il progetto usa il pacchetto `targets` per organizzare il flusso di lavoro nelle cartelle con lavoro più tecnico e "cumpute-intensive", ad esempio `map/*.qmd` che funzionano grazie alle funzioni definite in `R/*.R`.

+ Invece il resto dei file `.qmd` (in root o in cartelle tipo `dashnoard/`) sono documenti di analisi, report, dashboard, ecc che usano il workflow solito di un `quarto` website..


## TODO

+ Mappe (Qui vedo i dati del **2023** placed-based a livello comune)
  + `analysis/_02_base_maps.qmd` usando 
    - `data/data_out/comuni_ita_info_redux_sf.rds` JOINED con 
    - `data/data_out/istat_pop_com_ER_2023_AGE.rds` | ... `CITIZENSHIP` |
  + Capire se posso renderle interattive in modo piu facile (es `ggiraph`) tipo visualizzare il comune
  + 🟦 aggiungo layer aree interne
  + 🟦 aggiungo layer comunità montane

+ statistiche in **serie storica 2002-2024** 
  + cercare correlazione/discontinuita tra Parma / ER / Italia nei trend 

+ **piramide età** `data/data_out/istat_pop_com_ER_2023_AGE.rds`(FACETED x comune di ER) [https://rfortherestofus.com/2024/07/population-pyramid-part-1](https://rfortherestofus.com/2024/07/population-pyramid-part-1)


## Introduzione


## FASE I) PREPARAZIONE 

1. Fissare gli Obiettivi 

+ INGREDIENTS 
  + good questions
  + relevant data
  + insightlul analysis
  
<!--First off, every project in data science has a customer. | you need to ask good questions about their data.  -->
<!-- INGREDIENTS 
  + good questions
  + relevant data
  + insightlul analysis -->
 
2. Esplorazione Dati disponibili

<!-- OPTIONS 
  + Flat Files (csv, tsv), HTML, XML, JSON, Relational Databases, Non-Relational Databases, APIs
-->

3. Pulizia Dati

4. Valutazione Dati
<!-- Without a preliminary assessment (the 4th step), you may run into problems with outliers, biases, precision, specificity, or any number of other inherent aspects of the data. In order to uncover these and get to know the data better, the first step of post-wrangling data analysis is to calculate some descriptive statistics. -->

## FASE II) PROGETTAZIONE

5. Sviluppo piano
<!-- You know where you’d like to go and a few ways to get there, but at every intersection there might be a road closed, bad traffic, or pavement that’s pocked and crumbling. You’ll have to make decisions as you arrive at these obstacles, but for now it’s enough to have a backup plan or two.
 -->

6. Analisi dei dati


7. Costruzioe software (PoC)

<!-- If statistics is the framework for analyzing and drawing conclusions from the data, then software is the tool that puts this framework in action. 
 -->

8. Ottimizzazione
<!-- The 8th step in our process is to optimize a product with supplementary software. The software tools in our 7th step can be versatile, but they’re statistical by nature. Software can do much more than statistics. In particular, many tools are available that are designed to store, manage, and move data efficiently. -->

## Risultati

<!--  -->


## TODO

