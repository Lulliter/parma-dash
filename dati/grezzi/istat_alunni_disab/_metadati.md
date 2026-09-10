# 1. ISTAT_alunni-con-disabilita-as.-2024-25.xlsx

**Ente/fonte:** ISTAT — L'inclusione scolastica degli alunni con disabilità
**URL / API:** https://www.istat.it/comunicato-stampa/linclusione-scolastica-degli-alunni-con-disabilita-anno-scolastico-2024-2025/
**Come riscaricare:** tavole allegate al comunicato annuale (uscita di norma a inizio anno per l'a.s. precedente)
**Data ultimo download:** 2026-07-04
**Periodo coperto:** a.s. 2024-25
**Unità territoriale:** nazionale (Tavola 1, serie storica per ordine) / regioni (Tavole 2-12) / ripartizioni (Tavole 13-16); NESSUN dettaglio provinciale
**Licenza:** CC BY 4.0 (ISTAT)

**File:**

- `ISTAT_alunni-con-disabilita-as.-2024-25.xlsx` — Tavole 1-7 + foglio `Meta` (fonte e data accesso annotate dentro il file)

**Note/insidie:** serie annuale → il prossimo a.s. arriva con un nuovo comunicato;
verificare continuità delle tavole tra edizioni.

**Storico aggiornamenti:**

- 2026-07-04 prima acquisizione (a.s. 2024-25)

# 2. UffScuola_RER_anno2024-25.pdf

**Ente/fonte:** Ufficio scolastico regionale per l'Emilia-Romagna - Ministero dell'Istruzione e del Merito
**URL / API:** https://www.istruzioneer.gov.it/dati/ 
**Come riscaricare:** (pagina `Fact Sheet`) - Anno Scolastico - 2. Studenti e studenti con disabilità - 2024-25 - PDF
**Data ultimo download:** 2026-09-10
**Periodo coperto:** a.s. 2016/17 → 2024/25 (ogni fact sheet riporta l'a.s. corrente, provvisorio, e il precedente, definitivo); dettaglio per grado solo in alcune edizioni
**Unità territoriale:** province ER (+ totale regionale)
**Licenza:**  

**File:** 

- da `UffScuola_RER_fsheet-disab_anno2017_18.pdf` a `UffScuola_RER_fsheet-disab_anno2024-25.pdf` — Fact Sheet studenti e studenti con disabilità, scuola STATALE (mancano le edizioni 2019/20 e 2023/24, recuperate come a.s. "precedente" dei fact sheet successivi)
- `usr_er_alunni_disab.csv` — trascrizione delle tabelle dei pdf (1 riga = a.s. × provincia × grado; colonne `tipo_dato` provvisorio/definitivo e `file_fonte`), fatta il 2026-09-10 con `pdftotext` + controlli di quadratura (somma province = totale ER; somma gradi = totale). È l'input di `moduli/scuola_disabilita/01_dati.R`

**Note/insidie:** solo scuole statali (le paritarie non ci sono); fino al 2018/19 le tabelle dicono "alunni con handicap"; nel pdf 2024-25 i numeri di Bologna e Totale sono spezzati su più righe (trascritti a mano nel csv). Al prossimo aggiornamento: aggiungere le righe del nuovo fact sheet al csv (a.s. nuovo = provvisorio, a.s. precedente = definitivo)

**Storico aggiornamenti:**

- 2026-09-10 prima acquisizione (fact sheet 2017/18 → 2024/25) e trascrizione in csv
