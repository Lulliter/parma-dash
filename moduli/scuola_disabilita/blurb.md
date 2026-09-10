# Alunni con disabilità nelle scuole statali della provincia di Parma

**Fonte:** USR Emilia-Romagna, fact sheet "Studenti e studenti con disabilità" (a.s. 2017/18 → 2024/25; scuole STATALI, infanzia inclusa; organico di fatto, ultimo a.s. provvisorio), trascritti in `dati/grezzi/istat_alunni_disab/usr_er_alunni_disab.csv`; ISTAT, "L'inclusione scolastica degli alunni con disabilità, a.s. 2024/25" (contesto nazionale e regionale, statali + paritarie)
**Anno dati:** a.s. 2016/17 → 2024/25 (USR-ER); a.s. 2014/15 → 2024/25 (ISTAT, serie nazionale)
**Ultimo aggiornamento:** 2026-09-10
**Output principali:** `plot_disab_trend_prov_er` (quota per provincia ER), `plot_disab_grado_pr_er` (Parma vs ER per grado, ultimo a.s.), `plot_disab_indice_pr_er` (numeri indice alunni vs alunni con disabilità); tabelle `disab_trend_prov_er`, `disab_grado_prov_er`, `disab_trend_italia`

# Messaggio

- **Gli alunni con disabilità crescono mentre la scuola non cresce.** Nelle scuole statali della provincia di Parma sono passati da ~1.500 (a.s. 2016/17) a ~2.150 (a.s. 2024/25), +41%, mentre gli alunni totali sono rimasti attorno a 54 mila: la quota è salita dal 2,8% al 3,9%.
- **Parma resta sotto la media regionale** (4,4% nel 2024/25) lungo tutta la serie, con un andamento parallelo alle altre province: la dinamica è regionale e nazionale (ISTAT, tutte le scuole: dal 2,6% del 2014/15 al 4,8% del 2024/25).
- **La concentrazione è nel primo ciclo**: a Parma primaria (4,8%) e secondaria di I grado (4,5%) hanno quote più alte di infanzia (3,1%) e secondaria di II grado (3,2%).
- **Avvertenza di lettura**: il dato conta le certificazioni, non la prevalenza delle condizioni. Più certificazioni possono voler dire più riconoscimento e più sostegno, non necessariamente più bisogno; vanno letti come un segnale di fragilità *potenziale* della popolazione scolastica. Due ipotesi da verificare con altre fonti: il peso degli alunni con background migratorio (difficoltà linguistiche e certificazione possono confondersi; USR-ER non incrocia i due dati) e un effetto post-pandemia (a Parma calo nel 2020/21, poi +7% nel 2021/22 e +13% nel 2023/24; ma la crescita era già in atto prima del 2020).
- **Contesto regionale sul sostegno (ISTAT 2024/25)**: in Emilia-Romagna il 30,8% degli insegnanti di sostegno proviene dalle liste curricolari, senza specializzazione (Italia 22,1%); il 7,2% non era ancora assegnato al 10 ottobre (Italia 10,2%); rapporto alunni/assistenti all'autonomia 4,5 (Italia 4,1). Gli alunni con BES non riconducibili a disabilità sono il 12,0% (Italia 9,0%).

# Note

- USR-ER e ISTAT non sono confrontabili tra loro: la prima copre solo le statali, la seconda anche le paritarie.
- Dettaglio per grado disponibile solo per gli a.s. 2016/17, 2017/18, 2020/21, 2021/22, 2022/23, 2024/25.
