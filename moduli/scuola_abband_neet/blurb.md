# Giovani a rischio dispersione e NEET nella provincia di Parma

**Fonte:** ISTAT, Bes dei territori (ed. 2025; tavole provinciali per sesso): NEET 15-29 (stime da indagine campionaria, la Rilevazione sulle forze di lavoro; 2018→2024) e competenze alfabetica/numerica non adeguate in III media (di fonte INVALSI, dato censuario, 2018→2024); MIM, Portale unico dei dati della scuola: iscritti per anno di corso e fascia d'età (a.s. 2015/16→2024/25, statali + paritarie, no infanzia) → ritardo scolastico
**Anno dati:** 2018→2024 (BES); a.s. 2015/16→2024/25 (MIM)
**Ultimo aggiornamento:** 2026-09-10
**Output principali:** `plot_neet_prov_er`, `plot_competenze_prov_er`, `plot_ritardo_corso_pr_er`, `plot_ritardo_trend_pr_er`, `mappa_ritardo_sec1_comuni_pr` (+ tabelle `bes_istruzione_prov_er`, `ritardo_trend_prov_er`, `ritardo_corso_prov_er`, `ritardo_comuni_pr`)

# Messaggio

- **Quanti sono i NEET**: nel 2024 a Parma il 10,9% dei 15-29enni non studia e non lavora (ER 9,6%, Italia 15,2%). Dal picco pandemico (16% nel 2020-21) la quota si è quasi dimezzata, come nel resto del Nord; il rimbalzo 2024 su Parma (dall'8,4% del 2023) sta dentro l'errore campionario e va letto con cautela.
- **I segnali di rischio arrivano prima, in terza media**: a Parma il 39,1% degli studenti di III media non raggiunge un livello adeguato in italiano e il 41,1% in matematica (2024), sopra la media regionale (37,9% e 38,5%) e in crescita dal 2018 (31,5% e 34,0%). Divario di genere netto: i maschi peggio in italiano (44,9%), le femmine in matematica (42,4%).
- **Il ritardo scolastico è già in ingresso alle superiori**: gli alunni con un'età superiore a quella regolare sono il 2,8% alla primaria, l'8,6% alle medie e il 19,4% alle superiori (a.s. 2024/25), sempre un po' sopra la media regionale. Il salto avviene tra la terza media (10%) e la prima superiore (~20%: bocciature del primo anno e inserimenti tardivi); poi la quota non cresce più e in quinta scende al 17,7%, perché chi era in ritardo esce dal percorso: dalla prima alla quinta gli iscritti calano di un terzo (5.001 → 3.334 a Parma) per fine dell'obbligo a 16 anni, passaggi alla formazione professionale (IeFP, fuori dal perimetro MIM) e abbandoni, che i dati di stock non distinguono.
- **Nel tempo il ritardo cala** (alle superiori era il 22% nel 2015/16) mentre le competenze non adeguate salgono: meno bocciature non vuol dire più apprendimento. È il fenomeno che INVALSI chiama "dispersione implicita". Gradino nel 2020/21: a fine 2019/20 ammessi tutti per ordinanza (Covid). Dopo, alle medie Parma risale dal 6,4% all'8,6% contro il 6,8% regionale: forbice PR-ER riaperta, da approfondire (inserimenti dall'estero?).
- **Dove**: alle medie il ritardo è più alto nei comuni di montagna e della pedecollina a forte presenza straniera (Fornovo 24%, Langhirano 16,5%; i piccoli comuni appenninici hanno quote alte ma su poche decine di alunni). Il comune è quello della scuola, non della residenza.

# Note

- Ritardo scolastico = età al 31/12 superiore a quella regolare per l'anno di corso; include ripetenze e inserimenti in classi inferiori (frequenti per chi arriva dall'estero); NON conta chi ha già abbandonato. Esclusi corsi serali (percorsi di II livello), CPIA, sedi carcerarie e ospedaliere, tutti "in ritardo" per costruzione (a Parma ~530 alunni nei serali). Le paritarie di recupero anni non sono identificabili dall'anagrafe e restano dentro.
- L'uscita precoce dal sistema formativo (ELET 18-24) nel Bes dei territori non è provinciale: resta regionale, tra gli approfondimenti.
- Il calo degli iscritti lungo le superiori non è misurabile come abbandono con questi dati: serve un dato longitudinale (anagrafe studenti regionale + iscritti IeFP), vedi approfondimenti in pagina.
- NEET: stime campionarie; per Parma l'errore relativo è ampio, confrontare i trend più che i singoli anni.
