# sito/

Spazio di composizione: il sito Quarto **legge soltanto** gli output dei moduli
(`moduli/*/output/` e i relativi `blurb.md`) e li combina in pagine per tema
(`temi/`). Qui non si fa calcolo né pulizia dati.

`_quarto.yml` renderizza solo `index.qmd` (root) e `sito/**`: le voci di navbar
si aggiungono man mano che le pagine in `temi/` vengono create (punto 6 del piano).

Struttura di ogni pagina di `temi/` (deciso 2026-09-10; il sito è pubblico e in costruzione: deve far capire dove va il lavoro e raccogliere feedback):

- `## In sintesi`: 3-4 punti, i messaggi principali
- una sezione per domanda (quanti, chi, dove...): sotto il titolo 1-2 frasi col messaggio, non il metodo
- ogni grafico: bottoni di scarico + callout `Osservazioni` con il commento dettagliato (linguaggio riusabile in altri report) e, in coda, le note di metodo in corsivo `_NB: ..._`
- `## Possibili approfondimenti`: le domande aperte, scritte per il pubblico; fonti, file e percorsi operativi stanno in `_TODO.qmd`, non in pagina
- massima sintesi: niente ripetizioni tra sintesi, testo di sezione e osservazioni; grassetti con parsimonia; niente sottotitoli `### nome_grafico {.notitle}` (le ancore sono i label dei chunk)
