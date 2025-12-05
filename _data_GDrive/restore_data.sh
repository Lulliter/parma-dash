#!/bin/zsh
# restore-data.sh

# ===== BASIC STUFF =====
	#  $(...) = assegna il risultato a
	#  $HOME = ~ (ma portabile)
	# mkdir -p = crea la cartella (e tutte le cartelle parent se non esistono)

# ===== PROCESSO DA GoogleDrive A LOCALE =====
# cd ~/Github/R_farmacie

# --- Default directories (can be overridden)
DIR_INPUT="data/data_in"
DIR_OUTPUT="data/data_out"

# --- Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --input|-i)
            DIR_INPUT="$2"
            shift 2
            ;;
        --output|-o)
            DIR_OUTPUT="$2"
            shift 2
            ;;
        --help|-h)
            echo "Uso: $0 [opzioni]"
            echo ""
            echo "Opzioni:"
            echo "  --input, -i DIR    Directory input locale (default: data/data_in)"
            echo "  --output, -o DIR   Directory output locale (default: data/data_out)"
            echo "  --help, -h         Mostra questo messaggio"
            echo ""
            echo "Esempio:"
            echo "  $0                              # Usa i default"
            echo "  $0 -i data_input -o data_output # Directory custom"
            exit 0
            ;;
        *)
            echo "Opzione sconosciuta: $1"
            echo "Usa --help per vedere le opzioni disponibili"
            exit 1
            ;;
    esac
done

# --- Nome univoco del progetto/repo
PROJECT_NAME=$(basename "$PWD") # "R_farmacie"
# Path to GDrive Desktop folder
GDRIVE_BASE="$HOME/Library/CloudStorage/GoogleDrive-lmm76@georgetown.edu/Il mio Drive/R-progetti-data"
GDRIVE_REPO="$GDRIVE_BASE/$PROJECT_NAME"

# Check cartelle locali esistono
if [ ! -d "$DIR_INPUT" ] || [ ! -d "$DIR_OUTPUT" ]; then
    echo "Errore: cartelle $DIR_INPUT o $DIR_OUTPUT non trovate"
    exit 1
fi

# Check if Google Drive backup location exists
if [ ! -d "$GDRIVE_BASE" ]; then
    echo "Errore: cartella Google Drive non trovata: $GDRIVE_BASE"
    exit 1
fi

# Conferma
LAST_MOD=$(stat -f "%Sm" -t "%Y-%m-%d %H:%M" "$GDRIVE_REPO/$DIR_INPUT" 2>/dev/null)
echo "Ultimo backup: $LAST_MOD"

# Conflict detection
echo "Controllo conflitti..."
CONFLICTS=$(rsync -aun --itemize-changes --exclude='README.md' \
            "$GDRIVE_REPO/$DIR_INPUT/" "$DIR_INPUT/" \
            "$GDRIVE_REPO/$DIR_OUTPUT/" "$DIR_OUTPUT/" \
            2>/dev/null | grep '^\.f')

if [[ -n "$CONFLICTS" ]]; then
    echo "⚠️  ATTENZIONE: file locali più recenti di Google Drive (NON saranno sovrascritti):"
    echo "$CONFLICTS" | sed 's/^/  /'
    echo ""
    echo "Questi file locali sembrano più recenti del backup su Google Drive."
    echo "Possibile che tu abbia dimenticato di fare backup dall'altro Mac?"
fi

read "risposta?Procedere con restore? (y/n) "
[[ "$risposta" != "y" ]] && exit 0

# Restore (esclude README.md)
# OKKIO trailing "/" necessaria per copiare il contenuto delle cartelle
# --update (-u) = copia solo se il file di Google Drive è più recente del file locale
# --itemize-changes (-i) = mostra dettagli di ogni file (anche quelli saltati)
echo "$GDRIVE_REPO/$DIR_INPUT → $DIR_INPUT"
echo "$GDRIVE_REPO/$DIR_OUTPUT → $DIR_OUTPUT"
rsync -avu --itemize-changes --exclude='README.md' "$GDRIVE_REPO/$DIR_INPUT/" "$DIR_INPUT/"
rsync -avu --itemize-changes --exclude='README.md' "$GDRIVE_REPO/$DIR_OUTPUT/" "$DIR_OUTPUT/"

echo "Restore completato"

# ===== WORKFLOW COMPLETO =====
# (LA PPRIMA VOLTA SU UN COMPUTER NUOVO)
# chmod +x _data_GDrive/backup_data.sh
# chmod +x _data_GDrive/restore_data.sh
#
#
# --- MAC 1 (fine lavoro):
# 1. git add .
# 2. git commit -m "messaggio"
# 3. git push
# 4. ./_data_GDrive/backup_data.sh
# 5. Attendi sync Google Drive (icona nella barra)
#
# --- MAC 2 (inizio lavoro):
# 1. git pull
# 2. ./_data_GDrive/restore_data.sh
# 3. Lavora normalmente
# 4. Ripeti il ciclo quando torni su MAC 1
#
# IMPORTANTE:
# - README.md resta sempre tracciato da Git
# - I dati (csv, xlsx, Rds) vanno solo su Google Drive
# - Non lavorare contemporaneamente su entrambi i Mac
