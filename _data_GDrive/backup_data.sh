#!/bin/zsh
# backup-data.sh

# ===== BASIC STUFF =====
	#  $(...) = assegna il risultato a
	#  $HOME = ~ (ma portabile)
	# mkdir -p = crea la cartella (e tutte le cartelle parent se non esistono)

# ===== PROCESSO DA LOCALE A GoogleDrive =====
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

# Crea cartelle backup (mirroring exact structure)
mkdir -p "$GDRIVE_REPO/$DIR_INPUT"
mkdir -p "$GDRIVE_REPO/$DIR_OUTPUT"

# Conflict detection
echo "Controllo conflitti..."
CONFLICTS=$(rsync -aun --itemize-changes --exclude='README.md' \
            "$DIR_INPUT/" "$GDRIVE_REPO/$DIR_INPUT/" \
            "$DIR_OUTPUT/" "$GDRIVE_REPO/$DIR_OUTPUT/" \
            2>/dev/null | grep '^\.f')

if [[ -n "$CONFLICTS" ]]; then
    echo "⚠️  ATTENZIONE: file locali più vecchi di Google Drive (NON saranno copiati):"
    echo "$CONFLICTS" | sed 's/^/  /'
    echo ""
    echo "Questi file su Google Drive sono più recenti della tua versione locale."
    read "risposta?Continuare il backup (salta questi file)? (y/n) "
    [[ "$risposta" != "y" ]] && exit 0
fi

# Backup (esclude README.md)
# OKKIO trailing "/" necessaria per copiare il contenuto delle cartelle
# --update (-u) = copia solo file locali più recenti di Google Drive
echo "Backup in corso (solo file più recenti)..."
echo "  $DIR_INPUT → $GDRIVE_REPO/$DIR_INPUT"
echo "  $DIR_OUTPUT → $GDRIVE_REPO/$DIR_OUTPUT"
rsync -avu --itemize-changes --exclude='README.md' "$DIR_INPUT/" "$GDRIVE_REPO/$DIR_INPUT/"
rsync -avu --itemize-changes --exclude='README.md' "$DIR_OUTPUT/" "$GDRIVE_REPO/$DIR_OUTPUT/"

echo "Backup completato: $(date '+%Y-%m-%d %H:%M')"

# ===== WORKFLOW COMPLETO =====
# (LA PPRIMA VOLTA SU UN COMPUTER NUOVO)
# chmod +x _data_GDrive/backup_data.sh
# chmod +x _data_GDrive/restore_data.sh
#
# USO:
# ./backup_data.sh                              # Default: data/data_in e data/data_out
# ./backup_data.sh -i data_input -o data_output # Directory custom (altri progetti)
# ./backup_data.sh --help                       # Mostra tutte le opzioni
#
# --- MAC 1 (fine lavoro):
# 1. git add .
# 2. git commit -m "messaggio"
# 3. git push
# 4. ./_data_GDrive/backup_data.sh
# 5. Attendi sync Google Drive (icona nella barra) prima di passare a MAC 2
#
# --- MAC 2 (inizio lavoro):
# 1. git pull
# 2. ./_data_GDrive/restore_data.sh
# 3. Lavora normalmente
# 4. Ripeti il ciclo di `git commit + push | GD backup` (come in MAC 1)
#
# IMPORTANTE:
# - README.md resta sempre tracciato da Git
# - I dati (csv, xlsx, Rds) vanno solo su Google Drive
# - Non lavorare contemporaneamente su entrambi i Mac
