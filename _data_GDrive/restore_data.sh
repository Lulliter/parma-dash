#!/bin/zsh
# backup-data.sh

# ===== BASIC STUFF =====
	#  $(...) = assegna il risultato a
	#  $HOME = ~ (ma portabile)
	# mkdir -p = crea la cartella (e tutte le cartelle parent se non esistono)

# ===== PROCESSO DA GoogleDrive A LOCALE =====
# cd ~/Github/R_farmacie

# --- Nome univoco del progetto/repo
PROJECT_NAME=$(basename "$PWD") # "R_farmacie"
# Path to GDrive Desktop folder
GDRIVE_BASE="$HOME/Library/CloudStorage/GoogleDrive-lmm76@georgetown.edu/Il mio Drive/R-progetti-data"
GDRIVE_REPO="$GDRIVE_BASE/$PROJECT_NAME"

# Check cartelle locali esistono
if [ ! -d "data_input" ] || [ ! -d "data_output" ]; then
    echo "Errore: cartelle data_input o data_output non trovate"
    exit 1
fi

# Check if Google Drive backup location exists
if [ ! -d "$GDRIVE_BASE" ]; then
    echo "Errore: cartella Google Drive non trovata: $GDRIVE_BASE"
    exit 1
fi

# Conferma
LAST_MOD=$(stat -f "%Sm" -t "%Y-%m-%d %H:%M" "$GDRIVE_REPO/data_input" 2>/dev/null)
echo "Ultimo backup: $LAST_MOD"
read "risposta?Restore? (y/n) "
[[ "$risposta" != "y" ]] && exit 0

# Restore (esclude README.md)
# OKKIO trailing "/" necessaria per copiare il contenuto delle cartelle
rsync -av --exclude='README.md' "$GDRIVE_REPO/data_input/" data_input/
rsync -av --exclude='README.md' "$GDRIVE_REPO/data_output/" data_output/

echo "Restore completato"

# ===== WORKFLOW COMPLETO =====
# (LA PPRIMA VOLTA SU UN COMPUTER NUOVO)
# chmod +x shell/backup_data.sh
# chmod +x shell/restore_data.sh
#
#
# --- MAC 1 (fine lavoro):
# 1. git add .
# 2. git commit -m "messaggio"
# 3. git push
# 4. ./shell//backup_data.sh
# 5. Attendi sync Google Drive (icona nella barra)
#
# --- MAC 2 (inizio lavoro):
# 1. git pull
# 2. ./shell/restore_data.sh
# 3. Lavora normalmente
# 4. Ripeti il ciclo quando torni su MAC 1
#
# IMPORTANTE:
# - README.md resta sempre tracciato da Git
# - I dati (csv, xlsx, Rds) vanno solo su Google Drive
# - Non lavorare contemporaneamente su entrambi i Mac
