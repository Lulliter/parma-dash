# Data Backup and Restore via Google Drive

Process to keep track of heavy data files when:

- 1. I work on projects from 2 different computers (e.g., work and home)
- 2. I want to keep the repo size small on GitHub by not pushing heavy/private data files (so ignore them in `.gitignore`)
- 3. I use Google Drive to sync heavy files between my computers


Steps:

- 1. Work normally on Mac 1 and then run `backup_data.sh` to copy heavy files from the repo data folder to Google Drive
- 2. On Mac 2, run `restore_data.sh` to copy heavy files from Google Drive to the repo data folder

Details:

- 1. Heavy files are ignored in `.gitignore` to keep the repo size small on GitHub
- 2. Heavy files are stored in Google Drive folder `~/Library/CloudStorage/GoogleDrive-lmm76@georgetown.edu/Il mio Drive/R-progetti-data/<REPO NAME>`
 