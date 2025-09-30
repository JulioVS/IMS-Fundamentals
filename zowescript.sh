# IBM Z XPLORE *IMS* BACKUP
# Zowe CLI commands to download files from z/OS

# CLEANUP
rm -r IMS

# TSO/MVS LIBRARIES (PDS)
zowe files download am "Z45864.IMS.DBDSRC"  -e ".dbd" --po
zowe files download am "Z45864.IMS.JCL"     -e ".jcl" --po
zowe files download am "Z45864.IMS.PGMSRC"  -e ".cbl" --po
zowe files download am "Z45864.IMS.PROCLIB" -e ".jcl" --po
zowe files download am "Z45864.IMS.PSBSRC"  -e ".pcb" --po

# RENAME
mv Z45864/IMS .
rm -r Z45864

# LAST BACKUP => 30.09.2025.-

# RUN THIS AS: ./zowescript.sh
