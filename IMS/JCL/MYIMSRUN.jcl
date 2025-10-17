//MYIMSRUN  JOB FB3,,REGION=0M,NOTIFY=&SYSUID
//*
//*   MY IMS COBOL DL/I RUN JOB
//*
//SETLIB   JCLLIB ORDER=DFSF10.PROCLIB
//*
//IMSDLI   EXEC DLIBATCH,
//             MBR='IMSDBLAB',                   IMS DB LABS PROGRAM
//             PSB='PSB1CBL',                             MY OWN PSB
//             DBRC='N',                                   NO RECON!
//             IRLM='N'                                    NO LOCKS!
//*
//G.STEPLIB  DD DISP=SHR,DSN=&NODE2..&SYS2.SDFSRESL      IMS MODULES
//           DD DISP=SHR,DSN=&NODE1..&SYS2.PGMLIB           IMS PGMs
//           DD DISP=SHR,DSN=&SYSUID..IMS.PGMLIB             MY PGMs
//G.IMS      DD DISP=SHR,DSN=&SYSUID..IMS.PSBLIB             MY PSBs
//           DD DISP=SHR,DSN=&SYSUID..IMS.DBDLIB             MY DBDs
//           DD DISP=SHR,DSN=DFSF10.PSBLIB                  IMS PSBs
//           DD DISP=SHR,DSN=DFSF10.DBDLIB                  IMS DBDs
//*
//G.SKILL    DD DISP=SHR,DSN=&SYSUID..IMS.SKILL       MY DATA <VSAM>
//*
//G.DFSVSAMP DD DISP=SHR,DSN=DFSF10.PROCLIB(DFSVSM00)       BUFFERS!
//G.IEFRDER  DD DSN=&SYSUID..IMS.IMSLOG,                  MY IMS LOG
//             DISP=(,DELETE,DELETE),
//             UNIT=SYSDA,SPACE=(TRK,(10,5),RLSE),
//             DCB=(RECFM=VB,BLKSIZE=4096,LRECL=4092,BUFNO=5)
//G.SYSOUT   DD SYSOUT=*
//G.SYSPRINT DD SYSOUT=*
//G.SYSIN    DD *
*---------------------------------------------------------------
* LAB 1 INPUT
*---------------------------------------------------------------
1
GBU115   AKERSON, LAWRENCE R.                      1000
GED003   FERRIS, MONA J.                           1000
GKB750   AINLAY, HOWARD                            2220
GBC290   JAMORA, JOSEPH W.                         1000
*---------------------------------------------------------------
* LAST INPUT
*---------------------------------------------------------------
E
/*
