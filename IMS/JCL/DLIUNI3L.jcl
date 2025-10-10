//DLIUNI3L  JOB FB3,,REGION=0M,NOTIFY=&SYSUID
//*
//*   IMS DL/I TEST PROGRAM 'DFSDDLT0'
//*
//SETLIB   JCLLIB ORDER=DFSF10.PROCLIB
//*
//IMSDLI   EXEC DLIBATCH,
//             MBR='DFSDDLT0',                     DL/I TEST PROGRAM
//             PSB='PSBLOAD',                          MY COURSE PSB
//             DBRC='N',                                   NO RECON!
//             IRLM='N'                                    NO LOCKS!
//*
//G.IMS      DD DISP=SHR,DSN=&SYSUID..IMS.PSBLIB             MY PSBs
//           DD DISP=SHR,DSN=&SYSUID..IMS.DBDLIB             MY DBDs
//           DD DISP=SHR,DSN=DFSF10.PSBLIB                  IMS PSBs
//           DD DISP=SHR,DSN=DFSF10.DBDLIB                  IMS DBDs
//*
//G.COURSE   DD DISP=SHR,DSN=&SYSUID..IMS.COURSE      MY DATA <VSAM>
//*
//G.DFSVSAMP DD DISP=SHR,DSN=DFSF10.PROCLIB(DFSVSM00)       BUFFERS!
//G.IEFRDER  DD DSN=&SYSUID..IMS.IMSLOG,                  MY IMS LOG
//             DISP=(,DELETE,DELETE),
//             UNIT=SYSDA,SPACE=(TRK,(10,5),RLSE),
//             DCB=(RECFM=VB,BLKSIZE=4096,LRECL=4092,BUFNO=5)
//*
//G.PRINTDD  DD SYSOUT=*                                   FOR DLT0!
//G.SYSIN    DD DISP=SHR,DSN=&SYSUID..IMS.DLIIN(U3LOAD1)       INPUT
//*
//* //G.SYSIN    DD *
//* S 1 1 1 1 1    COURSE
//* U
//* L        ISRT  COURSE
//* L        DATA  CM17
//* /*
