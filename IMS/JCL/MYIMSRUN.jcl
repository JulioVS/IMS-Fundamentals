//MYIMSRUN  JOB FB3,,REGION=0M,NOTIFY=&SYSUID
//*
//*   IMS DB APP PROGRAMMING SKELETON
//*
//SETLIB   JCLLIB ORDER=DFSF10.PROCLIB
//*
//IMSDLI   EXEC DLIBATCH,
//             MBR='SKELETON',                     DL/I TEST PROGRAM
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
//G.DUMMY    DD DISP=SHR,DSN=&SYSUID..IMS.DUMMY       MY DATA <VSAM>
//*
//G.DFSVSAMP DD DISP=SHR,DSN=DFSF10.PROCLIB(DFSVSM00)       BUFFERS!
//G.IEFRDER  DD DSN=&SYSUID..IMS.IMSLOG,                  MY IMS LOG
//             DISP=(,DELETE,DELETE),
//             UNIT=SYSDA,SPACE=(TRK,(10,5),RLSE),
//             DCB=(RECFM=VB,BLKSIZE=4096,LRECL=4092,BUFNO=5)
//G.SYSOUT   DD SYSOUT=*
//G.SYSPRINT DD SYSOUT=*
//G.SYSIN    DD *
E
/*
