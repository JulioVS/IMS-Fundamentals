//DLIUNI9L  JOB FB3,,REGION=0M,NOTIFY=&SYSUID
//*
//*   IMS DL/I TEST PROGRAM 'DFSDDLT0'
//*
//SETLIB   JCLLIB ORDER=DFSF10.PROCLIB
//*
//IMSDLI   EXEC DLIBATCH,
//             MBR='DFSDDLT0',                     DL/I TEST PROGRAM
//             PSB='PSBLOAD',                          MY SKILL9 PSB
//             DBRC='N',                                   NO RECON!
//             IRLM='N'                                    NO LOCKS!
//*
//G.IMS      DD DISP=SHR,DSN=&SYSUID..IMS.PSBLIB             MY PSBs
//           DD DISP=SHR,DSN=&SYSUID..IMS.DBDLIB             MY DBDs
//           DD DISP=SHR,DSN=DFSF10.PSBLIB                  IMS PSBs
//           DD DISP=SHR,DSN=DFSF10.DBDLIB                  IMS DBDs
//*
//G.SKILL9   DD DISP=SHR,DSN=&SYSUID..IMS.SKILL9      MY DATA <VSAM>
//*
//G.DFSVSAMP DD DISP=SHR,DSN=DFSF10.PROCLIB(DFSVSM00)       BUFFERS!
//G.IEFRDER  DD DSN=&SYSUID..IMS.IMSLOG,                  MY IMS LOG
//             DISP=(,DELETE,DELETE),
//             UNIT=SYSDA,SPACE=(TRK,(10,5),RLSE),
//             DCB=(RECFM=VB,BLKSIZE=4096,LRECL=4092,BUFNO=5)
//G.PRINTDD  DD SYSOUT=*                                   FOR DLT0!
//G.SYSIN    DD DISP=SHR,DSN=&SYSUID..IMS.DLIIN(U9LOAD1)       INPUT
//* //G.SYSIN    DD *
//* U---------------------------------------------------------------------*
//* U                                                                     *
//* U---+----1----+----2----+----3----+----4----+----5----+----6----+----7*
//* S 1 1 1 1 1    SKILL9
//* U
//* L        ISRT  SKILL
//* L        DATA  ANALYST  100000 124000 SENIOR SYSTEMS ANALYST   SEGM #1
//* U
//* L        ISRT  NAME
//* L        DATA  014430 010 06 JONES         SEGM #2
//* L        ISRT  EXPR
//* L        DATA  RW  08 01 123-F-001         SEGM #3
//* L        ISRT  EDUC
//* L        DATA  BOLTON ECON BA 02 04        SEGM #4
//* U
//* L        ISRT  NAME
//* L        DATA  016638 021 06 SMITH         SEGM #5
//* L        ISRT  EXPR
//* L        DATA  NRW 03 01 123-F-003         SEGM #6
//* L        ISRT  EDUC
//* L        DATA  ROCKER CHEM BS 03 04        SEGM #7
//* L        ISRT  EDUC
//* L        DATA  WOLTON ECON MA 08 02        SEGM #8
//* U
//* L        ISRT  SKILL
//* L        DATA  ARTIST   079000 098000 STAFF ARTIST             SEGM #9
//* /*
