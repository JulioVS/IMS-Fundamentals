//MYBMPRUN  JOB FB3,,REGION=0M,NOTIFY=&SYSUID
//*
//*   IMS DL/I TEST PROGRAM 'DFSDDLT0' - 'BMP' BATCH MODE (USES ACB)
//*
//SETLIB   JCLLIB ORDER=DFSF10.PROCLIB
//*
//IMSDLI   EXEC IMSBATCH,
//             MBR='DFSDDLT0',                     DL/I TEST PROGRAM
//             PSB='DFSIVP1',                            IMS IVP PSB
//             IMSID='IVP1'                           IMSPLEX MEMBER
//*
//G.DFSVSAMP DD DISP=SHR,DSN=DFSF10.PROCLIB(DFSVSM00)       BUFFERS!
//G.IEFRDER  DD DSN=&SYSUID..IMS.IMSLOG,                  MY IMS LOG
//             DISP=(,DELETE,DELETE),
//             UNIT=SYSDA,SPACE=(TRK,(10,5),RLSE),
//             DCB=(RECFM=VB,BLKSIZE=4096,LRECL=4092,BUFNO=5)
//G.PRINTDD  DD SYSOUT=*                                   FOR DLT0!
//G.SYSIN    DD *
U---------------------------------------------------------------------*
U                                                                     *
U---+----1----+----2----+----3----+----4----+----5----+----6----+----7*
S 1 1 1 1 1    IVPDB1
L        GU    A1111111
L   0050 GN
/*

//* IF THIS ABENDS, TRY STOPPING AND THEN RESTARTING SCHEDULING OF PSB!
//*
//* UPDATE PGM NAME(DFSIVP1) STOP(SCHD)
//* UPDATE PGM NAME(DFSIVP1) START(SCHD)
//* QUERY  PGM NAME(DFSIVP1) SHOW(STATUS)
