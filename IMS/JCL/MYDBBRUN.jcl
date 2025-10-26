//MYDBBRUN  JOB FB3,,REGION=0M,NOTIFY=&SYSUID
//*
//*   IMS DL/I TEST PROGRAM 'DFSDDLT0' - 'DBB' BATCH MODE (USES ACB)
//*
//SETLIB   JCLLIB ORDER=DFSF10.PROCLIB
//*
//IMSDLI   EXEC DBBBATCH,
//             MBR='DFSDDLT0',                     DL/I TEST PROGRAM
//             PSB='IVPPSB1',                         MY OWN IVP PSB
//             DBRC='N',                                   NO RECON!
//             IRLM='N'                                    NO LOCKS!
//*
//G.IMSACB   DD DISP=SHR,DSN=&SYSUID..IMS.ACBLIB             MY ACBs
//           DD DISP=SHR,DSN=DFSF10.ACBLIB                  IMS ACBs
//*
//G.IMSACBA  DD DUMMY
//G.IMSACBB  DD DUMMY
//G.MODSTAT  DD DUMMY
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
