//DLIIVP1L  JOB FB3,,REGION=0M,NOTIFY=&SYSUID
//*
//*   IMS DL/I TEST PROGRAM 'DFSDDLT0'
//*
//SETLIB   JCLLIB ORDER=DFSF10.PROCLIB
//*
//IMSDLI   EXEC DLIBATCH,
//             MBR='DFSDDLT0',                     DL/I TEST PROGRAM
//             PSB='IVPPSBL',                         MY OWN IVP PSB
//             DBRC='N',                                   NO RECON!
//             IRLM='N'                                    NO LOCKS!
//*
//G.IMS      DD DISP=SHR,DSN=&SYSUID..IMS.PSBLIB             MY PSBs
//           DD DISP=SHR,DSN=&SYSUID..IMS.DBDLIB             MY DBDs
//           DD DISP=SHR,DSN=DFSF10.PSBLIB                  IMS PSBs
//           DD DISP=SHR,DSN=DFSF10.DBDLIB                  IMS DBDs
//*
//G.DFSIVD1  DD DISP=SHR,DSN=&SYSUID..IMS.IVPDB1      MY DATA <OSAM>
//G.DFSIVD1I DD DISP=SHR,DSN=&SYSUID..IMS.IVPDB1I     MY KEYS <VSAM>
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
L        ISRT  A1111111
L        DATA  GROSSKOPF MARKUS    1-111-1111ZP90210
L        ISRT  A1111111
L        DATA  HANSEN    KAI       2-222-2222ZP90125
L        ISRT  A1111111
L        DATA  KISKE     MICHAEL   3-333-3333ZPOU812
L        ISRT  A1111111
L        DATA  SWICHTBRG INGO      4-444-4444ZP05150
L        ISRT  A1111111
L        DATA  WEIKATH   MICHAEL   5-555-5555ZP02112
/*
