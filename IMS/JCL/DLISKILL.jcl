//DLISKILL  JOB FB3,,REGION=0M,NOTIFY=&SYSUID
//*
//*   IMS DL/I TEST PROGRAM 'DFSDDLT0'
//*
//SETLIB   JCLLIB ORDER=DFSF10.PROCLIB
//*
//IMSDLI   EXEC DLIBATCH,
//             MBR='DFSDDLT0',                     DL/I TEST PROGRAM
//             PSB='PSBLOAD',                           MY SKILL PSB
//             DBRC='N',                                   NO RECON!
//             IRLM='N'                                    NO LOCKS!
//*
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
//G.PRINTDD  DD SYSOUT=*                                   FOR DLT0!
//G.SYSIN    DD *
U---------------------------------------------------------------------*
U                                                                     *
U---+----1----+----2----+----3----+----4----+----5----+----6----+----7*
S 1 1 1 1 1    SKILL
U
L        ISRT  SKILL
L        DATA  BC290   MAGNETIC DEVICES
L        ISRT  NAME
L        DATA  JAMORA, JOSEPH W.
L        ISRT  EXPR
L        DATA  05P600-61164    1000
L        ISRT  EXPR
L        DATA  04P600-8        2000
L        ISRT  EDUC
L        DATA  HARVARD           MPHYSICS
U
L        ISRT  SKILL
L        DATA  BU115   HUMAN FACTORS-DESIGN
L        ISRT  NAME
L        DATA  AKERSON, LAWRENCE R.
L        ISRT  EXPR
L        DATA  05A100-30436    1000
L        ISRT  EXPR
L        DATA  06A200-10242    2000
L        ISRT  EXPR
L        DATA  09A200-41051    3000
L        ISRT  EXPR
L        DATA  10A300-2        4000
L        ISRT  EDUC
L        DATA  HARVARD           MBUSINESS ADM.
U
L        ISRT  SKILL
L        DATA  ED003   MARKETING
L        ISRT  NAME
L        DATA  FERRIS, MONA J.
L        ISRT  EXPR
L        DATA  01F700-30761    1000
L        ISRT  EXPR
L        DATA  01F700-30762    1000
L        ISRT  EXPR
L        DATA  01F700-50567    2000
L        ISRT  EXPR
L        DATA  02F700-601      3000
L        ISRT  EXPR
L        DATA  02F700-602      3000
L        ISRT  EDUC
L        DATA  BROWN             MECONOMICS
L        ISRT  EDUC
L        DATA  BROWN             MPHILOSOPHY
U
L        ISRT  SKILL
L        DATA  EK999   MANAGEMENT-SYSTEMS OR METHODS
L        ISRT  NAME
L        DATA  LINDHOLM, THOMAS C.
L        ISRT  EXPR
L        DATA  07G100-40261    1000
L        ISRT  EXPR
L        DATA  05G100-61266    2000
L        ISRT  EXPR
L        DATA  05G100-704      3000
L        ISRT  EDUC
L        DATA  MICHIGAN          BECONOMICS
U
L        ISRT  SKILL
L        DATA  KB750   PURCHASING APPLICATIONS
L        ISRT  NAME
L        DATA  FREIDMAN, JACK A.
L        ISRT  EXPR
L        DATA  02A900-10966    1000
L        ISRT  EXPR
L        DATA  01A900-20767    2000
L        ISRT  EXPR
L        DATA  01A900-40461    3000
L        ISRT  EXPR
L        DATA  01A900-40462    3000
L        ISRT  EXPR
L        DATA  01A900-40463    3000
L        ISRT  EXPR
L        DATA  01A900-6        4000
L        ISRT  EDUC
L        DATA  BROWN             BBUSINESS ADM. MGMGT
L        ISRT  EDUC
L        DATA  BROWN             HUMAN RESOURCES
L        ISRT  EDUC
L        DATA  BROWN             MARKETING
/*
