//DLILAB5J  JOB FB3,,REGION=0M,NOTIFY=&SYSUID
//*
//*   IMS DL/I TEST PROGRAM 'DFSDDLT0'
//*
//SETLIB   JCLLIB ORDER=DFSF10.PROCLIB
//*
//IMSDLI   EXEC DLIBATCH,
//             MBR='DFSDDLT0',                     DL/I TEST PROGRAM
//             PSB='PSB1CBL',                           MY SKILL PSB
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
L        GHU   SKILL   *D---(SKCLASS = KB750   )                       X
               NAME    *DP--(FULNAM  = FREIDMAN, JACK A.               X
         CONT            )                                             X
               EDUC    *L---(EDUID   = BROWN             )
U
L        REPL  SKILL   *N---                                           X
               NAME    *N---                                           X
               EDUC    *----
L        DATA  SKILL SEGMENT DATA, DON'T REPLACE!                      X
         CONT                                    NAME SEGMENT DATA, DONX
         CONT  'T REPLACE EITHER!                                      X
         CONT                                            BROWN         X
         CONT      MSYSTEMS (NOW, DO REPLACE THIS ONE:)
U
L        GNP   SKILL   *U---                                           X
               NAME    *U---                                           X
               EXPR    *F---(CLASSIF = 3000)
U
U              ---  JUST TO VERIFY PREVIOUS REPLACE CALL  ---
U
L        GU    SKILL   *D---(SKCLASS = KB750   )                       X
               NAME    *D---(FULNAM  = FREIDMAN, JACK A.               X
         CONT            )                                             X
               EDUC    *L---(EDUID   = BROWN             )
/*
