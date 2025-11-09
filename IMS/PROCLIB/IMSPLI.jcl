//******************************************************************    00010000
//*  IMSPLI Procedure                                                   00020000
//*                                                                     00030000
//*  This procedure is a two-step compile and bind procedure            00040000
//*  for IMS applications written in PL/I.                              00050000
//*                                                                     00060000
//*  The high-level qualifier of the IMS data sets is IMS.              00070000
//*  If your installation does not use this default value,              00080000
//*  then set the NODEx parameters, which correspond                    00090000
//*  to the names specified in the IMSGEN macro.                        00100000
//*                                                                     00110000
//***********************************************************@SCPYRT**  00120000
//*                                                                     00130000
//*  Licensed Materials - Property of IBM                               00140000
//*                                                                     00150000
//*  5635-A06                                                           00160000
//*                                                                     00170000
//*      Copyright IBM Corp. 2016      All Rights Reserved              00180000
//*                                                                     00190000
//*  US Government Users Restricted Rights - Use, duplication or        00200000
//*  disclosure restricted by GSA ADP Schedule contract with            00210000
//*  IBM Corp.                                                          00220000
//*                                                                     00230000
//***********************************************************@ECPYRT**  00240000
//*                                                                     00250000
//       PROC MBR=TEMPNAME,PAGES=50,SYS2=,                              00260000
//            LNGPRFX=IEL610,                                           00270000
//            LIBPRFX=CEE,                                              00280000
//            SOUT=A,                                                   00290000
//            NODE1=DFSF10,                                             00300000
//            NODE2=DFSF10                                              00310000
//*                                                                     00315000
//C      EXEC PGM=IBMZPLI,REGION=4M,                                    00320000
//        PARM='XREF,A,OBJ,NOMACRO,NOLIST,OPT(TIME),SOURCE,SYSTEM(IMS)' 00330000
//*                                                                     00345000
//STEPLIB  DD DISP=SHR,DSN=&LNGPRFX..SIBMZCMP                           00350000
//         DD DISP=SHR,DSN=&LIBPRFX..SCEERUN                            00360000
//SYSUT1   DD UNIT=SYSDA,                                               00370000
//            SPACE=(CYL,(1,1),RLSE,,ROUND),                            00380000
//            DCB=BLKSIZE=1024,DISP=(,DELETE)                           00390000
//SYSPRINT DD SYSOUT=&SOUT,                                             00400000
//            DCB=(LRECL=125,BLKSIZE=629,RECFM=VBA),                    00410000
//            SPACE=(CYL,(1,1),RLSE)                                    00420000
//SYSLIN   DD UNIT=SYSDA,SPACE=(CYL,(1,1),RLSE),                        00430000
//            DCB=(LRECL=80,BLKSIZE=3200),                              00440000
//            DISP=(,PASS)                                              00450000
//*                                                                     00455000
//L      EXEC PGM=IEWBLINK,PARM='XREF,COMPAT=PM3',                      00460000
//            COND=(9,LT,C),REGION=4M                                   00470000
//*                                                                     00475000
//SYSLIB   DD DISP=SHR,DSN=&NODE2..&SYS2.SDFSRESL                       00480000
//         DD DISP=SHR,DSN=&LIBPRFX..SCEELKED                           00490000
//SYSLIN   DD DSN=*.C.SYSLIN,DISP=(OLD,DELETE)                          00500000
//         DD DDNAME=SYSIN                                              00530000
//SYSLMOD  DD DISP=SHR,DSN=&NODE1..&SYS2.PGMLIB(&MBR)                   00550000
//SYSPRINT DD SYSOUT=&SOUT,                                             00560000
//            DCB=(LRECL=121,RECFM=FBA,BLKSIZE=605),                    00570000
//            SPACE=(CYL,(1,1),RLSE)                                    00580000
//SYSUT1   DD UNIT=SYSDA,DISP=(,DELETE),                                00590000
//            SPACE=(CYL,(5,1),RLSE)                                    00600000
//*                                                                     00610000
//       PEND                                                           00620000
//*                                                                     00630000
//*
//* NOTE:
//*
//*       IN THE ORIGINAL 'SYSLIN' CONCATENATION OF THE LINK STEP,
//*       THE FOLLOWING LINE WAS PRESENT:
//*
//*                'DD &NODE2..&SYS2.PROCLIB(PLITDLI)'
//*
//*       THIS 'PLITDLI' MEMBER WOULD HAVE CONTAINED THE CONTROL
//*       STATEMENTS NECESSARY TO BIND THE PROGRAM WITH DL/I SUPPORT.
//*
//*       HOWEVER, THIS MEMBER IS *NOT* PRESENT IN THE ZXP LIBRARIES
//*       CAUSING THE ORIGINAL PROCEDURE TO CRASH.
//*
//*       THIS MEMBER IN TURN GETS GENERATED IN THE IMS INSTALLATION
//*       BY RUNNING THE 'SDFSBASE(DFSPROCB)' JCL, AND ITS CONTENTS
//*       WOULD HAVE BEEN:
//*
//*                LIBRARY SDFSRESL(PLITDLI)     PL/I LANG INTF
//*                LIBRARY SDFSRESL(DFHEI01)     HLPI LANG INTF
//*                LIBRARY SDFSRESL(DFHEI1)      HLPI LANG INTF
//*                ENTRY PLICALLA
//*
//*       IN THE END, I DECIDED TO REMOVE IT COMPLETELY FROM HERE, ADD
//*       '&NODE2..&SYS2.SDFSRESL' TO THE 'SYSLIB' CARD AND PASS A
//*       SIMILAR DIRECTIVE FROM MY OWN JOB INSTEAD, LIKE THIS:
//*
//*                //L.SYSIN    DD *,SYMBOLS=CNVTSYS
//*                  INCLUDE SYSLIB(DFSLI000)
//*                     NAME &MYPRG(R)
//*                /*
//*
//*       NOTE THAT IN THIS CONTEXT 'PLITDLI' IS JUST AN ALIAS TO THE
//*       ACTUAL 'DFSLI000' INTERFACE MODULE, LOCATED IN THE 'SDFSRESL'
//*       DATA SET UNDER MANY ALIASES.
//*
