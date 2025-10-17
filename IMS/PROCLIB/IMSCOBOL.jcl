//******************************************************************    00010000
//*  IMSCOBOL Procedure                                                 00020000
//*                                                                     00030000
//*  This procedure is a two-step compile and bind procedure            00040000
//*  procedure for IMS applications that are written in COBOL.          00050000
//*                                                                     00060000
//*  The high-level qualifier of the IMS data sets is IMS.              00070000
//*  If your installation does not use this default value,              00080000
//*  then set the NODEx parameters, which correspond                    00090000
//*  to the names specified in the IMSGEN macro.                        00100000
//*                                                                     00110000
//************* C O N D I T I O N A L   C H A N G E ******************  00120000
//*  IMPORTANT: This procedure needs to be modified if                  00130000
//*  any of the following are true:                                     00140000
//*  - Dual DD statements are required for the system log               00150000
//*  Look for the 'Conditional Change' comment lines.                   00160000
//************* E N D   C O N D I T I O N A L   C H A N G E **********  00170000
//*                                                                     00180000
//***********************************************************@SCPYRT**  00190000
//*                                                                     00200000
//*  Licensed Materials - Property of IBM                               00210000
//*                                                                     00220000
//*  5635-A06                                                           00230000
//*                                                                     00240000
//*      Copyright IBM Corp. 2016      All Rights Reserved              00250000
//*                                                                     00260000
//*  US Government Users Restricted Rights - Use, duplication or        00270000
//*  disclosure restricted by GSA ADP Schedule contract with            00280000
//*  IBM Corp.                                                          00290000
//*                                                                     00300000
//***********************************************************@ECPYRT**  00310000
//*                                                                     00320000
//       PROC MBR=TEMPNAME,PAGES=60,SYS2=,                              00330000
//            LNGPRFX=IGY640,                                           00340000
//            LIBPRFX=CEE,                                              00350000
//            SOUT=A,                                                   00360000
//            NODE1=DFSF10,                                             00370000
//            NODE2=DFSF10                                              00380000
//*                                                                     00385000
//C      EXEC PGM=IGYCRCTL,REGION=4M,                                   00390000
//        PARM='SIZE(832K),BUF(10K),LINECOUNT(50)'                      00400000
//STEPLIB  DD DISP=SHR,DSN=&LNGPRFX..SIGYCOMP                           00410000
//SYSLIN   DD DSN=&&LIN,DISP=(MOD,PASS),UNIT=SYSDA,                     00430000
//            DCB=(&NODE2..&SYS2.PROCLIB),                              00440000
//            SPACE=(CYL,(1,1))                                         00450000
//SYSPRINT DD SYSOUT=*                                                  00460000
//SYSUT1   DD UNIT=SYSDA,SPACE=(CYL,(1,1))                              00490000
//SYSUT2   DD UNIT=SYSDA,SPACE=(CYL,(1,1))                              00500000
//SYSUT3   DD UNIT=SYSDA,SPACE=(CYL,(1,1))                              00510000
//SYSUT4   DD UNIT=SYSDA,SPACE=(CYL,(1,1))                              00520000
//SYSUT5   DD UNIT=SYSDA,SPACE=(CYL,(1,1))                              00530000
//SYSUT6   DD UNIT=SYSDA,SPACE=(CYL,(1,1))                              00540000
//SYSUT7   DD UNIT=SYSDA,SPACE=(CYL,(1,1))                              00550000
//SYSUT8   DD UNIT=SYSDA,SPACE=(CYL,(1,1))                              00550080
//SYSUT9   DD UNIT=SYSDA,SPACE=(CYL,(1,1))                              00550090
//SYSUT10  DD UNIT=SYSDA,SPACE=(CYL,(1,1))                              00550100
//SYSUT11  DD UNIT=SYSDA,SPACE=(CYL,(1,1))                              00550110
//SYSUT12  DD UNIT=SYSDA,SPACE=(CYL,(1,1))                              00550120
//SYSUT13  DD UNIT=SYSDA,SPACE=(CYL,(1,1))                              00550130
//SYSUT14  DD UNIT=SYSDA,SPACE=(CYL,(1,1))                              00550140
//SYSUT15  DD UNIT=SYSDA,SPACE=(CYL,(1,1))                              00550150
//SYSMDECK DD UNIT=SYSDA,SPACE=(CYL,(1,1))                              00550160
//*                                                                     0055%000
//L      EXEC PGM=IEWL,REGION=4M,                                       00560000
//            PARM='XREF,LET,LIST',                                     00570000
//            COND=(8,LT,C)                                             00580000
//SYSLIB   DD DISP=SHR,DSN=&NODE2..&SYS2.SDFSRESL                       00600000
//         DD DISP=SHR,DSN=&LIBPRFX..SCEELKED                           00610000
//SYSLIN   DD DSN=&&LIN,DISP=(OLD,DELETE)                               00620000
//         DD DDNAME=SYSIN                                              00660000
//SYSLMOD  DD DISP=SHR,DSN=&NODE1..&SYS2.PGMLIB                         00680000
//SYSPRINT DD SYSOUT=*                                                  00690000
//SYSUT1   DD UNIT=SYSDA,SPACE=(CYL,(5,1))                              00700000
//*                                                                     00715000
//       PEND                                                           00710000
//*                                                                     00715000
