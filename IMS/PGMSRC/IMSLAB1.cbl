       IDENTIFICATION DIVISION.
       PROGRAM-ID. COBPROG.
      *
       ENVIRONMENT DIVISION.
      *
       DATA DIVISION.
      *
       WORKING-STORAGE SECTION.
      *
      *
      ****************************************************************
      * COUNTERS, SWITCHES AND OTHER MISCELLANEOUS VARIABLES
      * - ADD NECESSARY COUNTERS
      ****************************************************************
      *
       77 LAB1-COUNTER            PIC 9(02).
      *
      ****************************************************************
      * SYSIN INPUT DATA
      * - DO NOT MODIFY THIS SECTION OF CODE
      ****************************************************************
      *
       01 SYSIN-INPUT.
          05 COL1                 PIC X(01).
          05 KSKILL               PIC X(08).
          05 KNAME                PIC X(42).
          05 KEXPR                PIC X(04).
          05 FILLER               PIC X(25).
      *
      ****************************************************************
      * DL/I CALL FUNCTIONS
      * - COPY FROM YOUR LIBRARY
      * - COMPLETE WHERE NECESSARY
      ****************************************************************
      *
       77 GU                      PIC X(04)  VALUE 'GU  '.
       77 GN                      PIC X(04)  VALUE 'GN  '.
       77 GNP                     PIC X(04)  VALUE 'GNP '.
       77 GHU                     PIC X(04)  VALUE 'GHU '.
       77 GHN                     PIC X(04)  VALUE 'GHN '.
       77 GHNP                    PIC X(04)  VALUE 'GHNP'.
      *
      *
      ****************************************************************
      * SEGMENT LAYOUTS - USED AS IOAREAS IN CALLS
      * - COPY FROM YOUR LIBRARY
      * - COMPLETE WHERE NECESSARY
      ****************************************************************
      *
       01 IOAREA-SKILL.
          05 SKCLASS              PIC X(08).
          05 SKILL-DATA           PIC X(82).
      *
       01 IOAREA-NAME.
          05 FULNAM               PIC X(42).
          05 NAME-DATA            PIC X(78).
      *
       01 IOAREA-EXPR.
          05 EXPR-DATA            PIC X(16).
          05 CLASSIF              PIC X(04).
      *
       01 IOAREA-EDUC.
          05 EDUID                PIC X(18).
          05 EDUC-DATA            PIC X(57).
      *
      ****************************************************************
      * SSA'S : FULLY QUALIFIED, INCLUDING NULL COMMAND CODES.
      * - COPY FROM YOUR LIBRARY
      * - COMPLETE WHERE NECESSARY
      ****************************************************************
      *
       01 SSA-QUAL-SKILL.
          05 SEGMENT-NAME         PIC X(08)  VALUE 'SKILL   '.
          05 COMMAND-CODES-START  PIC X(01)  VALUE '*'.
          05 COMMAND-CODES        PIC X(04)  VALUE '----'.
          05 QUAL-START           PIC X(01)  VALUE '('.
          05 QUAL-FIELD-NAME      PIC X(08)  VALUE 'SKCLASS '.
          05 QUAL-OPERATOR        PIC X(02)  VALUE '= '.
          05 QUAL-VALUE           PIC X(1).
          05 QUAL-END             PIC X(01)  VALUE ')'.
      *
       01 SSA-QUAL-NAME.
          05 SEGMENT-NAME         PIC X(08)  VALUE 'NAME    '.
          05 COMMAND-CODES-START  PIC X(01)  VALUE '*'.
          05 COMMAND-CODES        PIC X(04)  VALUE '----'.
          05 QUAL-START           PIC X(01)  VALUE '('.
          05 QUAL-FIELD-NAME      PIC X(08)  VALUE 'FULNAM  '.
          05 QUAL-OPERATOR        PIC X(02)  VALUE '= '.
          05 QUAL-VALUE           PIC X(1).
          05 QUAL-END             PIC X(01)  VALUE ')'.
      *
       01 SSA-QUAL-EXPR.
          05 SEGMENT-NAME         PIC X(08)  VALUE 'EXPR    '.
          05 COMMAND-CODES-START  PIC X(01)  VALUE '*'.
          05 COMMAND-CODES        PIC X(04)  VALUE '----'.
          05 QUAL-START           PIC X(01)  VALUE '('.
          05 QUAL-FIELD-NAME      PIC X(08)  VALUE 'CLASSIF '.
          05 QUAL-OPERATOR        PIC X(02)  VALUE '= '.
          05 QUAL-VALUE           PIC X(04).
          05 QUAL-END             PIC X(01)  VALUE ')'.
      *
       01 SSA-QUAL-EDUC.
          05 SEGMENT-NAME         PIC X(08)  VALUE 'EDUC    '.
          05 COMMAND-CODES-START  PIC X(01)  VALUE '*'.
          05 COMMAND-CODES        PIC X(04)  VALUE '----'.
          05 QUAL-START           PIC X(01)  VALUE '('.
          05 QUAL-FIELD-NAME      PIC X(08)  VALUE 'EDUID   '.
          05 QUAL-OPERATOR        PIC X(02)  VALUE '= '.
          05 QUAL-VALUE           PIC X(18).
          05 QUAL-END             PIC X(01)  VALUE ')'.
      *
       LINKAGE SECTION.
      *
      *
      ****************************************************************
      * PCB MASKS : ORDER CODED IN LINKAGE SECTION IS NOT IMPORTANT.
      * - SUPPLY YOUR OWN LEVEL 01 NAME ('SKILL-PCB' IS AN EXAMPLE)
      * - COPY FROM THIS LIBRARY THE REST OF THE PCB MASK FOR EACH
      * PCB (LEVEL 05 NAMES)
      * - COMPLETE WHERE NECESSARY
      ****************************************************************
      *
       01 SKILL-PCB.
          05 DBDNAME              PIC X(08).
          05 SEGMENT-LEVEL        PIC X(02).
          05 STATUS-CODE          PIC X(02).
          05 PROCOPT              PIC X(04).
          05 RESERVED             PIC S9(05) COMPUTATIONAL.
          05 SEGMENT-NAME         PIC X(08).
          05 KFBAREA-KEY-LENGTH   PIC S9(05) COMPUTATIONAL.
          05 NUMBER-OF-SENSEGS    PIC S9(05) COMPUTATIONAL.
          05 KFBAREA              PIC X(68).
      *
       PROCEDURE DIVISION.
      *
      *
      ****************************************************************
      * PROGRAM ENTRY POINT
      * - COMPLETE WHERE NECESSARY
      ****************************************************************
      *
           ENTRY 'DLITCBL' USING SKILL-PCB.
      *
      ****************************************************************
      * READ SYSIN INPUT
      * - DO NOT CHANGE THIS PIECE OF CODE
      ****************************************************************
      *
      *
       READ-SYSIN-INPUT.
      *
           ACCEPT SYSIN-INPUT.
      *
           IF COL1 OF SYSIN-INPUT = '*'
              GO TO READ-SYSIN-INPUT.
      *
           IF COL1 OF SYSIN-INPUT = '1'
              DISPLAY '*'
              DISPLAY '******************************************'
              DISPLAY '* LAB 1 OUTPUT STARTS HERE *'
              DISPLAY '******************************************'
              DISPLAY '*'
              GO TO READ-SYSIN-INPUT.
      *
           IF COL1 OF SYSIN-INPUT = '2'
              DISPLAY '*'
              DISPLAY '******************************************'
              DISPLAY '* LAB 2 OUTPUT STARTS HERE *'
              DISPLAY '******************************************'
              DISPLAY '*'
              GO TO READ-SYSIN-INPUT.
      *
           IF COL1 OF SYSIN-INPUT = '4'
              DISPLAY '*'
              DISPLAY '******************************************'
              DISPLAY '* LAB 4 OUTPUT STARTS HERE *'
              DISPLAY '******************************************'
              DISPLAY '*'
              GO TO READ-SYSIN-INPUT.
      *
           IF COL1 OF SYSIN-INPUT = '5'
              DISPLAY '*'
              DISPLAY '******************************************'
              DISPLAY '* LAB 5 OUTPUT STARTS HERE *'
              DISPLAY '******************************************'
              DISPLAY '*'
              GO TO READ-SYSIN-INPUT.
      *
           DISPLAY '*'.
           DISPLAY '****** INPUT CARD FOLLOWS **************'.
           DISPLAY '*'.
           DISPLAY SYSIN-INPUT.
           DISPLAY '*'.
      *
           IF COL1 OF SYSIN-INPUT = 'E'
              DISPLAY '*'
              DISPLAY '******************************************'
              DISPLAY '* END OF OUTPUT *'
              DISPLAY '******************************************'
              DISPLAY '*'
              GOBACK.
      *
           IF COL1 OF SYSIN-INPUT = 'G'
              PERFORM LAB1-START THRU LAB1-END.
      *
           IF COL1 OF SYSIN-INPUT = 'U'
              PERFORM LAB2-START THRU LAB2-END.
      *
           IF COL1 OF SYSIN-INPUT = 'C'
              PERFORM LAB4-START THRU LAB4-END.
      *
           IF COL1 OF SYSIN-INPUT = 'O'
              PERFORM LAB5-START THRU LAB5-END.
      *
           GO TO READ-SYSIN-INPUT.
      *
      *
      *---------------------------------------------------------------
      * LAB 1
      *---------------------------------------------------------------
      *
       LAB1-START.
           CALL 'CBLTDLI' USING GU, SKILL-PCB, IOAREA-SKILL
           ON EXCEPTION
              PERFORM ERROR-ROUTINE-START THRU ERROR-ROUTINE-END
           END-CALL.
      *
      ****************************************************************
      * LAB 1 LOGIC GOES HERE.
      * - IN CASE OF ERROR, USE THE ERROR ROUTINE
      * AT THE END OF THIS PROGRAM
      ****************************************************************
      *
       LAB1-END.
           EXIT.
      *
      *
      *---------------------------------------------------------------
      * LAB 2
      *---------------------------------------------------------------
      *
       LAB2-START.
           CONTINUE.
      *
      ****************************************************************
      * LAB 2 LOGIC GOES HERE.
      * - IN CASE OF ERROR, USE THE ERROR ROUTINE
      * AT THE END OF THIS PROGRAM
      ****************************************************************
      *
       LAB2-END.
           EXIT.
      *
      *
      *---------------------------------------------------------------
      * LAB 4
      *---------------------------------------------------------------
      *
       LAB4-START.
           CONTINUE.
      *
      ****************************************************************
      * LAB 4 LOGIC GOES HERE.
      * - IN CASE OF ERROR, USE THE ERROR ROUTINE
      * AT THE END OF THIS PROGRAM
      ****************************************************************
      *
       LAB4-END.
           EXIT.
      *
      *
      *---------------------------------------------------------------
      * LAB 5
      *---------------------------------------------------------------
      *
       LAB5-START.
           CONTINUE.
      *
      ****************************************************************
      * LAB 5 LOGIC GOES HERE.
      * - IN CASE OF ERROR, USE THE ERROR ROUTINE
      * AT THE END OF THIS PROGRAM
      ****************************************************************
      *
       LAB5-END.
           EXIT.
      *
      *
      *---------------------------------------------------------------
      * ERROR ROUTINE
      * - MODIFY TO YOUR PCBNAME(S) AS REQUIRED
      *---------------------------------------------------------------
      *
      *
       ERROR-ROUTINE-START.
      *
           DISPLAY '*'.
           DISPLAY '****** ERROR ROUTINE - START ***********'.
           DISPLAY '*'.

           DISPLAY 'DBDNAME = '
                   DBDNAME OF SKILL-PCB.
           DISPLAY 'SEGMENT-LEVEL = '
                   SEGMENT-LEVEL OF SKILL-PCB.
           DISPLAY 'STATUS-CODE = '
                   STATUS-CODE OF SKILL-PCB.
           DISPLAY 'PROCOPT = '
                   PROCOPT OF SKILL-PCB.
           DISPLAY 'RESERVED = '
                   RESERVED OF SKILL-PCB.
           DISPLAY 'SEGMENT-NAME = '
                   SEGMENT-NAME OF SKILL-PCB.
           DISPLAY 'KFBAREA-KEY-LENGTH = '
                   KFBAREA-KEY-LENGTH OF SKILL-PCB.
           DISPLAY 'NUMBER-OF-SENSEGS = '
                   NUMBER-OF-SENSEGS OF SKILL-PCB.
           DISPLAY 'KFBAREA = '
                   KFBAREA OF SKILL-PCB.

           DISPLAY '*'.
           DISPLAY '****** ERROR ROUTINE - END ***********'.
           DISPLAY '*'.
      *
       ERROR-ROUTINE-END.
           EXIT.
