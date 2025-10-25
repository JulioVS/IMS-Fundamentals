       IDENTIFICATION DIVISION.
       PROGRAM-ID. COBOL55.
      *
       ENVIRONMENT DIVISION.
      *
       DATA DIVISION.
      *
       WORKING-STORAGE SECTION.
      *
      *
      *--------------------------------------------------------------*
      * COUNTERS, SWITCHES AND OTHER MISCELLANEOUS VARIABLES         *
      * - COMPLETE IF NECESSARY                                      *
      *--------------------------------------------------------------*
      *
      *
      *--------------------------------------------------------------*
      * DL/I CALL FUNCTIONS                                          *
      *--------------------------------------------------------------*
      *
       77 GU                      PIC X(04)  VALUE 'GU  '.
       77 GN                      PIC X(04)  VALUE 'GN  '.
       77 GNP                     PIC X(04)  VALUE 'GNP '.
       77 GHU                     PIC X(04)  VALUE 'GHU '.
       77 GHN                     PIC X(04)  VALUE 'GHN '.
       77 GHNP                    PIC X(04)  VALUE 'GHNP'.
       77 REPL                    PIC X(04)  VALUE 'REPL'.
       77 ISRT                    PIC X(04)  VALUE 'ISRT'.
       77 DLET                    PIC X(04)  VALUE 'DLET'.
       77 ROLB                    PIC X(04)  VALUE 'ROLB'.
       77 CHNG                    PIC X(04)  VALUE 'CHNG'.
       77 PURG                    PIC X(04)  VALUE 'PURG'.
      *
      *--------------------------------------------------------------*
      * INPUT MESSAGE SEGMENTS LAYOUT                                *
      * *** DO NOT MODIFY ***                                        *
      *--------------------------------------------------------------*
      *
       01 INPUT-MESSAGE-SEGMENT.
          05 LL                   PIC S9(04) COMP.
          05 ZZ                   PIC X(02).
          05 TRANCODE             PIC X(08).
          05 KSKILL               PIC X(08).
          05 KNAME                PIC X(42).
          05 KEXPR                PIC X(04).
      *
      *--------------------------------------------------------------*
      * OUTPUT MESSAGE SEGMENT LAYOUT                                *
      * *** DO NOT MODIFY ***                                        *
      *--------------------------------------------------------------*
      *
       01 OUTPUT-MESSAGE-SEGMENT.
          05 LL                   PIC S9(04) COMP.
          05 ZZ                   PIC X(02)  VALUE LOW-VALUES.
          05 KSKILL-O             PIC X(08).
          05 KNAME-O              PIC X(42).
          05 KEXPR-O              PIC X(04).
          05 CKEY-O               PIC X(54).
          05 EXPR-O               PIC X(50).
          05 RESPONSE-TEXT-1      PIC X(50).
          05 RESPONSE-TEXT-2      PIC X(50).
      *
      *--------------------------------------------------------------*
      * DATABASE SEGMENT LAYOUTS - USED AS IOAREAS FOR DB CALLS      *
      * *** DO NOT MODIFY ***                                        *
      *--------------------------------------------------------------*
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
      *--------------------------------------------------------------*
      * SSA'S : FULLY QUALIFIED, INCLUDING NULL COMMAND CODES.       *
      * *** DO NOT MODIFY ***                                        *
      *--------------------------------------------------------------*
      *
       01 SSA-QUAL-SKILL.
          05 SEGMENT-NAME         PIC X(08)  VALUE 'SKILL '.
          05 COMMAND-CODES-START  PIC X(01)  VALUE '*'.
          05 COMMAND-CODES        PIC X(04)  VALUE '----'.
          05 QUAL-START           PIC X(01)  VALUE '('.
          05 QUAL-FIELD-NAME      PIC X(08)  VALUE 'SKCLASS '.
          05 QUAL-OPERATOR        PIC X(02)  VALUE ' ='.
          05 QUAL-VALUE           PIC X(08).
          05 QUAL-END             PIC X(01)  VALUE ')'.
      *
       01 SSA-QUAL-NAME.
          05 SEGMENT-NAME         PIC X(08)  VALUE 'NAME '.
          05 COMMAND-CODES-START  PIC X(01)  VALUE '*'.
          05 COMMAND-CODES        PIC X(04)  VALUE '----'.
          05 QUAL-START           PIC X(01)  VALUE '('.
          05 QUAL-FIELD-NAME      PIC X(08)  VALUE 'FULNAM '.
          05 QUAL-OPERATOR        PIC X(02)  VALUE ' ='.
          05 QUAL-VALUE           PIC X(42).
          05 QUAL-END             PIC X(01)  VALUE ')'.
      *
       01 SSA-QUAL-EXPR.
          05 SEGMENT-NAME         PIC X(08)  VALUE 'EXPR '.
          05 COMMAND-CODES-START  PIC X(01)  VALUE '*'.
          05 COMMAND-CODES        PIC X(04)  VALUE '----'.
          05 QUAL-START           PIC X(01)  VALUE '('.
          05 QUAL-FIELD-NAME      PIC X(08)  VALUE 'CLASSIF '.
          05 QUAL-OPERATOR        PIC X(02)  VALUE ' ='.
          05 QUAL-VALUE           PIC X(04).
          05 QUAL-END             PIC X(01)  VALUE ')'.
      *
       LINKAGE SECTION.
      *
      *--------------------------------------------------------------*
      * PCB MASKS                                                    *
      * - DO NOT MODIFY:                                             *
      * * THE ALTERNATE TP PCBS (ALT-PCB & EXP-PCB)                  *
      * * THE DB PCB (PCB-DB-SKILL)                                  *
      * - CREATE REQUIRED TP PCB                                     *
      * * COPY FROM MEMBER COBIOPCB AND COMPLETE IF NECESSARY        *
      *--------------------------------------------------------------*
      *
      *
       01 ALT-PCB.
          05 ALT-LTERM            PIC X(08).
          05 FILLER               PIC X(02).
          05 ALT-STATUS-CODE      PIC X(02).
      *
       01 EXP-PCB.
          05 EXP-LTERM            PIC X(08).
          05 FILLER               PIC X(02).
          05 EXP-STATUS-CODE      PIC X(02).
      *
       01 PCB-DB-SKILL.
          05 DBDNAME              PIC X(08).
          05 SEGMENT-LEVEL        PIC X(02).
          05 STATUS-CODE          PIC X(02).
          05 PROCOPT              PIC X(04).
          05 RESERVED             PIC S9(05) COMPUTATIONAL.
          05 SEGMENT-NAME         PIC X(08).
          05 KFBAREA-KEY-LENGTH   PIC S9(05) COMPUTATIONAL.
          05 NUMBER-OF-SENSEGS    PIC S9(05) COMPUTATIONAL.
          05 KFBAREA              PIC X(54).
      *
       PROCEDURE DIVISION.
      *
      *--------------------------------------------------------------*
      * PROGRAM ENTRY POINT                                          *
      * - COMPLETE WHERE NECESSARY                                   *
      *--------------------------------------------------------------*
      *
           ENTRY 'DLITCBL' USING ALT-PCB, EXP-PCB, PCB-DB-SKILL.
      *
      *
      *
      *
      *==============================================================*
      * LOGIC STARTS HERE                                            *
      *==============================================================*
      *    --- UNCOMMENT THIS CODE TO RUN !!! ---
      *
      *    PERFORM GET-INPUT-MESSAGE UNTIL IO-STATUS-CODE = 'QC'.
      *
      *    GOBACK.
      *
      *    --------------------------------------
      *
      *
       GET-INPUT-MESSAGE.
           CONTINUE.
      *
      *
      *==============================================================*
      * INSERT CODE TO GET INPUT MESSAGE:                            *
      * IF SUCCESSFUL, PERFORM PROCESS-INPUT-MESSAGE                 *
      * IF NO MORE MESSAGES, THEN RETURN                             *
      * IF ERROR, PERFORM PROCESS-UNEXPECTED-ERROR                   *
      *==============================================================*
      *
      *
       GET-INPUT-MESSAGE-END.
           EXIT.
      *
      *
      *==============================================================*
      * PROCESS-INPUT-MESSAGE PERFORMS THE DL/1 DB CALL & CHECKS     *
      * THE STATUS CODE FROM THE CALL                                *
      * *** DO NOT MODIFY ***                                        *
      *==============================================================*
      *
      *
       PROCESS-INPUT-MESSAGE.
      *
           MOVE KSKILL OF INPUT-MESSAGE-SEGMENT
              TO QUAL-VALUE OF SSA-QUAL-SKILL.
           MOVE KNAME OF INPUT-MESSAGE-SEGMENT
              TO QUAL-VALUE OF SSA-QUAL-NAME.
           MOVE KEXPR OF INPUT-MESSAGE-SEGMENT
              TO QUAL-VALUE OF SSA-QUAL-EXPR.
      *
           CALL 'CBLTDLI' USING GU,
                                PCB-DB-SKILL,
                                IOAREA-EXPR,
                                SSA-QUAL-SKILL,
                                SSA-QUAL-NAME,
                                SSA-QUAL-EXPR.
      *
      * CHECK STATUS CODE OF DATABASE RETRIEVAL CALL
      *
           IF STATUS-CODE OF PCB-DB-SKILL = ' '
              THEN
              PERFORM GOOD-DATABASE-CALL
           ELSE
              PERFORM BAD-DATABASE-CALL.
      *
       PROCESS-INPUT-MESSAGE-END.
           EXIT.
      *
      *
      *
      *
      *==============================================================*
      * INSERT CODE FOR SUCCESSFUL CALL CONDITION NEXT:              *
      *==============================================================*
       GOOD-DATABASE-CALL.
           CONTINUE.
      *
       GOOD-DATABASE-CALL-END.
           EXIT.
      *
      *
      *
      *
      *==============================================================*
      * INSERT CODE FOR *UN*-SUCCESSFUL CALL CONDITIONS NEXT:        *
      *==============================================================*
       BAD-DATABASE-CALL.
           CONTINUE.
      *
       BAD-DATABASE-CALL-END.
           EXIT.
      *
      *
      *
      *
      *==============================================================*
      * ANY OTHER ERRORS USE THIS ROUTINE                            *
      *==============================================================*
       PROCESS-UNEXPECTED-ERROR.
           CONTINUE.
      *
      *==============================================================*
      * A NORMAL IMS/DC APPLICATION SHOULD ABEND AT THIS POINT.
      * SINCE WE ARE IN A CLASSROOM/LAB MODE WE WILL EXECUTE A
      * GOBACK AND LOOK AT THE BTS OUTPUT FOR THE SOURCE OF OUR ERROR.
      *==============================================================*
      *
      *
       PROCESS-UNEXPECTED-ERROR-END.
           EXIT.
