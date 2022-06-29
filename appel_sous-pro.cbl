       IDENTIFICATION DIVISION.
       PROGRAM-ID. FOREX007.
      *==============================================================*
      * PROGRAMME appel de sous-pro avec param*
      *==============================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       SOURCE-COMPUTER. IBM-3090.
       OBJECT-COMPUTER. IBM-3090.
      *
      *==============================================================*
      * DATA                                                         *
      *==============================================================*
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-OP1.
         05  WS-OP1-C1  PIC 9(10).

       01  WS-OP2.
         05  WS-OP2-C1  PIC 9(10).

       01  WS-OP3-RES   PIC 9(10).

       77  WS-CALL-FOREX008   PIC X(10) VALUE 'FOREX008'.
      *
      *==============================================================*
      * PROCEDURE                                                    *
      *==============================================================*
       PROCEDURE DIVISION.

      * Début du pgm
       MAIN.

           DISPLAY 'TEST APPEL 1'

           MOVE 2        TO  WS-OP1-C1
           MOVE 2        TO  WS-OP2-C1

           DISPLAY 'WS-OP1 avant appel "'  WS-OP1  '"'
           DISPLAY 'WS-OP2 avant appel "'  WS-OP2  '"'

           *> FOREX008 va aditionner op1 et op2 dans op3-res
           CALL WS-CALL-FOREX008  USING WS-OP1 WS-OP2 WS-OP3-RES

           DISPLAY 'WS-OP3 apres appel "'  WS-OP3-RES  '"'

           DISPLAY '** Fin TEST APPEL 1'

      * Fin du pgm
           GOBACK.
