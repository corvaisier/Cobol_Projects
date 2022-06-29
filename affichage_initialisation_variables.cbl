       IDENTIFICATION DIVISION.
       PROGRAM-ID. FOREX001.
      *==============================================================*
      * PROGRAMME INITIALISATION - AFFICHAGE RUBRIQUES               *
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
       01  WS-TEST-DATA.
           02  WS-ZONE-NUM-01 PIC S9(9).
           02  WS-ZONE-NUM-02 PIC S9(9) COMP-3.

        77 WS-BLANC           PIC X(02)  VALUE X'4040'.
        77 WS-CRLF            PIC X(02)  VALUE X'0D0A'.

       01  WS-MA-DONNEE       PIC X(60)
                              VALUE 'Je suis une donnée assez longue,
      -                             'et je le vaux bien.'.
       01  WS-AUTRE-DONNEE   PIC X(60)   VALUE 'Je suis une autre donnée
      -     ' avec une value continuée aussi.'.

      *
      *==============================================================*
      * PROCEDURE                                                    *
      *==============================================================*
       PROCEDURE DIVISION.

      * Début du pgm

       MAIN.
           INITIALIZE   WS-TEST-DATA
           MOVE ZERO          TO WS-ZONE-NUM-01
           ADD 1              TO WS-ZONE-NUM-01
           DISPLAY 'Valeur de WS-ZONE-NUM-01  ' WS-ZONE-NUM-01
           DISPLAY 'Valeur de WS-MA-DONNEE    ' WS-MA-DONNEE
           DISPLAY 'Valeur de WS-AUTRE-DONNEE ' WS-AUTRE-DONNEE

      * Fin du pgm
           GOBACK.
