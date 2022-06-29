       IDENTIFICATION DIVISION.
       PROGRAM-ID. FOREX010.
      *==============================================================*
      * PROGRAMME manip XML                             *
      *   -                                                          *
      *==============================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       SOURCE-COMPUTER. IBM-3090.
       OBJECT-COMPUTER. IBM-3090.

       INPUT-OUTPUT SECTION.
       File-Control.
           SELECT FIC1-XML
                     ASSIGN to FIC1XML
                     ORGANIZATION is SEQUENTIAL
                     ACCESS MODE is SEQUENTIAL
                     FILE STATUS is WS-STATUS-FIC1-XML.

      *
      *==============================================================*
      * DATA                                                         *
      *==============================================================*
       DATA DIVISION.
       FILE SECTION.
       FD  FIC1-XML.
       01  FIC1-XML-REC  PIC X(80).


       WORKING-STORAGE SECTION.
       01  WS-INDICATEURS.
           02  WS-STATUS-FIC1-XML       PIC 9(02)   VALUE ZERO.
           02  WS-INDIC-FIN-FIC1-XML    PIC 9       VALUE ZERO.
               88 WS-FIN-FIC1-XML                   VALUE 1.

       01  WS-CALCULS  COMP SYNC.
           02  WS-NB-REC-LUS-FIC1-XML   PIC S9(9)   VALUE ZERO.

       01  WS-ZONES-AFFICHAGE.
         02  WS-IDX-EDT             PIC ZZ9.
         02  WS-MES-SORTIE          PIC X(80).
      *
      *==============================================================*
      * PROCEDURE                                                    *
      *==============================================================*
       PROCEDURE DIVISION.

      * Début du pgm

       MAIN.
           PERFORM INIT           *> Init du programme
           PERFORM TRAITEMENT     *> Traitement principal en boucle
             UNTIL WS-FIN-FIC1-XML
           PERFORM FIN            *> Fin du programme
           .

       INIT.
           INITIALIZE  WS-INDICATEURS
           INITIALIZE  WS-CALCULS
           INITIALIZE  WS-ZONES-AFFICHAGE

           OPEN INPUT FIC1-XML
           .

       TRAITEMENT.
           READ FIC1-XML
            AT END
              SET WS-FIN-FIC1-XML      TO TRUE
            NOT AT END
              ADD 1                TO WS-NB-REC-LUS-FIC1-XML
              PERFORM TRAITE-LIGNE-XML
      *       DISPLAY 'Rec lu '       FIC1-XML-CH1 FIC1-XML-CH2
           END-READ
           .

       TRAITE-LIGNE-XML.
           DISPLAY "Parsing de XML:" FIC1-XML-REC
           XML PARSE FIC1-XML-REC
               PROCESSING PROCEDURE PARSE-EVENT
            ON EXCEPTION CONTINUE
            NOT ON EXCEPTION
              CONTINUE
           END-XML.

       PARSE-EVENT.
           EVALUATE XML-EVENT
            WHEN 'START-OF-DOCUMENT'
              DISPLAY 'Debut parsing XML'
            WHEN 'END-OF-DOCUMENT'
              DISPLAY 'Fin parsing XML'
            WHEN 'START-OF-ELEMENT'
              DISPLAY 'Balise Debut ' XML-TEXT
            WHEN 'END-OF-ELEMENT'
              DISPLAY 'Balise Fin   ' XML-TEXT
            WHEN 'ATTRIBUTE-NAME'
              DISPLAY 'Nom attribut ' XML-TEXT
            WHEN 'ATTRIBUTE-CHARACTERS'
              DISPLAY 'Val attribut ' XML-TEXT
            WHEN 'CONTENT-CHARACTERS'
              DISPLAY 'Contenu      ' XML-TEXT
            WHEN OTHER
              DISPLAY 'Autre (' XML-EVENT ') ' XML-TEXT
           END-EVALUATE.



      * Fin du pgm
       FIN.
           MOVE WS-NB-REC-LUS-FIC1-XML  TO WS-IDX-EDT
           INITIALIZE WS-MES-SORTIE
           STRING "NB articles lus = " WS-IDX-EDT
                  DELIMITED BY SIZE
             INTO WS-MES-SORTIE

           CLOSE FIC1-XML
           GOBACK.
