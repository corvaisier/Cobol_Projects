       IDENTIFICATION DIVISION.
       PROGRAM-ID. FOREX009.
      *==============================================================*
      * PROGRAMME manip de fichier séquentiel                     *
      *   - FIC1 en lecture                                          *
      *   - FIC2-STATS en écriture                                   *
      *==============================================================*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-3090.
       OBJECT-COMPUTER. IBM-3090.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       File-Control.
      * FIC1 en lecture
           SELECT FICHIER1
                     ASSIGN to FIC1
                     ORGANIZATION is SEQUENTIAL
                     ACCESS MODE is SEQUENTIAL
                     FILE STATUS is WS-STATUS-FIC1.
      * FIC2-STATS en écriture : on écrira le nombre de lignes lues
      *                          de FIC1
           SELECT FIC2-STATS
                     ASSIGN to FIC2STAT
                     ORGANIZATION is SEQUENTIAL
                     ACCESS MODE is SEQUENTIAL
                     FILE STATUS is WS-STATUS-FIC2.

      *
      *==============================================================*
      * DATA                                                         *
      *==============================================================*
       DATA DIVISION.
       FILE SECTION.
       FD  FICHIER1.
       01  FIC1-REC.
         10 FIC1-CH1   PIC X(10).
         10 FIC1-CH2   PIC X(20).
         10 FILLER     PIC X(50).

       FD  FIC2-STATS.
       01  FIC2-REC    PIC X(80).

       WORKING-STORAGE SECTION.
       01  WS-INDICATEURS.
           02  WS-STATUS-FIC1       PIC 9(02)   VALUE ZERO.
           02  WS-INDIC-FIN-FIC1    PIC 9       VALUE ZERO.
               88 WS-FIN-FIC1                   VALUE 1.
           02  WS-STATUS-FIC2       PIC 9(02)   VALUE ZERO.

       01  WS-CALCULS  COMP SYNC.
           02  WS-NB-REC-LUS-FIC1   PIC S9(9)   VALUE ZERO.

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
           PERFORM TRAITEMENT     *> Traitement principal EN BOUCLE
             UNTIL WS-FIN-FIC1
           PERFORM FIN            *> Fin du programme
           .

       INIT.
      *    Init des zones de travail
           INITIALIZE  WS-INDICATEURS
           INITIALIZE  WS-CALCULS
           INITIALIZE  WS-ZONES-AFFICHAGE

      *    Ouverture des fichiers
           OPEN INPUT FICHIER1
           OPEN OUTPUT FIC2-STATS
           .

      * 1 lecture de FIC1
       TRAITEMENT.
           *> Lecture d'un record dans le fichier
           READ FICHIER1
            AT END
              SET WS-FIN-FIC1      TO TRUE
            NOT AT END
              ADD 1                TO WS-NB-REC-LUS-FIC1
              DISPLAY 'Rec lu '       FIC1-CH1 FIC1-CH2
           END-READ
           .

      * Fin du pgm
       FIN.
           *> Préparation du message à écrire dans FIC2-STATS
           MOVE WS-NB-REC-LUS-FIC1  TO WS-IDX-EDT
           INITIALIZE WS-MES-SORTIE
           STRING "NB articles lus = " WS-IDX-EDT
                  DELIMITED BY SIZE
             INTO WS-MES-SORTIE
           *> Ecriture dans FIC2-STATS
           WRITE FIC2-REC FROM WS-MES-SORTIE

           *> Fermeture des fichiers
           CLOSE FICHIER1
                 FIC2-STATS

           *> Sortie du pgm
           GOBACK.
