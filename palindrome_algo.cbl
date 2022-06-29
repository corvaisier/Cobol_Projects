       IDENTIFICATION DIVISION.
       PROGRAM-ID. FOREX00C.
      *==============================================================*
      * PROGRAMME PALINDROME                                      *
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
       01  WS-CONSTANTES.
           02  WS-MOT-TEST-1       PIC X(20)  VALUE " RADAR ".
           02  WS-MOT-TEST-2       PIC X(20)  VALUE "RADAR".
           02  WS-MOT-TEST-3       PIC X(20)  VALUE " XY ".


       01  WS-INDICES  COMP-3 SYNC.
           02  WS-IDX-COURANT      PIC S9(4).
           02  WS-IDX-COURANT-INV  PIC S9(4).
           02  WS-IDX-NB-BL        PIC S9(4).
           02  WS-IDX-DEBUT-MOT    PIC S9(4).
           02  WS-IDX-FIN-MOT      PIC S9(4).
           02  WS-IS-PALINDROME    PIC  9(1).

       77  WS-MOT-TEST-TRAV        PIC X(20).


      *
      *==============================================================*
      * PROCEDURE                                                    *
      *==============================================================*
       PROCEDURE DIVISION.

      * Début du pgm

       MAIN.
           PERFORM INIT           *> Init du programme

           MOVE WS-MOT-TEST-1     TO WS-MOT-TEST-TRAV
           PERFORM TEST-PALINDROME *> Traitement principal
           PERFORM AFFICHAGE      *> Affichage du res final

           MOVE WS-MOT-TEST-2     TO WS-MOT-TEST-TRAV
           PERFORM TEST-PALINDROME *> Traitement principal
           PERFORM AFFICHAGE      *> Affichage du res final

           MOVE WS-MOT-TEST-3     TO WS-MOT-TEST-TRAV
           PERFORM TEST-PALINDROME *> Traitement principal
           PERFORM AFFICHAGE      *> Affichage du res final

           PERFORM FIN            *> Fin du programme
           GOBACK
           .

      * Initialisation du programme
       INIT.
           INITIALIZE           WS-INDICES
           INITIALIZE           WS-MOT-TEST-TRAV
           .

      * Traiement de controle si WS-MOT-TEST-TRAV est un palindrome ;
      * Résultat dans WS-IS-PALINDROME (0 ou 1)
       TEST-PALINDROME.
           DISPLAY 'Test de palindrome pour "' WS-MOT-TEST-TRAV '"'
           IF WS-MOT-TEST-TRAV = SPACE
              MOVE ZERO TO WS-IS-PALINDROME
              EXIT PARAGRAPH
           ELSE
              MOVE 1 TO WS-IS-PALINDROME
           END-IF

           *> Recherche du début du mot ==> WS-IDX-DEBUT-MOT
           MOVE 1            TO WS-IDX-DEBUT-MOT
           MOVE ZERO         TO WS-IDX-NB-BL
           INSPECT WS-MOT-TEST-TRAV
                   TALLYING     WS-IDX-DEBUT-MOT
                   FOR LEADING SPACE
           *> Recherche de la fin du mot ==> WS-IDX-FIN-MOT
           INSPECT FUNCTION REVERSE(WS-MOT-TEST-TRAV)
                   TALLYING     WS-IDX-NB-BL
                   FOR LEADING SPACE
           COMPUTE WS-IDX-FIN-MOT =
                   LENGTH OF WS-MOT-TEST-TRAV - WS-IDX-NB-BL

           MOVE WS-IDX-FIN-MOT  TO WS-IDX-COURANT-INV
      *    DISPLAY 'Debut mot = ' WS-IDX-DEBUT-MOT
      *    DISPLAY 'Fin mot   = ' WS-IDX-FIN-MOT
           *> Boucle de parcours du mot à tester,
           *> jusqu'à la moitié du mot
           PERFORM VARYING WS-IDX-COURANT
                   FROM    WS-IDX-DEBUT-MOT BY 1
                   UNTIL   WS-IDX-COURANT >= WS-IDX-COURANT-INV
              *> controle d'égalité sur les cacractères
              *> à partir du début et à partir de la fin
              IF WS-MOT-TEST-TRAV (WS-IDX-COURANT : 1)
               NOT = WS-MOT-TEST-TRAV (WS-IDX-COURANT-INV : 1)
                 MOVE ZERO TO WS-IS-PALINDROME
      *          DISPLAY 'Sortie a indice ' WS-IDX-COURANT
                 EXIT PERFORM
              END-IF
              SUBTRACT 1 FROM WS-IDX-COURANT-INV
           END-PERFORM
           .

      * Affichage du résultat
       AFFICHAGE.
           IF WS-IS-PALINDROME = 1
              DISPLAY 'Le mot "' FUNCTION TRIM(WS-MOT-TEST-TRAV) '"'
                      ' est un palindrome.'
           ELSE
              IF WS-MOT-TEST-TRAV = SPACE
                 DISPLAY 'Le mot a tester est vide.'
              ELSE
                 DISPLAY 'Le mot "' FUNCTION TRIM(WS-MOT-TEST-TRAV) '"'
                      ' N''est PAS un palindrome.'
              END-IF
           END-IF
           .

      * Fin du pgm
       FIN.
           CONTINUE.
