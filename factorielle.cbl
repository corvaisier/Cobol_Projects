       IDENTIFICATION DIVISION.
       PROGRAM-ID. FOREX003.
      *==============================================================*
      * PROGRAMME TP factorielle                                     *
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
      *    N : Nombre dont on veut calculer la factorielle (constante)
           02  WS-N                PIC S9(9) COMP-3  VALUE 12.

      *    Zones d'indices et de calculs
       01  WS-INDICES  COMP-3 SYNC.
      *    Indice de calcul
           02  WS-IDX-CALC         PIC S9(9).
      *    Factorielle en cours de calcul
           02  WS-FACTORIELLE      PIC S9(9).
      *    Nb de répétitions du calcul (pour test performance slt)
           02  WS-IDX-REPET        PIC S9(9).
      *    Nb de passages dans la boucle (ajoute un calcul pour le test)
       01  WS-NB-PASSAGES      PIC S9(10)  COMP-3.

      * Affichages (en fin de traitementà)
       01  WS-ZONES-AFFICHAGE.
      *  Version num edité de N
         02  WS-N-EDT            PIC ZZZ.ZZZ.ZZ9.
      *  Version num edité de la factorielle calculée
         02  WS-FACTORIELLE-EDT  PIC ZZZ.ZZZ.ZZ9.
      *  Version num edité du nb de passages
         02  WS-NB-PASSAGES-EDT  PIC Z.ZZZ.ZZZ.ZZ9.

      *
      *==============================================================*
      * PROCEDURE                                                    *
      *==============================================================*
       PROCEDURE DIVISION.

      * Début du pgm

       MAIN.
           PERFORM INIT           *> Init du programme
           *> répétition du calcul de factorielle 100.000.000 fois
           *> pour test performance Comp-3 / Comp
           PERFORM VARYING WS-IDX-REPET
                   FROM 1 BY 1
                   UNTIL WS-IDX-REPET > 10
              PERFORM TRAITEMENT     *> Traitement principal
           END-PERFORM
           PERFORM AFFICHAGE      *> Affichage du res final
           PERFORM FIN            *> Fin du programme
           .

      * Initialisation du programme
       INIT.
           MOVE WS-N         TO WS-N-EDT
           DISPLAY 'Factorielle de ' WS-N-EDT ' demandee'
           INITIALIZE           WS-INDICES
           INITIALIZE           WS-NB-PASSAGES
           .

      * Traiement de calcul de factorielle de WS-N
       TRAITEMENT.
           MOVE 1            TO WS-FACTORIELLE
           PERFORM VARYING WS-IDX-CALC
                   FROM    1 BY 1
                   UNTIL   WS-IDX-CALC > WS-N
              COMPUTE WS-FACTORIELLE = WS-FACTORIELLE * WS-IDX-CALC
              ADD 1 TO WS-NB-PASSAGES
           END-PERFORM
           .

      * Affichage du résultat
       AFFICHAGE.
           MOVE WS-FACTORIELLE TO WS-FACTORIELLE-EDT
           MOVE WS-NB-PASSAGES TO WS-NB-PASSAGES-EDT
           DISPLAY 'FACTORIELLE DE '  WS-N-EDT
                   ' = '              WS-FACTORIELLE-EDT
           DISPLAY 'NB PASSAGES = '   WS-NB-PASSAGES-EDT
           .

      * Fin du pgm
       FIN.
           GOBACK.
