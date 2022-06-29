       IDENTIFICATION DIVISION.
       PROGRAM-ID. FOREX00B.
      *==============================================================*
      * PROGRAMME CALCULS                                         *
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
           02  WS-N                PIC S9(9) COMP-3  VALUE 10.
           02  CSTE-TVA            PIC S9(1)V999 COMP-3 VALUE 0,10.

       01  WS-TAB-MONTANTS.
         10 WS-TAB-MONTANT OCCURS 10
                       DEPENDING ON  WS-N
                       ASCENDING KEY WS-MONTANT
                       INDEXED BY WS-IDX-MONTANT.
           15 WS-MONTANT              PIC S9(09)V99.

       01  WS-TAB-TVAS.
         10 WS-TVA     OCCURS 10      PIC S9(09)V99.

       01  WS-INDICES  COMP-3 SYNC.
           02  WS-IDX                 PIC  9(04).
           02  WS-TOTAL-TRONQUE       PIC S9(10)V99.
           02  WS-TOTAL-ARRONDI       PIC S9(10)V99.
           02  WS-MOYENNE-TRONQUEE    PIC S9(09)V99.
           02  WS-MOYENNE-ARRONDIE    PIC S9(09)V99.
           02  WS-ECART-MOYEN-TRONQUE PIC S9(09)V99.
           02  WS-ECART-MOYEN-ARRONDI PIC S9(09)V99.
           02  WS-TOTAL-ECARTS        PIC S9(10)V99.

           02  WS-TOTAL-TVA           PIC S9(10)V99.
           02  WS-TOTAL-TTC           PIC S9(10)V99.

           02  WS-VAR-TROP-PETITE     PIC S9(04)V99.
           02  WS-RES-QUELCONQUE      PIC S9(10)V99.

       01  WS-ZONES-AFFICHAGE.
         02  WS-N-EDT                 PIC  ZZ9.
         02  WS-IDX-EDT               PIC  ZZ9.
         02  WS-MONTANT-EDT           PIC  ZZZ.ZZZ.ZZ9,99.
         02  WS-TOTAL-TRONQUE-EDT     PIC  Z.ZZZ.ZZZ.ZZ9,99.
         02  WS-TOTAL-ARRONDI-EDT     PIC  Z.ZZZ.ZZZ.ZZ9,99.
         02  WS-MOYENNE-TRONQUEE-EDT  PIC  Z.ZZZ.ZZZ.ZZ9,99.
         02  WS-MOYENNE-ARRONDIE-EDT  PIC  Z.ZZZ.ZZZ.ZZ9,99.
         02  WS-ECART-MOYEN-ARRONDI-EDT PIC Z.ZZZ.ZZZ.ZZ9,99.

         02  WS-TOTAL-TVA-EDT         PIC  Z.ZZZ.ZZZ.ZZ9,99.
         02  WS-TOTAL-TTC-EDT         PIC  Z.ZZZ.ZZZ.ZZ9,99.


      *
      *==============================================================*
      * PROCEDURE                                                    *
      *==============================================================*
       PROCEDURE DIVISION.

      * Début du pgm

       MAIN.
           PERFORM INIT           *> Init du programme
           PERFORM TRAITEMENT-STATS *> Traitement DES STATS
           PERFORM TRAITEMENT-TVA   *> Traitement DE LA TVA
           PERFORM AFFICHAGE      *> Affichage du res final
           PERFORM TRI-ET-RECH
           PERFORM TEST-RECUP-ERR-01 *> Calculs en erreurs
           PERFORM FIN            *> Fin du programme
           GOBACK
           .

      * Initialisation du programme
       INIT.
           MOVE  10000       TO WS-MONTANT (01)
           MOVE  15000       TO WS-MONTANT (02)
           MOVE  20000       TO WS-MONTANT (03)
           MOVE  05000,50    TO WS-MONTANT (04)
           MOVE     10,7     TO WS-MONTANT (05)
           MOVE  08000,25    TO WS-MONTANT (06)
           MOVE  12000       TO WS-MONTANT (07)
           MOVE  13000       TO WS-MONTANT (08)
           MOVE  07000       TO WS-MONTANT (09)
           MOVE  30000       TO WS-MONTANT (10)

           INITIALIZE           WS-TAB-TVAS

           DISPLAY 'CALCUL DE TOTAL, MOYENNE, ... '
           DISPLAY 'Version avec Depending On '
           MOVE 10           TO WS-N
           MOVE WS-N         TO WS-N-EDT
           INITIALIZE           WS-INDICES
           .

      * Traiement de calcul des total, moyenne, ecart-type
       TRAITEMENT-STATS.
           MOVE ZERO         TO WS-TOTAL-TRONQUE
                                WS-TOTAL-ARRONDI
                                WS-TOTAL-ECARTS
           PERFORM VARYING WS-IDX
                   FROM    1 BY 1
                   UNTIL   WS-IDX > WS-N
              ADD WS-MONTANT (WS-IDX)  TO WS-TOTAL-TRONQUE
              ADD WS-MONTANT (WS-IDX)  TO WS-TOTAL-ARRONDI ROUNDED
           END-PERFORM
           COMPUTE WS-MOYENNE-TRONQUEE = WS-TOTAL-TRONQUE / WS-N
           COMPUTE WS-MOYENNE-ARRONDIE ROUNDED = WS-TOTAL-ARRONDI / WS-N

           PERFORM VARYING WS-IDX
                   FROM    1 BY 1
                   UNTIL   WS-IDX > WS-N
              IF WS-MONTANT (WS-IDX) >= WS-MOYENNE-ARRONDIE
                 COMPUTE WS-TOTAL-ECARTS = WS-TOTAL-ECARTS
                         + (WS-MONTANT (WS-IDX) - WS-MOYENNE-ARRONDIE)
              ELSE
                 COMPUTE WS-TOTAL-ECARTS = WS-TOTAL-ECARTS
                         + (WS-MOYENNE-ARRONDIE - WS-MONTANT (WS-IDX))
              END-IF
           END-PERFORM
           COMPUTE WS-ECART-MOYEN-ARRONDI ROUNDED = WS-TOTAL-ECARTS
                                                 / WS-N
           .

      * Traitement de calcul de la TVA et TTC
       TRAITEMENT-TVA.
           MOVE ZERO                TO WS-TOTAL-TVA
                                       WS-TOTAL-TTC
           PERFORM VARYING WS-IDX
                   FROM    1 BY 1
                   UNTIL   WS-IDX > WS-N
              COMPUTE WS-TVA (WS-IDX) ROUNDED = WS-MONTANT (WS-IDX)
                                              * CSTE-TVA
              COMPUTE WS-TOTAL-TVA ROUNDED = WS-TOTAL-TVA
                                           + WS-TVA (WS-IDX)
              COMPUTE WS-TOTAL-TTC ROUNDED = WS-TOTAL-TTC
                                           + WS-MONTANT (WS-IDX)
                                           + WS-TVA (WS-IDX)
           END-PERFORM
           .

      * Affichage du résultat
       AFFICHAGE.
           MOVE WS-TOTAL-TRONQUE     TO WS-TOTAL-TRONQUE-EDT
           MOVE WS-TOTAL-ARRONDI     TO WS-TOTAL-ARRONDI-EDT
           MOVE WS-MOYENNE-TRONQUEE  TO WS-MOYENNE-TRONQUEE-EDT
           MOVE WS-MOYENNE-ARRONDIE  TO WS-MOYENNE-ARRONDIE-EDT
           MOVE WS-ECART-MOYEN-ARRONDI TO WS-ECART-MOYEN-ARRONDI-EDT
           DISPLAY 'TOTAL ARRONDI    = '  WS-TOTAL-ARRONDI-EDT
           DISPLAY 'TOTAL TRONQUE    = '  WS-TOTAL-TRONQUE-EDT
           DISPLAY 'MOYENNE ARRONDIE = '  WS-MOYENNE-ARRONDIE-EDT
           DISPLAY 'MOYENNE TRONQUEE = '  WS-MOYENNE-TRONQUEE-EDT
           DISPLAY 'ECART-MOYEN ARRN.= '  WS-ECART-MOYEN-ARRONDI-EDT

           MOVE WS-TOTAL-TVA         TO WS-TOTAL-TVA-EDT
           MOVE WS-TOTAL-TTC         TO WS-TOTAL-TTC-EDT
           DISPLAY 'TOTAL TVA        = '  WS-TOTAL-TVA-EDT
           DISPLAY 'TOTAL TTC        = '  WS-TOTAL-TTC-EDT
           .

       TRI-ET-RECH.
           SORT WS-TAB-MONTANT
           MOVE WS-MONTANT (1)        TO WS-MONTANT-EDT
           DISPLAY 'Montant le + petit ' WS-MONTANT-EDT
           MOVE WS-MONTANT (WS-N)     TO WS-MONTANT-EDT
           DISPLAY 'Montant le + grand ' WS-MONTANT-EDT

           SEARCH ALL WS-TAB-MONTANT
             AT END  DISPLAY 'Pas trouvé'
             WHEN WS-MONTANT (WS-IDX-MONTANT) = 12000
                SET WS-IDX          TO WS-IDX-MONTANT
                MOVE WS-IDX         TO WS-IDX-EDT
                DISPLAY 'Trouvé à l''indice ' WS-IDX-EDT
           END-SEARCH
           .

       TEST-RECUP-ERR-01.
           COMPUTE WS-VAR-TROP-PETITE ROUNDED = WS-TOTAL-ARRONDI
            ON SIZE ERROR
              DISPLAY 'Pb TAILLE dans le calcul'
           END-COMPUTE

           MOVE ZERO TO WS-VAR-TROP-PETITE
           COMPUTE WS-RES-QUELCONQUE = WS-TOTAL-ARRONDI
                                     / WS-VAR-TROP-PETITE
            ON SIZE ERROR
              DISPLAY 'Pb div par zéro'
           END-COMPUTE
           .

      * Fin du pgm
       FIN.
           DISPLAY 'Fin du programme'
           .
