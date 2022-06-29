       IDENTIFICATION DIVISION.
       PROGRAM-ID.  FABEX005.
      *==============================================================*
      * PROGRAMME TP CALCUL MOYEN TABLEAU                            *
      *==============================================================*

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.
           IBM-370.

       OBJECT-COMPUTER.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS01 COMP-3 SYNC.
           05 WS01-IND1         PIC 9(10).
           05 WS01-INDEX        PIC 9(10).
           05 WS01-OCCURS       PIC 9(03) .
           05 WS01-TOTAL        PIC 9(16)V99.
           05 WS01-MOY          PIC 9(16)V99.
       01 TA00-TABLE1.
          05 TA00-LIGNES  OCCURS 3
                    DEPENDING ON WS01-OCCURS
                    ASCENDING KEY TA00-MTHT
                    INDEXED   BY  WS01-INDEX.
             10 TA00-MTHT       PIC 9(16)V99.
       01 WE01-EDIT.
          05 WE01-MTHT          PIC Z.ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99.
          05 WE01-MOY           PIC Z.ZZZ.ZZZ.ZZZ.ZZZ.ZZ9,99.
       PROCEDURE DIVISION.
       MAIN.
           PERFORM INIT
      **** CALCUL TOTAL MONTANTS
           PERFORM VARYING WS01-IND1
              FROM 1 BY 1
              UNTIL WS01-IND1 > WS01-OCCURS
                   PERFORM TOTAL
           END-PERFORM
      **** CALCUL MOYENNE DES MONTANTS
           PERFORM MOYENNE
      **** TRI TABLEAU
           PERFORM TRI
      **** RECHERCHE TABLEAU
           PERFORM RECHTABLE
      **** AFFICHAGE RESULTATS
           PERFORM RESULTAT
      **** FIN PROGRAMME
           GOBACK.

      **** INITIALISATION DES DONNEES
       INIT.
           MOVE 3         TO WS01-OCCURS
           MOVE 1         TO WS01-IND1
           MOVE ZERO      TO WS01-TOTAL
           MOVE 145,12    TO TA00-MTHT(1)
           MOVE 327,45    TO TA00-MTHT(2)
           MOVE 126,89    TO TA00-MTHT(3)
           .
      **** CALCUL TOTAL
       TOTAL.
      D    DISPLAY "TA00-MTHT(WS01-IND1) : " TA00-MTHT(WS01-IND1)
           COMPUTE WS01-TOTAL = WS01-TOTAL + TA00-MTHT(WS01-IND1)
           .
      **** CALCUL MOYENNE
       MOYENNE.
           COMPUTE WS01-MOY ROUNDED = WS01-TOTAL / WS01-OCCURS
           .
      **** TRI TABLEAU MONTANT
       TRI.
      D    DISPLAY "TRI"
           SORT TA00-LIGNES
           DISPLAY "Montant + petit : " TA00-MTHT(1)
           DISPLAY "Montant + grand : " TA00-MTHT(WS01-OCCURS)
           .
      **** RECHERCHE TABLEAU
       RECHTABLE.
           DISPLAY "RECHERCHE"
      *     SEARCH ALL TA00-LIGNES
      *           AT END
      *              DISPLAY "Pas trouvé"
      *           WHEN TA00-MTHT (WS01-IND1) = "327,45"
      *              DISPLAY "Valeur trouvé : " 327,45
      *           END-SEARCH
           .
      **** AFFICHAGE RESULTATS
       RESULTAT.
           MOVE WS01-TOTAL   TO WE01-MTHT
           MOVE WS01-MOY     TO WE01-MOY
           DISPLAY "TOTAL            : " WE01-MTHT
           DISPLAY "MOYENNE ARRONDIE : " WE01-MOY
           .
