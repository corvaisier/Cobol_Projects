       IDENTIFICATION DIVISION.
       PROGRAM-ID. FOREX006.
      *==============================================================*
      * PROGRAMME TP utilisation de fonction intrinsèque             *
      *   -                                                          *
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
           02  WS-STR               PIC X(15)   VALUE '  123,45  '.
       01  WS-DATE-21               PIC X(21).

       01  WS-CALCULS  SYNC.
           02  WS-NUMVAL-STR   COMP-2    VALUE ZERO.

       01  WS-ZONES-AFFICHAGE.
         02  WS-NUM-EDT             PIC +ZZZ.ZZZ.ZZ9,99.
      *
      *==============================================================*
      * PROCEDURE                                                    *
      *==============================================================*
       PROCEDURE DIVISION.

      * Début du pgm

       MAIN.
           PERFORM INIT           *> Init du programme
           PERFORM TRAITEMENT     *> Traitement principal
           PERFORM FIN            *> Fin du programme
           .

       INIT.
           INITIALIZE  WS-CONSTANTES ALL TO VALUE
           INITIALIZE  WS-CALCULS
           INITIALIZE  WS-ZONES-AFFICHAGE

           .

       TRAITEMENT.
      *    Fonction intrinsèque sur la date et heure courante
           MOVE FUNCTION CURRENT-DATE   TO WS-DATE-21
           DISPLAY "Nous sommes le "
                   WS-DATE-21 (1:4)  "-"
                   WS-DATE-21 (5:2)  "-"
                   WS-DATE-21 (7:2)
                   "."
           DISPLAY "Il est "
                   WS-DATE-21  (9:2) ":"
                   WS-DATE-21  (11:2)
                   "."
           DISPLAY ' '

      *    Fonction intrinsèque NUMVAL
           COMPUTE WS-NUMVAL-STR = FUNCTION NUMVAL (WS-STR)
           MOVE WS-NUMVAL-STR            TO WS-NUM-EDT
           DISPLAY "WS-STR vaut " WS-NUM-EDT
                    "."

      *    Fonctions intrinsèques sur la casse
           DISPLAY "Chaine en minuscules = "
                   FUNCTION LOWER-CASE ('ABCDEF')
           DISPLAY "Chaine en majuscules = "
                   FUNCTION UPPER-CASE ('Abcedf')
           .

      * Fin du pgm
       FIN.
           GOBACK.
