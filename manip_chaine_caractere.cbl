       IDENTIFICATION DIVISION.
       PROGRAM-ID. FOREX00A.
      *==============================================================*
      * PROGRAMME de manipulation de chaine                       *
      *   - Concaténation : String,
      *   - Déconcaténation : Unstring
      *   - REcherches et remplacement : Inspect                     *
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
           02  WS-NB-01       PIC 9(4) COMP-3 .

       01  WS-PERSONNE-GRP.
           02  WS-PRENOM      PIC X(10).
           02  WS-NOM         PIC X(10).
       77  WS-PRN-NOM-CONCAT  PIC X(25).

       77  WS-DATE-ELEM       PIC X(10)   VALUE '31-05-2022'.
       01  WS-DATE-GRP.
           02 WS-JOUR         PIC X(02).
           02 WS-MOIS         PIC X(02).
           02 WS-ANNEE        PIC X(04).

       01  WS-MA-DONNEE       PIC X(60)
                              VALUE 'Je suis une donnée avec un e.'.

      *
      *==============================================================*
      * PROCEDURE                                                    *
      *==============================================================*
       PROCEDURE DIVISION.

      * Début du pgm
       MAIN.
           INITIALIZE   WS-TEST-DATA

           PERFORM EXO-CONCAT
           PERFORM EXO-DECONCAT
           PERFORM EXO-CPT-ET-CONVERSION

      * Fin du pgm
           GOBACK.

      * Concatène un prénom et un nom
       EXO-CONCAT.
           MOVE 'UnPrénom'  TO WS-PRENOM
           MOVE 'UnNom'     TO WS-NOM
           INITIALIZE          WS-PRN-NOM-CONCAT
           STRING WS-PRENOM DELIMITED BY SPACE
                  " "       DELIMITED BY SIZE
                  WS-NOM    DELIMITED BY SPACE
             INTO WS-PRN-NOM-CONCAT

           DISPLAY 'Prénom nom = ' WS-PRN-NOM-CONCAT
           .

      * Découpe une date en jour, mois, année
       EXO-DECONCAT.
           UNSTRING WS-DATE-ELEM
                    DELIMITED BY "-"
              INTO  WS-JOUR
                    WS-MOIS
                    WS-ANNEE
           DISPLAY ' '
           DISPLAY 'Jour = '    WS-JOUR
                   ', Mois = ' WS-MOIS
                   ', Année = ' WS-ANNEE
           .

      * Remplace les caractères accentués par car non accentué
       EXO-CPT-ET-CONVERSION.
           INSPECT WS-MA-DONNEE TALLYING WS-NB-01 FOR ALL "é"

           INSPECT WS-MA-DONNEE CONVERTING "àâéèô"
                                        TO "aaeeo"

           DISPLAY 'Nb de é avant conversion ' WS-NB-01
           DISPLAY 'Valeur de ma donnée ' WS-MA-DONNEE
           .
