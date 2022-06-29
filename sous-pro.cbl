       IDENTIFICATION DIVISION.
       PROGRAM-ID. FOREX008.
      *==============================================================*
      * PROGRAMME sous-pro avec param         *
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

       LINKAGE SECTION.
       01  LK-R1.
         05  LK-R1-C1  PIC 9(10).
       01  LK-R2.
         05  LK-R2-C1  PIC 9(10).
       01  LK-R3-RES   PIC 9(10).

      *
      *==============================================================*
      * PROCEDURE                                                    *
      *==============================================================*
       PROCEDURE DIVISION USING LK-R1 LK-R2 LK-R3-RES .

      * Début du pgm

       MAIN.
           DISPLAY 'Debut de FOREX008'
           DISPLAY 'Param 1 vaut ' LK-R1
           DISPLAY 'Param 2 vaut ' LK-R2

           COMPUTE LK-R3-RES = LK-R1-C1 + LK-R2-C1

           DISPLAY 'Fin de FOREX008'

      * Fin du pgm
           GOBACK.
