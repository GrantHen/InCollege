       *> Shared transcript and 1-digit menu helpers extracted from the
       *> main program so new feature work does not keep inflating it.
       PRINT-LINE.
           MOVE FUNCTION TRIM(LINE-TEXT) TO OUTPUT-REC
           DISPLAY FUNCTION TRIM(LINE-TEXT)
           WRITE OUTPUT-REC.

       READ-NEXT-INPUT.
           READ INPUT-FILE
               AT END
                   SET INPUT-EOF-YES TO TRUE
                   MOVE " " TO INPUT-REC
               NOT AT END
                   SET INPUT-EOF-NO TO TRUE
                   IF ECHO-ON
                       MOVE FUNCTION TRIM(INPUT-REC) TO LINE-TEXT
                       PERFORM PRINT-LINE
                   END-IF
           END-READ.

       GET-MENU-CHOICE.
           PERFORM READ-NEXT-INPUT
           IF INPUT-EOF-YES
               MOVE 0 TO MENU-CHOICE
           ELSE
               IF INPUT-REC(1:1) >= "0" AND INPUT-REC(1:1) <= "9"
                   COMPUTE MENU-CHOICE = FUNCTION NUMVAL(INPUT-REC(1:1))
               ELSE
                   MOVE 0 TO MENU-CHOICE
               END-IF
           END-IF.

       GET-POST-CHOICE.
           PERFORM READ-NEXT-INPUT
           IF INPUT-EOF-YES
               MOVE 0 TO POST-CHOICE
           ELSE
               IF INPUT-REC(1:1) >= "0" AND INPUT-REC(1:1) <= "9"
                   COMPUTE POST-CHOICE = FUNCTION NUMVAL(INPUT-REC(1:1))
               ELSE
                   MOVE 0 TO POST-CHOICE
               END-IF
           END-IF.

       GET-SKILL-CHOICE.
           PERFORM READ-NEXT-INPUT
           IF INPUT-EOF-YES
               MOVE 6 TO SKILL-CHOICE
           ELSE
               IF INPUT-REC(1:1) >= "0" AND INPUT-REC(1:1) <= "9"
                   COMPUTE SKILL-CHOICE = FUNCTION NUMVAL(INPUT-REC(1:1))
               ELSE
                   MOVE 0 TO SKILL-CHOICE
               END-IF
           END-IF.
