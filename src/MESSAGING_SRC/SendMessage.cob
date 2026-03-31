      *> ============================================================
      *> WEEK 8: Initialize messages file (create if not exists)
      *> ============================================================
       INIT-MESSAGES-FILE.
           OPEN EXTEND MESSAGES-FILE
           IF MSG-FILE-STATUS = "35"
               OPEN OUTPUT MESSAGES-FILE
               CLOSE MESSAGES-FILE
           ELSE
               CLOSE MESSAGES-FILE
           END-IF.
      *> ============================================================
      *> WEEK 8: Messages sub-menu
      *> ============================================================
       MESSAGES-MENU.
           MOVE 0 TO MSG-MENU-CHOICE
           PERFORM UNTIL MSG-MENU-CHOICE = 3
               MOVE "--- Messages Menu ---" TO LINE-TEXT
               PERFORM PRINT-LINE
               MOVE "1. Send a New Message" TO LINE-TEXT
               PERFORM PRINT-LINE
               MOVE "2. View My Messages" TO LINE-TEXT
               PERFORM PRINT-LINE
               MOVE "3. Back to Main Menu" TO LINE-TEXT
               PERFORM PRINT-LINE
               MOVE "Enter your choice: " TO LINE-TEXT
               PERFORM PRINT-LINE

               PERFORM READ-NEXT-INPUT
               IF INPUT-EOF-YES
                   MOVE 3 TO MSG-MENU-CHOICE
               ELSE
                   IF INPUT-REC(1:1) >= "1" AND INPUT-REC(1:1) <= "3"
                       COMPUTE MSG-MENU-CHOICE =
                           FUNCTION NUMVAL(INPUT-REC(1:1))
                   ELSE
                       MOVE 0 TO MSG-MENU-CHOICE
                   END-IF
               END-IF

               EVALUATE MSG-MENU-CHOICE
                   WHEN 1
                       PERFORM SEND-NEW-MESSAGE
                   WHEN 2
                       PERFORM VIEW-MY-MESSAGES
                   WHEN 3
                       CONTINUE
                   WHEN OTHER
                       MOVE "Invalid choice. Try again." TO LINE-TEXT
                       PERFORM PRINT-LINE
               END-EVALUATE
           END-PERFORM.

      *> ============================================================
      *> WEEK 8: Send a New Message
      *> Validates recipient is a connection, appends to messages.dat
      *> ============================================================
       SEND-NEW-MESSAGE.
           *> Input read from file via READ-NEXT-INPUT
           MOVE "Enter recipient's username (must be a connection): "
               TO LINE-TEXT
           PERFORM PRINT-LINE
           PERFORM READ-NEXT-INPUT
           MOVE FUNCTION TRIM(INPUT-REC) TO MSG-RECIPIENT-INPUT

           *> Validate recipient is an established connection
           SET RECIPIENT-NOT-VALID TO TRUE
           PERFORM VARYING J FROM 1 BY 1
                   UNTIL J > CONNECTION-COUNT
               IF (FUNCTION UPPER-CASE(FUNCTION TRIM(
                       CONN-USER1(J)))
                 = FUNCTION UPPER-CASE(FUNCTION TRIM(
                       STORED-USERNAME(CURRENT-USER-INDEX)))
                 AND FUNCTION UPPER-CASE(FUNCTION TRIM(
                       CONN-USER2(J)))
                 = FUNCTION UPPER-CASE(FUNCTION TRIM(
                       MSG-RECIPIENT-INPUT)))
               OR (FUNCTION UPPER-CASE(FUNCTION TRIM(
                       CONN-USER2(J)))
                 = FUNCTION UPPER-CASE(FUNCTION TRIM(
                       STORED-USERNAME(CURRENT-USER-INDEX)))
                 AND FUNCTION UPPER-CASE(FUNCTION TRIM(
                       CONN-USER1(J)))
                 = FUNCTION UPPER-CASE(FUNCTION TRIM(
                       MSG-RECIPIENT-INPUT)))
                   SET RECIPIENT-VALID TO TRUE
               END-IF
           END-PERFORM

           IF RECIPIENT-NOT-VALID
               MOVE "You can only message users you are connected with."
                   TO LINE-TEXT
               PERFORM PRINT-LINE
               EXIT PARAGRAPH
           END-IF

           *> Message content read from file via READ-NEXT-INPUT
           MOVE "Enter your message (max 200 chars): " TO LINE-TEXT
           PERFORM PRINT-LINE
           PERFORM READ-NEXT-INPUT
           MOVE FUNCTION TRIM(INPUT-REC) TO MSG-CONTENT-INPUT

           *> Generate timestamp YYYY-MM-DD HH:MM:SS
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-TIME
           MOVE WS-CURRENT-DATE-TIME(1:4)  TO WS-YEAR
           MOVE WS-CURRENT-DATE-TIME(5:2)  TO WS-MONTH
           MOVE WS-CURRENT-DATE-TIME(7:2)  TO WS-DAY
           MOVE WS-CURRENT-DATE-TIME(9:2)  TO WS-HOUR
           MOVE WS-CURRENT-DATE-TIME(11:2) TO WS-MINUTE
           MOVE WS-CURRENT-DATE-TIME(13:2) TO WS-SECOND
           MOVE SPACES TO WS-TIMESTAMP
           STRING WS-YEAR "-" WS-MONTH "-" WS-DAY
                  " " WS-HOUR ":" WS-MINUTE ":" WS-SECOND
                  DELIMITED BY SIZE
                  INTO WS-TIMESTAMP
           END-STRING

           *> Append message record to file
           *> Format: sender|recipient|timestamp|content
           OPEN EXTEND MESSAGES-FILE
           MOVE SPACES TO MESSAGES-REC
           STRING
               FUNCTION TRIM(STORED-USERNAME(CURRENT-USER-INDEX))
               "|"
               FUNCTION TRIM(MSG-RECIPIENT-INPUT)
               "|"
               FUNCTION TRIM(WS-TIMESTAMP)
               "|"
               FUNCTION TRIM(MSG-CONTENT-INPUT)
               DELIMITED BY SIZE
               INTO MESSAGES-REC
           END-STRING
           WRITE MESSAGES-REC
           CLOSE MESSAGES-FILE

           *> Confirm to user
           MOVE SPACES TO LINE-TEXT
           STRING "Message sent to "
                  FUNCTION TRIM(MSG-RECIPIENT-INPUT)
                  " successfully!"
                  DELIMITED BY SIZE
                  INTO LINE-TEXT
           END-STRING
           PERFORM PRINT-LINE.
