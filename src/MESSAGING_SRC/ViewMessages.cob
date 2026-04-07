      *> ============================================================
      *> WEEK 9: View My Messages
      *> ============================================================
       VIEW-MY-MESSAGES.
           MOVE "--- Your Messages ---" TO LINE-TEXT
           PERFORM PRINT-LINE

           SET MSG-FOUND-NONE TO TRUE
           SET MSG-VIEW-EOF-NO TO TRUE
           OPEN INPUT MESSAGES-FILE

           IF MSG-FILE-STATUS = "35"
               OPEN OUTPUT MESSAGES-FILE
               CLOSE MESSAGES-FILE
               MOVE "You have no messages at this time." TO LINE-TEXT
               PERFORM PRINT-LINE
               EXIT PARAGRAPH
           END-IF

           PERFORM UNTIL MSG-VIEW-EOF-YES
               READ MESSAGES-FILE
                   AT END
                       SET MSG-VIEW-EOF-YES TO TRUE
                   NOT AT END
                       INSPECT MESSAGES-REC
                           REPLACING ALL LOW-VALUES BY SPACE
                       MOVE SPACES TO MSG-VIEW-SENDER
                       MOVE SPACES TO MSG-VIEW-RECIPIENT
                       MOVE SPACES TO MSG-VIEW-TIMESTAMP
                       MOVE SPACES TO MSG-VIEW-CONTENT
                       UNSTRING MESSAGES-REC DELIMITED BY "|"
                           INTO
                           MSG-VIEW-SENDER
                           MSG-VIEW-RECIPIENT
                           MSG-VIEW-TIMESTAMP
                           MSG-VIEW-CONTENT
                       END-UNSTRING

                       IF FUNCTION UPPER-CASE(FUNCTION TRIM(
                               MSG-VIEW-RECIPIENT))
                         = FUNCTION UPPER-CASE(FUNCTION TRIM(
                               STORED-USERNAME(CURRENT-USER-INDEX)))
                           SET MSG-FOUND-ANY TO TRUE
                           MOVE SPACES TO LINE-TEXT
                           STRING "From: "
                               FUNCTION TRIM(MSG-VIEW-SENDER)
                               DELIMITED BY SIZE
                               INTO LINE-TEXT
                           END-STRING
                           PERFORM PRINT-LINE

                           MOVE SPACES TO LINE-TEXT
                           STRING "Message: "
                               FUNCTION TRIM(MSG-VIEW-CONTENT)
                               DELIMITED BY SIZE
                               INTO LINE-TEXT
                           END-STRING
                           PERFORM PRINT-LINE

                           MOVE SPACES TO LINE-TEXT
                           STRING "Sent: "
                               FUNCTION TRIM(MSG-VIEW-TIMESTAMP)
                               DELIMITED BY SIZE
                               INTO LINE-TEXT
                           END-STRING
                           PERFORM PRINT-LINE

                           MOVE "---" TO LINE-TEXT
                           PERFORM PRINT-LINE
                       END-IF
               END-READ
           END-PERFORM

           CLOSE MESSAGES-FILE

           IF MSG-FOUND-NONE
               MOVE "You have no messages at this time." TO LINE-TEXT
               PERFORM PRINT-LINE
           END-IF.
