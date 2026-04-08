      *> ============================================================
      *> WEEK 9: View My Messages - Retrieve, sort, and display inbox
      *> ============================================================
       VIEW-MY-MESSAGES.
           MOVE "--- Your Messages ---" TO LINE-TEXT
           PERFORM PRINT-LINE
           MOVE SPACES TO LINE-TEXT
           PERFORM PRINT-LINE

           MOVE 0 TO MSG-VIEW-COUNT
           MOVE 0 TO MSG-ARRAY-IDX
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

           *> ============================================================
           *> Load matching messages into array
           *> ============================================================
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
                           ADD 1 TO MSG-ARRAY-IDX
                           IF MSG-ARRAY-IDX <= 50
                               MOVE MSG-VIEW-SENDER TO 
                                   MSG-ENTRY-SENDER(MSG-ARRAY-IDX)
                               MOVE MSG-VIEW-RECIPIENT TO 
                                   MSG-ENTRY-RECIPIENT(MSG-ARRAY-IDX)
                               MOVE MSG-VIEW-TIMESTAMP TO 
                                   MSG-ENTRY-TIMESTAMP(MSG-ARRAY-IDX)
                               MOVE MSG-VIEW-CONTENT TO 
                                   MSG-ENTRY-CONTENT(MSG-ARRAY-IDX)
                           END-IF
                       END-IF
               END-READ
           END-PERFORM

           CLOSE MESSAGES-FILE
           MOVE MSG-ARRAY-IDX TO MSG-VIEW-COUNT

           *> ============================================================
           *> Sort messages by timestamp (newest first) using bubble sort
           *> ============================================================
           IF MSG-VIEW-COUNT > 0
               PERFORM VARYING MSG-SORT-IDX FROM 1 BY 1
                   UNTIL MSG-SORT-IDX >= MSG-VIEW-COUNT
                   MOVE "N" TO MSG-SWAP-NEEDED
                   PERFORM VARYING MSG-TEMP-IDX FROM 1 BY 1
                       UNTIL MSG-TEMP-IDX > 
                           MSG-VIEW-COUNT - MSG-SORT-IDX
                       IF MSG-ENTRY-TIMESTAMP(MSG-TEMP-IDX) <
                           MSG-ENTRY-TIMESTAMP(
                               MSG-TEMP-IDX + 1)
                           *> Swap entries
                           MOVE MSG-ENTRY-SENDER(MSG-TEMP-IDX)
                               TO MSG-VIEW-SENDER
                           MOVE MSG-ENTRY-SENDER(MSG-TEMP-IDX + 1)
                               TO MSG-ENTRY-SENDER(MSG-TEMP-IDX)
                           MOVE MSG-VIEW-SENDER TO 
                               MSG-ENTRY-SENDER(MSG-TEMP-IDX + 1)
                           
                           MOVE MSG-ENTRY-RECIPIENT(MSG-TEMP-IDX)
                               TO MSG-VIEW-RECIPIENT
                           MOVE MSG-ENTRY-RECIPIENT(MSG-TEMP-IDX + 1)
                               TO MSG-ENTRY-RECIPIENT(MSG-TEMP-IDX)
                           MOVE MSG-VIEW-RECIPIENT TO 
                               MSG-ENTRY-RECIPIENT(MSG-TEMP-IDX + 1)
                           
                           MOVE MSG-ENTRY-TIMESTAMP(MSG-TEMP-IDX)
                               TO MSG-VIEW-TIMESTAMP
                           MOVE MSG-ENTRY-TIMESTAMP(MSG-TEMP-IDX + 1)
                               TO MSG-ENTRY-TIMESTAMP(MSG-TEMP-IDX)
                           MOVE MSG-VIEW-TIMESTAMP TO 
                               MSG-ENTRY-TIMESTAMP(MSG-TEMP-IDX + 1)
                           
                           MOVE MSG-ENTRY-CONTENT(MSG-TEMP-IDX)
                               TO MSG-VIEW-CONTENT
                           MOVE MSG-ENTRY-CONTENT(MSG-TEMP-IDX + 1)
                               TO MSG-ENTRY-CONTENT(MSG-TEMP-IDX)
                           MOVE MSG-VIEW-CONTENT TO 
                               MSG-ENTRY-CONTENT(MSG-TEMP-IDX + 1)
                           
                           MOVE "Y" TO MSG-SWAP-NEEDED
                       END-IF
                   END-PERFORM
                   IF MSG-SWAP-NEEDED = "N"
                       MOVE MSG-VIEW-COUNT TO MSG-SORT-IDX
                   END-IF
               END-PERFORM
           END-IF

           *> ============================================================
           *> Display sorted messages (newest first)
           *> ============================================================
           IF MSG-FOUND-NONE
               MOVE "You have no messages at this time." TO LINE-TEXT
               PERFORM PRINT-LINE
           ELSE
               PERFORM VARYING MSG-DISPLAY-IDX FROM 1 BY 1
                   UNTIL MSG-DISPLAY-IDX > MSG-VIEW-COUNT
                   MOVE SPACES TO LINE-TEXT
                   STRING "From: "
                       FUNCTION TRIM(MSG-ENTRY-SENDER(MSG-DISPLAY-IDX))
                       DELIMITED BY SIZE
                       INTO LINE-TEXT
                   END-STRING
                   PERFORM PRINT-LINE

                   MOVE SPACES TO LINE-TEXT
                   STRING "Message: "
                       FUNCTION TRIM(MSG-ENTRY-CONTENT(MSG-DISPLAY-IDX))
                       DELIMITED BY SIZE
                       INTO LINE-TEXT
                   END-STRING
                   PERFORM PRINT-LINE

                   MOVE SPACES TO LINE-TEXT
                   STRING "Sent: "
                       FUNCTION TRIM(
                           MSG-ENTRY-TIMESTAMP(MSG-DISPLAY-IDX))
                       DELIMITED BY SIZE
                       INTO LINE-TEXT
                   END-STRING
                   PERFORM PRINT-LINE

                   MOVE "................................"
                       TO LINE-TEXT
                   PERFORM PRINT-LINE
                   MOVE SPACES TO LINE-TEXT
                   PERFORM PRINT-LINE
               END-PERFORM

               MOVE SPACES TO LINE-TEXT
               STRING "Total: " MSG-VIEW-COUNT " message(s)"
                   DELIMITED BY SIZE
                   INTO LINE-TEXT
               END-STRING
               PERFORM PRINT-LINE
           END-IF

           MOVE SPACES TO LINE-TEXT
           PERFORM PRINT-LINE
           MOVE "Press any key to return to Messages Menu..."
               TO LINE-TEXT
           PERFORM PRINT-LINE
           PERFORM READ-NEXT-INPUT.
