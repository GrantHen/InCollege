      *> ============================================================
      *> WEEK 4: Load established connections from file
      *> File format: user1|user2 (one pair per line)
      *> ============================================================
       LOAD-CONNECTIONS.
           SET EOF-NO TO TRUE
           MOVE 0 TO CONNECTION-COUNT
           OPEN INPUT CONNECTIONS-FILE

           *> If file does not exist, create it
           IF CONN-FILE-STATUS = "35"
               OPEN OUTPUT CONNECTIONS-FILE
               CLOSE CONNECTIONS-FILE
           ELSE
               PERFORM UNTIL EOF-YES
                   READ CONNECTIONS-FILE
                       AT END
                           SET EOF-YES TO TRUE
                       NOT AT END
                           INSPECT CONNECTIONS-REC
                               REPLACING ALL LOW-VALUES BY SPACE
                           IF CONNECTION-COUNT < MAX-CONNECTIONS
                               ADD 1 TO CONNECTION-COUNT
                               UNSTRING CONNECTIONS-REC
                                   DELIMITED BY "|"
                                   INTO
                                   CONN-USER1(CONNECTION-COUNT)
                                   CONN-USER2(CONNECTION-COUNT)
                               END-UNSTRING
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE CONNECTIONS-FILE
           END-IF
           SET EOF-NO TO TRUE.

      *> ============================================================
      *> WEEK 4: Save established connections to file
      *> ============================================================
       SAVE-CONNECTIONS.
           OPEN OUTPUT CONNECTIONS-FILE
           PERFORM VARYING J FROM 1 BY 1
                   UNTIL J > CONNECTION-COUNT
               MOVE SPACES TO CONNECTIONS-REC
               STRING
                   FUNCTION TRIM(CONN-USER1(J))
                   "|"
                   FUNCTION TRIM(CONN-USER2(J))
                   DELIMITED BY SIZE
                   INTO CONNECTIONS-REC
               END-STRING
               WRITE CONNECTIONS-REC
           END-PERFORM
           CLOSE CONNECTIONS-FILE.

      *> ============================================================
      *> WEEK 4: Load pending connection requests from file
      *> File format: sender_username|recipient_username
      *> ============================================================
       LOAD-REQUESTS.
           SET EOF-NO TO TRUE
           MOVE 0 TO REQUEST-COUNT
           OPEN INPUT REQUESTS-FILE

           *> If file does not exist, create it
           IF REQ-FILE-STATUS = "35"
               OPEN OUTPUT REQUESTS-FILE
               CLOSE REQUESTS-FILE
           ELSE
               PERFORM UNTIL EOF-YES
                   READ REQUESTS-FILE
                       AT END
                           SET EOF-YES TO TRUE
                       NOT AT END
                           INSPECT REQUESTS-REC
                               REPLACING ALL LOW-VALUES BY SPACE
                           IF REQUEST-COUNT < MAX-REQUESTS
                               ADD 1 TO REQUEST-COUNT
                               UNSTRING REQUESTS-REC
                                   DELIMITED BY "|"
                                   INTO
                                   REQ-SENDER(REQUEST-COUNT)
                                   REQ-RECIPIENT(REQUEST-COUNT)
                               END-UNSTRING
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE REQUESTS-FILE
           END-IF
           SET EOF-NO TO TRUE.

      *> ============================================================
      *> WEEK 4: Save pending connection requests to file
      *> ============================================================
       SAVE-REQUESTS.
           OPEN OUTPUT REQUESTS-FILE
           PERFORM VARYING J FROM 1 BY 1
                   UNTIL J > REQUEST-COUNT
               MOVE SPACES TO REQUESTS-REC
               STRING
                   FUNCTION TRIM(REQ-SENDER(J))
                   "|"
                   FUNCTION TRIM(REQ-RECIPIENT(J))
                   DELIMITED BY SIZE
                   INTO REQUESTS-REC
               END-STRING
               WRITE REQUESTS-REC
           END-PERFORM
           CLOSE REQUESTS-FILE.

      *> ============================================================
      *> WEEK 4: Check if two users are already connected
      *> Sets ALREADY-CONNECTED-FLAG
      *> Uses CURRENT-USER-INDEX and DISPLAY-USER-INDEX
      *> ============================================================
       CHECK-ALREADY-CONNECTED.
           SET NOT-ALREADY-CONNECTED TO TRUE
           PERFORM VARYING J FROM 1 BY 1
                   UNTIL J > CONNECTION-COUNT
               IF (FUNCTION UPPER-CASE(FUNCTION TRIM(
                   CONN-USER1(J)))
                 = FUNCTION UPPER-CASE(FUNCTION TRIM(
                   STORED-USERNAME(CURRENT-USER-INDEX)))
                 AND FUNCTION UPPER-CASE(FUNCTION TRIM(
                   CONN-USER2(J)))
                 = FUNCTION UPPER-CASE(FUNCTION TRIM(
                   STORED-USERNAME(DISPLAY-USER-INDEX))))
               OR (FUNCTION UPPER-CASE(FUNCTION TRIM(
                   CONN-USER1(J)))
                 = FUNCTION UPPER-CASE(FUNCTION TRIM(
                   STORED-USERNAME(DISPLAY-USER-INDEX)))
                 AND FUNCTION UPPER-CASE(FUNCTION TRIM(
                   CONN-USER2(J)))
                 = FUNCTION UPPER-CASE(FUNCTION TRIM(
                   STORED-USERNAME(CURRENT-USER-INDEX))))
                   SET ALREADY-CONNECTED TO TRUE
               END-IF
           END-PERFORM.

      *> ============================================================
      *> WEEK 4: Check if a pending request already exists
      *> from current user TO display user (PENDING-EXISTS-FLAG)
      *> or from display user TO current user (REVERSE-PENDING-FLAG)
      *> ============================================================
       CHECK-PENDING-REQUEST.
           SET PENDING-NOT-EXISTS TO TRUE
           SET REVERSE-NOT-PENDING TO TRUE
           PERFORM VARYING J FROM 1 BY 1
                   UNTIL J > REQUEST-COUNT
               *> Check if current user already sent to display user
               IF FUNCTION UPPER-CASE(FUNCTION TRIM(
                   REQ-SENDER(J)))
                 = FUNCTION UPPER-CASE(FUNCTION TRIM(
                   STORED-USERNAME(CURRENT-USER-INDEX)))
                 AND FUNCTION UPPER-CASE(FUNCTION TRIM(
                   REQ-RECIPIENT(J)))
                 = FUNCTION UPPER-CASE(FUNCTION TRIM(
                   STORED-USERNAME(DISPLAY-USER-INDEX)))
                   SET PENDING-EXISTS TO TRUE
               END-IF
               *> Check if display user already sent to current user
               IF FUNCTION UPPER-CASE(FUNCTION TRIM(
                   REQ-SENDER(J)))
                 = FUNCTION UPPER-CASE(FUNCTION TRIM(
                   STORED-USERNAME(DISPLAY-USER-INDEX)))
                 AND FUNCTION UPPER-CASE(FUNCTION TRIM(
                   REQ-RECIPIENT(J)))
                 = FUNCTION UPPER-CASE(FUNCTION TRIM(
                   STORED-USERNAME(CURRENT-USER-INDEX)))
                   SET REVERSE-PENDING TO TRUE
               END-IF
           END-PERFORM.

      *> ============================================================
      *> WEEK 4: Send Connection Request
      *> Called after viewing another user's profile from search
      *> DISPLAY-USER-INDEX must be set to the target user
      *> ============================================================
       SEND-CONNECTION-REQUEST.
           *> Don't allow sending request to yourself
           IF DISPLAY-USER-INDEX = CURRENT-USER-INDEX
               MOVE "You cannot send a connection request to yourself." TO LINE-TEXT
               PERFORM PRINT-LINE
               EXIT PARAGRAPH
           END-IF

           *> Check if already connected
           PERFORM CHECK-ALREADY-CONNECTED
           IF ALREADY-CONNECTED
               MOVE "You are already connected with this user." TO LINE-TEXT
               PERFORM PRINT-LINE
               EXIT PARAGRAPH
           END-IF

           *> Check for existing pending requests
           PERFORM CHECK-PENDING-REQUEST
           IF PENDING-EXISTS
               MOVE "You have already sent a connection request to this user." TO LINE-TEXT
               PERFORM PRINT-LINE
               EXIT PARAGRAPH
           END-IF

           IF REVERSE-PENDING
               MOVE "This user has already sent you a connection request." TO LINE-TEXT
               PERFORM PRINT-LINE
               EXIT PARAGRAPH
           END-IF

           *> All checks passed - create the pending request
           IF REQUEST-COUNT < MAX-REQUESTS
               ADD 1 TO REQUEST-COUNT
               MOVE STORED-USERNAME(CURRENT-USER-INDEX)
                   TO REQ-SENDER(REQUEST-COUNT)
               MOVE STORED-USERNAME(DISPLAY-USER-INDEX)
                   TO REQ-RECIPIENT(REQUEST-COUNT)

               PERFORM SAVE-REQUESTS

               MOVE SPACES TO LINE-TEXT
               STRING
                   "* Connection request sent to: "
                   FUNCTION TRIM(STORED-USERNAME(
                       DISPLAY-USER-INDEX))
                   DELIMITED BY SIZE
                   INTO LINE-TEXT
               END-STRING
               PERFORM PRINT-LINE
           ELSE
               MOVE "**************************************" TO LINE-TEXT
               PERFORM PRINT-LINE
               MOVE "*  Maximum pending requests reached  *" TO LINE-TEXT
               PERFORM PRINT-LINE
               MOVE "**************************************" TO LINE-TEXT
               PERFORM PRINT-LINE
               MOVE " " TO LINE-TEXT
               PERFORM PRINT-LINE
           END-IF.

       *> VIEW-PENDING-REQUESTS replaced by MANAGE-PENDING-REQUESTS
       *> (incollege_ViewNetwork.cob), which allows accepting/denying
