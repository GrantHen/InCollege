       MANAGE-PENDING-REQUESTS.

           MOVE "---- Pending Connection Requests ----" TO LINE-TEXT
           PERFORM PRINT-LINE

           *> Assume none until we find one
           SET NO-PENDING TO TRUE
           MOVE 1 TO J

           PERFORM UNTIL J > REQUEST-COUNT

               *> Only handle requests where I'M the recipient
               IF FUNCTION UPPER-CASE(FUNCTION TRIM(REQ-RECIPIENT(J))) = FUNCTION UPPER-CASE(FUNCTION TRIM(STORED-USERNAME(CURRENT-USER-INDEX)))

                   SET HAS-PENDING TO TRUE     *> We found at least one request for this user

                   *> Print who sent it
                   MOVE SPACES TO LINE-TEXT
                   STRING "Request from: "
                          FUNCTION TRIM(REQ-SENDER(J))
                          DELIMITED BY SIZE
                          INTO LINE-TEXT
                   END-STRING
                   PERFORM PRINT-LINE

                   *> Simple accept/reject menu
                   MOVE "1. Accept" TO LINE-TEXT
                   PERFORM PRINT-LINE
                   MOVE "2. Reject" TO LINE-TEXT
                   PERFORM PRINT-LINE

                   MOVE SPACES TO LINE-TEXT
                   STRING "Enter your choice for "
                          FUNCTION TRIM(REQ-SENDER(J))
                          ": "
                          DELIMITED BY SIZE
                          INTO LINE-TEXT
                   END-STRING
                   PERFORM PRINT-LINE

                   *> Keep reading until we get 1 or 2
                   MOVE 0 TO SEND-REQ-CHOICE
                   PERFORM UNTIL SEND-REQ-CHOICE = 1
                           OR SEND-REQ-CHOICE = 2

                       PERFORM READ-NEXT-INPUT

                       *> EOF: default to reject so we don't hang
                       IF INPUT-EOF-YES
                           MOVE 2 TO SEND-REQ-CHOICE
                       ELSE
                           IF INPUT-REC(1:1) >= "1"
                              AND INPUT-REC(1:1) <= "2"
                               COMPUTE SEND-REQ-CHOICE =
                                   FUNCTION NUMVAL(INPUT-REC(1:1))
                           ELSE
                               MOVE "Invalid choice. Try again."
                                   TO LINE-TEXT
                               PERFORM PRINT-LINE
                           END-IF
                       END-IF
                   END-PERFORM

                   *> Save the sender username for accept/reject messages
                   MOVE REQ-SENDER(J) TO CONNECTED-USERNAME

                   IF SEND-REQ-CHOICE = 1
                       *> Accept = add a connection pair + remove request
                       PERFORM ACCEPT-CONNECTION-REQUEST
                       PERFORM REMOVE-REQUEST-AT-J
                       PERFORM SAVE-REQUESTS
                       PERFORM SAVE-CONNECTIONS

                       MOVE SPACES TO LINE-TEXT
                       STRING "Connection request from "
                              FUNCTION TRIM(CONNECTED-USERNAME)
                              " ACCEPTED"
                              DELIMITED BY SIZE
                              INTO LINE-TEXT
                       END-STRING
                       PERFORM PRINT-LINE
                   ELSE
                       *> Reject = just remove request
                       PERFORM REMOVE-REQUEST-AT-J
                       PERFORM SAVE-REQUESTS

                       MOVE SPACES TO LINE-TEXT
                       STRING "Connection request from "
                              FUNCTION TRIM(CONNECTED-USERNAME)
                              " REJECTED"
                              DELIMITED BY SIZE
                              INTO LINE-TEXT
                       END-STRING
                       PERFORM PRINT-LINE
                   END-IF

               ELSE
                   *> Not for this user, move on
                   ADD 1 TO J
               END-IF

           END-PERFORM

           *> If we never found anything
           IF NO-PENDING
               MOVE "No pending connection requests." TO LINE-TEXT
               PERFORM PRINT-LINE
           END-IF

           MOVE " " TO LINE-TEXT
           PERFORM PRINT-LINE.


       ACCEPT-CONNECTION-REQUEST.

           *> Find the account index for the sender username
           MOVE CONNECTED-USERNAME TO LOOKUP-USERNAME
           PERFORM FIND-ACCOUNT-BY-USERNAME

           *> If not found, just stop
           IF FOUND-ACCOUNT-INDEX = 0
               EXIT PARAGRAPH
           END-IF

           MOVE FOUND-ACCOUNT-INDEX TO DISPLAY-USER-INDEX

           *> Avoid duplicate connections
           PERFORM CHECK-ALREADY-CONNECTED
           IF ALREADY-CONNECTED
               EXIT PARAGRAPH
           END-IF

           *> Store the new connection pair
           IF CONNECTION-COUNT < MAX-CONNECTIONS
               ADD 1 TO CONNECTION-COUNT
               MOVE STORED-USERNAME(CURRENT-USER-INDEX)
                   TO CONN-USER1(CONNECTION-COUNT)
               MOVE STORED-USERNAME(DISPLAY-USER-INDEX)
                   TO CONN-USER2(CONNECTION-COUNT)
           END-IF.

       REMOVE-REQUEST-AT-J.    *> Remove request J
           IF REQUEST-COUNT <= 0
               EXIT PARAGRAPH
           END-IF

           IF J < 1 OR J > REQUEST-COUNT
               EXIT PARAGRAPH
           END-IF

           PERFORM VARYING I FROM J BY 1
                   UNTIL I >= REQUEST-COUNT
               MOVE REQ-SENDER(I + 1)    TO REQ-SENDER(I)
               MOVE REQ-RECIPIENT(I + 1) TO REQ-RECIPIENT(I)
           END-PERFORM

           *> Clear the last slot
           MOVE SPACES TO REQ-SENDER(REQUEST-COUNT)
           MOVE SPACES TO REQ-RECIPIENT(REQUEST-COUNT)

           SUBTRACT 1 FROM REQUEST-COUNT.


       VIEW-MY-NETWORK.
           MOVE "---- Your Network ----" TO LINE-TEXT
           PERFORM PRINT-LINE

           *> Assume none until we print one
           SET NO-CONNECTIONS TO TRUE

           PERFORM VARYING J FROM 1 BY 1
                   UNTIL J > CONNECTION-COUNT

               MOVE SPACES TO CONNECTED-USERNAME

               *> Usernames are stored in pairs (user1-user2), so check both sides to find the "other user"
               IF FUNCTION UPPER-CASE(FUNCTION TRIM(CONN-USER1(J))) = FUNCTION UPPER-CASE(FUNCTION TRIM(STORED-USERNAME(CURRENT-USER-INDEX)))
                   MOVE CONN-USER2(J) TO CONNECTED-USERNAME
               END-IF

               IF FUNCTION UPPER-CASE(FUNCTION TRIM(CONN-USER2(J))) = FUNCTION UPPER-CASE(FUNCTION TRIM(STORED-USERNAME(CURRENT-USER-INDEX)))
                   MOVE CONN-USER1(J) TO CONNECTED-USERNAME
               END-IF

               *> If we got a real "other user", print it
               IF FUNCTION LENGTH(FUNCTION TRIM(CONNECTED-USERNAME)) > 0
                   SET HAS-CONNECTIONS TO TRUE

                   *> Print details if profile exists
                   MOVE CONNECTED-USERNAME TO LOOKUP-USERNAME
                   PERFORM FIND-ACCOUNT-BY-USERNAME

                   IF FOUND-ACCOUNT-INDEX > 0
                       IF PROFILE-EXISTS(FOUND-ACCOUNT-INDEX) = "Y"

                           MOVE SPACES TO LINE-TEXT
                           STRING "Connected with: "
                                  FUNCTION TRIM(
                                      PROFILE-FIRST-NAME(
                                      FOUND-ACCOUNT-INDEX))
                                  " "
                                  FUNCTION TRIM(
                                      PROFILE-LAST-NAME(
                                      FOUND-ACCOUNT-INDEX))
                                  " (University: "
                                  FUNCTION TRIM(
                                      PROFILE-UNIVERSITY(
                                      FOUND-ACCOUNT-INDEX))
                                  ", Major: "
                                  FUNCTION TRIM(
                                      PROFILE-MAJOR(
                                      FOUND-ACCOUNT-INDEX))
                                  ")"
                                  DELIMITED BY SIZE
                                  INTO LINE-TEXT
                           END-STRING
                           PERFORM PRINT-LINE
                       ELSE
                           *> No profile, just print username
                           MOVE SPACES TO LINE-TEXT
                           STRING "Connected with: "
                                  FUNCTION TRIM(CONNECTED-USERNAME)
                                  DELIMITED BY SIZE
                                  INTO LINE-TEXT
                           END-STRING
                           PERFORM PRINT-LINE
                       END-IF
                   ELSE
                       *> Lookup failed, still show username
                       MOVE SPACES TO LINE-TEXT
                       STRING "Connected with: "
                              FUNCTION TRIM(CONNECTED-USERNAME)
                              DELIMITED BY SIZE
                              INTO LINE-TEXT
                       END-STRING
                       PERFORM PRINT-LINE
                   END-IF

               END-IF

           END-PERFORM

           IF NO-CONNECTIONS
               MOVE "You have no connections." TO LINE-TEXT
               PERFORM PRINT-LINE
           END-IF

           *> Simple pause before returning
           MOVE "1. Return to Main Menu" TO LINE-TEXT
           PERFORM PRINT-LINE
           MOVE "Enter your choice: " TO LINE-TEXT
           PERFORM PRINT-LINE
           PERFORM READ-NEXT-INPUT

           MOVE " " TO LINE-TEXT
           PERFORM PRINT-LINE.
