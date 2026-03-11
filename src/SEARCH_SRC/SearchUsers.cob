       *> ============================================================
       *> TASK 2: EXACT NAME SEARCH LOGIC
       *> ============================================================
       *> Searches for a user by exact first and last name match
       *> Reads full name from input, splits it, and compares
       *> WEEK 4: After displaying found profile, offer to send
       *>         connection request
       *> ============================================================
       SEARCH-USER-BY-NAME.
           *> Initialize search flag
           SET USER-NOT-FOUND TO TRUE

           *> Prompt for full name
           MOVE "-------------------------------------------------------" TO LINE-TEXT
           PERFORM PRINT-LINE
           MOVE "Enter the full name of the person you are looking for:" TO LINE-TEXT
           PERFORM PRINT-LINE

           *> Read the search name from input
           PERFORM READ-NEXT-INPUT
           MOVE FUNCTION TRIM(INPUT-REC) TO SEARCH-FULL-NAME

           *> Split the full name into first and last (existing helper)
           PERFORM SPLIT-FULL-NAME

           *> Loop through all accounts and compare names
           IF SPACE-POS > 0
               PERFORM VARYING I FROM 1 BY 1 UNTIL I > ACCOUNT-COUNT
                   IF USER-NOT-FOUND
                       PERFORM COMPARE-NAMES
                       IF USER-FOUND
                           *> Found a match - set display index and show profile
                           MOVE I TO DISPLAY-USER-INDEX

                           *> Week 3: match sample output header for found user
                           MOVE "----------------- Found User Profile ------------------" TO LINE-TEXT
                           MOVE "-------------------------------------------------------" TO LINE-TEXT
                           PERFORM DISPLAY-USER-PROFILE
                           MOVE SPACES TO PROFILE-DISPLAY-HEADER
                           MOVE SPACES TO PROFILE-DISPLAY-FOOTER

                           *> Clear header/footer after use
                           MOVE SPACES TO LINE-TEXT
                           MOVE SPACES TO LINE-TEXT

                           *> WEEK 4: Offer to send connection request
                           *> Only if viewing someone else's profile
                           IF DISPLAY-USER-INDEX
                               NOT = CURRENT-USER-INDEX
                               PERFORM OFFER-SEND-REQUEST
                           END-IF

                           EXIT PERFORM
                       END-IF
                   END-IF
               END-PERFORM
           END-IF

           *> If we finished the loop without finding anyone
           IF USER-NOT-FOUND
               MOVE "No one by that name could be found." TO LINE-TEXT
               MOVE "-------------------------------------------------------" TO LINE-TEXT
               PERFORM PRINT-LINE
           END-IF

           *> Add blank line
           MOVE " " TO LINE-TEXT
           PERFORM PRINT-LINE.

      *> ============================================================
      *> WEEK 4: Offer to send connection request after viewing
      *> another user's profile from search
      *> ============================================================
       OFFER-SEND-REQUEST.
           MOVE 0 TO SEND-REQ-CHOICE
           PERFORM UNTIL SEND-REQ-CHOICE = 1
               OR SEND-REQ-CHOICE = 2
               MOVE "1. Send Connection Request" TO LINE-TEXT
               PERFORM PRINT-LINE
               MOVE "2. Return to Main Menu" TO LINE-TEXT
               PERFORM PRINT-LINE
               MOVE "--------------------------" TO LINE-TEXT
               PERFORM PRINT-LINE
               MOVE "Enter your choice: " TO LINE-TEXT
               PERFORM PRINT-LINE

               PERFORM READ-NEXT-INPUT
               IF INPUT-EOF-YES
                   MOVE 2 TO SEND-REQ-CHOICE
               ELSE
                   IF INPUT-REC(1:1) >= "1"
                       AND INPUT-REC(1:1) <= "2"
                       COMPUTE SEND-REQ-CHOICE =
                           FUNCTION NUMVAL(INPUT-REC(1:1))
                          
                       MOVE " " TO LINE-TEXT
                       PERFORM PRINT-LINE
                   ELSE
                       MOVE "Invalid choice. Try again."
                           TO LINE-TEXT
                       PERFORM PRINT-LINE

                       MOVE " " TO LINE-TEXT
                       PERFORM PRINT-LINE

                   END-IF
               END-IF
           END-PERFORM

           IF SEND-REQ-CHOICE = 1
               PERFORM SEND-CONNECTION-REQUEST
           END-IF.

       *> Helper: Split full name into first and last name
       SPLIT-FULL-NAME.
           MOVE SPACES TO SEARCH-FIRST-NAME
           MOVE SPACES TO SEARCH-LAST-NAME
           MOVE 0 TO SPACE-POS

           *> Find the first space in the full name
           PERFORM VARYING NAME-SCAN-IDX FROM 1 BY 1 
                   UNTIL NAME-SCAN-IDX > FUNCTION LENGTH(FUNCTION TRIM(SEARCH-FULL-NAME))
               IF SEARCH-FULL-NAME(NAME-SCAN-IDX:1) = " " AND SPACE-POS = 0
                   MOVE NAME-SCAN-IDX TO SPACE-POS
               END-IF
           END-PERFORM

           *> If space found, split the name
           IF SPACE-POS > 0
               *> Extract first name (everything before space)
               MOVE SEARCH-FULL-NAME(1:SPACE-POS - 1) TO SEARCH-FIRST-NAME
               
               *> Extract last name (everything after space)
               COMPUTE TEXT-LEN = FUNCTION LENGTH(FUNCTION TRIM(SEARCH-FULL-NAME)) - SPACE-POS
               IF TEXT-LEN > 0
                   MOVE SEARCH-FULL-NAME(SPACE-POS + 1:TEXT-LEN) TO SEARCH-LAST-NAME
               END-IF
           END-IF.

       *> Helper: Compare search names with user at index I (exact match)
       COMPARE-NAMES.
           *> Only compare if this user has a profile
           IF PROFILE-EXISTS(I) = "Y"
               *> Exact match: both first and last must match
               IF FUNCTION TRIM(SEARCH-FIRST-NAME)
                  = FUNCTION TRIM(PROFILE-FIRST-NAME(I))
                  AND FUNCTION TRIM(SEARCH-LAST-NAME)
                  = FUNCTION TRIM(PROFILE-LAST-NAME(I))
                   SET USER-FOUND TO TRUE
               END-IF
           END-IF.
