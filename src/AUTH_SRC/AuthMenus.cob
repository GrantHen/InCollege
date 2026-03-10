       START-SCREEN.
           PERFORM UNTIL MENU-CHOICE = 9
               MOVE "Welcome to InCollege!" TO LINE-TEXT
               PERFORM PRINT-LINE

               MOVE "1. Log In" TO LINE-TEXT
               PERFORM PRINT-LINE

               MOVE "2. Create New Account" TO LINE-TEXT
               PERFORM PRINT-LINE

               MOVE "9. Exit" TO LINE-TEXT
               PERFORM PRINT-LINE

               MOVE "Enter your choice: " TO LINE-TEXT
               PERFORM PRINT-LINE

               PERFORM GET-MENU-CHOICE

               EVALUATE MENU-CHOICE
                   WHEN 1
                       PERFORM LOGIN
                   WHEN 2
                       PERFORM CREATE-NEW-ACCOUNT
                   WHEN 9
                       MOVE "--- END_OF_PROGRAM_EXECUTION ---" TO LINE-TEXT
                       PERFORM PRINT-LINE
                   WHEN OTHER
                       MOVE "Invalid choice. Try again." TO LINE-TEXT
                       PERFORM PRINT-LINE
               END-EVALUATE

               MOVE " " TO LINE-TEXT
               PERFORM PRINT-LINE
           END-PERFORM.

       LOGIN.
           *> Unlimited attempts required (we keep looping until correct login)
           SET LOGIN-NO TO TRUE
           MOVE 0 TO CURRENT-USER-INDEX

           PERFORM UNTIL LOGIN-YES
               MOVE "Please enter your username: " TO LINE-TEXT
               PERFORM PRINT-LINE
               PERFORM READ-NEXT-INPUT
               MOVE FUNCTION TRIM(INPUT-REC) TO USERNAME-IN

               MOVE "Please enter your password: " TO LINE-TEXT
               PERFORM PRINT-LINE
               PERFORM READ-NEXT-INPUT
               MOVE FUNCTION TRIM(INPUT-REC) TO PASSWORD-IN

               PERFORM CHECK-LOGIN

               IF LOGIN-YES
                   MOVE "You have successfully logged in." TO LINE-TEXT
                   PERFORM PRINT-LINE
                   MOVE " " TO LINE-TEXT
                   PERFORM PRINT-LINE

                   STRING "Welcome, "
                          FUNCTION TRIM(USERNAME-IN)
                          "!"
                          DELIMITED BY SIZE
                          INTO LINE-TEXT
                   END-STRING
                   PERFORM PRINT-LINE

                   SET IS-LOGGED-IN TO TRUE
                   PERFORM POST-LOGIN-MENU
               ELSE
                   MOVE "Incorrect username/password, please try again" TO LINE-TEXT
                   PERFORM PRINT-LINE
                   MOVE " " TO LINE-TEXT
                   PERFORM PRINT-LINE
               END-IF
           END-PERFORM.

       CHECK-LOGIN.
           *> Search the table for matching username + password
           SET LOGIN-NO TO TRUE

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > ACCOUNT-COUNT
               IF FUNCTION UPPER-CASE(FUNCTION TRIM(USERNAME-IN))
                  = FUNCTION UPPER-CASE(FUNCTION TRIM(STORED-USERNAME(I)))
                  AND FUNCTION TRIM(PASSWORD-IN)
                  = FUNCTION TRIM(STORED-PASSWORD(I))
                   SET LOGIN-YES TO TRUE
                   MOVE I TO CURRENT-USER-INDEX
               END-IF
           END-PERFORM.

       POST-LOGIN-MENU.
           MOVE 0 TO POST-CHOICE
           PERFORM UNTIL POST-CHOICE = 9
               *> menu shows Create/Edit + View first
               MOVE "1. Create/Edit My Profile" TO LINE-TEXT
               PERFORM PRINT-LINE
               MOVE "2. View My Profile" TO LINE-TEXT
               PERFORM PRINT-LINE
               MOVE "3. Job Search / Internship" TO LINE-TEXT
               PERFORM PRINT-LINE
               MOVE "4. Find someone you know" TO LINE-TEXT
               PERFORM PRINT-LINE
               MOVE "5. Learn a New Skill" TO LINE-TEXT
               PERFORM PRINT-LINE
               MOVE "6. View My Pending Connection Requests" TO LINE-TEXT
               PERFORM PRINT-LINE
               MOVE "7. View My Network" TO LINE-TEXT
               PERFORM PRINT-LINE

               *> logout terminates
               MOVE "9. Logout" TO LINE-TEXT
               PERFORM PRINT-LINE

               MOVE "Enter your choice: " TO LINE-TEXT
               PERFORM PRINT-LINE

               PERFORM GET-POST-CHOICE

               EVALUATE POST-CHOICE
                   WHEN 1
                       PERFORM CREATE-EDIT-PROFILE
                   WHEN 2
                       PERFORM VIEW-MY-PROFILE
                   WHEN 3
                       PERFORM JOB-SEARCH-INTERNSHIP-MENU
                   WHEN 4
                       PERFORM SEARCH-USER-BY-NAME
                   WHEN 5
                       PERFORM LEARN-NEW-SKILL
                   WHEN 6
                       PERFORM MANAGE-PENDING-REQUESTS
                   WHEN 7
                       PERFORM VIEW-MY-NETWORK

                   WHEN 9
                       SET NOT-LOGGED-IN TO TRUE
                       EXIT PERFORM

                   WHEN OTHER
                       MOVE "Invalid choice. Try again." TO LINE-TEXT
                       PERFORM PRINT-LINE
                       MOVE " " TO LINE-TEXT
                       PERFORM PRINT-LINE
               END-EVALUATE
           END-PERFORM.
