       CREATE-NEW-ACCOUNT.
           *> If we already have 5 accounts, we must stop creating new ones
           IF ACCOUNT-COUNT >= MAX-ACCOUNTS
               MOVE "All permitted accounts have been created, please come back later" TO LINE-TEXT
               PERFORM PRINT-LINE
               EXIT PARAGRAPH
           END-IF

           *> Keep asking until we get a good username and good password
           SET USERNAME-NOT-VALID TO TRUE
           SET PASSWORD-NOT-VALID TO TRUE

           *> Get a valid username
           PERFORM UNTIL USERNAME-VALID
               MOVE "---------------------------" TO LINE-TEXT
               PERFORM PRINT-LINE
               MOVE "Please enter your username:" TO LINE-TEXT
               PERFORM PRINT-LINE

               PERFORM READ-NEXT-INPUT
               MOVE FUNCTION TRIM(INPUT-REC) TO USERNAME-IN

               PERFORM CHECK-USERNAME-UNIQUE

               IF DUPLICATE-YES
                   MOVE " " TO LINE-TEXT
                   PERFORM PRINT-LINE

                   MOVE "That username is already taken. Please try again." TO LINE-TEXT
                   PERFORM PRINT-LINE
               ELSE
                   SET USERNAME-VALID TO TRUE
               END-IF
           END-PERFORM

           MOVE " " TO LINE-TEXT
           PERFORM PRINT-LINE

           *> Get a valid password
           PERFORM UNTIL PASSWORD-VALID
               MOVE "Please enter your password:" TO LINE-TEXT
               PERFORM PRINT-LINE

               PERFORM READ-NEXT-INPUT
               MOVE FUNCTION TRIM(INPUT-REC) TO PASSWORD-IN

               PERFORM VALIDATE-PASSWORD

           MOVE "---------------------------" TO LINE-TEXT
           PERFORM PRINT-LINE

               IF PASSWORD-NOT-VALID
                   MOVE "Password must be 8-12 characters and include:" TO LINE-TEXT
                   PERFORM PRINT-LINE
                   MOVE "1 capital letter, 1 digit, and 1 special character." TO LINE-TEXT
                   PERFORM PRINT-LINE
                   MOVE "Please try again." TO LINE-TEXT
                   PERFORM PRINT-LINE
                   MOVE " " TO LINE-TEXT
                   PERFORM PRINT-LINE

               END-IF
           END-PERFORM

           *> At this point we have a unique username and a valid password
           ADD 1 TO ACCOUNT-COUNT
           MOVE USERNAME-IN TO STORED-USERNAME(ACCOUNT-COUNT)
           MOVE PASSWORD-IN TO STORED-PASSWORD(ACCOUNT-COUNT)

           PERFORM SAVE-ACCOUNTS

           MOVE "***********************************" TO LINE-TEXT
           PERFORM PRINT-LINE
           MOVE "*  Account created successfully!  *" TO LINE-TEXT
           PERFORM PRINT-LINE
           MOVE "***********************************" TO LINE-TEXT
           PERFORM PRINT-LINE
           MOVE " " TO LINE-TEXT
           PERFORM PRINT-LINE.

       CHECK-USERNAME-UNIQUE.
           *> This checks if the username matches any stored username
           SET DUPLICATE-NO TO TRUE

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > ACCOUNT-COUNT
               IF FUNCTION UPPER-CASE(FUNCTION TRIM(USERNAME-IN))
                  = FUNCTION UPPER-CASE(FUNCTION TRIM(STORED-USERNAME(I)))
                   SET DUPLICATE-YES TO TRUE
               END-IF
           END-PERFORM.

       VALIDATE-PASSWORD.
           *> Reset flags for each new password attempt
           SET UPPER-NO TO TRUE
           SET DIGIT-NO TO TRUE
           SET SPECIAL-NO TO TRUE
           SET PASSWORD-NOT-VALID TO TRUE

           *> Compute actual length ignoring trailing spaces
           COMPUTE PW-LEN = FUNCTION LENGTH(FUNCTION TRIM(PASSWORD-IN))

           *> First check length rule
           IF PW-LEN < 8 OR PW-LEN > 12
               EXIT PARAGRAPH
           END-IF

           *> Scan each character in the password to find required types
           PERFORM VARYING PW-SCAN FROM 1 BY 1 UNTIL PW-SCAN > PW-LEN
               MOVE PASSWORD-IN(PW-SCAN:1) TO ONE-CHAR

               *> Uppercase check: A-Z
               IF ONE-CHAR >= "A" AND ONE-CHAR <= "Z"
                   SET UPPER-YES TO TRUE
               END-IF

               *> Digit check: 0-9
               IF ONE-CHAR >= "0" AND ONE-CHAR <= "9"
                   SET DIGIT-YES TO TRUE
               END-IF

               *> Special character check (simple, beginner-friendly list)
               IF ONE-CHAR = "!" OR ONE-CHAR = "@" OR ONE-CHAR = "#" OR ONE-CHAR = "$" OR ONE-CHAR = "%" OR
                  ONE-CHAR = "^" OR ONE-CHAR = "&" OR ONE-CHAR = "*" OR ONE-CHAR = "(" OR ONE-CHAR = ")" OR
                  ONE-CHAR = "-" OR ONE-CHAR = "_" OR ONE-CHAR = "+" OR ONE-CHAR = "=" OR ONE-CHAR = "?" OR
                  ONE-CHAR = "."
                   SET SPECIAL-YES TO TRUE
               END-IF
           END-PERFORM

           *> Only valid if we found all 3 requirements
           IF UPPER-YES AND DIGIT-YES AND SPECIAL-YES
               SET PASSWORD-VALID TO TRUE
           END-IF.

       LOAD-ACCOUNTS.
           OPEN INPUT ACCOUNTS-FILE
       
           *> If file does not exist, create it, then reopen for input
           IF ACCOUNT-FILE-STATUS = "35"
               OPEN OUTPUT ACCOUNTS-FILE
               CLOSE ACCOUNTS-FILE
               OPEN INPUT ACCOUNTS-FILE
           END-IF

           *> Read all existing accounts into the table
           PERFORM UNTIL EOF-YES
               READ ACCOUNTS-FILE
                   AT END
                       SET EOF-YES TO TRUE
                   NOT AT END
                       *> Defensive action:convert any LOW-VALUES (NUL bytes) to spaces 
                           *> accounts.dat file was saving NUL bytes after login data was created.
                           *> This prevented new login info from working
                       *> so TRIM/UNSTRING behave correctly even if file was written by root/container.
                       INSPECT ACCOUNTS-REC REPLACING ALL LOW-VALUES BY SPACE
                       PERFORM PARSE-ACCOUNT-LINE
               END-READ
           END-PERFORM
           CLOSE ACCOUNTS-FILE.

       PARSE-ACCOUNT-LINE.
           *> Format use: username|password
           IF ACCOUNT-COUNT < MAX-ACCOUNTS
               ADD 1 TO ACCOUNT-COUNT
               UNSTRING ACCOUNTS-REC DELIMITED BY "|"
                   INTO STORED-USERNAME(ACCOUNT-COUNT)
                        STORED-PASSWORD(ACCOUNT-COUNT)
               END-UNSTRING
           END-IF.

       SAVE-ACCOUNTS.
           OPEN OUTPUT ACCOUNTS-FILE
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > ACCOUNT-COUNT
               MOVE SPACES TO ACCOUNTS-REC
               STRING
                   FUNCTION TRIM(STORED-USERNAME(I)) *> Trim blank spaces Username
                   "|"
                   FUNCTION TRIM(STORED-PASSWORD(I)) *> Trim blank spaces password
                   DELIMITED BY SIZE
                   INTO ACCOUNTS-REC
               END-STRING
               WRITE ACCOUNTS-REC
           END-PERFORM
           CLOSE ACCOUNTS-FILE.
