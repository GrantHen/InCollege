       IDENTIFICATION DIVISION.
       PROGRAM-ID. InCollege.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNTS-FILE ASSIGN TO "data/accounts.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS ACCOUNT-FILE-STATUS.

           *> all program input is read from a file
           SELECT INPUT-FILE ASSIGN TO "test/InCollege-Input.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

           *> exact same output must also be written to a file
           SELECT OUTPUT-FILE ASSIGN TO "out/InCollege-Output.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  ACCOUNTS-FILE. *> Define the accounts file (persistence)
       01  ACCOUNTS-REC               PIC X(80).

       FD  INPUT-FILE. *> Define the input file (all menu/user input comes from here)
       01  INPUT-REC                  PIC X(80).

       FD  OUTPUT-FILE. *> Define the output file (everything displayed is also written here)
       01  OUTPUT-REC                 PIC X(200).

       WORKING-STORAGE SECTION.
       *> File status for accounts file
       01  ACCOUNT-FILE-STATUS        PIC XX.

       01  MAX-ACCOUNTS               PIC 9 VALUE 5. *> Maximum number of accounts
       01  ACCOUNT-COUNT              PIC 9 VALUE 0. *> Current number of accounts

       *> User input storage
       01  USERNAME-IN                PIC X(20). *> Input username
       01  PASSWORD-IN                PIC X(12). *> Input password
       01  MENU-CHOICE                PIC 9 VALUE 0.

       *> EOF flag for accounts file
       01  EOF-FLAG                   PIC X VALUE "N".
           88  EOF-YES                VALUE "Y".
           88  EOF-NO                 VALUE "N".

       01  INPUT-EOF-FLAG             PIC X VALUE "N".
           88  INPUT-EOF-YES          VALUE "Y".
           88  INPUT-EOF-NO           VALUE "N".

       01  I                          PIC 9 VALUE 1. *> Loop index for accounts

       01  ACCOUNT-TABLE.
           05  ACCOUNT-ENTRY OCCURS 5 TIMES. *> Storage for accounts (Array)
               10  STORED-USERNAME    PIC X(20).
               10  STORED-PASSWORD    PIC X(12).

       *> Flags we use for validation checks
       01  USERNAME-OK                PIC X VALUE "N".
           88  USERNAME-VALID         VALUE "Y".
           88  USERNAME-NOT-VALID     VALUE "N".

       01  PASSWORD-OK                PIC X VALUE "N".
           88  PASSWORD-VALID         VALUE "Y".
           88  PASSWORD-NOT-VALID     VALUE "N".

       01  FOUND-DUPLICATE            PIC X VALUE "N".
           88  DUPLICATE-YES          VALUE "Y".
           88  DUPLICATE-NO           VALUE "N".

       01  HAS-UPPER                  PIC X VALUE "N".
           88  UPPER-YES              VALUE "Y".
           88  UPPER-NO               VALUE "N".

       01  HAS-DIGIT                  PIC X VALUE "N".
           88  DIGIT-YES              VALUE "Y".
           88  DIGIT-NO               VALUE "N".

       01  HAS-SPECIAL                PIC X VALUE "N".
           88  SPECIAL-YES            VALUE "Y".
           88  SPECIAL-NO             VALUE "N".

       01  PW-LEN                     PIC 99 VALUE 0. *> Password length (8-12)

       01  ONE-CHAR                   PIC X. *> One character at a time
       01  PW-SCAN                    PIC 99 VALUE 1. *> Loop index for password scan

       *> Logged-in flag
       01  LOGGED-IN                  PIC X VALUE "N".
           88  IS-LOGGED-IN           VALUE "Y".
           88  NOT-LOGGED-IN          VALUE "N".

       01  POST-CHOICE                PIC 9 VALUE 0.
       01  SKILL-CHOICE               PIC 9 VALUE 0.

       *> Login search helpers
       01  LOGIN-FOUND                PIC X VALUE "N".
           88  LOGIN-YES              VALUE "Y".
           88  LOGIN-NO               VALUE "N".

       01  TEMP-NUM                   PIC 9 VALUE 0. *> used for converting menu input
       01  LINE-TEXT                  PIC X(200). *> holds what we print/write

       PROCEDURE DIVISION.
       MAIN.
           *> Open input/output files at the start so all ACCEPTs are replaced with READs
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE

           PERFORM LOAD-ACCOUNTS
           PERFORM START-SCREEN

           *> Close files when program ends
           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE
           STOP RUN.


       *> Helper: Display a line AND write the exact same line to file
       PRINT-LINE.
           *> write the exact same thing we DISPLAY
           MOVE FUNCTION TRIM(LINE-TEXT) TO OUTPUT-REC
           DISPLAY FUNCTION TRIM(LINE-TEXT)
           WRITE OUTPUT-REC.

       *> Helper: Read next input line from input file Also echo the user input into output file
       READ-NEXT-INPUT.
           READ INPUT-FILE
               AT END
                   SET INPUT-EOF-YES TO TRUE
                   MOVE " " TO INPUT-REC
               NOT AT END
                   *> Echo what "user typed" into BOTH console and the output file
                   MOVE FUNCTION TRIM(INPUT-REC) TO LINE-TEXT
                   PERFORM PRINT-LINE
           END-READ.

       *> Helper: Get a 1-digit menu choice (from INPUT-REC)
       GET-MENU-CHOICE.
           PERFORM READ-NEXT-INPUT
           IF INPUT-EOF-YES
               MOVE 9 TO MENU-CHOICE
           ELSE
               IF INPUT-REC(1:1) >= "0" AND INPUT-REC(1:1) <= "9"
                   COMPUTE MENU-CHOICE = FUNCTION NUMVAL(INPUT-REC(1:1))
               ELSE
                   MOVE 0 TO MENU-CHOICE
               END-IF
           END-IF.

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

           PERFORM UNTIL USERNAME-VALID
               MOVE "Please enter your username: " TO LINE-TEXT
               PERFORM PRINT-LINE

               PERFORM READ-NEXT-INPUT
               MOVE FUNCTION TRIM(INPUT-REC) TO USERNAME-IN

               PERFORM CHECK-USERNAME-UNIQUE
               IF DUPLICATE-YES
                   MOVE "That username is already taken. Please try again." TO LINE-TEXT
                   PERFORM PRINT-LINE
               ELSE
                   SET USERNAME-VALID TO TRUE
               END-IF
           END-PERFORM

           PERFORM UNTIL PASSWORD-VALID
               MOVE "Please enter your password: " TO LINE-TEXT
               PERFORM PRINT-LINE

               PERFORM READ-NEXT-INPUT
               MOVE FUNCTION TRIM(INPUT-REC) TO PASSWORD-IN

               PERFORM VALIDATE-PASSWORD
               IF PASSWORD-NOT-VALID
                   MOVE "Password must be 8-12 characters and include:" TO LINE-TEXT
                   PERFORM PRINT-LINE
                   MOVE "1 capital letter, 1 digit, and 1 special character." TO LINE-TEXT
                   PERFORM PRINT-LINE
                   MOVE "Please try again." TO LINE-TEXT
                   PERFORM PRINT-LINE
               END-IF
           END-PERFORM

           *> At this point we have a unique username and a valid password
           ADD 1 TO ACCOUNT-COUNT
           MOVE USERNAME-IN TO STORED-USERNAME(ACCOUNT-COUNT)
           MOVE PASSWORD-IN TO STORED-PASSWORD(ACCOUNT-COUNT)

           PERFORM SAVE-ACCOUNTS

           MOVE "Account created successfully!" TO LINE-TEXT
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

       LOGIN.
           *> Unlimited attempts required (we keep looping until correct login)
           SET LOGIN-NO TO TRUE

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
               END-IF
           END-PERFORM.

       POST-LOGIN-MENU.
           MOVE 0 TO POST-CHOICE
           PERFORM UNTIL POST-CHOICE = 9
               MOVE "1. Search for a job" TO LINE-TEXT
               PERFORM PRINT-LINE
               MOVE "2. Find someone you know" TO LINE-TEXT
               PERFORM PRINT-LINE
               MOVE "3. Learn a new skill" TO LINE-TEXT
               PERFORM PRINT-LINE

               *> Spec note: top level has Logout that TERMINATES program
               MOVE "9. Logout" TO LINE-TEXT
               PERFORM PRINT-LINE

               MOVE "Enter your choice: " TO LINE-TEXT
               PERFORM PRINT-LINE

               PERFORM GET-POST-CHOICE

               EVALUATE POST-CHOICE
                   WHEN 1
                       MOVE "Job search/internship is under construction." TO LINE-TEXT
                       PERFORM PRINT-LINE
                       MOVE " " TO LINE-TEXT
                       PERFORM PRINT-LINE
                   WHEN 2
                       MOVE "Find someone you know is under construction." TO LINE-TEXT
                       PERFORM PRINT-LINE
                       MOVE " " TO LINE-TEXT
                       PERFORM PRINT-LINE
                   WHEN 3
                       PERFORM LEARN-NEW-SKILL
                   WHEN 9
                       MOVE "--- END_OF_PROGRAM_EXECUTION ---" TO LINE-TEXT
                       PERFORM PRINT-LINE
                       MOVE " " TO LINE-TEXT
                       PERFORM PRINT-LINE

                       CLOSE INPUT-FILE
                       CLOSE OUTPUT-FILE
                       STOP RUN

                   WHEN OTHER
                       MOVE "Invalid choice. Try again." TO LINE-TEXT
                       PERFORM PRINT-LINE
                       MOVE " " TO LINE-TEXT
                       PERFORM PRINT-LINE
               END-EVALUATE
           END-PERFORM.

       *> Post menu choice is also just a 1-digit line in input file
       GET-POST-CHOICE.
           PERFORM READ-NEXT-INPUT
           IF INPUT-EOF-YES
               MOVE 9 TO POST-CHOICE
           ELSE
               IF INPUT-REC(1:1) >= "0" AND INPUT-REC(1:1) <= "9"
                   COMPUTE POST-CHOICE = FUNCTION NUMVAL(INPUT-REC(1:1))
               ELSE
                   MOVE 0 TO POST-CHOICE
               END-IF
           END-IF.

       LEARN-NEW-SKILL.
           MOVE 0 TO SKILL-CHOICE
           PERFORM UNTIL SKILL-CHOICE = 6
               MOVE "Learn a New Skill:" TO LINE-TEXT
               PERFORM PRINT-LINE
               MOVE "1. Skill 1" TO LINE-TEXT
               PERFORM PRINT-LINE
               MOVE "2. Skill 2" TO LINE-TEXT
               PERFORM PRINT-LINE
               MOVE "3. Skill 3" TO LINE-TEXT
               PERFORM PRINT-LINE
               MOVE "4. Skill 4" TO LINE-TEXT
               PERFORM PRINT-LINE
               MOVE "5. Skill 5" TO LINE-TEXT
               PERFORM PRINT-LINE

               *> Spec wording: Go Back should return to previous menu
               MOVE "6. Go Back" TO LINE-TEXT
               PERFORM PRINT-LINE

               MOVE "Enter your choice: " TO LINE-TEXT
               PERFORM PRINT-LINE

               PERFORM GET-SKILL-CHOICE

               EVALUATE SKILL-CHOICE
                   WHEN 1 THRU 5
                       MOVE "This skill is under construction." TO LINE-TEXT
                       PERFORM PRINT-LINE
                       MOVE " " TO LINE-TEXT
                       PERFORM PRINT-LINE
                   WHEN 6
                       CONTINUE
                   WHEN OTHER
                       MOVE "Invalid choice. Try again." TO LINE-TEXT
                       PERFORM PRINT-LINE
                       MOVE " " TO LINE-TEXT
                       PERFORM PRINT-LINE
               END-EVALUATE
           END-PERFORM.

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
