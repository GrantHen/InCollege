       IDENTIFICATION DIVISION.
       PROGRAM-ID. InCollege.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNTS-FILE ASSIGN TO "data/accounts.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  ACCOUNTS-FILE. *> Define the accounts file
       01  ACCOUNTS-REC               PIC X(80).

       WORKING-STORAGE SECTION.

       01  MAX-ACCOUNTS               PIC 9 VALUE 5. *> Maximum number of accounts
       01  ACCOUNT-COUNT              PIC 9 VALUE 0. *> Current number of accounts

       01  USERNAME-IN                PIC X(20). *> Input username
       01  PASSWORD-IN                PIC X(12). *> Input password
       01  MENU-CHOICE                PIC 9.

       01  EOF-FLAG                   PIC X VALUE "N".
           88  EOF-YES                VALUE "Y".
           88  EOF-NO                 VALUE "N".

       01  I                          PIC 9 VALUE 1. *> Loop index for accounts

       01  ACCOUNT-TABLE.
           05  ACCOUNT-ENTRY OCCURS 5 TIMES. *> Storage for accounts (Array)
               10  STORED-USERNAME    PIC X(20).
               10  STORED-PASSWORD    PIC X(12).

       *> Flags we use for validation checks (simple Y/N instead of complex logic)
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

       01  CH                         PIC X. *> One character at a time
       01  POS                        PIC 99 VALUE 1. *> Loop index for password scan

       PROCEDURE DIVISION.
       MAIN.
           PERFORM LOAD-ACCOUNTS
           PERFORM START-SCREEN
           STOP RUN.

       START-SCREEN.
           PERFORM UNTIL MENU-CHOICE = 9
               DISPLAY "Welcome to InCollege!"
               DISPLAY "1. Log In"
               DISPLAY "2. Create New Account"
               DISPLAY "9. Exit"
               DISPLAY "Enter your choice: "
               ACCEPT MENU-CHOICE

               EVALUATE MENU-CHOICE
                   WHEN 1
                       DISPLAY "Login is under construction (for now)."
                   WHEN 2
                       PERFORM CREATE-NEW-ACCOUNT
                   WHEN 9
                       DISPLAY "--- END_OF_PROGRAM_EXECUTION ---"
                   WHEN OTHER
                       DISPLAY "Invalid choice. Try again."
               END-EVALUATE
               DISPLAY " "
           END-PERFORM.

       CREATE-NEW-ACCOUNT.
           *> If we already have 5 accounts, we must stop creating new ones
           IF ACCOUNT-COUNT >= MAX-ACCOUNTS
               DISPLAY "All permitted accounts have been created, please come back later"
               EXIT PARAGRAPH
           END-IF

           *> Keep asking until we get a good username and good password
           SET USERNAME-NOT-VALID TO TRUE
           SET PASSWORD-NOT-VALID TO TRUE

           PERFORM UNTIL USERNAME-VALID
               DISPLAY "Please enter your username: "
               ACCEPT USERNAME-IN
               PERFORM CHECK-USERNAME-UNIQUE
               IF DUPLICATE-YES
                   DISPLAY "That username is already taken. Please try again."
               ELSE
                   SET USERNAME-VALID TO TRUE
               END-IF
           END-PERFORM

           PERFORM UNTIL PASSWORD-VALID
               DISPLAY "Please enter your password: "
               ACCEPT PASSWORD-IN
               PERFORM VALIDATE-PASSWORD
               IF PASSWORD-NOT-VALID
                   DISPLAY "Password must be 8-12 characters and include:"
                   DISPLAY "1 capital letter, 1 digit, and 1 special character."
                   DISPLAY "Please try again."
               END-IF
           END-PERFORM

           *> At this point we have a unique username and a valid password
           ADD 1 TO ACCOUNT-COUNT
           MOVE USERNAME-IN TO STORED-USERNAME(ACCOUNT-COUNT)
           MOVE PASSWORD-IN TO STORED-PASSWORD(ACCOUNT-COUNT)

           PERFORM SAVE-ACCOUNTS

           DISPLAY "Account created successfully!".

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
           PERFORM VARYING POS FROM 1 BY 1 UNTIL POS > PW-LEN
               MOVE PASSWORD-IN(POS:1) TO CH

               *> Uppercase check: A-Z
               IF CH >= "A" AND CH <= "Z"
                   SET UPPER-YES TO TRUE
               END-IF

               *> Digit check: 0-9
               IF CH >= "0" AND CH <= "9"
                   SET DIGIT-YES TO TRUE
               END-IF

               *> Special character check (simple, beginner-friendly list)
               IF CH = "!" OR CH = "@" OR CH = "#" OR CH = "$" OR CH = "%" OR
                  CH = "^" OR CH = "&" OR CH = "*" OR CH = "(" OR CH = ")" OR
                  CH = "-" OR CH = "_" OR CH = "+" OR CH = "=" OR CH = "?" OR
                  CH = "."
                   SET SPECIAL-YES TO TRUE
               END-IF
           END-PERFORM

           *> Only valid if we found all 3 requirements
           IF UPPER-YES AND DIGIT-YES AND SPECIAL-YES
               SET PASSWORD-VALID TO TRUE
           END-IF.

       LOAD-ACCOUNTS.
           OPEN INPUT ACCOUNTS-FILE
           SET EOF-NO TO TRUE
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
