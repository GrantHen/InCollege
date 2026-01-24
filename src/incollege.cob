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
       01  ACCOUNT-COUNT              PIC 9 VALUE 0.*> Current number of accounts
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
           IF ACCOUNT-COUNT >= MAX-ACCOUNTS
               DISPLAY "All permitted accounts have been created, please come back later"
               EXIT PARAGRAPH
           END-IF

           DISPLAY "Please enter your username: "
           ACCEPT USERNAME-IN

           DISPLAY "Please enter your password: "
           ACCEPT PASSWORD-IN

           * For now: just store it (we'll add validation + uniqueness next)
           ADD 1 TO ACCOUNT-COUNT
           MOVE USERNAME-IN TO STORED-USERNAME(ACCOUNT-COUNT)
           MOVE PASSWORD-IN TO STORED-PASSWORD(ACCOUNT-COUNT)

           PERFORM SAVE-ACCOUNTS

           DISPLAY "Account created! (Validation coming next)".

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
           * Format we will use: username|password
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
                   FUNCTION TRIM(STORED-PASSWORD(I)) *> Trim blank spaces. password
                   DELIMITED BY SIZE
                   INTO ACCOUNTS-REC
               END-STRING
               WRITE ACCOUNTS-REC
           END-PERFORM
           CLOSE ACCOUNTS-FILE.
