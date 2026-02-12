       IDENTIFICATION DIVISION.
       PROGRAM-ID. InCollege.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCOUNTS-FILE ASSIGN TO "data/accounts.dat" *> changed pathing for consistent location
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS ACCOUNT-FILE-STATUS.

           SELECT PROFILES-FILE ASSIGN TO "data/profiles.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS PROFILE-FILE-STATUS.

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

       FD  PROFILES-FILE. *> Stores profile data per user
       01  PROFILES-REC               PIC X(1500).

       FD  INPUT-FILE. *> Define the input file (all menu/user input comes from here)
       01  INPUT-REC                  PIC X(200).

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
       01  POST-SAVE-CHOICE           PIC 9 VALUE 1.
       01  SKILL-CHOICE               PIC 9 VALUE 0.
       01  CURRENT-USER-INDEX         PIC 9 VALUE 0.

       *> Login search helpers
       01  LOGIN-FOUND                PIC X VALUE "N".
           88  LOGIN-YES              VALUE "Y".
           88  LOGIN-NO               VALUE "N".

       01  TEMP-NUM                   PIC 9 VALUE 0. *> used for converting menu input
       01  LINE-TEXT                  PIC X(200). *> holds what we print/write

       01  PROFILE-FILE-STATUS        PIC XX VALUE "00".
       01  PROFILE-POINTER            PIC 9(4) VALUE 1.
       01  LOOKUP-USERNAME            PIC X(20).
       01  FOUND-ACCOUNT-INDEX        PIC 9 VALUE 0.

       01  TEXT-LEN                   PIC 99 VALUE 0.
       01  TEMP-CHAR                  PIC X.
       01  TRIMMED-INPUT              PIC X(200).

       01  GRAD-YEAR-NUM              PIC 9(4) VALUE 0.
       01  GRAD-YEAR-OK               PIC X VALUE "N".
           88  GRAD-VALID             VALUE "Y".
           88  GRAD-NOT-VALID         VALUE "N".

       01  EXP-IDX                    PIC 9 VALUE 1.
       01  EDU-IDX                    PIC 9 VALUE 1.

       *> Controls whether to echo user input into output transcript
       01  ECHO-USER-INPUT            PIC X VALUE "Y".
           88  ECHO-ON                VALUE "Y".
           88  ECHO-OFF               VALUE "N".

       *> TASK 2: Search-related variables
       01  SEARCH-FULL-NAME           PIC X(100).
       01  SEARCH-FIRST-NAME          PIC X(30).
       01  SEARCH-LAST-NAME           PIC X(30).
       01  USER-FOUND-FLAG            PIC X VALUE "N".
           88  USER-FOUND             VALUE "Y".
           88  USER-NOT-FOUND         VALUE "N".
       01  DISPLAY-USER-INDEX         PIC 9 VALUE 0.
       01  SPACE-POS                  PIC 99 VALUE 0.
       01  NAME-SCAN-IDX              PIC 99 VALUE 1.

       01  PROFILE-TABLE.
           05  PROFILE-ENTRY OCCURS 5 TIMES.
               10  PROFILE-USERNAME      PIC X(20).
               10  PROFILE-FIRST-NAME    PIC X(30).
               10  PROFILE-LAST-NAME     PIC X(30).
               10  PROFILE-UNIVERSITY    PIC X(50).
               10  PROFILE-MAJOR         PIC X(50).
               10  PROFILE-GRAD-YEAR     PIC X(4).
               10  PROFILE-ABOUT         PIC X(200).
               10  PROFILE-EXPERIENCES.
                   15  PROFILE-EXP OCCURS 3 TIMES.
                       20  PROFILE-EXP-TITLE   PIC X(40).
                       20  PROFILE-EXP-COMPANY PIC X(60).
                       20  PROFILE-EXP-DATES   PIC X(40).
                       20  PROFILE-EXP-DESC    PIC X(150).
               10  PROFILE-EDUCATIONS.
                   15  PROFILE-EDU OCCURS 3 TIMES.
                       20  PROFILE-EDU-DEGREE  PIC X(60).
                       20  PROFILE-EDU-SCHOOL  PIC X(60).
                       20  PROFILE-EDU-YEARS   PIC X(30).

       01  PROFILE-EXISTS OCCURS 5 TIMES PIC X VALUE "N".

    *> Week 3: Allows profile display to vary header/footer by context
    01  PROFILE-DISPLAY-HEADER      PIC X(40) VALUE SPACES.
    01  PROFILE-DISPLAY-FOOTER      PIC X(40) VALUE SPACES.
    01  TEMP-FULL-NAME              PIC X(100) VALUE SPACES.

       PROCEDURE DIVISION.
       MAIN.
           *> Open input/output files at the start so all ACCEPTs are replaced with READs
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE

           PERFORM LOAD-ACCOUNTS
           PERFORM LOAD-PROFILES
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
                   IF ECHO-ON
                       *> Echo what "user typed" into BOTH console and the output file (optional)
                       MOVE FUNCTION TRIM(INPUT-REC) TO LINE-TEXT
                       PERFORM PRINT-LINE
                   END-IF
           END-READ.

       *> Helper: Get a 1-digit menu choice (from INPUT-REC)
       GET-MENU-CHOICE.
           PERFORM READ-NEXT-INPUT
           IF INPUT-EOF-YES
               MOVE 0 TO MENU-CHOICE
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

       LOAD-PROFILES.
           SET EOF-NO TO TRUE
           OPEN INPUT PROFILES-FILE

           *> If the file is missing, create an empty one so saves will work later
           IF PROFILE-FILE-STATUS = "35"
               OPEN OUTPUT PROFILES-FILE
               CLOSE PROFILES-FILE
           ELSE
               PERFORM UNTIL EOF-YES
                   READ PROFILES-FILE
                       AT END
                           SET EOF-YES TO TRUE
                       NOT AT END
                           PERFORM PARSE-PROFILE-LINE
                   END-READ
               END-PERFORM
           END-IF

           CLOSE PROFILES-FILE
           SET EOF-NO TO TRUE.

       PARSE-PROFILE-LINE.
           MOVE 1 TO PROFILE-POINTER
           MOVE SPACES TO LOOKUP-USERNAME

           UNSTRING PROFILES-REC DELIMITED BY "|"
               INTO LOOKUP-USERNAME
               WITH POINTER PROFILE-POINTER
           END-UNSTRING

           PERFORM FIND-ACCOUNT-BY-USERNAME

           IF FOUND-ACCOUNT-INDEX > 0
               UNSTRING PROFILES-REC(PROFILE-POINTER:)
                   DELIMITED BY "|"
                   INTO PROFILE-FIRST-NAME(FOUND-ACCOUNT-INDEX)
                        PROFILE-LAST-NAME(FOUND-ACCOUNT-INDEX)
                        PROFILE-UNIVERSITY(FOUND-ACCOUNT-INDEX)
                        PROFILE-MAJOR(FOUND-ACCOUNT-INDEX)
                        PROFILE-GRAD-YEAR(FOUND-ACCOUNT-INDEX)
                        PROFILE-ABOUT(FOUND-ACCOUNT-INDEX)
                        PROFILE-EXP-TITLE(FOUND-ACCOUNT-INDEX, 1)
                        PROFILE-EXP-COMPANY(FOUND-ACCOUNT-INDEX, 1)
                        PROFILE-EXP-DATES(FOUND-ACCOUNT-INDEX, 1)
                        PROFILE-EXP-DESC(FOUND-ACCOUNT-INDEX, 1)
                        PROFILE-EXP-TITLE(FOUND-ACCOUNT-INDEX, 2)
                        PROFILE-EXP-COMPANY(FOUND-ACCOUNT-INDEX, 2)
                        PROFILE-EXP-DATES(FOUND-ACCOUNT-INDEX, 2)
                        PROFILE-EXP-DESC(FOUND-ACCOUNT-INDEX, 2)
                        PROFILE-EXP-TITLE(FOUND-ACCOUNT-INDEX, 3)
                        PROFILE-EXP-COMPANY(FOUND-ACCOUNT-INDEX, 3)
                        PROFILE-EXP-DATES(FOUND-ACCOUNT-INDEX, 3)
                        PROFILE-EXP-DESC(FOUND-ACCOUNT-INDEX, 3)
                        PROFILE-EDU-DEGREE(FOUND-ACCOUNT-INDEX, 1)
                        PROFILE-EDU-SCHOOL(FOUND-ACCOUNT-INDEX, 1)
                        PROFILE-EDU-YEARS(FOUND-ACCOUNT-INDEX, 1)
                        PROFILE-EDU-DEGREE(FOUND-ACCOUNT-INDEX, 2)
                        PROFILE-EDU-SCHOOL(FOUND-ACCOUNT-INDEX, 2)
                        PROFILE-EDU-YEARS(FOUND-ACCOUNT-INDEX, 2)
                        PROFILE-EDU-DEGREE(FOUND-ACCOUNT-INDEX, 3)
                        PROFILE-EDU-SCHOOL(FOUND-ACCOUNT-INDEX, 3)
                        PROFILE-EDU-YEARS(FOUND-ACCOUNT-INDEX, 3)
               END-UNSTRING

               MOVE LOOKUP-USERNAME TO PROFILE-USERNAME(FOUND-ACCOUNT-INDEX)
               MOVE "Y" TO PROFILE-EXISTS(FOUND-ACCOUNT-INDEX)
           END-IF.

       FIND-ACCOUNT-BY-USERNAME.
           MOVE 0 TO FOUND-ACCOUNT-INDEX
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > ACCOUNT-COUNT
               IF FUNCTION UPPER-CASE(FUNCTION TRIM(LOOKUP-USERNAME))
                  = FUNCTION UPPER-CASE(FUNCTION TRIM(STORED-USERNAME(I)))
                   MOVE I TO FOUND-ACCOUNT-INDEX
               END-IF
           END-PERFORM.

       SAVE-PROFILES.
           OPEN OUTPUT PROFILES-FILE
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > ACCOUNT-COUNT
               IF PROFILE-EXISTS(I) = "Y"
                   MOVE SPACES TO PROFILES-REC
                   STRING
                       FUNCTION TRIM(STORED-USERNAME(I)) "|"
                       FUNCTION TRIM(PROFILE-FIRST-NAME(I)) "|"
                       FUNCTION TRIM(PROFILE-LAST-NAME(I)) "|"
                       FUNCTION TRIM(PROFILE-UNIVERSITY(I)) "|"
                       FUNCTION TRIM(PROFILE-MAJOR(I)) "|"
                       FUNCTION TRIM(PROFILE-GRAD-YEAR(I)) "|"
                       FUNCTION TRIM(PROFILE-ABOUT(I)) "|"
                       FUNCTION TRIM(PROFILE-EXP-TITLE(I, 1)) "|"
                       FUNCTION TRIM(PROFILE-EXP-COMPANY(I, 1)) "|"
                       FUNCTION TRIM(PROFILE-EXP-DATES(I, 1)) "|"
                       FUNCTION TRIM(PROFILE-EXP-DESC(I, 1)) "|"
                       FUNCTION TRIM(PROFILE-EXP-TITLE(I, 2)) "|"
                       FUNCTION TRIM(PROFILE-EXP-COMPANY(I, 2)) "|"
                       FUNCTION TRIM(PROFILE-EXP-DATES(I, 2)) "|"
                       FUNCTION TRIM(PROFILE-EXP-DESC(I, 2)) "|"
                       FUNCTION TRIM(PROFILE-EXP-TITLE(I, 3)) "|"
                       FUNCTION TRIM(PROFILE-EXP-COMPANY(I, 3)) "|"
                       FUNCTION TRIM(PROFILE-EXP-DATES(I, 3)) "|"
                       FUNCTION TRIM(PROFILE-EXP-DESC(I, 3)) "|"
                       FUNCTION TRIM(PROFILE-EDU-DEGREE(I, 1)) "|"
                       FUNCTION TRIM(PROFILE-EDU-SCHOOL(I, 1)) "|"
                       FUNCTION TRIM(PROFILE-EDU-YEARS(I, 1)) "|"
                       FUNCTION TRIM(PROFILE-EDU-DEGREE(I, 2)) "|"
                       FUNCTION TRIM(PROFILE-EDU-SCHOOL(I, 2)) "|"
                       FUNCTION TRIM(PROFILE-EDU-YEARS(I, 2)) "|"
                       FUNCTION TRIM(PROFILE-EDU-DEGREE(I, 3)) "|"
                       FUNCTION TRIM(PROFILE-EDU-SCHOOL(I, 3)) "|"
                       FUNCTION TRIM(PROFILE-EDU-YEARS(I, 3))
                       DELIMITED BY SIZE
                       INTO PROFILES-REC
                   END-STRING
                   WRITE PROFILES-REC
               END-IF
           END-PERFORM
           CLOSE PROFILES-FILE.

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
               MOVE "3. Search for a job" TO LINE-TEXT
               PERFORM PRINT-LINE
               MOVE "4. Find someone you know" TO LINE-TEXT
               PERFORM PRINT-LINE
               MOVE "5. Learn a New Skill" TO LINE-TEXT
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
                       MOVE "Job search/internship is under construction." TO LINE-TEXT
                       PERFORM PRINT-LINE
                       MOVE " " TO LINE-TEXT
                       PERFORM PRINT-LINE
                   WHEN 4
                       PERFORM SEARCH-USER-BY-NAME
                   WHEN 5
                       PERFORM LEARN-NEW-SKILL
                   WHEN 9
                       MOVE " " TO LINE-TEXT
                       PERFORM PRINT-LINE
                       PERFORM START-SCREEN

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
               MOVE 0 TO POST-CHOICE
           ELSE
               IF INPUT-REC(1:1) >= "0" AND INPUT-REC(1:1) <= "9"
                   COMPUTE POST-CHOICE = FUNCTION NUMVAL(INPUT-REC(1:1))
               ELSE
                   MOVE 0 TO POST-CHOICE
               END-IF
           END-IF.

       *> WEEK 2: Create/Edit My Profile
       CREATE-EDIT-PROFILE.
           PERFORM UNTIL 1 = 0
               *> Header matches sample output
               MOVE "--- Create/Edit Profile ---" TO LINE-TEXT
               PERFORM PRINT-LINE

               *> Mark that this user has a profile now (so SAVE-PROFILES will write it)
               MOVE STORED-USERNAME(CURRENT-USER-INDEX) TO PROFILE-USERNAME(CURRENT-USER-INDEX)
               MOVE "Y" TO PROFILE-EXISTS(CURRENT-USER-INDEX)

               *> Required fields (allow blank to keep existing values when editing)
               PERFORM GET-FIRST-NAME
               PERFORM GET-LAST-NAME
               PERFORM GET-UNIVERSITY
               PERFORM GET-MAJOR

               PERFORM GET-GRAD-YEAR

               *> Optional About Me (blank keeps current when editing)
               IF FUNCTION LENGTH(FUNCTION TRIM(PROFILE-ABOUT(CURRENT-USER-INDEX))) > 0
                   MOVE SPACES TO LINE-TEXT
                   STRING "Enter About Me (blank keeps current): "
                          DELIMITED BY SIZE
                          INTO LINE-TEXT
                   END-STRING
               ELSE
                   MOVE "Enter About Me (optional, max 200 chars, enter blank line to skip): " TO LINE-TEXT
               END-IF
               PERFORM PRINT-LINE
               PERFORM READ-NEXT-INPUT
               MOVE FUNCTION TRIM(INPUT-REC) TO TRIMMED-INPUT
               IF FUNCTION LENGTH(TRIMMED-INPUT) > 0
                   MOVE TRIMMED-INPUT TO PROFILE-ABOUT(CURRENT-USER-INDEX)
               END-IF

               *> Experience loop (optional, up to 3)
               PERFORM GET-EXPERIENCE

               *> Education loop (optional, up to 3)
               PERFORM GET-EDUCATION

               *> Save to profiles.dat
               PERFORM SAVE-PROFILES

               MOVE "Profile saved successfully!" TO LINE-TEXT
               PERFORM PRINT-LINE

               *> Present explicit option to return to main menu after saving
               MOVE 0 TO POST-SAVE-CHOICE
               PERFORM UNTIL POST-SAVE-CHOICE = 1 OR POST-SAVE-CHOICE = 2
                   MOVE "1. Return to Main Menu" TO LINE-TEXT
                   PERFORM PRINT-LINE
                   MOVE "2. Edit Profile Again" TO LINE-TEXT
                   PERFORM PRINT-LINE
                   MOVE "Enter your choice: " TO LINE-TEXT
                   PERFORM PRINT-LINE

                   PERFORM READ-NEXT-INPUT
                   IF INPUT-EOF-YES
                       MOVE 1 TO POST-SAVE-CHOICE
                   ELSE
                       IF INPUT-REC(1:1) >= "1" AND INPUT-REC(1:1) <= "2"
                           COMPUTE POST-SAVE-CHOICE = FUNCTION NUMVAL(INPUT-REC(1:1))
                       ELSE
                           MOVE "Invalid choice. Try again." TO LINE-TEXT
                           PERFORM PRINT-LINE
                       END-IF
                   END-IF
               END-PERFORM

               IF POST-SAVE-CHOICE = 2
                   MOVE " " TO LINE-TEXT
                   PERFORM PRINT-LINE
               ELSE
                   EXIT PERFORM
               END-IF
           END-PERFORM

           *> Return to top level post-login menu
           MOVE " " TO LINE-TEXT
           PERFORM PRINT-LINE.


       *> ============================================================
       *> TASK 1: FULL PROFILE DISPLAY ROUTINE (CORE LOGIC)
       *> ============================================================
       *> This routine displays ALL profile fields for a given user index
       *> It assumes DISPLAY-USER-INDEX is set before calling
       *> Uses PRINT-LINE for all output (writes to both screen and file)
       *> ============================================================
       DISPLAY-USER-PROFILE.
           *> Check if the user has a profile
           IF PROFILE-EXISTS(DISPLAY-USER-INDEX) = "N"
               MOVE "This user has not created a profile yet." TO LINE-TEXT
               PERFORM PRINT-LINE
               EXIT PARAGRAPH
           END-IF

           *> Display profile header
           IF FUNCTION LENGTH(FUNCTION TRIM(PROFILE-DISPLAY-HEADER)) > 0
               MOVE FUNCTION TRIM(PROFILE-DISPLAY-HEADER) TO LINE-TEXT
           ELSE
               MOVE "--- User Profile ---" TO LINE-TEXT
           END-IF
           PERFORM PRINT-LINE

           *> Display name
           MOVE SPACES TO LINE-TEXT
           STRING "Name: "
                  FUNCTION TRIM(PROFILE-FIRST-NAME(DISPLAY-USER-INDEX))
                  " "
                  FUNCTION TRIM(PROFILE-LAST-NAME(DISPLAY-USER-INDEX))
                  DELIMITED BY SIZE
                  INTO LINE-TEXT
           END-STRING
           PERFORM PRINT-LINE

           *> Display university
           MOVE SPACES TO LINE-TEXT
           STRING "University: "
                  FUNCTION TRIM(PROFILE-UNIVERSITY(DISPLAY-USER-INDEX))
                  DELIMITED BY SIZE
                  INTO LINE-TEXT
           END-STRING
           PERFORM PRINT-LINE

           *> Display major
           MOVE SPACES TO LINE-TEXT
           STRING "Major: "
                  FUNCTION TRIM(PROFILE-MAJOR(DISPLAY-USER-INDEX))
                  DELIMITED BY SIZE
                  INTO LINE-TEXT
           END-STRING
           PERFORM PRINT-LINE

           *> Display graduation year
           MOVE SPACES TO LINE-TEXT
           STRING "Graduation Year: "
                  FUNCTION TRIM(PROFILE-GRAD-YEAR(DISPLAY-USER-INDEX))
                  DELIMITED BY SIZE
                  INTO LINE-TEXT
           END-STRING
           PERFORM PRINT-LINE

           *> Display About Me (always show field, even if empty)
           MOVE SPACES TO LINE-TEXT
           IF FUNCTION LENGTH(FUNCTION TRIM(PROFILE-ABOUT(DISPLAY-USER-INDEX))) > 0
               STRING "About Me: "
                      FUNCTION TRIM(PROFILE-ABOUT(DISPLAY-USER-INDEX))
                      DELIMITED BY SIZE
                      INTO LINE-TEXT
               END-STRING
           ELSE
               MOVE "About Me: None" TO LINE-TEXT
           END-IF
           PERFORM PRINT-LINE

           *> Display experiences (show "None" if no experiences)
           MOVE 0 TO EXP-IDX
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
               IF FUNCTION LENGTH(FUNCTION TRIM(PROFILE-EXP-TITLE(DISPLAY-USER-INDEX, I))) > 0
                   ADD 1 TO EXP-IDX
               END-IF
           END-PERFORM

           IF EXP-IDX = 0
               MOVE "Experience: None" TO LINE-TEXT
               PERFORM PRINT-LINE
           ELSE
               MOVE "Experience:" TO LINE-TEXT
               PERFORM PRINT-LINE

               PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
                   IF FUNCTION LENGTH(FUNCTION TRIM(PROFILE-EXP-TITLE(DISPLAY-USER-INDEX, I))) > 0
                       MOVE SPACES TO LINE-TEXT
                       STRING "  Title: "
                              FUNCTION TRIM(PROFILE-EXP-TITLE(DISPLAY-USER-INDEX, I))
                              DELIMITED BY SIZE
                              INTO LINE-TEXT
                       END-STRING
                       PERFORM PRINT-LINE

                       MOVE SPACES TO LINE-TEXT
                       STRING "  Company: "
                              FUNCTION TRIM(PROFILE-EXP-COMPANY(DISPLAY-USER-INDEX, I))
                              DELIMITED BY SIZE
                              INTO LINE-TEXT
                       END-STRING
                       PERFORM PRINT-LINE

                       MOVE SPACES TO LINE-TEXT
                       STRING "  Dates: "
                              FUNCTION TRIM(PROFILE-EXP-DATES(DISPLAY-USER-INDEX, I))
                              DELIMITED BY SIZE
                              INTO LINE-TEXT
                       END-STRING
                       PERFORM PRINT-LINE

                       IF FUNCTION LENGTH(FUNCTION TRIM(PROFILE-EXP-DESC(DISPLAY-USER-INDEX, I))) > 0
                           MOVE SPACES TO LINE-TEXT
                           STRING "  Description: "
                                  FUNCTION TRIM(PROFILE-EXP-DESC(DISPLAY-USER-INDEX, I))
                                  DELIMITED BY SIZE
                                  INTO LINE-TEXT
                           END-STRING
                           PERFORM PRINT-LINE
                       END-IF
                   END-IF
               END-PERFORM
           END-IF

           *> Display education (show "None" if no education)
           MOVE 0 TO EDU-IDX
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
               IF FUNCTION LENGTH(FUNCTION TRIM(PROFILE-EDU-DEGREE(DISPLAY-USER-INDEX, I))) > 0
                   ADD 1 TO EDU-IDX
               END-IF
           END-PERFORM

           IF EDU-IDX = 0
               MOVE "Education: None" TO LINE-TEXT
               PERFORM PRINT-LINE
           ELSE
               MOVE "Education:" TO LINE-TEXT
               PERFORM PRINT-LINE

               PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
                   IF FUNCTION LENGTH(FUNCTION TRIM(PROFILE-EDU-DEGREE(DISPLAY-USER-INDEX, I))) > 0
                       MOVE SPACES TO LINE-TEXT
                       STRING "  Degree: "
                              FUNCTION TRIM(PROFILE-EDU-DEGREE(DISPLAY-USER-INDEX, I))
                              DELIMITED BY SIZE
                              INTO LINE-TEXT
                       END-STRING
                       PERFORM PRINT-LINE

                       MOVE SPACES TO LINE-TEXT
                       STRING "  University: "
                              FUNCTION TRIM(PROFILE-EDU-SCHOOL(DISPLAY-USER-INDEX, I))
                              DELIMITED BY SIZE
                              INTO LINE-TEXT
                       END-STRING
                       PERFORM PRINT-LINE

                       MOVE SPACES TO LINE-TEXT
                       STRING "  Years: "
                              FUNCTION TRIM(PROFILE-EDU-YEARS(DISPLAY-USER-INDEX, I))
                              DELIMITED BY SIZE
                              INTO LINE-TEXT
                       END-STRING
                       PERFORM PRINT-LINE
                   END-IF
               END-PERFORM
           END-IF

           *> Display footer
           IF FUNCTION LENGTH(FUNCTION TRIM(PROFILE-DISPLAY-FOOTER)) > 0
               MOVE FUNCTION TRIM(PROFILE-DISPLAY-FOOTER) TO LINE-TEXT
           ELSE
               MOVE "--------------------" TO LINE-TEXT
           END-IF
           PERFORM PRINT-LINE.

       *> Week 2: View My Profile (now uses the shared DISPLAY-USER-PROFILE)
       VIEW-MY-PROFILE.
           *> Set the display index to current user
           MOVE CURRENT-USER-INDEX TO DISPLAY-USER-INDEX

           *> Week 3: match sample output header for self profile
           MOVE "--- Your Profile ---" TO PROFILE-DISPLAY-HEADER
           MOVE "--------------------" TO PROFILE-DISPLAY-FOOTER
           
           *> Call the shared display routine
           PERFORM DISPLAY-USER-PROFILE

           *> Clear header/footer so other screens keep defaults
           MOVE SPACES TO PROFILE-DISPLAY-HEADER
           MOVE SPACES TO PROFILE-DISPLAY-FOOTER
           
           *> Add blank line after display
           MOVE " " TO LINE-TEXT
           PERFORM PRINT-LINE.

       *> ============================================================
       *> TASK 2: EXACT NAME SEARCH LOGIC
       *> ============================================================
       *> Searches for a user by exact first and last name match
       *> Reads full name from input, splits it, and compares
       *> ============================================================
       SEARCH-USER-BY-NAME.
           *> Initialize search flag
           SET USER-NOT-FOUND TO TRUE

           *> Prompt for full name
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
                           MOVE "--- Found User Profile ---" TO PROFILE-DISPLAY-HEADER
                           MOVE "-------------------------" TO PROFILE-DISPLAY-FOOTER
                           PERFORM DISPLAY-USER-PROFILE

                           *> Clear header/footer after use
                           MOVE SPACES TO PROFILE-DISPLAY-HEADER
                           MOVE SPACES TO PROFILE-DISPLAY-FOOTER
                           EXIT PERFORM
                       END-IF
                   END-IF
               END-PERFORM
           END-IF

           *> If we finished the loop without finding anyone
           IF USER-NOT-FOUND
               MOVE "No one by that name could be found." TO LINE-TEXT
               PERFORM PRINT-LINE
           END-IF

           *> Add blank line
           MOVE " " TO LINE-TEXT
           PERFORM PRINT-LINE.

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

       GET-FIRST-NAME.
           PERFORM UNTIL 1 = 0
               IF FUNCTION LENGTH(FUNCTION TRIM(PROFILE-FIRST-NAME(CURRENT-USER-INDEX))) > 0
                   MOVE SPACES TO LINE-TEXT
                   STRING "Enter First Name (blank keeps '"
                          FUNCTION TRIM(PROFILE-FIRST-NAME(CURRENT-USER-INDEX))
                          "'): "
                          DELIMITED BY SIZE
                          INTO LINE-TEXT
                   END-STRING
               ELSE
                   MOVE "Enter First Name: " TO LINE-TEXT
               END-IF

               PERFORM PRINT-LINE
               PERFORM READ-NEXT-INPUT
               MOVE FUNCTION TRIM(INPUT-REC) TO TRIMMED-INPUT

                   IF FUNCTION LENGTH(TRIMMED-INPUT) = 0
                       IF FUNCTION LENGTH(FUNCTION TRIM(PROFILE-FIRST-NAME(CURRENT-USER-INDEX))) > 0
                           EXIT PERFORM 
                       ELSE
                           MOVE "First Name is required." TO LINE-TEXT
                           PERFORM PRINT-LINE
                       END-IF
                   ELSE
                       MOVE TRIMMED-INPUT TO PROFILE-FIRST-NAME(CURRENT-USER-INDEX)
                       EXIT PERFORM
                   END-IF
           END-PERFORM.

       GET-LAST-NAME.
           PERFORM UNTIL 1 = 0
               IF FUNCTION LENGTH(FUNCTION TRIM(PROFILE-LAST-NAME(CURRENT-USER-INDEX))) > 0
                   MOVE SPACES TO LINE-TEXT
                   STRING "Enter Last Name (blank keeps '"
                          FUNCTION TRIM(PROFILE-LAST-NAME(CURRENT-USER-INDEX))
                          "'): "
                          DELIMITED BY SIZE
                          INTO LINE-TEXT
                   END-STRING
               ELSE
                   MOVE "Enter Last Name: " TO LINE-TEXT
               END-IF

               PERFORM PRINT-LINE
               PERFORM READ-NEXT-INPUT
               MOVE FUNCTION TRIM(INPUT-REC) TO TRIMMED-INPUT

                   IF FUNCTION LENGTH(TRIMMED-INPUT) = 0
                       IF FUNCTION LENGTH(FUNCTION TRIM(PROFILE-LAST-NAME(CURRENT-USER-INDEX))) > 0
                           EXIT PERFORM 
                       ELSE
                           MOVE "Last Name is required." TO LINE-TEXT
                           PERFORM PRINT-LINE
                       END-IF
                   ELSE
                       MOVE TRIMMED-INPUT TO PROFILE-LAST-NAME(CURRENT-USER-INDEX)
                       EXIT PERFORM
                   END-IF
           END-PERFORM.

       GET-UNIVERSITY.
           PERFORM UNTIL 1 = 0
               IF FUNCTION LENGTH(FUNCTION TRIM(PROFILE-UNIVERSITY(CURRENT-USER-INDEX))) > 0
                   MOVE SPACES TO LINE-TEXT
                   STRING "Enter University/College Attended (blank keeps '"
                          FUNCTION TRIM(PROFILE-UNIVERSITY(CURRENT-USER-INDEX))
                          "'): "
                          DELIMITED BY SIZE
                          INTO LINE-TEXT
                   END-STRING
               ELSE
                   MOVE "Enter University/College Attended: " TO LINE-TEXT
               END-IF

               PERFORM PRINT-LINE
               PERFORM READ-NEXT-INPUT
               MOVE FUNCTION TRIM(INPUT-REC) TO TRIMMED-INPUT

                   IF FUNCTION LENGTH(TRIMMED-INPUT) = 0
                       IF FUNCTION LENGTH(FUNCTION TRIM(PROFILE-UNIVERSITY(CURRENT-USER-INDEX))) > 0
                           EXIT PERFORM 
                       ELSE
                           MOVE "University/College is required." TO LINE-TEXT
                           PERFORM PRINT-LINE
                       END-IF
                   ELSE
                       MOVE TRIMMED-INPUT TO PROFILE-UNIVERSITY(CURRENT-USER-INDEX)
                       EXIT PERFORM
                   END-IF
           END-PERFORM.

       GET-MAJOR.
           PERFORM UNTIL 1 = 0
               IF FUNCTION LENGTH(FUNCTION TRIM(PROFILE-MAJOR(CURRENT-USER-INDEX))) > 0
                   MOVE SPACES TO LINE-TEXT
                   STRING "Enter Major (blank keeps '"
                          FUNCTION TRIM(PROFILE-MAJOR(CURRENT-USER-INDEX))
                          "'): "
                          DELIMITED BY SIZE
                          INTO LINE-TEXT
                   END-STRING
               ELSE
                   MOVE "Enter Major: " TO LINE-TEXT
               END-IF

               PERFORM PRINT-LINE
               PERFORM READ-NEXT-INPUT
               MOVE FUNCTION TRIM(INPUT-REC) TO TRIMMED-INPUT

                   IF FUNCTION LENGTH(TRIMMED-INPUT) = 0
                       IF FUNCTION LENGTH(FUNCTION TRIM(PROFILE-MAJOR(CURRENT-USER-INDEX))) > 0
                           EXIT PERFORM 
                       ELSE
                           MOVE "Major is required." TO LINE-TEXT
                           PERFORM PRINT-LINE
                       END-IF
                   ELSE
                       MOVE TRIMMED-INPUT TO PROFILE-MAJOR(CURRENT-USER-INDEX)
                       EXIT PERFORM
                   END-IF
           END-PERFORM.

       *> Graduation year must be 4-digit numeric and reasonable
       GET-GRAD-YEAR.
           SET GRAD-NOT-VALID TO TRUE
           PERFORM UNTIL GRAD-VALID
               IF FUNCTION LENGTH(FUNCTION TRIM(PROFILE-GRAD-YEAR(CURRENT-USER-INDEX))) > 0
                   MOVE SPACES TO LINE-TEXT
                   STRING "Enter Graduation Year (YYYY, blank keeps '"
                          FUNCTION TRIM(PROFILE-GRAD-YEAR(CURRENT-USER-INDEX))
                          "'): "
                          DELIMITED BY SIZE
                          INTO LINE-TEXT
                   END-STRING
               ELSE
                   MOVE "Enter Graduation Year (YYYY): " TO LINE-TEXT
               END-IF

               PERFORM PRINT-LINE
               PERFORM READ-NEXT-INPUT
               MOVE FUNCTION TRIM(INPUT-REC) TO TRIMMED-INPUT

               IF FUNCTION LENGTH(TRIMMED-INPUT) = 0
                   IF FUNCTION LENGTH(FUNCTION TRIM(PROFILE-GRAD-YEAR(CURRENT-USER-INDEX))) > 0
                       SET GRAD-VALID TO TRUE *> keep existing value
                   ELSE
                       MOVE "Graduation Year is required." TO LINE-TEXT
                       PERFORM PRINT-LINE
                   END-IF
               ELSE
                   MOVE TRIMMED-INPUT TO PROFILE-GRAD-YEAR(CURRENT-USER-INDEX)
                   PERFORM VALIDATE-GRAD-YEAR

                   IF GRAD-NOT-VALID
                       MOVE "Invalid graduation year. Please enter a valid 4-digit year." TO LINE-TEXT
                       PERFORM PRINT-LINE
                   END-IF
               END-IF
           END-PERFORM.

       VALIDATE-GRAD-YEAR.
           SET GRAD-NOT-VALID TO TRUE

           *> Must be exactly 4 chars after trim
           IF FUNCTION LENGTH(FUNCTION TRIM(PROFILE-GRAD-YEAR(CURRENT-USER-INDEX))) NOT = 4
               EXIT PARAGRAPH
           END-IF

           *> All 4 chars must be digits
           MOVE PROFILE-GRAD-YEAR(CURRENT-USER-INDEX)(1:1) TO TEMP-CHAR
           IF TEMP-CHAR < "0" OR TEMP-CHAR > "9" EXIT PARAGRAPH END-IF
           MOVE PROFILE-GRAD-YEAR(CURRENT-USER-INDEX)(2:1) TO TEMP-CHAR
           IF TEMP-CHAR < "0" OR TEMP-CHAR > "9" EXIT PARAGRAPH END-IF
           MOVE PROFILE-GRAD-YEAR(CURRENT-USER-INDEX)(3:1) TO TEMP-CHAR
           IF TEMP-CHAR < "0" OR TEMP-CHAR > "9" EXIT PARAGRAPH END-IF
           MOVE PROFILE-GRAD-YEAR(CURRENT-USER-INDEX)(4:1) TO TEMP-CHAR
           IF TEMP-CHAR < "0" OR TEMP-CHAR > "9" EXIT PARAGRAPH END-IF

           *> Numeric range check
           COMPUTE GRAD-YEAR-NUM = FUNCTION NUMVAL(PROFILE-GRAD-YEAR(CURRENT-USER-INDEX))
           IF GRAD-YEAR-NUM < 1900 OR GRAD-YEAR-NUM > 2100
               EXIT PARAGRAPH
           END-IF

           SET GRAD-VALID TO TRUE.

       GET-EXPERIENCE.
           PERFORM VARYING EXP-IDX FROM 1 BY 1 UNTIL EXP-IDX > 3
               MOVE SPACES TO LINE-TEXT
               STRING "Experience #"
                      EXP-IDX
                      " - Title (blank keeps current, 'CLEAR' removes entry, 'DONE' to finish): "
                      DELIMITED BY SIZE
                      INTO LINE-TEXT
               END-STRING
               PERFORM PRINT-LINE
               PERFORM READ-NEXT-INPUT
               MOVE FUNCTION TRIM(INPUT-REC) TO TRIMMED-INPUT

               IF FUNCTION UPPER-CASE(TRIMMED-INPUT) = "DONE"
                   EXIT PERFORM
               END-IF

               IF FUNCTION UPPER-CASE(TRIMMED-INPUT) = "CLEAR"
                   MOVE SPACES TO PROFILE-EXP-TITLE(CURRENT-USER-INDEX, EXP-IDX)
                   MOVE SPACES TO PROFILE-EXP-COMPANY(CURRENT-USER-INDEX, EXP-IDX)
                   MOVE SPACES TO PROFILE-EXP-DATES(CURRENT-USER-INDEX, EXP-IDX)
                   MOVE SPACES TO PROFILE-EXP-DESC(CURRENT-USER-INDEX, EXP-IDX)
                   CONTINUE
               END-IF

               IF FUNCTION LENGTH(TRIMMED-INPUT) > 0
                   MOVE TRIMMED-INPUT TO PROFILE-EXP-TITLE(CURRENT-USER-INDEX, EXP-IDX)
               END-IF

               IF FUNCTION LENGTH(FUNCTION TRIM(PROFILE-EXP-TITLE(CURRENT-USER-INDEX, EXP-IDX))) > 0
                   MOVE SPACES TO LINE-TEXT
                   STRING "Experience #"
                          EXP-IDX
                          " - Company/Organization (blank keeps current, 'DONE' to finish): "
                          DELIMITED BY SIZE
                          INTO LINE-TEXT
                   END-STRING
                   PERFORM PRINT-LINE
                   PERFORM READ-NEXT-INPUT
                   MOVE FUNCTION TRIM(INPUT-REC) TO TRIMMED-INPUT

                   IF FUNCTION UPPER-CASE(TRIMMED-INPUT) = "DONE"
                       EXIT PERFORM
                   END-IF
                   IF FUNCTION UPPER-CASE(TRIMMED-INPUT) = "CLEAR"
                       MOVE SPACES TO PROFILE-EXP-COMPANY(CURRENT-USER-INDEX, EXP-IDX)
                   ELSE
                       IF FUNCTION LENGTH(TRIMMED-INPUT) > 0
                           MOVE TRIMMED-INPUT TO PROFILE-EXP-COMPANY(CURRENT-USER-INDEX, EXP-IDX)
                       END-IF
                   END-IF

                   MOVE SPACES TO LINE-TEXT
                   STRING "Experience #"
                          EXP-IDX
                          " - Dates (e.g., Summer 2024) (blank keeps current, 'DONE' to finish): "
                          DELIMITED BY SIZE
                          INTO LINE-TEXT
                   END-STRING
                   PERFORM PRINT-LINE
                   PERFORM READ-NEXT-INPUT
                   MOVE FUNCTION TRIM(INPUT-REC) TO TRIMMED-INPUT

                   IF FUNCTION UPPER-CASE(TRIMMED-INPUT) = "DONE"
                       EXIT PERFORM
                   END-IF
                   IF FUNCTION UPPER-CASE(TRIMMED-INPUT) = "CLEAR"
                       MOVE SPACES TO PROFILE-EXP-DATES(CURRENT-USER-INDEX, EXP-IDX)
                   ELSE
                       IF FUNCTION LENGTH(TRIMMED-INPUT) > 0
                           MOVE TRIMMED-INPUT TO PROFILE-EXP-DATES(CURRENT-USER-INDEX, EXP-IDX)
                       END-IF
                   END-IF

                   MOVE SPACES TO LINE-TEXT
                   STRING "Experience #"
                          EXP-IDX
                          " - Description (optional, blank keeps current, 'DONE' to finish): "
                          DELIMITED BY SIZE
                          INTO LINE-TEXT
                   END-STRING
                   PERFORM PRINT-LINE
                   PERFORM READ-NEXT-INPUT
                   MOVE FUNCTION TRIM(INPUT-REC) TO TRIMMED-INPUT

                   IF FUNCTION UPPER-CASE(TRIMMED-INPUT) = "DONE"
                       EXIT PERFORM
                   END-IF
                   IF FUNCTION UPPER-CASE(TRIMMED-INPUT) = "CLEAR"
                       MOVE SPACES TO PROFILE-EXP-DESC(CURRENT-USER-INDEX, EXP-IDX)
                   ELSE
                       IF FUNCTION LENGTH(TRIMMED-INPUT) > 0
                           MOVE TRIMMED-INPUT TO PROFILE-EXP-DESC(CURRENT-USER-INDEX, EXP-IDX)
                       END-IF
                   END-IF
               END-IF
           END-PERFORM.

       GET-EDUCATION.
           PERFORM VARYING EDU-IDX FROM 1 BY 1 UNTIL EDU-IDX > 3
               MOVE SPACES TO LINE-TEXT
               STRING "Education #"
                      EDU-IDX
                      " - Degree (blank keeps current, 'CLEAR' removes entry, 'DONE' to finish): "
                      DELIMITED BY SIZE
                      INTO LINE-TEXT
               END-STRING
               PERFORM PRINT-LINE
               PERFORM READ-NEXT-INPUT
               MOVE FUNCTION TRIM(INPUT-REC) TO TRIMMED-INPUT

               IF FUNCTION UPPER-CASE(TRIMMED-INPUT) = "DONE"
                   EXIT PERFORM
               END-IF

               IF FUNCTION UPPER-CASE(TRIMMED-INPUT) = "CLEAR"
                   MOVE SPACES TO PROFILE-EDU-DEGREE(CURRENT-USER-INDEX, EDU-IDX)
                   MOVE SPACES TO PROFILE-EDU-SCHOOL(CURRENT-USER-INDEX, EDU-IDX)
                   MOVE SPACES TO PROFILE-EDU-YEARS(CURRENT-USER-INDEX, EDU-IDX)
                   CONTINUE
               END-IF

               IF FUNCTION LENGTH(TRIMMED-INPUT) > 0
                   MOVE TRIMMED-INPUT TO PROFILE-EDU-DEGREE(CURRENT-USER-INDEX, EDU-IDX)
               END-IF

               IF FUNCTION LENGTH(FUNCTION TRIM(PROFILE-EDU-DEGREE(CURRENT-USER-INDEX, EDU-IDX))) > 0
                   MOVE SPACES TO LINE-TEXT
                   STRING "Education #"
                          EDU-IDX
                          " - University/College (blank keeps current, 'DONE' to finish): "
                          DELIMITED BY SIZE
                          INTO LINE-TEXT
                   END-STRING
                   PERFORM PRINT-LINE
                   PERFORM READ-NEXT-INPUT
                   MOVE FUNCTION TRIM(INPUT-REC) TO TRIMMED-INPUT

                   IF FUNCTION UPPER-CASE(TRIMMED-INPUT) = "DONE"
                       EXIT PERFORM
                   END-IF
                   IF FUNCTION UPPER-CASE(TRIMMED-INPUT) = "CLEAR"
                       MOVE SPACES TO PROFILE-EDU-SCHOOL(CURRENT-USER-INDEX, EDU-IDX)
                   ELSE
                       IF FUNCTION LENGTH(TRIMMED-INPUT) > 0
                           MOVE TRIMMED-INPUT TO PROFILE-EDU-SCHOOL(CURRENT-USER-INDEX, EDU-IDX)
                       END-IF
                   END-IF

                   MOVE SPACES TO LINE-TEXT
                   STRING "Education #"
                          EDU-IDX
                          " - Years Attended (e.g., 2023-2025) (blank keeps current, 'DONE' to finish): "
                          DELIMITED BY SIZE
                          INTO LINE-TEXT
                   END-STRING
                   PERFORM PRINT-LINE
                   PERFORM READ-NEXT-INPUT
                   MOVE FUNCTION TRIM(INPUT-REC) TO TRIMMED-INPUT

                   IF FUNCTION UPPER-CASE(TRIMMED-INPUT) = "DONE"
                       EXIT PERFORM
                   END-IF
                   IF FUNCTION UPPER-CASE(TRIMMED-INPUT) = "CLEAR"
                       MOVE SPACES TO PROFILE-EDU-YEARS(CURRENT-USER-INDEX, EDU-IDX)
                   ELSE
                       IF FUNCTION LENGTH(TRIMMED-INPUT) > 0
                           MOVE TRIMMED-INPUT TO PROFILE-EDU-YEARS(CURRENT-USER-INDEX, EDU-IDX)
                       END-IF
                   END-IF
               END-IF
           END-PERFORM.

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

               *> Spec wording: Go Back return to previous menu
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
