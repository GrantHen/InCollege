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

           SELECT CONNECTIONS-FILE ASSIGN TO "data/connections.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS CONN-FILE-STATUS.

           SELECT REQUESTS-FILE ASSIGN TO "data/requests.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS REQ-FILE-STATUS.

           COPY "JOBS_SRC/JobsFileControl.cob".
           COPY "APPLYJOB_SRC/ApplicationsFileControl.cob".
           COPY "MESSAGING_SRC/MessagingFileControl.cob".


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

       FD  CONNECTIONS-FILE. *> Stores established connections
       01  CONNECTIONS-REC            PIC X(80).

       FD  REQUESTS-FILE. *> Stores pending connection requests
       01  REQUESTS-REC               PIC X(80).

       COPY "JOBS_SRC/JobsFileSection.cob".
       COPY "APPLYJOB_SRC/ApplicationsFileSection.cob".
       COPY "MESSAGING_SRC/MessagingFileSection.cob".
       FD  INPUT-FILE. *> Define the input file (all menu/user input comes from here)
       01  INPUT-REC                  PIC X(500).

       FD  OUTPUT-FILE. *> Define the output file (everything displayed is also written here)
       01  OUTPUT-REC                 PIC X(500).

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
       01  LINE-TEXT                  PIC X(500). *> holds what we print/write

       01  PROFILE-FILE-STATUS        PIC XX VALUE "00".
       01  PROFILE-POINTER            PIC 9(4) VALUE 1.
       01  LOOKUP-USERNAME            PIC X(20).
       01  FOUND-ACCOUNT-INDEX        PIC 9 VALUE 0.

       01  TEXT-LEN                   PIC 99 VALUE 0.
       01  TEMP-CHAR                  PIC X.
       01  TRIMMED-INPUT              PIC X(500).

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
       01  PROFILE-DISPLAY-HEADER      PIC X(100) VALUE SPACES.
       01  PROFILE-DISPLAY-FOOTER      PIC X(100) VALUE SPACES.
       01  TEMP-FULL-NAME              PIC X(100) VALUE SPACES.

      *> ============================================================
      *> WEEK 4: Connection request data structures
      *> ============================================================
      *> File statuses
       01  CONN-FILE-STATUS           PIC XX VALUE "00".
       01  REQ-FILE-STATUS            PIC XX VALUE "00".

      *> Established connections table (already connected pairs)
      *> Format in file: sender_username|recipient_username
       01  MAX-CONNECTIONS            PIC 99 VALUE 25.
       01  CONNECTION-COUNT           PIC 99 VALUE 0.
       01  CONNECTION-TABLE.
           05  CONNECTION-ENTRY OCCURS 25 TIMES.
               10  CONN-USER1         PIC X(20).
               10  CONN-USER2         PIC X(20).

      *> Pending connection requests table
      *> Format in file: sender_username|recipient_username
       01  MAX-REQUESTS               PIC 99 VALUE 25.
       01  REQUEST-COUNT              PIC 99 VALUE 0.
       01  REQUEST-TABLE.
           05  REQUEST-ENTRY OCCURS 25 TIMES.
               10  REQ-SENDER         PIC X(20).
               10  REQ-RECIPIENT      PIC X(20).

      *> Flags for connection request validation
       01  ALREADY-CONNECTED-FLAG     PIC X VALUE "N".
           88  ALREADY-CONNECTED      VALUE "Y".
           88  NOT-ALREADY-CONNECTED  VALUE "N".

       01  PENDING-EXISTS-FLAG        PIC X VALUE "N".
           88  PENDING-EXISTS         VALUE "Y".
           88  PENDING-NOT-EXISTS     VALUE "N".

       01  REVERSE-PENDING-FLAG       PIC X VALUE "N".
           88  REVERSE-PENDING        VALUE "Y".
           88  REVERSE-NOT-PENDING    VALUE "N".

       01  SEND-REQ-CHOICE            PIC 9 VALUE 0.

       01  J                          PIC 99 VALUE 1.

       01  PENDING-FOUND-ANY          PIC X VALUE "N".
           88  HAS-PENDING            VALUE "Y".
           88  NO-PENDING             VALUE "N".

       *> EPIC #5:
       *> Stores the username on the other side of a request/connection
       01  CONNECTED-USERNAME          PIC X(20) VALUE SPACES.

       *> used to know if the user has at least one connection when viewing network
       01  NETWORK-FOUND               PIC X VALUE "N".
           88  HAS-CONNECTIONS         VALUE "Y".
           88  NO-CONNECTIONS          VALUE "N".

       COPY "JOBS_SRC/JobsStorage.cob".
       COPY "APPLYJOB_SRC/ApplicationsStorage.cob".
       COPY "MESSAGING_SRC/MessagingStorage.cob".


       PROCEDURE DIVISION.
       MAIN.
           *> Open input/output files at the start so all ACCEPTs are replaced with READs
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE

           PERFORM LOAD-ACCOUNTS
           PERFORM LOAD-PROFILES
           PERFORM LOAD-CONNECTIONS
           PERFORM LOAD-REQUESTS
           PERFORM LOAD-JOBS
           PERFORM INIT-MESSAGES-FILE
           PERFORM START-SCREEN

           *> Close files when program ends
           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE
           STOP RUN.

       COPY "COMMON_SRC/IOHelpers.cob".

       COPY "AUTH_SRC/AuthMenus.cob".
       COPY "ACCOUNTS_SRC/Accounts.cob".
       COPY "PROFILES_SRC/Profiles.cob".
       COPY "CONNECTIONS_SRC/Connections.cob".
       COPY "SEARCH_SRC/SearchUsers.cob".
       COPY "SKILLS_SRC/Skills.cob".

       COPY "MESSAGING_SRC/SendMessage.cob".
       COPY "MESSAGING_SRC/ViewMessages.cob".

       COPY "JOBS_SRC/Jobs.cob".
       COPY "BROWSEJOBS_SRC/BrowseJobs.cob".
       COPY "APPLYJOB_SRC/ApplyJob.cob".
       COPY "VIEWNET_SRC/ViewNetwork.cob".
       COPY "VIEWAPPLICATIONS_SRC/ViewApplications.cob".
