       *> Week 6: job posting storage
       01  JOBS-FILE-STATUS           PIC XX VALUE "00".
       01  MAX-JOBS                   PIC 99 VALUE 25.
       01  JOB-COUNT                  PIC 99 VALUE 0.
       01  JOB-LAST-ID                PIC 9(4) VALUE 0.
       01  JOB-MENU-CHOICE            PIC 9 VALUE 0.
       01  JOB-ID-TEXT                PIC X(10).
       01  JOB-TITLE-IN               PIC X(60).
       01  JOB-DESCRIPTION-IN         PIC X(200).
       01  JOB-EMPLOYER-IN            PIC X(60).
       01  JOB-LOCATION-IN            PIC X(60).
       01  JOB-SALARY-IN              PIC X(30).
       01  JOB-POST-CANCELLED-FLAG    PIC X VALUE "N".
           88  JOB-POST-CANCELLED     VALUE "Y".
           88  JOB-POST-ACTIVE        VALUE "N".

       01  JOB-TABLE.
           05  JOB-ENTRY OCCURS 25 TIMES.
               10  JOB-ID             PIC 9(4).
               10  JOB-POSTED-BY      PIC X(20).
               10  JOB-TITLE          PIC X(60).
               10  JOB-DESCRIPTION    PIC X(200).
               10  JOB-EMPLOYER       PIC X(60).
               10  JOB-LOCATION       PIC X(60).
               10  JOB-SALARY         PIC X(30).
