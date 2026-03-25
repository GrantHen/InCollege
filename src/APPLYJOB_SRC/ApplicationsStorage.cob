       *> Epic 7 GH scope: minimal application persistence only.
       01  APPLICATIONS-FILE-STATUS   PIC XX VALUE "00".
       01  BROWSE-JOB-CHOICE          PIC 99 VALUE 0.
       01  SELECTED-JOB-INDEX         PIC 99 VALUE 0.
       01  JOB-DETAIL-CHOICE          PIC 9 VALUE 0.
       
       01  JOB-CHOICE-TEXT            PIC X(10).
       01  APP-USERNAME               PIC X(20).
       01  APP-JOB-ID-TEXT            PIC X(10).
       01  APP-JOB-ID-NUM             PIC 9(4).
       01  APP-COUNT                  PIC 99 VALUE 0.
       01  APP-MATCH-IDX              PIC 99 VALUE 0.