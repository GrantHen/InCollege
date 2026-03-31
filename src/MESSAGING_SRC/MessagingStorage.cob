      *> ============================================================
      *> WEEK 8: Message working storage
      *> ============================================================
       01  MSG-FILE-STATUS            PIC XX VALUE "00".
       01  MSG-MENU-CHOICE            PIC 9 VALUE 0.

       01  MSG-RECIPIENT-INPUT        PIC X(20).
       01  MSG-CONTENT-RAW            PIC X(500).
       01  MSG-CONTENT-INPUT          PIC X(200).

       01  MSG-RECIPIENT-VALID        PIC X VALUE "N".
           88  RECIPIENT-VALID        VALUE "Y".
           88  RECIPIENT-NOT-VALID    VALUE "N".

       01  MSG-RECIPIENT-EXISTS       PIC X VALUE "N".
           88  RECIPIENT-EXISTS       VALUE "Y".
           88  RECIPIENT-NOT-EXISTS   VALUE "N".

       01  MSG-CONTENT-LEN            PIC 9(3) VALUE 0.

      *> Timestamp fields 
       01  WS-CURRENT-DATE-TIME       PIC X(21).
       01  WS-TIMESTAMP               PIC X(20).
       01  WS-YEAR                    PIC X(4).
       01  WS-MONTH                   PIC X(2).
       01  WS-DAY                     PIC X(2).
       01  WS-HOUR                    PIC X(2).
       01  WS-MINUTE                  PIC X(2).
       01  WS-SECOND                  PIC X(2).
