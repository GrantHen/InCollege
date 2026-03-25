       APPLY-FOR-SELECTED-JOB.
           OPEN EXTEND APPLICATIONS-FILE

           IF APPLICATIONS-FILE-STATUS = "35"
               OPEN OUTPUT APPLICATIONS-FILE
               CLOSE APPLICATIONS-FILE
               OPEN EXTEND APPLICATIONS-FILE
           END-IF

           MOVE SPACES TO APPLICATIONS-REC
           STRING FUNCTION TRIM(STORED-USERNAME(CURRENT-USER-INDEX))
                  "|"
                  JOB-ID(SELECTED-JOB-INDEX)
                  DELIMITED BY SIZE
                  INTO APPLICATIONS-REC
           END-STRING
           WRITE APPLICATIONS-REC
           CLOSE APPLICATIONS-FILE

           MOVE SPACES TO LINE-TEXT
           STRING "Your application for "
                  FUNCTION TRIM(JOB-TITLE(SELECTED-JOB-INDEX))
                  " at "
                  FUNCTION TRIM(JOB-EMPLOYER(SELECTED-JOB-INDEX))
                  " has been submitted."
                  DELIMITED BY SIZE
                  INTO LINE-TEXT
           END-STRING
           PERFORM PRINT-LINE

           MOVE " " TO LINE-TEXT
           PERFORM PRINT-LINE.
