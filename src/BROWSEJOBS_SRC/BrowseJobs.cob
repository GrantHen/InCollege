       BROWSE-JOBS.
           *> If there are no jobs stored yet, inform the user
           IF JOB-COUNT = 0
               MOVE "No jobs available." TO LINE-TEXT
               PERFORM PRINT-LINE
               EXIT PARAGRAPH
           END-IF

           *> Start at the first job entry
           MOVE 1 TO I

           *> Loop through all stored jobs
           PERFORM UNTIL I > JOB-COUNT

               *> Display job title
               MOVE SPACES TO LINE-TEXT
               STRING
                   "Title: "
                   FUNCTION TRIM(JOB-TITLE(I))
                   DELIMITED BY SIZE
                   INTO LINE-TEXT
               END-STRING
               PERFORM PRINT-LINE

               *> Display employer
               MOVE SPACES TO LINE-TEXT
               STRING
                   "Employer: "
                   FUNCTION TRIM(JOB-EMPLOYER(I))
                   DELIMITED BY SIZE
                   INTO LINE-TEXT
               END-STRING
               PERFORM PRINT-LINE

               *> Display location
               MOVE SPACES TO LINE-TEXT
               STRING
                   "Location: "
                   FUNCTION TRIM(JOB-LOCATION(I))
                   DELIMITED BY SIZE
                   INTO LINE-TEXT
               END-STRING
               PERFORM PRINT-LINE

               *> Display salary
               MOVE SPACES TO LINE-TEXT
               STRING
                   "Salary: "
                   FUNCTION TRIM(JOB-SALARY(I))
                   DELIMITED BY SIZE
                   INTO LINE-TEXT
               END-STRING
               PERFORM PRINT-LINE


               *> Blank line between job listings
               MOVE " " TO LINE-TEXT
               PERFORM PRINT-LINE

               ADD 1 TO I

           END-PERFORM.
