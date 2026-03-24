       BROWSE-JOBS.
           IF JOB-COUNT = 0
               MOVE "No jobs/internships are currently available." TO LINE-TEXT
               PERFORM PRINT-LINE
               MOVE " " TO LINE-TEXT
               PERFORM PRINT-LINE
               EXIT PARAGRAPH
           END-IF

           MOVE 99 TO BROWSE-JOB-CHOICE
           PERFORM UNTIL BROWSE-JOB-CHOICE = 0
               PERFORM DISPLAY-JOB-SUMMARIES
               PERFORM GET-BROWSE-JOB-CHOICE

               IF BROWSE-JOB-CHOICE = 0
                   CONTINUE
               ELSE
                   IF BROWSE-JOB-CHOICE >= 1
                      AND BROWSE-JOB-CHOICE <= JOB-COUNT
                       MOVE BROWSE-JOB-CHOICE TO SELECTED-JOB-INDEX
                       PERFORM VIEW-SELECTED-JOB-DETAILS
                   ELSE
                       MOVE "Invalid job selection. Try again." TO LINE-TEXT
                       PERFORM PRINT-LINE
                       MOVE " " TO LINE-TEXT
                       PERFORM PRINT-LINE
                   END-IF
               END-IF
           END-PERFORM

           MOVE " " TO LINE-TEXT
           PERFORM PRINT-LINE.

       DISPLAY-JOB-SUMMARIES.
           MOVE "------- Browse Jobs / Internships -------" TO LINE-TEXT
           PERFORM PRINT-LINE

           PERFORM VARYING J FROM 1 BY 1
                   UNTIL J > JOB-COUNT
               MOVE SPACES TO LINE-TEXT
               STRING J
                      ". "
                      FUNCTION TRIM(JOB-TITLE(J))
                      " | "
                      FUNCTION TRIM(JOB-EMPLOYER(J))
                      " | "
                      FUNCTION TRIM(JOB-LOCATION(J))
                      DELIMITED BY SIZE
                      INTO LINE-TEXT
               END-STRING
               PERFORM PRINT-LINE
           END-PERFORM

           MOVE "0. Return to Job Menu" TO LINE-TEXT
           PERFORM PRINT-LINE
           MOVE "Enter a job number to view details: " TO LINE-TEXT
           PERFORM PRINT-LINE.

       GET-BROWSE-JOB-CHOICE.
           PERFORM READ-NEXT-INPUT

           IF INPUT-EOF-YES
               MOVE 0 TO BROWSE-JOB-CHOICE
               EXIT PARAGRAPH
           END-IF

           MOVE SPACES TO JOB-CHOICE-TEXT
           MOVE FUNCTION TRIM(INPUT-REC) TO JOB-CHOICE-TEXT

           IF FUNCTION LENGTH(FUNCTION TRIM(JOB-CHOICE-TEXT)) > 0
              AND FUNCTION TEST-NUMVAL(JOB-CHOICE-TEXT) = 0
               COMPUTE BROWSE-JOB-CHOICE =
                   FUNCTION NUMVAL(JOB-CHOICE-TEXT)
           ELSE
               MOVE 99 TO BROWSE-JOB-CHOICE
           END-IF.

       VIEW-SELECTED-JOB-DETAILS.
           MOVE 9 TO JOB-DETAIL-CHOICE

           PERFORM UNTIL JOB-DETAIL-CHOICE = 0
               MOVE "------- Job Details -------" TO LINE-TEXT
               PERFORM PRINT-LINE

               MOVE SPACES TO LINE-TEXT
               STRING "Title: "
                      FUNCTION TRIM(JOB-TITLE(SELECTED-JOB-INDEX))
                      DELIMITED BY SIZE
                      INTO LINE-TEXT
               END-STRING
               PERFORM PRINT-LINE

               MOVE SPACES TO LINE-TEXT
               STRING "Description: "
                      FUNCTION TRIM(JOB-DESCRIPTION(SELECTED-JOB-INDEX))
                      DELIMITED BY SIZE
                      INTO LINE-TEXT
               END-STRING
               PERFORM PRINT-LINE

               MOVE SPACES TO LINE-TEXT
               STRING "Employer: "
                      FUNCTION TRIM(JOB-EMPLOYER(SELECTED-JOB-INDEX))
                      DELIMITED BY SIZE
                      INTO LINE-TEXT
               END-STRING
               PERFORM PRINT-LINE

               MOVE SPACES TO LINE-TEXT
               STRING "Location: "
                      FUNCTION TRIM(JOB-LOCATION(SELECTED-JOB-INDEX))
                      DELIMITED BY SIZE
                      INTO LINE-TEXT
               END-STRING
               PERFORM PRINT-LINE

               MOVE SPACES TO LINE-TEXT
               STRING "Salary: "
                      FUNCTION TRIM(JOB-SALARY(SELECTED-JOB-INDEX))
                      DELIMITED BY SIZE
                      INTO LINE-TEXT
               END-STRING
               PERFORM PRINT-LINE

               MOVE "1. Apply for this Job" TO LINE-TEXT
               PERFORM PRINT-LINE
               MOVE "0. Return to Browse Jobs" TO LINE-TEXT
               PERFORM PRINT-LINE
               MOVE "Enter your choice: " TO LINE-TEXT
               PERFORM PRINT-LINE

               PERFORM GET-JOB-DETAIL-CHOICE

               EVALUATE JOB-DETAIL-CHOICE
                   WHEN 1
                       PERFORM APPLY-FOR-SELECTED-JOB
                       MOVE 0 TO JOB-DETAIL-CHOICE
                       MOVE 0 TO BROWSE-JOB-CHOICE
                   WHEN 0
                       CONTINUE
                   WHEN OTHER
                       MOVE "Invalid choice. Try again." TO LINE-TEXT
                       PERFORM PRINT-LINE
                       MOVE " " TO LINE-TEXT
                       PERFORM PRINT-LINE
               END-EVALUATE
           END-PERFORM.

       GET-JOB-DETAIL-CHOICE.
           PERFORM READ-NEXT-INPUT

           IF INPUT-EOF-YES
               MOVE 0 TO JOB-DETAIL-CHOICE
               EXIT PARAGRAPH
           END-IF

           IF INPUT-REC(1:1) >= "0" AND INPUT-REC(1:1) <= "1"
               COMPUTE JOB-DETAIL-CHOICE =
                   FUNCTION NUMVAL(INPUT-REC(1:1))
           ELSE
               MOVE 9 TO JOB-DETAIL-CHOICE
           END-IF.
