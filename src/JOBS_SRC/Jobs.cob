       *> Week 6 slice owned by this developer:
       *> - submenu
       *> - post job flow
       *> - file persistence
       *> TODO teammate: expand browse jobs and any richer validation/polish.
       JOB-SEARCH-INTERNSHIP-MENU.
           MOVE 0 TO JOB-MENU-CHOICE
           PERFORM UNTIL JOB-MENU-CHOICE = 3
               MOVE "--- Job Search / Internship ---" TO LINE-TEXT
               PERFORM PRINT-LINE
               MOVE "1. Post a Job/Internship" TO LINE-TEXT
               PERFORM PRINT-LINE
               MOVE "2. Browse Jobs/Internships" TO LINE-TEXT
               PERFORM PRINT-LINE
               MOVE "3. Back to Main Menu" TO LINE-TEXT
               PERFORM PRINT-LINE
               MOVE "Enter your choice: " TO LINE-TEXT
               PERFORM PRINT-LINE

               PERFORM GET-JOB-MENU-CHOICE

               EVALUATE JOB-MENU-CHOICE
                   WHEN 1
                       PERFORM POST-JOB-INTERNSHIP
                   WHEN 2
                       PERFORM BROWSE-JOBS-UNDER-CONSTRUCTION
                   WHEN 3
                       CONTINUE
                   WHEN OTHER
                       MOVE "Invalid choice. Try again." TO LINE-TEXT
                       PERFORM PRINT-LINE
                       MOVE " " TO LINE-TEXT
                       PERFORM PRINT-LINE
               END-EVALUATE
           END-PERFORM.

       GET-JOB-MENU-CHOICE.
           PERFORM READ-NEXT-INPUT
           IF INPUT-EOF-YES
               MOVE 3 TO JOB-MENU-CHOICE
           ELSE
               IF INPUT-REC(1:1) >= "0" AND INPUT-REC(1:1) <= "9"
                   COMPUTE JOB-MENU-CHOICE = FUNCTION NUMVAL(INPUT-REC(1:1))
               ELSE
                   MOVE 0 TO JOB-MENU-CHOICE
               END-IF
           END-IF.

       POST-JOB-INTERNSHIP.
           SET JOB-POST-ACTIVE TO TRUE
           MOVE SPACES TO JOB-TITLE-IN
           MOVE SPACES TO JOB-DESCRIPTION-IN
           MOVE SPACES TO JOB-EMPLOYER-IN
           MOVE SPACES TO JOB-LOCATION-IN
           MOVE "NONE" TO JOB-SALARY-IN

           MOVE "--- Post a Job/Internship ---" TO LINE-TEXT
           PERFORM PRINT-LINE

           PERFORM GET-JOB-TITLE
           IF JOB-POST-CANCELLED
               EXIT PARAGRAPH
           END-IF

           PERFORM GET-JOB-DESCRIPTION
           IF JOB-POST-CANCELLED
               EXIT PARAGRAPH
           END-IF

           PERFORM GET-JOB-EMPLOYER
           IF JOB-POST-CANCELLED
               EXIT PARAGRAPH
           END-IF

           PERFORM GET-JOB-LOCATION
           IF JOB-POST-CANCELLED
               EXIT PARAGRAPH
           END-IF

           PERFORM GET-JOB-SALARY

           IF JOB-COUNT < MAX-JOBS
               ADD 1 TO JOB-LAST-ID
               ADD 1 TO JOB-COUNT
               MOVE JOB-LAST-ID TO JOB-ID(JOB-COUNT)
               MOVE STORED-USERNAME(CURRENT-USER-INDEX)
                   TO JOB-POSTED-BY(JOB-COUNT)
               MOVE JOB-TITLE-IN TO JOB-TITLE(JOB-COUNT)
               MOVE JOB-DESCRIPTION-IN TO JOB-DESCRIPTION(JOB-COUNT)
               MOVE JOB-EMPLOYER-IN TO JOB-EMPLOYER(JOB-COUNT)
               MOVE JOB-LOCATION-IN TO JOB-LOCATION(JOB-COUNT)
               MOVE JOB-SALARY-IN TO JOB-SALARY(JOB-COUNT)

               PERFORM SAVE-JOBS

               MOVE SPACES TO LINE-TEXT
               STRING "Job/Internship posted successfully. ID: "
                      JOB-ID(JOB-COUNT)
                      DELIMITED BY SIZE
                      INTO LINE-TEXT
               END-STRING
               PERFORM PRINT-LINE
           ELSE
               MOVE "Maximum posted jobs reached." TO LINE-TEXT
               PERFORM PRINT-LINE
           END-IF

           MOVE " " TO LINE-TEXT
           PERFORM PRINT-LINE.

       GET-JOB-TITLE.
           PERFORM UNTIL FUNCTION LENGTH(FUNCTION TRIM(JOB-TITLE-IN)) > 0
               MOVE "Enter Job Title (required): " TO LINE-TEXT
               PERFORM PRINT-LINE
               PERFORM READ-NEXT-INPUT

               IF INPUT-EOF-YES
                   SET JOB-POST-CANCELLED TO TRUE
                   MOVE "Input ended before job posting was completed." TO LINE-TEXT
                   PERFORM PRINT-LINE
                   EXIT PERFORM
               END-IF

               MOVE FUNCTION TRIM(INPUT-REC) TO JOB-TITLE-IN

               IF FUNCTION LENGTH(FUNCTION TRIM(JOB-TITLE-IN)) = 0
                   MOVE "Job Title is required." TO LINE-TEXT
                   PERFORM PRINT-LINE
               END-IF
           END-PERFORM.

       GET-JOB-DESCRIPTION.
           PERFORM UNTIL FUNCTION LENGTH(FUNCTION TRIM(JOB-DESCRIPTION-IN)) > 0
               MOVE "Enter Description (required): " TO LINE-TEXT
               PERFORM PRINT-LINE
               PERFORM READ-NEXT-INPUT

               IF INPUT-EOF-YES
                   SET JOB-POST-CANCELLED TO TRUE
                   MOVE "Input ended before job posting was completed." TO LINE-TEXT
                   PERFORM PRINT-LINE
                   EXIT PERFORM
               END-IF

               MOVE FUNCTION TRIM(INPUT-REC) TO JOB-DESCRIPTION-IN

               IF FUNCTION LENGTH(FUNCTION TRIM(JOB-DESCRIPTION-IN)) = 0
                   MOVE "Description is required." TO LINE-TEXT
                   PERFORM PRINT-LINE
               END-IF
           END-PERFORM.

       GET-JOB-EMPLOYER.
           PERFORM UNTIL FUNCTION LENGTH(FUNCTION TRIM(JOB-EMPLOYER-IN)) > 0
               MOVE "Enter Employer (required): " TO LINE-TEXT
               PERFORM PRINT-LINE
               PERFORM READ-NEXT-INPUT

               IF INPUT-EOF-YES
                   SET JOB-POST-CANCELLED TO TRUE
                   MOVE "Input ended before job posting was completed." TO LINE-TEXT
                   PERFORM PRINT-LINE
                   EXIT PERFORM
               END-IF

               MOVE FUNCTION TRIM(INPUT-REC) TO JOB-EMPLOYER-IN

               IF FUNCTION LENGTH(FUNCTION TRIM(JOB-EMPLOYER-IN)) = 0
                   MOVE "Employer is required." TO LINE-TEXT
                   PERFORM PRINT-LINE
               END-IF
           END-PERFORM.

       GET-JOB-LOCATION.
           PERFORM UNTIL FUNCTION LENGTH(FUNCTION TRIM(JOB-LOCATION-IN)) > 0
               MOVE "Enter Location (required): " TO LINE-TEXT
               PERFORM PRINT-LINE
               PERFORM READ-NEXT-INPUT

               IF INPUT-EOF-YES
                   SET JOB-POST-CANCELLED TO TRUE
                   MOVE "Input ended before job posting was completed." TO LINE-TEXT
                   PERFORM PRINT-LINE
                   EXIT PERFORM
               END-IF

               MOVE FUNCTION TRIM(INPUT-REC) TO JOB-LOCATION-IN

               IF FUNCTION LENGTH(FUNCTION TRIM(JOB-LOCATION-IN)) = 0
                   MOVE "Location is required." TO LINE-TEXT
                   PERFORM PRINT-LINE
               END-IF
           END-PERFORM.

       GET-JOB-SALARY.
           MOVE "Enter Salary (optional; enter NONE to skip): " TO LINE-TEXT
           PERFORM PRINT-LINE
           PERFORM READ-NEXT-INPUT

           IF INPUT-EOF-YES
               MOVE "NONE" TO JOB-SALARY-IN
           ELSE
               MOVE FUNCTION TRIM(INPUT-REC) TO JOB-SALARY-IN
               IF FUNCTION LENGTH(FUNCTION TRIM(JOB-SALARY-IN)) = 0
                   MOVE "NONE" TO JOB-SALARY-IN
               END-IF

               IF FUNCTION UPPER-CASE(FUNCTION TRIM(JOB-SALARY-IN)) = "NONE"
                   MOVE "NONE" TO JOB-SALARY-IN
               END-IF
           END-IF.

       LOAD-JOBS.
           SET EOF-NO TO TRUE
           MOVE 0 TO JOB-COUNT
           MOVE 0 TO JOB-LAST-ID
           OPEN INPUT JOBS-FILE

           IF JOBS-FILE-STATUS = "35"
               OPEN OUTPUT JOBS-FILE
               CLOSE JOBS-FILE
           ELSE
               PERFORM UNTIL EOF-YES
                   READ JOBS-FILE
                       AT END
                           SET EOF-YES TO TRUE
                       NOT AT END
                           INSPECT JOBS-REC
                               REPLACING ALL LOW-VALUES BY SPACE
                           PERFORM PARSE-JOB-LINE
                   END-READ
               END-PERFORM
               CLOSE JOBS-FILE
           END-IF

           SET EOF-NO TO TRUE.

       PARSE-JOB-LINE.
           IF JOB-COUNT < MAX-JOBS
               ADD 1 TO JOB-COUNT
               MOVE SPACES TO JOB-ID-TEXT
               MOVE SPACES TO JOB-POSTED-BY(JOB-COUNT)
               MOVE SPACES TO JOB-TITLE(JOB-COUNT)
               MOVE SPACES TO JOB-DESCRIPTION(JOB-COUNT)
               MOVE SPACES TO JOB-EMPLOYER(JOB-COUNT)
               MOVE SPACES TO JOB-LOCATION(JOB-COUNT)
               MOVE SPACES TO JOB-SALARY(JOB-COUNT)

               UNSTRING JOBS-REC DELIMITED BY "|"
                   INTO JOB-ID-TEXT
                        JOB-POSTED-BY(JOB-COUNT)
                        JOB-TITLE(JOB-COUNT)
                        JOB-DESCRIPTION(JOB-COUNT)
                        JOB-EMPLOYER(JOB-COUNT)
                        JOB-LOCATION(JOB-COUNT)
                        JOB-SALARY(JOB-COUNT)
               END-UNSTRING

               IF FUNCTION LENGTH(FUNCTION TRIM(JOB-ID-TEXT)) > 0
                   COMPUTE JOB-ID(JOB-COUNT) = FUNCTION NUMVAL(JOB-ID-TEXT)
               ELSE
                   ADD 1 TO JOB-LAST-ID
                   MOVE JOB-LAST-ID TO JOB-ID(JOB-COUNT)
               END-IF

               IF JOB-ID(JOB-COUNT) > JOB-LAST-ID
                   MOVE JOB-ID(JOB-COUNT) TO JOB-LAST-ID
               END-IF
           END-IF.

       SAVE-JOBS.
           OPEN OUTPUT JOBS-FILE
           PERFORM VARYING J FROM 1 BY 1
                   UNTIL J > JOB-COUNT
               MOVE SPACES TO JOBS-REC
               STRING
                   JOB-ID(J)
                   "|"
                   FUNCTION TRIM(JOB-POSTED-BY(J))
                   "|"
                   FUNCTION TRIM(JOB-TITLE(J))
                   "|"
                   FUNCTION TRIM(JOB-DESCRIPTION(J))
                   "|"
                   FUNCTION TRIM(JOB-EMPLOYER(J))
                   "|"
                   FUNCTION TRIM(JOB-LOCATION(J))
                   "|"
                   FUNCTION TRIM(JOB-SALARY(J))
                   DELIMITED BY SIZE
                   INTO JOBS-REC
               END-STRING
               WRITE JOBS-REC
           END-PERFORM
           CLOSE JOBS-FILE.
