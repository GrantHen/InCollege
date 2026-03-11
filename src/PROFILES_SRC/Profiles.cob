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

       *> WEEK 2: Create/Edit My Profile
       CREATE-EDIT-PROFILE.
           PERFORM UNTIL 1 = 0
               *> Header matches sample output
               MOVE "----------- Create/Edit Profile -----------" TO LINE-TEXT
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

               MOVE "***********************************" TO LINE-TEXT
               PERFORM PRINT-LINE
               MOVE "*   Profile saved successfully!   *" TO LINE-TEXT
               PERFORM PRINT-LINE
               MOVE "***********************************" TO LINE-TEXT
               PERFORM PRINT-LINE

               *> Present explicit option to return to main menu after saving
               MOVE 0 TO POST-SAVE-CHOICE
               PERFORM UNTIL POST-SAVE-CHOICE = 1 OR POST-SAVE-CHOICE = 2
                   MOVE "----------------------" TO LINE-TEXT
                   PERFORM PRINT-LINE
                   MOVE "1. Return to Main Menu" TO LINE-TEXT
                   PERFORM PRINT-LINE
                   MOVE "2. Edit Profile Again" TO LINE-TEXT
                   PERFORM PRINT-LINE
                   MOVE "----------------------" TO LINE-TEXT
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
               MOVE " " TO LINE-TEXT
               PERFORM PRINT-LINE
               MOVE "----| User Profile |----" TO LINE-TEXT
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
               MOVE "-------------------------------------------------------" TO LINE-TEXT
           END-IF
           PERFORM PRINT-LINE.

       *> Week 2: View My Profile (now uses the shared DISPLAY-USER-PROFILE)
       VIEW-MY-PROFILE.
           *> Set the display index to current user
           MOVE CURRENT-USER-INDEX TO DISPLAY-USER-INDEX

           *> Week 3: match sample output header for self profileclear
           MOVE "-------------------- Your Profile ---------------------" TO PROFILE-DISPLAY-HEADER
           MOVE "-------------------------------------------------------" TO PROFILE-DISPLAY-FOOTER
           
           *> Call the shared display routine
           PERFORM DISPLAY-USER-PROFILE

           *> Clear header/footer so other screens keep defaults
           MOVE SPACES TO PROFILE-DISPLAY-HEADER
           MOVE SPACES TO PROFILE-DISPLAY-FOOTER
           
           *> Add blank line after display
           MOVE " " TO LINE-TEXT
           PERFORM PRINT-LINE.

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
