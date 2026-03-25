      VIEW-MY-APPLICATIONS.
          MOVE 0 TO APP-COUNT
          MOVE "--- Your Job Applications ---" TO LINE-TEXT
          PERFORM PRINT-LINE
          MOVE SPACES TO LINE-TEXT
          STRING "Application Summary for "
                 FUNCTION TRIM(STORED-USERNAME(CURRENT-USER-INDEX))
                 DELIMITED BY SIZE
                 INTO LINE-TEXT
          END-STRING
          PERFORM PRINT-LINE
          MOVE "------------------------------" TO LINE-TEXT
          PERFORM PRINT-LINE

          SET EOF-NO TO TRUE
          OPEN INPUT APPLICATIONS-FILE

          IF APPLICATIONS-FILE-STATUS = "35"
              OPEN OUTPUT APPLICATIONS-FILE
              CLOSE APPLICATIONS-FILE
          ELSE
              PERFORM UNTIL EOF-YES
                  READ APPLICATIONS-FILE
                      AT END
                          SET EOF-YES TO TRUE
                      NOT AT END
                          INSPECT APPLICATIONS-REC
                              REPLACING ALL LOW-VALUES BY SPACE

                          IF FUNCTION LENGTH(
                              FUNCTION TRIM(APPLICATIONS-REC)) > 0

                              MOVE SPACES TO APP-USERNAME
                              MOVE SPACES TO APP-JOB-ID-TEXT
                              UNSTRING APPLICATIONS-REC DELIMITED BY "|"
                                  INTO APP-USERNAME
                                       APP-JOB-ID-TEXT
                              END-UNSTRING

                              IF FUNCTION TRIM(APP-USERNAME) =
                                 FUNCTION TRIM(
                                     STORED-USERNAME(CURRENT-USER-INDEX))

                                  COMPUTE APP-JOB-ID-NUM =
                                      FUNCTION NUMVAL(APP-JOB-ID-TEXT)

                                  MOVE 0 TO APP-MATCH-IDX
                                  PERFORM VARYING J FROM 1 BY 1
                                          UNTIL J > JOB-COUNT
                                      IF JOB-ID(J) = APP-JOB-ID-NUM
                                          MOVE J TO APP-MATCH-IDX
                                      END-IF
                                  END-PERFORM

                                  IF APP-MATCH-IDX > 0
                                      ADD 1 TO APP-COUNT

                                      MOVE SPACES TO LINE-TEXT
                                      STRING "Job Title: "
                                             FUNCTION TRIM(
                                                 JOB-TITLE(APP-MATCH-IDX))
                                             DELIMITED BY SIZE
                                             INTO LINE-TEXT
                                      END-STRING
                                      PERFORM PRINT-LINE

                                      MOVE SPACES TO LINE-TEXT
                                      STRING "Employer: "
                                             FUNCTION TRIM(
                                                 JOB-EMPLOYER(APP-MATCH-IDX))
                                             DELIMITED BY SIZE
                                             INTO LINE-TEXT
                                      END-STRING
                                      PERFORM PRINT-LINE

                                      MOVE SPACES TO LINE-TEXT
                                      STRING "Location: "
                                             FUNCTION TRIM(
                                                 JOB-LOCATION(APP-MATCH-IDX))
                                             DELIMITED BY SIZE
                                             INTO LINE-TEXT
                                      END-STRING
                                      PERFORM PRINT-LINE

                                      MOVE "---" TO LINE-TEXT
                                      PERFORM PRINT-LINE
                                  END-IF
                              END-IF
                          END-IF
                  END-READ
              END-PERFORM
              CLOSE APPLICATIONS-FILE
          END-IF

          SET EOF-NO TO TRUE

          IF APP-COUNT = 0
              MOVE "You have not applied to any jobs yet." TO LINE-TEXT
              PERFORM PRINT-LINE
          ELSE
              MOVE "------------------------------" TO LINE-TEXT
              PERFORM PRINT-LINE
              MOVE SPACES TO LINE-TEXT
              STRING "Total Applications: "
                     FUNCTION TRIM (APP-COUNT)
                     DELIMITED BY SIZE
                     INTO LINE-TEXT
              END-STRING
              PERFORM PRINT-LINE
              MOVE "------------------------------" TO LINE-TEXT
              PERFORM PRINT-LINE
          END-IF

          MOVE " " TO LINE-TEXT
          PERFORM PRINT-LINE.
