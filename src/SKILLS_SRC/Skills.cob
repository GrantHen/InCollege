       LEARN-NEW-SKILL.
           MOVE 0 TO SKILL-CHOICE
           PERFORM UNTIL SKILL-CHOICE = 6
               MOVE "Learn a New Skill:" TO LINE-TEXT
               PERFORM PRINT-LINE
               MOVE "1. Skill 1" TO LINE-TEXT
               PERFORM PRINT-LINE
               MOVE "2. Skill 2" TO LINE-TEXT
               PERFORM PRINT-LINE
               MOVE "3. Skill 3" TO LINE-TEXT
               PERFORM PRINT-LINE
               MOVE "4. Skill 4" TO LINE-TEXT
               PERFORM PRINT-LINE
               MOVE "5. Skill 5" TO LINE-TEXT
               PERFORM PRINT-LINE

               *> Spec wording: Go Back return to previous menu
               MOVE "6. Go Back" TO LINE-TEXT
               PERFORM PRINT-LINE

               MOVE "Enter your choice: " TO LINE-TEXT
               PERFORM PRINT-LINE

               PERFORM GET-SKILL-CHOICE

               EVALUATE SKILL-CHOICE
                   WHEN 1 THRU 5
                       MOVE "This skill is under construction." TO LINE-TEXT
                       PERFORM PRINT-LINE
                       MOVE " " TO LINE-TEXT
                       PERFORM PRINT-LINE
                   WHEN 6
                       CONTINUE
                   WHEN OTHER
                       MOVE "Invalid choice. Try again." TO LINE-TEXT
                       PERFORM PRINT-LINE
                       MOVE " " TO LINE-TEXT
                       PERFORM PRINT-LINE
               END-EVALUATE
           END-PERFORM.
