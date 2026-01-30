----------- Docker Setup ------------------------------------------------------------------------------------------------------------------------------

Build the Docker image:
***docker build -t incollege docker***

Run the container and mount the project folder:
***docker run --rm -it -v ${PWD}:/app -w /app incollege***

----------- Compile Program ---------------------------------------------------------------------------------------------------------------------------

From the src folder, compile the program:
***cobc -x -free incollege.cob***

Then return to the project root:
***cd ..***

----------- Manual Input Execution --------------------------------------------------------------------------------------------------------------------

All program input is file-based. The program does NOT read from the keyboard *

To manually provide input, go to:
**test/InCollege-Input.txt**

Modify the file with the desired inputs such as menu choices, usernames, and passwords

Run the program from the project root:
***./incollege***

----------- Automated Testing (Test 1) ----------------------------------------------------------------------------------------------------------------

To run the automated test cases (from project's root):
***./scripts/run_test1.sh***

This script overwrites *test/InCollege-Input.txt* for each test case

This script resets *data/accounts.dat*

This script runs the program for each case

This script compares 'actual output' and 'expected output'

This script prints PASS or FAIL for each test

----------- Important Notes ---------------------------------------------------------------------------------------------------------------------------

Program output is written to:
**out/InCollege-Output.txt**

*test/InCollege-Input.txt* is always overwritten when running test scripts

*data/accounts.dat* is cleared during automated tests, but *test/InCollege-Input.txt* and *out/InCollege-Output.txt* 
will contain the data from the last automated test case.

After running the test script, manually edit *test/InCollege-Input.txt* again if you want to run the program by hand.















