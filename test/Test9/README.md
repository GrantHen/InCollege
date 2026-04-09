# Epic 9 Test Requirements

This suite covers the Week 9 "View My Messages" requirements from `InCollege Software Req - Epic #9.pdf`.

The tests live in `test/Test9` because the repo already groups week-by-week feature work under `test/TestN`.

## Coverage Matrix

`TC01_No_Messages`
- Verifies a logged-in user can open `Messages -> View My Messages`.
- Verifies the no-message prompt is shown when the inbox is empty.
- Verifies the user can return to the messages menu.
- Verifies screen output matches `out/InCollege-Output.txt`.

`TC02_View_One_Message`
- Verifies one persisted message is displayed for the logged-in recipient.
- Verifies sender, message body, and timestamp are all shown.
- Verifies messages addressed to another user are not displayed.
- Verifies screen output matches `out/InCollege-Output.txt`.

`TC03_View_Multiple_Messages`
- Verifies multiple persisted messages are displayed for the logged-in recipient.
- Verifies messages from different senders are shown.
- Verifies unrelated messages are filtered out.
- Verifies the current implementation's intended order of newest first.
- Verifies screen output matches `out/InCollege-Output.txt`.

`TC04_Persistence`
- Verifies previously stored messages are still visible after one full program restart.
- Verifies viewing messages does not consume or delete them.
- Verifies screen output matches `out/InCollege-Output.txt` in both runs.

## How The Suite Works

- Each case seeds the `.dat` files in `data/` from `test/Test9/Seed/<case>/`.
- Each case drives only the file-input navigation needed for the Epic 9 flow.
- Assertions are stored separately in `test/Test9/Assertions/`.
- `scripts/run_test9.sh` checks both functional assertions and stdout/output-file parity.

## Running

Use:

```bash
./scripts/run_test9.sh
```

The script prefers Docker when available and falls back to a local `cobc` install.
