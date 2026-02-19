# AGENTS.md

## Verification Rules (Required)

1. For UI changes, edit the actual served entrypoint used by this repo:
- `index.html` (root app entry)

2. Do not add or rely on an additional HTML entrypoint under `js/dist/`.

3. Add or update an automated regression test for every non-trivial UI structure change.
- Prefer Playwright tests in `js/playwright/`.
- The test must fail on the old behavior and pass on the new behavior.

4. Before reporting completion, run the relevant checks and report results:
- syntax check for changed JS (`node --check ...`)
- the new/updated Playwright spec(s)

5. If the UI shown by the user differs from expected, first verify which page/path they are using and reproduce there before making more changes.
