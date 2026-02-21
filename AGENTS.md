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

## Testing Approach (Required)

1. Prefer tests that exercise the real app execution path (`index.html` + browser runtime + worker path) over isolated harnesses.
- For UI/solver behavior, default to Playwright app-level tests.
- A lower-level unit/harness test may exist, but it is not sufficient on its own for parity claims.

2. When parity with Fortran/reference behavior is required, include at least one app-level assertion for the same scenario.
- The test should fail if app wiring/order-of-operations diverges even when core math tests pass.

3. If app-level testing is difficult because logic is tightly coupled, refactor code to make app-path testing possible.
- Add small, explicit test hooks/helpers rather than creating alternate execution paths.
- Avoid duplicating parsing/update pipelines in tests when the app already has one.
