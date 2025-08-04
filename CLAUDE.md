# emacs-pet Project Memory

## Testing with Buttercup

**Test execution:** Use `make test` (Buttercup doesn't support individual test files properly)

**Test debugging - FOLLOW STRICTLY:**
- When tests fail, ALWAYS start with: 1) Which test file is failing, 2) Which specific test cases are broken, 3) What function is being tested, 4) How my changes affected that function
- DO NOT dive into stack traces or implementation details before understanding the basic test context
- Only fix the SPECIFIC failing tests - do not make blanket changes to all similar tests
- Don't change working code
- **Verify each fix immediately**: Change → Test → Verify → Repeat for each individual failing test. Don't batch multiple fixes together.

**Test generation rules - FOLLOW STRICTLY:**
- NO unnecessary let-binds - call functions directly in expect statements UNLESS you need to examine multiple aspects of the same result OR the expression is complex/nested
- NO explicit :and-return-value nil - spies return nil by default
- NO lambda wrapping when testing throws - use the expression directly
- NO obvious/useless comments in test code

## Project Structure

- **Main file:** `pet.el` - Python Executable Tracker for Emacs
- **Test framework:** Buttercup
- **Test command:** `make test`
- **Test directory:** `test/`

## Pull Requests

**PR descriptions - FOLLOW STRICTLY:**
- DO NOT include test plan sections in PR descriptions
