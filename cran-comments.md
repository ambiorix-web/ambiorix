## Test environments
* local R installation, R 4.1.2
* win-builder (devel)
* Mac OS via Github actions

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.

---

- The Description field is intended to be a (one paragraph) description of what the package does and why it may be useful. Please add a few more details about the package functionality and
implemented methods in your Description text.

> I have improved the description of the package.

Please add \value to .Rd files regarding exported methods and explain the functions results in the documentation. Please write about the structure of the output (class) and also what the output means. (If a function does not return a value, please document that too, e.g. `\value{No return value, called for side effects}` or similar)
Missing Rd-tags:
      add_template.Rd: \value
      Ambiorix.Rd: \value
      create_ambiorix.Rd: \value
      create_dockerfile.Rd: \value
      import.Rd: \value
      Logger.Rd: \value
      parsers.Rd: \value

> All missing `\value` tags were added

Please do not modify the user's global environment or the user's home filespace in your functions by deleting objects.

> The package does not delete any file or modify the global environment.
