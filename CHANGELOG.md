# Changelog
Any changes related to Quark will be contained primarily in this file. Find all Quark updates below.

## [1.0.1]() - 2021-01-12

### Fixed
- List specific index modification fixed
- CLI location fixed
- Callback weren't working

### Removed
- Spreading and variadic support due to some bugs during function call

## [1.0.0]() - 2021-01-10
First stable Quark release. Some bugs could again occurs.

### Added
- Variables are now passed by reference by default
- Boolean, List and None types added
- Interpreter divided into multiple utility-classes
- Utility-functions like `isValue`, `isObject` or `isContainer`

### Changed
- License updated to MIT
- Stack woking

### Fixed
- Return statement was tricky in some cases
- Variable scoping didn't work properly in functions and callbacks
- Variable modification didn't work due to scoping
- Self-variables in blocks were processed as expression

## [0.1.0]() - 2021-01-03
This is the first Quark release. Please be aware that Quark can be unstable and if a bug occurs, please report it by creating an issue.

### Added
- Default simple and modulable interpreter
- REPL written in Quark
- Basic operations: `+`, `-`, `/`, `*`.
- Classic types support: `string`, `list`, `number`
- Typescript and Quark module file import
- Frame stacking and scoping
- Function definition and callback
- Variadic arguments and spread list
- Comments: `# inline` and `/* multiline */`
- While loop
- Condition
- Basic equalities: `=`, `!=`, `<`, `>`, `<=`, `>=`, `and`, `or`
- List mutability via function
