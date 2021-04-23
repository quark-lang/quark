# Changelog
Any changes related to Quark will be contained primarily in this file. Find all Quark updates below.

## [2.0.0]() - 2021-04-23
This new major version brings a lot of new features to the language and especially an important migration from Node to Deno. The latter will be continuously updated through PATCH versions to make Quark more and more stable.

### Added
- String interpolation ([#7acf575](https://github.com/quark-lang/quark/commit/7acf575))
- Syntaxic sugar for `begin` and `list` ([#92960bc](https://github.com/quark-lang/quark/commit/92960bc))
- Module URL import ([#68f5169](https://github.com/quark-lang/quark/commit/68f5169))
- Pretty printing ([#1e12adc](https://github.com/quark-lang/quark/commit/1e12adc))
- Expression quoting ([#e9ab606](https://github.com/quark-lang/quark/commit/e9ab606))
- Native JS API Support ([#0267d9e](https://github.com/quark-lang/quark/commit/0267d9e))

### Fixed
- New implementation of stack ([#ca27725](https://github.com/quark-lang/quark/commit/ca27725))
- Depthed imports ([#7e21de2](https://github.com/quark-lang/quark/commit/7e21de2))
- EOF prevention ([#31b531d](https://github.com/quark-lang/quark/commit/31b531d))
- String escaping ([#b8a9be2](https://github.com/quark-lang/quark/commit/b8a9be2))
- Import paths on Windows ([#5ea50e9](https://github.com/quark-lang/quark/commit/5ea50e9))

### Changed
- Use of Node.js instead of Deno ([#c476fa9](https://github.com/quark-lang/quark/commit/c476fa9))

## [1.3.0]() - 2021-01-26

### Fixed
- Value processing cleaned and improved
- Quarkify return and raw value
- QuarkRC useless fields removed

### Added
- List processing highly improved (50-100% faster)
- No processing argument keyword
- Object support
- New Quark environment variable

## [1.2.1]() - 2021-01-18

### Fixed
- Value interpretation :
  - `"true"` and `"false"` were parsed as boolean
  - Value were compared as stringified JSON output

## [1.2.0]() - 2021-01-18

### Added
- `getValue`, `setValue` and `QuarkType`
- `quarkify` function
- `or` operator
- Escape sequences (`\n`, `\r`, `\t`, ...)
- Quark configuration file `.quarkrc`
- Fibonacci tests

### Fixed
- Quark pathfinder
- Environment path variable looker
- Imports using root
- Quark Windows executable ignored

## [1.1.0]() - 2021-01-16

### Added
- Code analysis and autobuild actions
- STD loading by default
- Quark STD
- Assert library
- `and` operator
- Function object caller

### Changed
- REPL moved to cli folder

### Fixed
- Processing function call made asynchronous
- Build action
- Submodule fetching
- Absolute static import path

## [1.0.2]() - 2021-01-13

### Fixed
- Reference issue causing AST modification.
- Block scoping in modules
- Single expression return
- Boolean processing
- Function return
- Import on Windows
- Comparison operands

### Added
- Simple tests

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
