# ztar 1.0.1

Changes:
* Allow up to `zip-1.2.0`

# ztar 1.0.0

Breaking changes:
* Compiled against `zip-1.0.0`

# ztar 0.2.0

Breaking changes:
* `create'` is now `createFrom`

Changes:
* Removed the `typed-paths` flag
* Added `create'`, `createFrom'`, and `extract'` which use Path types

# ztar 0.1.3

Changes:
* Add the `typed-paths` flag for using the `Path` library

# ztar 0.1.2

Changes:
* Fix for creating archives of non-UTF8 encoded files

# ztar 0.1.1

Changes:
* Use the Unix `tar` command instead, because of issues with the Haskell `tar` library

# ztar 0.1.0

Breaking changes:
* Works against zip-0.2.0, will revert in future release

# ztar 0.0.3

Breaking changes:
* `create` now takes compression algorithm

Other changes:
* `extract` automatically detects compression algorithm used
* `extract` now handles ZIP archives and uncompressed TAR archives
* Add integration testing

# ztar 0.0.2

* Add createGZ'

# ztar 0.0.1

* Initial commit
