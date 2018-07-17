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
