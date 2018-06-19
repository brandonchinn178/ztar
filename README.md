# ztar

Reading and writing compressed `.tar` archives.

An extension of the `tar` library that can create/extract compressed tar archives.

```
import Codec.Archive.Tar.Extra

-- same as Codec.Archive.Tar.create
create' NoCompression "archive.tar" "dist/" ["."]

-- helper to compress a single directory; equivalent to previous
create NoCompression "archive.tar" "dist/"

-- compress with GZip
create GZip "archive.tar.gz" "dist/"

-- compress with Zip
create Zip "archive.zip" "dist/"

-- automatically determines compression
extract "archive-tar/" "archive.tar"
extract "archive-gz/" "archive.tar.gz"
extract "archive-zip/" "archive.zip"
```
