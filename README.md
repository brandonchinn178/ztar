# ztar

Reading and writing arbitrary archives.

An extension of the `tar` library that, similar to the `tar` UNIX executable,
can create an archive with a given compression algorithm and automatically
detect the compression algorithm of an archive when extracting.

```
import Codec.Archive.ZTar

-- equivalent to `Codec.Archive.Tar.create "archive.tar" "dist/" ["."]`
create' NoCompression "archive.tar" "dist/" ["."]

-- helper to compress a single directory; equivalent to previous line
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
