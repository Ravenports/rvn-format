# rvn-format
New package format for Ravenports

## Archive Format

This format replaces the equivalent of a GNU TAR far compressed by zstd program,
only with several advantages.  The format supports a file index which includes a
blake3 digest for each file.  In theory this provides the ability to extract
individual files without decompressing and disassembling the entire file, but the
the impact on the final file size to support this is very high.  In the end,
smaller archives are valued more than individual file extraction.  However, the
index will allow only decompressing up to the location of the desired file, and
the ability to discard the previous data in memory.

The headers are different than TAR, but much more compact.
Each file is represented by a 64-byte block.
For the sake of overall compactness, the directory data including these headers
are all compressed in the final archive.

```
Index  Information     bytes
------------------------------------------------------------
    0  Blake3 digest      32
   32  modification time   8   *64-bit time component (seconds)
   40  modification time   4   *32-bit time component (nanoseconds)
   44  owner user name     1   *normalized index
   45  owner group name    1   *normalized index
   46  file type           1   *regular, hardlink, symlink, directory, FIFO
   47  size multiplier     1   *File size div 4Gb
   48  file size           4   *File size modulo 4Gb
   52  file mode           2   *16 bits
   54  link path size      2   *linux supports up to 4095 characters
   56  parent directory    2   *index of parent directory
   58  directory index     2   *directory index (64K directories supported)
   60  filename length     1   *Filename length limited to 255 characters
   61  padding             3   *Unused bytes for potential use
```

## Structure

The archive contains 4 non-aligned sequential blocks.

### Block 1

64 bytes
```
Index  Information           bytes
------------------------------------------------------------
    0  Magic bytes             3
    3  version number          1
    4  Number of groups        2
    6  Number of owners        2
    8  Number of link blocks   4
   12  Number of files         4
   16  Metadata comp size      4
   20  File index comp size    4
   24  Archive comp size       4
   28  Number of fname blocks  4
   32  Flatsize of metadata    4
   36  Flatsize of file index  4
   40  Flatsize of archive     8
   48  unused1                 4
   52  unused2                 4
   56  unused3                 4
   60  unused4                 4
```
The first 64 bytes provides the number of groups, owners and files contained in the archive.
It also contains the compressed size in bytes of the next three blocks.  The index of
each block can easily be calculated:

    Block 1 index =  0
    Block 2 index = 64
    Block 3 index = 64 + length of block 2
    Block 4 index = 64 + length of block 2 + length of block 3


### Block 2

This block contains the zstd-compressed contents of the provided metadata file.
By reading the first two blocks of the RVN archive, the manifest can be surgically
extracted without unrolling the entire file.

Due to the nature of this block being reserved for metadata, the size of the
uncompressed file is limited to 512 kb (524,288 bytes) by rule.

### Block 3

This block contains the zstd-compressed contents of concatenated blocks
FA, FB, FC, FD, and FE.  By reading the Block 1 and Block 3, the archive's complete
directory index including the Blake3 checksum of every regular file can be
surgically extracted without unrolling the entire file.

#### Block FA - Group Names

32 bytes per group record.
The maximum length of a group name is 32.  The record is fixed size, and it zero padded.
```
         1111111111222222222333
1234567890123456789012345678012
-------------------------------
postgres@@@@@@@@@@@@@@@@@@@@@@@
-------------------------------
```
In the illustration above, "@" is the zero byte.
The index of the first record is 1, and the second record is 2, and so forth.

#### Block FB - Owner Names

32 bytes per owner record.
This structure is identical to the group records.
The index of the first record is 1, and the second record is 2, and so forth.

#### Block FC - Link Paths

32 bytes per link block.
All of the relative link paths are concatenated in order without delimiter.
The blocks should be read into a single continuous data structure.

#### Block FD - File Records

64 bytes per file record.
A file index can be quickly constructed by reading Block 1, and using the contents to read blocks FA through FE.  
At that point, the file index of the archive can be reconstructed.

The order of the files is determined by directory recursion.
The parent directory is scanned for true (not symlinked) directories.  They are then recursed, so the first
subdirectory is scanned for directories.  When the directory scan is over, the files (everything but
directories) are scanned and the function exits when complete.  

An index of the contents is computed sequentially.
The "zero" index starts with the link path of the first record, followed by the contents of the first file.  
Note that only the symlink types have link paths, so the length is zero in many cases.
Likewise directories have no contents, so they are also zero bytes.

#### Block FE - Filenames
32 bytes per filename block.
All filenames are concatenated in order without a delimiter.  The blocks could be read into a single continuous
data structure, but it's more memory efficient to extract the filenames during the read to avoid a double
penalty between the intermediate and final memory allocations.

### Block 4

The last block contains the variable data of the compressed single-file archive.
When extracting the files, the file permissions and ownership can be set immediately, but the directory
permissions and ownership have to be done at the end to prevent the overwriting of the modification
time of the directories.
