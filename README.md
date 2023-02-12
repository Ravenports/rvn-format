# rvn-format
New package format for Ravenports

## Archive Format

This format replaces the equivalent of a GNU TAR far compressed by zstd program,
only with several advantages.  The format supports a file index which includes a
blake3 digest for each file and the ability to extract individual files without
decompressing and disassembling the entire file.

The headers are different than TAR, but much more compact.
Each file is represented by a 320-byte block

```
Index  Information     bytes
------------------------------------------------------------
    0  Filename          256   *split into four 32-byte parts
  256  Blake3 digest      32
  288  modification time   8   *64-bit time
  296  owner user name     1   *normalized index
  297  owner group name    1   *normalized index
  298  file type           1   *regular, hardlink, symlink, directory, FIFO
  299  size multiplier     1   * File size div 4Gb
  300  file size           4   * File size modulo 4Gb
  304  compressed size     4   *4 Gb
  308  file mode           2   *16 bits
  310  link path size      2   *linux supports up to 4096 characters
  312  parent directory    2   *directory index (64k directories supported)
  314  padding             6   *stop at a nice increment of 64 bytes
```

## Structure

The archive contains 7 sequential blocks

### Block 1

32 bytes
```
Total  Information           bytes
------------------------------------------------------------
    3  Magic bytes             3
    4  version number          1
    6  Number of groups        2
    8  Number of owners        2
   12  Number of link blocks   4
   16  Number of files         4
   18  Num manifest blocks     2
   21  Manifest size (comp)    4
   32  Unused                 10
```
The first 32 bytes provides the number of groups, owners and files contained in the archive.
It also contains the number of 32-byte blocks required to hold the link data, and it
contains the number of 32-bytes blocks required to hold the manifest, as well as it's
zstd-compressed size.  As each type of information is a multiple of 32 bytes, the start
index of each block can be calculated.

### Block 2

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

### Block 3

32 bytes per owner record.
This structure is identical to the group records.
The index of the first record is 1, and the second record is 2, and so forth.

### Block 4

32 bytes per link block.
All of the relative link paths are concatenated without delimiter.  The blocks
should be read into a single continuous string.

### Block 5

320 bytes per file record.
A file index can be quickly constructed by reading Block 1, and using the contents to read blocks 2 through 5.  
At that point, the file index of the archive can be reconstructed.

The order of the files is determined by directory recursion.
The parent directory is scanned for true (not symlinked) directories.  They are then recursed, so the first
subdirectory is scanned for directories.  When the directory scan is over, the files (everything but
directories) are scanned and the function exits when complete.  

An index of the contents is computed sequentially.
The "zero" index starts with the link path of the first record, followed by the contents of the first file.  
Note that only the symlink types have link paths, so the length is zero in many cases.
Likewise directories have no contents, so they are also zero bytes.

### Block 6

32 bytes per manifest block.
The concatenated blocks contain the zstd-compressed version of the manifest, which can be surgically
extracted from the archive.

### Block 7

The last block contains the variable data of the file contents, sequentially.
When extracting the files, the file permissions and ownership can be set immediately, but the directory
permissions and ownership have to be done at the end to prevent the overwriting of the modification
time of the directories.
