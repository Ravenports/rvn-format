# rvn-format
New package format for Ravenports

## Archive Format

This is similar to the GNU TAR format with several improvements.
The format supports a file index which includes a blake3 digest for each file.

The headers are different, but much more compact. Each file is represented by a 320-byte block

```
Total  Information     bytes
------------------------------------------------------------
  256  Filename          256
  288  Blake3 digest      32
  289  owner user name     1   *normalized index
  290  owner group name    1   *normalized index
  291  file type           1   *regular, hardlink, symlink, directory, FIFO
  293  file mode           2   *12 bits
  298  file size           5   *1 Tb
  300  link path size      2   *linux supports up to 4096 characters
  308  modification time   8   *64-bit time
  310  parent directory    2   *directory index (64k directories supported)
  320  padding            10   *stop at a nice increment of 64 bytes
```

## Structure

The archive contains 5 sequential blocks

### Block 1

12 bytes
```
Total  Information     bytes
------------------------------------------------------------
    3  Magic bytes         3
    4  version number      1
    6  Number of groups    2
    8  Number of owners    2
   12  Number of files     4
```
The first 12 bytes provides the number of groups, owners and files contained in the archive.
As each type of information has a fixed size, the start index of each block can be calculated.

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

320 bytes per file record.
A file index can be quickly constructed by reading Block 1, and using the contents to read blocks 2 through 4.  At that point, the file index of the archive can be reconstructed.

The order of the files is determined by directory recursion.
The parent directory is scanned for directories and put in alphabetical order.
They are then recursed, so the first subdirectory is scanned for directories.  When the directory scan is over, the files (everything but directories) are scanned and the function exits when complete.  This ensures the same order every time the archive is created.

An index of the contents is computed sequentially.
The "zero" index starts with the link path of the first record, followed by the contents of the first file.  Note that only the symlink types have link paths, so the length is zero in many cases.  Likewise directories have no contents, so they are also zero bytes.

### Block 5

The last block contains the variable data of symbolic link targets and the file contents, sequentially.
When extracting the files, the file permissions and ownership can be set immediately, but the directory permissions and ownership have to be done at the end to prevent the overwriting of the modification time of the directories.
