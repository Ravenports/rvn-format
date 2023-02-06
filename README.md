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
