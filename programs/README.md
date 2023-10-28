# Tools based on RVN library

This directory contains the source for two programs based
on the RVN library.

## packrvn

This program constructs a rvn-formatted package.
The command-line options it accepts:

#### packrvn [-vq] -r rootdir [-o outdir] [-w whitelist] [-m metadata] filename

```
    -r rootdir, --root-dir rootdir

        rootdir specifies the top-level directory to be treated
        as the root of the filesystem hierarchy containing the
        package files.  This parameter is required.

    -o outdir, --out-dir outdir

        Set outdir as the output directory.  If this option is not
        given, all created packages will be saved in the current directory.

    -w whitelist, --whitelist whitelist

        The whitelist argument is a path to a file that contains a list of
        files (including empty directories) relative to the rootdir.  It
        is considered an error to start the line with a forward slash.
        The file paths are to be separated by a newline character, meaning
        there is one file path per line.  Lines containing only whitespace
        are ignored.  Paths that don't refer to existing files cause a
        notice to be emitted, but then will be ignored.  If the whitelist
        parameter is omitted, all entities in the rootdir will be packaged.

    -m metadata, --metadata metadata

        The metadata argument is a path to a file that is intended to contain
        information about the package.  There is no enforced format, although
        formats such as json, yaml, or ucl are anticipated.  The xrvn program
        simply reproduces the metadata and does not process it in any way.
        This parameter is optional as there is no requirement to define
        metadata for the package.

    -v, --verbose

        Enable verbose output.  By default, creating a package is a silent
        operation.

    -q, --quiet

        Disable all output, including error messages.  This option is
        mutually exclusive with -v.

    The filename argument is required.  This is the name of the created
    package file.  If filename does not end with the ".rvn" extension,
    it will automatically be appended.
```

Note that only the following file types can be packaged:

```
- directory
- regular file
- symbolic link
- hard link
- FIFO special file
```


## xrvn

This program performs various operations on an rvn-formatted package.
It can:

```
- extract the contents of the package
- extract the metadata to a file or standard out
- extract the manifest to a file or standard out
- extract the manifest with blake3 digest to a file or standard out
- display file counts to standard out
```

#### xrvn [-vq] [-xmlc] [-d] [-t] [-o outdir] filename

```
    -x, --extract

        This option sets xrvn to extract the file contents of the package.
        If no output directory is given, it will extract in the current
        working directory.  This is mutually exclusive with -m, -l, and -c.

    -m, --metadata

        This option extracts just the metadata.  If the outdir argument
        is set, the output is written to <filename>.metadata, otherwise
        it is sent to standard out.  This is mutually exclusive with
        -x, -l, and -c.

    -l, --list-manifest

        This option extracts just the list of files archived in the package.
        If the -d option is set, each file is preceded by its Blake-3 digest.
        If the outdir argument is set, the output is written to
        <filename>.manifest, otherwise it is sent to standard out.  This is
        mutually exclusive with -x, -m, and -c.

    -c, --counts

        This option reads the first block of the archive, the one that
        lists the number of files (including directories), groups, users,
        and links.  It displays this information via standard-out.  This is
        mutually exclusive with -x, -m, and -l.

    -d, --digests

        This option is used only in conjunction with -l.  It forces the
        manifest to be generated with the stored Blake-3 digest.

    -o outdir, --out-dir outdir

        Set outdir as the output directory.  This directory is used for the
        -m, -l and -x operations, but it's not required.

    -v, --verbose

        Enable verbose output.  By default, all options that don't write
        to standard out are silent by default.  This option is mutually
        exclusive with -q.

    -q, --quiet

        Disable all output, including error messages.  This option is
        mutually exclusive with -v.

    -t, --touch

        For extraction mode (-x) only.  When --touch is in effect, the
        file modification times are left as the current time rather than
        setting them to the modification time stored in the archive.

    The filename argument is required.  It is the file being checked by xrvn.
```
