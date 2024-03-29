# Tools based on RVN library

This directory contains the source for two programs based
on the RVN library.

## packrvn

This program constructs a rvn-formatted package.
The command-line options it accepts:

#### packrvn [-vq] -r rootdir [-o outdir] [-w whitelist] [-p prefix] [-a abi] [-k keyword_dir] [-m metadata] filename

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
        files relative to the prefix (which is a subdirectory of the rootdir.
        The file paths are to be separated by a newline character, meaning
        there is one file path per line.  Lines containing only whitespace
        are ignored.  Paths that don't refer to existing files cause a
        notice to be emitted, but then will be ignored.  If the whitelist
        parameter is omitted, all entities in the rootdir will be packaged.
        If the line starts with a forward slash then the prefix is ignored
        and the path is considered relative to the root directory.

    -p prefix, --prefix outdir

        Set the prefix which is a subdirectory of the rootdir, e.g. /usr/local
        or /prefix.

    -a abi, --abi abi

        The default for this parameter is "*:0:*".  This parameter is typically
        generated data contained in an ELF binary, or otherwise overridden in
        a configuration file.  This parameter is added the generated metadata file.

    -k keyword_dir, --keyword-dir

        The default for this parameter is "/var/ravenports/conspiracy/Mk/Keywords".
        It indicates the path of the directory that contains the UCL-based keyword
        definitions for the keywords used on the whitelist, (e.g. @info, @sample,
        @shell).

    -m metadata, --metadata metadata

        The metadata argument is a path to a file that is intended to contain
        information about the package.  There is no enforced format, although
        formats such as json, yaml, or ucl are anticipated.  The xrvn program
        simply reproduces the metadata and does not process it in any way.
        This parameter is optional as there is no requirement to define
        metadata for the package.

    -t, --timestamp

        Override the modification time of all archived files to given unix
        epoch.  This may be used to support reproducible packages.

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

#### xrvn [-vq] [-xmlc] [-d | -a] [-t] [-o outdir] filename

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
        manifest to be generated with the stored Blake-3 digest.  This is
        mutually exclusive with -a.

    -a, --attributes

        This option is used only in conjunction with -l.  It forces the
        manifest to be generated with the file permissions, ownership, group
        membership and file size.  This is mutually exclusive with -d.

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
