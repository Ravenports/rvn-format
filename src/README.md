# Manifests Keywords

The basic manifest is just a list of files to package.
They are listed one file per line.
If the line starts with a forward slash, the file is located relative to the stage directory.
If the line does not start with a forward slash, the line is located relative to the prefix
directory which itself is relative to the stage directory.

## Inbuilt keywords

When the last starts with the "@" character, it's defining a keyword that implies some
modification to the package is required.  This modification may be to the indicated file,
a non-packaged file (e.g. an empty directory), or some kind of action to be taken at
various phases of package deployment.

Most keywords are externally defined, but a couple are built into the rvn format.

### @(,,,) [file]

The empty keyword is a placeholder to use when the file’s owner, group, or mode need to be changed. 
For example, to set the group of the file to games and add the setgid bit, add:

```
@(,games,2755) sbin/daemon
```

Blank arguments of owner/group/permissions leave the attribute unchanged from what is present
in the stage directory.

### @comment [string]

Lines starting with @comment are ignored by the manifest file parser.

### @dir(,,) [path]

For directories created by file installation, this keyword will override the owner/group/permissions
of the given directory.   For all others, it will instruct the package to create that directory
with those attributes at package install, and attempt to remove the directory (if empty) when the
package is deinstalled.

### @dir [path]

For directories that aren't already created by file installation, this keyword will create the
directory at package installation and remove it (if empty) at package deinstallation.  The
attributes of the created directory will be standard.  Unnecessary lines startin with "@dir" will
invoke a notice in the verbose mode.

## External Keywords

Package list files can be extended by keywords that are defined in the Keywords directory.
The settings for each keyword are stored in a UCL file named <keyword>.ucl. The file must
contain at least one of these sections:

- action
- deprecated
- deprecation_message
- preformat_arguments
- pre-install
- post-install
- pre-deinstall
- post-deinstall

### action

Defines what happens to the keyword’s parameter. Contains an array where the possible values are:

- dir
    - Register a directory to be created on install and removed on deinstall.
      This is ignored if the directory would be created anyway.
    - If there are multiple arguments, the first is assumed to be the directory.
    - Mutually exclusive with **file** action
- file
    - Registers a file (ignored if duplicated).
    - If there are multiple arguments, the first is assumed to be the file.
    - Mutually exclusive with **dir** action

### attributes

Assists the actions of changing the owner, group, or mode. It contains an associative array where the
possible keys are owner, group, and mode. The values are, respectively, a user name, a group
name, and a file mode. For example:

```
attributes: { owner: "games", group: "games", mode: "0555" }
```

If the **dir** or **file** action is set, and attributes is not an empty array, the target of
the action will have the listed attributes overridden.

### deprecated

Boolean to mark a keyword as deprecated
```
deprecated: true
```

### deprecation_message

Message to be show if the keyword is used and mark as deprecated
```
deprecation_message: <<EOM
Use @preexec/@postexec instead
EOM
```

### [ argument references ]

Unlike FreeBSD's pkg, rvn automatically splits the argument line into multiple parts if they
are separated by spaces.

- The entire argument (including any spaces) is referred to be `%@`
- The argument is automatically split along spaces into numbered arguments like `%1`, `%2`, etc.
- If there are no spaces, `%1` is the same as `%@`

for example,
```
@foo some.content other.content
```
`%1` and `%2` will contain:
```
some.content
other.content
```

### [ other escape sequences ]

If the input line subject to escape sequence expansion contains the following sequences,
they will be expanded inline as follows.  For the purpose of illustration, assume the
defined prefix is `/raven` and the whitelist line equals `bin/emacs`.

- `%D` Expand to the defined prefix, in this case */raven*.
- `%B` Expand to the "dirname" of the fully qualified filename (prefix joined with the last filespec), in this case */raven/bin*.
- `%f` Expand to the "basename" of the fully qualified name, in this case *emacs*.

### preformat_arguments

Boolean to apply the escape sequence expansion to the given arguments of the keyword.  See `pre-install` action as an example.

### pre-install

Shell script to be run during the pre-install phase.  It will be
merged with any existing pre-install scripts.  The escape sequences will be
expanded as described earlier.

```
> cat preexec.ucl
actions: []
preformat_arguments: true
pre-install: <<EOS
%@
EOS
```

### post-install

Shell script to be run during the post-install phase.  It will be
merged with any existing post-install scripts.  The escape sequences will be
expanded as described earlier.

```
> cat info.ucl
actions: [file]
post-install: <<EOD
  case "%@" in
  /*) file="%@" ;;
   *) file="%D/%@" ;;
  esac
  indexinfo ${PKG_ROOTDIR}${file%/*}
EOD
post-deinstall: <<EOD
  case "%@" in
  /*) file="%@" ;;
   *) file="%D/%@" ;;
  esac
  indexinfo ${PKG_ROOTDIR}${file%/*}
EOD
```

### pre-deinstall

Shell script to be run during the pre-deinstall phase.  It will be
merged with any existing pre-deinstall scripts.  The escape sequences will be
expanded as described earlier.

```
> cat preunexec.ucl
actions: []
preformat_arguments: true
pre-deinstall: <<EOS
%@
EOS
```

### post-deinstall

Shell script to be run during the post-deinstall phase.  It will be
merged with any existing post-deinstall scripts.  The escape sequences will be
expanded as described earlier.  See the post-install section for an example.

### Lua versions

It has not been decided yet if rvn/Ravenports will support Lua scripts yet.
However, to protect for the case that it will, also support the Lua versions
of the phase scripts:

    - pre-install-lua
    - post-install-lua
    - pre-deinstall-lua
    - post-deinstall-lua

# Variables present in environment during package phases

This is out of scope for rvn format, but to put scripts in context, the following
variables will be defined by the handler of the rvn-formatted packages:

- PKG_ROOTDIR
    - Represents the root directory where the package is installed.
    - E.g `/opt/raven/bin/emacs` would have a PKG_ROOTDIR of `/opt`
- PKG_NAMEBASE
    - Equates to the namebase defined in the ucl manifest
- PKG_SUBPACKAGE
    - Equates to the subpackage defined in the ucl manifest
- PKG_VARIANT
    - Equates to the variant defined in the ucl manifest
