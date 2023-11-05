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

- attributes
- action
- pre-install
- post-install
- pre-deinstall
- post-deinstall

### attributes

Assists the actions of changing the owner, group, or mode. It contains an associative array where the
possible keys are owner, group, and mode. The values are, respectively, a user name, a group
name, and a file mode. For example:

```
attributes: { owner: "games", group: "games", mode: 0555 }
```

### action

Defines what happens to the keyword’s parameter. Contains an array where the possible values are:

- dir
    - Register a directory to be created on install and removed on deinstall.
      This is ignored if the directory would be created anyway.
    - Mutually exclusive with **file** action
- file
    - Registers a file (ignored if duplicated).
    - Mutually exclusive with **dir** action
- setmode
    - If mode attribute is set, and **dir** or **file** action set, the mode is overridden.
- setgroup
    - If group attribute is set, and **dir** or **file** action set, the group is overridden.
- setowner
    - If owner attribute is set, and **dir** or **file** action set, the owner is overridden.
