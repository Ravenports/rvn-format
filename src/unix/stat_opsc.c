/*
 *  SPDX-License-Identifier: ISC
 *  Reference: /License.txt
 */

#ifndef _WIN32

#include <sys/types.h>
#include <sys/stat.h>
#include <pwd.h>
#include <grp.h>
#include <fcntl.h>
#include <unistd.h>
#include <limits.h>

#ifndef S_ISUID
#define S_ISUID 0
#endif

#ifndef S_ISGID
#define S_ISGID 0
#endif

#ifndef S_ISVTX
#define S_ISVTX 0
#endif


struct timespec
get_mtime (struct stat *sb)
{
  return sb->st_mtim;
}

unsigned short
extract_permissions (struct stat *sb)
{
  // S_IRWXO  RWX mask for other
  // S_IRWXG  RWX mask for group
  // S_IRWXU  RWX mask for owner
  // S_ISVTX  sticky bit  (AKA S_ISTXT)
  // S_ISGID  set group id on execution
  // S_ISUID  set user id on execution

  return (sb->st_mode & (S_IRWXU | S_IRWXG | S_IRWXO | S_ISUID | S_ISGID | S_ISVTX));
}

mode_t
cuid_on_exec_bit_set (mode_t permissions) {
  return (permissions & S_ISUID);
}

mode_t
cgid_on_exec_bit_set (mode_t permissions) {
  return (permissions & S_ISGID);
}

mode_t
csticky_bit_set (mode_t permissions) {
  return (permissions & S_ISVTX);
}

char *
get_owner_name (struct stat *sb)
{
  struct passwd *pw = getpwuid(sb->st_uid);
  if (pw == NULL) {
     return NULL;
  }
  return pw->pw_name;
}

char *
get_group_name (struct stat *sb)
{
  struct group *gr = getgrgid(sb->st_gid);
  if (gr == NULL) {
    return NULL;
  }
  return gr->gr_name;
}

off_t
get_size (struct stat *sb)
{
  return sb->st_size;
}

gid_t
clookup_group (const char * name)
{
  struct group *gr = getgrnam (name);
  if (gr == NULL) {
    return 4000000000;
  }
  return gr->gr_gid;
}

uid_t
clookup_user (const char * name)
{
  struct passwd *pw = getpwnam(name);
  if (pw == NULL) {
    return 4000000000;
  }
  return pw->pw_uid;
}

uid_t
get_owner_id (struct stat *sb)
{
  return sb->st_uid;
}

gid_t
get_group_id (struct stat *sb)
{
  return sb->st_gid;
}

ino_t
get_inode_number (struct stat *sb)
{
  return sb->st_ino;
}

/*
 * Return number representing file type
 * 0 = unsupported
 * 1 = directory
 * 2 = regular file
 * 3 = symbolic ink
 * 4 = hard link
 * 5 = FIFO
 */
unsigned char
get_file_type (struct stat *sb)
{
  if (S_ISDIR (sb->st_mode))  { return 1; }
  if (S_ISLNK (sb->st_mode))  { return 3; }
  if (S_ISFIFO (sb->st_mode)) { return 5; }
  if (S_ISREG (sb->st_mode))
    {
      if (sb->st_nlink > 1) { return 4; }
      return 2;
    }
  return 0;
}

/*
 * set metadata return values
 *   0 Nothing done or all successful
 *
 *  Values greater than zero mean an error occurred.
 *  Bit position meanings:
 *  1 -> failed to open the file
 *  2 -> failed to close the file
 *  3 -> failed to set ownership
 *  4 -> failed to set permissions
 *  5 -> failed to set modification time
 */
unsigned char
set_metadata (const char *    path,
              unsigned char   reset_modtime,
              unsigned char   reset_ownership,
              unsigned char   reset_permissions,
              uid_t           new_user_id,
              gid_t           new_group_id,
              mode_t          new_permissions,
              time_t          new_mtime_epoch,
              long            new_mtime_nsecs)
{
  int filedesc;
  int rc = 0;
  const struct timespec times[2] = {{0, UTIME_OMIT}, {new_mtime_epoch, new_mtime_nsecs}};

  /* It is possible all three actions are disabled, but it shouldn't happen */
  if (!(reset_modtime || reset_ownership || reset_permissions)) { return 0; }

  if ((filedesc = open (path, O_RDONLY)) < 0) {
    return 1;
  }

  /* setting permissions has to follow ownership because chown clears SetUID and SetGID bits */
  if (reset_ownership) {
    if (fchown (filedesc, new_user_id, new_group_id) < 0) {
      rc += 4;
    }
  }

  if (reset_permissions) {
    if (fchmod (filedesc, new_permissions) < 0) {
      rc += 8;
    }
  }

  if (reset_modtime) {
    if (futimens (filedesc, times) < 0) {
      rc += 16;
    }
  }

  if (close (filedesc) < 0) {
    rc += 2;
  }

  return rc;
}

unsigned char
set_symlink_metadata (const char *    path,
                      const char *    path_directory,
                      const char *    symlink_filename,
                      unsigned char   reset_modtime,
                      unsigned char   reset_ownership,
                      unsigned char   reset_permissions,
                      uid_t           new_user_id,
                      gid_t           new_group_id,
                      mode_t          new_permissions,
                      time_t          new_mtime_epoch,
                      long            new_mtime_nsecs)
{
  int rc = 0;
  int root_fd;
  const struct timespec times[2] = {{0, UTIME_OMIT}, {new_mtime_epoch, new_mtime_nsecs}};

  /* It is possible all three actions are disabled, but it shouldn't happen */
  if (!(reset_modtime || reset_ownership || reset_permissions)) { return 0; }

  /* setting permissions has to follow ownership because chown clears SetUID and SetGID bits */
  if (reset_ownership) {
    if (lchown (path, new_user_id, new_group_id) < 0) {
      rc += 4;
    }
  }

  if (reset_permissions) {
    if (lchmod (path, new_permissions) < 0) {
      rc += 8;
    }
  }

  if (reset_modtime)
  {

    if ((root_fd = open (path_directory, O_DIRECTORY)) < 0)
    {
      rc += 16;
      return rc;
    }
    if (utimensat (root_fd, symlink_filename, times, AT_SYMLINK_NOFOLLOW) < 0)
    {
      rc += 16;
    }
    close (root_fd);
  }

  return rc;
}


unsigned char
set_fifo_metadata (const char *    path,
                   unsigned char   reset_modtime,
                   unsigned char   reset_ownership,
                   unsigned char   reset_permissions,
                   uid_t           new_user_id,
                   gid_t           new_group_id,
                   mode_t          new_permissions,
                   time_t          new_mtime_epoch,
                   long            new_mtime_nsecs)
{
  int rc = 0;
  const struct timespec times[2] = {{0, UTIME_OMIT}, {new_mtime_epoch, new_mtime_nsecs}};

  /* It is possible all three actions are disabled, but it shouldn't happen */
  if (!(reset_modtime || reset_ownership || reset_permissions)) { return 0; }

  /* setting permissions has to follow ownership because chown clears SetUID and SetGID bits */
  if (reset_ownership) {
    if (chown (path, new_user_id, new_group_id) < 0) {
      rc += 4;
    }
  }

  if (reset_permissions) {
    if (chmod (path, new_permissions) < 0) {
      rc += 8;
    }
  }

  if (reset_modtime) {
    if (utimensat (-1, path, times, AT_SYMLINK_NOFOLLOW) < 0) {
      rc += 16;
    }
  }

  return rc;
}


size_t
maximum_path_length () {
  return PATH_MAX;
}


int
writable_access (const char * path) {
  return access (path, W_OK);
}


size_t
cformat_file_time (time_t mtime_epoch, char * restrict buf, size_t maxsize) {
  // Format time, "yyyy-mm-dd hh:ss"
  struct tm ts;
  size_t res;

  ts = *localtime(&mtime_epoch);
  return strftime(buf, maxsize, "%Y-%m-%d %H:%M", &ts);
}

#endif /* __WIN32 */
