/*
 * This file is covered by the Internet Software Consortium (ISC) License
 * Reference: ../../License.txt
 */

#ifndef _WIN32

#include <sys/types.h>
#include <sys/stat.h>
#include <pwd.h>
#include <grp.h>

#ifndef S_ISUID
#define S_ISUID 0
#endif

#ifndef S_ISGID
#define S_ISGID 0
#endif

#ifndef S_ISVTX
#define S_ISVTX 0
#endif


long
get_mtime (struct stat *sb)
{
  return sb->st_mtime;
}

unsigned short
extract_permissions (struct stat *sb)
{
  // S_IRWXO  RWX mask for other
  // S_IRWXG  RWX mask for group
  // S_IRWXG  RWX mask for owner
  // S_ISVTX  sticky bit
  // S_ISGID  set group id on execution
  // S_ISUID  set user id on execution

  return (sb->st_mode & (S_IRWXO | S_IRWXG | S_IRWXG | S_ISUID | S_ISGID | S_ISVTX));
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

#endif /* __WIN32 */
