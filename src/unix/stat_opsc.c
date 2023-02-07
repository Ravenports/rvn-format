/*
 * This file is covered by the Internet Software Consortium (ISC) License
 * Reference: ../../License.txt
 */

#include <sys/types.h>
#include <sys/stat.h>
#ifndef _WIN32
#include <pwd.h>
#include <grp.h>
#endif

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
#ifdef _WIN32
  return NULL;
#else
  struct passwd *pw = getpwuid(sb->st_uid);
  return pw->pw_name;
#endif
}

char *
get_group_name (struct stat *sb)
{
#ifdef _WIN32
  return NULL;
#else
  struct group *gr = getgrgid(sb->st_gid);
  return gr->gr_name;
#endif
}
