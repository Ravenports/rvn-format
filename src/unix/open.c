/*
 *  SPDX-License-Identifier: ISC
 *  Reference: /License.txt
 */

#ifndef _WIN32

#include <fcntl.h>

int
try_open(const char *path,
	 int flag_rdonly,
	 int flag_wronly,
	 int flag_append,
	 int flag_nonblock,
	 int flag_directory,
	 int flag_cloexec,
	 int flag_creat,
         int flag_trunc)
{
  int flags = 0;

  if (flag_rdonly && !flag_wronly)
    flags |= O_RDONLY;

  if (flag_wronly && !flag_rdonly)
    flags |= O_WRONLY;

  if (flag_rdonly && flag_wronly)
    flags |= O_RDWR;

  if (flag_append)
    flags |= O_APPEND;	

#ifdef O_NONBLOCK
  if (flag_nonblock)
    flags |= O_NONBLOCK;
#endif

#ifdef O_CLOEXEC
  if (flag_cloexec)
    flags |= O_CLOEXEC;
#endif

#ifdef O_DIRECTORY
  if (flag_directory)
    flags |= O_DIRECTORY;
#endif

  if (flag_creat)
    flags |= O_CREAT;

  if (flag_trunc)
    flags |= O_TRUNC;

  if (flag_creat)
    return (open (path, flags, 0644));
  else
    return (open (path, flags));
}

#endif /* __WIN32 */
