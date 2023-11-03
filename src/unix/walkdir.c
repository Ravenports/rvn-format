/*
 * This file is covered by the Internet Software Consortium (ISC) License
 * Reference: ../../License.txt
 */

#ifndef _WIN32

#include <stdio.h>
#include <dirent.h>

DIR *FOLDER = NULL;

int
walkdir_open_folder (const char * path)
{
	if (FOLDER != NULL) {
		return 2;
	}

	FOLDER = opendir (path);
	if (FOLDER == NULL) {
		return 1;
	}
	return 0;
}

int
walkdir_close_folder ()
{
	if (FOLDER == NULL) {
		return 2;
	}
	if (closedir (FOLDER) < 0) {
		return 1;
	}
	FOLDER = NULL;
	return 0;
}

char *
walkdir_next_entry () {
	struct dirent *entry;

	entry = readdir (FOLDER);
	if (entry == NULL) {
		return NULL;
	}
	return entry->d_name;
}


#endif /* __WIN32 */
