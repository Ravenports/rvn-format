/*
 *  SPDX-License-Identifier: ISC
 *  Reference: /License.txt
 */

#ifndef _WIN32

#include <stdio.h>
#include <dirent.h>

DIR *FOLDER[128];

int
walkdir_open_folder (const char * path, int zbuilder)
{
	FOLDER[zbuilder] = opendir (path);
	if (FOLDER == NULL) {
		return 1;
	}
	return 0;
}

int
walkdir_close_folder (int zbuilder)
{
	if (FOLDER[zbuilder] == NULL) {
		return 2;
	}
	if (closedir (FOLDER[zbuilder]) < 0) {
		return 1;
	}
	return 0;
}

char *
walkdir_next_entry (int zbuilder)
{
	struct dirent *entry;

	entry = readdir (FOLDER[zbuilder]);
	if (entry == NULL) {
		return NULL;
	}
	return entry->d_name;
}


#endif /* __WIN32 */
