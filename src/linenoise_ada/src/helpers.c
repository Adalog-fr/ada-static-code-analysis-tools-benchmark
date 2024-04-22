/* SPDX-License-Identifier: BSD-2-Clause
 * For more license details, see LICENSE.
 */

#include <stdlib.h>
#include <string.h>

char *
hint_string_helper (const char *str)
{
	return strdup(str);
}

void
free_hint_helper (void *ptr)
{
	free(ptr);
}
