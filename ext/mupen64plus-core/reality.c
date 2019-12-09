#include <windows.h>
#include "reality.h"

#define X(name, args, ret) ret (*name) args;
REALITY_MUPEN_EXPORTS(X)
#undef X

void load_reality_mupen_library() {
	HMODULE mod = LoadLibrary("reality-mupen.dll");

	if (mod == NULL)
	{
		DWORD error = GetLastError();

		abort();
	}

#define X(name, args, ret)             \
	if (name != NULL) abort();		   \
	name = (void*)GetProcAddress(mod, #name); \
	if (name == NULL) abort();

	REALITY_MUPEN_EXPORTS(X)
#undef X
}